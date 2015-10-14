C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE SISDGEO6                                                 1
C SISDGEO6 IS THE SILICON STRIP DETECTOR WITH TUP MODIFICATIONS           1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'SISDGEO6'/, BANK_TITLE/'*'/                            1
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
      INTEGER BLOCK_SFLM                                                  10
      INTEGER BLOCK_SFDM                                                  10
      INTEGER BLOCK_SFSW                                                  10
      INTEGER BLOCK_SFSD                                                  10
      INTEGER BLOCK_SFSL                                                  10
      INTEGER BLOCK_SFSM                                                  10
      INTEGER BLOCK_SFLT                                                  10
      INTEGER BLOCK_SFLU                                                  10
      INTEGER BLOCK_SFFK                                                  10
      INTEGER BLOCK_SFFL                                                  10
      INTEGER BLOCK_SFKK                                                  10
      INTEGER BLOCK_SFKL                                                  10
      INTEGER BLOCK_SFRA                                                  10
      INTEGER BLOCK_SFRS                                                  10
      INTEGER BLOCK_SFFX                                                  10
      INTEGER BLOCK_SFPI                                                  10
      INTEGER BLOCK_SFPJ                                                  10
      INTEGER BLOCK_SFAA                                                  10
      INTEGER BLOCK_SFAM                                                  10
      INTEGER BLOCK_SFAB                                                  10
      INTEGER BLOCK_SFAS                                                  10
      INTEGER BLOCK_SAPP                                                  10
      INTEGER BLOCK_SAPC                                                  10
      INTEGER BLOCK_SAPS                                                  10
      INTEGER BLOCK_SAPT                                                  10
      INTEGER BLOCK_SFLA                                                  10
      INTEGER BLOCK_SFLB                                                  10
      INTEGER BLOCK_SFLC                                                  10
      INTEGER BLOCK_SFES                                                  10
      INTEGER BLOCK_SFEB                                                  10
      INTEGER BLOCK_SFCO                                                  10
      INTEGER BLOCK_SFCM                                                  10
      INTEGER BLOCK_SFCB                                                  10
      INTEGER BLOCK_SFCS                                                  10
      INTEGER BLOCK_SFKF                                                  10
      INTEGER BLOCK_SFKS                                                  10
      INTEGER BLOCK_SSBS                                                  10
      INTEGER BLOCK_SSBB                                                  10
      INTEGER BLOCK_SFPR                                                  10
      INTEGER BLOCK_SFPB                                                  10
      INTEGER BLOCK_SSST                                                  10
      INTEGER BLOCK_SSSS                                                  10
      INTEGER BLOCK_SSRS                                                  10
      INTEGER BLOCK_SSRT                                                  10
      INTEGER BLOCK_SSLB                                                  10
      INTEGER BLOCK_SBCH                                                  10
      INTEGER BLOCK_SBCV                                                  10
      INTEGER BLOCK_SBFH                                                  10
      INTEGER BLOCK_SBFV                                                  10
      INTEGER BLOCK_SSLT                                                  10
      INTEGER BLOCK_SCMP                                                  10
      INTEGER BLOCK_SCVM                                                  10
      INTEGER BLOCK_SCVB                                                  10
      INTEGER BLOCK_SCVS                                                  10
      INTEGER BLOCK_SOSK                                                  10
C -                                                                       11
      REAL SSDP_VERSION                                                   11
      EQUIVALENCE (BANK_SSDP(3),SSDP_VERSION,RE_SSDP_VERSION),            11
     *(NAMES_SSDP(1),NSSDP_VERSION)                                       11
      EQUIVALENCE (COMMEN_SSDP(1),CSSDP_VERSION),(MAP_SSDP(1,1),          11
     *MSSDP_VERSION)                                                      11
      CHARACTER CSSDP_VERSION*40/' '/                                     11
      CHARACTER NSSDP_VERSION*12/'R.VERSION'/                             11
      INTEGER MSSDP_VERSION(2)/1,1/                                       11
      REAL RE_SSDP_VERSION                                                11
C -                                                                       11
      INTEGER SSDP_CONFIG                                                 11
      EQUIVALENCE (BANK_SSDP(4),SSDP_CONFIG,RE_SSDP_CONFIG),              11
     *(NAMES_SSDP(2),NSSDP_CONFIG)                                        11
      EQUIVALENCE (COMMEN_SSDP(2),CSSDP_CONFIG),(MAP_SSDP(1,2),           11
     *MSSDP_CONFIG)                                                       11
      CHARACTER CSSDP_CONFIG*40/' '/                                      11
      CHARACTER NSSDP_CONFIG*12/'I.CONFIG'/                               11
      INTEGER MSSDP_CONFIG(2)/1,1/                                        11
      REAL RE_SSDP_CONFIG                                                 11
C -                                                                       11
      INTEGER SSDP_PLACEMENT                                              11
      EQUIVALENCE (BANK_SSDP(5),SSDP_PLACEMENT,RE_SSDP_PLACEMENT),        11
     *(NAMES_SSDP(3),NSSDP_PLACEMENT)                                     11
      EQUIVALENCE (COMMEN_SSDP(3),CSSDP_PLACEMENT),(MAP_SSDP(1,3),        11
     *MSSDP_PLACEMENT)                                                    11
      CHARACTER CSSDP_PLACEMENT*40/' '/                                   11
      CHARACTER NSSDP_PLACEMENT*12/'I.PLACEMENT'/                         11
      INTEGER MSSDP_PLACEMENT(2)/1,1/                                     11
      REAL RE_SSDP_PLACEMENT                                              11
C -                                                                       11
      INTEGER LEN_SSDP,LLEN_SSDP,LINK_SSDP                                11
      PARAMETER (LEN_SSDP=5)                                              11
      PARAMETER (LLEN_SSDP=3)                                             11
      INTEGER FLAG_SSDP/0/,MAP_SSDP(2,LLEN_SSDP),IDNUM_SSDP(10)           11
      CHARACTER NAMES_SSDP(LLEN_SSDP)*12,COMMEN_SSDP(LLEN_SSDP)*40        11
      DATA IDNUM_SSDP/10*0/                                               11
      REAL BANK_SSDP(LEN_SSDP)                                            11
      CHARACTER*60 BPATH_SSDP/' '/                                        11
      SAVE FLAG_SSDP,MAP_SSDP,IDNUM_SSDP,NAMES_SSDP,COMMEN_SSDP,          11
     *BPATH_SSDP                                                          11
      DATA LINK_SSDP/0/,BANK_SSDP/LEN_SSDP*0/                             11
C *                                                                       11
      INTEGER CONFIGA,CONFIGB                                             12
C -                                                                       29
      INTEGER SFJP_VERSION                                                29
      EQUIVALENCE (BANK_SFJP(3),SFJP_VERSION,RE_SFJP_VERSION),            29
     *(NAMES_SFJP(1),NSFJP_VERSION)                                       29
      EQUIVALENCE (COMMEN_SFJP(1),CSFJP_VERSION),(MAP_SFJP(1,1),          29
     *MSFJP_VERSION)                                                      29
      CHARACTER CSFJP_VERSION*40/' '/                                     29
      CHARACTER NSFJP_VERSION*12/'I.VERSION'/                             29
      INTEGER MSFJP_VERSION(2)/1,1/                                       29
      REAL RE_SFJP_VERSION                                                29
C -                                                                       29
      REAL SFJP_ALPHAZ                                                    29
      EQUIVALENCE (BANK_SFJP(4),SFJP_ALPHAZ,RE_SFJP_ALPHAZ),              29
     *(NAMES_SFJP(2),NSFJP_ALPHAZ)                                        29
      EQUIVALENCE (COMMEN_SFJP(2),CSFJP_ALPHAZ),(MAP_SFJP(1,2),           29
     *MSFJP_ALPHAZ)                                                       29
      CHARACTER CSFJP_ALPHAZ*40/' '/                                      29
      CHARACTER NSFJP_ALPHAZ*12/'R.ALPHAZ'/                               29
      INTEGER MSFJP_ALPHAZ(2)/1,1/                                        29
      REAL RE_SFJP_ALPHAZ                                                 29
C -                                                                       29
      REAL SFJP_ALPHAZH                                                   29
      EQUIVALENCE (BANK_SFJP(5),SFJP_ALPHAZH,RE_SFJP_ALPHAZH),            29
     *(NAMES_SFJP(3),NSFJP_ALPHAZH)                                       29
      EQUIVALENCE (COMMEN_SFJP(3),CSFJP_ALPHAZH),(MAP_SFJP(1,3),          29
     *MSFJP_ALPHAZH)                                                      29
      CHARACTER CSFJP_ALPHAZH*40/' '/                                     29
      CHARACTER NSFJP_ALPHAZH*12/'R.ALPHAZH'/                             29
      INTEGER MSFJP_ALPHAZH(2)/1,1/                                       29
      REAL RE_SFJP_ALPHAZH                                                29
C -                                                                       29
      REAL SFJP_SSST_RMIN                                                 29
      EQUIVALENCE (BANK_SFJP(6),SFJP_SSST_RMIN,RE_SFJP_SSST_RMIN),        29
     *(NAMES_SFJP(4),NSFJP_SSST_RMIN)                                     29
      EQUIVALENCE (COMMEN_SFJP(4),CSFJP_SSST_RMIN),(MAP_SFJP(1,4),        29
     *MSFJP_SSST_RMIN)                                                    29
      CHARACTER CSFJP_SSST_RMIN*40/' '/                                   29
      CHARACTER NSFJP_SSST_RMIN*12/'R.SSST_RMIN'/                         29
      INTEGER MSFJP_SSST_RMIN(2)/1,1/                                     29
      REAL RE_SFJP_SSST_RMIN                                              29
C -                                                                       29
      REAL SFJP_SSST_RMAX                                                 29
      EQUIVALENCE (BANK_SFJP(7),SFJP_SSST_RMAX,RE_SFJP_SSST_RMAX),        29
     *(NAMES_SFJP(5),NSFJP_SSST_RMAX)                                     29
      EQUIVALENCE (COMMEN_SFJP(5),CSFJP_SSST_RMAX),(MAP_SFJP(1,5),        29
     *MSFJP_SSST_RMAX)                                                    29
      CHARACTER CSFJP_SSST_RMAX*40/' '/                                   29
      CHARACTER NSFJP_SSST_RMAX*12/'R.SSST_RMAX'/                         29
      INTEGER MSFJP_SSST_RMAX(2)/1,1/                                     29
      REAL RE_SFJP_SSST_RMAX                                              29
C -                                                                       29
      REAL SFJP_SSST_WIDTH                                                29
      EQUIVALENCE (BANK_SFJP(8),SFJP_SSST_WIDTH,RE_SFJP_SSST_WIDTH),      29
     *(NAMES_SFJP(6),NSFJP_SSST_WIDTH)                                    29
      EQUIVALENCE (COMMEN_SFJP(6),CSFJP_SSST_WIDTH),(MAP_SFJP(1,6),       29
     *MSFJP_SSST_WIDTH)                                                   29
      CHARACTER CSFJP_SSST_WIDTH*40/' '/                                  29
      CHARACTER NSFJP_SSST_WIDTH*12/'R.SSST_WIDTH'/                       29
      INTEGER MSFJP_SSST_WIDTH(2)/1,1/                                    29
      REAL RE_SFJP_SSST_WIDTH                                             29
C -                                                                       29
      REAL SFJP_SSST_PZ                                                   29
      EQUIVALENCE (BANK_SFJP(9),SFJP_SSST_PZ,RE_SFJP_SSST_PZ),            29
     *(NAMES_SFJP(7),NSFJP_SSST_PZ)                                       29
      EQUIVALENCE (COMMEN_SFJP(7),CSFJP_SSST_PZ),(MAP_SFJP(1,7),          29
     *MSFJP_SSST_PZ)                                                      29
      CHARACTER CSFJP_SSST_PZ*40/' '/                                     29
      CHARACTER NSFJP_SSST_PZ*12/'R.SSST_PZ'/                             29
      INTEGER MSFJP_SSST_PZ(2)/1,1/                                       29
      REAL RE_SFJP_SSST_PZ                                                29
C -                                                                       29
      REAL SFJP_SSSS_RMIN                                                 29
      EQUIVALENCE (BANK_SFJP(10),SFJP_SSSS_RMIN,RE_SFJP_SSSS_RMIN),       29
     *(NAMES_SFJP(8),NSFJP_SSSS_RMIN)                                     29
      EQUIVALENCE (COMMEN_SFJP(8),CSFJP_SSSS_RMIN),(MAP_SFJP(1,8),        29
     *MSFJP_SSSS_RMIN)                                                    29
      CHARACTER CSFJP_SSSS_RMIN*40/' '/                                   29
      CHARACTER NSFJP_SSSS_RMIN*12/'R.SSSS_RMIN'/                         29
      INTEGER MSFJP_SSSS_RMIN(2)/1,1/                                     29
      REAL RE_SFJP_SSSS_RMIN                                              29
C -                                                                       29
      REAL SFJP_SSSS_WIDTH                                                29
      EQUIVALENCE (BANK_SFJP(11),SFJP_SSSS_WIDTH,RE_SFJP_SSSS_WIDTH),     29
     *(NAMES_SFJP(9),NSFJP_SSSS_WIDTH)                                    29
      EQUIVALENCE (COMMEN_SFJP(9),CSFJP_SSSS_WIDTH),(MAP_SFJP(1,9),       29
     *MSFJP_SSSS_WIDTH)                                                   29
      CHARACTER CSFJP_SSSS_WIDTH*40/' '/                                  29
      CHARACTER NSFJP_SSSS_WIDTH*12/'R.SSSS_WIDTH'/                       29
      INTEGER MSFJP_SSSS_WIDTH(2)/1,1/                                    29
      REAL RE_SFJP_SSSS_WIDTH                                             29
C -                                                                       29
      REAL SFJP_SSRS_RMIN                                                 29
      EQUIVALENCE (BANK_SFJP(12),SFJP_SSRS_RMIN,RE_SFJP_SSRS_RMIN),       29
     *(NAMES_SFJP(10),NSFJP_SSRS_RMIN)                                    29
      EQUIVALENCE (COMMEN_SFJP(10),CSFJP_SSRS_RMIN),(MAP_SFJP(1,10),      29
     *MSFJP_SSRS_RMIN)                                                    29
      CHARACTER CSFJP_SSRS_RMIN*40/' '/                                   29
      CHARACTER NSFJP_SSRS_RMIN*12/'R.SSRS_RMIN'/                         29
      INTEGER MSFJP_SSRS_RMIN(2)/1,1/                                     29
      REAL RE_SFJP_SSRS_RMIN                                              29
C -                                                                       29
      REAL SFJP_SSLB_DX                                                   29
      EQUIVALENCE (BANK_SFJP(13),SFJP_SSLB_DX,RE_SFJP_SSLB_DX),           29
     *(NAMES_SFJP(11),NSFJP_SSLB_DX)                                      29
      EQUIVALENCE (COMMEN_SFJP(11),CSFJP_SSLB_DX),(MAP_SFJP(1,11),        29
     *MSFJP_SSLB_DX)                                                      29
      CHARACTER CSFJP_SSLB_DX*40/' '/                                     29
      CHARACTER NSFJP_SSLB_DX*12/'R.SSLB_DX'/                             29
      INTEGER MSFJP_SSLB_DX(2)/1,1/                                       29
      REAL RE_SFJP_SSLB_DX                                                29
C -                                                                       29
      REAL SFJP_SSLB_PX                                                   29
      EQUIVALENCE (BANK_SFJP(14),SFJP_SSLB_PX,RE_SFJP_SSLB_PX),           29
     *(NAMES_SFJP(12),NSFJP_SSLB_PX)                                      29
      EQUIVALENCE (COMMEN_SFJP(12),CSFJP_SSLB_PX),(MAP_SFJP(1,12),        29
     *MSFJP_SSLB_PX)                                                      29
      CHARACTER CSFJP_SSLB_PX*40/' '/                                     29
      CHARACTER NSFJP_SSLB_PX*12/'R.SSLB_PX'/                             29
      INTEGER MSFJP_SSLB_PX(2)/1,1/                                       29
      REAL RE_SFJP_SSLB_PX                                                29
C -                                                                       29
      REAL SFJP_SSLB_DY                                                   29
      EQUIVALENCE (BANK_SFJP(15),SFJP_SSLB_DY,RE_SFJP_SSLB_DY),           29
     *(NAMES_SFJP(13),NSFJP_SSLB_DY)                                      29
      EQUIVALENCE (COMMEN_SFJP(13),CSFJP_SSLB_DY),(MAP_SFJP(1,13),        29
     *MSFJP_SSLB_DY)                                                      29
      CHARACTER CSFJP_SSLB_DY*40/' '/                                     29
      CHARACTER NSFJP_SSLB_DY*12/'R.SSLB_DY'/                             29
      INTEGER MSFJP_SSLB_DY(2)/1,1/                                       29
      REAL RE_SFJP_SSLB_DY                                                29
C -                                                                       29
      REAL SFJP_SSLB_DZ                                                   29
      EQUIVALENCE (BANK_SFJP(16),SFJP_SSLB_DZ,RE_SFJP_SSLB_DZ),           29
     *(NAMES_SFJP(14),NSFJP_SSLB_DZ)                                      29
      EQUIVALENCE (COMMEN_SFJP(14),CSFJP_SSLB_DZ),(MAP_SFJP(1,14),        29
     *MSFJP_SSLB_DZ)                                                      29
      CHARACTER CSFJP_SSLB_DZ*40/' '/                                     29
      CHARACTER NSFJP_SSLB_DZ*12/'R.SSLB_DZ'/                             29
      INTEGER MSFJP_SSLB_DZ(2)/1,1/                                       29
      REAL RE_SFJP_SSLB_DZ                                                29
C -                                                                       29
      REAL SFJP_SSBQ_DX                                                   29
      EQUIVALENCE (BANK_SFJP(17),SFJP_SSBQ_DX,RE_SFJP_SSBQ_DX),           29
     *(NAMES_SFJP(15),NSFJP_SSBQ_DX)                                      29
      EQUIVALENCE (COMMEN_SFJP(15),CSFJP_SSBQ_DX),(MAP_SFJP(1,15),        29
     *MSFJP_SSBQ_DX)                                                      29
      CHARACTER CSFJP_SSBQ_DX*40/' '/                                     29
      CHARACTER NSFJP_SSBQ_DX*12/'R.SSBQ_DX'/                             29
      INTEGER MSFJP_SSBQ_DX(2)/1,1/                                       29
      REAL RE_SFJP_SSBQ_DX                                                29
C -                                                                       29
      REAL SFJP_SSBQ_DY                                                   29
      EQUIVALENCE (BANK_SFJP(18),SFJP_SSBQ_DY,RE_SFJP_SSBQ_DY),           29
     *(NAMES_SFJP(16),NSFJP_SSBQ_DY)                                      29
      EQUIVALENCE (COMMEN_SFJP(16),CSFJP_SSBQ_DY),(MAP_SFJP(1,16),        29
     *MSFJP_SSBQ_DY)                                                      29
      CHARACTER CSFJP_SSBQ_DY*40/' '/                                     29
      CHARACTER NSFJP_SSBQ_DY*12/'R.SSBQ_DY'/                             29
      INTEGER MSFJP_SSBQ_DY(2)/1,1/                                       29
      REAL RE_SFJP_SSBQ_DY                                                29
C -                                                                       29
      REAL SFJP_SSBQ_DZ                                                   29
      EQUIVALENCE (BANK_SFJP(19),SFJP_SSBQ_DZ,RE_SFJP_SSBQ_DZ),           29
     *(NAMES_SFJP(17),NSFJP_SSBQ_DZ)                                      29
      EQUIVALENCE (COMMEN_SFJP(17),CSFJP_SSBQ_DZ),(MAP_SFJP(1,17),        29
     *MSFJP_SSBQ_DZ)                                                      29
      CHARACTER CSFJP_SSBQ_DZ*40/' '/                                     29
      CHARACTER NSFJP_SSBQ_DZ*12/'R.SSBQ_DZ'/                             29
      INTEGER MSFJP_SSBQ_DZ(2)/1,1/                                       29
      REAL RE_SFJP_SSBQ_DZ                                                29
C -                                                                       29
      REAL SFJP_SSCR_THA                                                  29
      EQUIVALENCE (BANK_SFJP(20),SFJP_SSCR_THA,RE_SFJP_SSCR_THA),         29
     *(NAMES_SFJP(18),NSFJP_SSCR_THA)                                     29
      EQUIVALENCE (COMMEN_SFJP(18),CSFJP_SSCR_THA),(MAP_SFJP(1,18),       29
     *MSFJP_SSCR_THA)                                                     29
      CHARACTER CSFJP_SSCR_THA*40/' '/                                    29
      CHARACTER NSFJP_SSCR_THA*12/'R.SSCR_THA'/                           29
      INTEGER MSFJP_SSCR_THA(2)/1,1/                                      29
      REAL RE_SFJP_SSCR_THA                                               29
C -                                                                       29
      REAL SFJP_SSCR_THD                                                  29
      EQUIVALENCE (BANK_SFJP(21),SFJP_SSCR_THD,RE_SFJP_SSCR_THD),         29
     *(NAMES_SFJP(19),NSFJP_SSCR_THD)                                     29
      EQUIVALENCE (COMMEN_SFJP(19),CSFJP_SSCR_THD),(MAP_SFJP(1,19),       29
     *MSFJP_SSCR_THD)                                                     29
      CHARACTER CSFJP_SSCR_THD*40/' '/                                    29
      CHARACTER NSFJP_SSCR_THD*12/'R.SSCR_THD'/                           29
      INTEGER MSFJP_SSCR_THD(2)/1,1/                                      29
      REAL RE_SFJP_SSCR_THD                                               29
C -                                                                       29
      REAL SFJP_SSCR_WD                                                   29
      EQUIVALENCE (BANK_SFJP(22),SFJP_SSCR_WD,RE_SFJP_SSCR_WD),           29
     *(NAMES_SFJP(20),NSFJP_SSCR_WD)                                      29
      EQUIVALENCE (COMMEN_SFJP(20),CSFJP_SSCR_WD),(MAP_SFJP(1,20),        29
     *MSFJP_SSCR_WD)                                                      29
      CHARACTER CSFJP_SSCR_WD*40/' '/                                     29
      CHARACTER NSFJP_SSCR_WD*12/'R.SSCR_WD'/                             29
      INTEGER MSFJP_SSCR_WD(2)/1,1/                                       29
      REAL RE_SFJP_SSCR_WD                                                29
C -                                                                       29
      REAL SFJP_SSCR_HL                                                   29
      EQUIVALENCE (BANK_SFJP(23),SFJP_SSCR_HL,RE_SFJP_SSCR_HL),           29
     *(NAMES_SFJP(21),NSFJP_SSCR_HL)                                      29
      EQUIVALENCE (COMMEN_SFJP(21),CSFJP_SSCR_HL),(MAP_SFJP(1,21),        29
     *MSFJP_SSCR_HL)                                                      29
      CHARACTER CSFJP_SSCR_HL*40/' '/                                     29
      CHARACTER NSFJP_SSCR_HL*12/'R.SSCR_HL'/                             29
      INTEGER MSFJP_SSCR_HL(2)/1,1/                                       29
      REAL RE_SFJP_SSCR_HL                                                29
C -                                                                       29
      REAL SFJP_SSCR_VL                                                   29
      EQUIVALENCE (BANK_SFJP(24),SFJP_SSCR_VL,RE_SFJP_SSCR_VL),           29
     *(NAMES_SFJP(22),NSFJP_SSCR_VL)                                      29
      EQUIVALENCE (COMMEN_SFJP(22),CSFJP_SSCR_VL),(MAP_SFJP(1,22),        29
     *MSFJP_SSCR_VL)                                                      29
      CHARACTER CSFJP_SSCR_VL*40/' '/                                     29
      CHARACTER NSFJP_SSCR_VL*12/'R.SSCR_VL'/                             29
      INTEGER MSFJP_SSCR_VL(2)/1,1/                                       29
      REAL RE_SFJP_SSCR_VL                                                29
C -                                                                       29
      REAL SFJP_SSLT_PX                                                   29
      EQUIVALENCE (BANK_SFJP(25),SFJP_SSLT_PX,RE_SFJP_SSLT_PX),           29
     *(NAMES_SFJP(23),NSFJP_SSLT_PX)                                      29
      EQUIVALENCE (COMMEN_SFJP(23),CSFJP_SSLT_PX),(MAP_SFJP(1,23),        29
     *MSFJP_SSLT_PX)                                                      29
      CHARACTER CSFJP_SSLT_PX*40/' '/                                     29
      CHARACTER NSFJP_SSLT_PX*12/'R.SSLT_PX'/                             29
      INTEGER MSFJP_SSLT_PX(2)/1,1/                                       29
      REAL RE_SFJP_SSLT_PX                                                29
C -                                                                       29
      REAL SFJP_SSLT_RMAX                                                 29
      EQUIVALENCE (BANK_SFJP(26),SFJP_SSLT_RMAX,RE_SFJP_SSLT_RMAX),       29
     *(NAMES_SFJP(24),NSFJP_SSLT_RMAX)                                    29
      EQUIVALENCE (COMMEN_SFJP(24),CSFJP_SSLT_RMAX),(MAP_SFJP(1,24),      29
     *MSFJP_SSLT_RMAX)                                                    29
      CHARACTER CSFJP_SSLT_RMAX*40/' '/                                   29
      CHARACTER NSFJP_SSLT_RMAX*12/'R.SSLT_RMAX'/                         29
      INTEGER MSFJP_SSLT_RMAX(2)/1,1/                                     29
      REAL RE_SFJP_SSLT_RMAX                                              29
C -                                                                       29
      REAL SFJP_SSLT_DZ                                                   29
      EQUIVALENCE (BANK_SFJP(27),SFJP_SSLT_DZ,RE_SFJP_SSLT_DZ),           29
     *(NAMES_SFJP(25),NSFJP_SSLT_DZ)                                      29
      EQUIVALENCE (COMMEN_SFJP(25),CSFJP_SSLT_DZ),(MAP_SFJP(1,25),        29
     *MSFJP_SSLT_DZ)                                                      29
      CHARACTER CSFJP_SSLT_DZ*40/' '/                                     29
      CHARACTER NSFJP_SSLT_DZ*12/'R.SSLT_DZ'/                             29
      INTEGER MSFJP_SSLT_DZ(2)/1,1/                                       29
      REAL RE_SFJP_SSLT_DZ                                                29
C -                                                                       29
      REAL SFJP_SCMP_DX                                                   29
      EQUIVALENCE (BANK_SFJP(28),SFJP_SCMP_DX,RE_SFJP_SCMP_DX),           29
     *(NAMES_SFJP(26),NSFJP_SCMP_DX)                                      29
      EQUIVALENCE (COMMEN_SFJP(26),CSFJP_SCMP_DX),(MAP_SFJP(1,26),        29
     *MSFJP_SCMP_DX)                                                      29
      CHARACTER CSFJP_SCMP_DX*40/' '/                                     29
      CHARACTER NSFJP_SCMP_DX*12/'R.SCMP_DX'/                             29
      INTEGER MSFJP_SCMP_DX(2)/1,1/                                       29
      REAL RE_SFJP_SCMP_DX                                                29
C -                                                                       29
      REAL SFJP_SCMP_DY                                                   29
      EQUIVALENCE (BANK_SFJP(29),SFJP_SCMP_DY,RE_SFJP_SCMP_DY),           29
     *(NAMES_SFJP(27),NSFJP_SCMP_DY)                                      29
      EQUIVALENCE (COMMEN_SFJP(27),CSFJP_SCMP_DY),(MAP_SFJP(1,27),        29
     *MSFJP_SCMP_DY)                                                      29
      CHARACTER CSFJP_SCMP_DY*40/' '/                                     29
      CHARACTER NSFJP_SCMP_DY*12/'R.SCMP_DY'/                             29
      INTEGER MSFJP_SCMP_DY(2)/1,1/                                       29
      REAL RE_SFJP_SCMP_DY                                                29
C -                                                                       29
      REAL SFJP_SCMP_DZ                                                   29
      EQUIVALENCE (BANK_SFJP(30),SFJP_SCMP_DZ,RE_SFJP_SCMP_DZ),           29
     *(NAMES_SFJP(28),NSFJP_SCMP_DZ)                                      29
      EQUIVALENCE (COMMEN_SFJP(28),CSFJP_SCMP_DZ),(MAP_SFJP(1,28),        29
     *MSFJP_SCMP_DZ)                                                      29
      CHARACTER CSFJP_SCMP_DZ*40/' '/                                     29
      CHARACTER NSFJP_SCMP_DZ*12/'R.SCMP_DZ'/                             29
      INTEGER MSFJP_SCMP_DZ(2)/1,1/                                       29
      REAL RE_SFJP_SCMP_DZ                                                29
C -                                                                       29
      REAL SFJP_SCMP_PX                                                   29
      EQUIVALENCE (BANK_SFJP(31),SFJP_SCMP_PX,RE_SFJP_SCMP_PX),           29
     *(NAMES_SFJP(29),NSFJP_SCMP_PX)                                      29
      EQUIVALENCE (COMMEN_SFJP(29),CSFJP_SCMP_PX),(MAP_SFJP(1,29),        29
     *MSFJP_SCMP_PX)                                                      29
      CHARACTER CSFJP_SCMP_PX*40/' '/                                     29
      CHARACTER NSFJP_SCMP_PX*12/'R.SCMP_PX'/                             29
      INTEGER MSFJP_SCMP_PX(2)/1,1/                                       29
      REAL RE_SFJP_SCMP_PX                                                29
C -                                                                       29
      REAL SFJP_SCVM_DZ                                                   29
      EQUIVALENCE (BANK_SFJP(32),SFJP_SCVM_DZ,RE_SFJP_SCVM_DZ),           29
     *(NAMES_SFJP(30),NSFJP_SCVM_DZ)                                      29
      EQUIVALENCE (COMMEN_SFJP(30),CSFJP_SCVM_DZ),(MAP_SFJP(1,30),        29
     *MSFJP_SCVM_DZ)                                                      29
      CHARACTER CSFJP_SCVM_DZ*40/' '/                                     29
      CHARACTER NSFJP_SCVM_DZ*12/'R.SCVM_DZ'/                             29
      INTEGER MSFJP_SCVM_DZ(2)/1,1/                                       29
      REAL RE_SFJP_SCVM_DZ                                                29
C -                                                                       29
      REAL SFJP_SCVM_DX                                                   29
      EQUIVALENCE (BANK_SFJP(33),SFJP_SCVM_DX,RE_SFJP_SCVM_DX),           29
     *(NAMES_SFJP(31),NSFJP_SCVM_DX)                                      29
      EQUIVALENCE (COMMEN_SFJP(31),CSFJP_SCVM_DX),(MAP_SFJP(1,31),        29
     *MSFJP_SCVM_DX)                                                      29
      CHARACTER CSFJP_SCVM_DX*40/' '/                                     29
      CHARACTER NSFJP_SCVM_DX*12/'R.SCVM_DX'/                             29
      INTEGER MSFJP_SCVM_DX(2)/1,1/                                       29
      REAL RE_SFJP_SCVM_DX                                                29
C -                                                                       29
      REAL SFJP_SCVM_PX                                                   29
      EQUIVALENCE (BANK_SFJP(34),SFJP_SCVM_PX,RE_SFJP_SCVM_PX),           29
     *(NAMES_SFJP(32),NSFJP_SCVM_PX)                                      29
      EQUIVALENCE (COMMEN_SFJP(32),CSFJP_SCVM_PX),(MAP_SFJP(1,32),        29
     *MSFJP_SCVM_PX)                                                      29
      CHARACTER CSFJP_SCVM_PX*40/' '/                                     29
      CHARACTER NSFJP_SCVM_PX*12/'R.SCVM_PX'/                             29
      INTEGER MSFJP_SCVM_PX(2)/1,1/                                       29
      REAL RE_SFJP_SCVM_PX                                                29
C -                                                                       29
      REAL SFJP_SCVM_DY                                                   29
      EQUIVALENCE (BANK_SFJP(35),SFJP_SCVM_DY,RE_SFJP_SCVM_DY),           29
     *(NAMES_SFJP(33),NSFJP_SCVM_DY)                                      29
      EQUIVALENCE (COMMEN_SFJP(33),CSFJP_SCVM_DY),(MAP_SFJP(1,33),        29
     *MSFJP_SCVM_DY)                                                      29
      CHARACTER CSFJP_SCVM_DY*40/' '/                                     29
      CHARACTER NSFJP_SCVM_DY*12/'R.SCVM_DY'/                             29
      INTEGER MSFJP_SCVM_DY(2)/1,1/                                       29
      REAL RE_SFJP_SCVM_DY                                                29
C -                                                                       29
      REAL SFJP_SCVM_PZ                                                   29
      EQUIVALENCE (BANK_SFJP(36),SFJP_SCVM_PZ,RE_SFJP_SCVM_PZ),           29
     *(NAMES_SFJP(34),NSFJP_SCVM_PZ)                                      29
      EQUIVALENCE (COMMEN_SFJP(34),CSFJP_SCVM_PZ),(MAP_SFJP(1,34),        29
     *MSFJP_SCVM_PZ)                                                      29
      CHARACTER CSFJP_SCVM_PZ*40/' '/                                     29
      CHARACTER NSFJP_SCVM_PZ*12/'R.SCVM_PZ'/                             29
      INTEGER MSFJP_SCVM_PZ(2)/1,1/                                       29
      REAL RE_SFJP_SCVM_PZ                                                29
C -                                                                       29
      REAL SFJP_SCVB_DY                                                   29
      EQUIVALENCE (BANK_SFJP(37),SFJP_SCVB_DY,RE_SFJP_SCVB_DY),           29
     *(NAMES_SFJP(35),NSFJP_SCVB_DY)                                      29
      EQUIVALENCE (COMMEN_SFJP(35),CSFJP_SCVB_DY),(MAP_SFJP(1,35),        29
     *MSFJP_SCVB_DY)                                                      29
      CHARACTER CSFJP_SCVB_DY*40/' '/                                     29
      CHARACTER NSFJP_SCVB_DY*12/'R.SCVB_DY'/                             29
      INTEGER MSFJP_SCVB_DY(2)/1,1/                                       29
      REAL RE_SFJP_SCVB_DY                                                29
C -                                                                       29
      REAL SFJP_SCVS_DX                                                   29
      EQUIVALENCE (BANK_SFJP(38),SFJP_SCVS_DX,RE_SFJP_SCVS_DX),           29
     *(NAMES_SFJP(36),NSFJP_SCVS_DX)                                      29
      EQUIVALENCE (COMMEN_SFJP(36),CSFJP_SCVS_DX),(MAP_SFJP(1,36),        29
     *MSFJP_SCVS_DX)                                                      29
      CHARACTER CSFJP_SCVS_DX*40/' '/                                     29
      CHARACTER NSFJP_SCVS_DX*12/'R.SCVS_DX'/                             29
      INTEGER MSFJP_SCVS_DX(2)/1,1/                                       29
      REAL RE_SFJP_SCVS_DX                                                29
C -                                                                       29
      REAL SFJP_SCVS_DY                                                   29
      EQUIVALENCE (BANK_SFJP(39),SFJP_SCVS_DY,RE_SFJP_SCVS_DY),           29
     *(NAMES_SFJP(37),NSFJP_SCVS_DY)                                      29
      EQUIVALENCE (COMMEN_SFJP(37),CSFJP_SCVS_DY),(MAP_SFJP(1,37),        29
     *MSFJP_SCVS_DY)                                                      29
      CHARACTER CSFJP_SCVS_DY*40/' '/                                     29
      CHARACTER NSFJP_SCVS_DY*12/'R.SCVS_DY'/                             29
      INTEGER MSFJP_SCVS_DY(2)/1,1/                                       29
      REAL RE_SFJP_SCVS_DY                                                29
C -                                                                       29
      REAL SFJP_SCVS_PX                                                   29
      EQUIVALENCE (BANK_SFJP(40),SFJP_SCVS_PX,RE_SFJP_SCVS_PX),           29
     *(NAMES_SFJP(38),NSFJP_SCVS_PX)                                      29
      EQUIVALENCE (COMMEN_SFJP(38),CSFJP_SCVS_PX),(MAP_SFJP(1,38),        29
     *MSFJP_SCVS_PX)                                                      29
      CHARACTER CSFJP_SCVS_PX*40/' '/                                     29
      CHARACTER NSFJP_SCVS_PX*12/'R.SCVS_PX'/                             29
      INTEGER MSFJP_SCVS_PX(2)/1,1/                                       29
      REAL RE_SFJP_SCVS_PX                                                29
C -                                                                       29
      REAL SFJP_SCVS_PY                                                   29
      EQUIVALENCE (BANK_SFJP(41),SFJP_SCVS_PY,RE_SFJP_SCVS_PY),           29
     *(NAMES_SFJP(39),NSFJP_SCVS_PY)                                      29
      EQUIVALENCE (COMMEN_SFJP(39),CSFJP_SCVS_PY),(MAP_SFJP(1,39),        29
     *MSFJP_SCVS_PY)                                                      29
      CHARACTER CSFJP_SCVS_PY*40/' '/                                     29
      CHARACTER NSFJP_SCVS_PY*12/'R.SCVS_PY'/                             29
      INTEGER MSFJP_SCVS_PY(2)/1,1/                                       29
      REAL RE_SFJP_SCVS_PY                                                29
C -                                                                       29
      REAL SFJP_SFCO_DX                                                   29
      EQUIVALENCE (BANK_SFJP(42),SFJP_SFCO_DX,RE_SFJP_SFCO_DX),           29
     *(NAMES_SFJP(40),NSFJP_SFCO_DX)                                      29
      EQUIVALENCE (COMMEN_SFJP(40),CSFJP_SFCO_DX),(MAP_SFJP(1,40),        29
     *MSFJP_SFCO_DX)                                                      29
      CHARACTER CSFJP_SFCO_DX*40/' '/                                     29
      CHARACTER NSFJP_SFCO_DX*12/'R.SFCO_DX'/                             29
      INTEGER MSFJP_SFCO_DX(2)/1,1/                                       29
      REAL RE_SFJP_SFCO_DX                                                29
C -                                                                       29
      REAL SFJP_SFCO_DY                                                   29
      EQUIVALENCE (BANK_SFJP(43),SFJP_SFCO_DY,RE_SFJP_SFCO_DY),           29
     *(NAMES_SFJP(41),NSFJP_SFCO_DY)                                      29
      EQUIVALENCE (COMMEN_SFJP(41),CSFJP_SFCO_DY),(MAP_SFJP(1,41),        29
     *MSFJP_SFCO_DY)                                                      29
      CHARACTER CSFJP_SFCO_DY*40/' '/                                     29
      CHARACTER NSFJP_SFCO_DY*12/'R.SFCO_DY'/                             29
      INTEGER MSFJP_SFCO_DY(2)/1,1/                                       29
      REAL RE_SFJP_SFCO_DY                                                29
C -                                                                       29
      REAL SFJP_SFCO_DZ                                                   29
      EQUIVALENCE (BANK_SFJP(44),SFJP_SFCO_DZ,RE_SFJP_SFCO_DZ),           29
     *(NAMES_SFJP(42),NSFJP_SFCO_DZ)                                      29
      EQUIVALENCE (COMMEN_SFJP(42),CSFJP_SFCO_DZ),(MAP_SFJP(1,42),        29
     *MSFJP_SFCO_DZ)                                                      29
      CHARACTER CSFJP_SFCO_DZ*40/' '/                                     29
      CHARACTER NSFJP_SFCO_DZ*12/'R.SFCO_DZ'/                             29
      INTEGER MSFJP_SFCO_DZ(2)/1,1/                                       29
      REAL RE_SFJP_SFCO_DZ                                                29
C -                                                                       29
      REAL SFJP_SFCO_PX                                                   29
      EQUIVALENCE (BANK_SFJP(45),SFJP_SFCO_PX,RE_SFJP_SFCO_PX),           29
     *(NAMES_SFJP(43),NSFJP_SFCO_PX)                                      29
      EQUIVALENCE (COMMEN_SFJP(43),CSFJP_SFCO_PX),(MAP_SFJP(1,43),        29
     *MSFJP_SFCO_PX)                                                      29
      CHARACTER CSFJP_SFCO_PX*40/' '/                                     29
      CHARACTER NSFJP_SFCO_PX*12/'R.SFCO_PX'/                             29
      INTEGER MSFJP_SFCO_PX(2)/1,1/                                       29
      REAL RE_SFJP_SFCO_PX                                                29
C -                                                                       29
      REAL SFJP_SFCO_PY                                                   29
      EQUIVALENCE (BANK_SFJP(46),SFJP_SFCO_PY,RE_SFJP_SFCO_PY),           29
     *(NAMES_SFJP(44),NSFJP_SFCO_PY)                                      29
      EQUIVALENCE (COMMEN_SFJP(44),CSFJP_SFCO_PY),(MAP_SFJP(1,44),        29
     *MSFJP_SFCO_PY)                                                      29
      CHARACTER CSFJP_SFCO_PY*40/' '/                                     29
      CHARACTER NSFJP_SFCO_PY*12/'R.SFCO_PY'/                             29
      INTEGER MSFJP_SFCO_PY(2)/1,1/                                       29
      REAL RE_SFJP_SFCO_PY                                                29
C -                                                                       29
      REAL SFJP_SFCM_DX                                                   29
      EQUIVALENCE (BANK_SFJP(47),SFJP_SFCM_DX,RE_SFJP_SFCM_DX),           29
     *(NAMES_SFJP(45),NSFJP_SFCM_DX)                                      29
      EQUIVALENCE (COMMEN_SFJP(45),CSFJP_SFCM_DX),(MAP_SFJP(1,45),        29
     *MSFJP_SFCM_DX)                                                      29
      CHARACTER CSFJP_SFCM_DX*40/' '/                                     29
      CHARACTER NSFJP_SFCM_DX*12/'R.SFCM_DX'/                             29
      INTEGER MSFJP_SFCM_DX(2)/1,1/                                       29
      REAL RE_SFJP_SFCM_DX                                                29
C -                                                                       29
      REAL SFJP_SFCS_DZ                                                   29
      EQUIVALENCE (BANK_SFJP(48),SFJP_SFCS_DZ,RE_SFJP_SFCS_DZ),           29
     *(NAMES_SFJP(46),NSFJP_SFCS_DZ)                                      29
      EQUIVALENCE (COMMEN_SFJP(46),CSFJP_SFCS_DZ),(MAP_SFJP(1,46),        29
     *MSFJP_SFCS_DZ)                                                      29
      CHARACTER CSFJP_SFCS_DZ*40/' '/                                     29
      CHARACTER NSFJP_SFCS_DZ*12/'R.SFCS_DZ'/                             29
      INTEGER MSFJP_SFCS_DZ(2)/1,1/                                       29
      REAL RE_SFJP_SFCS_DZ                                                29
C -                                                                       29
      REAL SFJP_SFKF_DY                                                   29
      EQUIVALENCE (BANK_SFJP(49),SFJP_SFKF_DY,RE_SFJP_SFKF_DY),           29
     *(NAMES_SFJP(47),NSFJP_SFKF_DY)                                      29
      EQUIVALENCE (COMMEN_SFJP(47),CSFJP_SFKF_DY),(MAP_SFJP(1,47),        29
     *MSFJP_SFKF_DY)                                                      29
      CHARACTER CSFJP_SFKF_DY*40/' '/                                     29
      CHARACTER NSFJP_SFKF_DY*12/'R.SFKF_DY'/                             29
      INTEGER MSFJP_SFKF_DY(2)/1,1/                                       29
      REAL RE_SFJP_SFKF_DY                                                29
C -                                                                       29
      REAL SFJP_SFKF_DZ                                                   29
      EQUIVALENCE (BANK_SFJP(50),SFJP_SFKF_DZ,RE_SFJP_SFKF_DZ),           29
     *(NAMES_SFJP(48),NSFJP_SFKF_DZ)                                      29
      EQUIVALENCE (COMMEN_SFJP(48),CSFJP_SFKF_DZ),(MAP_SFJP(1,48),        29
     *MSFJP_SFKF_DZ)                                                      29
      CHARACTER CSFJP_SFKF_DZ*40/' '/                                     29
      CHARACTER NSFJP_SFKF_DZ*12/'R.SFKF_DZ'/                             29
      INTEGER MSFJP_SFKF_DZ(2)/1,1/                                       29
      REAL RE_SFJP_SFKF_DZ                                                29
C -                                                                       29
      REAL SFJP_SFKF_DX                                                   29
      EQUIVALENCE (BANK_SFJP(51),SFJP_SFKF_DX,RE_SFJP_SFKF_DX),           29
     *(NAMES_SFJP(49),NSFJP_SFKF_DX)                                      29
      EQUIVALENCE (COMMEN_SFJP(49),CSFJP_SFKF_DX),(MAP_SFJP(1,49),        29
     *MSFJP_SFKF_DX)                                                      29
      CHARACTER CSFJP_SFKF_DX*40/' '/                                     29
      CHARACTER NSFJP_SFKF_DX*12/'R.SFKF_DX'/                             29
      INTEGER MSFJP_SFKF_DX(2)/1,1/                                       29
      REAL RE_SFJP_SFKF_DX                                                29
C -                                                                       29
      REAL SFJP_SFKF_PX                                                   29
      EQUIVALENCE (BANK_SFJP(52),SFJP_SFKF_PX,RE_SFJP_SFKF_PX),           29
     *(NAMES_SFJP(50),NSFJP_SFKF_PX)                                      29
      EQUIVALENCE (COMMEN_SFJP(50),CSFJP_SFKF_PX),(MAP_SFJP(1,50),        29
     *MSFJP_SFKF_PX)                                                      29
      CHARACTER CSFJP_SFKF_PX*40/' '/                                     29
      CHARACTER NSFJP_SFKF_PX*12/'R.SFKF_PX'/                             29
      INTEGER MSFJP_SFKF_PX(2)/1,1/                                       29
      REAL RE_SFJP_SFKF_PX                                                29
C -                                                                       29
      REAL SFJP_SFKF_PY                                                   29
      EQUIVALENCE (BANK_SFJP(53),SFJP_SFKF_PY,RE_SFJP_SFKF_PY),           29
     *(NAMES_SFJP(51),NSFJP_SFKF_PY)                                      29
      EQUIVALENCE (COMMEN_SFJP(51),CSFJP_SFKF_PY),(MAP_SFJP(1,51),        29
     *MSFJP_SFKF_PY)                                                      29
      CHARACTER CSFJP_SFKF_PY*40/' '/                                     29
      CHARACTER NSFJP_SFKF_PY*12/'R.SFKF_PY'/                             29
      INTEGER MSFJP_SFKF_PY(2)/1,1/                                       29
      REAL RE_SFJP_SFKF_PY                                                29
C -                                                                       29
      REAL SFJP_SFKS_DX                                                   29
      EQUIVALENCE (BANK_SFJP(54),SFJP_SFKS_DX,RE_SFJP_SFKS_DX),           29
     *(NAMES_SFJP(52),NSFJP_SFKS_DX)                                      29
      EQUIVALENCE (COMMEN_SFJP(52),CSFJP_SFKS_DX),(MAP_SFJP(1,52),        29
     *MSFJP_SFKS_DX)                                                      29
      CHARACTER CSFJP_SFKS_DX*40/' '/                                     29
      CHARACTER NSFJP_SFKS_DX*12/'R.SFKS_DX'/                             29
      INTEGER MSFJP_SFKS_DX(2)/1,1/                                       29
      REAL RE_SFJP_SFKS_DX                                                29
C -                                                                       29
      REAL SFJP_SFKS_PX                                                   29
      EQUIVALENCE (BANK_SFJP(55),SFJP_SFKS_PX,RE_SFJP_SFKS_PX),           29
     *(NAMES_SFJP(53),NSFJP_SFKS_PX)                                      29
      EQUIVALENCE (COMMEN_SFJP(53),CSFJP_SFKS_PX),(MAP_SFJP(1,53),        29
     *MSFJP_SFKS_PX)                                                      29
      CHARACTER CSFJP_SFKS_PX*40/' '/                                     29
      CHARACTER NSFJP_SFKS_PX*12/'R.SFKS_PX'/                             29
      INTEGER MSFJP_SFKS_PX(2)/1,1/                                       29
      REAL RE_SFJP_SFKS_PX                                                29
C -                                                                       29
      REAL SFJP_SFKS_PY                                                   29
      EQUIVALENCE (BANK_SFJP(56),SFJP_SFKS_PY,RE_SFJP_SFKS_PY),           29
     *(NAMES_SFJP(54),NSFJP_SFKS_PY)                                      29
      EQUIVALENCE (COMMEN_SFJP(54),CSFJP_SFKS_PY),(MAP_SFJP(1,54),        29
     *MSFJP_SFKS_PY)                                                      29
      CHARACTER CSFJP_SFKS_PY*40/' '/                                     29
      CHARACTER NSFJP_SFKS_PY*12/'R.SFKS_PY'/                             29
      INTEGER MSFJP_SFKS_PY(2)/1,1/                                       29
      REAL RE_SFJP_SFKS_PY                                                29
C -                                                                       29
      REAL SFJP_SFPR_PY                                                   29
      EQUIVALENCE (BANK_SFJP(57),SFJP_SFPR_PY,RE_SFJP_SFPR_PY),           29
     *(NAMES_SFJP(55),NSFJP_SFPR_PY)                                      29
      EQUIVALENCE (COMMEN_SFJP(55),CSFJP_SFPR_PY),(MAP_SFJP(1,55),        29
     *MSFJP_SFPR_PY)                                                      29
      CHARACTER CSFJP_SFPR_PY*40/' '/                                     29
      CHARACTER NSFJP_SFPR_PY*12/'R.SFPR_PY'/                             29
      INTEGER MSFJP_SFPR_PY(2)/1,1/                                       29
      REAL RE_SFJP_SFPR_PY                                                29
C -                                                                       29
      REAL SFJP_SFPB_PY                                                   29
      EQUIVALENCE (BANK_SFJP(58),SFJP_SFPB_PY,RE_SFJP_SFPB_PY),           29
     *(NAMES_SFJP(56),NSFJP_SFPB_PY)                                      29
      EQUIVALENCE (COMMEN_SFJP(56),CSFJP_SFPB_PY),(MAP_SFJP(1,56),        29
     *MSFJP_SFPB_PY)                                                      29
      CHARACTER CSFJP_SFPB_PY*40/' '/                                     29
      CHARACTER NSFJP_SFPB_PY*12/'R.SFPB_PY'/                             29
      INTEGER MSFJP_SFPB_PY(2)/1,1/                                       29
      REAL RE_SFJP_SFPB_PY                                                29
C -                                                                       29
      REAL SFJP_SFPB_PY2                                                  29
      EQUIVALENCE (BANK_SFJP(59),SFJP_SFPB_PY2,RE_SFJP_SFPB_PY2),         29
     *(NAMES_SFJP(57),NSFJP_SFPB_PY2)                                     29
      EQUIVALENCE (COMMEN_SFJP(57),CSFJP_SFPB_PY2),(MAP_SFJP(1,57),       29
     *MSFJP_SFPB_PY2)                                                     29
      CHARACTER CSFJP_SFPB_PY2*40/' '/                                    29
      CHARACTER NSFJP_SFPB_PY2*12/'R.SFPB_PY2'/                           29
      INTEGER MSFJP_SFPB_PY2(2)/1,1/                                      29
      REAL RE_SFJP_SFPB_PY2                                               29
C -                                                                       29
      REAL SFJP_SFCB_DX                                                   29
      EQUIVALENCE (BANK_SFJP(60),SFJP_SFCB_DX,RE_SFJP_SFCB_DX),           29
     *(NAMES_SFJP(58),NSFJP_SFCB_DX)                                      29
      EQUIVALENCE (COMMEN_SFJP(58),CSFJP_SFCB_DX),(MAP_SFJP(1,58),        29
     *MSFJP_SFCB_DX)                                                      29
      CHARACTER CSFJP_SFCB_DX*40/' '/                                     29
      CHARACTER NSFJP_SFCB_DX*12/'R.SFCB_DX'/                             29
      INTEGER MSFJP_SFCB_DX(2)/1,1/                                       29
      REAL RE_SFJP_SFCB_DX                                                29
C -                                                                       29
      REAL SFJP_SSBS_DY                                                   29
      EQUIVALENCE (BANK_SFJP(61),SFJP_SSBS_DY,RE_SFJP_SSBS_DY),           29
     *(NAMES_SFJP(59),NSFJP_SSBS_DY)                                      29
      EQUIVALENCE (COMMEN_SFJP(59),CSFJP_SSBS_DY),(MAP_SFJP(1,59),        29
     *MSFJP_SSBS_DY)                                                      29
      CHARACTER CSFJP_SSBS_DY*40/' '/                                     29
      CHARACTER NSFJP_SSBS_DY*12/'R.SSBS_DY'/                             29
      INTEGER MSFJP_SSBS_DY(2)/1,1/                                       29
      REAL RE_SFJP_SSBS_DY                                                29
C -                                                                       29
      REAL SFJP_SSBS_DX                                                   29
      EQUIVALENCE (BANK_SFJP(62),SFJP_SSBS_DX,RE_SFJP_SSBS_DX),           29
     *(NAMES_SFJP(60),NSFJP_SSBS_DX)                                      29
      EQUIVALENCE (COMMEN_SFJP(60),CSFJP_SSBS_DX),(MAP_SFJP(1,60),        29
     *MSFJP_SSBS_DX)                                                      29
      CHARACTER CSFJP_SSBS_DX*40/' '/                                     29
      CHARACTER NSFJP_SSBS_DX*12/'R.SSBS_DX'/                             29
      INTEGER MSFJP_SSBS_DX(2)/1,1/                                       29
      REAL RE_SFJP_SSBS_DX                                                29
C -                                                                       29
      REAL SFJP_SSBB_DX                                                   29
      EQUIVALENCE (BANK_SFJP(63),SFJP_SSBB_DX,RE_SFJP_SSBB_DX),           29
     *(NAMES_SFJP(61),NSFJP_SSBB_DX)                                      29
      EQUIVALENCE (COMMEN_SFJP(61),CSFJP_SSBB_DX),(MAP_SFJP(1,61),        29
     *MSFJP_SSBB_DX)                                                      29
      CHARACTER CSFJP_SSBB_DX*40/' '/                                     29
      CHARACTER NSFJP_SSBB_DX*12/'R.SSBB_DX'/                             29
      INTEGER MSFJP_SSBB_DX(2)/1,1/                                       29
      REAL RE_SFJP_SSBB_DX                                                29
C -                                                                       29
      REAL SFJP_SSBB_DZ                                                   29
      EQUIVALENCE (BANK_SFJP(64),SFJP_SSBB_DZ,RE_SFJP_SSBB_DZ),           29
     *(NAMES_SFJP(62),NSFJP_SSBB_DZ)                                      29
      EQUIVALENCE (COMMEN_SFJP(62),CSFJP_SSBB_DZ),(MAP_SFJP(1,62),        29
     *MSFJP_SSBB_DZ)                                                      29
      CHARACTER CSFJP_SSBB_DZ*40/' '/                                     29
      CHARACTER NSFJP_SSBB_DZ*12/'R.SSBB_DZ'/                             29
      INTEGER MSFJP_SSBB_DZ(2)/1,1/                                       29
      REAL RE_SFJP_SSBB_DZ                                                29
C -                                                                       29
      REAL SFJP_FLEX_DI                                                   29
      EQUIVALENCE (BANK_SFJP(65),SFJP_FLEX_DI,RE_SFJP_FLEX_DI),           29
     *(NAMES_SFJP(63),NSFJP_FLEX_DI)                                      29
      EQUIVALENCE (COMMEN_SFJP(63),CSFJP_FLEX_DI),(MAP_SFJP(1,63),        29
     *MSFJP_FLEX_DI)                                                      29
      CHARACTER CSFJP_FLEX_DI*40/' '/                                     29
      CHARACTER NSFJP_FLEX_DI*12/'R.FLEX_DI'/                             29
      INTEGER MSFJP_FLEX_DI(2)/1,1/                                       29
      REAL RE_SFJP_FLEX_DI                                                29
C -                                                                       29
      REAL SFJP_SFPB_DX                                                   29
      EQUIVALENCE (BANK_SFJP(66),SFJP_SFPB_DX,RE_SFJP_SFPB_DX),           29
     *(NAMES_SFJP(64),NSFJP_SFPB_DX)                                      29
      EQUIVALENCE (COMMEN_SFJP(64),CSFJP_SFPB_DX),(MAP_SFJP(1,64),        29
     *MSFJP_SFPB_DX)                                                      29
      CHARACTER CSFJP_SFPB_DX*40/' '/                                     29
      CHARACTER NSFJP_SFPB_DX*12/'R.SFPB_DX'/                             29
      INTEGER MSFJP_SFPB_DX(2)/1,1/                                       29
      REAL RE_SFJP_SFPB_DX                                                29
C -                                                                       29
      REAL SFJP_SFPB_DY                                                   29
      EQUIVALENCE (BANK_SFJP(67),SFJP_SFPB_DY,RE_SFJP_SFPB_DY),           29
     *(NAMES_SFJP(65),NSFJP_SFPB_DY)                                      29
      EQUIVALENCE (COMMEN_SFJP(65),CSFJP_SFPB_DY),(MAP_SFJP(1,65),        29
     *MSFJP_SFPB_DY)                                                      29
      CHARACTER CSFJP_SFPB_DY*40/' '/                                     29
      CHARACTER NSFJP_SFPB_DY*12/'R.SFPB_DY'/                             29
      INTEGER MSFJP_SFPB_DY(2)/1,1/                                       29
      REAL RE_SFJP_SFPB_DY                                                29
C -                                                                       29
      REAL SFJP_SFPBDZ                                                    29
      EQUIVALENCE (BANK_SFJP(68),SFJP_SFPBDZ,RE_SFJP_SFPBDZ),             29
     *(NAMES_SFJP(66),NSFJP_SFPBDZ)                                       29
      EQUIVALENCE (COMMEN_SFJP(66),CSFJP_SFPBDZ),(MAP_SFJP(1,66),         29
     *MSFJP_SFPBDZ)                                                       29
      CHARACTER CSFJP_SFPBDZ*40/' '/                                      29
      CHARACTER NSFJP_SFPBDZ*12/'R.SFPBDZ'/                               29
      INTEGER MSFJP_SFPBDZ(2)/1,1/                                        29
      REAL RE_SFJP_SFPBDZ                                                 29
C -                                                                       29
      REAL SFJP_SAPP_DXE                                                  29
      EQUIVALENCE (BANK_SFJP(69),SFJP_SAPP_DXE,RE_SFJP_SAPP_DXE),         29
     *(NAMES_SFJP(67),NSFJP_SAPP_DXE)                                     29
      EQUIVALENCE (COMMEN_SFJP(67),CSFJP_SAPP_DXE),(MAP_SFJP(1,67),       29
     *MSFJP_SAPP_DXE)                                                     29
      CHARACTER CSFJP_SAPP_DXE*40/' '/                                    29
      CHARACTER NSFJP_SAPP_DXE*12/'R.SAPP_DXE'/                           29
      INTEGER MSFJP_SAPP_DXE(2)/1,1/                                      29
      REAL RE_SFJP_SAPP_DXE                                               29
C -                                                                       29
      REAL SFJP_SAPP_DXZ                                                  29
      EQUIVALENCE (BANK_SFJP(70),SFJP_SAPP_DXZ,RE_SFJP_SAPP_DXZ),         29
     *(NAMES_SFJP(68),NSFJP_SAPP_DXZ)                                     29
      EQUIVALENCE (COMMEN_SFJP(68),CSFJP_SAPP_DXZ),(MAP_SFJP(1,68),       29
     *MSFJP_SAPP_DXZ)                                                     29
      CHARACTER CSFJP_SAPP_DXZ*40/' '/                                    29
      CHARACTER NSFJP_SAPP_DXZ*12/'R.SAPP_DXZ'/                           29
      INTEGER MSFJP_SAPP_DXZ(2)/1,1/                                      29
      REAL RE_SFJP_SAPP_DXZ                                               29
C -                                                                       29
      REAL SFJP_SAPP_DY                                                   29
      EQUIVALENCE (BANK_SFJP(71),SFJP_SAPP_DY,RE_SFJP_SAPP_DY),           29
     *(NAMES_SFJP(69),NSFJP_SAPP_DY)                                      29
      EQUIVALENCE (COMMEN_SFJP(69),CSFJP_SAPP_DY),(MAP_SFJP(1,69),        29
     *MSFJP_SAPP_DY)                                                      29
      CHARACTER CSFJP_SAPP_DY*40/' '/                                     29
      CHARACTER NSFJP_SAPP_DY*12/'R.SAPP_DY'/                             29
      INTEGER MSFJP_SAPP_DY(2)/1,1/                                       29
      REAL RE_SFJP_SAPP_DY                                                29
C -                                                                       29
      REAL SFJP_SAPP_DZ                                                   29
      EQUIVALENCE (BANK_SFJP(72),SFJP_SAPP_DZ,RE_SFJP_SAPP_DZ),           29
     *(NAMES_SFJP(70),NSFJP_SAPP_DZ)                                      29
      EQUIVALENCE (COMMEN_SFJP(70),CSFJP_SAPP_DZ),(MAP_SFJP(1,70),        29
     *MSFJP_SAPP_DZ)                                                      29
      CHARACTER CSFJP_SAPP_DZ*40/' '/                                     29
      CHARACTER NSFJP_SAPP_DZ*12/'R.SAPP_DZ'/                             29
      INTEGER MSFJP_SAPP_DZ(2)/1,1/                                       29
      REAL RE_SFJP_SAPP_DZ                                                29
C -                                                                       29
      REAL SFJP_SAPP_PY1                                                  29
      EQUIVALENCE (BANK_SFJP(73),SFJP_SAPP_PY1,RE_SFJP_SAPP_PY1),         29
     *(NAMES_SFJP(71),NSFJP_SAPP_PY1)                                     29
      EQUIVALENCE (COMMEN_SFJP(71),CSFJP_SAPP_PY1),(MAP_SFJP(1,71),       29
     *MSFJP_SAPP_PY1)                                                     29
      CHARACTER CSFJP_SAPP_PY1*40/' '/                                    29
      CHARACTER NSFJP_SAPP_PY1*12/'R.SAPP_PY1'/                           29
      INTEGER MSFJP_SAPP_PY1(2)/1,1/                                      29
      REAL RE_SFJP_SAPP_PY1                                               29
C -                                                                       29
      REAL SFJP_SAPP_PY2                                                  29
      EQUIVALENCE (BANK_SFJP(74),SFJP_SAPP_PY2,RE_SFJP_SAPP_PY2),         29
     *(NAMES_SFJP(72),NSFJP_SAPP_PY2)                                     29
      EQUIVALENCE (COMMEN_SFJP(72),CSFJP_SAPP_PY2),(MAP_SFJP(1,72),       29
     *MSFJP_SAPP_PY2)                                                     29
      CHARACTER CSFJP_SAPP_PY2*40/' '/                                    29
      CHARACTER NSFJP_SAPP_PY2*12/'R.SAPP_PY2'/                           29
      INTEGER MSFJP_SAPP_PY2(2)/1,1/                                      29
      REAL RE_SFJP_SAPP_PY2                                               29
C -                                                                       29
      REAL SFJP_SAPP_PY3                                                  29
      EQUIVALENCE (BANK_SFJP(75),SFJP_SAPP_PY3,RE_SFJP_SAPP_PY3),         29
     *(NAMES_SFJP(73),NSFJP_SAPP_PY3)                                     29
      EQUIVALENCE (COMMEN_SFJP(73),CSFJP_SAPP_PY3),(MAP_SFJP(1,73),       29
     *MSFJP_SAPP_PY3)                                                     29
      CHARACTER CSFJP_SAPP_PY3*40/' '/                                    29
      CHARACTER NSFJP_SAPP_PY3*12/'R.SAPP_PY3'/                           29
      INTEGER MSFJP_SAPP_PY3(2)/1,1/                                      29
      REAL RE_SFJP_SAPP_PY3                                               29
C -                                                                       29
      REAL SFJP_SFAM_DXE                                                  29
      EQUIVALENCE (BANK_SFJP(76),SFJP_SFAM_DXE,RE_SFJP_SFAM_DXE),         29
     *(NAMES_SFJP(74),NSFJP_SFAM_DXE)                                     29
      EQUIVALENCE (COMMEN_SFJP(74),CSFJP_SFAM_DXE),(MAP_SFJP(1,74),       29
     *MSFJP_SFAM_DXE)                                                     29
      CHARACTER CSFJP_SFAM_DXE*40/' '/                                    29
      CHARACTER NSFJP_SFAM_DXE*12/'R.SFAM_DXE'/                           29
      INTEGER MSFJP_SFAM_DXE(2)/1,1/                                      29
      REAL RE_SFJP_SFAM_DXE                                               29
C -                                                                       29
      REAL SFJP_SFAM_DXZ                                                  29
      EQUIVALENCE (BANK_SFJP(77),SFJP_SFAM_DXZ,RE_SFJP_SFAM_DXZ),         29
     *(NAMES_SFJP(75),NSFJP_SFAM_DXZ)                                     29
      EQUIVALENCE (COMMEN_SFJP(75),CSFJP_SFAM_DXZ),(MAP_SFJP(1,75),       29
     *MSFJP_SFAM_DXZ)                                                     29
      CHARACTER CSFJP_SFAM_DXZ*40/' '/                                    29
      CHARACTER NSFJP_SFAM_DXZ*12/'R.SFAM_DXZ'/                           29
      INTEGER MSFJP_SFAM_DXZ(2)/1,1/                                      29
      REAL RE_SFJP_SFAM_DXZ                                               29
C -                                                                       29
      REAL SFJP_SFAM_DY                                                   29
      EQUIVALENCE (BANK_SFJP(78),SFJP_SFAM_DY,RE_SFJP_SFAM_DY),           29
     *(NAMES_SFJP(76),NSFJP_SFAM_DY)                                      29
      EQUIVALENCE (COMMEN_SFJP(76),CSFJP_SFAM_DY),(MAP_SFJP(1,76),        29
     *MSFJP_SFAM_DY)                                                      29
      CHARACTER CSFJP_SFAM_DY*40/' '/                                     29
      CHARACTER NSFJP_SFAM_DY*12/'R.SFAM_DY'/                             29
      INTEGER MSFJP_SFAM_DY(2)/1,1/                                       29
      REAL RE_SFJP_SFAM_DY                                                29
C -                                                                       29
      REAL SFJP_SFAM_DZ                                                   29
      EQUIVALENCE (BANK_SFJP(79),SFJP_SFAM_DZ,RE_SFJP_SFAM_DZ),           29
     *(NAMES_SFJP(77),NSFJP_SFAM_DZ)                                      29
      EQUIVALENCE (COMMEN_SFJP(77),CSFJP_SFAM_DZ),(MAP_SFJP(1,77),        29
     *MSFJP_SFAM_DZ)                                                      29
      CHARACTER CSFJP_SFAM_DZ*40/' '/                                     29
      CHARACTER NSFJP_SFAM_DZ*12/'R.SFAM_DZ'/                             29
      INTEGER MSFJP_SFAM_DZ(2)/1,1/                                       29
      REAL RE_SFJP_SFAM_DZ                                                29
C -                                                                       29
      REAL SFJP_SFAM_DZS                                                  29
      EQUIVALENCE (BANK_SFJP(80),SFJP_SFAM_DZS,RE_SFJP_SFAM_DZS),         29
     *(NAMES_SFJP(78),NSFJP_SFAM_DZS)                                     29
      EQUIVALENCE (COMMEN_SFJP(78),CSFJP_SFAM_DZS),(MAP_SFJP(1,78),       29
     *MSFJP_SFAM_DZS)                                                     29
      CHARACTER CSFJP_SFAM_DZS*40/' '/                                    29
      CHARACTER NSFJP_SFAM_DZS*12/'R.SFAM_DZS'/                           29
      INTEGER MSFJP_SFAM_DZS(2)/1,1/                                      29
      REAL RE_SFJP_SFAM_DZS                                               29
C -                                                                       29
      REAL SFJP_SFLA_DX                                                   29
      EQUIVALENCE (BANK_SFJP(81),SFJP_SFLA_DX,RE_SFJP_SFLA_DX),           29
     *(NAMES_SFJP(79),NSFJP_SFLA_DX)                                      29
      EQUIVALENCE (COMMEN_SFJP(79),CSFJP_SFLA_DX),(MAP_SFJP(1,79),        29
     *MSFJP_SFLA_DX)                                                      29
      CHARACTER CSFJP_SFLA_DX*40/' '/                                     29
      CHARACTER NSFJP_SFLA_DX*12/'R.SFLA_DX'/                             29
      INTEGER MSFJP_SFLA_DX(2)/1,1/                                       29
      REAL RE_SFJP_SFLA_DX                                                29
C -                                                                       29
      REAL SFJP_SFLA_DY                                                   29
      EQUIVALENCE (BANK_SFJP(82),SFJP_SFLA_DY,RE_SFJP_SFLA_DY),           29
     *(NAMES_SFJP(80),NSFJP_SFLA_DY)                                      29
      EQUIVALENCE (COMMEN_SFJP(80),CSFJP_SFLA_DY),(MAP_SFJP(1,80),        29
     *MSFJP_SFLA_DY)                                                      29
      CHARACTER CSFJP_SFLA_DY*40/' '/                                     29
      CHARACTER NSFJP_SFLA_DY*12/'R.SFLA_DY'/                             29
      INTEGER MSFJP_SFLA_DY(2)/1,1/                                       29
      REAL RE_SFJP_SFLA_DY                                                29
C -                                                                       29
      REAL SFJP_SFLA_DZ                                                   29
      EQUIVALENCE (BANK_SFJP(83),SFJP_SFLA_DZ,RE_SFJP_SFLA_DZ),           29
     *(NAMES_SFJP(81),NSFJP_SFLA_DZ)                                      29
      EQUIVALENCE (COMMEN_SFJP(81),CSFJP_SFLA_DZ),(MAP_SFJP(1,81),        29
     *MSFJP_SFLA_DZ)                                                      29
      CHARACTER CSFJP_SFLA_DZ*40/' '/                                     29
      CHARACTER NSFJP_SFLA_DZ*12/'R.SFLA_DZ'/                             29
      INTEGER MSFJP_SFLA_DZ(2)/1,1/                                       29
      REAL RE_SFJP_SFLA_DZ                                                29
C -                                                                       29
      REAL SFJP_SFLB_DZ                                                   29
      EQUIVALENCE (BANK_SFJP(84),SFJP_SFLB_DZ,RE_SFJP_SFLB_DZ),           29
     *(NAMES_SFJP(82),NSFJP_SFLB_DZ)                                      29
      EQUIVALENCE (COMMEN_SFJP(82),CSFJP_SFLB_DZ),(MAP_SFJP(1,82),        29
     *MSFJP_SFLB_DZ)                                                      29
      CHARACTER CSFJP_SFLB_DZ*40/' '/                                     29
      CHARACTER NSFJP_SFLB_DZ*12/'R.SFLB_DZ'/                             29
      INTEGER MSFJP_SFLB_DZ(2)/1,1/                                       29
      REAL RE_SFJP_SFLB_DZ                                                29
C -                                                                       29
      REAL SFJP_SFLC_DZ                                                   29
      EQUIVALENCE (BANK_SFJP(85),SFJP_SFLC_DZ,RE_SFJP_SFLC_DZ),           29
     *(NAMES_SFJP(83),NSFJP_SFLC_DZ)                                      29
      EQUIVALENCE (COMMEN_SFJP(83),CSFJP_SFLC_DZ),(MAP_SFJP(1,83),        29
     *MSFJP_SFLC_DZ)                                                      29
      CHARACTER CSFJP_SFLC_DZ*40/' '/                                     29
      CHARACTER NSFJP_SFLC_DZ*12/'R.SFLC_DZ'/                             29
      INTEGER MSFJP_SFLC_DZ(2)/1,1/                                       29
      REAL RE_SFJP_SFLC_DZ                                                29
C -                                                                       29
      REAL SFJP_SFEB_DX                                                   29
      EQUIVALENCE (BANK_SFJP(86),SFJP_SFEB_DX,RE_SFJP_SFEB_DX),           29
     *(NAMES_SFJP(84),NSFJP_SFEB_DX)                                      29
      EQUIVALENCE (COMMEN_SFJP(84),CSFJP_SFEB_DX),(MAP_SFJP(1,84),        29
     *MSFJP_SFEB_DX)                                                      29
      CHARACTER CSFJP_SFEB_DX*40/' '/                                     29
      CHARACTER NSFJP_SFEB_DX*12/'R.SFEB_DX'/                             29
      INTEGER MSFJP_SFEB_DX(2)/1,1/                                       29
      REAL RE_SFJP_SFEB_DX                                                29
C -                                                                       29
      REAL SFJP_SFEB_DZ                                                   29
      EQUIVALENCE (BANK_SFJP(87),SFJP_SFEB_DZ,RE_SFJP_SFEB_DZ),           29
     *(NAMES_SFJP(85),NSFJP_SFEB_DZ)                                      29
      EQUIVALENCE (COMMEN_SFJP(85),CSFJP_SFEB_DZ),(MAP_SFJP(1,85),        29
     *MSFJP_SFEB_DZ)                                                      29
      CHARACTER CSFJP_SFEB_DZ*40/' '/                                     29
      CHARACTER NSFJP_SFEB_DZ*12/'R.SFEB_DZ'/                             29
      INTEGER MSFJP_SFEB_DZ(2)/1,1/                                       29
      REAL RE_SFJP_SFEB_DZ                                                29
C -                                                                       29
      REAL SFJP_SFES_DX                                                   29
      EQUIVALENCE (BANK_SFJP(88),SFJP_SFES_DX,RE_SFJP_SFES_DX),           29
     *(NAMES_SFJP(86),NSFJP_SFES_DX)                                      29
      EQUIVALENCE (COMMEN_SFJP(86),CSFJP_SFES_DX),(MAP_SFJP(1,86),        29
     *MSFJP_SFES_DX)                                                      29
      CHARACTER CSFJP_SFES_DX*40/' '/                                     29
      CHARACTER NSFJP_SFES_DX*12/'R.SFES_DX'/                             29
      INTEGER MSFJP_SFES_DX(2)/1,1/                                       29
      REAL RE_SFJP_SFES_DX                                                29
C -                                                                       29
      REAL SFJP_SFFK_DXE                                                  29
      EQUIVALENCE (BANK_SFJP(89),SFJP_SFFK_DXE,RE_SFJP_SFFK_DXE),         29
     *(NAMES_SFJP(87),NSFJP_SFFK_DXE)                                     29
      EQUIVALENCE (COMMEN_SFJP(87),CSFJP_SFFK_DXE),(MAP_SFJP(1,87),       29
     *MSFJP_SFFK_DXE)                                                     29
      CHARACTER CSFJP_SFFK_DXE*40/' '/                                    29
      CHARACTER NSFJP_SFFK_DXE*12/'R.SFFK_DXE'/                           29
      INTEGER MSFJP_SFFK_DXE(2)/1,1/                                      29
      REAL RE_SFJP_SFFK_DXE                                               29
C -                                                                       29
      REAL SFJP_SFFK_DY                                                   29
      EQUIVALENCE (BANK_SFJP(90),SFJP_SFFK_DY,RE_SFJP_SFFK_DY),           29
     *(NAMES_SFJP(88),NSFJP_SFFK_DY)                                      29
      EQUIVALENCE (COMMEN_SFJP(88),CSFJP_SFFK_DY),(MAP_SFJP(1,88),        29
     *MSFJP_SFFK_DY)                                                      29
      CHARACTER CSFJP_SFFK_DY*40/' '/                                     29
      CHARACTER NSFJP_SFFK_DY*12/'R.SFFK_DY'/                             29
      INTEGER MSFJP_SFFK_DY(2)/1,1/                                       29
      REAL RE_SFJP_SFFK_DY                                                29
C -                                                                       29
      REAL SFJP_SFFX_DYE                                                  29
      EQUIVALENCE (BANK_SFJP(91),SFJP_SFFX_DYE,RE_SFJP_SFFX_DYE),         29
     *(NAMES_SFJP(89),NSFJP_SFFX_DYE)                                     29
      EQUIVALENCE (COMMEN_SFJP(89),CSFJP_SFFX_DYE),(MAP_SFJP(1,89),       29
     *MSFJP_SFFX_DYE)                                                     29
      CHARACTER CSFJP_SFFX_DYE*40/' '/                                    29
      CHARACTER NSFJP_SFFX_DYE*12/'R.SFFX_DYE'/                           29
      INTEGER MSFJP_SFFX_DYE(2)/1,1/                                      29
      REAL RE_SFJP_SFFX_DYE                                               29
C -                                                                       29
      REAL SFJP_SFFK_DZ                                                   29
      EQUIVALENCE (BANK_SFJP(92),SFJP_SFFK_DZ,RE_SFJP_SFFK_DZ),           29
     *(NAMES_SFJP(90),NSFJP_SFFK_DZ)                                      29
      EQUIVALENCE (COMMEN_SFJP(90),CSFJP_SFFK_DZ),(MAP_SFJP(1,90),        29
     *MSFJP_SFFK_DZ)                                                      29
      CHARACTER CSFJP_SFFK_DZ*40/' '/                                     29
      CHARACTER NSFJP_SFFK_DZ*12/'R.SFFK_DZ'/                             29
      INTEGER MSFJP_SFFK_DZ(2)/1,1/                                       29
      REAL RE_SFJP_SFFK_DZ                                                29
C -                                                                       29
      REAL SFJP_SFFL_DX                                                   29
      EQUIVALENCE (BANK_SFJP(93),SFJP_SFFL_DX,RE_SFJP_SFFL_DX),           29
     *(NAMES_SFJP(91),NSFJP_SFFL_DX)                                      29
      EQUIVALENCE (COMMEN_SFJP(91),CSFJP_SFFL_DX),(MAP_SFJP(1,91),        29
     *MSFJP_SFFL_DX)                                                      29
      CHARACTER CSFJP_SFFL_DX*40/' '/                                     29
      CHARACTER NSFJP_SFFL_DX*12/'R.SFFL_DX'/                             29
      INTEGER MSFJP_SFFL_DX(2)/1,1/                                       29
      REAL RE_SFJP_SFFL_DX                                                29
C -                                                                       29
      REAL SFJP_SFFK_DXZ                                                  29
      EQUIVALENCE (BANK_SFJP(94),SFJP_SFFK_DXZ,RE_SFJP_SFFK_DXZ),         29
     *(NAMES_SFJP(92),NSFJP_SFFK_DXZ)                                     29
      EQUIVALENCE (COMMEN_SFJP(92),CSFJP_SFFK_DXZ),(MAP_SFJP(1,92),       29
     *MSFJP_SFFK_DXZ)                                                     29
      CHARACTER CSFJP_SFFK_DXZ*40/' '/                                    29
      CHARACTER NSFJP_SFFK_DXZ*12/'R.SFFK_DXZ'/                           29
      INTEGER MSFJP_SFFK_DXZ(2)/1,1/                                      29
      REAL RE_SFJP_SFFK_DXZ                                               29
C -                                                                       29
      REAL SFJP_SFFK_PX                                                   29
      EQUIVALENCE (BANK_SFJP(95),SFJP_SFFK_PX,RE_SFJP_SFFK_PX),           29
     *(NAMES_SFJP(93),NSFJP_SFFK_PX)                                      29
      EQUIVALENCE (COMMEN_SFJP(93),CSFJP_SFFK_PX),(MAP_SFJP(1,93),        29
     *MSFJP_SFFK_PX)                                                      29
      CHARACTER CSFJP_SFFK_PX*40/' '/                                     29
      CHARACTER NSFJP_SFFK_PX*12/'R.SFFK_PX'/                             29
      INTEGER MSFJP_SFFK_PX(2)/1,1/                                       29
      REAL RE_SFJP_SFFK_PX                                                29
C -                                                                       29
      REAL SFJP_SFFK_PY1                                                  29
      EQUIVALENCE (BANK_SFJP(96),SFJP_SFFK_PY1,RE_SFJP_SFFK_PY1),         29
     *(NAMES_SFJP(94),NSFJP_SFFK_PY1)                                     29
      EQUIVALENCE (COMMEN_SFJP(94),CSFJP_SFFK_PY1),(MAP_SFJP(1,94),       29
     *MSFJP_SFFK_PY1)                                                     29
      CHARACTER CSFJP_SFFK_PY1*40/' '/                                    29
      CHARACTER NSFJP_SFFK_PY1*12/'R.SFFK_PY1'/                           29
      INTEGER MSFJP_SFFK_PY1(2)/1,1/                                      29
      REAL RE_SFJP_SFFK_PY1                                               29
C -                                                                       29
      REAL SFJP_SFFK_PY2                                                  29
      EQUIVALENCE (BANK_SFJP(97),SFJP_SFFK_PY2,RE_SFJP_SFFK_PY2),         29
     *(NAMES_SFJP(95),NSFJP_SFFK_PY2)                                     29
      EQUIVALENCE (COMMEN_SFJP(95),CSFJP_SFFK_PY2),(MAP_SFJP(1,95),       29
     *MSFJP_SFFK_PY2)                                                     29
      CHARACTER CSFJP_SFFK_PY2*40/' '/                                    29
      CHARACTER NSFJP_SFFK_PY2*12/'R.SFFK_PY2'/                           29
      INTEGER MSFJP_SFFK_PY2(2)/1,1/                                      29
      REAL RE_SFJP_SFFK_PY2                                               29
C -                                                                       29
      REAL SFJP_SFKL_PX                                                   29
      EQUIVALENCE (BANK_SFJP(98),SFJP_SFKL_PX,RE_SFJP_SFKL_PX),           29
     *(NAMES_SFJP(96),NSFJP_SFKL_PX)                                      29
      EQUIVALENCE (COMMEN_SFJP(96),CSFJP_SFKL_PX),(MAP_SFJP(1,96),        29
     *MSFJP_SFKL_PX)                                                      29
      CHARACTER CSFJP_SFKL_PX*40/' '/                                     29
      CHARACTER NSFJP_SFKL_PX*12/'R.SFKL_PX'/                             29
      INTEGER MSFJP_SFKL_PX(2)/1,1/                                       29
      REAL RE_SFJP_SFKL_PX                                                29
C -                                                                       29
      REAL SFJP_SFKK_DY                                                   29
      EQUIVALENCE (BANK_SFJP(99),SFJP_SFKK_DY,RE_SFJP_SFKK_DY),           29
     *(NAMES_SFJP(97),NSFJP_SFKK_DY)                                      29
      EQUIVALENCE (COMMEN_SFJP(97),CSFJP_SFKK_DY),(MAP_SFJP(1,97),        29
     *MSFJP_SFKK_DY)                                                      29
      CHARACTER CSFJP_SFKK_DY*40/' '/                                     29
      CHARACTER NSFJP_SFKK_DY*12/'R.SFKK_DY'/                             29
      INTEGER MSFJP_SFKK_DY(2)/1,1/                                       29
      REAL RE_SFJP_SFKK_DY                                                29
C -                                                                       29
      REAL SFJP_SFLU_DZ                                                   29
      EQUIVALENCE (BANK_SFJP(100),SFJP_SFLU_DZ,RE_SFJP_SFLU_DZ),          29
     *(NAMES_SFJP(98),NSFJP_SFLU_DZ)                                      29
      EQUIVALENCE (COMMEN_SFJP(98),CSFJP_SFLU_DZ),(MAP_SFJP(1,98),        29
     *MSFJP_SFLU_DZ)                                                      29
      CHARACTER CSFJP_SFLU_DZ*40/' '/                                     29
      CHARACTER NSFJP_SFLU_DZ*12/'R.SFLU_DZ'/                             29
      INTEGER MSFJP_SFLU_DZ(2)/1,1/                                       29
      REAL RE_SFJP_SFLU_DZ                                                29
C -                                                                       29
      REAL SFJP_SFLU_H1                                                   29
      EQUIVALENCE (BANK_SFJP(101),SFJP_SFLU_H1,RE_SFJP_SFLU_H1),          29
     *(NAMES_SFJP(99),NSFJP_SFLU_H1)                                      29
      EQUIVALENCE (COMMEN_SFJP(99),CSFJP_SFLU_H1),(MAP_SFJP(1,99),        29
     *MSFJP_SFLU_H1)                                                      29
      CHARACTER CSFJP_SFLU_H1*40/' '/                                     29
      CHARACTER NSFJP_SFLU_H1*12/'R.SFLU_H1'/                             29
      INTEGER MSFJP_SFLU_H1(2)/1,1/                                       29
      REAL RE_SFJP_SFLU_H1                                                29
C -                                                                       29
      REAL SFJP_SFLU_BL1                                                  29
      EQUIVALENCE (BANK_SFJP(102),SFJP_SFLU_BL1,RE_SFJP_SFLU_BL1),        29
     *(NAMES_SFJP(100),NSFJP_SFLU_BL1)                                    29
      EQUIVALENCE (COMMEN_SFJP(100),CSFJP_SFLU_BL1),(MAP_SFJP(1,100),     29
     *MSFJP_SFLU_BL1)                                                     29
      CHARACTER CSFJP_SFLU_BL1*40/' '/                                    29
      CHARACTER NSFJP_SFLU_BL1*12/'R.SFLU_BL1'/                           29
      INTEGER MSFJP_SFLU_BL1(2)/1,1/                                      29
      REAL RE_SFJP_SFLU_BL1                                               29
C -                                                                       29
      REAL SFJP_SFRA_DX                                                   29
      EQUIVALENCE (BANK_SFJP(103),SFJP_SFRA_DX,RE_SFJP_SFRA_DX),          29
     *(NAMES_SFJP(101),NSFJP_SFRA_DX)                                     29
      EQUIVALENCE (COMMEN_SFJP(101),CSFJP_SFRA_DX),(MAP_SFJP(1,101),      29
     *MSFJP_SFRA_DX)                                                      29
      CHARACTER CSFJP_SFRA_DX*40/' '/                                     29
      CHARACTER NSFJP_SFRA_DX*12/'R.SFRA_DX'/                             29
      INTEGER MSFJP_SFRA_DX(2)/1,1/                                       29
      REAL RE_SFJP_SFRA_DX                                                29
C -                                                                       29
      REAL SFJP_SFRA_DY                                                   29
      EQUIVALENCE (BANK_SFJP(104),SFJP_SFRA_DY,RE_SFJP_SFRA_DY),          29
     *(NAMES_SFJP(102),NSFJP_SFRA_DY)                                     29
      EQUIVALENCE (COMMEN_SFJP(102),CSFJP_SFRA_DY),(MAP_SFJP(1,102),      29
     *MSFJP_SFRA_DY)                                                      29
      CHARACTER CSFJP_SFRA_DY*40/' '/                                     29
      CHARACTER NSFJP_SFRA_DY*12/'R.SFRA_DY'/                             29
      INTEGER MSFJP_SFRA_DY(2)/1,1/                                       29
      REAL RE_SFJP_SFRA_DY                                                29
C -                                                                       29
      REAL SFJP_SFRA_PY                                                   29
      EQUIVALENCE (BANK_SFJP(105),SFJP_SFRA_PY,RE_SFJP_SFRA_PY),          29
     *(NAMES_SFJP(103),NSFJP_SFRA_PY)                                     29
      EQUIVALENCE (COMMEN_SFJP(103),CSFJP_SFRA_PY),(MAP_SFJP(1,103),      29
     *MSFJP_SFRA_PY)                                                      29
      CHARACTER CSFJP_SFRA_PY*40/' '/                                     29
      CHARACTER NSFJP_SFRA_PY*12/'R.SFRA_PY'/                             29
      INTEGER MSFJP_SFRA_PY(2)/1,1/                                       29
      REAL RE_SFJP_SFRA_PY                                                29
C -                                                                       29
      REAL SFJP_SFRA_DZ                                                   29
      EQUIVALENCE (BANK_SFJP(106),SFJP_SFRA_DZ,RE_SFJP_SFRA_DZ),          29
     *(NAMES_SFJP(104),NSFJP_SFRA_DZ)                                     29
      EQUIVALENCE (COMMEN_SFJP(104),CSFJP_SFRA_DZ),(MAP_SFJP(1,104),      29
     *MSFJP_SFRA_DZ)                                                      29
      CHARACTER CSFJP_SFRA_DZ*40/' '/                                     29
      CHARACTER NSFJP_SFRA_DZ*12/'R.SFRA_DZ'/                             29
      INTEGER MSFJP_SFRA_DZ(2)/1,1/                                       29
      REAL RE_SFJP_SFRA_DZ                                                29
C -                                                                       29
      REAL SFJP_SFRA_PZ                                                   29
      EQUIVALENCE (BANK_SFJP(107),SFJP_SFRA_PZ,RE_SFJP_SFRA_PZ),          29
     *(NAMES_SFJP(105),NSFJP_SFRA_PZ)                                     29
      EQUIVALENCE (COMMEN_SFJP(105),CSFJP_SFRA_PZ),(MAP_SFJP(1,105),      29
     *MSFJP_SFRA_PZ)                                                      29
      CHARACTER CSFJP_SFRA_PZ*40/' '/                                     29
      CHARACTER NSFJP_SFRA_PZ*12/'R.SFRA_PZ'/                             29
      INTEGER MSFJP_SFRA_PZ(2)/1,1/                                       29
      REAL RE_SFJP_SFRA_PZ                                                29
C -                                                                       29
      REAL SFJP_SFSW_DY                                                   29
      EQUIVALENCE (BANK_SFJP(108),SFJP_SFSW_DY,RE_SFJP_SFSW_DY),          29
     *(NAMES_SFJP(106),NSFJP_SFSW_DY)                                     29
      EQUIVALENCE (COMMEN_SFJP(106),CSFJP_SFSW_DY),(MAP_SFJP(1,106),      29
     *MSFJP_SFSW_DY)                                                      29
      CHARACTER CSFJP_SFSW_DY*40/' '/                                     29
      CHARACTER NSFJP_SFSW_DY*12/'R.SFSW_DY'/                             29
      INTEGER MSFJP_SFSW_DY(2)/1,1/                                       29
      REAL RE_SFJP_SFSW_DY                                                29
C -                                                                       29
      REAL SFJP_SFSW_DZ                                                   29
      EQUIVALENCE (BANK_SFJP(109),SFJP_SFSW_DZ,RE_SFJP_SFSW_DZ),          29
     *(NAMES_SFJP(107),NSFJP_SFSW_DZ)                                     29
      EQUIVALENCE (COMMEN_SFJP(107),CSFJP_SFSW_DZ),(MAP_SFJP(1,107),      29
     *MSFJP_SFSW_DZ)                                                      29
      CHARACTER CSFJP_SFSW_DZ*40/' '/                                     29
      CHARACTER NSFJP_SFSW_DZ*12/'R.SFSW_DZ'/                             29
      INTEGER MSFJP_SFSW_DZ(2)/1,1/                                       29
      REAL RE_SFJP_SFSW_DZ                                                29
C -                                                                       29
      REAL SFJP_SFRS_DX                                                   29
      EQUIVALENCE (BANK_SFJP(110),SFJP_SFRS_DX,RE_SFJP_SFRS_DX),          29
     *(NAMES_SFJP(108),NSFJP_SFRS_DX)                                     29
      EQUIVALENCE (COMMEN_SFJP(108),CSFJP_SFRS_DX),(MAP_SFJP(1,108),      29
     *MSFJP_SFRS_DX)                                                      29
      CHARACTER CSFJP_SFRS_DX*40/' '/                                     29
      CHARACTER NSFJP_SFRS_DX*12/'R.SFRS_DX'/                             29
      INTEGER MSFJP_SFRS_DX(2)/1,1/                                       29
      REAL RE_SFJP_SFRS_DX                                                29
C -                                                                       29
      REAL SFJP_SFRS_DY                                                   29
      EQUIVALENCE (BANK_SFJP(111),SFJP_SFRS_DY,RE_SFJP_SFRS_DY),          29
     *(NAMES_SFJP(109),NSFJP_SFRS_DY)                                     29
      EQUIVALENCE (COMMEN_SFJP(109),CSFJP_SFRS_DY),(MAP_SFJP(1,109),      29
     *MSFJP_SFRS_DY)                                                      29
      CHARACTER CSFJP_SFRS_DY*40/' '/                                     29
      CHARACTER NSFJP_SFRS_DY*12/'R.SFRS_DY'/                             29
      INTEGER MSFJP_SFRS_DY(2)/1,1/                                       29
      REAL RE_SFJP_SFRS_DY                                                29
C -                                                                       29
      REAL SFJP_SFRS_PX                                                   29
      EQUIVALENCE (BANK_SFJP(112),SFJP_SFRS_PX,RE_SFJP_SFRS_PX),          29
     *(NAMES_SFJP(110),NSFJP_SFRS_PX)                                     29
      EQUIVALENCE (COMMEN_SFJP(110),CSFJP_SFRS_PX),(MAP_SFJP(1,110),      29
     *MSFJP_SFRS_PX)                                                      29
      CHARACTER CSFJP_SFRS_PX*40/' '/                                     29
      CHARACTER NSFJP_SFRS_PX*12/'R.SFRS_PX'/                             29
      INTEGER MSFJP_SFRS_PX(2)/1,1/                                       29
      REAL RE_SFJP_SFRS_PX                                                29
C -                                                                       29
      REAL SFJP_SFRS_PY                                                   29
      EQUIVALENCE (BANK_SFJP(113),SFJP_SFRS_PY,RE_SFJP_SFRS_PY),          29
     *(NAMES_SFJP(111),NSFJP_SFRS_PY)                                     29
      EQUIVALENCE (COMMEN_SFJP(111),CSFJP_SFRS_PY),(MAP_SFJP(1,111),      29
     *MSFJP_SFRS_PY)                                                      29
      CHARACTER CSFJP_SFRS_PY*40/' '/                                     29
      CHARACTER NSFJP_SFRS_PY*12/'R.SFRS_PY'/                             29
      INTEGER MSFJP_SFRS_PY(2)/1,1/                                       29
      REAL RE_SFJP_SFRS_PY                                                29
C -                                                                       29
      REAL SFJP_SFSM_LL                                                   29
      EQUIVALENCE (BANK_SFJP(114),SFJP_SFSM_LL,RE_SFJP_SFSM_LL),          29
     *(NAMES_SFJP(112),NSFJP_SFSM_LL)                                     29
      EQUIVALENCE (COMMEN_SFJP(112),CSFJP_SFSM_LL),(MAP_SFJP(1,112),      29
     *MSFJP_SFSM_LL)                                                      29
      CHARACTER CSFJP_SFSM_LL*40/' '/                                     29
      CHARACTER NSFJP_SFSM_LL*12/'R.SFSM_LL'/                             29
      INTEGER MSFJP_SFSM_LL(2)/1,1/                                       29
      REAL RE_SFJP_SFSM_LL                                                29
C -                                                                       29
      REAL SFJP_SFFX_DX                                                   29
      EQUIVALENCE (BANK_SFJP(115),SFJP_SFFX_DX,RE_SFJP_SFFX_DX),          29
     *(NAMES_SFJP(113),NSFJP_SFFX_DX)                                     29
      EQUIVALENCE (COMMEN_SFJP(113),CSFJP_SFFX_DX),(MAP_SFJP(1,113),      29
     *MSFJP_SFFX_DX)                                                      29
      CHARACTER CSFJP_SFFX_DX*40/' '/                                     29
      CHARACTER NSFJP_SFFX_DX*12/'R.SFFX_DX'/                             29
      INTEGER MSFJP_SFFX_DX(2)/1,1/                                       29
      REAL RE_SFJP_SFFX_DX                                                29
C -                                                                       29
      REAL SFJP_SFFX_DYZ                                                  29
      EQUIVALENCE (BANK_SFJP(116),SFJP_SFFX_DYZ,RE_SFJP_SFFX_DYZ),        29
     *(NAMES_SFJP(114),NSFJP_SFFX_DYZ)                                    29
      EQUIVALENCE (COMMEN_SFJP(114),CSFJP_SFFX_DYZ),(MAP_SFJP(1,114),     29
     *MSFJP_SFFX_DYZ)                                                     29
      CHARACTER CSFJP_SFFX_DYZ*40/' '/                                    29
      CHARACTER NSFJP_SFFX_DYZ*12/'R.SFFX_DYZ'/                           29
      INTEGER MSFJP_SFFX_DYZ(2)/1,1/                                      29
      REAL RE_SFJP_SFFX_DYZ                                               29
C -                                                                       29
      REAL SFJP_SFPI_RMIN                                                 29
      EQUIVALENCE (BANK_SFJP(117),SFJP_SFPI_RMIN,RE_SFJP_SFPI_RMIN),      29
     *(NAMES_SFJP(115),NSFJP_SFPI_RMIN)                                   29
      EQUIVALENCE (COMMEN_SFJP(115),CSFJP_SFPI_RMIN),(MAP_SFJP(1,115),    29
     *MSFJP_SFPI_RMIN)                                                    29
      CHARACTER CSFJP_SFPI_RMIN*40/' '/                                   29
      CHARACTER NSFJP_SFPI_RMIN*12/'R.SFPI_RMIN'/                         29
      INTEGER MSFJP_SFPI_RMIN(2)/1,1/                                     29
      REAL RE_SFJP_SFPI_RMIN                                              29
C -                                                                       29
      REAL SFJP_SFPI_RMAX                                                 29
      EQUIVALENCE (BANK_SFJP(118),SFJP_SFPI_RMAX,RE_SFJP_SFPI_RMAX),      29
     *(NAMES_SFJP(116),NSFJP_SFPI_RMAX)                                   29
      EQUIVALENCE (COMMEN_SFJP(116),CSFJP_SFPI_RMAX),(MAP_SFJP(1,116),    29
     *MSFJP_SFPI_RMAX)                                                    29
      CHARACTER CSFJP_SFPI_RMAX*40/' '/                                   29
      CHARACTER NSFJP_SFPI_RMAX*12/'R.SFPI_RMAX'/                         29
      INTEGER MSFJP_SFPI_RMAX(2)/1,1/                                     29
      REAL RE_SFJP_SFPI_RMAX                                              29
C -                                                                       29
      REAL SFJP_SFPI_PX                                                   29
      EQUIVALENCE (BANK_SFJP(119),SFJP_SFPI_PX,RE_SFJP_SFPI_PX),          29
     *(NAMES_SFJP(117),NSFJP_SFPI_PX)                                     29
      EQUIVALENCE (COMMEN_SFJP(117),CSFJP_SFPI_PX),(MAP_SFJP(1,117),      29
     *MSFJP_SFPI_PX)                                                      29
      CHARACTER CSFJP_SFPI_PX*40/' '/                                     29
      CHARACTER NSFJP_SFPI_PX*12/'R.SFPI_PX'/                             29
      INTEGER MSFJP_SFPI_PX(2)/1,1/                                       29
      REAL RE_SFJP_SFPI_PX                                                29
C -                                                                       29
      REAL SFJP_SFPI_PY1                                                  29
      EQUIVALENCE (BANK_SFJP(120),SFJP_SFPI_PY1,RE_SFJP_SFPI_PY1),        29
     *(NAMES_SFJP(118),NSFJP_SFPI_PY1)                                    29
      EQUIVALENCE (COMMEN_SFJP(118),CSFJP_SFPI_PY1),(MAP_SFJP(1,118),     29
     *MSFJP_SFPI_PY1)                                                     29
      CHARACTER CSFJP_SFPI_PY1*40/' '/                                    29
      CHARACTER NSFJP_SFPI_PY1*12/'R.SFPI_PY1'/                           29
      INTEGER MSFJP_SFPI_PY1(2)/1,1/                                      29
      REAL RE_SFJP_SFPI_PY1                                               29
C -                                                                       29
      REAL SFJP_SFPI_PY2                                                  29
      EQUIVALENCE (BANK_SFJP(121),SFJP_SFPI_PY2,RE_SFJP_SFPI_PY2),        29
     *(NAMES_SFJP(119),NSFJP_SFPI_PY2)                                    29
      EQUIVALENCE (COMMEN_SFJP(119),CSFJP_SFPI_PY2),(MAP_SFJP(1,119),     29
     *MSFJP_SFPI_PY2)                                                     29
      CHARACTER CSFJP_SFPI_PY2*40/' '/                                    29
      CHARACTER NSFJP_SFPI_PY2*12/'R.SFPI_PY2'/                           29
      INTEGER MSFJP_SFPI_PY2(2)/1,1/                                      29
      REAL RE_SFJP_SFPI_PY2                                               29
C -                                                                       29
      REAL SFJP_SFPI_PZ                                                   29
      EQUIVALENCE (BANK_SFJP(122),SFJP_SFPI_PZ,RE_SFJP_SFPI_PZ),          29
     *(NAMES_SFJP(120),NSFJP_SFPI_PZ)                                     29
      EQUIVALENCE (COMMEN_SFJP(120),CSFJP_SFPI_PZ),(MAP_SFJP(1,120),      29
     *MSFJP_SFPI_PZ)                                                      29
      CHARACTER CSFJP_SFPI_PZ*40/' '/                                     29
      CHARACTER NSFJP_SFPI_PZ*12/'R.SFPI_PZ'/                             29
      INTEGER MSFJP_SFPI_PZ(2)/1,1/                                       29
      REAL RE_SFJP_SFPI_PZ                                                29
C -                                                                       29
      REAL SFJP_SFPJ_DX1                                                  29
      EQUIVALENCE (BANK_SFJP(123),SFJP_SFPJ_DX1,RE_SFJP_SFPJ_DX1),        29
     *(NAMES_SFJP(121),NSFJP_SFPJ_DX1)                                    29
      EQUIVALENCE (COMMEN_SFJP(121),CSFJP_SFPJ_DX1),(MAP_SFJP(1,121),     29
     *MSFJP_SFPJ_DX1)                                                     29
      CHARACTER CSFJP_SFPJ_DX1*40/' '/                                    29
      CHARACTER NSFJP_SFPJ_DX1*12/'R.SFPJ_DX1'/                           29
      INTEGER MSFJP_SFPJ_DX1(2)/1,1/                                      29
      REAL RE_SFJP_SFPJ_DX1                                               29
C -                                                                       29
      REAL SFJP_SFPJ_DX2                                                  29
      EQUIVALENCE (BANK_SFJP(124),SFJP_SFPJ_DX2,RE_SFJP_SFPJ_DX2),        29
     *(NAMES_SFJP(122),NSFJP_SFPJ_DX2)                                    29
      EQUIVALENCE (COMMEN_SFJP(122),CSFJP_SFPJ_DX2),(MAP_SFJP(1,122),     29
     *MSFJP_SFPJ_DX2)                                                     29
      CHARACTER CSFJP_SFPJ_DX2*40/' '/                                    29
      CHARACTER NSFJP_SFPJ_DX2*12/'R.SFPJ_DX2'/                           29
      INTEGER MSFJP_SFPJ_DX2(2)/1,1/                                      29
      REAL RE_SFJP_SFPJ_DX2                                               29
C -                                                                       29
      REAL SFJP_SFPJ_DY                                                   29
      EQUIVALENCE (BANK_SFJP(125),SFJP_SFPJ_DY,RE_SFJP_SFPJ_DY),          29
     *(NAMES_SFJP(123),NSFJP_SFPJ_DY)                                     29
      EQUIVALENCE (COMMEN_SFJP(123),CSFJP_SFPJ_DY),(MAP_SFJP(1,123),      29
     *MSFJP_SFPJ_DY)                                                      29
      CHARACTER CSFJP_SFPJ_DY*40/' '/                                     29
      CHARACTER NSFJP_SFPJ_DY*12/'R.SFPJ_DY'/                             29
      INTEGER MSFJP_SFPJ_DY(2)/1,1/                                       29
      REAL RE_SFJP_SFPJ_DY                                                29
C -                                                                       29
      REAL SFJP_SFPJ_DZ                                                   29
      EQUIVALENCE (BANK_SFJP(126),SFJP_SFPJ_DZ,RE_SFJP_SFPJ_DZ),          29
     *(NAMES_SFJP(124),NSFJP_SFPJ_DZ)                                     29
      EQUIVALENCE (COMMEN_SFJP(124),CSFJP_SFPJ_DZ),(MAP_SFJP(1,124),      29
     *MSFJP_SFPJ_DZ)                                                      29
      CHARACTER CSFJP_SFPJ_DZ*40/' '/                                     29
      CHARACTER NSFJP_SFPJ_DZ*12/'R.SFPJ_DZ'/                             29
      INTEGER MSFJP_SFPJ_DZ(2)/1,1/                                       29
      REAL RE_SFJP_SFPJ_DZ                                                29
C -                                                                       29
      REAL SFJP_SFAA_DX                                                   29
      EQUIVALENCE (BANK_SFJP(127),SFJP_SFAA_DX,RE_SFJP_SFAA_DX),          29
     *(NAMES_SFJP(125),NSFJP_SFAA_DX)                                     29
      EQUIVALENCE (COMMEN_SFJP(125),CSFJP_SFAA_DX),(MAP_SFJP(1,125),      29
     *MSFJP_SFAA_DX)                                                      29
      CHARACTER CSFJP_SFAA_DX*40/' '/                                     29
      CHARACTER NSFJP_SFAA_DX*12/'R.SFAA_DX'/                             29
      INTEGER MSFJP_SFAA_DX(2)/1,1/                                       29
      REAL RE_SFJP_SFAA_DX                                                29
C -                                                                       29
      REAL SFJP_SFAA_DY                                                   29
      EQUIVALENCE (BANK_SFJP(128),SFJP_SFAA_DY,RE_SFJP_SFAA_DY),          29
     *(NAMES_SFJP(126),NSFJP_SFAA_DY)                                     29
      EQUIVALENCE (COMMEN_SFJP(126),CSFJP_SFAA_DY),(MAP_SFJP(1,126),      29
     *MSFJP_SFAA_DY)                                                      29
      CHARACTER CSFJP_SFAA_DY*40/' '/                                     29
      CHARACTER NSFJP_SFAA_DY*12/'R.SFAA_DY'/                             29
      INTEGER MSFJP_SFAA_DY(2)/1,1/                                       29
      REAL RE_SFJP_SFAA_DY                                                29
C -                                                                       29
      REAL SFJP_SFAA_DZ                                                   29
      EQUIVALENCE (BANK_SFJP(129),SFJP_SFAA_DZ,RE_SFJP_SFAA_DZ),          29
     *(NAMES_SFJP(127),NSFJP_SFAA_DZ)                                     29
      EQUIVALENCE (COMMEN_SFJP(127),CSFJP_SFAA_DZ),(MAP_SFJP(1,127),      29
     *MSFJP_SFAA_DZ)                                                      29
      CHARACTER CSFJP_SFAA_DZ*40/' '/                                     29
      CHARACTER NSFJP_SFAA_DZ*12/'R.SFAA_DZ'/                             29
      INTEGER MSFJP_SFAA_DZ(2)/1,1/                                       29
      REAL RE_SFJP_SFAA_DZ                                                29
C -                                                                       29
      REAL SFJP_SFAA_PX1                                                  29
      EQUIVALENCE (BANK_SFJP(130),SFJP_SFAA_PX1,RE_SFJP_SFAA_PX1),        29
     *(NAMES_SFJP(128),NSFJP_SFAA_PX1)                                    29
      EQUIVALENCE (COMMEN_SFJP(128),CSFJP_SFAA_PX1),(MAP_SFJP(1,128),     29
     *MSFJP_SFAA_PX1)                                                     29
      CHARACTER CSFJP_SFAA_PX1*40/' '/                                    29
      CHARACTER NSFJP_SFAA_PX1*12/'R.SFAA_PX1'/                           29
      INTEGER MSFJP_SFAA_PX1(2)/1,1/                                      29
      REAL RE_SFJP_SFAA_PX1                                               29
C -                                                                       29
      REAL SFJP_SFAA_PX2                                                  29
      EQUIVALENCE (BANK_SFJP(131),SFJP_SFAA_PX2,RE_SFJP_SFAA_PX2),        29
     *(NAMES_SFJP(129),NSFJP_SFAA_PX2)                                    29
      EQUIVALENCE (COMMEN_SFJP(129),CSFJP_SFAA_PX2),(MAP_SFJP(1,129),     29
     *MSFJP_SFAA_PX2)                                                     29
      CHARACTER CSFJP_SFAA_PX2*40/' '/                                    29
      CHARACTER NSFJP_SFAA_PX2*12/'R.SFAA_PX2'/                           29
      INTEGER MSFJP_SFAA_PX2(2)/1,1/                                      29
      REAL RE_SFJP_SFAA_PX2                                               29
C -                                                                       29
      REAL SFJP_SFAA_PX3                                                  29
      EQUIVALENCE (BANK_SFJP(132),SFJP_SFAA_PX3,RE_SFJP_SFAA_PX3),        29
     *(NAMES_SFJP(130),NSFJP_SFAA_PX3)                                    29
      EQUIVALENCE (COMMEN_SFJP(130),CSFJP_SFAA_PX3),(MAP_SFJP(1,130),     29
     *MSFJP_SFAA_PX3)                                                     29
      CHARACTER CSFJP_SFAA_PX3*40/' '/                                    29
      CHARACTER NSFJP_SFAA_PX3*12/'R.SFAA_PX3'/                           29
      INTEGER MSFJP_SFAA_PX3(2)/1,1/                                      29
      REAL RE_SFJP_SFAA_PX3                                               29
C -                                                                       29
      REAL SFJP_SFAA_PZ1                                                  29
      EQUIVALENCE (BANK_SFJP(133),SFJP_SFAA_PZ1,RE_SFJP_SFAA_PZ1),        29
     *(NAMES_SFJP(131),NSFJP_SFAA_PZ1)                                    29
      EQUIVALENCE (COMMEN_SFJP(131),CSFJP_SFAA_PZ1),(MAP_SFJP(1,131),     29
     *MSFJP_SFAA_PZ1)                                                     29
      CHARACTER CSFJP_SFAA_PZ1*40/' '/                                    29
      CHARACTER NSFJP_SFAA_PZ1*12/'R.SFAA_PZ1'/                           29
      INTEGER MSFJP_SFAA_PZ1(2)/1,1/                                      29
      REAL RE_SFJP_SFAA_PZ1                                               29
C -                                                                       29
      REAL SFJP_SFAA_PZ2                                                  29
      EQUIVALENCE (BANK_SFJP(134),SFJP_SFAA_PZ2,RE_SFJP_SFAA_PZ2),        29
     *(NAMES_SFJP(132),NSFJP_SFAA_PZ2)                                    29
      EQUIVALENCE (COMMEN_SFJP(132),CSFJP_SFAA_PZ2),(MAP_SFJP(1,132),     29
     *MSFJP_SFAA_PZ2)                                                     29
      CHARACTER CSFJP_SFAA_PZ2*40/' '/                                    29
      CHARACTER NSFJP_SFAA_PZ2*12/'R.SFAA_PZ2'/                           29
      INTEGER MSFJP_SFAA_PZ2(2)/1,1/                                      29
      REAL RE_SFJP_SFAA_PZ2                                               29
C -                                                                       29
      REAL SFJP_SFAA_PZ3                                                  29
      EQUIVALENCE (BANK_SFJP(135),SFJP_SFAA_PZ3,RE_SFJP_SFAA_PZ3),        29
     *(NAMES_SFJP(133),NSFJP_SFAA_PZ3)                                    29
      EQUIVALENCE (COMMEN_SFJP(133),CSFJP_SFAA_PZ3),(MAP_SFJP(1,133),     29
     *MSFJP_SFAA_PZ3)                                                     29
      CHARACTER CSFJP_SFAA_PZ3*40/' '/                                    29
      CHARACTER NSFJP_SFAA_PZ3*12/'R.SFAA_PZ3'/                           29
      INTEGER MSFJP_SFAA_PZ3(2)/1,1/                                      29
      REAL RE_SFJP_SFAA_PZ3                                               29
C -                                                                       29
      REAL SFJP_SFSL_DX                                                   29
      EQUIVALENCE (BANK_SFJP(136),SFJP_SFSL_DX,RE_SFJP_SFSL_DX),          29
     *(NAMES_SFJP(134),NSFJP_SFSL_DX)                                     29
      EQUIVALENCE (COMMEN_SFJP(134),CSFJP_SFSL_DX),(MAP_SFJP(1,134),      29
     *MSFJP_SFSL_DX)                                                      29
      CHARACTER CSFJP_SFSL_DX*40/' '/                                     29
      CHARACTER NSFJP_SFSL_DX*12/'R.SFSL_DX'/                             29
      INTEGER MSFJP_SFSL_DX(2)/1,1/                                       29
      REAL RE_SFJP_SFSL_DX                                                29
C -                                                                       29
      REAL SFJP_SFSL_DY                                                   29
      EQUIVALENCE (BANK_SFJP(137),SFJP_SFSL_DY,RE_SFJP_SFSL_DY),          29
     *(NAMES_SFJP(135),NSFJP_SFSL_DY)                                     29
      EQUIVALENCE (COMMEN_SFJP(135),CSFJP_SFSL_DY),(MAP_SFJP(1,135),      29
     *MSFJP_SFSL_DY)                                                      29
      CHARACTER CSFJP_SFSL_DY*40/' '/                                     29
      CHARACTER NSFJP_SFSL_DY*12/'R.SFSL_DY'/                             29
      INTEGER MSFJP_SFSL_DY(2)/1,1/                                       29
      REAL RE_SFJP_SFSL_DY                                                29
C -                                                                       29
      REAL SFJP_SFSL_DZ                                                   29
      EQUIVALENCE (BANK_SFJP(138),SFJP_SFSL_DZ,RE_SFJP_SFSL_DZ),          29
     *(NAMES_SFJP(136),NSFJP_SFSL_DZ)                                     29
      EQUIVALENCE (COMMEN_SFJP(136),CSFJP_SFSL_DZ),(MAP_SFJP(1,136),      29
     *MSFJP_SFSL_DZ)                                                      29
      CHARACTER CSFJP_SFSL_DZ*40/' '/                                     29
      CHARACTER NSFJP_SFSL_DZ*12/'R.SFSL_DZ'/                             29
      INTEGER MSFJP_SFSL_DZ(2)/1,1/                                       29
      REAL RE_SFJP_SFSL_DZ                                                29
C -                                                                       29
      REAL SFJP_SFSD_DX                                                   29
      EQUIVALENCE (BANK_SFJP(139),SFJP_SFSD_DX,RE_SFJP_SFSD_DX),          29
     *(NAMES_SFJP(137),NSFJP_SFSD_DX)                                     29
      EQUIVALENCE (COMMEN_SFJP(137),CSFJP_SFSD_DX),(MAP_SFJP(1,137),      29
     *MSFJP_SFSD_DX)                                                      29
      CHARACTER CSFJP_SFSD_DX*40/' '/                                     29
      CHARACTER NSFJP_SFSD_DX*12/'R.SFSD_DX'/                             29
      INTEGER MSFJP_SFSD_DX(2)/1,1/                                       29
      REAL RE_SFJP_SFSD_DX                                                29
C -                                                                       29
      REAL SFJP_SFSD_DY                                                   29
      EQUIVALENCE (BANK_SFJP(140),SFJP_SFSD_DY,RE_SFJP_SFSD_DY),          29
     *(NAMES_SFJP(138),NSFJP_SFSD_DY)                                     29
      EQUIVALENCE (COMMEN_SFJP(138),CSFJP_SFSD_DY),(MAP_SFJP(1,138),      29
     *MSFJP_SFSD_DY)                                                      29
      CHARACTER CSFJP_SFSD_DY*40/' '/                                     29
      CHARACTER NSFJP_SFSD_DY*12/'R.SFSD_DY'/                             29
      INTEGER MSFJP_SFSD_DY(2)/1,1/                                       29
      REAL RE_SFJP_SFSD_DY                                                29
C -                                                                       29
      REAL SFJP_SFSD_DZ                                                   29
      EQUIVALENCE (BANK_SFJP(141),SFJP_SFSD_DZ,RE_SFJP_SFSD_DZ),          29
     *(NAMES_SFJP(139),NSFJP_SFSD_DZ)                                     29
      EQUIVALENCE (COMMEN_SFJP(139),CSFJP_SFSD_DZ),(MAP_SFJP(1,139),      29
     *MSFJP_SFSD_DZ)                                                      29
      CHARACTER CSFJP_SFSD_DZ*40/' '/                                     29
      CHARACTER NSFJP_SFSD_DZ*12/'R.SFSD_DZ'/                             29
      INTEGER MSFJP_SFSD_DZ(2)/1,1/                                       29
      REAL RE_SFJP_SFSD_DZ                                                29
C -                                                                       29
      REAL SFJP_SFLA_PX                                                   29
      EQUIVALENCE (BANK_SFJP(142),SFJP_SFLA_PX,RE_SFJP_SFLA_PX),          29
     *(NAMES_SFJP(140),NSFJP_SFLA_PX)                                     29
      EQUIVALENCE (COMMEN_SFJP(140),CSFJP_SFLA_PX),(MAP_SFJP(1,140),      29
     *MSFJP_SFLA_PX)                                                      29
      CHARACTER CSFJP_SFLA_PX*40/' '/                                     29
      CHARACTER NSFJP_SFLA_PX*12/'R.SFLA_PX'/                             29
      INTEGER MSFJP_SFLA_PX(2)/1,1/                                       29
      REAL RE_SFJP_SFLA_PX                                                29
C -                                                                       29
      REAL SFJP_SFLA_PY                                                   29
      EQUIVALENCE (BANK_SFJP(143),SFJP_SFLA_PY,RE_SFJP_SFLA_PY),          29
     *(NAMES_SFJP(141),NSFJP_SFLA_PY)                                     29
      EQUIVALENCE (COMMEN_SFJP(141),CSFJP_SFLA_PY),(MAP_SFJP(1,141),      29
     *MSFJP_SFLA_PY)                                                      29
      CHARACTER CSFJP_SFLA_PY*40/' '/                                     29
      CHARACTER NSFJP_SFLA_PY*12/'R.SFLA_PY'/                             29
      INTEGER MSFJP_SFLA_PY(2)/1,1/                                       29
      REAL RE_SFJP_SFLA_PY                                                29
C -                                                                       29
      REAL SFJP_SFLC_PX                                                   29
      EQUIVALENCE (BANK_SFJP(144),SFJP_SFLC_PX,RE_SFJP_SFLC_PX),          29
     *(NAMES_SFJP(142),NSFJP_SFLC_PX)                                     29
      EQUIVALENCE (COMMEN_SFJP(142),CSFJP_SFLC_PX),(MAP_SFJP(1,142),      29
     *MSFJP_SFLC_PX)                                                      29
      CHARACTER CSFJP_SFLC_PX*40/' '/                                     29
      CHARACTER NSFJP_SFLC_PX*12/'R.SFLC_PX'/                             29
      INTEGER MSFJP_SFLC_PX(2)/1,1/                                       29
      REAL RE_SFJP_SFLC_PX                                                29
C -                                                                       29
      REAL SFJP_SFLC_PY                                                   29
      EQUIVALENCE (BANK_SFJP(145),SFJP_SFLC_PY,RE_SFJP_SFLC_PY),          29
     *(NAMES_SFJP(143),NSFJP_SFLC_PY)                                     29
      EQUIVALENCE (COMMEN_SFJP(143),CSFJP_SFLC_PY),(MAP_SFJP(1,143),      29
     *MSFJP_SFLC_PY)                                                      29
      CHARACTER CSFJP_SFLC_PY*40/' '/                                     29
      CHARACTER NSFJP_SFLC_PY*12/'R.SFLC_PY'/                             29
      INTEGER MSFJP_SFLC_PY(2)/1,1/                                       29
      REAL RE_SFJP_SFLC_PY                                                29
C -                                                                       29
      REAL SFJP_SFES_PX                                                   29
      EQUIVALENCE (BANK_SFJP(146),SFJP_SFES_PX,RE_SFJP_SFES_PX),          29
     *(NAMES_SFJP(144),NSFJP_SFES_PX)                                     29
      EQUIVALENCE (COMMEN_SFJP(144),CSFJP_SFES_PX),(MAP_SFJP(1,144),      29
     *MSFJP_SFES_PX)                                                      29
      CHARACTER CSFJP_SFES_PX*40/' '/                                     29
      CHARACTER NSFJP_SFES_PX*12/'R.SFES_PX'/                             29
      INTEGER MSFJP_SFES_PX(2)/1,1/                                       29
      REAL RE_SFJP_SFES_PX                                                29
C -                                                                       29
      REAL SFJP_SFES_PY                                                   29
      EQUIVALENCE (BANK_SFJP(147),SFJP_SFES_PY,RE_SFJP_SFES_PY),          29
     *(NAMES_SFJP(145),NSFJP_SFES_PY)                                     29
      EQUIVALENCE (COMMEN_SFJP(145),CSFJP_SFES_PY),(MAP_SFJP(1,145),      29
     *MSFJP_SFES_PY)                                                      29
      CHARACTER CSFJP_SFES_PY*40/' '/                                     29
      CHARACTER NSFJP_SFES_PY*12/'R.SFES_PY'/                             29
      INTEGER MSFJP_SFES_PY(2)/1,1/                                       29
      REAL RE_SFJP_SFES_PY                                                29
C -                                                                       29
      REAL SFJP_SFES_PZ                                                   29
      EQUIVALENCE (BANK_SFJP(148),SFJP_SFES_PZ,RE_SFJP_SFES_PZ),          29
     *(NAMES_SFJP(146),NSFJP_SFES_PZ)                                     29
      EQUIVALENCE (COMMEN_SFJP(146),CSFJP_SFES_PZ),(MAP_SFJP(1,146),      29
     *MSFJP_SFES_PZ)                                                      29
      CHARACTER CSFJP_SFES_PZ*40/' '/                                     29
      CHARACTER NSFJP_SFES_PZ*12/'R.SFES_PZ'/                             29
      INTEGER MSFJP_SFES_PZ(2)/1,1/                                       29
      REAL RE_SFJP_SFES_PZ                                                29
C -                                                                       29
      REAL SFJP_SFEB_PX                                                   29
      EQUIVALENCE (BANK_SFJP(149),SFJP_SFEB_PX,RE_SFJP_SFEB_PX),          29
     *(NAMES_SFJP(147),NSFJP_SFEB_PX)                                     29
      EQUIVALENCE (COMMEN_SFJP(147),CSFJP_SFEB_PX),(MAP_SFJP(1,147),      29
     *MSFJP_SFEB_PX)                                                      29
      CHARACTER CSFJP_SFEB_PX*40/' '/                                     29
      CHARACTER NSFJP_SFEB_PX*12/'R.SFEB_PX'/                             29
      INTEGER MSFJP_SFEB_PX(2)/1,1/                                       29
      REAL RE_SFJP_SFEB_PX                                                29
C -                                                                       29
      REAL SFJP_SFEB_PY                                                   29
      EQUIVALENCE (BANK_SFJP(150),SFJP_SFEB_PY,RE_SFJP_SFEB_PY),          29
     *(NAMES_SFJP(148),NSFJP_SFEB_PY)                                     29
      EQUIVALENCE (COMMEN_SFJP(148),CSFJP_SFEB_PY),(MAP_SFJP(1,148),      29
     *MSFJP_SFEB_PY)                                                      29
      CHARACTER CSFJP_SFEB_PY*40/' '/                                     29
      CHARACTER NSFJP_SFEB_PY*12/'R.SFEB_PY'/                             29
      INTEGER MSFJP_SFEB_PY(2)/1,1/                                       29
      REAL RE_SFJP_SFEB_PY                                                29
C -                                                                       29
      INTEGER LEN_SFJP,LLEN_SFJP,LINK_SFJP                                29
      PARAMETER (LEN_SFJP=150)                                            29
      PARAMETER (LLEN_SFJP=148)                                           29
      INTEGER FLAG_SFJP/0/,MAP_SFJP(2,LLEN_SFJP),IDNUM_SFJP(10)           29
      CHARACTER NAMES_SFJP(LLEN_SFJP)*12,COMMEN_SFJP(LLEN_SFJP)*40        29
      DATA IDNUM_SFJP/10*0/                                               29
      REAL BANK_SFJP(LEN_SFJP)                                            29
      CHARACTER*60 BPATH_SFJP/' '/                                        29
      SAVE FLAG_SFJP,MAP_SFJP,IDNUM_SFJP,NAMES_SFJP,COMMEN_SFJP,          29
     *BPATH_SFJP                                                          29
      DATA LINK_SFJP/0/,BANK_SFJP/LEN_SFJP*0/                             29
C *                                                                       29
C -                                                                       30
      REAL SFPB_HHIGHT                                                    30
      EQUIVALENCE (BANK_SFPB(3),SFPB_HHIGHT,RE_SFPB_HHIGHT),              30
     *(NAMES_SFPB(1),NSFPB_HHIGHT)                                        30
      EQUIVALENCE (COMMEN_SFPB(1),CSFPB_HHIGHT),(MAP_SFPB(1,1),           30
     *MSFPB_HHIGHT)                                                       30
      CHARACTER CSFPB_HHIGHT*40/' '/                                      30
      CHARACTER NSFPB_HHIGHT*12/'R.HHIGHT'/                               30
      INTEGER MSFPB_HHIGHT(2)/1,1/                                        30
      REAL RE_SFPB_HHIGHT                                                 30
C -                                                                       30
      REAL SFPB_KHIGHT                                                    30
      EQUIVALENCE (BANK_SFPB(4),SFPB_KHIGHT,RE_SFPB_KHIGHT),              30
     *(NAMES_SFPB(2),NSFPB_KHIGHT)                                        30
      EQUIVALENCE (COMMEN_SFPB(2),CSFPB_KHIGHT),(MAP_SFPB(1,2),           30
     *MSFPB_KHIGHT)                                                       30
      CHARACTER CSFPB_KHIGHT*40/' '/                                      30
      CHARACTER NSFPB_KHIGHT*12/'R.KHIGHT'/                               30
      INTEGER MSFPB_KHIGHT(2)/1,1/                                        30
      REAL RE_SFPB_KHIGHT                                                 30
C -                                                                       30
      REAL SFPB_HBASE                                                     30
      EQUIVALENCE (BANK_SFPB(5),SFPB_HBASE,RE_SFPB_HBASE),                30
     *(NAMES_SFPB(3),NSFPB_HBASE)                                         30
      EQUIVALENCE (COMMEN_SFPB(3),CSFPB_HBASE),(MAP_SFPB(1,3),            30
     *MSFPB_HBASE)                                                        30
      CHARACTER CSFPB_HBASE*40/' '/                                       30
      CHARACTER NSFPB_HBASE*12/'R.HBASE'/                                 30
      INTEGER MSFPB_HBASE(2)/1,1/                                         30
      REAL RE_SFPB_HBASE                                                  30
C -                                                                       30
      REAL SFPB_KBASE                                                     30
      EQUIVALENCE (BANK_SFPB(6),SFPB_KBASE,RE_SFPB_KBASE),                30
     *(NAMES_SFPB(4),NSFPB_KBASE)                                         30
      EQUIVALENCE (COMMEN_SFPB(4),CSFPB_KBASE),(MAP_SFPB(1,4),            30
     *MSFPB_KBASE)                                                        30
      CHARACTER CSFPB_KBASE*40/' '/                                       30
      CHARACTER NSFPB_KBASE*12/'R.KBASE'/                                 30
      INTEGER MSFPB_KBASE(2)/1,1/                                         30
      REAL RE_SFPB_KBASE                                                  30
C -                                                                       30
      REAL SFPB_FSIZE                                                     30
      EQUIVALENCE (BANK_SFPB(7),SFPB_FSIZE,RE_SFPB_FSIZE),                30
     *(NAMES_SFPB(5),NSFPB_FSIZE)                                         30
      EQUIVALENCE (COMMEN_SFPB(5),CSFPB_FSIZE),(MAP_SFPB(1,5),            30
     *MSFPB_FSIZE)                                                        30
      CHARACTER CSFPB_FSIZE*40/' '/                                       30
      CHARACTER NSFPB_FSIZE*12/'R.FSIZE'/                                 30
      INTEGER MSFPB_FSIZE(2)/1,1/                                         30
      REAL RE_SFPB_FSIZE                                                  30
C -                                                                       30
      REAL SFPB_ZCOOR                                                     30
      EQUIVALENCE (BANK_SFPB(8),SFPB_ZCOOR,RE_SFPB_ZCOOR),                30
     *(NAMES_SFPB(6),NSFPB_ZCOOR)                                         30
      EQUIVALENCE (COMMEN_SFPB(6),CSFPB_ZCOOR),(MAP_SFPB(1,6),            30
     *MSFPB_ZCOOR)                                                        30
      CHARACTER CSFPB_ZCOOR*40/' '/                                       30
      CHARACTER NSFPB_ZCOOR*12/'R.ZCOOR'/                                 30
      INTEGER MSFPB_ZCOOR(2)/1,1/                                         30
      REAL RE_SFPB_ZCOOR                                                  30
C -                                                                       30
      INTEGER LEN_SFPB,LLEN_SFPB,LINK_SFPB                                30
      PARAMETER (LEN_SFPB=8)                                              30
      PARAMETER (LLEN_SFPB=6)                                             30
      INTEGER FLAG_SFPB/0/,MAP_SFPB(2,LLEN_SFPB),IDNUM_SFPB(10)           30
      CHARACTER NAMES_SFPB(LLEN_SFPB)*12,COMMEN_SFPB(LLEN_SFPB)*40        30
      DATA IDNUM_SFPB/10*0/                                               30
      REAL BANK_SFPB(LEN_SFPB)                                            30
      CHARACTER*60 BPATH_SFPB/' '/                                        30
      SAVE FLAG_SFPB,MAP_SFPB,IDNUM_SFPB,NAMES_SFPB,COMMEN_SFPB,          30
     *BPATH_SFPB                                                          30
      DATA LINK_SFPB/0/,BANK_SFPB/LEN_SFPB*0/                             30
C *                                                                       30
C -                                                                       33
      INTEGER SFPA_VERSION                                                33
      EQUIVALENCE (BANK_SFPA(3),SFPA_VERSION,RE_SFPA_VERSION),            33
     *(NAMES_SFPA(1),NSFPA_VERSION)                                       33
      EQUIVALENCE (COMMEN_SFPA(1),CSFPA_VERSION),(MAP_SFPA(1,1),          33
     *MSFPA_VERSION)                                                      33
      CHARACTER CSFPA_VERSION*40/' '/                                     33
      CHARACTER NSFPA_VERSION*12/'I.VERSION'/                             33
      INTEGER MSFPA_VERSION(2)/1,1/                                       33
      REAL RE_SFPA_VERSION                                                33
C -                                                                       33
      REAL SFPA_RMIN                                                      33
      EQUIVALENCE (BANK_SFPA(4),SFPA_RMIN,RE_SFPA_RMIN),(NAMES_SFPA(2),   33
     *NSFPA_RMIN)                                                         33
      EQUIVALENCE (COMMEN_SFPA(2),CSFPA_RMIN),(MAP_SFPA(1,2),             33
     *MSFPA_RMIN)                                                         33
      CHARACTER CSFPA_RMIN*40/' '/                                        33
      CHARACTER NSFPA_RMIN*12/'R.RMIN'/                                   33
      INTEGER MSFPA_RMIN(2)/1,1/                                          33
      REAL RE_SFPA_RMIN                                                   33
C -                                                                       33
      REAL SFPA_RMAX                                                      33
      EQUIVALENCE (BANK_SFPA(5),SFPA_RMAX,RE_SFPA_RMAX),(NAMES_SFPA(3),   33
     *NSFPA_RMAX)                                                         33
      EQUIVALENCE (COMMEN_SFPA(3),CSFPA_RMAX),(MAP_SFPA(1,3),             33
     *MSFPA_RMAX)                                                         33
      CHARACTER CSFPA_RMAX*40/' '/                                        33
      CHARACTER NSFPA_RMAX*12/'R.RMAX'/                                   33
      INTEGER MSFPA_RMAX(2)/1,1/                                          33
      REAL RE_SFPA_RMAX                                                   33
C -                                                                       33
      REAL SFPA_LEN                                                       33
      EQUIVALENCE (BANK_SFPA(6),SFPA_LEN,RE_SFPA_LEN),(NAMES_SFPA(4),     33
     *NSFPA_LEN)                                                          33
      EQUIVALENCE (COMMEN_SFPA(4),CSFPA_LEN),(MAP_SFPA(1,4),MSFPA_LEN)    33
      CHARACTER CSFPA_LEN*40/' '/                                         33
      CHARACTER NSFPA_LEN*12/'R.LEN'/                                     33
      INTEGER MSFPA_LEN(2)/1,1/                                           33
      REAL RE_SFPA_LEN                                                    33
C -                                                                       33
      REAL SFPA_SOUTRINN                                                  33
      EQUIVALENCE (BANK_SFPA(7),SFPA_SOUTRINN,RE_SFPA_SOUTRINN),          33
     *(NAMES_SFPA(5),NSFPA_SOUTRINN)                                      33
      EQUIVALENCE (COMMEN_SFPA(5),CSFPA_SOUTRINN),(MAP_SFPA(1,5),         33
     *MSFPA_SOUTRINN)                                                     33
      CHARACTER CSFPA_SOUTRINN*40/' '/                                    33
      CHARACTER NSFPA_SOUTRINN*12/'R.SOUTRINN'/                           33
      INTEGER MSFPA_SOUTRINN(2)/1,1/                                      33
      REAL RE_SFPA_SOUTRINN                                               33
C -                                                                       33
      REAL SFPA_SOUTROUT                                                  33
      EQUIVALENCE (BANK_SFPA(8),SFPA_SOUTROUT,RE_SFPA_SOUTROUT),          33
     *(NAMES_SFPA(6),NSFPA_SOUTROUT)                                      33
      EQUIVALENCE (COMMEN_SFPA(6),CSFPA_SOUTROUT),(MAP_SFPA(1,6),         33
     *MSFPA_SOUTROUT)                                                     33
      CHARACTER CSFPA_SOUTROUT*40/' '/                                    33
      CHARACTER NSFPA_SOUTROUT*12/'R.SOUTROUT'/                           33
      INTEGER MSFPA_SOUTROUT(2)/1,1/                                      33
      REAL RE_SFPA_SOUTROUT                                               33
C -                                                                       33
      REAL SFPA_SOUTLEN                                                   33
      EQUIVALENCE (BANK_SFPA(9),SFPA_SOUTLEN,RE_SFPA_SOUTLEN),            33
     *(NAMES_SFPA(7),NSFPA_SOUTLEN)                                       33
      EQUIVALENCE (COMMEN_SFPA(7),CSFPA_SOUTLEN),(MAP_SFPA(1,7),          33
     *MSFPA_SOUTLEN)                                                      33
      CHARACTER CSFPA_SOUTLEN*40/' '/                                     33
      CHARACTER NSFPA_SOUTLEN*12/'R.SOUTLEN'/                             33
      INTEGER MSFPA_SOUTLEN(2)/1,1/                                       33
      REAL RE_SFPA_SOUTLEN                                                33
C -                                                                       33
      REAL SFPA_RAD                                                       33
      EQUIVALENCE (BANK_SFPA(10),SFPA_RAD,RE_SFPA_RAD),(NAMES_SFPA(8),    33
     *NSFPA_RAD)                                                          33
      EQUIVALENCE (COMMEN_SFPA(8),CSFPA_RAD),(MAP_SFPA(1,8),MSFPA_RAD)    33
      CHARACTER CSFPA_RAD*40/' '/                                         33
      CHARACTER NSFPA_RAD*12/'R.RAD'/                                     33
      INTEGER MSFPA_RAD(2)/1,1/                                           33
      REAL RE_SFPA_RAD                                                    33
C -                                                                       33
      REAL SFPA_NSSD                                                      33
      EQUIVALENCE (BANK_SFPA(11),SFPA_NSSD,RE_SFPA_NSSD),(NAMES_SFPA(9),  33
     *NSFPA_NSSD)                                                         33
      EQUIVALENCE (COMMEN_SFPA(9),CSFPA_NSSD),(MAP_SFPA(1,9),             33
     *MSFPA_NSSD)                                                         33
      CHARACTER CSFPA_NSSD*40/' '/                                        33
      CHARACTER NSFPA_NSSD*12/'R.NSSD'/                                   33
      INTEGER MSFPA_NSSD(2)/1,1/                                          33
      REAL RE_SFPA_NSSD                                                   33
C -                                                                       33
      REAL SFPA_DMWID                                                     33
      EQUIVALENCE (BANK_SFPA(12),SFPA_DMWID,RE_SFPA_DMWID),               33
     *(NAMES_SFPA(10),NSFPA_DMWID)                                        33
      EQUIVALENCE (COMMEN_SFPA(10),CSFPA_DMWID),(MAP_SFPA(1,10),          33
     *MSFPA_DMWID)                                                        33
      CHARACTER CSFPA_DMWID*40/' '/                                       33
      CHARACTER NSFPA_DMWID*12/'R.DMWID'/                                 33
      INTEGER MSFPA_DMWID(2)/1,1/                                         33
      REAL RE_SFPA_DMWID                                                  33
C -                                                                       33
      REAL SFPA_DMTHK                                                     33
      EQUIVALENCE (BANK_SFPA(13),SFPA_DMTHK,RE_SFPA_DMTHK),               33
     *(NAMES_SFPA(11),NSFPA_DMTHK)                                        33
      EQUIVALENCE (COMMEN_SFPA(11),CSFPA_DMTHK),(MAP_SFPA(1,11),          33
     *MSFPA_DMTHK)                                                        33
      CHARACTER CSFPA_DMTHK*40/' '/                                       33
      CHARACTER NSFPA_DMTHK*12/'R.DMTHK'/                                 33
      INTEGER MSFPA_DMTHK(2)/1,1/                                         33
      REAL RE_SFPA_DMTHK                                                  33
C -                                                                       33
      REAL SFPA_DMLEN                                                     33
      EQUIVALENCE (BANK_SFPA(14),SFPA_DMLEN,RE_SFPA_DMLEN),               33
     *(NAMES_SFPA(12),NSFPA_DMLEN)                                        33
      EQUIVALENCE (COMMEN_SFPA(12),CSFPA_DMLEN),(MAP_SFPA(1,12),          33
     *MSFPA_DMLEN)                                                        33
      CHARACTER CSFPA_DMLEN*40/' '/                                       33
      CHARACTER NSFPA_DMLEN*12/'R.DMLEN'/                                 33
      INTEGER MSFPA_DMLEN(2)/1,1/                                         33
      REAL RE_SFPA_DMLEN                                                  33
C -                                                                       33
      REAL SFPA_SMWID                                                     33
      EQUIVALENCE (BANK_SFPA(15),SFPA_SMWID,RE_SFPA_SMWID),               33
     *(NAMES_SFPA(13),NSFPA_SMWID)                                        33
      EQUIVALENCE (COMMEN_SFPA(13),CSFPA_SMWID),(MAP_SFPA(1,13),          33
     *MSFPA_SMWID)                                                        33
      CHARACTER CSFPA_SMWID*40/' '/                                       33
      CHARACTER NSFPA_SMWID*12/'R.SMWID'/                                 33
      INTEGER MSFPA_SMWID(2)/1,1/                                         33
      REAL RE_SFPA_SMWID                                                  33
C -                                                                       33
      REAL SFPA_SMTHK                                                     33
      EQUIVALENCE (BANK_SFPA(16),SFPA_SMTHK,RE_SFPA_SMTHK),               33
     *(NAMES_SFPA(14),NSFPA_SMTHK)                                        33
      EQUIVALENCE (COMMEN_SFPA(14),CSFPA_SMTHK),(MAP_SFPA(1,14),          33
     *MSFPA_SMTHK)                                                        33
      CHARACTER CSFPA_SMTHK*40/' '/                                       33
      CHARACTER NSFPA_SMTHK*12/'R.SMTHK'/                                 33
      INTEGER MSFPA_SMTHK(2)/1,1/                                         33
      REAL RE_SFPA_SMTHK                                                  33
C -                                                                       33
      REAL SFPA_SMLEN                                                     33
      EQUIVALENCE (BANK_SFPA(17),SFPA_SMLEN,RE_SFPA_SMLEN),               33
     *(NAMES_SFPA(15),NSFPA_SMLEN)                                        33
      EQUIVALENCE (COMMEN_SFPA(15),CSFPA_SMLEN),(MAP_SFPA(1,15),          33
     *MSFPA_SMLEN)                                                        33
      CHARACTER CSFPA_SMLEN*40/' '/                                       33
      CHARACTER NSFPA_SMLEN*12/'R.SMLEN'/                                 33
      INTEGER MSFPA_SMLEN(2)/1,1/                                         33
      REAL RE_SFPA_SMLEN                                                  33
C -                                                                       33
      REAL SFPA_SSLEN                                                     33
      EQUIVALENCE (BANK_SFPA(18),SFPA_SSLEN,RE_SFPA_SSLEN),               33
     *(NAMES_SFPA(16),NSFPA_SSLEN)                                        33
      EQUIVALENCE (COMMEN_SFPA(16),CSFPA_SSLEN),(MAP_SFPA(1,16),          33
     *MSFPA_SSLEN)                                                        33
      CHARACTER CSFPA_SSLEN*40/' '/                                       33
      CHARACTER NSFPA_SSLEN*12/'R.SSLEN'/                                 33
      INTEGER MSFPA_SSLEN(2)/1,1/                                         33
      REAL RE_SFPA_SSLEN                                                  33
C -                                                                       33
      REAL SFPA_WPLEN                                                     33
      EQUIVALENCE (BANK_SFPA(19),SFPA_WPLEN,RE_SFPA_WPLEN),               33
     *(NAMES_SFPA(17),NSFPA_WPLEN)                                        33
      EQUIVALENCE (COMMEN_SFPA(17),CSFPA_WPLEN),(MAP_SFPA(1,17),          33
     *MSFPA_WPLEN)                                                        33
      CHARACTER CSFPA_WPLEN*40/' '/                                       33
      CHARACTER NSFPA_WPLEN*12/'R.WPLEN'/                                 33
      INTEGER MSFPA_WPLEN(2)/1,1/                                         33
      REAL RE_SFPA_WPLEN                                                  33
C -                                                                       33
      REAL SFPA_SDLEN                                                     33
      EQUIVALENCE (BANK_SFPA(20),SFPA_SDLEN,RE_SFPA_SDLEN),               33
     *(NAMES_SFPA(18),NSFPA_SDLEN)                                        33
      EQUIVALENCE (COMMEN_SFPA(18),CSFPA_SDLEN),(MAP_SFPA(1,18),          33
     *MSFPA_SDLEN)                                                        33
      CHARACTER CSFPA_SDLEN*40/' '/                                       33
      CHARACTER NSFPA_SDLEN*12/'R.SDLEN'/                                 33
      INTEGER MSFPA_SDLEN(2)/1,1/                                         33
      REAL RE_SFPA_SDLEN                                                  33
C -                                                                       33
      REAL SFPA_TILT                                                      33
      EQUIVALENCE (BANK_SFPA(21),SFPA_TILT,RE_SFPA_TILT),                 33
     *(NAMES_SFPA(19),NSFPA_TILT)                                         33
      EQUIVALENCE (COMMEN_SFPA(19),CSFPA_TILT),(MAP_SFPA(1,19),           33
     *MSFPA_TILT)                                                         33
      CHARACTER CSFPA_TILT*40/' '/                                        33
      CHARACTER NSFPA_TILT*12/'R.TILT'/                                   33
      INTEGER MSFPA_TILT(2)/1,1/                                          33
      REAL RE_SFPA_TILT                                                   33
C -                                                                       33
      REAL SFPA_CPRAD                                                     33
      EQUIVALENCE (BANK_SFPA(22),SFPA_CPRAD,RE_SFPA_CPRAD),               33
     *(NAMES_SFPA(20),NSFPA_CPRAD)                                        33
      EQUIVALENCE (COMMEN_SFPA(20),CSFPA_CPRAD),(MAP_SFPA(1,20),          33
     *MSFPA_CPRAD)                                                        33
      CHARACTER CSFPA_CPRAD*40/' '/                                       33
      CHARACTER NSFPA_CPRAD*12/'R.CPRAD'/                                 33
      INTEGER MSFPA_CPRAD(2)/1,1/                                         33
      REAL RE_SFPA_CPRAD                                                  33
C -                                                                       33
      REAL SFPA_CPRAL                                                     33
      EQUIVALENCE (BANK_SFPA(23),SFPA_CPRAL,RE_SFPA_CPRAL),               33
     *(NAMES_SFPA(21),NSFPA_CPRAL)                                        33
      EQUIVALENCE (COMMEN_SFPA(21),CSFPA_CPRAL),(MAP_SFPA(1,21),          33
     *MSFPA_CPRAL)                                                        33
      CHARACTER CSFPA_CPRAL*40/' '/                                       33
      CHARACTER NSFPA_CPRAL*12/'R.CPRAL'/                                 33
      INTEGER MSFPA_CPRAL(2)/1,1/                                         33
      REAL RE_SFPA_CPRAL                                                  33
C -                                                                       33
      REAL SFPA_CFRAD                                                     33
      EQUIVALENCE (BANK_SFPA(24),SFPA_CFRAD,RE_SFPA_CFRAD),               33
     *(NAMES_SFPA(22),NSFPA_CFRAD)                                        33
      EQUIVALENCE (COMMEN_SFPA(22),CSFPA_CFRAD),(MAP_SFPA(1,22),          33
     *MSFPA_CFRAD)                                                        33
      CHARACTER CSFPA_CFRAD*40/' '/                                       33
      CHARACTER NSFPA_CFRAD*12/'R.CFRAD'/                                 33
      INTEGER MSFPA_CFRAD(2)/1,1/                                         33
      REAL RE_SFPA_CFRAD                                                  33
C -                                                                       33
      REAL SFPA_GPTHK                                                     33
      EQUIVALENCE (BANK_SFPA(25),SFPA_GPTHK,RE_SFPA_GPTHK),               33
     *(NAMES_SFPA(23),NSFPA_GPTHK)                                        33
      EQUIVALENCE (COMMEN_SFPA(23),CSFPA_GPTHK),(MAP_SFPA(1,23),          33
     *MSFPA_GPTHK)                                                        33
      CHARACTER CSFPA_GPTHK*40/' '/                                       33
      CHARACTER NSFPA_GPTHK*12/'R.GPTHK'/                                 33
      INTEGER MSFPA_GPTHK(2)/1,1/                                         33
      REAL RE_SFPA_GPTHK                                                  33
C -                                                                       33
      INTEGER SFPA_LADDERMAP(20)                                          33
      EQUIVALENCE (BANK_SFPA(26),SFPA_LADDERMAP,RE_SFPA_LADDERMAP),       33
     *(NAMES_SFPA(24),NSFPA_LADDERMAP)                                    33
      EQUIVALENCE (COMMEN_SFPA(24),CSFPA_LADDERMAP),(MAP_SFPA(1,24),      33
     *MSFPA_LADDERMAP)                                                    33
      CHARACTER CSFPA_LADDERMAP*40/' '/                                   33
      CHARACTER NSFPA_LADDERMAP*12/'I.LADDERMAP'/                         33
      INTEGER MSFPA_LADDERMAP(2)/20,1/                                    33
      REAL RE_SFPA_LADDERMAP                                              33
C -                                                                       33
      REAL SFPA_LADDERANGLE(20)                                           33
      EQUIVALENCE (BANK_SFPA(46),SFPA_LADDERANGLE,RE_SFPA_LADDERANGLE),   33
     *(NAMES_SFPA(25),NSFPA_LADDERANGLE)                                  33
      EQUIVALENCE (COMMEN_SFPA(25),CSFPA_LADDERANGLE),(MAP_SFPA(1,25),    33
     *MSFPA_LADDERANGLE)                                                  33
      CHARACTER CSFPA_LADDERANGLE*40/' '/                                 33
      CHARACTER NSFPA_LADDERANGLE*12/'R.LADDERANGLE'/                     33
      INTEGER MSFPA_LADDERANGLE(2)/20,1/                                  33
      REAL RE_SFPA_LADDERANGLE                                            33
C -                                                                       33
      REAL SFPA_LADDERTILT(20)                                            33
      EQUIVALENCE (BANK_SFPA(66),SFPA_LADDERTILT,RE_SFPA_LADDERTILT),     33
     *(NAMES_SFPA(26),NSFPA_LADDERTILT)                                   33
      EQUIVALENCE (COMMEN_SFPA(26),CSFPA_LADDERTILT),(MAP_SFPA(1,26),     33
     *MSFPA_LADDERTILT)                                                   33
      CHARACTER CSFPA_LADDERTILT*40/' '/                                  33
      CHARACTER NSFPA_LADDERTILT*12/'R.LADDERTILT'/                       33
      INTEGER MSFPA_LADDERTILT(2)/20,1/                                   33
      REAL RE_SFPA_LADDERTILT                                             33
C -                                                                       33
      REAL SFPA_LADDERRADIUS(20)                                          33
      EQUIVALENCE (BANK_SFPA(86),SFPA_LADDERRADIUS,                       33
     *RE_SFPA_LADDERRADIUS),(NAMES_SFPA(27),NSFPA_LADDERRADIUS)           33
      EQUIVALENCE (COMMEN_SFPA(27),CSFPA_LADDERRADIUS),(MAP_SFPA(1,27),   33
     *MSFPA_LADDERRADIUS)                                                 33
      CHARACTER CSFPA_LADDERRADIUS*40/' '/                                33
      CHARACTER NSFPA_LADDERRADIUS*12/'R.LADDERRADIUS'/                   33
      INTEGER MSFPA_LADDERRADIUS(2)/20,1/                                 33
      REAL RE_SFPA_LADDERRADIUS                                           33
C -                                                                       33
      INTEGER LEN_SFPA,LLEN_SFPA,LINK_SFPA                                33
      PARAMETER (LEN_SFPA=105)                                            33
      PARAMETER (LLEN_SFPA=27)                                            33
      INTEGER FLAG_SFPA/0/,MAP_SFPA(2,LLEN_SFPA),IDNUM_SFPA(10)           33
      CHARACTER NAMES_SFPA(LLEN_SFPA)*12,COMMEN_SFPA(LLEN_SFPA)*40        33
      DATA IDNUM_SFPA/10*0/                                               33
      REAL BANK_SFPA(LEN_SFPA)                                            33
      CHARACTER*60 BPATH_SFPA/' '/                                        33
      SAVE FLAG_SFPA,MAP_SFPA,IDNUM_SFPA,NAMES_SFPA,COMMEN_SFPA,          33
     *BPATH_SFPA                                                          33
      DATA LINK_SFPA/0/,BANK_SFPA/LEN_SFPA*0/                             33
C *                                                                       33
      INTEGER ILAD,IWAF,JWAF,NC                                           34
      REAL WAFPCKLEN,DTHK,RADTILT,ANG                                     36
      REAL ESSAI,HIGHT,LNG,LENG,YOFFSET                                   38
C *                                                                       40
      CALL AGSBEGM(MODULE,IPRIN)                                          40
      IF (IPRIN.LT.0) RETURN                                              40
C Filling bank SSDP - SILICON STRIPS                                      40
      BANK_TITLE='SILICON STRIPS'                                         40
      BPATH_SSDP='SSDP*'                                                  40
      IDNUM_SSDP(1)=0                                                     41
C VERSION                                                                 41
      SSDP_VERSION=1                                                      41
      CSSDP_VERSION='VERSION'                                             41
C THERE ARE A FEW CONFIGURAIONS POSSIBLE                                  42
      SSDP_CONFIG=1                                                       42
      CSSDP_CONFIG='THERE ARE A FEW CONFIGURAIONS POSSIBLE'               42
C 0=CAVE, 1=SVTT                                                          43
      SSDP_PLACEMENT=0                                                    43
      CSSDP_PLACEMENT='0=CAVE, 1=SVTT'                                    43
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSDP',    44
     *BPATH_SSDP,IDNUM_SSDP,1, MAP_SSDP,NAMES_SSDP,COMMEN_SSDP,           44
     *BANK_SSDP,LEN_SSDP,LLEN_SSDP,LINK_SSDP,FLAG_SSDP)                   44
C *                                                                       44
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSDP',    44
     *BPATH_SSDP,IDNUM_SSDP,1, MAP_SSDP,NAMES_SSDP,COMMEN_SSDP,           44
     *BANK_SSDP,LEN_SSDP,LLEN_SSDP,LINK_SSDP,FLAG_SSDP)                   44
C *                                                                       45
      CALL AGSBEGM(MODULE,IPRIN)                                          45
      IF (IPRIN.LT.0) RETURN                                              45
C Filling bank SFJP - DIFFERENT DIMENSION PARAMETERS                      45
      BANK_TITLE='DIFFERENT DIMENSION PARAMETERS'                         45
      BPATH_SFJP='SFJP*'                                                  45
      IDNUM_SFJP(1)=0                                                     46
C NEW VERSION INFORMATION                                                 46
      SFJP_VERSION=1                                                      46
      CSFJP_VERSION='NEW VERSION INFORMATION'                             46
C ALPHA OF THE SFLA AND OTHER PARTS                                       47
      SFJP_ALPHAZ=54                                                      47
      CSFJP_ALPHAZ='ALPHA OF THE SFLA AND OTHER PARTS'                    47
C ALPHA OF OTHER PARTS                                                    48
      SFJP_ALPHAZH=45                                                     48
      CSFJP_ALPHAZH='ALPHA OF OTHER PARTS'                                48
C MINIMUM RADIUS OF THE SMALL ALUMINIUM SECTOR (OUTSIDE STRUCTURE)        49
      SFJP_SSST_RMIN=31.285                                               49
      CSFJP_SSST_RMIN='MINIMUM RADIUS OF THE SMALL ALUMINIUM SECTOR (OUT  49
     *SIDE STRUCTURE)'                                                    49
C MAXIMUM RADIOS OF THE SMALL ALUMINIUM SECTOR (OUTSIDE STRUCTURE)        50
      SFJP_SSST_RMAX=31.8                                                 50
      CSFJP_SSST_RMAX='MAXIMUM RADIOS OF THE SMALL ALUMINIUM SECTOR (OUT  50
     *SIDE STRUCTURE)'                                                    50
C WIDTH OF THE SMALL ALUMINIUM SECTOR (OUTSIDE STRUCTURE)                 51
      SFJP_SSST_WIDTH=2.5                                                 51
      CSFJP_SSST_WIDTH='WIDTH OF THE SMALL ALUMINIUM SECTOR (OUTSIDE STR  51
     *UCTURE)'                                                            51
C ADDITIONAL Z-POSITION OF THE ALUMINIUM SECTORS                          52
      SFJP_SSST_PZ=0.2                                                    52
      CSFJP_SSST_PZ='ADDITIONAL Z-POSITION OF THE ALUMINIUM SECTORS'      52
C WIDTH OF THE SMALL INNER ALUMINIUM SECTOR (OUTSIDE STRUCTURE)           53
      SFJP_SSSS_RMIN=23.3                                                 53
      CSFJP_SSSS_RMIN='WIDTH OF THE SMALL INNER ALUMINIUM SECTOR (OUTSID  53
     *E STRUCTURE)'                                                       53
C WIDTH OF THE SMALL INNER ALUMINIUM SECTOR (OUTSIDE STRUCTURE)           54
      SFJP_SSSS_WIDTH=0.5                                                 54
      CSFJP_SSSS_WIDTH='WIDTH OF THE SMALL INNER ALUMINIUM SECTOR (OUTSI  54
     *DE STRUCTURE)'                                                      54
C WIDTH OF THE SIDE OF THE SMALL RIB (OUTSIDE STRUCTURE)                  55
      SFJP_SSRS_RMIN=28.0                                                 55
      CSFJP_SSRS_RMIN='WIDTH OF THE SIDE OF THE SMALL RIB (OUTSIDE STRUC  55
     *TURE)'                                                              55
C MOTHER VOLUME OF THE LINKING BOX (SECTOR TO THE CONE)                   56
      SFJP_SSLB_DX=8.5                                                    56
      CSFJP_SSLB_DX='MOTHER VOLUME OF THE LINKING BOX (SECTOR TO THE CON  56
     *E)'                                                                 56
C X-POSITION = Y-POSITION OF THE MOTHER VOLUME OF THE LINKING BOX         57
      SFJP_SSLB_PX=19.1                                                   57
      CSFJP_SSLB_PX='X-POSITION = Y-POSITION OF THE MOTHER VOLUME OF THE  57
     * LINKING BOX'                                                       57
C MOTHER VOLUME OF THE LINKING BOX (SECTOR TO THE CONE)                   58
      SFJP_SSLB_DY=7.3                                                    58
      CSFJP_SSLB_DY='MOTHER VOLUME OF THE LINKING BOX (SECTOR TO THE CON  58
     *E)'                                                                 58
C MOTHER VOLUME OF THE LINKING BOX (SECTOR TO THE CONE)                   59
      SFJP_SSLB_DZ=2.5                                                    59
      CSFJP_SSLB_DZ='MOTHER VOLUME OF THE LINKING BOX (SECTOR TO THE CON  59
     *E)'                                                                 59
C LINKING BOX (SECTOR TO THE CONE)                                        60
      SFJP_SSBQ_DX=7.8                                                    60
      CSFJP_SSBQ_DX='LINKING BOX (SECTOR TO THE CONE)'                    60
C LINKING BOX (SECTOR TO THE CONE)                                        61
      SFJP_SSBQ_DY=7.3                                                    61
      CSFJP_SSBQ_DY='LINKING BOX (SECTOR TO THE CONE)'                    61
C LINKING BOX (SECTOR TO THE CONE)                                        62
      SFJP_SSBQ_DZ=2.5                                                    62
      CSFJP_SSBQ_DZ='LINKING BOX (SECTOR TO THE CONE)'                    62
C LINKING CROSS TOTAL THICKNESS (ALUMINUM PART)                           63
      SFJP_SSCR_THA=1.5                                                   63
      CSFJP_SSCR_THA='LINKING CROSS TOTAL THICKNESS (ALUMINUM PART)'      63
C LINKING CROSS TOTAL THICKNESS (DELRIN PART)                             64
      SFJP_SSCR_THD=1.0                                                   64
      CSFJP_SSCR_THD='LINKING CROSS TOTAL THICKNESS (DELRIN PART)'        64
C LINKING CROSS TOTAL WIDTH                                               65
      SFJP_SSCR_WD=1.2                                                    65
      CSFJP_SSCR_WD='LINKING CROSS TOTAL WIDTH'                           65
C LINKING CROSS HORIZONTAL ARM FULL LENGTH                                66
      SFJP_SSCR_HL=8.5                                                    66
      CSFJP_SSCR_HL='LINKING CROSS HORIZONTAL ARM FULL LENGTH'            66
C LINKING CROSS VERTICAL ARM FULL LENGTH                                  67
      SFJP_SSCR_VL=1.6                                                    67
      CSFJP_SSCR_VL='LINKING CROSS VERTICAL ARM FULL LENGTH'              67
C X-POSITION = Y-POSITION OF THE LINKING TUBE                             68
      SFJP_SSLT_PX=18.1                                                   68
      CSFJP_SSLT_PX='X-POSITION = Y-POSITION OF THE LINKING TUBE'         68
C RADIUS OF THE LINKING TUBE (SECTOR TO THE CONE)                         69
      SFJP_SSLT_RMAX=0.8                                                  69
      CSFJP_SSLT_RMAX='RADIUS OF THE LINKING TUBE (SECTOR TO THE CONE)'   69
C WIDTH OF THE LINKING TUBE (SECTOR OF THE CONE)                          70
      SFJP_SSLT_DZ=5.05                                                   70
      CSFJP_SSLT_DZ='WIDTH OF THE LINKING TUBE (SECTOR OF THE CONE)'      70
C WIDTH OF THE MOUNTING PLATE INSERTED IN THE CONES                       71
      SFJP_SCMP_DX=6.3                                                    71
      CSFJP_SCMP_DX='WIDTH OF THE MOUNTING PLATE INSERTED IN THE CONES'   71
C WIDTH OF THE MOUNTING PLATE INSERTED IN THE CONES                       72
      SFJP_SCMP_DY=0.4                                                    72
      CSFJP_SCMP_DY='WIDTH OF THE MOUNTING PLATE INSERTED IN THE CONES'   72
C WIDTH OF THE MOUNTING PLATE INSERTED IN THE CONES                       73
      SFJP_SCMP_DZ=3.6                                                    73
      CSFJP_SCMP_DZ='WIDTH OF THE MOUNTING PLATE INSERTED IN THE CONES'   73
C X-POSITION = Y-POSITION OF THE MOUNTING PLATE                           74
      SFJP_SCMP_PX=15.945                                                 74
      CSFJP_SCMP_PX='X-POSITION = Y-POSITION OF THE MOUNTING PLATE'       74
C MOTHER VOLUME OF THE V-SHAPE PIECE LENGTH                               75
      SFJP_SCVM_DZ=3.6                                                    75
      CSFJP_SCVM_DZ='MOTHER VOLUME OF THE V-SHAPE PIECE LENGTH'           75
C MOTHER VOLUME OF THE V-SHAPE PIECE DX                                   76
      SFJP_SCVM_DX=6.4                                                    76
      CSFJP_SCVM_DX='MOTHER VOLUME OF THE V-SHAPE PIECE DX'               76
C X-POSITION = Y-POSITION OF THE V-SHAPE PIECE                            77
      SFJP_SCVM_PX=17.00                                                  77
      CSFJP_SCVM_PX='X-POSITION = Y-POSITION OF THE V-SHAPE PIECE'        77
C MOTHER VOLUME OF THE V-SHAPE PIECE DY                                   78
      SFJP_SCVM_DY=2.6                                                    78
      CSFJP_SCVM_DY='MOTHER VOLUME OF THE V-SHAPE PIECE DY'               78
C Z-POSITION OF THE MOTHER VOLUME                                         79
      SFJP_SCVM_PZ=57.5                                                   79
      CSFJP_SCVM_PZ='Z-POSITION OF THE MOTHER VOLUME'                     79
C HEIGHT OF THE BASE OF THE V-SHAPE PIECE                                 80
      SFJP_SCVB_DY=0.5                                                    80
      CSFJP_SCVB_DY='HEIGHT OF THE BASE OF THE V-SHAPE PIECE'             80
C DX OF THE V-PLATES OF THE V-SHAPE PIECE 45 DEGREES                      81
      SFJP_SCVS_DX=2.3                                                    81
      CSFJP_SCVS_DX='DX OF THE V-PLATES OF THE V-SHAPE PIECE 45           81
     *DEGREES'                                                            81
C MEDIUM HEIGHT OF THE V-PLATES OF THE V-SHAPE PIECE 45 DEGREES           82
      SFJP_SCVS_DY=0.6                                                    82
      CSFJP_SCVS_DY='MEDIUM HEIGHT OF THE V-PLATES OF THE V-SHAPE PIECE   82
     *45 DEGREES'                                                         82
C X-POSITION OF THE V-PLATES OF THE V-SHAPE PIECE 45 DEGREES              83
      SFJP_SCVS_PX=1.0                                                    83
      CSFJP_SCVS_PX='X-POSITION OF THE V-PLATES OF THE V-SHAPE PIECE 45   83
     *DEGREES'                                                            83
C X-POSITION OF THE V-PLATES OF THE V-SHAPE PIECE 45 DEGREES              84
      SFJP_SCVS_PY=0.22                                                   84
      CSFJP_SCVS_PY='X-POSITION OF THE V-PLATES OF THE V-SHAPE PIECE 45   84
     *DEGREES'                                                            84
C DX OF THE CONNECTION BOARD (END OF THE LADDER)                          85
      SFJP_SFCO_DX=4.4                                                    85
      CSFJP_SFCO_DX='DX OF THE CONNECTION BOARD (END OF THE LADDER)'      85
C WIDTH OF THE CONNECTION BOARD (END OF THE LADDER)                       86
      SFJP_SFCO_DY=0.37                                                   86
      CSFJP_SFCO_DY='WIDTH OF THE CONNECTION BOARD (END OF THE LADDER)'   86
C LENGTH OF THE CONNECTION BOARD (END OF THE LADDER)                      87
      SFJP_SFCO_DZ=15.8                                                   87
      CSFJP_SFCO_DZ='LENGTH OF THE CONNECTION BOARD (END OF THE           87
     *LADDER)'                                                            87
C X-POSITION OF THE CONNECTION BOARD (END OF THE LADDER)                  88
      SFJP_SFCO_PX=1.7                                                    88
      CSFJP_SFCO_PX='X-POSITION OF THE CONNECTION BOARD (END OF THE LADD  88
     *ER)'                                                                88
C Y-POSITION OF THE CONNECTION BOARD (END OF THE LADDER)                  89
      SFJP_SFCO_PY=0.2                                                    89
      CSFJP_SFCO_PY='Y-POSITION OF THE CONNECTION BOARD (END OF THE LADD  89
     *ER)'                                                                89
C DX OF THE SECOND CONNECTION BOARD (SMALLER THAN THE ABOVE ONE!!)        90
      SFJP_SFCM_DX=4.0                                                    90
      CSFJP_SFCM_DX='DX OF THE SECOND CONNECTION BOARD (SMALLER THAN THE  90
     * ABOVE ONE!!)'                                                      90
C DX OF THE SMALL PART OF THE SECOND CONNECTION BOARD                     91
      SFJP_SFCS_DZ=7.5                                                    91
      CSFJP_SFCS_DZ='DX OF THE SMALL PART OF THE SECOND CONNECTION BOARD  91
     *'                                                                   91
C WIDTH OF THE KAPTON FLEX CIRCUIT BETWEEN THE CONNECTION BOARDS          92
      SFJP_SFKF_DY=0.065                                                  92
      CSFJP_SFKF_DY='WIDTH OF THE KAPTON FLEX CIRCUIT BETWEEN THE CONNEC  92
     *TION BOARDS'                                                        92
C LENGTH OF THE KAPTON FLEX CIRCUIT BETWEEN THE CONNECTION BOARDS         93
      SFJP_SFKF_DZ=8.1                                                    93
      CSFJP_SFKF_DZ='LENGTH OF THE KAPTON FLEX CIRCUIT BETWEEN THE CONNE  93
     *CTION BOARDS'                                                       93
C DX OF THE FIRST PART OF THE KAPTON FLEX CIRCUIT                         94
      SFJP_SFKF_DX=0.44                                                   94
      CSFJP_SFKF_DX='DX OF THE FIRST PART OF THE KAPTON FLEX CIRCUIT'     94
C X-POSITION OF THE FIRST PART OF THE KAPTON FLEX CIRCUIT                 95
      SFJP_SFKF_PX=0.16                                                   95
      CSFJP_SFKF_PX='X-POSITION OF THE FIRST PART OF THE KAPTON FLEX CIR  95
     *CUIT'                                                               95
C Y-POSITION OF THE FIRST PART OF THE KAPTON FLEX CIRCUIT                 96
      SFJP_SFKF_PY=2.07                                                   96
      CSFJP_SFKF_PY='Y-POSITION OF THE FIRST PART OF THE KAPTON FLEX CIR  96
     *CUIT'                                                               96
C DX OF THE SECOND PART OF THE KAPTON FLEX CIRCUIT                        97
      SFJP_SFKS_DX=1.65                                                   97
      CSFJP_SFKS_DX='DX OF THE SECOND PART OF THE KAPTON FLEX CIRCUIT'    97
C X-POSITION OF THE SECOND PART OT THE KAPTON FLEX                        98
      SFJP_SFKS_PX=0.505                                                  98
      CSFJP_SFKS_PX='X-POSITION OF THE SECOND PART OT THE KAPTON FLEX'    98
C Y-POSITION OF THE SECOND PART OT THE KAPTON FLEX                        99
      SFJP_SFKS_PY=1.585                                                  99
      CSFJP_SFKS_PY='Y-POSITION OF THE SECOND PART OT THE KAPTON FLEX'    99
C Y-POSITION OF THE MECHANICAL PARTS                                      100
      SFJP_SFPR_PY=1.79                                                   100
      CSFJP_SFPR_PY='Y-POSITION OF THE MECHANICAL PARTS'                  100
C Y-POSITION OF THE MECHANICAL PARTS 2                                    101
      SFJP_SFPB_PY=3.48                                                   101
      CSFJP_SFPB_PY='Y-POSITION OF THE MECHANICAL PARTS 2'                101
C Y-POSITION OF THE MECHANICAL PARTS 2                                    102
      SFJP_SFPB_PY2=0.08                                                  102
      CSFJP_SFPB_PY2='Y-POSITION OF THE MECHANICAL PARTS 2'               102
C DX OF THE BIG PART OF THE SECOND CONNECTION BOARD                       103
      SFJP_SFCB_DX=3.0                                                    103
      CSFJP_SFCB_DX='DX OF THE BIG PART OF THE SECOND CONNECTION BOARD'   103
C WIDTH OF THE ALUMINIUM PLATES LINKING THE LADDER TO THE SECTOR          104
      SFJP_SSBS_DY=0.5                                                    104
      CSFJP_SSBS_DY='WIDTH OF THE ALUMINIUM PLATES LINKING THE LADDER TO  104
     * THE SECTOR'                                                        104
C DX = DZ = SMALL PART OF THE ALUMINIUM PLATE LINKING THE LADDER          105
      SFJP_SSBS_DX=2.5                                                    105
      CSFJP_SSBS_DX='DX = DZ = SMALL PART OF THE ALUMINIUM PLATE LINKING  105
     * THE LADDER'                                                        105
C DX OF THE BIG PART OF THE ALUMINIUM PLATE LINKING THE LADDER            106
      SFJP_SSBB_DX=4.3                                                    106
      CSFJP_SSBB_DX='DX OF THE BIG PART OF THE ALUMINIUM PLATE LINKING T  106
     *HE LADDER'                                                          106
C DZ OF THE BIG PART OF THE ALUMINIUM PLATE LINKING THE LADDER            107
      SFJP_SSBB_DZ=1.9                                                    107
      CSFJP_SSBB_DZ='DZ OF THE BIG PART OF THE ALUMINIUM PLATE LINKING T  107
     *HE LADDER'                                                          107
C DISTANZE BETWEEN OVERLAYING FLEXES                                      108
      SFJP_FLEX_DI=0.02                                                   108
      CSFJP_FLEX_DI='DISTANZE BETWEEN OVERLAYING FLEXES'                  108
C DX OF THE OUTSIDE MECHANICAL PART (G10 RECTANGLE - BOX BASE)            109
      SFJP_SFPB_DX=4.3                                                    109
      CSFJP_SFPB_DX='DX OF THE OUTSIDE MECHANICAL PART (G10 RECTANGLE -   109
     *BOX BASE)'                                                          109
C WIDTH OF THE OUTSIDE MECHANICAL PART (G10 RECTANGLE - BOX BASE)         110
      SFJP_SFPB_DY=0.8                                                    110
      CSFJP_SFPB_DY='WIDTH OF THE OUTSIDE MECHANICAL PART (G10 RECTANGLE  110
     * - BOX BASE)'                                                       110
C LENGTH OF THE OUTSIDE MECHANICAL PART (G10 RECTANGLE - BOX BASE)        111
      SFJP_SFPBDZ=0.95                                                    111
      CSFJP_SFPBDZ='LENGTH OF THE OUTSIDE MECHANICAL PART (G10 RECTANGLE  111
     * - BOX BASE)'                                                       111
C DX OF THE EPOXY CORE OF THE ADC BOARD APPENDICE MOTHER VOLUME           112
      SFJP_SAPP_DXE=7.6                                                   112
      CSFJP_SAPP_DXE='DX OF THE EPOXY CORE OF THE ADC BOARD APPENDICE MO  112
     *THER VOLUME'                                                        112
C DX OF THE CARBON SIDE SHELL OF THE ADC BOARD APPENDICE                  113
      SFJP_SAPP_DXZ=0.04                                                  113
      CSFJP_SAPP_DXZ='DX OF THE CARBON SIDE SHELL OF THE ADC BOARD APPEN  113
     *DICE'                                                               113
C WIDTH OF THE EPOXY CORE OF THE ADC BOARD APPENDICE MOTHER VOLUME        114
      SFJP_SAPP_DY=0.5                                                    114
      CSFJP_SAPP_DY='WIDTH OF THE EPOXY CORE OF THE ADC BOARD APPENDICE   114
     *MOTHER VOLUME'                                                      114
C LENGTH OF THE ADC BOARD APPENDICE MOTHER VOLUME                         115
      SFJP_SAPP_DZ=1.275                                                  115
      CSFJP_SAPP_DZ='LENGTH OF THE ADC BOARD APPENDICE MOTHER VOLUME'     115
C 1. Y-POSITION OF THE MOTHER COLUME OF THE ADC BOARD                     116
      SFJP_SAPP_PY1=0.04                                                  116
      CSFJP_SAPP_PY1='1. Y-POSITION OF THE MOTHER COLUME OF THE ADC BOAR  116
     *D'                                                                  116
C 2. Y-POSITION OF THE MOTHER COLUME OF THE ADC BOARD                     117
      SFJP_SAPP_PY2=0.5                                                   117
      CSFJP_SAPP_PY2='2. Y-POSITION OF THE MOTHER COLUME OF THE ADC BOAR  117
     *D'                                                                  117
C 3. Y-POSITION OF THE MOTHER COLUME OF THE ADC BOARD                     118
      SFJP_SAPP_PY3=0.08                                                  118
      CSFJP_SAPP_PY3='3. Y-POSITION OF THE MOTHER COLUME OF THE ADC BOAR  118
     *D'                                                                  118
C DX OF THE MOTHER VOLUME OF THE ADC BOARD                                119
      SFJP_SFAM_DXE=7.2                                                   119
      CSFJP_SFAM_DXE='DX OF THE MOTHER VOLUME OF THE ADC BOARD'           119
C DX OF THE SMALL VOLUME OF THE ADC BOARD                                 120
      SFJP_SFAM_DXZ=2.1                                                   120
      CSFJP_SFAM_DXZ='DX OF THE SMALL VOLUME OF THE ADC BOARD'            120
C WIDTH OF THE MOTHER VOLUME OF THE ADC BOARD                             121
      SFJP_SFAM_DY=0.3                                                    121
      CSFJP_SFAM_DY='WIDTH OF THE MOTHER VOLUME OF THE ADC BOARD'         121
C LENGTH OF THE MOTHER VOLUME OF THE ADC BOARD                            122
      SFJP_SFAM_DZ=12.4                                                   122
      CSFJP_SFAM_DZ='LENGTH OF THE MOTHER VOLUME OF THE ADC BOARD'        122
C LENGTH OF THE SMALL VOLUME OF THE ADC BOARD                             123
      SFJP_SFAM_DZS=2.15                                                  123
      CSFJP_SFAM_DZS='LENGTH OF THE SMALL VOLUME OF THE ADC BOARD'        123
C DX OF THE LONG PART OF THE BUS CABLE LINKING THE MODULES                124
      SFJP_SFLA_DX=1.5                                                    124
      CSFJP_SFLA_DX='DX OF THE LONG PART OF THE BUS CABLE LINKING THE MO  124
     *DULES'                                                              124
C WIDTH OF THE LONG PART OF THE BUS CABLE LINKING THE MODULES             125
      SFJP_SFLA_DY=0.0130                                                 125
      CSFJP_SFLA_DY='WIDTH OF THE LONG PART OF THE BUS CABLE LINKING THE  125
     * MODULES'                                                           125
C LENGTH OF THE LONG PART OF THE BUS CABLE LINKING THE MODULES            126
      SFJP_SFLA_DZ=4.3                                                    126
      CSFJP_SFLA_DZ='LENGTH OF THE LONG PART OF THE BUS CABLE LINKING TH  126
     *E MODULES'                                                          126
C LENGTH OF THE LONG BUS CABLE                                            127
      SFJP_SFLB_DZ=7.5                                                    127
      CSFJP_SFLB_DZ='LENGTH OF THE LONG BUS CABLE'                        127
C LENGTH OF THE LONG BUS CABLE ON THE CB UP TO THE CONNECTOR              128
      SFJP_SFLC_DZ=0.93                                                   128
      CSFJP_SFLC_DZ='LENGTH OF THE LONG BUS CABLE ON THE CB UP TO THE CO  128
     *NNECTOR'                                                            128
C DX OF THE BIG BUS ELBOW                                                 129
      SFJP_SFEB_DX=3.89                                                   129
      CSFJP_SFEB_DX='DX OF THE BIG BUS ELBOW'                             129
C LENGTH OF THE BUS ELBOW                                                 130
      SFJP_SFEB_DZ=1.5                                                    130
      CSFJP_SFEB_DZ='LENGTH OF THE BUS ELBOW'                             130
C DX OF THE BIG BUS ELBOW                                                 131
      SFJP_SFES_DX=1.875                                                  131
      CSFJP_SFES_DX='DX OF THE BIG BUS ELBOW'                             131
C DX OF THE HORIZONTAL PART OF THE LADDER SKELETON CARBON BASE            132
      SFJP_SFFK_DXE=0.4                                                   132
      CSFJP_SFFK_DXE='DX OF THE HORIZONTAL PART OF THE LADDER SKELETON C  132
     *ARBON BASE'                                                         132
C WIDTH OF THE HORIZONTAL PART OF THE LADDER SKELETON CARBON BASE         133
      SFJP_SFFK_DY=0.02                                                   133
      CSFJP_SFFK_DY='WIDTH OF THE HORIZONTAL PART OF THE LADDER SKELETON  133
     * CARBON BASE'                                                       133
C LENGTHOF OF THE HORIZONTAL PART OF THE LADDER SKELETON CARBON BASE      134
      SFJP_SFFK_DZ=69.75                                                  134
      CSFJP_SFFK_DZ='LENGTHOF OF THE HORIZONTAL PART OF THE LADDER SKELE  134
     *TON CARBON BASE'                                                    134
C DX OF THE TILTED PART OF THE LADDER                                     135
      SFJP_SFFL_DX=0.6                                                    135
      CSFJP_SFFL_DX='DX OF THE TILTED PART OF THE LADDER'                 135
C DX OF THE HORIZONTAL PART OF THE KAPTON FILM                            136
      SFJP_SFFK_DXZ=5.1                                                   136
      CSFJP_SFFK_DXZ='DX OF THE HORIZONTAL PART OF THE KAPTON FILM'       136
C X-POSITION OF THE CARBON BASE UNDER THE LADDER                          137
      SFJP_SFFK_PX=3.5                                                    137
      CSFJP_SFFK_PX='X-POSITION OF THE CARBON BASE UNDER THE LADDER'      137
C 1. Y-POSITION OF THE CARBON BASE UNDER THE LADDER                       138
      SFJP_SFFK_PY1=0.02                                                  138
      CSFJP_SFFK_PY1='1. Y-POSITION OF THE CARBON BASE UNDER THE          138
     *LADDER'                                                             138
C 2. Y-POSITION OF THE CARBON BASE UNDER THE LADDER                       139
      SFJP_SFFK_PY2=0.04                                                  139
      CSFJP_SFFK_PY2='2. Y-POSITION OF THE CARBON BASE UNDER THE          139
     *LADDER'                                                             139
C X-POSITION OF THE TILTED KAPTON FILM UNDER THE LADDER                   140
      SFJP_SFKL_PX=2.55                                                   140
      CSFJP_SFKL_PX='X-POSITION OF THE TILTED KAPTON FILM UNDER THE LADD  140
     *ER'                                                                 140
C WIDTH OF THE KAPTON FILM                                                141
      SFJP_SFKK_DY=0.0025                                                 141
      CSFJP_SFKK_DY='WIDTH OF THE KAPTON FILM'                            141
C LENGTH OF THE TRIANGULAR LADDER SKELETON                                142
      SFJP_SFLU_DZ=49.8                                                   142
      CSFJP_SFLU_DZ='LENGTH OF THE TRIANGULAR LADDER SKELETON'            142
C HEIGHT OF THE TRIANGULAR LADDER SKELETON                                143
      SFJP_SFLU_H1=0.02                                                   143
      CSFJP_SFLU_H1='HEIGHT OF THE TRIANGULAR LADDER SKELETON'            143
C BASE LENGTH OF THE TRIANGULAR LADDER SKELETON                           144
      SFJP_SFLU_BL1=0.2                                                   144
      CSFJP_SFLU_BL1='BASE LENGTH OF THE TRIANGULAR LADDER SKELETON'      144
C DX OF THE HYBRID STIFFNER                                               145
      SFJP_SFRA_DX=3.76                                                   145
      CSFJP_SFRA_DX='DX OF THE HYBRID STIFFNER'                           145
C WIDTH OF THE HYBRID STIFFNER                                            146
      SFJP_SFRA_DY=0.019                                                  146
      CSFJP_SFRA_DY='WIDTH OF THE HYBRID STIFFNER'                        146
C Y-POSITION OF THE HYBRID STIFFNER                                       147
      SFJP_SFRA_PY=0.099                                                  147
      CSFJP_SFRA_PY='Y-POSITION OF THE HYBRID STIFFNER'                   147
C LENGTH OF THE HYBRID STIFFNER                                           148
      SFJP_SFRA_DZ=1.0                                                    148
      CSFJP_SFRA_DZ='LENGTH OF THE HYBRID STIFFNER'                       148
C Z-POSITION OF THE HYBRID STIFFNER                                       149
      SFJP_SFRA_PZ=1.1                                                    149
      CSFJP_SFRA_PZ='Z-POSITION OF THE HYBRID STIFFNER'                   149
C WIDTH OF A SINGLE WAFER CONTAINER                                       150
      SFJP_SFSW_DY=1.0                                                    150
      CSFJP_SFSW_DY='WIDTH OF A SINGLE WAFER CONTAINER'                   150
C LENGTHOF OF A SINGLE WAFER CONTAINER                                    151
      SFJP_SFSW_DZ=2.25                                                   151
      CSFJP_SFSW_DZ='LENGTHOF OF A SINGLE WAFER CONTAINER'                151
C DX OF THE TWO SUPPORTS OF THE HYBRID STIFFNER                           152
      SFJP_SFRS_DX=0.04                                                   152
      CSFJP_SFRS_DX='DX OF THE TWO SUPPORTS OF THE HYBRID STIFFNER'       152
C DY OF THE TWO SUPPORTS OF THE HYBRID STIFFNER                           153
      SFJP_SFRS_DY=0.0915                                                 153
      CSFJP_SFRS_DY='DY OF THE TWO SUPPORTS OF THE HYBRID STIFFNER'       153
C X-POSITION OF THE TWO SUPPORTS OF THE HYBRID STIFFNER                   154
      SFJP_SFRS_PX=3.8                                                    154
      CSFJP_SFRS_PX='X-POSITION OF THE TWO SUPPORTS OF THE HYBRID STIFFN  154
     *ER'                                                                 154
C Y-POSITION OF THE TWO SUPPORTS OF THE HYBRID STIFFNER                   155
      SFJP_SFRS_PY=0.0265                                                 155
      CSFJP_SFRS_PY='Y-POSITION OF THE TWO SUPPORTS OF THE HYBRID STIFFN  155
     *ER'                                                                 155
C LENGTH OF THE TRIANGULAR BASE                                           156
      SFJP_SFSM_LL=2.6                                                    156
      CSFJP_SFSM_LL='LENGTH OF THE TRIANGULAR BASE'                       156
C DX OF THE FLEX                                                          157
      SFJP_SFFX_DX=3.84                                                   157
      CSFJP_SFFX_DX='DX OF THE FLEX'                                      157
C WIDTH OF THE FLEX                                                       158
      SFJP_SFFX_DYZ=0.0007                                                158
      CSFJP_SFFX_DYZ='WIDTH OF THE FLEX'                                  158
C RMIN OF THE PIONS                                                       159
      SFJP_SFPI_RMIN=0.15                                                 159
      CSFJP_SFPI_RMIN='RMIN OF THE PIONS'                                 159
C RMAX OF THE PIONS                                                       160
      SFJP_SFPI_RMAX=0.2                                                  160
      CSFJP_SFPI_RMAX='RMAX OF THE PIONS'                                 160
C X-POSITION OF THE FOUR PIONS                                            161
      SFJP_SFPI_PX=3.2                                                    161
      CSFJP_SFPI_PX='X-POSITION OF THE FOUR PIONS'                        161
C 1. Y-POSITION OF THE FOUR PIONS                                         162
      SFJP_SFPI_PY1=0.35                                                  162
      CSFJP_SFPI_PY1='1. Y-POSITION OF THE FOUR PIONS'                    162
C 2. Y-POSITION OF THE FOUR PIONS                                         163
      SFJP_SFPI_PY2=0.025                                                 163
      CSFJP_SFPI_PY2='2. Y-POSITION OF THE FOUR PIONS'                    163
C Z-POSITION OF THE FOUR PIONS                                            164
      SFJP_SFPI_PZ=0.7                                                    164
      CSFJP_SFPI_PZ='Z-POSITION OF THE FOUR PIONS'                        164
C 1. DX OF THE BASE OF THE PIONS                                          165
      SFJP_SFPJ_DX1=0.075                                                 165
      CSFJP_SFPJ_DX1='1. DX OF THE BASE OF THE PIONS'                     165
C 2. DX OF THE BASE OF THE PIONS                                          166
      SFJP_SFPJ_DX2=0.025                                                 166
      CSFJP_SFPJ_DX2='2. DX OF THE BASE OF THE PIONS'                     166
C DY OF THE BASE OF THE PIONS                                             167
      SFJP_SFPJ_DY=0.3                                                    167
      CSFJP_SFPJ_DY='DY OF THE BASE OF THE PIONS'                         167
C DZ OF THE BASE OF THE PIONS                                             168
      SFJP_SFPJ_DZ=0.0125                                                 168
      CSFJP_SFPJ_DZ='DZ OF THE BASE OF THE PIONS'                         168
C DX OF THE A128C CHIP                                                    169
      SFJP_SFAA_DX=0.3                                                    169
      CSFJP_SFAA_DX='DX OF THE A128C CHIP'                                169
C DY OF THE A128C CHIP                                                    170
      SFJP_SFAA_DY=0.015                                                  170
      CSFJP_SFAA_DY='DY OF THE A128C CHIP'                                170
C DZ OF THE A128C CHIP                                                    171
      SFJP_SFAA_DZ=0.4                                                    171
      CSFJP_SFAA_DZ='DZ OF THE A128C CHIP'                                171
C 1. X=^FYLL SFJP 01-0 SFAA_PX1 = 0.325 ! 1. X-POSITION OF THE A128C CHI  172
C PS                                                                      172
      SFJP_SFAA_PX1=0.325                                                 172
      CSFJP_SFAA_PX1='1. X=^FYLL SFJP 01-0 SFAA_PX1 = 0.325 ! 1. X-POSIT  172
     *ION OF THE A128C CHIPS'                                             172
C 2. X=^FYLL SFJP 01-0 SFAA_PX2 = 0.65 ! 2. X-POSITION OF THE A128C CHIP  173
C S                                                                       173
      SFJP_SFAA_PX2=0.65                                                  173
      CSFJP_SFAA_PX2='2. X=^FYLL SFJP 01-0 SFAA_PX2 = 0.65 ! 2. X-POSITI  173
     *ON OF THE A128C CHIPS'                                              173
C 3. X=^FYLL SFJP 01-0 SFAA_PX3 = 0.6 ! 3. X-POSITION OF THE A128C CHIPS  174
      SFJP_SFAA_PX3=0.6                                                   174
      CSFJP_SFAA_PX3='3. X=^FYLL SFJP 01-0 SFAA_PX3 = 0.6 ! 3. X-POSITIO  174
     *N OF THE A128C CHIPS'                                               174
C 1. Z=^FYLL SFJP 01-0 SFAA_PZ1 = 1.1 ! 1. Z-POSITION OF THE A128C CHIPS  175
      SFJP_SFAA_PZ1=1.1                                                   175
      CSFJP_SFAA_PZ1='1. Z=^FYLL SFJP 01-0 SFAA_PZ1 = 1.1 ! 1. Z-POSITIO  175
     *N OF THE A128C CHIPS'                                               175
C 2. Z=^FYLL SFJP 01-0 SFAA_PZ2 = 1.0 ! 2. Z-POSITION OF THE A128C CHIPS  176
      SFJP_SFAA_PZ2=1.0                                                   176
      CSFJP_SFAA_PZ2='2. Z=^FYLL SFJP 01-0 SFAA_PZ2 = 1.0 ! 2. Z-POSITIO  176
     *N OF THE A128C CHIPS'                                               176
C 3. Z=^FYLL SFJP 01-0 SFAA_PZ3 = 0.02 ! 3. Z-POSITION OF THE A128C CHIP  177
C S                                                                       177
      SFJP_SFAA_PZ3=0.02                                                  177
      CSFJP_SFAA_PZ3='3. Z=^FYLL SFJP 01-0 SFAA_PZ3 = 0.02 ! 3. Z-POSITI  177
     *ON OF THE A128C CHIPS'                                              177
C DX OF THE STRIP DETECTOR                                                178
      SFJP_SFSL_DX=3.75                                                   178
      CSFJP_SFSL_DX='DX OF THE STRIP DETECTOR'                            178
C DY OF THE STRIP DETECTOR                                                179
      SFJP_SFSL_DY=0.015                                                  179
      CSFJP_SFSL_DY='DY OF THE STRIP DETECTOR'                            179
C DZ OF THE STRIP DETECTOR                                                180
      SFJP_SFSL_DZ=2.1                                                    180
      CSFJP_SFSL_DZ='DZ OF THE STRIP DETECTOR'                            180
C DX OF THE SENSITIVE AREA                                                181
      SFJP_SFSD_DX=3.65                                                   181
      CSFJP_SFSD_DX='DX OF THE SENSITIVE AREA'                            181
C DY OF THE SENSITIVE AREA                                                182
      SFJP_SFSD_DY=0.015                                                  182
      CSFJP_SFSD_DY='DY OF THE SENSITIVE AREA'                            182
C DZ OF THE SENSITIVE AREA                                                183
      SFJP_SFSD_DZ=2.0                                                    183
      CSFJP_SFSD_DZ='DZ OF THE SENSITIVE AREA'                            183
C X-POSITION OF THE TOP FLEX ALONG THE Z-AXIS                             184
      SFJP_SFLA_PX=1.1                                                    184
      CSFJP_SFLA_PX='X-POSITION OF THE TOP FLEX ALONG THE Z-AXIS'         184
C Y-POSITION OF THE TOP FLEX ALONG THE Z-AXIS                             185
      SFJP_SFLA_PY=1.35                                                   185
      CSFJP_SFLA_PY='Y-POSITION OF THE TOP FLEX ALONG THE Z-AXIS'         185
C X-POSITION OF THE BOTTOM FLEX ALONG THE Z-AXIS                          186
      SFJP_SFLC_PX=2.3                                                    186
      CSFJP_SFLC_PX='X-POSITION OF THE BOTTOM FLEX ALONG THE Z-AXIS'      186
C Y-POSITION OF THE BOTTOM FLEX ALONG THE Z-AXIS                          187
      SFJP_SFLC_PY=0.3                                                    187
      CSFJP_SFLC_PY='Y-POSITION OF THE BOTTOM FLEX ALONG THE Z-AXIS'      187
C X-POSITION OF THE SMALL ELBOW BUS                                       188
      SFJP_SFES_PX=3.32                                                   188
      CSFJP_SFES_PX='X-POSITION OF THE SMALL ELBOW BUS'                   188
C Y-POSITION OF THE SMALL ELBOW BUS                                       189
      SFJP_SFES_PY=1.6                                                    189
      CSFJP_SFES_PY='Y-POSITION OF THE SMALL ELBOW BUS'                   189
C Z-POSITION OF THE SMALL ELBOW BUS                                       190
      SFJP_SFES_PZ=0.1                                                    190
      CSFJP_SFES_PZ='Z-POSITION OF THE SMALL ELBOW BUS'                   190
C X-POSITION OF THE BIG ELBOW BUS                                         191
      SFJP_SFEB_PX=2.71                                                   191
      CSFJP_SFEB_PX='X-POSITION OF THE BIG ELBOW BUS'                     191
C Y-POSITION OF THE BIG ELBOW BUS                                         192
      SFJP_SFEB_PY=0.75                                                   192
      CSFJP_SFEB_PY='Y-POSITION OF THE BIG ELBOW BUS'                     192
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFJP',    193
     *BPATH_SFJP,IDNUM_SFJP,1, MAP_SFJP,NAMES_SFJP,COMMEN_SFJP,           193
     *BANK_SFJP,LEN_SFJP,LLEN_SFJP,LINK_SFJP,FLAG_SFJP)                   193
C *                                                                       193
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFJP',    193
     *BPATH_SFJP,IDNUM_SFJP,1, MAP_SFJP,NAMES_SFJP,COMMEN_SFJP,           193
     *BANK_SFJP,LEN_SFJP,LLEN_SFJP,LINK_SFJP,FLAG_SFJP)                   193
C *                                                                       194
      CALL AGSBEGM(MODULE,IPRIN)                                          194
      IF (IPRIN.LT.0) RETURN                                              194
C Filling bank SFJP - DIFFERENT DIMENSION PARAMETERS                      194
      BANK_TITLE='DIFFERENT DIMENSION PARAMETERS'                         194
      BPATH_SFJP='SFJP*'                                                  194
      IDNUM_SFJP(1)=0                                                     195
C VERSION                                                                 195
      SFJP_VERSION=2                                                      195
      CSFJP_VERSION='VERSION'                                             195
C WIDTH OF THE SIDE OF THE SMALL RIB (OUTSIDE STRUCTURE)                  196
      SFJP_SSRS_RMIN=23.3                                                 196
      CSFJP_SSRS_RMIN='WIDTH OF THE SIDE OF THE SMALL RIB (OUTSIDE STRUC  196
     *TURE)'                                                              196
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFJP',    197
     *BPATH_SFJP,IDNUM_SFJP,1, MAP_SFJP,NAMES_SFJP,COMMEN_SFJP,           197
     *BANK_SFJP,LEN_SFJP,LLEN_SFJP,LINK_SFJP,FLAG_SFJP)                   197
C *                                                                       197
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFJP',    197
     *BPATH_SFJP,IDNUM_SFJP,1, MAP_SFJP,NAMES_SFJP,COMMEN_SFJP,           197
     *BANK_SFJP,LEN_SFJP,LLEN_SFJP,LINK_SFJP,FLAG_SFJP)                   197
C *                                                                       198
      CALL AGSBEGM(MODULE,IPRIN)                                          198
      IF (IPRIN.LT.0) RETURN                                              198
C Filling bank SFPB - SOME SSD SHELL DIMENSIONS                           198
      BANK_TITLE='SOME SSD SHELL DIMENSIONS'                              198
      BPATH_SFPB='SFPB*'                                                  198
      IDNUM_SFPB(1)=0                                                     199
C HAUT 1                                                                  199
      SFPB_HHIGHT=(0.02/TAN(54*PI/180)+0.02/(2*TAN(PI/5))+0.2)*SIN(PI/    199
     *5)                                                                  199
      CSFPB_HHIGHT='HAUT 1'                                               199
C HAUT 2                                                                  200
      SFPB_KHIGHT=0.02/SIN(54*PI/180)                                     200
      CSFPB_KHIGHT='HAUT 2'                                               200
C BAS 1                                                                   201
      SFPB_HBASE=(0.02/TAN(63*PI/180)+0.01/TAN(27*PI/                     201
     *180)+0.2)*SIN(27*PI/180)                                            201
      CSFPB_HBASE='BAS 1'                                                 201
C BAS 2                                                                   202
      SFPB_KBASE=0.02/SIN(63*PI/180)                                      202
      CSFPB_KBASE='BAS 2'                                                 202
C LENGHT OF THE SIDE KAPTON FILM                                          203
      SFPB_FSIZE=0.6/COS(15*PI/180)+0.02*TAN(15*PI/180)                   203
      CSFPB_FSIZE='LENGHT OF THE SIDE KAPTON FILM'                        203
C Z COORD                                                                 204
      SFPB_ZCOOR=5.18462095393834410                                      204
      CSFPB_ZCOOR='Z COORD'                                               204
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPB',    205
     *BPATH_SFPB,IDNUM_SFPB,1, MAP_SFPB,NAMES_SFPB,COMMEN_SFPB,           205
     *BANK_SFPB,LEN_SFPB,LLEN_SFPB,LINK_SFPB,FLAG_SFPB)                   205
C *                                                                       205
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPB',    205
     *BPATH_SFPB,IDNUM_SFPB,1, MAP_SFPB,NAMES_SFPB,COMMEN_SFPB,           205
     *BANK_SFPB,LEN_SFPB,LLEN_SFPB,LINK_SFPB,FLAG_SFPB)                   205
C *                                                                       206
      CALL AGSBEGM(MODULE,IPRIN)                                          206
      IF (IPRIN.LT.0) RETURN                                              206
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   206
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      206
      BPATH_SFPA='SFPA*'                                                  206
      IDNUM_SFPA(1)=0                                                     207
C GEOMETRY VERSION                                                        207
      SFPA_VERSION=1                                                      207
      CSFPA_VERSION='GEOMETRY VERSION'                                    207
C MOTHER RMIN                                                             208
      SFPA_RMIN=21.8                                                      208
      CSFPA_RMIN='MOTHER RMIN'                                            208
C MOTHER RMAX                                                             209
      SFPA_RMAX=32.                                                       209
      CSFPA_RMAX='MOTHER RMAX'                                            209
C OUTER SHIELD CYLINDER, INNER RADIUS                                     210
      SFPA_SOUTRINN=29.5                                                  210
      CSFPA_SOUTRINN='OUTER SHIELD CYLINDER, INNER RADIUS'                210
C OUTER SHIELD CYLINDER, OUTER RADIUS                                     211
      SFPA_SOUTROUT=29.52                                                 211
      CSFPA_SOUTROUT='OUTER SHIELD CYLINDER, OUTER RADIUS'                211
C OUTER SHIELD CYLINDER, HALF LENGTH                                      212
      SFPA_SOUTLEN=65.4                                                   212
      CSFPA_SOUTLEN='OUTER SHIELD CYLINDER, HALF LENGTH'                  212
C MOTHER LEN ALONG THE Z DIRECTION                                        213
      SFPA_LEN=107.                                                       213
      CSFPA_LEN='MOTHER LEN ALONG THE Z DIRECTION'                        213
C DISTANCE FROM BEAM AXIS TO DETECTOR CENTER                              214
      SFPA_RAD=23.                                                        214
      CSFPA_RAD='DISTANCE FROM BEAM AXIS TO DETECTOR CENTER'              214
C NUMBER OF SILICON STRIP DETECTORS                                       215
      SFPA_NSSD=16                                                        215
      CSFPA_NSSD='NUMBER OF SILICON STRIP DETECTORS'                      215
C DETECTOR MOTHER WIDTH                                                   216
      SFPA_DMWID=7.8                                                      216
      CSFPA_DMWID='DETECTOR MOTHER WIDTH'                                 216
C DETECTOR MOTHER THICKNESS                                               217
      SFPA_DMTHK=2.0                                                      217
      CSFPA_DMTHK='DETECTOR MOTHER THICKNESS'                             217
C DETECTOR MOTHER LENGTH (DETECTORS + ADC BOARD)                          218
      SFPA_DMLEN=90.                                                      218
      CSFPA_DMLEN='DETECTOR MOTHER LENGTH (DETECTORS + ADC BOARD)'        218
C STRUCTURE MOTHER WIDTH                                                  219
      SFPA_SMWID=7.5                                                      219
      CSFPA_SMWID='STRUCTURE MOTHER WIDTH'                                219
C STRUCTURE MOTHER THICKNESS                                              220
      SFPA_SMTHK=5.0                                                      220
      CSFPA_SMTHK='STRUCTURE MOTHER THICKNESS'                            220
C STRUCTURE MOTHER LENGTH                                                 221
      SFPA_SMLEN=101.9                                                    221
      CSFPA_SMLEN='STRUCTURE MOTHER LENGTH'                               221
C LENGTH OF A SUBVOLUME OF THE STRUCTURE                                  222
      SFPA_SSLEN=95./20.                                                  222
      CSFPA_SSLEN='LENGTH OF A SUBVOLUME OF THE STRUCTURE'                222
C LENGTH OF WAFER PACK                                                    223
      SFPA_WPLEN=69.6                                                     223
      CSFPA_WPLEN='LENGTH OF WAFER PACK'                                  223
C LENGTH OF ONE STRIP DETECTOR (ALONG BEAM AXIS)                          224
      SFPA_SDLEN=4.2                                                      224
      CSFPA_SDLEN='LENGTH OF ONE STRIP DETECTOR (ALONG BEAM AXIS)'        224
C TILING ANGLE (DEGREES)                                                  225
      SFPA_TILT=5.0                                                       225
      CSFPA_TILT='TILING ANGLE (DEGREES)'                                 225
C COOLING PIPE OUTER RADIUS                                               226
      SFPA_CPRAD=0.1                                                      226
      CSFPA_CPRAD='COOLING PIPE OUTER RADIUS'                             226
C COOLING PIPE INNER RADIUS                                               227
      SFPA_CPRAL=0.09                                                     227
      CSFPA_CPRAL='COOLING PIPE INNER RADIUS'                             227
C CARBON FIBER TUBE RADIUS (SUPPORT STRUCTURE)                            228
      SFPA_CFRAD=0.1                                                      228
      CSFPA_CFRAD='CARBON FIBER TUBE RADIUS (SUPPORT STRUCTURE)'          228
C GAP BETWEEN STRUCTURE MOTHER AND DETECTOR                               229
      SFPA_GPTHK=-1.0                                                     229
      CSFPA_GPTHK='GAP BETWEEN STRUCTURE MOTHER AND DETECTOR'             229
C PRESENCE OF LADDERS                                                     231
      SFPA_LADDERMAP(1)=0                                                 231
      SFPA_LADDERMAP(2)=0                                                 231
      SFPA_LADDERMAP(3)=1                                                 231
      SFPA_LADDERMAP(4)=0                                                 231
      SFPA_LADDERMAP(5)=0                                                 231
      SFPA_LADDERMAP(6)=0                                                 231
      SFPA_LADDERMAP(7)=0                                                 231
      SFPA_LADDERMAP(8)=0                                                 231
      SFPA_LADDERMAP(9)=0                                                 231
      SFPA_LADDERMAP(10)=0                                                231
      SFPA_LADDERMAP(11)=0                                                231
      SFPA_LADDERMAP(12)=0                                                231
      SFPA_LADDERMAP(13)=0                                                231
      SFPA_LADDERMAP(14)=0                                                231
      SFPA_LADDERMAP(15)=0                                                231
      SFPA_LADDERMAP(16)=0                                                231
      SFPA_LADDERMAP(17)=0                                                231
      SFPA_LADDERMAP(18)=0                                                231
      SFPA_LADDERMAP(19)=0                                                231
      SFPA_LADDERMAP(20)=0                                                231
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               231
C INDIVIDUAL ANGLES                                                       235
      SFPA_LADDERANGLE(1)=-1.0                                            235
      SFPA_LADDERANGLE(2)=-1.0                                            235
      SFPA_LADDERANGLE(3)=45.0                                            235
      SFPA_LADDERANGLE(4)=-1.0                                            235
      SFPA_LADDERANGLE(5)=-1.0                                            235
      SFPA_LADDERANGLE(6)=-1.0                                            235
      SFPA_LADDERANGLE(7)=-1.0                                            235
      SFPA_LADDERANGLE(8)=-1.0                                            235
      SFPA_LADDERANGLE(9)=-1.0                                            235
      SFPA_LADDERANGLE(10)=-1.0                                           235
      SFPA_LADDERANGLE(11)=-1.0                                           235
      SFPA_LADDERANGLE(12)=-1.0                                           235
      SFPA_LADDERANGLE(13)=-1.0                                           235
      SFPA_LADDERANGLE(14)=-1.0                                           235
      SFPA_LADDERANGLE(15)=-1.0                                           235
      SFPA_LADDERANGLE(16)=-1.0                                           235
      SFPA_LADDERANGLE(17)=-1.0                                           235
      SFPA_LADDERANGLE(18)=-1.0                                           235
      SFPA_LADDERANGLE(19)=-1.0                                           235
      SFPA_LADDERANGLE(20)=-1.0                                           235
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               235
C INDIVIDUAL TILTS                                                        239
      SFPA_LADDERTILT(1)=-1.0                                             239
      SFPA_LADDERTILT(2)=-1.0                                             239
      SFPA_LADDERTILT(3)=0.0                                              239
      SFPA_LADDERTILT(4)=-1.0                                             239
      SFPA_LADDERTILT(5)=-1.0                                             239
      SFPA_LADDERTILT(6)=-1.0                                             239
      SFPA_LADDERTILT(7)=-1.0                                             239
      SFPA_LADDERTILT(8)=-1.0                                             239
      SFPA_LADDERTILT(9)=-1.0                                             239
      SFPA_LADDERTILT(10)=-1.0                                            239
      SFPA_LADDERTILT(11)=-1.0                                            239
      SFPA_LADDERTILT(12)=-1.0                                            239
      SFPA_LADDERTILT(13)=-1.0                                            239
      SFPA_LADDERTILT(14)=-1.0                                            239
      SFPA_LADDERTILT(15)=-1.0                                            239
      SFPA_LADDERTILT(16)=-1.0                                            239
      SFPA_LADDERTILT(17)=-1.0                                            239
      SFPA_LADDERTILT(18)=-1.0                                            239
      SFPA_LADDERTILT(19)=-1.0                                            239
      SFPA_LADDERTILT(20)=-1.0                                            239
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 239
C INDIVIDUAL RADII                                                        244
      SFPA_LADDERRADIUS(1)=-1.0                                           244
      SFPA_LADDERRADIUS(2)=-1.0                                           244
      SFPA_LADDERRADIUS(3)=23.000                                         244
      SFPA_LADDERRADIUS(4)=-1.0                                           244
      SFPA_LADDERRADIUS(5)=-1.0                                           244
      SFPA_LADDERRADIUS(6)=-1.0                                           244
      SFPA_LADDERRADIUS(7)=-1.0                                           244
      SFPA_LADDERRADIUS(8)=-1.0                                           244
      SFPA_LADDERRADIUS(9)=-1.0                                           244
      SFPA_LADDERRADIUS(10)=-1.0                                          244
      SFPA_LADDERRADIUS(11)=-1.0                                          244
      SFPA_LADDERRADIUS(12)=-1.0                                          244
      SFPA_LADDERRADIUS(13)=-1.0                                          244
      SFPA_LADDERRADIUS(14)=-1.0                                          244
      SFPA_LADDERRADIUS(15)=-1.0                                          244
      SFPA_LADDERRADIUS(16)=-1.0                                          244
      SFPA_LADDERRADIUS(17)=-1.0                                          244
      SFPA_LADDERRADIUS(18)=-1.0                                          244
      SFPA_LADDERRADIUS(19)=-1.0                                          244
      SFPA_LADDERRADIUS(20)=-1.0                                          244
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               244
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    245
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           245
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   245
C *                                                                       245
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    245
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           245
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   245
C *                                                                       246
      CALL AGSBEGM(MODULE,IPRIN)                                          246
      IF (IPRIN.LT.0) RETURN                                              246
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   246
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      246
      BPATH_SFPA='SFPA*'                                                  246
      IDNUM_SFPA(1)=0                                                     247
C GEOMETRY VERSION                                                        247
      SFPA_VERSION=2                                                      247
      CSFPA_VERSION='GEOMETRY VERSION'                                    247
C PRESENCE OF LADDERS                                                     249
      SFPA_LADDERMAP(1)=1                                                 249
      SFPA_LADDERMAP(2)=1                                                 249
      SFPA_LADDERMAP(3)=1                                                 249
      SFPA_LADDERMAP(4)=0                                                 249
      SFPA_LADDERMAP(5)=0                                                 249
      SFPA_LADDERMAP(6)=0                                                 249
      SFPA_LADDERMAP(7)=0                                                 249
      SFPA_LADDERMAP(8)=0                                                 249
      SFPA_LADDERMAP(9)=1                                                 249
      SFPA_LADDERMAP(10)=1                                                249
      SFPA_LADDERMAP(11)=1                                                249
      SFPA_LADDERMAP(12)=1                                                249
      SFPA_LADDERMAP(13)=1                                                249
      SFPA_LADDERMAP(14)=0                                                249
      SFPA_LADDERMAP(15)=0                                                249
      SFPA_LADDERMAP(16)=0                                                249
      SFPA_LADDERMAP(17)=0                                                249
      SFPA_LADDERMAP(18)=0                                                249
      SFPA_LADDERMAP(19)=1                                                249
      SFPA_LADDERMAP(20)=1                                                249
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               249
C INDIVIDUAL ANGLES                                                       253
      SFPA_LADDERANGLE(1)=90.0                                            253
      SFPA_LADDERANGLE(2)=108.3                                           253
      SFPA_LADDERANGLE(3)=130.0                                           253
      SFPA_LADDERANGLE(4)=-1.0                                            253
      SFPA_LADDERANGLE(5)=-1.0                                            253
      SFPA_LADDERANGLE(6)=-1.0                                            253
      SFPA_LADDERANGLE(7)=-1.0                                            253
      SFPA_LADDERANGLE(8)=-1.0                                            253
      SFPA_LADDERANGLE(9)=230.0                                           253
      SFPA_LADDERANGLE(10)=251.7                                          253
      SFPA_LADDERANGLE(11)=270.0                                          253
      SFPA_LADDERANGLE(12)=288.3                                          253
      SFPA_LADDERANGLE(13)=310.0                                          253
      SFPA_LADDERANGLE(14)=-1.0                                           253
      SFPA_LADDERANGLE(15)=-1.0                                           253
      SFPA_LADDERANGLE(16)=-1.0                                           253
      SFPA_LADDERANGLE(17)=-1.0                                           253
      SFPA_LADDERANGLE(18)=-1.0                                           253
      SFPA_LADDERANGLE(19)=50.0                                           253
      SFPA_LADDERANGLE(20)=71.7                                           253
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               253
C INDIVIDUAL TILTS                                                        257
      SFPA_LADDERTILT(1)=0.0                                              257
      SFPA_LADDERTILT(2)=-6.0                                             257
      SFPA_LADDERTILT(3)=0.0                                              257
      SFPA_LADDERTILT(4)=-1.0                                             257
      SFPA_LADDERTILT(5)=-1.0                                             257
      SFPA_LADDERTILT(6)=-1.0                                             257
      SFPA_LADDERTILT(7)=-1.0                                             257
      SFPA_LADDERTILT(8)=0.0                                              257
      SFPA_LADDERTILT(9)=0.0                                              257
      SFPA_LADDERTILT(10)=6.0                                             257
      SFPA_LADDERTILT(11)=0.0                                             257
      SFPA_LADDERTILT(12)=-6.0                                            257
      SFPA_LADDERTILT(13)=0.0                                             257
      SFPA_LADDERTILT(14)=-1.0                                            257
      SFPA_LADDERTILT(15)=-1.0                                            257
      SFPA_LADDERTILT(16)=-1.0                                            257
      SFPA_LADDERTILT(17)=-1.0                                            257
      SFPA_LADDERTILT(18)=-1.0                                            257
      SFPA_LADDERTILT(19)=0.0                                             257
      SFPA_LADDERTILT(20)=6.0                                             257
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 257
C INDIVIDUAL RADII                                                        262
      SFPA_LADDERRADIUS(1)=23.174                                         262
      SFPA_LADDERRADIUS(2)=22.800                                         262
      SFPA_LADDERRADIUS(3)=24.600                                         262
      SFPA_LADDERRADIUS(4)=-1.0                                           262
      SFPA_LADDERRADIUS(5)=-1.0                                           262
      SFPA_LADDERRADIUS(6)=-1.0                                           262
      SFPA_LADDERRADIUS(7)=-1.0                                           262
      SFPA_LADDERRADIUS(8)=-1.0                                           262
      SFPA_LADDERRADIUS(9)=24.600                                         262
      SFPA_LADDERRADIUS(10)=22.800                                        262
      SFPA_LADDERRADIUS(11)=23.174                                        262
      SFPA_LADDERRADIUS(12)=22.800                                        262
      SFPA_LADDERRADIUS(13)=24.600                                        262
      SFPA_LADDERRADIUS(14)=-1.0                                          262
      SFPA_LADDERRADIUS(15)=-1.0                                          262
      SFPA_LADDERRADIUS(16)=-1.0                                          262
      SFPA_LADDERRADIUS(17)=-1.0                                          262
      SFPA_LADDERRADIUS(18)=-1.0                                          262
      SFPA_LADDERRADIUS(19)=24.600                                        262
      SFPA_LADDERRADIUS(20)=22.800                                        262
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               262
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    263
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           263
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   263
C *                                                                       263
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    263
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           263
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   263
C *                                                                       264
      CALL AGSBEGM(MODULE,IPRIN)                                          264
      IF (IPRIN.LT.0) RETURN                                              264
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   264
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      264
      BPATH_SFPA='SFPA*'                                                  264
      IDNUM_SFPA(1)=0                                                     265
C GEOMETRY VERSION                                                        265
      SFPA_VERSION=3                                                      265
      CSFPA_VERSION='GEOMETRY VERSION'                                    265
C PRESENCE OF LADDERS                                                     267
      SFPA_LADDERMAP(1)=1                                                 267
      SFPA_LADDERMAP(2)=1                                                 267
      SFPA_LADDERMAP(3)=1                                                 267
      SFPA_LADDERMAP(4)=1                                                 267
      SFPA_LADDERMAP(5)=1                                                 267
      SFPA_LADDERMAP(6)=1                                                 267
      SFPA_LADDERMAP(7)=1                                                 267
      SFPA_LADDERMAP(8)=1                                                 267
      SFPA_LADDERMAP(9)=1                                                 267
      SFPA_LADDERMAP(10)=1                                                267
      SFPA_LADDERMAP(11)=1                                                267
      SFPA_LADDERMAP(12)=1                                                267
      SFPA_LADDERMAP(13)=1                                                267
      SFPA_LADDERMAP(14)=1                                                267
      SFPA_LADDERMAP(15)=1                                                267
      SFPA_LADDERMAP(16)=1                                                267
      SFPA_LADDERMAP(17)=1                                                267
      SFPA_LADDERMAP(18)=1                                                267
      SFPA_LADDERMAP(19)=1                                                267
      SFPA_LADDERMAP(20)=1                                                267
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               267
C INDIVIDUAL ANGLES                                                       271
      SFPA_LADDERANGLE(1)=90.0                                            271
      SFPA_LADDERANGLE(2)=108.3                                           271
      SFPA_LADDERANGLE(3)=126.6                                           271
      SFPA_LADDERANGLE(4)=144.4                                           271
      SFPA_LADDERANGLE(5)=162.2                                           271
      SFPA_LADDERANGLE(6)=180.0                                           271
      SFPA_LADDERANGLE(7)=197.8                                           271
      SFPA_LADDERANGLE(8)=215.6                                           271
      SFPA_LADDERANGLE(9)=233.4                                           271
      SFPA_LADDERANGLE(10)=251.7                                          271
      SFPA_LADDERANGLE(11)=270.0                                          271
      SFPA_LADDERANGLE(12)=288.3                                          271
      SFPA_LADDERANGLE(13)=306.6                                          271
      SFPA_LADDERANGLE(14)=324.4                                          271
      SFPA_LADDERANGLE(15)=342.2                                          271
      SFPA_LADDERANGLE(16)=0.0                                            271
      SFPA_LADDERANGLE(17)=17.8                                           271
      SFPA_LADDERANGLE(18)=35.6                                           271
      SFPA_LADDERANGLE(19)=53.4                                           271
      SFPA_LADDERANGLE(20)=71.7                                           271
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               271
C INDIVIDUAL TILTS                                                        275
      SFPA_LADDERTILT(1)=0.0                                              275
      SFPA_LADDERTILT(2)=-6.0                                             275
      SFPA_LADDERTILT(3)=-7.0                                             275
      SFPA_LADDERTILT(4)=-7.0                                             275
      SFPA_LADDERTILT(5)=-7.0                                             275
      SFPA_LADDERTILT(6)=0.0                                              275
      SFPA_LADDERTILT(7)=7.0                                              275
      SFPA_LADDERTILT(8)=7.0                                              275
      SFPA_LADDERTILT(9)=7.0                                              275
      SFPA_LADDERTILT(10)=6.0                                             275
      SFPA_LADDERTILT(11)=0.0                                             275
      SFPA_LADDERTILT(12)=-6.0                                            275
      SFPA_LADDERTILT(13)=-7.0                                            275
      SFPA_LADDERTILT(14)=-7.0                                            275
      SFPA_LADDERTILT(15)=-7.0                                            275
      SFPA_LADDERTILT(16)=0.0                                             275
      SFPA_LADDERTILT(17)=7.0                                             275
      SFPA_LADDERTILT(18)=7.0                                             275
      SFPA_LADDERTILT(19)=7.0                                             275
      SFPA_LADDERTILT(20)=6.0                                             275
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 275
C INDIVIDUAL RADII                                                        280
      SFPA_LADDERRADIUS(1)=23.177                                         280
      SFPA_LADDERRADIUS(2)=22.800                                         280
      SFPA_LADDERRADIUS(3)=22.800                                         280
      SFPA_LADDERRADIUS(4)=22.800                                         280
      SFPA_LADDERRADIUS(5)=23.800                                         280
      SFPA_LADDERRADIUS(6)=22.500                                         280
      SFPA_LADDERRADIUS(7)=23.800                                         280
      SFPA_LADDERRADIUS(8)=22.800                                         280
      SFPA_LADDERRADIUS(9)=22.800                                         280
      SFPA_LADDERRADIUS(10)=22.800                                        280
      SFPA_LADDERRADIUS(11)=23.177                                        280
      SFPA_LADDERRADIUS(12)=22.800                                        280
      SFPA_LADDERRADIUS(13)=22.800                                        280
      SFPA_LADDERRADIUS(14)=22.800                                        280
      SFPA_LADDERRADIUS(15)=23.800                                        280
      SFPA_LADDERRADIUS(16)=22.500                                        280
      SFPA_LADDERRADIUS(17)=23.800                                        280
      SFPA_LADDERRADIUS(18)=22.800                                        280
      SFPA_LADDERRADIUS(19)=22.800                                        280
      SFPA_LADDERRADIUS(20)=22.800                                        280
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               280
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    281
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           281
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   281
C *                                                                       281
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    281
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           281
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   281
C *                                                                       282
      CALL AGSBEGM(MODULE,IPRIN)                                          282
      IF (IPRIN.LT.0) RETURN                                              282
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   282
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      282
      BPATH_SFPA='SFPA*'                                                  282
      IDNUM_SFPA(1)=0                                                     283
C GEOMETRY VERSION                                                        283
      SFPA_VERSION=4                                                      283
      CSFPA_VERSION='GEOMETRY VERSION'                                    283
C PRESENCE OF LADDERS                                                     285
      SFPA_LADDERMAP(1)=1                                                 285
      SFPA_LADDERMAP(2)=1                                                 285
      SFPA_LADDERMAP(3)=1                                                 285
      SFPA_LADDERMAP(4)=1                                                 285
      SFPA_LADDERMAP(5)=1                                                 285
      SFPA_LADDERMAP(6)=1                                                 285
      SFPA_LADDERMAP(7)=1                                                 285
      SFPA_LADDERMAP(8)=1                                                 285
      SFPA_LADDERMAP(9)=1                                                 285
      SFPA_LADDERMAP(10)=1                                                285
      SFPA_LADDERMAP(11)=1                                                285
      SFPA_LADDERMAP(12)=1                                                285
      SFPA_LADDERMAP(13)=1                                                285
      SFPA_LADDERMAP(14)=1                                                285
      SFPA_LADDERMAP(15)=1                                                285
      SFPA_LADDERMAP(16)=1                                                285
      SFPA_LADDERMAP(17)=1                                                285
      SFPA_LADDERMAP(18)=1                                                285
      SFPA_LADDERMAP(19)=1                                                285
      SFPA_LADDERMAP(20)=1                                                285
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               285
C INDIVIDUAL ANGLES                                                       289
      SFPA_LADDERANGLE(1)=90.0                                            289
      SFPA_LADDERANGLE(2)=108.3                                           289
      SFPA_LADDERANGLE(3)=126.6                                           289
      SFPA_LADDERANGLE(4)=144.4                                           289
      SFPA_LADDERANGLE(5)=162.2                                           289
      SFPA_LADDERANGLE(6)=180.0                                           289
      SFPA_LADDERANGLE(7)=197.8                                           289
      SFPA_LADDERANGLE(8)=215.6                                           289
      SFPA_LADDERANGLE(9)=233.4                                           289
      SFPA_LADDERANGLE(10)=251.7                                          289
      SFPA_LADDERANGLE(11)=270.0                                          289
      SFPA_LADDERANGLE(12)=288.3                                          289
      SFPA_LADDERANGLE(13)=306.6                                          289
      SFPA_LADDERANGLE(14)=324.4                                          289
      SFPA_LADDERANGLE(15)=342.2                                          289
      SFPA_LADDERANGLE(16)=0.0                                            289
      SFPA_LADDERANGLE(17)=17.8                                           289
      SFPA_LADDERANGLE(18)=35.6                                           289
      SFPA_LADDERANGLE(19)=53.4                                           289
      SFPA_LADDERANGLE(20)=71.7                                           289
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               289
C INDIVIDUAL TILTS                                                        293
      SFPA_LADDERTILT(1)=0.0                                              293
      SFPA_LADDERTILT(2)=-6.0                                             293
      SFPA_LADDERTILT(3)=-7.0                                             293
      SFPA_LADDERTILT(4)=-7.0                                             293
      SFPA_LADDERTILT(5)=-7.0                                             293
      SFPA_LADDERTILT(6)=0.0                                              293
      SFPA_LADDERTILT(7)=7.0                                              293
      SFPA_LADDERTILT(8)=7.0                                              293
      SFPA_LADDERTILT(9)=7.0                                              293
      SFPA_LADDERTILT(10)=6.0                                             293
      SFPA_LADDERTILT(11)=0.0                                             293
      SFPA_LADDERTILT(12)=-6.0                                            293
      SFPA_LADDERTILT(13)=-7.0                                            293
      SFPA_LADDERTILT(14)=-7.0                                            293
      SFPA_LADDERTILT(15)=-7.0                                            293
      SFPA_LADDERTILT(16)=0.0                                             293
      SFPA_LADDERTILT(17)=7.0                                             293
      SFPA_LADDERTILT(18)=7.0                                             293
      SFPA_LADDERTILT(19)=7.0                                             293
      SFPA_LADDERTILT(20)=6.0                                             293
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 293
C INDIVIDUAL RADII                                                        298
      SFPA_LADDERRADIUS(1)=23.177                                         298
      SFPA_LADDERRADIUS(2)=22.800                                         298
      SFPA_LADDERRADIUS(3)=22.800                                         298
      SFPA_LADDERRADIUS(4)=22.800                                         298
      SFPA_LADDERRADIUS(5)=22.800                                         298
      SFPA_LADDERRADIUS(6)=22.500                                         298
      SFPA_LADDERRADIUS(7)=22.800                                         298
      SFPA_LADDERRADIUS(8)=22.800                                         298
      SFPA_LADDERRADIUS(9)=22.800                                         298
      SFPA_LADDERRADIUS(10)=22.800                                        298
      SFPA_LADDERRADIUS(11)=23.177                                        298
      SFPA_LADDERRADIUS(12)=22.800                                        298
      SFPA_LADDERRADIUS(13)=22.800                                        298
      SFPA_LADDERRADIUS(14)=22.800                                        298
      SFPA_LADDERRADIUS(15)=22.800                                        298
      SFPA_LADDERRADIUS(16)=22.500                                        298
      SFPA_LADDERRADIUS(17)=22.800                                        298
      SFPA_LADDERRADIUS(18)=22.800                                        298
      SFPA_LADDERRADIUS(19)=22.800                                        298
      SFPA_LADDERRADIUS(20)=22.800                                        298
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               298
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    299
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           299
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   299
C *                                                                       299
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    299
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           299
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   299
C *                                                                       300
      CALL AGSBEGM(MODULE,IPRIN)                                          300
      IF (IPRIN.LT.0) RETURN                                              300
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   300
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      300
      BPATH_SFPA='SFPA*'                                                  300
      IDNUM_SFPA(1)=0                                                     301
C GEOMETRY VERSION                                                        301
      SFPA_VERSION=5                                                      301
      CSFPA_VERSION='GEOMETRY VERSION'                                    301
C PRESENCE OF LADDERS                                                     303
      SFPA_LADDERMAP(1)=1                                                 303
      SFPA_LADDERMAP(2)=1                                                 303
      SFPA_LADDERMAP(3)=1                                                 303
      SFPA_LADDERMAP(4)=1                                                 303
      SFPA_LADDERMAP(5)=1                                                 303
      SFPA_LADDERMAP(6)=1                                                 303
      SFPA_LADDERMAP(7)=1                                                 303
      SFPA_LADDERMAP(8)=1                                                 303
      SFPA_LADDERMAP(9)=1                                                 303
      SFPA_LADDERMAP(10)=1                                                303
      SFPA_LADDERMAP(11)=1                                                303
      SFPA_LADDERMAP(12)=1                                                303
      SFPA_LADDERMAP(13)=1                                                303
      SFPA_LADDERMAP(14)=1                                                303
      SFPA_LADDERMAP(15)=1                                                303
      SFPA_LADDERMAP(16)=1                                                303
      SFPA_LADDERMAP(17)=1                                                303
      SFPA_LADDERMAP(18)=1                                                303
      SFPA_LADDERMAP(19)=1                                                303
      SFPA_LADDERMAP(20)=1                                                303
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               303
C INDIVIDUAL ANGLES                                                       307
      SFPA_LADDERANGLE(1)=90.0                                            307
      SFPA_LADDERANGLE(2)=108.3                                           307
      SFPA_LADDERANGLE(3)=126.6                                           307
      SFPA_LADDERANGLE(4)=144.4                                           307
      SFPA_LADDERANGLE(5)=162.2                                           307
      SFPA_LADDERANGLE(6)=180.0                                           307
      SFPA_LADDERANGLE(7)=197.8                                           307
      SFPA_LADDERANGLE(8)=215.6                                           307
      SFPA_LADDERANGLE(9)=233.4                                           307
      SFPA_LADDERANGLE(10)=251.7                                          307
      SFPA_LADDERANGLE(11)=270.0                                          307
      SFPA_LADDERANGLE(12)=288.3                                          307
      SFPA_LADDERANGLE(13)=306.6                                          307
      SFPA_LADDERANGLE(14)=324.4                                          307
      SFPA_LADDERANGLE(15)=342.2                                          307
      SFPA_LADDERANGLE(16)=0.0                                            307
      SFPA_LADDERANGLE(17)=17.8                                           307
      SFPA_LADDERANGLE(18)=35.6                                           307
      SFPA_LADDERANGLE(19)=53.4                                           307
      SFPA_LADDERANGLE(20)=71.7                                           307
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               307
C INDIVIDUAL TILTS                                                        311
      SFPA_LADDERTILT(1)=0.0                                              311
      SFPA_LADDERTILT(2)=-6.0                                             311
      SFPA_LADDERTILT(3)=-7.0                                             311
      SFPA_LADDERTILT(4)=-7.0                                             311
      SFPA_LADDERTILT(5)=-7.0                                             311
      SFPA_LADDERTILT(6)=0.0                                              311
      SFPA_LADDERTILT(7)=7.0                                              311
      SFPA_LADDERTILT(8)=7.0                                              311
      SFPA_LADDERTILT(9)=7.0                                              311
      SFPA_LADDERTILT(10)=6.0                                             311
      SFPA_LADDERTILT(11)=0.0                                             311
      SFPA_LADDERTILT(12)=-6.0                                            311
      SFPA_LADDERTILT(13)=-7.0                                            311
      SFPA_LADDERTILT(14)=-7.0                                            311
      SFPA_LADDERTILT(15)=-7.0                                            311
      SFPA_LADDERTILT(16)=0.0                                             311
      SFPA_LADDERTILT(17)=7.0                                             311
      SFPA_LADDERTILT(18)=7.0                                             311
      SFPA_LADDERTILT(19)=7.0                                             311
      SFPA_LADDERTILT(20)=6.0                                             311
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 311
C INDIVIDUAL RADII                                                        316
      SFPA_LADDERRADIUS(1)=23.177                                         316
      SFPA_LADDERRADIUS(2)=22.800                                         316
      SFPA_LADDERRADIUS(3)=22.600                                         316
      SFPA_LADDERRADIUS(4)=22.600                                         316
      SFPA_LADDERRADIUS(5)=22.600                                         316
      SFPA_LADDERRADIUS(6)=22.300                                         316
      SFPA_LADDERRADIUS(7)=22.600                                         316
      SFPA_LADDERRADIUS(8)=22.600                                         316
      SFPA_LADDERRADIUS(9)=22.600                                         316
      SFPA_LADDERRADIUS(10)=22.800                                        316
      SFPA_LADDERRADIUS(11)=23.177                                        316
      SFPA_LADDERRADIUS(12)=22.800                                        316
      SFPA_LADDERRADIUS(13)=22.600                                        316
      SFPA_LADDERRADIUS(14)=22.600                                        316
      SFPA_LADDERRADIUS(15)=22.600                                        316
      SFPA_LADDERRADIUS(16)=22.500                                        316
      SFPA_LADDERRADIUS(17)=22.600                                        316
      SFPA_LADDERRADIUS(18)=22.600                                        316
      SFPA_LADDERRADIUS(19)=22.600                                        316
      SFPA_LADDERRADIUS(20)=22.800                                        316
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               316
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    317
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           317
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   317
C *                                                                       317
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    317
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           317
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   317
C use default SSDP bank                                                   318
      BPATH_SSDP='SSDP*'                                                  318
      IDNUM_SSDP(1)=0                                                     318
      CALL AGDGETP('SISDGEO6','*',-999,'SSDP',' ',BANK_SSDP, BPATH_SSDP,  318
     *IDNUM_SSDP,1,MAP_SSDP,NAMES_SSDP,BANK_SSDP,LEN_SSDP,LLEN_SSDP,      318
     *LINK_SSDP,FLAG_SSDP)                                                318
      CONFIGA = MOD(SSDP_CONFIG ,10)                                      319
      CONFIGB = MOD(SSDP_CONFIG/10,10)                                    320
C use SFPA bank                                                           321
      BPATH_SFPA='SFPA*'                                                  321
      IDNUM_SFPA(1)=0                                                     321
      SFPA_VERSION=CONFIGA                                                321
      CALL AGDGETP('SISDGEO6','*',-999,'SFPA',NSFPA_VERSION,              321
     *RE_SFPA_VERSION,BPATH_SFPA,IDNUM_SFPA,1,MAP_SFPA,NAMES_SFPA,        321
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   321
C use SFJP bank                                                           322
      BPATH_SFJP='SFJP*'                                                  322
      IDNUM_SFJP(1)=0                                                     322
      SFJP_VERSION=2                                                      322
      CALL AGDGETP('SISDGEO6','*',-999,'SFJP',NSFJP_VERSION,              322
     *RE_SFJP_VERSION,BPATH_SFJP,IDNUM_SFJP,1,MAP_SFJP,NAMES_SFJP,        322
     *BANK_SFJP,LEN_SFJP,LLEN_SFJP,LINK_SFJP,FLAG_SFJP)                   322
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             323
      CALL AGSRESET                                                       323
      AG_A = 28.08                                                        323
      AG_Z = 14                                                           323
      AG_W = 0.6*1*28./60.                                                323
      AG_TITLE = 'SI'                                                     323
      AG_EXNAME = 'COMPONENT'                                             323
      AG_PARLIST = 'A_Z_W'                                                323
      CALL AXCOMPONENT                                                    323
C *                                                                       323
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  323
C =                                                                       323
      CALL AGSRESET                                                       324
      AG_A = 16                                                           324
      AG_Z = 8                                                            324
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               324
      AG_TITLE = 'O'                                                      324
      AG_EXNAME = 'COMPONENT'                                             324
      AG_PARLIST = 'A_Z_W'                                                324
      CALL AXCOMPONENT                                                    324
C *                                                                       324
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 324
      CALL AGSRESET                                                       325
      AG_A = 12                                                           325
      AG_Z = 6                                                            325
      AG_W = 0.4*8*12./174.                                               325
      AG_TITLE = 'C'                                                      325
      AG_EXNAME = 'COMPONENT'                                             325
      AG_PARLIST = 'A_Z_W'                                                325
      CALL AXCOMPONENT                                                    325
C *                                                                       325
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  325
      CALL AGSRESET                                                       326
      AG_A = 1                                                            326
      AG_Z = 1                                                            326
      AG_W = 0.4*14*1./174.                                               326
      AG_TITLE = 'H'                                                      326
      AG_EXNAME = 'COMPONENT'                                             326
      AG_PARLIST = 'A_Z_W'                                                326
      CALL AXCOMPONENT                                                    326
C *                                                                       326
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  326
      CALL AGSRESET                                                       327
      AG_DENS = 1.7                                                       327
      AG_TITLE = 'G10'                                                    327
      AG_EXNAME = 'MIXTURE'                                               327
      AG_PARLIST = 'DENS'                                                 327
      CALL AXMIXTURE                                                      327
C *                                                                       327
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             327
      CALL AGSRESET                                                       328
      AG_A = 28.08                                                        328
      AG_Z = 14                                                           328
      AG_W = 0.6*1*28./60.                                                328
      AG_TITLE = 'SI'                                                     328
      AG_EXNAME = 'COMPONENT'                                             328
      AG_PARLIST = 'A_Z_W'                                                328
      CALL AXCOMPONENT                                                    328
C *                                                                       328
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  328
C =                                                                       328
      CALL AGSRESET                                                       329
      AG_A = 16                                                           329
      AG_Z = 8                                                            329
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               329
      AG_TITLE = 'O'                                                      329
      AG_EXNAME = 'COMPONENT'                                             329
      AG_PARLIST = 'A_Z_W'                                                329
      CALL AXCOMPONENT                                                    329
C *                                                                       329
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 329
      CALL AGSRESET                                                       330
      AG_A = 12                                                           330
      AG_Z = 6                                                            330
      AG_W = 0.4*8*12./174.                                               330
      AG_TITLE = 'C'                                                      330
      AG_EXNAME = 'COMPONENT'                                             330
      AG_PARLIST = 'A_Z_W'                                                330
      CALL AXCOMPONENT                                                    330
C *                                                                       330
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  330
      CALL AGSRESET                                                       331
      AG_A = 1                                                            331
      AG_Z = 1                                                            331
      AG_W = 0.4*14*1./174.                                               331
      AG_TITLE = 'H'                                                      331
      AG_EXNAME = 'COMPONENT'                                             331
      AG_PARLIST = 'A_Z_W'                                                331
      CALL AXCOMPONENT                                                    331
C *                                                                       331
C Geant MIXTURE : G5 ===> DENS=0.85 <===                                  331
      CALL AGSRESET                                                       332
      AG_DENS = 0.85                                                      332
      AG_TITLE = 'G5'                                                     332
      AG_EXNAME = 'MIXTURE'                                               332
      AG_PARLIST = 'DENS'                                                 332
      CALL AXMIXTURE                                                      332
C *                                                                       332
C Geant COMPONENT : O ===> A=16 Z=8 W=2*16./100. <===                     332
      CALL AGSRESET                                                       333
      AG_A = 16                                                           333
      AG_Z = 8                                                            333
      AG_W = 2*16./100.                                                   333
      AG_TITLE = 'O'                                                      333
      AG_EXNAME = 'COMPONENT'                                             333
      AG_PARLIST = 'A_Z_W'                                                333
      CALL AXCOMPONENT                                                    333
C *                                                                       333
C Geant COMPONENT : C ===> A=12 Z=6 W=5*12./100. <===                     333
      CALL AGSRESET                                                       334
      AG_A = 12                                                           334
      AG_Z = 6                                                            334
      AG_W = 5*12./100.                                                   334
      AG_TITLE = 'C'                                                      334
      AG_EXNAME = 'COMPONENT'                                             334
      AG_PARLIST = 'A_Z_W'                                                334
      CALL AXCOMPONENT                                                    334
C *                                                                       334
C Geant COMPONENT : H ===> A=1 Z=1 W=8*1.0/100. <===                      334
      CALL AGSRESET                                                       335
      AG_A = 1                                                            335
      AG_Z = 1                                                            335
      AG_W = 8*1.0/100.                                                   335
      AG_TITLE = 'H'                                                      335
      AG_EXNAME = 'COMPONENT'                                             335
      AG_PARLIST = 'A_Z_W'                                                335
      CALL AXCOMPONENT                                                    335
C *                                                                       335
C Geant MIXTURE : DELRIN ===> DENS=1.2 <===                               335
      CALL AGSRESET                                                       336
      AG_DENS = 1.2                                                       336
      AG_TITLE = 'DELRIN'                                                 336
      AG_EXNAME = 'MIXTURE'                                               336
      AG_PARLIST = 'DENS'                                                 336
      CALL AXMIXTURE                                                      336
C *                                                                       336
C Check SSDP_PLACEMENT==1                                                 336
      IF (SSDP_PLACEMENT.EQ.1) THEN                                       337
      CALL AGSBEGM(MODULE,IPRIN)                                          338
C Check IPRIN<0                                                           338
      IF (IPRIN.LT.0) RETURN                                              338
C Create SFMO :                                                           338
C Call To Local Procedure SFMO                                            338
      ASSIGN 5020 TO BLOCK_SFMO                                           338
      GO TO  5010                                                         338
5020  CONTINUE                                                            338
C *                                                                       338
C Geant POSITION : SFMO ===> MOTHER='SVTT' <===                           339
      CALL AGSRESET                                                       339
      AG_MOTHER = 'SVTT'                                                  339
      AG_TITLE = 'SFMO'                                                   339
      AG_EXNAME = 'POSITION'                                              339
      AG_PARLIST = 'MOTHER'                                               339
      CALL AXPOSITION                                                     339
C *                                                                       339
      ELSE                                                                339
      CALL AGSBEGM(MODULE,IPRIN)                                          341
C Check IPRIN<0                                                           341
      IF (IPRIN.LT.0) RETURN                                              341
C Create SFMO :                                                           341
C Call To Local Procedure SFMO                                            341
      ASSIGN 5030 TO BLOCK_SFMO                                           341
      GO TO  5010                                                         341
5030  CONTINUE                                                            341
C *                                                                       341
C Geant POSITION : SFMO ===> MOTHER='CAVE' KONLY='ONLY' <===              342
      CALL AGSRESET                                                       342
      AG_MOTHER = 'CAVE'                                                  342
      AG_KONLY = 'ONLY'                                                   342
      AG_TITLE = 'SFMO'                                                   342
      AG_EXNAME = 'POSITION'                                              342
      AG_PARLIST = 'MOTHER_KONLY'                                         342
      CALL AXPOSITION                                                     342
C *                                                                       342
      ENDIF                                                               342
C ----------------------------------------------------------------------  344
C ------------ SFMO                                                       344
      CALL AGSENDM                                                        345
      RETURN                                                              345
C Begin Local Procedure SFMO                                              345
C SFMO IS THE MOTHER OF ALL SILICON STRIP DETECTOR VOLUMES                345
5010  CONTINUE                                                            345
         AG_TITLE='SFMO'                                                  345
         AG_MODULE=MODULE                                                 345
         AG_MARK='V00'                                                    345
         CALL AGSPUSH                                                     345
         AG_MODULE=MODULE                                                 345
C    *                                                                    345
C    Geant MATERIAL : AIR ===> inherited arguments used <===              346
         CALL AGSRESET                                                    346
         AG_TITLE = 'AIR'                                                 346
         AG_EXNAME = 'MATERIAL'                                           346
         AG_PARLIST = 'NONE'                                              346
         CALL AXMATERIAL                                                  346
C    *                                                                    346
C    Geant ATTRIBUTE : SFMO ===> SEEN=1 COLO=6 <===                       346
         CALL AGSRESET                                                    347
         AG_SEEN = 1                                                      347
         AG_COLO = 6                                                      347
         AG_TITLE = 'SFMO'                                                347
         AG_EXNAME = 'ATTRIBUTE'                                          347
         AG_PARLIST = 'SEEN_COLO'                                         347
         CALL AXATTRIBUTE                                                 347
C    *                                                                    347
C    Geant SHAPE : TUBE ===> RMIN=SFPA_RMIN RMAX=SFPA_RMAX, DZ=SFPA_LEN/  347
C    2 <===                                                               347
         CALL AGSRESET                                                    349
         AG_RMIN = SFPA_RMIN                                              349
         AG_RMAX = SFPA_RMAX                                              349
         AG_DZ = SFPA_LEN/2                                               349
         AG_TITLE = 'TUBE'                                                349
         AG_EXNAME = 'SHAPE'                                              349
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      349
         CALL AXSHAPE                                                     349
C    *                                                                    349
         DTHK = SFPA_SMTHK + SFPA_GPTHK                                   349
C    Check CONFIGB.GE.7                                                   351
         IF (CONFIGB.GE.7) THEN                                           351
C    Create SOSK :                                                        352
C    Call To Local Procedure SOSK                                         352
         ASSIGN 5050 TO BLOCK_SOSK                                        352
         GO TO  5040                                                      352
5050     CONTINUE                                                         352
C    *                                                                    352
C    Geant POSITION : SOSK ===> inherited arguments used <===             353
         CALL AGSRESET                                                    353
         AG_TITLE = 'SOSK'                                                353
         AG_EXNAME = 'POSITION'                                           353
         AG_PARLIST = 'NONE'                                              353
         CALL AXPOSITION                                                  353
C    *                                                                    353
         ENDIF                                                            353
C    Create SFLM :                                                        355
C    Call To Local Procedure SFLM                                         355
         ASSIGN 5070 TO BLOCK_SFLM                                        355
         GO TO  5060                                                      355
5070     CONTINUE                                                         355
C    *                                                                    355
C    *                                                                    356
         ILAD=1                                                           356
            GO TO 5083                                                    356
5081        ILAD=ILAD+1                                                   356
5083        IF(ILAD-(20).GT.0) GO TO 5082                                 356
C       Check SFPA_LADDERMAP(ILAD).GT.0                                   357
            IF (SFPA_LADDERMAP(ILAD).GT.0) THEN                           357
            ANG = (SFPA_LADDERANGLE(ILAD)*PI)/180.0                       358
            RADTILT = (SFPA_LADDERTILT(ILAD) *PI)/180.0                   359
C       Check ILAD.EQ.1                                                   360
            IF (ILAD.EQ.1) THEN                                           360
            NC=1                                                          361
            ELSE                                                          362
            NC=20-ILAD+2                                                  363
            ENDIF                                                         364
C       Geant POSITION : SFLM ===> X=(SFPA_LADDERRADIUS(ILAD)*COS(ANG)+(  367
C       DTHK*COS(ANG+RADTILT))/2.0) Y=(SFPA_LADDERRADIUS(ILAD)*SIN(ANG)+  367
C       (DTHK*SIN(ANG+RADTILT))/2.0) Z=0 ALPHAZ=SFPA_LADDERANGLE(ILAD)-9  367
C       0.0+SFPA_LADDERTILT(ILAD) NCOPY=NC KONLY='MANY' <===              367
            CALL AGSRESET                                                 367
            AG_X =                                                        367
     *      (SFPA_LADDERRADIUS(ILAD)*COS(ANG)+(DTHK*COS(ANG+RADTILT))/    367
     *      2.0)                                                          367
            AG_Y =                                                        367
     *      (SFPA_LADDERRADIUS(ILAD)*SIN(ANG)+(DTHK*SIN(ANG+RADTILT))/    367
     *      2.0)                                                          367
            AG_Z = 0                                                      367
            AG_ALPHAN=AG_ALPHAN+1                                         367
            AG_ALPHAK(AG_ALPHAN)=3                                        367
            AG_ALPHAL(AG_ALPHAN)=                                         367
     *      SFPA_LADDERANGLE(ILAD)-90.0+SFPA_LADDERTILT(ILAD)             367
            AG_NCOPY = NC                                                 367
            AG_KONLY = 'MANY'                                             367
            AG_TITLE = 'SFLM'                                             367
            AG_EXNAME = 'POSITION'                                        367
            AG_PARLIST = 'X_Y_Z_ALPHAZ_NCOPY_KONLY'                       367
            CALL AXPOSITION                                               367
C       *                                                                 367
            ENDIF                                                         367
         GO TO 5081                                                       369
5082     CONTINUE                                                         369
C    Create SSST :                                                        370
C    Call To Local Procedure SSST                                         370
         ASSIGN 5100 TO BLOCK_SSST                                        370
         GO TO  5090                                                      370
5100     CONTINUE                                                         370
C    *                                                                    370
C    Geant POSITION : SSST ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  372
C    JP_SSST_WIDTH/2.-SFJP_SSST_PZ <===                                   372
         CALL AGSRESET                                                    372
         AG_X = 0.                                                        372
         AG_Y = 0.                                                        372
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSST_WIDTH/                372
     *   2.-SFJP_SSST_PZ                                                  372
         AG_TITLE = 'SSST'                                                372
         AG_EXNAME = 'POSITION'                                           372
         AG_PARLIST = 'X_Y_Z'                                             372
         CALL AXPOSITION                                                  372
C    *                                                                    372
C    Geant POSITION : SSST ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  372
C    JP_SSST_WIDTH/2.+SFJP_SSST_PZ <===                                   372
         CALL AGSRESET                                                    374
         AG_X = 0.                                                        374
         AG_Y = 0.                                                        374
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSST_WIDTH/                374
     *   2.+SFJP_SSST_PZ                                                  374
         AG_TITLE = 'SSST'                                                374
         AG_EXNAME = 'POSITION'                                           374
         AG_PARLIST = 'X_Y_Z'                                             374
         CALL AXPOSITION                                                  374
C    *                                                                    374
C    Geant POSITION : SSST ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  374
C    JP_SSST_WIDTH/2.-SFJP_SSST_PZ ALPHAZ=180. <===                       374
         CALL AGSRESET                                                    376
         AG_X = 0.                                                        376
         AG_Y = 0.                                                        376
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSST_WIDTH/                376
     *   2.-SFJP_SSST_PZ                                                  376
         AG_ALPHAN=AG_ALPHAN+1                                            376
         AG_ALPHAK(AG_ALPHAN)=3                                           376
         AG_ALPHAL(AG_ALPHAN)=180.                                        376
         AG_TITLE = 'SSST'                                                376
         AG_EXNAME = 'POSITION'                                           376
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      376
         CALL AXPOSITION                                                  376
C    *                                                                    376
C    Geant POSITION : SSST ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  376
C    JP_SSST_WIDTH/2.+SFJP_SSST_PZ ALPHAZ=180. <===                       376
         CALL AGSRESET                                                    378
         AG_X = 0.                                                        378
         AG_Y = 0.                                                        378
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSST_WIDTH/                378
     *   2.+SFJP_SSST_PZ                                                  378
         AG_ALPHAN=AG_ALPHAN+1                                            378
         AG_ALPHAK(AG_ALPHAN)=3                                           378
         AG_ALPHAL(AG_ALPHAN)=180.                                        378
         AG_TITLE = 'SSST'                                                378
         AG_EXNAME = 'POSITION'                                           378
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      378
         CALL AXPOSITION                                                  378
C    *                                                                    378
C    Create SSSS :                                                        379
C    Call To Local Procedure SSSS                                         379
         ASSIGN 5120 TO BLOCK_SSSS                                        379
         GO TO  5110                                                      379
5120     CONTINUE                                                         379
C    *                                                                    379
C    Geant POSITION : SSSS ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  381
C    JP_SSSS_WIDTH/2.-SFJP_SSST_PZ <===                                   381
         CALL AGSRESET                                                    381
         AG_X = 0.                                                        381
         AG_Y = 0.                                                        381
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH/                381
     *   2.-SFJP_SSST_PZ                                                  381
         AG_TITLE = 'SSSS'                                                381
         AG_EXNAME = 'POSITION'                                           381
         AG_PARLIST = 'X_Y_Z'                                             381
         CALL AXPOSITION                                                  381
C    *                                                                    381
C    Geant POSITION : SSSS ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  381
C    JP_SSSS_WIDTH/2.+SFJP_SSST_PZ <===                                   381
         CALL AGSRESET                                                    383
         AG_X = 0.                                                        383
         AG_Y = 0.                                                        383
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH/                383
     *   2.+SFJP_SSST_PZ                                                  383
         AG_TITLE = 'SSSS'                                                383
         AG_EXNAME = 'POSITION'                                           383
         AG_PARLIST = 'X_Y_Z'                                             383
         CALL AXPOSITION                                                  383
C    *                                                                    383
C    Geant POSITION : SSSS ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  383
C    JP_SSSS_WIDTH/2.-SFJP_SSST_PZ ALPHAZ=180. <===                       383
         CALL AGSRESET                                                    385
         AG_X = 0.                                                        385
         AG_Y = 0.                                                        385
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH/                385
     *   2.-SFJP_SSST_PZ                                                  385
         AG_ALPHAN=AG_ALPHAN+1                                            385
         AG_ALPHAK(AG_ALPHAN)=3                                           385
         AG_ALPHAL(AG_ALPHAN)=180.                                        385
         AG_TITLE = 'SSSS'                                                385
         AG_EXNAME = 'POSITION'                                           385
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      385
         CALL AXPOSITION                                                  385
C    *                                                                    385
C    Geant POSITION : SSSS ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  385
C    JP_SSSS_WIDTH/2.+SFJP_SSST_PZ ALPHAZ=180. <===                       385
         CALL AGSRESET                                                    387
         AG_X = 0.                                                        387
         AG_Y = 0.                                                        387
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH/                387
     *   2.+SFJP_SSST_PZ                                                  387
         AG_ALPHAN=AG_ALPHAN+1                                            387
         AG_ALPHAK(AG_ALPHAN)=3                                           387
         AG_ALPHAL(AG_ALPHAN)=180.                                        387
         AG_TITLE = 'SSSS'                                                387
         AG_EXNAME = 'POSITION'                                           387
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      387
         CALL AXPOSITION                                                  387
C    *                                                                    387
C    Create SSRT :                                                        388
C    Call To Local Procedure SSRT                                         388
         ASSIGN 5140 TO BLOCK_SSRT                                        388
         GO TO  5130                                                      388
5140     CONTINUE                                                         388
C    *                                                                    388
C    Geant POSITION : SSRT ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  390
C    JP_SSST_WIDTH/2.-SFJP_SSST_PZ <===                                   390
         CALL AGSRESET                                                    390
         AG_X = 0.                                                        390
         AG_Y = 0.                                                        390
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSST_WIDTH/                390
     *   2.-SFJP_SSST_PZ                                                  390
         AG_TITLE = 'SSRT'                                                390
         AG_EXNAME = 'POSITION'                                           390
         AG_PARLIST = 'X_Y_Z'                                             390
         CALL AXPOSITION                                                  390
C    *                                                                    390
C    Geant POSITION : SSRT ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  390
C    JP_SSST_WIDTH/2.+SFJP_SSST_PZ <===                                   390
         CALL AGSRESET                                                    392
         AG_X = 0.                                                        392
         AG_Y = 0.                                                        392
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSST_WIDTH/                392
     *   2.+SFJP_SSST_PZ                                                  392
         AG_TITLE = 'SSRT'                                                392
         AG_EXNAME = 'POSITION'                                           392
         AG_PARLIST = 'X_Y_Z'                                             392
         CALL AXPOSITION                                                  392
C    *                                                                    392
C    Geant POSITION : SSRT ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  392
C    JP_SSST_WIDTH/2.-SFJP_SSST_PZ ALPHAZ=180. <===                       392
         CALL AGSRESET                                                    394
         AG_X = 0.                                                        394
         AG_Y = 0.                                                        394
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSST_WIDTH/                394
     *   2.-SFJP_SSST_PZ                                                  394
         AG_ALPHAN=AG_ALPHAN+1                                            394
         AG_ALPHAK(AG_ALPHAN)=3                                           394
         AG_ALPHAL(AG_ALPHAN)=180.                                        394
         AG_TITLE = 'SSRT'                                                394
         AG_EXNAME = 'POSITION'                                           394
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      394
         CALL AXPOSITION                                                  394
C    *                                                                    394
C    Geant POSITION : SSRT ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  394
C    JP_SSST_WIDTH/2.+SFJP_SSST_PZ ALPHAZ=180. <===                       394
         CALL AGSRESET                                                    396
         AG_X = 0.                                                        396
         AG_Y = 0.                                                        396
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSST_WIDTH/                396
     *   2.+SFJP_SSST_PZ                                                  396
         AG_ALPHAN=AG_ALPHAN+1                                            396
         AG_ALPHAK(AG_ALPHAN)=3                                           396
         AG_ALPHAL(AG_ALPHAN)=180.                                        396
         AG_TITLE = 'SSRT'                                                396
         AG_EXNAME = 'POSITION'                                           396
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      396
         CALL AXPOSITION                                                  396
C    *                                                                    396
C    Create SSRS :                                                        397
C    Call To Local Procedure SSRS                                         397
         ASSIGN 5160 TO BLOCK_SSRS                                        397
         GO TO  5150                                                      397
5160     CONTINUE                                                         397
C    *                                                                    397
C    Geant POSITION : SSRS ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  399
C    JP_SSSS_WIDTH/2.-SFJP_SSST_PZ <===                                   399
         CALL AGSRESET                                                    399
         AG_X = 0.                                                        399
         AG_Y = 0.                                                        399
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH/                399
     *   2.-SFJP_SSST_PZ                                                  399
         AG_TITLE = 'SSRS'                                                399
         AG_EXNAME = 'POSITION'                                           399
         AG_PARLIST = 'X_Y_Z'                                             399
         CALL AXPOSITION                                                  399
C    *                                                                    399
C    Geant POSITION : SSRS ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  399
C    JP_SSSS_WIDTH/2.+SFJP_SSST_PZ <===                                   399
         CALL AGSRESET                                                    401
         AG_X = 0.                                                        401
         AG_Y = 0.                                                        401
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH/                401
     *   2.+SFJP_SSST_PZ                                                  401
         AG_TITLE = 'SSRS'                                                401
         AG_EXNAME = 'POSITION'                                           401
         AG_PARLIST = 'X_Y_Z'                                             401
         CALL AXPOSITION                                                  401
C    *                                                                    401
C    Geant POSITION : SSRS ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  401
C    JP_SSSS_WIDTH/2.-SFJP_SSST_PZ ALPHAZ=180. <===                       401
         CALL AGSRESET                                                    403
         AG_X = 0.                                                        403
         AG_Y = 0.                                                        403
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH/                403
     *   2.-SFJP_SSST_PZ                                                  403
         AG_ALPHAN=AG_ALPHAN+1                                            403
         AG_ALPHAK(AG_ALPHAN)=3                                           403
         AG_ALPHAL(AG_ALPHAN)=180.                                        403
         AG_TITLE = 'SSRS'                                                403
         AG_EXNAME = 'POSITION'                                           403
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      403
         CALL AXPOSITION                                                  403
C    *                                                                    403
C    Geant POSITION : SSRS ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  403
C    JP_SSSS_WIDTH/2.+SFJP_SSST_PZ ALPHAZ=180. <===                       403
         CALL AGSRESET                                                    405
         AG_X = 0.                                                        405
         AG_Y = 0.                                                        405
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH/                405
     *   2.+SFJP_SSST_PZ                                                  405
         AG_ALPHAN=AG_ALPHAN+1                                            405
         AG_ALPHAK(AG_ALPHAN)=3                                           405
         AG_ALPHAL(AG_ALPHAN)=180.                                        405
         AG_TITLE = 'SSRS'                                                405
         AG_EXNAME = 'POSITION'                                           405
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      405
         CALL AXPOSITION                                                  405
C    *                                                                    405
C    Create SSLB :                                                        406
C    Call To Local Procedure SSLB                                         406
         ASSIGN 5180 TO BLOCK_SSLB                                        406
         GO TO  5170                                                      406
5180     CONTINUE                                                         406
C    *                                                                    406
C    Check SFJP_VERSION==1                                                407
         IF (SFJP_VERSION.EQ.1) THEN                                      407
C    Geant POSITION : SSLB ===> X=SFJP_SSLB_PX Y=SFJP_SSLB_PX Z=-SFJP_SF  410
C    LU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ/2. ALPH  410
C    AZ=SFJP_ALPHAZH <===                                                 410
         CALL AGSRESET                                                    410
         AG_X = SFJP_SSLB_PX                                              410
         AG_Y = SFJP_SSLB_PX                                              410
         AG_Z =                                                           410
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          410
     *   SFJP_SSLB_DZ/2.                                                  410
         AG_ALPHAN=AG_ALPHAN+1                                            410
         AG_ALPHAK(AG_ALPHAN)=3                                           410
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                410
         AG_TITLE = 'SSLB'                                                410
         AG_EXNAME = 'POSITION'                                           410
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      410
         CALL AXPOSITION                                                  410
C    *                                                                    410
C    Geant POSITION : SSLB ===> X=-SFJP_SSLB_PX Y=-SFJP_SSLB_PX Z=-SFJP_  410
C    SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ/2. AL  410
C    PHAZ=SFJP_ALPHAZH <===                                               410
         CALL AGSRESET                                                    413
         AG_X = -SFJP_SSLB_PX                                             413
         AG_Y = -SFJP_SSLB_PX                                             413
         AG_Z =                                                           413
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          413
     *   SFJP_SSLB_DZ/2.                                                  413
         AG_ALPHAN=AG_ALPHAN+1                                            413
         AG_ALPHAK(AG_ALPHAN)=3                                           413
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                413
         AG_TITLE = 'SSLB'                                                413
         AG_EXNAME = 'POSITION'                                           413
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      413
         CALL AXPOSITION                                                  413
C    *                                                                    413
C    Geant POSITION : SSLB ===> X=SFJP_SSLB_PX Y=-SFJP_SSLB_PX Z=-SFJP_S  413
C    FLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ/2. ALP  413
C    HAZ=-SFJP_ALPHAZH <===                                               413
         CALL AGSRESET                                                    416
         AG_X = SFJP_SSLB_PX                                              416
         AG_Y = -SFJP_SSLB_PX                                             416
         AG_Z =                                                           416
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          416
     *   SFJP_SSLB_DZ/2.                                                  416
         AG_ALPHAN=AG_ALPHAN+1                                            416
         AG_ALPHAK(AG_ALPHAN)=3                                           416
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               416
         AG_TITLE = 'SSLB'                                                416
         AG_EXNAME = 'POSITION'                                           416
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      416
         CALL AXPOSITION                                                  416
C    *                                                                    416
C    Geant POSITION : SSLB ===> X=-SFJP_SSLB_PX Y=SFJP_SSLB_PX Z=-SFJP_S  416
C    FLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ/2. ALP  416
C    HAZ=-SFJP_ALPHAZH <===                                               416
         CALL AGSRESET                                                    419
         AG_X = -SFJP_SSLB_PX                                             419
         AG_Y = SFJP_SSLB_PX                                              419
         AG_Z =                                                           419
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          419
     *   SFJP_SSLB_DZ/2.                                                  419
         AG_ALPHAN=AG_ALPHAN+1                                            419
         AG_ALPHAK(AG_ALPHAN)=3                                           419
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               419
         AG_TITLE = 'SSLB'                                                419
         AG_EXNAME = 'POSITION'                                           419
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      419
         CALL AXPOSITION                                                  419
C    *                                                                    419
C    Geant POSITION : SSLB ===> X=SFJP_SSLB_PX Y=SFJP_SSLB_PX Z=+SFJP_SF  419
C    LU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ/2. ALPH  419
C    AZ=SFJP_ALPHAZH <===                                                 419
         CALL AGSRESET                                                    422
         AG_X = SFJP_SSLB_PX                                              422
         AG_Y = SFJP_SSLB_PX                                              422
         AG_Z =                                                           422
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          422
     *   SFJP_SSLB_DZ/2.                                                  422
         AG_ALPHAN=AG_ALPHAN+1                                            422
         AG_ALPHAK(AG_ALPHAN)=3                                           422
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                422
         AG_TITLE = 'SSLB'                                                422
         AG_EXNAME = 'POSITION'                                           422
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      422
         CALL AXPOSITION                                                  422
C    *                                                                    422
C    Geant POSITION : SSLB ===> X=-SFJP_SSLB_PX Y=-SFJP_SSLB_PX Z=+SFJP_  422
C    SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ/2. AL  422
C    PHAZ=SFJP_ALPHAZH <===                                               422
         CALL AGSRESET                                                    425
         AG_X = -SFJP_SSLB_PX                                             425
         AG_Y = -SFJP_SSLB_PX                                             425
         AG_Z =                                                           425
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          425
     *   SFJP_SSLB_DZ/2.                                                  425
         AG_ALPHAN=AG_ALPHAN+1                                            425
         AG_ALPHAK(AG_ALPHAN)=3                                           425
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                425
         AG_TITLE = 'SSLB'                                                425
         AG_EXNAME = 'POSITION'                                           425
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      425
         CALL AXPOSITION                                                  425
C    *                                                                    425
C    Geant POSITION : SSLB ===> X=SFJP_SSLB_PX Y=-SFJP_SSLB_PX Z=+SFJP_S  425
C    FLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ/2. ALP  425
C    HAZ=-SFJP_ALPHAZH <===                                               425
         CALL AGSRESET                                                    428
         AG_X = SFJP_SSLB_PX                                              428
         AG_Y = -SFJP_SSLB_PX                                             428
         AG_Z =                                                           428
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          428
     *   SFJP_SSLB_DZ/2.                                                  428
         AG_ALPHAN=AG_ALPHAN+1                                            428
         AG_ALPHAK(AG_ALPHAN)=3                                           428
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               428
         AG_TITLE = 'SSLB'                                                428
         AG_EXNAME = 'POSITION'                                           428
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      428
         CALL AXPOSITION                                                  428
C    *                                                                    428
C    Geant POSITION : SSLB ===> X=-SFJP_SSLB_PX Y=SFJP_SSLB_PX Z=+SFJP_S  428
C    FLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ/2. ALP  428
C    HAZ=-SFJP_ALPHAZH <===                                               428
         CALL AGSRESET                                                    431
         AG_X = -SFJP_SSLB_PX                                             431
         AG_Y = SFJP_SSLB_PX                                              431
         AG_Z =                                                           431
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          431
     *   SFJP_SSLB_DZ/2.                                                  431
         AG_ALPHAN=AG_ALPHAN+1                                            431
         AG_ALPHAK(AG_ALPHAN)=3                                           431
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               431
         AG_TITLE = 'SSLB'                                                431
         AG_EXNAME = 'POSITION'                                           431
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      431
         CALL AXPOSITION                                                  431
C    *                                                                    431
         ELSE                                                             431
C    Geant POSITION : SSLB ===> X=SFJP_SSLT_PX Y=SFJP_SSLT_PX Z=-SFJP_SF  435
C    LU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ/2. ALPH  435
C    AZ=0 <===                                                            435
         CALL AGSRESET                                                    435
         AG_X = SFJP_SSLT_PX                                              435
         AG_Y = SFJP_SSLT_PX                                              435
         AG_Z =                                                           435
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          435
     *   SFJP_SSLB_DZ/2.                                                  435
         AG_ALPHAN=AG_ALPHAN+1                                            435
         AG_ALPHAK(AG_ALPHAN)=3                                           435
         AG_ALPHAL(AG_ALPHAN)=0                                           435
         AG_TITLE = 'SSLB'                                                435
         AG_EXNAME = 'POSITION'                                           435
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      435
         CALL AXPOSITION                                                  435
C    *                                                                    435
C    Geant POSITION : SSLB ===> X=-SFJP_SSLT_PX Y=-SFJP_SSLT_PX Z=-SFJP_  435
C    SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ/2. AL  435
C    PHAZ=0 <===                                                          435
         CALL AGSRESET                                                    438
         AG_X = -SFJP_SSLT_PX                                             438
         AG_Y = -SFJP_SSLT_PX                                             438
         AG_Z =                                                           438
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          438
     *   SFJP_SSLB_DZ/2.                                                  438
         AG_ALPHAN=AG_ALPHAN+1                                            438
         AG_ALPHAK(AG_ALPHAN)=3                                           438
         AG_ALPHAL(AG_ALPHAN)=0                                           438
         AG_TITLE = 'SSLB'                                                438
         AG_EXNAME = 'POSITION'                                           438
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      438
         CALL AXPOSITION                                                  438
C    *                                                                    438
C    Geant POSITION : SSLB ===> X=SFJP_SSLT_PX Y=-SFJP_SSLT_PX Z=-SFJP_S  438
C    FLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ/2. ALP  438
C    HAZ=0 <===                                                           438
         CALL AGSRESET                                                    441
         AG_X = SFJP_SSLT_PX                                              441
         AG_Y = -SFJP_SSLT_PX                                             441
         AG_Z =                                                           441
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          441
     *   SFJP_SSLB_DZ/2.                                                  441
         AG_ALPHAN=AG_ALPHAN+1                                            441
         AG_ALPHAK(AG_ALPHAN)=3                                           441
         AG_ALPHAL(AG_ALPHAN)=0                                           441
         AG_TITLE = 'SSLB'                                                441
         AG_EXNAME = 'POSITION'                                           441
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      441
         CALL AXPOSITION                                                  441
C    *                                                                    441
C    Geant POSITION : SSLB ===> X=-SFJP_SSLT_PX Y=SFJP_SSLT_PX Z=-SFJP_S  441
C    FLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ/2. ALP  441
C    HAZ=0 <===                                                           441
         CALL AGSRESET                                                    444
         AG_X = -SFJP_SSLT_PX                                             444
         AG_Y = SFJP_SSLT_PX                                              444
         AG_Z =                                                           444
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          444
     *   SFJP_SSLB_DZ/2.                                                  444
         AG_ALPHAN=AG_ALPHAN+1                                            444
         AG_ALPHAK(AG_ALPHAN)=3                                           444
         AG_ALPHAL(AG_ALPHAN)=0                                           444
         AG_TITLE = 'SSLB'                                                444
         AG_EXNAME = 'POSITION'                                           444
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      444
         CALL AXPOSITION                                                  444
C    *                                                                    444
C    Geant POSITION : SSLB ===> X=SFJP_SSLT_PX Y=SFJP_SSLT_PX Z=+SFJP_SF  444
C    LU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ/2. ALPH  444
C    AY=180 <===                                                          444
         CALL AGSRESET                                                    447
         AG_X = SFJP_SSLT_PX                                              447
         AG_Y = SFJP_SSLT_PX                                              447
         AG_Z =                                                           447
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          447
     *   SFJP_SSLB_DZ/2.                                                  447
         AG_ALPHAN=AG_ALPHAN+1                                            447
         AG_ALPHAK(AG_ALPHAN)=2                                           447
         AG_ALPHAL(AG_ALPHAN)=180                                         447
         AG_TITLE = 'SSLB'                                                447
         AG_EXNAME = 'POSITION'                                           447
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      447
         CALL AXPOSITION                                                  447
C    *                                                                    447
C    Geant POSITION : SSLB ===> X=-SFJP_SSLT_PX Y=-SFJP_SSLT_PX Z=+SFJP_  447
C    SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ/2. AL  447
C    PHAY=180 <===                                                        447
         CALL AGSRESET                                                    450
         AG_X = -SFJP_SSLT_PX                                             450
         AG_Y = -SFJP_SSLT_PX                                             450
         AG_Z =                                                           450
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          450
     *   SFJP_SSLB_DZ/2.                                                  450
         AG_ALPHAN=AG_ALPHAN+1                                            450
         AG_ALPHAK(AG_ALPHAN)=2                                           450
         AG_ALPHAL(AG_ALPHAN)=180                                         450
         AG_TITLE = 'SSLB'                                                450
         AG_EXNAME = 'POSITION'                                           450
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      450
         CALL AXPOSITION                                                  450
C    *                                                                    450
C    Geant POSITION : SSLB ===> X=SFJP_SSLT_PX Y=-SFJP_SSLT_PX Z=+SFJP_S  450
C    FLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ/2. ALP  450
C    HAY=180 <===                                                         450
         CALL AGSRESET                                                    453
         AG_X = SFJP_SSLT_PX                                              453
         AG_Y = -SFJP_SSLT_PX                                             453
         AG_Z =                                                           453
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          453
     *   SFJP_SSLB_DZ/2.                                                  453
         AG_ALPHAN=AG_ALPHAN+1                                            453
         AG_ALPHAK(AG_ALPHAN)=2                                           453
         AG_ALPHAL(AG_ALPHAN)=180                                         453
         AG_TITLE = 'SSLB'                                                453
         AG_EXNAME = 'POSITION'                                           453
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      453
         CALL AXPOSITION                                                  453
C    *                                                                    453
C    Geant POSITION : SSLB ===> X=-SFJP_SSLT_PX Y=SFJP_SSLT_PX Z=+SFJP_S  453
C    FLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ/2. ALP  453
C    HAY=180 <===                                                         453
         CALL AGSRESET                                                    456
         AG_X = -SFJP_SSLT_PX                                             456
         AG_Y = SFJP_SSLT_PX                                              456
         AG_Z =                                                           456
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          456
     *   SFJP_SSLB_DZ/2.                                                  456
         AG_ALPHAN=AG_ALPHAN+1                                            456
         AG_ALPHAK(AG_ALPHAN)=2                                           456
         AG_ALPHAL(AG_ALPHAN)=180                                         456
         AG_TITLE = 'SSLB'                                                456
         AG_EXNAME = 'POSITION'                                           456
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      456
         CALL AXPOSITION                                                  456
C    *                                                                    456
         ENDIF                                                            456
C    Create SSLT :                                                        458
C    Call To Local Procedure SSLT                                         458
         ASSIGN 5200 TO BLOCK_SSLT                                        458
         GO TO  5190                                                      458
5200     CONTINUE                                                         458
C    *                                                                    458
C    Geant POSITION : SSLT ===> X=SFJP_SSLT_PX Y=SFJP_SSLT_PX Z=-SFJP_SF  460
C    LU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ-SFJP_SS  460
C    LT_DZ/2. <===                                                        460
         CALL AGSRESET                                                    460
         AG_X = SFJP_SSLT_PX                                              460
         AG_Y = SFJP_SSLT_PX                                              460
         AG_Z =                                                           460
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          460
     *   SFJP_SSLB_DZ-SFJP_SSLT_DZ/2.                                     460
         AG_TITLE = 'SSLT'                                                460
         AG_EXNAME = 'POSITION'                                           460
         AG_PARLIST = 'X_Y_Z'                                             460
         CALL AXPOSITION                                                  460
C    *                                                                    460
C    Geant POSITION : SSLT ===> X=-SFJP_SSLT_PX Y=-SFJP_SSLT_PX Z=-SFJP_  460
C    SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ-SFJP_  460
C    SSLT_DZ/2. <===                                                      460
         CALL AGSRESET                                                    462
         AG_X = -SFJP_SSLT_PX                                             462
         AG_Y = -SFJP_SSLT_PX                                             462
         AG_Z =                                                           462
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          462
     *   SFJP_SSLB_DZ-SFJP_SSLT_DZ/2.                                     462
         AG_TITLE = 'SSLT'                                                462
         AG_EXNAME = 'POSITION'                                           462
         AG_PARLIST = 'X_Y_Z'                                             462
         CALL AXPOSITION                                                  462
C    *                                                                    462
C    Geant POSITION : SSLT ===> X=SFJP_SSLT_PX Y=-SFJP_SSLT_PX Z=-SFJP_S  462
C    FLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ-SFJP_S  462
C    SLT_DZ/2. <===                                                       462
         CALL AGSRESET                                                    464
         AG_X = SFJP_SSLT_PX                                              464
         AG_Y = -SFJP_SSLT_PX                                             464
         AG_Z =                                                           464
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          464
     *   SFJP_SSLB_DZ-SFJP_SSLT_DZ/2.                                     464
         AG_TITLE = 'SSLT'                                                464
         AG_EXNAME = 'POSITION'                                           464
         AG_PARLIST = 'X_Y_Z'                                             464
         CALL AXPOSITION                                                  464
C    *                                                                    464
C    Geant POSITION : SSLT ===> X=-SFJP_SSLT_PX Y=SFJP_SSLT_PX Z=-SFJP_S  464
C    FLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ-SFJP_S  464
C    SLT_DZ/2. <===                                                       464
         CALL AGSRESET                                                    466
         AG_X = -SFJP_SSLT_PX                                             466
         AG_Y = SFJP_SSLT_PX                                              466
         AG_Z =                                                           466
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          466
     *   SFJP_SSLB_DZ-SFJP_SSLT_DZ/2.                                     466
         AG_TITLE = 'SSLT'                                                466
         AG_EXNAME = 'POSITION'                                           466
         AG_PARLIST = 'X_Y_Z'                                             466
         CALL AXPOSITION                                                  466
C    *                                                                    466
C    Geant POSITION : SSLT ===> X=SFJP_SSLT_PX Y=SFJP_SSLT_PX Z=+SFJP_SF  466
C    LU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ+SFJP_SS  466
C    LT_DZ/2. <===                                                        466
         CALL AGSRESET                                                    468
         AG_X = SFJP_SSLT_PX                                              468
         AG_Y = SFJP_SSLT_PX                                              468
         AG_Z =                                                           468
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          468
     *   SFJP_SSLB_DZ+SFJP_SSLT_DZ/2.                                     468
         AG_TITLE = 'SSLT'                                                468
         AG_EXNAME = 'POSITION'                                           468
         AG_PARLIST = 'X_Y_Z'                                             468
         CALL AXPOSITION                                                  468
C    *                                                                    468
C    Geant POSITION : SSLT ===> X=-SFJP_SSLT_PX Y=-SFJP_SSLT_PX Z=+SFJP_  468
C    SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ+SFJP_  468
C    SSLT_DZ/2. <===                                                      468
         CALL AGSRESET                                                    470
         AG_X = -SFJP_SSLT_PX                                             470
         AG_Y = -SFJP_SSLT_PX                                             470
         AG_Z =                                                           470
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          470
     *   SFJP_SSLB_DZ+SFJP_SSLT_DZ/2.                                     470
         AG_TITLE = 'SSLT'                                                470
         AG_EXNAME = 'POSITION'                                           470
         AG_PARLIST = 'X_Y_Z'                                             470
         CALL AXPOSITION                                                  470
C    *                                                                    470
C    Geant POSITION : SSLT ===> X=SFJP_SSLT_PX Y=-SFJP_SSLT_PX Z=+SFJP_S  470
C    FLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ+SFJP_S  470
C    SLT_DZ/2. <===                                                       470
         CALL AGSRESET                                                    472
         AG_X = SFJP_SSLT_PX                                              472
         AG_Y = -SFJP_SSLT_PX                                             472
         AG_Z =                                                           472
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          472
     *   SFJP_SSLB_DZ+SFJP_SSLT_DZ/2.                                     472
         AG_TITLE = 'SSLT'                                                472
         AG_EXNAME = 'POSITION'                                           472
         AG_PARLIST = 'X_Y_Z'                                             472
         CALL AXPOSITION                                                  472
C    *                                                                    472
C    Geant POSITION : SSLT ===> X=-SFJP_SSLT_PX Y=SFJP_SSLT_PX Z=+SFJP_S  472
C    FLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ+SFJP_S  472
C    SLT_DZ/2. <===                                                       472
         CALL AGSRESET                                                    474
         AG_X = -SFJP_SSLT_PX                                             474
         AG_Y = SFJP_SSLT_PX                                              474
         AG_Z =                                                           474
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          474
     *   SFJP_SSLB_DZ+SFJP_SSLT_DZ/2.                                     474
         AG_TITLE = 'SSLT'                                                474
         AG_EXNAME = 'POSITION'                                           474
         AG_PARLIST = 'X_Y_Z'                                             474
         CALL AXPOSITION                                                  474
C    *                                                                    474
C    Create SCMP :                                                        475
C    Call To Local Procedure SCMP                                         475
         ASSIGN 5220 TO BLOCK_SCMP                                        475
         GO TO  5210                                                      475
5220     CONTINUE                                                         475
C    *                                                                    475
C    Geant POSITION : SCMP ===> X=SFJP_SCMP_PX Y=SFJP_SCMP_PX Z=SFJP_SCV  476
C    M_PZ ALPHAZ=-SFJP_ALPHAZH <===                                       476
         CALL AGSRESET                                                    476
         AG_X = SFJP_SCMP_PX                                              476
         AG_Y = SFJP_SCMP_PX                                              476
         AG_Z = SFJP_SCVM_PZ                                              476
         AG_ALPHAN=AG_ALPHAN+1                                            476
         AG_ALPHAK(AG_ALPHAN)=3                                           476
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               476
         AG_TITLE = 'SCMP'                                                476
         AG_EXNAME = 'POSITION'                                           476
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      476
         CALL AXPOSITION                                                  476
C    *                                                                    476
C    Geant POSITION : SCMP ===> X=-SFJP_SCMP_PX Y=-SFJP_SCMP_PX Z=SFJP_S  476
C    CVM_PZ ALPHAZ=-SFJP_ALPHAZH <===                                     476
         CALL AGSRESET                                                    478
         AG_X = -SFJP_SCMP_PX                                             478
         AG_Y = -SFJP_SCMP_PX                                             478
         AG_Z = SFJP_SCVM_PZ                                              478
         AG_ALPHAN=AG_ALPHAN+1                                            478
         AG_ALPHAK(AG_ALPHAN)=3                                           478
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               478
         AG_TITLE = 'SCMP'                                                478
         AG_EXNAME = 'POSITION'                                           478
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      478
         CALL AXPOSITION                                                  478
C    *                                                                    478
C    Geant POSITION : SCMP ===> X=SFJP_SCMP_PX Y=-SFJP_SCMP_PX Z=SFJP_SC  478
C    VM_PZ ALPHAZ=SFJP_ALPHAZH <===                                       478
         CALL AGSRESET                                                    479
         AG_X = SFJP_SCMP_PX                                              479
         AG_Y = -SFJP_SCMP_PX                                             479
         AG_Z = SFJP_SCVM_PZ                                              479
         AG_ALPHAN=AG_ALPHAN+1                                            479
         AG_ALPHAK(AG_ALPHAN)=3                                           479
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                479
         AG_TITLE = 'SCMP'                                                479
         AG_EXNAME = 'POSITION'                                           479
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      479
         CALL AXPOSITION                                                  479
C    *                                                                    479
C    Geant POSITION : SCMP ===> X=-SFJP_SCMP_PX Y=SFJP_SCMP_PX Z=SFJP_SC  479
C    VM_PZ ALPHAZ=SFJP_ALPHAZH <===                                       479
         CALL AGSRESET                                                    480
         AG_X = -SFJP_SCMP_PX                                             480
         AG_Y = SFJP_SCMP_PX                                              480
         AG_Z = SFJP_SCVM_PZ                                              480
         AG_ALPHAN=AG_ALPHAN+1                                            480
         AG_ALPHAK(AG_ALPHAN)=3                                           480
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                480
         AG_TITLE = 'SCMP'                                                480
         AG_EXNAME = 'POSITION'                                           480
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      480
         CALL AXPOSITION                                                  480
C    *                                                                    480
C    Geant POSITION : SCMP ===> X=SFJP_SCMP_PX Y=SFJP_SCMP_PX Z=-SFJP_SC  480
C    VM_PZ ALPHAZ=-SFJP_ALPHAZH <===                                      480
         CALL AGSRESET                                                    482
         AG_X = SFJP_SCMP_PX                                              482
         AG_Y = SFJP_SCMP_PX                                              482
         AG_Z = -SFJP_SCVM_PZ                                             482
         AG_ALPHAN=AG_ALPHAN+1                                            482
         AG_ALPHAK(AG_ALPHAN)=3                                           482
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               482
         AG_TITLE = 'SCMP'                                                482
         AG_EXNAME = 'POSITION'                                           482
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      482
         CALL AXPOSITION                                                  482
C    *                                                                    482
C    Geant POSITION : SCMP ===> X=-SFJP_SCMP_PX Y=-SFJP_SCMP_PX Z=-SFJP_  482
C    SCVM_PZ ALPHAZ=-SFJP_ALPHAZH <===                                    482
         CALL AGSRESET                                                    484
         AG_X = -SFJP_SCMP_PX                                             484
         AG_Y = -SFJP_SCMP_PX                                             484
         AG_Z = -SFJP_SCVM_PZ                                             484
         AG_ALPHAN=AG_ALPHAN+1                                            484
         AG_ALPHAK(AG_ALPHAN)=3                                           484
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               484
         AG_TITLE = 'SCMP'                                                484
         AG_EXNAME = 'POSITION'                                           484
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      484
         CALL AXPOSITION                                                  484
C    *                                                                    484
C    Geant POSITION : SCMP ===> X=SFJP_SCMP_PX Y=-SFJP_SCMP_PX Z=-SFJP_S  484
C    CVM_PZ ALPHAZ=SFJP_ALPHAZH <===                                      484
         CALL AGSRESET                                                    486
         AG_X = SFJP_SCMP_PX                                              486
         AG_Y = -SFJP_SCMP_PX                                             486
         AG_Z = -SFJP_SCVM_PZ                                             486
         AG_ALPHAN=AG_ALPHAN+1                                            486
         AG_ALPHAK(AG_ALPHAN)=3                                           486
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                486
         AG_TITLE = 'SCMP'                                                486
         AG_EXNAME = 'POSITION'                                           486
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      486
         CALL AXPOSITION                                                  486
C    *                                                                    486
C    Geant POSITION : SCMP ===> X=-SFJP_SCMP_PX Y=SFJP_SCMP_PX Z=-SFJP_S  486
C    CVM_PZ ALPHAZ=SFJP_ALPHAZH <===                                      486
         CALL AGSRESET                                                    488
         AG_X = -SFJP_SCMP_PX                                             488
         AG_Y = SFJP_SCMP_PX                                              488
         AG_Z = -SFJP_SCVM_PZ                                             488
         AG_ALPHAN=AG_ALPHAN+1                                            488
         AG_ALPHAK(AG_ALPHAN)=3                                           488
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                488
         AG_TITLE = 'SCMP'                                                488
         AG_EXNAME = 'POSITION'                                           488
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      488
         CALL AXPOSITION                                                  488
C    *                                                                    488
C    Create SCVM :                                                        489
C    Call To Local Procedure SCVM                                         489
         ASSIGN 5240 TO BLOCK_SCVM                                        489
         GO TO  5230                                                      489
5240     CONTINUE                                                         489
C    *                                                                    489
C    Geant POSITION : SCVM ===> X=SFJP_SCVM_PX Y=SFJP_SCVM_PX Z=SFJP_SCV  490
C    M_PZ ALPHAZ=-SFJP_ALPHAZH <===                                       490
         CALL AGSRESET                                                    490
         AG_X = SFJP_SCVM_PX                                              490
         AG_Y = SFJP_SCVM_PX                                              490
         AG_Z = SFJP_SCVM_PZ                                              490
         AG_ALPHAN=AG_ALPHAN+1                                            490
         AG_ALPHAK(AG_ALPHAN)=3                                           490
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               490
         AG_TITLE = 'SCVM'                                                490
         AG_EXNAME = 'POSITION'                                           490
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      490
         CALL AXPOSITION                                                  490
C    *                                                                    490
C    Geant POSITION : SCVM ===> X=-SFJP_SCVM_PX Y=-SFJP_SCVM_PX Z=SFJP_S  490
C    CVM_PZ ALPHAZ=(180.-SFJP_ALPHAZH) <===                               490
         CALL AGSRESET                                                    492
         AG_X = -SFJP_SCVM_PX                                             492
         AG_Y = -SFJP_SCVM_PX                                             492
         AG_Z = SFJP_SCVM_PZ                                              492
         AG_ALPHAN=AG_ALPHAN+1                                            492
         AG_ALPHAK(AG_ALPHAN)=3                                           492
         AG_ALPHAL(AG_ALPHAN)=(180.-SFJP_ALPHAZH)                         492
         AG_TITLE = 'SCVM'                                                492
         AG_EXNAME = 'POSITION'                                           492
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      492
         CALL AXPOSITION                                                  492
C    *                                                                    492
C    Geant POSITION : SCVM ===> X=SFJP_SCVM_PX Y=-SFJP_SCVM_PX Z=SFJP_SC  492
C    VM_PZ ALPHAZ=-(180.-SFJP_ALPHAZH) <===                               492
         CALL AGSRESET                                                    494
         AG_X = SFJP_SCVM_PX                                              494
         AG_Y = -SFJP_SCVM_PX                                             494
         AG_Z = SFJP_SCVM_PZ                                              494
         AG_ALPHAN=AG_ALPHAN+1                                            494
         AG_ALPHAK(AG_ALPHAN)=3                                           494
         AG_ALPHAL(AG_ALPHAN)=-(180.-SFJP_ALPHAZH)                        494
         AG_TITLE = 'SCVM'                                                494
         AG_EXNAME = 'POSITION'                                           494
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      494
         CALL AXPOSITION                                                  494
C    *                                                                    494
C    Geant POSITION : SCVM ===> X=-SFJP_SCVM_PX Y=SFJP_SCVM_PX Z=SFJP_SC  494
C    VM_PZ ALPHAZ=SFJP_ALPHAZH <===                                       494
         CALL AGSRESET                                                    495
         AG_X = -SFJP_SCVM_PX                                             495
         AG_Y = SFJP_SCVM_PX                                              495
         AG_Z = SFJP_SCVM_PZ                                              495
         AG_ALPHAN=AG_ALPHAN+1                                            495
         AG_ALPHAK(AG_ALPHAN)=3                                           495
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                495
         AG_TITLE = 'SCVM'                                                495
         AG_EXNAME = 'POSITION'                                           495
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      495
         CALL AXPOSITION                                                  495
C    *                                                                    495
C    Geant POSITION : SCVM ===> X=SFJP_SCVM_PX Y=SFJP_SCVM_PX Z=-SFJP_SC  495
C    VM_PZ ALPHAZ=-SFJP_ALPHAZH <===                                      495
         CALL AGSRESET                                                    497
         AG_X = SFJP_SCVM_PX                                              497
         AG_Y = SFJP_SCVM_PX                                              497
         AG_Z = -SFJP_SCVM_PZ                                             497
         AG_ALPHAN=AG_ALPHAN+1                                            497
         AG_ALPHAK(AG_ALPHAN)=3                                           497
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               497
         AG_TITLE = 'SCVM'                                                497
         AG_EXNAME = 'POSITION'                                           497
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      497
         CALL AXPOSITION                                                  497
C    *                                                                    497
C    Geant POSITION : SCVM ===> X=-SFJP_SCVM_PX Y=-SFJP_SCVM_PX Z=-SFJP_  497
C    SCVM_PZ ALPHAZ=(180.-SFJP_ALPHAZH) <===                              497
         CALL AGSRESET                                                    499
         AG_X = -SFJP_SCVM_PX                                             499
         AG_Y = -SFJP_SCVM_PX                                             499
         AG_Z = -SFJP_SCVM_PZ                                             499
         AG_ALPHAN=AG_ALPHAN+1                                            499
         AG_ALPHAK(AG_ALPHAN)=3                                           499
         AG_ALPHAL(AG_ALPHAN)=(180.-SFJP_ALPHAZH)                         499
         AG_TITLE = 'SCVM'                                                499
         AG_EXNAME = 'POSITION'                                           499
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      499
         CALL AXPOSITION                                                  499
C    *                                                                    499
C    Geant POSITION : SCVM ===> X=SFJP_SCVM_PX Y=-SFJP_SCVM_PX Z=-SFJP_S  499
C    CVM_PZ ALPHAZ=-(180.-SFJP_ALPHAZH) <===                              499
         CALL AGSRESET                                                    501
         AG_X = SFJP_SCVM_PX                                              501
         AG_Y = -SFJP_SCVM_PX                                             501
         AG_Z = -SFJP_SCVM_PZ                                             501
         AG_ALPHAN=AG_ALPHAN+1                                            501
         AG_ALPHAK(AG_ALPHAN)=3                                           501
         AG_ALPHAL(AG_ALPHAN)=-(180.-SFJP_ALPHAZH)                        501
         AG_TITLE = 'SCVM'                                                501
         AG_EXNAME = 'POSITION'                                           501
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      501
         CALL AXPOSITION                                                  501
C    *                                                                    501
C    Geant POSITION : SCVM ===> X=-SFJP_SCVM_PX Y=SFJP_SCVM_PX Z=-SFJP_S  501
C    CVM_PZ ALPHAZ=SFJP_ALPHAZH <===                                      501
         CALL AGSRESET                                                    503
         AG_X = -SFJP_SCVM_PX                                             503
         AG_Y = SFJP_SCVM_PX                                              503
         AG_Z = -SFJP_SCVM_PZ                                             503
         AG_ALPHAN=AG_ALPHAN+1                                            503
         AG_ALPHAK(AG_ALPHAN)=3                                           503
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                503
         AG_TITLE = 'SCVM'                                                503
         AG_EXNAME = 'POSITION'                                           503
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      503
         CALL AXPOSITION                                                  503
C    *                                                                    503
5011  CALL AGSPOPB                                                        504
      GO TO BLOCK_SFMO, (5010 ,5020,5030)                                 504
C End of Local Procedure                                                  504
C ----------------------------------------------------------------------  505
C ------------ SFLM                                                       505
C Begin Local Procedure SFLM                                              506
C SFLM IS THE MOTHER OF THE LADDER                                        506
5060  CONTINUE                                                            506
         AG_TITLE='SFLM'                                                  506
         AG_MODULE=MODULE                                                 506
         AG_MARK='V00'                                                    506
         CALL AGSPUSH                                                     506
         AG_MODULE=MODULE                                                 506
C    *                                                                    506
C    Geant MATERIAL : AIR ===> inherited arguments used <===              507
         CALL AGSRESET                                                    507
         AG_TITLE = 'AIR'                                                 507
         AG_EXNAME = 'MATERIAL'                                           507
         AG_PARLIST = 'NONE'                                              507
         CALL AXMATERIAL                                                  507
C    *                                                                    507
C    Geant ATTRIBUTE : SFLM ===> SEEN=1 COLO=6 <===                       507
         CALL AGSRESET                                                    508
         AG_SEEN = 1                                                      508
         AG_COLO = 6                                                      508
         AG_TITLE = 'SFLM'                                                508
         AG_EXNAME = 'ATTRIBUTE'                                          508
         AG_PARLIST = 'SEEN_COLO'                                         508
         CALL AXATTRIBUTE                                                 508
C    *                                                                    508
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=(SFPA_DMTHK+SFPA_GPTHK+SF  508
C    PA_SMTHK)/2, DZ=SFPA_SMLEN/2 <===                                    508
         CALL AGSRESET                                                    510
         AG_DX = SFPA_DMWID/2                                             510
         AG_DY = (SFPA_DMTHK+SFPA_GPTHK+SFPA_SMTHK)/2                     510
         AG_DZ = SFPA_SMLEN/2                                             510
         AG_TITLE = 'BOX'                                                 510
         AG_EXNAME = 'SHAPE'                                              510
         AG_PARLIST = 'DX_DY_DZ'                                          510
         CALL AXSHAPE                                                     510
C    *                                                                    510
C    Create SFDM :                                                        511
C    Call To Local Procedure SFDM                                         511
         ASSIGN 5260 TO BLOCK_SFDM                                        511
         GO TO  5250                                                      511
5260     CONTINUE                                                         511
C    *                                                                    511
C    Geant POSITION : SFDM ===> Y=-(SFPA_SMTHK+SFPA_GPTHK)/2 <===         512
         CALL AGSRESET                                                    512
         AG_Y = -(SFPA_SMTHK+SFPA_GPTHK)/2                                512
         AG_TITLE = 'SFDM'                                                512
         AG_EXNAME = 'POSITION'                                           512
         AG_PARLIST = 'Y'                                                 512
         CALL AXPOSITION                                                  512
C    *                                                                    512
C    Create SFSM :                                                        513
C    Call To Local Procedure SFSM                                         513
         ASSIGN 5280 TO BLOCK_SFSM                                        513
         GO TO  5270                                                      513
5280     CONTINUE                                                         513
C    *                                                                    513
C    Geant POSITION : SFSM ===> Y=(SFPA_DMTHK+SFPA_GPTHK)/2 <===          514
         CALL AGSRESET                                                    514
         AG_Y = (SFPA_DMTHK+SFPA_GPTHK)/2                                 514
         AG_TITLE = 'SFSM'                                                514
         AG_EXNAME = 'POSITION'                                           514
         AG_PARLIST = 'Y'                                                 514
         CALL AXPOSITION                                                  514
C    *                                                                    514
5061  CALL AGSPOPB                                                        515
      GO TO BLOCK_SFLM, (5060 ,5070)                                      515
C End of Local Procedure                                                  515
C ----------------------------------------------------------------------  516
C ------------ SFDM                                                       516
C Begin Local Procedure SFDM                                              517
C SFDM IS THE MOTHER OF THE DETECTORS                                     517
5250  CONTINUE                                                            517
         AG_TITLE='SFDM'                                                  517
         AG_MODULE=MODULE                                                 517
         AG_MARK='V00'                                                    517
         CALL AGSPUSH                                                     517
         AG_MODULE=MODULE                                                 517
C    *                                                                    517
C    Geant MATERIAL : AIR ===> inherited arguments used <===              518
         CALL AGSRESET                                                    518
         AG_TITLE = 'AIR'                                                 518
         AG_EXNAME = 'MATERIAL'                                           518
         AG_PARLIST = 'NONE'                                              518
         CALL AXMATERIAL                                                  518
C    *                                                                    518
C    Geant ATTRIBUTE : SFDM ===> SEEN=0 COLO=6 <===                       518
         CALL AGSRESET                                                    519
         AG_SEEN = 0                                                      519
         AG_COLO = 6                                                      519
         AG_TITLE = 'SFDM'                                                519
         AG_EXNAME = 'ATTRIBUTE'                                          519
         AG_PARLIST = 'SEEN_COLO'                                         519
         CALL AXATTRIBUTE                                                 519
C    *                                                                    519
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=SFPA_DMTHK/2, DZ=SFPA_WPL  519
C    EN/2 <===                                                            519
         CALL AGSRESET                                                    521
         AG_DX = SFPA_DMWID/2                                             521
         AG_DY = SFPA_DMTHK/2                                             521
         AG_DZ = SFPA_WPLEN/2                                             521
         AG_TITLE = 'BOX'                                                 521
         AG_EXNAME = 'SHAPE'                                              521
         AG_PARLIST = 'DX_DY_DZ'                                          521
         CALL AXSHAPE                                                     521
C    *                                                                    521
         WAFPCKLEN=SFPA_WPLEN/(SFPA_NSSD*1.)                              521
C    *                                                                    523
         IWAF=1                                                           523
            GO TO 5293                                                    523
5291        IWAF=IWAF+1                                                   523
5293        IF(IWAF-(SFPA_NSSD).GT.0) GO TO 5292                          523
C       Create SFSW :                                                     524
C       Call To Local Procedure SFSW                                      524
            ASSIGN 5310 TO BLOCK_SFSW                                     524
            GO TO  5300                                                   524
5310        CONTINUE                                                      524
C       *                                                                 524
C       Geant POSITION : SFSW ===> Z=-(SFPA_WPLEN+WAFPCKLEN)/2+IWAF*WAFP  525
C       CKLEN <===                                                        525
            CALL AGSRESET                                                 525
            AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+IWAF*WAFPCKLEN               525
            AG_TITLE = 'SFSW'                                             525
            AG_EXNAME = 'POSITION'                                        525
            AG_PARLIST = 'Z'                                              525
            CALL AXPOSITION                                               525
C       *                                                                 525
         GO TO 5291                                                       526
5292     CONTINUE                                                         526
5251  CALL AGSPOPB                                                        527
      GO TO BLOCK_SFDM, (5250 ,5260)                                      527
C End of Local Procedure                                                  527
C ----------------------------------------------------------------------  528
C ------------ SFSW                                                       528
C Begin Local Procedure SFSW                                              529
C SFSW IS A SINGLE WAFER CONTAINER                                        529
5300  CONTINUE                                                            529
         AG_TITLE='SFSW'                                                  529
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
C    Geant ATTRIBUTE : SFSW ===> SEEN=0 COLO=6 <===                       530
         CALL AGSRESET                                                    531
         AG_SEEN = 0                                                      531
         AG_COLO = 6                                                      531
         AG_TITLE = 'SFSW'                                                531
         AG_EXNAME = 'ATTRIBUTE'                                          531
         AG_PARLIST = 'SEEN_COLO'                                         531
         CALL AXATTRIBUTE                                                 531
C    *                                                                    531
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=SFJP_SFSW_DY, DZ=SFJP_SFS  531
C    W_DZ <===                                                            531
         CALL AGSRESET                                                    533
         AG_DX = SFPA_DMWID/2                                             533
         AG_DY = SFJP_SFSW_DY                                             533
         AG_DZ = SFJP_SFSW_DZ                                             533
         AG_TITLE = 'BOX'                                                 533
         AG_EXNAME = 'SHAPE'                                              533
         AG_PARLIST = 'DX_DY_DZ'                                          533
         CALL AXSHAPE                                                     533
C    *                                                                    533
C    Create SFSL :                                                        534
C    Call To Local Procedure SFSL                                         534
         ASSIGN 5330 TO BLOCK_SFSL                                        534
         GO TO  5320                                                      534
5330     CONTINUE                                                         534
C    *                                                                    534
C    Geant POSITION : SFSL ===> inherited arguments used <===             535
         CALL AGSRESET                                                    535
         AG_TITLE = 'SFSL'                                                535
         AG_EXNAME = 'POSITION'                                           535
         AG_PARLIST = 'NONE'                                              535
         CALL AXPOSITION                                                  535
C    *                                                                    535
C    Create SFRA :                                                        536
C    Call To Local Procedure SFRA                                         536
         ASSIGN 5350 TO BLOCK_SFRA                                        536
         GO TO  5340                                                      536
5350     CONTINUE                                                         536
C    *                                                                    536
C    Geant POSITION : SFRA ===> X=0.0 Y=SFJP_SFRA_PY Z=SFJP_SFRA_PZ <===  537
         CALL AGSRESET                                                    537
         AG_X = 0.0                                                       537
         AG_Y = SFJP_SFRA_PY                                              537
         AG_Z = SFJP_SFRA_PZ                                              537
         AG_TITLE = 'SFRA'                                                537
         AG_EXNAME = 'POSITION'                                           537
         AG_PARLIST = 'X_Y_Z'                                             537
         CALL AXPOSITION                                                  537
C    *                                                                    537
C    Geant POSITION : SFRA ===> X=0.0 Y=SFJP_SFRA_PY Z=-SFJP_SFRA_PZ <==  537
C    =                                                                    537
         CALL AGSRESET                                                    538
         AG_X = 0.0                                                       538
         AG_Y = SFJP_SFRA_PY                                              538
         AG_Z = -SFJP_SFRA_PZ                                             538
         AG_TITLE = 'SFRA'                                                538
         AG_EXNAME = 'POSITION'                                           538
         AG_PARLIST = 'X_Y_Z'                                             538
         CALL AXPOSITION                                                  538
C    *                                                                    538
C    Create SFRS :                                                        539
C    Call To Local Procedure SFRS                                         539
         ASSIGN 5370 TO BLOCK_SFRS                                        539
         GO TO  5360                                                      539
5370     CONTINUE                                                         539
C    *                                                                    539
C    Geant POSITION : SFRS ===> X=SFJP_SFRS_PX Y=SFJP_SFRS_PY Z=SFJP_SFR  540
C    A_PZ <===                                                            540
         CALL AGSRESET                                                    540
         AG_X = SFJP_SFRS_PX                                              540
         AG_Y = SFJP_SFRS_PY                                              540
         AG_Z = SFJP_SFRA_PZ                                              540
         AG_TITLE = 'SFRS'                                                540
         AG_EXNAME = 'POSITION'                                           540
         AG_PARLIST = 'X_Y_Z'                                             540
         CALL AXPOSITION                                                  540
C    *                                                                    540
C    Geant POSITION : SFRS ===> X=SFJP_SFRS_PX Y=SFJP_SFRS_PY Z=-SFJP_SF  540
C    RA_PZ <===                                                           540
         CALL AGSRESET                                                    541
         AG_X = SFJP_SFRS_PX                                              541
         AG_Y = SFJP_SFRS_PY                                              541
         AG_Z = -SFJP_SFRA_PZ                                             541
         AG_TITLE = 'SFRS'                                                541
         AG_EXNAME = 'POSITION'                                           541
         AG_PARLIST = 'X_Y_Z'                                             541
         CALL AXPOSITION                                                  541
C    *                                                                    541
C    Geant POSITION : SFRS ===> X=-SFJP_SFRS_PX Y=SFJP_SFRS_PY Z=SFJP_SF  541
C    RA_PZ <===                                                           541
         CALL AGSRESET                                                    542
         AG_X = -SFJP_SFRS_PX                                             542
         AG_Y = SFJP_SFRS_PY                                              542
         AG_Z = SFJP_SFRA_PZ                                              542
         AG_TITLE = 'SFRS'                                                542
         AG_EXNAME = 'POSITION'                                           542
         AG_PARLIST = 'X_Y_Z'                                             542
         CALL AXPOSITION                                                  542
C    *                                                                    542
C    Geant POSITION : SFRS ===> X=-SFJP_SFRS_PX Y=SFJP_SFRS_PY Z=-SFJP_S  542
C    FRA_PZ <===                                                          542
         CALL AGSRESET                                                    543
         AG_X = -SFJP_SFRS_PX                                             543
         AG_Y = SFJP_SFRS_PY                                              543
         AG_Z = -SFJP_SFRA_PZ                                             543
         AG_TITLE = 'SFRS'                                                543
         AG_EXNAME = 'POSITION'                                           543
         AG_PARLIST = 'X_Y_Z'                                             543
         CALL AXPOSITION                                                  543
C    *                                                                    543
C    Create SFFX :                                                        544
C    Call To Local Procedure SFFX                                         544
         ASSIGN 5390 TO BLOCK_SFFX                                        544
         GO TO  5380                                                      544
5390     CONTINUE                                                         544
C    *                                                                    544
C    Geant POSITION : SFFX ===> X=0.0 Y=SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_S  545
C    FFX_DYZ Z=SFJP_SFRA_PZ <===                                          545
         CALL AGSRESET                                                    545
         AG_X = 0.0                                                       545
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ                   545
         AG_Z = SFJP_SFRA_PZ                                              545
         AG_TITLE = 'SFFX'                                                545
         AG_EXNAME = 'POSITION'                                           545
         AG_PARLIST = 'X_Y_Z'                                             545
         CALL AXPOSITION                                                  545
C    *                                                                    545
C    Geant POSITION : SFFX ===> X=0.0 Y=SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_S  545
C    FFX_DYZ Z=-SFJP_SFRA_PZ <===                                         545
         CALL AGSRESET                                                    546
         AG_X = 0.0                                                       546
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ                   546
         AG_Z = -SFJP_SFRA_PZ                                             546
         AG_TITLE = 'SFFX'                                                546
         AG_EXNAME = 'POSITION'                                           546
         AG_PARLIST = 'X_Y_Z'                                             546
         CALL AXPOSITION                                                  546
C    *                                                                    546
C    Create SFPI :                                                        547
C    Call To Local Procedure SFPI                                         547
         ASSIGN 5410 TO BLOCK_SFPI                                        547
         GO TO  5400                                                      547
5410     CONTINUE                                                         547
C    *                                                                    547
C    Geant POSITION : SFPI ===> X=SFJP_SFPI_PX Y=SFJP_SFRA_PY+SFJP_SFRA_  550
C    DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2 Z=SFJP_SFPI_PZ ALPHAX=90 <===         550
         CALL AGSRESET                                                    550
         AG_X = SFJP_SFPI_PX                                              550
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2     550
         AG_Z = SFJP_SFPI_PZ                                              550
         AG_ALPHAN=AG_ALPHAN+1                                            550
         AG_ALPHAK(AG_ALPHAN)=1                                           550
         AG_ALPHAL(AG_ALPHAN)=90                                          550
         AG_TITLE = 'SFPI'                                                550
         AG_EXNAME = 'POSITION'                                           550
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      550
         CALL AXPOSITION                                                  550
C    *                                                                    550
C    Geant POSITION : SFPI ===> X=-SFJP_SFPI_PX Y=SFJP_SFRA_PY+SFJP_SFRA  550
C    _DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2 Z=SFJP_SFPI_PZ ALPHAX=90 <===        550
         CALL AGSRESET                                                    553
         AG_X = -SFJP_SFPI_PX                                             553
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2     553
         AG_Z = SFJP_SFPI_PZ                                              553
         AG_ALPHAN=AG_ALPHAN+1                                            553
         AG_ALPHAK(AG_ALPHAN)=1                                           553
         AG_ALPHAL(AG_ALPHAN)=90                                          553
         AG_TITLE = 'SFPI'                                                553
         AG_EXNAME = 'POSITION'                                           553
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      553
         CALL AXPOSITION                                                  553
C    *                                                                    553
C    Geant POSITION : SFPI ===> X=SFJP_SFPI_PX Y=SFJP_SFRA_PY+SFJP_SFRA_  553
C    DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2 Z=-SFJP_SFPI_PZ ALPHAX=90 <===        553
         CALL AGSRESET                                                    556
         AG_X = SFJP_SFPI_PX                                              556
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2     556
         AG_Z = -SFJP_SFPI_PZ                                             556
         AG_ALPHAN=AG_ALPHAN+1                                            556
         AG_ALPHAK(AG_ALPHAN)=1                                           556
         AG_ALPHAL(AG_ALPHAN)=90                                          556
         AG_TITLE = 'SFPI'                                                556
         AG_EXNAME = 'POSITION'                                           556
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      556
         CALL AXPOSITION                                                  556
C    *                                                                    556
C    Geant POSITION : SFPI ===> X=-SFJP_SFPI_PX Y=SFJP_SFRA_PY+SFJP_SFRA  556
C    _DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2 Z=-SFJP_SFPI_PZ ALPHAX=90 <===       556
         CALL AGSRESET                                                    559
         AG_X = -SFJP_SFPI_PX                                             559
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2     559
         AG_Z = -SFJP_SFPI_PZ                                             559
         AG_ALPHAN=AG_ALPHAN+1                                            559
         AG_ALPHAK(AG_ALPHAN)=1                                           559
         AG_ALPHAL(AG_ALPHAN)=90                                          559
         AG_TITLE = 'SFPI'                                                559
         AG_EXNAME = 'POSITION'                                           559
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      559
         CALL AXPOSITION                                                  559
C    *                                                                    559
C    Create SFAA :                                                        560
C    Call To Local Procedure SFAA                                         560
         ASSIGN 5430 TO BLOCK_SFAA                                        560
         GO TO  5420                                                      560
5430     CONTINUE                                                         560
C    *                                                                    560
C    Geant POSITION : SFAA ===> X=-SFJP_SFAA_PX1-SFJP_SFAA_DX Y=SFJP_SFR  563
C    A_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=SFJP_SFAA_PZ1+SFJP  563
C    _SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ <===                            563
         CALL AGSRESET                                                    563
         AG_X = -SFJP_SFAA_PX1-SFJP_SFAA_DX                               563
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    563
         AG_Z = SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ    563
         AG_TITLE = 'SFAA'                                                563
         AG_EXNAME = 'POSITION'                                           563
         AG_PARLIST = 'X_Y_Z'                                             563
         CALL AXPOSITION                                                  563
C    *                                                                    563
C    Geant POSITION : SFAA ===> X=-SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_  563
C    PX2-SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_  563
C    SFAA_DY Z=SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ <=  563
C    ==                                                                   563
         CALL AGSRESET                                                    566
         AG_X = -SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_PX2-SFJP_SFAA_PX3   566
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    566
         AG_Z = SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ    566
         AG_TITLE = 'SFAA'                                                566
         AG_EXNAME = 'POSITION'                                           566
         AG_PARLIST = 'X_Y_Z'                                             566
         CALL AXPOSITION                                                  566
C    *                                                                    566
C    Geant POSITION : SFAA ===> X=-SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_  566
C    PX2-SFJP_SFAA_PX3-SFJP_SFAA_PX2-SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_S  566
C    FRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-S  566
C    FJP_SFAA_PZ3-SFJP_SFAA_DZ <===                                       566
         CALL AGSRESET                                                    570
         AG_X =                                                           570
     *   -SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_PX2-SFJP_SFAA_PX3-         570
     *   SFJP_SFAA_PX2-SFJP_SFAA_PX3                                      570
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    570
         AG_Z = SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ    570
         AG_TITLE = 'SFAA'                                                570
         AG_EXNAME = 'POSITION'                                           570
         AG_PARLIST = 'X_Y_Z'                                             570
         CALL AXPOSITION                                                  570
C    *                                                                    570
C    Geant POSITION : SFAA ===> X=+SFJP_SFAA_PX1+SFJP_SFAA_DX Y=SFJP_SFR  570
C    A_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=SFJP_SFAA_PZ1+SFJP  570
C    _SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ <===                            570
         CALL AGSRESET                                                    573
         AG_X = +SFJP_SFAA_PX1+SFJP_SFAA_DX                               573
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    573
         AG_Z = SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ    573
         AG_TITLE = 'SFAA'                                                573
         AG_EXNAME = 'POSITION'                                           573
         AG_PARLIST = 'X_Y_Z'                                             573
         CALL AXPOSITION                                                  573
C    *                                                                    573
C    Geant POSITION : SFAA ===> X=+SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_  573
C    PX2+SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_  573
C    SFAA_DY Z=SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ <=  573
C    ==                                                                   573
         CALL AGSRESET                                                    576
         AG_X = +SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_PX2+SFJP_SFAA_PX3   576
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    576
         AG_Z = SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ    576
         AG_TITLE = 'SFAA'                                                576
         AG_EXNAME = 'POSITION'                                           576
         AG_PARLIST = 'X_Y_Z'                                             576
         CALL AXPOSITION                                                  576
C    *                                                                    576
C    Geant POSITION : SFAA ===> X=+SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_  576
C    PX2+SFJP_SFAA_PX3+SFJP_SFAA_PX2+SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_S  576
C    FRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-S  576
C    FJP_SFAA_PZ3-SFJP_SFAA_DZ <===                                       576
         CALL AGSRESET                                                    580
         AG_X =                                                           580
     *   +SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_PX2+SFJP_SFAA_PX3+         580
     *   SFJP_SFAA_PX2+SFJP_SFAA_PX3                                      580
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    580
         AG_Z = SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ    580
         AG_TITLE = 'SFAA'                                                580
         AG_EXNAME = 'POSITION'                                           580
         AG_PARLIST = 'X_Y_Z'                                             580
         CALL AXPOSITION                                                  580
C    *                                                                    580
C    Geant POSITION : SFAA ===> X=-SFJP_SFAA_PX1-SFJP_SFAA_DX Y=SFJP_SFR  580
C    A_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=-SFJP_SFAA_PZ1-SFJ  580
C    P_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ <===                           580
         CALL AGSRESET                                                    583
         AG_X = -SFJP_SFAA_PX1-SFJP_SFAA_DX                               583
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    583
         AG_Z = -SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ   583
         AG_TITLE = 'SFAA'                                                583
         AG_EXNAME = 'POSITION'                                           583
         AG_PARLIST = 'X_Y_Z'                                             583
         CALL AXPOSITION                                                  583
C    *                                                                    583
C    Geant POSITION : SFAA ===> X=-SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_  583
C    PX2-SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_  583
C    SFAA_DY Z=-SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ <  583
C    ===                                                                  583
         CALL AGSRESET                                                    586
         AG_X = -SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_PX2-SFJP_SFAA_PX3   586
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    586
         AG_Z = -SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ   586
         AG_TITLE = 'SFAA'                                                586
         AG_EXNAME = 'POSITION'                                           586
         AG_PARLIST = 'X_Y_Z'                                             586
         CALL AXPOSITION                                                  586
C    *                                                                    586
C    Geant POSITION : SFAA ===> X=-SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_  586
C    PX2-SFJP_SFAA_PX3-SFJP_SFAA_PX2-SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_S  586
C    FRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=-SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+  586
C    SFJP_SFAA_PZ3+SFJP_SFAA_DZ <===                                      586
         CALL AGSRESET                                                    590
         AG_X =                                                           590
     *   -SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_PX2-SFJP_SFAA_PX3-         590
     *   SFJP_SFAA_PX2-SFJP_SFAA_PX3                                      590
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    590
         AG_Z = -SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ   590
         AG_TITLE = 'SFAA'                                                590
         AG_EXNAME = 'POSITION'                                           590
         AG_PARLIST = 'X_Y_Z'                                             590
         CALL AXPOSITION                                                  590
C    *                                                                    590
C    Geant POSITION : SFAA ===> X=+SFJP_SFAA_PX1+SFJP_SFAA_DX Y=SFJP_SFR  590
C    A_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=-SFJP_SFAA_PZ1-SFJ  590
C    P_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ <===                           590
         CALL AGSRESET                                                    593
         AG_X = +SFJP_SFAA_PX1+SFJP_SFAA_DX                               593
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    593
         AG_Z = -SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ   593
         AG_TITLE = 'SFAA'                                                593
         AG_EXNAME = 'POSITION'                                           593
         AG_PARLIST = 'X_Y_Z'                                             593
         CALL AXPOSITION                                                  593
C    *                                                                    593
C    Geant POSITION : SFAA ===> X=+SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_  593
C    PX2+SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_  593
C    SFAA_DY Z=-SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ <  593
C    ===                                                                  593
         CALL AGSRESET                                                    596
         AG_X = +SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_PX2+SFJP_SFAA_PX3   596
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    596
         AG_Z = -SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ   596
         AG_TITLE = 'SFAA'                                                596
         AG_EXNAME = 'POSITION'                                           596
         AG_PARLIST = 'X_Y_Z'                                             596
         CALL AXPOSITION                                                  596
C    *                                                                    596
C    Geant POSITION : SFAA ===> X=+SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_  596
C    PX2+SFJP_SFAA_PX3+SFJP_SFAA_PX2+SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_S  596
C    FRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=-SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+  596
C    SFJP_SFAA_PZ3+SFJP_SFAA_DZ <===                                      596
         CALL AGSRESET                                                    600
         AG_X =                                                           600
     *   +SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_PX2+SFJP_SFAA_PX3+         600
     *   SFJP_SFAA_PX2+SFJP_SFAA_PX3                                      600
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    600
         AG_Z = -SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ   600
         AG_TITLE = 'SFAA'                                                600
         AG_EXNAME = 'POSITION'                                           600
         AG_PARLIST = 'X_Y_Z'                                             600
         CALL AXPOSITION                                                  600
C    *                                                                    600
5301  CALL AGSPOPB                                                        601
      GO TO BLOCK_SFSW, (5300 ,5310)                                      601
C End of Local Procedure                                                  601
C ----------------------------------------------------------------------  602
C ------------ SFRA                                                       602
C Begin Local Procedure SFRA                                              603
C SFRA IS THE HYBRID STIFFNEER                                            603
5340  CONTINUE                                                            603
         AG_TITLE='SFRA'                                                  603
         AG_MODULE=MODULE                                                 603
         AG_MARK='V00'                                                    603
         CALL AGSPUSH                                                     603
         AG_MODULE=MODULE                                                 603
C    *                                                                    603
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           604
         CALL AGSRESET                                                    604
         AG_TITLE = 'CARBON'                                              604
         AG_EXNAME = 'MATERIAL'                                           604
         AG_PARLIST = 'NONE'                                              604
         CALL AXMATERIAL                                                  604
C    *                                                                    604
C    Geant ATTRIBUTE : SFRA ===> SEEN=2 COLO=1 <===                       604
         CALL AGSRESET                                                    605
         AG_SEEN = 2                                                      605
         AG_COLO = 1                                                      605
         AG_TITLE = 'SFRA'                                                605
         AG_EXNAME = 'ATTRIBUTE'                                          605
         AG_PARLIST = 'SEEN_COLO'                                         605
         CALL AXATTRIBUTE                                                 605
C    *                                                                    605
C    Geant SHAPE : BOX ===> DX=SFJP_SFRA_DX DY=SFJP_SFRA_DY, DZ=SFJP_SFR  605
C    A_DZ <===                                                            605
         CALL AGSRESET                                                    607
         AG_DX = SFJP_SFRA_DX                                             607
         AG_DY = SFJP_SFRA_DY                                             607
         AG_DZ = SFJP_SFRA_DZ                                             607
         AG_TITLE = 'BOX'                                                 607
         AG_EXNAME = 'SHAPE'                                              607
         AG_PARLIST = 'DX_DY_DZ'                                          607
         CALL AXSHAPE                                                     607
C    *                                                                    607
5341  CALL AGSPOPB                                                        608
      GO TO BLOCK_SFRA, (5340 ,5350)                                      608
C End of Local Procedure                                                  608
C ----------------------------------------------------------------------  609
C ------------ SFRS                                                       609
C Begin Local Procedure SFRS                                              610
C SFRS TWO SUPPORTS OF THE HYBRID STIFFNEER (PIECE OF IT)                 610
5360  CONTINUE                                                            610
         AG_TITLE='SFRS'                                                  610
         AG_MODULE=MODULE                                                 610
         AG_MARK='V00'                                                    610
         CALL AGSPUSH                                                     610
         AG_MODULE=MODULE                                                 610
C    *                                                                    610
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           611
         CALL AGSRESET                                                    611
         AG_TITLE = 'CARBON'                                              611
         AG_EXNAME = 'MATERIAL'                                           611
         AG_PARLIST = 'NONE'                                              611
         CALL AXMATERIAL                                                  611
C    *                                                                    611
C    Geant ATTRIBUTE : SFRS ===> SEEN=2 COLO=1 <===                       611
         CALL AGSRESET                                                    612
         AG_SEEN = 2                                                      612
         AG_COLO = 1                                                      612
         AG_TITLE = 'SFRS'                                                612
         AG_EXNAME = 'ATTRIBUTE'                                          612
         AG_PARLIST = 'SEEN_COLO'                                         612
         CALL AXATTRIBUTE                                                 612
C    *                                                                    612
C    Geant SHAPE : BOX ===> DX=SFJP_SFRS_DX DY=SFJP_SFRS_DY, DZ=SFJP_SFR  612
C    A_DZ <===                                                            612
         CALL AGSRESET                                                    614
         AG_DX = SFJP_SFRS_DX                                             614
         AG_DY = SFJP_SFRS_DY                                             614
         AG_DZ = SFJP_SFRA_DZ                                             614
         AG_TITLE = 'BOX'                                                 614
         AG_EXNAME = 'SHAPE'                                              614
         AG_PARLIST = 'DX_DY_DZ'                                          614
         CALL AXSHAPE                                                     614
C    *                                                                    614
5361  CALL AGSPOPB                                                        615
      GO TO BLOCK_SFRS, (5360 ,5370)                                      615
C End of Local Procedure                                                  615
C ----------------------------------------------------------------------  616
C ------------ SFFX                                                       616
C Begin Local Procedure SFFX                                              617
C SFFX IS THE FLEX                                                        617
5380  CONTINUE                                                            617
         AG_TITLE='SFFX'                                                  617
         AG_MODULE=MODULE                                                 617
         AG_MARK='V00'                                                    617
         CALL AGSPUSH                                                     617
         AG_MODULE=MODULE                                                 617
C    *                                                                    617
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           618
         CALL AGSRESET                                                    618
         AG_TITLE = 'COPPER'                                              618
         AG_EXNAME = 'MATERIAL'                                           618
         AG_PARLIST = 'NONE'                                              618
         CALL AXMATERIAL                                                  618
C    *                                                                    618
C    Geant ATTRIBUTE : SFFX ===> SEEN=1 COLO=42 <===                      618
         CALL AGSRESET                                                    619
         AG_SEEN = 1                                                      619
         AG_COLO = 42                                                     619
         AG_TITLE = 'SFFX'                                                619
         AG_EXNAME = 'ATTRIBUTE'                                          619
         AG_PARLIST = 'SEEN_COLO'                                         619
         CALL AXATTRIBUTE                                                 619
C    *                                                                    619
C    Geant SHAPE : BOX ===> DX=SFJP_SFFX_DX DY=SFJP_SFFX_DYZ, DZ=SFJP_SF  619
C    RA_DZ <===                                                           619
         CALL AGSRESET                                                    621
         AG_DX = SFJP_SFFX_DX                                             621
         AG_DY = SFJP_SFFX_DYZ                                            621
         AG_DZ = SFJP_SFRA_DZ                                             621
         AG_TITLE = 'BOX'                                                 621
         AG_EXNAME = 'SHAPE'                                              621
         AG_PARLIST = 'DX_DY_DZ'                                          621
         CALL AXSHAPE                                                     621
C    *                                                                    621
5381  CALL AGSPOPB                                                        622
      GO TO BLOCK_SFFX, (5380 ,5390)                                      622
C End of Local Procedure                                                  622
C ----------------------------------------------------------------------  623
C ------------ SFAA                                                       623
C Begin Local Procedure SFAA                                              624
C SFAA IS THE A128C CHIP                                                  624
5420  CONTINUE                                                            624
         AG_TITLE='SFAA'                                                  624
         AG_MODULE=MODULE                                                 624
         AG_MARK='V00'                                                    624
         CALL AGSPUSH                                                     624
         AG_MODULE=MODULE                                                 624
C    *                                                                    624
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          625
         CALL AGSRESET                                                    625
         AG_TITLE = 'SILICON'                                             625
         AG_EXNAME = 'MATERIAL'                                           625
         AG_PARLIST = 'NONE'                                              625
         CALL AXMATERIAL                                                  625
C    *                                                                    625
C    Geant ATTRIBUTE : SFAA ===> SEEN=1 COLO=41 <===                      625
         CALL AGSRESET                                                    626
         AG_SEEN = 1                                                      626
         AG_COLO = 41                                                     626
         AG_TITLE = 'SFAA'                                                626
         AG_EXNAME = 'ATTRIBUTE'                                          626
         AG_PARLIST = 'SEEN_COLO'                                         626
         CALL AXATTRIBUTE                                                 626
C    *                                                                    626
C    Geant SHAPE : BOX ===> DX=SFJP_SFAA_DX DY=SFJP_SFAA_DY, DZ=SFJP_SFA  626
C    A_DZ <===                                                            626
         CALL AGSRESET                                                    628
         AG_DX = SFJP_SFAA_DX                                             628
         AG_DY = SFJP_SFAA_DY                                             628
         AG_DZ = SFJP_SFAA_DZ                                             628
         AG_TITLE = 'BOX'                                                 628
         AG_EXNAME = 'SHAPE'                                              628
         AG_PARLIST = 'DX_DY_DZ'                                          628
         CALL AXSHAPE                                                     628
C    *                                                                    628
5421  CALL AGSPOPB                                                        629
      GO TO BLOCK_SFAA, (5420 ,5430)                                      629
C End of Local Procedure                                                  629
C ----------------------------------------------------------------------  630
C ------------ SFPI                                                       630
C Begin Local Procedure SFPI                                              631
C SFPI ARE THE PIONS                                                      631
5400  CONTINUE                                                            631
         AG_TITLE='SFPI'                                                  631
         AG_MODULE=MODULE                                                 631
         AG_MARK='V00'                                                    631
         CALL AGSPUSH                                                     631
         AG_MODULE=MODULE                                                 631
C    *                                                                    631
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        632
         CALL AGSRESET                                                    632
         AG_TITLE = 'ALUMINIUM'                                           632
         AG_EXNAME = 'MATERIAL'                                           632
         AG_PARLIST = 'NONE'                                              632
         CALL AXMATERIAL                                                  632
C    *                                                                    632
C    Geant ATTRIBUTE : SFPI ===> SEEN=2 COLO=33 <===                      632
         CALL AGSRESET                                                    633
         AG_SEEN = 2                                                      633
         AG_COLO = 33                                                     633
         AG_TITLE = 'SFPI'                                                633
         AG_EXNAME = 'ATTRIBUTE'                                          633
         AG_PARLIST = 'SEEN_COLO'                                         633
         CALL AXATTRIBUTE                                                 633
C    *                                                                    633
C    Geant SHAPE : TUBE ===> RMIN=SFJP_SFPI_RMIN/2. RMAX=SFJP_SFPI_RMAX/  633
C    2., DZ=SFJP_SFPI_PZ/2.0 <===                                         633
         CALL AGSRESET                                                    635
         AG_RMIN = SFJP_SFPI_RMIN/2.                                      635
         AG_RMAX = SFJP_SFPI_RMAX/2.                                      635
         AG_DZ = SFJP_SFPI_PZ/2.0                                         635
         AG_TITLE = 'TUBE'                                                635
         AG_EXNAME = 'SHAPE'                                              635
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      635
         CALL AXSHAPE                                                     635
C    *                                                                    635
C    Create SFPJ :                                                        636
C    Call To Local Procedure SFPJ                                         636
         ASSIGN 5450 TO BLOCK_SFPJ                                        636
         GO TO  5440                                                      636
5450     CONTINUE                                                         636
C    *                                                                    636
C    Geant POSITION : SFPJ ===> X=0. Y=0. Z=-SFJP_SFPI_PZ/2.0+SFJP_SFPJ_  637
C    DZ <===                                                              637
         CALL AGSRESET                                                    637
         AG_X = 0.                                                        637
         AG_Y = 0.                                                        637
         AG_Z = -SFJP_SFPI_PZ/2.0+SFJP_SFPJ_DZ                            637
         AG_TITLE = 'SFPJ'                                                637
         AG_EXNAME = 'POSITION'                                           637
         AG_PARLIST = 'X_Y_Z'                                             637
         CALL AXPOSITION                                                  637
C    *                                                                    637
5401  CALL AGSPOPB                                                        638
      GO TO BLOCK_SFPI, (5400 ,5410)                                      638
C End of Local Procedure                                                  638
C ----------------------------------------------------------------------  639
C ------------ SFPJ                                                       639
C Begin Local Procedure SFPJ                                              640
C SFPJ IS THE BASE OF THE PIONS                                           640
5440  CONTINUE                                                            640
         AG_TITLE='SFPJ'                                                  640
         AG_MODULE=MODULE                                                 640
         AG_MARK='V00'                                                    640
         CALL AGSPUSH                                                     640
         AG_MODULE=MODULE                                                 640
C    *                                                                    640
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        641
         CALL AGSRESET                                                    641
         AG_TITLE = 'ALUMINIUM'                                           641
         AG_EXNAME = 'MATERIAL'                                           641
         AG_PARLIST = 'NONE'                                              641
         CALL AXMATERIAL                                                  641
C    *                                                                    641
C    Geant ATTRIBUTE : SFPJ ===> SEEN=2 COLO=33 <===                      641
         CALL AGSRESET                                                    642
         AG_SEEN = 2                                                      642
         AG_COLO = 33                                                     642
         AG_TITLE = 'SFPJ'                                                642
         AG_EXNAME = 'ATTRIBUTE'                                          642
         AG_PARLIST = 'SEEN_COLO'                                         642
         CALL AXATTRIBUTE                                                 642
C    *                                                                    642
C    Geant SHAPE : TUBE ===> RMIN=SFJP_SFPJ_DX1+SFJP_SFPJ_DX2, RMAX=SFJP  642
C    _SFPJ_DY DZ=SFJP_SFPJ_DZ <===                                        642
         CALL AGSRESET                                                    644
         AG_RMIN = SFJP_SFPJ_DX1+SFJP_SFPJ_DX2                            644
         AG_RMAX = SFJP_SFPJ_DY                                           644
         AG_DZ = SFJP_SFPJ_DZ                                             644
         AG_TITLE = 'TUBE'                                                644
         AG_EXNAME = 'SHAPE'                                              644
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      644
         CALL AXSHAPE                                                     644
C    *                                                                    644
5441  CALL AGSPOPB                                                        645
      GO TO BLOCK_SFPJ, (5440 ,5450)                                      645
C End of Local Procedure                                                  645
C ----------------------------------------------------------------------  646
C ------------ SFSL                                                       646
C Begin Local Procedure SFSL                                              647
C SFSL IS THE SILICON OF THE STRIP DETECTOR                               647
5320  CONTINUE                                                            647
         AG_TITLE='SFSL'                                                  647
         AG_MODULE=MODULE                                                 647
         AG_MARK='V00'                                                    647
         CALL AGSPUSH                                                     647
         AG_MODULE=MODULE                                                 647
C    *                                                                    647
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          648
         CALL AGSRESET                                                    648
         AG_TITLE = 'SILICON'                                             648
         AG_EXNAME = 'MATERIAL'                                           648
         AG_PARLIST = 'NONE'                                              648
         CALL AXMATERIAL                                                  648
C    *                                                                    648
C    Geant ATTRIBUTE : SFSL ===> SEEN=2 COLO=1 <===                       648
         CALL AGSRESET                                                    649
         AG_SEEN = 2                                                      649
         AG_COLO = 1                                                      649
         AG_TITLE = 'SFSL'                                                649
         AG_EXNAME = 'ATTRIBUTE'                                          649
         AG_PARLIST = 'SEEN_COLO'                                         649
         CALL AXATTRIBUTE                                                 649
C    *                                                                    649
C    Geant SHAPE : BOX ===> DX=SFJP_SFSL_DX DY=SFJP_SFSL_DY, DZ=SFJP_SFS  649
C    L_DZ <===                                                            649
         CALL AGSRESET                                                    651
         AG_DX = SFJP_SFSL_DX                                             651
         AG_DY = SFJP_SFSL_DY                                             651
         AG_DZ = SFJP_SFSL_DZ                                             651
         AG_TITLE = 'BOX'                                                 651
         AG_EXNAME = 'SHAPE'                                              651
         AG_PARLIST = 'DX_DY_DZ'                                          651
         CALL AXSHAPE                                                     651
C    *                                                                    651
C    Create SFSD :                                                        652
C    Call To Local Procedure SFSD                                         652
         ASSIGN 5470 TO BLOCK_SFSD                                        652
         GO TO  5460                                                      652
5470     CONTINUE                                                         652
C    *                                                                    652
C    Geant POSITION : SFSD ===> inherited arguments used <===             653
         CALL AGSRESET                                                    653
         AG_TITLE = 'SFSD'                                                653
         AG_EXNAME = 'POSITION'                                           653
         AG_PARLIST = 'NONE'                                              653
         CALL AXPOSITION                                                  653
C    *                                                                    653
5321  CALL AGSPOPB                                                        654
      GO TO BLOCK_SFSL, (5320 ,5330)                                      654
C End of Local Procedure                                                  654
C ----------------------------------------------------------------------  655
C ------------ SFSD                                                       655
C Begin Local Procedure SFSD                                              656
C SFSD IS THE STRIP DETECTOR                                              656
5460  CONTINUE                                                            656
         AG_TITLE='SFSD'                                                  656
         AG_MODULE=MODULE                                                 656
         AG_MARK='V00'                                                    656
         CALL AGSPUSH                                                     656
         AG_MODULE=MODULE                                                 656
C    *                                                                    656
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          657
         CALL AGSRESET                                                    657
         AG_TITLE = 'SILICON'                                             657
         AG_EXNAME = 'MATERIAL'                                           657
         AG_PARLIST = 'NONE'                                              657
         CALL AXMATERIAL                                                  657
C    *                                                                    657
C    Geant MATERIAL : SENSITIVE ===> ISVOL=1 <===                         657
         CALL AGSRESET                                                    658
         AG_ISVOL = 1                                                     658
         AG_TITLE = 'SENSITIVE'                                           658
         AG_EXNAME = 'MATERIAL'                                           658
         AG_PARLIST = 'ISVOL'                                             658
         CALL AXMATERIAL                                                  658
C    *                                                                    658
C    Geant ATTRIBUTE : SFSD ===> SEEN=2 COLO=41 <===                      658
         CALL AGSRESET                                                    659
         AG_SEEN = 2                                                      659
         AG_COLO = 41                                                     659
         AG_TITLE = 'SFSD'                                                659
         AG_EXNAME = 'ATTRIBUTE'                                          659
         AG_PARLIST = 'SEEN_COLO'                                         659
         CALL AXATTRIBUTE                                                 659
C    *                                                                    659
C    Geant SHAPE : BOX ===> DX=SFJP_SFSD_DX DY=SFJP_SFSD_DY, DZ=SFJP_SFS  659
C    D_DZ <===                                                            659
         CALL AGSRESET                                                    661
         AG_DX = SFJP_SFSD_DX                                             661
         AG_DY = SFJP_SFSD_DY                                             661
         AG_DZ = SFJP_SFSD_DZ                                             661
         AG_TITLE = 'BOX'                                                 661
         AG_EXNAME = 'SHAPE'                                              661
         AG_PARLIST = 'DX_DY_DZ'                                          661
         CALL AXSHAPE                                                     661
C    *                                                                    661
         CALL GSTPAR( AG_IMED,'STRA',1. )                                 661
C    Defining Hits: X:0.001:S Y:0.001: Z:0.001: CX:10: CY:10: CZ:10: STE  664
C    P:0.01: SLENG:16:(0,500), TOF:16:(0,1.0E-6) PTOT:16:(0,100.0) ELOSS  664
C    :16:(0,0.01)                                                         664
         AG_TITLE = 'SFSD'                                                664
         AG_EXNAME = 'HITS'                                               664
         AG_TYPE=AG_MAGIC                                                 664
         CALL AGSDETV('SFSD')                                             664
C    *                                                                    664
         AG_PARLIST='X'                                                   664
         AG_TITLE = 'DETH'                                                664
         AG_NBITS = AG_MAGIC                                              664
         AG_BIN=0.001                                                     664
         AG_OPTION='S'                                                    664
         AG_FHMIN = AG_MAGIC                                              664
         AG_FHMAX = AG_MAGIC                                              664
         CALL AGSDETH (0)                                                 664
C    *                                                                    664
         AG_PARLIST='Y'                                                   664
         AG_TITLE = 'DETH'                                                664
         AG_NBITS = AG_MAGIC                                              664
         AG_BIN=0.001                                                     664
         AG_FHMIN=AG_MAGIC                                                664
         AG_FHMAX=AG_MAGIC                                                664
         AG_OPTION='*'                                                    664
         CALL AGSDETH (0)                                                 664
C    *                                                                    664
         AG_PARLIST='Z'                                                   664
         AG_TITLE = 'DETH'                                                664
         AG_NBITS = AG_MAGIC                                              664
         AG_BIN=0.001                                                     664
         AG_FHMIN=AG_MAGIC                                                664
         AG_FHMAX=AG_MAGIC                                                664
         AG_OPTION='*'                                                    664
         CALL AGSDETH (0)                                                 664
C    *                                                                    664
         AG_PARLIST='CX'                                                  664
         AG_TITLE = 'DETH'                                                664
         AG_NBITS = 10                                                    664
         AG_BIN=AG_MAGIC                                                  664
         AG_FHMIN=AG_MAGIC                                                664
         AG_FHMAX=AG_MAGIC                                                664
         AG_OPTION='*'                                                    664
         CALL AGSDETH (0)                                                 664
C    *                                                                    664
         AG_PARLIST='CY'                                                  664
         AG_TITLE = 'DETH'                                                664
         AG_NBITS = 10                                                    664
         AG_BIN=AG_MAGIC                                                  664
         AG_FHMIN=AG_MAGIC                                                664
         AG_FHMAX=AG_MAGIC                                                664
         AG_OPTION='*'                                                    664
         CALL AGSDETH (0)                                                 664
C    *                                                                    664
         AG_PARLIST='CZ'                                                  664
         AG_TITLE = 'DETH'                                                664
         AG_NBITS = 10                                                    664
         AG_BIN=AG_MAGIC                                                  664
         AG_FHMIN=AG_MAGIC                                                664
         AG_FHMAX=AG_MAGIC                                                664
         AG_OPTION='*'                                                    664
         CALL AGSDETH (0)                                                 664
C    *                                                                    664
         AG_PARLIST='STEP'                                                664
         AG_TITLE = 'DETH'                                                664
         AG_NBITS = AG_MAGIC                                              664
         AG_BIN=0.01                                                      664
         AG_FHMIN=AG_MAGIC                                                664
         AG_FHMAX=AG_MAGIC                                                664
         AG_OPTION='*'                                                    664
         CALL AGSDETH (0)                                                 664
C    *                                                                    664
         AG_PARLIST='SLENG'                                               664
         AG_TITLE = 'DETH'                                                664
         AG_NBITS = 16                                                    664
         AG_BIN=AG_MAGIC                                                  664
         AG_OPTION='*'                                                    664
         AG_FHMIN = 0                                                     664
         AG_FHMAX = 500                                                   664
         CALL AGSDETH (0)                                                 664
C    *                                                                    664
         AG_PARLIST='TOF'                                                 664
         AG_TITLE = 'DETH'                                                664
         AG_NBITS = 16                                                    664
         AG_BIN=AG_MAGIC                                                  664
         AG_OPTION='*'                                                    664
         AG_FHMIN = 0                                                     664
         AG_FHMAX = 1.0E-6                                                664
         CALL AGSDETH (0)                                                 664
C    *                                                                    664
         AG_PARLIST='PTOT'                                                664
         AG_TITLE = 'DETH'                                                664
         AG_NBITS = 16                                                    664
         AG_BIN=AG_MAGIC                                                  664
         AG_OPTION='*'                                                    664
         AG_FHMIN = 0                                                     664
         AG_FHMAX = 100.0                                                 664
         CALL AGSDETH (0)                                                 664
C    *                                                                    664
         AG_PARLIST='ELOSS'                                               664
         AG_TITLE = 'DETH'                                                664
         AG_NBITS = 16                                                    664
         AG_BIN=AG_MAGIC                                                  664
         AG_OPTION='*'                                                    664
         AG_FHMIN = 0                                                     664
         AG_FHMAX = 0.01                                                  664
         CALL AGSDETH (0)                                                 664
C    *                                                                    664
         CALL AGSDETU('SFSD')                                             664
C    *                                                                    664
5461  CALL AGSPOPB                                                        665
      GO TO BLOCK_SFSD, (5460 ,5470)                                      665
C End of Local Procedure                                                  665
C ----------------------------------------------------------------------  666
C ------------ SFSM                                                       666
C Begin Local Procedure SFSM                                              667
C SFSM IS THE MOTHER VOLUME OF THE LADDER (MECHANIC STRUCTURE)            667
5270  CONTINUE                                                            667
         AG_TITLE='SFSM'                                                  667
         AG_MODULE=MODULE                                                 667
         AG_MARK='V00'                                                    667
         CALL AGSPUSH                                                     667
         AG_MODULE=MODULE                                                 667
C    *                                                                    667
C    Geant MATERIAL : AIR ===> inherited arguments used <===              668
         CALL AGSRESET                                                    668
         AG_TITLE = 'AIR'                                                 668
         AG_EXNAME = 'MATERIAL'                                           668
         AG_PARLIST = 'NONE'                                              668
         CALL AXMATERIAL                                                  668
C    *                                                                    668
C    Geant ATTRIBUTE : SFSM ===> SEEN=0 COLO=6 <===                       668
         CALL AGSRESET                                                    669
         AG_SEEN = 0                                                      669
         AG_COLO = 6                                                      669
         AG_TITLE = 'SFSM'                                                669
         AG_EXNAME = 'ATTRIBUTE'                                          669
         AG_PARLIST = 'SEEN_COLO'                                         669
         CALL AXATTRIBUTE                                                 669
C    *                                                                    669
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=SFPA_SMTHK/2, DZ=SFPA_SML  669
C    EN/2. <===                                                           669
         CALL AGSRESET                                                    671
         AG_DX = SFPA_DMWID/2                                             671
         AG_DY = SFPA_SMTHK/2                                             671
         AG_DZ = SFPA_SMLEN/2.                                            671
         AG_TITLE = 'BOX'                                                 671
         AG_EXNAME = 'SHAPE'                                              671
         AG_PARLIST = 'DX_DY_DZ'                                          671
         CALL AXSHAPE                                                     671
C    *                                                                    671
         YOFFSET = - 1.7                                                  671
C    Create SFLT :                                                        673
C    Call To Local Procedure SFLT                                         673
         ASSIGN 5490 TO BLOCK_SFLT                                        673
         GO TO  5480                                                      673
5490     CONTINUE                                                         673
C    *                                                                    673
         HIGHT = SFJP_SFSM_LL*TAN(54*PI/180) - SFPB_HHIGHT/TAN(PI/5) -    674
     *   0.02                                                             674
C    Geant POSITION : SFLT ===> X=-(SFPB_HHIGHT-SFPB_KHIGHT) Y=HIGHT+YOF  675
C    FSET Z=0.0 ALPHAZ=-90-36 <===                                        675
         CALL AGSRESET                                                    675
         AG_X = -(SFPB_HHIGHT-SFPB_KHIGHT)                                675
         AG_Y = HIGHT+YOFFSET                                             675
         AG_Z = 0.0                                                       675
         AG_ALPHAN=AG_ALPHAN+1                                            675
         AG_ALPHAK(AG_ALPHAN)=3                                           675
         AG_ALPHAL(AG_ALPHAN)=-90-36                                      675
         AG_TITLE = 'SFLT'                                                675
         AG_EXNAME = 'POSITION'                                           675
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      675
         CALL AXPOSITION                                                  675
C    *                                                                    675
C    Geant POSITION : SFLT ===> X=(SFPB_HHIGHT-SFPB_KHIGHT) Y=HIGHT+YOFF  675
C    SET Z=0 ALPHAY=180. ALPHAZ=90.+36. <===                              675
         CALL AGSRESET                                                    677
         AG_X = (SFPB_HHIGHT-SFPB_KHIGHT)                                 677
         AG_Y = HIGHT+YOFFSET                                             677
         AG_Z = 0                                                         677
         AG_ALPHAN=AG_ALPHAN+1                                            677
         AG_ALPHAK(AG_ALPHAN)=2                                           677
         AG_ALPHAL(AG_ALPHAN)=180.                                        677
         AG_ALPHAN=AG_ALPHAN+1                                            677
         AG_ALPHAK(AG_ALPHAN)=3                                           677
         AG_ALPHAL(AG_ALPHAN)=90.+36.                                     677
         AG_TITLE = 'SFLT'                                                677
         AG_EXNAME = 'POSITION'                                           677
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               677
         CALL AXPOSITION                                                  677
C    *                                                                    677
C    Create SFLU :                                                        678
C    Call To Local Procedure SFLU                                         678
         ASSIGN 5510 TO BLOCK_SFLU                                        678
         GO TO  5500                                                      678
5510     CONTINUE                                                         678
C    *                                                                    678
C    Geant POSITION : SFLU ===> X=0.-(SFJP_SFSM_LL-(SFJP_SFLU_BL1+SFJP_S  680
C    FLU_H1/2*1/TAN(PI/5))) Y=0.+YOFFSET Z=0. <===                        680
         CALL AGSRESET                                                    680
         AG_X = 0.-(SFJP_SFSM_LL-(SFJP_SFLU_BL1+SFJP_SFLU_H1/2*1/TAN(PI/  680
     *   5)))                                                             680
         AG_Y = 0.+YOFFSET                                                680
         AG_Z = 0.                                                        680
         AG_TITLE = 'SFLU'                                                680
         AG_EXNAME = 'POSITION'                                           680
         AG_PARLIST = 'X_Y_Z'                                             680
         CALL AXPOSITION                                                  680
C    *                                                                    680
         ESSAI = 180                                                      680
         ESSAI = ESSAI + 27 + 27                                          682
C    Geant POSITION : SFLU ===> X=-COS(63*PI/180)*2*(SFPB_HBASE-SFPB_KBA  685
C    SE)-(SFJP_SFSM_LL-(0.2+0.01/TAN(PI/5))) Y=COS(27*PI/180)*2*(SFPB_HB  685
C    ASE-SFPB_KBASE)+YOFFSET Z=0 ALPHAY=180 ALPHAZ=ESSAI <===             685
         CALL AGSRESET                                                    685
         AG_X = -COS(63*PI/                                               685
     *   180)*2*(SFPB_HBASE-SFPB_KBASE)-(SFJP_SFSM_LL-(0.2+0.01/TAN(PI/   685
     *   5)))                                                             685
         AG_Y = COS(27*PI/180)*2*(SFPB_HBASE-SFPB_KBASE)+YOFFSET          685
         AG_Z = 0                                                         685
         AG_ALPHAN=AG_ALPHAN+1                                            685
         AG_ALPHAK(AG_ALPHAN)=2                                           685
         AG_ALPHAL(AG_ALPHAN)=180                                         685
         AG_ALPHAN=AG_ALPHAN+1                                            685
         AG_ALPHAK(AG_ALPHAN)=3                                           685
         AG_ALPHAL(AG_ALPHAN)=ESSAI                                       685
         AG_TITLE = 'SFLU'                                                685
         AG_EXNAME = 'POSITION'                                           685
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               685
         CALL AXPOSITION                                                  685
C    *                                                                    685
C    Geant POSITION : SFLU ===> X=0.+(SFJP_SFSM_LL-(0.2+0.01/TAN(PI/5)))  685
C     Y=0.+YOFFSET Z=0. ALPHAY=180. <===                                  685
         CALL AGSRESET                                                    687
         AG_X = 0.+(SFJP_SFSM_LL-(0.2+0.01/TAN(PI/5)))                    687
         AG_Y = 0.+YOFFSET                                                687
         AG_Z = 0.                                                        687
         AG_ALPHAN=AG_ALPHAN+1                                            687
         AG_ALPHAK(AG_ALPHAN)=2                                           687
         AG_ALPHAL(AG_ALPHAN)=180.                                        687
         AG_TITLE = 'SFLU'                                                687
         AG_EXNAME = 'POSITION'                                           687
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      687
         CALL AXPOSITION                                                  687
C    *                                                                    687
C    Geant POSITION : SFLU ===> X=COS(63*PI/180)*2*(SFPB_HBASE-SFPB_KBAS  687
C    E)+(SFJP_SFSM_LL-(0.2+0.01/TAN(PI/5))) Y=COS(27*PI/180)*2*(SFPB_HBA  687
C    SE-SFPB_KBASE)+YOFFSET Z=0 ALPHAZ=-ESSAI <===                        687
         CALL AGSRESET                                                    690
         AG_X = COS(63*PI/                                                690
     *   180)*2*(SFPB_HBASE-SFPB_KBASE)+(SFJP_SFSM_LL-(0.2+0.01/TAN(PI/   690
     *   5)))                                                             690
         AG_Y = COS(27*PI/180)*2*(SFPB_HBASE-SFPB_KBASE)+YOFFSET          690
         AG_Z = 0                                                         690
         AG_ALPHAN=AG_ALPHAN+1                                            690
         AG_ALPHAK(AG_ALPHAN)=3                                           690
         AG_ALPHAL(AG_ALPHAN)=-ESSAI                                      690
         AG_TITLE = 'SFLU'                                                690
         AG_EXNAME = 'POSITION'                                           690
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      690
         CALL AXPOSITION                                                  690
C    *                                                                    690
C    Create SFFK :                                                        691
C    Call To Local Procedure SFFK                                         691
         ASSIGN 5530 TO BLOCK_SFFK                                        691
         GO TO  5520                                                      691
5530     CONTINUE                                                         691
C    *                                                                    691
C    Geant POSITION : SFFK ===> X=SFJP_SFFK_PX/2+SFJP_SFFK_DXE Y=-SFJP_S  693
C    FFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY+YOFFSET Z=0.0 <===                693
         CALL AGSRESET                                                    693
         AG_X = SFJP_SFFK_PX/2+SFJP_SFFK_DXE                              693
         AG_Y = -SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY+YOFFSET         693
         AG_Z = 0.0                                                       693
         AG_TITLE = 'SFFK'                                                693
         AG_EXNAME = 'POSITION'                                           693
         AG_PARLIST = 'X_Y_Z'                                             693
         CALL AXPOSITION                                                  693
C    *                                                                    693
C    Geant POSITION : SFFK ===> X=-SFJP_SFFK_PX/2-SFJP_SFFK_DXE Y=-SFJP_  693
C    SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY+YOFFSET Z=0.0 <===               693
         CALL AGSRESET                                                    695
         AG_X = -SFJP_SFFK_PX/2-SFJP_SFFK_DXE                             695
         AG_Y = -SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY+YOFFSET         695
         AG_Z = 0.0                                                       695
         AG_TITLE = 'SFFK'                                                695
         AG_EXNAME = 'POSITION'                                           695
         AG_PARLIST = 'X_Y_Z'                                             695
         CALL AXPOSITION                                                  695
C    *                                                                    695
C    Create SFFL :                                                        696
C    Call To Local Procedure SFFL                                         696
         ASSIGN 5550 TO BLOCK_SFFL                                        696
         GO TO  5540                                                      696
5550     CONTINUE                                                         696
C    *                                                                    696
C    Geant POSITION : SFFL ===> X=-SFJP_SFKL_PX-SFPB_FSIZE*COS(15*PI/180  699
C    ) Y=-SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(75*PI/  699
C    180)+YOFFSET Z=0 ALPHAZ=15 <===                                      699
         CALL AGSRESET                                                    699
         AG_X = -SFJP_SFKL_PX-SFPB_FSIZE*COS(15*PI/180)                   699
         AG_Y =                                                           699
     *   -SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(75*PI/  699
     *   180)+YOFFSET                                                     699
         AG_Z = 0                                                         699
         AG_ALPHAN=AG_ALPHAN+1                                            699
         AG_ALPHAK(AG_ALPHAN)=3                                           699
         AG_ALPHAL(AG_ALPHAN)=15                                          699
         AG_TITLE = 'SFFL'                                                699
         AG_EXNAME = 'POSITION'                                           699
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      699
         CALL AXPOSITION                                                  699
C    *                                                                    699
C    Geant POSITION : SFFL ===> X=SFJP_SFKL_PX+SFPB_FSIZE*COS(15*PI/180)  699
C     Y=-SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(75*PI/1  699
C    80)+YOFFSET Z=0 ALPHAZ=-15 <===                                      699
         CALL AGSRESET                                                    702
         AG_X = SFJP_SFKL_PX+SFPB_FSIZE*COS(15*PI/180)                    702
         AG_Y =                                                           702
     *   -SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(75*PI/  702
     *   180)+YOFFSET                                                     702
         AG_Z = 0                                                         702
         AG_ALPHAN=AG_ALPHAN+1                                            702
         AG_ALPHAK(AG_ALPHAN)=3                                           702
         AG_ALPHAL(AG_ALPHAN)=-15                                         702
         AG_TITLE = 'SFFL'                                                702
         AG_EXNAME = 'POSITION'                                           702
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      702
         CALL AXPOSITION                                                  702
C    *                                                                    702
C    Create SFKK :                                                        703
C    Call To Local Procedure SFKK                                         703
         ASSIGN 5570 TO BLOCK_SFKK                                        703
         GO TO  5560                                                      703
5570     CONTINUE                                                         703
C    *                                                                    703
C    Geant POSITION : SFKK ===> X=0. Y=-SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP  705
C    _SFFK_PY2+YOFFSET-SFJP_SFKK_DY/2. Z=0. <===                          705
         CALL AGSRESET                                                    705
         AG_X = 0.                                                        705
         AG_Y =                                                           705
     *   -SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_PY2+YOFFSET-              705
     *   SFJP_SFKK_DY/2.                                                  705
         AG_Z = 0.                                                        705
         AG_TITLE = 'SFKK'                                                705
         AG_EXNAME = 'POSITION'                                           705
         AG_PARLIST = 'X_Y_Z'                                             705
         CALL AXPOSITION                                                  705
C    *                                                                    705
C    Create SFKL :                                                        706
C    Call To Local Procedure SFKL                                         706
         ASSIGN 5590 TO BLOCK_SFKL                                        706
         GO TO  5580                                                      706
5590     CONTINUE                                                         706
C    *                                                                    706
C    Geant POSITION : SFKL ===> X=-SFJP_SFKL_PX-SFPB_FSIZE*COS(15*PI/180  709
C    )+0.005 Y=-SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(  709
C    75*PI/180)+YOFFSET-0.021 Z=0 ALPHAZ=15 <===                          709
         CALL AGSRESET                                                    709
         AG_X = -SFJP_SFKL_PX-SFPB_FSIZE*COS(15*PI/180)+0.005             709
         AG_Y =                                                           709
     *   -SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(75*PI/  709
     *   180)+YOFFSET-0.021                                               709
         AG_Z = 0                                                         709
         AG_ALPHAN=AG_ALPHAN+1                                            709
         AG_ALPHAK(AG_ALPHAN)=3                                           709
         AG_ALPHAL(AG_ALPHAN)=15                                          709
         AG_TITLE = 'SFKL'                                                709
         AG_EXNAME = 'POSITION'                                           709
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      709
         CALL AXPOSITION                                                  709
C    *                                                                    709
C    Geant POSITION : SFKL ===> X=+SFJP_SFKL_PX+SFPB_FSIZE*COS(15*PI/180  709
C    )-0.005 Y=-SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(  709
C    75*PI/180)+YOFFSET-0.021 Z=0 ALPHAZ=-15 <===                         709
         CALL AGSRESET                                                    712
         AG_X = +SFJP_SFKL_PX+SFPB_FSIZE*COS(15*PI/180)-0.005             712
         AG_Y =                                                           712
     *   -SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(75*PI/  712
     *   180)+YOFFSET-0.021                                               712
         AG_Z = 0                                                         712
         AG_ALPHAN=AG_ALPHAN+1                                            712
         AG_ALPHAK(AG_ALPHAN)=3                                           712
         AG_ALPHAL(AG_ALPHAN)=-15                                         712
         AG_TITLE = 'SFKL'                                                712
         AG_EXNAME = 'POSITION'                                           712
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      712
         CALL AXPOSITION                                                  712
C    *                                                                    712
C    Create SAPP :                                                        713
C    Call To Local Procedure SAPP                                         713
         ASSIGN 5610 TO BLOCK_SAPP                                        713
         GO TO  5600                                                      713
5610     CONTINUE                                                         713
C    *                                                                    713
C    Geant POSITION : SAPP ===> X=0. Y=-SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJ  715
C    P_SAPP_PY3)/2.+YOFFSET Z=SFJP_SFFK_DZ/2+SFJP_SAPP_DZ/2. ALPHAY=180.  715
C     <===                                                                715
         CALL AGSRESET                                                    715
         AG_X = 0.                                                        715
         AG_Y = -SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJP_SAPP_PY3)/2.+YOFFSET   715
         AG_Z = SFJP_SFFK_DZ/2+SFJP_SAPP_DZ/2.                            715
         AG_ALPHAN=AG_ALPHAN+1                                            715
         AG_ALPHAK(AG_ALPHAN)=2                                           715
         AG_ALPHAL(AG_ALPHAN)=180.                                        715
         AG_TITLE = 'SAPP'                                                715
         AG_EXNAME = 'POSITION'                                           715
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      715
         CALL AXPOSITION                                                  715
C    *                                                                    715
C    Geant POSITION : SAPP ===> X=0. Y=-SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJ  715
C    P_SAPP_PY3)/2.+YOFFSET Z=-(SFJP_SFFK_DZ/2+SFJP_SAPP_DZ/2.) <===      715
         CALL AGSRESET                                                    717
         AG_X = 0.                                                        717
         AG_Y = -SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJP_SAPP_PY3)/2.+YOFFSET   717
         AG_Z = -(SFJP_SFFK_DZ/2+SFJP_SAPP_DZ/2.)                         717
         AG_TITLE = 'SAPP'                                                717
         AG_EXNAME = 'POSITION'                                           717
         AG_PARLIST = 'X_Y_Z'                                             717
         CALL AXPOSITION                                                  717
C    *                                                                    717
C    Create SFAM :                                                        718
C    Call To Local Procedure SFAM                                         718
         ASSIGN 5630 TO BLOCK_SFAM                                        718
         GO TO  5620                                                      718
5630     CONTINUE                                                         718
C    *                                                                    718
C    Geant POSITION : SFAM ===> X=0. Y=-SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJ  720
C    P_SAPP_PY3)/2.+YOFFSET Z=-(SFJP_SFFK_DZ/2+SFJP_SAPP_DZ)-SFJP_SFAM_D  720
C    Z/2. <===                                                            720
         CALL AGSRESET                                                    720
         AG_X = 0.                                                        720
         AG_Y = -SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJP_SAPP_PY3)/2.+YOFFSET   720
         AG_Z = -(SFJP_SFFK_DZ/2+SFJP_SAPP_DZ)-SFJP_SFAM_DZ/2.            720
         AG_TITLE = 'SFAM'                                                720
         AG_EXNAME = 'POSITION'                                           720
         AG_PARLIST = 'X_Y_Z'                                             720
         CALL AXPOSITION                                                  720
C    *                                                                    720
C    Geant POSITION : SFAM ===> X=0. Y=-SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJ  720
C    P_SAPP_PY3)/2.+YOFFSET Z=+(SFJP_SFFK_DZ/2+SFJP_SAPP_DZ)+SFJP_SFAM_D  720
C    Z/2. ALPHAY=180. <===                                                720
         CALL AGSRESET                                                    722
         AG_X = 0.                                                        722
         AG_Y = -SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJP_SAPP_PY3)/2.+YOFFSET   722
         AG_Z = +(SFJP_SFFK_DZ/2+SFJP_SAPP_DZ)+SFJP_SFAM_DZ/2.            722
         AG_ALPHAN=AG_ALPHAN+1                                            722
         AG_ALPHAK(AG_ALPHAN)=2                                           722
         AG_ALPHAL(AG_ALPHAN)=180.                                        722
         AG_TITLE = 'SFAM'                                                722
         AG_EXNAME = 'POSITION'                                           722
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      722
         CALL AXPOSITION                                                  722
C    *                                                                    722
C    Create SFCO :                                                        723
C    Call To Local Procedure SFCO                                         723
         ASSIGN 5650 TO BLOCK_SFCO                                        723
         GO TO  5640                                                      723
5650     CONTINUE                                                         723
C    *                                                                    723
C    Geant POSITION : SFCO ===> X=-SFJP_SFCO_PX Y=SFJP_SFCO_PY Z=-(SFJP_  726
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFCO_DZ/2. ALPHAZ=+SFJP  726
C    _ALPHAZ <===                                                         726
         CALL AGSRESET                                                    726
         AG_X = -SFJP_SFCO_PX                                             726
         AG_Y = SFJP_SFCO_PY                                              726
         AG_Z = -(SFJP_SFFK_DZ/                                           726
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFCO_DZ/2.                   726
         AG_ALPHAN=AG_ALPHAN+1                                            726
         AG_ALPHAK(AG_ALPHAN)=3                                           726
         AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                                726
         AG_TITLE = 'SFCO'                                                726
         AG_EXNAME = 'POSITION'                                           726
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      726
         CALL AXPOSITION                                                  726
C    *                                                                    726
C    Geant POSITION : SFCO ===> X=+SFJP_SFCO_PX Y=SFJP_SFCO_PY Z=+(SFJP_  726
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFCO_DZ/2. ALPHAZ=-SFJP  726
C    _ALPHAZ <===                                                         726
         CALL AGSRESET                                                    729
         AG_X = +SFJP_SFCO_PX                                             729
         AG_Y = SFJP_SFCO_PY                                              729
         AG_Z = +(SFJP_SFFK_DZ/                                           729
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFCO_DZ/2.                   729
         AG_ALPHAN=AG_ALPHAN+1                                            729
         AG_ALPHAK(AG_ALPHAN)=3                                           729
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                                729
         AG_TITLE = 'SFCO'                                                729
         AG_EXNAME = 'POSITION'                                           729
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      729
         CALL AXPOSITION                                                  729
C    *                                                                    729
C    Create SFCM :                                                        730
C    Call To Local Procedure SFCM                                         730
         ASSIGN 5670 TO BLOCK_SFCM                                        730
         GO TO  5660                                                      730
5670     CONTINUE                                                         730
C    *                                                                    730
C    Geant POSITION : SFCM ===> X=+SFJP_SFCO_PX Y=SFJP_SFCO_PY Z=-(SFJP_  733
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFCO_DZ/2. ALPHAZ=-SFJP  733
C    _ALPHAZ <===                                                         733
         CALL AGSRESET                                                    733
         AG_X = +SFJP_SFCO_PX                                             733
         AG_Y = SFJP_SFCO_PY                                              733
         AG_Z = -(SFJP_SFFK_DZ/                                           733
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFCO_DZ/2.                   733
         AG_ALPHAN=AG_ALPHAN+1                                            733
         AG_ALPHAK(AG_ALPHAN)=3                                           733
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                                733
         AG_TITLE = 'SFCM'                                                733
         AG_EXNAME = 'POSITION'                                           733
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      733
         CALL AXPOSITION                                                  733
C    *                                                                    733
C    Geant POSITION : SFCM ===> X=-SFJP_SFCO_PX Y=SFJP_SFCO_PY Z=+(SFJP_  733
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFCO_DZ/2. ALPHAY=180 A  733
C    LPHAZ=+SFJP_ALPHAZ <===                                              733
         CALL AGSRESET                                                    736
         AG_X = -SFJP_SFCO_PX                                             736
         AG_Y = SFJP_SFCO_PY                                              736
         AG_Z = +(SFJP_SFFK_DZ/                                           736
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFCO_DZ/2.                   736
         AG_ALPHAN=AG_ALPHAN+1                                            736
         AG_ALPHAK(AG_ALPHAN)=2                                           736
         AG_ALPHAL(AG_ALPHAN)=180                                         736
         AG_ALPHAN=AG_ALPHAN+1                                            736
         AG_ALPHAK(AG_ALPHAN)=3                                           736
         AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                                736
         AG_TITLE = 'SFCM'                                                736
         AG_EXNAME = 'POSITION'                                           736
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               736
         CALL AXPOSITION                                                  736
C    *                                                                    736
C    Create SFKF :                                                        737
C    Call To Local Procedure SFKF                                         737
         ASSIGN 5690 TO BLOCK_SFKF                                        737
         GO TO  5680                                                      737
5690     CONTINUE                                                         737
C    *                                                                    737
C    Geant POSITION : SFKF ===> X=-SFJP_SFKF_PX Y=SFJP_SFKF_PY Z=-(SFJP_  740
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFKF_DZ/2-0.2 ALPHAZ=+S  740
C    FJP_ALPHAZ <===                                                      740
         CALL AGSRESET                                                    740
         AG_X = -SFJP_SFKF_PX                                             740
         AG_Y = SFJP_SFKF_PY                                              740
         AG_Z = -(SFJP_SFFK_DZ/                                           740
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFKF_DZ/2-0.2                740
         AG_ALPHAN=AG_ALPHAN+1                                            740
         AG_ALPHAK(AG_ALPHAN)=3                                           740
         AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                                740
         AG_TITLE = 'SFKF'                                                740
         AG_EXNAME = 'POSITION'                                           740
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      740
         CALL AXPOSITION                                                  740
C    *                                                                    740
C    Geant POSITION : SFKF ===> X=+SFJP_SFKF_PX Y=SFJP_SFKF_PY Z=+(SFJP_  740
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFKF_DZ/2+0.2 ALPHAZ=-S  740
C    FJP_ALPHAZ <===                                                      740
         CALL AGSRESET                                                    743
         AG_X = +SFJP_SFKF_PX                                             743
         AG_Y = SFJP_SFKF_PY                                              743
         AG_Z = +(SFJP_SFFK_DZ/                                           743
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFKF_DZ/2+0.2                743
         AG_ALPHAN=AG_ALPHAN+1                                            743
         AG_ALPHAK(AG_ALPHAN)=3                                           743
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                                743
         AG_TITLE = 'SFKF'                                                743
         AG_EXNAME = 'POSITION'                                           743
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      743
         CALL AXPOSITION                                                  743
C    *                                                                    743
C    Create SFKS :                                                        744
C    Call To Local Procedure SFKS                                         744
         ASSIGN 5710 TO BLOCK_SFKS                                        744
         GO TO  5700                                                      744
5710     CONTINUE                                                         744
C    *                                                                    744
C    Geant POSITION : SFKS ===> X=+SFJP_SFKS_PX Y=SFJP_SFKS_PY Z=-(SFJP_  747
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFKF_DZ/2-0.2 ALPHAZ=-S  747
C    FJP_ALPHAZ <===                                                      747
         CALL AGSRESET                                                    747
         AG_X = +SFJP_SFKS_PX                                             747
         AG_Y = SFJP_SFKS_PY                                              747
         AG_Z = -(SFJP_SFFK_DZ/                                           747
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFKF_DZ/2-0.2                747
         AG_ALPHAN=AG_ALPHAN+1                                            747
         AG_ALPHAK(AG_ALPHAN)=3                                           747
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                                747
         AG_TITLE = 'SFKS'                                                747
         AG_EXNAME = 'POSITION'                                           747
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      747
         CALL AXPOSITION                                                  747
C    *                                                                    747
C    Geant POSITION : SFKS ===> X=-SFJP_SFKS_PX Y=SFJP_SFKS_PY Z=+(SFJP_  747
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFKF_DZ/2+0.2 ALPHAZ=+S  747
C    FJP_ALPHAZ <===                                                      747
         CALL AGSRESET                                                    750
         AG_X = -SFJP_SFKS_PX                                             750
         AG_Y = SFJP_SFKS_PY                                              750
         AG_Z = +(SFJP_SFFK_DZ/                                           750
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFKF_DZ/2+0.2                750
         AG_ALPHAN=AG_ALPHAN+1                                            750
         AG_ALPHAK(AG_ALPHAN)=3                                           750
         AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                                750
         AG_TITLE = 'SFKS'                                                750
         AG_EXNAME = 'POSITION'                                           750
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      750
         CALL AXPOSITION                                                  750
C    *                                                                    750
C    Create SFPR :                                                        751
C    Call To Local Procedure SFPR                                         751
         ASSIGN 5730 TO BLOCK_SFPR                                        751
         GO TO  5720                                                      751
5730     CONTINUE                                                         751
C    *                                                                    751
C    Geant POSITION : SFPR ===> X=0 Y=+SFJP_SFPR_PY Z=-SFJP_SFLU_DZ <===  752
         CALL AGSRESET                                                    752
         AG_X = 0                                                         752
         AG_Y = +SFJP_SFPR_PY                                             752
         AG_Z = -SFJP_SFLU_DZ                                             752
         AG_TITLE = 'SFPR'                                                752
         AG_EXNAME = 'POSITION'                                           752
         AG_PARLIST = 'X_Y_Z'                                             752
         CALL AXPOSITION                                                  752
C    *                                                                    752
C    Geant POSITION : SFPR ===> X=0 Y=+SFJP_SFPR_PY Z=+SFJP_SFLU_DZ-3.7   752
C    <===                                                                 752
         CALL AGSRESET                                                    753
         AG_X = 0                                                         753
         AG_Y = +SFJP_SFPR_PY                                             753
         AG_Z = +SFJP_SFLU_DZ-3.7                                         753
         AG_TITLE = 'SFPR'                                                753
         AG_EXNAME = 'POSITION'                                           753
         AG_PARLIST = 'X_Y_Z'                                             753
         CALL AXPOSITION                                                  753
C    *                                                                    753
C    Create SFPB :                                                        754
C    Call To Local Procedure SFPB                                         754
         ASSIGN 5750 TO BLOCK_SFPB                                        754
         GO TO  5740                                                      754
5750     CONTINUE                                                         754
C    *                                                                    754
C    Geant POSITION : SFPB ===> X=0 Y=+SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SF  756
C    PB_PY2 Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ/2. <===                           756
         CALL AGSRESET                                                    756
         AG_X = 0                                                         756
         AG_Y = +SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SFPB_PY2                  756
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ/2.                              756
         AG_TITLE = 'SFPB'                                                756
         AG_EXNAME = 'POSITION'                                           756
         AG_PARLIST = 'X_Y_Z'                                             756
         CALL AXPOSITION                                                  756
C    *                                                                    756
C    Geant POSITION : SFPB ===> X=0 Y=+SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SF  756
C    PB_PY2 Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ/2. <===                           756
         CALL AGSRESET                                                    758
         AG_X = 0                                                         758
         AG_Y = +SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SFPB_PY2                  758
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ/2.                              758
         AG_TITLE = 'SFPB'                                                758
         AG_EXNAME = 'POSITION'                                           758
         AG_PARLIST = 'X_Y_Z'                                             758
         CALL AXPOSITION                                                  758
C    *                                                                    758
C    Create SSBS :                                                        759
C    Call To Local Procedure SSBS                                         759
         ASSIGN 5770 TO BLOCK_SSBS                                        759
         GO TO  5760                                                      759
5770     CONTINUE                                                         759
C    *                                                                    759
C    Geant POSITION : SSBS ===> X=0. Y=+SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_S  762
C    FPB_PY2-0.8/2.-SFJP_SSBS_DY/2. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-0.2+SFJP  762
C    _SSBB_DZ+SFJP_SSBS_DX/2. <===                                        762
         CALL AGSRESET                                                    762
         AG_X = 0.                                                        762
         AG_Y = +SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SFPB_PY2-0.8/             762
     *   2.-SFJP_SSBS_DY/2.                                               762
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-0.2+SFJP_SSBB_DZ+SFJP_SSBS_DX/  762
     *   2.                                                               762
         AG_TITLE = 'SSBS'                                                762
         AG_EXNAME = 'POSITION'                                           762
         AG_PARLIST = 'X_Y_Z'                                             762
         CALL AXPOSITION                                                  762
C    *                                                                    762
C    Geant POSITION : SSBS ===> X=0. Y=+SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_S  762
C    FPB_PY2-0.8/2.-SFJP_SSBS_DY/2. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+0.2-SFJP  762
C    _SSBB_DZ-SFJP_SSBS_DX/2. <===                                        762
         CALL AGSRESET                                                    765
         AG_X = 0.                                                        765
         AG_Y = +SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SFPB_PY2-0.8/             765
     *   2.-SFJP_SSBS_DY/2.                                               765
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+0.2-SFJP_SSBB_DZ-SFJP_SSBS_DX/  765
     *   2.                                                               765
         AG_TITLE = 'SSBS'                                                765
         AG_EXNAME = 'POSITION'                                           765
         AG_PARLIST = 'X_Y_Z'                                             765
         CALL AXPOSITION                                                  765
C    *                                                                    765
C    Create SSBB :                                                        766
C    Call To Local Procedure SSBB                                         766
         ASSIGN 5790 TO BLOCK_SSBB                                        766
         GO TO  5780                                                      766
5790     CONTINUE                                                         766
C    *                                                                    766
C    Geant POSITION : SSBB ===> X=0. Y=+SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_S  769
C    FPB_PY2-0.8/2.-SFJP_SSBS_DY/2. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-0.2+SFJP  769
C    _SSBB_DZ/2. <===                                                     769
         CALL AGSRESET                                                    769
         AG_X = 0.                                                        769
         AG_Y = +SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SFPB_PY2-0.8/             769
     *   2.-SFJP_SSBS_DY/2.                                               769
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-0.2+SFJP_SSBB_DZ/2.             769
         AG_TITLE = 'SSBB'                                                769
         AG_EXNAME = 'POSITION'                                           769
         AG_PARLIST = 'X_Y_Z'                                             769
         CALL AXPOSITION                                                  769
C    *                                                                    769
C    Geant POSITION : SSBB ===> X=0. Y=+SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_S  769
C    FPB_PY2-0.8/2.-SFJP_SSBS_DY/2. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+0.2-SFJP  769
C    _SSBB_DZ/2. <===                                                     769
         CALL AGSRESET                                                    772
         AG_X = 0.                                                        772
         AG_Y = +SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SFPB_PY2-0.8/             772
     *   2.-SFJP_SSBS_DY/2.                                               772
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+0.2-SFJP_SSBB_DZ/2.             772
         AG_TITLE = 'SSBB'                                                772
         AG_EXNAME = 'POSITION'                                           772
         AG_PARLIST = 'X_Y_Z'                                             772
         CALL AXPOSITION                                                  772
C    *                                                                    772
C    Create SFLA :                                                        773
C    Call To Local Procedure SFLA                                         773
         ASSIGN 5810 TO BLOCK_SFLA                                        773
         GO TO  5800                                                      773
5810     CONTINUE                                                         773
C    *                                                                    773
C    Create SFLB :                                                        774
C    Call To Local Procedure SFLB                                         774
         ASSIGN 5830 TO BLOCK_SFLB                                        774
         GO TO  5820                                                      774
5830     CONTINUE                                                         774
C    *                                                                    774
C    Create SFLC :                                                        775
C    Call To Local Procedure SFLC                                         775
         ASSIGN 5850 TO BLOCK_SFLC                                        775
         GO TO  5840                                                      775
5850     CONTINUE                                                         775
C    *                                                                    775
C    Create SFEB :                                                        776
C    Call To Local Procedure SFEB                                         776
         ASSIGN 5870 TO BLOCK_SFEB                                        776
         GO TO  5860                                                      776
5870     CONTINUE                                                         776
C    *                                                                    776
C    Create SFES :                                                        777
C    Call To Local Procedure SFES                                         777
         ASSIGN 5890 TO BLOCK_SFES                                        777
         GO TO  5880                                                      777
5890     CONTINUE                                                         777
C    *                                                                    777
         WAFPCKLEN=SFPA_WPLEN/(SFPA_NSSD*1.)                              778
C    *                                                                    779
         IWAF=1                                                           779
            GO TO 5903                                                    779
5901        IWAF=IWAF+1                                                   779
5903        IF(IWAF-(8).GT.0) GO TO 5902                                  779
C       *                                                                 780
            JWAF=1                                                        780
               GO TO 5913                                                 780
5911           JWAF=JWAF+1                                                780
5913           IF(JWAF-(IWAF).GT.0) GO TO 5912                            780
C          Geant POSITION : SFLA ===> X=+SFJP_SFLA_PX+SFJP_FLEX_DI*JWAF   783
C          Y=SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF Z=-(SFPA_WPLEN+WAFPCKLEN)/2+  783
C          IWAF*WAFPCKLEN ALPHAZ=-SFJP_ALPHAZ <===                        783
               CALL AGSRESET                                              783
               AG_X = +SFJP_SFLA_PX+SFJP_FLEX_DI*JWAF                     783
               AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF                      783
               AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+IWAF*WAFPCKLEN            783
               AG_ALPHAN=AG_ALPHAN+1                                      783
               AG_ALPHAK(AG_ALPHAN)=3                                     783
               AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                          783
               AG_TITLE = 'SFLA'                                          783
               AG_EXNAME = 'POSITION'                                     783
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                783
               CALL AXPOSITION                                            783
C          *                                                              783
C          Geant POSITION : SFLA ===> X=-SFJP_SFLA_PX-SFJP_FLEX_DI*JWAF   783
C          Y=SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF Z=+(SFPA_WPLEN+WAFPCKLEN)/2-  783
C          IWAF*WAFPCKLEN ALPHAZ=+SFJP_ALPHAZ <===                        783
               CALL AGSRESET                                              786
               AG_X = -SFJP_SFLA_PX-SFJP_FLEX_DI*JWAF                     786
               AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF                      786
               AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-IWAF*WAFPCKLEN            786
               AG_ALPHAN=AG_ALPHAN+1                                      786
               AG_ALPHAK(AG_ALPHAN)=3                                     786
               AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                          786
               AG_TITLE = 'SFLA'                                          786
               AG_EXNAME = 'POSITION'                                     786
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                786
               CALL AXPOSITION                                            786
C          *                                                              786
C          Geant POSITION : SFLC ===> X=+SFJP_SFLA_PX+SFJP_FLEX_DI*JWAF   786
C          Y=SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF Z=-(SFPA_WPLEN+WAFPCKLEN)/2.  786
C          +16*WAFPCKLEN+WAFPCKLEN/2.+SFJP_SFLB_DZ+8.5*SFJP_SFLC_DZ-IWAF  786
C          *SFJP_SFLC_DZ ALPHAZ=-SFJP_ALPHAZ <===                         786
               CALL AGSRESET                                              790
               AG_X = +SFJP_SFLA_PX+SFJP_FLEX_DI*JWAF                     790
               AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF                      790
               AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2.+16*WAFPCKLEN+WAFPCKLEN/  790
     *         2.+SFJP_SFLB_DZ+8.5*SFJP_SFLC_DZ-IWAF*SFJP_SFLC_DZ         790
               AG_ALPHAN=AG_ALPHAN+1                                      790
               AG_ALPHAK(AG_ALPHAN)=3                                     790
               AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                          790
               AG_TITLE = 'SFLC'                                          790
               AG_EXNAME = 'POSITION'                                     790
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                790
               CALL AXPOSITION                                            790
C          *                                                              790
C          Geant POSITION : SFLC ===> X=-SFJP_SFLA_PX-SFJP_FLEX_DI*JWAF   790
C          Y=SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF Z=+(SFPA_WPLEN+WAFPCKLEN)/2.  790
C          -16*WAFPCKLEN-WAFPCKLEN/2.-SFJP_SFLB_DZ-8.5*SFJP_SFLC_DZ+IWAF  790
C          *SFJP_SFLC_DZ ALPHAZ=+SFJP_ALPHAZ <===                         790
               CALL AGSRESET                                              794
               AG_X = -SFJP_SFLA_PX-SFJP_FLEX_DI*JWAF                     794
               AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF                      794
               AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2.-16*WAFPCKLEN-WAFPCKLEN/  794
     *         2.-SFJP_SFLB_DZ-8.5*SFJP_SFLC_DZ+IWAF*SFJP_SFLC_DZ         794
               AG_ALPHAN=AG_ALPHAN+1                                      794
               AG_ALPHAK(AG_ALPHAN)=3                                     794
               AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                          794
               AG_TITLE = 'SFLC'                                          794
               AG_EXNAME = 'POSITION'                                     794
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                794
               CALL AXPOSITION                                            794
C          *                                                              794
C          Geant POSITION : SFLA ===> X=+SFJP_SFLC_PX+SFJP_FLEX_DI*JWAF   794
C          Y=-SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF Z=-(SFPA_WPLEN+WAFPCKLEN)/2  794
C          +(IWAF+8)*WAFPCKLEN ALPHAZ=-SFJP_ALPHAZ <===                   794
               CALL AGSRESET                                              797
               AG_X = +SFJP_SFLC_PX+SFJP_FLEX_DI*JWAF                     797
               AG_Y = -SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF                     797
               AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+(IWAF+8)*WAFPCKLEN        797
               AG_ALPHAN=AG_ALPHAN+1                                      797
               AG_ALPHAK(AG_ALPHAN)=3                                     797
               AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                          797
               AG_TITLE = 'SFLA'                                          797
               AG_EXNAME = 'POSITION'                                     797
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                797
               CALL AXPOSITION                                            797
C          *                                                              797
C          Geant POSITION : SFLA ===> X=-SFJP_SFLC_PX-SFJP_FLEX_DI*JWAF   797
C          Y=-SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF Z=+(SFPA_WPLEN+WAFPCKLEN)/2  797
C          -(IWAF+8)*WAFPCKLEN ALPHAZ=+SFJP_ALPHAZ <===                   797
               CALL AGSRESET                                              800
               AG_X = -SFJP_SFLC_PX-SFJP_FLEX_DI*JWAF                     800
               AG_Y = -SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF                     800
               AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-(IWAF+8)*WAFPCKLEN        800
               AG_ALPHAN=AG_ALPHAN+1                                      800
               AG_ALPHAK(AG_ALPHAN)=3                                     800
               AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                          800
               AG_TITLE = 'SFLA'                                          800
               AG_EXNAME = 'POSITION'                                     800
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                800
               CALL AXPOSITION                                            800
C          *                                                              800
C          Geant POSITION : SFLC ===> X=+SFJP_SFLC_PX+SFJP_FLEX_DI*JWAF   800
C          Y=-SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF Z=-(SFPA_WPLEN+WAFPCKLEN)/2  800
C          +16*WAFPCKLEN+WAFPCKLEN/2.+8.5*SFJP_SFLC_DZ-IWAF*SFJP_SFLC_DZ  800
C           ALPHAZ=-SFJP_ALPHAZ <===                                      800
               CALL AGSRESET                                              804
               AG_X = +SFJP_SFLC_PX+SFJP_FLEX_DI*JWAF                     804
               AG_Y = -SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF                     804
               AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+16*WAFPCKLEN+WAFPCKLEN/   804
     *         2.+8.5*SFJP_SFLC_DZ-IWAF*SFJP_SFLC_DZ                      804
               AG_ALPHAN=AG_ALPHAN+1                                      804
               AG_ALPHAK(AG_ALPHAN)=3                                     804
               AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                          804
               AG_TITLE = 'SFLC'                                          804
               AG_EXNAME = 'POSITION'                                     804
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                804
               CALL AXPOSITION                                            804
C          *                                                              804
C          Geant POSITION : SFLC ===> X=-SFJP_SFLC_PX-SFJP_FLEX_DI*JWAF   804
C          Y=-SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF Z=+(SFPA_WPLEN+WAFPCKLEN)/2  804
C          -16*WAFPCKLEN-WAFPCKLEN/2.-8.5*SFJP_SFLC_DZ+IWAF*SFJP_SFLC_DZ  804
C           ALPHAZ=+SFJP_ALPHAZ <===                                      804
               CALL AGSRESET                                              808
               AG_X = -SFJP_SFLC_PX-SFJP_FLEX_DI*JWAF                     808
               AG_Y = -SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF                     808
               AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-16*WAFPCKLEN-WAFPCKLEN/   808
     *         2.-8.5*SFJP_SFLC_DZ+IWAF*SFJP_SFLC_DZ                      808
               AG_ALPHAN=AG_ALPHAN+1                                      808
               AG_ALPHAK(AG_ALPHAN)=3                                     808
               AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                          808
               AG_TITLE = 'SFLC'                                          808
               AG_EXNAME = 'POSITION'                                     808
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                808
               CALL AXPOSITION                                            808
C          *                                                              808
            GO TO 5911                                                    809
5912        CONTINUE                                                      809
C       *                                                                 810
            JWAF=1                                                        810
               GO TO 5923                                                 810
5921           JWAF=JWAF+1                                                810
5923           IF(JWAF-(8).GT.0) GO TO 5922                               810
C          Geant POSITION : SFLA ===> X=+SFJP_SFLA_PX+SFJP_FLEX_DI*JWAF   813
C          Y=SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF Z=-(SFPA_WPLEN+WAFPCKLEN)/2+  813
C          (IWAF+8)*WAFPCKLEN ALPHAZ=-SFJP_ALPHAZ <===                    813
               CALL AGSRESET                                              813
               AG_X = +SFJP_SFLA_PX+SFJP_FLEX_DI*JWAF                     813
               AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF                      813
               AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+(IWAF+8)*WAFPCKLEN        813
               AG_ALPHAN=AG_ALPHAN+1                                      813
               AG_ALPHAK(AG_ALPHAN)=3                                     813
               AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                          813
               AG_TITLE = 'SFLA'                                          813
               AG_EXNAME = 'POSITION'                                     813
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                813
               CALL AXPOSITION                                            813
C          *                                                              813
C          Geant POSITION : SFLA ===> X=-SFJP_SFLA_PX-SFJP_FLEX_DI*JWAF   813
C          Y=SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF Z=+(SFPA_WPLEN+WAFPCKLEN)/2-  813
C          (IWAF+8)*WAFPCKLEN ALPHAZ=+SFJP_ALPHAZ <===                    813
               CALL AGSRESET                                              816
               AG_X = -SFJP_SFLA_PX-SFJP_FLEX_DI*JWAF                     816
               AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF                      816
               AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-(IWAF+8)*WAFPCKLEN        816
               AG_ALPHAN=AG_ALPHAN+1                                      816
               AG_ALPHAK(AG_ALPHAN)=3                                     816
               AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                          816
               AG_TITLE = 'SFLA'                                          816
               AG_EXNAME = 'POSITION'                                     816
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                816
               CALL AXPOSITION                                            816
C          *                                                              816
            GO TO 5921                                                    817
5922        CONTINUE                                                      817
C       Geant POSITION : SFLB ===> X=+SFJP_SFLA_PX+SFJP_FLEX_DI*IWAF Y=S  821
C       FJP_SFLA_PY+SFJP_FLEX_DI*IWAF Z=-(SFPA_WPLEN+WAFPCKLEN)/2+16*WAF  821
C       PCKLEN+WAFPCKLEN/2.+SFJP_SFLB_DZ/2. ALPHAZ=-SFJP_ALPHAZ <===      821
            CALL AGSRESET                                                 821
            AG_X = +SFJP_SFLA_PX+SFJP_FLEX_DI*IWAF                        821
            AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*IWAF                         821
            AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+16*WAFPCKLEN+WAFPCKLEN/      821
     *      2.+SFJP_SFLB_DZ/2.                                            821
            AG_ALPHAN=AG_ALPHAN+1                                         821
            AG_ALPHAK(AG_ALPHAN)=3                                        821
            AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                             821
            AG_TITLE = 'SFLB'                                             821
            AG_EXNAME = 'POSITION'                                        821
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   821
            CALL AXPOSITION                                               821
C       *                                                                 821
C       Geant POSITION : SFLB ===> X=-SFJP_SFLA_PX-SFJP_FLEX_DI*IWAF Y=S  821
C       FJP_SFLA_PY+SFJP_FLEX_DI*IWAF Z=+(SFPA_WPLEN+WAFPCKLEN)/2-16*WAF  821
C       PCKLEN-WAFPCKLEN/2.-SFJP_SFLB_DZ/2. ALPHAZ=+SFJP_ALPHAZ <===      821
            CALL AGSRESET                                                 825
            AG_X = -SFJP_SFLA_PX-SFJP_FLEX_DI*IWAF                        825
            AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*IWAF                         825
            AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-16*WAFPCKLEN-WAFPCKLEN/      825
     *      2.-SFJP_SFLB_DZ/2.                                            825
            AG_ALPHAN=AG_ALPHAN+1                                         825
            AG_ALPHAK(AG_ALPHAN)=3                                        825
            AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                             825
            AG_TITLE = 'SFLB'                                             825
            AG_EXNAME = 'POSITION'                                        825
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   825
            CALL AXPOSITION                                               825
C       *                                                                 825
C       Geant POSITION : SFES ===> X=-SFJP_SFES_PX Y=-SFJP_SFES_PY Z=-(S  825
C       FPA_WPLEN+WAFPCKLEN)/2+(IWAF)*WAFPCKLEN+WAFPCKLEN/2.-SFJP_SFEB_D  825
C       Z/2.-SFJP_SFES_PZ/2. ALPHAZ=57.66 <===                            825
            CALL AGSRESET                                                 828
            AG_X = -SFJP_SFES_PX                                          828
            AG_Y = -SFJP_SFES_PY                                          828
            AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+(IWAF)*WAFPCKLEN+WAFPCKLEN/  828
     *      2.-SFJP_SFEB_DZ/2.-SFJP_SFES_PZ/2.                            828
            AG_ALPHAN=AG_ALPHAN+1                                         828
            AG_ALPHAK(AG_ALPHAN)=3                                        828
            AG_ALPHAL(AG_ALPHAN)=57.66                                    828
            AG_TITLE = 'SFES'                                             828
            AG_EXNAME = 'POSITION'                                        828
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   828
            CALL AXPOSITION                                               828
C       *                                                                 828
C       Geant POSITION : SFES ===> X=+SFJP_SFES_PX Y=-SFJP_SFES_PY Z=+(S  828
C       FPA_WPLEN+WAFPCKLEN)/2-(IWAF)*WAFPCKLEN-WAFPCKLEN/2.+SFJP_SFEB_D  828
C       Z/2.+SFJP_SFES_PZ/2. ALPHAZ=-57.66 <===                           828
            CALL AGSRESET                                                 831
            AG_X = +SFJP_SFES_PX                                          831
            AG_Y = -SFJP_SFES_PY                                          831
            AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-(IWAF)*WAFPCKLEN-WAFPCKLEN/  831
     *      2.+SFJP_SFEB_DZ/2.+SFJP_SFES_PZ/2.                            831
            AG_ALPHAN=AG_ALPHAN+1                                         831
            AG_ALPHAK(AG_ALPHAN)=3                                        831
            AG_ALPHAL(AG_ALPHAN)=-57.66                                   831
            AG_TITLE = 'SFES'                                             831
            AG_EXNAME = 'POSITION'                                        831
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   831
            CALL AXPOSITION                                               831
C       *                                                                 831
C       Geant POSITION : SFEB ===> X=-SFJP_SFEB_PX Y=-SFJP_SFEB_PY Z=-(S  831
C       FPA_WPLEN+WAFPCKLEN)/2+(IWAF+8)*WAFPCKLEN+WAFPCKLEN/2.-SFJP_SFEB  831
C       _DZ/2.-SFJP_SFES_PZ/2. ALPHAZ=55.35 <===                          831
            CALL AGSRESET                                                 834
            AG_X = -SFJP_SFEB_PX                                          834
            AG_Y = -SFJP_SFEB_PY                                          834
            AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/                               834
     *      2+(IWAF+8)*WAFPCKLEN+WAFPCKLEN/2.-SFJP_SFEB_DZ/               834
     *      2.-SFJP_SFES_PZ/2.                                            834
            AG_ALPHAN=AG_ALPHAN+1                                         834
            AG_ALPHAK(AG_ALPHAN)=3                                        834
            AG_ALPHAL(AG_ALPHAN)=55.35                                    834
            AG_TITLE = 'SFEB'                                             834
            AG_EXNAME = 'POSITION'                                        834
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   834
            CALL AXPOSITION                                               834
C       *                                                                 834
C       Geant POSITION : SFEB ===> X=+SFJP_SFEB_PX Y=-SFJP_SFEB_PY Z=+(S  834
C       FPA_WPLEN+WAFPCKLEN)/2-(IWAF+8)*WAFPCKLEN-WAFPCKLEN/2.+SFJP_SFEB  834
C       _DZ/2.+SFJP_SFES_PZ/2. ALPHAZ=-55.35 <===                         834
            CALL AGSRESET                                                 837
            AG_X = +SFJP_SFEB_PX                                          837
            AG_Y = -SFJP_SFEB_PY                                          837
            AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/                               837
     *      2-(IWAF+8)*WAFPCKLEN-WAFPCKLEN/2.+SFJP_SFEB_DZ/               837
     *      2.+SFJP_SFES_PZ/2.                                            837
            AG_ALPHAN=AG_ALPHAN+1                                         837
            AG_ALPHAK(AG_ALPHAN)=3                                        837
            AG_ALPHAL(AG_ALPHAN)=-55.35                                   837
            AG_TITLE = 'SFEB'                                             837
            AG_EXNAME = 'POSITION'                                        837
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   837
            CALL AXPOSITION                                               837
C       *                                                                 837
         GO TO 5901                                                       838
5902     CONTINUE                                                         838
5271  CALL AGSPOPB                                                        839
      GO TO BLOCK_SFSM, (5270 ,5280)                                      839
C End of Local Procedure                                                  839
C ----------------------------------------------------------------------  840
C ------------ SFLT                                                       840
C Begin Local Procedure SFLT                                              841
C SFLT IS (HALF) THE TOP CORNER OF THE TRIANGULAR LADDER SKELETON         841
5480  CONTINUE                                                            841
         AG_TITLE='SFLT'                                                  841
         AG_MODULE=MODULE                                                 841
         AG_MARK='V00'                                                    841
         CALL AGSPUSH                                                     841
         AG_MODULE=MODULE                                                 841
C    *                                                                    841
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           842
         CALL AGSRESET                                                    842
         AG_TITLE = 'CARBON'                                              842
         AG_EXNAME = 'MATERIAL'                                           842
         AG_PARLIST = 'NONE'                                              842
         CALL AXMATERIAL                                                  842
C    *                                                                    842
C    Geant ATTRIBUTE : SFLT ===> SEEN=1 COLO=1 <===                       842
         CALL AGSRESET                                                    843
         AG_SEEN = 1                                                      843
         AG_COLO = 1                                                      843
         AG_TITLE = 'SFLT'                                                843
         AG_EXNAME = 'ATTRIBUTE'                                          843
         AG_PARLIST = 'SEEN_COLO'                                         843
         CALL AXATTRIBUTE                                                 843
C    *                                                                    843
C    Geant SHAPE : TRAP ===> DZ=SFJP_SFLU_DZ THET=0 PHI=0, H1=SFJP_SFLU_  843
C    H1 BL1=SFJP_SFLU_BL1 TL1=SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(PI/5), ALP1  843
C    =(PI/2-ATAN(2*TAN(PI/5)))*360/(2*PI), H2=SFJP_SFLU_H1 BL2=SFJP_SFLU  843
C    _BL1, TL2=SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(PI/5), ALP2=(PI/2-ATAN(2*T  843
C    AN(PI/5)))*360/(2*PI) <===                                           843
         CALL AGSRESET                                                    849
         AG_DZ = SFJP_SFLU_DZ                                             849
         AG_THET = 0                                                      849
         AG_PHI = 0                                                       849
         AG_H1 = SFJP_SFLU_H1                                             849
         AG_BL1 = SFJP_SFLU_BL1                                           849
         AG_TL1 = SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(PI/5)                    849
         AG_ALP1 = (PI/2-ATAN(2*TAN(PI/5)))*360/(2*PI)                    849
         AG_H2 = SFJP_SFLU_H1                                             849
         AG_BL2 = SFJP_SFLU_BL1                                           849
         AG_TL2 = SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(PI/5)                    849
         AG_ALP2 = (PI/2-ATAN(2*TAN(PI/5)))*360/(2*PI)                    849
         AG_TITLE = 'TRAP'                                                849
         AG_EXNAME = 'SHAPE'                                              849
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       849
         CALL AXSHAPE                                                     849
C    *                                                                    849
5481  CALL AGSPOPB                                                        850
      GO TO BLOCK_SFLT, (5480 ,5490)                                      850
C End of Local Procedure                                                  850
C ----------------------------------------------------------------------  851
C ------------ SFLU                                                       851
C Begin Local Procedure SFLU                                              852
C SFLU IS (HALF) THE SIDE CORNER OF THE TRIANGULAR LADDER SKELETON        852
5500  CONTINUE                                                            852
         AG_TITLE='SFLU'                                                  852
         AG_MODULE=MODULE                                                 852
         AG_MARK='V00'                                                    852
         CALL AGSPUSH                                                     852
         AG_MODULE=MODULE                                                 852
C    *                                                                    852
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           853
         CALL AGSRESET                                                    853
         AG_TITLE = 'CARBON'                                              853
         AG_EXNAME = 'MATERIAL'                                           853
         AG_PARLIST = 'NONE'                                              853
         CALL AXMATERIAL                                                  853
C    *                                                                    853
C    Geant ATTRIBUTE : SFLU ===> SEEN=1 COLO=1 <===                       853
         CALL AGSRESET                                                    854
         AG_SEEN = 1                                                      854
         AG_COLO = 1                                                      854
         AG_TITLE = 'SFLU'                                                854
         AG_EXNAME = 'ATTRIBUTE'                                          854
         AG_PARLIST = 'SEEN_COLO'                                         854
         CALL AXATTRIBUTE                                                 854
C    *                                                                    854
C    Geant SHAPE : TRAP ===> DZ=SFJP_SFLU_DZ THET=0 PHI=0, H1=SFJP_SFLU_  854
C    H1 BL1=SFJP_SFLU_BL1 TL1=SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(27*2*PI/360  854
C    ), ALP1=(PI/2-ATAN(2*TAN(27*2*PI/360)))*360/(2*PI), H2=SFJP_SFLU_H1  854
C    , BL2=SFJP_SFLU_BL1 TL2=SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(27*2*PI/360)  854
C    , ALP2=(PI/2-ATAN(2*TAN(27*2*PI/360)))*360/(2*PI) <===               854
         CALL AGSRESET                                                    860
         AG_DZ = SFJP_SFLU_DZ                                             860
         AG_THET = 0                                                      860
         AG_PHI = 0                                                       860
         AG_H1 = SFJP_SFLU_H1                                             860
         AG_BL1 = SFJP_SFLU_BL1                                           860
         AG_TL1 = SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(27*2*PI/360)             860
         AG_ALP1 = (PI/2-ATAN(2*TAN(27*2*PI/360)))*360/(2*PI)             860
         AG_H2 = SFJP_SFLU_H1                                             860
         AG_BL2 = SFJP_SFLU_BL1                                           860
         AG_TL2 = SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(27*2*PI/360)             860
         AG_ALP2 = (PI/2-ATAN(2*TAN(27*2*PI/360)))*360/(2*PI)             860
         AG_TITLE = 'TRAP'                                                860
         AG_EXNAME = 'SHAPE'                                              860
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       860
         CALL AXSHAPE                                                     860
C    *                                                                    860
5501  CALL AGSPOPB                                                        861
      GO TO BLOCK_SFLU, (5500 ,5510)                                      861
C End of Local Procedure                                                  861
C ----------------------------------------------------------------------  862
C ------------ SFFK                                                       862
C Begin Local Procedure SFFK                                              863
C SFFK HORIZONTAL PART OF THE LADDER SKELETON CARBON BASE                 863
5520  CONTINUE                                                            863
         AG_TITLE='SFFK'                                                  863
         AG_MODULE=MODULE                                                 863
         AG_MARK='V00'                                                    863
         CALL AGSPUSH                                                     863
         AG_MODULE=MODULE                                                 863
C    *                                                                    863
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           864
         CALL AGSRESET                                                    864
         AG_TITLE = 'CARBON'                                              864
         AG_EXNAME = 'MATERIAL'                                           864
         AG_PARLIST = 'NONE'                                              864
         CALL AXMATERIAL                                                  864
C    *                                                                    864
C    Geant ATTRIBUTE : SFFK ===> SEEN=1 COLO=1 <===                       864
         CALL AGSRESET                                                    865
         AG_SEEN = 1                                                      865
         AG_COLO = 1                                                      865
         AG_TITLE = 'SFFK'                                                865
         AG_EXNAME = 'ATTRIBUTE'                                          865
         AG_PARLIST = 'SEEN_COLO'                                         865
         CALL AXATTRIBUTE                                                 865
C    *                                                                    865
C    Geant SHAPE : BOX ===> DX=SFJP_SFFK_DXE DY=SFJP_SFFK_DY, DZ=SFJP_SF  865
C    FK_DZ/2 <===                                                         865
         CALL AGSRESET                                                    867
         AG_DX = SFJP_SFFK_DXE                                            867
         AG_DY = SFJP_SFFK_DY                                             867
         AG_DZ = SFJP_SFFK_DZ/2                                           867
         AG_TITLE = 'BOX'                                                 867
         AG_EXNAME = 'SHAPE'                                              867
         AG_PARLIST = 'DX_DY_DZ'                                          867
         CALL AXSHAPE                                                     867
C    *                                                                    867
5521  CALL AGSPOPB                                                        868
      GO TO BLOCK_SFFK, (5520 ,5530)                                      868
C End of Local Procedure                                                  868
C ----------------------------------------------------------------------  869
C ------------ SFFL                                                       869
C Begin Local Procedure SFFL                                              870
C SFFL TITLED PART OF THE LADDER SKELETON CARBON BASE                     870
5540  CONTINUE                                                            870
         AG_TITLE='SFFL'                                                  870
         AG_MODULE=MODULE                                                 870
         AG_MARK='V00'                                                    870
         CALL AGSPUSH                                                     870
         AG_MODULE=MODULE                                                 870
C    *                                                                    870
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           871
         CALL AGSRESET                                                    871
         AG_TITLE = 'CARBON'                                              871
         AG_EXNAME = 'MATERIAL'                                           871
         AG_PARLIST = 'NONE'                                              871
         CALL AXMATERIAL                                                  871
C    *                                                                    871
C    Geant ATTRIBUTE : SFFL ===> SEEN=1 COLO=1 <===                       871
         CALL AGSRESET                                                    872
         AG_SEEN = 1                                                      872
         AG_COLO = 1                                                      872
         AG_TITLE = 'SFFL'                                                872
         AG_EXNAME = 'ATTRIBUTE'                                          872
         AG_PARLIST = 'SEEN_COLO'                                         872
         CALL AXATTRIBUTE                                                 872
C    *                                                                    872
C    Geant SHAPE : BOX ===> DX=SFJP_SFFL_DX/COS(15*PI/180), DY=SFJP_SFFK  872
C    _DY DZ=SFJP_SFFK_DZ/2 <===                                           872
         CALL AGSRESET                                                    874
         AG_DX = SFJP_SFFL_DX/COS(15*PI/180)                              874
         AG_DY = SFJP_SFFK_DY                                             874
         AG_DZ = SFJP_SFFK_DZ/2                                           874
         AG_TITLE = 'BOX'                                                 874
         AG_EXNAME = 'SHAPE'                                              874
         AG_PARLIST = 'DX_DY_DZ'                                          874
         CALL AXSHAPE                                                     874
C    *                                                                    874
5541  CALL AGSPOPB                                                        875
      GO TO BLOCK_SFFL, (5540 ,5550)                                      875
C End of Local Procedure                                                  875
C ----------------------------------------------------------------------  876
C ------------ SFKK                                                       876
C Begin Local Procedure SFKK                                              877
C SFKK HORIZONTAL PART OF THE KAPTON FILM UNDER THE LADDER BASE           877
5560  CONTINUE                                                            877
         AG_TITLE='SFKK'                                                  877
         AG_MODULE=MODULE                                                 877
         AG_MARK='V00'                                                    877
         CALL AGSPUSH                                                     877
         AG_MODULE=MODULE                                                 877
C    *                                                                    877
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            878
         CALL AGSRESET                                                    878
         AG_TITLE = 'MYLAR'                                               878
         AG_EXNAME = 'MATERIAL'                                           878
         AG_PARLIST = 'NONE'                                              878
         CALL AXMATERIAL                                                  878
C    *                                                                    878
C    Geant ATTRIBUTE : SFKK ===> SEEN=1 COLO=1 <===                       878
         CALL AGSRESET                                                    879
         AG_SEEN = 1                                                      879
         AG_COLO = 1                                                      879
         AG_TITLE = 'SFKK'                                                879
         AG_EXNAME = 'ATTRIBUTE'                                          879
         AG_PARLIST = 'SEEN_COLO'                                         879
         CALL AXATTRIBUTE                                                 879
C    *                                                                    879
C    Geant SHAPE : BOX ===> DX=SFJP_SFFK_DXZ/2. DY=SFJP_SFKK_DY/2., DZ=S  879
C    FJP_SFFK_DZ/2 <===                                                   879
         CALL AGSRESET                                                    881
         AG_DX = SFJP_SFFK_DXZ/2.                                         881
         AG_DY = SFJP_SFKK_DY/2.                                          881
         AG_DZ = SFJP_SFFK_DZ/2                                           881
         AG_TITLE = 'BOX'                                                 881
         AG_EXNAME = 'SHAPE'                                              881
         AG_PARLIST = 'DX_DY_DZ'                                          881
         CALL AXSHAPE                                                     881
C    *                                                                    881
5561  CALL AGSPOPB                                                        882
      GO TO BLOCK_SFKK, (5560 ,5570)                                      882
C End of Local Procedure                                                  882
C ----------------------------------------------------------------------  883
C ------------ SFKL                                                       883
C Begin Local Procedure SFKL                                              884
C SFKL TITLED PART OF THE KPATON FILM UNDER THE LADDER BASE               884
5580  CONTINUE                                                            884
         AG_TITLE='SFKL'                                                  884
         AG_MODULE=MODULE                                                 884
         AG_MARK='V00'                                                    884
         CALL AGSPUSH                                                     884
         AG_MODULE=MODULE                                                 884
C    *                                                                    884
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            885
         CALL AGSRESET                                                    885
         AG_TITLE = 'MYLAR'                                               885
         AG_EXNAME = 'MATERIAL'                                           885
         AG_PARLIST = 'NONE'                                              885
         CALL AXMATERIAL                                                  885
C    *                                                                    885
C    Geant ATTRIBUTE : SFKL ===> SEEN=1 COLO=1 <===                       885
         CALL AGSRESET                                                    886
         AG_SEEN = 1                                                      886
         AG_COLO = 1                                                      886
         AG_TITLE = 'SFKL'                                                886
         AG_EXNAME = 'ATTRIBUTE'                                          886
         AG_PARLIST = 'SEEN_COLO'                                         886
         CALL AXATTRIBUTE                                                 886
C    *                                                                    886
C    Geant SHAPE : BOX ===> DX=SFJP_SFFL_DX/COS(15*PI/180), DY=SFJP_SFKK  886
C    _DY/2. DZ=SFJP_SFFK_DZ/2 <===                                        886
         CALL AGSRESET                                                    888
         AG_DX = SFJP_SFFL_DX/COS(15*PI/180)                              888
         AG_DY = SFJP_SFKK_DY/2.                                          888
         AG_DZ = SFJP_SFFK_DZ/2                                           888
         AG_TITLE = 'BOX'                                                 888
         AG_EXNAME = 'SHAPE'                                              888
         AG_PARLIST = 'DX_DY_DZ'                                          888
         CALL AXSHAPE                                                     888
C    *                                                                    888
5581  CALL AGSPOPB                                                        889
      GO TO BLOCK_SFKL, (5580 ,5590)                                      889
C End of Local Procedure                                                  889
C ----------------------------------------------------------------------  890
C ------------ SFLA                                                       890
C Begin Local Procedure SFLA                                              892
C SFLA IS THE LONG PART OF THE BUS CABLE LINKING THE MODULES TO THE CONN  892
C ECTION, BOARD                                                           892
5800  CONTINUE                                                            892
         AG_TITLE='SFLA'                                                  892
         AG_MODULE=MODULE                                                 892
         AG_MARK='V00'                                                    892
         CALL AGSPUSH                                                     892
         AG_MODULE=MODULE                                                 892
C    *                                                                    892
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            893
         CALL AGSRESET                                                    893
         AG_TITLE = 'MYLAR'                                               893
         AG_EXNAME = 'MATERIAL'                                           893
         AG_PARLIST = 'NONE'                                              893
         CALL AXMATERIAL                                                  893
C    *                                                                    893
C    Geant ATTRIBUTE : SFLA ===> SEEN=1 COLO=1 <===                       893
         CALL AGSRESET                                                    894
         AG_SEEN = 1                                                      894
         AG_COLO = 1                                                      894
         AG_TITLE = 'SFLA'                                                894
         AG_EXNAME = 'ATTRIBUTE'                                          894
         AG_PARLIST = 'SEEN_COLO'                                         894
         CALL AXATTRIBUTE                                                 894
C    *                                                                    894
C    Geant SHAPE : BOX ===> DX=SFJP_SFLA_DX/2. DY=SFJP_SFLA_DY/2., DZ=SF  894
C    JP_SFLA_DZ/2. <===                                                   894
         CALL AGSRESET                                                    896
         AG_DX = SFJP_SFLA_DX/2.                                          896
         AG_DY = SFJP_SFLA_DY/2.                                          896
         AG_DZ = SFJP_SFLA_DZ/2.                                          896
         AG_TITLE = 'BOX'                                                 896
         AG_EXNAME = 'SHAPE'                                              896
         AG_PARLIST = 'DX_DY_DZ'                                          896
         CALL AXSHAPE                                                     896
C    *                                                                    896
5801  CALL AGSPOPB                                                        897
      GO TO BLOCK_SFLA, (5800 ,5810)                                      897
C End of Local Procedure                                                  897
C ----------------------------------------------------------------------  898
C ------------ SFLB                                                       898
C Begin Local Procedure SFLB                                              899
C SFLB IS THE PART OF THE LONG BUS CABLE ON THE CONNECTION BOARD          899
5820  CONTINUE                                                            899
         AG_TITLE='SFLB'                                                  899
         AG_MODULE=MODULE                                                 899
         AG_MARK='V00'                                                    899
         CALL AGSPUSH                                                     899
         AG_MODULE=MODULE                                                 899
C    *                                                                    899
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            900
         CALL AGSRESET                                                    900
         AG_TITLE = 'MYLAR'                                               900
         AG_EXNAME = 'MATERIAL'                                           900
         AG_PARLIST = 'NONE'                                              900
         CALL AXMATERIAL                                                  900
C    *                                                                    900
C    Geant ATTRIBUTE : SFLB ===> SEEN=1 COLO=1 <===                       900
         CALL AGSRESET                                                    901
         AG_SEEN = 1                                                      901
         AG_COLO = 1                                                      901
         AG_TITLE = 'SFLB'                                                901
         AG_EXNAME = 'ATTRIBUTE'                                          901
         AG_PARLIST = 'SEEN_COLO'                                         901
         CALL AXATTRIBUTE                                                 901
C    *                                                                    901
C    Geant SHAPE : BOX ===> DX=SFJP_SFLA_DX/2. DY=SFJP_SFLA_DY/2., DZ=SF  901
C    JP_SFLB_DZ/2. <===                                                   901
         CALL AGSRESET                                                    903
         AG_DX = SFJP_SFLA_DX/2.                                          903
         AG_DY = SFJP_SFLA_DY/2.                                          903
         AG_DZ = SFJP_SFLB_DZ/2.                                          903
         AG_TITLE = 'BOX'                                                 903
         AG_EXNAME = 'SHAPE'                                              903
         AG_PARLIST = 'DX_DY_DZ'                                          903
         CALL AXSHAPE                                                     903
C    *                                                                    903
5821  CALL AGSPOPB                                                        904
      GO TO BLOCK_SFLB, (5820 ,5830)                                      904
C End of Local Procedure                                                  904
C ----------------------------------------------------------------------  905
C ------------ SFLC                                                       905
C Begin Local Procedure SFLC                                              907
C SFLC IS THE PART OF THE LONG BUS CABLE ON THE CONNECTION BOARD UP TO,   907
C THE CONNECTOR                                                           907
5840  CONTINUE                                                            907
         AG_TITLE='SFLC'                                                  907
         AG_MODULE=MODULE                                                 907
         AG_MARK='V00'                                                    907
         CALL AGSPUSH                                                     907
         AG_MODULE=MODULE                                                 907
C    *                                                                    907
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            908
         CALL AGSRESET                                                    908
         AG_TITLE = 'MYLAR'                                               908
         AG_EXNAME = 'MATERIAL'                                           908
         AG_PARLIST = 'NONE'                                              908
         CALL AXMATERIAL                                                  908
C    *                                                                    908
C    Geant ATTRIBUTE : SFLC ===> SEEN=1 COLO=1 <===                       908
         CALL AGSRESET                                                    909
         AG_SEEN = 1                                                      909
         AG_COLO = 1                                                      909
         AG_TITLE = 'SFLC'                                                909
         AG_EXNAME = 'ATTRIBUTE'                                          909
         AG_PARLIST = 'SEEN_COLO'                                         909
         CALL AXATTRIBUTE                                                 909
C    *                                                                    909
C    Geant SHAPE : BOX ===> DX=SFJP_SFLA_DX/2. DY=SFJP_SFLA_DY/2., DZ=SF  909
C    JP_SFLC_DZ/2. <===                                                   909
         CALL AGSRESET                                                    911
         AG_DX = SFJP_SFLA_DX/2.                                          911
         AG_DY = SFJP_SFLA_DY/2.                                          911
         AG_DZ = SFJP_SFLC_DZ/2.                                          911
         AG_TITLE = 'BOX'                                                 911
         AG_EXNAME = 'SHAPE'                                              911
         AG_PARLIST = 'DX_DY_DZ'                                          911
         CALL AXSHAPE                                                     911
C    *                                                                    911
5841  CALL AGSPOPB                                                        912
      GO TO BLOCK_SFLC, (5840 ,5850)                                      912
C End of Local Procedure                                                  912
C ----------------------------------------------------------------------  913
C ------------ SFEB                                                       913
C Begin Local Procedure SFEB                                              914
C SFEB IS THE BIG BUS ELBOW                                               914
5860  CONTINUE                                                            914
         AG_TITLE='SFEB'                                                  914
         AG_MODULE=MODULE                                                 914
         AG_MARK='V00'                                                    914
         CALL AGSPUSH                                                     914
         AG_MODULE=MODULE                                                 914
C    *                                                                    914
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            915
         CALL AGSRESET                                                    915
         AG_TITLE = 'MYLAR'                                               915
         AG_EXNAME = 'MATERIAL'                                           915
         AG_PARLIST = 'NONE'                                              915
         CALL AXMATERIAL                                                  915
C    *                                                                    915
C    Geant ATTRIBUTE : SFEB ===> SEEN=1 COLO=1 <===                       915
         CALL AGSRESET                                                    916
         AG_SEEN = 1                                                      916
         AG_COLO = 1                                                      916
         AG_TITLE = 'SFEB'                                                916
         AG_EXNAME = 'ATTRIBUTE'                                          916
         AG_PARLIST = 'SEEN_COLO'                                         916
         CALL AXATTRIBUTE                                                 916
C    *                                                                    916
C    Geant SHAPE : BOX ===> DX=SFJP_SFEB_DX/2. DY=SFJP_SFLA_DY/2., DZ=SF  916
C    JP_SFEB_DZ/2. <===                                                   916
         CALL AGSRESET                                                    918
         AG_DX = SFJP_SFEB_DX/2.                                          918
         AG_DY = SFJP_SFLA_DY/2.                                          918
         AG_DZ = SFJP_SFEB_DZ/2.                                          918
         AG_TITLE = 'BOX'                                                 918
         AG_EXNAME = 'SHAPE'                                              918
         AG_PARLIST = 'DX_DY_DZ'                                          918
         CALL AXSHAPE                                                     918
C    *                                                                    918
5861  CALL AGSPOPB                                                        919
      GO TO BLOCK_SFEB, (5860 ,5870)                                      919
C End of Local Procedure                                                  919
C ----------------------------------------------------------------------  920
C ------------ SFES                                                       920
C Begin Local Procedure SFES                                              921
C SFES IS THE SMALL BUS ELBOW                                             921
5880  CONTINUE                                                            921
         AG_TITLE='SFES'                                                  921
         AG_MODULE=MODULE                                                 921
         AG_MARK='V00'                                                    921
         CALL AGSPUSH                                                     921
         AG_MODULE=MODULE                                                 921
C    *                                                                    921
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            922
         CALL AGSRESET                                                    922
         AG_TITLE = 'MYLAR'                                               922
         AG_EXNAME = 'MATERIAL'                                           922
         AG_PARLIST = 'NONE'                                              922
         CALL AXMATERIAL                                                  922
C    *                                                                    922
C    Geant ATTRIBUTE : SFES ===> SEEN=1 COLO=1 <===                       922
         CALL AGSRESET                                                    923
         AG_SEEN = 1                                                      923
         AG_COLO = 1                                                      923
         AG_TITLE = 'SFES'                                                923
         AG_EXNAME = 'ATTRIBUTE'                                          923
         AG_PARLIST = 'SEEN_COLO'                                         923
         CALL AXATTRIBUTE                                                 923
C    *                                                                    923
C    Geant SHAPE : BOX ===> DX=SFJP_SFES_DX/2. DY=SFJP_SFLA_DY/2., DZ=SF  923
C    JP_SFEB_DZ/2. <===                                                   923
         CALL AGSRESET                                                    925
         AG_DX = SFJP_SFES_DX/2.                                          925
         AG_DY = SFJP_SFLA_DY/2.                                          925
         AG_DZ = SFJP_SFEB_DZ/2.                                          925
         AG_TITLE = 'BOX'                                                 925
         AG_EXNAME = 'SHAPE'                                              925
         AG_PARLIST = 'DX_DY_DZ'                                          925
         CALL AXSHAPE                                                     925
C    *                                                                    925
5881  CALL AGSPOPB                                                        926
      GO TO BLOCK_SFES, (5880 ,5890)                                      926
C End of Local Procedure                                                  926
C ----------------------------------------------------------------------  927
C ------------ SFAM                                                       927
C Begin Local Procedure SFAM                                              928
C SFAM IS THE MOTHER VOLUME OF THE ADC BOARD                              928
5620  CONTINUE                                                            928
         AG_TITLE='SFAM'                                                  928
         AG_MODULE=MODULE                                                 928
         AG_MARK='V00'                                                    928
         CALL AGSPUSH                                                     928
         AG_MODULE=MODULE                                                 928
C    *                                                                    928
C    Geant MATERIAL : AIR ===> inherited arguments used <===              929
         CALL AGSRESET                                                    929
         AG_TITLE = 'AIR'                                                 929
         AG_EXNAME = 'MATERIAL'                                           929
         AG_PARLIST = 'NONE'                                              929
         CALL AXMATERIAL                                                  929
C    *                                                                    929
C    Geant ATTRIBUTE : SFAM ===> SEEN=0 COLO=6 <===                       929
         CALL AGSRESET                                                    930
         AG_SEEN = 0                                                      930
         AG_COLO = 6                                                      930
         AG_TITLE = 'SFAM'                                                930
         AG_EXNAME = 'ATTRIBUTE'                                          930
         AG_PARLIST = 'SEEN_COLO'                                         930
         CALL AXATTRIBUTE                                                 930
C    *                                                                    930
C    Geant SHAPE : BOX ===> DX=SFJP_SFAM_DXE/2. DY=SFJP_SFAM_DY/2., DZ=S  930
C    FJP_SFAM_DZ/2 <===                                                   930
         CALL AGSRESET                                                    932
         AG_DX = SFJP_SFAM_DXE/2.                                         932
         AG_DY = SFJP_SFAM_DY/2.                                          932
         AG_DZ = SFJP_SFAM_DZ/2                                           932
         AG_TITLE = 'BOX'                                                 932
         AG_EXNAME = 'SHAPE'                                              932
         AG_PARLIST = 'DX_DY_DZ'                                          932
         CALL AXSHAPE                                                     932
C    *                                                                    932
C    Create SFAB :                                                        933
C    Call To Local Procedure SFAB                                         933
         ASSIGN 5940 TO BLOCK_SFAB                                        933
         GO TO  5930                                                      933
5940     CONTINUE                                                         933
C    *                                                                    933
C    Geant POSITION : SFAB ===> X=0. Y=0. Z=SFJP_SFAM_DZS/2 <===          934
         CALL AGSRESET                                                    934
         AG_X = 0.                                                        934
         AG_Y = 0.                                                        934
         AG_Z = SFJP_SFAM_DZS/2                                           934
         AG_TITLE = 'SFAB'                                                934
         AG_EXNAME = 'POSITION'                                           934
         AG_PARLIST = 'X_Y_Z'                                             934
         CALL AXPOSITION                                                  934
C    *                                                                    934
C    Create SFAS :                                                        935
C    Call To Local Procedure SFAS                                         935
         ASSIGN 5960 TO BLOCK_SFAS                                        935
         GO TO  5950                                                      935
5960     CONTINUE                                                         935
C    *                                                                    935
C    Geant POSITION : SFAS ===> X=(SFJP_SFAM_DXE-SFJP_SFAM_DXZ)/2. Y=0.   937
C    Z=(-SFJP_SFAM_DZ+SFJP_SFAM_DZS)/2 <===                               937
         CALL AGSRESET                                                    937
         AG_X = (SFJP_SFAM_DXE-SFJP_SFAM_DXZ)/2.                          937
         AG_Y = 0.                                                        937
         AG_Z = (-SFJP_SFAM_DZ+SFJP_SFAM_DZS)/2                           937
         AG_TITLE = 'SFAS'                                                937
         AG_EXNAME = 'POSITION'                                           937
         AG_PARLIST = 'X_Y_Z'                                             937
         CALL AXPOSITION                                                  937
C    *                                                                    937
C    Geant POSITION : SFAS ===> X=(-SFJP_SFAM_DXE+SFJP_SFAM_DXZ)/2. Y=0.  937
C     Z=(-SFJP_SFAM_DZ+SFJP_SFAM_DZS)/2 <===                              937
         CALL AGSRESET                                                    939
         AG_X = (-SFJP_SFAM_DXE+SFJP_SFAM_DXZ)/2.                         939
         AG_Y = 0.                                                        939
         AG_Z = (-SFJP_SFAM_DZ+SFJP_SFAM_DZS)/2                           939
         AG_TITLE = 'SFAS'                                                939
         AG_EXNAME = 'POSITION'                                           939
         AG_PARLIST = 'X_Y_Z'                                             939
         CALL AXPOSITION                                                  939
C    *                                                                    939
5621  CALL AGSPOPB                                                        940
      GO TO BLOCK_SFAM, (5620 ,5630)                                      940
C End of Local Procedure                                                  940
C ----------------------------------------------------------------------  941
C ------------ SFAB                                                       941
C Begin Local Procedure SFAB                                              942
C SFAB IS THE BIG VOLUME OF THE ADC BOARD                                 942
5930  CONTINUE                                                            942
         AG_TITLE='SFAB'                                                  942
         AG_MODULE=MODULE                                                 942
         AG_MARK='V00'                                                    942
         CALL AGSPUSH                                                     942
         AG_MODULE=MODULE                                                 942
C    *                                                                    942
C    Geant MATERIAL : G10 ===> inherited arguments used <===              943
         CALL AGSRESET                                                    943
         AG_TITLE = 'G10'                                                 943
         AG_EXNAME = 'MATERIAL'                                           943
         AG_PARLIST = 'NONE'                                              943
         CALL AXMATERIAL                                                  943
C    *                                                                    943
C    Geant ATTRIBUTE : SFAB ===> SEEN=1 COLO=30 <===                      943
         CALL AGSRESET                                                    944
         AG_SEEN = 1                                                      944
         AG_COLO = 30                                                     944
         AG_TITLE = 'SFAB'                                                944
         AG_EXNAME = 'ATTRIBUTE'                                          944
         AG_PARLIST = 'SEEN_COLO'                                         944
         CALL AXATTRIBUTE                                                 944
C    *                                                                    944
C    Geant SHAPE : BOX ===> DX=SFJP_SFAM_DXE/2. DY=SFJP_SFAM_DY/2., DZ=(  944
C    SFJP_SFAM_DZ-SFJP_SFAM_DZS)/2 <===                                   944
         CALL AGSRESET                                                    946
         AG_DX = SFJP_SFAM_DXE/2.                                         946
         AG_DY = SFJP_SFAM_DY/2.                                          946
         AG_DZ = (SFJP_SFAM_DZ-SFJP_SFAM_DZS)/2                           946
         AG_TITLE = 'BOX'                                                 946
         AG_EXNAME = 'SHAPE'                                              946
         AG_PARLIST = 'DX_DY_DZ'                                          946
         CALL AXSHAPE                                                     946
C    *                                                                    946
5931  CALL AGSPOPB                                                        947
      GO TO BLOCK_SFAB, (5930 ,5940)                                      947
C End of Local Procedure                                                  947
C ----------------------------------------------------------------------  948
C ------------ SFAS                                                       948
C Begin Local Procedure SFAS                                              949
C SFAS IS THE SMALL VOLUME OF THE ADC BOARD                               949
5950  CONTINUE                                                            949
         AG_TITLE='SFAS'                                                  949
         AG_MODULE=MODULE                                                 949
         AG_MARK='V00'                                                    949
         CALL AGSPUSH                                                     949
         AG_MODULE=MODULE                                                 949
C    *                                                                    949
C    Geant MATERIAL : G10 ===> inherited arguments used <===              950
         CALL AGSRESET                                                    950
         AG_TITLE = 'G10'                                                 950
         AG_EXNAME = 'MATERIAL'                                           950
         AG_PARLIST = 'NONE'                                              950
         CALL AXMATERIAL                                                  950
C    *                                                                    950
C    Geant ATTRIBUTE : SFAS ===> SEEN=1 COLO=30 <===                      950
         CALL AGSRESET                                                    951
         AG_SEEN = 1                                                      951
         AG_COLO = 30                                                     951
         AG_TITLE = 'SFAS'                                                951
         AG_EXNAME = 'ATTRIBUTE'                                          951
         AG_PARLIST = 'SEEN_COLO'                                         951
         CALL AXATTRIBUTE                                                 951
C    *                                                                    951
C    Geant SHAPE : BOX ===> DX=(SFJP_SFAM_DXZ)/2. DY=(SFJP_SFAM_DY)/2.,   951
C    DZ=(SFJP_SFAM_DZS)/2 <===                                            951
         CALL AGSRESET                                                    953
         AG_DX = (SFJP_SFAM_DXZ)/2.                                       953
         AG_DY = (SFJP_SFAM_DY)/2.                                        953
         AG_DZ = (SFJP_SFAM_DZS)/2                                        953
         AG_TITLE = 'BOX'                                                 953
         AG_EXNAME = 'SHAPE'                                              953
         AG_PARLIST = 'DX_DY_DZ'                                          953
         CALL AXSHAPE                                                     953
C    *                                                                    953
5951  CALL AGSPOPB                                                        954
      GO TO BLOCK_SFAS, (5950 ,5960)                                      954
C End of Local Procedure                                                  954
C ----------------------------------------------------------------------  955
C ------------ SAPP                                                       955
C Begin Local Procedure SAPP                                              956
C SAPP IS THE MOTHER VOLUME OF THE ADC BOARD APPENDICE                    956
5600  CONTINUE                                                            956
         AG_TITLE='SAPP'                                                  956
         AG_MODULE=MODULE                                                 956
         AG_MARK='V00'                                                    956
         CALL AGSPUSH                                                     956
         AG_MODULE=MODULE                                                 956
C    *                                                                    956
C    Geant MATERIAL : AIR ===> inherited arguments used <===              957
         CALL AGSRESET                                                    957
         AG_TITLE = 'AIR'                                                 957
         AG_EXNAME = 'MATERIAL'                                           957
         AG_PARLIST = 'NONE'                                              957
         CALL AXMATERIAL                                                  957
C    *                                                                    957
C    Geant ATTRIBUTE : SAPP ===> SEEN=0 COLO=6 <===                       957
         CALL AGSRESET                                                    958
         AG_SEEN = 0                                                      958
         AG_COLO = 6                                                      958
         AG_TITLE = 'SAPP'                                                958
         AG_EXNAME = 'ATTRIBUTE'                                          958
         AG_PARLIST = 'SEEN_COLO'                                         958
         CALL AXATTRIBUTE                                                 958
C    *                                                                    958
C    Geant SHAPE : BOX ===> DX=(SFJP_SAPP_DXE+2*SFJP_SAPP_DXZ)/2., DY=(S  958
C    FJP_SAPP_DY+2*SFJP_SAPP_DXZ)/2., DZ=SFJP_SAPP_DZ/2. <===             958
         CALL AGSRESET                                                    961
         AG_DX = (SFJP_SAPP_DXE+2*SFJP_SAPP_DXZ)/2.                       961
         AG_DY = (SFJP_SAPP_DY+2*SFJP_SAPP_DXZ)/2.                        961
         AG_DZ = SFJP_SAPP_DZ/2.                                          961
         AG_TITLE = 'BOX'                                                 961
         AG_EXNAME = 'SHAPE'                                              961
         AG_PARLIST = 'DX_DY_DZ'                                          961
         CALL AXSHAPE                                                     961
C    *                                                                    961
C    Create SAPC :                                                        962
C    Call To Local Procedure SAPC                                         962
         ASSIGN 5980 TO BLOCK_SAPC                                        962
         GO TO  5970                                                      962
5980     CONTINUE                                                         962
C    *                                                                    962
C    Geant POSITION : SAPC ===> X=0. Y=0. Z=(-SFJP_SAPP_DZ+SFJP_SAPP_DY)  963
C    /2. <===                                                             963
         CALL AGSRESET                                                    963
         AG_X = 0.                                                        963
         AG_Y = 0.                                                        963
         AG_Z = (-SFJP_SAPP_DZ+SFJP_SAPP_DY)/2.                           963
         AG_TITLE = 'SAPC'                                                963
         AG_EXNAME = 'POSITION'                                           963
         AG_PARLIST = 'X_Y_Z'                                             963
         CALL AXPOSITION                                                  963
C    *                                                                    963
C    Create SAPS :                                                        964
C    Call To Local Procedure SAPS                                         964
         ASSIGN 6000 TO BLOCK_SAPS                                        964
         GO TO  5990                                                      964
6000     CONTINUE                                                         964
C    *                                                                    964
C    Geant POSITION : SAPS ===> X=SFJP_SAPP_DXE/2.+SFJP_SAPP_DXZ/2 Y=0 Z  965
C    =0. <===                                                             965
         CALL AGSRESET                                                    965
         AG_X = SFJP_SAPP_DXE/2.+SFJP_SAPP_DXZ/2                          965
         AG_Y = 0                                                         965
         AG_Z = 0.                                                        965
         AG_TITLE = 'SAPS'                                                965
         AG_EXNAME = 'POSITION'                                           965
         AG_PARLIST = 'X_Y_Z'                                             965
         CALL AXPOSITION                                                  965
C    *                                                                    965
C    Geant POSITION : SAPS ===> X=-(SFJP_SAPP_DXE/2.+SFJP_SAPP_DXZ/2) Y=  965
C    0 Z=0. <===                                                          965
         CALL AGSRESET                                                    966
         AG_X = -(SFJP_SAPP_DXE/2.+SFJP_SAPP_DXZ/2)                       966
         AG_Y = 0                                                         966
         AG_Z = 0.                                                        966
         AG_TITLE = 'SAPS'                                                966
         AG_EXNAME = 'POSITION'                                           966
         AG_PARLIST = 'X_Y_Z'                                             966
         CALL AXPOSITION                                                  966
C    *                                                                    966
C    Create SAPT :                                                        967
C    Call To Local Procedure SAPT                                         967
         ASSIGN 6020 TO BLOCK_SAPT                                        967
         GO TO  6010                                                      967
6020     CONTINUE                                                         967
C    *                                                                    967
C    Geant POSITION : SAPT ===> X=0. Y=(SFJP_SAPP_DY+SFJP_SAPP_DXZ)/2. Z  968
C    =0. <===                                                             968
         CALL AGSRESET                                                    968
         AG_X = 0.                                                        968
         AG_Y = (SFJP_SAPP_DY+SFJP_SAPP_DXZ)/2.                           968
         AG_Z = 0.                                                        968
         AG_TITLE = 'SAPT'                                                968
         AG_EXNAME = 'POSITION'                                           968
         AG_PARLIST = 'X_Y_Z'                                             968
         CALL AXPOSITION                                                  968
C    *                                                                    968
C    Geant POSITION : SAPT ===> X=0. Y=-(SFJP_SAPP_DY+SFJP_SAPP_DXZ)/2.   968
C    Z=0. <===                                                            968
         CALL AGSRESET                                                    969
         AG_X = 0.                                                        969
         AG_Y = -(SFJP_SAPP_DY+SFJP_SAPP_DXZ)/2.                          969
         AG_Z = 0.                                                        969
         AG_TITLE = 'SAPT'                                                969
         AG_EXNAME = 'POSITION'                                           969
         AG_PARLIST = 'X_Y_Z'                                             969
         CALL AXPOSITION                                                  969
C    *                                                                    969
5601  CALL AGSPOPB                                                        970
      GO TO BLOCK_SAPP, (5600 ,5610)                                      970
C End of Local Procedure                                                  970
C ----------------------------------------------------------------------  971
C ------------ SAPC                                                       971
C Begin Local Procedure SAPC                                              972
C SAPC IS THE CORE (EPOXY) OF THE ADC BOARD APPENDICE                     972
5970  CONTINUE                                                            972
         AG_TITLE='SAPC'                                                  972
         AG_MODULE=MODULE                                                 972
         AG_MARK='V00'                                                    972
         CALL AGSPUSH                                                     972
         AG_MODULE=MODULE                                                 972
C    *                                                                    972
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           973
         CALL AGSRESET                                                    973
         AG_TITLE = 'CARBON'                                              973
         AG_EXNAME = 'MATERIAL'                                           973
         AG_PARLIST = 'NONE'                                              973
         CALL AXMATERIAL                                                  973
C    *                                                                    973
C    Geant ATTRIBUTE : SAPC ===> SEEN=1 COLO=1 <===                       973
         CALL AGSRESET                                                    974
         AG_SEEN = 1                                                      974
         AG_COLO = 1                                                      974
         AG_TITLE = 'SAPC'                                                974
         AG_EXNAME = 'ATTRIBUTE'                                          974
         AG_PARLIST = 'SEEN_COLO'                                         974
         CALL AXATTRIBUTE                                                 974
C    *                                                                    974
C    Geant SHAPE : BOX ===> DX=SFJP_SAPP_DXE/2. DY=SFJP_SAPP_DY/2., DZ=S  974
C    FJP_SAPP_DY/2. <===                                                  974
         CALL AGSRESET                                                    976
         AG_DX = SFJP_SAPP_DXE/2.                                         976
         AG_DY = SFJP_SAPP_DY/2.                                          976
         AG_DZ = SFJP_SAPP_DY/2.                                          976
         AG_TITLE = 'BOX'                                                 976
         AG_EXNAME = 'SHAPE'                                              976
         AG_PARLIST = 'DX_DY_DZ'                                          976
         CALL AXSHAPE                                                     976
C    *                                                                    976
5971  CALL AGSPOPB                                                        977
      GO TO BLOCK_SAPC, (5970 ,5980)                                      977
C End of Local Procedure                                                  977
C ----------------------------------------------------------------------  978
C ------------ SAPS                                                       978
C Begin Local Procedure SAPS                                              979
C SAPS IS THE SIDE SHELL (CARBON) OF THE ADC BOARD APPENDICE              979
5990  CONTINUE                                                            979
         AG_TITLE='SAPS'                                                  979
         AG_MODULE=MODULE                                                 979
         AG_MARK='V00'                                                    979
         CALL AGSPUSH                                                     979
         AG_MODULE=MODULE                                                 979
C    *                                                                    979
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           980
         CALL AGSRESET                                                    980
         AG_TITLE = 'CARBON'                                              980
         AG_EXNAME = 'MATERIAL'                                           980
         AG_PARLIST = 'NONE'                                              980
         CALL AXMATERIAL                                                  980
C    *                                                                    980
C    Geant ATTRIBUTE : SAPS ===> SEEN=1 COLO=1 <===                       980
         CALL AGSRESET                                                    981
         AG_SEEN = 1                                                      981
         AG_COLO = 1                                                      981
         AG_TITLE = 'SAPS'                                                981
         AG_EXNAME = 'ATTRIBUTE'                                          981
         AG_PARLIST = 'SEEN_COLO'                                         981
         CALL AXATTRIBUTE                                                 981
C    *                                                                    981
C    Geant SHAPE : BOX ===> DX=SFJP_SAPP_DXZ/2. DY=(SFJP_SAPP_DY+2*SFJP_  981
C    SAPP_DXZ)/2., DZ=SFJP_SAPP_DZ/2. <===                                981
         CALL AGSRESET                                                    983
         AG_DX = SFJP_SAPP_DXZ/2.                                         983
         AG_DY = (SFJP_SAPP_DY+2*SFJP_SAPP_DXZ)/2.                        983
         AG_DZ = SFJP_SAPP_DZ/2.                                          983
         AG_TITLE = 'BOX'                                                 983
         AG_EXNAME = 'SHAPE'                                              983
         AG_PARLIST = 'DX_DY_DZ'                                          983
         CALL AXSHAPE                                                     983
C    *                                                                    983
5991  CALL AGSPOPB                                                        984
      GO TO BLOCK_SAPS, (5990 ,6000)                                      984
C End of Local Procedure                                                  984
C ----------------------------------------------------------------------  985
C ------------ SAPT                                                       985
C Begin Local Procedure SAPT                                              986
C SAPT IS THE TOP-BOTTOM SHELL (CARBON) OF THE ADC BOARD APPENDICE        986
6010  CONTINUE                                                            986
         AG_TITLE='SAPT'                                                  986
         AG_MODULE=MODULE                                                 986
         AG_MARK='V00'                                                    986
         CALL AGSPUSH                                                     986
         AG_MODULE=MODULE                                                 986
C    *                                                                    986
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           987
         CALL AGSRESET                                                    987
         AG_TITLE = 'CARBON'                                              987
         AG_EXNAME = 'MATERIAL'                                           987
         AG_PARLIST = 'NONE'                                              987
         CALL AXMATERIAL                                                  987
C    *                                                                    987
C    Geant ATTRIBUTE : SAPT ===> SEEN=1 COLO=1 <===                       987
         CALL AGSRESET                                                    988
         AG_SEEN = 1                                                      988
         AG_COLO = 1                                                      988
         AG_TITLE = 'SAPT'                                                988
         AG_EXNAME = 'ATTRIBUTE'                                          988
         AG_PARLIST = 'SEEN_COLO'                                         988
         CALL AXATTRIBUTE                                                 988
C    *                                                                    988
C    Geant SHAPE : BOX ===> DX=SFJP_SAPP_DXE/2. DY=SFJP_SAPP_DXZ/2., DZ=  988
C    SFJP_SAPP_DZ/2. <===                                                 988
         CALL AGSRESET                                                    990
         AG_DX = SFJP_SAPP_DXE/2.                                         990
         AG_DY = SFJP_SAPP_DXZ/2.                                         990
         AG_DZ = SFJP_SAPP_DZ/2.                                          990
         AG_TITLE = 'BOX'                                                 990
         AG_EXNAME = 'SHAPE'                                              990
         AG_PARLIST = 'DX_DY_DZ'                                          990
         CALL AXSHAPE                                                     990
C    *                                                                    990
6011  CALL AGSPOPB                                                        991
      GO TO BLOCK_SAPT, (6010 ,6020)                                      991
C End of Local Procedure                                                  991
C ----------------------------------------------------------------------  992
C ------------ SFCO                                                       992
C Begin Local Procedure SFCO                                              993
C SFCO IS THE CONNECTION BOARD (RECTANGULAR WITH HIROSE CONNECTORS)       993
5640  CONTINUE                                                            993
         AG_TITLE='SFCO'                                                  993
         AG_MODULE=MODULE                                                 993
         AG_MARK='V00'                                                    993
         CALL AGSPUSH                                                     993
         AG_MODULE=MODULE                                                 993
C    *                                                                    993
C    Geant MATERIAL : G10 ===> inherited arguments used <===              994
         CALL AGSRESET                                                    994
         AG_TITLE = 'G10'                                                 994
         AG_EXNAME = 'MATERIAL'                                           994
         AG_PARLIST = 'NONE'                                              994
         CALL AXMATERIAL                                                  994
C    *                                                                    994
C    Geant ATTRIBUTE : SFCO ===> SEEN=1 COLO=30 <===                      994
         CALL AGSRESET                                                    995
         AG_SEEN = 1                                                      995
         AG_COLO = 30                                                     995
         AG_TITLE = 'SFCO'                                                995
         AG_EXNAME = 'ATTRIBUTE'                                          995
         AG_PARLIST = 'SEEN_COLO'                                         995
         CALL AXATTRIBUTE                                                 995
C    *                                                                    995
C    Geant SHAPE : BOX ===> DX=SFJP_SFCO_DX/2. DY=SFJP_SFCO_DY/2., DZ=SF  995
C    JP_SFCO_DZ/2 <===                                                    995
         CALL AGSRESET                                                    997
         AG_DX = SFJP_SFCO_DX/2.                                          997
         AG_DY = SFJP_SFCO_DY/2.                                          997
         AG_DZ = SFJP_SFCO_DZ/2                                           997
         AG_TITLE = 'BOX'                                                 997
         AG_EXNAME = 'SHAPE'                                              997
         AG_PARLIST = 'DX_DY_DZ'                                          997
         CALL AXSHAPE                                                     997
C    *                                                                    997
5641  CALL AGSPOPB                                                        998
      GO TO BLOCK_SFCO, (5640 ,5650)                                      998
C End of Local Procedure                                                  998
C ----------------------------------------------------------------------  999
C ------------ SFCM                                                       999
C Begin Local Procedure SFCM                                              1000
C SFCM IS THE MOTHER VOLUME OF THE SECOND CONNECTION BOARD                1000
5660  CONTINUE                                                            1000
         AG_TITLE='SFCM'                                                  1000
         AG_MODULE=MODULE                                                 1000
         AG_MARK='V00'                                                    1000
         CALL AGSPUSH                                                     1000
         AG_MODULE=MODULE                                                 1000
C    *                                                                    1000
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1001
         CALL AGSRESET                                                    1001
         AG_TITLE = 'AIR'                                                 1001
         AG_EXNAME = 'MATERIAL'                                           1001
         AG_PARLIST = 'NONE'                                              1001
         CALL AXMATERIAL                                                  1001
C    *                                                                    1001
C    Geant ATTRIBUTE : SFCM ===> SEEN=0 COLO=6 <===                       1001
         CALL AGSRESET                                                    1002
         AG_SEEN = 0                                                      1002
         AG_COLO = 6                                                      1002
         AG_TITLE = 'SFCM'                                                1002
         AG_EXNAME = 'ATTRIBUTE'                                          1002
         AG_PARLIST = 'SEEN_COLO'                                         1002
         CALL AXATTRIBUTE                                                 1002
C    *                                                                    1002
C    Geant SHAPE : BOX ===> DX=SFJP_SFCM_DX/2. DY=SFJP_SFCO_DY/2., DZ=SF  1002
C    JP_SFCO_DZ/2 <===                                                    1002
         CALL AGSRESET                                                    1004
         AG_DX = SFJP_SFCM_DX/2.                                          1004
         AG_DY = SFJP_SFCO_DY/2.                                          1004
         AG_DZ = SFJP_SFCO_DZ/2                                           1004
         AG_TITLE = 'BOX'                                                 1004
         AG_EXNAME = 'SHAPE'                                              1004
         AG_PARLIST = 'DX_DY_DZ'                                          1004
         CALL AXSHAPE                                                     1004
C    *                                                                    1004
C    Create SFCB :                                                        1005
C    Call To Local Procedure SFCB                                         1005
         ASSIGN 6040 TO BLOCK_SFCB                                        1005
         GO TO  6030                                                      1005
6040     CONTINUE                                                         1005
C    *                                                                    1005
C    Geant POSITION : SFCB ===> X=+0.5 Y=0 Z=0 <===                       1006
         CALL AGSRESET                                                    1006
         AG_X = +0.5                                                      1006
         AG_Y = 0                                                         1006
         AG_Z = 0                                                         1006
         AG_TITLE = 'SFCB'                                                1006
         AG_EXNAME = 'POSITION'                                           1006
         AG_PARLIST = 'X_Y_Z'                                             1006
         CALL AXPOSITION                                                  1006
C    *                                                                    1006
C    Create SFCS :                                                        1007
C    Call To Local Procedure SFCS                                         1007
         ASSIGN 6060 TO BLOCK_SFCS                                        1007
         GO TO  6050                                                      1007
6060     CONTINUE                                                         1007
C    *                                                                    1007
C    Geant POSITION : SFCS ===> X=-1.5 Y=0 Z=-SFJP_SFCO_DZ/2+SFJP_SFCS_D  1008
C    Z/2 <===                                                             1008
         CALL AGSRESET                                                    1008
         AG_X = -1.5                                                      1008
         AG_Y = 0                                                         1008
         AG_Z = -SFJP_SFCO_DZ/2+SFJP_SFCS_DZ/2                            1008
         AG_TITLE = 'SFCS'                                                1008
         AG_EXNAME = 'POSITION'                                           1008
         AG_PARLIST = 'X_Y_Z'                                             1008
         CALL AXPOSITION                                                  1008
C    *                                                                    1008
5661  CALL AGSPOPB                                                        1009
      GO TO BLOCK_SFCM, (5660 ,5670)                                      1009
C End of Local Procedure                                                  1009
C ----------------------------------------------------------------------  1010
C ------------ SFCB                                                       1010
C Begin Local Procedure SFCB                                              1011
C SFCB IS THE BIG PART OF THE SECOND CONNECTION BOARD                     1011
6030  CONTINUE                                                            1011
         AG_TITLE='SFCB'                                                  1011
         AG_MODULE=MODULE                                                 1011
         AG_MARK='V00'                                                    1011
         CALL AGSPUSH                                                     1011
         AG_MODULE=MODULE                                                 1011
C    *                                                                    1011
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1012
         CALL AGSRESET                                                    1012
         AG_TITLE = 'G10'                                                 1012
         AG_EXNAME = 'MATERIAL'                                           1012
         AG_PARLIST = 'NONE'                                              1012
         CALL AXMATERIAL                                                  1012
C    *                                                                    1012
C    Geant ATTRIBUTE : SFCB ===> SEEN=1 COLO=30 <===                      1012
         CALL AGSRESET                                                    1013
         AG_SEEN = 1                                                      1013
         AG_COLO = 30                                                     1013
         AG_TITLE = 'SFCB'                                                1013
         AG_EXNAME = 'ATTRIBUTE'                                          1013
         AG_PARLIST = 'SEEN_COLO'                                         1013
         CALL AXATTRIBUTE                                                 1013
C    *                                                                    1013
C    Geant SHAPE : BOX ===> DX=SFJP_SFCB_DX/2. DY=SFJP_SFCO_DY/2., DZ=SF  1013
C    JP_SFCO_DZ/2 <===                                                    1013
         CALL AGSRESET                                                    1015
         AG_DX = SFJP_SFCB_DX/2.                                          1015
         AG_DY = SFJP_SFCO_DY/2.                                          1015
         AG_DZ = SFJP_SFCO_DZ/2                                           1015
         AG_TITLE = 'BOX'                                                 1015
         AG_EXNAME = 'SHAPE'                                              1015
         AG_PARLIST = 'DX_DY_DZ'                                          1015
         CALL AXSHAPE                                                     1015
C    *                                                                    1015
6031  CALL AGSPOPB                                                        1016
      GO TO BLOCK_SFCB, (6030 ,6040)                                      1016
C End of Local Procedure                                                  1016
C ----------------------------------------------------------------------  1017
C ------------ SFCS                                                       1017
C Begin Local Procedure SFCS                                              1018
C SFCS IS THE BIG PART OF THE SECOND CONNECTION BOARD                     1018
6050  CONTINUE                                                            1018
         AG_TITLE='SFCS'                                                  1018
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
C    Geant ATTRIBUTE : SFCS ===> SEEN=1 COLO=30 <===                      1019
         CALL AGSRESET                                                    1020
         AG_SEEN = 1                                                      1020
         AG_COLO = 30                                                     1020
         AG_TITLE = 'SFCS'                                                1020
         AG_EXNAME = 'ATTRIBUTE'                                          1020
         AG_PARLIST = 'SEEN_COLO'                                         1020
         CALL AXATTRIBUTE                                                 1020
C    *                                                                    1020
C    Geant SHAPE : BOX ===> DX=(SFJP_SFCM_DX-SFJP_SFCB_DX)/2., DY=SFJP_S  1020
C    FCO_DY/2. DZ=SFJP_SFCS_DZ/2 <===                                     1020
         CALL AGSRESET                                                    1022
         AG_DX = (SFJP_SFCM_DX-SFJP_SFCB_DX)/2.                           1022
         AG_DY = SFJP_SFCO_DY/2.                                          1022
         AG_DZ = SFJP_SFCS_DZ/2                                           1022
         AG_TITLE = 'BOX'                                                 1022
         AG_EXNAME = 'SHAPE'                                              1022
         AG_PARLIST = 'DX_DY_DZ'                                          1022
         CALL AXSHAPE                                                     1022
C    *                                                                    1022
6051  CALL AGSPOPB                                                        1023
      GO TO BLOCK_SFCS, (6050 ,6060)                                      1023
C End of Local Procedure                                                  1023
C ----------------------------------------------------------------------  1024
C ------------ SFKF                                                       1024
C Begin Local Procedure SFKF                                              1025
C SFKF IS THE FIRST PART OF THE KAPTON FLEX CIRCUIT                       1025
5680  CONTINUE                                                            1025
         AG_TITLE='SFKF'                                                  1025
         AG_MODULE=MODULE                                                 1025
         AG_MARK='V00'                                                    1025
         CALL AGSPUSH                                                     1025
         AG_MODULE=MODULE                                                 1025
C    *                                                                    1025
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1026
         CALL AGSRESET                                                    1026
         AG_TITLE = 'G10'                                                 1026
         AG_EXNAME = 'MATERIAL'                                           1026
         AG_PARLIST = 'NONE'                                              1026
         CALL AXMATERIAL                                                  1026
C    *                                                                    1026
C    Geant ATTRIBUTE : SFKF ===> SEEN=1 COLO=30 <===                      1026
         CALL AGSRESET                                                    1027
         AG_SEEN = 1                                                      1027
         AG_COLO = 30                                                     1027
         AG_TITLE = 'SFKF'                                                1027
         AG_EXNAME = 'ATTRIBUTE'                                          1027
         AG_PARLIST = 'SEEN_COLO'                                         1027
         CALL AXATTRIBUTE                                                 1027
C    *                                                                    1027
C    Geant SHAPE : BOX ===> DX=SFJP_SFKF_DX/2. DY=SFJP_SFKF_DY/2., DZ=SF  1027
C    JP_SFKF_DZ/2 <===                                                    1027
         CALL AGSRESET                                                    1029
         AG_DX = SFJP_SFKF_DX/2.                                          1029
         AG_DY = SFJP_SFKF_DY/2.                                          1029
         AG_DZ = SFJP_SFKF_DZ/2                                           1029
         AG_TITLE = 'BOX'                                                 1029
         AG_EXNAME = 'SHAPE'                                              1029
         AG_PARLIST = 'DX_DY_DZ'                                          1029
         CALL AXSHAPE                                                     1029
C    *                                                                    1029
5681  CALL AGSPOPB                                                        1030
      GO TO BLOCK_SFKF, (5680 ,5690)                                      1030
C End of Local Procedure                                                  1030
C ----------------------------------------------------------------------  1031
C ------------ SFKS                                                       1031
C Begin Local Procedure SFKS                                              1032
C SFKS IS THE SECOND PART OF THE KAPTON FLEX CIRCUIT                      1032
5700  CONTINUE                                                            1032
         AG_TITLE='SFKS'                                                  1032
         AG_MODULE=MODULE                                                 1032
         AG_MARK='V00'                                                    1032
         CALL AGSPUSH                                                     1032
         AG_MODULE=MODULE                                                 1032
C    *                                                                    1032
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1033
         CALL AGSRESET                                                    1033
         AG_TITLE = 'G10'                                                 1033
         AG_EXNAME = 'MATERIAL'                                           1033
         AG_PARLIST = 'NONE'                                              1033
         CALL AXMATERIAL                                                  1033
C    *                                                                    1033
C    Geant ATTRIBUTE : SFKS ===> SEEN=1 COLO=30 <===                      1033
         CALL AGSRESET                                                    1034
         AG_SEEN = 1                                                      1034
         AG_COLO = 30                                                     1034
         AG_TITLE = 'SFKS'                                                1034
         AG_EXNAME = 'ATTRIBUTE'                                          1034
         AG_PARLIST = 'SEEN_COLO'                                         1034
         CALL AXATTRIBUTE                                                 1034
C    *                                                                    1034
C    Geant SHAPE : BOX ===> DX=SFJP_SFKS_DX/2. DY=SFJP_SFKF_DY/2., DZ=SF  1034
C    JP_SFKF_DZ/2 <===                                                    1034
         CALL AGSRESET                                                    1036
         AG_DX = SFJP_SFKS_DX/2.                                          1036
         AG_DY = SFJP_SFKF_DY/2.                                          1036
         AG_DZ = SFJP_SFKF_DZ/2                                           1036
         AG_TITLE = 'BOX'                                                 1036
         AG_EXNAME = 'SHAPE'                                              1036
         AG_PARLIST = 'DX_DY_DZ'                                          1036
         CALL AXSHAPE                                                     1036
C    *                                                                    1036
5701  CALL AGSPOPB                                                        1037
      GO TO BLOCK_SFKS, (5700 ,5710)                                      1037
C End of Local Procedure                                                  1037
C ----------------------------------------------------------------------  1038
C ------------ SFPR                                                       1038
C Begin Local Procedure SFPR                                              1040
C SFPR IS THE LADDER END INSIDE MECHANICAL PART (PRISM WITH G10 WITH, HA  1040
C LF DENSITY)                                                             1040
5720  CONTINUE                                                            1040
         AG_TITLE='SFPR'                                                  1040
         AG_MODULE=MODULE                                                 1040
         AG_MARK='V00'                                                    1040
         CALL AGSPUSH                                                     1040
         AG_MODULE=MODULE                                                 1040
C    *                                                                    1040
C    Geant MATERIAL : G5 ===> inherited arguments used <===               1041
         CALL AGSRESET                                                    1041
         AG_TITLE = 'G5'                                                  1041
         AG_EXNAME = 'MATERIAL'                                           1041
         AG_PARLIST = 'NONE'                                              1041
         CALL AXMATERIAL                                                  1041
C    *                                                                    1041
C    Geant ATTRIBUTE : SFPR ===> SEEN=1 COLO=4 <===                       1041
         CALL AGSRESET                                                    1042
         AG_SEEN = 1                                                      1042
         AG_COLO = 4                                                      1042
         AG_TITLE = 'SFPR'                                                1042
         AG_EXNAME = 'ATTRIBUTE'                                          1042
         AG_PARLIST = 'SEEN_COLO'                                         1042
         CALL AXATTRIBUTE                                                 1042
C    *                                                                    1042
C    Geant SHAPE : PGON ===> PHI1=-126 DPHI=72 NPDIV=1 NZ=2, ZI={0,3.7}   1042
C    RMN={0,0} RMX={3.48,3.48} <===                                       1042
         CALL AGSRESET                                                    1044
         AG_PHI1 = -126                                                   1044
         AG_DPHI = 72                                                     1044
         AG_NPDIV = 1                                                     1044
         AG_NZ = 2                                                        1044
         AG_ZI(1)=0                                                       1044
         AG_ZI(2)=3.7                                                     1044
         AG_RMN(1)=0                                                      1044
         AG_RMN(2)=0                                                      1044
         AG_RMX(1)=3.48                                                   1044
         AG_RMX(2)=3.48                                                   1044
         AG_TITLE = 'PGON'                                                1044
         AG_EXNAME = 'SHAPE'                                              1044
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1044
         CALL AXSHAPE                                                     1044
C    *                                                                    1044
5721  CALL AGSPOPB                                                        1045
      GO TO BLOCK_SFPR, (5720 ,5730)                                      1045
C End of Local Procedure                                                  1045
C ----------------------------------------------------------------------  1046
C ------------ SFPB                                                       1046
C Begin Local Procedure SFPB                                              1047
C SFPB IS THE LADDER END OUTSIDE MECHANICAL PART (RECTANGLE WITH G10)     1047
5740  CONTINUE                                                            1047
         AG_TITLE='SFPB'                                                  1047
         AG_MODULE=MODULE                                                 1047
         AG_MARK='V00'                                                    1047
         CALL AGSPUSH                                                     1047
         AG_MODULE=MODULE                                                 1047
C    *                                                                    1047
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1048
         CALL AGSRESET                                                    1048
         AG_TITLE = 'G10'                                                 1048
         AG_EXNAME = 'MATERIAL'                                           1048
         AG_PARLIST = 'NONE'                                              1048
         CALL AXMATERIAL                                                  1048
C    *                                                                    1048
C    Geant ATTRIBUTE : SFPB ===> SEEN=1 COLO=4 <===                       1048
         CALL AGSRESET                                                    1049
         AG_SEEN = 1                                                      1049
         AG_COLO = 4                                                      1049
         AG_TITLE = 'SFPB'                                                1049
         AG_EXNAME = 'ATTRIBUTE'                                          1049
         AG_PARLIST = 'SEEN_COLO'                                         1049
         CALL AXATTRIBUTE                                                 1049
C    *                                                                    1049
C    Geant SHAPE : BOX ===> DX=SFJP_SFPB_DX/2. DY=SFJP_SFPB_DY/2., DZ=SF  1049
C    JP_SFPBDZ/2. <===                                                    1049
         CALL AGSRESET                                                    1051
         AG_DX = SFJP_SFPB_DX/2.                                          1051
         AG_DY = SFJP_SFPB_DY/2.                                          1051
         AG_DZ = SFJP_SFPBDZ/2.                                           1051
         AG_TITLE = 'BOX'                                                 1051
         AG_EXNAME = 'SHAPE'                                              1051
         AG_PARLIST = 'DX_DY_DZ'                                          1051
         CALL AXSHAPE                                                     1051
C    *                                                                    1051
5741  CALL AGSPOPB                                                        1052
      GO TO BLOCK_SFPB, (5740 ,5750)                                      1052
C End of Local Procedure                                                  1052
C ----------------------------------------------------------------------  1053
C ------------ SSBS                                                       1053
C Begin Local Procedure SSBS                                              1055
C SSBS IS THE SMALL PART OF THE ALUMINIUM PLATE LINKING THE LADDER TO, T  1055
C HE SECTOR                                                               1055
5760  CONTINUE                                                            1055
         AG_TITLE='SSBS'                                                  1055
         AG_MODULE=MODULE                                                 1055
         AG_MARK='V00'                                                    1055
         CALL AGSPUSH                                                     1055
         AG_MODULE=MODULE                                                 1055
C    *                                                                    1055
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1056
         CALL AGSRESET                                                    1056
         AG_TITLE = 'ALUMINIUM'                                           1056
         AG_EXNAME = 'MATERIAL'                                           1056
         AG_PARLIST = 'NONE'                                              1056
         CALL AXMATERIAL                                                  1056
C    *                                                                    1056
C    Geant ATTRIBUTE : SSBS ===> SEEN=1 COLO=1 <===                       1056
         CALL AGSRESET                                                    1057
         AG_SEEN = 1                                                      1057
         AG_COLO = 1                                                      1057
         AG_TITLE = 'SSBS'                                                1057
         AG_EXNAME = 'ATTRIBUTE'                                          1057
         AG_PARLIST = 'SEEN_COLO'                                         1057
         CALL AXATTRIBUTE                                                 1057
C    *                                                                    1057
C    Geant SHAPE : BOX ===> DX=SFJP_SSBS_DX/2. DY=SFJP_SSBS_DY/2., DZ=SF  1057
C    JP_SSBS_DX/2. <===                                                   1057
         CALL AGSRESET                                                    1059
         AG_DX = SFJP_SSBS_DX/2.                                          1059
         AG_DY = SFJP_SSBS_DY/2.                                          1059
         AG_DZ = SFJP_SSBS_DX/2.                                          1059
         AG_TITLE = 'BOX'                                                 1059
         AG_EXNAME = 'SHAPE'                                              1059
         AG_PARLIST = 'DX_DY_DZ'                                          1059
         CALL AXSHAPE                                                     1059
C    *                                                                    1059
5761  CALL AGSPOPB                                                        1060
      GO TO BLOCK_SSBS, (5760 ,5770)                                      1060
C End of Local Procedure                                                  1060
C ----------------------------------------------------------------------  1061
C ------------ SSBB                                                       1061
C Begin Local Procedure SSBB                                              1063
C SSBB IS THE BIG PART OF THE ALUMINIUM PLATE LINKING THE LADDER TO THE,  1063
C  SECTOR                                                                 1063
5780  CONTINUE                                                            1063
         AG_TITLE='SSBB'                                                  1063
         AG_MODULE=MODULE                                                 1063
         AG_MARK='V00'                                                    1063
         CALL AGSPUSH                                                     1063
         AG_MODULE=MODULE                                                 1063
C    *                                                                    1063
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1064
         CALL AGSRESET                                                    1064
         AG_TITLE = 'ALUMINIUM'                                           1064
         AG_EXNAME = 'MATERIAL'                                           1064
         AG_PARLIST = 'NONE'                                              1064
         CALL AXMATERIAL                                                  1064
C    *                                                                    1064
C    Geant ATTRIBUTE : SSBB ===> SEEN=1 COLO=1 <===                       1064
         CALL AGSRESET                                                    1065
         AG_SEEN = 1                                                      1065
         AG_COLO = 1                                                      1065
         AG_TITLE = 'SSBB'                                                1065
         AG_EXNAME = 'ATTRIBUTE'                                          1065
         AG_PARLIST = 'SEEN_COLO'                                         1065
         CALL AXATTRIBUTE                                                 1065
C    *                                                                    1065
C    Geant SHAPE : BOX ===> DX=SFJP_SSBB_DX/2. DY=SFJP_SSBS_DY/2., DZ=SF  1065
C    JP_SSBB_DZ/2. <===                                                   1065
         CALL AGSRESET                                                    1067
         AG_DX = SFJP_SSBB_DX/2.                                          1067
         AG_DY = SFJP_SSBS_DY/2.                                          1067
         AG_DZ = SFJP_SSBB_DZ/2.                                          1067
         AG_TITLE = 'BOX'                                                 1067
         AG_EXNAME = 'SHAPE'                                              1067
         AG_PARLIST = 'DX_DY_DZ'                                          1067
         CALL AXSHAPE                                                     1067
C    *                                                                    1067
5781  CALL AGSPOPB                                                        1068
      GO TO BLOCK_SSBB, (5780 ,5790)                                      1068
C End of Local Procedure                                                  1068
C ----------------------------------------------------------------------  1069
C ------------ SSST                                                       1069
C Begin Local Procedure SSST                                              1070
C SSST IS THE TOP OF THE SMALL SECTOR                                     1070
5090  CONTINUE                                                            1070
         AG_TITLE='SSST'                                                  1070
         AG_MODULE=MODULE                                                 1070
         AG_MARK='V00'                                                    1070
         CALL AGSPUSH                                                     1070
         AG_MODULE=MODULE                                                 1070
C    *                                                                    1070
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1071
         CALL AGSRESET                                                    1071
         AG_TITLE = 'ALUMINIUM'                                           1071
         AG_EXNAME = 'MATERIAL'                                           1071
         AG_PARLIST = 'NONE'                                              1071
         CALL AXMATERIAL                                                  1071
C    *                                                                    1071
C    Geant ATTRIBUTE : SSST ===> SEEN=1 COLO=1 <===                       1071
         CALL AGSRESET                                                    1072
         AG_SEEN = 1                                                      1072
         AG_COLO = 1                                                      1072
         AG_TITLE = 'SSST'                                                1072
         AG_EXNAME = 'ATTRIBUTE'                                          1072
         AG_PARLIST = 'SEEN_COLO'                                         1072
         CALL AXATTRIBUTE                                                 1072
C    *                                                                    1072
C    Geant SHAPE : TUBS ===> RMIN=SFJP_SSST_RMIN RMAX=SFJP_SSST_RMAX, PH  1072
C    I1=SFJP_ALPHAZH PHI2=(180.-SFJP_ALPHAZH), DZ=SFJP_SSST_WIDTH/2. <==  1072
C    =                                                                    1072
         CALL AGSRESET                                                    1075
         AG_RMIN = SFJP_SSST_RMIN                                         1075
         AG_RMAX = SFJP_SSST_RMAX                                         1075
         AG_PHI1 = SFJP_ALPHAZH                                           1075
         AG_PHI2 = (180.-SFJP_ALPHAZH)                                    1075
         AG_DZ = SFJP_SSST_WIDTH/2.                                       1075
         AG_TITLE = 'TUBS'                                                1075
         AG_EXNAME = 'SHAPE'                                              1075
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1075
         CALL AXSHAPE                                                     1075
C    *                                                                    1075
5091  CALL AGSPOPB                                                        1076
      GO TO BLOCK_SSST, (5090 ,5100)                                      1076
C End of Local Procedure                                                  1076
C ----------------------------------------------------------------------  1077
C ------------ SSSS                                                       1077
C Begin Local Procedure SSSS                                              1078
C SSSS IS THE SIDE OF THE SMALL SECTOR                                    1078
5110  CONTINUE                                                            1078
         AG_TITLE='SSSS'                                                  1078
         AG_MODULE=MODULE                                                 1078
         AG_MARK='V00'                                                    1078
         CALL AGSPUSH                                                     1078
         AG_MODULE=MODULE                                                 1078
C    *                                                                    1078
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1079
         CALL AGSRESET                                                    1079
         AG_TITLE = 'ALUMINIUM'                                           1079
         AG_EXNAME = 'MATERIAL'                                           1079
         AG_PARLIST = 'NONE'                                              1079
         CALL AXMATERIAL                                                  1079
C    *                                                                    1079
C    Geant ATTRIBUTE : SSSS ===> SEEN=1 COLO=1 <===                       1079
         CALL AGSRESET                                                    1080
         AG_SEEN = 1                                                      1080
         AG_COLO = 1                                                      1080
         AG_TITLE = 'SSSS'                                                1080
         AG_EXNAME = 'ATTRIBUTE'                                          1080
         AG_PARLIST = 'SEEN_COLO'                                         1080
         CALL AXATTRIBUTE                                                 1080
C    *                                                                    1080
C    Geant SHAPE : TUBS ===> RMIN=SFJP_SSSS_RMIN RMAX=SFJP_SSST_RMIN, PH  1080
C    I1=SFJP_ALPHAZH PHI2=(180.-SFJP_ALPHAZH), DZ=SFJP_SSSS_WIDTH/2. <==  1080
C    =                                                                    1080
         CALL AGSRESET                                                    1083
         AG_RMIN = SFJP_SSSS_RMIN                                         1083
         AG_RMAX = SFJP_SSST_RMIN                                         1083
         AG_PHI1 = SFJP_ALPHAZH                                           1083
         AG_PHI2 = (180.-SFJP_ALPHAZH)                                    1083
         AG_DZ = SFJP_SSSS_WIDTH/2.                                       1083
         AG_TITLE = 'TUBS'                                                1083
         AG_EXNAME = 'SHAPE'                                              1083
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1083
         CALL AXSHAPE                                                     1083
C    *                                                                    1083
5111  CALL AGSPOPB                                                        1084
      GO TO BLOCK_SSSS, (5110 ,5120)                                      1084
C End of Local Procedure                                                  1084
C ----------------------------------------------------------------------  1085
C ------------ SSRT                                                       1085
C Begin Local Procedure SSRT                                              1086
C SSRT IS THE TOP OF THE SIDE RIB                                         1086
5130  CONTINUE                                                            1086
         AG_TITLE='SSRT'                                                  1086
         AG_MODULE=MODULE                                                 1086
         AG_MARK='V00'                                                    1086
         CALL AGSPUSH                                                     1086
         AG_MODULE=MODULE                                                 1086
C    *                                                                    1086
C    Check CONFIGB.LE.5                                                   1087
         IF (CONFIGB.LE.5) THEN                                           1087
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1088
         CALL AGSRESET                                                    1088
         AG_TITLE = 'ALUMINIUM'                                           1088
         AG_EXNAME = 'MATERIAL'                                           1088
         AG_PARLIST = 'NONE'                                              1088
         CALL AXMATERIAL                                                  1088
C    *                                                                    1088
         ELSE                                                             1088
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           1090
         CALL AGSRESET                                                    1090
         AG_TITLE = 'CARBON'                                              1090
         AG_EXNAME = 'MATERIAL'                                           1090
         AG_PARLIST = 'NONE'                                              1090
         CALL AXMATERIAL                                                  1090
C    *                                                                    1090
         ENDIF                                                            1090
C    Geant ATTRIBUTE : SSRT ===> SEEN=1 COLO=1 <===                       1092
         CALL AGSRESET                                                    1092
         AG_SEEN = 1                                                      1092
         AG_COLO = 1                                                      1092
         AG_TITLE = 'SSRT'                                                1092
         AG_EXNAME = 'ATTRIBUTE'                                          1092
         AG_PARLIST = 'SEEN_COLO'                                         1092
         CALL AXATTRIBUTE                                                 1092
C    *                                                                    1092
C    Geant SHAPE : TUBS ===> RMIN=SFJP_SSST_RMIN RMAX=SFJP_SSST_RMAX, PH  1092
C    I1=-SFJP_ALPHAZH PHI2=+SFJP_ALPHAZH, DZ=SFJP_SSST_WIDTH/2. <===      1092
         CALL AGSRESET                                                    1095
         AG_RMIN = SFJP_SSST_RMIN                                         1095
         AG_RMAX = SFJP_SSST_RMAX                                         1095
         AG_PHI1 = -SFJP_ALPHAZH                                          1095
         AG_PHI2 = +SFJP_ALPHAZH                                          1095
         AG_DZ = SFJP_SSST_WIDTH/2.                                       1095
         AG_TITLE = 'TUBS'                                                1095
         AG_EXNAME = 'SHAPE'                                              1095
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1095
         CALL AXSHAPE                                                     1095
C    *                                                                    1095
5131  CALL AGSPOPB                                                        1096
      GO TO BLOCK_SSRT, (5130 ,5140)                                      1096
C End of Local Procedure                                                  1096
C ----------------------------------------------------------------------  1097
C ------------ SSRS                                                       1097
C Begin Local Procedure SSRS                                              1098
C SSRS IS THE SIDE OF THE SMALL RIB                                       1098
5150  CONTINUE                                                            1098
         AG_TITLE='SSRS'                                                  1098
         AG_MODULE=MODULE                                                 1098
         AG_MARK='V00'                                                    1098
         CALL AGSPUSH                                                     1098
         AG_MODULE=MODULE                                                 1098
C    *                                                                    1098
C    Check CONFIGB.LE.5                                                   1099
         IF (CONFIGB.LE.5) THEN                                           1099
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1100
         CALL AGSRESET                                                    1100
         AG_TITLE = 'ALUMINIUM'                                           1100
         AG_EXNAME = 'MATERIAL'                                           1100
         AG_PARLIST = 'NONE'                                              1100
         CALL AXMATERIAL                                                  1100
C    *                                                                    1100
         ELSE                                                             1100
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           1102
         CALL AGSRESET                                                    1102
         AG_TITLE = 'CARBON'                                              1102
         AG_EXNAME = 'MATERIAL'                                           1102
         AG_PARLIST = 'NONE'                                              1102
         CALL AXMATERIAL                                                  1102
C    *                                                                    1102
         ENDIF                                                            1102
C    Geant ATTRIBUTE : SSRS ===> SEEN=1 COLO=1 <===                       1104
         CALL AGSRESET                                                    1104
         AG_SEEN = 1                                                      1104
         AG_COLO = 1                                                      1104
         AG_TITLE = 'SSRS'                                                1104
         AG_EXNAME = 'ATTRIBUTE'                                          1104
         AG_PARLIST = 'SEEN_COLO'                                         1104
         CALL AXATTRIBUTE                                                 1104
C    *                                                                    1104
C    Geant SHAPE : TUBS ===> RMIN=SFJP_SSRS_RMIN RMAX=SFJP_SSST_RMIN, PH  1104
C    I1=-SFJP_ALPHAZH PHI2=+SFJP_ALPHAZH, DZ=SFJP_SSSS_WIDTH/2. <===      1104
         CALL AGSRESET                                                    1107
         AG_RMIN = SFJP_SSRS_RMIN                                         1107
         AG_RMAX = SFJP_SSST_RMIN                                         1107
         AG_PHI1 = -SFJP_ALPHAZH                                          1107
         AG_PHI2 = +SFJP_ALPHAZH                                          1107
         AG_DZ = SFJP_SSSS_WIDTH/2.                                       1107
         AG_TITLE = 'TUBS'                                                1107
         AG_EXNAME = 'SHAPE'                                              1107
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1107
         CALL AXSHAPE                                                     1107
C    *                                                                    1107
5151  CALL AGSPOPB                                                        1108
      GO TO BLOCK_SSRS, (5150 ,5160)                                      1108
C End of Local Procedure                                                  1108
C ----------------------------------------------------------------------  1109
C ------------ SSLB                                                       1109
C Begin Local Procedure SSLB                                              1110
C SSLB IS THE LINKING (SECTOR TO THE CONE) BOX                            1110
5170  CONTINUE                                                            1110
         AG_TITLE='SSLB'                                                  1110
         AG_MODULE=MODULE                                                 1110
         AG_MARK='V00'                                                    1110
         CALL AGSPUSH                                                     1110
         AG_MODULE=MODULE                                                 1110
C    *                                                                    1110
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1111
         CALL AGSRESET                                                    1111
         AG_TITLE = 'AIR'                                                 1111
         AG_EXNAME = 'MATERIAL'                                           1111
         AG_PARLIST = 'NONE'                                              1111
         CALL AXMATERIAL                                                  1111
C    *                                                                    1111
C    Geant ATTRIBUTE : SSLB ===> SEEN=0 COLO=6 <===                       1111
         CALL AGSRESET                                                    1112
         AG_SEEN = 0                                                      1112
         AG_COLO = 6                                                      1112
         AG_TITLE = 'SSLB'                                                1112
         AG_EXNAME = 'ATTRIBUTE'                                          1112
         AG_PARLIST = 'SEEN_COLO'                                         1112
         CALL AXATTRIBUTE                                                 1112
C    *                                                                    1112
C    Geant SHAPE : BOX ===> DX=SFJP_SSLB_DX/2. DY=SFJP_SSLB_DY/2., DZ=SF  1112
C    JP_SSLB_DZ/2. <===                                                   1112
         CALL AGSRESET                                                    1114
         AG_DX = SFJP_SSLB_DX/2.                                          1114
         AG_DY = SFJP_SSLB_DY/2.                                          1114
         AG_DZ = SFJP_SSLB_DZ/2.                                          1114
         AG_TITLE = 'BOX'                                                 1114
         AG_EXNAME = 'SHAPE'                                              1114
         AG_PARLIST = 'DX_DY_DZ'                                          1114
         CALL AXSHAPE                                                     1114
C    *                                                                    1114
C    Create SBCH :                                                        1115
C    Call To Local Procedure SBCH                                         1115
         ASSIGN 6080 TO BLOCK_SBCH                                        1115
         GO TO  6070                                                      1115
6080     CONTINUE                                                         1115
C    *                                                                    1115
C    Geant POSITION : SBCH ===> X=0 Y=0 Z=SFJP_SSLB_DZ/2.-SFJP_SSCR_THD/  1116
C    2. <===                                                              1116
         CALL AGSRESET                                                    1116
         AG_X = 0                                                         1116
         AG_Y = 0                                                         1116
         AG_Z = SFJP_SSLB_DZ/2.-SFJP_SSCR_THD/2.                          1116
         AG_TITLE = 'SBCH'                                                1116
         AG_EXNAME = 'POSITION'                                           1116
         AG_PARLIST = 'X_Y_Z'                                             1116
         CALL AXPOSITION                                                  1116
C    *                                                                    1116
C    Create SBCV :                                                        1117
C    Call To Local Procedure SBCV                                         1117
         ASSIGN 6100 TO BLOCK_SBCV                                        1117
         GO TO  6090                                                      1117
6100     CONTINUE                                                         1117
C    *                                                                    1117
C    Geant POSITION : SBCV ===> X=0 Y=(SFJP_SSCR_WD+SFJP_SSCR_VL)/2 Z=SF  1119
C    JP_SSLB_DZ/2.-SFJP_SSCR_THD/2. <===                                  1119
         CALL AGSRESET                                                    1119
         AG_X = 0                                                         1119
         AG_Y = (SFJP_SSCR_WD+SFJP_SSCR_VL)/2                             1119
         AG_Z = SFJP_SSLB_DZ/2.-SFJP_SSCR_THD/2.                          1119
         AG_TITLE = 'SBCV'                                                1119
         AG_EXNAME = 'POSITION'                                           1119
         AG_PARLIST = 'X_Y_Z'                                             1119
         CALL AXPOSITION                                                  1119
C    *                                                                    1119
C    Geant POSITION : SBCV ===> X=0 Y=-(SFJP_SSCR_WD+SFJP_SSCR_VL)/2 Z=S  1119
C    FJP_SSLB_DZ/2.-SFJP_SSCR_THD/2. <===                                 1119
         CALL AGSRESET                                                    1121
         AG_X = 0                                                         1121
         AG_Y = -(SFJP_SSCR_WD+SFJP_SSCR_VL)/2                            1121
         AG_Z = SFJP_SSLB_DZ/2.-SFJP_SSCR_THD/2.                          1121
         AG_TITLE = 'SBCV'                                                1121
         AG_EXNAME = 'POSITION'                                           1121
         AG_PARLIST = 'X_Y_Z'                                             1121
         CALL AXPOSITION                                                  1121
C    *                                                                    1121
C    Create SBFH :                                                        1122
C    Call To Local Procedure SBFH                                         1122
         ASSIGN 6120 TO BLOCK_SBFH                                        1122
         GO TO  6110                                                      1122
6120     CONTINUE                                                         1122
C    *                                                                    1122
C    Geant POSITION : SBFH ===> X=0 Y=0 Z=SFJP_SSLB_DZ/2.-SFJP_SSCR_THD-  1123
C    SFJP_SSCR_THA/2. <===                                                1123
         CALL AGSRESET                                                    1123
         AG_X = 0                                                         1123
         AG_Y = 0                                                         1123
         AG_Z = SFJP_SSLB_DZ/2.-SFJP_SSCR_THD-SFJP_SSCR_THA/2.            1123
         AG_TITLE = 'SBFH'                                                1123
         AG_EXNAME = 'POSITION'                                           1123
         AG_PARLIST = 'X_Y_Z'                                             1123
         CALL AXPOSITION                                                  1123
C    *                                                                    1123
C    Create SBFV :                                                        1124
C    Call To Local Procedure SBFV                                         1124
         ASSIGN 6140 TO BLOCK_SBFV                                        1124
         GO TO  6130                                                      1124
6140     CONTINUE                                                         1124
C    *                                                                    1124
C    Geant POSITION : SBFV ===> X=0 Y=(SFJP_SSCR_WD+SFJP_SSCR_VL)/2 Z=SF  1126
C    JP_SSLB_DZ/2.-SFJP_SSCR_THD-SFJP_SSCR_THA/2. <===                    1126
         CALL AGSRESET                                                    1126
         AG_X = 0                                                         1126
         AG_Y = (SFJP_SSCR_WD+SFJP_SSCR_VL)/2                             1126
         AG_Z = SFJP_SSLB_DZ/2.-SFJP_SSCR_THD-SFJP_SSCR_THA/2.            1126
         AG_TITLE = 'SBFV'                                                1126
         AG_EXNAME = 'POSITION'                                           1126
         AG_PARLIST = 'X_Y_Z'                                             1126
         CALL AXPOSITION                                                  1126
C    *                                                                    1126
C    Geant POSITION : SBFV ===> X=0 Y=-(SFJP_SSCR_WD+SFJP_SSCR_VL)/2 Z=S  1126
C    FJP_SSLB_DZ/2.-SFJP_SSCR_THD-SFJP_SSCR_THA/2. <===                   1126
         CALL AGSRESET                                                    1128
         AG_X = 0                                                         1128
         AG_Y = -(SFJP_SSCR_WD+SFJP_SSCR_VL)/2                            1128
         AG_Z = SFJP_SSLB_DZ/2.-SFJP_SSCR_THD-SFJP_SSCR_THA/2.            1128
         AG_TITLE = 'SBFV'                                                1128
         AG_EXNAME = 'POSITION'                                           1128
         AG_PARLIST = 'X_Y_Z'                                             1128
         CALL AXPOSITION                                                  1128
C    *                                                                    1128
5171  CALL AGSPOPB                                                        1129
      GO TO BLOCK_SSLB, (5170 ,5180)                                      1129
C End of Local Procedure                                                  1129
C ----------------------------------------------------------------------  1130
C ------------ SBCH                                                       1130
C Begin Local Procedure SBCH                                              1131
C SBCH IS THE HORIZONTAL BRANCH OF THE DELRIN CROSS                       1131
6070  CONTINUE                                                            1131
         AG_TITLE='SBCH'                                                  1131
         AG_MODULE=MODULE                                                 1131
         AG_MARK='V00'                                                    1131
         CALL AGSPUSH                                                     1131
         AG_MODULE=MODULE                                                 1131
C    *                                                                    1131
C    Geant MATERIAL : DELRIN ===> inherited arguments used <===           1132
         CALL AGSRESET                                                    1132
         AG_TITLE = 'DELRIN'                                              1132
         AG_EXNAME = 'MATERIAL'                                           1132
         AG_PARLIST = 'NONE'                                              1132
         CALL AXMATERIAL                                                  1132
C    *                                                                    1132
C    Geant ATTRIBUTE : SBCH ===> SEEN=1 COLO=2 <===                       1132
         CALL AGSRESET                                                    1133
         AG_SEEN = 1                                                      1133
         AG_COLO = 2                                                      1133
         AG_TITLE = 'SBCH'                                                1133
         AG_EXNAME = 'ATTRIBUTE'                                          1133
         AG_PARLIST = 'SEEN_COLO'                                         1133
         CALL AXATTRIBUTE                                                 1133
C    *                                                                    1133
C    Geant SHAPE : BOX ===> DX=SFJP_SSCR_HL/2. DY=SFJP_SSCR_WD/2., DZ=SF  1133
C    JP_SSCR_THD/2. <===                                                  1133
         CALL AGSRESET                                                    1135
         AG_DX = SFJP_SSCR_HL/2.                                          1135
         AG_DY = SFJP_SSCR_WD/2.                                          1135
         AG_DZ = SFJP_SSCR_THD/2.                                         1135
         AG_TITLE = 'BOX'                                                 1135
         AG_EXNAME = 'SHAPE'                                              1135
         AG_PARLIST = 'DX_DY_DZ'                                          1135
         CALL AXSHAPE                                                     1135
C    *                                                                    1135
6071  CALL AGSPOPB                                                        1136
      GO TO BLOCK_SBCH, (6070 ,6080)                                      1136
C End of Local Procedure                                                  1136
C ----------------------------------------------------------------------  1137
C ------------ SBFH                                                       1137
C Begin Local Procedure SBFH                                              1138
C SBFH IS THE HORIZONTAL BRANCH OF THE ALUMIMIUM CROSS                    1138
6110  CONTINUE                                                            1138
         AG_TITLE='SBFH'                                                  1138
         AG_MODULE=MODULE                                                 1138
         AG_MARK='V00'                                                    1138
         CALL AGSPUSH                                                     1138
         AG_MODULE=MODULE                                                 1138
C    *                                                                    1138
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1139
         CALL AGSRESET                                                    1139
         AG_TITLE = 'ALUMINIUM'                                           1139
         AG_EXNAME = 'MATERIAL'                                           1139
         AG_PARLIST = 'NONE'                                              1139
         CALL AXMATERIAL                                                  1139
C    *                                                                    1139
C    Geant ATTRIBUTE : SBFH ===> SEEN=1 COLO=1 <===                       1139
         CALL AGSRESET                                                    1140
         AG_SEEN = 1                                                      1140
         AG_COLO = 1                                                      1140
         AG_TITLE = 'SBFH'                                                1140
         AG_EXNAME = 'ATTRIBUTE'                                          1140
         AG_PARLIST = 'SEEN_COLO'                                         1140
         CALL AXATTRIBUTE                                                 1140
C    *                                                                    1140
C    Geant SHAPE : BOX ===> DX=SFJP_SSCR_HL/2. DY=SFJP_SSCR_WD/2., DZ=SF  1140
C    JP_SSCR_THA/2. <===                                                  1140
         CALL AGSRESET                                                    1142
         AG_DX = SFJP_SSCR_HL/2.                                          1142
         AG_DY = SFJP_SSCR_WD/2.                                          1142
         AG_DZ = SFJP_SSCR_THA/2.                                         1142
         AG_TITLE = 'BOX'                                                 1142
         AG_EXNAME = 'SHAPE'                                              1142
         AG_PARLIST = 'DX_DY_DZ'                                          1142
         CALL AXSHAPE                                                     1142
C    *                                                                    1142
6111  CALL AGSPOPB                                                        1143
      GO TO BLOCK_SBFH, (6110 ,6120)                                      1143
C End of Local Procedure                                                  1143
C ----------------------------------------------------------------------  1144
C ------------ SBCV                                                       1144
C Begin Local Procedure SBCV                                              1145
C SBCV IS THE VERTICAL BRANCH OF THE DELRIN CROSS                         1145
6090  CONTINUE                                                            1145
         AG_TITLE='SBCV'                                                  1145
         AG_MODULE=MODULE                                                 1145
         AG_MARK='V00'                                                    1145
         CALL AGSPUSH                                                     1145
         AG_MODULE=MODULE                                                 1145
C    *                                                                    1145
C    Geant MATERIAL : DELRIN ===> inherited arguments used <===           1146
         CALL AGSRESET                                                    1146
         AG_TITLE = 'DELRIN'                                              1146
         AG_EXNAME = 'MATERIAL'                                           1146
         AG_PARLIST = 'NONE'                                              1146
         CALL AXMATERIAL                                                  1146
C    *                                                                    1146
C    Geant ATTRIBUTE : SBCV ===> SEEN=1 COLO=2 <===                       1146
         CALL AGSRESET                                                    1147
         AG_SEEN = 1                                                      1147
         AG_COLO = 2                                                      1147
         AG_TITLE = 'SBCV'                                                1147
         AG_EXNAME = 'ATTRIBUTE'                                          1147
         AG_PARLIST = 'SEEN_COLO'                                         1147
         CALL AXATTRIBUTE                                                 1147
C    *                                                                    1147
C    Geant SHAPE : BOX ===> DX=SFJP_SSCR_WD/2. DY=SFJP_SSCR_VL/2., DZ=SF  1147
C    JP_SSCR_THD/2. <===                                                  1147
         CALL AGSRESET                                                    1149
         AG_DX = SFJP_SSCR_WD/2.                                          1149
         AG_DY = SFJP_SSCR_VL/2.                                          1149
         AG_DZ = SFJP_SSCR_THD/2.                                         1149
         AG_TITLE = 'BOX'                                                 1149
         AG_EXNAME = 'SHAPE'                                              1149
         AG_PARLIST = 'DX_DY_DZ'                                          1149
         CALL AXSHAPE                                                     1149
C    *                                                                    1149
6091  CALL AGSPOPB                                                        1150
      GO TO BLOCK_SBCV, (6090 ,6100)                                      1150
C End of Local Procedure                                                  1150
C ----------------------------------------------------------------------  1151
C ------------ SBFV                                                       1151
C Begin Local Procedure SBFV                                              1152
C SBFV IS THE VERTICAL BRANCH OF THE ALUMIMIUM CROSS                      1152
6130  CONTINUE                                                            1152
         AG_TITLE='SBFV'                                                  1152
         AG_MODULE=MODULE                                                 1152
         AG_MARK='V00'                                                    1152
         CALL AGSPUSH                                                     1152
         AG_MODULE=MODULE                                                 1152
C    *                                                                    1152
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1153
         CALL AGSRESET                                                    1153
         AG_TITLE = 'ALUMINIUM'                                           1153
         AG_EXNAME = 'MATERIAL'                                           1153
         AG_PARLIST = 'NONE'                                              1153
         CALL AXMATERIAL                                                  1153
C    *                                                                    1153
C    Geant ATTRIBUTE : SBFV ===> SEEN=1 COLO=1 <===                       1153
         CALL AGSRESET                                                    1154
         AG_SEEN = 1                                                      1154
         AG_COLO = 1                                                      1154
         AG_TITLE = 'SBFV'                                                1154
         AG_EXNAME = 'ATTRIBUTE'                                          1154
         AG_PARLIST = 'SEEN_COLO'                                         1154
         CALL AXATTRIBUTE                                                 1154
C    *                                                                    1154
C    Geant SHAPE : BOX ===> DX=SFJP_SSCR_WD/2. DY=SFJP_SSCR_VL/2., DZ=SF  1154
C    JP_SSCR_THA/2. <===                                                  1154
         CALL AGSRESET                                                    1156
         AG_DX = SFJP_SSCR_WD/2.                                          1156
         AG_DY = SFJP_SSCR_VL/2.                                          1156
         AG_DZ = SFJP_SSCR_THA/2.                                         1156
         AG_TITLE = 'BOX'                                                 1156
         AG_EXNAME = 'SHAPE'                                              1156
         AG_PARLIST = 'DX_DY_DZ'                                          1156
         CALL AXSHAPE                                                     1156
C    *                                                                    1156
6131  CALL AGSPOPB                                                        1157
      GO TO BLOCK_SBFV, (6130 ,6140)                                      1157
C End of Local Procedure                                                  1157
C ----------------------------------------------------------------------  1158
C ------------ SSLT                                                       1158
C Begin Local Procedure SSLT                                              1159
C SSLT IS THE LINKING (SECTOR TO THE CONE) TUBE                           1159
5190  CONTINUE                                                            1159
         AG_TITLE='SSLT'                                                  1159
         AG_MODULE=MODULE                                                 1159
         AG_MARK='V00'                                                    1159
         CALL AGSPUSH                                                     1159
         AG_MODULE=MODULE                                                 1159
C    *                                                                    1159
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1160
         CALL AGSRESET                                                    1160
         AG_TITLE = 'ALUMINIUM'                                           1160
         AG_EXNAME = 'MATERIAL'                                           1160
         AG_PARLIST = 'NONE'                                              1160
         CALL AXMATERIAL                                                  1160
C    *                                                                    1160
C    Geant ATTRIBUTE : SSLT ===> SEEN=1 COLO=1 <===                       1160
         CALL AGSRESET                                                    1161
         AG_SEEN = 1                                                      1161
         AG_COLO = 1                                                      1161
         AG_TITLE = 'SSLT'                                                1161
         AG_EXNAME = 'ATTRIBUTE'                                          1161
         AG_PARLIST = 'SEEN_COLO'                                         1161
         CALL AXATTRIBUTE                                                 1161
C    *                                                                    1161
C    Geant SHAPE : TUBE ===> RMIN=0. RMAX=SFJP_SSLT_RMAX DZ=SFJP_SSLT_DZ  1161
C    /2. <===                                                             1161
         CALL AGSRESET                                                    1162
         AG_RMIN = 0.                                                     1162
         AG_RMAX = SFJP_SSLT_RMAX                                         1162
         AG_DZ = SFJP_SSLT_DZ/2.                                          1162
         AG_TITLE = 'TUBE'                                                1162
         AG_EXNAME = 'SHAPE'                                              1162
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1162
         CALL AXSHAPE                                                     1162
C    *                                                                    1162
5191  CALL AGSPOPB                                                        1163
      GO TO BLOCK_SSLT, (5190 ,5200)                                      1163
C End of Local Procedure                                                  1163
C ----------------------------------------------------------------------  1164
C ------------ SCMP                                                       1164
C Begin Local Procedure SCMP                                              1165
C SCMP IS THE MOUNTING PLATE INSERTED IN THE CONES.                       1165
5210  CONTINUE                                                            1165
         AG_TITLE='SCMP'                                                  1165
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
C    Geant ATTRIBUTE : SCMP ===> SEEN=1 COLO=1 <===                       1166
         CALL AGSRESET                                                    1167
         AG_SEEN = 1                                                      1167
         AG_COLO = 1                                                      1167
         AG_TITLE = 'SCMP'                                                1167
         AG_EXNAME = 'ATTRIBUTE'                                          1167
         AG_PARLIST = 'SEEN_COLO'                                         1167
         CALL AXATTRIBUTE                                                 1167
C    *                                                                    1167
C    Geant SHAPE : BOX ===> DX=SFJP_SCMP_DX/2. DY=SFJP_SCMP_DY/2., DZ=SF  1167
C    JP_SCMP_DZ/2 <===                                                    1167
         CALL AGSRESET                                                    1169
         AG_DX = SFJP_SCMP_DX/2.                                          1169
         AG_DY = SFJP_SCMP_DY/2.                                          1169
         AG_DZ = SFJP_SCMP_DZ/2                                           1169
         AG_TITLE = 'BOX'                                                 1169
         AG_EXNAME = 'SHAPE'                                              1169
         AG_PARLIST = 'DX_DY_DZ'                                          1169
         CALL AXSHAPE                                                     1169
C    *                                                                    1169
5211  CALL AGSPOPB                                                        1170
      GO TO BLOCK_SCMP, (5210 ,5220)                                      1170
C End of Local Procedure                                                  1170
C ----------------------------------------------------------------------  1171
C ------------ SCVM                                                       1171
C Begin Local Procedure SCVM                                              1172
C SCVM IS THE MOTHER VOLUME OF THE V-SHAPE PIECE                          1172
5230  CONTINUE                                                            1172
         AG_TITLE='SCVM'                                                  1172
         AG_MODULE=MODULE                                                 1172
         AG_MARK='V00'                                                    1172
         CALL AGSPUSH                                                     1172
         AG_MODULE=MODULE                                                 1172
C    *                                                                    1172
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1173
         CALL AGSRESET                                                    1173
         AG_TITLE = 'AIR'                                                 1173
         AG_EXNAME = 'MATERIAL'                                           1173
         AG_PARLIST = 'NONE'                                              1173
         CALL AXMATERIAL                                                  1173
C    *                                                                    1173
C    Geant ATTRIBUTE : SCVM ===> SEEN=0 COLO=6 <===                       1173
         CALL AGSRESET                                                    1174
         AG_SEEN = 0                                                      1174
         AG_COLO = 6                                                      1174
         AG_TITLE = 'SCVM'                                                1174
         AG_EXNAME = 'ATTRIBUTE'                                          1174
         AG_PARLIST = 'SEEN_COLO'                                         1174
         CALL AXATTRIBUTE                                                 1174
C    *                                                                    1174
C    Geant SHAPE : BOX ===> DX=SFJP_SCVM_DX/2. DY=SFJP_SCVM_DY/2., DZ=SF  1174
C    JP_SCVM_DZ/2 <===                                                    1174
         CALL AGSRESET                                                    1176
         AG_DX = SFJP_SCVM_DX/2.                                          1176
         AG_DY = SFJP_SCVM_DY/2.                                          1176
         AG_DZ = SFJP_SCVM_DZ/2                                           1176
         AG_TITLE = 'BOX'                                                 1176
         AG_EXNAME = 'SHAPE'                                              1176
         AG_PARLIST = 'DX_DY_DZ'                                          1176
         CALL AXSHAPE                                                     1176
C    *                                                                    1176
C    Create SCVB :                                                        1177
C    Call To Local Procedure SCVB                                         1177
         ASSIGN 6160 TO BLOCK_SCVB                                        1177
         GO TO  6150                                                      1177
6160     CONTINUE                                                         1177
C    *                                                                    1177
C    Geant POSITION : SCVB ===> X=0. Y=-SFJP_SCVM_DY/2.+SFJP_SCVB_DY/2.   1178
C    Z=0. <===                                                            1178
         CALL AGSRESET                                                    1178
         AG_X = 0.                                                        1178
         AG_Y = -SFJP_SCVM_DY/2.+SFJP_SCVB_DY/2.                          1178
         AG_Z = 0.                                                        1178
         AG_TITLE = 'SCVB'                                                1178
         AG_EXNAME = 'POSITION'                                           1178
         AG_PARLIST = 'X_Y_Z'                                             1178
         CALL AXPOSITION                                                  1178
C    *                                                                    1178
C    Create SCVS :                                                        1179
C    Call To Local Procedure SCVS                                         1179
         ASSIGN 6180 TO BLOCK_SCVS                                        1179
         GO TO  6170                                                      1179
6180     CONTINUE                                                         1179
C    *                                                                    1179
C    Geant POSITION : SCVS ===> X=SFJP_SCVS_PX Y=SFJP_SCVS_PY Z=0.0 ALPH  1180
C    AZ=SFJP_ALPHAZH <===                                                 1180
         CALL AGSRESET                                                    1180
         AG_X = SFJP_SCVS_PX                                              1180
         AG_Y = SFJP_SCVS_PY                                              1180
         AG_Z = 0.0                                                       1180
         AG_ALPHAN=AG_ALPHAN+1                                            1180
         AG_ALPHAK(AG_ALPHAN)=3                                           1180
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                1180
         AG_TITLE = 'SCVS'                                                1180
         AG_EXNAME = 'POSITION'                                           1180
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      1180
         CALL AXPOSITION                                                  1180
C    *                                                                    1180
C    Geant POSITION : SCVS ===> X=-SFJP_SCVS_PX Y=SFJP_SCVS_PY Z=0.0 ALP  1180
C    HAZ=-SFJP_ALPHAZH <===                                               1180
         CALL AGSRESET                                                    1181
         AG_X = -SFJP_SCVS_PX                                             1181
         AG_Y = SFJP_SCVS_PY                                              1181
         AG_Z = 0.0                                                       1181
         AG_ALPHAN=AG_ALPHAN+1                                            1181
         AG_ALPHAK(AG_ALPHAN)=3                                           1181
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               1181
         AG_TITLE = 'SCVS'                                                1181
         AG_EXNAME = 'POSITION'                                           1181
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      1181
         CALL AXPOSITION                                                  1181
C    *                                                                    1181
5231  CALL AGSPOPB                                                        1182
      GO TO BLOCK_SCVM, (5230 ,5240)                                      1182
C End of Local Procedure                                                  1182
C ----------------------------------------------------------------------  1183
C ------------ SCVB                                                       1183
C Begin Local Procedure SCVB                                              1184
C SCVB IS THE BASE PLATE OF THE V-SHAPE PIECE                             1184
6150  CONTINUE                                                            1184
         AG_TITLE='SCVB'                                                  1184
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
C    Geant ATTRIBUTE : SCVB ===> SEEN=1 COLO=1 <===                       1185
         CALL AGSRESET                                                    1186
         AG_SEEN = 1                                                      1186
         AG_COLO = 1                                                      1186
         AG_TITLE = 'SCVB'                                                1186
         AG_EXNAME = 'ATTRIBUTE'                                          1186
         AG_PARLIST = 'SEEN_COLO'                                         1186
         CALL AXATTRIBUTE                                                 1186
C    *                                                                    1186
C    Geant SHAPE : BOX ===> DX=SFJP_SCVM_DX/2. DY=SFJP_SCVB_DY/2., DZ=SF  1186
C    JP_SCVM_DZ/2 <===                                                    1186
         CALL AGSRESET                                                    1188
         AG_DX = SFJP_SCVM_DX/2.                                          1188
         AG_DY = SFJP_SCVB_DY/2.                                          1188
         AG_DZ = SFJP_SCVM_DZ/2                                           1188
         AG_TITLE = 'BOX'                                                 1188
         AG_EXNAME = 'SHAPE'                                              1188
         AG_PARLIST = 'DX_DY_DZ'                                          1188
         CALL AXSHAPE                                                     1188
C    *                                                                    1188
6151  CALL AGSPOPB                                                        1189
      GO TO BLOCK_SCVB, (6150 ,6160)                                      1189
C End of Local Procedure                                                  1189
C ----------------------------------------------------------------------  1190
C ------------ SCVS                                                       1190
C Begin Local Procedure SCVS                                              1191
C SCVS IS THE SIDE PLATE OF THE V-SHAPE PIECE                             1191
6170  CONTINUE                                                            1191
         AG_TITLE='SCVS'                                                  1191
         AG_MODULE=MODULE                                                 1191
         AG_MARK='V00'                                                    1191
         CALL AGSPUSH                                                     1191
         AG_MODULE=MODULE                                                 1191
C    *                                                                    1191
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1192
         CALL AGSRESET                                                    1192
         AG_TITLE = 'ALUMINIUM'                                           1192
         AG_EXNAME = 'MATERIAL'                                           1192
         AG_PARLIST = 'NONE'                                              1192
         CALL AXMATERIAL                                                  1192
C    *                                                                    1192
C    Geant ATTRIBUTE : SCVS ===> SEEN=1 COLO=1 <===                       1192
         CALL AGSRESET                                                    1193
         AG_SEEN = 1                                                      1193
         AG_COLO = 1                                                      1193
         AG_TITLE = 'SCVS'                                                1193
         AG_EXNAME = 'ATTRIBUTE'                                          1193
         AG_PARLIST = 'SEEN_COLO'                                         1193
         CALL AXATTRIBUTE                                                 1193
C    *                                                                    1193
C    Geant SHAPE : BOX ===> DX=SFJP_SCVS_DX/2. DY=SFJP_SCVS_DY/2., DZ=SF  1193
C    JP_SCVM_DZ/2 <===                                                    1193
         CALL AGSRESET                                                    1195
         AG_DX = SFJP_SCVS_DX/2.                                          1195
         AG_DY = SFJP_SCVS_DY/2.                                          1195
         AG_DZ = SFJP_SCVM_DZ/2                                           1195
         AG_TITLE = 'BOX'                                                 1195
         AG_EXNAME = 'SHAPE'                                              1195
         AG_PARLIST = 'DX_DY_DZ'                                          1195
         CALL AXSHAPE                                                     1195
C    *                                                                    1195
6171  CALL AGSPOPB                                                        1196
      GO TO BLOCK_SCVS, (6170 ,6180)                                      1196
C End of Local Procedure                                                  1196
C ----------------------------------------------------------------------  1197
C ------------ SOSK                                                       1197
C Begin Local Procedure SOSK                                              1198
C SOSK IS THE SEPARATION SHIELD CYLINDER                                  1198
5040  CONTINUE                                                            1198
         AG_TITLE='SOSK'                                                  1198
         AG_MODULE=MODULE                                                 1198
         AG_MARK='V00'                                                    1198
         CALL AGSPUSH                                                     1198
         AG_MODULE=MODULE                                                 1198
C    *                                                                    1198
C    Geant COMPONENT : C5 ===> A=12 Z=6 W=10000*5 <===                    1199
         CALL AGSRESET                                                    1199
         AG_A = 12                                                        1199
         AG_Z = 6                                                         1199
         AG_W = 10000*5                                                   1199
         AG_TITLE = 'C5'                                                  1199
         AG_EXNAME = 'COMPONENT'                                          1199
         AG_PARLIST = 'A_Z_W'                                             1199
         CALL AXCOMPONENT                                                 1199
C    *                                                                    1199
C    Geant COMPONENT : H4 ===> A=1 Z=1 W=10000*4 <===                     1199
         CALL AGSRESET                                                    1200
         AG_A = 1                                                         1200
         AG_Z = 1                                                         1200
         AG_W = 10000*4                                                   1200
         AG_TITLE = 'H4'                                                  1200
         AG_EXNAME = 'COMPONENT'                                          1200
         AG_PARLIST = 'A_Z_W'                                             1200
         CALL AXCOMPONENT                                                 1200
C    *                                                                    1200
C    Geant COMPONENT : O2 ===> A=16 Z=8 W=10000*2 <===                    1200
         CALL AGSRESET                                                    1201
         AG_A = 16                                                        1201
         AG_Z = 8                                                         1201
         AG_W = 10000*2                                                   1201
         AG_TITLE = 'O2'                                                  1201
         AG_EXNAME = 'COMPONENT'                                          1201
         AG_PARLIST = 'A_Z_W'                                             1201
         CALL AXCOMPONENT                                                 1201
C    *                                                                    1201
C    Geant COMPONENT : AL ===> A=27 Z=13 W=986 <===                       1201
         CALL AGSRESET                                                    1202
         AG_A = 27                                                        1202
         AG_Z = 13                                                        1202
         AG_W = 986                                                       1202
         AG_TITLE = 'AL'                                                  1202
         AG_EXNAME = 'COMPONENT'                                          1202
         AG_PARLIST = 'A_Z_W'                                             1202
         CALL AXCOMPONENT                                                 1202
C    *                                                                    1202
C    Geant MIXTURE : SSDALMY ===> DENS=1.40845 <===                       1202
         CALL AGSRESET                                                    1203
         AG_DENS = 1.40845                                                1203
         AG_TITLE = 'SSDALMY'                                             1203
         AG_EXNAME = 'MIXTURE'                                            1203
         AG_PARLIST = 'DENS'                                              1203
         CALL AXMIXTURE                                                   1203
C    *                                                                    1203
C    Geant MATERIAL : SSDALMY ===> inherited arguments used <===          1203
         CALL AGSRESET                                                    1204
         AG_TITLE = 'SSDALMY'                                             1204
         AG_EXNAME = 'MATERIAL'                                           1204
         AG_PARLIST = 'NONE'                                              1204
         CALL AXMATERIAL                                                  1204
C    *                                                                    1204
C    Geant ATTRIBUTE : SOSK ===> SEEN=1 COLO=3 <===                       1204
         CALL AGSRESET                                                    1205
         AG_SEEN = 1                                                      1205
         AG_COLO = 3                                                      1205
         AG_TITLE = 'SOSK'                                                1205
         AG_EXNAME = 'ATTRIBUTE'                                          1205
         AG_PARLIST = 'SEEN_COLO'                                         1205
         CALL AXATTRIBUTE                                                 1205
C    *                                                                    1205
C    Geant SHAPE : TUBE ===> RMIN=SFPA_SOUTRINN RMAX=SFPA_SOUTROUT, DZ=S  1205
C    FPA_SOUTLEN <===                                                     1205
         CALL AGSRESET                                                    1207
         AG_RMIN = SFPA_SOUTRINN                                          1207
         AG_RMAX = SFPA_SOUTROUT                                          1207
         AG_DZ = SFPA_SOUTLEN                                             1207
         AG_TITLE = 'TUBE'                                                1207
         AG_EXNAME = 'SHAPE'                                              1207
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1207
         CALL AXSHAPE                                                     1207
C    *                                                                    1207
5041  CALL AGSPOPB                                                        1208
      GO TO BLOCK_SOSK, (5040 ,5050)                                      1208
C End of Local Procedure                                                  1208
      WRITE(LOUT,6190)'===>>  [End AgML Module SISDGEO6]  <<==='          1208
6190  FORMAT(A32,/,/)                                                     1208
C MODULE SISDGEO6                                                         1210
      END                                                                 1210
