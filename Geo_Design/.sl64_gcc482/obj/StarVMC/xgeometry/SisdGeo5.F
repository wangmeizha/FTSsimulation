C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE SISDGEO5                                                 1
C SISDGEO5 IS THE SILICON STRIP DETECTOR                                  1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'SISDGEO5'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'IS THE SILICON STRIP DETECTOR'/                      1
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
      REAL SSDP_CONFIG                                                    11
      EQUIVALENCE (BANK_SSDP(4),SSDP_CONFIG,RE_SSDP_CONFIG),              11
     *(NAMES_SSDP(2),NSSDP_CONFIG)                                        11
      EQUIVALENCE (COMMEN_SSDP(2),CSSDP_CONFIG),(MAP_SSDP(1,2),           11
     *MSSDP_CONFIG)                                                       11
      CHARACTER CSSDP_CONFIG*40/' '/                                      11
      CHARACTER NSSDP_CONFIG*12/'R.CONFIG'/                               11
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
C -                                                                       27
      REAL SFJP_VERSION                                                   27
      EQUIVALENCE (BANK_SFJP(3),SFJP_VERSION,RE_SFJP_VERSION),            27
     *(NAMES_SFJP(1),NSFJP_VERSION)                                       27
      EQUIVALENCE (COMMEN_SFJP(1),CSFJP_VERSION),(MAP_SFJP(1,1),          27
     *MSFJP_VERSION)                                                      27
      CHARACTER CSFJP_VERSION*40/' '/                                     27
      CHARACTER NSFJP_VERSION*12/'R.VERSION'/                             27
      INTEGER MSFJP_VERSION(2)/1,1/                                       27
      REAL RE_SFJP_VERSION                                                27
C -                                                                       27
      REAL SFJP_ALPHAZ                                                    27
      EQUIVALENCE (BANK_SFJP(4),SFJP_ALPHAZ,RE_SFJP_ALPHAZ),              27
     *(NAMES_SFJP(2),NSFJP_ALPHAZ)                                        27
      EQUIVALENCE (COMMEN_SFJP(2),CSFJP_ALPHAZ),(MAP_SFJP(1,2),           27
     *MSFJP_ALPHAZ)                                                       27
      CHARACTER CSFJP_ALPHAZ*40/' '/                                      27
      CHARACTER NSFJP_ALPHAZ*12/'R.ALPHAZ'/                               27
      INTEGER MSFJP_ALPHAZ(2)/1,1/                                        27
      REAL RE_SFJP_ALPHAZ                                                 27
C -                                                                       27
      REAL SFJP_ALPHAZH                                                   27
      EQUIVALENCE (BANK_SFJP(5),SFJP_ALPHAZH,RE_SFJP_ALPHAZH),            27
     *(NAMES_SFJP(3),NSFJP_ALPHAZH)                                       27
      EQUIVALENCE (COMMEN_SFJP(3),CSFJP_ALPHAZH),(MAP_SFJP(1,3),          27
     *MSFJP_ALPHAZH)                                                      27
      CHARACTER CSFJP_ALPHAZH*40/' '/                                     27
      CHARACTER NSFJP_ALPHAZH*12/'R.ALPHAZH'/                             27
      INTEGER MSFJP_ALPHAZH(2)/1,1/                                       27
      REAL RE_SFJP_ALPHAZH                                                27
C -                                                                       27
      REAL SFJP_SSST_RMIN                                                 27
      EQUIVALENCE (BANK_SFJP(6),SFJP_SSST_RMIN,RE_SFJP_SSST_RMIN),        27
     *(NAMES_SFJP(4),NSFJP_SSST_RMIN)                                     27
      EQUIVALENCE (COMMEN_SFJP(4),CSFJP_SSST_RMIN),(MAP_SFJP(1,4),        27
     *MSFJP_SSST_RMIN)                                                    27
      CHARACTER CSFJP_SSST_RMIN*40/' '/                                   27
      CHARACTER NSFJP_SSST_RMIN*12/'R.SSST_RMIN'/                         27
      INTEGER MSFJP_SSST_RMIN(2)/1,1/                                     27
      REAL RE_SFJP_SSST_RMIN                                              27
C -                                                                       27
      REAL SFJP_SSST_RMAX                                                 27
      EQUIVALENCE (BANK_SFJP(7),SFJP_SSST_RMAX,RE_SFJP_SSST_RMAX),        27
     *(NAMES_SFJP(5),NSFJP_SSST_RMAX)                                     27
      EQUIVALENCE (COMMEN_SFJP(5),CSFJP_SSST_RMAX),(MAP_SFJP(1,5),        27
     *MSFJP_SSST_RMAX)                                                    27
      CHARACTER CSFJP_SSST_RMAX*40/' '/                                   27
      CHARACTER NSFJP_SSST_RMAX*12/'R.SSST_RMAX'/                         27
      INTEGER MSFJP_SSST_RMAX(2)/1,1/                                     27
      REAL RE_SFJP_SSST_RMAX                                              27
C -                                                                       27
      REAL SFJP_SSST_WIDTH                                                27
      EQUIVALENCE (BANK_SFJP(8),SFJP_SSST_WIDTH,RE_SFJP_SSST_WIDTH),      27
     *(NAMES_SFJP(6),NSFJP_SSST_WIDTH)                                    27
      EQUIVALENCE (COMMEN_SFJP(6),CSFJP_SSST_WIDTH),(MAP_SFJP(1,6),       27
     *MSFJP_SSST_WIDTH)                                                   27
      CHARACTER CSFJP_SSST_WIDTH*40/' '/                                  27
      CHARACTER NSFJP_SSST_WIDTH*12/'R.SSST_WIDTH'/                       27
      INTEGER MSFJP_SSST_WIDTH(2)/1,1/                                    27
      REAL RE_SFJP_SSST_WIDTH                                             27
C -                                                                       27
      REAL SFJP_SSST_PZ                                                   27
      EQUIVALENCE (BANK_SFJP(9),SFJP_SSST_PZ,RE_SFJP_SSST_PZ),            27
     *(NAMES_SFJP(7),NSFJP_SSST_PZ)                                       27
      EQUIVALENCE (COMMEN_SFJP(7),CSFJP_SSST_PZ),(MAP_SFJP(1,7),          27
     *MSFJP_SSST_PZ)                                                      27
      CHARACTER CSFJP_SSST_PZ*40/' '/                                     27
      CHARACTER NSFJP_SSST_PZ*12/'R.SSST_PZ'/                             27
      INTEGER MSFJP_SSST_PZ(2)/1,1/                                       27
      REAL RE_SFJP_SSST_PZ                                                27
C -                                                                       27
      REAL SFJP_SSSS_RMIN                                                 27
      EQUIVALENCE (BANK_SFJP(10),SFJP_SSSS_RMIN,RE_SFJP_SSSS_RMIN),       27
     *(NAMES_SFJP(8),NSFJP_SSSS_RMIN)                                     27
      EQUIVALENCE (COMMEN_SFJP(8),CSFJP_SSSS_RMIN),(MAP_SFJP(1,8),        27
     *MSFJP_SSSS_RMIN)                                                    27
      CHARACTER CSFJP_SSSS_RMIN*40/' '/                                   27
      CHARACTER NSFJP_SSSS_RMIN*12/'R.SSSS_RMIN'/                         27
      INTEGER MSFJP_SSSS_RMIN(2)/1,1/                                     27
      REAL RE_SFJP_SSSS_RMIN                                              27
C -                                                                       27
      REAL SFJP_SSSS_WIDTH                                                27
      EQUIVALENCE (BANK_SFJP(11),SFJP_SSSS_WIDTH,RE_SFJP_SSSS_WIDTH),     27
     *(NAMES_SFJP(9),NSFJP_SSSS_WIDTH)                                    27
      EQUIVALENCE (COMMEN_SFJP(9),CSFJP_SSSS_WIDTH),(MAP_SFJP(1,9),       27
     *MSFJP_SSSS_WIDTH)                                                   27
      CHARACTER CSFJP_SSSS_WIDTH*40/' '/                                  27
      CHARACTER NSFJP_SSSS_WIDTH*12/'R.SSSS_WIDTH'/                       27
      INTEGER MSFJP_SSSS_WIDTH(2)/1,1/                                    27
      REAL RE_SFJP_SSSS_WIDTH                                             27
C -                                                                       27
      REAL SFJP_SSRS_RMIN                                                 27
      EQUIVALENCE (BANK_SFJP(12),SFJP_SSRS_RMIN,RE_SFJP_SSRS_RMIN),       27
     *(NAMES_SFJP(10),NSFJP_SSRS_RMIN)                                    27
      EQUIVALENCE (COMMEN_SFJP(10),CSFJP_SSRS_RMIN),(MAP_SFJP(1,10),      27
     *MSFJP_SSRS_RMIN)                                                    27
      CHARACTER CSFJP_SSRS_RMIN*40/' '/                                   27
      CHARACTER NSFJP_SSRS_RMIN*12/'R.SSRS_RMIN'/                         27
      INTEGER MSFJP_SSRS_RMIN(2)/1,1/                                     27
      REAL RE_SFJP_SSRS_RMIN                                              27
C -                                                                       27
      REAL SFJP_SSLB_DX                                                   27
      EQUIVALENCE (BANK_SFJP(13),SFJP_SSLB_DX,RE_SFJP_SSLB_DX),           27
     *(NAMES_SFJP(11),NSFJP_SSLB_DX)                                      27
      EQUIVALENCE (COMMEN_SFJP(11),CSFJP_SSLB_DX),(MAP_SFJP(1,11),        27
     *MSFJP_SSLB_DX)                                                      27
      CHARACTER CSFJP_SSLB_DX*40/' '/                                     27
      CHARACTER NSFJP_SSLB_DX*12/'R.SSLB_DX'/                             27
      INTEGER MSFJP_SSLB_DX(2)/1,1/                                       27
      REAL RE_SFJP_SSLB_DX                                                27
C -                                                                       27
      REAL SFJP_SSLB_PX                                                   27
      EQUIVALENCE (BANK_SFJP(14),SFJP_SSLB_PX,RE_SFJP_SSLB_PX),           27
     *(NAMES_SFJP(12),NSFJP_SSLB_PX)                                      27
      EQUIVALENCE (COMMEN_SFJP(12),CSFJP_SSLB_PX),(MAP_SFJP(1,12),        27
     *MSFJP_SSLB_PX)                                                      27
      CHARACTER CSFJP_SSLB_PX*40/' '/                                     27
      CHARACTER NSFJP_SSLB_PX*12/'R.SSLB_PX'/                             27
      INTEGER MSFJP_SSLB_PX(2)/1,1/                                       27
      REAL RE_SFJP_SSLB_PX                                                27
C -                                                                       27
      REAL SFJP_SSLB_DY                                                   27
      EQUIVALENCE (BANK_SFJP(15),SFJP_SSLB_DY,RE_SFJP_SSLB_DY),           27
     *(NAMES_SFJP(13),NSFJP_SSLB_DY)                                      27
      EQUIVALENCE (COMMEN_SFJP(13),CSFJP_SSLB_DY),(MAP_SFJP(1,13),        27
     *MSFJP_SSLB_DY)                                                      27
      CHARACTER CSFJP_SSLB_DY*40/' '/                                     27
      CHARACTER NSFJP_SSLB_DY*12/'R.SSLB_DY'/                             27
      INTEGER MSFJP_SSLB_DY(2)/1,1/                                       27
      REAL RE_SFJP_SSLB_DY                                                27
C -                                                                       27
      REAL SFJP_SSLB_DZ                                                   27
      EQUIVALENCE (BANK_SFJP(16),SFJP_SSLB_DZ,RE_SFJP_SSLB_DZ),           27
     *(NAMES_SFJP(14),NSFJP_SSLB_DZ)                                      27
      EQUIVALENCE (COMMEN_SFJP(14),CSFJP_SSLB_DZ),(MAP_SFJP(1,14),        27
     *MSFJP_SSLB_DZ)                                                      27
      CHARACTER CSFJP_SSLB_DZ*40/' '/                                     27
      CHARACTER NSFJP_SSLB_DZ*12/'R.SSLB_DZ'/                             27
      INTEGER MSFJP_SSLB_DZ(2)/1,1/                                       27
      REAL RE_SFJP_SSLB_DZ                                                27
C -                                                                       27
      REAL SFJP_SSBQ_DX                                                   27
      EQUIVALENCE (BANK_SFJP(17),SFJP_SSBQ_DX,RE_SFJP_SSBQ_DX),           27
     *(NAMES_SFJP(15),NSFJP_SSBQ_DX)                                      27
      EQUIVALENCE (COMMEN_SFJP(15),CSFJP_SSBQ_DX),(MAP_SFJP(1,15),        27
     *MSFJP_SSBQ_DX)                                                      27
      CHARACTER CSFJP_SSBQ_DX*40/' '/                                     27
      CHARACTER NSFJP_SSBQ_DX*12/'R.SSBQ_DX'/                             27
      INTEGER MSFJP_SSBQ_DX(2)/1,1/                                       27
      REAL RE_SFJP_SSBQ_DX                                                27
C -                                                                       27
      REAL SFJP_SSBQ_DY                                                   27
      EQUIVALENCE (BANK_SFJP(18),SFJP_SSBQ_DY,RE_SFJP_SSBQ_DY),           27
     *(NAMES_SFJP(16),NSFJP_SSBQ_DY)                                      27
      EQUIVALENCE (COMMEN_SFJP(16),CSFJP_SSBQ_DY),(MAP_SFJP(1,16),        27
     *MSFJP_SSBQ_DY)                                                      27
      CHARACTER CSFJP_SSBQ_DY*40/' '/                                     27
      CHARACTER NSFJP_SSBQ_DY*12/'R.SSBQ_DY'/                             27
      INTEGER MSFJP_SSBQ_DY(2)/1,1/                                       27
      REAL RE_SFJP_SSBQ_DY                                                27
C -                                                                       27
      REAL SFJP_SSBQ_DZ                                                   27
      EQUIVALENCE (BANK_SFJP(19),SFJP_SSBQ_DZ,RE_SFJP_SSBQ_DZ),           27
     *(NAMES_SFJP(17),NSFJP_SSBQ_DZ)                                      27
      EQUIVALENCE (COMMEN_SFJP(17),CSFJP_SSBQ_DZ),(MAP_SFJP(1,17),        27
     *MSFJP_SSBQ_DZ)                                                      27
      CHARACTER CSFJP_SSBQ_DZ*40/' '/                                     27
      CHARACTER NSFJP_SSBQ_DZ*12/'R.SSBQ_DZ'/                             27
      INTEGER MSFJP_SSBQ_DZ(2)/1,1/                                       27
      REAL RE_SFJP_SSBQ_DZ                                                27
C -                                                                       27
      REAL SFJP_SSCR_THA                                                  27
      EQUIVALENCE (BANK_SFJP(20),SFJP_SSCR_THA,RE_SFJP_SSCR_THA),         27
     *(NAMES_SFJP(18),NSFJP_SSCR_THA)                                     27
      EQUIVALENCE (COMMEN_SFJP(18),CSFJP_SSCR_THA),(MAP_SFJP(1,18),       27
     *MSFJP_SSCR_THA)                                                     27
      CHARACTER CSFJP_SSCR_THA*40/' '/                                    27
      CHARACTER NSFJP_SSCR_THA*12/'R.SSCR_THA'/                           27
      INTEGER MSFJP_SSCR_THA(2)/1,1/                                      27
      REAL RE_SFJP_SSCR_THA                                               27
C -                                                                       27
      REAL SFJP_SSCR_THD                                                  27
      EQUIVALENCE (BANK_SFJP(21),SFJP_SSCR_THD,RE_SFJP_SSCR_THD),         27
     *(NAMES_SFJP(19),NSFJP_SSCR_THD)                                     27
      EQUIVALENCE (COMMEN_SFJP(19),CSFJP_SSCR_THD),(MAP_SFJP(1,19),       27
     *MSFJP_SSCR_THD)                                                     27
      CHARACTER CSFJP_SSCR_THD*40/' '/                                    27
      CHARACTER NSFJP_SSCR_THD*12/'R.SSCR_THD'/                           27
      INTEGER MSFJP_SSCR_THD(2)/1,1/                                      27
      REAL RE_SFJP_SSCR_THD                                               27
C -                                                                       27
      REAL SFJP_SSCR_WD                                                   27
      EQUIVALENCE (BANK_SFJP(22),SFJP_SSCR_WD,RE_SFJP_SSCR_WD),           27
     *(NAMES_SFJP(20),NSFJP_SSCR_WD)                                      27
      EQUIVALENCE (COMMEN_SFJP(20),CSFJP_SSCR_WD),(MAP_SFJP(1,20),        27
     *MSFJP_SSCR_WD)                                                      27
      CHARACTER CSFJP_SSCR_WD*40/' '/                                     27
      CHARACTER NSFJP_SSCR_WD*12/'R.SSCR_WD'/                             27
      INTEGER MSFJP_SSCR_WD(2)/1,1/                                       27
      REAL RE_SFJP_SSCR_WD                                                27
C -                                                                       27
      REAL SFJP_SSCR_HL                                                   27
      EQUIVALENCE (BANK_SFJP(23),SFJP_SSCR_HL,RE_SFJP_SSCR_HL),           27
     *(NAMES_SFJP(21),NSFJP_SSCR_HL)                                      27
      EQUIVALENCE (COMMEN_SFJP(21),CSFJP_SSCR_HL),(MAP_SFJP(1,21),        27
     *MSFJP_SSCR_HL)                                                      27
      CHARACTER CSFJP_SSCR_HL*40/' '/                                     27
      CHARACTER NSFJP_SSCR_HL*12/'R.SSCR_HL'/                             27
      INTEGER MSFJP_SSCR_HL(2)/1,1/                                       27
      REAL RE_SFJP_SSCR_HL                                                27
C -                                                                       27
      REAL SFJP_SSCR_VL                                                   27
      EQUIVALENCE (BANK_SFJP(24),SFJP_SSCR_VL,RE_SFJP_SSCR_VL),           27
     *(NAMES_SFJP(22),NSFJP_SSCR_VL)                                      27
      EQUIVALENCE (COMMEN_SFJP(22),CSFJP_SSCR_VL),(MAP_SFJP(1,22),        27
     *MSFJP_SSCR_VL)                                                      27
      CHARACTER CSFJP_SSCR_VL*40/' '/                                     27
      CHARACTER NSFJP_SSCR_VL*12/'R.SSCR_VL'/                             27
      INTEGER MSFJP_SSCR_VL(2)/1,1/                                       27
      REAL RE_SFJP_SSCR_VL                                                27
C -                                                                       27
      REAL SFJP_SSLT_PX                                                   27
      EQUIVALENCE (BANK_SFJP(25),SFJP_SSLT_PX,RE_SFJP_SSLT_PX),           27
     *(NAMES_SFJP(23),NSFJP_SSLT_PX)                                      27
      EQUIVALENCE (COMMEN_SFJP(23),CSFJP_SSLT_PX),(MAP_SFJP(1,23),        27
     *MSFJP_SSLT_PX)                                                      27
      CHARACTER CSFJP_SSLT_PX*40/' '/                                     27
      CHARACTER NSFJP_SSLT_PX*12/'R.SSLT_PX'/                             27
      INTEGER MSFJP_SSLT_PX(2)/1,1/                                       27
      REAL RE_SFJP_SSLT_PX                                                27
C -                                                                       27
      REAL SFJP_SSLT_RMAX                                                 27
      EQUIVALENCE (BANK_SFJP(26),SFJP_SSLT_RMAX,RE_SFJP_SSLT_RMAX),       27
     *(NAMES_SFJP(24),NSFJP_SSLT_RMAX)                                    27
      EQUIVALENCE (COMMEN_SFJP(24),CSFJP_SSLT_RMAX),(MAP_SFJP(1,24),      27
     *MSFJP_SSLT_RMAX)                                                    27
      CHARACTER CSFJP_SSLT_RMAX*40/' '/                                   27
      CHARACTER NSFJP_SSLT_RMAX*12/'R.SSLT_RMAX'/                         27
      INTEGER MSFJP_SSLT_RMAX(2)/1,1/                                     27
      REAL RE_SFJP_SSLT_RMAX                                              27
C -                                                                       27
      REAL SFJP_SSLT_DZ                                                   27
      EQUIVALENCE (BANK_SFJP(27),SFJP_SSLT_DZ,RE_SFJP_SSLT_DZ),           27
     *(NAMES_SFJP(25),NSFJP_SSLT_DZ)                                      27
      EQUIVALENCE (COMMEN_SFJP(25),CSFJP_SSLT_DZ),(MAP_SFJP(1,25),        27
     *MSFJP_SSLT_DZ)                                                      27
      CHARACTER CSFJP_SSLT_DZ*40/' '/                                     27
      CHARACTER NSFJP_SSLT_DZ*12/'R.SSLT_DZ'/                             27
      INTEGER MSFJP_SSLT_DZ(2)/1,1/                                       27
      REAL RE_SFJP_SSLT_DZ                                                27
C -                                                                       27
      REAL SFJP_SCMP_DX                                                   27
      EQUIVALENCE (BANK_SFJP(28),SFJP_SCMP_DX,RE_SFJP_SCMP_DX),           27
     *(NAMES_SFJP(26),NSFJP_SCMP_DX)                                      27
      EQUIVALENCE (COMMEN_SFJP(26),CSFJP_SCMP_DX),(MAP_SFJP(1,26),        27
     *MSFJP_SCMP_DX)                                                      27
      CHARACTER CSFJP_SCMP_DX*40/' '/                                     27
      CHARACTER NSFJP_SCMP_DX*12/'R.SCMP_DX'/                             27
      INTEGER MSFJP_SCMP_DX(2)/1,1/                                       27
      REAL RE_SFJP_SCMP_DX                                                27
C -                                                                       27
      REAL SFJP_SCMP_DY                                                   27
      EQUIVALENCE (BANK_SFJP(29),SFJP_SCMP_DY,RE_SFJP_SCMP_DY),           27
     *(NAMES_SFJP(27),NSFJP_SCMP_DY)                                      27
      EQUIVALENCE (COMMEN_SFJP(27),CSFJP_SCMP_DY),(MAP_SFJP(1,27),        27
     *MSFJP_SCMP_DY)                                                      27
      CHARACTER CSFJP_SCMP_DY*40/' '/                                     27
      CHARACTER NSFJP_SCMP_DY*12/'R.SCMP_DY'/                             27
      INTEGER MSFJP_SCMP_DY(2)/1,1/                                       27
      REAL RE_SFJP_SCMP_DY                                                27
C -                                                                       27
      REAL SFJP_SCMP_DZ                                                   27
      EQUIVALENCE (BANK_SFJP(30),SFJP_SCMP_DZ,RE_SFJP_SCMP_DZ),           27
     *(NAMES_SFJP(28),NSFJP_SCMP_DZ)                                      27
      EQUIVALENCE (COMMEN_SFJP(28),CSFJP_SCMP_DZ),(MAP_SFJP(1,28),        27
     *MSFJP_SCMP_DZ)                                                      27
      CHARACTER CSFJP_SCMP_DZ*40/' '/                                     27
      CHARACTER NSFJP_SCMP_DZ*12/'R.SCMP_DZ'/                             27
      INTEGER MSFJP_SCMP_DZ(2)/1,1/                                       27
      REAL RE_SFJP_SCMP_DZ                                                27
C -                                                                       27
      REAL SFJP_SCMP_PX                                                   27
      EQUIVALENCE (BANK_SFJP(31),SFJP_SCMP_PX,RE_SFJP_SCMP_PX),           27
     *(NAMES_SFJP(29),NSFJP_SCMP_PX)                                      27
      EQUIVALENCE (COMMEN_SFJP(29),CSFJP_SCMP_PX),(MAP_SFJP(1,29),        27
     *MSFJP_SCMP_PX)                                                      27
      CHARACTER CSFJP_SCMP_PX*40/' '/                                     27
      CHARACTER NSFJP_SCMP_PX*12/'R.SCMP_PX'/                             27
      INTEGER MSFJP_SCMP_PX(2)/1,1/                                       27
      REAL RE_SFJP_SCMP_PX                                                27
C -                                                                       27
      REAL SFJP_SCVM_DZ                                                   27
      EQUIVALENCE (BANK_SFJP(32),SFJP_SCVM_DZ,RE_SFJP_SCVM_DZ),           27
     *(NAMES_SFJP(30),NSFJP_SCVM_DZ)                                      27
      EQUIVALENCE (COMMEN_SFJP(30),CSFJP_SCVM_DZ),(MAP_SFJP(1,30),        27
     *MSFJP_SCVM_DZ)                                                      27
      CHARACTER CSFJP_SCVM_DZ*40/' '/                                     27
      CHARACTER NSFJP_SCVM_DZ*12/'R.SCVM_DZ'/                             27
      INTEGER MSFJP_SCVM_DZ(2)/1,1/                                       27
      REAL RE_SFJP_SCVM_DZ                                                27
C -                                                                       27
      REAL SFJP_SCVM_DX                                                   27
      EQUIVALENCE (BANK_SFJP(33),SFJP_SCVM_DX,RE_SFJP_SCVM_DX),           27
     *(NAMES_SFJP(31),NSFJP_SCVM_DX)                                      27
      EQUIVALENCE (COMMEN_SFJP(31),CSFJP_SCVM_DX),(MAP_SFJP(1,31),        27
     *MSFJP_SCVM_DX)                                                      27
      CHARACTER CSFJP_SCVM_DX*40/' '/                                     27
      CHARACTER NSFJP_SCVM_DX*12/'R.SCVM_DX'/                             27
      INTEGER MSFJP_SCVM_DX(2)/1,1/                                       27
      REAL RE_SFJP_SCVM_DX                                                27
C -                                                                       27
      REAL SFJP_SCVM_PX                                                   27
      EQUIVALENCE (BANK_SFJP(34),SFJP_SCVM_PX,RE_SFJP_SCVM_PX),           27
     *(NAMES_SFJP(32),NSFJP_SCVM_PX)                                      27
      EQUIVALENCE (COMMEN_SFJP(32),CSFJP_SCVM_PX),(MAP_SFJP(1,32),        27
     *MSFJP_SCVM_PX)                                                      27
      CHARACTER CSFJP_SCVM_PX*40/' '/                                     27
      CHARACTER NSFJP_SCVM_PX*12/'R.SCVM_PX'/                             27
      INTEGER MSFJP_SCVM_PX(2)/1,1/                                       27
      REAL RE_SFJP_SCVM_PX                                                27
C -                                                                       27
      REAL SFJP_SCVM_DY                                                   27
      EQUIVALENCE (BANK_SFJP(35),SFJP_SCVM_DY,RE_SFJP_SCVM_DY),           27
     *(NAMES_SFJP(33),NSFJP_SCVM_DY)                                      27
      EQUIVALENCE (COMMEN_SFJP(33),CSFJP_SCVM_DY),(MAP_SFJP(1,33),        27
     *MSFJP_SCVM_DY)                                                      27
      CHARACTER CSFJP_SCVM_DY*40/' '/                                     27
      CHARACTER NSFJP_SCVM_DY*12/'R.SCVM_DY'/                             27
      INTEGER MSFJP_SCVM_DY(2)/1,1/                                       27
      REAL RE_SFJP_SCVM_DY                                                27
C -                                                                       27
      REAL SFJP_SCVM_PZ                                                   27
      EQUIVALENCE (BANK_SFJP(36),SFJP_SCVM_PZ,RE_SFJP_SCVM_PZ),           27
     *(NAMES_SFJP(34),NSFJP_SCVM_PZ)                                      27
      EQUIVALENCE (COMMEN_SFJP(34),CSFJP_SCVM_PZ),(MAP_SFJP(1,34),        27
     *MSFJP_SCVM_PZ)                                                      27
      CHARACTER CSFJP_SCVM_PZ*40/' '/                                     27
      CHARACTER NSFJP_SCVM_PZ*12/'R.SCVM_PZ'/                             27
      INTEGER MSFJP_SCVM_PZ(2)/1,1/                                       27
      REAL RE_SFJP_SCVM_PZ                                                27
C -                                                                       27
      REAL SFJP_SCVB_DY                                                   27
      EQUIVALENCE (BANK_SFJP(37),SFJP_SCVB_DY,RE_SFJP_SCVB_DY),           27
     *(NAMES_SFJP(35),NSFJP_SCVB_DY)                                      27
      EQUIVALENCE (COMMEN_SFJP(35),CSFJP_SCVB_DY),(MAP_SFJP(1,35),        27
     *MSFJP_SCVB_DY)                                                      27
      CHARACTER CSFJP_SCVB_DY*40/' '/                                     27
      CHARACTER NSFJP_SCVB_DY*12/'R.SCVB_DY'/                             27
      INTEGER MSFJP_SCVB_DY(2)/1,1/                                       27
      REAL RE_SFJP_SCVB_DY                                                27
C -                                                                       27
      REAL SFJP_SCVS_DX                                                   27
      EQUIVALENCE (BANK_SFJP(38),SFJP_SCVS_DX,RE_SFJP_SCVS_DX),           27
     *(NAMES_SFJP(36),NSFJP_SCVS_DX)                                      27
      EQUIVALENCE (COMMEN_SFJP(36),CSFJP_SCVS_DX),(MAP_SFJP(1,36),        27
     *MSFJP_SCVS_DX)                                                      27
      CHARACTER CSFJP_SCVS_DX*40/' '/                                     27
      CHARACTER NSFJP_SCVS_DX*12/'R.SCVS_DX'/                             27
      INTEGER MSFJP_SCVS_DX(2)/1,1/                                       27
      REAL RE_SFJP_SCVS_DX                                                27
C -                                                                       27
      REAL SFJP_SCVS_DY                                                   27
      EQUIVALENCE (BANK_SFJP(39),SFJP_SCVS_DY,RE_SFJP_SCVS_DY),           27
     *(NAMES_SFJP(37),NSFJP_SCVS_DY)                                      27
      EQUIVALENCE (COMMEN_SFJP(37),CSFJP_SCVS_DY),(MAP_SFJP(1,37),        27
     *MSFJP_SCVS_DY)                                                      27
      CHARACTER CSFJP_SCVS_DY*40/' '/                                     27
      CHARACTER NSFJP_SCVS_DY*12/'R.SCVS_DY'/                             27
      INTEGER MSFJP_SCVS_DY(2)/1,1/                                       27
      REAL RE_SFJP_SCVS_DY                                                27
C -                                                                       27
      REAL SFJP_SCVS_PX                                                   27
      EQUIVALENCE (BANK_SFJP(40),SFJP_SCVS_PX,RE_SFJP_SCVS_PX),           27
     *(NAMES_SFJP(38),NSFJP_SCVS_PX)                                      27
      EQUIVALENCE (COMMEN_SFJP(38),CSFJP_SCVS_PX),(MAP_SFJP(1,38),        27
     *MSFJP_SCVS_PX)                                                      27
      CHARACTER CSFJP_SCVS_PX*40/' '/                                     27
      CHARACTER NSFJP_SCVS_PX*12/'R.SCVS_PX'/                             27
      INTEGER MSFJP_SCVS_PX(2)/1,1/                                       27
      REAL RE_SFJP_SCVS_PX                                                27
C -                                                                       27
      REAL SFJP_SCVS_PY                                                   27
      EQUIVALENCE (BANK_SFJP(41),SFJP_SCVS_PY,RE_SFJP_SCVS_PY),           27
     *(NAMES_SFJP(39),NSFJP_SCVS_PY)                                      27
      EQUIVALENCE (COMMEN_SFJP(39),CSFJP_SCVS_PY),(MAP_SFJP(1,39),        27
     *MSFJP_SCVS_PY)                                                      27
      CHARACTER CSFJP_SCVS_PY*40/' '/                                     27
      CHARACTER NSFJP_SCVS_PY*12/'R.SCVS_PY'/                             27
      INTEGER MSFJP_SCVS_PY(2)/1,1/                                       27
      REAL RE_SFJP_SCVS_PY                                                27
C -                                                                       27
      REAL SFJP_SFCO_DX                                                   27
      EQUIVALENCE (BANK_SFJP(42),SFJP_SFCO_DX,RE_SFJP_SFCO_DX),           27
     *(NAMES_SFJP(40),NSFJP_SFCO_DX)                                      27
      EQUIVALENCE (COMMEN_SFJP(40),CSFJP_SFCO_DX),(MAP_SFJP(1,40),        27
     *MSFJP_SFCO_DX)                                                      27
      CHARACTER CSFJP_SFCO_DX*40/' '/                                     27
      CHARACTER NSFJP_SFCO_DX*12/'R.SFCO_DX'/                             27
      INTEGER MSFJP_SFCO_DX(2)/1,1/                                       27
      REAL RE_SFJP_SFCO_DX                                                27
C -                                                                       27
      REAL SFJP_SFCO_DY                                                   27
      EQUIVALENCE (BANK_SFJP(43),SFJP_SFCO_DY,RE_SFJP_SFCO_DY),           27
     *(NAMES_SFJP(41),NSFJP_SFCO_DY)                                      27
      EQUIVALENCE (COMMEN_SFJP(41),CSFJP_SFCO_DY),(MAP_SFJP(1,41),        27
     *MSFJP_SFCO_DY)                                                      27
      CHARACTER CSFJP_SFCO_DY*40/' '/                                     27
      CHARACTER NSFJP_SFCO_DY*12/'R.SFCO_DY'/                             27
      INTEGER MSFJP_SFCO_DY(2)/1,1/                                       27
      REAL RE_SFJP_SFCO_DY                                                27
C -                                                                       27
      REAL SFJP_SFCO_DZ                                                   27
      EQUIVALENCE (BANK_SFJP(44),SFJP_SFCO_DZ,RE_SFJP_SFCO_DZ),           27
     *(NAMES_SFJP(42),NSFJP_SFCO_DZ)                                      27
      EQUIVALENCE (COMMEN_SFJP(42),CSFJP_SFCO_DZ),(MAP_SFJP(1,42),        27
     *MSFJP_SFCO_DZ)                                                      27
      CHARACTER CSFJP_SFCO_DZ*40/' '/                                     27
      CHARACTER NSFJP_SFCO_DZ*12/'R.SFCO_DZ'/                             27
      INTEGER MSFJP_SFCO_DZ(2)/1,1/                                       27
      REAL RE_SFJP_SFCO_DZ                                                27
C -                                                                       27
      REAL SFJP_SFCO_PX                                                   27
      EQUIVALENCE (BANK_SFJP(45),SFJP_SFCO_PX,RE_SFJP_SFCO_PX),           27
     *(NAMES_SFJP(43),NSFJP_SFCO_PX)                                      27
      EQUIVALENCE (COMMEN_SFJP(43),CSFJP_SFCO_PX),(MAP_SFJP(1,43),        27
     *MSFJP_SFCO_PX)                                                      27
      CHARACTER CSFJP_SFCO_PX*40/' '/                                     27
      CHARACTER NSFJP_SFCO_PX*12/'R.SFCO_PX'/                             27
      INTEGER MSFJP_SFCO_PX(2)/1,1/                                       27
      REAL RE_SFJP_SFCO_PX                                                27
C -                                                                       27
      REAL SFJP_SFCO_PY                                                   27
      EQUIVALENCE (BANK_SFJP(46),SFJP_SFCO_PY,RE_SFJP_SFCO_PY),           27
     *(NAMES_SFJP(44),NSFJP_SFCO_PY)                                      27
      EQUIVALENCE (COMMEN_SFJP(44),CSFJP_SFCO_PY),(MAP_SFJP(1,44),        27
     *MSFJP_SFCO_PY)                                                      27
      CHARACTER CSFJP_SFCO_PY*40/' '/                                     27
      CHARACTER NSFJP_SFCO_PY*12/'R.SFCO_PY'/                             27
      INTEGER MSFJP_SFCO_PY(2)/1,1/                                       27
      REAL RE_SFJP_SFCO_PY                                                27
C -                                                                       27
      REAL SFJP_SFCM_DX                                                   27
      EQUIVALENCE (BANK_SFJP(47),SFJP_SFCM_DX,RE_SFJP_SFCM_DX),           27
     *(NAMES_SFJP(45),NSFJP_SFCM_DX)                                      27
      EQUIVALENCE (COMMEN_SFJP(45),CSFJP_SFCM_DX),(MAP_SFJP(1,45),        27
     *MSFJP_SFCM_DX)                                                      27
      CHARACTER CSFJP_SFCM_DX*40/' '/                                     27
      CHARACTER NSFJP_SFCM_DX*12/'R.SFCM_DX'/                             27
      INTEGER MSFJP_SFCM_DX(2)/1,1/                                       27
      REAL RE_SFJP_SFCM_DX                                                27
C -                                                                       27
      REAL SFJP_SFCS_DZ                                                   27
      EQUIVALENCE (BANK_SFJP(48),SFJP_SFCS_DZ,RE_SFJP_SFCS_DZ),           27
     *(NAMES_SFJP(46),NSFJP_SFCS_DZ)                                      27
      EQUIVALENCE (COMMEN_SFJP(46),CSFJP_SFCS_DZ),(MAP_SFJP(1,46),        27
     *MSFJP_SFCS_DZ)                                                      27
      CHARACTER CSFJP_SFCS_DZ*40/' '/                                     27
      CHARACTER NSFJP_SFCS_DZ*12/'R.SFCS_DZ'/                             27
      INTEGER MSFJP_SFCS_DZ(2)/1,1/                                       27
      REAL RE_SFJP_SFCS_DZ                                                27
C -                                                                       27
      REAL SFJP_SFKF_DY                                                   27
      EQUIVALENCE (BANK_SFJP(49),SFJP_SFKF_DY,RE_SFJP_SFKF_DY),           27
     *(NAMES_SFJP(47),NSFJP_SFKF_DY)                                      27
      EQUIVALENCE (COMMEN_SFJP(47),CSFJP_SFKF_DY),(MAP_SFJP(1,47),        27
     *MSFJP_SFKF_DY)                                                      27
      CHARACTER CSFJP_SFKF_DY*40/' '/                                     27
      CHARACTER NSFJP_SFKF_DY*12/'R.SFKF_DY'/                             27
      INTEGER MSFJP_SFKF_DY(2)/1,1/                                       27
      REAL RE_SFJP_SFKF_DY                                                27
C -                                                                       27
      REAL SFJP_SFKF_DZ                                                   27
      EQUIVALENCE (BANK_SFJP(50),SFJP_SFKF_DZ,RE_SFJP_SFKF_DZ),           27
     *(NAMES_SFJP(48),NSFJP_SFKF_DZ)                                      27
      EQUIVALENCE (COMMEN_SFJP(48),CSFJP_SFKF_DZ),(MAP_SFJP(1,48),        27
     *MSFJP_SFKF_DZ)                                                      27
      CHARACTER CSFJP_SFKF_DZ*40/' '/                                     27
      CHARACTER NSFJP_SFKF_DZ*12/'R.SFKF_DZ'/                             27
      INTEGER MSFJP_SFKF_DZ(2)/1,1/                                       27
      REAL RE_SFJP_SFKF_DZ                                                27
C -                                                                       27
      REAL SFJP_SFKF_DX                                                   27
      EQUIVALENCE (BANK_SFJP(51),SFJP_SFKF_DX,RE_SFJP_SFKF_DX),           27
     *(NAMES_SFJP(49),NSFJP_SFKF_DX)                                      27
      EQUIVALENCE (COMMEN_SFJP(49),CSFJP_SFKF_DX),(MAP_SFJP(1,49),        27
     *MSFJP_SFKF_DX)                                                      27
      CHARACTER CSFJP_SFKF_DX*40/' '/                                     27
      CHARACTER NSFJP_SFKF_DX*12/'R.SFKF_DX'/                             27
      INTEGER MSFJP_SFKF_DX(2)/1,1/                                       27
      REAL RE_SFJP_SFKF_DX                                                27
C -                                                                       27
      REAL SFJP_SFKF_PX                                                   27
      EQUIVALENCE (BANK_SFJP(52),SFJP_SFKF_PX,RE_SFJP_SFKF_PX),           27
     *(NAMES_SFJP(50),NSFJP_SFKF_PX)                                      27
      EQUIVALENCE (COMMEN_SFJP(50),CSFJP_SFKF_PX),(MAP_SFJP(1,50),        27
     *MSFJP_SFKF_PX)                                                      27
      CHARACTER CSFJP_SFKF_PX*40/' '/                                     27
      CHARACTER NSFJP_SFKF_PX*12/'R.SFKF_PX'/                             27
      INTEGER MSFJP_SFKF_PX(2)/1,1/                                       27
      REAL RE_SFJP_SFKF_PX                                                27
C -                                                                       27
      REAL SFJP_SFKF_PY                                                   27
      EQUIVALENCE (BANK_SFJP(53),SFJP_SFKF_PY,RE_SFJP_SFKF_PY),           27
     *(NAMES_SFJP(51),NSFJP_SFKF_PY)                                      27
      EQUIVALENCE (COMMEN_SFJP(51),CSFJP_SFKF_PY),(MAP_SFJP(1,51),        27
     *MSFJP_SFKF_PY)                                                      27
      CHARACTER CSFJP_SFKF_PY*40/' '/                                     27
      CHARACTER NSFJP_SFKF_PY*12/'R.SFKF_PY'/                             27
      INTEGER MSFJP_SFKF_PY(2)/1,1/                                       27
      REAL RE_SFJP_SFKF_PY                                                27
C -                                                                       27
      REAL SFJP_SFKS_DX                                                   27
      EQUIVALENCE (BANK_SFJP(54),SFJP_SFKS_DX,RE_SFJP_SFKS_DX),           27
     *(NAMES_SFJP(52),NSFJP_SFKS_DX)                                      27
      EQUIVALENCE (COMMEN_SFJP(52),CSFJP_SFKS_DX),(MAP_SFJP(1,52),        27
     *MSFJP_SFKS_DX)                                                      27
      CHARACTER CSFJP_SFKS_DX*40/' '/                                     27
      CHARACTER NSFJP_SFKS_DX*12/'R.SFKS_DX'/                             27
      INTEGER MSFJP_SFKS_DX(2)/1,1/                                       27
      REAL RE_SFJP_SFKS_DX                                                27
C -                                                                       27
      REAL SFJP_SFKS_PX                                                   27
      EQUIVALENCE (BANK_SFJP(55),SFJP_SFKS_PX,RE_SFJP_SFKS_PX),           27
     *(NAMES_SFJP(53),NSFJP_SFKS_PX)                                      27
      EQUIVALENCE (COMMEN_SFJP(53),CSFJP_SFKS_PX),(MAP_SFJP(1,53),        27
     *MSFJP_SFKS_PX)                                                      27
      CHARACTER CSFJP_SFKS_PX*40/' '/                                     27
      CHARACTER NSFJP_SFKS_PX*12/'R.SFKS_PX'/                             27
      INTEGER MSFJP_SFKS_PX(2)/1,1/                                       27
      REAL RE_SFJP_SFKS_PX                                                27
C -                                                                       27
      REAL SFJP_SFKS_PY                                                   27
      EQUIVALENCE (BANK_SFJP(56),SFJP_SFKS_PY,RE_SFJP_SFKS_PY),           27
     *(NAMES_SFJP(54),NSFJP_SFKS_PY)                                      27
      EQUIVALENCE (COMMEN_SFJP(54),CSFJP_SFKS_PY),(MAP_SFJP(1,54),        27
     *MSFJP_SFKS_PY)                                                      27
      CHARACTER CSFJP_SFKS_PY*40/' '/                                     27
      CHARACTER NSFJP_SFKS_PY*12/'R.SFKS_PY'/                             27
      INTEGER MSFJP_SFKS_PY(2)/1,1/                                       27
      REAL RE_SFJP_SFKS_PY                                                27
C -                                                                       27
      REAL SFJP_SFPR_PY                                                   27
      EQUIVALENCE (BANK_SFJP(57),SFJP_SFPR_PY,RE_SFJP_SFPR_PY),           27
     *(NAMES_SFJP(55),NSFJP_SFPR_PY)                                      27
      EQUIVALENCE (COMMEN_SFJP(55),CSFJP_SFPR_PY),(MAP_SFJP(1,55),        27
     *MSFJP_SFPR_PY)                                                      27
      CHARACTER CSFJP_SFPR_PY*40/' '/                                     27
      CHARACTER NSFJP_SFPR_PY*12/'R.SFPR_PY'/                             27
      INTEGER MSFJP_SFPR_PY(2)/1,1/                                       27
      REAL RE_SFJP_SFPR_PY                                                27
C -                                                                       27
      REAL SFJP_SFPB_PY                                                   27
      EQUIVALENCE (BANK_SFJP(58),SFJP_SFPB_PY,RE_SFJP_SFPB_PY),           27
     *(NAMES_SFJP(56),NSFJP_SFPB_PY)                                      27
      EQUIVALENCE (COMMEN_SFJP(56),CSFJP_SFPB_PY),(MAP_SFJP(1,56),        27
     *MSFJP_SFPB_PY)                                                      27
      CHARACTER CSFJP_SFPB_PY*40/' '/                                     27
      CHARACTER NSFJP_SFPB_PY*12/'R.SFPB_PY'/                             27
      INTEGER MSFJP_SFPB_PY(2)/1,1/                                       27
      REAL RE_SFJP_SFPB_PY                                                27
C -                                                                       27
      REAL SFJP_SFPB_PY2                                                  27
      EQUIVALENCE (BANK_SFJP(59),SFJP_SFPB_PY2,RE_SFJP_SFPB_PY2),         27
     *(NAMES_SFJP(57),NSFJP_SFPB_PY2)                                     27
      EQUIVALENCE (COMMEN_SFJP(57),CSFJP_SFPB_PY2),(MAP_SFJP(1,57),       27
     *MSFJP_SFPB_PY2)                                                     27
      CHARACTER CSFJP_SFPB_PY2*40/' '/                                    27
      CHARACTER NSFJP_SFPB_PY2*12/'R.SFPB_PY2'/                           27
      INTEGER MSFJP_SFPB_PY2(2)/1,1/                                      27
      REAL RE_SFJP_SFPB_PY2                                               27
C -                                                                       27
      REAL SFJP_SFCB_DX                                                   27
      EQUIVALENCE (BANK_SFJP(60),SFJP_SFCB_DX,RE_SFJP_SFCB_DX),           27
     *(NAMES_SFJP(58),NSFJP_SFCB_DX)                                      27
      EQUIVALENCE (COMMEN_SFJP(58),CSFJP_SFCB_DX),(MAP_SFJP(1,58),        27
     *MSFJP_SFCB_DX)                                                      27
      CHARACTER CSFJP_SFCB_DX*40/' '/                                     27
      CHARACTER NSFJP_SFCB_DX*12/'R.SFCB_DX'/                             27
      INTEGER MSFJP_SFCB_DX(2)/1,1/                                       27
      REAL RE_SFJP_SFCB_DX                                                27
C -                                                                       27
      REAL SFJP_SSBS_DY                                                   27
      EQUIVALENCE (BANK_SFJP(61),SFJP_SSBS_DY,RE_SFJP_SSBS_DY),           27
     *(NAMES_SFJP(59),NSFJP_SSBS_DY)                                      27
      EQUIVALENCE (COMMEN_SFJP(59),CSFJP_SSBS_DY),(MAP_SFJP(1,59),        27
     *MSFJP_SSBS_DY)                                                      27
      CHARACTER CSFJP_SSBS_DY*40/' '/                                     27
      CHARACTER NSFJP_SSBS_DY*12/'R.SSBS_DY'/                             27
      INTEGER MSFJP_SSBS_DY(2)/1,1/                                       27
      REAL RE_SFJP_SSBS_DY                                                27
C -                                                                       27
      REAL SFJP_SSBS_DX                                                   27
      EQUIVALENCE (BANK_SFJP(62),SFJP_SSBS_DX,RE_SFJP_SSBS_DX),           27
     *(NAMES_SFJP(60),NSFJP_SSBS_DX)                                      27
      EQUIVALENCE (COMMEN_SFJP(60),CSFJP_SSBS_DX),(MAP_SFJP(1,60),        27
     *MSFJP_SSBS_DX)                                                      27
      CHARACTER CSFJP_SSBS_DX*40/' '/                                     27
      CHARACTER NSFJP_SSBS_DX*12/'R.SSBS_DX'/                             27
      INTEGER MSFJP_SSBS_DX(2)/1,1/                                       27
      REAL RE_SFJP_SSBS_DX                                                27
C -                                                                       27
      REAL SFJP_SSBB_DX                                                   27
      EQUIVALENCE (BANK_SFJP(63),SFJP_SSBB_DX,RE_SFJP_SSBB_DX),           27
     *(NAMES_SFJP(61),NSFJP_SSBB_DX)                                      27
      EQUIVALENCE (COMMEN_SFJP(61),CSFJP_SSBB_DX),(MAP_SFJP(1,61),        27
     *MSFJP_SSBB_DX)                                                      27
      CHARACTER CSFJP_SSBB_DX*40/' '/                                     27
      CHARACTER NSFJP_SSBB_DX*12/'R.SSBB_DX'/                             27
      INTEGER MSFJP_SSBB_DX(2)/1,1/                                       27
      REAL RE_SFJP_SSBB_DX                                                27
C -                                                                       27
      REAL SFJP_SSBB_DZ                                                   27
      EQUIVALENCE (BANK_SFJP(64),SFJP_SSBB_DZ,RE_SFJP_SSBB_DZ),           27
     *(NAMES_SFJP(62),NSFJP_SSBB_DZ)                                      27
      EQUIVALENCE (COMMEN_SFJP(62),CSFJP_SSBB_DZ),(MAP_SFJP(1,62),        27
     *MSFJP_SSBB_DZ)                                                      27
      CHARACTER CSFJP_SSBB_DZ*40/' '/                                     27
      CHARACTER NSFJP_SSBB_DZ*12/'R.SSBB_DZ'/                             27
      INTEGER MSFJP_SSBB_DZ(2)/1,1/                                       27
      REAL RE_SFJP_SSBB_DZ                                                27
C -                                                                       27
      REAL SFJP_FLEX_DI                                                   27
      EQUIVALENCE (BANK_SFJP(65),SFJP_FLEX_DI,RE_SFJP_FLEX_DI),           27
     *(NAMES_SFJP(63),NSFJP_FLEX_DI)                                      27
      EQUIVALENCE (COMMEN_SFJP(63),CSFJP_FLEX_DI),(MAP_SFJP(1,63),        27
     *MSFJP_FLEX_DI)                                                      27
      CHARACTER CSFJP_FLEX_DI*40/' '/                                     27
      CHARACTER NSFJP_FLEX_DI*12/'R.FLEX_DI'/                             27
      INTEGER MSFJP_FLEX_DI(2)/1,1/                                       27
      REAL RE_SFJP_FLEX_DI                                                27
C -                                                                       27
      REAL SFJP_SFPB_DX                                                   27
      EQUIVALENCE (BANK_SFJP(66),SFJP_SFPB_DX,RE_SFJP_SFPB_DX),           27
     *(NAMES_SFJP(64),NSFJP_SFPB_DX)                                      27
      EQUIVALENCE (COMMEN_SFJP(64),CSFJP_SFPB_DX),(MAP_SFJP(1,64),        27
     *MSFJP_SFPB_DX)                                                      27
      CHARACTER CSFJP_SFPB_DX*40/' '/                                     27
      CHARACTER NSFJP_SFPB_DX*12/'R.SFPB_DX'/                             27
      INTEGER MSFJP_SFPB_DX(2)/1,1/                                       27
      REAL RE_SFJP_SFPB_DX                                                27
C -                                                                       27
      REAL SFJP_SFPB_DY                                                   27
      EQUIVALENCE (BANK_SFJP(67),SFJP_SFPB_DY,RE_SFJP_SFPB_DY),           27
     *(NAMES_SFJP(65),NSFJP_SFPB_DY)                                      27
      EQUIVALENCE (COMMEN_SFJP(65),CSFJP_SFPB_DY),(MAP_SFJP(1,65),        27
     *MSFJP_SFPB_DY)                                                      27
      CHARACTER CSFJP_SFPB_DY*40/' '/                                     27
      CHARACTER NSFJP_SFPB_DY*12/'R.SFPB_DY'/                             27
      INTEGER MSFJP_SFPB_DY(2)/1,1/                                       27
      REAL RE_SFJP_SFPB_DY                                                27
C -                                                                       27
      REAL SFJP_SFPBDZ                                                    27
      EQUIVALENCE (BANK_SFJP(68),SFJP_SFPBDZ,RE_SFJP_SFPBDZ),             27
     *(NAMES_SFJP(66),NSFJP_SFPBDZ)                                       27
      EQUIVALENCE (COMMEN_SFJP(66),CSFJP_SFPBDZ),(MAP_SFJP(1,66),         27
     *MSFJP_SFPBDZ)                                                       27
      CHARACTER CSFJP_SFPBDZ*40/' '/                                      27
      CHARACTER NSFJP_SFPBDZ*12/'R.SFPBDZ'/                               27
      INTEGER MSFJP_SFPBDZ(2)/1,1/                                        27
      REAL RE_SFJP_SFPBDZ                                                 27
C -                                                                       27
      REAL SFJP_SAPP_DXE                                                  27
      EQUIVALENCE (BANK_SFJP(69),SFJP_SAPP_DXE,RE_SFJP_SAPP_DXE),         27
     *(NAMES_SFJP(67),NSFJP_SAPP_DXE)                                     27
      EQUIVALENCE (COMMEN_SFJP(67),CSFJP_SAPP_DXE),(MAP_SFJP(1,67),       27
     *MSFJP_SAPP_DXE)                                                     27
      CHARACTER CSFJP_SAPP_DXE*40/' '/                                    27
      CHARACTER NSFJP_SAPP_DXE*12/'R.SAPP_DXE'/                           27
      INTEGER MSFJP_SAPP_DXE(2)/1,1/                                      27
      REAL RE_SFJP_SAPP_DXE                                               27
C -                                                                       27
      REAL SFJP_SAPP_DXZ                                                  27
      EQUIVALENCE (BANK_SFJP(70),SFJP_SAPP_DXZ,RE_SFJP_SAPP_DXZ),         27
     *(NAMES_SFJP(68),NSFJP_SAPP_DXZ)                                     27
      EQUIVALENCE (COMMEN_SFJP(68),CSFJP_SAPP_DXZ),(MAP_SFJP(1,68),       27
     *MSFJP_SAPP_DXZ)                                                     27
      CHARACTER CSFJP_SAPP_DXZ*40/' '/                                    27
      CHARACTER NSFJP_SAPP_DXZ*12/'R.SAPP_DXZ'/                           27
      INTEGER MSFJP_SAPP_DXZ(2)/1,1/                                      27
      REAL RE_SFJP_SAPP_DXZ                                               27
C -                                                                       27
      REAL SFJP_SAPP_DY                                                   27
      EQUIVALENCE (BANK_SFJP(71),SFJP_SAPP_DY,RE_SFJP_SAPP_DY),           27
     *(NAMES_SFJP(69),NSFJP_SAPP_DY)                                      27
      EQUIVALENCE (COMMEN_SFJP(69),CSFJP_SAPP_DY),(MAP_SFJP(1,69),        27
     *MSFJP_SAPP_DY)                                                      27
      CHARACTER CSFJP_SAPP_DY*40/' '/                                     27
      CHARACTER NSFJP_SAPP_DY*12/'R.SAPP_DY'/                             27
      INTEGER MSFJP_SAPP_DY(2)/1,1/                                       27
      REAL RE_SFJP_SAPP_DY                                                27
C -                                                                       27
      REAL SFJP_SAPP_DZ                                                   27
      EQUIVALENCE (BANK_SFJP(72),SFJP_SAPP_DZ,RE_SFJP_SAPP_DZ),           27
     *(NAMES_SFJP(70),NSFJP_SAPP_DZ)                                      27
      EQUIVALENCE (COMMEN_SFJP(70),CSFJP_SAPP_DZ),(MAP_SFJP(1,70),        27
     *MSFJP_SAPP_DZ)                                                      27
      CHARACTER CSFJP_SAPP_DZ*40/' '/                                     27
      CHARACTER NSFJP_SAPP_DZ*12/'R.SAPP_DZ'/                             27
      INTEGER MSFJP_SAPP_DZ(2)/1,1/                                       27
      REAL RE_SFJP_SAPP_DZ                                                27
C -                                                                       27
      REAL SFJP_SAPP_PY1                                                  27
      EQUIVALENCE (BANK_SFJP(73),SFJP_SAPP_PY1,RE_SFJP_SAPP_PY1),         27
     *(NAMES_SFJP(71),NSFJP_SAPP_PY1)                                     27
      EQUIVALENCE (COMMEN_SFJP(71),CSFJP_SAPP_PY1),(MAP_SFJP(1,71),       27
     *MSFJP_SAPP_PY1)                                                     27
      CHARACTER CSFJP_SAPP_PY1*40/' '/                                    27
      CHARACTER NSFJP_SAPP_PY1*12/'R.SAPP_PY1'/                           27
      INTEGER MSFJP_SAPP_PY1(2)/1,1/                                      27
      REAL RE_SFJP_SAPP_PY1                                               27
C -                                                                       27
      REAL SFJP_SAPP_PY2                                                  27
      EQUIVALENCE (BANK_SFJP(74),SFJP_SAPP_PY2,RE_SFJP_SAPP_PY2),         27
     *(NAMES_SFJP(72),NSFJP_SAPP_PY2)                                     27
      EQUIVALENCE (COMMEN_SFJP(72),CSFJP_SAPP_PY2),(MAP_SFJP(1,72),       27
     *MSFJP_SAPP_PY2)                                                     27
      CHARACTER CSFJP_SAPP_PY2*40/' '/                                    27
      CHARACTER NSFJP_SAPP_PY2*12/'R.SAPP_PY2'/                           27
      INTEGER MSFJP_SAPP_PY2(2)/1,1/                                      27
      REAL RE_SFJP_SAPP_PY2                                               27
C -                                                                       27
      REAL SFJP_SAPP_PY3                                                  27
      EQUIVALENCE (BANK_SFJP(75),SFJP_SAPP_PY3,RE_SFJP_SAPP_PY3),         27
     *(NAMES_SFJP(73),NSFJP_SAPP_PY3)                                     27
      EQUIVALENCE (COMMEN_SFJP(73),CSFJP_SAPP_PY3),(MAP_SFJP(1,73),       27
     *MSFJP_SAPP_PY3)                                                     27
      CHARACTER CSFJP_SAPP_PY3*40/' '/                                    27
      CHARACTER NSFJP_SAPP_PY3*12/'R.SAPP_PY3'/                           27
      INTEGER MSFJP_SAPP_PY3(2)/1,1/                                      27
      REAL RE_SFJP_SAPP_PY3                                               27
C -                                                                       27
      REAL SFJP_SFAM_DXE                                                  27
      EQUIVALENCE (BANK_SFJP(76),SFJP_SFAM_DXE,RE_SFJP_SFAM_DXE),         27
     *(NAMES_SFJP(74),NSFJP_SFAM_DXE)                                     27
      EQUIVALENCE (COMMEN_SFJP(74),CSFJP_SFAM_DXE),(MAP_SFJP(1,74),       27
     *MSFJP_SFAM_DXE)                                                     27
      CHARACTER CSFJP_SFAM_DXE*40/' '/                                    27
      CHARACTER NSFJP_SFAM_DXE*12/'R.SFAM_DXE'/                           27
      INTEGER MSFJP_SFAM_DXE(2)/1,1/                                      27
      REAL RE_SFJP_SFAM_DXE                                               27
C -                                                                       27
      REAL SFJP_SFAM_DXZ                                                  27
      EQUIVALENCE (BANK_SFJP(77),SFJP_SFAM_DXZ,RE_SFJP_SFAM_DXZ),         27
     *(NAMES_SFJP(75),NSFJP_SFAM_DXZ)                                     27
      EQUIVALENCE (COMMEN_SFJP(75),CSFJP_SFAM_DXZ),(MAP_SFJP(1,75),       27
     *MSFJP_SFAM_DXZ)                                                     27
      CHARACTER CSFJP_SFAM_DXZ*40/' '/                                    27
      CHARACTER NSFJP_SFAM_DXZ*12/'R.SFAM_DXZ'/                           27
      INTEGER MSFJP_SFAM_DXZ(2)/1,1/                                      27
      REAL RE_SFJP_SFAM_DXZ                                               27
C -                                                                       27
      REAL SFJP_SFAM_DY                                                   27
      EQUIVALENCE (BANK_SFJP(78),SFJP_SFAM_DY,RE_SFJP_SFAM_DY),           27
     *(NAMES_SFJP(76),NSFJP_SFAM_DY)                                      27
      EQUIVALENCE (COMMEN_SFJP(76),CSFJP_SFAM_DY),(MAP_SFJP(1,76),        27
     *MSFJP_SFAM_DY)                                                      27
      CHARACTER CSFJP_SFAM_DY*40/' '/                                     27
      CHARACTER NSFJP_SFAM_DY*12/'R.SFAM_DY'/                             27
      INTEGER MSFJP_SFAM_DY(2)/1,1/                                       27
      REAL RE_SFJP_SFAM_DY                                                27
C -                                                                       27
      REAL SFJP_SFAM_DZ                                                   27
      EQUIVALENCE (BANK_SFJP(79),SFJP_SFAM_DZ,RE_SFJP_SFAM_DZ),           27
     *(NAMES_SFJP(77),NSFJP_SFAM_DZ)                                      27
      EQUIVALENCE (COMMEN_SFJP(77),CSFJP_SFAM_DZ),(MAP_SFJP(1,77),        27
     *MSFJP_SFAM_DZ)                                                      27
      CHARACTER CSFJP_SFAM_DZ*40/' '/                                     27
      CHARACTER NSFJP_SFAM_DZ*12/'R.SFAM_DZ'/                             27
      INTEGER MSFJP_SFAM_DZ(2)/1,1/                                       27
      REAL RE_SFJP_SFAM_DZ                                                27
C -                                                                       27
      REAL SFJP_SFAM_DZS                                                  27
      EQUIVALENCE (BANK_SFJP(80),SFJP_SFAM_DZS,RE_SFJP_SFAM_DZS),         27
     *(NAMES_SFJP(78),NSFJP_SFAM_DZS)                                     27
      EQUIVALENCE (COMMEN_SFJP(78),CSFJP_SFAM_DZS),(MAP_SFJP(1,78),       27
     *MSFJP_SFAM_DZS)                                                     27
      CHARACTER CSFJP_SFAM_DZS*40/' '/                                    27
      CHARACTER NSFJP_SFAM_DZS*12/'R.SFAM_DZS'/                           27
      INTEGER MSFJP_SFAM_DZS(2)/1,1/                                      27
      REAL RE_SFJP_SFAM_DZS                                               27
C -                                                                       27
      REAL SFJP_SFLA_DX                                                   27
      EQUIVALENCE (BANK_SFJP(81),SFJP_SFLA_DX,RE_SFJP_SFLA_DX),           27
     *(NAMES_SFJP(79),NSFJP_SFLA_DX)                                      27
      EQUIVALENCE (COMMEN_SFJP(79),CSFJP_SFLA_DX),(MAP_SFJP(1,79),        27
     *MSFJP_SFLA_DX)                                                      27
      CHARACTER CSFJP_SFLA_DX*40/' '/                                     27
      CHARACTER NSFJP_SFLA_DX*12/'R.SFLA_DX'/                             27
      INTEGER MSFJP_SFLA_DX(2)/1,1/                                       27
      REAL RE_SFJP_SFLA_DX                                                27
C -                                                                       27
      REAL SFJP_SFLA_DY                                                   27
      EQUIVALENCE (BANK_SFJP(82),SFJP_SFLA_DY,RE_SFJP_SFLA_DY),           27
     *(NAMES_SFJP(80),NSFJP_SFLA_DY)                                      27
      EQUIVALENCE (COMMEN_SFJP(80),CSFJP_SFLA_DY),(MAP_SFJP(1,80),        27
     *MSFJP_SFLA_DY)                                                      27
      CHARACTER CSFJP_SFLA_DY*40/' '/                                     27
      CHARACTER NSFJP_SFLA_DY*12/'R.SFLA_DY'/                             27
      INTEGER MSFJP_SFLA_DY(2)/1,1/                                       27
      REAL RE_SFJP_SFLA_DY                                                27
C -                                                                       27
      REAL SFJP_SFLA_DZ                                                   27
      EQUIVALENCE (BANK_SFJP(83),SFJP_SFLA_DZ,RE_SFJP_SFLA_DZ),           27
     *(NAMES_SFJP(81),NSFJP_SFLA_DZ)                                      27
      EQUIVALENCE (COMMEN_SFJP(81),CSFJP_SFLA_DZ),(MAP_SFJP(1,81),        27
     *MSFJP_SFLA_DZ)                                                      27
      CHARACTER CSFJP_SFLA_DZ*40/' '/                                     27
      CHARACTER NSFJP_SFLA_DZ*12/'R.SFLA_DZ'/                             27
      INTEGER MSFJP_SFLA_DZ(2)/1,1/                                       27
      REAL RE_SFJP_SFLA_DZ                                                27
C -                                                                       27
      REAL SFJP_SFLB_DZ                                                   27
      EQUIVALENCE (BANK_SFJP(84),SFJP_SFLB_DZ,RE_SFJP_SFLB_DZ),           27
     *(NAMES_SFJP(82),NSFJP_SFLB_DZ)                                      27
      EQUIVALENCE (COMMEN_SFJP(82),CSFJP_SFLB_DZ),(MAP_SFJP(1,82),        27
     *MSFJP_SFLB_DZ)                                                      27
      CHARACTER CSFJP_SFLB_DZ*40/' '/                                     27
      CHARACTER NSFJP_SFLB_DZ*12/'R.SFLB_DZ'/                             27
      INTEGER MSFJP_SFLB_DZ(2)/1,1/                                       27
      REAL RE_SFJP_SFLB_DZ                                                27
C -                                                                       27
      REAL SFJP_SFLC_DZ                                                   27
      EQUIVALENCE (BANK_SFJP(85),SFJP_SFLC_DZ,RE_SFJP_SFLC_DZ),           27
     *(NAMES_SFJP(83),NSFJP_SFLC_DZ)                                      27
      EQUIVALENCE (COMMEN_SFJP(83),CSFJP_SFLC_DZ),(MAP_SFJP(1,83),        27
     *MSFJP_SFLC_DZ)                                                      27
      CHARACTER CSFJP_SFLC_DZ*40/' '/                                     27
      CHARACTER NSFJP_SFLC_DZ*12/'R.SFLC_DZ'/                             27
      INTEGER MSFJP_SFLC_DZ(2)/1,1/                                       27
      REAL RE_SFJP_SFLC_DZ                                                27
C -                                                                       27
      REAL SFJP_SFEB_DX                                                   27
      EQUIVALENCE (BANK_SFJP(86),SFJP_SFEB_DX,RE_SFJP_SFEB_DX),           27
     *(NAMES_SFJP(84),NSFJP_SFEB_DX)                                      27
      EQUIVALENCE (COMMEN_SFJP(84),CSFJP_SFEB_DX),(MAP_SFJP(1,84),        27
     *MSFJP_SFEB_DX)                                                      27
      CHARACTER CSFJP_SFEB_DX*40/' '/                                     27
      CHARACTER NSFJP_SFEB_DX*12/'R.SFEB_DX'/                             27
      INTEGER MSFJP_SFEB_DX(2)/1,1/                                       27
      REAL RE_SFJP_SFEB_DX                                                27
C -                                                                       27
      REAL SFJP_SFEB_DZ                                                   27
      EQUIVALENCE (BANK_SFJP(87),SFJP_SFEB_DZ,RE_SFJP_SFEB_DZ),           27
     *(NAMES_SFJP(85),NSFJP_SFEB_DZ)                                      27
      EQUIVALENCE (COMMEN_SFJP(85),CSFJP_SFEB_DZ),(MAP_SFJP(1,85),        27
     *MSFJP_SFEB_DZ)                                                      27
      CHARACTER CSFJP_SFEB_DZ*40/' '/                                     27
      CHARACTER NSFJP_SFEB_DZ*12/'R.SFEB_DZ'/                             27
      INTEGER MSFJP_SFEB_DZ(2)/1,1/                                       27
      REAL RE_SFJP_SFEB_DZ                                                27
C -                                                                       27
      REAL SFJP_SFES_DX                                                   27
      EQUIVALENCE (BANK_SFJP(88),SFJP_SFES_DX,RE_SFJP_SFES_DX),           27
     *(NAMES_SFJP(86),NSFJP_SFES_DX)                                      27
      EQUIVALENCE (COMMEN_SFJP(86),CSFJP_SFES_DX),(MAP_SFJP(1,86),        27
     *MSFJP_SFES_DX)                                                      27
      CHARACTER CSFJP_SFES_DX*40/' '/                                     27
      CHARACTER NSFJP_SFES_DX*12/'R.SFES_DX'/                             27
      INTEGER MSFJP_SFES_DX(2)/1,1/                                       27
      REAL RE_SFJP_SFES_DX                                                27
C -                                                                       27
      REAL SFJP_SFFK_DXE                                                  27
      EQUIVALENCE (BANK_SFJP(89),SFJP_SFFK_DXE,RE_SFJP_SFFK_DXE),         27
     *(NAMES_SFJP(87),NSFJP_SFFK_DXE)                                     27
      EQUIVALENCE (COMMEN_SFJP(87),CSFJP_SFFK_DXE),(MAP_SFJP(1,87),       27
     *MSFJP_SFFK_DXE)                                                     27
      CHARACTER CSFJP_SFFK_DXE*40/' '/                                    27
      CHARACTER NSFJP_SFFK_DXE*12/'R.SFFK_DXE'/                           27
      INTEGER MSFJP_SFFK_DXE(2)/1,1/                                      27
      REAL RE_SFJP_SFFK_DXE                                               27
C -                                                                       27
      REAL SFJP_SFFK_DY                                                   27
      EQUIVALENCE (BANK_SFJP(90),SFJP_SFFK_DY,RE_SFJP_SFFK_DY),           27
     *(NAMES_SFJP(88),NSFJP_SFFK_DY)                                      27
      EQUIVALENCE (COMMEN_SFJP(88),CSFJP_SFFK_DY),(MAP_SFJP(1,88),        27
     *MSFJP_SFFK_DY)                                                      27
      CHARACTER CSFJP_SFFK_DY*40/' '/                                     27
      CHARACTER NSFJP_SFFK_DY*12/'R.SFFK_DY'/                             27
      INTEGER MSFJP_SFFK_DY(2)/1,1/                                       27
      REAL RE_SFJP_SFFK_DY                                                27
C -                                                                       27
      REAL SFJP_SFFX_DYE                                                  27
      EQUIVALENCE (BANK_SFJP(91),SFJP_SFFX_DYE,RE_SFJP_SFFX_DYE),         27
     *(NAMES_SFJP(89),NSFJP_SFFX_DYE)                                     27
      EQUIVALENCE (COMMEN_SFJP(89),CSFJP_SFFX_DYE),(MAP_SFJP(1,89),       27
     *MSFJP_SFFX_DYE)                                                     27
      CHARACTER CSFJP_SFFX_DYE*40/' '/                                    27
      CHARACTER NSFJP_SFFX_DYE*12/'R.SFFX_DYE'/                           27
      INTEGER MSFJP_SFFX_DYE(2)/1,1/                                      27
      REAL RE_SFJP_SFFX_DYE                                               27
C -                                                                       27
      REAL SFJP_SFFK_DZ                                                   27
      EQUIVALENCE (BANK_SFJP(92),SFJP_SFFK_DZ,RE_SFJP_SFFK_DZ),           27
     *(NAMES_SFJP(90),NSFJP_SFFK_DZ)                                      27
      EQUIVALENCE (COMMEN_SFJP(90),CSFJP_SFFK_DZ),(MAP_SFJP(1,90),        27
     *MSFJP_SFFK_DZ)                                                      27
      CHARACTER CSFJP_SFFK_DZ*40/' '/                                     27
      CHARACTER NSFJP_SFFK_DZ*12/'R.SFFK_DZ'/                             27
      INTEGER MSFJP_SFFK_DZ(2)/1,1/                                       27
      REAL RE_SFJP_SFFK_DZ                                                27
C -                                                                       27
      REAL SFJP_SFFL_DX                                                   27
      EQUIVALENCE (BANK_SFJP(93),SFJP_SFFL_DX,RE_SFJP_SFFL_DX),           27
     *(NAMES_SFJP(91),NSFJP_SFFL_DX)                                      27
      EQUIVALENCE (COMMEN_SFJP(91),CSFJP_SFFL_DX),(MAP_SFJP(1,91),        27
     *MSFJP_SFFL_DX)                                                      27
      CHARACTER CSFJP_SFFL_DX*40/' '/                                     27
      CHARACTER NSFJP_SFFL_DX*12/'R.SFFL_DX'/                             27
      INTEGER MSFJP_SFFL_DX(2)/1,1/                                       27
      REAL RE_SFJP_SFFL_DX                                                27
C -                                                                       27
      REAL SFJP_SFFK_DXZ                                                  27
      EQUIVALENCE (BANK_SFJP(94),SFJP_SFFK_DXZ,RE_SFJP_SFFK_DXZ),         27
     *(NAMES_SFJP(92),NSFJP_SFFK_DXZ)                                     27
      EQUIVALENCE (COMMEN_SFJP(92),CSFJP_SFFK_DXZ),(MAP_SFJP(1,92),       27
     *MSFJP_SFFK_DXZ)                                                     27
      CHARACTER CSFJP_SFFK_DXZ*40/' '/                                    27
      CHARACTER NSFJP_SFFK_DXZ*12/'R.SFFK_DXZ'/                           27
      INTEGER MSFJP_SFFK_DXZ(2)/1,1/                                      27
      REAL RE_SFJP_SFFK_DXZ                                               27
C -                                                                       27
      REAL SFJP_SFFK_PX                                                   27
      EQUIVALENCE (BANK_SFJP(95),SFJP_SFFK_PX,RE_SFJP_SFFK_PX),           27
     *(NAMES_SFJP(93),NSFJP_SFFK_PX)                                      27
      EQUIVALENCE (COMMEN_SFJP(93),CSFJP_SFFK_PX),(MAP_SFJP(1,93),        27
     *MSFJP_SFFK_PX)                                                      27
      CHARACTER CSFJP_SFFK_PX*40/' '/                                     27
      CHARACTER NSFJP_SFFK_PX*12/'R.SFFK_PX'/                             27
      INTEGER MSFJP_SFFK_PX(2)/1,1/                                       27
      REAL RE_SFJP_SFFK_PX                                                27
C -                                                                       27
      REAL SFJP_SFFK_PY1                                                  27
      EQUIVALENCE (BANK_SFJP(96),SFJP_SFFK_PY1,RE_SFJP_SFFK_PY1),         27
     *(NAMES_SFJP(94),NSFJP_SFFK_PY1)                                     27
      EQUIVALENCE (COMMEN_SFJP(94),CSFJP_SFFK_PY1),(MAP_SFJP(1,94),       27
     *MSFJP_SFFK_PY1)                                                     27
      CHARACTER CSFJP_SFFK_PY1*40/' '/                                    27
      CHARACTER NSFJP_SFFK_PY1*12/'R.SFFK_PY1'/                           27
      INTEGER MSFJP_SFFK_PY1(2)/1,1/                                      27
      REAL RE_SFJP_SFFK_PY1                                               27
C -                                                                       27
      REAL SFJP_SFFK_PY2                                                  27
      EQUIVALENCE (BANK_SFJP(97),SFJP_SFFK_PY2,RE_SFJP_SFFK_PY2),         27
     *(NAMES_SFJP(95),NSFJP_SFFK_PY2)                                     27
      EQUIVALENCE (COMMEN_SFJP(95),CSFJP_SFFK_PY2),(MAP_SFJP(1,95),       27
     *MSFJP_SFFK_PY2)                                                     27
      CHARACTER CSFJP_SFFK_PY2*40/' '/                                    27
      CHARACTER NSFJP_SFFK_PY2*12/'R.SFFK_PY2'/                           27
      INTEGER MSFJP_SFFK_PY2(2)/1,1/                                      27
      REAL RE_SFJP_SFFK_PY2                                               27
C -                                                                       27
      REAL SFJP_SFKL_PX                                                   27
      EQUIVALENCE (BANK_SFJP(98),SFJP_SFKL_PX,RE_SFJP_SFKL_PX),           27
     *(NAMES_SFJP(96),NSFJP_SFKL_PX)                                      27
      EQUIVALENCE (COMMEN_SFJP(96),CSFJP_SFKL_PX),(MAP_SFJP(1,96),        27
     *MSFJP_SFKL_PX)                                                      27
      CHARACTER CSFJP_SFKL_PX*40/' '/                                     27
      CHARACTER NSFJP_SFKL_PX*12/'R.SFKL_PX'/                             27
      INTEGER MSFJP_SFKL_PX(2)/1,1/                                       27
      REAL RE_SFJP_SFKL_PX                                                27
C -                                                                       27
      REAL SFJP_SFKK_DY                                                   27
      EQUIVALENCE (BANK_SFJP(99),SFJP_SFKK_DY,RE_SFJP_SFKK_DY),           27
     *(NAMES_SFJP(97),NSFJP_SFKK_DY)                                      27
      EQUIVALENCE (COMMEN_SFJP(97),CSFJP_SFKK_DY),(MAP_SFJP(1,97),        27
     *MSFJP_SFKK_DY)                                                      27
      CHARACTER CSFJP_SFKK_DY*40/' '/                                     27
      CHARACTER NSFJP_SFKK_DY*12/'R.SFKK_DY'/                             27
      INTEGER MSFJP_SFKK_DY(2)/1,1/                                       27
      REAL RE_SFJP_SFKK_DY                                                27
C -                                                                       27
      REAL SFJP_SFLU_DZ                                                   27
      EQUIVALENCE (BANK_SFJP(100),SFJP_SFLU_DZ,RE_SFJP_SFLU_DZ),          27
     *(NAMES_SFJP(98),NSFJP_SFLU_DZ)                                      27
      EQUIVALENCE (COMMEN_SFJP(98),CSFJP_SFLU_DZ),(MAP_SFJP(1,98),        27
     *MSFJP_SFLU_DZ)                                                      27
      CHARACTER CSFJP_SFLU_DZ*40/' '/                                     27
      CHARACTER NSFJP_SFLU_DZ*12/'R.SFLU_DZ'/                             27
      INTEGER MSFJP_SFLU_DZ(2)/1,1/                                       27
      REAL RE_SFJP_SFLU_DZ                                                27
C -                                                                       27
      REAL SFJP_SFLU_H1                                                   27
      EQUIVALENCE (BANK_SFJP(101),SFJP_SFLU_H1,RE_SFJP_SFLU_H1),          27
     *(NAMES_SFJP(99),NSFJP_SFLU_H1)                                      27
      EQUIVALENCE (COMMEN_SFJP(99),CSFJP_SFLU_H1),(MAP_SFJP(1,99),        27
     *MSFJP_SFLU_H1)                                                      27
      CHARACTER CSFJP_SFLU_H1*40/' '/                                     27
      CHARACTER NSFJP_SFLU_H1*12/'R.SFLU_H1'/                             27
      INTEGER MSFJP_SFLU_H1(2)/1,1/                                       27
      REAL RE_SFJP_SFLU_H1                                                27
C -                                                                       27
      REAL SFJP_SFLU_BL1                                                  27
      EQUIVALENCE (BANK_SFJP(102),SFJP_SFLU_BL1,RE_SFJP_SFLU_BL1),        27
     *(NAMES_SFJP(100),NSFJP_SFLU_BL1)                                    27
      EQUIVALENCE (COMMEN_SFJP(100),CSFJP_SFLU_BL1),(MAP_SFJP(1,100),     27
     *MSFJP_SFLU_BL1)                                                     27
      CHARACTER CSFJP_SFLU_BL1*40/' '/                                    27
      CHARACTER NSFJP_SFLU_BL1*12/'R.SFLU_BL1'/                           27
      INTEGER MSFJP_SFLU_BL1(2)/1,1/                                      27
      REAL RE_SFJP_SFLU_BL1                                               27
C -                                                                       27
      REAL SFJP_SFRA_DX                                                   27
      EQUIVALENCE (BANK_SFJP(103),SFJP_SFRA_DX,RE_SFJP_SFRA_DX),          27
     *(NAMES_SFJP(101),NSFJP_SFRA_DX)                                     27
      EQUIVALENCE (COMMEN_SFJP(101),CSFJP_SFRA_DX),(MAP_SFJP(1,101),      27
     *MSFJP_SFRA_DX)                                                      27
      CHARACTER CSFJP_SFRA_DX*40/' '/                                     27
      CHARACTER NSFJP_SFRA_DX*12/'R.SFRA_DX'/                             27
      INTEGER MSFJP_SFRA_DX(2)/1,1/                                       27
      REAL RE_SFJP_SFRA_DX                                                27
C -                                                                       27
      REAL SFJP_SFRA_DY                                                   27
      EQUIVALENCE (BANK_SFJP(104),SFJP_SFRA_DY,RE_SFJP_SFRA_DY),          27
     *(NAMES_SFJP(102),NSFJP_SFRA_DY)                                     27
      EQUIVALENCE (COMMEN_SFJP(102),CSFJP_SFRA_DY),(MAP_SFJP(1,102),      27
     *MSFJP_SFRA_DY)                                                      27
      CHARACTER CSFJP_SFRA_DY*40/' '/                                     27
      CHARACTER NSFJP_SFRA_DY*12/'R.SFRA_DY'/                             27
      INTEGER MSFJP_SFRA_DY(2)/1,1/                                       27
      REAL RE_SFJP_SFRA_DY                                                27
C -                                                                       27
      REAL SFJP_SFRA_PY                                                   27
      EQUIVALENCE (BANK_SFJP(105),SFJP_SFRA_PY,RE_SFJP_SFRA_PY),          27
     *(NAMES_SFJP(103),NSFJP_SFRA_PY)                                     27
      EQUIVALENCE (COMMEN_SFJP(103),CSFJP_SFRA_PY),(MAP_SFJP(1,103),      27
     *MSFJP_SFRA_PY)                                                      27
      CHARACTER CSFJP_SFRA_PY*40/' '/                                     27
      CHARACTER NSFJP_SFRA_PY*12/'R.SFRA_PY'/                             27
      INTEGER MSFJP_SFRA_PY(2)/1,1/                                       27
      REAL RE_SFJP_SFRA_PY                                                27
C -                                                                       27
      REAL SFJP_SFRA_DZ                                                   27
      EQUIVALENCE (BANK_SFJP(106),SFJP_SFRA_DZ,RE_SFJP_SFRA_DZ),          27
     *(NAMES_SFJP(104),NSFJP_SFRA_DZ)                                     27
      EQUIVALENCE (COMMEN_SFJP(104),CSFJP_SFRA_DZ),(MAP_SFJP(1,104),      27
     *MSFJP_SFRA_DZ)                                                      27
      CHARACTER CSFJP_SFRA_DZ*40/' '/                                     27
      CHARACTER NSFJP_SFRA_DZ*12/'R.SFRA_DZ'/                             27
      INTEGER MSFJP_SFRA_DZ(2)/1,1/                                       27
      REAL RE_SFJP_SFRA_DZ                                                27
C -                                                                       27
      REAL SFJP_SFRA_PZ                                                   27
      EQUIVALENCE (BANK_SFJP(107),SFJP_SFRA_PZ,RE_SFJP_SFRA_PZ),          27
     *(NAMES_SFJP(105),NSFJP_SFRA_PZ)                                     27
      EQUIVALENCE (COMMEN_SFJP(105),CSFJP_SFRA_PZ),(MAP_SFJP(1,105),      27
     *MSFJP_SFRA_PZ)                                                      27
      CHARACTER CSFJP_SFRA_PZ*40/' '/                                     27
      CHARACTER NSFJP_SFRA_PZ*12/'R.SFRA_PZ'/                             27
      INTEGER MSFJP_SFRA_PZ(2)/1,1/                                       27
      REAL RE_SFJP_SFRA_PZ                                                27
C -                                                                       27
      REAL SFJP_SFSW_DY                                                   27
      EQUIVALENCE (BANK_SFJP(108),SFJP_SFSW_DY,RE_SFJP_SFSW_DY),          27
     *(NAMES_SFJP(106),NSFJP_SFSW_DY)                                     27
      EQUIVALENCE (COMMEN_SFJP(106),CSFJP_SFSW_DY),(MAP_SFJP(1,106),      27
     *MSFJP_SFSW_DY)                                                      27
      CHARACTER CSFJP_SFSW_DY*40/' '/                                     27
      CHARACTER NSFJP_SFSW_DY*12/'R.SFSW_DY'/                             27
      INTEGER MSFJP_SFSW_DY(2)/1,1/                                       27
      REAL RE_SFJP_SFSW_DY                                                27
C -                                                                       27
      REAL SFJP_SFSW_DZ                                                   27
      EQUIVALENCE (BANK_SFJP(109),SFJP_SFSW_DZ,RE_SFJP_SFSW_DZ),          27
     *(NAMES_SFJP(107),NSFJP_SFSW_DZ)                                     27
      EQUIVALENCE (COMMEN_SFJP(107),CSFJP_SFSW_DZ),(MAP_SFJP(1,107),      27
     *MSFJP_SFSW_DZ)                                                      27
      CHARACTER CSFJP_SFSW_DZ*40/' '/                                     27
      CHARACTER NSFJP_SFSW_DZ*12/'R.SFSW_DZ'/                             27
      INTEGER MSFJP_SFSW_DZ(2)/1,1/                                       27
      REAL RE_SFJP_SFSW_DZ                                                27
C -                                                                       27
      REAL SFJP_SFRS_DX                                                   27
      EQUIVALENCE (BANK_SFJP(110),SFJP_SFRS_DX,RE_SFJP_SFRS_DX),          27
     *(NAMES_SFJP(108),NSFJP_SFRS_DX)                                     27
      EQUIVALENCE (COMMEN_SFJP(108),CSFJP_SFRS_DX),(MAP_SFJP(1,108),      27
     *MSFJP_SFRS_DX)                                                      27
      CHARACTER CSFJP_SFRS_DX*40/' '/                                     27
      CHARACTER NSFJP_SFRS_DX*12/'R.SFRS_DX'/                             27
      INTEGER MSFJP_SFRS_DX(2)/1,1/                                       27
      REAL RE_SFJP_SFRS_DX                                                27
C -                                                                       27
      REAL SFJP_SFRS_DY                                                   27
      EQUIVALENCE (BANK_SFJP(111),SFJP_SFRS_DY,RE_SFJP_SFRS_DY),          27
     *(NAMES_SFJP(109),NSFJP_SFRS_DY)                                     27
      EQUIVALENCE (COMMEN_SFJP(109),CSFJP_SFRS_DY),(MAP_SFJP(1,109),      27
     *MSFJP_SFRS_DY)                                                      27
      CHARACTER CSFJP_SFRS_DY*40/' '/                                     27
      CHARACTER NSFJP_SFRS_DY*12/'R.SFRS_DY'/                             27
      INTEGER MSFJP_SFRS_DY(2)/1,1/                                       27
      REAL RE_SFJP_SFRS_DY                                                27
C -                                                                       27
      REAL SFJP_SFRS_PX                                                   27
      EQUIVALENCE (BANK_SFJP(112),SFJP_SFRS_PX,RE_SFJP_SFRS_PX),          27
     *(NAMES_SFJP(110),NSFJP_SFRS_PX)                                     27
      EQUIVALENCE (COMMEN_SFJP(110),CSFJP_SFRS_PX),(MAP_SFJP(1,110),      27
     *MSFJP_SFRS_PX)                                                      27
      CHARACTER CSFJP_SFRS_PX*40/' '/                                     27
      CHARACTER NSFJP_SFRS_PX*12/'R.SFRS_PX'/                             27
      INTEGER MSFJP_SFRS_PX(2)/1,1/                                       27
      REAL RE_SFJP_SFRS_PX                                                27
C -                                                                       27
      REAL SFJP_SFRS_PY                                                   27
      EQUIVALENCE (BANK_SFJP(113),SFJP_SFRS_PY,RE_SFJP_SFRS_PY),          27
     *(NAMES_SFJP(111),NSFJP_SFRS_PY)                                     27
      EQUIVALENCE (COMMEN_SFJP(111),CSFJP_SFRS_PY),(MAP_SFJP(1,111),      27
     *MSFJP_SFRS_PY)                                                      27
      CHARACTER CSFJP_SFRS_PY*40/' '/                                     27
      CHARACTER NSFJP_SFRS_PY*12/'R.SFRS_PY'/                             27
      INTEGER MSFJP_SFRS_PY(2)/1,1/                                       27
      REAL RE_SFJP_SFRS_PY                                                27
C -                                                                       27
      REAL SFJP_SFSM_LL                                                   27
      EQUIVALENCE (BANK_SFJP(114),SFJP_SFSM_LL,RE_SFJP_SFSM_LL),          27
     *(NAMES_SFJP(112),NSFJP_SFSM_LL)                                     27
      EQUIVALENCE (COMMEN_SFJP(112),CSFJP_SFSM_LL),(MAP_SFJP(1,112),      27
     *MSFJP_SFSM_LL)                                                      27
      CHARACTER CSFJP_SFSM_LL*40/' '/                                     27
      CHARACTER NSFJP_SFSM_LL*12/'R.SFSM_LL'/                             27
      INTEGER MSFJP_SFSM_LL(2)/1,1/                                       27
      REAL RE_SFJP_SFSM_LL                                                27
C -                                                                       27
      REAL SFJP_SFFX_DX                                                   27
      EQUIVALENCE (BANK_SFJP(115),SFJP_SFFX_DX,RE_SFJP_SFFX_DX),          27
     *(NAMES_SFJP(113),NSFJP_SFFX_DX)                                     27
      EQUIVALENCE (COMMEN_SFJP(113),CSFJP_SFFX_DX),(MAP_SFJP(1,113),      27
     *MSFJP_SFFX_DX)                                                      27
      CHARACTER CSFJP_SFFX_DX*40/' '/                                     27
      CHARACTER NSFJP_SFFX_DX*12/'R.SFFX_DX'/                             27
      INTEGER MSFJP_SFFX_DX(2)/1,1/                                       27
      REAL RE_SFJP_SFFX_DX                                                27
C -                                                                       27
      REAL SFJP_SFFX_DYZ                                                  27
      EQUIVALENCE (BANK_SFJP(116),SFJP_SFFX_DYZ,RE_SFJP_SFFX_DYZ),        27
     *(NAMES_SFJP(114),NSFJP_SFFX_DYZ)                                    27
      EQUIVALENCE (COMMEN_SFJP(114),CSFJP_SFFX_DYZ),(MAP_SFJP(1,114),     27
     *MSFJP_SFFX_DYZ)                                                     27
      CHARACTER CSFJP_SFFX_DYZ*40/' '/                                    27
      CHARACTER NSFJP_SFFX_DYZ*12/'R.SFFX_DYZ'/                           27
      INTEGER MSFJP_SFFX_DYZ(2)/1,1/                                      27
      REAL RE_SFJP_SFFX_DYZ                                               27
C -                                                                       27
      REAL SFJP_SFPI_RMIN                                                 27
      EQUIVALENCE (BANK_SFJP(117),SFJP_SFPI_RMIN,RE_SFJP_SFPI_RMIN),      27
     *(NAMES_SFJP(115),NSFJP_SFPI_RMIN)                                   27
      EQUIVALENCE (COMMEN_SFJP(115),CSFJP_SFPI_RMIN),(MAP_SFJP(1,115),    27
     *MSFJP_SFPI_RMIN)                                                    27
      CHARACTER CSFJP_SFPI_RMIN*40/' '/                                   27
      CHARACTER NSFJP_SFPI_RMIN*12/'R.SFPI_RMIN'/                         27
      INTEGER MSFJP_SFPI_RMIN(2)/1,1/                                     27
      REAL RE_SFJP_SFPI_RMIN                                              27
C -                                                                       27
      REAL SFJP_SFPI_RMAX                                                 27
      EQUIVALENCE (BANK_SFJP(118),SFJP_SFPI_RMAX,RE_SFJP_SFPI_RMAX),      27
     *(NAMES_SFJP(116),NSFJP_SFPI_RMAX)                                   27
      EQUIVALENCE (COMMEN_SFJP(116),CSFJP_SFPI_RMAX),(MAP_SFJP(1,116),    27
     *MSFJP_SFPI_RMAX)                                                    27
      CHARACTER CSFJP_SFPI_RMAX*40/' '/                                   27
      CHARACTER NSFJP_SFPI_RMAX*12/'R.SFPI_RMAX'/                         27
      INTEGER MSFJP_SFPI_RMAX(2)/1,1/                                     27
      REAL RE_SFJP_SFPI_RMAX                                              27
C -                                                                       27
      REAL SFJP_SFPI_PX                                                   27
      EQUIVALENCE (BANK_SFJP(119),SFJP_SFPI_PX,RE_SFJP_SFPI_PX),          27
     *(NAMES_SFJP(117),NSFJP_SFPI_PX)                                     27
      EQUIVALENCE (COMMEN_SFJP(117),CSFJP_SFPI_PX),(MAP_SFJP(1,117),      27
     *MSFJP_SFPI_PX)                                                      27
      CHARACTER CSFJP_SFPI_PX*40/' '/                                     27
      CHARACTER NSFJP_SFPI_PX*12/'R.SFPI_PX'/                             27
      INTEGER MSFJP_SFPI_PX(2)/1,1/                                       27
      REAL RE_SFJP_SFPI_PX                                                27
C -                                                                       27
      REAL SFJP_SFPI_PY1                                                  27
      EQUIVALENCE (BANK_SFJP(120),SFJP_SFPI_PY1,RE_SFJP_SFPI_PY1),        27
     *(NAMES_SFJP(118),NSFJP_SFPI_PY1)                                    27
      EQUIVALENCE (COMMEN_SFJP(118),CSFJP_SFPI_PY1),(MAP_SFJP(1,118),     27
     *MSFJP_SFPI_PY1)                                                     27
      CHARACTER CSFJP_SFPI_PY1*40/' '/                                    27
      CHARACTER NSFJP_SFPI_PY1*12/'R.SFPI_PY1'/                           27
      INTEGER MSFJP_SFPI_PY1(2)/1,1/                                      27
      REAL RE_SFJP_SFPI_PY1                                               27
C -                                                                       27
      REAL SFJP_SFPI_PY2                                                  27
      EQUIVALENCE (BANK_SFJP(121),SFJP_SFPI_PY2,RE_SFJP_SFPI_PY2),        27
     *(NAMES_SFJP(119),NSFJP_SFPI_PY2)                                    27
      EQUIVALENCE (COMMEN_SFJP(119),CSFJP_SFPI_PY2),(MAP_SFJP(1,119),     27
     *MSFJP_SFPI_PY2)                                                     27
      CHARACTER CSFJP_SFPI_PY2*40/' '/                                    27
      CHARACTER NSFJP_SFPI_PY2*12/'R.SFPI_PY2'/                           27
      INTEGER MSFJP_SFPI_PY2(2)/1,1/                                      27
      REAL RE_SFJP_SFPI_PY2                                               27
C -                                                                       27
      REAL SFJP_SFPI_PZ                                                   27
      EQUIVALENCE (BANK_SFJP(122),SFJP_SFPI_PZ,RE_SFJP_SFPI_PZ),          27
     *(NAMES_SFJP(120),NSFJP_SFPI_PZ)                                     27
      EQUIVALENCE (COMMEN_SFJP(120),CSFJP_SFPI_PZ),(MAP_SFJP(1,120),      27
     *MSFJP_SFPI_PZ)                                                      27
      CHARACTER CSFJP_SFPI_PZ*40/' '/                                     27
      CHARACTER NSFJP_SFPI_PZ*12/'R.SFPI_PZ'/                             27
      INTEGER MSFJP_SFPI_PZ(2)/1,1/                                       27
      REAL RE_SFJP_SFPI_PZ                                                27
C -                                                                       27
      REAL SFJP_SFPJ_DX1                                                  27
      EQUIVALENCE (BANK_SFJP(123),SFJP_SFPJ_DX1,RE_SFJP_SFPJ_DX1),        27
     *(NAMES_SFJP(121),NSFJP_SFPJ_DX1)                                    27
      EQUIVALENCE (COMMEN_SFJP(121),CSFJP_SFPJ_DX1),(MAP_SFJP(1,121),     27
     *MSFJP_SFPJ_DX1)                                                     27
      CHARACTER CSFJP_SFPJ_DX1*40/' '/                                    27
      CHARACTER NSFJP_SFPJ_DX1*12/'R.SFPJ_DX1'/                           27
      INTEGER MSFJP_SFPJ_DX1(2)/1,1/                                      27
      REAL RE_SFJP_SFPJ_DX1                                               27
C -                                                                       27
      REAL SFJP_SFPJ_DX2                                                  27
      EQUIVALENCE (BANK_SFJP(124),SFJP_SFPJ_DX2,RE_SFJP_SFPJ_DX2),        27
     *(NAMES_SFJP(122),NSFJP_SFPJ_DX2)                                    27
      EQUIVALENCE (COMMEN_SFJP(122),CSFJP_SFPJ_DX2),(MAP_SFJP(1,122),     27
     *MSFJP_SFPJ_DX2)                                                     27
      CHARACTER CSFJP_SFPJ_DX2*40/' '/                                    27
      CHARACTER NSFJP_SFPJ_DX2*12/'R.SFPJ_DX2'/                           27
      INTEGER MSFJP_SFPJ_DX2(2)/1,1/                                      27
      REAL RE_SFJP_SFPJ_DX2                                               27
C -                                                                       27
      REAL SFJP_SFPJ_DY                                                   27
      EQUIVALENCE (BANK_SFJP(125),SFJP_SFPJ_DY,RE_SFJP_SFPJ_DY),          27
     *(NAMES_SFJP(123),NSFJP_SFPJ_DY)                                     27
      EQUIVALENCE (COMMEN_SFJP(123),CSFJP_SFPJ_DY),(MAP_SFJP(1,123),      27
     *MSFJP_SFPJ_DY)                                                      27
      CHARACTER CSFJP_SFPJ_DY*40/' '/                                     27
      CHARACTER NSFJP_SFPJ_DY*12/'R.SFPJ_DY'/                             27
      INTEGER MSFJP_SFPJ_DY(2)/1,1/                                       27
      REAL RE_SFJP_SFPJ_DY                                                27
C -                                                                       27
      REAL SFJP_SFPJ_DZ                                                   27
      EQUIVALENCE (BANK_SFJP(126),SFJP_SFPJ_DZ,RE_SFJP_SFPJ_DZ),          27
     *(NAMES_SFJP(124),NSFJP_SFPJ_DZ)                                     27
      EQUIVALENCE (COMMEN_SFJP(124),CSFJP_SFPJ_DZ),(MAP_SFJP(1,124),      27
     *MSFJP_SFPJ_DZ)                                                      27
      CHARACTER CSFJP_SFPJ_DZ*40/' '/                                     27
      CHARACTER NSFJP_SFPJ_DZ*12/'R.SFPJ_DZ'/                             27
      INTEGER MSFJP_SFPJ_DZ(2)/1,1/                                       27
      REAL RE_SFJP_SFPJ_DZ                                                27
C -                                                                       27
      REAL SFJP_SFAA_DX                                                   27
      EQUIVALENCE (BANK_SFJP(127),SFJP_SFAA_DX,RE_SFJP_SFAA_DX),          27
     *(NAMES_SFJP(125),NSFJP_SFAA_DX)                                     27
      EQUIVALENCE (COMMEN_SFJP(125),CSFJP_SFAA_DX),(MAP_SFJP(1,125),      27
     *MSFJP_SFAA_DX)                                                      27
      CHARACTER CSFJP_SFAA_DX*40/' '/                                     27
      CHARACTER NSFJP_SFAA_DX*12/'R.SFAA_DX'/                             27
      INTEGER MSFJP_SFAA_DX(2)/1,1/                                       27
      REAL RE_SFJP_SFAA_DX                                                27
C -                                                                       27
      REAL SFJP_SFAA_DY                                                   27
      EQUIVALENCE (BANK_SFJP(128),SFJP_SFAA_DY,RE_SFJP_SFAA_DY),          27
     *(NAMES_SFJP(126),NSFJP_SFAA_DY)                                     27
      EQUIVALENCE (COMMEN_SFJP(126),CSFJP_SFAA_DY),(MAP_SFJP(1,126),      27
     *MSFJP_SFAA_DY)                                                      27
      CHARACTER CSFJP_SFAA_DY*40/' '/                                     27
      CHARACTER NSFJP_SFAA_DY*12/'R.SFAA_DY'/                             27
      INTEGER MSFJP_SFAA_DY(2)/1,1/                                       27
      REAL RE_SFJP_SFAA_DY                                                27
C -                                                                       27
      REAL SFJP_SFAA_DZ                                                   27
      EQUIVALENCE (BANK_SFJP(129),SFJP_SFAA_DZ,RE_SFJP_SFAA_DZ),          27
     *(NAMES_SFJP(127),NSFJP_SFAA_DZ)                                     27
      EQUIVALENCE (COMMEN_SFJP(127),CSFJP_SFAA_DZ),(MAP_SFJP(1,127),      27
     *MSFJP_SFAA_DZ)                                                      27
      CHARACTER CSFJP_SFAA_DZ*40/' '/                                     27
      CHARACTER NSFJP_SFAA_DZ*12/'R.SFAA_DZ'/                             27
      INTEGER MSFJP_SFAA_DZ(2)/1,1/                                       27
      REAL RE_SFJP_SFAA_DZ                                                27
C -                                                                       27
      REAL SFJP_SFAA_PX1                                                  27
      EQUIVALENCE (BANK_SFJP(130),SFJP_SFAA_PX1,RE_SFJP_SFAA_PX1),        27
     *(NAMES_SFJP(128),NSFJP_SFAA_PX1)                                    27
      EQUIVALENCE (COMMEN_SFJP(128),CSFJP_SFAA_PX1),(MAP_SFJP(1,128),     27
     *MSFJP_SFAA_PX1)                                                     27
      CHARACTER CSFJP_SFAA_PX1*40/' '/                                    27
      CHARACTER NSFJP_SFAA_PX1*12/'R.SFAA_PX1'/                           27
      INTEGER MSFJP_SFAA_PX1(2)/1,1/                                      27
      REAL RE_SFJP_SFAA_PX1                                               27
C -                                                                       27
      REAL SFJP_SFAA_PX2                                                  27
      EQUIVALENCE (BANK_SFJP(131),SFJP_SFAA_PX2,RE_SFJP_SFAA_PX2),        27
     *(NAMES_SFJP(129),NSFJP_SFAA_PX2)                                    27
      EQUIVALENCE (COMMEN_SFJP(129),CSFJP_SFAA_PX2),(MAP_SFJP(1,129),     27
     *MSFJP_SFAA_PX2)                                                     27
      CHARACTER CSFJP_SFAA_PX2*40/' '/                                    27
      CHARACTER NSFJP_SFAA_PX2*12/'R.SFAA_PX2'/                           27
      INTEGER MSFJP_SFAA_PX2(2)/1,1/                                      27
      REAL RE_SFJP_SFAA_PX2                                               27
C -                                                                       27
      REAL SFJP_SFAA_PX3                                                  27
      EQUIVALENCE (BANK_SFJP(132),SFJP_SFAA_PX3,RE_SFJP_SFAA_PX3),        27
     *(NAMES_SFJP(130),NSFJP_SFAA_PX3)                                    27
      EQUIVALENCE (COMMEN_SFJP(130),CSFJP_SFAA_PX3),(MAP_SFJP(1,130),     27
     *MSFJP_SFAA_PX3)                                                     27
      CHARACTER CSFJP_SFAA_PX3*40/' '/                                    27
      CHARACTER NSFJP_SFAA_PX3*12/'R.SFAA_PX3'/                           27
      INTEGER MSFJP_SFAA_PX3(2)/1,1/                                      27
      REAL RE_SFJP_SFAA_PX3                                               27
C -                                                                       27
      REAL SFJP_SFAA_PZ1                                                  27
      EQUIVALENCE (BANK_SFJP(133),SFJP_SFAA_PZ1,RE_SFJP_SFAA_PZ1),        27
     *(NAMES_SFJP(131),NSFJP_SFAA_PZ1)                                    27
      EQUIVALENCE (COMMEN_SFJP(131),CSFJP_SFAA_PZ1),(MAP_SFJP(1,131),     27
     *MSFJP_SFAA_PZ1)                                                     27
      CHARACTER CSFJP_SFAA_PZ1*40/' '/                                    27
      CHARACTER NSFJP_SFAA_PZ1*12/'R.SFAA_PZ1'/                           27
      INTEGER MSFJP_SFAA_PZ1(2)/1,1/                                      27
      REAL RE_SFJP_SFAA_PZ1                                               27
C -                                                                       27
      REAL SFJP_SFAA_PZ2                                                  27
      EQUIVALENCE (BANK_SFJP(134),SFJP_SFAA_PZ2,RE_SFJP_SFAA_PZ2),        27
     *(NAMES_SFJP(132),NSFJP_SFAA_PZ2)                                    27
      EQUIVALENCE (COMMEN_SFJP(132),CSFJP_SFAA_PZ2),(MAP_SFJP(1,132),     27
     *MSFJP_SFAA_PZ2)                                                     27
      CHARACTER CSFJP_SFAA_PZ2*40/' '/                                    27
      CHARACTER NSFJP_SFAA_PZ2*12/'R.SFAA_PZ2'/                           27
      INTEGER MSFJP_SFAA_PZ2(2)/1,1/                                      27
      REAL RE_SFJP_SFAA_PZ2                                               27
C -                                                                       27
      REAL SFJP_SFAA_PZ3                                                  27
      EQUIVALENCE (BANK_SFJP(135),SFJP_SFAA_PZ3,RE_SFJP_SFAA_PZ3),        27
     *(NAMES_SFJP(133),NSFJP_SFAA_PZ3)                                    27
      EQUIVALENCE (COMMEN_SFJP(133),CSFJP_SFAA_PZ3),(MAP_SFJP(1,133),     27
     *MSFJP_SFAA_PZ3)                                                     27
      CHARACTER CSFJP_SFAA_PZ3*40/' '/                                    27
      CHARACTER NSFJP_SFAA_PZ3*12/'R.SFAA_PZ3'/                           27
      INTEGER MSFJP_SFAA_PZ3(2)/1,1/                                      27
      REAL RE_SFJP_SFAA_PZ3                                               27
C -                                                                       27
      REAL SFJP_SFSL_DX                                                   27
      EQUIVALENCE (BANK_SFJP(136),SFJP_SFSL_DX,RE_SFJP_SFSL_DX),          27
     *(NAMES_SFJP(134),NSFJP_SFSL_DX)                                     27
      EQUIVALENCE (COMMEN_SFJP(134),CSFJP_SFSL_DX),(MAP_SFJP(1,134),      27
     *MSFJP_SFSL_DX)                                                      27
      CHARACTER CSFJP_SFSL_DX*40/' '/                                     27
      CHARACTER NSFJP_SFSL_DX*12/'R.SFSL_DX'/                             27
      INTEGER MSFJP_SFSL_DX(2)/1,1/                                       27
      REAL RE_SFJP_SFSL_DX                                                27
C -                                                                       27
      REAL SFJP_SFSL_DY                                                   27
      EQUIVALENCE (BANK_SFJP(137),SFJP_SFSL_DY,RE_SFJP_SFSL_DY),          27
     *(NAMES_SFJP(135),NSFJP_SFSL_DY)                                     27
      EQUIVALENCE (COMMEN_SFJP(135),CSFJP_SFSL_DY),(MAP_SFJP(1,135),      27
     *MSFJP_SFSL_DY)                                                      27
      CHARACTER CSFJP_SFSL_DY*40/' '/                                     27
      CHARACTER NSFJP_SFSL_DY*12/'R.SFSL_DY'/                             27
      INTEGER MSFJP_SFSL_DY(2)/1,1/                                       27
      REAL RE_SFJP_SFSL_DY                                                27
C -                                                                       27
      REAL SFJP_SFSL_DZ                                                   27
      EQUIVALENCE (BANK_SFJP(138),SFJP_SFSL_DZ,RE_SFJP_SFSL_DZ),          27
     *(NAMES_SFJP(136),NSFJP_SFSL_DZ)                                     27
      EQUIVALENCE (COMMEN_SFJP(136),CSFJP_SFSL_DZ),(MAP_SFJP(1,136),      27
     *MSFJP_SFSL_DZ)                                                      27
      CHARACTER CSFJP_SFSL_DZ*40/' '/                                     27
      CHARACTER NSFJP_SFSL_DZ*12/'R.SFSL_DZ'/                             27
      INTEGER MSFJP_SFSL_DZ(2)/1,1/                                       27
      REAL RE_SFJP_SFSL_DZ                                                27
C -                                                                       27
      REAL SFJP_SFSD_DX                                                   27
      EQUIVALENCE (BANK_SFJP(139),SFJP_SFSD_DX,RE_SFJP_SFSD_DX),          27
     *(NAMES_SFJP(137),NSFJP_SFSD_DX)                                     27
      EQUIVALENCE (COMMEN_SFJP(137),CSFJP_SFSD_DX),(MAP_SFJP(1,137),      27
     *MSFJP_SFSD_DX)                                                      27
      CHARACTER CSFJP_SFSD_DX*40/' '/                                     27
      CHARACTER NSFJP_SFSD_DX*12/'R.SFSD_DX'/                             27
      INTEGER MSFJP_SFSD_DX(2)/1,1/                                       27
      REAL RE_SFJP_SFSD_DX                                                27
C -                                                                       27
      REAL SFJP_SFSD_DY                                                   27
      EQUIVALENCE (BANK_SFJP(140),SFJP_SFSD_DY,RE_SFJP_SFSD_DY),          27
     *(NAMES_SFJP(138),NSFJP_SFSD_DY)                                     27
      EQUIVALENCE (COMMEN_SFJP(138),CSFJP_SFSD_DY),(MAP_SFJP(1,138),      27
     *MSFJP_SFSD_DY)                                                      27
      CHARACTER CSFJP_SFSD_DY*40/' '/                                     27
      CHARACTER NSFJP_SFSD_DY*12/'R.SFSD_DY'/                             27
      INTEGER MSFJP_SFSD_DY(2)/1,1/                                       27
      REAL RE_SFJP_SFSD_DY                                                27
C -                                                                       27
      REAL SFJP_SFSD_DZ                                                   27
      EQUIVALENCE (BANK_SFJP(141),SFJP_SFSD_DZ,RE_SFJP_SFSD_DZ),          27
     *(NAMES_SFJP(139),NSFJP_SFSD_DZ)                                     27
      EQUIVALENCE (COMMEN_SFJP(139),CSFJP_SFSD_DZ),(MAP_SFJP(1,139),      27
     *MSFJP_SFSD_DZ)                                                      27
      CHARACTER CSFJP_SFSD_DZ*40/' '/                                     27
      CHARACTER NSFJP_SFSD_DZ*12/'R.SFSD_DZ'/                             27
      INTEGER MSFJP_SFSD_DZ(2)/1,1/                                       27
      REAL RE_SFJP_SFSD_DZ                                                27
C -                                                                       27
      REAL SFJP_SFLA_PX                                                   27
      EQUIVALENCE (BANK_SFJP(142),SFJP_SFLA_PX,RE_SFJP_SFLA_PX),          27
     *(NAMES_SFJP(140),NSFJP_SFLA_PX)                                     27
      EQUIVALENCE (COMMEN_SFJP(140),CSFJP_SFLA_PX),(MAP_SFJP(1,140),      27
     *MSFJP_SFLA_PX)                                                      27
      CHARACTER CSFJP_SFLA_PX*40/' '/                                     27
      CHARACTER NSFJP_SFLA_PX*12/'R.SFLA_PX'/                             27
      INTEGER MSFJP_SFLA_PX(2)/1,1/                                       27
      REAL RE_SFJP_SFLA_PX                                                27
C -                                                                       27
      REAL SFJP_SFLA_PY                                                   27
      EQUIVALENCE (BANK_SFJP(143),SFJP_SFLA_PY,RE_SFJP_SFLA_PY),          27
     *(NAMES_SFJP(141),NSFJP_SFLA_PY)                                     27
      EQUIVALENCE (COMMEN_SFJP(141),CSFJP_SFLA_PY),(MAP_SFJP(1,141),      27
     *MSFJP_SFLA_PY)                                                      27
      CHARACTER CSFJP_SFLA_PY*40/' '/                                     27
      CHARACTER NSFJP_SFLA_PY*12/'R.SFLA_PY'/                             27
      INTEGER MSFJP_SFLA_PY(2)/1,1/                                       27
      REAL RE_SFJP_SFLA_PY                                                27
C -                                                                       27
      REAL SFJP_SFLC_PX                                                   27
      EQUIVALENCE (BANK_SFJP(144),SFJP_SFLC_PX,RE_SFJP_SFLC_PX),          27
     *(NAMES_SFJP(142),NSFJP_SFLC_PX)                                     27
      EQUIVALENCE (COMMEN_SFJP(142),CSFJP_SFLC_PX),(MAP_SFJP(1,142),      27
     *MSFJP_SFLC_PX)                                                      27
      CHARACTER CSFJP_SFLC_PX*40/' '/                                     27
      CHARACTER NSFJP_SFLC_PX*12/'R.SFLC_PX'/                             27
      INTEGER MSFJP_SFLC_PX(2)/1,1/                                       27
      REAL RE_SFJP_SFLC_PX                                                27
C -                                                                       27
      REAL SFJP_SFLC_PY                                                   27
      EQUIVALENCE (BANK_SFJP(145),SFJP_SFLC_PY,RE_SFJP_SFLC_PY),          27
     *(NAMES_SFJP(143),NSFJP_SFLC_PY)                                     27
      EQUIVALENCE (COMMEN_SFJP(143),CSFJP_SFLC_PY),(MAP_SFJP(1,143),      27
     *MSFJP_SFLC_PY)                                                      27
      CHARACTER CSFJP_SFLC_PY*40/' '/                                     27
      CHARACTER NSFJP_SFLC_PY*12/'R.SFLC_PY'/                             27
      INTEGER MSFJP_SFLC_PY(2)/1,1/                                       27
      REAL RE_SFJP_SFLC_PY                                                27
C -                                                                       27
      REAL SFJP_SFES_PX                                                   27
      EQUIVALENCE (BANK_SFJP(146),SFJP_SFES_PX,RE_SFJP_SFES_PX),          27
     *(NAMES_SFJP(144),NSFJP_SFES_PX)                                     27
      EQUIVALENCE (COMMEN_SFJP(144),CSFJP_SFES_PX),(MAP_SFJP(1,144),      27
     *MSFJP_SFES_PX)                                                      27
      CHARACTER CSFJP_SFES_PX*40/' '/                                     27
      CHARACTER NSFJP_SFES_PX*12/'R.SFES_PX'/                             27
      INTEGER MSFJP_SFES_PX(2)/1,1/                                       27
      REAL RE_SFJP_SFES_PX                                                27
C -                                                                       27
      REAL SFJP_SFES_PY                                                   27
      EQUIVALENCE (BANK_SFJP(147),SFJP_SFES_PY,RE_SFJP_SFES_PY),          27
     *(NAMES_SFJP(145),NSFJP_SFES_PY)                                     27
      EQUIVALENCE (COMMEN_SFJP(145),CSFJP_SFES_PY),(MAP_SFJP(1,145),      27
     *MSFJP_SFES_PY)                                                      27
      CHARACTER CSFJP_SFES_PY*40/' '/                                     27
      CHARACTER NSFJP_SFES_PY*12/'R.SFES_PY'/                             27
      INTEGER MSFJP_SFES_PY(2)/1,1/                                       27
      REAL RE_SFJP_SFES_PY                                                27
C -                                                                       27
      REAL SFJP_SFES_PZ                                                   27
      EQUIVALENCE (BANK_SFJP(148),SFJP_SFES_PZ,RE_SFJP_SFES_PZ),          27
     *(NAMES_SFJP(146),NSFJP_SFES_PZ)                                     27
      EQUIVALENCE (COMMEN_SFJP(146),CSFJP_SFES_PZ),(MAP_SFJP(1,146),      27
     *MSFJP_SFES_PZ)                                                      27
      CHARACTER CSFJP_SFES_PZ*40/' '/                                     27
      CHARACTER NSFJP_SFES_PZ*12/'R.SFES_PZ'/                             27
      INTEGER MSFJP_SFES_PZ(2)/1,1/                                       27
      REAL RE_SFJP_SFES_PZ                                                27
C -                                                                       27
      REAL SFJP_SFEB_PX                                                   27
      EQUIVALENCE (BANK_SFJP(149),SFJP_SFEB_PX,RE_SFJP_SFEB_PX),          27
     *(NAMES_SFJP(147),NSFJP_SFEB_PX)                                     27
      EQUIVALENCE (COMMEN_SFJP(147),CSFJP_SFEB_PX),(MAP_SFJP(1,147),      27
     *MSFJP_SFEB_PX)                                                      27
      CHARACTER CSFJP_SFEB_PX*40/' '/                                     27
      CHARACTER NSFJP_SFEB_PX*12/'R.SFEB_PX'/                             27
      INTEGER MSFJP_SFEB_PX(2)/1,1/                                       27
      REAL RE_SFJP_SFEB_PX                                                27
C -                                                                       27
      REAL SFJP_SFEB_PY                                                   27
      EQUIVALENCE (BANK_SFJP(150),SFJP_SFEB_PY,RE_SFJP_SFEB_PY),          27
     *(NAMES_SFJP(148),NSFJP_SFEB_PY)                                     27
      EQUIVALENCE (COMMEN_SFJP(148),CSFJP_SFEB_PY),(MAP_SFJP(1,148),      27
     *MSFJP_SFEB_PY)                                                      27
      CHARACTER CSFJP_SFEB_PY*40/' '/                                     27
      CHARACTER NSFJP_SFEB_PY*12/'R.SFEB_PY'/                             27
      INTEGER MSFJP_SFEB_PY(2)/1,1/                                       27
      REAL RE_SFJP_SFEB_PY                                                27
C -                                                                       27
      INTEGER LEN_SFJP,LLEN_SFJP,LINK_SFJP                                27
      PARAMETER (LEN_SFJP=150)                                            27
      PARAMETER (LLEN_SFJP=148)                                           27
      INTEGER FLAG_SFJP/0/,MAP_SFJP(2,LLEN_SFJP),IDNUM_SFJP(10)           27
      CHARACTER NAMES_SFJP(LLEN_SFJP)*12,COMMEN_SFJP(LLEN_SFJP)*40        27
      DATA IDNUM_SFJP/10*0/                                               27
      REAL BANK_SFJP(LEN_SFJP)                                            27
      CHARACTER*60 BPATH_SFJP/' '/                                        27
      SAVE FLAG_SFJP,MAP_SFJP,IDNUM_SFJP,NAMES_SFJP,COMMEN_SFJP,          27
     *BPATH_SFJP                                                          27
      DATA LINK_SFJP/0/,BANK_SFJP/LEN_SFJP*0/                             27
C *                                                                       27
C -                                                                       28
      REAL SFPB_HHIGHT                                                    28
      EQUIVALENCE (BANK_SFPB(3),SFPB_HHIGHT,RE_SFPB_HHIGHT),              28
     *(NAMES_SFPB(1),NSFPB_HHIGHT)                                        28
      EQUIVALENCE (COMMEN_SFPB(1),CSFPB_HHIGHT),(MAP_SFPB(1,1),           28
     *MSFPB_HHIGHT)                                                       28
      CHARACTER CSFPB_HHIGHT*40/' '/                                      28
      CHARACTER NSFPB_HHIGHT*12/'R.HHIGHT'/                               28
      INTEGER MSFPB_HHIGHT(2)/1,1/                                        28
      REAL RE_SFPB_HHIGHT                                                 28
C -                                                                       28
      REAL SFPB_KHIGHT                                                    28
      EQUIVALENCE (BANK_SFPB(4),SFPB_KHIGHT,RE_SFPB_KHIGHT),              28
     *(NAMES_SFPB(2),NSFPB_KHIGHT)                                        28
      EQUIVALENCE (COMMEN_SFPB(2),CSFPB_KHIGHT),(MAP_SFPB(1,2),           28
     *MSFPB_KHIGHT)                                                       28
      CHARACTER CSFPB_KHIGHT*40/' '/                                      28
      CHARACTER NSFPB_KHIGHT*12/'R.KHIGHT'/                               28
      INTEGER MSFPB_KHIGHT(2)/1,1/                                        28
      REAL RE_SFPB_KHIGHT                                                 28
C -                                                                       28
      REAL SFPB_HBASE                                                     28
      EQUIVALENCE (BANK_SFPB(5),SFPB_HBASE,RE_SFPB_HBASE),                28
     *(NAMES_SFPB(3),NSFPB_HBASE)                                         28
      EQUIVALENCE (COMMEN_SFPB(3),CSFPB_HBASE),(MAP_SFPB(1,3),            28
     *MSFPB_HBASE)                                                        28
      CHARACTER CSFPB_HBASE*40/' '/                                       28
      CHARACTER NSFPB_HBASE*12/'R.HBASE'/                                 28
      INTEGER MSFPB_HBASE(2)/1,1/                                         28
      REAL RE_SFPB_HBASE                                                  28
C -                                                                       28
      REAL SFPB_KBASE                                                     28
      EQUIVALENCE (BANK_SFPB(6),SFPB_KBASE,RE_SFPB_KBASE),                28
     *(NAMES_SFPB(4),NSFPB_KBASE)                                         28
      EQUIVALENCE (COMMEN_SFPB(4),CSFPB_KBASE),(MAP_SFPB(1,4),            28
     *MSFPB_KBASE)                                                        28
      CHARACTER CSFPB_KBASE*40/' '/                                       28
      CHARACTER NSFPB_KBASE*12/'R.KBASE'/                                 28
      INTEGER MSFPB_KBASE(2)/1,1/                                         28
      REAL RE_SFPB_KBASE                                                  28
C -                                                                       28
      REAL SFPB_FSIZE                                                     28
      EQUIVALENCE (BANK_SFPB(7),SFPB_FSIZE,RE_SFPB_FSIZE),                28
     *(NAMES_SFPB(5),NSFPB_FSIZE)                                         28
      EQUIVALENCE (COMMEN_SFPB(5),CSFPB_FSIZE),(MAP_SFPB(1,5),            28
     *MSFPB_FSIZE)                                                        28
      CHARACTER CSFPB_FSIZE*40/' '/                                       28
      CHARACTER NSFPB_FSIZE*12/'R.FSIZE'/                                 28
      INTEGER MSFPB_FSIZE(2)/1,1/                                         28
      REAL RE_SFPB_FSIZE                                                  28
C -                                                                       28
      REAL SFPB_ZCOOR                                                     28
      EQUIVALENCE (BANK_SFPB(8),SFPB_ZCOOR,RE_SFPB_ZCOOR),                28
     *(NAMES_SFPB(6),NSFPB_ZCOOR)                                         28
      EQUIVALENCE (COMMEN_SFPB(6),CSFPB_ZCOOR),(MAP_SFPB(1,6),            28
     *MSFPB_ZCOOR)                                                        28
      CHARACTER CSFPB_ZCOOR*40/' '/                                       28
      CHARACTER NSFPB_ZCOOR*12/'R.ZCOOR'/                                 28
      INTEGER MSFPB_ZCOOR(2)/1,1/                                         28
      REAL RE_SFPB_ZCOOR                                                  28
C -                                                                       28
      INTEGER LEN_SFPB,LLEN_SFPB,LINK_SFPB                                28
      PARAMETER (LEN_SFPB=8)                                              28
      PARAMETER (LLEN_SFPB=6)                                             28
      INTEGER FLAG_SFPB/0/,MAP_SFPB(2,LLEN_SFPB),IDNUM_SFPB(10)           28
      CHARACTER NAMES_SFPB(LLEN_SFPB)*12,COMMEN_SFPB(LLEN_SFPB)*40        28
      DATA IDNUM_SFPB/10*0/                                               28
      REAL BANK_SFPB(LEN_SFPB)                                            28
      CHARACTER*60 BPATH_SFPB/' '/                                        28
      SAVE FLAG_SFPB,MAP_SFPB,IDNUM_SFPB,NAMES_SFPB,COMMEN_SFPB,          28
     *BPATH_SFPB                                                          28
      DATA LINK_SFPB/0/,BANK_SFPB/LEN_SFPB*0/                             28
C *                                                                       28
C -                                                                       31
      REAL SFPA_VERSION                                                   31
      EQUIVALENCE (BANK_SFPA(3),SFPA_VERSION,RE_SFPA_VERSION),            31
     *(NAMES_SFPA(1),NSFPA_VERSION)                                       31
      EQUIVALENCE (COMMEN_SFPA(1),CSFPA_VERSION),(MAP_SFPA(1,1),          31
     *MSFPA_VERSION)                                                      31
      CHARACTER CSFPA_VERSION*40/' '/                                     31
      CHARACTER NSFPA_VERSION*12/'R.VERSION'/                             31
      INTEGER MSFPA_VERSION(2)/1,1/                                       31
      REAL RE_SFPA_VERSION                                                31
C -                                                                       31
      REAL SFPA_RMIN                                                      31
      EQUIVALENCE (BANK_SFPA(4),SFPA_RMIN,RE_SFPA_RMIN),(NAMES_SFPA(2),   31
     *NSFPA_RMIN)                                                         31
      EQUIVALENCE (COMMEN_SFPA(2),CSFPA_RMIN),(MAP_SFPA(1,2),             31
     *MSFPA_RMIN)                                                         31
      CHARACTER CSFPA_RMIN*40/' '/                                        31
      CHARACTER NSFPA_RMIN*12/'R.RMIN'/                                   31
      INTEGER MSFPA_RMIN(2)/1,1/                                          31
      REAL RE_SFPA_RMIN                                                   31
C -                                                                       31
      REAL SFPA_RMAX                                                      31
      EQUIVALENCE (BANK_SFPA(5),SFPA_RMAX,RE_SFPA_RMAX),(NAMES_SFPA(3),   31
     *NSFPA_RMAX)                                                         31
      EQUIVALENCE (COMMEN_SFPA(3),CSFPA_RMAX),(MAP_SFPA(1,3),             31
     *MSFPA_RMAX)                                                         31
      CHARACTER CSFPA_RMAX*40/' '/                                        31
      CHARACTER NSFPA_RMAX*12/'R.RMAX'/                                   31
      INTEGER MSFPA_RMAX(2)/1,1/                                          31
      REAL RE_SFPA_RMAX                                                   31
C -                                                                       31
      REAL SFPA_LEN                                                       31
      EQUIVALENCE (BANK_SFPA(6),SFPA_LEN,RE_SFPA_LEN),(NAMES_SFPA(4),     31
     *NSFPA_LEN)                                                          31
      EQUIVALENCE (COMMEN_SFPA(4),CSFPA_LEN),(MAP_SFPA(1,4),MSFPA_LEN)    31
      CHARACTER CSFPA_LEN*40/' '/                                         31
      CHARACTER NSFPA_LEN*12/'R.LEN'/                                     31
      INTEGER MSFPA_LEN(2)/1,1/                                           31
      REAL RE_SFPA_LEN                                                    31
C -                                                                       31
      REAL SFPA_RAD                                                       31
      EQUIVALENCE (BANK_SFPA(7),SFPA_RAD,RE_SFPA_RAD),(NAMES_SFPA(5),     31
     *NSFPA_RAD)                                                          31
      EQUIVALENCE (COMMEN_SFPA(5),CSFPA_RAD),(MAP_SFPA(1,5),MSFPA_RAD)    31
      CHARACTER CSFPA_RAD*40/' '/                                         31
      CHARACTER NSFPA_RAD*12/'R.RAD'/                                     31
      INTEGER MSFPA_RAD(2)/1,1/                                           31
      REAL RE_SFPA_RAD                                                    31
C -                                                                       31
      REAL SFPA_NSSD                                                      31
      EQUIVALENCE (BANK_SFPA(8),SFPA_NSSD,RE_SFPA_NSSD),(NAMES_SFPA(6),   31
     *NSFPA_NSSD)                                                         31
      EQUIVALENCE (COMMEN_SFPA(6),CSFPA_NSSD),(MAP_SFPA(1,6),             31
     *MSFPA_NSSD)                                                         31
      CHARACTER CSFPA_NSSD*40/' '/                                        31
      CHARACTER NSFPA_NSSD*12/'R.NSSD'/                                   31
      INTEGER MSFPA_NSSD(2)/1,1/                                          31
      REAL RE_SFPA_NSSD                                                   31
C -                                                                       31
      REAL SFPA_DMWID                                                     31
      EQUIVALENCE (BANK_SFPA(9),SFPA_DMWID,RE_SFPA_DMWID),                31
     *(NAMES_SFPA(7),NSFPA_DMWID)                                         31
      EQUIVALENCE (COMMEN_SFPA(7),CSFPA_DMWID),(MAP_SFPA(1,7),            31
     *MSFPA_DMWID)                                                        31
      CHARACTER CSFPA_DMWID*40/' '/                                       31
      CHARACTER NSFPA_DMWID*12/'R.DMWID'/                                 31
      INTEGER MSFPA_DMWID(2)/1,1/                                         31
      REAL RE_SFPA_DMWID                                                  31
C -                                                                       31
      REAL SFPA_DMTHK                                                     31
      EQUIVALENCE (BANK_SFPA(10),SFPA_DMTHK,RE_SFPA_DMTHK),               31
     *(NAMES_SFPA(8),NSFPA_DMTHK)                                         31
      EQUIVALENCE (COMMEN_SFPA(8),CSFPA_DMTHK),(MAP_SFPA(1,8),            31
     *MSFPA_DMTHK)                                                        31
      CHARACTER CSFPA_DMTHK*40/' '/                                       31
      CHARACTER NSFPA_DMTHK*12/'R.DMTHK'/                                 31
      INTEGER MSFPA_DMTHK(2)/1,1/                                         31
      REAL RE_SFPA_DMTHK                                                  31
C -                                                                       31
      REAL SFPA_DMLEN                                                     31
      EQUIVALENCE (BANK_SFPA(11),SFPA_DMLEN,RE_SFPA_DMLEN),               31
     *(NAMES_SFPA(9),NSFPA_DMLEN)                                         31
      EQUIVALENCE (COMMEN_SFPA(9),CSFPA_DMLEN),(MAP_SFPA(1,9),            31
     *MSFPA_DMLEN)                                                        31
      CHARACTER CSFPA_DMLEN*40/' '/                                       31
      CHARACTER NSFPA_DMLEN*12/'R.DMLEN'/                                 31
      INTEGER MSFPA_DMLEN(2)/1,1/                                         31
      REAL RE_SFPA_DMLEN                                                  31
C -                                                                       31
      REAL SFPA_SMWID                                                     31
      EQUIVALENCE (BANK_SFPA(12),SFPA_SMWID,RE_SFPA_SMWID),               31
     *(NAMES_SFPA(10),NSFPA_SMWID)                                        31
      EQUIVALENCE (COMMEN_SFPA(10),CSFPA_SMWID),(MAP_SFPA(1,10),          31
     *MSFPA_SMWID)                                                        31
      CHARACTER CSFPA_SMWID*40/' '/                                       31
      CHARACTER NSFPA_SMWID*12/'R.SMWID'/                                 31
      INTEGER MSFPA_SMWID(2)/1,1/                                         31
      REAL RE_SFPA_SMWID                                                  31
C -                                                                       31
      REAL SFPA_SMTHK                                                     31
      EQUIVALENCE (BANK_SFPA(13),SFPA_SMTHK,RE_SFPA_SMTHK),               31
     *(NAMES_SFPA(11),NSFPA_SMTHK)                                        31
      EQUIVALENCE (COMMEN_SFPA(11),CSFPA_SMTHK),(MAP_SFPA(1,11),          31
     *MSFPA_SMTHK)                                                        31
      CHARACTER CSFPA_SMTHK*40/' '/                                       31
      CHARACTER NSFPA_SMTHK*12/'R.SMTHK'/                                 31
      INTEGER MSFPA_SMTHK(2)/1,1/                                         31
      REAL RE_SFPA_SMTHK                                                  31
C -                                                                       31
      REAL SFPA_SMLEN                                                     31
      EQUIVALENCE (BANK_SFPA(14),SFPA_SMLEN,RE_SFPA_SMLEN),               31
     *(NAMES_SFPA(12),NSFPA_SMLEN)                                        31
      EQUIVALENCE (COMMEN_SFPA(12),CSFPA_SMLEN),(MAP_SFPA(1,12),          31
     *MSFPA_SMLEN)                                                        31
      CHARACTER CSFPA_SMLEN*40/' '/                                       31
      CHARACTER NSFPA_SMLEN*12/'R.SMLEN'/                                 31
      INTEGER MSFPA_SMLEN(2)/1,1/                                         31
      REAL RE_SFPA_SMLEN                                                  31
C -                                                                       31
      REAL SFPA_SSLEN                                                     31
      EQUIVALENCE (BANK_SFPA(15),SFPA_SSLEN,RE_SFPA_SSLEN),               31
     *(NAMES_SFPA(13),NSFPA_SSLEN)                                        31
      EQUIVALENCE (COMMEN_SFPA(13),CSFPA_SSLEN),(MAP_SFPA(1,13),          31
     *MSFPA_SSLEN)                                                        31
      CHARACTER CSFPA_SSLEN*40/' '/                                       31
      CHARACTER NSFPA_SSLEN*12/'R.SSLEN'/                                 31
      INTEGER MSFPA_SSLEN(2)/1,1/                                         31
      REAL RE_SFPA_SSLEN                                                  31
C -                                                                       31
      REAL SFPA_WPLEN                                                     31
      EQUIVALENCE (BANK_SFPA(16),SFPA_WPLEN,RE_SFPA_WPLEN),               31
     *(NAMES_SFPA(14),NSFPA_WPLEN)                                        31
      EQUIVALENCE (COMMEN_SFPA(14),CSFPA_WPLEN),(MAP_SFPA(1,14),          31
     *MSFPA_WPLEN)                                                        31
      CHARACTER CSFPA_WPLEN*40/' '/                                       31
      CHARACTER NSFPA_WPLEN*12/'R.WPLEN'/                                 31
      INTEGER MSFPA_WPLEN(2)/1,1/                                         31
      REAL RE_SFPA_WPLEN                                                  31
C -                                                                       31
      REAL SFPA_SDLEN                                                     31
      EQUIVALENCE (BANK_SFPA(17),SFPA_SDLEN,RE_SFPA_SDLEN),               31
     *(NAMES_SFPA(15),NSFPA_SDLEN)                                        31
      EQUIVALENCE (COMMEN_SFPA(15),CSFPA_SDLEN),(MAP_SFPA(1,15),          31
     *MSFPA_SDLEN)                                                        31
      CHARACTER CSFPA_SDLEN*40/' '/                                       31
      CHARACTER NSFPA_SDLEN*12/'R.SDLEN'/                                 31
      INTEGER MSFPA_SDLEN(2)/1,1/                                         31
      REAL RE_SFPA_SDLEN                                                  31
C -                                                                       31
      REAL SFPA_TILT                                                      31
      EQUIVALENCE (BANK_SFPA(18),SFPA_TILT,RE_SFPA_TILT),                 31
     *(NAMES_SFPA(16),NSFPA_TILT)                                         31
      EQUIVALENCE (COMMEN_SFPA(16),CSFPA_TILT),(MAP_SFPA(1,16),           31
     *MSFPA_TILT)                                                         31
      CHARACTER CSFPA_TILT*40/' '/                                        31
      CHARACTER NSFPA_TILT*12/'R.TILT'/                                   31
      INTEGER MSFPA_TILT(2)/1,1/                                          31
      REAL RE_SFPA_TILT                                                   31
C -                                                                       31
      REAL SFPA_CPRAD                                                     31
      EQUIVALENCE (BANK_SFPA(19),SFPA_CPRAD,RE_SFPA_CPRAD),               31
     *(NAMES_SFPA(17),NSFPA_CPRAD)                                        31
      EQUIVALENCE (COMMEN_SFPA(17),CSFPA_CPRAD),(MAP_SFPA(1,17),          31
     *MSFPA_CPRAD)                                                        31
      CHARACTER CSFPA_CPRAD*40/' '/                                       31
      CHARACTER NSFPA_CPRAD*12/'R.CPRAD'/                                 31
      INTEGER MSFPA_CPRAD(2)/1,1/                                         31
      REAL RE_SFPA_CPRAD                                                  31
C -                                                                       31
      REAL SFPA_CPRAL                                                     31
      EQUIVALENCE (BANK_SFPA(20),SFPA_CPRAL,RE_SFPA_CPRAL),               31
     *(NAMES_SFPA(18),NSFPA_CPRAL)                                        31
      EQUIVALENCE (COMMEN_SFPA(18),CSFPA_CPRAL),(MAP_SFPA(1,18),          31
     *MSFPA_CPRAL)                                                        31
      CHARACTER CSFPA_CPRAL*40/' '/                                       31
      CHARACTER NSFPA_CPRAL*12/'R.CPRAL'/                                 31
      INTEGER MSFPA_CPRAL(2)/1,1/                                         31
      REAL RE_SFPA_CPRAL                                                  31
C -                                                                       31
      REAL SFPA_CFRAD                                                     31
      EQUIVALENCE (BANK_SFPA(21),SFPA_CFRAD,RE_SFPA_CFRAD),               31
     *(NAMES_SFPA(19),NSFPA_CFRAD)                                        31
      EQUIVALENCE (COMMEN_SFPA(19),CSFPA_CFRAD),(MAP_SFPA(1,19),          31
     *MSFPA_CFRAD)                                                        31
      CHARACTER CSFPA_CFRAD*40/' '/                                       31
      CHARACTER NSFPA_CFRAD*12/'R.CFRAD'/                                 31
      INTEGER MSFPA_CFRAD(2)/1,1/                                         31
      REAL RE_SFPA_CFRAD                                                  31
C -                                                                       31
      REAL SFPA_GPTHK                                                     31
      EQUIVALENCE (BANK_SFPA(22),SFPA_GPTHK,RE_SFPA_GPTHK),               31
     *(NAMES_SFPA(20),NSFPA_GPTHK)                                        31
      EQUIVALENCE (COMMEN_SFPA(20),CSFPA_GPTHK),(MAP_SFPA(1,20),          31
     *MSFPA_GPTHK)                                                        31
      CHARACTER CSFPA_GPTHK*40/' '/                                       31
      CHARACTER NSFPA_GPTHK*12/'R.GPTHK'/                                 31
      INTEGER MSFPA_GPTHK(2)/1,1/                                         31
      REAL RE_SFPA_GPTHK                                                  31
C -                                                                       31
      INTEGER SFPA_LADDERMAP(20)                                          31
      EQUIVALENCE (BANK_SFPA(23),SFPA_LADDERMAP,RE_SFPA_LADDERMAP),       31
     *(NAMES_SFPA(21),NSFPA_LADDERMAP)                                    31
      EQUIVALENCE (COMMEN_SFPA(21),CSFPA_LADDERMAP),(MAP_SFPA(1,21),      31
     *MSFPA_LADDERMAP)                                                    31
      CHARACTER CSFPA_LADDERMAP*40/' '/                                   31
      CHARACTER NSFPA_LADDERMAP*12/'I.LADDERMAP'/                         31
      INTEGER MSFPA_LADDERMAP(2)/20,1/                                    31
      REAL RE_SFPA_LADDERMAP                                              31
C -                                                                       31
      REAL SFPA_LADDERANGLE(20)                                           31
      EQUIVALENCE (BANK_SFPA(43),SFPA_LADDERANGLE,RE_SFPA_LADDERANGLE),   31
     *(NAMES_SFPA(22),NSFPA_LADDERANGLE)                                  31
      EQUIVALENCE (COMMEN_SFPA(22),CSFPA_LADDERANGLE),(MAP_SFPA(1,22),    31
     *MSFPA_LADDERANGLE)                                                  31
      CHARACTER CSFPA_LADDERANGLE*40/' '/                                 31
      CHARACTER NSFPA_LADDERANGLE*12/'R.LADDERANGLE'/                     31
      INTEGER MSFPA_LADDERANGLE(2)/20,1/                                  31
      REAL RE_SFPA_LADDERANGLE                                            31
C -                                                                       31
      REAL SFPA_LADDERTILT(20)                                            31
      EQUIVALENCE (BANK_SFPA(63),SFPA_LADDERTILT,RE_SFPA_LADDERTILT),     31
     *(NAMES_SFPA(23),NSFPA_LADDERTILT)                                   31
      EQUIVALENCE (COMMEN_SFPA(23),CSFPA_LADDERTILT),(MAP_SFPA(1,23),     31
     *MSFPA_LADDERTILT)                                                   31
      CHARACTER CSFPA_LADDERTILT*40/' '/                                  31
      CHARACTER NSFPA_LADDERTILT*12/'R.LADDERTILT'/                       31
      INTEGER MSFPA_LADDERTILT(2)/20,1/                                   31
      REAL RE_SFPA_LADDERTILT                                             31
C -                                                                       31
      REAL SFPA_LADDERRADIUS(20)                                          31
      EQUIVALENCE (BANK_SFPA(83),SFPA_LADDERRADIUS,                       31
     *RE_SFPA_LADDERRADIUS),(NAMES_SFPA(24),NSFPA_LADDERRADIUS)           31
      EQUIVALENCE (COMMEN_SFPA(24),CSFPA_LADDERRADIUS),(MAP_SFPA(1,24),   31
     *MSFPA_LADDERRADIUS)                                                 31
      CHARACTER CSFPA_LADDERRADIUS*40/' '/                                31
      CHARACTER NSFPA_LADDERRADIUS*12/'R.LADDERRADIUS'/                   31
      INTEGER MSFPA_LADDERRADIUS(2)/20,1/                                 31
      REAL RE_SFPA_LADDERRADIUS                                           31
C -                                                                       31
      INTEGER LEN_SFPA,LLEN_SFPA,LINK_SFPA                                31
      PARAMETER (LEN_SFPA=102)                                            31
      PARAMETER (LLEN_SFPA=24)                                            31
      INTEGER FLAG_SFPA/0/,MAP_SFPA(2,LLEN_SFPA),IDNUM_SFPA(10)           31
      CHARACTER NAMES_SFPA(LLEN_SFPA)*12,COMMEN_SFPA(LLEN_SFPA)*40        31
      DATA IDNUM_SFPA/10*0/                                               31
      REAL BANK_SFPA(LEN_SFPA)                                            31
      CHARACTER*60 BPATH_SFPA/' '/                                        31
      SAVE FLAG_SFPA,MAP_SFPA,IDNUM_SFPA,NAMES_SFPA,COMMEN_SFPA,          31
     *BPATH_SFPA                                                          31
      DATA LINK_SFPA/0/,BANK_SFPA/LEN_SFPA*0/                             31
C *                                                                       31
      INTEGER ILAD,IWAF,JWAF,NC                                           32
      REAL WAFPCKLEN,DTHK,RADTILT,ANG                                     34
      REAL ESSAI,HIGHT,LNG,LENG,YOFFSET                                   36
C *                                                                       38
      CALL AGSBEGM(MODULE,IPRIN)                                          38
      IF (IPRIN.LT.0) RETURN                                              38
C Filling bank SSDP - SILICON STRIPS                                      38
      BANK_TITLE='SILICON STRIPS'                                         38
      BPATH_SSDP='SSDP*'                                                  38
      IDNUM_SSDP(1)=0                                                     39
C VERSION                                                                 39
      SSDP_VERSION=1                                                      39
      CSSDP_VERSION='VERSION'                                             39
C THERE ARE A FEW CONFIGURAIONS POSSIBLE                                  40
      SSDP_CONFIG=1                                                       40
      CSSDP_CONFIG='THERE ARE A FEW CONFIGURAIONS POSSIBLE'               40
C 0=CAVE, 1=SVTT                                                          41
      SSDP_PLACEMENT=0                                                    41
      CSSDP_PLACEMENT='0=CAVE, 1=SVTT'                                    41
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSDP',    42
     *BPATH_SSDP,IDNUM_SSDP,1, MAP_SSDP,NAMES_SSDP,COMMEN_SSDP,           42
     *BANK_SSDP,LEN_SSDP,LLEN_SSDP,LINK_SSDP,FLAG_SSDP)                   42
C *                                                                       42
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSDP',    42
     *BPATH_SSDP,IDNUM_SSDP,1, MAP_SSDP,NAMES_SSDP,COMMEN_SSDP,           42
     *BANK_SSDP,LEN_SSDP,LLEN_SSDP,LINK_SSDP,FLAG_SSDP)                   42
C *                                                                       43
      CALL AGSBEGM(MODULE,IPRIN)                                          43
      IF (IPRIN.LT.0) RETURN                                              43
C Filling bank SFJP - DIFFERENT DIMENSION PARAMETERS                      43
      BANK_TITLE='DIFFERENT DIMENSION PARAMETERS'                         43
      BPATH_SFJP='SFJP*'                                                  43
      IDNUM_SFJP(1)=0                                                     44
C NEW VERSION INFORMATION                                                 44
      SFJP_VERSION=1                                                      44
      CSFJP_VERSION='NEW VERSION INFORMATION'                             44
C ALPHA OF THE SFLA AND OTHER PARTS                                       45
      SFJP_ALPHAZ=54                                                      45
      CSFJP_ALPHAZ='ALPHA OF THE SFLA AND OTHER PARTS'                    45
C ALPHA OF OTHER PARTS                                                    46
      SFJP_ALPHAZH=45                                                     46
      CSFJP_ALPHAZH='ALPHA OF OTHER PARTS'                                46
C MINIMUM RADIUS OF THE SMALL ALUMINIUM SECTOR (OUTSIDE STRUCTURE)        47
      SFJP_SSST_RMIN=31.285                                               47
      CSFJP_SSST_RMIN='MINIMUM RADIUS OF THE SMALL ALUMINIUM SECTOR (OUT  47
     *SIDE STRUCTURE)'                                                    47
C MAXIMUM RADIOS OF THE SMALL ALUMINIUM SECTOR (OUTSIDE STRUCTURE)        48
      SFJP_SSST_RMAX=31.8                                                 48
      CSFJP_SSST_RMAX='MAXIMUM RADIOS OF THE SMALL ALUMINIUM SECTOR (OUT  48
     *SIDE STRUCTURE)'                                                    48
C WIDTH OF THE SMALL ALUMINIUM SECTOR (OUTSIDE STRUCTURE)                 49
      SFJP_SSST_WIDTH=2.5                                                 49
      CSFJP_SSST_WIDTH='WIDTH OF THE SMALL ALUMINIUM SECTOR (OUTSIDE STR  49
     *UCTURE)'                                                            49
C ADDITIONAL Z-POSITION OF THE ALUMINIUM SECTORS                          50
      SFJP_SSST_PZ=0.2                                                    50
      CSFJP_SSST_PZ='ADDITIONAL Z-POSITION OF THE ALUMINIUM SECTORS'      50
C WIDTH OF THE SMALL INNER ALUMINIUM SECTOR (OUTSIDE STRUCTURE)           51
      SFJP_SSSS_RMIN=23.3                                                 51
      CSFJP_SSSS_RMIN='WIDTH OF THE SMALL INNER ALUMINIUM SECTOR (OUTSID  51
     *E STRUCTURE)'                                                       51
C WIDTH OF THE SMALL INNER ALUMINIUM SECTOR (OUTSIDE STRUCTURE)           52
      SFJP_SSSS_WIDTH=0.5                                                 52
      CSFJP_SSSS_WIDTH='WIDTH OF THE SMALL INNER ALUMINIUM SECTOR (OUTSI  52
     *DE STRUCTURE)'                                                      52
C WIDTH OF THE SIDE OF THE SMALL RIB (OUTSIDE STRUCTURE)                  53
      SFJP_SSRS_RMIN=28.0                                                 53
      CSFJP_SSRS_RMIN='WIDTH OF THE SIDE OF THE SMALL RIB (OUTSIDE STRUC  53
     *TURE)'                                                              53
C MOTHER VOLUME OF THE LINKING BOX (SECTOR TO THE CONE)                   54
      SFJP_SSLB_DX=8.5                                                    54
      CSFJP_SSLB_DX='MOTHER VOLUME OF THE LINKING BOX (SECTOR TO THE CON  54
     *E)'                                                                 54
C X-POSITION = Y-POSITION OF THE MOTHER VOLUME OF THE LINKING BOX         55
      SFJP_SSLB_PX=19.1                                                   55
      CSFJP_SSLB_PX='X-POSITION = Y-POSITION OF THE MOTHER VOLUME OF THE  55
     * LINKING BOX'                                                       55
C MOTHER VOLUME OF THE LINKING BOX (SECTOR TO THE CONE)                   56
      SFJP_SSLB_DY=7.3                                                    56
      CSFJP_SSLB_DY='MOTHER VOLUME OF THE LINKING BOX (SECTOR TO THE CON  56
     *E)'                                                                 56
C MOTHER VOLUME OF THE LINKING BOX (SECTOR TO THE CONE)                   57
      SFJP_SSLB_DZ=2.5                                                    57
      CSFJP_SSLB_DZ='MOTHER VOLUME OF THE LINKING BOX (SECTOR TO THE CON  57
     *E)'                                                                 57
C LINKING BOX (SECTOR TO THE CONE)                                        58
      SFJP_SSBQ_DX=7.8                                                    58
      CSFJP_SSBQ_DX='LINKING BOX (SECTOR TO THE CONE)'                    58
C LINKING BOX (SECTOR TO THE CONE)                                        59
      SFJP_SSBQ_DY=7.3                                                    59
      CSFJP_SSBQ_DY='LINKING BOX (SECTOR TO THE CONE)'                    59
C LINKING BOX (SECTOR TO THE CONE)                                        60
      SFJP_SSBQ_DZ=2.5                                                    60
      CSFJP_SSBQ_DZ='LINKING BOX (SECTOR TO THE CONE)'                    60
C LINKING CROSS TOTAL THICKNESS (ALUMINUM PART)                           61
      SFJP_SSCR_THA=1.5                                                   61
      CSFJP_SSCR_THA='LINKING CROSS TOTAL THICKNESS (ALUMINUM PART)'      61
C LINKING CROSS TOTAL THICKNESS (DELRIN PART)                             62
      SFJP_SSCR_THD=1.0                                                   62
      CSFJP_SSCR_THD='LINKING CROSS TOTAL THICKNESS (DELRIN PART)'        62
C LINKING CROSS TOTAL WIDTH                                               63
      SFJP_SSCR_WD=1.2                                                    63
      CSFJP_SSCR_WD='LINKING CROSS TOTAL WIDTH'                           63
C LINKING CROSS HORIZONTAL ARM FULL LENGTH                                64
      SFJP_SSCR_HL=8.5                                                    64
      CSFJP_SSCR_HL='LINKING CROSS HORIZONTAL ARM FULL LENGTH'            64
C LINKING CROSS VERTICAL ARM FULL LENGTH                                  65
      SFJP_SSCR_VL=1.6                                                    65
      CSFJP_SSCR_VL='LINKING CROSS VERTICAL ARM FULL LENGTH'              65
C X-POSITION = Y-POSITION OF THE LINKING TUBE                             66
      SFJP_SSLT_PX=18.1                                                   66
      CSFJP_SSLT_PX='X-POSITION = Y-POSITION OF THE LINKING TUBE'         66
C RADIUS OF THE LINKING TUBE (SECTOR TO THE CONE)                         67
      SFJP_SSLT_RMAX=0.8                                                  67
      CSFJP_SSLT_RMAX='RADIUS OF THE LINKING TUBE (SECTOR TO THE CONE)'   67
C WIDTH OF THE LINKING TUBE (SECTOR OF THE CONE)                          68
      SFJP_SSLT_DZ=5.05                                                   68
      CSFJP_SSLT_DZ='WIDTH OF THE LINKING TUBE (SECTOR OF THE CONE)'      68
C WIDTH OF THE MOUNTING PLATE INSERTED IN THE CONES                       69
      SFJP_SCMP_DX=6.3                                                    69
      CSFJP_SCMP_DX='WIDTH OF THE MOUNTING PLATE INSERTED IN THE CONES'   69
C WIDTH OF THE MOUNTING PLATE INSERTED IN THE CONES                       70
      SFJP_SCMP_DY=0.4                                                    70
      CSFJP_SCMP_DY='WIDTH OF THE MOUNTING PLATE INSERTED IN THE CONES'   70
C WIDTH OF THE MOUNTING PLATE INSERTED IN THE CONES                       71
      SFJP_SCMP_DZ=3.6                                                    71
      CSFJP_SCMP_DZ='WIDTH OF THE MOUNTING PLATE INSERTED IN THE CONES'   71
C X-POSITION = Y-POSITION OF THE MOUNTING PLATE                           72
      SFJP_SCMP_PX=15.945                                                 72
      CSFJP_SCMP_PX='X-POSITION = Y-POSITION OF THE MOUNTING PLATE'       72
C MOTHER VOLUME OF THE V-SHAPE PIECE LENGTH                               73
      SFJP_SCVM_DZ=3.6                                                    73
      CSFJP_SCVM_DZ='MOTHER VOLUME OF THE V-SHAPE PIECE LENGTH'           73
C MOTHER VOLUME OF THE V-SHAPE PIECE DX                                   74
      SFJP_SCVM_DX=6.4                                                    74
      CSFJP_SCVM_DX='MOTHER VOLUME OF THE V-SHAPE PIECE DX'               74
C X-POSITION = Y-POSITION OF THE V-SHAPE PIECE                            75
      SFJP_SCVM_PX=17.00                                                  75
      CSFJP_SCVM_PX='X-POSITION = Y-POSITION OF THE V-SHAPE PIECE'        75
C MOTHER VOLUME OF THE V-SHAPE PIECE DY                                   76
      SFJP_SCVM_DY=2.6                                                    76
      CSFJP_SCVM_DY='MOTHER VOLUME OF THE V-SHAPE PIECE DY'               76
C Z-POSITION OF THE MOTHER VOLUME                                         77
      SFJP_SCVM_PZ=57.5                                                   77
      CSFJP_SCVM_PZ='Z-POSITION OF THE MOTHER VOLUME'                     77
C HEIGHT OF THE BASE OF THE V-SHAPE PIECE                                 78
      SFJP_SCVB_DY=0.5                                                    78
      CSFJP_SCVB_DY='HEIGHT OF THE BASE OF THE V-SHAPE PIECE'             78
C DX OF THE V-PLATES OF THE V-SHAPE PIECE 45 DEGREES                      79
      SFJP_SCVS_DX=2.3                                                    79
      CSFJP_SCVS_DX='DX OF THE V-PLATES OF THE V-SHAPE PIECE 45           79
     *DEGREES'                                                            79
C MEDIUM HEIGHT OF THE V-PLATES OF THE V-SHAPE PIECE 45 DEGREES           80
      SFJP_SCVS_DY=0.6                                                    80
      CSFJP_SCVS_DY='MEDIUM HEIGHT OF THE V-PLATES OF THE V-SHAPE PIECE   80
     *45 DEGREES'                                                         80
C X-POSITION OF THE V-PLATES OF THE V-SHAPE PIECE 45 DEGREES              81
      SFJP_SCVS_PX=1.0                                                    81
      CSFJP_SCVS_PX='X-POSITION OF THE V-PLATES OF THE V-SHAPE PIECE 45   81
     *DEGREES'                                                            81
C X-POSITION OF THE V-PLATES OF THE V-SHAPE PIECE 45 DEGREES              82
      SFJP_SCVS_PY=0.22                                                   82
      CSFJP_SCVS_PY='X-POSITION OF THE V-PLATES OF THE V-SHAPE PIECE 45   82
     *DEGREES'                                                            82
C DX OF THE CONNECTION BOARD (END OF THE LADDER)                          83
      SFJP_SFCO_DX=4.4                                                    83
      CSFJP_SFCO_DX='DX OF THE CONNECTION BOARD (END OF THE LADDER)'      83
C WIDTH OF THE CONNECTION BOARD (END OF THE LADDER)                       84
      SFJP_SFCO_DY=0.37                                                   84
      CSFJP_SFCO_DY='WIDTH OF THE CONNECTION BOARD (END OF THE LADDER)'   84
C LENGTH OF THE CONNECTION BOARD (END OF THE LADDER)                      85
      SFJP_SFCO_DZ=15.8                                                   85
      CSFJP_SFCO_DZ='LENGTH OF THE CONNECTION BOARD (END OF THE           85
     *LADDER)'                                                            85
C X-POSITION OF THE CONNECTION BOARD (END OF THE LADDER)                  86
      SFJP_SFCO_PX=1.7                                                    86
      CSFJP_SFCO_PX='X-POSITION OF THE CONNECTION BOARD (END OF THE LADD  86
     *ER)'                                                                86
C Y-POSITION OF THE CONNECTION BOARD (END OF THE LADDER)                  87
      SFJP_SFCO_PY=0.2                                                    87
      CSFJP_SFCO_PY='Y-POSITION OF THE CONNECTION BOARD (END OF THE LADD  87
     *ER)'                                                                87
C DX OF THE SECOND CONNECTION BOARD (SMALLER THAN THE ABOVE ONE!!)        88
      SFJP_SFCM_DX=4.0                                                    88
      CSFJP_SFCM_DX='DX OF THE SECOND CONNECTION BOARD (SMALLER THAN THE  88
     * ABOVE ONE!!)'                                                      88
C DX OF THE SMALL PART OF THE SECOND CONNECTION BOARD                     89
      SFJP_SFCS_DZ=7.5                                                    89
      CSFJP_SFCS_DZ='DX OF THE SMALL PART OF THE SECOND CONNECTION BOARD  89
     *'                                                                   89
C WIDTH OF THE KAPTON FLEX CIRCUIT BETWEEN THE CONNECTION BOARDS          90
      SFJP_SFKF_DY=0.065                                                  90
      CSFJP_SFKF_DY='WIDTH OF THE KAPTON FLEX CIRCUIT BETWEEN THE CONNEC  90
     *TION BOARDS'                                                        90
C LENGTH OF THE KAPTON FLEX CIRCUIT BETWEEN THE CONNECTION BOARDS         91
      SFJP_SFKF_DZ=8.1                                                    91
      CSFJP_SFKF_DZ='LENGTH OF THE KAPTON FLEX CIRCUIT BETWEEN THE CONNE  91
     *CTION BOARDS'                                                       91
C DX OF THE FIRST PART OF THE KAPTON FLEX CIRCUIT                         92
      SFJP_SFKF_DX=0.44                                                   92
      CSFJP_SFKF_DX='DX OF THE FIRST PART OF THE KAPTON FLEX CIRCUIT'     92
C X-POSITION OF THE FIRST PART OF THE KAPTON FLEX CIRCUIT                 93
      SFJP_SFKF_PX=0.16                                                   93
      CSFJP_SFKF_PX='X-POSITION OF THE FIRST PART OF THE KAPTON FLEX CIR  93
     *CUIT'                                                               93
C Y-POSITION OF THE FIRST PART OF THE KAPTON FLEX CIRCUIT                 94
      SFJP_SFKF_PY=2.07                                                   94
      CSFJP_SFKF_PY='Y-POSITION OF THE FIRST PART OF THE KAPTON FLEX CIR  94
     *CUIT'                                                               94
C DX OF THE SECOND PART OF THE KAPTON FLEX CIRCUIT                        95
      SFJP_SFKS_DX=1.65                                                   95
      CSFJP_SFKS_DX='DX OF THE SECOND PART OF THE KAPTON FLEX CIRCUIT'    95
C X-POSITION OF THE SECOND PART OT THE KAPTON FLEX                        96
      SFJP_SFKS_PX=0.505                                                  96
      CSFJP_SFKS_PX='X-POSITION OF THE SECOND PART OT THE KAPTON FLEX'    96
C Y-POSITION OF THE SECOND PART OT THE KAPTON FLEX                        97
      SFJP_SFKS_PY=1.585                                                  97
      CSFJP_SFKS_PY='Y-POSITION OF THE SECOND PART OT THE KAPTON FLEX'    97
C Y-POSITION OF THE MECHANICAL PARTS                                      98
      SFJP_SFPR_PY=1.79                                                   98
      CSFJP_SFPR_PY='Y-POSITION OF THE MECHANICAL PARTS'                  98
C Y-POSITION OF THE MECHANICAL PARTS 2                                    99
      SFJP_SFPB_PY=3.48                                                   99
      CSFJP_SFPB_PY='Y-POSITION OF THE MECHANICAL PARTS 2'                99
C Y-POSITION OF THE MECHANICAL PARTS 2                                    100
      SFJP_SFPB_PY2=0.08                                                  100
      CSFJP_SFPB_PY2='Y-POSITION OF THE MECHANICAL PARTS 2'               100
C DX OF THE BIG PART OF THE SECOND CONNECTION BOARD                       101
      SFJP_SFCB_DX=3.0                                                    101
      CSFJP_SFCB_DX='DX OF THE BIG PART OF THE SECOND CONNECTION BOARD'   101
C WIDTH OF THE ALUMINIUM PLATES LINKING THE LADDER TO THE SECTOR          102
      SFJP_SSBS_DY=0.5                                                    102
      CSFJP_SSBS_DY='WIDTH OF THE ALUMINIUM PLATES LINKING THE LADDER TO  102
     * THE SECTOR'                                                        102
C DX = DZ = SMALL PART OF THE ALUMINIUM PLATE LINKING THE LADDER          103
      SFJP_SSBS_DX=2.5                                                    103
      CSFJP_SSBS_DX='DX = DZ = SMALL PART OF THE ALUMINIUM PLATE LINKING  103
     * THE LADDER'                                                        103
C DX OF THE BIG PART OF THE ALUMINIUM PLATE LINKING THE LADDER            104
      SFJP_SSBB_DX=4.3                                                    104
      CSFJP_SSBB_DX='DX OF THE BIG PART OF THE ALUMINIUM PLATE LINKING T  104
     *HE LADDER'                                                          104
C DZ OF THE BIG PART OF THE ALUMINIUM PLATE LINKING THE LADDER            105
      SFJP_SSBB_DZ=1.9                                                    105
      CSFJP_SSBB_DZ='DZ OF THE BIG PART OF THE ALUMINIUM PLATE LINKING T  105
     *HE LADDER'                                                          105
C DISTANZE BETWEEN OVERLAYING FLEXES                                      106
      SFJP_FLEX_DI=0.02                                                   106
      CSFJP_FLEX_DI='DISTANZE BETWEEN OVERLAYING FLEXES'                  106
C DX OF THE OUTSIDE MECHANICAL PART (G10 RECTANGLE - BOX BASE)            107
      SFJP_SFPB_DX=4.3                                                    107
      CSFJP_SFPB_DX='DX OF THE OUTSIDE MECHANICAL PART (G10 RECTANGLE -   107
     *BOX BASE)'                                                          107
C WIDTH OF THE OUTSIDE MECHANICAL PART (G10 RECTANGLE - BOX BASE)         108
      SFJP_SFPB_DY=0.8                                                    108
      CSFJP_SFPB_DY='WIDTH OF THE OUTSIDE MECHANICAL PART (G10 RECTANGLE  108
     * - BOX BASE)'                                                       108
C LENGTH OF THE OUTSIDE MECHANICAL PART (G10 RECTANGLE - BOX BASE)        109
      SFJP_SFPBDZ=0.95                                                    109
      CSFJP_SFPBDZ='LENGTH OF THE OUTSIDE MECHANICAL PART (G10 RECTANGLE  109
     * - BOX BASE)'                                                       109
C DX OF THE EPOXY CORE OF THE ADC BOARD APPENDICE MOTHER VOLUME           110
      SFJP_SAPP_DXE=7.6                                                   110
      CSFJP_SAPP_DXE='DX OF THE EPOXY CORE OF THE ADC BOARD APPENDICE MO  110
     *THER VOLUME'                                                        110
C DX OF THE CARBON SIDE SHELL OF THE ADC BOARD APPENDICE                  111
      SFJP_SAPP_DXZ=0.04                                                  111
      CSFJP_SAPP_DXZ='DX OF THE CARBON SIDE SHELL OF THE ADC BOARD APPEN  111
     *DICE'                                                               111
C WIDTH OF THE EPOXY CORE OF THE ADC BOARD APPENDICE MOTHER VOLUME        112
      SFJP_SAPP_DY=0.5                                                    112
      CSFJP_SAPP_DY='WIDTH OF THE EPOXY CORE OF THE ADC BOARD APPENDICE   112
     *MOTHER VOLUME'                                                      112
C LENGTH OF THE ADC BOARD APPENDICE MOTHER VOLUME                         113
      SFJP_SAPP_DZ=1.275                                                  113
      CSFJP_SAPP_DZ='LENGTH OF THE ADC BOARD APPENDICE MOTHER VOLUME'     113
C 1. Y-POSITION OF THE MOTHER COLUME OF THE ADC BOARD                     114
      SFJP_SAPP_PY1=0.04                                                  114
      CSFJP_SAPP_PY1='1. Y-POSITION OF THE MOTHER COLUME OF THE ADC BOAR  114
     *D'                                                                  114
C 2. Y-POSITION OF THE MOTHER COLUME OF THE ADC BOARD                     115
      SFJP_SAPP_PY2=0.5                                                   115
      CSFJP_SAPP_PY2='2. Y-POSITION OF THE MOTHER COLUME OF THE ADC BOAR  115
     *D'                                                                  115
C 3. Y-POSITION OF THE MOTHER COLUME OF THE ADC BOARD                     116
      SFJP_SAPP_PY3=0.08                                                  116
      CSFJP_SAPP_PY3='3. Y-POSITION OF THE MOTHER COLUME OF THE ADC BOAR  116
     *D'                                                                  116
C DX OF THE MOTHER VOLUME OF THE ADC BOARD                                117
      SFJP_SFAM_DXE=7.2                                                   117
      CSFJP_SFAM_DXE='DX OF THE MOTHER VOLUME OF THE ADC BOARD'           117
C DX OF THE SMALL VOLUME OF THE ADC BOARD                                 118
      SFJP_SFAM_DXZ=2.1                                                   118
      CSFJP_SFAM_DXZ='DX OF THE SMALL VOLUME OF THE ADC BOARD'            118
C WIDTH OF THE MOTHER VOLUME OF THE ADC BOARD                             119
      SFJP_SFAM_DY=0.3                                                    119
      CSFJP_SFAM_DY='WIDTH OF THE MOTHER VOLUME OF THE ADC BOARD'         119
C LENGTH OF THE MOTHER VOLUME OF THE ADC BOARD                            120
      SFJP_SFAM_DZ=12.4                                                   120
      CSFJP_SFAM_DZ='LENGTH OF THE MOTHER VOLUME OF THE ADC BOARD'        120
C LENGTH OF THE SMALL VOLUME OF THE ADC BOARD                             121
      SFJP_SFAM_DZS=2.15                                                  121
      CSFJP_SFAM_DZS='LENGTH OF THE SMALL VOLUME OF THE ADC BOARD'        121
C DX OF THE LONG PART OF THE BUS CABLE LINKING THE MODULES                122
      SFJP_SFLA_DX=1.5                                                    122
      CSFJP_SFLA_DX='DX OF THE LONG PART OF THE BUS CABLE LINKING THE MO  122
     *DULES'                                                              122
C WIDTH OF THE LONG PART OF THE BUS CABLE LINKING THE MODULES             123
      SFJP_SFLA_DY=0.0130                                                 123
      CSFJP_SFLA_DY='WIDTH OF THE LONG PART OF THE BUS CABLE LINKING THE  123
     * MODULES'                                                           123
C LENGTH OF THE LONG PART OF THE BUS CABLE LINKING THE MODULES            124
      SFJP_SFLA_DZ=4.3                                                    124
      CSFJP_SFLA_DZ='LENGTH OF THE LONG PART OF THE BUS CABLE LINKING TH  124
     *E MODULES'                                                          124
C LENGTH OF THE LONG BUS CABLE                                            125
      SFJP_SFLB_DZ=7.5                                                    125
      CSFJP_SFLB_DZ='LENGTH OF THE LONG BUS CABLE'                        125
C LENGTH OF THE LONG BUS CABLE ON THE CB UP TO THE CONNECTOR              126
      SFJP_SFLC_DZ=0.93                                                   126
      CSFJP_SFLC_DZ='LENGTH OF THE LONG BUS CABLE ON THE CB UP TO THE CO  126
     *NNECTOR'                                                            126
C DX OF THE BIG BUS ELBOW                                                 127
      SFJP_SFEB_DX=3.89                                                   127
      CSFJP_SFEB_DX='DX OF THE BIG BUS ELBOW'                             127
C LENGTH OF THE BUS ELBOW                                                 128
      SFJP_SFEB_DZ=1.5                                                    128
      CSFJP_SFEB_DZ='LENGTH OF THE BUS ELBOW'                             128
C DX OF THE BIG BUS ELBOW                                                 129
      SFJP_SFES_DX=1.875                                                  129
      CSFJP_SFES_DX='DX OF THE BIG BUS ELBOW'                             129
C DX OF THE HORIZONTAL PART OF THE LADDER SKELETON CARBON BASE            130
      SFJP_SFFK_DXE=0.4                                                   130
      CSFJP_SFFK_DXE='DX OF THE HORIZONTAL PART OF THE LADDER SKELETON C  130
     *ARBON BASE'                                                         130
C WIDTH OF THE HORIZONTAL PART OF THE LADDER SKELETON CARBON BASE         131
      SFJP_SFFK_DY=0.02                                                   131
      CSFJP_SFFK_DY='WIDTH OF THE HORIZONTAL PART OF THE LADDER SKELETON  131
     * CARBON BASE'                                                       131
C LENGTHOF OF THE HORIZONTAL PART OF THE LADDER SKELETON CARBON BASE      132
      SFJP_SFFK_DZ=69.75                                                  132
      CSFJP_SFFK_DZ='LENGTHOF OF THE HORIZONTAL PART OF THE LADDER SKELE  132
     *TON CARBON BASE'                                                    132
C DX OF THE TILTED PART OF THE LADDER                                     133
      SFJP_SFFL_DX=0.6                                                    133
      CSFJP_SFFL_DX='DX OF THE TILTED PART OF THE LADDER'                 133
C DX OF THE HORIZONTAL PART OF THE KAPTON FILM                            134
      SFJP_SFFK_DXZ=5.1                                                   134
      CSFJP_SFFK_DXZ='DX OF THE HORIZONTAL PART OF THE KAPTON FILM'       134
C X-POSITION OF THE CARBON BASE UNDER THE LADDER                          135
      SFJP_SFFK_PX=3.5                                                    135
      CSFJP_SFFK_PX='X-POSITION OF THE CARBON BASE UNDER THE LADDER'      135
C 1. Y-POSITION OF THE CARBON BASE UNDER THE LADDER                       136
      SFJP_SFFK_PY1=0.02                                                  136
      CSFJP_SFFK_PY1='1. Y-POSITION OF THE CARBON BASE UNDER THE          136
     *LADDER'                                                             136
C 2. Y-POSITION OF THE CARBON BASE UNDER THE LADDER                       137
      SFJP_SFFK_PY2=0.04                                                  137
      CSFJP_SFFK_PY2='2. Y-POSITION OF THE CARBON BASE UNDER THE          137
     *LADDER'                                                             137
C X-POSITION OF THE TILTED KAPTON FILM UNDER THE LADDER                   138
      SFJP_SFKL_PX=2.55                                                   138
      CSFJP_SFKL_PX='X-POSITION OF THE TILTED KAPTON FILM UNDER THE LADD  138
     *ER'                                                                 138
C WIDTH OF THE KAPTON FILM                                                139
      SFJP_SFKK_DY=0.0025                                                 139
      CSFJP_SFKK_DY='WIDTH OF THE KAPTON FILM'                            139
C LENGTH OF THE TRIANGULAR LADDER SKELETON                                140
      SFJP_SFLU_DZ=49.8                                                   140
      CSFJP_SFLU_DZ='LENGTH OF THE TRIANGULAR LADDER SKELETON'            140
C HEIGHT OF THE TRIANGULAR LADDER SKELETON                                141
      SFJP_SFLU_H1=0.02                                                   141
      CSFJP_SFLU_H1='HEIGHT OF THE TRIANGULAR LADDER SKELETON'            141
C BASE LENGTH OF THE TRIANGULAR LADDER SKELETON                           142
      SFJP_SFLU_BL1=0.2                                                   142
      CSFJP_SFLU_BL1='BASE LENGTH OF THE TRIANGULAR LADDER SKELETON'      142
C DX OF THE HYBRID STIFFNER                                               143
      SFJP_SFRA_DX=3.76                                                   143
      CSFJP_SFRA_DX='DX OF THE HYBRID STIFFNER'                           143
C WIDTH OF THE HYBRID STIFFNER                                            144
      SFJP_SFRA_DY=0.019                                                  144
      CSFJP_SFRA_DY='WIDTH OF THE HYBRID STIFFNER'                        144
C Y-POSITION OF THE HYBRID STIFFNER                                       145
      SFJP_SFRA_PY=0.099                                                  145
      CSFJP_SFRA_PY='Y-POSITION OF THE HYBRID STIFFNER'                   145
C LENGTH OF THE HYBRID STIFFNER                                           146
      SFJP_SFRA_DZ=1.0                                                    146
      CSFJP_SFRA_DZ='LENGTH OF THE HYBRID STIFFNER'                       146
C Z-POSITION OF THE HYBRID STIFFNER                                       147
      SFJP_SFRA_PZ=1.1                                                    147
      CSFJP_SFRA_PZ='Z-POSITION OF THE HYBRID STIFFNER'                   147
C WIDTH OF A SINGLE WAFER CONTAINER                                       148
      SFJP_SFSW_DY=1.0                                                    148
      CSFJP_SFSW_DY='WIDTH OF A SINGLE WAFER CONTAINER'                   148
C LENGTHOF OF A SINGLE WAFER CONTAINER                                    149
      SFJP_SFSW_DZ=2.25                                                   149
      CSFJP_SFSW_DZ='LENGTHOF OF A SINGLE WAFER CONTAINER'                149
C DX OF THE TWO SUPPORTS OF THE HYBRID STIFFNER                           150
      SFJP_SFRS_DX=0.04                                                   150
      CSFJP_SFRS_DX='DX OF THE TWO SUPPORTS OF THE HYBRID STIFFNER'       150
C DY OF THE TWO SUPPORTS OF THE HYBRID STIFFNER                           151
      SFJP_SFRS_DY=0.0915                                                 151
      CSFJP_SFRS_DY='DY OF THE TWO SUPPORTS OF THE HYBRID STIFFNER'       151
C X-POSITION OF THE TWO SUPPORTS OF THE HYBRID STIFFNER                   152
      SFJP_SFRS_PX=3.8                                                    152
      CSFJP_SFRS_PX='X-POSITION OF THE TWO SUPPORTS OF THE HYBRID STIFFN  152
     *ER'                                                                 152
C Y-POSITION OF THE TWO SUPPORTS OF THE HYBRID STIFFNER                   153
      SFJP_SFRS_PY=0.0265                                                 153
      CSFJP_SFRS_PY='Y-POSITION OF THE TWO SUPPORTS OF THE HYBRID STIFFN  153
     *ER'                                                                 153
C LENGTH OF THE TRIANGULAR BASE                                           154
      SFJP_SFSM_LL=2.6                                                    154
      CSFJP_SFSM_LL='LENGTH OF THE TRIANGULAR BASE'                       154
C DX OF THE FLEX                                                          155
      SFJP_SFFX_DX=3.84                                                   155
      CSFJP_SFFX_DX='DX OF THE FLEX'                                      155
C WIDTH OF THE FLEX                                                       156
      SFJP_SFFX_DYZ=0.0007                                                156
      CSFJP_SFFX_DYZ='WIDTH OF THE FLEX'                                  156
C RMIN OF THE PIONS                                                       157
      SFJP_SFPI_RMIN=0.15                                                 157
      CSFJP_SFPI_RMIN='RMIN OF THE PIONS'                                 157
C RMAX OF THE PIONS                                                       158
      SFJP_SFPI_RMAX=0.2                                                  158
      CSFJP_SFPI_RMAX='RMAX OF THE PIONS'                                 158
C X-POSITION OF THE FOUR PIONS                                            159
      SFJP_SFPI_PX=3.2                                                    159
      CSFJP_SFPI_PX='X-POSITION OF THE FOUR PIONS'                        159
C 1. Y-POSITION OF THE FOUR PIONS                                         160
      SFJP_SFPI_PY1=0.35                                                  160
      CSFJP_SFPI_PY1='1. Y-POSITION OF THE FOUR PIONS'                    160
C 2. Y-POSITION OF THE FOUR PIONS                                         161
      SFJP_SFPI_PY2=0.025                                                 161
      CSFJP_SFPI_PY2='2. Y-POSITION OF THE FOUR PIONS'                    161
C Z-POSITION OF THE FOUR PIONS                                            162
      SFJP_SFPI_PZ=0.7                                                    162
      CSFJP_SFPI_PZ='Z-POSITION OF THE FOUR PIONS'                        162
C 1. DX OF THE BASE OF THE PIONS                                          163
      SFJP_SFPJ_DX1=0.075                                                 163
      CSFJP_SFPJ_DX1='1. DX OF THE BASE OF THE PIONS'                     163
C 2. DX OF THE BASE OF THE PIONS                                          164
      SFJP_SFPJ_DX2=0.025                                                 164
      CSFJP_SFPJ_DX2='2. DX OF THE BASE OF THE PIONS'                     164
C DY OF THE BASE OF THE PIONS                                             165
      SFJP_SFPJ_DY=0.3                                                    165
      CSFJP_SFPJ_DY='DY OF THE BASE OF THE PIONS'                         165
C DZ OF THE BASE OF THE PIONS                                             166
      SFJP_SFPJ_DZ=0.0125                                                 166
      CSFJP_SFPJ_DZ='DZ OF THE BASE OF THE PIONS'                         166
C DX OF THE A128C CHIP                                                    167
      SFJP_SFAA_DX=0.3                                                    167
      CSFJP_SFAA_DX='DX OF THE A128C CHIP'                                167
C DY OF THE A128C CHIP                                                    168
      SFJP_SFAA_DY=0.015                                                  168
      CSFJP_SFAA_DY='DY OF THE A128C CHIP'                                168
C DZ OF THE A128C CHIP                                                    169
      SFJP_SFAA_DZ=0.4                                                    169
      CSFJP_SFAA_DZ='DZ OF THE A128C CHIP'                                169
C 1. X=^FYLL SFJP 01-0 SFAA_PX1 = 0.325 ! 1. X-POSITION OF THE A128C CHI  170
C PS                                                                      170
      SFJP_SFAA_PX1=0.325                                                 170
      CSFJP_SFAA_PX1='1. X=^FYLL SFJP 01-0 SFAA_PX1 = 0.325 ! 1. X-POSIT  170
     *ION OF THE A128C CHIPS'                                             170
C 2. X=^FYLL SFJP 01-0 SFAA_PX2 = 0.65 ! 2. X-POSITION OF THE A128C CHIP  171
C S                                                                       171
      SFJP_SFAA_PX2=0.65                                                  171
      CSFJP_SFAA_PX2='2. X=^FYLL SFJP 01-0 SFAA_PX2 = 0.65 ! 2. X-POSITI  171
     *ON OF THE A128C CHIPS'                                              171
C 3. X=^FYLL SFJP 01-0 SFAA_PX3 = 0.6 ! 3. X-POSITION OF THE A128C CHIPS  172
      SFJP_SFAA_PX3=0.6                                                   172
      CSFJP_SFAA_PX3='3. X=^FYLL SFJP 01-0 SFAA_PX3 = 0.6 ! 3. X-POSITIO  172
     *N OF THE A128C CHIPS'                                               172
C 1. Z=^FYLL SFJP 01-0 SFAA_PZ1 = 1.1 ! 1. Z-POSITION OF THE A128C CHIPS  173
      SFJP_SFAA_PZ1=1.1                                                   173
      CSFJP_SFAA_PZ1='1. Z=^FYLL SFJP 01-0 SFAA_PZ1 = 1.1 ! 1. Z-POSITIO  173
     *N OF THE A128C CHIPS'                                               173
C 2. Z=^FYLL SFJP 01-0 SFAA_PZ2 = 1.0 ! 2. Z-POSITION OF THE A128C CHIPS  174
      SFJP_SFAA_PZ2=1.0                                                   174
      CSFJP_SFAA_PZ2='2. Z=^FYLL SFJP 01-0 SFAA_PZ2 = 1.0 ! 2. Z-POSITIO  174
     *N OF THE A128C CHIPS'                                               174
C 3. Z=^FYLL SFJP 01-0 SFAA_PZ3 = 0.02 ! 3. Z-POSITION OF THE A128C CHIP  175
C S                                                                       175
      SFJP_SFAA_PZ3=0.02                                                  175
      CSFJP_SFAA_PZ3='3. Z=^FYLL SFJP 01-0 SFAA_PZ3 = 0.02 ! 3. Z-POSITI  175
     *ON OF THE A128C CHIPS'                                              175
C DX OF THE STRIP DETECTOR                                                176
      SFJP_SFSL_DX=3.75                                                   176
      CSFJP_SFSL_DX='DX OF THE STRIP DETECTOR'                            176
C DY OF THE STRIP DETECTOR                                                177
      SFJP_SFSL_DY=0.015                                                  177
      CSFJP_SFSL_DY='DY OF THE STRIP DETECTOR'                            177
C DZ OF THE STRIP DETECTOR                                                178
      SFJP_SFSL_DZ=2.1                                                    178
      CSFJP_SFSL_DZ='DZ OF THE STRIP DETECTOR'                            178
C DX OF THE SENSITIVE AREA                                                179
      SFJP_SFSD_DX=3.65                                                   179
      CSFJP_SFSD_DX='DX OF THE SENSITIVE AREA'                            179
C DY OF THE SENSITIVE AREA                                                180
      SFJP_SFSD_DY=0.015                                                  180
      CSFJP_SFSD_DY='DY OF THE SENSITIVE AREA'                            180
C DZ OF THE SENSITIVE AREA                                                181
      SFJP_SFSD_DZ=2.0                                                    181
      CSFJP_SFSD_DZ='DZ OF THE SENSITIVE AREA'                            181
C X-POSITION OF THE TOP FLEX ALONG THE Z-AXIS                             182
      SFJP_SFLA_PX=1.1                                                    182
      CSFJP_SFLA_PX='X-POSITION OF THE TOP FLEX ALONG THE Z-AXIS'         182
C Y-POSITION OF THE TOP FLEX ALONG THE Z-AXIS                             183
      SFJP_SFLA_PY=1.35                                                   183
      CSFJP_SFLA_PY='Y-POSITION OF THE TOP FLEX ALONG THE Z-AXIS'         183
C X-POSITION OF THE BOTTOM FLEX ALONG THE Z-AXIS                          184
      SFJP_SFLC_PX=2.3                                                    184
      CSFJP_SFLC_PX='X-POSITION OF THE BOTTOM FLEX ALONG THE Z-AXIS'      184
C Y-POSITION OF THE BOTTOM FLEX ALONG THE Z-AXIS                          185
      SFJP_SFLC_PY=0.3                                                    185
      CSFJP_SFLC_PY='Y-POSITION OF THE BOTTOM FLEX ALONG THE Z-AXIS'      185
C X-POSITION OF THE SMALL ELBOW BUS                                       186
      SFJP_SFES_PX=3.32                                                   186
      CSFJP_SFES_PX='X-POSITION OF THE SMALL ELBOW BUS'                   186
C Y-POSITION OF THE SMALL ELBOW BUS                                       187
      SFJP_SFES_PY=1.6                                                    187
      CSFJP_SFES_PY='Y-POSITION OF THE SMALL ELBOW BUS'                   187
C Z-POSITION OF THE SMALL ELBOW BUS                                       188
      SFJP_SFES_PZ=0.1                                                    188
      CSFJP_SFES_PZ='Z-POSITION OF THE SMALL ELBOW BUS'                   188
C X-POSITION OF THE BIG ELBOW BUS                                         189
      SFJP_SFEB_PX=2.71                                                   189
      CSFJP_SFEB_PX='X-POSITION OF THE BIG ELBOW BUS'                     189
C Y-POSITION OF THE BIG ELBOW BUS                                         190
      SFJP_SFEB_PY=0.75                                                   190
      CSFJP_SFEB_PY='Y-POSITION OF THE BIG ELBOW BUS'                     190
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFJP',    191
     *BPATH_SFJP,IDNUM_SFJP,1, MAP_SFJP,NAMES_SFJP,COMMEN_SFJP,           191
     *BANK_SFJP,LEN_SFJP,LLEN_SFJP,LINK_SFJP,FLAG_SFJP)                   191
C *                                                                       191
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFJP',    191
     *BPATH_SFJP,IDNUM_SFJP,1, MAP_SFJP,NAMES_SFJP,COMMEN_SFJP,           191
     *BANK_SFJP,LEN_SFJP,LLEN_SFJP,LINK_SFJP,FLAG_SFJP)                   191
C *                                                                       192
      CALL AGSBEGM(MODULE,IPRIN)                                          192
      IF (IPRIN.LT.0) RETURN                                              192
C Filling bank SFJP - DIFFERENT DIMENSION PARAMETERS                      192
      BANK_TITLE='DIFFERENT DIMENSION PARAMETERS'                         192
      BPATH_SFJP='SFJP*'                                                  192
      IDNUM_SFJP(1)=0                                                     193
C VERSION                                                                 193
      SFJP_VERSION=2                                                      193
      CSFJP_VERSION='VERSION'                                             193
C WIDTH OF THE SIDE OF THE SMALL RIB (OUTSIDE STRUCTURE)                  194
      SFJP_SSRS_RMIN=23.3                                                 194
      CSFJP_SSRS_RMIN='WIDTH OF THE SIDE OF THE SMALL RIB (OUTSIDE STRUC  194
     *TURE)'                                                              194
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFJP',    195
     *BPATH_SFJP,IDNUM_SFJP,1, MAP_SFJP,NAMES_SFJP,COMMEN_SFJP,           195
     *BANK_SFJP,LEN_SFJP,LLEN_SFJP,LINK_SFJP,FLAG_SFJP)                   195
C *                                                                       195
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFJP',    195
     *BPATH_SFJP,IDNUM_SFJP,1, MAP_SFJP,NAMES_SFJP,COMMEN_SFJP,           195
     *BANK_SFJP,LEN_SFJP,LLEN_SFJP,LINK_SFJP,FLAG_SFJP)                   195
C *                                                                       196
      CALL AGSBEGM(MODULE,IPRIN)                                          196
      IF (IPRIN.LT.0) RETURN                                              196
C Filling bank SFPB - SOME SSD SHELL DIMENSIONS                           196
      BANK_TITLE='SOME SSD SHELL DIMENSIONS'                              196
      BPATH_SFPB='SFPB*'                                                  196
      IDNUM_SFPB(1)=0                                                     197
C HAUT 1                                                                  197
      SFPB_HHIGHT=(0.02/TAN(54*PI/180)+0.02/(2*TAN(PI/5))+0.2)*SIN(PI/    197
     *5)                                                                  197
      CSFPB_HHIGHT='HAUT 1'                                               197
C HAUT 2                                                                  198
      SFPB_KHIGHT=0.02/SIN(54*PI/180)                                     198
      CSFPB_KHIGHT='HAUT 2'                                               198
C BAS 1                                                                   199
      SFPB_HBASE=(0.02/TAN(63*PI/180)+0.01/TAN(27*PI/                     199
     *180)+0.2)*SIN(27*PI/180)                                            199
      CSFPB_HBASE='BAS 1'                                                 199
C BAS 2                                                                   200
      SFPB_KBASE=0.02/SIN(63*PI/180)                                      200
      CSFPB_KBASE='BAS 2'                                                 200
C LENGHT OF THE SIDE KAPTON FILM                                          201
      SFPB_FSIZE=0.6/COS(15*PI/180)+0.02*TAN(15*PI/180)                   201
      CSFPB_FSIZE='LENGHT OF THE SIDE KAPTON FILM'                        201
C Z COORD                                                                 202
      SFPB_ZCOOR=5.18462095393834410                                      202
      CSFPB_ZCOOR='Z COORD'                                               202
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPB',    203
     *BPATH_SFPB,IDNUM_SFPB,1, MAP_SFPB,NAMES_SFPB,COMMEN_SFPB,           203
     *BANK_SFPB,LEN_SFPB,LLEN_SFPB,LINK_SFPB,FLAG_SFPB)                   203
C *                                                                       203
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPB',    203
     *BPATH_SFPB,IDNUM_SFPB,1, MAP_SFPB,NAMES_SFPB,COMMEN_SFPB,           203
     *BANK_SFPB,LEN_SFPB,LLEN_SFPB,LINK_SFPB,FLAG_SFPB)                   203
C *                                                                       204
      CALL AGSBEGM(MODULE,IPRIN)                                          204
      IF (IPRIN.LT.0) RETURN                                              204
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   204
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      204
      BPATH_SFPA='SFPA*'                                                  204
      IDNUM_SFPA(1)=0                                                     205
C GEOMETRY VERSION                                                        205
      SFPA_VERSION=1                                                      205
      CSFPA_VERSION='GEOMETRY VERSION'                                    205
C MOTHER RMIN                                                             206
      SFPA_RMIN=21.8                                                      206
      CSFPA_RMIN='MOTHER RMIN'                                            206
C MOTHER RMAX                                                             207
      SFPA_RMAX=32.                                                       207
      CSFPA_RMAX='MOTHER RMAX'                                            207
C MOTHER LEN ALONG THE Z DIRECTION                                        208
      SFPA_LEN=107.                                                       208
      CSFPA_LEN='MOTHER LEN ALONG THE Z DIRECTION'                        208
C DISTANCE FROM BEAM AXIS TO DETECTOR CENTER                              209
      SFPA_RAD=23.                                                        209
      CSFPA_RAD='DISTANCE FROM BEAM AXIS TO DETECTOR CENTER'              209
C NUMBER OF SILICON STRIP DETECTORS                                       210
      SFPA_NSSD=16                                                        210
      CSFPA_NSSD='NUMBER OF SILICON STRIP DETECTORS'                      210
C DETECTOR MOTHER WIDTH                                                   211
      SFPA_DMWID=7.8                                                      211
      CSFPA_DMWID='DETECTOR MOTHER WIDTH'                                 211
C DETECTOR MOTHER THICKNESS                                               212
      SFPA_DMTHK=2.0                                                      212
      CSFPA_DMTHK='DETECTOR MOTHER THICKNESS'                             212
C DETECTOR MOTHER LENGTH (DETECTORS + ADC BOARD)                          213
      SFPA_DMLEN=90.                                                      213
      CSFPA_DMLEN='DETECTOR MOTHER LENGTH (DETECTORS + ADC BOARD)'        213
C STRUCTURE MOTHER WIDTH                                                  214
      SFPA_SMWID=7.5                                                      214
      CSFPA_SMWID='STRUCTURE MOTHER WIDTH'                                214
C STRUCTURE MOTHER THICKNESS                                              215
      SFPA_SMTHK=5.0                                                      215
      CSFPA_SMTHK='STRUCTURE MOTHER THICKNESS'                            215
C STRUCTURE MOTHER LENGTH                                                 216
      SFPA_SMLEN=101.9                                                    216
      CSFPA_SMLEN='STRUCTURE MOTHER LENGTH'                               216
C LENGTH OF A SUBVOLUME OF THE STRUCTURE                                  217
      SFPA_SSLEN=95./20.                                                  217
      CSFPA_SSLEN='LENGTH OF A SUBVOLUME OF THE STRUCTURE'                217
C LENGTH OF WAFER PACK                                                    218
      SFPA_WPLEN=69.6                                                     218
      CSFPA_WPLEN='LENGTH OF WAFER PACK'                                  218
C LENGTH OF ONE STRIP DETECTOR (ALONG BEAM AXIS)                          219
      SFPA_SDLEN=4.2                                                      219
      CSFPA_SDLEN='LENGTH OF ONE STRIP DETECTOR (ALONG BEAM AXIS)'        219
C TILING ANGLE (DEGREES)                                                  220
      SFPA_TILT=5.0                                                       220
      CSFPA_TILT='TILING ANGLE (DEGREES)'                                 220
C COOLING PIPE OUTER RADIUS                                               221
      SFPA_CPRAD=0.1                                                      221
      CSFPA_CPRAD='COOLING PIPE OUTER RADIUS'                             221
C COOLING PIPE INNER RADIUS                                               222
      SFPA_CPRAL=0.09                                                     222
      CSFPA_CPRAL='COOLING PIPE INNER RADIUS'                             222
C CARBON FIBER TUBE RADIUS (SUPPORT STRUCTURE)                            223
      SFPA_CFRAD=0.1                                                      223
      CSFPA_CFRAD='CARBON FIBER TUBE RADIUS (SUPPORT STRUCTURE)'          223
C GAP BETWEEN STRUCTURE MOTHER AND DETECTOR                               224
      SFPA_GPTHK=-1.0                                                     224
      CSFPA_GPTHK='GAP BETWEEN STRUCTURE MOTHER AND DETECTOR'             224
C PRESENCE OF LADDERS                                                     226
      SFPA_LADDERMAP(1)=0                                                 226
      SFPA_LADDERMAP(2)=0                                                 226
      SFPA_LADDERMAP(3)=1                                                 226
      SFPA_LADDERMAP(4)=0                                                 226
      SFPA_LADDERMAP(5)=0                                                 226
      SFPA_LADDERMAP(6)=0                                                 226
      SFPA_LADDERMAP(7)=0                                                 226
      SFPA_LADDERMAP(8)=0                                                 226
      SFPA_LADDERMAP(9)=0                                                 226
      SFPA_LADDERMAP(10)=0                                                226
      SFPA_LADDERMAP(11)=0                                                226
      SFPA_LADDERMAP(12)=0                                                226
      SFPA_LADDERMAP(13)=0                                                226
      SFPA_LADDERMAP(14)=0                                                226
      SFPA_LADDERMAP(15)=0                                                226
      SFPA_LADDERMAP(16)=0                                                226
      SFPA_LADDERMAP(17)=0                                                226
      SFPA_LADDERMAP(18)=0                                                226
      SFPA_LADDERMAP(19)=0                                                226
      SFPA_LADDERMAP(20)=0                                                226
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               226
C INDIVIDUAL ANGLES                                                       230
      SFPA_LADDERANGLE(1)=-1.0                                            230
      SFPA_LADDERANGLE(2)=-1.0                                            230
      SFPA_LADDERANGLE(3)=45.0                                            230
      SFPA_LADDERANGLE(4)=-1.0                                            230
      SFPA_LADDERANGLE(5)=-1.0                                            230
      SFPA_LADDERANGLE(6)=-1.0                                            230
      SFPA_LADDERANGLE(7)=-1.0                                            230
      SFPA_LADDERANGLE(8)=-1.0                                            230
      SFPA_LADDERANGLE(9)=-1.0                                            230
      SFPA_LADDERANGLE(10)=-1.0                                           230
      SFPA_LADDERANGLE(11)=-1.0                                           230
      SFPA_LADDERANGLE(12)=-1.0                                           230
      SFPA_LADDERANGLE(13)=-1.0                                           230
      SFPA_LADDERANGLE(14)=-1.0                                           230
      SFPA_LADDERANGLE(15)=-1.0                                           230
      SFPA_LADDERANGLE(16)=-1.0                                           230
      SFPA_LADDERANGLE(17)=-1.0                                           230
      SFPA_LADDERANGLE(18)=-1.0                                           230
      SFPA_LADDERANGLE(19)=-1.0                                           230
      SFPA_LADDERANGLE(20)=-1.0                                           230
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               230
C INDIVIDUAL TILTS                                                        234
      SFPA_LADDERTILT(1)=-1.0                                             234
      SFPA_LADDERTILT(2)=-1.0                                             234
      SFPA_LADDERTILT(3)=0.0                                              234
      SFPA_LADDERTILT(4)=-1.0                                             234
      SFPA_LADDERTILT(5)=-1.0                                             234
      SFPA_LADDERTILT(6)=-1.0                                             234
      SFPA_LADDERTILT(7)=-1.0                                             234
      SFPA_LADDERTILT(8)=-1.0                                             234
      SFPA_LADDERTILT(9)=-1.0                                             234
      SFPA_LADDERTILT(10)=-1.0                                            234
      SFPA_LADDERTILT(11)=-1.0                                            234
      SFPA_LADDERTILT(12)=-1.0                                            234
      SFPA_LADDERTILT(13)=-1.0                                            234
      SFPA_LADDERTILT(14)=-1.0                                            234
      SFPA_LADDERTILT(15)=-1.0                                            234
      SFPA_LADDERTILT(16)=-1.0                                            234
      SFPA_LADDERTILT(17)=-1.0                                            234
      SFPA_LADDERTILT(18)=-1.0                                            234
      SFPA_LADDERTILT(19)=-1.0                                            234
      SFPA_LADDERTILT(20)=-1.0                                            234
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 234
C INDIVIDUAL RADII                                                        239
      SFPA_LADDERRADIUS(1)=-1.0                                           239
      SFPA_LADDERRADIUS(2)=-1.0                                           239
      SFPA_LADDERRADIUS(3)=23.000                                         239
      SFPA_LADDERRADIUS(4)=-1.0                                           239
      SFPA_LADDERRADIUS(5)=-1.0                                           239
      SFPA_LADDERRADIUS(6)=-1.0                                           239
      SFPA_LADDERRADIUS(7)=-1.0                                           239
      SFPA_LADDERRADIUS(8)=-1.0                                           239
      SFPA_LADDERRADIUS(9)=-1.0                                           239
      SFPA_LADDERRADIUS(10)=-1.0                                          239
      SFPA_LADDERRADIUS(11)=-1.0                                          239
      SFPA_LADDERRADIUS(12)=-1.0                                          239
      SFPA_LADDERRADIUS(13)=-1.0                                          239
      SFPA_LADDERRADIUS(14)=-1.0                                          239
      SFPA_LADDERRADIUS(15)=-1.0                                          239
      SFPA_LADDERRADIUS(16)=-1.0                                          239
      SFPA_LADDERRADIUS(17)=-1.0                                          239
      SFPA_LADDERRADIUS(18)=-1.0                                          239
      SFPA_LADDERRADIUS(19)=-1.0                                          239
      SFPA_LADDERRADIUS(20)=-1.0                                          239
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               239
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    240
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           240
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   240
C *                                                                       240
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    240
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           240
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   240
C *                                                                       241
      CALL AGSBEGM(MODULE,IPRIN)                                          241
      IF (IPRIN.LT.0) RETURN                                              241
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   241
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      241
      BPATH_SFPA='SFPA*'                                                  241
      IDNUM_SFPA(1)=0                                                     242
C GEOMETRY VERSION                                                        242
      SFPA_VERSION=2                                                      242
      CSFPA_VERSION='GEOMETRY VERSION'                                    242
C PRESENCE OF LADDERS                                                     244
      SFPA_LADDERMAP(1)=1                                                 244
      SFPA_LADDERMAP(2)=1                                                 244
      SFPA_LADDERMAP(3)=1                                                 244
      SFPA_LADDERMAP(4)=0                                                 244
      SFPA_LADDERMAP(5)=0                                                 244
      SFPA_LADDERMAP(6)=0                                                 244
      SFPA_LADDERMAP(7)=0                                                 244
      SFPA_LADDERMAP(8)=0                                                 244
      SFPA_LADDERMAP(9)=1                                                 244
      SFPA_LADDERMAP(10)=1                                                244
      SFPA_LADDERMAP(11)=1                                                244
      SFPA_LADDERMAP(12)=1                                                244
      SFPA_LADDERMAP(13)=1                                                244
      SFPA_LADDERMAP(14)=0                                                244
      SFPA_LADDERMAP(15)=0                                                244
      SFPA_LADDERMAP(16)=0                                                244
      SFPA_LADDERMAP(17)=0                                                244
      SFPA_LADDERMAP(18)=0                                                244
      SFPA_LADDERMAP(19)=1                                                244
      SFPA_LADDERMAP(20)=1                                                244
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               244
C INDIVIDUAL ANGLES                                                       248
      SFPA_LADDERANGLE(1)=90.0                                            248
      SFPA_LADDERANGLE(2)=108.3                                           248
      SFPA_LADDERANGLE(3)=130.0                                           248
      SFPA_LADDERANGLE(4)=-1.0                                            248
      SFPA_LADDERANGLE(5)=-1.0                                            248
      SFPA_LADDERANGLE(6)=-1.0                                            248
      SFPA_LADDERANGLE(7)=-1.0                                            248
      SFPA_LADDERANGLE(8)=-1.0                                            248
      SFPA_LADDERANGLE(9)=230.0                                           248
      SFPA_LADDERANGLE(10)=251.7                                          248
      SFPA_LADDERANGLE(11)=270.0                                          248
      SFPA_LADDERANGLE(12)=288.3                                          248
      SFPA_LADDERANGLE(13)=310.0                                          248
      SFPA_LADDERANGLE(14)=-1.0                                           248
      SFPA_LADDERANGLE(15)=-1.0                                           248
      SFPA_LADDERANGLE(16)=-1.0                                           248
      SFPA_LADDERANGLE(17)=-1.0                                           248
      SFPA_LADDERANGLE(18)=-1.0                                           248
      SFPA_LADDERANGLE(19)=50.0                                           248
      SFPA_LADDERANGLE(20)=71.7                                           248
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               248
C INDIVIDUAL TILTS                                                        252
      SFPA_LADDERTILT(1)=0.0                                              252
      SFPA_LADDERTILT(2)=-6.0                                             252
      SFPA_LADDERTILT(3)=0.0                                              252
      SFPA_LADDERTILT(4)=-1.0                                             252
      SFPA_LADDERTILT(5)=-1.0                                             252
      SFPA_LADDERTILT(6)=-1.0                                             252
      SFPA_LADDERTILT(7)=-1.0                                             252
      SFPA_LADDERTILT(8)=0.0                                              252
      SFPA_LADDERTILT(9)=0.0                                              252
      SFPA_LADDERTILT(10)=6.0                                             252
      SFPA_LADDERTILT(11)=0.0                                             252
      SFPA_LADDERTILT(12)=-6.0                                            252
      SFPA_LADDERTILT(13)=0.0                                             252
      SFPA_LADDERTILT(14)=-1.0                                            252
      SFPA_LADDERTILT(15)=-1.0                                            252
      SFPA_LADDERTILT(16)=-1.0                                            252
      SFPA_LADDERTILT(17)=-1.0                                            252
      SFPA_LADDERTILT(18)=-1.0                                            252
      SFPA_LADDERTILT(19)=0.0                                             252
      SFPA_LADDERTILT(20)=6.0                                             252
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 252
C INDIVIDUAL RADII                                                        257
      SFPA_LADDERRADIUS(1)=23.174                                         257
      SFPA_LADDERRADIUS(2)=22.800                                         257
      SFPA_LADDERRADIUS(3)=24.600                                         257
      SFPA_LADDERRADIUS(4)=-1.0                                           257
      SFPA_LADDERRADIUS(5)=-1.0                                           257
      SFPA_LADDERRADIUS(6)=-1.0                                           257
      SFPA_LADDERRADIUS(7)=-1.0                                           257
      SFPA_LADDERRADIUS(8)=-1.0                                           257
      SFPA_LADDERRADIUS(9)=24.600                                         257
      SFPA_LADDERRADIUS(10)=22.800                                        257
      SFPA_LADDERRADIUS(11)=23.174                                        257
      SFPA_LADDERRADIUS(12)=22.800                                        257
      SFPA_LADDERRADIUS(13)=24.600                                        257
      SFPA_LADDERRADIUS(14)=-1.0                                          257
      SFPA_LADDERRADIUS(15)=-1.0                                          257
      SFPA_LADDERRADIUS(16)=-1.0                                          257
      SFPA_LADDERRADIUS(17)=-1.0                                          257
      SFPA_LADDERRADIUS(18)=-1.0                                          257
      SFPA_LADDERRADIUS(19)=24.600                                        257
      SFPA_LADDERRADIUS(20)=22.800                                        257
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               257
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    258
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           258
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   258
C *                                                                       258
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    258
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           258
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   258
C *                                                                       259
      CALL AGSBEGM(MODULE,IPRIN)                                          259
      IF (IPRIN.LT.0) RETURN                                              259
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   259
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      259
      BPATH_SFPA='SFPA*'                                                  259
      IDNUM_SFPA(1)=0                                                     260
C GEOMETRY VERSION                                                        260
      SFPA_VERSION=3                                                      260
      CSFPA_VERSION='GEOMETRY VERSION'                                    260
C PRESENCE OF LADDERS                                                     262
      SFPA_LADDERMAP(1)=1                                                 262
      SFPA_LADDERMAP(2)=1                                                 262
      SFPA_LADDERMAP(3)=1                                                 262
      SFPA_LADDERMAP(4)=1                                                 262
      SFPA_LADDERMAP(5)=1                                                 262
      SFPA_LADDERMAP(6)=1                                                 262
      SFPA_LADDERMAP(7)=1                                                 262
      SFPA_LADDERMAP(8)=1                                                 262
      SFPA_LADDERMAP(9)=1                                                 262
      SFPA_LADDERMAP(10)=1                                                262
      SFPA_LADDERMAP(11)=1                                                262
      SFPA_LADDERMAP(12)=1                                                262
      SFPA_LADDERMAP(13)=1                                                262
      SFPA_LADDERMAP(14)=1                                                262
      SFPA_LADDERMAP(15)=1                                                262
      SFPA_LADDERMAP(16)=1                                                262
      SFPA_LADDERMAP(17)=1                                                262
      SFPA_LADDERMAP(18)=1                                                262
      SFPA_LADDERMAP(19)=1                                                262
      SFPA_LADDERMAP(20)=1                                                262
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               262
C INDIVIDUAL ANGLES                                                       266
      SFPA_LADDERANGLE(1)=90.0                                            266
      SFPA_LADDERANGLE(2)=108.3                                           266
      SFPA_LADDERANGLE(3)=126.6                                           266
      SFPA_LADDERANGLE(4)=144.4                                           266
      SFPA_LADDERANGLE(5)=162.2                                           266
      SFPA_LADDERANGLE(6)=180.0                                           266
      SFPA_LADDERANGLE(7)=197.8                                           266
      SFPA_LADDERANGLE(8)=215.6                                           266
      SFPA_LADDERANGLE(9)=233.4                                           266
      SFPA_LADDERANGLE(10)=251.7                                          266
      SFPA_LADDERANGLE(11)=270.0                                          266
      SFPA_LADDERANGLE(12)=288.3                                          266
      SFPA_LADDERANGLE(13)=306.6                                          266
      SFPA_LADDERANGLE(14)=324.4                                          266
      SFPA_LADDERANGLE(15)=342.2                                          266
      SFPA_LADDERANGLE(16)=0.0                                            266
      SFPA_LADDERANGLE(17)=17.8                                           266
      SFPA_LADDERANGLE(18)=35.6                                           266
      SFPA_LADDERANGLE(19)=53.4                                           266
      SFPA_LADDERANGLE(20)=71.7                                           266
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               266
C INDIVIDUAL TILTS                                                        270
      SFPA_LADDERTILT(1)=0.0                                              270
      SFPA_LADDERTILT(2)=-6.0                                             270
      SFPA_LADDERTILT(3)=-7.0                                             270
      SFPA_LADDERTILT(4)=-7.0                                             270
      SFPA_LADDERTILT(5)=-7.0                                             270
      SFPA_LADDERTILT(6)=0.0                                              270
      SFPA_LADDERTILT(7)=7.0                                              270
      SFPA_LADDERTILT(8)=7.0                                              270
      SFPA_LADDERTILT(9)=7.0                                              270
      SFPA_LADDERTILT(10)=6.0                                             270
      SFPA_LADDERTILT(11)=0.0                                             270
      SFPA_LADDERTILT(12)=-6.0                                            270
      SFPA_LADDERTILT(13)=-7.0                                            270
      SFPA_LADDERTILT(14)=-7.0                                            270
      SFPA_LADDERTILT(15)=-7.0                                            270
      SFPA_LADDERTILT(16)=0.0                                             270
      SFPA_LADDERTILT(17)=7.0                                             270
      SFPA_LADDERTILT(18)=7.0                                             270
      SFPA_LADDERTILT(19)=7.0                                             270
      SFPA_LADDERTILT(20)=6.0                                             270
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 270
C INDIVIDUAL RADII                                                        275
      SFPA_LADDERRADIUS(1)=23.177                                         275
      SFPA_LADDERRADIUS(2)=22.800                                         275
      SFPA_LADDERRADIUS(3)=22.800                                         275
      SFPA_LADDERRADIUS(4)=22.800                                         275
      SFPA_LADDERRADIUS(5)=23.800                                         275
      SFPA_LADDERRADIUS(6)=22.500                                         275
      SFPA_LADDERRADIUS(7)=23.800                                         275
      SFPA_LADDERRADIUS(8)=22.800                                         275
      SFPA_LADDERRADIUS(9)=22.800                                         275
      SFPA_LADDERRADIUS(10)=22.800                                        275
      SFPA_LADDERRADIUS(11)=23.177                                        275
      SFPA_LADDERRADIUS(12)=22.800                                        275
      SFPA_LADDERRADIUS(13)=22.800                                        275
      SFPA_LADDERRADIUS(14)=22.800                                        275
      SFPA_LADDERRADIUS(15)=23.800                                        275
      SFPA_LADDERRADIUS(16)=22.500                                        275
      SFPA_LADDERRADIUS(17)=23.800                                        275
      SFPA_LADDERRADIUS(18)=22.800                                        275
      SFPA_LADDERRADIUS(19)=22.800                                        275
      SFPA_LADDERRADIUS(20)=22.800                                        275
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               275
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    276
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           276
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   276
C *                                                                       276
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    276
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           276
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   276
C *                                                                       277
      CALL AGSBEGM(MODULE,IPRIN)                                          277
      IF (IPRIN.LT.0) RETURN                                              277
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   277
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      277
      BPATH_SFPA='SFPA*'                                                  277
      IDNUM_SFPA(1)=0                                                     278
C GEOMETRY VERSION                                                        278
      SFPA_VERSION=4                                                      278
      CSFPA_VERSION='GEOMETRY VERSION'                                    278
C PRESENCE OF LADDERS                                                     280
      SFPA_LADDERMAP(1)=1                                                 280
      SFPA_LADDERMAP(2)=1                                                 280
      SFPA_LADDERMAP(3)=1                                                 280
      SFPA_LADDERMAP(4)=1                                                 280
      SFPA_LADDERMAP(5)=1                                                 280
      SFPA_LADDERMAP(6)=1                                                 280
      SFPA_LADDERMAP(7)=1                                                 280
      SFPA_LADDERMAP(8)=1                                                 280
      SFPA_LADDERMAP(9)=1                                                 280
      SFPA_LADDERMAP(10)=1                                                280
      SFPA_LADDERMAP(11)=1                                                280
      SFPA_LADDERMAP(12)=1                                                280
      SFPA_LADDERMAP(13)=1                                                280
      SFPA_LADDERMAP(14)=1                                                280
      SFPA_LADDERMAP(15)=1                                                280
      SFPA_LADDERMAP(16)=1                                                280
      SFPA_LADDERMAP(17)=1                                                280
      SFPA_LADDERMAP(18)=1                                                280
      SFPA_LADDERMAP(19)=1                                                280
      SFPA_LADDERMAP(20)=1                                                280
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               280
C INDIVIDUAL ANGLES                                                       284
      SFPA_LADDERANGLE(1)=90.0                                            284
      SFPA_LADDERANGLE(2)=108.3                                           284
      SFPA_LADDERANGLE(3)=126.6                                           284
      SFPA_LADDERANGLE(4)=144.4                                           284
      SFPA_LADDERANGLE(5)=162.2                                           284
      SFPA_LADDERANGLE(6)=180.0                                           284
      SFPA_LADDERANGLE(7)=197.8                                           284
      SFPA_LADDERANGLE(8)=215.6                                           284
      SFPA_LADDERANGLE(9)=233.4                                           284
      SFPA_LADDERANGLE(10)=251.7                                          284
      SFPA_LADDERANGLE(11)=270.0                                          284
      SFPA_LADDERANGLE(12)=288.3                                          284
      SFPA_LADDERANGLE(13)=306.6                                          284
      SFPA_LADDERANGLE(14)=324.4                                          284
      SFPA_LADDERANGLE(15)=342.2                                          284
      SFPA_LADDERANGLE(16)=0.0                                            284
      SFPA_LADDERANGLE(17)=17.8                                           284
      SFPA_LADDERANGLE(18)=35.6                                           284
      SFPA_LADDERANGLE(19)=53.4                                           284
      SFPA_LADDERANGLE(20)=71.7                                           284
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               284
C INDIVIDUAL TILTS                                                        288
      SFPA_LADDERTILT(1)=0.0                                              288
      SFPA_LADDERTILT(2)=-6.0                                             288
      SFPA_LADDERTILT(3)=-7.0                                             288
      SFPA_LADDERTILT(4)=-7.0                                             288
      SFPA_LADDERTILT(5)=-7.0                                             288
      SFPA_LADDERTILT(6)=0.0                                              288
      SFPA_LADDERTILT(7)=7.0                                              288
      SFPA_LADDERTILT(8)=7.0                                              288
      SFPA_LADDERTILT(9)=7.0                                              288
      SFPA_LADDERTILT(10)=6.0                                             288
      SFPA_LADDERTILT(11)=0.0                                             288
      SFPA_LADDERTILT(12)=-6.0                                            288
      SFPA_LADDERTILT(13)=-7.0                                            288
      SFPA_LADDERTILT(14)=-7.0                                            288
      SFPA_LADDERTILT(15)=-7.0                                            288
      SFPA_LADDERTILT(16)=0.0                                             288
      SFPA_LADDERTILT(17)=7.0                                             288
      SFPA_LADDERTILT(18)=7.0                                             288
      SFPA_LADDERTILT(19)=7.0                                             288
      SFPA_LADDERTILT(20)=6.0                                             288
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 288
C INDIVIDUAL RADII                                                        293
      SFPA_LADDERRADIUS(1)=23.177                                         293
      SFPA_LADDERRADIUS(2)=22.800                                         293
      SFPA_LADDERRADIUS(3)=22.800                                         293
      SFPA_LADDERRADIUS(4)=22.800                                         293
      SFPA_LADDERRADIUS(5)=22.800                                         293
      SFPA_LADDERRADIUS(6)=22.500                                         293
      SFPA_LADDERRADIUS(7)=22.800                                         293
      SFPA_LADDERRADIUS(8)=22.800                                         293
      SFPA_LADDERRADIUS(9)=22.800                                         293
      SFPA_LADDERRADIUS(10)=22.800                                        293
      SFPA_LADDERRADIUS(11)=23.177                                        293
      SFPA_LADDERRADIUS(12)=22.800                                        293
      SFPA_LADDERRADIUS(13)=22.800                                        293
      SFPA_LADDERRADIUS(14)=22.800                                        293
      SFPA_LADDERRADIUS(15)=22.800                                        293
      SFPA_LADDERRADIUS(16)=22.500                                        293
      SFPA_LADDERRADIUS(17)=22.800                                        293
      SFPA_LADDERRADIUS(18)=22.800                                        293
      SFPA_LADDERRADIUS(19)=22.800                                        293
      SFPA_LADDERRADIUS(20)=22.800                                        293
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               293
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    294
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           294
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   294
C *                                                                       294
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    294
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           294
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   294
C *                                                                       295
      CALL AGSBEGM(MODULE,IPRIN)                                          295
      IF (IPRIN.LT.0) RETURN                                              295
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   295
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      295
      BPATH_SFPA='SFPA*'                                                  295
      IDNUM_SFPA(1)=0                                                     296
C GEOMETRY VERSION                                                        296
      SFPA_VERSION=5                                                      296
      CSFPA_VERSION='GEOMETRY VERSION'                                    296
C PRESENCE OF LADDERS                                                     298
      SFPA_LADDERMAP(1)=1                                                 298
      SFPA_LADDERMAP(2)=1                                                 298
      SFPA_LADDERMAP(3)=1                                                 298
      SFPA_LADDERMAP(4)=1                                                 298
      SFPA_LADDERMAP(5)=1                                                 298
      SFPA_LADDERMAP(6)=1                                                 298
      SFPA_LADDERMAP(7)=1                                                 298
      SFPA_LADDERMAP(8)=1                                                 298
      SFPA_LADDERMAP(9)=1                                                 298
      SFPA_LADDERMAP(10)=1                                                298
      SFPA_LADDERMAP(11)=1                                                298
      SFPA_LADDERMAP(12)=1                                                298
      SFPA_LADDERMAP(13)=1                                                298
      SFPA_LADDERMAP(14)=1                                                298
      SFPA_LADDERMAP(15)=1                                                298
      SFPA_LADDERMAP(16)=1                                                298
      SFPA_LADDERMAP(17)=1                                                298
      SFPA_LADDERMAP(18)=1                                                298
      SFPA_LADDERMAP(19)=1                                                298
      SFPA_LADDERMAP(20)=1                                                298
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               298
C INDIVIDUAL ANGLES                                                       302
      SFPA_LADDERANGLE(1)=90.0                                            302
      SFPA_LADDERANGLE(2)=108.3                                           302
      SFPA_LADDERANGLE(3)=126.6                                           302
      SFPA_LADDERANGLE(4)=144.4                                           302
      SFPA_LADDERANGLE(5)=162.2                                           302
      SFPA_LADDERANGLE(6)=180.0                                           302
      SFPA_LADDERANGLE(7)=197.8                                           302
      SFPA_LADDERANGLE(8)=215.6                                           302
      SFPA_LADDERANGLE(9)=233.4                                           302
      SFPA_LADDERANGLE(10)=251.7                                          302
      SFPA_LADDERANGLE(11)=270.0                                          302
      SFPA_LADDERANGLE(12)=288.3                                          302
      SFPA_LADDERANGLE(13)=306.6                                          302
      SFPA_LADDERANGLE(14)=324.4                                          302
      SFPA_LADDERANGLE(15)=342.2                                          302
      SFPA_LADDERANGLE(16)=0.0                                            302
      SFPA_LADDERANGLE(17)=17.8                                           302
      SFPA_LADDERANGLE(18)=35.6                                           302
      SFPA_LADDERANGLE(19)=53.4                                           302
      SFPA_LADDERANGLE(20)=71.7                                           302
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               302
C INDIVIDUAL TILTS                                                        306
      SFPA_LADDERTILT(1)=0.0                                              306
      SFPA_LADDERTILT(2)=-6.0                                             306
      SFPA_LADDERTILT(3)=-7.0                                             306
      SFPA_LADDERTILT(4)=-7.0                                             306
      SFPA_LADDERTILT(5)=-7.0                                             306
      SFPA_LADDERTILT(6)=0.0                                              306
      SFPA_LADDERTILT(7)=7.0                                              306
      SFPA_LADDERTILT(8)=7.0                                              306
      SFPA_LADDERTILT(9)=7.0                                              306
      SFPA_LADDERTILT(10)=6.0                                             306
      SFPA_LADDERTILT(11)=0.0                                             306
      SFPA_LADDERTILT(12)=-6.0                                            306
      SFPA_LADDERTILT(13)=-7.0                                            306
      SFPA_LADDERTILT(14)=-7.0                                            306
      SFPA_LADDERTILT(15)=-7.0                                            306
      SFPA_LADDERTILT(16)=0.0                                             306
      SFPA_LADDERTILT(17)=7.0                                             306
      SFPA_LADDERTILT(18)=7.0                                             306
      SFPA_LADDERTILT(19)=7.0                                             306
      SFPA_LADDERTILT(20)=6.0                                             306
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 306
C INDIVIDUAL RADII                                                        311
      SFPA_LADDERRADIUS(1)=23.177                                         311
      SFPA_LADDERRADIUS(2)=22.800                                         311
      SFPA_LADDERRADIUS(3)=22.600                                         311
      SFPA_LADDERRADIUS(4)=22.600                                         311
      SFPA_LADDERRADIUS(5)=22.600                                         311
      SFPA_LADDERRADIUS(6)=22.300                                         311
      SFPA_LADDERRADIUS(7)=22.600                                         311
      SFPA_LADDERRADIUS(8)=22.600                                         311
      SFPA_LADDERRADIUS(9)=22.600                                         311
      SFPA_LADDERRADIUS(10)=22.800                                        311
      SFPA_LADDERRADIUS(11)=23.177                                        311
      SFPA_LADDERRADIUS(12)=22.800                                        311
      SFPA_LADDERRADIUS(13)=22.600                                        311
      SFPA_LADDERRADIUS(14)=22.600                                        311
      SFPA_LADDERRADIUS(15)=22.600                                        311
      SFPA_LADDERRADIUS(16)=22.500                                        311
      SFPA_LADDERRADIUS(17)=22.600                                        311
      SFPA_LADDERRADIUS(18)=22.600                                        311
      SFPA_LADDERRADIUS(19)=22.600                                        311
      SFPA_LADDERRADIUS(20)=22.800                                        311
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               311
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    312
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           312
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   312
C *                                                                       312
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    312
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           312
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   312
C use default SSDP bank                                                   313
      BPATH_SSDP='SSDP*'                                                  313
      IDNUM_SSDP(1)=0                                                     313
      CALL AGDGETP('SISDGEO5','*',-999,'SSDP',' ',BANK_SSDP, BPATH_SSDP,  313
     *IDNUM_SSDP,1,MAP_SSDP,NAMES_SSDP,BANK_SSDP,LEN_SSDP,LLEN_SSDP,      313
     *LINK_SSDP,FLAG_SSDP)                                                313
C use SFPA bank                                                           314
      BPATH_SFPA='SFPA*'                                                  314
      IDNUM_SFPA(1)=0                                                     314
      SFPA_VERSION=SSDP_CONFIG                                            314
      CALL AGDGETP('SISDGEO5','*',-999,'SFPA',NSFPA_VERSION,              314
     *RE_SFPA_VERSION,BPATH_SFPA,IDNUM_SFPA,1,MAP_SFPA,NAMES_SFPA,        314
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   314
C use SFJP bank                                                           315
      BPATH_SFJP='SFJP*'                                                  315
      IDNUM_SFJP(1)=0                                                     315
      SFJP_VERSION=2                                                      315
      CALL AGDGETP('SISDGEO5','*',-999,'SFJP',NSFJP_VERSION,              315
     *RE_SFJP_VERSION,BPATH_SFJP,IDNUM_SFJP,1,MAP_SFJP,NAMES_SFJP,        315
     *BANK_SFJP,LEN_SFJP,LLEN_SFJP,LINK_SFJP,FLAG_SFJP)                   315
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             316
      CALL AGSRESET                                                       316
      AG_A = 28.08                                                        316
      AG_Z = 14                                                           316
      AG_W = 0.6*1*28./60.                                                316
      AG_TITLE = 'SI'                                                     316
      AG_EXNAME = 'COMPONENT'                                             316
      AG_PARLIST = 'A_Z_W'                                                316
      CALL AXCOMPONENT                                                    316
C *                                                                       316
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  316
C =                                                                       316
      CALL AGSRESET                                                       317
      AG_A = 16                                                           317
      AG_Z = 8                                                            317
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               317
      AG_TITLE = 'O'                                                      317
      AG_EXNAME = 'COMPONENT'                                             317
      AG_PARLIST = 'A_Z_W'                                                317
      CALL AXCOMPONENT                                                    317
C *                                                                       317
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 317
      CALL AGSRESET                                                       318
      AG_A = 12                                                           318
      AG_Z = 6                                                            318
      AG_W = 0.4*8*12./174.                                               318
      AG_TITLE = 'C'                                                      318
      AG_EXNAME = 'COMPONENT'                                             318
      AG_PARLIST = 'A_Z_W'                                                318
      CALL AXCOMPONENT                                                    318
C *                                                                       318
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  318
      CALL AGSRESET                                                       319
      AG_A = 1                                                            319
      AG_Z = 1                                                            319
      AG_W = 0.4*14*1./174.                                               319
      AG_TITLE = 'H'                                                      319
      AG_EXNAME = 'COMPONENT'                                             319
      AG_PARLIST = 'A_Z_W'                                                319
      CALL AXCOMPONENT                                                    319
C *                                                                       319
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  319
      CALL AGSRESET                                                       320
      AG_DENS = 1.7                                                       320
      AG_TITLE = 'G10'                                                    320
      AG_EXNAME = 'MIXTURE'                                               320
      AG_PARLIST = 'DENS'                                                 320
      CALL AXMIXTURE                                                      320
C *                                                                       320
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             320
      CALL AGSRESET                                                       321
      AG_A = 28.08                                                        321
      AG_Z = 14                                                           321
      AG_W = 0.6*1*28./60.                                                321
      AG_TITLE = 'SI'                                                     321
      AG_EXNAME = 'COMPONENT'                                             321
      AG_PARLIST = 'A_Z_W'                                                321
      CALL AXCOMPONENT                                                    321
C *                                                                       321
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  321
C =                                                                       321
      CALL AGSRESET                                                       322
      AG_A = 16                                                           322
      AG_Z = 8                                                            322
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               322
      AG_TITLE = 'O'                                                      322
      AG_EXNAME = 'COMPONENT'                                             322
      AG_PARLIST = 'A_Z_W'                                                322
      CALL AXCOMPONENT                                                    322
C *                                                                       322
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 322
      CALL AGSRESET                                                       323
      AG_A = 12                                                           323
      AG_Z = 6                                                            323
      AG_W = 0.4*8*12./174.                                               323
      AG_TITLE = 'C'                                                      323
      AG_EXNAME = 'COMPONENT'                                             323
      AG_PARLIST = 'A_Z_W'                                                323
      CALL AXCOMPONENT                                                    323
C *                                                                       323
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  323
      CALL AGSRESET                                                       324
      AG_A = 1                                                            324
      AG_Z = 1                                                            324
      AG_W = 0.4*14*1./174.                                               324
      AG_TITLE = 'H'                                                      324
      AG_EXNAME = 'COMPONENT'                                             324
      AG_PARLIST = 'A_Z_W'                                                324
      CALL AXCOMPONENT                                                    324
C *                                                                       324
C Geant MIXTURE : G5 ===> DENS=0.85 <===                                  324
      CALL AGSRESET                                                       325
      AG_DENS = 0.85                                                      325
      AG_TITLE = 'G5'                                                     325
      AG_EXNAME = 'MIXTURE'                                               325
      AG_PARLIST = 'DENS'                                                 325
      CALL AXMIXTURE                                                      325
C *                                                                       325
C Geant COMPONENT : O ===> A=16 Z=8 W=2*16./100. <===                     325
      CALL AGSRESET                                                       326
      AG_A = 16                                                           326
      AG_Z = 8                                                            326
      AG_W = 2*16./100.                                                   326
      AG_TITLE = 'O'                                                      326
      AG_EXNAME = 'COMPONENT'                                             326
      AG_PARLIST = 'A_Z_W'                                                326
      CALL AXCOMPONENT                                                    326
C *                                                                       326
C Geant COMPONENT : C ===> A=12 Z=6 W=5*12./100. <===                     326
      CALL AGSRESET                                                       327
      AG_A = 12                                                           327
      AG_Z = 6                                                            327
      AG_W = 5*12./100.                                                   327
      AG_TITLE = 'C'                                                      327
      AG_EXNAME = 'COMPONENT'                                             327
      AG_PARLIST = 'A_Z_W'                                                327
      CALL AXCOMPONENT                                                    327
C *                                                                       327
C Geant COMPONENT : H ===> A=1 Z=1 W=8*1.0/100. <===                      327
      CALL AGSRESET                                                       328
      AG_A = 1                                                            328
      AG_Z = 1                                                            328
      AG_W = 8*1.0/100.                                                   328
      AG_TITLE = 'H'                                                      328
      AG_EXNAME = 'COMPONENT'                                             328
      AG_PARLIST = 'A_Z_W'                                                328
      CALL AXCOMPONENT                                                    328
C *                                                                       328
C Geant MIXTURE : DELRIN ===> DENS=1.2 <===                               328
      CALL AGSRESET                                                       329
      AG_DENS = 1.2                                                       329
      AG_TITLE = 'DELRIN'                                                 329
      AG_EXNAME = 'MIXTURE'                                               329
      AG_PARLIST = 'DENS'                                                 329
      CALL AXMIXTURE                                                      329
C *                                                                       329
C Check SSDP_PLACEMENT==1                                                 329
      IF (SSDP_PLACEMENT.EQ.1) THEN                                       330
      CALL AGSBEGM(MODULE,IPRIN)                                          331
C Check IPRIN<0                                                           331
      IF (IPRIN.LT.0) RETURN                                              331
C Create SFMO :                                                           331
C Call To Local Procedure SFMO                                            331
      ASSIGN 5020 TO BLOCK_SFMO                                           331
      GO TO  5010                                                         331
5020  CONTINUE                                                            331
C *                                                                       331
C Geant POSITION : SFMO ===> MOTHER='SVTT' <===                           332
      CALL AGSRESET                                                       332
      AG_MOTHER = 'SVTT'                                                  332
      AG_TITLE = 'SFMO'                                                   332
      AG_EXNAME = 'POSITION'                                              332
      AG_PARLIST = 'MOTHER'                                               332
      CALL AXPOSITION                                                     332
C *                                                                       332
      ELSE                                                                332
      CALL AGSBEGM(MODULE,IPRIN)                                          334
C Check IPRIN<0                                                           334
      IF (IPRIN.LT.0) RETURN                                              334
C Create SFMO :                                                           334
C Call To Local Procedure SFMO                                            334
      ASSIGN 5030 TO BLOCK_SFMO                                           334
      GO TO  5010                                                         334
5030  CONTINUE                                                            334
C *                                                                       334
C Geant POSITION : SFMO ===> MOTHER='CAVE' KONLY='ONLY' <===              335
      CALL AGSRESET                                                       335
      AG_MOTHER = 'CAVE'                                                  335
      AG_KONLY = 'ONLY'                                                   335
      AG_TITLE = 'SFMO'                                                   335
      AG_EXNAME = 'POSITION'                                              335
      AG_PARLIST = 'MOTHER_KONLY'                                         335
      CALL AXPOSITION                                                     335
C *                                                                       335
      ENDIF                                                               335
C ----------------------------------------------------------------------  337
C ------------ SFMO                                                       337
      CALL AGSENDM                                                        338
      RETURN                                                              338
C Begin Local Procedure SFMO                                              338
C SFMO IS THE MOTHER OF ALL SILICON STRIP DETECTOR VOLUMES                338
5010  CONTINUE                                                            338
         AG_TITLE='SFMO'                                                  338
         AG_MODULE=MODULE                                                 338
         AG_MARK='V00'                                                    338
         CALL AGSPUSH                                                     338
         AG_MODULE=MODULE                                                 338
C    *                                                                    338
C    Geant MATERIAL : AIR ===> inherited arguments used <===              339
         CALL AGSRESET                                                    339
         AG_TITLE = 'AIR'                                                 339
         AG_EXNAME = 'MATERIAL'                                           339
         AG_PARLIST = 'NONE'                                              339
         CALL AXMATERIAL                                                  339
C    *                                                                    339
C    Geant ATTRIBUTE : SFMO ===> SEEN=1 COLO=6 <===                       339
         CALL AGSRESET                                                    340
         AG_SEEN = 1                                                      340
         AG_COLO = 6                                                      340
         AG_TITLE = 'SFMO'                                                340
         AG_EXNAME = 'ATTRIBUTE'                                          340
         AG_PARLIST = 'SEEN_COLO'                                         340
         CALL AXATTRIBUTE                                                 340
C    *                                                                    340
C    Geant SHAPE : TUBE ===> RMIN=SFPA_RMIN RMAX=SFPA_RMAX, DZ=SFPA_LEN/  340
C    2 <===                                                               340
         CALL AGSRESET                                                    342
         AG_RMIN = SFPA_RMIN                                              342
         AG_RMAX = SFPA_RMAX                                              342
         AG_DZ = SFPA_LEN/2                                               342
         AG_TITLE = 'TUBE'                                                342
         AG_EXNAME = 'SHAPE'                                              342
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      342
         CALL AXSHAPE                                                     342
C    *                                                                    342
         DTHK = SFPA_SMTHK + SFPA_GPTHK                                   342
C    Create SFLM :                                                        344
C    Call To Local Procedure SFLM                                         344
         ASSIGN 5050 TO BLOCK_SFLM                                        344
         GO TO  5040                                                      344
5050     CONTINUE                                                         344
C    *                                                                    344
C    *                                                                    345
         ILAD=1                                                           345
            GO TO 5063                                                    345
5061        ILAD=ILAD+1                                                   345
5063        IF(ILAD-(20).GT.0) GO TO 5062                                 345
C       Check SFPA_LADDERMAP(ILAD).GT.0                                   346
            IF (SFPA_LADDERMAP(ILAD).GT.0) THEN                           346
            ANG = (SFPA_LADDERANGLE(ILAD)*PI)/180.0                       347
            RADTILT = (SFPA_LADDERTILT(ILAD) *PI)/180.0                   348
C       Check ILAD.EQ.1                                                   349
            IF (ILAD.EQ.1) THEN                                           349
            NC=1                                                          350
            ELSE                                                          351
            NC=20-ILAD+2                                                  352
            ENDIF                                                         353
C       Geant POSITION : SFLM ===> X=(SFPA_LADDERRADIUS(ILAD)*COS(ANG)+(  356
C       DTHK*COS(ANG+RADTILT))/2.0) Y=(SFPA_LADDERRADIUS(ILAD)*SIN(ANG)+  356
C       (DTHK*SIN(ANG+RADTILT))/2.0) Z=0 ALPHAZ=SFPA_LADDERANGLE(ILAD)-9  356
C       0.0+SFPA_LADDERTILT(ILAD) NCOPY=NC KONLY='MANY' <===              356
            CALL AGSRESET                                                 356
            AG_X =                                                        356
     *      (SFPA_LADDERRADIUS(ILAD)*COS(ANG)+(DTHK*COS(ANG+RADTILT))/    356
     *      2.0)                                                          356
            AG_Y =                                                        356
     *      (SFPA_LADDERRADIUS(ILAD)*SIN(ANG)+(DTHK*SIN(ANG+RADTILT))/    356
     *      2.0)                                                          356
            AG_Z = 0                                                      356
            AG_ALPHAN=AG_ALPHAN+1                                         356
            AG_ALPHAK(AG_ALPHAN)=3                                        356
            AG_ALPHAL(AG_ALPHAN)=                                         356
     *      SFPA_LADDERANGLE(ILAD)-90.0+SFPA_LADDERTILT(ILAD)             356
            AG_NCOPY = NC                                                 356
            AG_KONLY = 'MANY'                                             356
            AG_TITLE = 'SFLM'                                             356
            AG_EXNAME = 'POSITION'                                        356
            AG_PARLIST = 'X_Y_Z_ALPHAZ_NCOPY_KONLY'                       356
            CALL AXPOSITION                                               356
C       *                                                                 356
            ENDIF                                                         356
         GO TO 5061                                                       358
5062     CONTINUE                                                         358
C    Create SSST :                                                        359
C    Call To Local Procedure SSST                                         359
         ASSIGN 5080 TO BLOCK_SSST                                        359
         GO TO  5070                                                      359
5080     CONTINUE                                                         359
C    *                                                                    359
C    Geant POSITION : SSST ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  361
C    JP_SSST_WIDTH/2.-SFJP_SSST_PZ <===                                   361
         CALL AGSRESET                                                    361
         AG_X = 0.                                                        361
         AG_Y = 0.                                                        361
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSST_WIDTH/                361
     *   2.-SFJP_SSST_PZ                                                  361
         AG_TITLE = 'SSST'                                                361
         AG_EXNAME = 'POSITION'                                           361
         AG_PARLIST = 'X_Y_Z'                                             361
         CALL AXPOSITION                                                  361
C    *                                                                    361
C    Geant POSITION : SSST ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  361
C    JP_SSST_WIDTH/2.+SFJP_SSST_PZ <===                                   361
         CALL AGSRESET                                                    363
         AG_X = 0.                                                        363
         AG_Y = 0.                                                        363
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSST_WIDTH/                363
     *   2.+SFJP_SSST_PZ                                                  363
         AG_TITLE = 'SSST'                                                363
         AG_EXNAME = 'POSITION'                                           363
         AG_PARLIST = 'X_Y_Z'                                             363
         CALL AXPOSITION                                                  363
C    *                                                                    363
C    Geant POSITION : SSST ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  363
C    JP_SSST_WIDTH/2.-SFJP_SSST_PZ ALPHAZ=180. <===                       363
         CALL AGSRESET                                                    365
         AG_X = 0.                                                        365
         AG_Y = 0.                                                        365
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSST_WIDTH/                365
     *   2.-SFJP_SSST_PZ                                                  365
         AG_ALPHAN=AG_ALPHAN+1                                            365
         AG_ALPHAK(AG_ALPHAN)=3                                           365
         AG_ALPHAL(AG_ALPHAN)=180.                                        365
         AG_TITLE = 'SSST'                                                365
         AG_EXNAME = 'POSITION'                                           365
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      365
         CALL AXPOSITION                                                  365
C    *                                                                    365
C    Geant POSITION : SSST ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  365
C    JP_SSST_WIDTH/2.+SFJP_SSST_PZ ALPHAZ=180. <===                       365
         CALL AGSRESET                                                    367
         AG_X = 0.                                                        367
         AG_Y = 0.                                                        367
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSST_WIDTH/                367
     *   2.+SFJP_SSST_PZ                                                  367
         AG_ALPHAN=AG_ALPHAN+1                                            367
         AG_ALPHAK(AG_ALPHAN)=3                                           367
         AG_ALPHAL(AG_ALPHAN)=180.                                        367
         AG_TITLE = 'SSST'                                                367
         AG_EXNAME = 'POSITION'                                           367
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      367
         CALL AXPOSITION                                                  367
C    *                                                                    367
C    Create SSSS :                                                        368
C    Call To Local Procedure SSSS                                         368
         ASSIGN 5100 TO BLOCK_SSSS                                        368
         GO TO  5090                                                      368
5100     CONTINUE                                                         368
C    *                                                                    368
C    Geant POSITION : SSSS ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  370
C    JP_SSSS_WIDTH/2.-SFJP_SSST_PZ <===                                   370
         CALL AGSRESET                                                    370
         AG_X = 0.                                                        370
         AG_Y = 0.                                                        370
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH/                370
     *   2.-SFJP_SSST_PZ                                                  370
         AG_TITLE = 'SSSS'                                                370
         AG_EXNAME = 'POSITION'                                           370
         AG_PARLIST = 'X_Y_Z'                                             370
         CALL AXPOSITION                                                  370
C    *                                                                    370
C    Geant POSITION : SSSS ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  370
C    JP_SSSS_WIDTH/2.+SFJP_SSST_PZ <===                                   370
         CALL AGSRESET                                                    372
         AG_X = 0.                                                        372
         AG_Y = 0.                                                        372
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH/                372
     *   2.+SFJP_SSST_PZ                                                  372
         AG_TITLE = 'SSSS'                                                372
         AG_EXNAME = 'POSITION'                                           372
         AG_PARLIST = 'X_Y_Z'                                             372
         CALL AXPOSITION                                                  372
C    *                                                                    372
C    Geant POSITION : SSSS ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  372
C    JP_SSSS_WIDTH/2.-SFJP_SSST_PZ ALPHAZ=180. <===                       372
         CALL AGSRESET                                                    374
         AG_X = 0.                                                        374
         AG_Y = 0.                                                        374
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH/                374
     *   2.-SFJP_SSST_PZ                                                  374
         AG_ALPHAN=AG_ALPHAN+1                                            374
         AG_ALPHAK(AG_ALPHAN)=3                                           374
         AG_ALPHAL(AG_ALPHAN)=180.                                        374
         AG_TITLE = 'SSSS'                                                374
         AG_EXNAME = 'POSITION'                                           374
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      374
         CALL AXPOSITION                                                  374
C    *                                                                    374
C    Geant POSITION : SSSS ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  374
C    JP_SSSS_WIDTH/2.+SFJP_SSST_PZ ALPHAZ=180. <===                       374
         CALL AGSRESET                                                    376
         AG_X = 0.                                                        376
         AG_Y = 0.                                                        376
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH/                376
     *   2.+SFJP_SSST_PZ                                                  376
         AG_ALPHAN=AG_ALPHAN+1                                            376
         AG_ALPHAK(AG_ALPHAN)=3                                           376
         AG_ALPHAL(AG_ALPHAN)=180.                                        376
         AG_TITLE = 'SSSS'                                                376
         AG_EXNAME = 'POSITION'                                           376
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      376
         CALL AXPOSITION                                                  376
C    *                                                                    376
C    Create SSRT :                                                        377
C    Call To Local Procedure SSRT                                         377
         ASSIGN 5120 TO BLOCK_SSRT                                        377
         GO TO  5110                                                      377
5120     CONTINUE                                                         377
C    *                                                                    377
C    Geant POSITION : SSRT ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  379
C    JP_SSST_WIDTH/2.-SFJP_SSST_PZ <===                                   379
         CALL AGSRESET                                                    379
         AG_X = 0.                                                        379
         AG_Y = 0.                                                        379
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSST_WIDTH/                379
     *   2.-SFJP_SSST_PZ                                                  379
         AG_TITLE = 'SSRT'                                                379
         AG_EXNAME = 'POSITION'                                           379
         AG_PARLIST = 'X_Y_Z'                                             379
         CALL AXPOSITION                                                  379
C    *                                                                    379
C    Geant POSITION : SSRT ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  379
C    JP_SSST_WIDTH/2.+SFJP_SSST_PZ <===                                   379
         CALL AGSRESET                                                    381
         AG_X = 0.                                                        381
         AG_Y = 0.                                                        381
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSST_WIDTH/                381
     *   2.+SFJP_SSST_PZ                                                  381
         AG_TITLE = 'SSRT'                                                381
         AG_EXNAME = 'POSITION'                                           381
         AG_PARLIST = 'X_Y_Z'                                             381
         CALL AXPOSITION                                                  381
C    *                                                                    381
C    Geant POSITION : SSRT ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  381
C    JP_SSST_WIDTH/2.-SFJP_SSST_PZ ALPHAZ=180. <===                       381
         CALL AGSRESET                                                    383
         AG_X = 0.                                                        383
         AG_Y = 0.                                                        383
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSST_WIDTH/                383
     *   2.-SFJP_SSST_PZ                                                  383
         AG_ALPHAN=AG_ALPHAN+1                                            383
         AG_ALPHAK(AG_ALPHAN)=3                                           383
         AG_ALPHAL(AG_ALPHAN)=180.                                        383
         AG_TITLE = 'SSRT'                                                383
         AG_EXNAME = 'POSITION'                                           383
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      383
         CALL AXPOSITION                                                  383
C    *                                                                    383
C    Geant POSITION : SSRT ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  383
C    JP_SSST_WIDTH/2.+SFJP_SSST_PZ ALPHAZ=180. <===                       383
         CALL AGSRESET                                                    385
         AG_X = 0.                                                        385
         AG_Y = 0.                                                        385
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSST_WIDTH/                385
     *   2.+SFJP_SSST_PZ                                                  385
         AG_ALPHAN=AG_ALPHAN+1                                            385
         AG_ALPHAK(AG_ALPHAN)=3                                           385
         AG_ALPHAL(AG_ALPHAN)=180.                                        385
         AG_TITLE = 'SSRT'                                                385
         AG_EXNAME = 'POSITION'                                           385
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      385
         CALL AXPOSITION                                                  385
C    *                                                                    385
C    Create SSRS :                                                        386
C    Call To Local Procedure SSRS                                         386
         ASSIGN 5140 TO BLOCK_SSRS                                        386
         GO TO  5130                                                      386
5140     CONTINUE                                                         386
C    *                                                                    386
C    Geant POSITION : SSRS ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  388
C    JP_SSSS_WIDTH/2.-SFJP_SSST_PZ <===                                   388
         CALL AGSRESET                                                    388
         AG_X = 0.                                                        388
         AG_Y = 0.                                                        388
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH/                388
     *   2.-SFJP_SSST_PZ                                                  388
         AG_TITLE = 'SSRS'                                                388
         AG_EXNAME = 'POSITION'                                           388
         AG_PARLIST = 'X_Y_Z'                                             388
         CALL AXPOSITION                                                  388
C    *                                                                    388
C    Geant POSITION : SSRS ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  388
C    JP_SSSS_WIDTH/2.+SFJP_SSST_PZ <===                                   388
         CALL AGSRESET                                                    390
         AG_X = 0.                                                        390
         AG_Y = 0.                                                        390
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH/                390
     *   2.+SFJP_SSST_PZ                                                  390
         AG_TITLE = 'SSRS'                                                390
         AG_EXNAME = 'POSITION'                                           390
         AG_PARLIST = 'X_Y_Z'                                             390
         CALL AXPOSITION                                                  390
C    *                                                                    390
C    Geant POSITION : SSRS ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  390
C    JP_SSSS_WIDTH/2.-SFJP_SSST_PZ ALPHAZ=180. <===                       390
         CALL AGSRESET                                                    392
         AG_X = 0.                                                        392
         AG_Y = 0.                                                        392
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH/                392
     *   2.-SFJP_SSST_PZ                                                  392
         AG_ALPHAN=AG_ALPHAN+1                                            392
         AG_ALPHAK(AG_ALPHAN)=3                                           392
         AG_ALPHAL(AG_ALPHAN)=180.                                        392
         AG_TITLE = 'SSRS'                                                392
         AG_EXNAME = 'POSITION'                                           392
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      392
         CALL AXPOSITION                                                  392
C    *                                                                    392
C    Geant POSITION : SSRS ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  392
C    JP_SSSS_WIDTH/2.+SFJP_SSST_PZ ALPHAZ=180. <===                       392
         CALL AGSRESET                                                    394
         AG_X = 0.                                                        394
         AG_Y = 0.                                                        394
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH/                394
     *   2.+SFJP_SSST_PZ                                                  394
         AG_ALPHAN=AG_ALPHAN+1                                            394
         AG_ALPHAK(AG_ALPHAN)=3                                           394
         AG_ALPHAL(AG_ALPHAN)=180.                                        394
         AG_TITLE = 'SSRS'                                                394
         AG_EXNAME = 'POSITION'                                           394
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      394
         CALL AXPOSITION                                                  394
C    *                                                                    394
C    Create SSLB :                                                        395
C    Call To Local Procedure SSLB                                         395
         ASSIGN 5160 TO BLOCK_SSLB                                        395
         GO TO  5150                                                      395
5160     CONTINUE                                                         395
C    *                                                                    395
C    Check SFJP_VERSION==1                                                396
         IF (SFJP_VERSION.EQ.1) THEN                                      396
C    Geant POSITION : SSLB ===> X=SFJP_SSLB_PX Y=SFJP_SSLB_PX Z=-SFJP_SF  399
C    LU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ/2. ALPH  399
C    AZ=SFJP_ALPHAZH <===                                                 399
         CALL AGSRESET                                                    399
         AG_X = SFJP_SSLB_PX                                              399
         AG_Y = SFJP_SSLB_PX                                              399
         AG_Z =                                                           399
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          399
     *   SFJP_SSLB_DZ/2.                                                  399
         AG_ALPHAN=AG_ALPHAN+1                                            399
         AG_ALPHAK(AG_ALPHAN)=3                                           399
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                399
         AG_TITLE = 'SSLB'                                                399
         AG_EXNAME = 'POSITION'                                           399
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      399
         CALL AXPOSITION                                                  399
C    *                                                                    399
C    Geant POSITION : SSLB ===> X=-SFJP_SSLB_PX Y=-SFJP_SSLB_PX Z=-SFJP_  399
C    SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ/2. AL  399
C    PHAZ=SFJP_ALPHAZH <===                                               399
         CALL AGSRESET                                                    402
         AG_X = -SFJP_SSLB_PX                                             402
         AG_Y = -SFJP_SSLB_PX                                             402
         AG_Z =                                                           402
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          402
     *   SFJP_SSLB_DZ/2.                                                  402
         AG_ALPHAN=AG_ALPHAN+1                                            402
         AG_ALPHAK(AG_ALPHAN)=3                                           402
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                402
         AG_TITLE = 'SSLB'                                                402
         AG_EXNAME = 'POSITION'                                           402
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      402
         CALL AXPOSITION                                                  402
C    *                                                                    402
C    Geant POSITION : SSLB ===> X=SFJP_SSLB_PX Y=-SFJP_SSLB_PX Z=-SFJP_S  402
C    FLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ/2. ALP  402
C    HAZ=-SFJP_ALPHAZH <===                                               402
         CALL AGSRESET                                                    405
         AG_X = SFJP_SSLB_PX                                              405
         AG_Y = -SFJP_SSLB_PX                                             405
         AG_Z =                                                           405
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          405
     *   SFJP_SSLB_DZ/2.                                                  405
         AG_ALPHAN=AG_ALPHAN+1                                            405
         AG_ALPHAK(AG_ALPHAN)=3                                           405
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               405
         AG_TITLE = 'SSLB'                                                405
         AG_EXNAME = 'POSITION'                                           405
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      405
         CALL AXPOSITION                                                  405
C    *                                                                    405
C    Geant POSITION : SSLB ===> X=-SFJP_SSLB_PX Y=SFJP_SSLB_PX Z=-SFJP_S  405
C    FLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ/2. ALP  405
C    HAZ=-SFJP_ALPHAZH <===                                               405
         CALL AGSRESET                                                    408
         AG_X = -SFJP_SSLB_PX                                             408
         AG_Y = SFJP_SSLB_PX                                              408
         AG_Z =                                                           408
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          408
     *   SFJP_SSLB_DZ/2.                                                  408
         AG_ALPHAN=AG_ALPHAN+1                                            408
         AG_ALPHAK(AG_ALPHAN)=3                                           408
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               408
         AG_TITLE = 'SSLB'                                                408
         AG_EXNAME = 'POSITION'                                           408
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      408
         CALL AXPOSITION                                                  408
C    *                                                                    408
C    Geant POSITION : SSLB ===> X=SFJP_SSLB_PX Y=SFJP_SSLB_PX Z=+SFJP_SF  408
C    LU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ/2. ALPH  408
C    AZ=SFJP_ALPHAZH <===                                                 408
         CALL AGSRESET                                                    411
         AG_X = SFJP_SSLB_PX                                              411
         AG_Y = SFJP_SSLB_PX                                              411
         AG_Z =                                                           411
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          411
     *   SFJP_SSLB_DZ/2.                                                  411
         AG_ALPHAN=AG_ALPHAN+1                                            411
         AG_ALPHAK(AG_ALPHAN)=3                                           411
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                411
         AG_TITLE = 'SSLB'                                                411
         AG_EXNAME = 'POSITION'                                           411
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      411
         CALL AXPOSITION                                                  411
C    *                                                                    411
C    Geant POSITION : SSLB ===> X=-SFJP_SSLB_PX Y=-SFJP_SSLB_PX Z=+SFJP_  411
C    SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ/2. AL  411
C    PHAZ=SFJP_ALPHAZH <===                                               411
         CALL AGSRESET                                                    414
         AG_X = -SFJP_SSLB_PX                                             414
         AG_Y = -SFJP_SSLB_PX                                             414
         AG_Z =                                                           414
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          414
     *   SFJP_SSLB_DZ/2.                                                  414
         AG_ALPHAN=AG_ALPHAN+1                                            414
         AG_ALPHAK(AG_ALPHAN)=3                                           414
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                414
         AG_TITLE = 'SSLB'                                                414
         AG_EXNAME = 'POSITION'                                           414
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      414
         CALL AXPOSITION                                                  414
C    *                                                                    414
C    Geant POSITION : SSLB ===> X=SFJP_SSLB_PX Y=-SFJP_SSLB_PX Z=+SFJP_S  414
C    FLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ/2. ALP  414
C    HAZ=-SFJP_ALPHAZH <===                                               414
         CALL AGSRESET                                                    417
         AG_X = SFJP_SSLB_PX                                              417
         AG_Y = -SFJP_SSLB_PX                                             417
         AG_Z =                                                           417
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          417
     *   SFJP_SSLB_DZ/2.                                                  417
         AG_ALPHAN=AG_ALPHAN+1                                            417
         AG_ALPHAK(AG_ALPHAN)=3                                           417
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               417
         AG_TITLE = 'SSLB'                                                417
         AG_EXNAME = 'POSITION'                                           417
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      417
         CALL AXPOSITION                                                  417
C    *                                                                    417
C    Geant POSITION : SSLB ===> X=-SFJP_SSLB_PX Y=SFJP_SSLB_PX Z=+SFJP_S  417
C    FLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ/2. ALP  417
C    HAZ=-SFJP_ALPHAZH <===                                               417
         CALL AGSRESET                                                    420
         AG_X = -SFJP_SSLB_PX                                             420
         AG_Y = SFJP_SSLB_PX                                              420
         AG_Z =                                                           420
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          420
     *   SFJP_SSLB_DZ/2.                                                  420
         AG_ALPHAN=AG_ALPHAN+1                                            420
         AG_ALPHAK(AG_ALPHAN)=3                                           420
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               420
         AG_TITLE = 'SSLB'                                                420
         AG_EXNAME = 'POSITION'                                           420
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      420
         CALL AXPOSITION                                                  420
C    *                                                                    420
         ELSE                                                             420
C    Geant POSITION : SSLB ===> X=SFJP_SSLT_PX Y=SFJP_SSLT_PX Z=-SFJP_SF  424
C    LU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ/2. ALPH  424
C    AZ=0 <===                                                            424
         CALL AGSRESET                                                    424
         AG_X = SFJP_SSLT_PX                                              424
         AG_Y = SFJP_SSLT_PX                                              424
         AG_Z =                                                           424
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          424
     *   SFJP_SSLB_DZ/2.                                                  424
         AG_ALPHAN=AG_ALPHAN+1                                            424
         AG_ALPHAK(AG_ALPHAN)=3                                           424
         AG_ALPHAL(AG_ALPHAN)=0                                           424
         AG_TITLE = 'SSLB'                                                424
         AG_EXNAME = 'POSITION'                                           424
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      424
         CALL AXPOSITION                                                  424
C    *                                                                    424
C    Geant POSITION : SSLB ===> X=-SFJP_SSLT_PX Y=-SFJP_SSLT_PX Z=-SFJP_  424
C    SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ/2. AL  424
C    PHAZ=0 <===                                                          424
         CALL AGSRESET                                                    427
         AG_X = -SFJP_SSLT_PX                                             427
         AG_Y = -SFJP_SSLT_PX                                             427
         AG_Z =                                                           427
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          427
     *   SFJP_SSLB_DZ/2.                                                  427
         AG_ALPHAN=AG_ALPHAN+1                                            427
         AG_ALPHAK(AG_ALPHAN)=3                                           427
         AG_ALPHAL(AG_ALPHAN)=0                                           427
         AG_TITLE = 'SSLB'                                                427
         AG_EXNAME = 'POSITION'                                           427
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      427
         CALL AXPOSITION                                                  427
C    *                                                                    427
C    Geant POSITION : SSLB ===> X=SFJP_SSLT_PX Y=-SFJP_SSLT_PX Z=-SFJP_S  427
C    FLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ/2. ALP  427
C    HAZ=0 <===                                                           427
         CALL AGSRESET                                                    430
         AG_X = SFJP_SSLT_PX                                              430
         AG_Y = -SFJP_SSLT_PX                                             430
         AG_Z =                                                           430
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          430
     *   SFJP_SSLB_DZ/2.                                                  430
         AG_ALPHAN=AG_ALPHAN+1                                            430
         AG_ALPHAK(AG_ALPHAN)=3                                           430
         AG_ALPHAL(AG_ALPHAN)=0                                           430
         AG_TITLE = 'SSLB'                                                430
         AG_EXNAME = 'POSITION'                                           430
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      430
         CALL AXPOSITION                                                  430
C    *                                                                    430
C    Geant POSITION : SSLB ===> X=-SFJP_SSLT_PX Y=SFJP_SSLT_PX Z=-SFJP_S  430
C    FLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ/2. ALP  430
C    HAZ=0 <===                                                           430
         CALL AGSRESET                                                    433
         AG_X = -SFJP_SSLT_PX                                             433
         AG_Y = SFJP_SSLT_PX                                              433
         AG_Z =                                                           433
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          433
     *   SFJP_SSLB_DZ/2.                                                  433
         AG_ALPHAN=AG_ALPHAN+1                                            433
         AG_ALPHAK(AG_ALPHAN)=3                                           433
         AG_ALPHAL(AG_ALPHAN)=0                                           433
         AG_TITLE = 'SSLB'                                                433
         AG_EXNAME = 'POSITION'                                           433
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      433
         CALL AXPOSITION                                                  433
C    *                                                                    433
C    Geant POSITION : SSLB ===> X=SFJP_SSLT_PX Y=SFJP_SSLT_PX Z=+SFJP_SF  433
C    LU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ/2. ALPH  433
C    AY=180 <===                                                          433
         CALL AGSRESET                                                    436
         AG_X = SFJP_SSLT_PX                                              436
         AG_Y = SFJP_SSLT_PX                                              436
         AG_Z =                                                           436
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          436
     *   SFJP_SSLB_DZ/2.                                                  436
         AG_ALPHAN=AG_ALPHAN+1                                            436
         AG_ALPHAK(AG_ALPHAN)=2                                           436
         AG_ALPHAL(AG_ALPHAN)=180                                         436
         AG_TITLE = 'SSLB'                                                436
         AG_EXNAME = 'POSITION'                                           436
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      436
         CALL AXPOSITION                                                  436
C    *                                                                    436
C    Geant POSITION : SSLB ===> X=-SFJP_SSLT_PX Y=-SFJP_SSLT_PX Z=+SFJP_  436
C    SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ/2. AL  436
C    PHAY=180 <===                                                        436
         CALL AGSRESET                                                    439
         AG_X = -SFJP_SSLT_PX                                             439
         AG_Y = -SFJP_SSLT_PX                                             439
         AG_Z =                                                           439
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          439
     *   SFJP_SSLB_DZ/2.                                                  439
         AG_ALPHAN=AG_ALPHAN+1                                            439
         AG_ALPHAK(AG_ALPHAN)=2                                           439
         AG_ALPHAL(AG_ALPHAN)=180                                         439
         AG_TITLE = 'SSLB'                                                439
         AG_EXNAME = 'POSITION'                                           439
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      439
         CALL AXPOSITION                                                  439
C    *                                                                    439
C    Geant POSITION : SSLB ===> X=SFJP_SSLT_PX Y=-SFJP_SSLT_PX Z=+SFJP_S  439
C    FLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ/2. ALP  439
C    HAY=180 <===                                                         439
         CALL AGSRESET                                                    442
         AG_X = SFJP_SSLT_PX                                              442
         AG_Y = -SFJP_SSLT_PX                                             442
         AG_Z =                                                           442
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          442
     *   SFJP_SSLB_DZ/2.                                                  442
         AG_ALPHAN=AG_ALPHAN+1                                            442
         AG_ALPHAK(AG_ALPHAN)=2                                           442
         AG_ALPHAL(AG_ALPHAN)=180                                         442
         AG_TITLE = 'SSLB'                                                442
         AG_EXNAME = 'POSITION'                                           442
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      442
         CALL AXPOSITION                                                  442
C    *                                                                    442
C    Geant POSITION : SSLB ===> X=-SFJP_SSLT_PX Y=SFJP_SSLT_PX Z=+SFJP_S  442
C    FLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ/2. ALP  442
C    HAY=180 <===                                                         442
         CALL AGSRESET                                                    445
         AG_X = -SFJP_SSLT_PX                                             445
         AG_Y = SFJP_SSLT_PX                                              445
         AG_Z =                                                           445
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          445
     *   SFJP_SSLB_DZ/2.                                                  445
         AG_ALPHAN=AG_ALPHAN+1                                            445
         AG_ALPHAK(AG_ALPHAN)=2                                           445
         AG_ALPHAL(AG_ALPHAN)=180                                         445
         AG_TITLE = 'SSLB'                                                445
         AG_EXNAME = 'POSITION'                                           445
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      445
         CALL AXPOSITION                                                  445
C    *                                                                    445
         ENDIF                                                            445
C    Create SSLT :                                                        447
C    Call To Local Procedure SSLT                                         447
         ASSIGN 5180 TO BLOCK_SSLT                                        447
         GO TO  5170                                                      447
5180     CONTINUE                                                         447
C    *                                                                    447
C    Geant POSITION : SSLT ===> X=SFJP_SSLT_PX Y=SFJP_SSLT_PX Z=-SFJP_SF  449
C    LU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ-SFJP_SS  449
C    LT_DZ/2. <===                                                        449
         CALL AGSRESET                                                    449
         AG_X = SFJP_SSLT_PX                                              449
         AG_Y = SFJP_SSLT_PX                                              449
         AG_Z =                                                           449
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          449
     *   SFJP_SSLB_DZ-SFJP_SSLT_DZ/2.                                     449
         AG_TITLE = 'SSLT'                                                449
         AG_EXNAME = 'POSITION'                                           449
         AG_PARLIST = 'X_Y_Z'                                             449
         CALL AXPOSITION                                                  449
C    *                                                                    449
C    Geant POSITION : SSLT ===> X=-SFJP_SSLT_PX Y=-SFJP_SSLT_PX Z=-SFJP_  449
C    SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ-SFJP_  449
C    SSLT_DZ/2. <===                                                      449
         CALL AGSRESET                                                    451
         AG_X = -SFJP_SSLT_PX                                             451
         AG_Y = -SFJP_SSLT_PX                                             451
         AG_Z =                                                           451
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          451
     *   SFJP_SSLB_DZ-SFJP_SSLT_DZ/2.                                     451
         AG_TITLE = 'SSLT'                                                451
         AG_EXNAME = 'POSITION'                                           451
         AG_PARLIST = 'X_Y_Z'                                             451
         CALL AXPOSITION                                                  451
C    *                                                                    451
C    Geant POSITION : SSLT ===> X=SFJP_SSLT_PX Y=-SFJP_SSLT_PX Z=-SFJP_S  451
C    FLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ-SFJP_S  451
C    SLT_DZ/2. <===                                                       451
         CALL AGSRESET                                                    453
         AG_X = SFJP_SSLT_PX                                              453
         AG_Y = -SFJP_SSLT_PX                                             453
         AG_Z =                                                           453
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          453
     *   SFJP_SSLB_DZ-SFJP_SSLT_DZ/2.                                     453
         AG_TITLE = 'SSLT'                                                453
         AG_EXNAME = 'POSITION'                                           453
         AG_PARLIST = 'X_Y_Z'                                             453
         CALL AXPOSITION                                                  453
C    *                                                                    453
C    Geant POSITION : SSLT ===> X=-SFJP_SSLT_PX Y=SFJP_SSLT_PX Z=-SFJP_S  453
C    FLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ-SFJP_S  453
C    SLT_DZ/2. <===                                                       453
         CALL AGSRESET                                                    455
         AG_X = -SFJP_SSLT_PX                                             455
         AG_Y = SFJP_SSLT_PX                                              455
         AG_Z =                                                           455
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          455
     *   SFJP_SSLB_DZ-SFJP_SSLT_DZ/2.                                     455
         AG_TITLE = 'SSLT'                                                455
         AG_EXNAME = 'POSITION'                                           455
         AG_PARLIST = 'X_Y_Z'                                             455
         CALL AXPOSITION                                                  455
C    *                                                                    455
C    Geant POSITION : SSLT ===> X=SFJP_SSLT_PX Y=SFJP_SSLT_PX Z=+SFJP_SF  455
C    LU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ+SFJP_SS  455
C    LT_DZ/2. <===                                                        455
         CALL AGSRESET                                                    457
         AG_X = SFJP_SSLT_PX                                              457
         AG_Y = SFJP_SSLT_PX                                              457
         AG_Z =                                                           457
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          457
     *   SFJP_SSLB_DZ+SFJP_SSLT_DZ/2.                                     457
         AG_TITLE = 'SSLT'                                                457
         AG_EXNAME = 'POSITION'                                           457
         AG_PARLIST = 'X_Y_Z'                                             457
         CALL AXPOSITION                                                  457
C    *                                                                    457
C    Geant POSITION : SSLT ===> X=-SFJP_SSLT_PX Y=-SFJP_SSLT_PX Z=+SFJP_  457
C    SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ+SFJP_  457
C    SSLT_DZ/2. <===                                                      457
         CALL AGSRESET                                                    459
         AG_X = -SFJP_SSLT_PX                                             459
         AG_Y = -SFJP_SSLT_PX                                             459
         AG_Z =                                                           459
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          459
     *   SFJP_SSLB_DZ+SFJP_SSLT_DZ/2.                                     459
         AG_TITLE = 'SSLT'                                                459
         AG_EXNAME = 'POSITION'                                           459
         AG_PARLIST = 'X_Y_Z'                                             459
         CALL AXPOSITION                                                  459
C    *                                                                    459
C    Geant POSITION : SSLT ===> X=SFJP_SSLT_PX Y=-SFJP_SSLT_PX Z=+SFJP_S  459
C    FLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ+SFJP_S  459
C    SLT_DZ/2. <===                                                       459
         CALL AGSRESET                                                    461
         AG_X = SFJP_SSLT_PX                                              461
         AG_Y = -SFJP_SSLT_PX                                             461
         AG_Z =                                                           461
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          461
     *   SFJP_SSLB_DZ+SFJP_SSLT_DZ/2.                                     461
         AG_TITLE = 'SSLT'                                                461
         AG_EXNAME = 'POSITION'                                           461
         AG_PARLIST = 'X_Y_Z'                                             461
         CALL AXPOSITION                                                  461
C    *                                                                    461
C    Geant POSITION : SSLT ===> X=-SFJP_SSLT_PX Y=SFJP_SSLT_PX Z=+SFJP_S  461
C    FLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ+SFJP_S  461
C    SLT_DZ/2. <===                                                       461
         CALL AGSRESET                                                    463
         AG_X = -SFJP_SSLT_PX                                             463
         AG_Y = SFJP_SSLT_PX                                              463
         AG_Z =                                                           463
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          463
     *   SFJP_SSLB_DZ+SFJP_SSLT_DZ/2.                                     463
         AG_TITLE = 'SSLT'                                                463
         AG_EXNAME = 'POSITION'                                           463
         AG_PARLIST = 'X_Y_Z'                                             463
         CALL AXPOSITION                                                  463
C    *                                                                    463
C    Create SCMP :                                                        464
C    Call To Local Procedure SCMP                                         464
         ASSIGN 5200 TO BLOCK_SCMP                                        464
         GO TO  5190                                                      464
5200     CONTINUE                                                         464
C    *                                                                    464
C    Geant POSITION : SCMP ===> X=SFJP_SCMP_PX Y=SFJP_SCMP_PX Z=SFJP_SCV  465
C    M_PZ ALPHAZ=-SFJP_ALPHAZH <===                                       465
         CALL AGSRESET                                                    465
         AG_X = SFJP_SCMP_PX                                              465
         AG_Y = SFJP_SCMP_PX                                              465
         AG_Z = SFJP_SCVM_PZ                                              465
         AG_ALPHAN=AG_ALPHAN+1                                            465
         AG_ALPHAK(AG_ALPHAN)=3                                           465
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               465
         AG_TITLE = 'SCMP'                                                465
         AG_EXNAME = 'POSITION'                                           465
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      465
         CALL AXPOSITION                                                  465
C    *                                                                    465
C    Geant POSITION : SCMP ===> X=-SFJP_SCMP_PX Y=-SFJP_SCMP_PX Z=SFJP_S  465
C    CVM_PZ ALPHAZ=-SFJP_ALPHAZH <===                                     465
         CALL AGSRESET                                                    467
         AG_X = -SFJP_SCMP_PX                                             467
         AG_Y = -SFJP_SCMP_PX                                             467
         AG_Z = SFJP_SCVM_PZ                                              467
         AG_ALPHAN=AG_ALPHAN+1                                            467
         AG_ALPHAK(AG_ALPHAN)=3                                           467
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               467
         AG_TITLE = 'SCMP'                                                467
         AG_EXNAME = 'POSITION'                                           467
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      467
         CALL AXPOSITION                                                  467
C    *                                                                    467
C    Geant POSITION : SCMP ===> X=SFJP_SCMP_PX Y=-SFJP_SCMP_PX Z=SFJP_SC  467
C    VM_PZ ALPHAZ=SFJP_ALPHAZH <===                                       467
         CALL AGSRESET                                                    468
         AG_X = SFJP_SCMP_PX                                              468
         AG_Y = -SFJP_SCMP_PX                                             468
         AG_Z = SFJP_SCVM_PZ                                              468
         AG_ALPHAN=AG_ALPHAN+1                                            468
         AG_ALPHAK(AG_ALPHAN)=3                                           468
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                468
         AG_TITLE = 'SCMP'                                                468
         AG_EXNAME = 'POSITION'                                           468
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      468
         CALL AXPOSITION                                                  468
C    *                                                                    468
C    Geant POSITION : SCMP ===> X=-SFJP_SCMP_PX Y=SFJP_SCMP_PX Z=SFJP_SC  468
C    VM_PZ ALPHAZ=SFJP_ALPHAZH <===                                       468
         CALL AGSRESET                                                    469
         AG_X = -SFJP_SCMP_PX                                             469
         AG_Y = SFJP_SCMP_PX                                              469
         AG_Z = SFJP_SCVM_PZ                                              469
         AG_ALPHAN=AG_ALPHAN+1                                            469
         AG_ALPHAK(AG_ALPHAN)=3                                           469
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                469
         AG_TITLE = 'SCMP'                                                469
         AG_EXNAME = 'POSITION'                                           469
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      469
         CALL AXPOSITION                                                  469
C    *                                                                    469
C    Geant POSITION : SCMP ===> X=SFJP_SCMP_PX Y=SFJP_SCMP_PX Z=-SFJP_SC  469
C    VM_PZ ALPHAZ=-SFJP_ALPHAZH <===                                      469
         CALL AGSRESET                                                    471
         AG_X = SFJP_SCMP_PX                                              471
         AG_Y = SFJP_SCMP_PX                                              471
         AG_Z = -SFJP_SCVM_PZ                                             471
         AG_ALPHAN=AG_ALPHAN+1                                            471
         AG_ALPHAK(AG_ALPHAN)=3                                           471
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               471
         AG_TITLE = 'SCMP'                                                471
         AG_EXNAME = 'POSITION'                                           471
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      471
         CALL AXPOSITION                                                  471
C    *                                                                    471
C    Geant POSITION : SCMP ===> X=-SFJP_SCMP_PX Y=-SFJP_SCMP_PX Z=-SFJP_  471
C    SCVM_PZ ALPHAZ=-SFJP_ALPHAZH <===                                    471
         CALL AGSRESET                                                    473
         AG_X = -SFJP_SCMP_PX                                             473
         AG_Y = -SFJP_SCMP_PX                                             473
         AG_Z = -SFJP_SCVM_PZ                                             473
         AG_ALPHAN=AG_ALPHAN+1                                            473
         AG_ALPHAK(AG_ALPHAN)=3                                           473
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               473
         AG_TITLE = 'SCMP'                                                473
         AG_EXNAME = 'POSITION'                                           473
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      473
         CALL AXPOSITION                                                  473
C    *                                                                    473
C    Geant POSITION : SCMP ===> X=SFJP_SCMP_PX Y=-SFJP_SCMP_PX Z=-SFJP_S  473
C    CVM_PZ ALPHAZ=SFJP_ALPHAZH <===                                      473
         CALL AGSRESET                                                    475
         AG_X = SFJP_SCMP_PX                                              475
         AG_Y = -SFJP_SCMP_PX                                             475
         AG_Z = -SFJP_SCVM_PZ                                             475
         AG_ALPHAN=AG_ALPHAN+1                                            475
         AG_ALPHAK(AG_ALPHAN)=3                                           475
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                475
         AG_TITLE = 'SCMP'                                                475
         AG_EXNAME = 'POSITION'                                           475
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      475
         CALL AXPOSITION                                                  475
C    *                                                                    475
C    Geant POSITION : SCMP ===> X=-SFJP_SCMP_PX Y=SFJP_SCMP_PX Z=-SFJP_S  475
C    CVM_PZ ALPHAZ=SFJP_ALPHAZH <===                                      475
         CALL AGSRESET                                                    477
         AG_X = -SFJP_SCMP_PX                                             477
         AG_Y = SFJP_SCMP_PX                                              477
         AG_Z = -SFJP_SCVM_PZ                                             477
         AG_ALPHAN=AG_ALPHAN+1                                            477
         AG_ALPHAK(AG_ALPHAN)=3                                           477
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                477
         AG_TITLE = 'SCMP'                                                477
         AG_EXNAME = 'POSITION'                                           477
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      477
         CALL AXPOSITION                                                  477
C    *                                                                    477
C    Create SCVM :                                                        478
C    Call To Local Procedure SCVM                                         478
         ASSIGN 5220 TO BLOCK_SCVM                                        478
         GO TO  5210                                                      478
5220     CONTINUE                                                         478
C    *                                                                    478
C    Geant POSITION : SCVM ===> X=SFJP_SCVM_PX Y=SFJP_SCVM_PX Z=SFJP_SCV  479
C    M_PZ ALPHAZ=-SFJP_ALPHAZH <===                                       479
         CALL AGSRESET                                                    479
         AG_X = SFJP_SCVM_PX                                              479
         AG_Y = SFJP_SCVM_PX                                              479
         AG_Z = SFJP_SCVM_PZ                                              479
         AG_ALPHAN=AG_ALPHAN+1                                            479
         AG_ALPHAK(AG_ALPHAN)=3                                           479
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               479
         AG_TITLE = 'SCVM'                                                479
         AG_EXNAME = 'POSITION'                                           479
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      479
         CALL AXPOSITION                                                  479
C    *                                                                    479
C    Geant POSITION : SCVM ===> X=-SFJP_SCVM_PX Y=-SFJP_SCVM_PX Z=SFJP_S  479
C    CVM_PZ ALPHAZ=(180.-SFJP_ALPHAZH) <===                               479
         CALL AGSRESET                                                    481
         AG_X = -SFJP_SCVM_PX                                             481
         AG_Y = -SFJP_SCVM_PX                                             481
         AG_Z = SFJP_SCVM_PZ                                              481
         AG_ALPHAN=AG_ALPHAN+1                                            481
         AG_ALPHAK(AG_ALPHAN)=3                                           481
         AG_ALPHAL(AG_ALPHAN)=(180.-SFJP_ALPHAZH)                         481
         AG_TITLE = 'SCVM'                                                481
         AG_EXNAME = 'POSITION'                                           481
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      481
         CALL AXPOSITION                                                  481
C    *                                                                    481
C    Geant POSITION : SCVM ===> X=SFJP_SCVM_PX Y=-SFJP_SCVM_PX Z=SFJP_SC  481
C    VM_PZ ALPHAZ=-(180.-SFJP_ALPHAZH) <===                               481
         CALL AGSRESET                                                    483
         AG_X = SFJP_SCVM_PX                                              483
         AG_Y = -SFJP_SCVM_PX                                             483
         AG_Z = SFJP_SCVM_PZ                                              483
         AG_ALPHAN=AG_ALPHAN+1                                            483
         AG_ALPHAK(AG_ALPHAN)=3                                           483
         AG_ALPHAL(AG_ALPHAN)=-(180.-SFJP_ALPHAZH)                        483
         AG_TITLE = 'SCVM'                                                483
         AG_EXNAME = 'POSITION'                                           483
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      483
         CALL AXPOSITION                                                  483
C    *                                                                    483
C    Geant POSITION : SCVM ===> X=-SFJP_SCVM_PX Y=SFJP_SCVM_PX Z=SFJP_SC  483
C    VM_PZ ALPHAZ=SFJP_ALPHAZH <===                                       483
         CALL AGSRESET                                                    484
         AG_X = -SFJP_SCVM_PX                                             484
         AG_Y = SFJP_SCVM_PX                                              484
         AG_Z = SFJP_SCVM_PZ                                              484
         AG_ALPHAN=AG_ALPHAN+1                                            484
         AG_ALPHAK(AG_ALPHAN)=3                                           484
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                484
         AG_TITLE = 'SCVM'                                                484
         AG_EXNAME = 'POSITION'                                           484
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      484
         CALL AXPOSITION                                                  484
C    *                                                                    484
C    Geant POSITION : SCVM ===> X=SFJP_SCVM_PX Y=SFJP_SCVM_PX Z=-SFJP_SC  484
C    VM_PZ ALPHAZ=-SFJP_ALPHAZH <===                                      484
         CALL AGSRESET                                                    486
         AG_X = SFJP_SCVM_PX                                              486
         AG_Y = SFJP_SCVM_PX                                              486
         AG_Z = -SFJP_SCVM_PZ                                             486
         AG_ALPHAN=AG_ALPHAN+1                                            486
         AG_ALPHAK(AG_ALPHAN)=3                                           486
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               486
         AG_TITLE = 'SCVM'                                                486
         AG_EXNAME = 'POSITION'                                           486
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      486
         CALL AXPOSITION                                                  486
C    *                                                                    486
C    Geant POSITION : SCVM ===> X=-SFJP_SCVM_PX Y=-SFJP_SCVM_PX Z=-SFJP_  486
C    SCVM_PZ ALPHAZ=(180.-SFJP_ALPHAZH) <===                              486
         CALL AGSRESET                                                    488
         AG_X = -SFJP_SCVM_PX                                             488
         AG_Y = -SFJP_SCVM_PX                                             488
         AG_Z = -SFJP_SCVM_PZ                                             488
         AG_ALPHAN=AG_ALPHAN+1                                            488
         AG_ALPHAK(AG_ALPHAN)=3                                           488
         AG_ALPHAL(AG_ALPHAN)=(180.-SFJP_ALPHAZH)                         488
         AG_TITLE = 'SCVM'                                                488
         AG_EXNAME = 'POSITION'                                           488
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      488
         CALL AXPOSITION                                                  488
C    *                                                                    488
C    Geant POSITION : SCVM ===> X=SFJP_SCVM_PX Y=-SFJP_SCVM_PX Z=-SFJP_S  488
C    CVM_PZ ALPHAZ=-(180.-SFJP_ALPHAZH) <===                              488
         CALL AGSRESET                                                    490
         AG_X = SFJP_SCVM_PX                                              490
         AG_Y = -SFJP_SCVM_PX                                             490
         AG_Z = -SFJP_SCVM_PZ                                             490
         AG_ALPHAN=AG_ALPHAN+1                                            490
         AG_ALPHAK(AG_ALPHAN)=3                                           490
         AG_ALPHAL(AG_ALPHAN)=-(180.-SFJP_ALPHAZH)                        490
         AG_TITLE = 'SCVM'                                                490
         AG_EXNAME = 'POSITION'                                           490
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      490
         CALL AXPOSITION                                                  490
C    *                                                                    490
C    Geant POSITION : SCVM ===> X=-SFJP_SCVM_PX Y=SFJP_SCVM_PX Z=-SFJP_S  490
C    CVM_PZ ALPHAZ=SFJP_ALPHAZH <===                                      490
         CALL AGSRESET                                                    492
         AG_X = -SFJP_SCVM_PX                                             492
         AG_Y = SFJP_SCVM_PX                                              492
         AG_Z = -SFJP_SCVM_PZ                                             492
         AG_ALPHAN=AG_ALPHAN+1                                            492
         AG_ALPHAK(AG_ALPHAN)=3                                           492
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                492
         AG_TITLE = 'SCVM'                                                492
         AG_EXNAME = 'POSITION'                                           492
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      492
         CALL AXPOSITION                                                  492
C    *                                                                    492
5011  CALL AGSPOPB                                                        493
      GO TO BLOCK_SFMO, (5010 ,5020,5030)                                 493
C End of Local Procedure                                                  493
C ----------------------------------------------------------------------  494
C ------------ SFLM                                                       494
C Begin Local Procedure SFLM                                              495
C SFLM IS THE MOTHER OF THE LADDER                                        495
5040  CONTINUE                                                            495
         AG_TITLE='SFLM'                                                  495
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
C    Geant ATTRIBUTE : SFLM ===> SEEN=1 COLO=6 <===                       496
         CALL AGSRESET                                                    497
         AG_SEEN = 1                                                      497
         AG_COLO = 6                                                      497
         AG_TITLE = 'SFLM'                                                497
         AG_EXNAME = 'ATTRIBUTE'                                          497
         AG_PARLIST = 'SEEN_COLO'                                         497
         CALL AXATTRIBUTE                                                 497
C    *                                                                    497
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=(SFPA_DMTHK+SFPA_GPTHK+SF  497
C    PA_SMTHK)/2, DZ=SFPA_SMLEN/2 <===                                    497
         CALL AGSRESET                                                    499
         AG_DX = SFPA_DMWID/2                                             499
         AG_DY = (SFPA_DMTHK+SFPA_GPTHK+SFPA_SMTHK)/2                     499
         AG_DZ = SFPA_SMLEN/2                                             499
         AG_TITLE = 'BOX'                                                 499
         AG_EXNAME = 'SHAPE'                                              499
         AG_PARLIST = 'DX_DY_DZ'                                          499
         CALL AXSHAPE                                                     499
C    *                                                                    499
C    Create SFDM :                                                        500
C    Call To Local Procedure SFDM                                         500
         ASSIGN 5240 TO BLOCK_SFDM                                        500
         GO TO  5230                                                      500
5240     CONTINUE                                                         500
C    *                                                                    500
C    Geant POSITION : SFDM ===> Y=-(SFPA_SMTHK+SFPA_GPTHK)/2 <===         501
         CALL AGSRESET                                                    501
         AG_Y = -(SFPA_SMTHK+SFPA_GPTHK)/2                                501
         AG_TITLE = 'SFDM'                                                501
         AG_EXNAME = 'POSITION'                                           501
         AG_PARLIST = 'Y'                                                 501
         CALL AXPOSITION                                                  501
C    *                                                                    501
C    Create SFSM :                                                        502
C    Call To Local Procedure SFSM                                         502
         ASSIGN 5260 TO BLOCK_SFSM                                        502
         GO TO  5250                                                      502
5260     CONTINUE                                                         502
C    *                                                                    502
C    Geant POSITION : SFSM ===> Y=(SFPA_DMTHK+SFPA_GPTHK)/2 <===          503
         CALL AGSRESET                                                    503
         AG_Y = (SFPA_DMTHK+SFPA_GPTHK)/2                                 503
         AG_TITLE = 'SFSM'                                                503
         AG_EXNAME = 'POSITION'                                           503
         AG_PARLIST = 'Y'                                                 503
         CALL AXPOSITION                                                  503
C    *                                                                    503
5041  CALL AGSPOPB                                                        504
      GO TO BLOCK_SFLM, (5040 ,5050)                                      504
C End of Local Procedure                                                  504
C ----------------------------------------------------------------------  505
C ------------ SFDM                                                       505
C Begin Local Procedure SFDM                                              506
C SFDM IS THE MOTHER OF THE DETECTORS                                     506
5230  CONTINUE                                                            506
         AG_TITLE='SFDM'                                                  506
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
C    Geant ATTRIBUTE : SFDM ===> SEEN=0 COLO=6 <===                       507
         CALL AGSRESET                                                    508
         AG_SEEN = 0                                                      508
         AG_COLO = 6                                                      508
         AG_TITLE = 'SFDM'                                                508
         AG_EXNAME = 'ATTRIBUTE'                                          508
         AG_PARLIST = 'SEEN_COLO'                                         508
         CALL AXATTRIBUTE                                                 508
C    *                                                                    508
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=SFPA_DMTHK/2, DZ=SFPA_WPL  508
C    EN/2 <===                                                            508
         CALL AGSRESET                                                    510
         AG_DX = SFPA_DMWID/2                                             510
         AG_DY = SFPA_DMTHK/2                                             510
         AG_DZ = SFPA_WPLEN/2                                             510
         AG_TITLE = 'BOX'                                                 510
         AG_EXNAME = 'SHAPE'                                              510
         AG_PARLIST = 'DX_DY_DZ'                                          510
         CALL AXSHAPE                                                     510
C    *                                                                    510
         WAFPCKLEN=SFPA_WPLEN/(SFPA_NSSD*1.)                              510
C    *                                                                    512
         IWAF=1                                                           512
            GO TO 5273                                                    512
5271        IWAF=IWAF+1                                                   512
5273        IF(IWAF-(SFPA_NSSD).GT.0) GO TO 5272                          512
C       Create SFSW :                                                     513
C       Call To Local Procedure SFSW                                      513
            ASSIGN 5290 TO BLOCK_SFSW                                     513
            GO TO  5280                                                   513
5290        CONTINUE                                                      513
C       *                                                                 513
C       Geant POSITION : SFSW ===> Z=-(SFPA_WPLEN+WAFPCKLEN)/2+IWAF*WAFP  514
C       CKLEN <===                                                        514
            CALL AGSRESET                                                 514
            AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+IWAF*WAFPCKLEN               514
            AG_TITLE = 'SFSW'                                             514
            AG_EXNAME = 'POSITION'                                        514
            AG_PARLIST = 'Z'                                              514
            CALL AXPOSITION                                               514
C       *                                                                 514
         GO TO 5271                                                       515
5272     CONTINUE                                                         515
5231  CALL AGSPOPB                                                        516
      GO TO BLOCK_SFDM, (5230 ,5240)                                      516
C End of Local Procedure                                                  516
C ----------------------------------------------------------------------  517
C ------------ SFSW                                                       517
C Begin Local Procedure SFSW                                              518
C SFSW IS A SINGLE WAFER CONTAINER                                        518
5280  CONTINUE                                                            518
         AG_TITLE='SFSW'                                                  518
         AG_MODULE=MODULE                                                 518
         AG_MARK='V00'                                                    518
         CALL AGSPUSH                                                     518
         AG_MODULE=MODULE                                                 518
C    *                                                                    518
C    Geant MATERIAL : AIR ===> inherited arguments used <===              519
         CALL AGSRESET                                                    519
         AG_TITLE = 'AIR'                                                 519
         AG_EXNAME = 'MATERIAL'                                           519
         AG_PARLIST = 'NONE'                                              519
         CALL AXMATERIAL                                                  519
C    *                                                                    519
C    Geant ATTRIBUTE : SFSW ===> SEEN=0 COLO=6 <===                       519
         CALL AGSRESET                                                    520
         AG_SEEN = 0                                                      520
         AG_COLO = 6                                                      520
         AG_TITLE = 'SFSW'                                                520
         AG_EXNAME = 'ATTRIBUTE'                                          520
         AG_PARLIST = 'SEEN_COLO'                                         520
         CALL AXATTRIBUTE                                                 520
C    *                                                                    520
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=SFJP_SFSW_DY, DZ=SFJP_SFS  520
C    W_DZ <===                                                            520
         CALL AGSRESET                                                    522
         AG_DX = SFPA_DMWID/2                                             522
         AG_DY = SFJP_SFSW_DY                                             522
         AG_DZ = SFJP_SFSW_DZ                                             522
         AG_TITLE = 'BOX'                                                 522
         AG_EXNAME = 'SHAPE'                                              522
         AG_PARLIST = 'DX_DY_DZ'                                          522
         CALL AXSHAPE                                                     522
C    *                                                                    522
C    Create SFSL :                                                        523
C    Call To Local Procedure SFSL                                         523
         ASSIGN 5310 TO BLOCK_SFSL                                        523
         GO TO  5300                                                      523
5310     CONTINUE                                                         523
C    *                                                                    523
C    Geant POSITION : SFSL ===> inherited arguments used <===             524
         CALL AGSRESET                                                    524
         AG_TITLE = 'SFSL'                                                524
         AG_EXNAME = 'POSITION'                                           524
         AG_PARLIST = 'NONE'                                              524
         CALL AXPOSITION                                                  524
C    *                                                                    524
C    Create SFRA :                                                        525
C    Call To Local Procedure SFRA                                         525
         ASSIGN 5330 TO BLOCK_SFRA                                        525
         GO TO  5320                                                      525
5330     CONTINUE                                                         525
C    *                                                                    525
C    Geant POSITION : SFRA ===> X=0.0 Y=SFJP_SFRA_PY Z=SFJP_SFRA_PZ <===  526
         CALL AGSRESET                                                    526
         AG_X = 0.0                                                       526
         AG_Y = SFJP_SFRA_PY                                              526
         AG_Z = SFJP_SFRA_PZ                                              526
         AG_TITLE = 'SFRA'                                                526
         AG_EXNAME = 'POSITION'                                           526
         AG_PARLIST = 'X_Y_Z'                                             526
         CALL AXPOSITION                                                  526
C    *                                                                    526
C    Geant POSITION : SFRA ===> X=0.0 Y=SFJP_SFRA_PY Z=-SFJP_SFRA_PZ <==  526
C    =                                                                    526
         CALL AGSRESET                                                    527
         AG_X = 0.0                                                       527
         AG_Y = SFJP_SFRA_PY                                              527
         AG_Z = -SFJP_SFRA_PZ                                             527
         AG_TITLE = 'SFRA'                                                527
         AG_EXNAME = 'POSITION'                                           527
         AG_PARLIST = 'X_Y_Z'                                             527
         CALL AXPOSITION                                                  527
C    *                                                                    527
C    Create SFRS :                                                        528
C    Call To Local Procedure SFRS                                         528
         ASSIGN 5350 TO BLOCK_SFRS                                        528
         GO TO  5340                                                      528
5350     CONTINUE                                                         528
C    *                                                                    528
C    Geant POSITION : SFRS ===> X=SFJP_SFRS_PX Y=SFJP_SFRS_PY Z=SFJP_SFR  529
C    A_PZ <===                                                            529
         CALL AGSRESET                                                    529
         AG_X = SFJP_SFRS_PX                                              529
         AG_Y = SFJP_SFRS_PY                                              529
         AG_Z = SFJP_SFRA_PZ                                              529
         AG_TITLE = 'SFRS'                                                529
         AG_EXNAME = 'POSITION'                                           529
         AG_PARLIST = 'X_Y_Z'                                             529
         CALL AXPOSITION                                                  529
C    *                                                                    529
C    Geant POSITION : SFRS ===> X=SFJP_SFRS_PX Y=SFJP_SFRS_PY Z=-SFJP_SF  529
C    RA_PZ <===                                                           529
         CALL AGSRESET                                                    530
         AG_X = SFJP_SFRS_PX                                              530
         AG_Y = SFJP_SFRS_PY                                              530
         AG_Z = -SFJP_SFRA_PZ                                             530
         AG_TITLE = 'SFRS'                                                530
         AG_EXNAME = 'POSITION'                                           530
         AG_PARLIST = 'X_Y_Z'                                             530
         CALL AXPOSITION                                                  530
C    *                                                                    530
C    Geant POSITION : SFRS ===> X=-SFJP_SFRS_PX Y=SFJP_SFRS_PY Z=SFJP_SF  530
C    RA_PZ <===                                                           530
         CALL AGSRESET                                                    531
         AG_X = -SFJP_SFRS_PX                                             531
         AG_Y = SFJP_SFRS_PY                                              531
         AG_Z = SFJP_SFRA_PZ                                              531
         AG_TITLE = 'SFRS'                                                531
         AG_EXNAME = 'POSITION'                                           531
         AG_PARLIST = 'X_Y_Z'                                             531
         CALL AXPOSITION                                                  531
C    *                                                                    531
C    Geant POSITION : SFRS ===> X=-SFJP_SFRS_PX Y=SFJP_SFRS_PY Z=-SFJP_S  531
C    FRA_PZ <===                                                          531
         CALL AGSRESET                                                    532
         AG_X = -SFJP_SFRS_PX                                             532
         AG_Y = SFJP_SFRS_PY                                              532
         AG_Z = -SFJP_SFRA_PZ                                             532
         AG_TITLE = 'SFRS'                                                532
         AG_EXNAME = 'POSITION'                                           532
         AG_PARLIST = 'X_Y_Z'                                             532
         CALL AXPOSITION                                                  532
C    *                                                                    532
C    Create SFFX :                                                        533
C    Call To Local Procedure SFFX                                         533
         ASSIGN 5370 TO BLOCK_SFFX                                        533
         GO TO  5360                                                      533
5370     CONTINUE                                                         533
C    *                                                                    533
C    Geant POSITION : SFFX ===> X=0.0 Y=SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_S  534
C    FFX_DYZ Z=SFJP_SFRA_PZ <===                                          534
         CALL AGSRESET                                                    534
         AG_X = 0.0                                                       534
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ                   534
         AG_Z = SFJP_SFRA_PZ                                              534
         AG_TITLE = 'SFFX'                                                534
         AG_EXNAME = 'POSITION'                                           534
         AG_PARLIST = 'X_Y_Z'                                             534
         CALL AXPOSITION                                                  534
C    *                                                                    534
C    Geant POSITION : SFFX ===> X=0.0 Y=SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_S  534
C    FFX_DYZ Z=-SFJP_SFRA_PZ <===                                         534
         CALL AGSRESET                                                    535
         AG_X = 0.0                                                       535
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ                   535
         AG_Z = -SFJP_SFRA_PZ                                             535
         AG_TITLE = 'SFFX'                                                535
         AG_EXNAME = 'POSITION'                                           535
         AG_PARLIST = 'X_Y_Z'                                             535
         CALL AXPOSITION                                                  535
C    *                                                                    535
C    Create SFPI :                                                        536
C    Call To Local Procedure SFPI                                         536
         ASSIGN 5390 TO BLOCK_SFPI                                        536
         GO TO  5380                                                      536
5390     CONTINUE                                                         536
C    *                                                                    536
C    Geant POSITION : SFPI ===> X=SFJP_SFPI_PX Y=SFJP_SFRA_PY+SFJP_SFRA_  539
C    DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2 Z=SFJP_SFPI_PZ ALPHAX=90 <===         539
         CALL AGSRESET                                                    539
         AG_X = SFJP_SFPI_PX                                              539
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2     539
         AG_Z = SFJP_SFPI_PZ                                              539
         AG_ALPHAN=AG_ALPHAN+1                                            539
         AG_ALPHAK(AG_ALPHAN)=1                                           539
         AG_ALPHAL(AG_ALPHAN)=90                                          539
         AG_TITLE = 'SFPI'                                                539
         AG_EXNAME = 'POSITION'                                           539
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      539
         CALL AXPOSITION                                                  539
C    *                                                                    539
C    Geant POSITION : SFPI ===> X=-SFJP_SFPI_PX Y=SFJP_SFRA_PY+SFJP_SFRA  539
C    _DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2 Z=SFJP_SFPI_PZ ALPHAX=90 <===        539
         CALL AGSRESET                                                    542
         AG_X = -SFJP_SFPI_PX                                             542
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2     542
         AG_Z = SFJP_SFPI_PZ                                              542
         AG_ALPHAN=AG_ALPHAN+1                                            542
         AG_ALPHAK(AG_ALPHAN)=1                                           542
         AG_ALPHAL(AG_ALPHAN)=90                                          542
         AG_TITLE = 'SFPI'                                                542
         AG_EXNAME = 'POSITION'                                           542
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      542
         CALL AXPOSITION                                                  542
C    *                                                                    542
C    Geant POSITION : SFPI ===> X=SFJP_SFPI_PX Y=SFJP_SFRA_PY+SFJP_SFRA_  542
C    DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2 Z=-SFJP_SFPI_PZ ALPHAX=90 <===        542
         CALL AGSRESET                                                    545
         AG_X = SFJP_SFPI_PX                                              545
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2     545
         AG_Z = -SFJP_SFPI_PZ                                             545
         AG_ALPHAN=AG_ALPHAN+1                                            545
         AG_ALPHAK(AG_ALPHAN)=1                                           545
         AG_ALPHAL(AG_ALPHAN)=90                                          545
         AG_TITLE = 'SFPI'                                                545
         AG_EXNAME = 'POSITION'                                           545
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      545
         CALL AXPOSITION                                                  545
C    *                                                                    545
C    Geant POSITION : SFPI ===> X=-SFJP_SFPI_PX Y=SFJP_SFRA_PY+SFJP_SFRA  545
C    _DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2 Z=-SFJP_SFPI_PZ ALPHAX=90 <===       545
         CALL AGSRESET                                                    548
         AG_X = -SFJP_SFPI_PX                                             548
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2     548
         AG_Z = -SFJP_SFPI_PZ                                             548
         AG_ALPHAN=AG_ALPHAN+1                                            548
         AG_ALPHAK(AG_ALPHAN)=1                                           548
         AG_ALPHAL(AG_ALPHAN)=90                                          548
         AG_TITLE = 'SFPI'                                                548
         AG_EXNAME = 'POSITION'                                           548
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      548
         CALL AXPOSITION                                                  548
C    *                                                                    548
C    Create SFAA :                                                        549
C    Call To Local Procedure SFAA                                         549
         ASSIGN 5410 TO BLOCK_SFAA                                        549
         GO TO  5400                                                      549
5410     CONTINUE                                                         549
C    *                                                                    549
C    Geant POSITION : SFAA ===> X=-SFJP_SFAA_PX1-SFJP_SFAA_DX Y=SFJP_SFR  552
C    A_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=SFJP_SFAA_PZ1+SFJP  552
C    _SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ <===                            552
         CALL AGSRESET                                                    552
         AG_X = -SFJP_SFAA_PX1-SFJP_SFAA_DX                               552
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    552
         AG_Z = SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ    552
         AG_TITLE = 'SFAA'                                                552
         AG_EXNAME = 'POSITION'                                           552
         AG_PARLIST = 'X_Y_Z'                                             552
         CALL AXPOSITION                                                  552
C    *                                                                    552
C    Geant POSITION : SFAA ===> X=-SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_  552
C    PX2-SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_  552
C    SFAA_DY Z=SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ <=  552
C    ==                                                                   552
         CALL AGSRESET                                                    555
         AG_X = -SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_PX2-SFJP_SFAA_PX3   555
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    555
         AG_Z = SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ    555
         AG_TITLE = 'SFAA'                                                555
         AG_EXNAME = 'POSITION'                                           555
         AG_PARLIST = 'X_Y_Z'                                             555
         CALL AXPOSITION                                                  555
C    *                                                                    555
C    Geant POSITION : SFAA ===> X=-SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_  555
C    PX2-SFJP_SFAA_PX3-SFJP_SFAA_PX2-SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_S  555
C    FRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-S  555
C    FJP_SFAA_PZ3-SFJP_SFAA_DZ <===                                       555
         CALL AGSRESET                                                    559
         AG_X =                                                           559
     *   -SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_PX2-SFJP_SFAA_PX3-         559
     *   SFJP_SFAA_PX2-SFJP_SFAA_PX3                                      559
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    559
         AG_Z = SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ    559
         AG_TITLE = 'SFAA'                                                559
         AG_EXNAME = 'POSITION'                                           559
         AG_PARLIST = 'X_Y_Z'                                             559
         CALL AXPOSITION                                                  559
C    *                                                                    559
C    Geant POSITION : SFAA ===> X=+SFJP_SFAA_PX1+SFJP_SFAA_DX Y=SFJP_SFR  559
C    A_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=SFJP_SFAA_PZ1+SFJP  559
C    _SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ <===                            559
         CALL AGSRESET                                                    562
         AG_X = +SFJP_SFAA_PX1+SFJP_SFAA_DX                               562
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    562
         AG_Z = SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ    562
         AG_TITLE = 'SFAA'                                                562
         AG_EXNAME = 'POSITION'                                           562
         AG_PARLIST = 'X_Y_Z'                                             562
         CALL AXPOSITION                                                  562
C    *                                                                    562
C    Geant POSITION : SFAA ===> X=+SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_  562
C    PX2+SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_  562
C    SFAA_DY Z=SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ <=  562
C    ==                                                                   562
         CALL AGSRESET                                                    565
         AG_X = +SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_PX2+SFJP_SFAA_PX3   565
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    565
         AG_Z = SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ    565
         AG_TITLE = 'SFAA'                                                565
         AG_EXNAME = 'POSITION'                                           565
         AG_PARLIST = 'X_Y_Z'                                             565
         CALL AXPOSITION                                                  565
C    *                                                                    565
C    Geant POSITION : SFAA ===> X=+SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_  565
C    PX2+SFJP_SFAA_PX3+SFJP_SFAA_PX2+SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_S  565
C    FRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-S  565
C    FJP_SFAA_PZ3-SFJP_SFAA_DZ <===                                       565
         CALL AGSRESET                                                    569
         AG_X =                                                           569
     *   +SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_PX2+SFJP_SFAA_PX3+         569
     *   SFJP_SFAA_PX2+SFJP_SFAA_PX3                                      569
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    569
         AG_Z = SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ    569
         AG_TITLE = 'SFAA'                                                569
         AG_EXNAME = 'POSITION'                                           569
         AG_PARLIST = 'X_Y_Z'                                             569
         CALL AXPOSITION                                                  569
C    *                                                                    569
C    Geant POSITION : SFAA ===> X=-SFJP_SFAA_PX1-SFJP_SFAA_DX Y=SFJP_SFR  569
C    A_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=-SFJP_SFAA_PZ1-SFJ  569
C    P_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ <===                           569
         CALL AGSRESET                                                    572
         AG_X = -SFJP_SFAA_PX1-SFJP_SFAA_DX                               572
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    572
         AG_Z = -SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ   572
         AG_TITLE = 'SFAA'                                                572
         AG_EXNAME = 'POSITION'                                           572
         AG_PARLIST = 'X_Y_Z'                                             572
         CALL AXPOSITION                                                  572
C    *                                                                    572
C    Geant POSITION : SFAA ===> X=-SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_  572
C    PX2-SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_  572
C    SFAA_DY Z=-SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ <  572
C    ===                                                                  572
         CALL AGSRESET                                                    575
         AG_X = -SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_PX2-SFJP_SFAA_PX3   575
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    575
         AG_Z = -SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ   575
         AG_TITLE = 'SFAA'                                                575
         AG_EXNAME = 'POSITION'                                           575
         AG_PARLIST = 'X_Y_Z'                                             575
         CALL AXPOSITION                                                  575
C    *                                                                    575
C    Geant POSITION : SFAA ===> X=-SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_  575
C    PX2-SFJP_SFAA_PX3-SFJP_SFAA_PX2-SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_S  575
C    FRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=-SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+  575
C    SFJP_SFAA_PZ3+SFJP_SFAA_DZ <===                                      575
         CALL AGSRESET                                                    579
         AG_X =                                                           579
     *   -SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_PX2-SFJP_SFAA_PX3-         579
     *   SFJP_SFAA_PX2-SFJP_SFAA_PX3                                      579
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    579
         AG_Z = -SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ   579
         AG_TITLE = 'SFAA'                                                579
         AG_EXNAME = 'POSITION'                                           579
         AG_PARLIST = 'X_Y_Z'                                             579
         CALL AXPOSITION                                                  579
C    *                                                                    579
C    Geant POSITION : SFAA ===> X=+SFJP_SFAA_PX1+SFJP_SFAA_DX Y=SFJP_SFR  579
C    A_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=-SFJP_SFAA_PZ1-SFJ  579
C    P_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ <===                           579
         CALL AGSRESET                                                    582
         AG_X = +SFJP_SFAA_PX1+SFJP_SFAA_DX                               582
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    582
         AG_Z = -SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ   582
         AG_TITLE = 'SFAA'                                                582
         AG_EXNAME = 'POSITION'                                           582
         AG_PARLIST = 'X_Y_Z'                                             582
         CALL AXPOSITION                                                  582
C    *                                                                    582
C    Geant POSITION : SFAA ===> X=+SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_  582
C    PX2+SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_  582
C    SFAA_DY Z=-SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ <  582
C    ===                                                                  582
         CALL AGSRESET                                                    585
         AG_X = +SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_PX2+SFJP_SFAA_PX3   585
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    585
         AG_Z = -SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ   585
         AG_TITLE = 'SFAA'                                                585
         AG_EXNAME = 'POSITION'                                           585
         AG_PARLIST = 'X_Y_Z'                                             585
         CALL AXPOSITION                                                  585
C    *                                                                    585
C    Geant POSITION : SFAA ===> X=+SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_  585
C    PX2+SFJP_SFAA_PX3+SFJP_SFAA_PX2+SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_S  585
C    FRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=-SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+  585
C    SFJP_SFAA_PZ3+SFJP_SFAA_DZ <===                                      585
         CALL AGSRESET                                                    589
         AG_X =                                                           589
     *   +SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_PX2+SFJP_SFAA_PX3+         589
     *   SFJP_SFAA_PX2+SFJP_SFAA_PX3                                      589
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    589
         AG_Z = -SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ   589
         AG_TITLE = 'SFAA'                                                589
         AG_EXNAME = 'POSITION'                                           589
         AG_PARLIST = 'X_Y_Z'                                             589
         CALL AXPOSITION                                                  589
C    *                                                                    589
5281  CALL AGSPOPB                                                        590
      GO TO BLOCK_SFSW, (5280 ,5290)                                      590
C End of Local Procedure                                                  590
C ----------------------------------------------------------------------  591
C ------------ SFRA                                                       591
C Begin Local Procedure SFRA                                              592
C SFRA IS THE HYBRID STIFFNEER                                            592
5320  CONTINUE                                                            592
         AG_TITLE='SFRA'                                                  592
         AG_MODULE=MODULE                                                 592
         AG_MARK='V00'                                                    592
         CALL AGSPUSH                                                     592
         AG_MODULE=MODULE                                                 592
C    *                                                                    592
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           593
         CALL AGSRESET                                                    593
         AG_TITLE = 'CARBON'                                              593
         AG_EXNAME = 'MATERIAL'                                           593
         AG_PARLIST = 'NONE'                                              593
         CALL AXMATERIAL                                                  593
C    *                                                                    593
C    Geant ATTRIBUTE : SFRA ===> SEEN=2 COLO=1 <===                       593
         CALL AGSRESET                                                    594
         AG_SEEN = 2                                                      594
         AG_COLO = 1                                                      594
         AG_TITLE = 'SFRA'                                                594
         AG_EXNAME = 'ATTRIBUTE'                                          594
         AG_PARLIST = 'SEEN_COLO'                                         594
         CALL AXATTRIBUTE                                                 594
C    *                                                                    594
C    Geant SHAPE : BOX ===> DX=SFJP_SFRA_DX DY=SFJP_SFRA_DY, DZ=SFJP_SFR  594
C    A_DZ <===                                                            594
         CALL AGSRESET                                                    596
         AG_DX = SFJP_SFRA_DX                                             596
         AG_DY = SFJP_SFRA_DY                                             596
         AG_DZ = SFJP_SFRA_DZ                                             596
         AG_TITLE = 'BOX'                                                 596
         AG_EXNAME = 'SHAPE'                                              596
         AG_PARLIST = 'DX_DY_DZ'                                          596
         CALL AXSHAPE                                                     596
C    *                                                                    596
5321  CALL AGSPOPB                                                        597
      GO TO BLOCK_SFRA, (5320 ,5330)                                      597
C End of Local Procedure                                                  597
C ----------------------------------------------------------------------  598
C ------------ SFRS                                                       598
C Begin Local Procedure SFRS                                              599
C SFRS TWO SUPPORTS OF THE HYBRID STIFFNEER (PIECE OF IT)                 599
5340  CONTINUE                                                            599
         AG_TITLE='SFRS'                                                  599
         AG_MODULE=MODULE                                                 599
         AG_MARK='V00'                                                    599
         CALL AGSPUSH                                                     599
         AG_MODULE=MODULE                                                 599
C    *                                                                    599
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           600
         CALL AGSRESET                                                    600
         AG_TITLE = 'CARBON'                                              600
         AG_EXNAME = 'MATERIAL'                                           600
         AG_PARLIST = 'NONE'                                              600
         CALL AXMATERIAL                                                  600
C    *                                                                    600
C    Geant ATTRIBUTE : SFRS ===> SEEN=2 COLO=1 <===                       600
         CALL AGSRESET                                                    601
         AG_SEEN = 2                                                      601
         AG_COLO = 1                                                      601
         AG_TITLE = 'SFRS'                                                601
         AG_EXNAME = 'ATTRIBUTE'                                          601
         AG_PARLIST = 'SEEN_COLO'                                         601
         CALL AXATTRIBUTE                                                 601
C    *                                                                    601
C    Geant SHAPE : BOX ===> DX=SFJP_SFRS_DX DY=SFJP_SFRS_DY, DZ=SFJP_SFR  601
C    A_DZ <===                                                            601
         CALL AGSRESET                                                    603
         AG_DX = SFJP_SFRS_DX                                             603
         AG_DY = SFJP_SFRS_DY                                             603
         AG_DZ = SFJP_SFRA_DZ                                             603
         AG_TITLE = 'BOX'                                                 603
         AG_EXNAME = 'SHAPE'                                              603
         AG_PARLIST = 'DX_DY_DZ'                                          603
         CALL AXSHAPE                                                     603
C    *                                                                    603
5341  CALL AGSPOPB                                                        604
      GO TO BLOCK_SFRS, (5340 ,5350)                                      604
C End of Local Procedure                                                  604
C ----------------------------------------------------------------------  605
C ------------ SFFX                                                       605
C Begin Local Procedure SFFX                                              606
C SFFX IS THE FLEX                                                        606
5360  CONTINUE                                                            606
         AG_TITLE='SFFX'                                                  606
         AG_MODULE=MODULE                                                 606
         AG_MARK='V00'                                                    606
         CALL AGSPUSH                                                     606
         AG_MODULE=MODULE                                                 606
C    *                                                                    606
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           607
         CALL AGSRESET                                                    607
         AG_TITLE = 'COPPER'                                              607
         AG_EXNAME = 'MATERIAL'                                           607
         AG_PARLIST = 'NONE'                                              607
         CALL AXMATERIAL                                                  607
C    *                                                                    607
C    Geant ATTRIBUTE : SFFX ===> SEEN=1 COLO=42 <===                      607
         CALL AGSRESET                                                    608
         AG_SEEN = 1                                                      608
         AG_COLO = 42                                                     608
         AG_TITLE = 'SFFX'                                                608
         AG_EXNAME = 'ATTRIBUTE'                                          608
         AG_PARLIST = 'SEEN_COLO'                                         608
         CALL AXATTRIBUTE                                                 608
C    *                                                                    608
C    Geant SHAPE : BOX ===> DX=SFJP_SFFX_DX DY=SFJP_SFFX_DYZ, DZ=SFJP_SF  608
C    RA_DZ <===                                                           608
         CALL AGSRESET                                                    610
         AG_DX = SFJP_SFFX_DX                                             610
         AG_DY = SFJP_SFFX_DYZ                                            610
         AG_DZ = SFJP_SFRA_DZ                                             610
         AG_TITLE = 'BOX'                                                 610
         AG_EXNAME = 'SHAPE'                                              610
         AG_PARLIST = 'DX_DY_DZ'                                          610
         CALL AXSHAPE                                                     610
C    *                                                                    610
5361  CALL AGSPOPB                                                        611
      GO TO BLOCK_SFFX, (5360 ,5370)                                      611
C End of Local Procedure                                                  611
C ----------------------------------------------------------------------  612
C ------------ SFAA                                                       612
C Begin Local Procedure SFAA                                              613
C SFAA IS THE A128C CHIP                                                  613
5400  CONTINUE                                                            613
         AG_TITLE='SFAA'                                                  613
         AG_MODULE=MODULE                                                 613
         AG_MARK='V00'                                                    613
         CALL AGSPUSH                                                     613
         AG_MODULE=MODULE                                                 613
C    *                                                                    613
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          614
         CALL AGSRESET                                                    614
         AG_TITLE = 'SILICON'                                             614
         AG_EXNAME = 'MATERIAL'                                           614
         AG_PARLIST = 'NONE'                                              614
         CALL AXMATERIAL                                                  614
C    *                                                                    614
C    Geant ATTRIBUTE : SFAA ===> SEEN=1 COLO=41 <===                      614
         CALL AGSRESET                                                    615
         AG_SEEN = 1                                                      615
         AG_COLO = 41                                                     615
         AG_TITLE = 'SFAA'                                                615
         AG_EXNAME = 'ATTRIBUTE'                                          615
         AG_PARLIST = 'SEEN_COLO'                                         615
         CALL AXATTRIBUTE                                                 615
C    *                                                                    615
C    Geant SHAPE : BOX ===> DX=SFJP_SFAA_DX DY=SFJP_SFAA_DY, DZ=SFJP_SFA  615
C    A_DZ <===                                                            615
         CALL AGSRESET                                                    617
         AG_DX = SFJP_SFAA_DX                                             617
         AG_DY = SFJP_SFAA_DY                                             617
         AG_DZ = SFJP_SFAA_DZ                                             617
         AG_TITLE = 'BOX'                                                 617
         AG_EXNAME = 'SHAPE'                                              617
         AG_PARLIST = 'DX_DY_DZ'                                          617
         CALL AXSHAPE                                                     617
C    *                                                                    617
5401  CALL AGSPOPB                                                        618
      GO TO BLOCK_SFAA, (5400 ,5410)                                      618
C End of Local Procedure                                                  618
C ----------------------------------------------------------------------  619
C ------------ SFPI                                                       619
C Begin Local Procedure SFPI                                              620
C SFPI ARE THE PIONS                                                      620
5380  CONTINUE                                                            620
         AG_TITLE='SFPI'                                                  620
         AG_MODULE=MODULE                                                 620
         AG_MARK='V00'                                                    620
         CALL AGSPUSH                                                     620
         AG_MODULE=MODULE                                                 620
C    *                                                                    620
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        621
         CALL AGSRESET                                                    621
         AG_TITLE = 'ALUMINIUM'                                           621
         AG_EXNAME = 'MATERIAL'                                           621
         AG_PARLIST = 'NONE'                                              621
         CALL AXMATERIAL                                                  621
C    *                                                                    621
C    Geant ATTRIBUTE : SFPI ===> SEEN=2 COLO=33 <===                      621
         CALL AGSRESET                                                    622
         AG_SEEN = 2                                                      622
         AG_COLO = 33                                                     622
         AG_TITLE = 'SFPI'                                                622
         AG_EXNAME = 'ATTRIBUTE'                                          622
         AG_PARLIST = 'SEEN_COLO'                                         622
         CALL AXATTRIBUTE                                                 622
C    *                                                                    622
C    Geant SHAPE : TUBE ===> RMIN=SFJP_SFPI_RMIN/2. RMAX=SFJP_SFPI_RMAX/  622
C    2., DZ=SFJP_SFPI_PZ/2.0 <===                                         622
         CALL AGSRESET                                                    624
         AG_RMIN = SFJP_SFPI_RMIN/2.                                      624
         AG_RMAX = SFJP_SFPI_RMAX/2.                                      624
         AG_DZ = SFJP_SFPI_PZ/2.0                                         624
         AG_TITLE = 'TUBE'                                                624
         AG_EXNAME = 'SHAPE'                                              624
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      624
         CALL AXSHAPE                                                     624
C    *                                                                    624
C    Create SFPJ :                                                        625
C    Call To Local Procedure SFPJ                                         625
         ASSIGN 5430 TO BLOCK_SFPJ                                        625
         GO TO  5420                                                      625
5430     CONTINUE                                                         625
C    *                                                                    625
C    Geant POSITION : SFPJ ===> X=0. Y=0. Z=-SFJP_SFPI_PZ/2.0+SFJP_SFPJ_  626
C    DZ <===                                                              626
         CALL AGSRESET                                                    626
         AG_X = 0.                                                        626
         AG_Y = 0.                                                        626
         AG_Z = -SFJP_SFPI_PZ/2.0+SFJP_SFPJ_DZ                            626
         AG_TITLE = 'SFPJ'                                                626
         AG_EXNAME = 'POSITION'                                           626
         AG_PARLIST = 'X_Y_Z'                                             626
         CALL AXPOSITION                                                  626
C    *                                                                    626
5381  CALL AGSPOPB                                                        627
      GO TO BLOCK_SFPI, (5380 ,5390)                                      627
C End of Local Procedure                                                  627
C ----------------------------------------------------------------------  628
C ------------ SFPJ                                                       628
C Begin Local Procedure SFPJ                                              629
C SFPJ IS THE BASE OF THE PIONS                                           629
5420  CONTINUE                                                            629
         AG_TITLE='SFPJ'                                                  629
         AG_MODULE=MODULE                                                 629
         AG_MARK='V00'                                                    629
         CALL AGSPUSH                                                     629
         AG_MODULE=MODULE                                                 629
C    *                                                                    629
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        630
         CALL AGSRESET                                                    630
         AG_TITLE = 'ALUMINIUM'                                           630
         AG_EXNAME = 'MATERIAL'                                           630
         AG_PARLIST = 'NONE'                                              630
         CALL AXMATERIAL                                                  630
C    *                                                                    630
C    Geant ATTRIBUTE : SFPJ ===> SEEN=2 COLO=33 <===                      630
         CALL AGSRESET                                                    631
         AG_SEEN = 2                                                      631
         AG_COLO = 33                                                     631
         AG_TITLE = 'SFPJ'                                                631
         AG_EXNAME = 'ATTRIBUTE'                                          631
         AG_PARLIST = 'SEEN_COLO'                                         631
         CALL AXATTRIBUTE                                                 631
C    *                                                                    631
C    Geant SHAPE : TUBE ===> RMIN=SFJP_SFPJ_DX1+SFJP_SFPJ_DX2, RMAX=SFJP  631
C    _SFPJ_DY DZ=SFJP_SFPJ_DZ <===                                        631
         CALL AGSRESET                                                    633
         AG_RMIN = SFJP_SFPJ_DX1+SFJP_SFPJ_DX2                            633
         AG_RMAX = SFJP_SFPJ_DY                                           633
         AG_DZ = SFJP_SFPJ_DZ                                             633
         AG_TITLE = 'TUBE'                                                633
         AG_EXNAME = 'SHAPE'                                              633
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      633
         CALL AXSHAPE                                                     633
C    *                                                                    633
5421  CALL AGSPOPB                                                        634
      GO TO BLOCK_SFPJ, (5420 ,5430)                                      634
C End of Local Procedure                                                  634
C ----------------------------------------------------------------------  635
C ------------ SFSL                                                       635
C Begin Local Procedure SFSL                                              636
C SFSL IS THE SILICON OF THE STRIP DETECTOR                               636
5300  CONTINUE                                                            636
         AG_TITLE='SFSL'                                                  636
         AG_MODULE=MODULE                                                 636
         AG_MARK='V00'                                                    636
         CALL AGSPUSH                                                     636
         AG_MODULE=MODULE                                                 636
C    *                                                                    636
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          637
         CALL AGSRESET                                                    637
         AG_TITLE = 'SILICON'                                             637
         AG_EXNAME = 'MATERIAL'                                           637
         AG_PARLIST = 'NONE'                                              637
         CALL AXMATERIAL                                                  637
C    *                                                                    637
C    Geant ATTRIBUTE : SFSL ===> SEEN=2 COLO=1 <===                       637
         CALL AGSRESET                                                    638
         AG_SEEN = 2                                                      638
         AG_COLO = 1                                                      638
         AG_TITLE = 'SFSL'                                                638
         AG_EXNAME = 'ATTRIBUTE'                                          638
         AG_PARLIST = 'SEEN_COLO'                                         638
         CALL AXATTRIBUTE                                                 638
C    *                                                                    638
C    Geant SHAPE : BOX ===> DX=SFJP_SFSL_DX DY=SFJP_SFSL_DY, DZ=SFJP_SFS  638
C    L_DZ <===                                                            638
         CALL AGSRESET                                                    640
         AG_DX = SFJP_SFSL_DX                                             640
         AG_DY = SFJP_SFSL_DY                                             640
         AG_DZ = SFJP_SFSL_DZ                                             640
         AG_TITLE = 'BOX'                                                 640
         AG_EXNAME = 'SHAPE'                                              640
         AG_PARLIST = 'DX_DY_DZ'                                          640
         CALL AXSHAPE                                                     640
C    *                                                                    640
C    Create SFSD :                                                        641
C    Call To Local Procedure SFSD                                         641
         ASSIGN 5450 TO BLOCK_SFSD                                        641
         GO TO  5440                                                      641
5450     CONTINUE                                                         641
C    *                                                                    641
C    Geant POSITION : SFSD ===> inherited arguments used <===             642
         CALL AGSRESET                                                    642
         AG_TITLE = 'SFSD'                                                642
         AG_EXNAME = 'POSITION'                                           642
         AG_PARLIST = 'NONE'                                              642
         CALL AXPOSITION                                                  642
C    *                                                                    642
5301  CALL AGSPOPB                                                        643
      GO TO BLOCK_SFSL, (5300 ,5310)                                      643
C End of Local Procedure                                                  643
C ----------------------------------------------------------------------  644
C ------------ SFSD                                                       644
C Begin Local Procedure SFSD                                              645
C SFSD IS THE STRIP DETECTOR                                              645
5440  CONTINUE                                                            645
         AG_TITLE='SFSD'                                                  645
         AG_MODULE=MODULE                                                 645
         AG_MARK='V00'                                                    645
         CALL AGSPUSH                                                     645
         AG_MODULE=MODULE                                                 645
C    *                                                                    645
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          646
         CALL AGSRESET                                                    646
         AG_TITLE = 'SILICON'                                             646
         AG_EXNAME = 'MATERIAL'                                           646
         AG_PARLIST = 'NONE'                                              646
         CALL AXMATERIAL                                                  646
C    *                                                                    646
C    Geant MATERIAL : SENSITIVE ===> ISVOL=1 <===                         646
         CALL AGSRESET                                                    647
         AG_ISVOL = 1                                                     647
         AG_TITLE = 'SENSITIVE'                                           647
         AG_EXNAME = 'MATERIAL'                                           647
         AG_PARLIST = 'ISVOL'                                             647
         CALL AXMATERIAL                                                  647
C    *                                                                    647
C    Geant ATTRIBUTE : SFSD ===> SEEN=2 COLO=41 <===                      647
         CALL AGSRESET                                                    648
         AG_SEEN = 2                                                      648
         AG_COLO = 41                                                     648
         AG_TITLE = 'SFSD'                                                648
         AG_EXNAME = 'ATTRIBUTE'                                          648
         AG_PARLIST = 'SEEN_COLO'                                         648
         CALL AXATTRIBUTE                                                 648
C    *                                                                    648
C    Geant SHAPE : BOX ===> DX=SFJP_SFSD_DX DY=SFJP_SFSD_DY, DZ=SFJP_SFS  648
C    D_DZ <===                                                            648
         CALL AGSRESET                                                    650
         AG_DX = SFJP_SFSD_DX                                             650
         AG_DY = SFJP_SFSD_DY                                             650
         AG_DZ = SFJP_SFSD_DZ                                             650
         AG_TITLE = 'BOX'                                                 650
         AG_EXNAME = 'SHAPE'                                              650
         AG_PARLIST = 'DX_DY_DZ'                                          650
         CALL AXSHAPE                                                     650
C    *                                                                    650
         CALL GSTPAR( AG_IMED,'STRA',1. )                                 650
C    Defining Hits: X:0.001:S Y:0.001: Z:0.001: CX:10: CY:10: CZ:10: STE  653
C    P:0.01: SLENG:16:(0,500), TOF:16:(0,1.0E-6) PTOT:16:(0,100.0) ELOSS  653
C    :16:(0,0.01)                                                         653
         AG_TITLE = 'SFSD'                                                653
         AG_EXNAME = 'HITS'                                               653
         AG_TYPE=AG_MAGIC                                                 653
         CALL AGSDETV('SFSD')                                             653
C    *                                                                    653
         AG_PARLIST='X'                                                   653
         AG_TITLE = 'DETH'                                                653
         AG_NBITS = AG_MAGIC                                              653
         AG_BIN=0.001                                                     653
         AG_OPTION='S'                                                    653
         AG_FHMIN = AG_MAGIC                                              653
         AG_FHMAX = AG_MAGIC                                              653
         CALL AGSDETH (0)                                                 653
C    *                                                                    653
         AG_PARLIST='Y'                                                   653
         AG_TITLE = 'DETH'                                                653
         AG_NBITS = AG_MAGIC                                              653
         AG_BIN=0.001                                                     653
         AG_FHMIN=AG_MAGIC                                                653
         AG_FHMAX=AG_MAGIC                                                653
         AG_OPTION='*'                                                    653
         CALL AGSDETH (0)                                                 653
C    *                                                                    653
         AG_PARLIST='Z'                                                   653
         AG_TITLE = 'DETH'                                                653
         AG_NBITS = AG_MAGIC                                              653
         AG_BIN=0.001                                                     653
         AG_FHMIN=AG_MAGIC                                                653
         AG_FHMAX=AG_MAGIC                                                653
         AG_OPTION='*'                                                    653
         CALL AGSDETH (0)                                                 653
C    *                                                                    653
         AG_PARLIST='CX'                                                  653
         AG_TITLE = 'DETH'                                                653
         AG_NBITS = 10                                                    653
         AG_BIN=AG_MAGIC                                                  653
         AG_FHMIN=AG_MAGIC                                                653
         AG_FHMAX=AG_MAGIC                                                653
         AG_OPTION='*'                                                    653
         CALL AGSDETH (0)                                                 653
C    *                                                                    653
         AG_PARLIST='CY'                                                  653
         AG_TITLE = 'DETH'                                                653
         AG_NBITS = 10                                                    653
         AG_BIN=AG_MAGIC                                                  653
         AG_FHMIN=AG_MAGIC                                                653
         AG_FHMAX=AG_MAGIC                                                653
         AG_OPTION='*'                                                    653
         CALL AGSDETH (0)                                                 653
C    *                                                                    653
         AG_PARLIST='CZ'                                                  653
         AG_TITLE = 'DETH'                                                653
         AG_NBITS = 10                                                    653
         AG_BIN=AG_MAGIC                                                  653
         AG_FHMIN=AG_MAGIC                                                653
         AG_FHMAX=AG_MAGIC                                                653
         AG_OPTION='*'                                                    653
         CALL AGSDETH (0)                                                 653
C    *                                                                    653
         AG_PARLIST='STEP'                                                653
         AG_TITLE = 'DETH'                                                653
         AG_NBITS = AG_MAGIC                                              653
         AG_BIN=0.01                                                      653
         AG_FHMIN=AG_MAGIC                                                653
         AG_FHMAX=AG_MAGIC                                                653
         AG_OPTION='*'                                                    653
         CALL AGSDETH (0)                                                 653
C    *                                                                    653
         AG_PARLIST='SLENG'                                               653
         AG_TITLE = 'DETH'                                                653
         AG_NBITS = 16                                                    653
         AG_BIN=AG_MAGIC                                                  653
         AG_OPTION='*'                                                    653
         AG_FHMIN = 0                                                     653
         AG_FHMAX = 500                                                   653
         CALL AGSDETH (0)                                                 653
C    *                                                                    653
         AG_PARLIST='TOF'                                                 653
         AG_TITLE = 'DETH'                                                653
         AG_NBITS = 16                                                    653
         AG_BIN=AG_MAGIC                                                  653
         AG_OPTION='*'                                                    653
         AG_FHMIN = 0                                                     653
         AG_FHMAX = 1.0E-6                                                653
         CALL AGSDETH (0)                                                 653
C    *                                                                    653
         AG_PARLIST='PTOT'                                                653
         AG_TITLE = 'DETH'                                                653
         AG_NBITS = 16                                                    653
         AG_BIN=AG_MAGIC                                                  653
         AG_OPTION='*'                                                    653
         AG_FHMIN = 0                                                     653
         AG_FHMAX = 100.0                                                 653
         CALL AGSDETH (0)                                                 653
C    *                                                                    653
         AG_PARLIST='ELOSS'                                               653
         AG_TITLE = 'DETH'                                                653
         AG_NBITS = 16                                                    653
         AG_BIN=AG_MAGIC                                                  653
         AG_OPTION='*'                                                    653
         AG_FHMIN = 0                                                     653
         AG_FHMAX = 0.01                                                  653
         CALL AGSDETH (0)                                                 653
C    *                                                                    653
         CALL AGSDETU('SFSD')                                             653
C    *                                                                    653
5441  CALL AGSPOPB                                                        654
      GO TO BLOCK_SFSD, (5440 ,5450)                                      654
C End of Local Procedure                                                  654
C ----------------------------------------------------------------------  655
C ------------ SFSM                                                       655
C Begin Local Procedure SFSM                                              656
C SFSM IS THE MOTHER VOLUME OF THE LADDER (MECHANIC STRUCTURE)            656
5250  CONTINUE                                                            656
         AG_TITLE='SFSM'                                                  656
         AG_MODULE=MODULE                                                 656
         AG_MARK='V00'                                                    656
         CALL AGSPUSH                                                     656
         AG_MODULE=MODULE                                                 656
C    *                                                                    656
C    Geant MATERIAL : AIR ===> inherited arguments used <===              657
         CALL AGSRESET                                                    657
         AG_TITLE = 'AIR'                                                 657
         AG_EXNAME = 'MATERIAL'                                           657
         AG_PARLIST = 'NONE'                                              657
         CALL AXMATERIAL                                                  657
C    *                                                                    657
C    Geant ATTRIBUTE : SFSM ===> SEEN=0 COLO=6 <===                       657
         CALL AGSRESET                                                    658
         AG_SEEN = 0                                                      658
         AG_COLO = 6                                                      658
         AG_TITLE = 'SFSM'                                                658
         AG_EXNAME = 'ATTRIBUTE'                                          658
         AG_PARLIST = 'SEEN_COLO'                                         658
         CALL AXATTRIBUTE                                                 658
C    *                                                                    658
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=SFPA_SMTHK/2, DZ=SFPA_SML  658
C    EN/2. <===                                                           658
         CALL AGSRESET                                                    660
         AG_DX = SFPA_DMWID/2                                             660
         AG_DY = SFPA_SMTHK/2                                             660
         AG_DZ = SFPA_SMLEN/2.                                            660
         AG_TITLE = 'BOX'                                                 660
         AG_EXNAME = 'SHAPE'                                              660
         AG_PARLIST = 'DX_DY_DZ'                                          660
         CALL AXSHAPE                                                     660
C    *                                                                    660
         YOFFSET = - 1.7                                                  660
C    Create SFLT :                                                        662
C    Call To Local Procedure SFLT                                         662
         ASSIGN 5470 TO BLOCK_SFLT                                        662
         GO TO  5460                                                      662
5470     CONTINUE                                                         662
C    *                                                                    662
         HIGHT = SFJP_SFSM_LL*TAN(54*PI/180) - SFPB_HHIGHT/TAN(PI/5) -    663
     *   0.02                                                             663
C    Geant POSITION : SFLT ===> X=-(SFPB_HHIGHT-SFPB_KHIGHT) Y=HIGHT+YOF  664
C    FSET Z=0.0 ALPHAZ=-90-36 <===                                        664
         CALL AGSRESET                                                    664
         AG_X = -(SFPB_HHIGHT-SFPB_KHIGHT)                                664
         AG_Y = HIGHT+YOFFSET                                             664
         AG_Z = 0.0                                                       664
         AG_ALPHAN=AG_ALPHAN+1                                            664
         AG_ALPHAK(AG_ALPHAN)=3                                           664
         AG_ALPHAL(AG_ALPHAN)=-90-36                                      664
         AG_TITLE = 'SFLT'                                                664
         AG_EXNAME = 'POSITION'                                           664
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      664
         CALL AXPOSITION                                                  664
C    *                                                                    664
C    Geant POSITION : SFLT ===> X=(SFPB_HHIGHT-SFPB_KHIGHT) Y=HIGHT+YOFF  664
C    SET Z=0 ALPHAY=180. ALPHAZ=90.+36. <===                              664
         CALL AGSRESET                                                    666
         AG_X = (SFPB_HHIGHT-SFPB_KHIGHT)                                 666
         AG_Y = HIGHT+YOFFSET                                             666
         AG_Z = 0                                                         666
         AG_ALPHAN=AG_ALPHAN+1                                            666
         AG_ALPHAK(AG_ALPHAN)=2                                           666
         AG_ALPHAL(AG_ALPHAN)=180.                                        666
         AG_ALPHAN=AG_ALPHAN+1                                            666
         AG_ALPHAK(AG_ALPHAN)=3                                           666
         AG_ALPHAL(AG_ALPHAN)=90.+36.                                     666
         AG_TITLE = 'SFLT'                                                666
         AG_EXNAME = 'POSITION'                                           666
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               666
         CALL AXPOSITION                                                  666
C    *                                                                    666
C    Create SFLU :                                                        667
C    Call To Local Procedure SFLU                                         667
         ASSIGN 5490 TO BLOCK_SFLU                                        667
         GO TO  5480                                                      667
5490     CONTINUE                                                         667
C    *                                                                    667
C    Geant POSITION : SFLU ===> X=0.-(SFJP_SFSM_LL-(SFJP_SFLU_BL1+SFJP_S  669
C    FLU_H1/2*1/TAN(PI/5))) Y=0.+YOFFSET Z=0. <===                        669
         CALL AGSRESET                                                    669
         AG_X = 0.-(SFJP_SFSM_LL-(SFJP_SFLU_BL1+SFJP_SFLU_H1/2*1/TAN(PI/  669
     *   5)))                                                             669
         AG_Y = 0.+YOFFSET                                                669
         AG_Z = 0.                                                        669
         AG_TITLE = 'SFLU'                                                669
         AG_EXNAME = 'POSITION'                                           669
         AG_PARLIST = 'X_Y_Z'                                             669
         CALL AXPOSITION                                                  669
C    *                                                                    669
         ESSAI = 180                                                      669
         ESSAI = ESSAI + 27 + 27                                          671
C    Geant POSITION : SFLU ===> X=-COS(63*PI/180)*2*(SFPB_HBASE-SFPB_KBA  674
C    SE)-(SFJP_SFSM_LL-(0.2+0.01/TAN(PI/5))) Y=COS(27*PI/180)*2*(SFPB_HB  674
C    ASE-SFPB_KBASE)+YOFFSET Z=0 ALPHAY=180 ALPHAZ=ESSAI <===             674
         CALL AGSRESET                                                    674
         AG_X = -COS(63*PI/                                               674
     *   180)*2*(SFPB_HBASE-SFPB_KBASE)-(SFJP_SFSM_LL-(0.2+0.01/TAN(PI/   674
     *   5)))                                                             674
         AG_Y = COS(27*PI/180)*2*(SFPB_HBASE-SFPB_KBASE)+YOFFSET          674
         AG_Z = 0                                                         674
         AG_ALPHAN=AG_ALPHAN+1                                            674
         AG_ALPHAK(AG_ALPHAN)=2                                           674
         AG_ALPHAL(AG_ALPHAN)=180                                         674
         AG_ALPHAN=AG_ALPHAN+1                                            674
         AG_ALPHAK(AG_ALPHAN)=3                                           674
         AG_ALPHAL(AG_ALPHAN)=ESSAI                                       674
         AG_TITLE = 'SFLU'                                                674
         AG_EXNAME = 'POSITION'                                           674
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               674
         CALL AXPOSITION                                                  674
C    *                                                                    674
C    Geant POSITION : SFLU ===> X=0.+(SFJP_SFSM_LL-(0.2+0.01/TAN(PI/5)))  674
C     Y=0.+YOFFSET Z=0. ALPHAY=180. <===                                  674
         CALL AGSRESET                                                    676
         AG_X = 0.+(SFJP_SFSM_LL-(0.2+0.01/TAN(PI/5)))                    676
         AG_Y = 0.+YOFFSET                                                676
         AG_Z = 0.                                                        676
         AG_ALPHAN=AG_ALPHAN+1                                            676
         AG_ALPHAK(AG_ALPHAN)=2                                           676
         AG_ALPHAL(AG_ALPHAN)=180.                                        676
         AG_TITLE = 'SFLU'                                                676
         AG_EXNAME = 'POSITION'                                           676
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      676
         CALL AXPOSITION                                                  676
C    *                                                                    676
C    Geant POSITION : SFLU ===> X=COS(63*PI/180)*2*(SFPB_HBASE-SFPB_KBAS  676
C    E)+(SFJP_SFSM_LL-(0.2+0.01/TAN(PI/5))) Y=COS(27*PI/180)*2*(SFPB_HBA  676
C    SE-SFPB_KBASE)+YOFFSET Z=0 ALPHAZ=-ESSAI <===                        676
         CALL AGSRESET                                                    679
         AG_X = COS(63*PI/                                                679
     *   180)*2*(SFPB_HBASE-SFPB_KBASE)+(SFJP_SFSM_LL-(0.2+0.01/TAN(PI/   679
     *   5)))                                                             679
         AG_Y = COS(27*PI/180)*2*(SFPB_HBASE-SFPB_KBASE)+YOFFSET          679
         AG_Z = 0                                                         679
         AG_ALPHAN=AG_ALPHAN+1                                            679
         AG_ALPHAK(AG_ALPHAN)=3                                           679
         AG_ALPHAL(AG_ALPHAN)=-ESSAI                                      679
         AG_TITLE = 'SFLU'                                                679
         AG_EXNAME = 'POSITION'                                           679
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      679
         CALL AXPOSITION                                                  679
C    *                                                                    679
C    Create SFFK :                                                        680
C    Call To Local Procedure SFFK                                         680
         ASSIGN 5510 TO BLOCK_SFFK                                        680
         GO TO  5500                                                      680
5510     CONTINUE                                                         680
C    *                                                                    680
C    Geant POSITION : SFFK ===> X=SFJP_SFFK_PX/2+SFJP_SFFK_DXE Y=-SFJP_S  682
C    FFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY+YOFFSET Z=0.0 <===                682
         CALL AGSRESET                                                    682
         AG_X = SFJP_SFFK_PX/2+SFJP_SFFK_DXE                              682
         AG_Y = -SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY+YOFFSET         682
         AG_Z = 0.0                                                       682
         AG_TITLE = 'SFFK'                                                682
         AG_EXNAME = 'POSITION'                                           682
         AG_PARLIST = 'X_Y_Z'                                             682
         CALL AXPOSITION                                                  682
C    *                                                                    682
C    Geant POSITION : SFFK ===> X=-SFJP_SFFK_PX/2-SFJP_SFFK_DXE Y=-SFJP_  682
C    SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY+YOFFSET Z=0.0 <===               682
         CALL AGSRESET                                                    684
         AG_X = -SFJP_SFFK_PX/2-SFJP_SFFK_DXE                             684
         AG_Y = -SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY+YOFFSET         684
         AG_Z = 0.0                                                       684
         AG_TITLE = 'SFFK'                                                684
         AG_EXNAME = 'POSITION'                                           684
         AG_PARLIST = 'X_Y_Z'                                             684
         CALL AXPOSITION                                                  684
C    *                                                                    684
C    Create SFFL :                                                        685
C    Call To Local Procedure SFFL                                         685
         ASSIGN 5530 TO BLOCK_SFFL                                        685
         GO TO  5520                                                      685
5530     CONTINUE                                                         685
C    *                                                                    685
C    Geant POSITION : SFFL ===> X=-SFJP_SFKL_PX-SFPB_FSIZE*COS(15*PI/180  688
C    ) Y=-SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(75*PI/  688
C    180)+YOFFSET Z=0 ALPHAZ=15 <===                                      688
         CALL AGSRESET                                                    688
         AG_X = -SFJP_SFKL_PX-SFPB_FSIZE*COS(15*PI/180)                   688
         AG_Y =                                                           688
     *   -SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(75*PI/  688
     *   180)+YOFFSET                                                     688
         AG_Z = 0                                                         688
         AG_ALPHAN=AG_ALPHAN+1                                            688
         AG_ALPHAK(AG_ALPHAN)=3                                           688
         AG_ALPHAL(AG_ALPHAN)=15                                          688
         AG_TITLE = 'SFFL'                                                688
         AG_EXNAME = 'POSITION'                                           688
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      688
         CALL AXPOSITION                                                  688
C    *                                                                    688
C    Geant POSITION : SFFL ===> X=SFJP_SFKL_PX+SFPB_FSIZE*COS(15*PI/180)  688
C     Y=-SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(75*PI/1  688
C    80)+YOFFSET Z=0 ALPHAZ=-15 <===                                      688
         CALL AGSRESET                                                    691
         AG_X = SFJP_SFKL_PX+SFPB_FSIZE*COS(15*PI/180)                    691
         AG_Y =                                                           691
     *   -SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(75*PI/  691
     *   180)+YOFFSET                                                     691
         AG_Z = 0                                                         691
         AG_ALPHAN=AG_ALPHAN+1                                            691
         AG_ALPHAK(AG_ALPHAN)=3                                           691
         AG_ALPHAL(AG_ALPHAN)=-15                                         691
         AG_TITLE = 'SFFL'                                                691
         AG_EXNAME = 'POSITION'                                           691
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      691
         CALL AXPOSITION                                                  691
C    *                                                                    691
C    Create SFKK :                                                        692
C    Call To Local Procedure SFKK                                         692
         ASSIGN 5550 TO BLOCK_SFKK                                        692
         GO TO  5540                                                      692
5550     CONTINUE                                                         692
C    *                                                                    692
C    Geant POSITION : SFKK ===> X=0. Y=-SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP  694
C    _SFFK_PY2+YOFFSET-SFJP_SFKK_DY/2. Z=0. <===                          694
         CALL AGSRESET                                                    694
         AG_X = 0.                                                        694
         AG_Y =                                                           694
     *   -SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_PY2+YOFFSET-              694
     *   SFJP_SFKK_DY/2.                                                  694
         AG_Z = 0.                                                        694
         AG_TITLE = 'SFKK'                                                694
         AG_EXNAME = 'POSITION'                                           694
         AG_PARLIST = 'X_Y_Z'                                             694
         CALL AXPOSITION                                                  694
C    *                                                                    694
C    Create SFKL :                                                        695
C    Call To Local Procedure SFKL                                         695
         ASSIGN 5570 TO BLOCK_SFKL                                        695
         GO TO  5560                                                      695
5570     CONTINUE                                                         695
C    *                                                                    695
C    Geant POSITION : SFKL ===> X=-SFJP_SFKL_PX-SFPB_FSIZE*COS(15*PI/180  698
C    )+0.005 Y=-SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(  698
C    75*PI/180)+YOFFSET-0.021 Z=0 ALPHAZ=15 <===                          698
         CALL AGSRESET                                                    698
         AG_X = -SFJP_SFKL_PX-SFPB_FSIZE*COS(15*PI/180)+0.005             698
         AG_Y =                                                           698
     *   -SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(75*PI/  698
     *   180)+YOFFSET-0.021                                               698
         AG_Z = 0                                                         698
         AG_ALPHAN=AG_ALPHAN+1                                            698
         AG_ALPHAK(AG_ALPHAN)=3                                           698
         AG_ALPHAL(AG_ALPHAN)=15                                          698
         AG_TITLE = 'SFKL'                                                698
         AG_EXNAME = 'POSITION'                                           698
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      698
         CALL AXPOSITION                                                  698
C    *                                                                    698
C    Geant POSITION : SFKL ===> X=+SFJP_SFKL_PX+SFPB_FSIZE*COS(15*PI/180  698
C    )-0.005 Y=-SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(  698
C    75*PI/180)+YOFFSET-0.021 Z=0 ALPHAZ=-15 <===                         698
         CALL AGSRESET                                                    701
         AG_X = +SFJP_SFKL_PX+SFPB_FSIZE*COS(15*PI/180)-0.005             701
         AG_Y =                                                           701
     *   -SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(75*PI/  701
     *   180)+YOFFSET-0.021                                               701
         AG_Z = 0                                                         701
         AG_ALPHAN=AG_ALPHAN+1                                            701
         AG_ALPHAK(AG_ALPHAN)=3                                           701
         AG_ALPHAL(AG_ALPHAN)=-15                                         701
         AG_TITLE = 'SFKL'                                                701
         AG_EXNAME = 'POSITION'                                           701
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      701
         CALL AXPOSITION                                                  701
C    *                                                                    701
C    Create SAPP :                                                        702
C    Call To Local Procedure SAPP                                         702
         ASSIGN 5590 TO BLOCK_SAPP                                        702
         GO TO  5580                                                      702
5590     CONTINUE                                                         702
C    *                                                                    702
C    Geant POSITION : SAPP ===> X=0. Y=-SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJ  704
C    P_SAPP_PY3)/2.+YOFFSET Z=SFJP_SFFK_DZ/2+SFJP_SAPP_DZ/2. ALPHAY=180.  704
C     <===                                                                704
         CALL AGSRESET                                                    704
         AG_X = 0.                                                        704
         AG_Y = -SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJP_SAPP_PY3)/2.+YOFFSET   704
         AG_Z = SFJP_SFFK_DZ/2+SFJP_SAPP_DZ/2.                            704
         AG_ALPHAN=AG_ALPHAN+1                                            704
         AG_ALPHAK(AG_ALPHAN)=2                                           704
         AG_ALPHAL(AG_ALPHAN)=180.                                        704
         AG_TITLE = 'SAPP'                                                704
         AG_EXNAME = 'POSITION'                                           704
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      704
         CALL AXPOSITION                                                  704
C    *                                                                    704
C    Geant POSITION : SAPP ===> X=0. Y=-SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJ  704
C    P_SAPP_PY3)/2.+YOFFSET Z=-(SFJP_SFFK_DZ/2+SFJP_SAPP_DZ/2.) <===      704
         CALL AGSRESET                                                    706
         AG_X = 0.                                                        706
         AG_Y = -SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJP_SAPP_PY3)/2.+YOFFSET   706
         AG_Z = -(SFJP_SFFK_DZ/2+SFJP_SAPP_DZ/2.)                         706
         AG_TITLE = 'SAPP'                                                706
         AG_EXNAME = 'POSITION'                                           706
         AG_PARLIST = 'X_Y_Z'                                             706
         CALL AXPOSITION                                                  706
C    *                                                                    706
C    Create SFAM :                                                        707
C    Call To Local Procedure SFAM                                         707
         ASSIGN 5610 TO BLOCK_SFAM                                        707
         GO TO  5600                                                      707
5610     CONTINUE                                                         707
C    *                                                                    707
C    Geant POSITION : SFAM ===> X=0. Y=-SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJ  709
C    P_SAPP_PY3)/2.+YOFFSET Z=-(SFJP_SFFK_DZ/2+SFJP_SAPP_DZ)-SFJP_SFAM_D  709
C    Z/2. <===                                                            709
         CALL AGSRESET                                                    709
         AG_X = 0.                                                        709
         AG_Y = -SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJP_SAPP_PY3)/2.+YOFFSET   709
         AG_Z = -(SFJP_SFFK_DZ/2+SFJP_SAPP_DZ)-SFJP_SFAM_DZ/2.            709
         AG_TITLE = 'SFAM'                                                709
         AG_EXNAME = 'POSITION'                                           709
         AG_PARLIST = 'X_Y_Z'                                             709
         CALL AXPOSITION                                                  709
C    *                                                                    709
C    Geant POSITION : SFAM ===> X=0. Y=-SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJ  709
C    P_SAPP_PY3)/2.+YOFFSET Z=+(SFJP_SFFK_DZ/2+SFJP_SAPP_DZ)+SFJP_SFAM_D  709
C    Z/2. ALPHAY=180. <===                                                709
         CALL AGSRESET                                                    711
         AG_X = 0.                                                        711
         AG_Y = -SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJP_SAPP_PY3)/2.+YOFFSET   711
         AG_Z = +(SFJP_SFFK_DZ/2+SFJP_SAPP_DZ)+SFJP_SFAM_DZ/2.            711
         AG_ALPHAN=AG_ALPHAN+1                                            711
         AG_ALPHAK(AG_ALPHAN)=2                                           711
         AG_ALPHAL(AG_ALPHAN)=180.                                        711
         AG_TITLE = 'SFAM'                                                711
         AG_EXNAME = 'POSITION'                                           711
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      711
         CALL AXPOSITION                                                  711
C    *                                                                    711
C    Create SFCO :                                                        712
C    Call To Local Procedure SFCO                                         712
         ASSIGN 5630 TO BLOCK_SFCO                                        712
         GO TO  5620                                                      712
5630     CONTINUE                                                         712
C    *                                                                    712
C    Geant POSITION : SFCO ===> X=-SFJP_SFCO_PX Y=SFJP_SFCO_PY Z=-(SFJP_  715
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFCO_DZ/2. ALPHAZ=+SFJP  715
C    _ALPHAZ <===                                                         715
         CALL AGSRESET                                                    715
         AG_X = -SFJP_SFCO_PX                                             715
         AG_Y = SFJP_SFCO_PY                                              715
         AG_Z = -(SFJP_SFFK_DZ/                                           715
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFCO_DZ/2.                   715
         AG_ALPHAN=AG_ALPHAN+1                                            715
         AG_ALPHAK(AG_ALPHAN)=3                                           715
         AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                                715
         AG_TITLE = 'SFCO'                                                715
         AG_EXNAME = 'POSITION'                                           715
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      715
         CALL AXPOSITION                                                  715
C    *                                                                    715
C    Geant POSITION : SFCO ===> X=+SFJP_SFCO_PX Y=SFJP_SFCO_PY Z=+(SFJP_  715
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFCO_DZ/2. ALPHAZ=-SFJP  715
C    _ALPHAZ <===                                                         715
         CALL AGSRESET                                                    718
         AG_X = +SFJP_SFCO_PX                                             718
         AG_Y = SFJP_SFCO_PY                                              718
         AG_Z = +(SFJP_SFFK_DZ/                                           718
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFCO_DZ/2.                   718
         AG_ALPHAN=AG_ALPHAN+1                                            718
         AG_ALPHAK(AG_ALPHAN)=3                                           718
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                                718
         AG_TITLE = 'SFCO'                                                718
         AG_EXNAME = 'POSITION'                                           718
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      718
         CALL AXPOSITION                                                  718
C    *                                                                    718
C    Create SFCM :                                                        719
C    Call To Local Procedure SFCM                                         719
         ASSIGN 5650 TO BLOCK_SFCM                                        719
         GO TO  5640                                                      719
5650     CONTINUE                                                         719
C    *                                                                    719
C    Geant POSITION : SFCM ===> X=+SFJP_SFCO_PX Y=SFJP_SFCO_PY Z=-(SFJP_  722
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFCO_DZ/2. ALPHAZ=-SFJP  722
C    _ALPHAZ <===                                                         722
         CALL AGSRESET                                                    722
         AG_X = +SFJP_SFCO_PX                                             722
         AG_Y = SFJP_SFCO_PY                                              722
         AG_Z = -(SFJP_SFFK_DZ/                                           722
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFCO_DZ/2.                   722
         AG_ALPHAN=AG_ALPHAN+1                                            722
         AG_ALPHAK(AG_ALPHAN)=3                                           722
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                                722
         AG_TITLE = 'SFCM'                                                722
         AG_EXNAME = 'POSITION'                                           722
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      722
         CALL AXPOSITION                                                  722
C    *                                                                    722
C    Geant POSITION : SFCM ===> X=-SFJP_SFCO_PX Y=SFJP_SFCO_PY Z=+(SFJP_  722
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFCO_DZ/2. ALPHAY=180 A  722
C    LPHAZ=+SFJP_ALPHAZ <===                                              722
         CALL AGSRESET                                                    725
         AG_X = -SFJP_SFCO_PX                                             725
         AG_Y = SFJP_SFCO_PY                                              725
         AG_Z = +(SFJP_SFFK_DZ/                                           725
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFCO_DZ/2.                   725
         AG_ALPHAN=AG_ALPHAN+1                                            725
         AG_ALPHAK(AG_ALPHAN)=2                                           725
         AG_ALPHAL(AG_ALPHAN)=180                                         725
         AG_ALPHAN=AG_ALPHAN+1                                            725
         AG_ALPHAK(AG_ALPHAN)=3                                           725
         AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                                725
         AG_TITLE = 'SFCM'                                                725
         AG_EXNAME = 'POSITION'                                           725
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               725
         CALL AXPOSITION                                                  725
C    *                                                                    725
C    Create SFKF :                                                        726
C    Call To Local Procedure SFKF                                         726
         ASSIGN 5670 TO BLOCK_SFKF                                        726
         GO TO  5660                                                      726
5670     CONTINUE                                                         726
C    *                                                                    726
C    Geant POSITION : SFKF ===> X=-SFJP_SFKF_PX Y=SFJP_SFKF_PY Z=-(SFJP_  729
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFKF_DZ/2-0.2 ALPHAZ=+S  729
C    FJP_ALPHAZ <===                                                      729
         CALL AGSRESET                                                    729
         AG_X = -SFJP_SFKF_PX                                             729
         AG_Y = SFJP_SFKF_PY                                              729
         AG_Z = -(SFJP_SFFK_DZ/                                           729
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFKF_DZ/2-0.2                729
         AG_ALPHAN=AG_ALPHAN+1                                            729
         AG_ALPHAK(AG_ALPHAN)=3                                           729
         AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                                729
         AG_TITLE = 'SFKF'                                                729
         AG_EXNAME = 'POSITION'                                           729
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      729
         CALL AXPOSITION                                                  729
C    *                                                                    729
C    Geant POSITION : SFKF ===> X=+SFJP_SFKF_PX Y=SFJP_SFKF_PY Z=+(SFJP_  729
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFKF_DZ/2+0.2 ALPHAZ=-S  729
C    FJP_ALPHAZ <===                                                      729
         CALL AGSRESET                                                    732
         AG_X = +SFJP_SFKF_PX                                             732
         AG_Y = SFJP_SFKF_PY                                              732
         AG_Z = +(SFJP_SFFK_DZ/                                           732
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFKF_DZ/2+0.2                732
         AG_ALPHAN=AG_ALPHAN+1                                            732
         AG_ALPHAK(AG_ALPHAN)=3                                           732
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                                732
         AG_TITLE = 'SFKF'                                                732
         AG_EXNAME = 'POSITION'                                           732
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      732
         CALL AXPOSITION                                                  732
C    *                                                                    732
C    Create SFKS :                                                        733
C    Call To Local Procedure SFKS                                         733
         ASSIGN 5690 TO BLOCK_SFKS                                        733
         GO TO  5680                                                      733
5690     CONTINUE                                                         733
C    *                                                                    733
C    Geant POSITION : SFKS ===> X=+SFJP_SFKS_PX Y=SFJP_SFKS_PY Z=-(SFJP_  736
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFKF_DZ/2-0.2 ALPHAZ=-S  736
C    FJP_ALPHAZ <===                                                      736
         CALL AGSRESET                                                    736
         AG_X = +SFJP_SFKS_PX                                             736
         AG_Y = SFJP_SFKS_PY                                              736
         AG_Z = -(SFJP_SFFK_DZ/                                           736
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFKF_DZ/2-0.2                736
         AG_ALPHAN=AG_ALPHAN+1                                            736
         AG_ALPHAK(AG_ALPHAN)=3                                           736
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                                736
         AG_TITLE = 'SFKS'                                                736
         AG_EXNAME = 'POSITION'                                           736
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      736
         CALL AXPOSITION                                                  736
C    *                                                                    736
C    Geant POSITION : SFKS ===> X=-SFJP_SFKS_PX Y=SFJP_SFKS_PY Z=+(SFJP_  736
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFKF_DZ/2+0.2 ALPHAZ=+S  736
C    FJP_ALPHAZ <===                                                      736
         CALL AGSRESET                                                    739
         AG_X = -SFJP_SFKS_PX                                             739
         AG_Y = SFJP_SFKS_PY                                              739
         AG_Z = +(SFJP_SFFK_DZ/                                           739
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFKF_DZ/2+0.2                739
         AG_ALPHAN=AG_ALPHAN+1                                            739
         AG_ALPHAK(AG_ALPHAN)=3                                           739
         AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                                739
         AG_TITLE = 'SFKS'                                                739
         AG_EXNAME = 'POSITION'                                           739
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      739
         CALL AXPOSITION                                                  739
C    *                                                                    739
C    Create SFPR :                                                        740
C    Call To Local Procedure SFPR                                         740
         ASSIGN 5710 TO BLOCK_SFPR                                        740
         GO TO  5700                                                      740
5710     CONTINUE                                                         740
C    *                                                                    740
C    Geant POSITION : SFPR ===> X=0 Y=+SFJP_SFPR_PY Z=-SFJP_SFLU_DZ <===  741
         CALL AGSRESET                                                    741
         AG_X = 0                                                         741
         AG_Y = +SFJP_SFPR_PY                                             741
         AG_Z = -SFJP_SFLU_DZ                                             741
         AG_TITLE = 'SFPR'                                                741
         AG_EXNAME = 'POSITION'                                           741
         AG_PARLIST = 'X_Y_Z'                                             741
         CALL AXPOSITION                                                  741
C    *                                                                    741
C    Geant POSITION : SFPR ===> X=0 Y=+SFJP_SFPR_PY Z=+SFJP_SFLU_DZ-3.7   741
C    <===                                                                 741
         CALL AGSRESET                                                    742
         AG_X = 0                                                         742
         AG_Y = +SFJP_SFPR_PY                                             742
         AG_Z = +SFJP_SFLU_DZ-3.7                                         742
         AG_TITLE = 'SFPR'                                                742
         AG_EXNAME = 'POSITION'                                           742
         AG_PARLIST = 'X_Y_Z'                                             742
         CALL AXPOSITION                                                  742
C    *                                                                    742
C    Create SFPB :                                                        743
C    Call To Local Procedure SFPB                                         743
         ASSIGN 5730 TO BLOCK_SFPB                                        743
         GO TO  5720                                                      743
5730     CONTINUE                                                         743
C    *                                                                    743
C    Geant POSITION : SFPB ===> X=0 Y=+SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SF  745
C    PB_PY2 Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ/2. <===                           745
         CALL AGSRESET                                                    745
         AG_X = 0                                                         745
         AG_Y = +SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SFPB_PY2                  745
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ/2.                              745
         AG_TITLE = 'SFPB'                                                745
         AG_EXNAME = 'POSITION'                                           745
         AG_PARLIST = 'X_Y_Z'                                             745
         CALL AXPOSITION                                                  745
C    *                                                                    745
C    Geant POSITION : SFPB ===> X=0 Y=+SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SF  745
C    PB_PY2 Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ/2. <===                           745
         CALL AGSRESET                                                    747
         AG_X = 0                                                         747
         AG_Y = +SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SFPB_PY2                  747
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ/2.                              747
         AG_TITLE = 'SFPB'                                                747
         AG_EXNAME = 'POSITION'                                           747
         AG_PARLIST = 'X_Y_Z'                                             747
         CALL AXPOSITION                                                  747
C    *                                                                    747
C    Create SSBS :                                                        748
C    Call To Local Procedure SSBS                                         748
         ASSIGN 5750 TO BLOCK_SSBS                                        748
         GO TO  5740                                                      748
5750     CONTINUE                                                         748
C    *                                                                    748
C    Geant POSITION : SSBS ===> X=0. Y=+SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_S  751
C    FPB_PY2-0.8/2.-SFJP_SSBS_DY/2. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-0.2+SFJP  751
C    _SSBB_DZ+SFJP_SSBS_DX/2. <===                                        751
         CALL AGSRESET                                                    751
         AG_X = 0.                                                        751
         AG_Y = +SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SFPB_PY2-0.8/             751
     *   2.-SFJP_SSBS_DY/2.                                               751
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-0.2+SFJP_SSBB_DZ+SFJP_SSBS_DX/  751
     *   2.                                                               751
         AG_TITLE = 'SSBS'                                                751
         AG_EXNAME = 'POSITION'                                           751
         AG_PARLIST = 'X_Y_Z'                                             751
         CALL AXPOSITION                                                  751
C    *                                                                    751
C    Geant POSITION : SSBS ===> X=0. Y=+SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_S  751
C    FPB_PY2-0.8/2.-SFJP_SSBS_DY/2. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+0.2-SFJP  751
C    _SSBB_DZ-SFJP_SSBS_DX/2. <===                                        751
         CALL AGSRESET                                                    754
         AG_X = 0.                                                        754
         AG_Y = +SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SFPB_PY2-0.8/             754
     *   2.-SFJP_SSBS_DY/2.                                               754
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+0.2-SFJP_SSBB_DZ-SFJP_SSBS_DX/  754
     *   2.                                                               754
         AG_TITLE = 'SSBS'                                                754
         AG_EXNAME = 'POSITION'                                           754
         AG_PARLIST = 'X_Y_Z'                                             754
         CALL AXPOSITION                                                  754
C    *                                                                    754
C    Create SSBB :                                                        755
C    Call To Local Procedure SSBB                                         755
         ASSIGN 5770 TO BLOCK_SSBB                                        755
         GO TO  5760                                                      755
5770     CONTINUE                                                         755
C    *                                                                    755
C    Geant POSITION : SSBB ===> X=0. Y=+SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_S  758
C    FPB_PY2-0.8/2.-SFJP_SSBS_DY/2. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-0.2+SFJP  758
C    _SSBB_DZ/2. <===                                                     758
         CALL AGSRESET                                                    758
         AG_X = 0.                                                        758
         AG_Y = +SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SFPB_PY2-0.8/             758
     *   2.-SFJP_SSBS_DY/2.                                               758
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-0.2+SFJP_SSBB_DZ/2.             758
         AG_TITLE = 'SSBB'                                                758
         AG_EXNAME = 'POSITION'                                           758
         AG_PARLIST = 'X_Y_Z'                                             758
         CALL AXPOSITION                                                  758
C    *                                                                    758
C    Geant POSITION : SSBB ===> X=0. Y=+SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_S  758
C    FPB_PY2-0.8/2.-SFJP_SSBS_DY/2. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+0.2-SFJP  758
C    _SSBB_DZ/2. <===                                                     758
         CALL AGSRESET                                                    761
         AG_X = 0.                                                        761
         AG_Y = +SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SFPB_PY2-0.8/             761
     *   2.-SFJP_SSBS_DY/2.                                               761
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+0.2-SFJP_SSBB_DZ/2.             761
         AG_TITLE = 'SSBB'                                                761
         AG_EXNAME = 'POSITION'                                           761
         AG_PARLIST = 'X_Y_Z'                                             761
         CALL AXPOSITION                                                  761
C    *                                                                    761
C    Create SFLA :                                                        762
C    Call To Local Procedure SFLA                                         762
         ASSIGN 5790 TO BLOCK_SFLA                                        762
         GO TO  5780                                                      762
5790     CONTINUE                                                         762
C    *                                                                    762
C    Create SFLB :                                                        763
C    Call To Local Procedure SFLB                                         763
         ASSIGN 5810 TO BLOCK_SFLB                                        763
         GO TO  5800                                                      763
5810     CONTINUE                                                         763
C    *                                                                    763
C    Create SFLC :                                                        764
C    Call To Local Procedure SFLC                                         764
         ASSIGN 5830 TO BLOCK_SFLC                                        764
         GO TO  5820                                                      764
5830     CONTINUE                                                         764
C    *                                                                    764
C    Create SFEB :                                                        765
C    Call To Local Procedure SFEB                                         765
         ASSIGN 5850 TO BLOCK_SFEB                                        765
         GO TO  5840                                                      765
5850     CONTINUE                                                         765
C    *                                                                    765
C    Create SFES :                                                        766
C    Call To Local Procedure SFES                                         766
         ASSIGN 5870 TO BLOCK_SFES                                        766
         GO TO  5860                                                      766
5870     CONTINUE                                                         766
C    *                                                                    766
         WAFPCKLEN=SFPA_WPLEN/(SFPA_NSSD*1.)                              767
C    *                                                                    768
         IWAF=1                                                           768
            GO TO 5883                                                    768
5881        IWAF=IWAF+1                                                   768
5883        IF(IWAF-(8).GT.0) GO TO 5882                                  768
C       *                                                                 769
            JWAF=1                                                        769
               GO TO 5893                                                 769
5891           JWAF=JWAF+1                                                769
5893           IF(JWAF-(IWAF).GT.0) GO TO 5892                            769
C          Geant POSITION : SFLA ===> X=+SFJP_SFLA_PX+SFJP_FLEX_DI*JWAF   772
C          Y=SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF Z=-(SFPA_WPLEN+WAFPCKLEN)/2+  772
C          IWAF*WAFPCKLEN ALPHAZ=-SFJP_ALPHAZ <===                        772
               CALL AGSRESET                                              772
               AG_X = +SFJP_SFLA_PX+SFJP_FLEX_DI*JWAF                     772
               AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF                      772
               AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+IWAF*WAFPCKLEN            772
               AG_ALPHAN=AG_ALPHAN+1                                      772
               AG_ALPHAK(AG_ALPHAN)=3                                     772
               AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                          772
               AG_TITLE = 'SFLA'                                          772
               AG_EXNAME = 'POSITION'                                     772
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                772
               CALL AXPOSITION                                            772
C          *                                                              772
C          Geant POSITION : SFLA ===> X=-SFJP_SFLA_PX-SFJP_FLEX_DI*JWAF   772
C          Y=SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF Z=+(SFPA_WPLEN+WAFPCKLEN)/2-  772
C          IWAF*WAFPCKLEN ALPHAZ=+SFJP_ALPHAZ <===                        772
               CALL AGSRESET                                              775
               AG_X = -SFJP_SFLA_PX-SFJP_FLEX_DI*JWAF                     775
               AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF                      775
               AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-IWAF*WAFPCKLEN            775
               AG_ALPHAN=AG_ALPHAN+1                                      775
               AG_ALPHAK(AG_ALPHAN)=3                                     775
               AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                          775
               AG_TITLE = 'SFLA'                                          775
               AG_EXNAME = 'POSITION'                                     775
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                775
               CALL AXPOSITION                                            775
C          *                                                              775
C          Geant POSITION : SFLC ===> X=+SFJP_SFLA_PX+SFJP_FLEX_DI*JWAF   775
C          Y=SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF Z=-(SFPA_WPLEN+WAFPCKLEN)/2.  775
C          +16*WAFPCKLEN+WAFPCKLEN/2.+SFJP_SFLB_DZ+8.5*SFJP_SFLC_DZ-IWAF  775
C          *SFJP_SFLC_DZ ALPHAZ=-SFJP_ALPHAZ <===                         775
               CALL AGSRESET                                              779
               AG_X = +SFJP_SFLA_PX+SFJP_FLEX_DI*JWAF                     779
               AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF                      779
               AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2.+16*WAFPCKLEN+WAFPCKLEN/  779
     *         2.+SFJP_SFLB_DZ+8.5*SFJP_SFLC_DZ-IWAF*SFJP_SFLC_DZ         779
               AG_ALPHAN=AG_ALPHAN+1                                      779
               AG_ALPHAK(AG_ALPHAN)=3                                     779
               AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                          779
               AG_TITLE = 'SFLC'                                          779
               AG_EXNAME = 'POSITION'                                     779
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                779
               CALL AXPOSITION                                            779
C          *                                                              779
C          Geant POSITION : SFLC ===> X=-SFJP_SFLA_PX-SFJP_FLEX_DI*JWAF   779
C          Y=SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF Z=+(SFPA_WPLEN+WAFPCKLEN)/2.  779
C          -16*WAFPCKLEN-WAFPCKLEN/2.-SFJP_SFLB_DZ-8.5*SFJP_SFLC_DZ+IWAF  779
C          *SFJP_SFLC_DZ ALPHAZ=+SFJP_ALPHAZ <===                         779
               CALL AGSRESET                                              783
               AG_X = -SFJP_SFLA_PX-SFJP_FLEX_DI*JWAF                     783
               AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF                      783
               AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2.-16*WAFPCKLEN-WAFPCKLEN/  783
     *         2.-SFJP_SFLB_DZ-8.5*SFJP_SFLC_DZ+IWAF*SFJP_SFLC_DZ         783
               AG_ALPHAN=AG_ALPHAN+1                                      783
               AG_ALPHAK(AG_ALPHAN)=3                                     783
               AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                          783
               AG_TITLE = 'SFLC'                                          783
               AG_EXNAME = 'POSITION'                                     783
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                783
               CALL AXPOSITION                                            783
C          *                                                              783
C          Geant POSITION : SFLA ===> X=+SFJP_SFLC_PX+SFJP_FLEX_DI*JWAF   783
C          Y=-SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF Z=-(SFPA_WPLEN+WAFPCKLEN)/2  783
C          +(IWAF+8)*WAFPCKLEN ALPHAZ=-SFJP_ALPHAZ <===                   783
               CALL AGSRESET                                              786
               AG_X = +SFJP_SFLC_PX+SFJP_FLEX_DI*JWAF                     786
               AG_Y = -SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF                     786
               AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+(IWAF+8)*WAFPCKLEN        786
               AG_ALPHAN=AG_ALPHAN+1                                      786
               AG_ALPHAK(AG_ALPHAN)=3                                     786
               AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                          786
               AG_TITLE = 'SFLA'                                          786
               AG_EXNAME = 'POSITION'                                     786
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                786
               CALL AXPOSITION                                            786
C          *                                                              786
C          Geant POSITION : SFLA ===> X=-SFJP_SFLC_PX-SFJP_FLEX_DI*JWAF   786
C          Y=-SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF Z=+(SFPA_WPLEN+WAFPCKLEN)/2  786
C          -(IWAF+8)*WAFPCKLEN ALPHAZ=+SFJP_ALPHAZ <===                   786
               CALL AGSRESET                                              789
               AG_X = -SFJP_SFLC_PX-SFJP_FLEX_DI*JWAF                     789
               AG_Y = -SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF                     789
               AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-(IWAF+8)*WAFPCKLEN        789
               AG_ALPHAN=AG_ALPHAN+1                                      789
               AG_ALPHAK(AG_ALPHAN)=3                                     789
               AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                          789
               AG_TITLE = 'SFLA'                                          789
               AG_EXNAME = 'POSITION'                                     789
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                789
               CALL AXPOSITION                                            789
C          *                                                              789
C          Geant POSITION : SFLC ===> X=+SFJP_SFLC_PX+SFJP_FLEX_DI*JWAF   789
C          Y=-SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF Z=-(SFPA_WPLEN+WAFPCKLEN)/2  789
C          +16*WAFPCKLEN+WAFPCKLEN/2.+8.5*SFJP_SFLC_DZ-IWAF*SFJP_SFLC_DZ  789
C           ALPHAZ=-SFJP_ALPHAZ <===                                      789
               CALL AGSRESET                                              793
               AG_X = +SFJP_SFLC_PX+SFJP_FLEX_DI*JWAF                     793
               AG_Y = -SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF                     793
               AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+16*WAFPCKLEN+WAFPCKLEN/   793
     *         2.+8.5*SFJP_SFLC_DZ-IWAF*SFJP_SFLC_DZ                      793
               AG_ALPHAN=AG_ALPHAN+1                                      793
               AG_ALPHAK(AG_ALPHAN)=3                                     793
               AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                          793
               AG_TITLE = 'SFLC'                                          793
               AG_EXNAME = 'POSITION'                                     793
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                793
               CALL AXPOSITION                                            793
C          *                                                              793
C          Geant POSITION : SFLC ===> X=-SFJP_SFLC_PX-SFJP_FLEX_DI*JWAF   793
C          Y=-SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF Z=+(SFPA_WPLEN+WAFPCKLEN)/2  793
C          -16*WAFPCKLEN-WAFPCKLEN/2.-8.5*SFJP_SFLC_DZ+IWAF*SFJP_SFLC_DZ  793
C           ALPHAZ=+SFJP_ALPHAZ <===                                      793
               CALL AGSRESET                                              797
               AG_X = -SFJP_SFLC_PX-SFJP_FLEX_DI*JWAF                     797
               AG_Y = -SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF                     797
               AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-16*WAFPCKLEN-WAFPCKLEN/   797
     *         2.-8.5*SFJP_SFLC_DZ+IWAF*SFJP_SFLC_DZ                      797
               AG_ALPHAN=AG_ALPHAN+1                                      797
               AG_ALPHAK(AG_ALPHAN)=3                                     797
               AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                          797
               AG_TITLE = 'SFLC'                                          797
               AG_EXNAME = 'POSITION'                                     797
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                797
               CALL AXPOSITION                                            797
C          *                                                              797
            GO TO 5891                                                    798
5892        CONTINUE                                                      798
C       *                                                                 799
            JWAF=1                                                        799
               GO TO 5903                                                 799
5901           JWAF=JWAF+1                                                799
5903           IF(JWAF-(8).GT.0) GO TO 5902                               799
C          Geant POSITION : SFLA ===> X=+SFJP_SFLA_PX+SFJP_FLEX_DI*JWAF   802
C          Y=SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF Z=-(SFPA_WPLEN+WAFPCKLEN)/2+  802
C          (IWAF+8)*WAFPCKLEN ALPHAZ=-SFJP_ALPHAZ <===                    802
               CALL AGSRESET                                              802
               AG_X = +SFJP_SFLA_PX+SFJP_FLEX_DI*JWAF                     802
               AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF                      802
               AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+(IWAF+8)*WAFPCKLEN        802
               AG_ALPHAN=AG_ALPHAN+1                                      802
               AG_ALPHAK(AG_ALPHAN)=3                                     802
               AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                          802
               AG_TITLE = 'SFLA'                                          802
               AG_EXNAME = 'POSITION'                                     802
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                802
               CALL AXPOSITION                                            802
C          *                                                              802
C          Geant POSITION : SFLA ===> X=-SFJP_SFLA_PX-SFJP_FLEX_DI*JWAF   802
C          Y=SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF Z=+(SFPA_WPLEN+WAFPCKLEN)/2-  802
C          (IWAF+8)*WAFPCKLEN ALPHAZ=+SFJP_ALPHAZ <===                    802
               CALL AGSRESET                                              805
               AG_X = -SFJP_SFLA_PX-SFJP_FLEX_DI*JWAF                     805
               AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF                      805
               AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-(IWAF+8)*WAFPCKLEN        805
               AG_ALPHAN=AG_ALPHAN+1                                      805
               AG_ALPHAK(AG_ALPHAN)=3                                     805
               AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                          805
               AG_TITLE = 'SFLA'                                          805
               AG_EXNAME = 'POSITION'                                     805
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                805
               CALL AXPOSITION                                            805
C          *                                                              805
            GO TO 5901                                                    806
5902        CONTINUE                                                      806
C       Geant POSITION : SFLB ===> X=+SFJP_SFLA_PX+SFJP_FLEX_DI*IWAF Y=S  810
C       FJP_SFLA_PY+SFJP_FLEX_DI*IWAF Z=-(SFPA_WPLEN+WAFPCKLEN)/2+16*WAF  810
C       PCKLEN+WAFPCKLEN/2.+SFJP_SFLB_DZ/2. ALPHAZ=-SFJP_ALPHAZ <===      810
            CALL AGSRESET                                                 810
            AG_X = +SFJP_SFLA_PX+SFJP_FLEX_DI*IWAF                        810
            AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*IWAF                         810
            AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+16*WAFPCKLEN+WAFPCKLEN/      810
     *      2.+SFJP_SFLB_DZ/2.                                            810
            AG_ALPHAN=AG_ALPHAN+1                                         810
            AG_ALPHAK(AG_ALPHAN)=3                                        810
            AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                             810
            AG_TITLE = 'SFLB'                                             810
            AG_EXNAME = 'POSITION'                                        810
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   810
            CALL AXPOSITION                                               810
C       *                                                                 810
C       Geant POSITION : SFLB ===> X=-SFJP_SFLA_PX-SFJP_FLEX_DI*IWAF Y=S  810
C       FJP_SFLA_PY+SFJP_FLEX_DI*IWAF Z=+(SFPA_WPLEN+WAFPCKLEN)/2-16*WAF  810
C       PCKLEN-WAFPCKLEN/2.-SFJP_SFLB_DZ/2. ALPHAZ=+SFJP_ALPHAZ <===      810
            CALL AGSRESET                                                 814
            AG_X = -SFJP_SFLA_PX-SFJP_FLEX_DI*IWAF                        814
            AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*IWAF                         814
            AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-16*WAFPCKLEN-WAFPCKLEN/      814
     *      2.-SFJP_SFLB_DZ/2.                                            814
            AG_ALPHAN=AG_ALPHAN+1                                         814
            AG_ALPHAK(AG_ALPHAN)=3                                        814
            AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                             814
            AG_TITLE = 'SFLB'                                             814
            AG_EXNAME = 'POSITION'                                        814
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   814
            CALL AXPOSITION                                               814
C       *                                                                 814
C       Geant POSITION : SFES ===> X=-SFJP_SFES_PX Y=-SFJP_SFES_PY Z=-(S  814
C       FPA_WPLEN+WAFPCKLEN)/2+(IWAF)*WAFPCKLEN+WAFPCKLEN/2.-SFJP_SFEB_D  814
C       Z/2.-SFJP_SFES_PZ/2. ALPHAZ=57.66 <===                            814
            CALL AGSRESET                                                 817
            AG_X = -SFJP_SFES_PX                                          817
            AG_Y = -SFJP_SFES_PY                                          817
            AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+(IWAF)*WAFPCKLEN+WAFPCKLEN/  817
     *      2.-SFJP_SFEB_DZ/2.-SFJP_SFES_PZ/2.                            817
            AG_ALPHAN=AG_ALPHAN+1                                         817
            AG_ALPHAK(AG_ALPHAN)=3                                        817
            AG_ALPHAL(AG_ALPHAN)=57.66                                    817
            AG_TITLE = 'SFES'                                             817
            AG_EXNAME = 'POSITION'                                        817
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   817
            CALL AXPOSITION                                               817
C       *                                                                 817
C       Geant POSITION : SFES ===> X=+SFJP_SFES_PX Y=-SFJP_SFES_PY Z=+(S  817
C       FPA_WPLEN+WAFPCKLEN)/2-(IWAF)*WAFPCKLEN-WAFPCKLEN/2.+SFJP_SFEB_D  817
C       Z/2.+SFJP_SFES_PZ/2. ALPHAZ=-57.66 <===                           817
            CALL AGSRESET                                                 820
            AG_X = +SFJP_SFES_PX                                          820
            AG_Y = -SFJP_SFES_PY                                          820
            AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-(IWAF)*WAFPCKLEN-WAFPCKLEN/  820
     *      2.+SFJP_SFEB_DZ/2.+SFJP_SFES_PZ/2.                            820
            AG_ALPHAN=AG_ALPHAN+1                                         820
            AG_ALPHAK(AG_ALPHAN)=3                                        820
            AG_ALPHAL(AG_ALPHAN)=-57.66                                   820
            AG_TITLE = 'SFES'                                             820
            AG_EXNAME = 'POSITION'                                        820
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   820
            CALL AXPOSITION                                               820
C       *                                                                 820
C       Geant POSITION : SFEB ===> X=-SFJP_SFEB_PX Y=-SFJP_SFEB_PY Z=-(S  820
C       FPA_WPLEN+WAFPCKLEN)/2+(IWAF+8)*WAFPCKLEN+WAFPCKLEN/2.-SFJP_SFEB  820
C       _DZ/2.-SFJP_SFES_PZ/2. ALPHAZ=55.35 <===                          820
            CALL AGSRESET                                                 823
            AG_X = -SFJP_SFEB_PX                                          823
            AG_Y = -SFJP_SFEB_PY                                          823
            AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/                               823
     *      2+(IWAF+8)*WAFPCKLEN+WAFPCKLEN/2.-SFJP_SFEB_DZ/               823
     *      2.-SFJP_SFES_PZ/2.                                            823
            AG_ALPHAN=AG_ALPHAN+1                                         823
            AG_ALPHAK(AG_ALPHAN)=3                                        823
            AG_ALPHAL(AG_ALPHAN)=55.35                                    823
            AG_TITLE = 'SFEB'                                             823
            AG_EXNAME = 'POSITION'                                        823
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   823
            CALL AXPOSITION                                               823
C       *                                                                 823
C       Geant POSITION : SFEB ===> X=+SFJP_SFEB_PX Y=-SFJP_SFEB_PY Z=+(S  823
C       FPA_WPLEN+WAFPCKLEN)/2-(IWAF+8)*WAFPCKLEN-WAFPCKLEN/2.+SFJP_SFEB  823
C       _DZ/2.+SFJP_SFES_PZ/2. ALPHAZ=-55.35 <===                         823
            CALL AGSRESET                                                 826
            AG_X = +SFJP_SFEB_PX                                          826
            AG_Y = -SFJP_SFEB_PY                                          826
            AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/                               826
     *      2-(IWAF+8)*WAFPCKLEN-WAFPCKLEN/2.+SFJP_SFEB_DZ/               826
     *      2.+SFJP_SFES_PZ/2.                                            826
            AG_ALPHAN=AG_ALPHAN+1                                         826
            AG_ALPHAK(AG_ALPHAN)=3                                        826
            AG_ALPHAL(AG_ALPHAN)=-55.35                                   826
            AG_TITLE = 'SFEB'                                             826
            AG_EXNAME = 'POSITION'                                        826
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   826
            CALL AXPOSITION                                               826
C       *                                                                 826
         GO TO 5881                                                       827
5882     CONTINUE                                                         827
5251  CALL AGSPOPB                                                        828
      GO TO BLOCK_SFSM, (5250 ,5260)                                      828
C End of Local Procedure                                                  828
C ----------------------------------------------------------------------  829
C ------------ SFLT                                                       829
C Begin Local Procedure SFLT                                              830
C SFLT IS (HALF) THE TOP CORNER OF THE TRIANGULAR LADDER SKELETON         830
5460  CONTINUE                                                            830
         AG_TITLE='SFLT'                                                  830
         AG_MODULE=MODULE                                                 830
         AG_MARK='V00'                                                    830
         CALL AGSPUSH                                                     830
         AG_MODULE=MODULE                                                 830
C    *                                                                    830
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           831
         CALL AGSRESET                                                    831
         AG_TITLE = 'CARBON'                                              831
         AG_EXNAME = 'MATERIAL'                                           831
         AG_PARLIST = 'NONE'                                              831
         CALL AXMATERIAL                                                  831
C    *                                                                    831
C    Geant ATTRIBUTE : SFLT ===> SEEN=1 COLO=1 <===                       831
         CALL AGSRESET                                                    832
         AG_SEEN = 1                                                      832
         AG_COLO = 1                                                      832
         AG_TITLE = 'SFLT'                                                832
         AG_EXNAME = 'ATTRIBUTE'                                          832
         AG_PARLIST = 'SEEN_COLO'                                         832
         CALL AXATTRIBUTE                                                 832
C    *                                                                    832
C    Geant SHAPE : TRAP ===> DZ=SFJP_SFLU_DZ THET=0 PHI=0, H1=SFJP_SFLU_  832
C    H1 BL1=SFJP_SFLU_BL1 TL1=SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(PI/5), ALP1  832
C    =(PI/2-ATAN(2*TAN(PI/5)))*360/(2*PI), H2=SFJP_SFLU_H1 BL2=SFJP_SFLU  832
C    _BL1, TL2=SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(PI/5), ALP2=(PI/2-ATAN(2*T  832
C    AN(PI/5)))*360/(2*PI) <===                                           832
         CALL AGSRESET                                                    838
         AG_DZ = SFJP_SFLU_DZ                                             838
         AG_THET = 0                                                      838
         AG_PHI = 0                                                       838
         AG_H1 = SFJP_SFLU_H1                                             838
         AG_BL1 = SFJP_SFLU_BL1                                           838
         AG_TL1 = SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(PI/5)                    838
         AG_ALP1 = (PI/2-ATAN(2*TAN(PI/5)))*360/(2*PI)                    838
         AG_H2 = SFJP_SFLU_H1                                             838
         AG_BL2 = SFJP_SFLU_BL1                                           838
         AG_TL2 = SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(PI/5)                    838
         AG_ALP2 = (PI/2-ATAN(2*TAN(PI/5)))*360/(2*PI)                    838
         AG_TITLE = 'TRAP'                                                838
         AG_EXNAME = 'SHAPE'                                              838
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       838
         CALL AXSHAPE                                                     838
C    *                                                                    838
5461  CALL AGSPOPB                                                        839
      GO TO BLOCK_SFLT, (5460 ,5470)                                      839
C End of Local Procedure                                                  839
C ----------------------------------------------------------------------  840
C ------------ SFLU                                                       840
C Begin Local Procedure SFLU                                              841
C SFLU IS (HALF) THE SIDE CORNER OF THE TRIANGULAR LADDER SKELETON        841
5480  CONTINUE                                                            841
         AG_TITLE='SFLU'                                                  841
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
C    Geant ATTRIBUTE : SFLU ===> SEEN=1 COLO=1 <===                       842
         CALL AGSRESET                                                    843
         AG_SEEN = 1                                                      843
         AG_COLO = 1                                                      843
         AG_TITLE = 'SFLU'                                                843
         AG_EXNAME = 'ATTRIBUTE'                                          843
         AG_PARLIST = 'SEEN_COLO'                                         843
         CALL AXATTRIBUTE                                                 843
C    *                                                                    843
C    Geant SHAPE : TRAP ===> DZ=SFJP_SFLU_DZ THET=0 PHI=0, H1=SFJP_SFLU_  843
C    H1 BL1=SFJP_SFLU_BL1 TL1=SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(27*2*PI/360  843
C    ), ALP1=(PI/2-ATAN(2*TAN(27*2*PI/360)))*360/(2*PI), H2=SFJP_SFLU_H1  843
C    , BL2=SFJP_SFLU_BL1 TL2=SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(27*2*PI/360)  843
C    , ALP2=(PI/2-ATAN(2*TAN(27*2*PI/360)))*360/(2*PI) <===               843
         CALL AGSRESET                                                    849
         AG_DZ = SFJP_SFLU_DZ                                             849
         AG_THET = 0                                                      849
         AG_PHI = 0                                                       849
         AG_H1 = SFJP_SFLU_H1                                             849
         AG_BL1 = SFJP_SFLU_BL1                                           849
         AG_TL1 = SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(27*2*PI/360)             849
         AG_ALP1 = (PI/2-ATAN(2*TAN(27*2*PI/360)))*360/(2*PI)             849
         AG_H2 = SFJP_SFLU_H1                                             849
         AG_BL2 = SFJP_SFLU_BL1                                           849
         AG_TL2 = SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(27*2*PI/360)             849
         AG_ALP2 = (PI/2-ATAN(2*TAN(27*2*PI/360)))*360/(2*PI)             849
         AG_TITLE = 'TRAP'                                                849
         AG_EXNAME = 'SHAPE'                                              849
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       849
         CALL AXSHAPE                                                     849
C    *                                                                    849
5481  CALL AGSPOPB                                                        850
      GO TO BLOCK_SFLU, (5480 ,5490)                                      850
C End of Local Procedure                                                  850
C ----------------------------------------------------------------------  851
C ------------ SFFK                                                       851
C Begin Local Procedure SFFK                                              852
C SFFK HORIZONTAL PART OF THE LADDER SKELETON CARBON BASE                 852
5500  CONTINUE                                                            852
         AG_TITLE='SFFK'                                                  852
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
C    Geant ATTRIBUTE : SFFK ===> SEEN=1 COLO=1 <===                       853
         CALL AGSRESET                                                    854
         AG_SEEN = 1                                                      854
         AG_COLO = 1                                                      854
         AG_TITLE = 'SFFK'                                                854
         AG_EXNAME = 'ATTRIBUTE'                                          854
         AG_PARLIST = 'SEEN_COLO'                                         854
         CALL AXATTRIBUTE                                                 854
C    *                                                                    854
C    Geant SHAPE : BOX ===> DX=SFJP_SFFK_DXE DY=SFJP_SFFK_DY, DZ=SFJP_SF  854
C    FK_DZ/2 <===                                                         854
         CALL AGSRESET                                                    856
         AG_DX = SFJP_SFFK_DXE                                            856
         AG_DY = SFJP_SFFK_DY                                             856
         AG_DZ = SFJP_SFFK_DZ/2                                           856
         AG_TITLE = 'BOX'                                                 856
         AG_EXNAME = 'SHAPE'                                              856
         AG_PARLIST = 'DX_DY_DZ'                                          856
         CALL AXSHAPE                                                     856
C    *                                                                    856
5501  CALL AGSPOPB                                                        857
      GO TO BLOCK_SFFK, (5500 ,5510)                                      857
C End of Local Procedure                                                  857
C ----------------------------------------------------------------------  858
C ------------ SFFL                                                       858
C Begin Local Procedure SFFL                                              859
C SFFL TITLED PART OF THE LADDER SKELETON CARBON BASE                     859
5520  CONTINUE                                                            859
         AG_TITLE='SFFL'                                                  859
         AG_MODULE=MODULE                                                 859
         AG_MARK='V00'                                                    859
         CALL AGSPUSH                                                     859
         AG_MODULE=MODULE                                                 859
C    *                                                                    859
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           860
         CALL AGSRESET                                                    860
         AG_TITLE = 'CARBON'                                              860
         AG_EXNAME = 'MATERIAL'                                           860
         AG_PARLIST = 'NONE'                                              860
         CALL AXMATERIAL                                                  860
C    *                                                                    860
C    Geant ATTRIBUTE : SFFL ===> SEEN=1 COLO=1 <===                       860
         CALL AGSRESET                                                    861
         AG_SEEN = 1                                                      861
         AG_COLO = 1                                                      861
         AG_TITLE = 'SFFL'                                                861
         AG_EXNAME = 'ATTRIBUTE'                                          861
         AG_PARLIST = 'SEEN_COLO'                                         861
         CALL AXATTRIBUTE                                                 861
C    *                                                                    861
C    Geant SHAPE : BOX ===> DX=SFJP_SFFL_DX/COS(15*PI/180), DY=SFJP_SFFK  861
C    _DY DZ=SFJP_SFFK_DZ/2 <===                                           861
         CALL AGSRESET                                                    863
         AG_DX = SFJP_SFFL_DX/COS(15*PI/180)                              863
         AG_DY = SFJP_SFFK_DY                                             863
         AG_DZ = SFJP_SFFK_DZ/2                                           863
         AG_TITLE = 'BOX'                                                 863
         AG_EXNAME = 'SHAPE'                                              863
         AG_PARLIST = 'DX_DY_DZ'                                          863
         CALL AXSHAPE                                                     863
C    *                                                                    863
5521  CALL AGSPOPB                                                        864
      GO TO BLOCK_SFFL, (5520 ,5530)                                      864
C End of Local Procedure                                                  864
C ----------------------------------------------------------------------  865
C ------------ SFKK                                                       865
C Begin Local Procedure SFKK                                              866
C SFKK HORIZONTAL PART OF THE KAPTON FILM UNDER THE LADDER BASE           866
5540  CONTINUE                                                            866
         AG_TITLE='SFKK'                                                  866
         AG_MODULE=MODULE                                                 866
         AG_MARK='V00'                                                    866
         CALL AGSPUSH                                                     866
         AG_MODULE=MODULE                                                 866
C    *                                                                    866
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            867
         CALL AGSRESET                                                    867
         AG_TITLE = 'MYLAR'                                               867
         AG_EXNAME = 'MATERIAL'                                           867
         AG_PARLIST = 'NONE'                                              867
         CALL AXMATERIAL                                                  867
C    *                                                                    867
C    Geant ATTRIBUTE : SFKK ===> SEEN=1 COLO=1 <===                       867
         CALL AGSRESET                                                    868
         AG_SEEN = 1                                                      868
         AG_COLO = 1                                                      868
         AG_TITLE = 'SFKK'                                                868
         AG_EXNAME = 'ATTRIBUTE'                                          868
         AG_PARLIST = 'SEEN_COLO'                                         868
         CALL AXATTRIBUTE                                                 868
C    *                                                                    868
C    Geant SHAPE : BOX ===> DX=SFJP_SFFK_DXZ/2. DY=SFJP_SFKK_DY/2., DZ=S  868
C    FJP_SFFK_DZ/2 <===                                                   868
         CALL AGSRESET                                                    870
         AG_DX = SFJP_SFFK_DXZ/2.                                         870
         AG_DY = SFJP_SFKK_DY/2.                                          870
         AG_DZ = SFJP_SFFK_DZ/2                                           870
         AG_TITLE = 'BOX'                                                 870
         AG_EXNAME = 'SHAPE'                                              870
         AG_PARLIST = 'DX_DY_DZ'                                          870
         CALL AXSHAPE                                                     870
C    *                                                                    870
5541  CALL AGSPOPB                                                        871
      GO TO BLOCK_SFKK, (5540 ,5550)                                      871
C End of Local Procedure                                                  871
C ----------------------------------------------------------------------  872
C ------------ SFKL                                                       872
C Begin Local Procedure SFKL                                              873
C SFKL TITLED PART OF THE KPATON FILM UNDER THE LADDER BASE               873
5560  CONTINUE                                                            873
         AG_TITLE='SFKL'                                                  873
         AG_MODULE=MODULE                                                 873
         AG_MARK='V00'                                                    873
         CALL AGSPUSH                                                     873
         AG_MODULE=MODULE                                                 873
C    *                                                                    873
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            874
         CALL AGSRESET                                                    874
         AG_TITLE = 'MYLAR'                                               874
         AG_EXNAME = 'MATERIAL'                                           874
         AG_PARLIST = 'NONE'                                              874
         CALL AXMATERIAL                                                  874
C    *                                                                    874
C    Geant ATTRIBUTE : SFKL ===> SEEN=1 COLO=1 <===                       874
         CALL AGSRESET                                                    875
         AG_SEEN = 1                                                      875
         AG_COLO = 1                                                      875
         AG_TITLE = 'SFKL'                                                875
         AG_EXNAME = 'ATTRIBUTE'                                          875
         AG_PARLIST = 'SEEN_COLO'                                         875
         CALL AXATTRIBUTE                                                 875
C    *                                                                    875
C    Geant SHAPE : BOX ===> DX=SFJP_SFFL_DX/COS(15*PI/180), DY=SFJP_SFKK  875
C    _DY/2. DZ=SFJP_SFFK_DZ/2 <===                                        875
         CALL AGSRESET                                                    877
         AG_DX = SFJP_SFFL_DX/COS(15*PI/180)                              877
         AG_DY = SFJP_SFKK_DY/2.                                          877
         AG_DZ = SFJP_SFFK_DZ/2                                           877
         AG_TITLE = 'BOX'                                                 877
         AG_EXNAME = 'SHAPE'                                              877
         AG_PARLIST = 'DX_DY_DZ'                                          877
         CALL AXSHAPE                                                     877
C    *                                                                    877
5561  CALL AGSPOPB                                                        878
      GO TO BLOCK_SFKL, (5560 ,5570)                                      878
C End of Local Procedure                                                  878
C ----------------------------------------------------------------------  879
C ------------ SFLA                                                       879
C Begin Local Procedure SFLA                                              881
C SFLA IS THE LONG PART OF THE BUS CABLE LINKING THE MODULES TO THE CONN  881
C ECTION, BOARD                                                           881
5780  CONTINUE                                                            881
         AG_TITLE='SFLA'                                                  881
         AG_MODULE=MODULE                                                 881
         AG_MARK='V00'                                                    881
         CALL AGSPUSH                                                     881
         AG_MODULE=MODULE                                                 881
C    *                                                                    881
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            882
         CALL AGSRESET                                                    882
         AG_TITLE = 'MYLAR'                                               882
         AG_EXNAME = 'MATERIAL'                                           882
         AG_PARLIST = 'NONE'                                              882
         CALL AXMATERIAL                                                  882
C    *                                                                    882
C    Geant ATTRIBUTE : SFLA ===> SEEN=1 COLO=1 <===                       882
         CALL AGSRESET                                                    883
         AG_SEEN = 1                                                      883
         AG_COLO = 1                                                      883
         AG_TITLE = 'SFLA'                                                883
         AG_EXNAME = 'ATTRIBUTE'                                          883
         AG_PARLIST = 'SEEN_COLO'                                         883
         CALL AXATTRIBUTE                                                 883
C    *                                                                    883
C    Geant SHAPE : BOX ===> DX=SFJP_SFLA_DX/2. DY=SFJP_SFLA_DY/2., DZ=SF  883
C    JP_SFLA_DZ/2. <===                                                   883
         CALL AGSRESET                                                    885
         AG_DX = SFJP_SFLA_DX/2.                                          885
         AG_DY = SFJP_SFLA_DY/2.                                          885
         AG_DZ = SFJP_SFLA_DZ/2.                                          885
         AG_TITLE = 'BOX'                                                 885
         AG_EXNAME = 'SHAPE'                                              885
         AG_PARLIST = 'DX_DY_DZ'                                          885
         CALL AXSHAPE                                                     885
C    *                                                                    885
5781  CALL AGSPOPB                                                        886
      GO TO BLOCK_SFLA, (5780 ,5790)                                      886
C End of Local Procedure                                                  886
C ----------------------------------------------------------------------  887
C ------------ SFLB                                                       887
C Begin Local Procedure SFLB                                              888
C SFLB IS THE PART OF THE LONG BUS CABLE ON THE CONNECTION BOARD          888
5800  CONTINUE                                                            888
         AG_TITLE='SFLB'                                                  888
         AG_MODULE=MODULE                                                 888
         AG_MARK='V00'                                                    888
         CALL AGSPUSH                                                     888
         AG_MODULE=MODULE                                                 888
C    *                                                                    888
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            889
         CALL AGSRESET                                                    889
         AG_TITLE = 'MYLAR'                                               889
         AG_EXNAME = 'MATERIAL'                                           889
         AG_PARLIST = 'NONE'                                              889
         CALL AXMATERIAL                                                  889
C    *                                                                    889
C    Geant ATTRIBUTE : SFLB ===> SEEN=1 COLO=1 <===                       889
         CALL AGSRESET                                                    890
         AG_SEEN = 1                                                      890
         AG_COLO = 1                                                      890
         AG_TITLE = 'SFLB'                                                890
         AG_EXNAME = 'ATTRIBUTE'                                          890
         AG_PARLIST = 'SEEN_COLO'                                         890
         CALL AXATTRIBUTE                                                 890
C    *                                                                    890
C    Geant SHAPE : BOX ===> DX=SFJP_SFLA_DX/2. DY=SFJP_SFLA_DY/2., DZ=SF  890
C    JP_SFLB_DZ/2. <===                                                   890
         CALL AGSRESET                                                    892
         AG_DX = SFJP_SFLA_DX/2.                                          892
         AG_DY = SFJP_SFLA_DY/2.                                          892
         AG_DZ = SFJP_SFLB_DZ/2.                                          892
         AG_TITLE = 'BOX'                                                 892
         AG_EXNAME = 'SHAPE'                                              892
         AG_PARLIST = 'DX_DY_DZ'                                          892
         CALL AXSHAPE                                                     892
C    *                                                                    892
5801  CALL AGSPOPB                                                        893
      GO TO BLOCK_SFLB, (5800 ,5810)                                      893
C End of Local Procedure                                                  893
C ----------------------------------------------------------------------  894
C ------------ SFLC                                                       894
C Begin Local Procedure SFLC                                              896
C SFLC IS THE PART OF THE LONG BUS CABLE ON THE CONNECTION BOARD UP TO,   896
C THE CONNECTOR                                                           896
5820  CONTINUE                                                            896
         AG_TITLE='SFLC'                                                  896
         AG_MODULE=MODULE                                                 896
         AG_MARK='V00'                                                    896
         CALL AGSPUSH                                                     896
         AG_MODULE=MODULE                                                 896
C    *                                                                    896
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            897
         CALL AGSRESET                                                    897
         AG_TITLE = 'MYLAR'                                               897
         AG_EXNAME = 'MATERIAL'                                           897
         AG_PARLIST = 'NONE'                                              897
         CALL AXMATERIAL                                                  897
C    *                                                                    897
C    Geant ATTRIBUTE : SFLC ===> SEEN=1 COLO=1 <===                       897
         CALL AGSRESET                                                    898
         AG_SEEN = 1                                                      898
         AG_COLO = 1                                                      898
         AG_TITLE = 'SFLC'                                                898
         AG_EXNAME = 'ATTRIBUTE'                                          898
         AG_PARLIST = 'SEEN_COLO'                                         898
         CALL AXATTRIBUTE                                                 898
C    *                                                                    898
C    Geant SHAPE : BOX ===> DX=SFJP_SFLA_DX/2. DY=SFJP_SFLA_DY/2., DZ=SF  898
C    JP_SFLC_DZ/2. <===                                                   898
         CALL AGSRESET                                                    900
         AG_DX = SFJP_SFLA_DX/2.                                          900
         AG_DY = SFJP_SFLA_DY/2.                                          900
         AG_DZ = SFJP_SFLC_DZ/2.                                          900
         AG_TITLE = 'BOX'                                                 900
         AG_EXNAME = 'SHAPE'                                              900
         AG_PARLIST = 'DX_DY_DZ'                                          900
         CALL AXSHAPE                                                     900
C    *                                                                    900
5821  CALL AGSPOPB                                                        901
      GO TO BLOCK_SFLC, (5820 ,5830)                                      901
C End of Local Procedure                                                  901
C ----------------------------------------------------------------------  902
C ------------ SFEB                                                       902
C Begin Local Procedure SFEB                                              903
C SFEB IS THE BIG BUS ELBOW                                               903
5840  CONTINUE                                                            903
         AG_TITLE='SFEB'                                                  903
         AG_MODULE=MODULE                                                 903
         AG_MARK='V00'                                                    903
         CALL AGSPUSH                                                     903
         AG_MODULE=MODULE                                                 903
C    *                                                                    903
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            904
         CALL AGSRESET                                                    904
         AG_TITLE = 'MYLAR'                                               904
         AG_EXNAME = 'MATERIAL'                                           904
         AG_PARLIST = 'NONE'                                              904
         CALL AXMATERIAL                                                  904
C    *                                                                    904
C    Geant ATTRIBUTE : SFEB ===> SEEN=1 COLO=1 <===                       904
         CALL AGSRESET                                                    905
         AG_SEEN = 1                                                      905
         AG_COLO = 1                                                      905
         AG_TITLE = 'SFEB'                                                905
         AG_EXNAME = 'ATTRIBUTE'                                          905
         AG_PARLIST = 'SEEN_COLO'                                         905
         CALL AXATTRIBUTE                                                 905
C    *                                                                    905
C    Geant SHAPE : BOX ===> DX=SFJP_SFEB_DX/2. DY=SFJP_SFLA_DY/2., DZ=SF  905
C    JP_SFEB_DZ/2. <===                                                   905
         CALL AGSRESET                                                    907
         AG_DX = SFJP_SFEB_DX/2.                                          907
         AG_DY = SFJP_SFLA_DY/2.                                          907
         AG_DZ = SFJP_SFEB_DZ/2.                                          907
         AG_TITLE = 'BOX'                                                 907
         AG_EXNAME = 'SHAPE'                                              907
         AG_PARLIST = 'DX_DY_DZ'                                          907
         CALL AXSHAPE                                                     907
C    *                                                                    907
5841  CALL AGSPOPB                                                        908
      GO TO BLOCK_SFEB, (5840 ,5850)                                      908
C End of Local Procedure                                                  908
C ----------------------------------------------------------------------  909
C ------------ SFES                                                       909
C Begin Local Procedure SFES                                              910
C SFES IS THE SMALL BUS ELBOW                                             910
5860  CONTINUE                                                            910
         AG_TITLE='SFES'                                                  910
         AG_MODULE=MODULE                                                 910
         AG_MARK='V00'                                                    910
         CALL AGSPUSH                                                     910
         AG_MODULE=MODULE                                                 910
C    *                                                                    910
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            911
         CALL AGSRESET                                                    911
         AG_TITLE = 'MYLAR'                                               911
         AG_EXNAME = 'MATERIAL'                                           911
         AG_PARLIST = 'NONE'                                              911
         CALL AXMATERIAL                                                  911
C    *                                                                    911
C    Geant ATTRIBUTE : SFES ===> SEEN=1 COLO=1 <===                       911
         CALL AGSRESET                                                    912
         AG_SEEN = 1                                                      912
         AG_COLO = 1                                                      912
         AG_TITLE = 'SFES'                                                912
         AG_EXNAME = 'ATTRIBUTE'                                          912
         AG_PARLIST = 'SEEN_COLO'                                         912
         CALL AXATTRIBUTE                                                 912
C    *                                                                    912
C    Geant SHAPE : BOX ===> DX=SFJP_SFES_DX/2. DY=SFJP_SFLA_DY/2., DZ=SF  912
C    JP_SFEB_DZ/2. <===                                                   912
         CALL AGSRESET                                                    914
         AG_DX = SFJP_SFES_DX/2.                                          914
         AG_DY = SFJP_SFLA_DY/2.                                          914
         AG_DZ = SFJP_SFEB_DZ/2.                                          914
         AG_TITLE = 'BOX'                                                 914
         AG_EXNAME = 'SHAPE'                                              914
         AG_PARLIST = 'DX_DY_DZ'                                          914
         CALL AXSHAPE                                                     914
C    *                                                                    914
5861  CALL AGSPOPB                                                        915
      GO TO BLOCK_SFES, (5860 ,5870)                                      915
C End of Local Procedure                                                  915
C ----------------------------------------------------------------------  916
C ------------ SFAM                                                       916
C Begin Local Procedure SFAM                                              917
C SFAM IS THE MOTHER VOLUME OF THE ADC BOARD                              917
5600  CONTINUE                                                            917
         AG_TITLE='SFAM'                                                  917
         AG_MODULE=MODULE                                                 917
         AG_MARK='V00'                                                    917
         CALL AGSPUSH                                                     917
         AG_MODULE=MODULE                                                 917
C    *                                                                    917
C    Geant MATERIAL : AIR ===> inherited arguments used <===              918
         CALL AGSRESET                                                    918
         AG_TITLE = 'AIR'                                                 918
         AG_EXNAME = 'MATERIAL'                                           918
         AG_PARLIST = 'NONE'                                              918
         CALL AXMATERIAL                                                  918
C    *                                                                    918
C    Geant ATTRIBUTE : SFAM ===> SEEN=0 COLO=6 <===                       918
         CALL AGSRESET                                                    919
         AG_SEEN = 0                                                      919
         AG_COLO = 6                                                      919
         AG_TITLE = 'SFAM'                                                919
         AG_EXNAME = 'ATTRIBUTE'                                          919
         AG_PARLIST = 'SEEN_COLO'                                         919
         CALL AXATTRIBUTE                                                 919
C    *                                                                    919
C    Geant SHAPE : BOX ===> DX=SFJP_SFAM_DXE/2. DY=SFJP_SFAM_DY/2., DZ=S  919
C    FJP_SFAM_DZ/2 <===                                                   919
         CALL AGSRESET                                                    921
         AG_DX = SFJP_SFAM_DXE/2.                                         921
         AG_DY = SFJP_SFAM_DY/2.                                          921
         AG_DZ = SFJP_SFAM_DZ/2                                           921
         AG_TITLE = 'BOX'                                                 921
         AG_EXNAME = 'SHAPE'                                              921
         AG_PARLIST = 'DX_DY_DZ'                                          921
         CALL AXSHAPE                                                     921
C    *                                                                    921
C    Create SFAB :                                                        922
C    Call To Local Procedure SFAB                                         922
         ASSIGN 5920 TO BLOCK_SFAB                                        922
         GO TO  5910                                                      922
5920     CONTINUE                                                         922
C    *                                                                    922
C    Geant POSITION : SFAB ===> X=0. Y=0. Z=SFJP_SFAM_DZS/2 <===          923
         CALL AGSRESET                                                    923
         AG_X = 0.                                                        923
         AG_Y = 0.                                                        923
         AG_Z = SFJP_SFAM_DZS/2                                           923
         AG_TITLE = 'SFAB'                                                923
         AG_EXNAME = 'POSITION'                                           923
         AG_PARLIST = 'X_Y_Z'                                             923
         CALL AXPOSITION                                                  923
C    *                                                                    923
C    Create SFAS :                                                        924
C    Call To Local Procedure SFAS                                         924
         ASSIGN 5940 TO BLOCK_SFAS                                        924
         GO TO  5930                                                      924
5940     CONTINUE                                                         924
C    *                                                                    924
C    Geant POSITION : SFAS ===> X=(SFJP_SFAM_DXE-SFJP_SFAM_DXZ)/2. Y=0.   926
C    Z=(-SFJP_SFAM_DZ+SFJP_SFAM_DZS)/2 <===                               926
         CALL AGSRESET                                                    926
         AG_X = (SFJP_SFAM_DXE-SFJP_SFAM_DXZ)/2.                          926
         AG_Y = 0.                                                        926
         AG_Z = (-SFJP_SFAM_DZ+SFJP_SFAM_DZS)/2                           926
         AG_TITLE = 'SFAS'                                                926
         AG_EXNAME = 'POSITION'                                           926
         AG_PARLIST = 'X_Y_Z'                                             926
         CALL AXPOSITION                                                  926
C    *                                                                    926
C    Geant POSITION : SFAS ===> X=(-SFJP_SFAM_DXE+SFJP_SFAM_DXZ)/2. Y=0.  926
C     Z=(-SFJP_SFAM_DZ+SFJP_SFAM_DZS)/2 <===                              926
         CALL AGSRESET                                                    928
         AG_X = (-SFJP_SFAM_DXE+SFJP_SFAM_DXZ)/2.                         928
         AG_Y = 0.                                                        928
         AG_Z = (-SFJP_SFAM_DZ+SFJP_SFAM_DZS)/2                           928
         AG_TITLE = 'SFAS'                                                928
         AG_EXNAME = 'POSITION'                                           928
         AG_PARLIST = 'X_Y_Z'                                             928
         CALL AXPOSITION                                                  928
C    *                                                                    928
5601  CALL AGSPOPB                                                        929
      GO TO BLOCK_SFAM, (5600 ,5610)                                      929
C End of Local Procedure                                                  929
C ----------------------------------------------------------------------  930
C ------------ SFAB                                                       930
C Begin Local Procedure SFAB                                              931
C SFAB IS THE BIG VOLUME OF THE ADC BOARD                                 931
5910  CONTINUE                                                            931
         AG_TITLE='SFAB'                                                  931
         AG_MODULE=MODULE                                                 931
         AG_MARK='V00'                                                    931
         CALL AGSPUSH                                                     931
         AG_MODULE=MODULE                                                 931
C    *                                                                    931
C    Geant MATERIAL : G10 ===> inherited arguments used <===              932
         CALL AGSRESET                                                    932
         AG_TITLE = 'G10'                                                 932
         AG_EXNAME = 'MATERIAL'                                           932
         AG_PARLIST = 'NONE'                                              932
         CALL AXMATERIAL                                                  932
C    *                                                                    932
C    Geant ATTRIBUTE : SFAB ===> SEEN=1 COLO=30 <===                      932
         CALL AGSRESET                                                    933
         AG_SEEN = 1                                                      933
         AG_COLO = 30                                                     933
         AG_TITLE = 'SFAB'                                                933
         AG_EXNAME = 'ATTRIBUTE'                                          933
         AG_PARLIST = 'SEEN_COLO'                                         933
         CALL AXATTRIBUTE                                                 933
C    *                                                                    933
C    Geant SHAPE : BOX ===> DX=SFJP_SFAM_DXE/2. DY=SFJP_SFAM_DY/2., DZ=(  933
C    SFJP_SFAM_DZ-SFJP_SFAM_DZS)/2 <===                                   933
         CALL AGSRESET                                                    935
         AG_DX = SFJP_SFAM_DXE/2.                                         935
         AG_DY = SFJP_SFAM_DY/2.                                          935
         AG_DZ = (SFJP_SFAM_DZ-SFJP_SFAM_DZS)/2                           935
         AG_TITLE = 'BOX'                                                 935
         AG_EXNAME = 'SHAPE'                                              935
         AG_PARLIST = 'DX_DY_DZ'                                          935
         CALL AXSHAPE                                                     935
C    *                                                                    935
5911  CALL AGSPOPB                                                        936
      GO TO BLOCK_SFAB, (5910 ,5920)                                      936
C End of Local Procedure                                                  936
C ----------------------------------------------------------------------  937
C ------------ SFAS                                                       937
C Begin Local Procedure SFAS                                              938
C SFAS IS THE SMALL VOLUME OF THE ADC BOARD                               938
5930  CONTINUE                                                            938
         AG_TITLE='SFAS'                                                  938
         AG_MODULE=MODULE                                                 938
         AG_MARK='V00'                                                    938
         CALL AGSPUSH                                                     938
         AG_MODULE=MODULE                                                 938
C    *                                                                    938
C    Geant MATERIAL : G10 ===> inherited arguments used <===              939
         CALL AGSRESET                                                    939
         AG_TITLE = 'G10'                                                 939
         AG_EXNAME = 'MATERIAL'                                           939
         AG_PARLIST = 'NONE'                                              939
         CALL AXMATERIAL                                                  939
C    *                                                                    939
C    Geant ATTRIBUTE : SFAS ===> SEEN=1 COLO=30 <===                      939
         CALL AGSRESET                                                    940
         AG_SEEN = 1                                                      940
         AG_COLO = 30                                                     940
         AG_TITLE = 'SFAS'                                                940
         AG_EXNAME = 'ATTRIBUTE'                                          940
         AG_PARLIST = 'SEEN_COLO'                                         940
         CALL AXATTRIBUTE                                                 940
C    *                                                                    940
C    Geant SHAPE : BOX ===> DX=(SFJP_SFAM_DXZ)/2. DY=(SFJP_SFAM_DY)/2.,   940
C    DZ=(SFJP_SFAM_DZS)/2 <===                                            940
         CALL AGSRESET                                                    942
         AG_DX = (SFJP_SFAM_DXZ)/2.                                       942
         AG_DY = (SFJP_SFAM_DY)/2.                                        942
         AG_DZ = (SFJP_SFAM_DZS)/2                                        942
         AG_TITLE = 'BOX'                                                 942
         AG_EXNAME = 'SHAPE'                                              942
         AG_PARLIST = 'DX_DY_DZ'                                          942
         CALL AXSHAPE                                                     942
C    *                                                                    942
5931  CALL AGSPOPB                                                        943
      GO TO BLOCK_SFAS, (5930 ,5940)                                      943
C End of Local Procedure                                                  943
C ----------------------------------------------------------------------  944
C ------------ SAPP                                                       944
C Begin Local Procedure SAPP                                              945
C SAPP IS THE MOTHER VOLUME OF THE ADC BOARD APPENDICE                    945
5580  CONTINUE                                                            945
         AG_TITLE='SAPP'                                                  945
         AG_MODULE=MODULE                                                 945
         AG_MARK='V00'                                                    945
         CALL AGSPUSH                                                     945
         AG_MODULE=MODULE                                                 945
C    *                                                                    945
C    Geant MATERIAL : AIR ===> inherited arguments used <===              946
         CALL AGSRESET                                                    946
         AG_TITLE = 'AIR'                                                 946
         AG_EXNAME = 'MATERIAL'                                           946
         AG_PARLIST = 'NONE'                                              946
         CALL AXMATERIAL                                                  946
C    *                                                                    946
C    Geant ATTRIBUTE : SAPP ===> SEEN=0 COLO=6 <===                       946
         CALL AGSRESET                                                    947
         AG_SEEN = 0                                                      947
         AG_COLO = 6                                                      947
         AG_TITLE = 'SAPP'                                                947
         AG_EXNAME = 'ATTRIBUTE'                                          947
         AG_PARLIST = 'SEEN_COLO'                                         947
         CALL AXATTRIBUTE                                                 947
C    *                                                                    947
C    Geant SHAPE : BOX ===> DX=(SFJP_SAPP_DXE+2*SFJP_SAPP_DXZ)/2., DY=(S  947
C    FJP_SAPP_DY+2*SFJP_SAPP_DXZ)/2., DZ=SFJP_SAPP_DZ/2. <===             947
         CALL AGSRESET                                                    950
         AG_DX = (SFJP_SAPP_DXE+2*SFJP_SAPP_DXZ)/2.                       950
         AG_DY = (SFJP_SAPP_DY+2*SFJP_SAPP_DXZ)/2.                        950
         AG_DZ = SFJP_SAPP_DZ/2.                                          950
         AG_TITLE = 'BOX'                                                 950
         AG_EXNAME = 'SHAPE'                                              950
         AG_PARLIST = 'DX_DY_DZ'                                          950
         CALL AXSHAPE                                                     950
C    *                                                                    950
C    Create SAPC :                                                        951
C    Call To Local Procedure SAPC                                         951
         ASSIGN 5960 TO BLOCK_SAPC                                        951
         GO TO  5950                                                      951
5960     CONTINUE                                                         951
C    *                                                                    951
C    Geant POSITION : SAPC ===> X=0. Y=0. Z=(-SFJP_SAPP_DZ+SFJP_SAPP_DY)  952
C    /2. <===                                                             952
         CALL AGSRESET                                                    952
         AG_X = 0.                                                        952
         AG_Y = 0.                                                        952
         AG_Z = (-SFJP_SAPP_DZ+SFJP_SAPP_DY)/2.                           952
         AG_TITLE = 'SAPC'                                                952
         AG_EXNAME = 'POSITION'                                           952
         AG_PARLIST = 'X_Y_Z'                                             952
         CALL AXPOSITION                                                  952
C    *                                                                    952
C    Create SAPS :                                                        953
C    Call To Local Procedure SAPS                                         953
         ASSIGN 5980 TO BLOCK_SAPS                                        953
         GO TO  5970                                                      953
5980     CONTINUE                                                         953
C    *                                                                    953
C    Geant POSITION : SAPS ===> X=SFJP_SAPP_DXE/2.+SFJP_SAPP_DXZ/2 Y=0 Z  954
C    =0. <===                                                             954
         CALL AGSRESET                                                    954
         AG_X = SFJP_SAPP_DXE/2.+SFJP_SAPP_DXZ/2                          954
         AG_Y = 0                                                         954
         AG_Z = 0.                                                        954
         AG_TITLE = 'SAPS'                                                954
         AG_EXNAME = 'POSITION'                                           954
         AG_PARLIST = 'X_Y_Z'                                             954
         CALL AXPOSITION                                                  954
C    *                                                                    954
C    Geant POSITION : SAPS ===> X=-(SFJP_SAPP_DXE/2.+SFJP_SAPP_DXZ/2) Y=  954
C    0 Z=0. <===                                                          954
         CALL AGSRESET                                                    955
         AG_X = -(SFJP_SAPP_DXE/2.+SFJP_SAPP_DXZ/2)                       955
         AG_Y = 0                                                         955
         AG_Z = 0.                                                        955
         AG_TITLE = 'SAPS'                                                955
         AG_EXNAME = 'POSITION'                                           955
         AG_PARLIST = 'X_Y_Z'                                             955
         CALL AXPOSITION                                                  955
C    *                                                                    955
C    Create SAPT :                                                        956
C    Call To Local Procedure SAPT                                         956
         ASSIGN 6000 TO BLOCK_SAPT                                        956
         GO TO  5990                                                      956
6000     CONTINUE                                                         956
C    *                                                                    956
C    Geant POSITION : SAPT ===> X=0. Y=(SFJP_SAPP_DY+SFJP_SAPP_DXZ)/2. Z  957
C    =0. <===                                                             957
         CALL AGSRESET                                                    957
         AG_X = 0.                                                        957
         AG_Y = (SFJP_SAPP_DY+SFJP_SAPP_DXZ)/2.                           957
         AG_Z = 0.                                                        957
         AG_TITLE = 'SAPT'                                                957
         AG_EXNAME = 'POSITION'                                           957
         AG_PARLIST = 'X_Y_Z'                                             957
         CALL AXPOSITION                                                  957
C    *                                                                    957
C    Geant POSITION : SAPT ===> X=0. Y=-(SFJP_SAPP_DY+SFJP_SAPP_DXZ)/2.   957
C    Z=0. <===                                                            957
         CALL AGSRESET                                                    958
         AG_X = 0.                                                        958
         AG_Y = -(SFJP_SAPP_DY+SFJP_SAPP_DXZ)/2.                          958
         AG_Z = 0.                                                        958
         AG_TITLE = 'SAPT'                                                958
         AG_EXNAME = 'POSITION'                                           958
         AG_PARLIST = 'X_Y_Z'                                             958
         CALL AXPOSITION                                                  958
C    *                                                                    958
5581  CALL AGSPOPB                                                        959
      GO TO BLOCK_SAPP, (5580 ,5590)                                      959
C End of Local Procedure                                                  959
C ----------------------------------------------------------------------  960
C ------------ SAPC                                                       960
C Begin Local Procedure SAPC                                              961
C SAPC IS THE CORE (EPOXY) OF THE ADC BOARD APPENDICE                     961
5950  CONTINUE                                                            961
         AG_TITLE='SAPC'                                                  961
         AG_MODULE=MODULE                                                 961
         AG_MARK='V00'                                                    961
         CALL AGSPUSH                                                     961
         AG_MODULE=MODULE                                                 961
C    *                                                                    961
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           962
         CALL AGSRESET                                                    962
         AG_TITLE = 'CARBON'                                              962
         AG_EXNAME = 'MATERIAL'                                           962
         AG_PARLIST = 'NONE'                                              962
         CALL AXMATERIAL                                                  962
C    *                                                                    962
C    Geant ATTRIBUTE : SAPC ===> SEEN=1 COLO=1 <===                       962
         CALL AGSRESET                                                    963
         AG_SEEN = 1                                                      963
         AG_COLO = 1                                                      963
         AG_TITLE = 'SAPC'                                                963
         AG_EXNAME = 'ATTRIBUTE'                                          963
         AG_PARLIST = 'SEEN_COLO'                                         963
         CALL AXATTRIBUTE                                                 963
C    *                                                                    963
C    Geant SHAPE : BOX ===> DX=SFJP_SAPP_DXE/2. DY=SFJP_SAPP_DY/2., DZ=S  963
C    FJP_SAPP_DY/2. <===                                                  963
         CALL AGSRESET                                                    965
         AG_DX = SFJP_SAPP_DXE/2.                                         965
         AG_DY = SFJP_SAPP_DY/2.                                          965
         AG_DZ = SFJP_SAPP_DY/2.                                          965
         AG_TITLE = 'BOX'                                                 965
         AG_EXNAME = 'SHAPE'                                              965
         AG_PARLIST = 'DX_DY_DZ'                                          965
         CALL AXSHAPE                                                     965
C    *                                                                    965
5951  CALL AGSPOPB                                                        966
      GO TO BLOCK_SAPC, (5950 ,5960)                                      966
C End of Local Procedure                                                  966
C ----------------------------------------------------------------------  967
C ------------ SAPS                                                       967
C Begin Local Procedure SAPS                                              968
C SAPS IS THE SIDE SHELL (CARBON) OF THE ADC BOARD APPENDICE              968
5970  CONTINUE                                                            968
         AG_TITLE='SAPS'                                                  968
         AG_MODULE=MODULE                                                 968
         AG_MARK='V00'                                                    968
         CALL AGSPUSH                                                     968
         AG_MODULE=MODULE                                                 968
C    *                                                                    968
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           969
         CALL AGSRESET                                                    969
         AG_TITLE = 'CARBON'                                              969
         AG_EXNAME = 'MATERIAL'                                           969
         AG_PARLIST = 'NONE'                                              969
         CALL AXMATERIAL                                                  969
C    *                                                                    969
C    Geant ATTRIBUTE : SAPS ===> SEEN=1 COLO=1 <===                       969
         CALL AGSRESET                                                    970
         AG_SEEN = 1                                                      970
         AG_COLO = 1                                                      970
         AG_TITLE = 'SAPS'                                                970
         AG_EXNAME = 'ATTRIBUTE'                                          970
         AG_PARLIST = 'SEEN_COLO'                                         970
         CALL AXATTRIBUTE                                                 970
C    *                                                                    970
C    Geant SHAPE : BOX ===> DX=SFJP_SAPP_DXZ/2. DY=(SFJP_SAPP_DY+2*SFJP_  970
C    SAPP_DXZ)/2., DZ=SFJP_SAPP_DZ/2. <===                                970
         CALL AGSRESET                                                    972
         AG_DX = SFJP_SAPP_DXZ/2.                                         972
         AG_DY = (SFJP_SAPP_DY+2*SFJP_SAPP_DXZ)/2.                        972
         AG_DZ = SFJP_SAPP_DZ/2.                                          972
         AG_TITLE = 'BOX'                                                 972
         AG_EXNAME = 'SHAPE'                                              972
         AG_PARLIST = 'DX_DY_DZ'                                          972
         CALL AXSHAPE                                                     972
C    *                                                                    972
5971  CALL AGSPOPB                                                        973
      GO TO BLOCK_SAPS, (5970 ,5980)                                      973
C End of Local Procedure                                                  973
C ----------------------------------------------------------------------  974
C ------------ SAPT                                                       974
C Begin Local Procedure SAPT                                              975
C SAPT IS THE TOP-BOTTOM SHELL (CARBON) OF THE ADC BOARD APPENDICE        975
5990  CONTINUE                                                            975
         AG_TITLE='SAPT'                                                  975
         AG_MODULE=MODULE                                                 975
         AG_MARK='V00'                                                    975
         CALL AGSPUSH                                                     975
         AG_MODULE=MODULE                                                 975
C    *                                                                    975
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           976
         CALL AGSRESET                                                    976
         AG_TITLE = 'CARBON'                                              976
         AG_EXNAME = 'MATERIAL'                                           976
         AG_PARLIST = 'NONE'                                              976
         CALL AXMATERIAL                                                  976
C    *                                                                    976
C    Geant ATTRIBUTE : SAPT ===> SEEN=1 COLO=1 <===                       976
         CALL AGSRESET                                                    977
         AG_SEEN = 1                                                      977
         AG_COLO = 1                                                      977
         AG_TITLE = 'SAPT'                                                977
         AG_EXNAME = 'ATTRIBUTE'                                          977
         AG_PARLIST = 'SEEN_COLO'                                         977
         CALL AXATTRIBUTE                                                 977
C    *                                                                    977
C    Geant SHAPE : BOX ===> DX=SFJP_SAPP_DXE/2. DY=SFJP_SAPP_DXZ/2., DZ=  977
C    SFJP_SAPP_DZ/2. <===                                                 977
         CALL AGSRESET                                                    979
         AG_DX = SFJP_SAPP_DXE/2.                                         979
         AG_DY = SFJP_SAPP_DXZ/2.                                         979
         AG_DZ = SFJP_SAPP_DZ/2.                                          979
         AG_TITLE = 'BOX'                                                 979
         AG_EXNAME = 'SHAPE'                                              979
         AG_PARLIST = 'DX_DY_DZ'                                          979
         CALL AXSHAPE                                                     979
C    *                                                                    979
5991  CALL AGSPOPB                                                        980
      GO TO BLOCK_SAPT, (5990 ,6000)                                      980
C End of Local Procedure                                                  980
C ----------------------------------------------------------------------  981
C ------------ SFCO                                                       981
C Begin Local Procedure SFCO                                              982
C SFCO IS THE CONNECTION BOARD (RECTANGULAR WITH HIROSE CONNECTORS)       982
5620  CONTINUE                                                            982
         AG_TITLE='SFCO'                                                  982
         AG_MODULE=MODULE                                                 982
         AG_MARK='V00'                                                    982
         CALL AGSPUSH                                                     982
         AG_MODULE=MODULE                                                 982
C    *                                                                    982
C    Geant MATERIAL : G10 ===> inherited arguments used <===              983
         CALL AGSRESET                                                    983
         AG_TITLE = 'G10'                                                 983
         AG_EXNAME = 'MATERIAL'                                           983
         AG_PARLIST = 'NONE'                                              983
         CALL AXMATERIAL                                                  983
C    *                                                                    983
C    Geant ATTRIBUTE : SFCO ===> SEEN=1 COLO=30 <===                      983
         CALL AGSRESET                                                    984
         AG_SEEN = 1                                                      984
         AG_COLO = 30                                                     984
         AG_TITLE = 'SFCO'                                                984
         AG_EXNAME = 'ATTRIBUTE'                                          984
         AG_PARLIST = 'SEEN_COLO'                                         984
         CALL AXATTRIBUTE                                                 984
C    *                                                                    984
C    Geant SHAPE : BOX ===> DX=SFJP_SFCO_DX/2. DY=SFJP_SFCO_DY/2., DZ=SF  984
C    JP_SFCO_DZ/2 <===                                                    984
         CALL AGSRESET                                                    986
         AG_DX = SFJP_SFCO_DX/2.                                          986
         AG_DY = SFJP_SFCO_DY/2.                                          986
         AG_DZ = SFJP_SFCO_DZ/2                                           986
         AG_TITLE = 'BOX'                                                 986
         AG_EXNAME = 'SHAPE'                                              986
         AG_PARLIST = 'DX_DY_DZ'                                          986
         CALL AXSHAPE                                                     986
C    *                                                                    986
5621  CALL AGSPOPB                                                        987
      GO TO BLOCK_SFCO, (5620 ,5630)                                      987
C End of Local Procedure                                                  987
C ----------------------------------------------------------------------  988
C ------------ SFCM                                                       988
C Begin Local Procedure SFCM                                              989
C SFCM IS THE MOTHER VOLUME OF THE SECOND CONNECTION BOARD                989
5640  CONTINUE                                                            989
         AG_TITLE='SFCM'                                                  989
         AG_MODULE=MODULE                                                 989
         AG_MARK='V00'                                                    989
         CALL AGSPUSH                                                     989
         AG_MODULE=MODULE                                                 989
C    *                                                                    989
C    Geant MATERIAL : AIR ===> inherited arguments used <===              990
         CALL AGSRESET                                                    990
         AG_TITLE = 'AIR'                                                 990
         AG_EXNAME = 'MATERIAL'                                           990
         AG_PARLIST = 'NONE'                                              990
         CALL AXMATERIAL                                                  990
C    *                                                                    990
C    Geant ATTRIBUTE : SFCM ===> SEEN=0 COLO=6 <===                       990
         CALL AGSRESET                                                    991
         AG_SEEN = 0                                                      991
         AG_COLO = 6                                                      991
         AG_TITLE = 'SFCM'                                                991
         AG_EXNAME = 'ATTRIBUTE'                                          991
         AG_PARLIST = 'SEEN_COLO'                                         991
         CALL AXATTRIBUTE                                                 991
C    *                                                                    991
C    Geant SHAPE : BOX ===> DX=SFJP_SFCM_DX/2. DY=SFJP_SFCO_DY/2., DZ=SF  991
C    JP_SFCO_DZ/2 <===                                                    991
         CALL AGSRESET                                                    993
         AG_DX = SFJP_SFCM_DX/2.                                          993
         AG_DY = SFJP_SFCO_DY/2.                                          993
         AG_DZ = SFJP_SFCO_DZ/2                                           993
         AG_TITLE = 'BOX'                                                 993
         AG_EXNAME = 'SHAPE'                                              993
         AG_PARLIST = 'DX_DY_DZ'                                          993
         CALL AXSHAPE                                                     993
C    *                                                                    993
C    Create SFCB :                                                        994
C    Call To Local Procedure SFCB                                         994
         ASSIGN 6020 TO BLOCK_SFCB                                        994
         GO TO  6010                                                      994
6020     CONTINUE                                                         994
C    *                                                                    994
C    Geant POSITION : SFCB ===> X=+0.5 Y=0 Z=0 <===                       995
         CALL AGSRESET                                                    995
         AG_X = +0.5                                                      995
         AG_Y = 0                                                         995
         AG_Z = 0                                                         995
         AG_TITLE = 'SFCB'                                                995
         AG_EXNAME = 'POSITION'                                           995
         AG_PARLIST = 'X_Y_Z'                                             995
         CALL AXPOSITION                                                  995
C    *                                                                    995
C    Create SFCS :                                                        996
C    Call To Local Procedure SFCS                                         996
         ASSIGN 6040 TO BLOCK_SFCS                                        996
         GO TO  6030                                                      996
6040     CONTINUE                                                         996
C    *                                                                    996
C    Geant POSITION : SFCS ===> X=-1.5 Y=0 Z=-SFJP_SFCO_DZ/2+SFJP_SFCS_D  997
C    Z/2 <===                                                             997
         CALL AGSRESET                                                    997
         AG_X = -1.5                                                      997
         AG_Y = 0                                                         997
         AG_Z = -SFJP_SFCO_DZ/2+SFJP_SFCS_DZ/2                            997
         AG_TITLE = 'SFCS'                                                997
         AG_EXNAME = 'POSITION'                                           997
         AG_PARLIST = 'X_Y_Z'                                             997
         CALL AXPOSITION                                                  997
C    *                                                                    997
5641  CALL AGSPOPB                                                        998
      GO TO BLOCK_SFCM, (5640 ,5650)                                      998
C End of Local Procedure                                                  998
C ----------------------------------------------------------------------  999
C ------------ SFCB                                                       999
C Begin Local Procedure SFCB                                              1000
C SFCB IS THE BIG PART OF THE SECOND CONNECTION BOARD                     1000
6010  CONTINUE                                                            1000
         AG_TITLE='SFCB'                                                  1000
         AG_MODULE=MODULE                                                 1000
         AG_MARK='V00'                                                    1000
         CALL AGSPUSH                                                     1000
         AG_MODULE=MODULE                                                 1000
C    *                                                                    1000
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1001
         CALL AGSRESET                                                    1001
         AG_TITLE = 'G10'                                                 1001
         AG_EXNAME = 'MATERIAL'                                           1001
         AG_PARLIST = 'NONE'                                              1001
         CALL AXMATERIAL                                                  1001
C    *                                                                    1001
C    Geant ATTRIBUTE : SFCB ===> SEEN=1 COLO=30 <===                      1001
         CALL AGSRESET                                                    1002
         AG_SEEN = 1                                                      1002
         AG_COLO = 30                                                     1002
         AG_TITLE = 'SFCB'                                                1002
         AG_EXNAME = 'ATTRIBUTE'                                          1002
         AG_PARLIST = 'SEEN_COLO'                                         1002
         CALL AXATTRIBUTE                                                 1002
C    *                                                                    1002
C    Geant SHAPE : BOX ===> DX=SFJP_SFCB_DX/2. DY=SFJP_SFCO_DY/2., DZ=SF  1002
C    JP_SFCO_DZ/2 <===                                                    1002
         CALL AGSRESET                                                    1004
         AG_DX = SFJP_SFCB_DX/2.                                          1004
         AG_DY = SFJP_SFCO_DY/2.                                          1004
         AG_DZ = SFJP_SFCO_DZ/2                                           1004
         AG_TITLE = 'BOX'                                                 1004
         AG_EXNAME = 'SHAPE'                                              1004
         AG_PARLIST = 'DX_DY_DZ'                                          1004
         CALL AXSHAPE                                                     1004
C    *                                                                    1004
6011  CALL AGSPOPB                                                        1005
      GO TO BLOCK_SFCB, (6010 ,6020)                                      1005
C End of Local Procedure                                                  1005
C ----------------------------------------------------------------------  1006
C ------------ SFCS                                                       1006
C Begin Local Procedure SFCS                                              1007
C SFCS IS THE BIG PART OF THE SECOND CONNECTION BOARD                     1007
6030  CONTINUE                                                            1007
         AG_TITLE='SFCS'                                                  1007
         AG_MODULE=MODULE                                                 1007
         AG_MARK='V00'                                                    1007
         CALL AGSPUSH                                                     1007
         AG_MODULE=MODULE                                                 1007
C    *                                                                    1007
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1008
         CALL AGSRESET                                                    1008
         AG_TITLE = 'G10'                                                 1008
         AG_EXNAME = 'MATERIAL'                                           1008
         AG_PARLIST = 'NONE'                                              1008
         CALL AXMATERIAL                                                  1008
C    *                                                                    1008
C    Geant ATTRIBUTE : SFCS ===> SEEN=1 COLO=30 <===                      1008
         CALL AGSRESET                                                    1009
         AG_SEEN = 1                                                      1009
         AG_COLO = 30                                                     1009
         AG_TITLE = 'SFCS'                                                1009
         AG_EXNAME = 'ATTRIBUTE'                                          1009
         AG_PARLIST = 'SEEN_COLO'                                         1009
         CALL AXATTRIBUTE                                                 1009
C    *                                                                    1009
C    Geant SHAPE : BOX ===> DX=(SFJP_SFCM_DX-SFJP_SFCB_DX)/2., DY=SFJP_S  1009
C    FCO_DY/2. DZ=SFJP_SFCS_DZ/2 <===                                     1009
         CALL AGSRESET                                                    1011
         AG_DX = (SFJP_SFCM_DX-SFJP_SFCB_DX)/2.                           1011
         AG_DY = SFJP_SFCO_DY/2.                                          1011
         AG_DZ = SFJP_SFCS_DZ/2                                           1011
         AG_TITLE = 'BOX'                                                 1011
         AG_EXNAME = 'SHAPE'                                              1011
         AG_PARLIST = 'DX_DY_DZ'                                          1011
         CALL AXSHAPE                                                     1011
C    *                                                                    1011
6031  CALL AGSPOPB                                                        1012
      GO TO BLOCK_SFCS, (6030 ,6040)                                      1012
C End of Local Procedure                                                  1012
C ----------------------------------------------------------------------  1013
C ------------ SFKF                                                       1013
C Begin Local Procedure SFKF                                              1014
C SFKF IS THE FIRST PART OF THE KAPTON FLEX CIRCUIT                       1014
5660  CONTINUE                                                            1014
         AG_TITLE='SFKF'                                                  1014
         AG_MODULE=MODULE                                                 1014
         AG_MARK='V00'                                                    1014
         CALL AGSPUSH                                                     1014
         AG_MODULE=MODULE                                                 1014
C    *                                                                    1014
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1015
         CALL AGSRESET                                                    1015
         AG_TITLE = 'G10'                                                 1015
         AG_EXNAME = 'MATERIAL'                                           1015
         AG_PARLIST = 'NONE'                                              1015
         CALL AXMATERIAL                                                  1015
C    *                                                                    1015
C    Geant ATTRIBUTE : SFKF ===> SEEN=1 COLO=30 <===                      1015
         CALL AGSRESET                                                    1016
         AG_SEEN = 1                                                      1016
         AG_COLO = 30                                                     1016
         AG_TITLE = 'SFKF'                                                1016
         AG_EXNAME = 'ATTRIBUTE'                                          1016
         AG_PARLIST = 'SEEN_COLO'                                         1016
         CALL AXATTRIBUTE                                                 1016
C    *                                                                    1016
C    Geant SHAPE : BOX ===> DX=SFJP_SFKF_DX/2. DY=SFJP_SFKF_DY/2., DZ=SF  1016
C    JP_SFKF_DZ/2 <===                                                    1016
         CALL AGSRESET                                                    1018
         AG_DX = SFJP_SFKF_DX/2.                                          1018
         AG_DY = SFJP_SFKF_DY/2.                                          1018
         AG_DZ = SFJP_SFKF_DZ/2                                           1018
         AG_TITLE = 'BOX'                                                 1018
         AG_EXNAME = 'SHAPE'                                              1018
         AG_PARLIST = 'DX_DY_DZ'                                          1018
         CALL AXSHAPE                                                     1018
C    *                                                                    1018
5661  CALL AGSPOPB                                                        1019
      GO TO BLOCK_SFKF, (5660 ,5670)                                      1019
C End of Local Procedure                                                  1019
C ----------------------------------------------------------------------  1020
C ------------ SFKS                                                       1020
C Begin Local Procedure SFKS                                              1021
C SFKS IS THE SECOND PART OF THE KAPTON FLEX CIRCUIT                      1021
5680  CONTINUE                                                            1021
         AG_TITLE='SFKS'                                                  1021
         AG_MODULE=MODULE                                                 1021
         AG_MARK='V00'                                                    1021
         CALL AGSPUSH                                                     1021
         AG_MODULE=MODULE                                                 1021
C    *                                                                    1021
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1022
         CALL AGSRESET                                                    1022
         AG_TITLE = 'G10'                                                 1022
         AG_EXNAME = 'MATERIAL'                                           1022
         AG_PARLIST = 'NONE'                                              1022
         CALL AXMATERIAL                                                  1022
C    *                                                                    1022
C    Geant ATTRIBUTE : SFKS ===> SEEN=1 COLO=30 <===                      1022
         CALL AGSRESET                                                    1023
         AG_SEEN = 1                                                      1023
         AG_COLO = 30                                                     1023
         AG_TITLE = 'SFKS'                                                1023
         AG_EXNAME = 'ATTRIBUTE'                                          1023
         AG_PARLIST = 'SEEN_COLO'                                         1023
         CALL AXATTRIBUTE                                                 1023
C    *                                                                    1023
C    Geant SHAPE : BOX ===> DX=SFJP_SFKS_DX/2. DY=SFJP_SFKF_DY/2., DZ=SF  1023
C    JP_SFKF_DZ/2 <===                                                    1023
         CALL AGSRESET                                                    1025
         AG_DX = SFJP_SFKS_DX/2.                                          1025
         AG_DY = SFJP_SFKF_DY/2.                                          1025
         AG_DZ = SFJP_SFKF_DZ/2                                           1025
         AG_TITLE = 'BOX'                                                 1025
         AG_EXNAME = 'SHAPE'                                              1025
         AG_PARLIST = 'DX_DY_DZ'                                          1025
         CALL AXSHAPE                                                     1025
C    *                                                                    1025
5681  CALL AGSPOPB                                                        1026
      GO TO BLOCK_SFKS, (5680 ,5690)                                      1026
C End of Local Procedure                                                  1026
C ----------------------------------------------------------------------  1027
C ------------ SFPR                                                       1027
C Begin Local Procedure SFPR                                              1029
C SFPR IS THE LADDER END INSIDE MECHANICAL PART (PRISM WITH G10 WITH, HA  1029
C LF DENSITY)                                                             1029
5700  CONTINUE                                                            1029
         AG_TITLE='SFPR'                                                  1029
         AG_MODULE=MODULE                                                 1029
         AG_MARK='V00'                                                    1029
         CALL AGSPUSH                                                     1029
         AG_MODULE=MODULE                                                 1029
C    *                                                                    1029
C    Geant MATERIAL : G5 ===> inherited arguments used <===               1030
         CALL AGSRESET                                                    1030
         AG_TITLE = 'G5'                                                  1030
         AG_EXNAME = 'MATERIAL'                                           1030
         AG_PARLIST = 'NONE'                                              1030
         CALL AXMATERIAL                                                  1030
C    *                                                                    1030
C    Geant ATTRIBUTE : SFPR ===> SEEN=1 COLO=4 <===                       1030
         CALL AGSRESET                                                    1031
         AG_SEEN = 1                                                      1031
         AG_COLO = 4                                                      1031
         AG_TITLE = 'SFPR'                                                1031
         AG_EXNAME = 'ATTRIBUTE'                                          1031
         AG_PARLIST = 'SEEN_COLO'                                         1031
         CALL AXATTRIBUTE                                                 1031
C    *                                                                    1031
C    Geant SHAPE : PGON ===> PHI1=-126 DPHI=72 NPDIV=1 NZ=2, ZI={0,3.7}   1031
C    RMN={0,0} RMX={3.48,3.48} <===                                       1031
         CALL AGSRESET                                                    1033
         AG_PHI1 = -126                                                   1033
         AG_DPHI = 72                                                     1033
         AG_NPDIV = 1                                                     1033
         AG_NZ = 2                                                        1033
         AG_ZI(1)=0                                                       1033
         AG_ZI(2)=3.7                                                     1033
         AG_RMN(1)=0                                                      1033
         AG_RMN(2)=0                                                      1033
         AG_RMX(1)=3.48                                                   1033
         AG_RMX(2)=3.48                                                   1033
         AG_TITLE = 'PGON'                                                1033
         AG_EXNAME = 'SHAPE'                                              1033
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1033
         CALL AXSHAPE                                                     1033
C    *                                                                    1033
5701  CALL AGSPOPB                                                        1034
      GO TO BLOCK_SFPR, (5700 ,5710)                                      1034
C End of Local Procedure                                                  1034
C ----------------------------------------------------------------------  1035
C ------------ SFPB                                                       1035
C Begin Local Procedure SFPB                                              1036
C SFPB IS THE LADDER END OUTSIDE MECHANICAL PART (RECTANGLE WITH G10)     1036
5720  CONTINUE                                                            1036
         AG_TITLE='SFPB'                                                  1036
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
C    Geant ATTRIBUTE : SFPB ===> SEEN=1 COLO=4 <===                       1037
         CALL AGSRESET                                                    1038
         AG_SEEN = 1                                                      1038
         AG_COLO = 4                                                      1038
         AG_TITLE = 'SFPB'                                                1038
         AG_EXNAME = 'ATTRIBUTE'                                          1038
         AG_PARLIST = 'SEEN_COLO'                                         1038
         CALL AXATTRIBUTE                                                 1038
C    *                                                                    1038
C    Geant SHAPE : BOX ===> DX=SFJP_SFPB_DX/2. DY=SFJP_SFPB_DY/2., DZ=SF  1038
C    JP_SFPBDZ/2. <===                                                    1038
         CALL AGSRESET                                                    1040
         AG_DX = SFJP_SFPB_DX/2.                                          1040
         AG_DY = SFJP_SFPB_DY/2.                                          1040
         AG_DZ = SFJP_SFPBDZ/2.                                           1040
         AG_TITLE = 'BOX'                                                 1040
         AG_EXNAME = 'SHAPE'                                              1040
         AG_PARLIST = 'DX_DY_DZ'                                          1040
         CALL AXSHAPE                                                     1040
C    *                                                                    1040
5721  CALL AGSPOPB                                                        1041
      GO TO BLOCK_SFPB, (5720 ,5730)                                      1041
C End of Local Procedure                                                  1041
C ----------------------------------------------------------------------  1042
C ------------ SSBS                                                       1042
C Begin Local Procedure SSBS                                              1044
C SSBS IS THE SMALL PART OF THE ALUMINIUM PLATE LINKING THE LADDER TO, T  1044
C HE SECTOR                                                               1044
5740  CONTINUE                                                            1044
         AG_TITLE='SSBS'                                                  1044
         AG_MODULE=MODULE                                                 1044
         AG_MARK='V00'                                                    1044
         CALL AGSPUSH                                                     1044
         AG_MODULE=MODULE                                                 1044
C    *                                                                    1044
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1045
         CALL AGSRESET                                                    1045
         AG_TITLE = 'ALUMINIUM'                                           1045
         AG_EXNAME = 'MATERIAL'                                           1045
         AG_PARLIST = 'NONE'                                              1045
         CALL AXMATERIAL                                                  1045
C    *                                                                    1045
C    Geant ATTRIBUTE : SSBS ===> SEEN=1 COLO=1 <===                       1045
         CALL AGSRESET                                                    1046
         AG_SEEN = 1                                                      1046
         AG_COLO = 1                                                      1046
         AG_TITLE = 'SSBS'                                                1046
         AG_EXNAME = 'ATTRIBUTE'                                          1046
         AG_PARLIST = 'SEEN_COLO'                                         1046
         CALL AXATTRIBUTE                                                 1046
C    *                                                                    1046
C    Geant SHAPE : BOX ===> DX=SFJP_SSBS_DX/2. DY=SFJP_SSBS_DY/2., DZ=SF  1046
C    JP_SSBS_DX/2. <===                                                   1046
         CALL AGSRESET                                                    1048
         AG_DX = SFJP_SSBS_DX/2.                                          1048
         AG_DY = SFJP_SSBS_DY/2.                                          1048
         AG_DZ = SFJP_SSBS_DX/2.                                          1048
         AG_TITLE = 'BOX'                                                 1048
         AG_EXNAME = 'SHAPE'                                              1048
         AG_PARLIST = 'DX_DY_DZ'                                          1048
         CALL AXSHAPE                                                     1048
C    *                                                                    1048
5741  CALL AGSPOPB                                                        1049
      GO TO BLOCK_SSBS, (5740 ,5750)                                      1049
C End of Local Procedure                                                  1049
C ----------------------------------------------------------------------  1050
C ------------ SSBB                                                       1050
C Begin Local Procedure SSBB                                              1052
C SSBB IS THE BIG PART OF THE ALUMINIUM PLATE LINKING THE LADDER TO THE,  1052
C  SECTOR                                                                 1052
5760  CONTINUE                                                            1052
         AG_TITLE='SSBB'                                                  1052
         AG_MODULE=MODULE                                                 1052
         AG_MARK='V00'                                                    1052
         CALL AGSPUSH                                                     1052
         AG_MODULE=MODULE                                                 1052
C    *                                                                    1052
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1053
         CALL AGSRESET                                                    1053
         AG_TITLE = 'ALUMINIUM'                                           1053
         AG_EXNAME = 'MATERIAL'                                           1053
         AG_PARLIST = 'NONE'                                              1053
         CALL AXMATERIAL                                                  1053
C    *                                                                    1053
C    Geant ATTRIBUTE : SSBB ===> SEEN=1 COLO=1 <===                       1053
         CALL AGSRESET                                                    1054
         AG_SEEN = 1                                                      1054
         AG_COLO = 1                                                      1054
         AG_TITLE = 'SSBB'                                                1054
         AG_EXNAME = 'ATTRIBUTE'                                          1054
         AG_PARLIST = 'SEEN_COLO'                                         1054
         CALL AXATTRIBUTE                                                 1054
C    *                                                                    1054
C    Geant SHAPE : BOX ===> DX=SFJP_SSBB_DX/2. DY=SFJP_SSBS_DY/2., DZ=SF  1054
C    JP_SSBB_DZ/2. <===                                                   1054
         CALL AGSRESET                                                    1056
         AG_DX = SFJP_SSBB_DX/2.                                          1056
         AG_DY = SFJP_SSBS_DY/2.                                          1056
         AG_DZ = SFJP_SSBB_DZ/2.                                          1056
         AG_TITLE = 'BOX'                                                 1056
         AG_EXNAME = 'SHAPE'                                              1056
         AG_PARLIST = 'DX_DY_DZ'                                          1056
         CALL AXSHAPE                                                     1056
C    *                                                                    1056
5761  CALL AGSPOPB                                                        1057
      GO TO BLOCK_SSBB, (5760 ,5770)                                      1057
C End of Local Procedure                                                  1057
C ----------------------------------------------------------------------  1058
C ------------ SSST                                                       1058
C Begin Local Procedure SSST                                              1059
C SSST IS THE TOP OF THE SMALL SECTOR                                     1059
5070  CONTINUE                                                            1059
         AG_TITLE='SSST'                                                  1059
         AG_MODULE=MODULE                                                 1059
         AG_MARK='V00'                                                    1059
         CALL AGSPUSH                                                     1059
         AG_MODULE=MODULE                                                 1059
C    *                                                                    1059
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1060
         CALL AGSRESET                                                    1060
         AG_TITLE = 'ALUMINIUM'                                           1060
         AG_EXNAME = 'MATERIAL'                                           1060
         AG_PARLIST = 'NONE'                                              1060
         CALL AXMATERIAL                                                  1060
C    *                                                                    1060
C    Geant ATTRIBUTE : SSST ===> SEEN=1 COLO=1 <===                       1060
         CALL AGSRESET                                                    1061
         AG_SEEN = 1                                                      1061
         AG_COLO = 1                                                      1061
         AG_TITLE = 'SSST'                                                1061
         AG_EXNAME = 'ATTRIBUTE'                                          1061
         AG_PARLIST = 'SEEN_COLO'                                         1061
         CALL AXATTRIBUTE                                                 1061
C    *                                                                    1061
C    Geant SHAPE : TUBS ===> RMIN=SFJP_SSST_RMIN RMAX=SFJP_SSST_RMAX, PH  1061
C    I1=SFJP_ALPHAZH PHI2=(180.-SFJP_ALPHAZH), DZ=SFJP_SSST_WIDTH/2. <==  1061
C    =                                                                    1061
         CALL AGSRESET                                                    1064
         AG_RMIN = SFJP_SSST_RMIN                                         1064
         AG_RMAX = SFJP_SSST_RMAX                                         1064
         AG_PHI1 = SFJP_ALPHAZH                                           1064
         AG_PHI2 = (180.-SFJP_ALPHAZH)                                    1064
         AG_DZ = SFJP_SSST_WIDTH/2.                                       1064
         AG_TITLE = 'TUBS'                                                1064
         AG_EXNAME = 'SHAPE'                                              1064
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1064
         CALL AXSHAPE                                                     1064
C    *                                                                    1064
5071  CALL AGSPOPB                                                        1065
      GO TO BLOCK_SSST, (5070 ,5080)                                      1065
C End of Local Procedure                                                  1065
C ----------------------------------------------------------------------  1066
C ------------ SSSS                                                       1066
C Begin Local Procedure SSSS                                              1067
C SSSS IS THE SIDE OF THE SMALL SECTOR                                    1067
5090  CONTINUE                                                            1067
         AG_TITLE='SSSS'                                                  1067
         AG_MODULE=MODULE                                                 1067
         AG_MARK='V00'                                                    1067
         CALL AGSPUSH                                                     1067
         AG_MODULE=MODULE                                                 1067
C    *                                                                    1067
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1068
         CALL AGSRESET                                                    1068
         AG_TITLE = 'ALUMINIUM'                                           1068
         AG_EXNAME = 'MATERIAL'                                           1068
         AG_PARLIST = 'NONE'                                              1068
         CALL AXMATERIAL                                                  1068
C    *                                                                    1068
C    Geant ATTRIBUTE : SSSS ===> SEEN=1 COLO=1 <===                       1068
         CALL AGSRESET                                                    1069
         AG_SEEN = 1                                                      1069
         AG_COLO = 1                                                      1069
         AG_TITLE = 'SSSS'                                                1069
         AG_EXNAME = 'ATTRIBUTE'                                          1069
         AG_PARLIST = 'SEEN_COLO'                                         1069
         CALL AXATTRIBUTE                                                 1069
C    *                                                                    1069
C    Geant SHAPE : TUBS ===> RMIN=SFJP_SSSS_RMIN RMAX=SFJP_SSST_RMIN, PH  1069
C    I1=SFJP_ALPHAZH PHI2=(180.-SFJP_ALPHAZH), DZ=SFJP_SSSS_WIDTH/2. <==  1069
C    =                                                                    1069
         CALL AGSRESET                                                    1072
         AG_RMIN = SFJP_SSSS_RMIN                                         1072
         AG_RMAX = SFJP_SSST_RMIN                                         1072
         AG_PHI1 = SFJP_ALPHAZH                                           1072
         AG_PHI2 = (180.-SFJP_ALPHAZH)                                    1072
         AG_DZ = SFJP_SSSS_WIDTH/2.                                       1072
         AG_TITLE = 'TUBS'                                                1072
         AG_EXNAME = 'SHAPE'                                              1072
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1072
         CALL AXSHAPE                                                     1072
C    *                                                                    1072
5091  CALL AGSPOPB                                                        1073
      GO TO BLOCK_SSSS, (5090 ,5100)                                      1073
C End of Local Procedure                                                  1073
C ----------------------------------------------------------------------  1074
C ------------ SSRT                                                       1074
C Begin Local Procedure SSRT                                              1075
C SSRT IS THE TOP OF THE SIDE RIB                                         1075
5110  CONTINUE                                                            1075
         AG_TITLE='SSRT'                                                  1075
         AG_MODULE=MODULE                                                 1075
         AG_MARK='V00'                                                    1075
         CALL AGSPUSH                                                     1075
         AG_MODULE=MODULE                                                 1075
C    *                                                                    1075
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1076
         CALL AGSRESET                                                    1076
         AG_TITLE = 'ALUMINIUM'                                           1076
         AG_EXNAME = 'MATERIAL'                                           1076
         AG_PARLIST = 'NONE'                                              1076
         CALL AXMATERIAL                                                  1076
C    *                                                                    1076
C    Geant ATTRIBUTE : SSRT ===> SEEN=1 COLO=1 <===                       1076
         CALL AGSRESET                                                    1077
         AG_SEEN = 1                                                      1077
         AG_COLO = 1                                                      1077
         AG_TITLE = 'SSRT'                                                1077
         AG_EXNAME = 'ATTRIBUTE'                                          1077
         AG_PARLIST = 'SEEN_COLO'                                         1077
         CALL AXATTRIBUTE                                                 1077
C    *                                                                    1077
C    Geant SHAPE : TUBS ===> RMIN=SFJP_SSST_RMIN RMAX=SFJP_SSST_RMAX, PH  1077
C    I1=-SFJP_ALPHAZH PHI2=+SFJP_ALPHAZH, DZ=SFJP_SSST_WIDTH/2. <===      1077
         CALL AGSRESET                                                    1080
         AG_RMIN = SFJP_SSST_RMIN                                         1080
         AG_RMAX = SFJP_SSST_RMAX                                         1080
         AG_PHI1 = -SFJP_ALPHAZH                                          1080
         AG_PHI2 = +SFJP_ALPHAZH                                          1080
         AG_DZ = SFJP_SSST_WIDTH/2.                                       1080
         AG_TITLE = 'TUBS'                                                1080
         AG_EXNAME = 'SHAPE'                                              1080
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1080
         CALL AXSHAPE                                                     1080
C    *                                                                    1080
5111  CALL AGSPOPB                                                        1081
      GO TO BLOCK_SSRT, (5110 ,5120)                                      1081
C End of Local Procedure                                                  1081
C ----------------------------------------------------------------------  1082
C ------------ SSRS                                                       1082
C Begin Local Procedure SSRS                                              1083
C SSRS IS THE SIDE OF THE SMALL RIB                                       1083
5130  CONTINUE                                                            1083
         AG_TITLE='SSRS'                                                  1083
         AG_MODULE=MODULE                                                 1083
         AG_MARK='V00'                                                    1083
         CALL AGSPUSH                                                     1083
         AG_MODULE=MODULE                                                 1083
C    *                                                                    1083
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1084
         CALL AGSRESET                                                    1084
         AG_TITLE = 'ALUMINIUM'                                           1084
         AG_EXNAME = 'MATERIAL'                                           1084
         AG_PARLIST = 'NONE'                                              1084
         CALL AXMATERIAL                                                  1084
C    *                                                                    1084
C    Geant ATTRIBUTE : SSRS ===> SEEN=1 COLO=1 <===                       1084
         CALL AGSRESET                                                    1085
         AG_SEEN = 1                                                      1085
         AG_COLO = 1                                                      1085
         AG_TITLE = 'SSRS'                                                1085
         AG_EXNAME = 'ATTRIBUTE'                                          1085
         AG_PARLIST = 'SEEN_COLO'                                         1085
         CALL AXATTRIBUTE                                                 1085
C    *                                                                    1085
C    Geant SHAPE : TUBS ===> RMIN=SFJP_SSRS_RMIN RMAX=SFJP_SSST_RMIN, PH  1085
C    I1=-SFJP_ALPHAZH PHI2=+SFJP_ALPHAZH, DZ=SFJP_SSSS_WIDTH/2. <===      1085
         CALL AGSRESET                                                    1088
         AG_RMIN = SFJP_SSRS_RMIN                                         1088
         AG_RMAX = SFJP_SSST_RMIN                                         1088
         AG_PHI1 = -SFJP_ALPHAZH                                          1088
         AG_PHI2 = +SFJP_ALPHAZH                                          1088
         AG_DZ = SFJP_SSSS_WIDTH/2.                                       1088
         AG_TITLE = 'TUBS'                                                1088
         AG_EXNAME = 'SHAPE'                                              1088
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1088
         CALL AXSHAPE                                                     1088
C    *                                                                    1088
5131  CALL AGSPOPB                                                        1089
      GO TO BLOCK_SSRS, (5130 ,5140)                                      1089
C End of Local Procedure                                                  1089
C ----------------------------------------------------------------------  1090
C ------------ SSLB                                                       1090
C Begin Local Procedure SSLB                                              1091
C SSLB IS THE LINKING (SECTOR TO THE CONE) BOX                            1091
5150  CONTINUE                                                            1091
         AG_TITLE='SSLB'                                                  1091
         AG_MODULE=MODULE                                                 1091
         AG_MARK='V00'                                                    1091
         CALL AGSPUSH                                                     1091
         AG_MODULE=MODULE                                                 1091
C    *                                                                    1091
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1092
         CALL AGSRESET                                                    1092
         AG_TITLE = 'AIR'                                                 1092
         AG_EXNAME = 'MATERIAL'                                           1092
         AG_PARLIST = 'NONE'                                              1092
         CALL AXMATERIAL                                                  1092
C    *                                                                    1092
C    Geant ATTRIBUTE : SSLB ===> SEEN=0 COLO=6 <===                       1092
         CALL AGSRESET                                                    1093
         AG_SEEN = 0                                                      1093
         AG_COLO = 6                                                      1093
         AG_TITLE = 'SSLB'                                                1093
         AG_EXNAME = 'ATTRIBUTE'                                          1093
         AG_PARLIST = 'SEEN_COLO'                                         1093
         CALL AXATTRIBUTE                                                 1093
C    *                                                                    1093
C    Geant SHAPE : BOX ===> DX=SFJP_SSLB_DX/2. DY=SFJP_SSLB_DY/2., DZ=SF  1093
C    JP_SSLB_DZ/2. <===                                                   1093
         CALL AGSRESET                                                    1095
         AG_DX = SFJP_SSLB_DX/2.                                          1095
         AG_DY = SFJP_SSLB_DY/2.                                          1095
         AG_DZ = SFJP_SSLB_DZ/2.                                          1095
         AG_TITLE = 'BOX'                                                 1095
         AG_EXNAME = 'SHAPE'                                              1095
         AG_PARLIST = 'DX_DY_DZ'                                          1095
         CALL AXSHAPE                                                     1095
C    *                                                                    1095
C    Create SBCH :                                                        1096
C    Call To Local Procedure SBCH                                         1096
         ASSIGN 6060 TO BLOCK_SBCH                                        1096
         GO TO  6050                                                      1096
6060     CONTINUE                                                         1096
C    *                                                                    1096
C    Geant POSITION : SBCH ===> X=0 Y=0 Z=SFJP_SSLB_DZ/2.-SFJP_SSCR_THD/  1097
C    2. <===                                                              1097
         CALL AGSRESET                                                    1097
         AG_X = 0                                                         1097
         AG_Y = 0                                                         1097
         AG_Z = SFJP_SSLB_DZ/2.-SFJP_SSCR_THD/2.                          1097
         AG_TITLE = 'SBCH'                                                1097
         AG_EXNAME = 'POSITION'                                           1097
         AG_PARLIST = 'X_Y_Z'                                             1097
         CALL AXPOSITION                                                  1097
C    *                                                                    1097
C    Create SBCV :                                                        1098
C    Call To Local Procedure SBCV                                         1098
         ASSIGN 6080 TO BLOCK_SBCV                                        1098
         GO TO  6070                                                      1098
6080     CONTINUE                                                         1098
C    *                                                                    1098
C    Geant POSITION : SBCV ===> X=0 Y=(SFJP_SSCR_WD+SFJP_SSCR_VL)/2 Z=SF  1100
C    JP_SSLB_DZ/2.-SFJP_SSCR_THD/2. <===                                  1100
         CALL AGSRESET                                                    1100
         AG_X = 0                                                         1100
         AG_Y = (SFJP_SSCR_WD+SFJP_SSCR_VL)/2                             1100
         AG_Z = SFJP_SSLB_DZ/2.-SFJP_SSCR_THD/2.                          1100
         AG_TITLE = 'SBCV'                                                1100
         AG_EXNAME = 'POSITION'                                           1100
         AG_PARLIST = 'X_Y_Z'                                             1100
         CALL AXPOSITION                                                  1100
C    *                                                                    1100
C    Geant POSITION : SBCV ===> X=0 Y=-(SFJP_SSCR_WD+SFJP_SSCR_VL)/2 Z=S  1100
C    FJP_SSLB_DZ/2.-SFJP_SSCR_THD/2. <===                                 1100
         CALL AGSRESET                                                    1102
         AG_X = 0                                                         1102
         AG_Y = -(SFJP_SSCR_WD+SFJP_SSCR_VL)/2                            1102
         AG_Z = SFJP_SSLB_DZ/2.-SFJP_SSCR_THD/2.                          1102
         AG_TITLE = 'SBCV'                                                1102
         AG_EXNAME = 'POSITION'                                           1102
         AG_PARLIST = 'X_Y_Z'                                             1102
         CALL AXPOSITION                                                  1102
C    *                                                                    1102
C    Create SBFH :                                                        1103
C    Call To Local Procedure SBFH                                         1103
         ASSIGN 6100 TO BLOCK_SBFH                                        1103
         GO TO  6090                                                      1103
6100     CONTINUE                                                         1103
C    *                                                                    1103
C    Geant POSITION : SBFH ===> X=0 Y=0 Z=SFJP_SSLB_DZ/2.-SFJP_SSCR_THD-  1104
C    SFJP_SSCR_THA/2. <===                                                1104
         CALL AGSRESET                                                    1104
         AG_X = 0                                                         1104
         AG_Y = 0                                                         1104
         AG_Z = SFJP_SSLB_DZ/2.-SFJP_SSCR_THD-SFJP_SSCR_THA/2.            1104
         AG_TITLE = 'SBFH'                                                1104
         AG_EXNAME = 'POSITION'                                           1104
         AG_PARLIST = 'X_Y_Z'                                             1104
         CALL AXPOSITION                                                  1104
C    *                                                                    1104
C    Create SBFV :                                                        1105
C    Call To Local Procedure SBFV                                         1105
         ASSIGN 6120 TO BLOCK_SBFV                                        1105
         GO TO  6110                                                      1105
6120     CONTINUE                                                         1105
C    *                                                                    1105
C    Geant POSITION : SBFV ===> X=0 Y=(SFJP_SSCR_WD+SFJP_SSCR_VL)/2 Z=SF  1107
C    JP_SSLB_DZ/2.-SFJP_SSCR_THD-SFJP_SSCR_THA/2. <===                    1107
         CALL AGSRESET                                                    1107
         AG_X = 0                                                         1107
         AG_Y = (SFJP_SSCR_WD+SFJP_SSCR_VL)/2                             1107
         AG_Z = SFJP_SSLB_DZ/2.-SFJP_SSCR_THD-SFJP_SSCR_THA/2.            1107
         AG_TITLE = 'SBFV'                                                1107
         AG_EXNAME = 'POSITION'                                           1107
         AG_PARLIST = 'X_Y_Z'                                             1107
         CALL AXPOSITION                                                  1107
C    *                                                                    1107
C    Geant POSITION : SBFV ===> X=0 Y=-(SFJP_SSCR_WD+SFJP_SSCR_VL)/2 Z=S  1107
C    FJP_SSLB_DZ/2.-SFJP_SSCR_THD-SFJP_SSCR_THA/2. <===                   1107
         CALL AGSRESET                                                    1109
         AG_X = 0                                                         1109
         AG_Y = -(SFJP_SSCR_WD+SFJP_SSCR_VL)/2                            1109
         AG_Z = SFJP_SSLB_DZ/2.-SFJP_SSCR_THD-SFJP_SSCR_THA/2.            1109
         AG_TITLE = 'SBFV'                                                1109
         AG_EXNAME = 'POSITION'                                           1109
         AG_PARLIST = 'X_Y_Z'                                             1109
         CALL AXPOSITION                                                  1109
C    *                                                                    1109
5151  CALL AGSPOPB                                                        1110
      GO TO BLOCK_SSLB, (5150 ,5160)                                      1110
C End of Local Procedure                                                  1110
C ----------------------------------------------------------------------  1111
C ------------ SBCH                                                       1111
C Begin Local Procedure SBCH                                              1112
C SBCH IS THE HORIZONTAL BRANCH OF THE DELRIN CROSS                       1112
6050  CONTINUE                                                            1112
         AG_TITLE='SBCH'                                                  1112
         AG_MODULE=MODULE                                                 1112
         AG_MARK='V00'                                                    1112
         CALL AGSPUSH                                                     1112
         AG_MODULE=MODULE                                                 1112
C    *                                                                    1112
C    Geant MATERIAL : DELRIN ===> inherited arguments used <===           1113
         CALL AGSRESET                                                    1113
         AG_TITLE = 'DELRIN'                                              1113
         AG_EXNAME = 'MATERIAL'                                           1113
         AG_PARLIST = 'NONE'                                              1113
         CALL AXMATERIAL                                                  1113
C    *                                                                    1113
C    Geant ATTRIBUTE : SBCH ===> SEEN=1 COLO=2 <===                       1113
         CALL AGSRESET                                                    1114
         AG_SEEN = 1                                                      1114
         AG_COLO = 2                                                      1114
         AG_TITLE = 'SBCH'                                                1114
         AG_EXNAME = 'ATTRIBUTE'                                          1114
         AG_PARLIST = 'SEEN_COLO'                                         1114
         CALL AXATTRIBUTE                                                 1114
C    *                                                                    1114
C    Geant SHAPE : BOX ===> DX=SFJP_SSCR_HL/2. DY=SFJP_SSCR_WD/2., DZ=SF  1114
C    JP_SSCR_THD/2. <===                                                  1114
         CALL AGSRESET                                                    1116
         AG_DX = SFJP_SSCR_HL/2.                                          1116
         AG_DY = SFJP_SSCR_WD/2.                                          1116
         AG_DZ = SFJP_SSCR_THD/2.                                         1116
         AG_TITLE = 'BOX'                                                 1116
         AG_EXNAME = 'SHAPE'                                              1116
         AG_PARLIST = 'DX_DY_DZ'                                          1116
         CALL AXSHAPE                                                     1116
C    *                                                                    1116
6051  CALL AGSPOPB                                                        1117
      GO TO BLOCK_SBCH, (6050 ,6060)                                      1117
C End of Local Procedure                                                  1117
C ----------------------------------------------------------------------  1118
C ------------ SBFH                                                       1118
C Begin Local Procedure SBFH                                              1119
C SBFH IS THE HORIZONTAL BRANCH OF THE ALUMIMIUM CROSS                    1119
6090  CONTINUE                                                            1119
         AG_TITLE='SBFH'                                                  1119
         AG_MODULE=MODULE                                                 1119
         AG_MARK='V00'                                                    1119
         CALL AGSPUSH                                                     1119
         AG_MODULE=MODULE                                                 1119
C    *                                                                    1119
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1120
         CALL AGSRESET                                                    1120
         AG_TITLE = 'ALUMINIUM'                                           1120
         AG_EXNAME = 'MATERIAL'                                           1120
         AG_PARLIST = 'NONE'                                              1120
         CALL AXMATERIAL                                                  1120
C    *                                                                    1120
C    Geant ATTRIBUTE : SBFH ===> SEEN=1 COLO=1 <===                       1120
         CALL AGSRESET                                                    1121
         AG_SEEN = 1                                                      1121
         AG_COLO = 1                                                      1121
         AG_TITLE = 'SBFH'                                                1121
         AG_EXNAME = 'ATTRIBUTE'                                          1121
         AG_PARLIST = 'SEEN_COLO'                                         1121
         CALL AXATTRIBUTE                                                 1121
C    *                                                                    1121
C    Geant SHAPE : BOX ===> DX=SFJP_SSCR_HL/2. DY=SFJP_SSCR_WD/2., DZ=SF  1121
C    JP_SSCR_THA/2. <===                                                  1121
         CALL AGSRESET                                                    1123
         AG_DX = SFJP_SSCR_HL/2.                                          1123
         AG_DY = SFJP_SSCR_WD/2.                                          1123
         AG_DZ = SFJP_SSCR_THA/2.                                         1123
         AG_TITLE = 'BOX'                                                 1123
         AG_EXNAME = 'SHAPE'                                              1123
         AG_PARLIST = 'DX_DY_DZ'                                          1123
         CALL AXSHAPE                                                     1123
C    *                                                                    1123
6091  CALL AGSPOPB                                                        1124
      GO TO BLOCK_SBFH, (6090 ,6100)                                      1124
C End of Local Procedure                                                  1124
C ----------------------------------------------------------------------  1125
C ------------ SBCV                                                       1125
C Begin Local Procedure SBCV                                              1126
C SBCV IS THE VERTICAL BRANCH OF THE DELRIN CROSS                         1126
6070  CONTINUE                                                            1126
         AG_TITLE='SBCV'                                                  1126
         AG_MODULE=MODULE                                                 1126
         AG_MARK='V00'                                                    1126
         CALL AGSPUSH                                                     1126
         AG_MODULE=MODULE                                                 1126
C    *                                                                    1126
C    Geant MATERIAL : DELRIN ===> inherited arguments used <===           1127
         CALL AGSRESET                                                    1127
         AG_TITLE = 'DELRIN'                                              1127
         AG_EXNAME = 'MATERIAL'                                           1127
         AG_PARLIST = 'NONE'                                              1127
         CALL AXMATERIAL                                                  1127
C    *                                                                    1127
C    Geant ATTRIBUTE : SBCV ===> SEEN=1 COLO=2 <===                       1127
         CALL AGSRESET                                                    1128
         AG_SEEN = 1                                                      1128
         AG_COLO = 2                                                      1128
         AG_TITLE = 'SBCV'                                                1128
         AG_EXNAME = 'ATTRIBUTE'                                          1128
         AG_PARLIST = 'SEEN_COLO'                                         1128
         CALL AXATTRIBUTE                                                 1128
C    *                                                                    1128
C    Geant SHAPE : BOX ===> DX=SFJP_SSCR_WD/2. DY=SFJP_SSCR_VL/2., DZ=SF  1128
C    JP_SSCR_THD/2. <===                                                  1128
         CALL AGSRESET                                                    1130
         AG_DX = SFJP_SSCR_WD/2.                                          1130
         AG_DY = SFJP_SSCR_VL/2.                                          1130
         AG_DZ = SFJP_SSCR_THD/2.                                         1130
         AG_TITLE = 'BOX'                                                 1130
         AG_EXNAME = 'SHAPE'                                              1130
         AG_PARLIST = 'DX_DY_DZ'                                          1130
         CALL AXSHAPE                                                     1130
C    *                                                                    1130
6071  CALL AGSPOPB                                                        1131
      GO TO BLOCK_SBCV, (6070 ,6080)                                      1131
C End of Local Procedure                                                  1131
C ----------------------------------------------------------------------  1132
C ------------ SBFV                                                       1132
C Begin Local Procedure SBFV                                              1133
C SBFV IS THE VERTICAL BRANCH OF THE ALUMIMIUM CROSS                      1133
6110  CONTINUE                                                            1133
         AG_TITLE='SBFV'                                                  1133
         AG_MODULE=MODULE                                                 1133
         AG_MARK='V00'                                                    1133
         CALL AGSPUSH                                                     1133
         AG_MODULE=MODULE                                                 1133
C    *                                                                    1133
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1134
         CALL AGSRESET                                                    1134
         AG_TITLE = 'ALUMINIUM'                                           1134
         AG_EXNAME = 'MATERIAL'                                           1134
         AG_PARLIST = 'NONE'                                              1134
         CALL AXMATERIAL                                                  1134
C    *                                                                    1134
C    Geant ATTRIBUTE : SBFV ===> SEEN=1 COLO=1 <===                       1134
         CALL AGSRESET                                                    1135
         AG_SEEN = 1                                                      1135
         AG_COLO = 1                                                      1135
         AG_TITLE = 'SBFV'                                                1135
         AG_EXNAME = 'ATTRIBUTE'                                          1135
         AG_PARLIST = 'SEEN_COLO'                                         1135
         CALL AXATTRIBUTE                                                 1135
C    *                                                                    1135
C    Geant SHAPE : BOX ===> DX=SFJP_SSCR_WD/2. DY=SFJP_SSCR_VL/2., DZ=SF  1135
C    JP_SSCR_THA/2. <===                                                  1135
         CALL AGSRESET                                                    1137
         AG_DX = SFJP_SSCR_WD/2.                                          1137
         AG_DY = SFJP_SSCR_VL/2.                                          1137
         AG_DZ = SFJP_SSCR_THA/2.                                         1137
         AG_TITLE = 'BOX'                                                 1137
         AG_EXNAME = 'SHAPE'                                              1137
         AG_PARLIST = 'DX_DY_DZ'                                          1137
         CALL AXSHAPE                                                     1137
C    *                                                                    1137
6111  CALL AGSPOPB                                                        1138
      GO TO BLOCK_SBFV, (6110 ,6120)                                      1138
C End of Local Procedure                                                  1138
C ----------------------------------------------------------------------  1139
C ------------ SSLT                                                       1139
C Begin Local Procedure SSLT                                              1140
C SSLT IS THE LINKING (SECTOR TO THE CONE) TUBE                           1140
5170  CONTINUE                                                            1140
         AG_TITLE='SSLT'                                                  1140
         AG_MODULE=MODULE                                                 1140
         AG_MARK='V00'                                                    1140
         CALL AGSPUSH                                                     1140
         AG_MODULE=MODULE                                                 1140
C    *                                                                    1140
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1141
         CALL AGSRESET                                                    1141
         AG_TITLE = 'ALUMINIUM'                                           1141
         AG_EXNAME = 'MATERIAL'                                           1141
         AG_PARLIST = 'NONE'                                              1141
         CALL AXMATERIAL                                                  1141
C    *                                                                    1141
C    Geant ATTRIBUTE : SSLT ===> SEEN=1 COLO=1 <===                       1141
         CALL AGSRESET                                                    1142
         AG_SEEN = 1                                                      1142
         AG_COLO = 1                                                      1142
         AG_TITLE = 'SSLT'                                                1142
         AG_EXNAME = 'ATTRIBUTE'                                          1142
         AG_PARLIST = 'SEEN_COLO'                                         1142
         CALL AXATTRIBUTE                                                 1142
C    *                                                                    1142
C    Geant SHAPE : TUBE ===> RMIN=0. RMAX=SFJP_SSLT_RMAX DZ=SFJP_SSLT_DZ  1142
C    /2. <===                                                             1142
         CALL AGSRESET                                                    1143
         AG_RMIN = 0.                                                     1143
         AG_RMAX = SFJP_SSLT_RMAX                                         1143
         AG_DZ = SFJP_SSLT_DZ/2.                                          1143
         AG_TITLE = 'TUBE'                                                1143
         AG_EXNAME = 'SHAPE'                                              1143
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1143
         CALL AXSHAPE                                                     1143
C    *                                                                    1143
5171  CALL AGSPOPB                                                        1144
      GO TO BLOCK_SSLT, (5170 ,5180)                                      1144
C End of Local Procedure                                                  1144
C ----------------------------------------------------------------------  1145
C ------------ SCMP                                                       1145
C Begin Local Procedure SCMP                                              1146
C SCMP IS THE MOUNTING PLATE INSERTED IN THE CONES.                       1146
5190  CONTINUE                                                            1146
         AG_TITLE='SCMP'                                                  1146
         AG_MODULE=MODULE                                                 1146
         AG_MARK='V00'                                                    1146
         CALL AGSPUSH                                                     1146
         AG_MODULE=MODULE                                                 1146
C    *                                                                    1146
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1147
         CALL AGSRESET                                                    1147
         AG_TITLE = 'ALUMINIUM'                                           1147
         AG_EXNAME = 'MATERIAL'                                           1147
         AG_PARLIST = 'NONE'                                              1147
         CALL AXMATERIAL                                                  1147
C    *                                                                    1147
C    Geant ATTRIBUTE : SCMP ===> SEEN=1 COLO=1 <===                       1147
         CALL AGSRESET                                                    1148
         AG_SEEN = 1                                                      1148
         AG_COLO = 1                                                      1148
         AG_TITLE = 'SCMP'                                                1148
         AG_EXNAME = 'ATTRIBUTE'                                          1148
         AG_PARLIST = 'SEEN_COLO'                                         1148
         CALL AXATTRIBUTE                                                 1148
C    *                                                                    1148
C    Geant SHAPE : BOX ===> DX=SFJP_SCMP_DX/2. DY=SFJP_SCMP_DY/2., DZ=SF  1148
C    JP_SCMP_DZ/2 <===                                                    1148
         CALL AGSRESET                                                    1150
         AG_DX = SFJP_SCMP_DX/2.                                          1150
         AG_DY = SFJP_SCMP_DY/2.                                          1150
         AG_DZ = SFJP_SCMP_DZ/2                                           1150
         AG_TITLE = 'BOX'                                                 1150
         AG_EXNAME = 'SHAPE'                                              1150
         AG_PARLIST = 'DX_DY_DZ'                                          1150
         CALL AXSHAPE                                                     1150
C    *                                                                    1150
5191  CALL AGSPOPB                                                        1151
      GO TO BLOCK_SCMP, (5190 ,5200)                                      1151
C End of Local Procedure                                                  1151
C ----------------------------------------------------------------------  1152
C ------------ SCVM                                                       1152
C Begin Local Procedure SCVM                                              1153
C SCVM IS THE MOTHER VOLUME OF THE V-SHAPE PIECE                          1153
5210  CONTINUE                                                            1153
         AG_TITLE='SCVM'                                                  1153
         AG_MODULE=MODULE                                                 1153
         AG_MARK='V00'                                                    1153
         CALL AGSPUSH                                                     1153
         AG_MODULE=MODULE                                                 1153
C    *                                                                    1153
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1154
         CALL AGSRESET                                                    1154
         AG_TITLE = 'AIR'                                                 1154
         AG_EXNAME = 'MATERIAL'                                           1154
         AG_PARLIST = 'NONE'                                              1154
         CALL AXMATERIAL                                                  1154
C    *                                                                    1154
C    Geant ATTRIBUTE : SCVM ===> SEEN=0 COLO=6 <===                       1154
         CALL AGSRESET                                                    1155
         AG_SEEN = 0                                                      1155
         AG_COLO = 6                                                      1155
         AG_TITLE = 'SCVM'                                                1155
         AG_EXNAME = 'ATTRIBUTE'                                          1155
         AG_PARLIST = 'SEEN_COLO'                                         1155
         CALL AXATTRIBUTE                                                 1155
C    *                                                                    1155
C    Geant SHAPE : BOX ===> DX=SFJP_SCVM_DX/2. DY=SFJP_SCVM_DY/2., DZ=SF  1155
C    JP_SCVM_DZ/2 <===                                                    1155
         CALL AGSRESET                                                    1157
         AG_DX = SFJP_SCVM_DX/2.                                          1157
         AG_DY = SFJP_SCVM_DY/2.                                          1157
         AG_DZ = SFJP_SCVM_DZ/2                                           1157
         AG_TITLE = 'BOX'                                                 1157
         AG_EXNAME = 'SHAPE'                                              1157
         AG_PARLIST = 'DX_DY_DZ'                                          1157
         CALL AXSHAPE                                                     1157
C    *                                                                    1157
C    Create SCVB :                                                        1158
C    Call To Local Procedure SCVB                                         1158
         ASSIGN 6140 TO BLOCK_SCVB                                        1158
         GO TO  6130                                                      1158
6140     CONTINUE                                                         1158
C    *                                                                    1158
C    Geant POSITION : SCVB ===> X=0. Y=-SFJP_SCVM_DY/2.+SFJP_SCVB_DY/2.   1159
C    Z=0. <===                                                            1159
         CALL AGSRESET                                                    1159
         AG_X = 0.                                                        1159
         AG_Y = -SFJP_SCVM_DY/2.+SFJP_SCVB_DY/2.                          1159
         AG_Z = 0.                                                        1159
         AG_TITLE = 'SCVB'                                                1159
         AG_EXNAME = 'POSITION'                                           1159
         AG_PARLIST = 'X_Y_Z'                                             1159
         CALL AXPOSITION                                                  1159
C    *                                                                    1159
C    Create SCVS :                                                        1160
C    Call To Local Procedure SCVS                                         1160
         ASSIGN 6160 TO BLOCK_SCVS                                        1160
         GO TO  6150                                                      1160
6160     CONTINUE                                                         1160
C    *                                                                    1160
C    Geant POSITION : SCVS ===> X=SFJP_SCVS_PX Y=SFJP_SCVS_PY Z=0.0 ALPH  1161
C    AZ=SFJP_ALPHAZH <===                                                 1161
         CALL AGSRESET                                                    1161
         AG_X = SFJP_SCVS_PX                                              1161
         AG_Y = SFJP_SCVS_PY                                              1161
         AG_Z = 0.0                                                       1161
         AG_ALPHAN=AG_ALPHAN+1                                            1161
         AG_ALPHAK(AG_ALPHAN)=3                                           1161
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                1161
         AG_TITLE = 'SCVS'                                                1161
         AG_EXNAME = 'POSITION'                                           1161
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      1161
         CALL AXPOSITION                                                  1161
C    *                                                                    1161
C    Geant POSITION : SCVS ===> X=-SFJP_SCVS_PX Y=SFJP_SCVS_PY Z=0.0 ALP  1161
C    HAZ=-SFJP_ALPHAZH <===                                               1161
         CALL AGSRESET                                                    1162
         AG_X = -SFJP_SCVS_PX                                             1162
         AG_Y = SFJP_SCVS_PY                                              1162
         AG_Z = 0.0                                                       1162
         AG_ALPHAN=AG_ALPHAN+1                                            1162
         AG_ALPHAK(AG_ALPHAN)=3                                           1162
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               1162
         AG_TITLE = 'SCVS'                                                1162
         AG_EXNAME = 'POSITION'                                           1162
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      1162
         CALL AXPOSITION                                                  1162
C    *                                                                    1162
5211  CALL AGSPOPB                                                        1163
      GO TO BLOCK_SCVM, (5210 ,5220)                                      1163
C End of Local Procedure                                                  1163
C ----------------------------------------------------------------------  1164
C ------------ SCVB                                                       1164
C Begin Local Procedure SCVB                                              1165
C SCVB IS THE BASE PLATE OF THE V-SHAPE PIECE                             1165
6130  CONTINUE                                                            1165
         AG_TITLE='SCVB'                                                  1165
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
C    Geant ATTRIBUTE : SCVB ===> SEEN=1 COLO=1 <===                       1166
         CALL AGSRESET                                                    1167
         AG_SEEN = 1                                                      1167
         AG_COLO = 1                                                      1167
         AG_TITLE = 'SCVB'                                                1167
         AG_EXNAME = 'ATTRIBUTE'                                          1167
         AG_PARLIST = 'SEEN_COLO'                                         1167
         CALL AXATTRIBUTE                                                 1167
C    *                                                                    1167
C    Geant SHAPE : BOX ===> DX=SFJP_SCVM_DX/2. DY=SFJP_SCVB_DY/2., DZ=SF  1167
C    JP_SCVM_DZ/2 <===                                                    1167
         CALL AGSRESET                                                    1169
         AG_DX = SFJP_SCVM_DX/2.                                          1169
         AG_DY = SFJP_SCVB_DY/2.                                          1169
         AG_DZ = SFJP_SCVM_DZ/2                                           1169
         AG_TITLE = 'BOX'                                                 1169
         AG_EXNAME = 'SHAPE'                                              1169
         AG_PARLIST = 'DX_DY_DZ'                                          1169
         CALL AXSHAPE                                                     1169
C    *                                                                    1169
6131  CALL AGSPOPB                                                        1170
      GO TO BLOCK_SCVB, (6130 ,6140)                                      1170
C End of Local Procedure                                                  1170
C ----------------------------------------------------------------------  1171
C ------------ SCVS                                                       1171
C Begin Local Procedure SCVS                                              1172
C SCVS IS THE SIDE PLATE OF THE V-SHAPE PIECE                             1172
6150  CONTINUE                                                            1172
         AG_TITLE='SCVS'                                                  1172
         AG_MODULE=MODULE                                                 1172
         AG_MARK='V00'                                                    1172
         CALL AGSPUSH                                                     1172
         AG_MODULE=MODULE                                                 1172
C    *                                                                    1172
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1173
         CALL AGSRESET                                                    1173
         AG_TITLE = 'ALUMINIUM'                                           1173
         AG_EXNAME = 'MATERIAL'                                           1173
         AG_PARLIST = 'NONE'                                              1173
         CALL AXMATERIAL                                                  1173
C    *                                                                    1173
C    Geant ATTRIBUTE : SCVS ===> SEEN=1 COLO=1 <===                       1173
         CALL AGSRESET                                                    1174
         AG_SEEN = 1                                                      1174
         AG_COLO = 1                                                      1174
         AG_TITLE = 'SCVS'                                                1174
         AG_EXNAME = 'ATTRIBUTE'                                          1174
         AG_PARLIST = 'SEEN_COLO'                                         1174
         CALL AXATTRIBUTE                                                 1174
C    *                                                                    1174
C    Geant SHAPE : BOX ===> DX=SFJP_SCVS_DX/2. DY=SFJP_SCVS_DY/2., DZ=SF  1174
C    JP_SCVM_DZ/2 <===                                                    1174
         CALL AGSRESET                                                    1176
         AG_DX = SFJP_SCVS_DX/2.                                          1176
         AG_DY = SFJP_SCVS_DY/2.                                          1176
         AG_DZ = SFJP_SCVM_DZ/2                                           1176
         AG_TITLE = 'BOX'                                                 1176
         AG_EXNAME = 'SHAPE'                                              1176
         AG_PARLIST = 'DX_DY_DZ'                                          1176
         CALL AXSHAPE                                                     1176
C    *                                                                    1176
6151  CALL AGSPOPB                                                        1177
      GO TO BLOCK_SCVS, (6150 ,6160)                                      1177
C End of Local Procedure                                                  1177
      WRITE(LOUT,6170)'===>>  [End AgML Module SISDGEO5]  <<==='          1177
6170  FORMAT(A32,/,/)                                                     1177
C MODULE SISDGEO5                                                         1179
      END                                                                 1179
