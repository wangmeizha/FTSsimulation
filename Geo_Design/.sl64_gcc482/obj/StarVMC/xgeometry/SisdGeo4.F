C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE SISDGEO4                                                 1
C SISDGEO4 IS THE SILICON STRIP DETECTOR                                  1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'SISDGEO4'/, BANK_TITLE/'*'/                            1
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
C AUTHOR LILIAN MARTIN                                                    1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'LILIAN MARTIN'/                               2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C CREATED 14 MARCH 06                                                     2
      CHARACTER*40 CREATED /'14 MARCH 06'/                                3
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
      REAL SFJP_SFSD_DX                                                   27
      EQUIVALENCE (BANK_SFJP(136),SFJP_SFSD_DX,RE_SFJP_SFSD_DX),          27
     *(NAMES_SFJP(134),NSFJP_SFSD_DX)                                     27
      EQUIVALENCE (COMMEN_SFJP(134),CSFJP_SFSD_DX),(MAP_SFJP(1,134),      27
     *MSFJP_SFSD_DX)                                                      27
      CHARACTER CSFJP_SFSD_DX*40/' '/                                     27
      CHARACTER NSFJP_SFSD_DX*12/'R.SFSD_DX'/                             27
      INTEGER MSFJP_SFSD_DX(2)/1,1/                                       27
      REAL RE_SFJP_SFSD_DX                                                27
C -                                                                       27
      REAL SFJP_SFSD_DY                                                   27
      EQUIVALENCE (BANK_SFJP(137),SFJP_SFSD_DY,RE_SFJP_SFSD_DY),          27
     *(NAMES_SFJP(135),NSFJP_SFSD_DY)                                     27
      EQUIVALENCE (COMMEN_SFJP(135),CSFJP_SFSD_DY),(MAP_SFJP(1,135),      27
     *MSFJP_SFSD_DY)                                                      27
      CHARACTER CSFJP_SFSD_DY*40/' '/                                     27
      CHARACTER NSFJP_SFSD_DY*12/'R.SFSD_DY'/                             27
      INTEGER MSFJP_SFSD_DY(2)/1,1/                                       27
      REAL RE_SFJP_SFSD_DY                                                27
C -                                                                       27
      REAL SFJP_SFSD_DZ                                                   27
      EQUIVALENCE (BANK_SFJP(138),SFJP_SFSD_DZ,RE_SFJP_SFSD_DZ),          27
     *(NAMES_SFJP(136),NSFJP_SFSD_DZ)                                     27
      EQUIVALENCE (COMMEN_SFJP(136),CSFJP_SFSD_DZ),(MAP_SFJP(1,136),      27
     *MSFJP_SFSD_DZ)                                                      27
      CHARACTER CSFJP_SFSD_DZ*40/' '/                                     27
      CHARACTER NSFJP_SFSD_DZ*12/'R.SFSD_DZ'/                             27
      INTEGER MSFJP_SFSD_DZ(2)/1,1/                                       27
      REAL RE_SFJP_SFSD_DZ                                                27
C -                                                                       27
      REAL SFJP_SFLA_PX                                                   27
      EQUIVALENCE (BANK_SFJP(139),SFJP_SFLA_PX,RE_SFJP_SFLA_PX),          27
     *(NAMES_SFJP(137),NSFJP_SFLA_PX)                                     27
      EQUIVALENCE (COMMEN_SFJP(137),CSFJP_SFLA_PX),(MAP_SFJP(1,137),      27
     *MSFJP_SFLA_PX)                                                      27
      CHARACTER CSFJP_SFLA_PX*40/' '/                                     27
      CHARACTER NSFJP_SFLA_PX*12/'R.SFLA_PX'/                             27
      INTEGER MSFJP_SFLA_PX(2)/1,1/                                       27
      REAL RE_SFJP_SFLA_PX                                                27
C -                                                                       27
      REAL SFJP_SFLA_PY                                                   27
      EQUIVALENCE (BANK_SFJP(140),SFJP_SFLA_PY,RE_SFJP_SFLA_PY),          27
     *(NAMES_SFJP(138),NSFJP_SFLA_PY)                                     27
      EQUIVALENCE (COMMEN_SFJP(138),CSFJP_SFLA_PY),(MAP_SFJP(1,138),      27
     *MSFJP_SFLA_PY)                                                      27
      CHARACTER CSFJP_SFLA_PY*40/' '/                                     27
      CHARACTER NSFJP_SFLA_PY*12/'R.SFLA_PY'/                             27
      INTEGER MSFJP_SFLA_PY(2)/1,1/                                       27
      REAL RE_SFJP_SFLA_PY                                                27
C -                                                                       27
      REAL SFJP_SFLC_PX                                                   27
      EQUIVALENCE (BANK_SFJP(141),SFJP_SFLC_PX,RE_SFJP_SFLC_PX),          27
     *(NAMES_SFJP(139),NSFJP_SFLC_PX)                                     27
      EQUIVALENCE (COMMEN_SFJP(139),CSFJP_SFLC_PX),(MAP_SFJP(1,139),      27
     *MSFJP_SFLC_PX)                                                      27
      CHARACTER CSFJP_SFLC_PX*40/' '/                                     27
      CHARACTER NSFJP_SFLC_PX*12/'R.SFLC_PX'/                             27
      INTEGER MSFJP_SFLC_PX(2)/1,1/                                       27
      REAL RE_SFJP_SFLC_PX                                                27
C -                                                                       27
      REAL SFJP_SFLC_PY                                                   27
      EQUIVALENCE (BANK_SFJP(142),SFJP_SFLC_PY,RE_SFJP_SFLC_PY),          27
     *(NAMES_SFJP(140),NSFJP_SFLC_PY)                                     27
      EQUIVALENCE (COMMEN_SFJP(140),CSFJP_SFLC_PY),(MAP_SFJP(1,140),      27
     *MSFJP_SFLC_PY)                                                      27
      CHARACTER CSFJP_SFLC_PY*40/' '/                                     27
      CHARACTER NSFJP_SFLC_PY*12/'R.SFLC_PY'/                             27
      INTEGER MSFJP_SFLC_PY(2)/1,1/                                       27
      REAL RE_SFJP_SFLC_PY                                                27
C -                                                                       27
      REAL SFJP_SFES_PX                                                   27
      EQUIVALENCE (BANK_SFJP(143),SFJP_SFES_PX,RE_SFJP_SFES_PX),          27
     *(NAMES_SFJP(141),NSFJP_SFES_PX)                                     27
      EQUIVALENCE (COMMEN_SFJP(141),CSFJP_SFES_PX),(MAP_SFJP(1,141),      27
     *MSFJP_SFES_PX)                                                      27
      CHARACTER CSFJP_SFES_PX*40/' '/                                     27
      CHARACTER NSFJP_SFES_PX*12/'R.SFES_PX'/                             27
      INTEGER MSFJP_SFES_PX(2)/1,1/                                       27
      REAL RE_SFJP_SFES_PX                                                27
C -                                                                       27
      REAL SFJP_SFES_PY                                                   27
      EQUIVALENCE (BANK_SFJP(144),SFJP_SFES_PY,RE_SFJP_SFES_PY),          27
     *(NAMES_SFJP(142),NSFJP_SFES_PY)                                     27
      EQUIVALENCE (COMMEN_SFJP(142),CSFJP_SFES_PY),(MAP_SFJP(1,142),      27
     *MSFJP_SFES_PY)                                                      27
      CHARACTER CSFJP_SFES_PY*40/' '/                                     27
      CHARACTER NSFJP_SFES_PY*12/'R.SFES_PY'/                             27
      INTEGER MSFJP_SFES_PY(2)/1,1/                                       27
      REAL RE_SFJP_SFES_PY                                                27
C -                                                                       27
      REAL SFJP_SFES_PZ                                                   27
      EQUIVALENCE (BANK_SFJP(145),SFJP_SFES_PZ,RE_SFJP_SFES_PZ),          27
     *(NAMES_SFJP(143),NSFJP_SFES_PZ)                                     27
      EQUIVALENCE (COMMEN_SFJP(143),CSFJP_SFES_PZ),(MAP_SFJP(1,143),      27
     *MSFJP_SFES_PZ)                                                      27
      CHARACTER CSFJP_SFES_PZ*40/' '/                                     27
      CHARACTER NSFJP_SFES_PZ*12/'R.SFES_PZ'/                             27
      INTEGER MSFJP_SFES_PZ(2)/1,1/                                       27
      REAL RE_SFJP_SFES_PZ                                                27
C -                                                                       27
      REAL SFJP_SFEB_PX                                                   27
      EQUIVALENCE (BANK_SFJP(146),SFJP_SFEB_PX,RE_SFJP_SFEB_PX),          27
     *(NAMES_SFJP(144),NSFJP_SFEB_PX)                                     27
      EQUIVALENCE (COMMEN_SFJP(144),CSFJP_SFEB_PX),(MAP_SFJP(1,144),      27
     *MSFJP_SFEB_PX)                                                      27
      CHARACTER CSFJP_SFEB_PX*40/' '/                                     27
      CHARACTER NSFJP_SFEB_PX*12/'R.SFEB_PX'/                             27
      INTEGER MSFJP_SFEB_PX(2)/1,1/                                       27
      REAL RE_SFJP_SFEB_PX                                                27
C -                                                                       27
      REAL SFJP_SFEB_PY                                                   27
      EQUIVALENCE (BANK_SFJP(147),SFJP_SFEB_PY,RE_SFJP_SFEB_PY),          27
     *(NAMES_SFJP(145),NSFJP_SFEB_PY)                                     27
      EQUIVALENCE (COMMEN_SFJP(145),CSFJP_SFEB_PY),(MAP_SFJP(1,145),      27
     *MSFJP_SFEB_PY)                                                      27
      CHARACTER CSFJP_SFEB_PY*40/' '/                                     27
      CHARACTER NSFJP_SFEB_PY*12/'R.SFEB_PY'/                             27
      INTEGER MSFJP_SFEB_PY(2)/1,1/                                       27
      REAL RE_SFJP_SFEB_PY                                                27
C -                                                                       27
      INTEGER LEN_SFJP,LLEN_SFJP,LINK_SFJP                                27
      PARAMETER (LEN_SFJP=147)                                            27
      PARAMETER (LLEN_SFJP=145)                                           27
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
      SFJP_SFSD_DX=3.75                                                   176
      CSFJP_SFSD_DX='DX OF THE STRIP DETECTOR'                            176
C DY OF THE STRIP DETECTOR                                                177
      SFJP_SFSD_DY=0.015                                                  177
      CSFJP_SFSD_DY='DY OF THE STRIP DETECTOR'                            177
C DZ OF THE STRIP DETECTOR                                                178
      SFJP_SFSD_DZ=2.1                                                    178
      CSFJP_SFSD_DZ='DZ OF THE STRIP DETECTOR'                            178
C X-POSITION OF THE TOP FLEX ALONG THE Z-AXIS                             179
      SFJP_SFLA_PX=1.1                                                    179
      CSFJP_SFLA_PX='X-POSITION OF THE TOP FLEX ALONG THE Z-AXIS'         179
C Y-POSITION OF THE TOP FLEX ALONG THE Z-AXIS                             180
      SFJP_SFLA_PY=1.35                                                   180
      CSFJP_SFLA_PY='Y-POSITION OF THE TOP FLEX ALONG THE Z-AXIS'         180
C X-POSITION OF THE BOTTOM FLEX ALONG THE Z-AXIS                          181
      SFJP_SFLC_PX=2.3                                                    181
      CSFJP_SFLC_PX='X-POSITION OF THE BOTTOM FLEX ALONG THE Z-AXIS'      181
C Y-POSITION OF THE BOTTOM FLEX ALONG THE Z-AXIS                          182
      SFJP_SFLC_PY=0.3                                                    182
      CSFJP_SFLC_PY='Y-POSITION OF THE BOTTOM FLEX ALONG THE Z-AXIS'      182
C X-POSITION OF THE SMALL ELBOW BUS                                       183
      SFJP_SFES_PX=3.32                                                   183
      CSFJP_SFES_PX='X-POSITION OF THE SMALL ELBOW BUS'                   183
C Y-POSITION OF THE SMALL ELBOW BUS                                       184
      SFJP_SFES_PY=1.6                                                    184
      CSFJP_SFES_PY='Y-POSITION OF THE SMALL ELBOW BUS'                   184
C Z-POSITION OF THE SMALL ELBOW BUS                                       185
      SFJP_SFES_PZ=0.1                                                    185
      CSFJP_SFES_PZ='Z-POSITION OF THE SMALL ELBOW BUS'                   185
C X-POSITION OF THE BIG ELBOW BUS                                         186
      SFJP_SFEB_PX=2.71                                                   186
      CSFJP_SFEB_PX='X-POSITION OF THE BIG ELBOW BUS'                     186
C Y-POSITION OF THE BIG ELBOW BUS                                         187
      SFJP_SFEB_PY=0.75                                                   187
      CSFJP_SFEB_PY='Y-POSITION OF THE BIG ELBOW BUS'                     187
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFJP',    188
     *BPATH_SFJP,IDNUM_SFJP,1, MAP_SFJP,NAMES_SFJP,COMMEN_SFJP,           188
     *BANK_SFJP,LEN_SFJP,LLEN_SFJP,LINK_SFJP,FLAG_SFJP)                   188
C *                                                                       188
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFJP',    188
     *BPATH_SFJP,IDNUM_SFJP,1, MAP_SFJP,NAMES_SFJP,COMMEN_SFJP,           188
     *BANK_SFJP,LEN_SFJP,LLEN_SFJP,LINK_SFJP,FLAG_SFJP)                   188
C *                                                                       189
      CALL AGSBEGM(MODULE,IPRIN)                                          189
      IF (IPRIN.LT.0) RETURN                                              189
C Filling bank SFJP - DIFFERENT DIMENSION PARAMETERS                      189
      BANK_TITLE='DIFFERENT DIMENSION PARAMETERS'                         189
      BPATH_SFJP='SFJP*'                                                  189
      IDNUM_SFJP(1)=0                                                     190
C VERSION                                                                 190
      SFJP_VERSION=2                                                      190
      CSFJP_VERSION='VERSION'                                             190
C WIDTH OF THE SIDE OF THE SMALL RIB (OUTSIDE STRUCTURE)                  191
      SFJP_SSRS_RMIN=23.3                                                 191
      CSFJP_SSRS_RMIN='WIDTH OF THE SIDE OF THE SMALL RIB (OUTSIDE STRUC  191
     *TURE)'                                                              191
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFJP',    192
     *BPATH_SFJP,IDNUM_SFJP,1, MAP_SFJP,NAMES_SFJP,COMMEN_SFJP,           192
     *BANK_SFJP,LEN_SFJP,LLEN_SFJP,LINK_SFJP,FLAG_SFJP)                   192
C *                                                                       192
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFJP',    192
     *BPATH_SFJP,IDNUM_SFJP,1, MAP_SFJP,NAMES_SFJP,COMMEN_SFJP,           192
     *BANK_SFJP,LEN_SFJP,LLEN_SFJP,LINK_SFJP,FLAG_SFJP)                   192
C *                                                                       193
      CALL AGSBEGM(MODULE,IPRIN)                                          193
      IF (IPRIN.LT.0) RETURN                                              193
C Filling bank SFPB - SOME SSD SHELL DIMENSIONS                           193
      BANK_TITLE='SOME SSD SHELL DIMENSIONS'                              193
      BPATH_SFPB='SFPB*'                                                  193
      IDNUM_SFPB(1)=0                                                     194
C HAUT 1                                                                  194
      SFPB_HHIGHT=(0.02/TAN(54*PI/180)+0.02/(2*TAN(PI/5))+0.2)*SIN(PI/    194
     *5)                                                                  194
      CSFPB_HHIGHT='HAUT 1'                                               194
C HAUT 2                                                                  195
      SFPB_KHIGHT=0.02/SIN(54*PI/180)                                     195
      CSFPB_KHIGHT='HAUT 2'                                               195
C BAS 1                                                                   196
      SFPB_HBASE=(0.02/TAN(63*PI/180)+0.01/TAN(27*PI/                     196
     *180)+0.2)*SIN(27*PI/180)                                            196
      CSFPB_HBASE='BAS 1'                                                 196
C BAS 2                                                                   197
      SFPB_KBASE=0.02/SIN(63*PI/180)                                      197
      CSFPB_KBASE='BAS 2'                                                 197
C LENGHT OF THE SIDE KAPTON FILM                                          198
      SFPB_FSIZE=0.6/COS(15*PI/180)+0.02*TAN(15*PI/180)                   198
      CSFPB_FSIZE='LENGHT OF THE SIDE KAPTON FILM'                        198
C Z COORD                                                                 199
      SFPB_ZCOOR=5.18462095393834410                                      199
      CSFPB_ZCOOR='Z COORD'                                               199
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPB',    200
     *BPATH_SFPB,IDNUM_SFPB,1, MAP_SFPB,NAMES_SFPB,COMMEN_SFPB,           200
     *BANK_SFPB,LEN_SFPB,LLEN_SFPB,LINK_SFPB,FLAG_SFPB)                   200
C *                                                                       200
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPB',    200
     *BPATH_SFPB,IDNUM_SFPB,1, MAP_SFPB,NAMES_SFPB,COMMEN_SFPB,           200
     *BANK_SFPB,LEN_SFPB,LLEN_SFPB,LINK_SFPB,FLAG_SFPB)                   200
C *                                                                       201
      CALL AGSBEGM(MODULE,IPRIN)                                          201
      IF (IPRIN.LT.0) RETURN                                              201
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   201
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      201
      BPATH_SFPA='SFPA*'                                                  201
      IDNUM_SFPA(1)=0                                                     202
C GEOMETRY VERSION                                                        202
      SFPA_VERSION=1                                                      202
      CSFPA_VERSION='GEOMETRY VERSION'                                    202
C MOTHER RMIN                                                             203
      SFPA_RMIN=21.8                                                      203
      CSFPA_RMIN='MOTHER RMIN'                                            203
C MOTHER RMAX                                                             204
      SFPA_RMAX=32.                                                       204
      CSFPA_RMAX='MOTHER RMAX'                                            204
C MOTHER LEN ALONG THE Z DIRECTION                                        205
      SFPA_LEN=107.                                                       205
      CSFPA_LEN='MOTHER LEN ALONG THE Z DIRECTION'                        205
C DISTANCE FROM BEAM AXIS TO DETECTOR CENTER                              206
      SFPA_RAD=23.                                                        206
      CSFPA_RAD='DISTANCE FROM BEAM AXIS TO DETECTOR CENTER'              206
C NUMBER OF SILICON STRIP DETECTORS                                       207
      SFPA_NSSD=16                                                        207
      CSFPA_NSSD='NUMBER OF SILICON STRIP DETECTORS'                      207
C DETECTOR MOTHER WIDTH                                                   208
      SFPA_DMWID=7.8                                                      208
      CSFPA_DMWID='DETECTOR MOTHER WIDTH'                                 208
C DETECTOR MOTHER THICKNESS                                               209
      SFPA_DMTHK=2.0                                                      209
      CSFPA_DMTHK='DETECTOR MOTHER THICKNESS'                             209
C DETECTOR MOTHER LENGTH (DETECTORS + ADC BOARD)                          210
      SFPA_DMLEN=90.                                                      210
      CSFPA_DMLEN='DETECTOR MOTHER LENGTH (DETECTORS + ADC BOARD)'        210
C STRUCTURE MOTHER WIDTH                                                  211
      SFPA_SMWID=7.5                                                      211
      CSFPA_SMWID='STRUCTURE MOTHER WIDTH'                                211
C STRUCTURE MOTHER THICKNESS                                              212
      SFPA_SMTHK=5.0                                                      212
      CSFPA_SMTHK='STRUCTURE MOTHER THICKNESS'                            212
C STRUCTURE MOTHER LENGTH                                                 213
      SFPA_SMLEN=101.9                                                    213
      CSFPA_SMLEN='STRUCTURE MOTHER LENGTH'                               213
C LENGTH OF A SUBVOLUME OF THE STRUCTURE                                  214
      SFPA_SSLEN=95./20.                                                  214
      CSFPA_SSLEN='LENGTH OF A SUBVOLUME OF THE STRUCTURE'                214
C LENGTH OF WAFER PACK                                                    215
      SFPA_WPLEN=69.6                                                     215
      CSFPA_WPLEN='LENGTH OF WAFER PACK'                                  215
C LENGTH OF ONE STRIP DETECTOR (ALONG BEAM AXIS)                          216
      SFPA_SDLEN=4.2                                                      216
      CSFPA_SDLEN='LENGTH OF ONE STRIP DETECTOR (ALONG BEAM AXIS)'        216
C TILING ANGLE (DEGREES)                                                  217
      SFPA_TILT=5.0                                                       217
      CSFPA_TILT='TILING ANGLE (DEGREES)'                                 217
C COOLING PIPE OUTER RADIUS                                               218
      SFPA_CPRAD=0.1                                                      218
      CSFPA_CPRAD='COOLING PIPE OUTER RADIUS'                             218
C COOLING PIPE INNER RADIUS                                               219
      SFPA_CPRAL=0.09                                                     219
      CSFPA_CPRAL='COOLING PIPE INNER RADIUS'                             219
C CARBON FIBER TUBE RADIUS (SUPPORT STRUCTURE)                            220
      SFPA_CFRAD=0.1                                                      220
      CSFPA_CFRAD='CARBON FIBER TUBE RADIUS (SUPPORT STRUCTURE)'          220
C GAP BETWEEN STRUCTURE MOTHER AND DETECTOR                               221
      SFPA_GPTHK=-1.0                                                     221
      CSFPA_GPTHK='GAP BETWEEN STRUCTURE MOTHER AND DETECTOR'             221
C PRESENCE OF LADDERS                                                     223
      SFPA_LADDERMAP(1)=0                                                 223
      SFPA_LADDERMAP(2)=0                                                 223
      SFPA_LADDERMAP(3)=1                                                 223
      SFPA_LADDERMAP(4)=0                                                 223
      SFPA_LADDERMAP(5)=0                                                 223
      SFPA_LADDERMAP(6)=0                                                 223
      SFPA_LADDERMAP(7)=0                                                 223
      SFPA_LADDERMAP(8)=0                                                 223
      SFPA_LADDERMAP(9)=0                                                 223
      SFPA_LADDERMAP(10)=0                                                223
      SFPA_LADDERMAP(11)=0                                                223
      SFPA_LADDERMAP(12)=0                                                223
      SFPA_LADDERMAP(13)=0                                                223
      SFPA_LADDERMAP(14)=0                                                223
      SFPA_LADDERMAP(15)=0                                                223
      SFPA_LADDERMAP(16)=0                                                223
      SFPA_LADDERMAP(17)=0                                                223
      SFPA_LADDERMAP(18)=0                                                223
      SFPA_LADDERMAP(19)=0                                                223
      SFPA_LADDERMAP(20)=0                                                223
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               223
C INDIVIDUAL ANGLES                                                       227
      SFPA_LADDERANGLE(1)=-1.0                                            227
      SFPA_LADDERANGLE(2)=-1.0                                            227
      SFPA_LADDERANGLE(3)=45.0                                            227
      SFPA_LADDERANGLE(4)=-1.0                                            227
      SFPA_LADDERANGLE(5)=-1.0                                            227
      SFPA_LADDERANGLE(6)=-1.0                                            227
      SFPA_LADDERANGLE(7)=-1.0                                            227
      SFPA_LADDERANGLE(8)=-1.0                                            227
      SFPA_LADDERANGLE(9)=-1.0                                            227
      SFPA_LADDERANGLE(10)=-1.0                                           227
      SFPA_LADDERANGLE(11)=-1.0                                           227
      SFPA_LADDERANGLE(12)=-1.0                                           227
      SFPA_LADDERANGLE(13)=-1.0                                           227
      SFPA_LADDERANGLE(14)=-1.0                                           227
      SFPA_LADDERANGLE(15)=-1.0                                           227
      SFPA_LADDERANGLE(16)=-1.0                                           227
      SFPA_LADDERANGLE(17)=-1.0                                           227
      SFPA_LADDERANGLE(18)=-1.0                                           227
      SFPA_LADDERANGLE(19)=-1.0                                           227
      SFPA_LADDERANGLE(20)=-1.0                                           227
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               227
C INDIVIDUAL TILTS                                                        231
      SFPA_LADDERTILT(1)=-1.0                                             231
      SFPA_LADDERTILT(2)=-1.0                                             231
      SFPA_LADDERTILT(3)=0.0                                              231
      SFPA_LADDERTILT(4)=-1.0                                             231
      SFPA_LADDERTILT(5)=-1.0                                             231
      SFPA_LADDERTILT(6)=-1.0                                             231
      SFPA_LADDERTILT(7)=-1.0                                             231
      SFPA_LADDERTILT(8)=-1.0                                             231
      SFPA_LADDERTILT(9)=-1.0                                             231
      SFPA_LADDERTILT(10)=-1.0                                            231
      SFPA_LADDERTILT(11)=-1.0                                            231
      SFPA_LADDERTILT(12)=-1.0                                            231
      SFPA_LADDERTILT(13)=-1.0                                            231
      SFPA_LADDERTILT(14)=-1.0                                            231
      SFPA_LADDERTILT(15)=-1.0                                            231
      SFPA_LADDERTILT(16)=-1.0                                            231
      SFPA_LADDERTILT(17)=-1.0                                            231
      SFPA_LADDERTILT(18)=-1.0                                            231
      SFPA_LADDERTILT(19)=-1.0                                            231
      SFPA_LADDERTILT(20)=-1.0                                            231
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 231
C INDIVIDUAL RADII                                                        236
      SFPA_LADDERRADIUS(1)=-1.0                                           236
      SFPA_LADDERRADIUS(2)=-1.0                                           236
      SFPA_LADDERRADIUS(3)=23.000                                         236
      SFPA_LADDERRADIUS(4)=-1.0                                           236
      SFPA_LADDERRADIUS(5)=-1.0                                           236
      SFPA_LADDERRADIUS(6)=-1.0                                           236
      SFPA_LADDERRADIUS(7)=-1.0                                           236
      SFPA_LADDERRADIUS(8)=-1.0                                           236
      SFPA_LADDERRADIUS(9)=-1.0                                           236
      SFPA_LADDERRADIUS(10)=-1.0                                          236
      SFPA_LADDERRADIUS(11)=-1.0                                          236
      SFPA_LADDERRADIUS(12)=-1.0                                          236
      SFPA_LADDERRADIUS(13)=-1.0                                          236
      SFPA_LADDERRADIUS(14)=-1.0                                          236
      SFPA_LADDERRADIUS(15)=-1.0                                          236
      SFPA_LADDERRADIUS(16)=-1.0                                          236
      SFPA_LADDERRADIUS(17)=-1.0                                          236
      SFPA_LADDERRADIUS(18)=-1.0                                          236
      SFPA_LADDERRADIUS(19)=-1.0                                          236
      SFPA_LADDERRADIUS(20)=-1.0                                          236
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               236
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    237
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           237
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   237
C *                                                                       237
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    237
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           237
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   237
C *                                                                       238
      CALL AGSBEGM(MODULE,IPRIN)                                          238
      IF (IPRIN.LT.0) RETURN                                              238
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   238
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      238
      BPATH_SFPA='SFPA*'                                                  238
      IDNUM_SFPA(1)=0                                                     239
C GEOMETRY VERSION                                                        239
      SFPA_VERSION=2                                                      239
      CSFPA_VERSION='GEOMETRY VERSION'                                    239
C PRESENCE OF LADDERS                                                     241
      SFPA_LADDERMAP(1)=1                                                 241
      SFPA_LADDERMAP(2)=1                                                 241
      SFPA_LADDERMAP(3)=1                                                 241
      SFPA_LADDERMAP(4)=0                                                 241
      SFPA_LADDERMAP(5)=0                                                 241
      SFPA_LADDERMAP(6)=0                                                 241
      SFPA_LADDERMAP(7)=0                                                 241
      SFPA_LADDERMAP(8)=0                                                 241
      SFPA_LADDERMAP(9)=1                                                 241
      SFPA_LADDERMAP(10)=1                                                241
      SFPA_LADDERMAP(11)=1                                                241
      SFPA_LADDERMAP(12)=1                                                241
      SFPA_LADDERMAP(13)=1                                                241
      SFPA_LADDERMAP(14)=0                                                241
      SFPA_LADDERMAP(15)=0                                                241
      SFPA_LADDERMAP(16)=0                                                241
      SFPA_LADDERMAP(17)=0                                                241
      SFPA_LADDERMAP(18)=0                                                241
      SFPA_LADDERMAP(19)=1                                                241
      SFPA_LADDERMAP(20)=1                                                241
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               241
C INDIVIDUAL ANGLES                                                       245
      SFPA_LADDERANGLE(1)=90.0                                            245
      SFPA_LADDERANGLE(2)=108.3                                           245
      SFPA_LADDERANGLE(3)=130.0                                           245
      SFPA_LADDERANGLE(4)=-1.0                                            245
      SFPA_LADDERANGLE(5)=-1.0                                            245
      SFPA_LADDERANGLE(6)=-1.0                                            245
      SFPA_LADDERANGLE(7)=-1.0                                            245
      SFPA_LADDERANGLE(8)=-1.0                                            245
      SFPA_LADDERANGLE(9)=230.0                                           245
      SFPA_LADDERANGLE(10)=251.7                                          245
      SFPA_LADDERANGLE(11)=270.0                                          245
      SFPA_LADDERANGLE(12)=288.3                                          245
      SFPA_LADDERANGLE(13)=310.0                                          245
      SFPA_LADDERANGLE(14)=-1.0                                           245
      SFPA_LADDERANGLE(15)=-1.0                                           245
      SFPA_LADDERANGLE(16)=-1.0                                           245
      SFPA_LADDERANGLE(17)=-1.0                                           245
      SFPA_LADDERANGLE(18)=-1.0                                           245
      SFPA_LADDERANGLE(19)=50.0                                           245
      SFPA_LADDERANGLE(20)=71.7                                           245
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               245
C INDIVIDUAL TILTS                                                        249
      SFPA_LADDERTILT(1)=0.0                                              249
      SFPA_LADDERTILT(2)=-6.0                                             249
      SFPA_LADDERTILT(3)=0.0                                              249
      SFPA_LADDERTILT(4)=-1.0                                             249
      SFPA_LADDERTILT(5)=-1.0                                             249
      SFPA_LADDERTILT(6)=-1.0                                             249
      SFPA_LADDERTILT(7)=-1.0                                             249
      SFPA_LADDERTILT(8)=0.0                                              249
      SFPA_LADDERTILT(9)=0.0                                              249
      SFPA_LADDERTILT(10)=6.0                                             249
      SFPA_LADDERTILT(11)=0.0                                             249
      SFPA_LADDERTILT(12)=-6.0                                            249
      SFPA_LADDERTILT(13)=0.0                                             249
      SFPA_LADDERTILT(14)=-1.0                                            249
      SFPA_LADDERTILT(15)=-1.0                                            249
      SFPA_LADDERTILT(16)=-1.0                                            249
      SFPA_LADDERTILT(17)=-1.0                                            249
      SFPA_LADDERTILT(18)=-1.0                                            249
      SFPA_LADDERTILT(19)=0.0                                             249
      SFPA_LADDERTILT(20)=6.0                                             249
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 249
C INDIVIDUAL RADII                                                        254
      SFPA_LADDERRADIUS(1)=23.174                                         254
      SFPA_LADDERRADIUS(2)=22.800                                         254
      SFPA_LADDERRADIUS(3)=24.600                                         254
      SFPA_LADDERRADIUS(4)=-1.0                                           254
      SFPA_LADDERRADIUS(5)=-1.0                                           254
      SFPA_LADDERRADIUS(6)=-1.0                                           254
      SFPA_LADDERRADIUS(7)=-1.0                                           254
      SFPA_LADDERRADIUS(8)=-1.0                                           254
      SFPA_LADDERRADIUS(9)=24.600                                         254
      SFPA_LADDERRADIUS(10)=22.800                                        254
      SFPA_LADDERRADIUS(11)=23.174                                        254
      SFPA_LADDERRADIUS(12)=22.800                                        254
      SFPA_LADDERRADIUS(13)=24.600                                        254
      SFPA_LADDERRADIUS(14)=-1.0                                          254
      SFPA_LADDERRADIUS(15)=-1.0                                          254
      SFPA_LADDERRADIUS(16)=-1.0                                          254
      SFPA_LADDERRADIUS(17)=-1.0                                          254
      SFPA_LADDERRADIUS(18)=-1.0                                          254
      SFPA_LADDERRADIUS(19)=24.600                                        254
      SFPA_LADDERRADIUS(20)=22.800                                        254
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               254
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    255
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           255
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   255
C *                                                                       255
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    255
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           255
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   255
C *                                                                       256
      CALL AGSBEGM(MODULE,IPRIN)                                          256
      IF (IPRIN.LT.0) RETURN                                              256
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   256
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      256
      BPATH_SFPA='SFPA*'                                                  256
      IDNUM_SFPA(1)=0                                                     257
C GEOMETRY VERSION                                                        257
      SFPA_VERSION=3                                                      257
      CSFPA_VERSION='GEOMETRY VERSION'                                    257
C PRESENCE OF LADDERS                                                     259
      SFPA_LADDERMAP(1)=1                                                 259
      SFPA_LADDERMAP(2)=1                                                 259
      SFPA_LADDERMAP(3)=1                                                 259
      SFPA_LADDERMAP(4)=1                                                 259
      SFPA_LADDERMAP(5)=1                                                 259
      SFPA_LADDERMAP(6)=1                                                 259
      SFPA_LADDERMAP(7)=1                                                 259
      SFPA_LADDERMAP(8)=1                                                 259
      SFPA_LADDERMAP(9)=1                                                 259
      SFPA_LADDERMAP(10)=1                                                259
      SFPA_LADDERMAP(11)=1                                                259
      SFPA_LADDERMAP(12)=1                                                259
      SFPA_LADDERMAP(13)=1                                                259
      SFPA_LADDERMAP(14)=1                                                259
      SFPA_LADDERMAP(15)=1                                                259
      SFPA_LADDERMAP(16)=1                                                259
      SFPA_LADDERMAP(17)=1                                                259
      SFPA_LADDERMAP(18)=1                                                259
      SFPA_LADDERMAP(19)=1                                                259
      SFPA_LADDERMAP(20)=1                                                259
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               259
C INDIVIDUAL ANGLES                                                       263
      SFPA_LADDERANGLE(1)=90.0                                            263
      SFPA_LADDERANGLE(2)=108.3                                           263
      SFPA_LADDERANGLE(3)=126.6                                           263
      SFPA_LADDERANGLE(4)=144.4                                           263
      SFPA_LADDERANGLE(5)=162.2                                           263
      SFPA_LADDERANGLE(6)=180.0                                           263
      SFPA_LADDERANGLE(7)=197.8                                           263
      SFPA_LADDERANGLE(8)=215.6                                           263
      SFPA_LADDERANGLE(9)=233.4                                           263
      SFPA_LADDERANGLE(10)=251.7                                          263
      SFPA_LADDERANGLE(11)=270.0                                          263
      SFPA_LADDERANGLE(12)=288.3                                          263
      SFPA_LADDERANGLE(13)=306.6                                          263
      SFPA_LADDERANGLE(14)=324.4                                          263
      SFPA_LADDERANGLE(15)=342.2                                          263
      SFPA_LADDERANGLE(16)=0.0                                            263
      SFPA_LADDERANGLE(17)=17.8                                           263
      SFPA_LADDERANGLE(18)=35.6                                           263
      SFPA_LADDERANGLE(19)=53.4                                           263
      SFPA_LADDERANGLE(20)=71.7                                           263
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               263
C INDIVIDUAL TILTS                                                        267
      SFPA_LADDERTILT(1)=0.0                                              267
      SFPA_LADDERTILT(2)=-6.0                                             267
      SFPA_LADDERTILT(3)=-7.0                                             267
      SFPA_LADDERTILT(4)=-7.0                                             267
      SFPA_LADDERTILT(5)=-7.0                                             267
      SFPA_LADDERTILT(6)=0.0                                              267
      SFPA_LADDERTILT(7)=7.0                                              267
      SFPA_LADDERTILT(8)=7.0                                              267
      SFPA_LADDERTILT(9)=7.0                                              267
      SFPA_LADDERTILT(10)=6.0                                             267
      SFPA_LADDERTILT(11)=0.0                                             267
      SFPA_LADDERTILT(12)=-6.0                                            267
      SFPA_LADDERTILT(13)=-7.0                                            267
      SFPA_LADDERTILT(14)=-7.0                                            267
      SFPA_LADDERTILT(15)=-7.0                                            267
      SFPA_LADDERTILT(16)=0.0                                             267
      SFPA_LADDERTILT(17)=7.0                                             267
      SFPA_LADDERTILT(18)=7.0                                             267
      SFPA_LADDERTILT(19)=7.0                                             267
      SFPA_LADDERTILT(20)=6.0                                             267
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 267
C INDIVIDUAL RADII                                                        272
      SFPA_LADDERRADIUS(1)=23.177                                         272
      SFPA_LADDERRADIUS(2)=22.800                                         272
      SFPA_LADDERRADIUS(3)=22.800                                         272
      SFPA_LADDERRADIUS(4)=22.800                                         272
      SFPA_LADDERRADIUS(5)=23.800                                         272
      SFPA_LADDERRADIUS(6)=22.500                                         272
      SFPA_LADDERRADIUS(7)=23.800                                         272
      SFPA_LADDERRADIUS(8)=22.800                                         272
      SFPA_LADDERRADIUS(9)=22.800                                         272
      SFPA_LADDERRADIUS(10)=22.800                                        272
      SFPA_LADDERRADIUS(11)=23.177                                        272
      SFPA_LADDERRADIUS(12)=22.800                                        272
      SFPA_LADDERRADIUS(13)=22.800                                        272
      SFPA_LADDERRADIUS(14)=22.800                                        272
      SFPA_LADDERRADIUS(15)=23.800                                        272
      SFPA_LADDERRADIUS(16)=22.500                                        272
      SFPA_LADDERRADIUS(17)=23.800                                        272
      SFPA_LADDERRADIUS(18)=22.800                                        272
      SFPA_LADDERRADIUS(19)=22.800                                        272
      SFPA_LADDERRADIUS(20)=22.800                                        272
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               272
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    273
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           273
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   273
C *                                                                       273
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    273
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           273
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   273
C *                                                                       274
      CALL AGSBEGM(MODULE,IPRIN)                                          274
      IF (IPRIN.LT.0) RETURN                                              274
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   274
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      274
      BPATH_SFPA='SFPA*'                                                  274
      IDNUM_SFPA(1)=0                                                     275
C GEOMETRY VERSION                                                        275
      SFPA_VERSION=4                                                      275
      CSFPA_VERSION='GEOMETRY VERSION'                                    275
C PRESENCE OF LADDERS                                                     277
      SFPA_LADDERMAP(1)=1                                                 277
      SFPA_LADDERMAP(2)=1                                                 277
      SFPA_LADDERMAP(3)=1                                                 277
      SFPA_LADDERMAP(4)=1                                                 277
      SFPA_LADDERMAP(5)=1                                                 277
      SFPA_LADDERMAP(6)=1                                                 277
      SFPA_LADDERMAP(7)=1                                                 277
      SFPA_LADDERMAP(8)=1                                                 277
      SFPA_LADDERMAP(9)=1                                                 277
      SFPA_LADDERMAP(10)=1                                                277
      SFPA_LADDERMAP(11)=1                                                277
      SFPA_LADDERMAP(12)=1                                                277
      SFPA_LADDERMAP(13)=1                                                277
      SFPA_LADDERMAP(14)=1                                                277
      SFPA_LADDERMAP(15)=1                                                277
      SFPA_LADDERMAP(16)=1                                                277
      SFPA_LADDERMAP(17)=1                                                277
      SFPA_LADDERMAP(18)=1                                                277
      SFPA_LADDERMAP(19)=1                                                277
      SFPA_LADDERMAP(20)=1                                                277
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               277
C INDIVIDUAL ANGLES                                                       281
      SFPA_LADDERANGLE(1)=90.0                                            281
      SFPA_LADDERANGLE(2)=108.3                                           281
      SFPA_LADDERANGLE(3)=126.6                                           281
      SFPA_LADDERANGLE(4)=144.4                                           281
      SFPA_LADDERANGLE(5)=162.2                                           281
      SFPA_LADDERANGLE(6)=180.0                                           281
      SFPA_LADDERANGLE(7)=197.8                                           281
      SFPA_LADDERANGLE(8)=215.6                                           281
      SFPA_LADDERANGLE(9)=233.4                                           281
      SFPA_LADDERANGLE(10)=251.7                                          281
      SFPA_LADDERANGLE(11)=270.0                                          281
      SFPA_LADDERANGLE(12)=288.3                                          281
      SFPA_LADDERANGLE(13)=306.6                                          281
      SFPA_LADDERANGLE(14)=324.4                                          281
      SFPA_LADDERANGLE(15)=342.2                                          281
      SFPA_LADDERANGLE(16)=0.0                                            281
      SFPA_LADDERANGLE(17)=17.8                                           281
      SFPA_LADDERANGLE(18)=35.6                                           281
      SFPA_LADDERANGLE(19)=53.4                                           281
      SFPA_LADDERANGLE(20)=71.7                                           281
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               281
C INDIVIDUAL TILTS                                                        285
      SFPA_LADDERTILT(1)=0.0                                              285
      SFPA_LADDERTILT(2)=-6.0                                             285
      SFPA_LADDERTILT(3)=-7.0                                             285
      SFPA_LADDERTILT(4)=-7.0                                             285
      SFPA_LADDERTILT(5)=-7.0                                             285
      SFPA_LADDERTILT(6)=0.0                                              285
      SFPA_LADDERTILT(7)=7.0                                              285
      SFPA_LADDERTILT(8)=7.0                                              285
      SFPA_LADDERTILT(9)=7.0                                              285
      SFPA_LADDERTILT(10)=6.0                                             285
      SFPA_LADDERTILT(11)=0.0                                             285
      SFPA_LADDERTILT(12)=-6.0                                            285
      SFPA_LADDERTILT(13)=-7.0                                            285
      SFPA_LADDERTILT(14)=-7.0                                            285
      SFPA_LADDERTILT(15)=-7.0                                            285
      SFPA_LADDERTILT(16)=0.0                                             285
      SFPA_LADDERTILT(17)=7.0                                             285
      SFPA_LADDERTILT(18)=7.0                                             285
      SFPA_LADDERTILT(19)=7.0                                             285
      SFPA_LADDERTILT(20)=6.0                                             285
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 285
C INDIVIDUAL RADII                                                        290
      SFPA_LADDERRADIUS(1)=23.177                                         290
      SFPA_LADDERRADIUS(2)=22.800                                         290
      SFPA_LADDERRADIUS(3)=22.800                                         290
      SFPA_LADDERRADIUS(4)=22.800                                         290
      SFPA_LADDERRADIUS(5)=22.800                                         290
      SFPA_LADDERRADIUS(6)=22.500                                         290
      SFPA_LADDERRADIUS(7)=22.800                                         290
      SFPA_LADDERRADIUS(8)=22.800                                         290
      SFPA_LADDERRADIUS(9)=22.800                                         290
      SFPA_LADDERRADIUS(10)=22.800                                        290
      SFPA_LADDERRADIUS(11)=23.177                                        290
      SFPA_LADDERRADIUS(12)=22.800                                        290
      SFPA_LADDERRADIUS(13)=22.800                                        290
      SFPA_LADDERRADIUS(14)=22.800                                        290
      SFPA_LADDERRADIUS(15)=22.800                                        290
      SFPA_LADDERRADIUS(16)=22.500                                        290
      SFPA_LADDERRADIUS(17)=22.800                                        290
      SFPA_LADDERRADIUS(18)=22.800                                        290
      SFPA_LADDERRADIUS(19)=22.800                                        290
      SFPA_LADDERRADIUS(20)=22.800                                        290
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               290
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    291
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           291
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   291
C *                                                                       291
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    291
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           291
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   291
C *                                                                       292
      CALL AGSBEGM(MODULE,IPRIN)                                          292
      IF (IPRIN.LT.0) RETURN                                              292
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   292
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      292
      BPATH_SFPA='SFPA*'                                                  292
      IDNUM_SFPA(1)=0                                                     293
C GEOMETRY VERSION                                                        293
      SFPA_VERSION=5                                                      293
      CSFPA_VERSION='GEOMETRY VERSION'                                    293
C PRESENCE OF LADDERS                                                     295
      SFPA_LADDERMAP(1)=1                                                 295
      SFPA_LADDERMAP(2)=1                                                 295
      SFPA_LADDERMAP(3)=1                                                 295
      SFPA_LADDERMAP(4)=1                                                 295
      SFPA_LADDERMAP(5)=1                                                 295
      SFPA_LADDERMAP(6)=1                                                 295
      SFPA_LADDERMAP(7)=1                                                 295
      SFPA_LADDERMAP(8)=1                                                 295
      SFPA_LADDERMAP(9)=1                                                 295
      SFPA_LADDERMAP(10)=1                                                295
      SFPA_LADDERMAP(11)=1                                                295
      SFPA_LADDERMAP(12)=1                                                295
      SFPA_LADDERMAP(13)=1                                                295
      SFPA_LADDERMAP(14)=1                                                295
      SFPA_LADDERMAP(15)=1                                                295
      SFPA_LADDERMAP(16)=1                                                295
      SFPA_LADDERMAP(17)=1                                                295
      SFPA_LADDERMAP(18)=1                                                295
      SFPA_LADDERMAP(19)=1                                                295
      SFPA_LADDERMAP(20)=1                                                295
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               295
C INDIVIDUAL ANGLES                                                       299
      SFPA_LADDERANGLE(1)=90.0                                            299
      SFPA_LADDERANGLE(2)=108.3                                           299
      SFPA_LADDERANGLE(3)=126.6                                           299
      SFPA_LADDERANGLE(4)=144.4                                           299
      SFPA_LADDERANGLE(5)=162.2                                           299
      SFPA_LADDERANGLE(6)=180.0                                           299
      SFPA_LADDERANGLE(7)=197.8                                           299
      SFPA_LADDERANGLE(8)=215.6                                           299
      SFPA_LADDERANGLE(9)=233.4                                           299
      SFPA_LADDERANGLE(10)=251.7                                          299
      SFPA_LADDERANGLE(11)=270.0                                          299
      SFPA_LADDERANGLE(12)=288.3                                          299
      SFPA_LADDERANGLE(13)=306.6                                          299
      SFPA_LADDERANGLE(14)=324.4                                          299
      SFPA_LADDERANGLE(15)=342.2                                          299
      SFPA_LADDERANGLE(16)=0.0                                            299
      SFPA_LADDERANGLE(17)=17.8                                           299
      SFPA_LADDERANGLE(18)=35.6                                           299
      SFPA_LADDERANGLE(19)=53.4                                           299
      SFPA_LADDERANGLE(20)=71.7                                           299
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               299
C INDIVIDUAL TILTS                                                        303
      SFPA_LADDERTILT(1)=0.0                                              303
      SFPA_LADDERTILT(2)=-6.0                                             303
      SFPA_LADDERTILT(3)=-7.0                                             303
      SFPA_LADDERTILT(4)=-7.0                                             303
      SFPA_LADDERTILT(5)=-7.0                                             303
      SFPA_LADDERTILT(6)=0.0                                              303
      SFPA_LADDERTILT(7)=7.0                                              303
      SFPA_LADDERTILT(8)=7.0                                              303
      SFPA_LADDERTILT(9)=7.0                                              303
      SFPA_LADDERTILT(10)=6.0                                             303
      SFPA_LADDERTILT(11)=0.0                                             303
      SFPA_LADDERTILT(12)=-6.0                                            303
      SFPA_LADDERTILT(13)=-7.0                                            303
      SFPA_LADDERTILT(14)=-7.0                                            303
      SFPA_LADDERTILT(15)=-7.0                                            303
      SFPA_LADDERTILT(16)=0.0                                             303
      SFPA_LADDERTILT(17)=7.0                                             303
      SFPA_LADDERTILT(18)=7.0                                             303
      SFPA_LADDERTILT(19)=7.0                                             303
      SFPA_LADDERTILT(20)=6.0                                             303
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 303
C INDIVIDUAL RADII                                                        308
      SFPA_LADDERRADIUS(1)=23.177                                         308
      SFPA_LADDERRADIUS(2)=22.800                                         308
      SFPA_LADDERRADIUS(3)=22.600                                         308
      SFPA_LADDERRADIUS(4)=22.600                                         308
      SFPA_LADDERRADIUS(5)=22.600                                         308
      SFPA_LADDERRADIUS(6)=22.300                                         308
      SFPA_LADDERRADIUS(7)=22.600                                         308
      SFPA_LADDERRADIUS(8)=22.600                                         308
      SFPA_LADDERRADIUS(9)=22.600                                         308
      SFPA_LADDERRADIUS(10)=22.800                                        308
      SFPA_LADDERRADIUS(11)=23.177                                        308
      SFPA_LADDERRADIUS(12)=22.800                                        308
      SFPA_LADDERRADIUS(13)=22.600                                        308
      SFPA_LADDERRADIUS(14)=22.600                                        308
      SFPA_LADDERRADIUS(15)=22.600                                        308
      SFPA_LADDERRADIUS(16)=22.500                                        308
      SFPA_LADDERRADIUS(17)=22.600                                        308
      SFPA_LADDERRADIUS(18)=22.600                                        308
      SFPA_LADDERRADIUS(19)=22.600                                        308
      SFPA_LADDERRADIUS(20)=22.800                                        308
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               308
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    309
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           309
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   309
C *                                                                       309
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    309
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           309
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   309
C use default SSDP bank                                                   310
      BPATH_SSDP='SSDP*'                                                  310
      IDNUM_SSDP(1)=0                                                     310
      CALL AGDGETP('SISDGEO4','*',-999,'SSDP',' ',BANK_SSDP, BPATH_SSDP,  310
     *IDNUM_SSDP,1,MAP_SSDP,NAMES_SSDP,BANK_SSDP,LEN_SSDP,LLEN_SSDP,      310
     *LINK_SSDP,FLAG_SSDP)                                                310
C use SFPA bank                                                           311
      BPATH_SFPA='SFPA*'                                                  311
      IDNUM_SFPA(1)=0                                                     311
      SFPA_VERSION=SSDP_CONFIG                                            311
      CALL AGDGETP('SISDGEO4','*',-999,'SFPA',NSFPA_VERSION,              311
     *RE_SFPA_VERSION,BPATH_SFPA,IDNUM_SFPA,1,MAP_SFPA,NAMES_SFPA,        311
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   311
C use SFJP bank                                                           312
      BPATH_SFJP='SFJP*'                                                  312
      IDNUM_SFJP(1)=0                                                     312
      SFJP_VERSION=2                                                      312
      CALL AGDGETP('SISDGEO4','*',-999,'SFJP',NSFJP_VERSION,              312
     *RE_SFJP_VERSION,BPATH_SFJP,IDNUM_SFJP,1,MAP_SFJP,NAMES_SFJP,        312
     *BANK_SFJP,LEN_SFJP,LLEN_SFJP,LINK_SFJP,FLAG_SFJP)                   312
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             313
      CALL AGSRESET                                                       313
      AG_A = 28.08                                                        313
      AG_Z = 14                                                           313
      AG_W = 0.6*1*28./60.                                                313
      AG_TITLE = 'SI'                                                     313
      AG_EXNAME = 'COMPONENT'                                             313
      AG_PARLIST = 'A_Z_W'                                                313
      CALL AXCOMPONENT                                                    313
C *                                                                       313
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  313
C =                                                                       313
      CALL AGSRESET                                                       314
      AG_A = 16                                                           314
      AG_Z = 8                                                            314
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               314
      AG_TITLE = 'O'                                                      314
      AG_EXNAME = 'COMPONENT'                                             314
      AG_PARLIST = 'A_Z_W'                                                314
      CALL AXCOMPONENT                                                    314
C *                                                                       314
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 314
      CALL AGSRESET                                                       315
      AG_A = 12                                                           315
      AG_Z = 6                                                            315
      AG_W = 0.4*8*12./174.                                               315
      AG_TITLE = 'C'                                                      315
      AG_EXNAME = 'COMPONENT'                                             315
      AG_PARLIST = 'A_Z_W'                                                315
      CALL AXCOMPONENT                                                    315
C *                                                                       315
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  315
      CALL AGSRESET                                                       316
      AG_A = 1                                                            316
      AG_Z = 1                                                            316
      AG_W = 0.4*14*1./174.                                               316
      AG_TITLE = 'H'                                                      316
      AG_EXNAME = 'COMPONENT'                                             316
      AG_PARLIST = 'A_Z_W'                                                316
      CALL AXCOMPONENT                                                    316
C *                                                                       316
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  316
      CALL AGSRESET                                                       317
      AG_DENS = 1.7                                                       317
      AG_TITLE = 'G10'                                                    317
      AG_EXNAME = 'MIXTURE'                                               317
      AG_PARLIST = 'DENS'                                                 317
      CALL AXMIXTURE                                                      317
C *                                                                       317
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             317
      CALL AGSRESET                                                       318
      AG_A = 28.08                                                        318
      AG_Z = 14                                                           318
      AG_W = 0.6*1*28./60.                                                318
      AG_TITLE = 'SI'                                                     318
      AG_EXNAME = 'COMPONENT'                                             318
      AG_PARLIST = 'A_Z_W'                                                318
      CALL AXCOMPONENT                                                    318
C *                                                                       318
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  318
C =                                                                       318
      CALL AGSRESET                                                       319
      AG_A = 16                                                           319
      AG_Z = 8                                                            319
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               319
      AG_TITLE = 'O'                                                      319
      AG_EXNAME = 'COMPONENT'                                             319
      AG_PARLIST = 'A_Z_W'                                                319
      CALL AXCOMPONENT                                                    319
C *                                                                       319
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 319
      CALL AGSRESET                                                       320
      AG_A = 12                                                           320
      AG_Z = 6                                                            320
      AG_W = 0.4*8*12./174.                                               320
      AG_TITLE = 'C'                                                      320
      AG_EXNAME = 'COMPONENT'                                             320
      AG_PARLIST = 'A_Z_W'                                                320
      CALL AXCOMPONENT                                                    320
C *                                                                       320
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  320
      CALL AGSRESET                                                       321
      AG_A = 1                                                            321
      AG_Z = 1                                                            321
      AG_W = 0.4*14*1./174.                                               321
      AG_TITLE = 'H'                                                      321
      AG_EXNAME = 'COMPONENT'                                             321
      AG_PARLIST = 'A_Z_W'                                                321
      CALL AXCOMPONENT                                                    321
C *                                                                       321
C Geant MIXTURE : G5 ===> DENS=0.85 <===                                  321
      CALL AGSRESET                                                       322
      AG_DENS = 0.85                                                      322
      AG_TITLE = 'G5'                                                     322
      AG_EXNAME = 'MIXTURE'                                               322
      AG_PARLIST = 'DENS'                                                 322
      CALL AXMIXTURE                                                      322
C *                                                                       322
C Geant COMPONENT : O ===> A=16 Z=8 W=2*16./100. <===                     322
      CALL AGSRESET                                                       323
      AG_A = 16                                                           323
      AG_Z = 8                                                            323
      AG_W = 2*16./100.                                                   323
      AG_TITLE = 'O'                                                      323
      AG_EXNAME = 'COMPONENT'                                             323
      AG_PARLIST = 'A_Z_W'                                                323
      CALL AXCOMPONENT                                                    323
C *                                                                       323
C Geant COMPONENT : C ===> A=12 Z=6 W=5*12./100. <===                     323
      CALL AGSRESET                                                       324
      AG_A = 12                                                           324
      AG_Z = 6                                                            324
      AG_W = 5*12./100.                                                   324
      AG_TITLE = 'C'                                                      324
      AG_EXNAME = 'COMPONENT'                                             324
      AG_PARLIST = 'A_Z_W'                                                324
      CALL AXCOMPONENT                                                    324
C *                                                                       324
C Geant COMPONENT : H ===> A=1 Z=1 W=8*1.0/100. <===                      324
      CALL AGSRESET                                                       325
      AG_A = 1                                                            325
      AG_Z = 1                                                            325
      AG_W = 8*1.0/100.                                                   325
      AG_TITLE = 'H'                                                      325
      AG_EXNAME = 'COMPONENT'                                             325
      AG_PARLIST = 'A_Z_W'                                                325
      CALL AXCOMPONENT                                                    325
C *                                                                       325
C Geant MIXTURE : DELRIN ===> DENS=1.2 <===                               325
      CALL AGSRESET                                                       326
      AG_DENS = 1.2                                                       326
      AG_TITLE = 'DELRIN'                                                 326
      AG_EXNAME = 'MIXTURE'                                               326
      AG_PARLIST = 'DENS'                                                 326
      CALL AXMIXTURE                                                      326
C *                                                                       326
C Check SSDP_PLACEMENT==1                                                 326
      IF (SSDP_PLACEMENT.EQ.1) THEN                                       327
      CALL AGSBEGM(MODULE,IPRIN)                                          328
C Check IPRIN<0                                                           328
      IF (IPRIN.LT.0) RETURN                                              328
C Create SFMO :                                                           328
C Call To Local Procedure SFMO                                            328
      ASSIGN 5020 TO BLOCK_SFMO                                           328
      GO TO  5010                                                         328
5020  CONTINUE                                                            328
C *                                                                       328
C Geant POSITION : SFMO ===> MOTHER='SVTT' <===                           329
      CALL AGSRESET                                                       329
      AG_MOTHER = 'SVTT'                                                  329
      AG_TITLE = 'SFMO'                                                   329
      AG_EXNAME = 'POSITION'                                              329
      AG_PARLIST = 'MOTHER'                                               329
      CALL AXPOSITION                                                     329
C *                                                                       329
      ELSE                                                                329
      CALL AGSBEGM(MODULE,IPRIN)                                          331
C Check IPRIN<0                                                           331
      IF (IPRIN.LT.0) RETURN                                              331
C Create SFMO :                                                           331
C Call To Local Procedure SFMO                                            331
      ASSIGN 5030 TO BLOCK_SFMO                                           331
      GO TO  5010                                                         331
5030  CONTINUE                                                            331
C *                                                                       331
C Geant POSITION : SFMO ===> MOTHER='CAVE' KONLY='ONLY' <===              332
      CALL AGSRESET                                                       332
      AG_MOTHER = 'CAVE'                                                  332
      AG_KONLY = 'ONLY'                                                   332
      AG_TITLE = 'SFMO'                                                   332
      AG_EXNAME = 'POSITION'                                              332
      AG_PARLIST = 'MOTHER_KONLY'                                         332
      CALL AXPOSITION                                                     332
C *                                                                       332
      ENDIF                                                               332
C ----------------------------------------------------------------------  334
C ------------ SFMO                                                       334
      CALL AGSENDM                                                        335
      RETURN                                                              335
C Begin Local Procedure SFMO                                              335
C SFMO IS THE MOTHER OF ALL SILICON STRIP DETECTOR VOLUMES                335
5010  CONTINUE                                                            335
         AG_TITLE='SFMO'                                                  335
         AG_MODULE=MODULE                                                 335
         AG_MARK='V00'                                                    335
         CALL AGSPUSH                                                     335
         AG_MODULE=MODULE                                                 335
C    *                                                                    335
C    Geant MATERIAL : AIR ===> inherited arguments used <===              336
         CALL AGSRESET                                                    336
         AG_TITLE = 'AIR'                                                 336
         AG_EXNAME = 'MATERIAL'                                           336
         AG_PARLIST = 'NONE'                                              336
         CALL AXMATERIAL                                                  336
C    *                                                                    336
C    Geant ATTRIBUTE : SFMO ===> SEEN=1 COLO=6 <===                       336
         CALL AGSRESET                                                    337
         AG_SEEN = 1                                                      337
         AG_COLO = 6                                                      337
         AG_TITLE = 'SFMO'                                                337
         AG_EXNAME = 'ATTRIBUTE'                                          337
         AG_PARLIST = 'SEEN_COLO'                                         337
         CALL AXATTRIBUTE                                                 337
C    *                                                                    337
C    Geant SHAPE : TUBE ===> RMIN=SFPA_RMIN RMAX=SFPA_RMAX, DZ=SFPA_LEN/  337
C    2 <===                                                               337
         CALL AGSRESET                                                    339
         AG_RMIN = SFPA_RMIN                                              339
         AG_RMAX = SFPA_RMAX                                              339
         AG_DZ = SFPA_LEN/2                                               339
         AG_TITLE = 'TUBE'                                                339
         AG_EXNAME = 'SHAPE'                                              339
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      339
         CALL AXSHAPE                                                     339
C    *                                                                    339
         DTHK = SFPA_SMTHK + SFPA_GPTHK                                   339
C    Create SFLM :                                                        341
C    Call To Local Procedure SFLM                                         341
         ASSIGN 5050 TO BLOCK_SFLM                                        341
         GO TO  5040                                                      341
5050     CONTINUE                                                         341
C    *                                                                    341
C    *                                                                    342
         ILAD=1                                                           342
            GO TO 5063                                                    342
5061        ILAD=ILAD+1                                                   342
5063        IF(ILAD-(20).GT.0) GO TO 5062                                 342
C       Check SFPA_LADDERMAP(ILAD).GT.0                                   343
            IF (SFPA_LADDERMAP(ILAD).GT.0) THEN                           343
            ANG = (SFPA_LADDERANGLE(ILAD)*PI)/180.0                       344
            RADTILT = (SFPA_LADDERTILT(ILAD) *PI)/180.0                   345
C       Check ILAD.EQ.1                                                   346
            IF (ILAD.EQ.1) THEN                                           346
            NC=1                                                          347
            ELSE                                                          348
            NC=20-ILAD+2                                                  349
            ENDIF                                                         350
C       Geant POSITION : SFLM ===> X=(SFPA_LADDERRADIUS(ILAD)*COS(ANG)+(  353
C       DTHK*COS(ANG+RADTILT))/2.0) Y=(SFPA_LADDERRADIUS(ILAD)*SIN(ANG)+  353
C       (DTHK*SIN(ANG+RADTILT))/2.0) Z=0 ALPHAZ=SFPA_LADDERANGLE(ILAD)-9  353
C       0.0+SFPA_LADDERTILT(ILAD) NCOPY=NC KONLY='MANY' <===              353
            CALL AGSRESET                                                 353
            AG_X =                                                        353
     *      (SFPA_LADDERRADIUS(ILAD)*COS(ANG)+(DTHK*COS(ANG+RADTILT))/    353
     *      2.0)                                                          353
            AG_Y =                                                        353
     *      (SFPA_LADDERRADIUS(ILAD)*SIN(ANG)+(DTHK*SIN(ANG+RADTILT))/    353
     *      2.0)                                                          353
            AG_Z = 0                                                      353
            AG_ALPHAN=AG_ALPHAN+1                                         353
            AG_ALPHAK(AG_ALPHAN)=3                                        353
            AG_ALPHAL(AG_ALPHAN)=                                         353
     *      SFPA_LADDERANGLE(ILAD)-90.0+SFPA_LADDERTILT(ILAD)             353
            AG_NCOPY = NC                                                 353
            AG_KONLY = 'MANY'                                             353
            AG_TITLE = 'SFLM'                                             353
            AG_EXNAME = 'POSITION'                                        353
            AG_PARLIST = 'X_Y_Z_ALPHAZ_NCOPY_KONLY'                       353
            CALL AXPOSITION                                               353
C       *                                                                 353
            ENDIF                                                         353
         GO TO 5061                                                       355
5062     CONTINUE                                                         355
C    Create SSST :                                                        356
C    Call To Local Procedure SSST                                         356
         ASSIGN 5080 TO BLOCK_SSST                                        356
         GO TO  5070                                                      356
5080     CONTINUE                                                         356
C    *                                                                    356
C    Geant POSITION : SSST ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  358
C    JP_SSST_WIDTH/2.-SFJP_SSST_PZ <===                                   358
         CALL AGSRESET                                                    358
         AG_X = 0.                                                        358
         AG_Y = 0.                                                        358
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSST_WIDTH/                358
     *   2.-SFJP_SSST_PZ                                                  358
         AG_TITLE = 'SSST'                                                358
         AG_EXNAME = 'POSITION'                                           358
         AG_PARLIST = 'X_Y_Z'                                             358
         CALL AXPOSITION                                                  358
C    *                                                                    358
C    Geant POSITION : SSST ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  358
C    JP_SSST_WIDTH/2.+SFJP_SSST_PZ <===                                   358
         CALL AGSRESET                                                    360
         AG_X = 0.                                                        360
         AG_Y = 0.                                                        360
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSST_WIDTH/                360
     *   2.+SFJP_SSST_PZ                                                  360
         AG_TITLE = 'SSST'                                                360
         AG_EXNAME = 'POSITION'                                           360
         AG_PARLIST = 'X_Y_Z'                                             360
         CALL AXPOSITION                                                  360
C    *                                                                    360
C    Geant POSITION : SSST ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  360
C    JP_SSST_WIDTH/2.-SFJP_SSST_PZ ALPHAZ=180. <===                       360
         CALL AGSRESET                                                    362
         AG_X = 0.                                                        362
         AG_Y = 0.                                                        362
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSST_WIDTH/                362
     *   2.-SFJP_SSST_PZ                                                  362
         AG_ALPHAN=AG_ALPHAN+1                                            362
         AG_ALPHAK(AG_ALPHAN)=3                                           362
         AG_ALPHAL(AG_ALPHAN)=180.                                        362
         AG_TITLE = 'SSST'                                                362
         AG_EXNAME = 'POSITION'                                           362
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      362
         CALL AXPOSITION                                                  362
C    *                                                                    362
C    Geant POSITION : SSST ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  362
C    JP_SSST_WIDTH/2.+SFJP_SSST_PZ ALPHAZ=180. <===                       362
         CALL AGSRESET                                                    364
         AG_X = 0.                                                        364
         AG_Y = 0.                                                        364
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSST_WIDTH/                364
     *   2.+SFJP_SSST_PZ                                                  364
         AG_ALPHAN=AG_ALPHAN+1                                            364
         AG_ALPHAK(AG_ALPHAN)=3                                           364
         AG_ALPHAL(AG_ALPHAN)=180.                                        364
         AG_TITLE = 'SSST'                                                364
         AG_EXNAME = 'POSITION'                                           364
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      364
         CALL AXPOSITION                                                  364
C    *                                                                    364
C    Create SSSS :                                                        365
C    Call To Local Procedure SSSS                                         365
         ASSIGN 5100 TO BLOCK_SSSS                                        365
         GO TO  5090                                                      365
5100     CONTINUE                                                         365
C    *                                                                    365
C    Geant POSITION : SSSS ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  367
C    JP_SSSS_WIDTH/2.-SFJP_SSST_PZ <===                                   367
         CALL AGSRESET                                                    367
         AG_X = 0.                                                        367
         AG_Y = 0.                                                        367
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH/                367
     *   2.-SFJP_SSST_PZ                                                  367
         AG_TITLE = 'SSSS'                                                367
         AG_EXNAME = 'POSITION'                                           367
         AG_PARLIST = 'X_Y_Z'                                             367
         CALL AXPOSITION                                                  367
C    *                                                                    367
C    Geant POSITION : SSSS ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  367
C    JP_SSSS_WIDTH/2.+SFJP_SSST_PZ <===                                   367
         CALL AGSRESET                                                    369
         AG_X = 0.                                                        369
         AG_Y = 0.                                                        369
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH/                369
     *   2.+SFJP_SSST_PZ                                                  369
         AG_TITLE = 'SSSS'                                                369
         AG_EXNAME = 'POSITION'                                           369
         AG_PARLIST = 'X_Y_Z'                                             369
         CALL AXPOSITION                                                  369
C    *                                                                    369
C    Geant POSITION : SSSS ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  369
C    JP_SSSS_WIDTH/2.-SFJP_SSST_PZ ALPHAZ=180. <===                       369
         CALL AGSRESET                                                    371
         AG_X = 0.                                                        371
         AG_Y = 0.                                                        371
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH/                371
     *   2.-SFJP_SSST_PZ                                                  371
         AG_ALPHAN=AG_ALPHAN+1                                            371
         AG_ALPHAK(AG_ALPHAN)=3                                           371
         AG_ALPHAL(AG_ALPHAN)=180.                                        371
         AG_TITLE = 'SSSS'                                                371
         AG_EXNAME = 'POSITION'                                           371
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      371
         CALL AXPOSITION                                                  371
C    *                                                                    371
C    Geant POSITION : SSSS ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  371
C    JP_SSSS_WIDTH/2.+SFJP_SSST_PZ ALPHAZ=180. <===                       371
         CALL AGSRESET                                                    373
         AG_X = 0.                                                        373
         AG_Y = 0.                                                        373
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH/                373
     *   2.+SFJP_SSST_PZ                                                  373
         AG_ALPHAN=AG_ALPHAN+1                                            373
         AG_ALPHAK(AG_ALPHAN)=3                                           373
         AG_ALPHAL(AG_ALPHAN)=180.                                        373
         AG_TITLE = 'SSSS'                                                373
         AG_EXNAME = 'POSITION'                                           373
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      373
         CALL AXPOSITION                                                  373
C    *                                                                    373
C    Create SSRT :                                                        374
C    Call To Local Procedure SSRT                                         374
         ASSIGN 5120 TO BLOCK_SSRT                                        374
         GO TO  5110                                                      374
5120     CONTINUE                                                         374
C    *                                                                    374
C    Geant POSITION : SSRT ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  376
C    JP_SSST_WIDTH/2.-SFJP_SSST_PZ <===                                   376
         CALL AGSRESET                                                    376
         AG_X = 0.                                                        376
         AG_Y = 0.                                                        376
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSST_WIDTH/                376
     *   2.-SFJP_SSST_PZ                                                  376
         AG_TITLE = 'SSRT'                                                376
         AG_EXNAME = 'POSITION'                                           376
         AG_PARLIST = 'X_Y_Z'                                             376
         CALL AXPOSITION                                                  376
C    *                                                                    376
C    Geant POSITION : SSRT ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  376
C    JP_SSST_WIDTH/2.+SFJP_SSST_PZ <===                                   376
         CALL AGSRESET                                                    378
         AG_X = 0.                                                        378
         AG_Y = 0.                                                        378
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSST_WIDTH/                378
     *   2.+SFJP_SSST_PZ                                                  378
         AG_TITLE = 'SSRT'                                                378
         AG_EXNAME = 'POSITION'                                           378
         AG_PARLIST = 'X_Y_Z'                                             378
         CALL AXPOSITION                                                  378
C    *                                                                    378
C    Geant POSITION : SSRT ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  378
C    JP_SSST_WIDTH/2.-SFJP_SSST_PZ ALPHAZ=180. <===                       378
         CALL AGSRESET                                                    380
         AG_X = 0.                                                        380
         AG_Y = 0.                                                        380
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSST_WIDTH/                380
     *   2.-SFJP_SSST_PZ                                                  380
         AG_ALPHAN=AG_ALPHAN+1                                            380
         AG_ALPHAK(AG_ALPHAN)=3                                           380
         AG_ALPHAL(AG_ALPHAN)=180.                                        380
         AG_TITLE = 'SSRT'                                                380
         AG_EXNAME = 'POSITION'                                           380
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      380
         CALL AXPOSITION                                                  380
C    *                                                                    380
C    Geant POSITION : SSRT ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  380
C    JP_SSST_WIDTH/2.+SFJP_SSST_PZ ALPHAZ=180. <===                       380
         CALL AGSRESET                                                    382
         AG_X = 0.                                                        382
         AG_Y = 0.                                                        382
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSST_WIDTH/                382
     *   2.+SFJP_SSST_PZ                                                  382
         AG_ALPHAN=AG_ALPHAN+1                                            382
         AG_ALPHAK(AG_ALPHAN)=3                                           382
         AG_ALPHAL(AG_ALPHAN)=180.                                        382
         AG_TITLE = 'SSRT'                                                382
         AG_EXNAME = 'POSITION'                                           382
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      382
         CALL AXPOSITION                                                  382
C    *                                                                    382
C    Create SSRS :                                                        383
C    Call To Local Procedure SSRS                                         383
         ASSIGN 5140 TO BLOCK_SSRS                                        383
         GO TO  5130                                                      383
5140     CONTINUE                                                         383
C    *                                                                    383
C    Geant POSITION : SSRS ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  385
C    JP_SSSS_WIDTH/2.-SFJP_SSST_PZ <===                                   385
         CALL AGSRESET                                                    385
         AG_X = 0.                                                        385
         AG_Y = 0.                                                        385
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH/                385
     *   2.-SFJP_SSST_PZ                                                  385
         AG_TITLE = 'SSRS'                                                385
         AG_EXNAME = 'POSITION'                                           385
         AG_PARLIST = 'X_Y_Z'                                             385
         CALL AXPOSITION                                                  385
C    *                                                                    385
C    Geant POSITION : SSRS ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  385
C    JP_SSSS_WIDTH/2.+SFJP_SSST_PZ <===                                   385
         CALL AGSRESET                                                    387
         AG_X = 0.                                                        387
         AG_Y = 0.                                                        387
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH/                387
     *   2.+SFJP_SSST_PZ                                                  387
         AG_TITLE = 'SSRS'                                                387
         AG_EXNAME = 'POSITION'                                           387
         AG_PARLIST = 'X_Y_Z'                                             387
         CALL AXPOSITION                                                  387
C    *                                                                    387
C    Geant POSITION : SSRS ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  387
C    JP_SSSS_WIDTH/2.-SFJP_SSST_PZ ALPHAZ=180. <===                       387
         CALL AGSRESET                                                    389
         AG_X = 0.                                                        389
         AG_Y = 0.                                                        389
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH/                389
     *   2.-SFJP_SSST_PZ                                                  389
         AG_ALPHAN=AG_ALPHAN+1                                            389
         AG_ALPHAK(AG_ALPHAN)=3                                           389
         AG_ALPHAL(AG_ALPHAN)=180.                                        389
         AG_TITLE = 'SSRS'                                                389
         AG_EXNAME = 'POSITION'                                           389
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      389
         CALL AXPOSITION                                                  389
C    *                                                                    389
C    Geant POSITION : SSRS ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  389
C    JP_SSSS_WIDTH/2.+SFJP_SSST_PZ ALPHAZ=180. <===                       389
         CALL AGSRESET                                                    391
         AG_X = 0.                                                        391
         AG_Y = 0.                                                        391
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH/                391
     *   2.+SFJP_SSST_PZ                                                  391
         AG_ALPHAN=AG_ALPHAN+1                                            391
         AG_ALPHAK(AG_ALPHAN)=3                                           391
         AG_ALPHAL(AG_ALPHAN)=180.                                        391
         AG_TITLE = 'SSRS'                                                391
         AG_EXNAME = 'POSITION'                                           391
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      391
         CALL AXPOSITION                                                  391
C    *                                                                    391
C    Create SSLB :                                                        392
C    Call To Local Procedure SSLB                                         392
         ASSIGN 5160 TO BLOCK_SSLB                                        392
         GO TO  5150                                                      392
5160     CONTINUE                                                         392
C    *                                                                    392
C    Check SFJP_VERSION==1                                                393
         IF (SFJP_VERSION.EQ.1) THEN                                      393
C    Geant POSITION : SSLB ===> X=SFJP_SSLB_PX Y=SFJP_SSLB_PX Z=-SFJP_SF  396
C    LU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ/2. ALPH  396
C    AZ=SFJP_ALPHAZH <===                                                 396
         CALL AGSRESET                                                    396
         AG_X = SFJP_SSLB_PX                                              396
         AG_Y = SFJP_SSLB_PX                                              396
         AG_Z =                                                           396
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          396
     *   SFJP_SSLB_DZ/2.                                                  396
         AG_ALPHAN=AG_ALPHAN+1                                            396
         AG_ALPHAK(AG_ALPHAN)=3                                           396
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                396
         AG_TITLE = 'SSLB'                                                396
         AG_EXNAME = 'POSITION'                                           396
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      396
         CALL AXPOSITION                                                  396
C    *                                                                    396
C    Geant POSITION : SSLB ===> X=-SFJP_SSLB_PX Y=-SFJP_SSLB_PX Z=-SFJP_  396
C    SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ/2. AL  396
C    PHAZ=SFJP_ALPHAZH <===                                               396
         CALL AGSRESET                                                    399
         AG_X = -SFJP_SSLB_PX                                             399
         AG_Y = -SFJP_SSLB_PX                                             399
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
C    Geant POSITION : SSLB ===> X=SFJP_SSLB_PX Y=-SFJP_SSLB_PX Z=-SFJP_S  399
C    FLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ/2. ALP  399
C    HAZ=-SFJP_ALPHAZH <===                                               399
         CALL AGSRESET                                                    402
         AG_X = SFJP_SSLB_PX                                              402
         AG_Y = -SFJP_SSLB_PX                                             402
         AG_Z =                                                           402
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          402
     *   SFJP_SSLB_DZ/2.                                                  402
         AG_ALPHAN=AG_ALPHAN+1                                            402
         AG_ALPHAK(AG_ALPHAN)=3                                           402
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               402
         AG_TITLE = 'SSLB'                                                402
         AG_EXNAME = 'POSITION'                                           402
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      402
         CALL AXPOSITION                                                  402
C    *                                                                    402
C    Geant POSITION : SSLB ===> X=-SFJP_SSLB_PX Y=SFJP_SSLB_PX Z=-SFJP_S  402
C    FLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ/2. ALP  402
C    HAZ=-SFJP_ALPHAZH <===                                               402
         CALL AGSRESET                                                    405
         AG_X = -SFJP_SSLB_PX                                             405
         AG_Y = SFJP_SSLB_PX                                              405
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
C    Geant POSITION : SSLB ===> X=SFJP_SSLB_PX Y=SFJP_SSLB_PX Z=+SFJP_SF  405
C    LU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ/2. ALPH  405
C    AZ=SFJP_ALPHAZH <===                                                 405
         CALL AGSRESET                                                    408
         AG_X = SFJP_SSLB_PX                                              408
         AG_Y = SFJP_SSLB_PX                                              408
         AG_Z =                                                           408
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          408
     *   SFJP_SSLB_DZ/2.                                                  408
         AG_ALPHAN=AG_ALPHAN+1                                            408
         AG_ALPHAK(AG_ALPHAN)=3                                           408
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                408
         AG_TITLE = 'SSLB'                                                408
         AG_EXNAME = 'POSITION'                                           408
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      408
         CALL AXPOSITION                                                  408
C    *                                                                    408
C    Geant POSITION : SSLB ===> X=-SFJP_SSLB_PX Y=-SFJP_SSLB_PX Z=+SFJP_  408
C    SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ/2. AL  408
C    PHAZ=SFJP_ALPHAZH <===                                               408
         CALL AGSRESET                                                    411
         AG_X = -SFJP_SSLB_PX                                             411
         AG_Y = -SFJP_SSLB_PX                                             411
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
C    Geant POSITION : SSLB ===> X=SFJP_SSLB_PX Y=-SFJP_SSLB_PX Z=+SFJP_S  411
C    FLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ/2. ALP  411
C    HAZ=-SFJP_ALPHAZH <===                                               411
         CALL AGSRESET                                                    414
         AG_X = SFJP_SSLB_PX                                              414
         AG_Y = -SFJP_SSLB_PX                                             414
         AG_Z =                                                           414
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          414
     *   SFJP_SSLB_DZ/2.                                                  414
         AG_ALPHAN=AG_ALPHAN+1                                            414
         AG_ALPHAK(AG_ALPHAN)=3                                           414
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               414
         AG_TITLE = 'SSLB'                                                414
         AG_EXNAME = 'POSITION'                                           414
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      414
         CALL AXPOSITION                                                  414
C    *                                                                    414
C    Geant POSITION : SSLB ===> X=-SFJP_SSLB_PX Y=SFJP_SSLB_PX Z=+SFJP_S  414
C    FLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ/2. ALP  414
C    HAZ=-SFJP_ALPHAZH <===                                               414
         CALL AGSRESET                                                    417
         AG_X = -SFJP_SSLB_PX                                             417
         AG_Y = SFJP_SSLB_PX                                              417
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
         ELSE                                                             417
C    Geant POSITION : SSLB ===> X=SFJP_SSLT_PX Y=SFJP_SSLT_PX Z=-SFJP_SF  421
C    LU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ/2. ALPH  421
C    AZ=0 <===                                                            421
         CALL AGSRESET                                                    421
         AG_X = SFJP_SSLT_PX                                              421
         AG_Y = SFJP_SSLT_PX                                              421
         AG_Z =                                                           421
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          421
     *   SFJP_SSLB_DZ/2.                                                  421
         AG_ALPHAN=AG_ALPHAN+1                                            421
         AG_ALPHAK(AG_ALPHAN)=3                                           421
         AG_ALPHAL(AG_ALPHAN)=0                                           421
         AG_TITLE = 'SSLB'                                                421
         AG_EXNAME = 'POSITION'                                           421
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      421
         CALL AXPOSITION                                                  421
C    *                                                                    421
C    Geant POSITION : SSLB ===> X=-SFJP_SSLT_PX Y=-SFJP_SSLT_PX Z=-SFJP_  421
C    SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ/2. AL  421
C    PHAZ=0 <===                                                          421
         CALL AGSRESET                                                    424
         AG_X = -SFJP_SSLT_PX                                             424
         AG_Y = -SFJP_SSLT_PX                                             424
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
C    Geant POSITION : SSLB ===> X=SFJP_SSLT_PX Y=-SFJP_SSLT_PX Z=-SFJP_S  424
C    FLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ/2. ALP  424
C    HAZ=0 <===                                                           424
         CALL AGSRESET                                                    427
         AG_X = SFJP_SSLT_PX                                              427
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
C    Geant POSITION : SSLB ===> X=-SFJP_SSLT_PX Y=SFJP_SSLT_PX Z=-SFJP_S  427
C    FLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ/2. ALP  427
C    HAZ=0 <===                                                           427
         CALL AGSRESET                                                    430
         AG_X = -SFJP_SSLT_PX                                             430
         AG_Y = SFJP_SSLT_PX                                              430
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
C    Geant POSITION : SSLB ===> X=SFJP_SSLT_PX Y=SFJP_SSLT_PX Z=+SFJP_SF  430
C    LU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ/2. ALPH  430
C    AY=180 <===                                                          430
         CALL AGSRESET                                                    433
         AG_X = SFJP_SSLT_PX                                              433
         AG_Y = SFJP_SSLT_PX                                              433
         AG_Z =                                                           433
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          433
     *   SFJP_SSLB_DZ/2.                                                  433
         AG_ALPHAN=AG_ALPHAN+1                                            433
         AG_ALPHAK(AG_ALPHAN)=2                                           433
         AG_ALPHAL(AG_ALPHAN)=180                                         433
         AG_TITLE = 'SSLB'                                                433
         AG_EXNAME = 'POSITION'                                           433
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      433
         CALL AXPOSITION                                                  433
C    *                                                                    433
C    Geant POSITION : SSLB ===> X=-SFJP_SSLT_PX Y=-SFJP_SSLT_PX Z=+SFJP_  433
C    SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ/2. AL  433
C    PHAY=180 <===                                                        433
         CALL AGSRESET                                                    436
         AG_X = -SFJP_SSLT_PX                                             436
         AG_Y = -SFJP_SSLT_PX                                             436
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
C    Geant POSITION : SSLB ===> X=SFJP_SSLT_PX Y=-SFJP_SSLT_PX Z=+SFJP_S  436
C    FLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ/2. ALP  436
C    HAY=180 <===                                                         436
         CALL AGSRESET                                                    439
         AG_X = SFJP_SSLT_PX                                              439
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
C    Geant POSITION : SSLB ===> X=-SFJP_SSLT_PX Y=SFJP_SSLT_PX Z=+SFJP_S  439
C    FLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ/2. ALP  439
C    HAY=180 <===                                                         439
         CALL AGSRESET                                                    442
         AG_X = -SFJP_SSLT_PX                                             442
         AG_Y = SFJP_SSLT_PX                                              442
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
         ENDIF                                                            442
C    Create SSLT :                                                        444
C    Call To Local Procedure SSLT                                         444
         ASSIGN 5180 TO BLOCK_SSLT                                        444
         GO TO  5170                                                      444
5180     CONTINUE                                                         444
C    *                                                                    444
C    Geant POSITION : SSLT ===> X=SFJP_SSLT_PX Y=SFJP_SSLT_PX Z=-SFJP_SF  446
C    LU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ-SFJP_SS  446
C    LT_DZ/2. <===                                                        446
         CALL AGSRESET                                                    446
         AG_X = SFJP_SSLT_PX                                              446
         AG_Y = SFJP_SSLT_PX                                              446
         AG_Z =                                                           446
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          446
     *   SFJP_SSLB_DZ-SFJP_SSLT_DZ/2.                                     446
         AG_TITLE = 'SSLT'                                                446
         AG_EXNAME = 'POSITION'                                           446
         AG_PARLIST = 'X_Y_Z'                                             446
         CALL AXPOSITION                                                  446
C    *                                                                    446
C    Geant POSITION : SSLT ===> X=-SFJP_SSLT_PX Y=-SFJP_SSLT_PX Z=-SFJP_  446
C    SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ-SFJP_  446
C    SSLT_DZ/2. <===                                                      446
         CALL AGSRESET                                                    448
         AG_X = -SFJP_SSLT_PX                                             448
         AG_Y = -SFJP_SSLT_PX                                             448
         AG_Z =                                                           448
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          448
     *   SFJP_SSLB_DZ-SFJP_SSLT_DZ/2.                                     448
         AG_TITLE = 'SSLT'                                                448
         AG_EXNAME = 'POSITION'                                           448
         AG_PARLIST = 'X_Y_Z'                                             448
         CALL AXPOSITION                                                  448
C    *                                                                    448
C    Geant POSITION : SSLT ===> X=SFJP_SSLT_PX Y=-SFJP_SSLT_PX Z=-SFJP_S  448
C    FLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ-SFJP_S  448
C    SLT_DZ/2. <===                                                       448
         CALL AGSRESET                                                    450
         AG_X = SFJP_SSLT_PX                                              450
         AG_Y = -SFJP_SSLT_PX                                             450
         AG_Z =                                                           450
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          450
     *   SFJP_SSLB_DZ-SFJP_SSLT_DZ/2.                                     450
         AG_TITLE = 'SSLT'                                                450
         AG_EXNAME = 'POSITION'                                           450
         AG_PARLIST = 'X_Y_Z'                                             450
         CALL AXPOSITION                                                  450
C    *                                                                    450
C    Geant POSITION : SSLT ===> X=-SFJP_SSLT_PX Y=SFJP_SSLT_PX Z=-SFJP_S  450
C    FLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ-SFJP_S  450
C    SLT_DZ/2. <===                                                       450
         CALL AGSRESET                                                    452
         AG_X = -SFJP_SSLT_PX                                             452
         AG_Y = SFJP_SSLT_PX                                              452
         AG_Z =                                                           452
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          452
     *   SFJP_SSLB_DZ-SFJP_SSLT_DZ/2.                                     452
         AG_TITLE = 'SSLT'                                                452
         AG_EXNAME = 'POSITION'                                           452
         AG_PARLIST = 'X_Y_Z'                                             452
         CALL AXPOSITION                                                  452
C    *                                                                    452
C    Geant POSITION : SSLT ===> X=SFJP_SSLT_PX Y=SFJP_SSLT_PX Z=+SFJP_SF  452
C    LU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ+SFJP_SS  452
C    LT_DZ/2. <===                                                        452
         CALL AGSRESET                                                    454
         AG_X = SFJP_SSLT_PX                                              454
         AG_Y = SFJP_SSLT_PX                                              454
         AG_Z =                                                           454
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          454
     *   SFJP_SSLB_DZ+SFJP_SSLT_DZ/2.                                     454
         AG_TITLE = 'SSLT'                                                454
         AG_EXNAME = 'POSITION'                                           454
         AG_PARLIST = 'X_Y_Z'                                             454
         CALL AXPOSITION                                                  454
C    *                                                                    454
C    Geant POSITION : SSLT ===> X=-SFJP_SSLT_PX Y=-SFJP_SSLT_PX Z=+SFJP_  454
C    SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ+SFJP_  454
C    SSLT_DZ/2. <===                                                      454
         CALL AGSRESET                                                    456
         AG_X = -SFJP_SSLT_PX                                             456
         AG_Y = -SFJP_SSLT_PX                                             456
         AG_Z =                                                           456
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          456
     *   SFJP_SSLB_DZ+SFJP_SSLT_DZ/2.                                     456
         AG_TITLE = 'SSLT'                                                456
         AG_EXNAME = 'POSITION'                                           456
         AG_PARLIST = 'X_Y_Z'                                             456
         CALL AXPOSITION                                                  456
C    *                                                                    456
C    Geant POSITION : SSLT ===> X=SFJP_SSLT_PX Y=-SFJP_SSLT_PX Z=+SFJP_S  456
C    FLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ+SFJP_S  456
C    SLT_DZ/2. <===                                                       456
         CALL AGSRESET                                                    458
         AG_X = SFJP_SSLT_PX                                              458
         AG_Y = -SFJP_SSLT_PX                                             458
         AG_Z =                                                           458
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          458
     *   SFJP_SSLB_DZ+SFJP_SSLT_DZ/2.                                     458
         AG_TITLE = 'SSLT'                                                458
         AG_EXNAME = 'POSITION'                                           458
         AG_PARLIST = 'X_Y_Z'                                             458
         CALL AXPOSITION                                                  458
C    *                                                                    458
C    Geant POSITION : SSLT ===> X=-SFJP_SSLT_PX Y=SFJP_SSLT_PX Z=+SFJP_S  458
C    FLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ+SFJP_S  458
C    SLT_DZ/2. <===                                                       458
         CALL AGSRESET                                                    460
         AG_X = -SFJP_SSLT_PX                                             460
         AG_Y = SFJP_SSLT_PX                                              460
         AG_Z =                                                           460
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          460
     *   SFJP_SSLB_DZ+SFJP_SSLT_DZ/2.                                     460
         AG_TITLE = 'SSLT'                                                460
         AG_EXNAME = 'POSITION'                                           460
         AG_PARLIST = 'X_Y_Z'                                             460
         CALL AXPOSITION                                                  460
C    *                                                                    460
C    Create SCMP :                                                        461
C    Call To Local Procedure SCMP                                         461
         ASSIGN 5200 TO BLOCK_SCMP                                        461
         GO TO  5190                                                      461
5200     CONTINUE                                                         461
C    *                                                                    461
C    Geant POSITION : SCMP ===> X=SFJP_SCMP_PX Y=SFJP_SCMP_PX Z=SFJP_SCV  462
C    M_PZ ALPHAZ=-SFJP_ALPHAZH <===                                       462
         CALL AGSRESET                                                    462
         AG_X = SFJP_SCMP_PX                                              462
         AG_Y = SFJP_SCMP_PX                                              462
         AG_Z = SFJP_SCVM_PZ                                              462
         AG_ALPHAN=AG_ALPHAN+1                                            462
         AG_ALPHAK(AG_ALPHAN)=3                                           462
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               462
         AG_TITLE = 'SCMP'                                                462
         AG_EXNAME = 'POSITION'                                           462
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      462
         CALL AXPOSITION                                                  462
C    *                                                                    462
C    Geant POSITION : SCMP ===> X=-SFJP_SCMP_PX Y=-SFJP_SCMP_PX Z=SFJP_S  462
C    CVM_PZ ALPHAZ=-SFJP_ALPHAZH <===                                     462
         CALL AGSRESET                                                    464
         AG_X = -SFJP_SCMP_PX                                             464
         AG_Y = -SFJP_SCMP_PX                                             464
         AG_Z = SFJP_SCVM_PZ                                              464
         AG_ALPHAN=AG_ALPHAN+1                                            464
         AG_ALPHAK(AG_ALPHAN)=3                                           464
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               464
         AG_TITLE = 'SCMP'                                                464
         AG_EXNAME = 'POSITION'                                           464
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      464
         CALL AXPOSITION                                                  464
C    *                                                                    464
C    Geant POSITION : SCMP ===> X=SFJP_SCMP_PX Y=-SFJP_SCMP_PX Z=SFJP_SC  464
C    VM_PZ ALPHAZ=SFJP_ALPHAZH <===                                       464
         CALL AGSRESET                                                    465
         AG_X = SFJP_SCMP_PX                                              465
         AG_Y = -SFJP_SCMP_PX                                             465
         AG_Z = SFJP_SCVM_PZ                                              465
         AG_ALPHAN=AG_ALPHAN+1                                            465
         AG_ALPHAK(AG_ALPHAN)=3                                           465
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                465
         AG_TITLE = 'SCMP'                                                465
         AG_EXNAME = 'POSITION'                                           465
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      465
         CALL AXPOSITION                                                  465
C    *                                                                    465
C    Geant POSITION : SCMP ===> X=-SFJP_SCMP_PX Y=SFJP_SCMP_PX Z=SFJP_SC  465
C    VM_PZ ALPHAZ=SFJP_ALPHAZH <===                                       465
         CALL AGSRESET                                                    466
         AG_X = -SFJP_SCMP_PX                                             466
         AG_Y = SFJP_SCMP_PX                                              466
         AG_Z = SFJP_SCVM_PZ                                              466
         AG_ALPHAN=AG_ALPHAN+1                                            466
         AG_ALPHAK(AG_ALPHAN)=3                                           466
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                466
         AG_TITLE = 'SCMP'                                                466
         AG_EXNAME = 'POSITION'                                           466
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      466
         CALL AXPOSITION                                                  466
C    *                                                                    466
C    Geant POSITION : SCMP ===> X=SFJP_SCMP_PX Y=SFJP_SCMP_PX Z=-SFJP_SC  466
C    VM_PZ ALPHAZ=-SFJP_ALPHAZH <===                                      466
         CALL AGSRESET                                                    468
         AG_X = SFJP_SCMP_PX                                              468
         AG_Y = SFJP_SCMP_PX                                              468
         AG_Z = -SFJP_SCVM_PZ                                             468
         AG_ALPHAN=AG_ALPHAN+1                                            468
         AG_ALPHAK(AG_ALPHAN)=3                                           468
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               468
         AG_TITLE = 'SCMP'                                                468
         AG_EXNAME = 'POSITION'                                           468
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      468
         CALL AXPOSITION                                                  468
C    *                                                                    468
C    Geant POSITION : SCMP ===> X=-SFJP_SCMP_PX Y=-SFJP_SCMP_PX Z=-SFJP_  468
C    SCVM_PZ ALPHAZ=-SFJP_ALPHAZH <===                                    468
         CALL AGSRESET                                                    470
         AG_X = -SFJP_SCMP_PX                                             470
         AG_Y = -SFJP_SCMP_PX                                             470
         AG_Z = -SFJP_SCVM_PZ                                             470
         AG_ALPHAN=AG_ALPHAN+1                                            470
         AG_ALPHAK(AG_ALPHAN)=3                                           470
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               470
         AG_TITLE = 'SCMP'                                                470
         AG_EXNAME = 'POSITION'                                           470
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      470
         CALL AXPOSITION                                                  470
C    *                                                                    470
C    Geant POSITION : SCMP ===> X=SFJP_SCMP_PX Y=-SFJP_SCMP_PX Z=-SFJP_S  470
C    CVM_PZ ALPHAZ=SFJP_ALPHAZH <===                                      470
         CALL AGSRESET                                                    472
         AG_X = SFJP_SCMP_PX                                              472
         AG_Y = -SFJP_SCMP_PX                                             472
         AG_Z = -SFJP_SCVM_PZ                                             472
         AG_ALPHAN=AG_ALPHAN+1                                            472
         AG_ALPHAK(AG_ALPHAN)=3                                           472
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                472
         AG_TITLE = 'SCMP'                                                472
         AG_EXNAME = 'POSITION'                                           472
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      472
         CALL AXPOSITION                                                  472
C    *                                                                    472
C    Geant POSITION : SCMP ===> X=-SFJP_SCMP_PX Y=SFJP_SCMP_PX Z=-SFJP_S  472
C    CVM_PZ ALPHAZ=SFJP_ALPHAZH <===                                      472
         CALL AGSRESET                                                    474
         AG_X = -SFJP_SCMP_PX                                             474
         AG_Y = SFJP_SCMP_PX                                              474
         AG_Z = -SFJP_SCVM_PZ                                             474
         AG_ALPHAN=AG_ALPHAN+1                                            474
         AG_ALPHAK(AG_ALPHAN)=3                                           474
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                474
         AG_TITLE = 'SCMP'                                                474
         AG_EXNAME = 'POSITION'                                           474
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      474
         CALL AXPOSITION                                                  474
C    *                                                                    474
C    Create SCVM :                                                        475
C    Call To Local Procedure SCVM                                         475
         ASSIGN 5220 TO BLOCK_SCVM                                        475
         GO TO  5210                                                      475
5220     CONTINUE                                                         475
C    *                                                                    475
C    Geant POSITION : SCVM ===> X=SFJP_SCVM_PX Y=SFJP_SCVM_PX Z=SFJP_SCV  476
C    M_PZ ALPHAZ=-SFJP_ALPHAZH <===                                       476
         CALL AGSRESET                                                    476
         AG_X = SFJP_SCVM_PX                                              476
         AG_Y = SFJP_SCVM_PX                                              476
         AG_Z = SFJP_SCVM_PZ                                              476
         AG_ALPHAN=AG_ALPHAN+1                                            476
         AG_ALPHAK(AG_ALPHAN)=3                                           476
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               476
         AG_TITLE = 'SCVM'                                                476
         AG_EXNAME = 'POSITION'                                           476
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      476
         CALL AXPOSITION                                                  476
C    *                                                                    476
C    Geant POSITION : SCVM ===> X=-SFJP_SCVM_PX Y=-SFJP_SCVM_PX Z=SFJP_S  476
C    CVM_PZ ALPHAZ=(180.-SFJP_ALPHAZH) <===                               476
         CALL AGSRESET                                                    478
         AG_X = -SFJP_SCVM_PX                                             478
         AG_Y = -SFJP_SCVM_PX                                             478
         AG_Z = SFJP_SCVM_PZ                                              478
         AG_ALPHAN=AG_ALPHAN+1                                            478
         AG_ALPHAK(AG_ALPHAN)=3                                           478
         AG_ALPHAL(AG_ALPHAN)=(180.-SFJP_ALPHAZH)                         478
         AG_TITLE = 'SCVM'                                                478
         AG_EXNAME = 'POSITION'                                           478
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      478
         CALL AXPOSITION                                                  478
C    *                                                                    478
C    Geant POSITION : SCVM ===> X=SFJP_SCVM_PX Y=-SFJP_SCVM_PX Z=SFJP_SC  478
C    VM_PZ ALPHAZ=-(180.-SFJP_ALPHAZH) <===                               478
         CALL AGSRESET                                                    480
         AG_X = SFJP_SCVM_PX                                              480
         AG_Y = -SFJP_SCVM_PX                                             480
         AG_Z = SFJP_SCVM_PZ                                              480
         AG_ALPHAN=AG_ALPHAN+1                                            480
         AG_ALPHAK(AG_ALPHAN)=3                                           480
         AG_ALPHAL(AG_ALPHAN)=-(180.-SFJP_ALPHAZH)                        480
         AG_TITLE = 'SCVM'                                                480
         AG_EXNAME = 'POSITION'                                           480
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      480
         CALL AXPOSITION                                                  480
C    *                                                                    480
C    Geant POSITION : SCVM ===> X=-SFJP_SCVM_PX Y=SFJP_SCVM_PX Z=SFJP_SC  480
C    VM_PZ ALPHAZ=SFJP_ALPHAZH <===                                       480
         CALL AGSRESET                                                    481
         AG_X = -SFJP_SCVM_PX                                             481
         AG_Y = SFJP_SCVM_PX                                              481
         AG_Z = SFJP_SCVM_PZ                                              481
         AG_ALPHAN=AG_ALPHAN+1                                            481
         AG_ALPHAK(AG_ALPHAN)=3                                           481
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                481
         AG_TITLE = 'SCVM'                                                481
         AG_EXNAME = 'POSITION'                                           481
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      481
         CALL AXPOSITION                                                  481
C    *                                                                    481
C    Geant POSITION : SCVM ===> X=SFJP_SCVM_PX Y=SFJP_SCVM_PX Z=-SFJP_SC  481
C    VM_PZ ALPHAZ=-SFJP_ALPHAZH <===                                      481
         CALL AGSRESET                                                    483
         AG_X = SFJP_SCVM_PX                                              483
         AG_Y = SFJP_SCVM_PX                                              483
         AG_Z = -SFJP_SCVM_PZ                                             483
         AG_ALPHAN=AG_ALPHAN+1                                            483
         AG_ALPHAK(AG_ALPHAN)=3                                           483
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               483
         AG_TITLE = 'SCVM'                                                483
         AG_EXNAME = 'POSITION'                                           483
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      483
         CALL AXPOSITION                                                  483
C    *                                                                    483
C    Geant POSITION : SCVM ===> X=-SFJP_SCVM_PX Y=-SFJP_SCVM_PX Z=-SFJP_  483
C    SCVM_PZ ALPHAZ=(180.-SFJP_ALPHAZH) <===                              483
         CALL AGSRESET                                                    485
         AG_X = -SFJP_SCVM_PX                                             485
         AG_Y = -SFJP_SCVM_PX                                             485
         AG_Z = -SFJP_SCVM_PZ                                             485
         AG_ALPHAN=AG_ALPHAN+1                                            485
         AG_ALPHAK(AG_ALPHAN)=3                                           485
         AG_ALPHAL(AG_ALPHAN)=(180.-SFJP_ALPHAZH)                         485
         AG_TITLE = 'SCVM'                                                485
         AG_EXNAME = 'POSITION'                                           485
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      485
         CALL AXPOSITION                                                  485
C    *                                                                    485
C    Geant POSITION : SCVM ===> X=SFJP_SCVM_PX Y=-SFJP_SCVM_PX Z=-SFJP_S  485
C    CVM_PZ ALPHAZ=-(180.-SFJP_ALPHAZH) <===                              485
         CALL AGSRESET                                                    487
         AG_X = SFJP_SCVM_PX                                              487
         AG_Y = -SFJP_SCVM_PX                                             487
         AG_Z = -SFJP_SCVM_PZ                                             487
         AG_ALPHAN=AG_ALPHAN+1                                            487
         AG_ALPHAK(AG_ALPHAN)=3                                           487
         AG_ALPHAL(AG_ALPHAN)=-(180.-SFJP_ALPHAZH)                        487
         AG_TITLE = 'SCVM'                                                487
         AG_EXNAME = 'POSITION'                                           487
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      487
         CALL AXPOSITION                                                  487
C    *                                                                    487
C    Geant POSITION : SCVM ===> X=-SFJP_SCVM_PX Y=SFJP_SCVM_PX Z=-SFJP_S  487
C    CVM_PZ ALPHAZ=SFJP_ALPHAZH <===                                      487
         CALL AGSRESET                                                    489
         AG_X = -SFJP_SCVM_PX                                             489
         AG_Y = SFJP_SCVM_PX                                              489
         AG_Z = -SFJP_SCVM_PZ                                             489
         AG_ALPHAN=AG_ALPHAN+1                                            489
         AG_ALPHAK(AG_ALPHAN)=3                                           489
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                489
         AG_TITLE = 'SCVM'                                                489
         AG_EXNAME = 'POSITION'                                           489
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      489
         CALL AXPOSITION                                                  489
C    *                                                                    489
5011  CALL AGSPOPB                                                        490
      GO TO BLOCK_SFMO, (5010 ,5020,5030)                                 490
C End of Local Procedure                                                  490
C ----------------------------------------------------------------------  491
C ------------ SFLM                                                       491
C Begin Local Procedure SFLM                                              492
C SFLM IS THE MOTHER OF THE LADDER                                        492
5040  CONTINUE                                                            492
         AG_TITLE='SFLM'                                                  492
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
C    Geant ATTRIBUTE : SFLM ===> SEEN=1 COLO=6 <===                       493
         CALL AGSRESET                                                    494
         AG_SEEN = 1                                                      494
         AG_COLO = 6                                                      494
         AG_TITLE = 'SFLM'                                                494
         AG_EXNAME = 'ATTRIBUTE'                                          494
         AG_PARLIST = 'SEEN_COLO'                                         494
         CALL AXATTRIBUTE                                                 494
C    *                                                                    494
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=(SFPA_DMTHK+SFPA_GPTHK+SF  494
C    PA_SMTHK)/2, DZ=SFPA_SMLEN/2 <===                                    494
         CALL AGSRESET                                                    496
         AG_DX = SFPA_DMWID/2                                             496
         AG_DY = (SFPA_DMTHK+SFPA_GPTHK+SFPA_SMTHK)/2                     496
         AG_DZ = SFPA_SMLEN/2                                             496
         AG_TITLE = 'BOX'                                                 496
         AG_EXNAME = 'SHAPE'                                              496
         AG_PARLIST = 'DX_DY_DZ'                                          496
         CALL AXSHAPE                                                     496
C    *                                                                    496
C    Create SFDM :                                                        497
C    Call To Local Procedure SFDM                                         497
         ASSIGN 5240 TO BLOCK_SFDM                                        497
         GO TO  5230                                                      497
5240     CONTINUE                                                         497
C    *                                                                    497
C    Geant POSITION : SFDM ===> Y=-(SFPA_SMTHK+SFPA_GPTHK)/2 <===         498
         CALL AGSRESET                                                    498
         AG_Y = -(SFPA_SMTHK+SFPA_GPTHK)/2                                498
         AG_TITLE = 'SFDM'                                                498
         AG_EXNAME = 'POSITION'                                           498
         AG_PARLIST = 'Y'                                                 498
         CALL AXPOSITION                                                  498
C    *                                                                    498
C    Create SFSM :                                                        499
C    Call To Local Procedure SFSM                                         499
         ASSIGN 5260 TO BLOCK_SFSM                                        499
         GO TO  5250                                                      499
5260     CONTINUE                                                         499
C    *                                                                    499
C    Geant POSITION : SFSM ===> Y=(SFPA_DMTHK+SFPA_GPTHK)/2 <===          500
         CALL AGSRESET                                                    500
         AG_Y = (SFPA_DMTHK+SFPA_GPTHK)/2                                 500
         AG_TITLE = 'SFSM'                                                500
         AG_EXNAME = 'POSITION'                                           500
         AG_PARLIST = 'Y'                                                 500
         CALL AXPOSITION                                                  500
C    *                                                                    500
5041  CALL AGSPOPB                                                        501
      GO TO BLOCK_SFLM, (5040 ,5050)                                      501
C End of Local Procedure                                                  501
C ----------------------------------------------------------------------  502
C ------------ SFDM                                                       502
C Begin Local Procedure SFDM                                              503
C SFDM IS THE MOTHER OF THE DETECTORS                                     503
5230  CONTINUE                                                            503
         AG_TITLE='SFDM'                                                  503
         AG_MODULE=MODULE                                                 503
         AG_MARK='V00'                                                    503
         CALL AGSPUSH                                                     503
         AG_MODULE=MODULE                                                 503
C    *                                                                    503
C    Geant MATERIAL : AIR ===> inherited arguments used <===              504
         CALL AGSRESET                                                    504
         AG_TITLE = 'AIR'                                                 504
         AG_EXNAME = 'MATERIAL'                                           504
         AG_PARLIST = 'NONE'                                              504
         CALL AXMATERIAL                                                  504
C    *                                                                    504
C    Geant ATTRIBUTE : SFDM ===> SEEN=0 COLO=6 <===                       504
         CALL AGSRESET                                                    505
         AG_SEEN = 0                                                      505
         AG_COLO = 6                                                      505
         AG_TITLE = 'SFDM'                                                505
         AG_EXNAME = 'ATTRIBUTE'                                          505
         AG_PARLIST = 'SEEN_COLO'                                         505
         CALL AXATTRIBUTE                                                 505
C    *                                                                    505
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=SFPA_DMTHK/2, DZ=SFPA_WPL  505
C    EN/2 <===                                                            505
         CALL AGSRESET                                                    507
         AG_DX = SFPA_DMWID/2                                             507
         AG_DY = SFPA_DMTHK/2                                             507
         AG_DZ = SFPA_WPLEN/2                                             507
         AG_TITLE = 'BOX'                                                 507
         AG_EXNAME = 'SHAPE'                                              507
         AG_PARLIST = 'DX_DY_DZ'                                          507
         CALL AXSHAPE                                                     507
C    *                                                                    507
         WAFPCKLEN=SFPA_WPLEN/(SFPA_NSSD*1.)                              507
C    *                                                                    509
         IWAF=1                                                           509
            GO TO 5273                                                    509
5271        IWAF=IWAF+1                                                   509
5273        IF(IWAF-(SFPA_NSSD).GT.0) GO TO 5272                          509
C       Create SFSW :                                                     510
C       Call To Local Procedure SFSW                                      510
            ASSIGN 5290 TO BLOCK_SFSW                                     510
            GO TO  5280                                                   510
5290        CONTINUE                                                      510
C       *                                                                 510
C       Geant POSITION : SFSW ===> Z=-(SFPA_WPLEN+WAFPCKLEN)/2+IWAF*WAFP  511
C       CKLEN <===                                                        511
            CALL AGSRESET                                                 511
            AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+IWAF*WAFPCKLEN               511
            AG_TITLE = 'SFSW'                                             511
            AG_EXNAME = 'POSITION'                                        511
            AG_PARLIST = 'Z'                                              511
            CALL AXPOSITION                                               511
C       *                                                                 511
         GO TO 5271                                                       512
5272     CONTINUE                                                         512
5231  CALL AGSPOPB                                                        513
      GO TO BLOCK_SFDM, (5230 ,5240)                                      513
C End of Local Procedure                                                  513
C ----------------------------------------------------------------------  514
C ------------ SFSW                                                       514
C Begin Local Procedure SFSW                                              515
C SFSW IS A SINGLE WAFER CONTAINER                                        515
5280  CONTINUE                                                            515
         AG_TITLE='SFSW'                                                  515
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
C    Geant ATTRIBUTE : SFSW ===> SEEN=0 COLO=6 <===                       516
         CALL AGSRESET                                                    517
         AG_SEEN = 0                                                      517
         AG_COLO = 6                                                      517
         AG_TITLE = 'SFSW'                                                517
         AG_EXNAME = 'ATTRIBUTE'                                          517
         AG_PARLIST = 'SEEN_COLO'                                         517
         CALL AXATTRIBUTE                                                 517
C    *                                                                    517
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=SFJP_SFSW_DY, DZ=SFJP_SFS  517
C    W_DZ <===                                                            517
         CALL AGSRESET                                                    519
         AG_DX = SFPA_DMWID/2                                             519
         AG_DY = SFJP_SFSW_DY                                             519
         AG_DZ = SFJP_SFSW_DZ                                             519
         AG_TITLE = 'BOX'                                                 519
         AG_EXNAME = 'SHAPE'                                              519
         AG_PARLIST = 'DX_DY_DZ'                                          519
         CALL AXSHAPE                                                     519
C    *                                                                    519
C    Create SFSD :                                                        520
C    Call To Local Procedure SFSD                                         520
         ASSIGN 5310 TO BLOCK_SFSD                                        520
         GO TO  5300                                                      520
5310     CONTINUE                                                         520
C    *                                                                    520
C    Geant POSITION : SFSD ===> inherited arguments used <===             521
         CALL AGSRESET                                                    521
         AG_TITLE = 'SFSD'                                                521
         AG_EXNAME = 'POSITION'                                           521
         AG_PARLIST = 'NONE'                                              521
         CALL AXPOSITION                                                  521
C    *                                                                    521
C    Create SFRA :                                                        522
C    Call To Local Procedure SFRA                                         522
         ASSIGN 5330 TO BLOCK_SFRA                                        522
         GO TO  5320                                                      522
5330     CONTINUE                                                         522
C    *                                                                    522
C    Geant POSITION : SFRA ===> X=0.0 Y=SFJP_SFRA_PY Z=SFJP_SFRA_PZ <===  523
         CALL AGSRESET                                                    523
         AG_X = 0.0                                                       523
         AG_Y = SFJP_SFRA_PY                                              523
         AG_Z = SFJP_SFRA_PZ                                              523
         AG_TITLE = 'SFRA'                                                523
         AG_EXNAME = 'POSITION'                                           523
         AG_PARLIST = 'X_Y_Z'                                             523
         CALL AXPOSITION                                                  523
C    *                                                                    523
C    Geant POSITION : SFRA ===> X=0.0 Y=SFJP_SFRA_PY Z=-SFJP_SFRA_PZ <==  523
C    =                                                                    523
         CALL AGSRESET                                                    524
         AG_X = 0.0                                                       524
         AG_Y = SFJP_SFRA_PY                                              524
         AG_Z = -SFJP_SFRA_PZ                                             524
         AG_TITLE = 'SFRA'                                                524
         AG_EXNAME = 'POSITION'                                           524
         AG_PARLIST = 'X_Y_Z'                                             524
         CALL AXPOSITION                                                  524
C    *                                                                    524
C    Create SFRS :                                                        525
C    Call To Local Procedure SFRS                                         525
         ASSIGN 5350 TO BLOCK_SFRS                                        525
         GO TO  5340                                                      525
5350     CONTINUE                                                         525
C    *                                                                    525
C    Geant POSITION : SFRS ===> X=SFJP_SFRS_PX Y=SFJP_SFRS_PY Z=SFJP_SFR  526
C    A_PZ <===                                                            526
         CALL AGSRESET                                                    526
         AG_X = SFJP_SFRS_PX                                              526
         AG_Y = SFJP_SFRS_PY                                              526
         AG_Z = SFJP_SFRA_PZ                                              526
         AG_TITLE = 'SFRS'                                                526
         AG_EXNAME = 'POSITION'                                           526
         AG_PARLIST = 'X_Y_Z'                                             526
         CALL AXPOSITION                                                  526
C    *                                                                    526
C    Geant POSITION : SFRS ===> X=SFJP_SFRS_PX Y=SFJP_SFRS_PY Z=-SFJP_SF  526
C    RA_PZ <===                                                           526
         CALL AGSRESET                                                    527
         AG_X = SFJP_SFRS_PX                                              527
         AG_Y = SFJP_SFRS_PY                                              527
         AG_Z = -SFJP_SFRA_PZ                                             527
         AG_TITLE = 'SFRS'                                                527
         AG_EXNAME = 'POSITION'                                           527
         AG_PARLIST = 'X_Y_Z'                                             527
         CALL AXPOSITION                                                  527
C    *                                                                    527
C    Geant POSITION : SFRS ===> X=-SFJP_SFRS_PX Y=SFJP_SFRS_PY Z=SFJP_SF  527
C    RA_PZ <===                                                           527
         CALL AGSRESET                                                    528
         AG_X = -SFJP_SFRS_PX                                             528
         AG_Y = SFJP_SFRS_PY                                              528
         AG_Z = SFJP_SFRA_PZ                                              528
         AG_TITLE = 'SFRS'                                                528
         AG_EXNAME = 'POSITION'                                           528
         AG_PARLIST = 'X_Y_Z'                                             528
         CALL AXPOSITION                                                  528
C    *                                                                    528
C    Geant POSITION : SFRS ===> X=-SFJP_SFRS_PX Y=SFJP_SFRS_PY Z=-SFJP_S  528
C    FRA_PZ <===                                                          528
         CALL AGSRESET                                                    529
         AG_X = -SFJP_SFRS_PX                                             529
         AG_Y = SFJP_SFRS_PY                                              529
         AG_Z = -SFJP_SFRA_PZ                                             529
         AG_TITLE = 'SFRS'                                                529
         AG_EXNAME = 'POSITION'                                           529
         AG_PARLIST = 'X_Y_Z'                                             529
         CALL AXPOSITION                                                  529
C    *                                                                    529
C    Create SFFX :                                                        530
C    Call To Local Procedure SFFX                                         530
         ASSIGN 5370 TO BLOCK_SFFX                                        530
         GO TO  5360                                                      530
5370     CONTINUE                                                         530
C    *                                                                    530
C    Geant POSITION : SFFX ===> X=0.0 Y=SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_S  531
C    FFX_DYZ Z=SFJP_SFRA_PZ <===                                          531
         CALL AGSRESET                                                    531
         AG_X = 0.0                                                       531
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ                   531
         AG_Z = SFJP_SFRA_PZ                                              531
         AG_TITLE = 'SFFX'                                                531
         AG_EXNAME = 'POSITION'                                           531
         AG_PARLIST = 'X_Y_Z'                                             531
         CALL AXPOSITION                                                  531
C    *                                                                    531
C    Geant POSITION : SFFX ===> X=0.0 Y=SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_S  531
C    FFX_DYZ Z=-SFJP_SFRA_PZ <===                                         531
         CALL AGSRESET                                                    532
         AG_X = 0.0                                                       532
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ                   532
         AG_Z = -SFJP_SFRA_PZ                                             532
         AG_TITLE = 'SFFX'                                                532
         AG_EXNAME = 'POSITION'                                           532
         AG_PARLIST = 'X_Y_Z'                                             532
         CALL AXPOSITION                                                  532
C    *                                                                    532
C    Create SFPI :                                                        533
C    Call To Local Procedure SFPI                                         533
         ASSIGN 5390 TO BLOCK_SFPI                                        533
         GO TO  5380                                                      533
5390     CONTINUE                                                         533
C    *                                                                    533
C    Geant POSITION : SFPI ===> X=SFJP_SFPI_PX Y=SFJP_SFRA_PY+SFJP_SFRA_  536
C    DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2 Z=SFJP_SFPI_PZ ALPHAX=90 <===         536
         CALL AGSRESET                                                    536
         AG_X = SFJP_SFPI_PX                                              536
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2     536
         AG_Z = SFJP_SFPI_PZ                                              536
         AG_ALPHAN=AG_ALPHAN+1                                            536
         AG_ALPHAK(AG_ALPHAN)=1                                           536
         AG_ALPHAL(AG_ALPHAN)=90                                          536
         AG_TITLE = 'SFPI'                                                536
         AG_EXNAME = 'POSITION'                                           536
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      536
         CALL AXPOSITION                                                  536
C    *                                                                    536
C    Geant POSITION : SFPI ===> X=-SFJP_SFPI_PX Y=SFJP_SFRA_PY+SFJP_SFRA  536
C    _DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2 Z=SFJP_SFPI_PZ ALPHAX=90 <===        536
         CALL AGSRESET                                                    539
         AG_X = -SFJP_SFPI_PX                                             539
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
C    Geant POSITION : SFPI ===> X=SFJP_SFPI_PX Y=SFJP_SFRA_PY+SFJP_SFRA_  539
C    DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2 Z=-SFJP_SFPI_PZ ALPHAX=90 <===        539
         CALL AGSRESET                                                    542
         AG_X = SFJP_SFPI_PX                                              542
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2     542
         AG_Z = -SFJP_SFPI_PZ                                             542
         AG_ALPHAN=AG_ALPHAN+1                                            542
         AG_ALPHAK(AG_ALPHAN)=1                                           542
         AG_ALPHAL(AG_ALPHAN)=90                                          542
         AG_TITLE = 'SFPI'                                                542
         AG_EXNAME = 'POSITION'                                           542
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      542
         CALL AXPOSITION                                                  542
C    *                                                                    542
C    Geant POSITION : SFPI ===> X=-SFJP_SFPI_PX Y=SFJP_SFRA_PY+SFJP_SFRA  542
C    _DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2 Z=-SFJP_SFPI_PZ ALPHAX=90 <===       542
         CALL AGSRESET                                                    545
         AG_X = -SFJP_SFPI_PX                                             545
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
C    Create SFAA :                                                        546
C    Call To Local Procedure SFAA                                         546
         ASSIGN 5410 TO BLOCK_SFAA                                        546
         GO TO  5400                                                      546
5410     CONTINUE                                                         546
C    *                                                                    546
C    Geant POSITION : SFAA ===> X=-SFJP_SFAA_PX1-SFJP_SFAA_DX Y=SFJP_SFR  549
C    A_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=SFJP_SFAA_PZ1+SFJP  549
C    _SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ <===                            549
         CALL AGSRESET                                                    549
         AG_X = -SFJP_SFAA_PX1-SFJP_SFAA_DX                               549
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    549
         AG_Z = SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ    549
         AG_TITLE = 'SFAA'                                                549
         AG_EXNAME = 'POSITION'                                           549
         AG_PARLIST = 'X_Y_Z'                                             549
         CALL AXPOSITION                                                  549
C    *                                                                    549
C    Geant POSITION : SFAA ===> X=-SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_  549
C    PX2-SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_  549
C    SFAA_DY Z=SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ <=  549
C    ==                                                                   549
         CALL AGSRESET                                                    552
         AG_X = -SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_PX2-SFJP_SFAA_PX3   552
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    552
         AG_Z = SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ    552
         AG_TITLE = 'SFAA'                                                552
         AG_EXNAME = 'POSITION'                                           552
         AG_PARLIST = 'X_Y_Z'                                             552
         CALL AXPOSITION                                                  552
C    *                                                                    552
C    Geant POSITION : SFAA ===> X=-SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_  552
C    PX2-SFJP_SFAA_PX3-SFJP_SFAA_PX2-SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_S  552
C    FRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-S  552
C    FJP_SFAA_PZ3-SFJP_SFAA_DZ <===                                       552
         CALL AGSRESET                                                    556
         AG_X =                                                           556
     *   -SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_PX2-SFJP_SFAA_PX3-         556
     *   SFJP_SFAA_PX2-SFJP_SFAA_PX3                                      556
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    556
         AG_Z = SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ    556
         AG_TITLE = 'SFAA'                                                556
         AG_EXNAME = 'POSITION'                                           556
         AG_PARLIST = 'X_Y_Z'                                             556
         CALL AXPOSITION                                                  556
C    *                                                                    556
C    Geant POSITION : SFAA ===> X=+SFJP_SFAA_PX1+SFJP_SFAA_DX Y=SFJP_SFR  556
C    A_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=SFJP_SFAA_PZ1+SFJP  556
C    _SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ <===                            556
         CALL AGSRESET                                                    559
         AG_X = +SFJP_SFAA_PX1+SFJP_SFAA_DX                               559
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    559
         AG_Z = SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ    559
         AG_TITLE = 'SFAA'                                                559
         AG_EXNAME = 'POSITION'                                           559
         AG_PARLIST = 'X_Y_Z'                                             559
         CALL AXPOSITION                                                  559
C    *                                                                    559
C    Geant POSITION : SFAA ===> X=+SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_  559
C    PX2+SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_  559
C    SFAA_DY Z=SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ <=  559
C    ==                                                                   559
         CALL AGSRESET                                                    562
         AG_X = +SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_PX2+SFJP_SFAA_PX3   562
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    562
         AG_Z = SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ    562
         AG_TITLE = 'SFAA'                                                562
         AG_EXNAME = 'POSITION'                                           562
         AG_PARLIST = 'X_Y_Z'                                             562
         CALL AXPOSITION                                                  562
C    *                                                                    562
C    Geant POSITION : SFAA ===> X=+SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_  562
C    PX2+SFJP_SFAA_PX3+SFJP_SFAA_PX2+SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_S  562
C    FRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-S  562
C    FJP_SFAA_PZ3-SFJP_SFAA_DZ <===                                       562
         CALL AGSRESET                                                    566
         AG_X =                                                           566
     *   +SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_PX2+SFJP_SFAA_PX3+         566
     *   SFJP_SFAA_PX2+SFJP_SFAA_PX3                                      566
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    566
         AG_Z = SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ    566
         AG_TITLE = 'SFAA'                                                566
         AG_EXNAME = 'POSITION'                                           566
         AG_PARLIST = 'X_Y_Z'                                             566
         CALL AXPOSITION                                                  566
C    *                                                                    566
C    Geant POSITION : SFAA ===> X=-SFJP_SFAA_PX1-SFJP_SFAA_DX Y=SFJP_SFR  566
C    A_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=-SFJP_SFAA_PZ1-SFJ  566
C    P_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ <===                           566
         CALL AGSRESET                                                    569
         AG_X = -SFJP_SFAA_PX1-SFJP_SFAA_DX                               569
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    569
         AG_Z = -SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ   569
         AG_TITLE = 'SFAA'                                                569
         AG_EXNAME = 'POSITION'                                           569
         AG_PARLIST = 'X_Y_Z'                                             569
         CALL AXPOSITION                                                  569
C    *                                                                    569
C    Geant POSITION : SFAA ===> X=-SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_  569
C    PX2-SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_  569
C    SFAA_DY Z=-SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ <  569
C    ===                                                                  569
         CALL AGSRESET                                                    572
         AG_X = -SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_PX2-SFJP_SFAA_PX3   572
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    572
         AG_Z = -SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ   572
         AG_TITLE = 'SFAA'                                                572
         AG_EXNAME = 'POSITION'                                           572
         AG_PARLIST = 'X_Y_Z'                                             572
         CALL AXPOSITION                                                  572
C    *                                                                    572
C    Geant POSITION : SFAA ===> X=-SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_  572
C    PX2-SFJP_SFAA_PX3-SFJP_SFAA_PX2-SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_S  572
C    FRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=-SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+  572
C    SFJP_SFAA_PZ3+SFJP_SFAA_DZ <===                                      572
         CALL AGSRESET                                                    576
         AG_X =                                                           576
     *   -SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_PX2-SFJP_SFAA_PX3-         576
     *   SFJP_SFAA_PX2-SFJP_SFAA_PX3                                      576
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    576
         AG_Z = -SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ   576
         AG_TITLE = 'SFAA'                                                576
         AG_EXNAME = 'POSITION'                                           576
         AG_PARLIST = 'X_Y_Z'                                             576
         CALL AXPOSITION                                                  576
C    *                                                                    576
C    Geant POSITION : SFAA ===> X=+SFJP_SFAA_PX1+SFJP_SFAA_DX Y=SFJP_SFR  576
C    A_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=-SFJP_SFAA_PZ1-SFJ  576
C    P_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ <===                           576
         CALL AGSRESET                                                    579
         AG_X = +SFJP_SFAA_PX1+SFJP_SFAA_DX                               579
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    579
         AG_Z = -SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ   579
         AG_TITLE = 'SFAA'                                                579
         AG_EXNAME = 'POSITION'                                           579
         AG_PARLIST = 'X_Y_Z'                                             579
         CALL AXPOSITION                                                  579
C    *                                                                    579
C    Geant POSITION : SFAA ===> X=+SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_  579
C    PX2+SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_  579
C    SFAA_DY Z=-SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ <  579
C    ===                                                                  579
         CALL AGSRESET                                                    582
         AG_X = +SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_PX2+SFJP_SFAA_PX3   582
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    582
         AG_Z = -SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ   582
         AG_TITLE = 'SFAA'                                                582
         AG_EXNAME = 'POSITION'                                           582
         AG_PARLIST = 'X_Y_Z'                                             582
         CALL AXPOSITION                                                  582
C    *                                                                    582
C    Geant POSITION : SFAA ===> X=+SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_  582
C    PX2+SFJP_SFAA_PX3+SFJP_SFAA_PX2+SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_S  582
C    FRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=-SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+  582
C    SFJP_SFAA_PZ3+SFJP_SFAA_DZ <===                                      582
         CALL AGSRESET                                                    586
         AG_X =                                                           586
     *   +SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_PX2+SFJP_SFAA_PX3+         586
     *   SFJP_SFAA_PX2+SFJP_SFAA_PX3                                      586
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    586
         AG_Z = -SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ   586
         AG_TITLE = 'SFAA'                                                586
         AG_EXNAME = 'POSITION'                                           586
         AG_PARLIST = 'X_Y_Z'                                             586
         CALL AXPOSITION                                                  586
C    *                                                                    586
5281  CALL AGSPOPB                                                        587
      GO TO BLOCK_SFSW, (5280 ,5290)                                      587
C End of Local Procedure                                                  587
C ----------------------------------------------------------------------  588
C ------------ SFRA                                                       588
C Begin Local Procedure SFRA                                              589
C SFRA IS THE HYBRID STIFFNEER                                            589
5320  CONTINUE                                                            589
         AG_TITLE='SFRA'                                                  589
         AG_MODULE=MODULE                                                 589
         AG_MARK='V00'                                                    589
         CALL AGSPUSH                                                     589
         AG_MODULE=MODULE                                                 589
C    *                                                                    589
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           590
         CALL AGSRESET                                                    590
         AG_TITLE = 'CARBON'                                              590
         AG_EXNAME = 'MATERIAL'                                           590
         AG_PARLIST = 'NONE'                                              590
         CALL AXMATERIAL                                                  590
C    *                                                                    590
C    Geant ATTRIBUTE : SFRA ===> SEEN=2 COLO=1 <===                       590
         CALL AGSRESET                                                    591
         AG_SEEN = 2                                                      591
         AG_COLO = 1                                                      591
         AG_TITLE = 'SFRA'                                                591
         AG_EXNAME = 'ATTRIBUTE'                                          591
         AG_PARLIST = 'SEEN_COLO'                                         591
         CALL AXATTRIBUTE                                                 591
C    *                                                                    591
C    Geant SHAPE : BOX ===> DX=SFJP_SFRA_DX DY=SFJP_SFRA_DY, DZ=SFJP_SFR  591
C    A_DZ <===                                                            591
         CALL AGSRESET                                                    593
         AG_DX = SFJP_SFRA_DX                                             593
         AG_DY = SFJP_SFRA_DY                                             593
         AG_DZ = SFJP_SFRA_DZ                                             593
         AG_TITLE = 'BOX'                                                 593
         AG_EXNAME = 'SHAPE'                                              593
         AG_PARLIST = 'DX_DY_DZ'                                          593
         CALL AXSHAPE                                                     593
C    *                                                                    593
5321  CALL AGSPOPB                                                        594
      GO TO BLOCK_SFRA, (5320 ,5330)                                      594
C End of Local Procedure                                                  594
C ----------------------------------------------------------------------  595
C ------------ SFRS                                                       595
C Begin Local Procedure SFRS                                              596
C SFRS TWO SUPPORTS OF THE HYBRID STIFFNEER (PIECE OF IT)                 596
5340  CONTINUE                                                            596
         AG_TITLE='SFRS'                                                  596
         AG_MODULE=MODULE                                                 596
         AG_MARK='V00'                                                    596
         CALL AGSPUSH                                                     596
         AG_MODULE=MODULE                                                 596
C    *                                                                    596
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           597
         CALL AGSRESET                                                    597
         AG_TITLE = 'CARBON'                                              597
         AG_EXNAME = 'MATERIAL'                                           597
         AG_PARLIST = 'NONE'                                              597
         CALL AXMATERIAL                                                  597
C    *                                                                    597
C    Geant ATTRIBUTE : SFRS ===> SEEN=2 COLO=1 <===                       597
         CALL AGSRESET                                                    598
         AG_SEEN = 2                                                      598
         AG_COLO = 1                                                      598
         AG_TITLE = 'SFRS'                                                598
         AG_EXNAME = 'ATTRIBUTE'                                          598
         AG_PARLIST = 'SEEN_COLO'                                         598
         CALL AXATTRIBUTE                                                 598
C    *                                                                    598
C    Geant SHAPE : BOX ===> DX=SFJP_SFRS_DX DY=SFJP_SFRS_DY, DZ=SFJP_SFR  598
C    A_DZ <===                                                            598
         CALL AGSRESET                                                    600
         AG_DX = SFJP_SFRS_DX                                             600
         AG_DY = SFJP_SFRS_DY                                             600
         AG_DZ = SFJP_SFRA_DZ                                             600
         AG_TITLE = 'BOX'                                                 600
         AG_EXNAME = 'SHAPE'                                              600
         AG_PARLIST = 'DX_DY_DZ'                                          600
         CALL AXSHAPE                                                     600
C    *                                                                    600
5341  CALL AGSPOPB                                                        601
      GO TO BLOCK_SFRS, (5340 ,5350)                                      601
C End of Local Procedure                                                  601
C ----------------------------------------------------------------------  602
C ------------ SFFX                                                       602
C Begin Local Procedure SFFX                                              603
C SFFX IS THE FLEX                                                        603
5360  CONTINUE                                                            603
         AG_TITLE='SFFX'                                                  603
         AG_MODULE=MODULE                                                 603
         AG_MARK='V00'                                                    603
         CALL AGSPUSH                                                     603
         AG_MODULE=MODULE                                                 603
C    *                                                                    603
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           604
         CALL AGSRESET                                                    604
         AG_TITLE = 'COPPER'                                              604
         AG_EXNAME = 'MATERIAL'                                           604
         AG_PARLIST = 'NONE'                                              604
         CALL AXMATERIAL                                                  604
C    *                                                                    604
C    Geant ATTRIBUTE : SFFX ===> SEEN=1 COLO=42 <===                      604
         CALL AGSRESET                                                    605
         AG_SEEN = 1                                                      605
         AG_COLO = 42                                                     605
         AG_TITLE = 'SFFX'                                                605
         AG_EXNAME = 'ATTRIBUTE'                                          605
         AG_PARLIST = 'SEEN_COLO'                                         605
         CALL AXATTRIBUTE                                                 605
C    *                                                                    605
C    Geant SHAPE : BOX ===> DX=SFJP_SFFX_DX DY=SFJP_SFFX_DYZ, DZ=SFJP_SF  605
C    RA_DZ <===                                                           605
         CALL AGSRESET                                                    607
         AG_DX = SFJP_SFFX_DX                                             607
         AG_DY = SFJP_SFFX_DYZ                                            607
         AG_DZ = SFJP_SFRA_DZ                                             607
         AG_TITLE = 'BOX'                                                 607
         AG_EXNAME = 'SHAPE'                                              607
         AG_PARLIST = 'DX_DY_DZ'                                          607
         CALL AXSHAPE                                                     607
C    *                                                                    607
5361  CALL AGSPOPB                                                        608
      GO TO BLOCK_SFFX, (5360 ,5370)                                      608
C End of Local Procedure                                                  608
C ----------------------------------------------------------------------  609
C ------------ SFAA                                                       609
C Begin Local Procedure SFAA                                              610
C SFAA IS THE A128C CHIP                                                  610
5400  CONTINUE                                                            610
         AG_TITLE='SFAA'                                                  610
         AG_MODULE=MODULE                                                 610
         AG_MARK='V00'                                                    610
         CALL AGSPUSH                                                     610
         AG_MODULE=MODULE                                                 610
C    *                                                                    610
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          611
         CALL AGSRESET                                                    611
         AG_TITLE = 'SILICON'                                             611
         AG_EXNAME = 'MATERIAL'                                           611
         AG_PARLIST = 'NONE'                                              611
         CALL AXMATERIAL                                                  611
C    *                                                                    611
C    Geant ATTRIBUTE : SFAA ===> SEEN=1 COLO=41 <===                      611
         CALL AGSRESET                                                    612
         AG_SEEN = 1                                                      612
         AG_COLO = 41                                                     612
         AG_TITLE = 'SFAA'                                                612
         AG_EXNAME = 'ATTRIBUTE'                                          612
         AG_PARLIST = 'SEEN_COLO'                                         612
         CALL AXATTRIBUTE                                                 612
C    *                                                                    612
C    Geant SHAPE : BOX ===> DX=SFJP_SFAA_DX DY=SFJP_SFAA_DY, DZ=SFJP_SFA  612
C    A_DZ <===                                                            612
         CALL AGSRESET                                                    614
         AG_DX = SFJP_SFAA_DX                                             614
         AG_DY = SFJP_SFAA_DY                                             614
         AG_DZ = SFJP_SFAA_DZ                                             614
         AG_TITLE = 'BOX'                                                 614
         AG_EXNAME = 'SHAPE'                                              614
         AG_PARLIST = 'DX_DY_DZ'                                          614
         CALL AXSHAPE                                                     614
C    *                                                                    614
5401  CALL AGSPOPB                                                        615
      GO TO BLOCK_SFAA, (5400 ,5410)                                      615
C End of Local Procedure                                                  615
C ----------------------------------------------------------------------  616
C ------------ SFPI                                                       616
C Begin Local Procedure SFPI                                              617
C SFPI ARE THE PIONS                                                      617
5380  CONTINUE                                                            617
         AG_TITLE='SFPI'                                                  617
         AG_MODULE=MODULE                                                 617
         AG_MARK='V00'                                                    617
         CALL AGSPUSH                                                     617
         AG_MODULE=MODULE                                                 617
C    *                                                                    617
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        618
         CALL AGSRESET                                                    618
         AG_TITLE = 'ALUMINIUM'                                           618
         AG_EXNAME = 'MATERIAL'                                           618
         AG_PARLIST = 'NONE'                                              618
         CALL AXMATERIAL                                                  618
C    *                                                                    618
C    Geant ATTRIBUTE : SFPI ===> SEEN=2 COLO=33 <===                      618
         CALL AGSRESET                                                    619
         AG_SEEN = 2                                                      619
         AG_COLO = 33                                                     619
         AG_TITLE = 'SFPI'                                                619
         AG_EXNAME = 'ATTRIBUTE'                                          619
         AG_PARLIST = 'SEEN_COLO'                                         619
         CALL AXATTRIBUTE                                                 619
C    *                                                                    619
C    Geant SHAPE : TUBE ===> RMIN=SFJP_SFPI_RMIN/2. RMAX=SFJP_SFPI_RMAX/  619
C    2., DZ=SFJP_SFPI_PZ/2.0 <===                                         619
         CALL AGSRESET                                                    621
         AG_RMIN = SFJP_SFPI_RMIN/2.                                      621
         AG_RMAX = SFJP_SFPI_RMAX/2.                                      621
         AG_DZ = SFJP_SFPI_PZ/2.0                                         621
         AG_TITLE = 'TUBE'                                                621
         AG_EXNAME = 'SHAPE'                                              621
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      621
         CALL AXSHAPE                                                     621
C    *                                                                    621
C    Create SFPJ :                                                        622
C    Call To Local Procedure SFPJ                                         622
         ASSIGN 5430 TO BLOCK_SFPJ                                        622
         GO TO  5420                                                      622
5430     CONTINUE                                                         622
C    *                                                                    622
C    Geant POSITION : SFPJ ===> X=0. Y=0. Z=-SFJP_SFPI_PZ/2.0+SFJP_SFPJ_  623
C    DZ <===                                                              623
         CALL AGSRESET                                                    623
         AG_X = 0.                                                        623
         AG_Y = 0.                                                        623
         AG_Z = -SFJP_SFPI_PZ/2.0+SFJP_SFPJ_DZ                            623
         AG_TITLE = 'SFPJ'                                                623
         AG_EXNAME = 'POSITION'                                           623
         AG_PARLIST = 'X_Y_Z'                                             623
         CALL AXPOSITION                                                  623
C    *                                                                    623
5381  CALL AGSPOPB                                                        624
      GO TO BLOCK_SFPI, (5380 ,5390)                                      624
C End of Local Procedure                                                  624
C ----------------------------------------------------------------------  625
C ------------ SFPJ                                                       625
C Begin Local Procedure SFPJ                                              626
C SFPJ IS THE BASE OF THE PIONS                                           626
5420  CONTINUE                                                            626
         AG_TITLE='SFPJ'                                                  626
         AG_MODULE=MODULE                                                 626
         AG_MARK='V00'                                                    626
         CALL AGSPUSH                                                     626
         AG_MODULE=MODULE                                                 626
C    *                                                                    626
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        627
         CALL AGSRESET                                                    627
         AG_TITLE = 'ALUMINIUM'                                           627
         AG_EXNAME = 'MATERIAL'                                           627
         AG_PARLIST = 'NONE'                                              627
         CALL AXMATERIAL                                                  627
C    *                                                                    627
C    Geant ATTRIBUTE : SFPJ ===> SEEN=2 COLO=33 <===                      627
         CALL AGSRESET                                                    628
         AG_SEEN = 2                                                      628
         AG_COLO = 33                                                     628
         AG_TITLE = 'SFPJ'                                                628
         AG_EXNAME = 'ATTRIBUTE'                                          628
         AG_PARLIST = 'SEEN_COLO'                                         628
         CALL AXATTRIBUTE                                                 628
C    *                                                                    628
C    Geant SHAPE : TUBE ===> RMIN=SFJP_SFPJ_DX1+SFJP_SFPJ_DX2, RMAX=SFJP  628
C    _SFPJ_DY DZ=SFJP_SFPJ_DZ <===                                        628
         CALL AGSRESET                                                    630
         AG_RMIN = SFJP_SFPJ_DX1+SFJP_SFPJ_DX2                            630
         AG_RMAX = SFJP_SFPJ_DY                                           630
         AG_DZ = SFJP_SFPJ_DZ                                             630
         AG_TITLE = 'TUBE'                                                630
         AG_EXNAME = 'SHAPE'                                              630
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      630
         CALL AXSHAPE                                                     630
C    *                                                                    630
5421  CALL AGSPOPB                                                        631
      GO TO BLOCK_SFPJ, (5420 ,5430)                                      631
C End of Local Procedure                                                  631
C ----------------------------------------------------------------------  632
C ------------ SFSD                                                       632
C Begin Local Procedure SFSD                                              633
C SFSD IS THE STRIP DETECTOR                                              633
5300  CONTINUE                                                            633
         AG_TITLE='SFSD'                                                  633
         AG_MODULE=MODULE                                                 633
         AG_MARK='V00'                                                    633
         CALL AGSPUSH                                                     633
         AG_MODULE=MODULE                                                 633
C    *                                                                    633
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          634
         CALL AGSRESET                                                    634
         AG_TITLE = 'SILICON'                                             634
         AG_EXNAME = 'MATERIAL'                                           634
         AG_PARLIST = 'NONE'                                              634
         CALL AXMATERIAL                                                  634
C    *                                                                    634
C    Geant MATERIAL : SENSITIVE ===> ISVOL=1 <===                         634
         CALL AGSRESET                                                    635
         AG_ISVOL = 1                                                     635
         AG_TITLE = 'SENSITIVE'                                           635
         AG_EXNAME = 'MATERIAL'                                           635
         AG_PARLIST = 'ISVOL'                                             635
         CALL AXMATERIAL                                                  635
C    *                                                                    635
C    Geant ATTRIBUTE : SFSD ===> SEEN=2 COLO=41 <===                      635
         CALL AGSRESET                                                    636
         AG_SEEN = 2                                                      636
         AG_COLO = 41                                                     636
         AG_TITLE = 'SFSD'                                                636
         AG_EXNAME = 'ATTRIBUTE'                                          636
         AG_PARLIST = 'SEEN_COLO'                                         636
         CALL AXATTRIBUTE                                                 636
C    *                                                                    636
C    Geant SHAPE : BOX ===> DX=SFJP_SFSD_DX DY=SFJP_SFSD_DY, DZ=SFJP_SFS  636
C    D_DZ <===                                                            636
         CALL AGSRESET                                                    638
         AG_DX = SFJP_SFSD_DX                                             638
         AG_DY = SFJP_SFSD_DY                                             638
         AG_DZ = SFJP_SFSD_DZ                                             638
         AG_TITLE = 'BOX'                                                 638
         AG_EXNAME = 'SHAPE'                                              638
         AG_PARLIST = 'DX_DY_DZ'                                          638
         CALL AXSHAPE                                                     638
C    *                                                                    638
         CALL GSTPAR( AG_IMED,'STRA',1. )                                 638
C    Defining Hits: X:0.001:S Y:0.001: Z:0.001: CX:10: CY:10: CZ:10: STE  641
C    P:0.01: SLENG:16:(0,500), TOF:16:(0,1.0E-6) PTOT:16:(0,100.0) ELOSS  641
C    :16:(0,0.001)                                                        641
         AG_TITLE = 'SFSD'                                                641
         AG_EXNAME = 'HITS'                                               641
         AG_TYPE=AG_MAGIC                                                 641
         CALL AGSDETV('SFSD')                                             641
C    *                                                                    641
         AG_PARLIST='X'                                                   641
         AG_TITLE = 'DETH'                                                641
         AG_NBITS = AG_MAGIC                                              641
         AG_BIN=0.001                                                     641
         AG_OPTION='S'                                                    641
         AG_FHMIN = AG_MAGIC                                              641
         AG_FHMAX = AG_MAGIC                                              641
         CALL AGSDETH (0)                                                 641
C    *                                                                    641
         AG_PARLIST='Y'                                                   641
         AG_TITLE = 'DETH'                                                641
         AG_NBITS = AG_MAGIC                                              641
         AG_BIN=0.001                                                     641
         AG_FHMIN=AG_MAGIC                                                641
         AG_FHMAX=AG_MAGIC                                                641
         AG_OPTION='*'                                                    641
         CALL AGSDETH (0)                                                 641
C    *                                                                    641
         AG_PARLIST='Z'                                                   641
         AG_TITLE = 'DETH'                                                641
         AG_NBITS = AG_MAGIC                                              641
         AG_BIN=0.001                                                     641
         AG_FHMIN=AG_MAGIC                                                641
         AG_FHMAX=AG_MAGIC                                                641
         AG_OPTION='*'                                                    641
         CALL AGSDETH (0)                                                 641
C    *                                                                    641
         AG_PARLIST='CX'                                                  641
         AG_TITLE = 'DETH'                                                641
         AG_NBITS = 10                                                    641
         AG_BIN=AG_MAGIC                                                  641
         AG_FHMIN=AG_MAGIC                                                641
         AG_FHMAX=AG_MAGIC                                                641
         AG_OPTION='*'                                                    641
         CALL AGSDETH (0)                                                 641
C    *                                                                    641
         AG_PARLIST='CY'                                                  641
         AG_TITLE = 'DETH'                                                641
         AG_NBITS = 10                                                    641
         AG_BIN=AG_MAGIC                                                  641
         AG_FHMIN=AG_MAGIC                                                641
         AG_FHMAX=AG_MAGIC                                                641
         AG_OPTION='*'                                                    641
         CALL AGSDETH (0)                                                 641
C    *                                                                    641
         AG_PARLIST='CZ'                                                  641
         AG_TITLE = 'DETH'                                                641
         AG_NBITS = 10                                                    641
         AG_BIN=AG_MAGIC                                                  641
         AG_FHMIN=AG_MAGIC                                                641
         AG_FHMAX=AG_MAGIC                                                641
         AG_OPTION='*'                                                    641
         CALL AGSDETH (0)                                                 641
C    *                                                                    641
         AG_PARLIST='STEP'                                                641
         AG_TITLE = 'DETH'                                                641
         AG_NBITS = AG_MAGIC                                              641
         AG_BIN=0.01                                                      641
         AG_FHMIN=AG_MAGIC                                                641
         AG_FHMAX=AG_MAGIC                                                641
         AG_OPTION='*'                                                    641
         CALL AGSDETH (0)                                                 641
C    *                                                                    641
         AG_PARLIST='SLENG'                                               641
         AG_TITLE = 'DETH'                                                641
         AG_NBITS = 16                                                    641
         AG_BIN=AG_MAGIC                                                  641
         AG_OPTION='*'                                                    641
         AG_FHMIN = 0                                                     641
         AG_FHMAX = 500                                                   641
         CALL AGSDETH (0)                                                 641
C    *                                                                    641
         AG_PARLIST='TOF'                                                 641
         AG_TITLE = 'DETH'                                                641
         AG_NBITS = 16                                                    641
         AG_BIN=AG_MAGIC                                                  641
         AG_OPTION='*'                                                    641
         AG_FHMIN = 0                                                     641
         AG_FHMAX = 1.0E-6                                                641
         CALL AGSDETH (0)                                                 641
C    *                                                                    641
         AG_PARLIST='PTOT'                                                641
         AG_TITLE = 'DETH'                                                641
         AG_NBITS = 16                                                    641
         AG_BIN=AG_MAGIC                                                  641
         AG_OPTION='*'                                                    641
         AG_FHMIN = 0                                                     641
         AG_FHMAX = 100.0                                                 641
         CALL AGSDETH (0)                                                 641
C    *                                                                    641
         AG_PARLIST='ELOSS'                                               641
         AG_TITLE = 'DETH'                                                641
         AG_NBITS = 16                                                    641
         AG_BIN=AG_MAGIC                                                  641
         AG_OPTION='*'                                                    641
         AG_FHMIN = 0                                                     641
         AG_FHMAX = 0.001                                                 641
         CALL AGSDETH (0)                                                 641
C    *                                                                    641
         CALL AGSDETU('SFSD')                                             641
C    *                                                                    641
5301  CALL AGSPOPB                                                        642
      GO TO BLOCK_SFSD, (5300 ,5310)                                      642
C End of Local Procedure                                                  642
C ----------------------------------------------------------------------  643
C ------------ SFSM                                                       643
C Begin Local Procedure SFSM                                              644
C SFSM IS THE MOTHER VOLUME OF THE LADDER (MECHANIC STRUCTURE)            644
5250  CONTINUE                                                            644
         AG_TITLE='SFSM'                                                  644
         AG_MODULE=MODULE                                                 644
         AG_MARK='V00'                                                    644
         CALL AGSPUSH                                                     644
         AG_MODULE=MODULE                                                 644
C    *                                                                    644
C    Geant MATERIAL : AIR ===> inherited arguments used <===              645
         CALL AGSRESET                                                    645
         AG_TITLE = 'AIR'                                                 645
         AG_EXNAME = 'MATERIAL'                                           645
         AG_PARLIST = 'NONE'                                              645
         CALL AXMATERIAL                                                  645
C    *                                                                    645
C    Geant ATTRIBUTE : SFSM ===> SEEN=0 COLO=6 <===                       645
         CALL AGSRESET                                                    646
         AG_SEEN = 0                                                      646
         AG_COLO = 6                                                      646
         AG_TITLE = 'SFSM'                                                646
         AG_EXNAME = 'ATTRIBUTE'                                          646
         AG_PARLIST = 'SEEN_COLO'                                         646
         CALL AXATTRIBUTE                                                 646
C    *                                                                    646
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=SFPA_SMTHK/2, DZ=SFPA_SML  646
C    EN/2. <===                                                           646
         CALL AGSRESET                                                    648
         AG_DX = SFPA_DMWID/2                                             648
         AG_DY = SFPA_SMTHK/2                                             648
         AG_DZ = SFPA_SMLEN/2.                                            648
         AG_TITLE = 'BOX'                                                 648
         AG_EXNAME = 'SHAPE'                                              648
         AG_PARLIST = 'DX_DY_DZ'                                          648
         CALL AXSHAPE                                                     648
C    *                                                                    648
         YOFFSET = - 1.7                                                  648
C    Create SFLT :                                                        650
C    Call To Local Procedure SFLT                                         650
         ASSIGN 5450 TO BLOCK_SFLT                                        650
         GO TO  5440                                                      650
5450     CONTINUE                                                         650
C    *                                                                    650
         HIGHT = SFJP_SFSM_LL*TAN(54*PI/180) - SFPB_HHIGHT/TAN(PI/5) -    651
     *   0.02                                                             651
C    Geant POSITION : SFLT ===> X=-(SFPB_HHIGHT-SFPB_KHIGHT) Y=HIGHT+YOF  652
C    FSET Z=0.0 ALPHAZ=-90-36 <===                                        652
         CALL AGSRESET                                                    652
         AG_X = -(SFPB_HHIGHT-SFPB_KHIGHT)                                652
         AG_Y = HIGHT+YOFFSET                                             652
         AG_Z = 0.0                                                       652
         AG_ALPHAN=AG_ALPHAN+1                                            652
         AG_ALPHAK(AG_ALPHAN)=3                                           652
         AG_ALPHAL(AG_ALPHAN)=-90-36                                      652
         AG_TITLE = 'SFLT'                                                652
         AG_EXNAME = 'POSITION'                                           652
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      652
         CALL AXPOSITION                                                  652
C    *                                                                    652
C    Geant POSITION : SFLT ===> X=(SFPB_HHIGHT-SFPB_KHIGHT) Y=HIGHT+YOFF  652
C    SET Z=0 ALPHAY=180. ALPHAZ=90.+36. <===                              652
         CALL AGSRESET                                                    654
         AG_X = (SFPB_HHIGHT-SFPB_KHIGHT)                                 654
         AG_Y = HIGHT+YOFFSET                                             654
         AG_Z = 0                                                         654
         AG_ALPHAN=AG_ALPHAN+1                                            654
         AG_ALPHAK(AG_ALPHAN)=2                                           654
         AG_ALPHAL(AG_ALPHAN)=180.                                        654
         AG_ALPHAN=AG_ALPHAN+1                                            654
         AG_ALPHAK(AG_ALPHAN)=3                                           654
         AG_ALPHAL(AG_ALPHAN)=90.+36.                                     654
         AG_TITLE = 'SFLT'                                                654
         AG_EXNAME = 'POSITION'                                           654
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               654
         CALL AXPOSITION                                                  654
C    *                                                                    654
C    Create SFLU :                                                        655
C    Call To Local Procedure SFLU                                         655
         ASSIGN 5470 TO BLOCK_SFLU                                        655
         GO TO  5460                                                      655
5470     CONTINUE                                                         655
C    *                                                                    655
C    Geant POSITION : SFLU ===> X=0.-(SFJP_SFSM_LL-(SFJP_SFLU_BL1+SFJP_S  657
C    FLU_H1/2*1/TAN(PI/5))) Y=0.+YOFFSET Z=0. <===                        657
         CALL AGSRESET                                                    657
         AG_X = 0.-(SFJP_SFSM_LL-(SFJP_SFLU_BL1+SFJP_SFLU_H1/2*1/TAN(PI/  657
     *   5)))                                                             657
         AG_Y = 0.+YOFFSET                                                657
         AG_Z = 0.                                                        657
         AG_TITLE = 'SFLU'                                                657
         AG_EXNAME = 'POSITION'                                           657
         AG_PARLIST = 'X_Y_Z'                                             657
         CALL AXPOSITION                                                  657
C    *                                                                    657
         ESSAI = 180                                                      657
         ESSAI = ESSAI + 27 + 27                                          659
C    Geant POSITION : SFLU ===> X=-COS(63*PI/180)*2*(SFPB_HBASE-SFPB_KBA  662
C    SE)-(SFJP_SFSM_LL-(0.2+0.01/TAN(PI/5))) Y=COS(27*PI/180)*2*(SFPB_HB  662
C    ASE-SFPB_KBASE)+YOFFSET Z=0 ALPHAY=180 ALPHAZ=ESSAI <===             662
         CALL AGSRESET                                                    662
         AG_X = -COS(63*PI/                                               662
     *   180)*2*(SFPB_HBASE-SFPB_KBASE)-(SFJP_SFSM_LL-(0.2+0.01/TAN(PI/   662
     *   5)))                                                             662
         AG_Y = COS(27*PI/180)*2*(SFPB_HBASE-SFPB_KBASE)+YOFFSET          662
         AG_Z = 0                                                         662
         AG_ALPHAN=AG_ALPHAN+1                                            662
         AG_ALPHAK(AG_ALPHAN)=2                                           662
         AG_ALPHAL(AG_ALPHAN)=180                                         662
         AG_ALPHAN=AG_ALPHAN+1                                            662
         AG_ALPHAK(AG_ALPHAN)=3                                           662
         AG_ALPHAL(AG_ALPHAN)=ESSAI                                       662
         AG_TITLE = 'SFLU'                                                662
         AG_EXNAME = 'POSITION'                                           662
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               662
         CALL AXPOSITION                                                  662
C    *                                                                    662
C    Geant POSITION : SFLU ===> X=0.+(SFJP_SFSM_LL-(0.2+0.01/TAN(PI/5)))  662
C     Y=0.+YOFFSET Z=0. ALPHAY=180. <===                                  662
         CALL AGSRESET                                                    664
         AG_X = 0.+(SFJP_SFSM_LL-(0.2+0.01/TAN(PI/5)))                    664
         AG_Y = 0.+YOFFSET                                                664
         AG_Z = 0.                                                        664
         AG_ALPHAN=AG_ALPHAN+1                                            664
         AG_ALPHAK(AG_ALPHAN)=2                                           664
         AG_ALPHAL(AG_ALPHAN)=180.                                        664
         AG_TITLE = 'SFLU'                                                664
         AG_EXNAME = 'POSITION'                                           664
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      664
         CALL AXPOSITION                                                  664
C    *                                                                    664
C    Geant POSITION : SFLU ===> X=COS(63*PI/180)*2*(SFPB_HBASE-SFPB_KBAS  664
C    E)+(SFJP_SFSM_LL-(0.2+0.01/TAN(PI/5))) Y=COS(27*PI/180)*2*(SFPB_HBA  664
C    SE-SFPB_KBASE)+YOFFSET Z=0 ALPHAZ=-ESSAI <===                        664
         CALL AGSRESET                                                    667
         AG_X = COS(63*PI/                                                667
     *   180)*2*(SFPB_HBASE-SFPB_KBASE)+(SFJP_SFSM_LL-(0.2+0.01/TAN(PI/   667
     *   5)))                                                             667
         AG_Y = COS(27*PI/180)*2*(SFPB_HBASE-SFPB_KBASE)+YOFFSET          667
         AG_Z = 0                                                         667
         AG_ALPHAN=AG_ALPHAN+1                                            667
         AG_ALPHAK(AG_ALPHAN)=3                                           667
         AG_ALPHAL(AG_ALPHAN)=-ESSAI                                      667
         AG_TITLE = 'SFLU'                                                667
         AG_EXNAME = 'POSITION'                                           667
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      667
         CALL AXPOSITION                                                  667
C    *                                                                    667
C    Create SFFK :                                                        668
C    Call To Local Procedure SFFK                                         668
         ASSIGN 5490 TO BLOCK_SFFK                                        668
         GO TO  5480                                                      668
5490     CONTINUE                                                         668
C    *                                                                    668
C    Geant POSITION : SFFK ===> X=SFJP_SFFK_PX/2+SFJP_SFFK_DXE Y=-SFJP_S  670
C    FFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY+YOFFSET Z=0.0 <===                670
         CALL AGSRESET                                                    670
         AG_X = SFJP_SFFK_PX/2+SFJP_SFFK_DXE                              670
         AG_Y = -SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY+YOFFSET         670
         AG_Z = 0.0                                                       670
         AG_TITLE = 'SFFK'                                                670
         AG_EXNAME = 'POSITION'                                           670
         AG_PARLIST = 'X_Y_Z'                                             670
         CALL AXPOSITION                                                  670
C    *                                                                    670
C    Geant POSITION : SFFK ===> X=-SFJP_SFFK_PX/2-SFJP_SFFK_DXE Y=-SFJP_  670
C    SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY+YOFFSET Z=0.0 <===               670
         CALL AGSRESET                                                    672
         AG_X = -SFJP_SFFK_PX/2-SFJP_SFFK_DXE                             672
         AG_Y = -SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY+YOFFSET         672
         AG_Z = 0.0                                                       672
         AG_TITLE = 'SFFK'                                                672
         AG_EXNAME = 'POSITION'                                           672
         AG_PARLIST = 'X_Y_Z'                                             672
         CALL AXPOSITION                                                  672
C    *                                                                    672
C    Create SFFL :                                                        673
C    Call To Local Procedure SFFL                                         673
         ASSIGN 5510 TO BLOCK_SFFL                                        673
         GO TO  5500                                                      673
5510     CONTINUE                                                         673
C    *                                                                    673
C    Geant POSITION : SFFL ===> X=-SFJP_SFKL_PX-SFPB_FSIZE*COS(15*PI/180  676
C    ) Y=-SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(75*PI/  676
C    180)+YOFFSET Z=0 ALPHAZ=15 <===                                      676
         CALL AGSRESET                                                    676
         AG_X = -SFJP_SFKL_PX-SFPB_FSIZE*COS(15*PI/180)                   676
         AG_Y =                                                           676
     *   -SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(75*PI/  676
     *   180)+YOFFSET                                                     676
         AG_Z = 0                                                         676
         AG_ALPHAN=AG_ALPHAN+1                                            676
         AG_ALPHAK(AG_ALPHAN)=3                                           676
         AG_ALPHAL(AG_ALPHAN)=15                                          676
         AG_TITLE = 'SFFL'                                                676
         AG_EXNAME = 'POSITION'                                           676
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      676
         CALL AXPOSITION                                                  676
C    *                                                                    676
C    Geant POSITION : SFFL ===> X=SFJP_SFKL_PX+SFPB_FSIZE*COS(15*PI/180)  676
C     Y=-SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(75*PI/1  676
C    80)+YOFFSET Z=0 ALPHAZ=-15 <===                                      676
         CALL AGSRESET                                                    679
         AG_X = SFJP_SFKL_PX+SFPB_FSIZE*COS(15*PI/180)                    679
         AG_Y =                                                           679
     *   -SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(75*PI/  679
     *   180)+YOFFSET                                                     679
         AG_Z = 0                                                         679
         AG_ALPHAN=AG_ALPHAN+1                                            679
         AG_ALPHAK(AG_ALPHAN)=3                                           679
         AG_ALPHAL(AG_ALPHAN)=-15                                         679
         AG_TITLE = 'SFFL'                                                679
         AG_EXNAME = 'POSITION'                                           679
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      679
         CALL AXPOSITION                                                  679
C    *                                                                    679
C    Create SFKK :                                                        680
C    Call To Local Procedure SFKK                                         680
         ASSIGN 5530 TO BLOCK_SFKK                                        680
         GO TO  5520                                                      680
5530     CONTINUE                                                         680
C    *                                                                    680
C    Geant POSITION : SFKK ===> X=0. Y=-SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP  682
C    _SFFK_PY2+YOFFSET-SFJP_SFKK_DY/2. Z=0. <===                          682
         CALL AGSRESET                                                    682
         AG_X = 0.                                                        682
         AG_Y =                                                           682
     *   -SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_PY2+YOFFSET-              682
     *   SFJP_SFKK_DY/2.                                                  682
         AG_Z = 0.                                                        682
         AG_TITLE = 'SFKK'                                                682
         AG_EXNAME = 'POSITION'                                           682
         AG_PARLIST = 'X_Y_Z'                                             682
         CALL AXPOSITION                                                  682
C    *                                                                    682
C    Create SFKL :                                                        683
C    Call To Local Procedure SFKL                                         683
         ASSIGN 5550 TO BLOCK_SFKL                                        683
         GO TO  5540                                                      683
5550     CONTINUE                                                         683
C    *                                                                    683
C    Geant POSITION : SFKL ===> X=-SFJP_SFKL_PX-SFPB_FSIZE*COS(15*PI/180  686
C    )+0.005 Y=-SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(  686
C    75*PI/180)+YOFFSET-0.021 Z=0 ALPHAZ=15 <===                          686
         CALL AGSRESET                                                    686
         AG_X = -SFJP_SFKL_PX-SFPB_FSIZE*COS(15*PI/180)+0.005             686
         AG_Y =                                                           686
     *   -SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(75*PI/  686
     *   180)+YOFFSET-0.021                                               686
         AG_Z = 0                                                         686
         AG_ALPHAN=AG_ALPHAN+1                                            686
         AG_ALPHAK(AG_ALPHAN)=3                                           686
         AG_ALPHAL(AG_ALPHAN)=15                                          686
         AG_TITLE = 'SFKL'                                                686
         AG_EXNAME = 'POSITION'                                           686
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      686
         CALL AXPOSITION                                                  686
C    *                                                                    686
C    Geant POSITION : SFKL ===> X=+SFJP_SFKL_PX+SFPB_FSIZE*COS(15*PI/180  686
C    )-0.005 Y=-SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(  686
C    75*PI/180)+YOFFSET-0.021 Z=0 ALPHAZ=-15 <===                         686
         CALL AGSRESET                                                    689
         AG_X = +SFJP_SFKL_PX+SFPB_FSIZE*COS(15*PI/180)-0.005             689
         AG_Y =                                                           689
     *   -SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(75*PI/  689
     *   180)+YOFFSET-0.021                                               689
         AG_Z = 0                                                         689
         AG_ALPHAN=AG_ALPHAN+1                                            689
         AG_ALPHAK(AG_ALPHAN)=3                                           689
         AG_ALPHAL(AG_ALPHAN)=-15                                         689
         AG_TITLE = 'SFKL'                                                689
         AG_EXNAME = 'POSITION'                                           689
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      689
         CALL AXPOSITION                                                  689
C    *                                                                    689
C    Create SAPP :                                                        690
C    Call To Local Procedure SAPP                                         690
         ASSIGN 5570 TO BLOCK_SAPP                                        690
         GO TO  5560                                                      690
5570     CONTINUE                                                         690
C    *                                                                    690
C    Geant POSITION : SAPP ===> X=0. Y=-SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJ  692
C    P_SAPP_PY3)/2.+YOFFSET Z=SFJP_SFFK_DZ/2+SFJP_SAPP_DZ/2. ALPHAY=180.  692
C     <===                                                                692
         CALL AGSRESET                                                    692
         AG_X = 0.                                                        692
         AG_Y = -SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJP_SAPP_PY3)/2.+YOFFSET   692
         AG_Z = SFJP_SFFK_DZ/2+SFJP_SAPP_DZ/2.                            692
         AG_ALPHAN=AG_ALPHAN+1                                            692
         AG_ALPHAK(AG_ALPHAN)=2                                           692
         AG_ALPHAL(AG_ALPHAN)=180.                                        692
         AG_TITLE = 'SAPP'                                                692
         AG_EXNAME = 'POSITION'                                           692
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      692
         CALL AXPOSITION                                                  692
C    *                                                                    692
C    Geant POSITION : SAPP ===> X=0. Y=-SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJ  692
C    P_SAPP_PY3)/2.+YOFFSET Z=-(SFJP_SFFK_DZ/2+SFJP_SAPP_DZ/2.) <===      692
         CALL AGSRESET                                                    694
         AG_X = 0.                                                        694
         AG_Y = -SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJP_SAPP_PY3)/2.+YOFFSET   694
         AG_Z = -(SFJP_SFFK_DZ/2+SFJP_SAPP_DZ/2.)                         694
         AG_TITLE = 'SAPP'                                                694
         AG_EXNAME = 'POSITION'                                           694
         AG_PARLIST = 'X_Y_Z'                                             694
         CALL AXPOSITION                                                  694
C    *                                                                    694
C    Create SFAM :                                                        695
C    Call To Local Procedure SFAM                                         695
         ASSIGN 5590 TO BLOCK_SFAM                                        695
         GO TO  5580                                                      695
5590     CONTINUE                                                         695
C    *                                                                    695
C    Geant POSITION : SFAM ===> X=0. Y=-SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJ  697
C    P_SAPP_PY3)/2.+YOFFSET Z=-(SFJP_SFFK_DZ/2+SFJP_SAPP_DZ)-SFJP_SFAM_D  697
C    Z/2. <===                                                            697
         CALL AGSRESET                                                    697
         AG_X = 0.                                                        697
         AG_Y = -SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJP_SAPP_PY3)/2.+YOFFSET   697
         AG_Z = -(SFJP_SFFK_DZ/2+SFJP_SAPP_DZ)-SFJP_SFAM_DZ/2.            697
         AG_TITLE = 'SFAM'                                                697
         AG_EXNAME = 'POSITION'                                           697
         AG_PARLIST = 'X_Y_Z'                                             697
         CALL AXPOSITION                                                  697
C    *                                                                    697
C    Geant POSITION : SFAM ===> X=0. Y=-SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJ  697
C    P_SAPP_PY3)/2.+YOFFSET Z=+(SFJP_SFFK_DZ/2+SFJP_SAPP_DZ)+SFJP_SFAM_D  697
C    Z/2. ALPHAY=180. <===                                                697
         CALL AGSRESET                                                    699
         AG_X = 0.                                                        699
         AG_Y = -SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJP_SAPP_PY3)/2.+YOFFSET   699
         AG_Z = +(SFJP_SFFK_DZ/2+SFJP_SAPP_DZ)+SFJP_SFAM_DZ/2.            699
         AG_ALPHAN=AG_ALPHAN+1                                            699
         AG_ALPHAK(AG_ALPHAN)=2                                           699
         AG_ALPHAL(AG_ALPHAN)=180.                                        699
         AG_TITLE = 'SFAM'                                                699
         AG_EXNAME = 'POSITION'                                           699
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      699
         CALL AXPOSITION                                                  699
C    *                                                                    699
C    Create SFCO :                                                        700
C    Call To Local Procedure SFCO                                         700
         ASSIGN 5610 TO BLOCK_SFCO                                        700
         GO TO  5600                                                      700
5610     CONTINUE                                                         700
C    *                                                                    700
C    Geant POSITION : SFCO ===> X=-SFJP_SFCO_PX Y=SFJP_SFCO_PY Z=-(SFJP_  703
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFCO_DZ/2. ALPHAZ=+SFJP  703
C    _ALPHAZ <===                                                         703
         CALL AGSRESET                                                    703
         AG_X = -SFJP_SFCO_PX                                             703
         AG_Y = SFJP_SFCO_PY                                              703
         AG_Z = -(SFJP_SFFK_DZ/                                           703
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFCO_DZ/2.                   703
         AG_ALPHAN=AG_ALPHAN+1                                            703
         AG_ALPHAK(AG_ALPHAN)=3                                           703
         AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                                703
         AG_TITLE = 'SFCO'                                                703
         AG_EXNAME = 'POSITION'                                           703
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      703
         CALL AXPOSITION                                                  703
C    *                                                                    703
C    Geant POSITION : SFCO ===> X=+SFJP_SFCO_PX Y=SFJP_SFCO_PY Z=+(SFJP_  703
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFCO_DZ/2. ALPHAZ=-SFJP  703
C    _ALPHAZ <===                                                         703
         CALL AGSRESET                                                    706
         AG_X = +SFJP_SFCO_PX                                             706
         AG_Y = SFJP_SFCO_PY                                              706
         AG_Z = +(SFJP_SFFK_DZ/                                           706
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFCO_DZ/2.                   706
         AG_ALPHAN=AG_ALPHAN+1                                            706
         AG_ALPHAK(AG_ALPHAN)=3                                           706
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                                706
         AG_TITLE = 'SFCO'                                                706
         AG_EXNAME = 'POSITION'                                           706
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      706
         CALL AXPOSITION                                                  706
C    *                                                                    706
C    Create SFCM :                                                        707
C    Call To Local Procedure SFCM                                         707
         ASSIGN 5630 TO BLOCK_SFCM                                        707
         GO TO  5620                                                      707
5630     CONTINUE                                                         707
C    *                                                                    707
C    Geant POSITION : SFCM ===> X=+SFJP_SFCO_PX Y=SFJP_SFCO_PY Z=-(SFJP_  710
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFCO_DZ/2. ALPHAZ=-SFJP  710
C    _ALPHAZ <===                                                         710
         CALL AGSRESET                                                    710
         AG_X = +SFJP_SFCO_PX                                             710
         AG_Y = SFJP_SFCO_PY                                              710
         AG_Z = -(SFJP_SFFK_DZ/                                           710
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFCO_DZ/2.                   710
         AG_ALPHAN=AG_ALPHAN+1                                            710
         AG_ALPHAK(AG_ALPHAN)=3                                           710
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                                710
         AG_TITLE = 'SFCM'                                                710
         AG_EXNAME = 'POSITION'                                           710
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      710
         CALL AXPOSITION                                                  710
C    *                                                                    710
C    Geant POSITION : SFCM ===> X=-SFJP_SFCO_PX Y=SFJP_SFCO_PY Z=+(SFJP_  710
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFCO_DZ/2. ALPHAY=180 A  710
C    LPHAZ=+SFJP_ALPHAZ <===                                              710
         CALL AGSRESET                                                    713
         AG_X = -SFJP_SFCO_PX                                             713
         AG_Y = SFJP_SFCO_PY                                              713
         AG_Z = +(SFJP_SFFK_DZ/                                           713
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFCO_DZ/2.                   713
         AG_ALPHAN=AG_ALPHAN+1                                            713
         AG_ALPHAK(AG_ALPHAN)=2                                           713
         AG_ALPHAL(AG_ALPHAN)=180                                         713
         AG_ALPHAN=AG_ALPHAN+1                                            713
         AG_ALPHAK(AG_ALPHAN)=3                                           713
         AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                                713
         AG_TITLE = 'SFCM'                                                713
         AG_EXNAME = 'POSITION'                                           713
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               713
         CALL AXPOSITION                                                  713
C    *                                                                    713
C    Create SFKF :                                                        714
C    Call To Local Procedure SFKF                                         714
         ASSIGN 5650 TO BLOCK_SFKF                                        714
         GO TO  5640                                                      714
5650     CONTINUE                                                         714
C    *                                                                    714
C    Geant POSITION : SFKF ===> X=-SFJP_SFKF_PX Y=SFJP_SFKF_PY Z=-(SFJP_  717
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFKF_DZ/2-0.2 ALPHAZ=+S  717
C    FJP_ALPHAZ <===                                                      717
         CALL AGSRESET                                                    717
         AG_X = -SFJP_SFKF_PX                                             717
         AG_Y = SFJP_SFKF_PY                                              717
         AG_Z = -(SFJP_SFFK_DZ/                                           717
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFKF_DZ/2-0.2                717
         AG_ALPHAN=AG_ALPHAN+1                                            717
         AG_ALPHAK(AG_ALPHAN)=3                                           717
         AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                                717
         AG_TITLE = 'SFKF'                                                717
         AG_EXNAME = 'POSITION'                                           717
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      717
         CALL AXPOSITION                                                  717
C    *                                                                    717
C    Geant POSITION : SFKF ===> X=+SFJP_SFKF_PX Y=SFJP_SFKF_PY Z=+(SFJP_  717
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFKF_DZ/2+0.2 ALPHAZ=-S  717
C    FJP_ALPHAZ <===                                                      717
         CALL AGSRESET                                                    720
         AG_X = +SFJP_SFKF_PX                                             720
         AG_Y = SFJP_SFKF_PY                                              720
         AG_Z = +(SFJP_SFFK_DZ/                                           720
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFKF_DZ/2+0.2                720
         AG_ALPHAN=AG_ALPHAN+1                                            720
         AG_ALPHAK(AG_ALPHAN)=3                                           720
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                                720
         AG_TITLE = 'SFKF'                                                720
         AG_EXNAME = 'POSITION'                                           720
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      720
         CALL AXPOSITION                                                  720
C    *                                                                    720
C    Create SFKS :                                                        721
C    Call To Local Procedure SFKS                                         721
         ASSIGN 5670 TO BLOCK_SFKS                                        721
         GO TO  5660                                                      721
5670     CONTINUE                                                         721
C    *                                                                    721
C    Geant POSITION : SFKS ===> X=+SFJP_SFKS_PX Y=SFJP_SFKS_PY Z=-(SFJP_  724
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFKF_DZ/2-0.2 ALPHAZ=-S  724
C    FJP_ALPHAZ <===                                                      724
         CALL AGSRESET                                                    724
         AG_X = +SFJP_SFKS_PX                                             724
         AG_Y = SFJP_SFKS_PY                                              724
         AG_Z = -(SFJP_SFFK_DZ/                                           724
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFKF_DZ/2-0.2                724
         AG_ALPHAN=AG_ALPHAN+1                                            724
         AG_ALPHAK(AG_ALPHAN)=3                                           724
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                                724
         AG_TITLE = 'SFKS'                                                724
         AG_EXNAME = 'POSITION'                                           724
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      724
         CALL AXPOSITION                                                  724
C    *                                                                    724
C    Geant POSITION : SFKS ===> X=-SFJP_SFKS_PX Y=SFJP_SFKS_PY Z=+(SFJP_  724
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFKF_DZ/2+0.2 ALPHAZ=+S  724
C    FJP_ALPHAZ <===                                                      724
         CALL AGSRESET                                                    727
         AG_X = -SFJP_SFKS_PX                                             727
         AG_Y = SFJP_SFKS_PY                                              727
         AG_Z = +(SFJP_SFFK_DZ/                                           727
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFKF_DZ/2+0.2                727
         AG_ALPHAN=AG_ALPHAN+1                                            727
         AG_ALPHAK(AG_ALPHAN)=3                                           727
         AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                                727
         AG_TITLE = 'SFKS'                                                727
         AG_EXNAME = 'POSITION'                                           727
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      727
         CALL AXPOSITION                                                  727
C    *                                                                    727
C    Create SFPR :                                                        728
C    Call To Local Procedure SFPR                                         728
         ASSIGN 5690 TO BLOCK_SFPR                                        728
         GO TO  5680                                                      728
5690     CONTINUE                                                         728
C    *                                                                    728
C    Geant POSITION : SFPR ===> X=0 Y=+SFJP_SFPR_PY Z=-SFJP_SFLU_DZ <===  729
         CALL AGSRESET                                                    729
         AG_X = 0                                                         729
         AG_Y = +SFJP_SFPR_PY                                             729
         AG_Z = -SFJP_SFLU_DZ                                             729
         AG_TITLE = 'SFPR'                                                729
         AG_EXNAME = 'POSITION'                                           729
         AG_PARLIST = 'X_Y_Z'                                             729
         CALL AXPOSITION                                                  729
C    *                                                                    729
C    Geant POSITION : SFPR ===> X=0 Y=+SFJP_SFPR_PY Z=+SFJP_SFLU_DZ-3.7   729
C    <===                                                                 729
         CALL AGSRESET                                                    730
         AG_X = 0                                                         730
         AG_Y = +SFJP_SFPR_PY                                             730
         AG_Z = +SFJP_SFLU_DZ-3.7                                         730
         AG_TITLE = 'SFPR'                                                730
         AG_EXNAME = 'POSITION'                                           730
         AG_PARLIST = 'X_Y_Z'                                             730
         CALL AXPOSITION                                                  730
C    *                                                                    730
C    Create SFPB :                                                        731
C    Call To Local Procedure SFPB                                         731
         ASSIGN 5710 TO BLOCK_SFPB                                        731
         GO TO  5700                                                      731
5710     CONTINUE                                                         731
C    *                                                                    731
C    Geant POSITION : SFPB ===> X=0 Y=+SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SF  733
C    PB_PY2 Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ/2. <===                           733
         CALL AGSRESET                                                    733
         AG_X = 0                                                         733
         AG_Y = +SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SFPB_PY2                  733
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ/2.                              733
         AG_TITLE = 'SFPB'                                                733
         AG_EXNAME = 'POSITION'                                           733
         AG_PARLIST = 'X_Y_Z'                                             733
         CALL AXPOSITION                                                  733
C    *                                                                    733
C    Geant POSITION : SFPB ===> X=0 Y=+SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SF  733
C    PB_PY2 Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ/2. <===                           733
         CALL AGSRESET                                                    735
         AG_X = 0                                                         735
         AG_Y = +SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SFPB_PY2                  735
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ/2.                              735
         AG_TITLE = 'SFPB'                                                735
         AG_EXNAME = 'POSITION'                                           735
         AG_PARLIST = 'X_Y_Z'                                             735
         CALL AXPOSITION                                                  735
C    *                                                                    735
C    Create SSBS :                                                        736
C    Call To Local Procedure SSBS                                         736
         ASSIGN 5730 TO BLOCK_SSBS                                        736
         GO TO  5720                                                      736
5730     CONTINUE                                                         736
C    *                                                                    736
C    Geant POSITION : SSBS ===> X=0. Y=+SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_S  739
C    FPB_PY2-0.8/2.-SFJP_SSBS_DY/2. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-0.2+SFJP  739
C    _SSBB_DZ+SFJP_SSBS_DX/2. <===                                        739
         CALL AGSRESET                                                    739
         AG_X = 0.                                                        739
         AG_Y = +SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SFPB_PY2-0.8/             739
     *   2.-SFJP_SSBS_DY/2.                                               739
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-0.2+SFJP_SSBB_DZ+SFJP_SSBS_DX/  739
     *   2.                                                               739
         AG_TITLE = 'SSBS'                                                739
         AG_EXNAME = 'POSITION'                                           739
         AG_PARLIST = 'X_Y_Z'                                             739
         CALL AXPOSITION                                                  739
C    *                                                                    739
C    Geant POSITION : SSBS ===> X=0. Y=+SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_S  739
C    FPB_PY2-0.8/2.-SFJP_SSBS_DY/2. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+0.2-SFJP  739
C    _SSBB_DZ-SFJP_SSBS_DX/2. <===                                        739
         CALL AGSRESET                                                    742
         AG_X = 0.                                                        742
         AG_Y = +SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SFPB_PY2-0.8/             742
     *   2.-SFJP_SSBS_DY/2.                                               742
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+0.2-SFJP_SSBB_DZ-SFJP_SSBS_DX/  742
     *   2.                                                               742
         AG_TITLE = 'SSBS'                                                742
         AG_EXNAME = 'POSITION'                                           742
         AG_PARLIST = 'X_Y_Z'                                             742
         CALL AXPOSITION                                                  742
C    *                                                                    742
C    Create SSBB :                                                        743
C    Call To Local Procedure SSBB                                         743
         ASSIGN 5750 TO BLOCK_SSBB                                        743
         GO TO  5740                                                      743
5750     CONTINUE                                                         743
C    *                                                                    743
C    Geant POSITION : SSBB ===> X=0. Y=+SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_S  746
C    FPB_PY2-0.8/2.-SFJP_SSBS_DY/2. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-0.2+SFJP  746
C    _SSBB_DZ/2. <===                                                     746
         CALL AGSRESET                                                    746
         AG_X = 0.                                                        746
         AG_Y = +SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SFPB_PY2-0.8/             746
     *   2.-SFJP_SSBS_DY/2.                                               746
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-0.2+SFJP_SSBB_DZ/2.             746
         AG_TITLE = 'SSBB'                                                746
         AG_EXNAME = 'POSITION'                                           746
         AG_PARLIST = 'X_Y_Z'                                             746
         CALL AXPOSITION                                                  746
C    *                                                                    746
C    Geant POSITION : SSBB ===> X=0. Y=+SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_S  746
C    FPB_PY2-0.8/2.-SFJP_SSBS_DY/2. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+0.2-SFJP  746
C    _SSBB_DZ/2. <===                                                     746
         CALL AGSRESET                                                    749
         AG_X = 0.                                                        749
         AG_Y = +SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SFPB_PY2-0.8/             749
     *   2.-SFJP_SSBS_DY/2.                                               749
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+0.2-SFJP_SSBB_DZ/2.             749
         AG_TITLE = 'SSBB'                                                749
         AG_EXNAME = 'POSITION'                                           749
         AG_PARLIST = 'X_Y_Z'                                             749
         CALL AXPOSITION                                                  749
C    *                                                                    749
C    Create SFLA :                                                        750
C    Call To Local Procedure SFLA                                         750
         ASSIGN 5770 TO BLOCK_SFLA                                        750
         GO TO  5760                                                      750
5770     CONTINUE                                                         750
C    *                                                                    750
C    Create SFLB :                                                        751
C    Call To Local Procedure SFLB                                         751
         ASSIGN 5790 TO BLOCK_SFLB                                        751
         GO TO  5780                                                      751
5790     CONTINUE                                                         751
C    *                                                                    751
C    Create SFLC :                                                        752
C    Call To Local Procedure SFLC                                         752
         ASSIGN 5810 TO BLOCK_SFLC                                        752
         GO TO  5800                                                      752
5810     CONTINUE                                                         752
C    *                                                                    752
C    Create SFEB :                                                        753
C    Call To Local Procedure SFEB                                         753
         ASSIGN 5830 TO BLOCK_SFEB                                        753
         GO TO  5820                                                      753
5830     CONTINUE                                                         753
C    *                                                                    753
C    Create SFES :                                                        754
C    Call To Local Procedure SFES                                         754
         ASSIGN 5850 TO BLOCK_SFES                                        754
         GO TO  5840                                                      754
5850     CONTINUE                                                         754
C    *                                                                    754
         WAFPCKLEN=SFPA_WPLEN/(SFPA_NSSD*1.)                              755
C    *                                                                    756
         IWAF=1                                                           756
            GO TO 5863                                                    756
5861        IWAF=IWAF+1                                                   756
5863        IF(IWAF-(8).GT.0) GO TO 5862                                  756
C       *                                                                 757
            JWAF=1                                                        757
               GO TO 5873                                                 757
5871           JWAF=JWAF+1                                                757
5873           IF(JWAF-(IWAF).GT.0) GO TO 5872                            757
C          Geant POSITION : SFLA ===> X=+SFJP_SFLA_PX+SFJP_FLEX_DI*JWAF   760
C          Y=SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF Z=-(SFPA_WPLEN+WAFPCKLEN)/2+  760
C          IWAF*WAFPCKLEN ALPHAZ=-SFJP_ALPHAZ <===                        760
               CALL AGSRESET                                              760
               AG_X = +SFJP_SFLA_PX+SFJP_FLEX_DI*JWAF                     760
               AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF                      760
               AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+IWAF*WAFPCKLEN            760
               AG_ALPHAN=AG_ALPHAN+1                                      760
               AG_ALPHAK(AG_ALPHAN)=3                                     760
               AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                          760
               AG_TITLE = 'SFLA'                                          760
               AG_EXNAME = 'POSITION'                                     760
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                760
               CALL AXPOSITION                                            760
C          *                                                              760
C          Geant POSITION : SFLA ===> X=-SFJP_SFLA_PX-SFJP_FLEX_DI*JWAF   760
C          Y=SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF Z=+(SFPA_WPLEN+WAFPCKLEN)/2-  760
C          IWAF*WAFPCKLEN ALPHAZ=+SFJP_ALPHAZ <===                        760
               CALL AGSRESET                                              763
               AG_X = -SFJP_SFLA_PX-SFJP_FLEX_DI*JWAF                     763
               AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF                      763
               AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-IWAF*WAFPCKLEN            763
               AG_ALPHAN=AG_ALPHAN+1                                      763
               AG_ALPHAK(AG_ALPHAN)=3                                     763
               AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                          763
               AG_TITLE = 'SFLA'                                          763
               AG_EXNAME = 'POSITION'                                     763
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                763
               CALL AXPOSITION                                            763
C          *                                                              763
C          Geant POSITION : SFLC ===> X=+SFJP_SFLA_PX+SFJP_FLEX_DI*JWAF   763
C          Y=SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF Z=-(SFPA_WPLEN+WAFPCKLEN)/2.  763
C          +16*WAFPCKLEN+WAFPCKLEN/2.+SFJP_SFLB_DZ+8.5*SFJP_SFLC_DZ-IWAF  763
C          *SFJP_SFLC_DZ ALPHAZ=-SFJP_ALPHAZ <===                         763
               CALL AGSRESET                                              767
               AG_X = +SFJP_SFLA_PX+SFJP_FLEX_DI*JWAF                     767
               AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF                      767
               AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2.+16*WAFPCKLEN+WAFPCKLEN/  767
     *         2.+SFJP_SFLB_DZ+8.5*SFJP_SFLC_DZ-IWAF*SFJP_SFLC_DZ         767
               AG_ALPHAN=AG_ALPHAN+1                                      767
               AG_ALPHAK(AG_ALPHAN)=3                                     767
               AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                          767
               AG_TITLE = 'SFLC'                                          767
               AG_EXNAME = 'POSITION'                                     767
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                767
               CALL AXPOSITION                                            767
C          *                                                              767
C          Geant POSITION : SFLC ===> X=-SFJP_SFLA_PX-SFJP_FLEX_DI*JWAF   767
C          Y=SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF Z=+(SFPA_WPLEN+WAFPCKLEN)/2.  767
C          -16*WAFPCKLEN-WAFPCKLEN/2.-SFJP_SFLB_DZ-8.5*SFJP_SFLC_DZ+IWAF  767
C          *SFJP_SFLC_DZ ALPHAZ=+SFJP_ALPHAZ <===                         767
               CALL AGSRESET                                              771
               AG_X = -SFJP_SFLA_PX-SFJP_FLEX_DI*JWAF                     771
               AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF                      771
               AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2.-16*WAFPCKLEN-WAFPCKLEN/  771
     *         2.-SFJP_SFLB_DZ-8.5*SFJP_SFLC_DZ+IWAF*SFJP_SFLC_DZ         771
               AG_ALPHAN=AG_ALPHAN+1                                      771
               AG_ALPHAK(AG_ALPHAN)=3                                     771
               AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                          771
               AG_TITLE = 'SFLC'                                          771
               AG_EXNAME = 'POSITION'                                     771
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                771
               CALL AXPOSITION                                            771
C          *                                                              771
C          Geant POSITION : SFLA ===> X=+SFJP_SFLC_PX+SFJP_FLEX_DI*JWAF   771
C          Y=-SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF Z=-(SFPA_WPLEN+WAFPCKLEN)/2  771
C          +(IWAF+8)*WAFPCKLEN ALPHAZ=-SFJP_ALPHAZ <===                   771
               CALL AGSRESET                                              774
               AG_X = +SFJP_SFLC_PX+SFJP_FLEX_DI*JWAF                     774
               AG_Y = -SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF                     774
               AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+(IWAF+8)*WAFPCKLEN        774
               AG_ALPHAN=AG_ALPHAN+1                                      774
               AG_ALPHAK(AG_ALPHAN)=3                                     774
               AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                          774
               AG_TITLE = 'SFLA'                                          774
               AG_EXNAME = 'POSITION'                                     774
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                774
               CALL AXPOSITION                                            774
C          *                                                              774
C          Geant POSITION : SFLA ===> X=-SFJP_SFLC_PX-SFJP_FLEX_DI*JWAF   774
C          Y=-SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF Z=+(SFPA_WPLEN+WAFPCKLEN)/2  774
C          -(IWAF+8)*WAFPCKLEN ALPHAZ=+SFJP_ALPHAZ <===                   774
               CALL AGSRESET                                              777
               AG_X = -SFJP_SFLC_PX-SFJP_FLEX_DI*JWAF                     777
               AG_Y = -SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF                     777
               AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-(IWAF+8)*WAFPCKLEN        777
               AG_ALPHAN=AG_ALPHAN+1                                      777
               AG_ALPHAK(AG_ALPHAN)=3                                     777
               AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                          777
               AG_TITLE = 'SFLA'                                          777
               AG_EXNAME = 'POSITION'                                     777
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                777
               CALL AXPOSITION                                            777
C          *                                                              777
C          Geant POSITION : SFLC ===> X=+SFJP_SFLC_PX+SFJP_FLEX_DI*JWAF   777
C          Y=-SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF Z=-(SFPA_WPLEN+WAFPCKLEN)/2  777
C          +16*WAFPCKLEN+WAFPCKLEN/2.+8.5*SFJP_SFLC_DZ-IWAF*SFJP_SFLC_DZ  777
C           ALPHAZ=-SFJP_ALPHAZ <===                                      777
               CALL AGSRESET                                              781
               AG_X = +SFJP_SFLC_PX+SFJP_FLEX_DI*JWAF                     781
               AG_Y = -SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF                     781
               AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+16*WAFPCKLEN+WAFPCKLEN/   781
     *         2.+8.5*SFJP_SFLC_DZ-IWAF*SFJP_SFLC_DZ                      781
               AG_ALPHAN=AG_ALPHAN+1                                      781
               AG_ALPHAK(AG_ALPHAN)=3                                     781
               AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                          781
               AG_TITLE = 'SFLC'                                          781
               AG_EXNAME = 'POSITION'                                     781
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                781
               CALL AXPOSITION                                            781
C          *                                                              781
C          Geant POSITION : SFLC ===> X=-SFJP_SFLC_PX-SFJP_FLEX_DI*JWAF   781
C          Y=-SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF Z=+(SFPA_WPLEN+WAFPCKLEN)/2  781
C          -16*WAFPCKLEN-WAFPCKLEN/2.-8.5*SFJP_SFLC_DZ+IWAF*SFJP_SFLC_DZ  781
C           ALPHAZ=+SFJP_ALPHAZ <===                                      781
               CALL AGSRESET                                              785
               AG_X = -SFJP_SFLC_PX-SFJP_FLEX_DI*JWAF                     785
               AG_Y = -SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF                     785
               AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-16*WAFPCKLEN-WAFPCKLEN/   785
     *         2.-8.5*SFJP_SFLC_DZ+IWAF*SFJP_SFLC_DZ                      785
               AG_ALPHAN=AG_ALPHAN+1                                      785
               AG_ALPHAK(AG_ALPHAN)=3                                     785
               AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                          785
               AG_TITLE = 'SFLC'                                          785
               AG_EXNAME = 'POSITION'                                     785
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                785
               CALL AXPOSITION                                            785
C          *                                                              785
            GO TO 5871                                                    786
5872        CONTINUE                                                      786
C       *                                                                 787
            JWAF=1                                                        787
               GO TO 5883                                                 787
5881           JWAF=JWAF+1                                                787
5883           IF(JWAF-(8).GT.0) GO TO 5882                               787
C          Geant POSITION : SFLA ===> X=+SFJP_SFLA_PX+SFJP_FLEX_DI*JWAF   790
C          Y=SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF Z=-(SFPA_WPLEN+WAFPCKLEN)/2+  790
C          (IWAF+8)*WAFPCKLEN ALPHAZ=-SFJP_ALPHAZ <===                    790
               CALL AGSRESET                                              790
               AG_X = +SFJP_SFLA_PX+SFJP_FLEX_DI*JWAF                     790
               AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF                      790
               AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+(IWAF+8)*WAFPCKLEN        790
               AG_ALPHAN=AG_ALPHAN+1                                      790
               AG_ALPHAK(AG_ALPHAN)=3                                     790
               AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                          790
               AG_TITLE = 'SFLA'                                          790
               AG_EXNAME = 'POSITION'                                     790
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                790
               CALL AXPOSITION                                            790
C          *                                                              790
C          Geant POSITION : SFLA ===> X=-SFJP_SFLA_PX-SFJP_FLEX_DI*JWAF   790
C          Y=SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF Z=+(SFPA_WPLEN+WAFPCKLEN)/2-  790
C          (IWAF+8)*WAFPCKLEN ALPHAZ=+SFJP_ALPHAZ <===                    790
               CALL AGSRESET                                              793
               AG_X = -SFJP_SFLA_PX-SFJP_FLEX_DI*JWAF                     793
               AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF                      793
               AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-(IWAF+8)*WAFPCKLEN        793
               AG_ALPHAN=AG_ALPHAN+1                                      793
               AG_ALPHAK(AG_ALPHAN)=3                                     793
               AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                          793
               AG_TITLE = 'SFLA'                                          793
               AG_EXNAME = 'POSITION'                                     793
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                793
               CALL AXPOSITION                                            793
C          *                                                              793
            GO TO 5881                                                    794
5882        CONTINUE                                                      794
C       Geant POSITION : SFLB ===> X=+SFJP_SFLA_PX+SFJP_FLEX_DI*IWAF Y=S  798
C       FJP_SFLA_PY+SFJP_FLEX_DI*IWAF Z=-(SFPA_WPLEN+WAFPCKLEN)/2+16*WAF  798
C       PCKLEN+WAFPCKLEN/2.+SFJP_SFLB_DZ/2. ALPHAZ=-SFJP_ALPHAZ <===      798
            CALL AGSRESET                                                 798
            AG_X = +SFJP_SFLA_PX+SFJP_FLEX_DI*IWAF                        798
            AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*IWAF                         798
            AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+16*WAFPCKLEN+WAFPCKLEN/      798
     *      2.+SFJP_SFLB_DZ/2.                                            798
            AG_ALPHAN=AG_ALPHAN+1                                         798
            AG_ALPHAK(AG_ALPHAN)=3                                        798
            AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                             798
            AG_TITLE = 'SFLB'                                             798
            AG_EXNAME = 'POSITION'                                        798
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   798
            CALL AXPOSITION                                               798
C       *                                                                 798
C       Geant POSITION : SFLB ===> X=-SFJP_SFLA_PX-SFJP_FLEX_DI*IWAF Y=S  798
C       FJP_SFLA_PY+SFJP_FLEX_DI*IWAF Z=+(SFPA_WPLEN+WAFPCKLEN)/2-16*WAF  798
C       PCKLEN-WAFPCKLEN/2.-SFJP_SFLB_DZ/2. ALPHAZ=+SFJP_ALPHAZ <===      798
            CALL AGSRESET                                                 802
            AG_X = -SFJP_SFLA_PX-SFJP_FLEX_DI*IWAF                        802
            AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*IWAF                         802
            AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-16*WAFPCKLEN-WAFPCKLEN/      802
     *      2.-SFJP_SFLB_DZ/2.                                            802
            AG_ALPHAN=AG_ALPHAN+1                                         802
            AG_ALPHAK(AG_ALPHAN)=3                                        802
            AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                             802
            AG_TITLE = 'SFLB'                                             802
            AG_EXNAME = 'POSITION'                                        802
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   802
            CALL AXPOSITION                                               802
C       *                                                                 802
C       Geant POSITION : SFES ===> X=-SFJP_SFES_PX Y=-SFJP_SFES_PY Z=-(S  802
C       FPA_WPLEN+WAFPCKLEN)/2+(IWAF)*WAFPCKLEN+WAFPCKLEN/2.-SFJP_SFEB_D  802
C       Z/2.-SFJP_SFES_PZ/2. ALPHAZ=57.66 <===                            802
            CALL AGSRESET                                                 805
            AG_X = -SFJP_SFES_PX                                          805
            AG_Y = -SFJP_SFES_PY                                          805
            AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+(IWAF)*WAFPCKLEN+WAFPCKLEN/  805
     *      2.-SFJP_SFEB_DZ/2.-SFJP_SFES_PZ/2.                            805
            AG_ALPHAN=AG_ALPHAN+1                                         805
            AG_ALPHAK(AG_ALPHAN)=3                                        805
            AG_ALPHAL(AG_ALPHAN)=57.66                                    805
            AG_TITLE = 'SFES'                                             805
            AG_EXNAME = 'POSITION'                                        805
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   805
            CALL AXPOSITION                                               805
C       *                                                                 805
C       Geant POSITION : SFES ===> X=+SFJP_SFES_PX Y=-SFJP_SFES_PY Z=+(S  805
C       FPA_WPLEN+WAFPCKLEN)/2-(IWAF)*WAFPCKLEN-WAFPCKLEN/2.+SFJP_SFEB_D  805
C       Z/2.+SFJP_SFES_PZ/2. ALPHAZ=-57.66 <===                           805
            CALL AGSRESET                                                 808
            AG_X = +SFJP_SFES_PX                                          808
            AG_Y = -SFJP_SFES_PY                                          808
            AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-(IWAF)*WAFPCKLEN-WAFPCKLEN/  808
     *      2.+SFJP_SFEB_DZ/2.+SFJP_SFES_PZ/2.                            808
            AG_ALPHAN=AG_ALPHAN+1                                         808
            AG_ALPHAK(AG_ALPHAN)=3                                        808
            AG_ALPHAL(AG_ALPHAN)=-57.66                                   808
            AG_TITLE = 'SFES'                                             808
            AG_EXNAME = 'POSITION'                                        808
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   808
            CALL AXPOSITION                                               808
C       *                                                                 808
C       Geant POSITION : SFEB ===> X=-SFJP_SFEB_PX Y=-SFJP_SFEB_PY Z=-(S  808
C       FPA_WPLEN+WAFPCKLEN)/2+(IWAF+8)*WAFPCKLEN+WAFPCKLEN/2.-SFJP_SFEB  808
C       _DZ/2.-SFJP_SFES_PZ/2. ALPHAZ=55.35 <===                          808
            CALL AGSRESET                                                 811
            AG_X = -SFJP_SFEB_PX                                          811
            AG_Y = -SFJP_SFEB_PY                                          811
            AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/                               811
     *      2+(IWAF+8)*WAFPCKLEN+WAFPCKLEN/2.-SFJP_SFEB_DZ/               811
     *      2.-SFJP_SFES_PZ/2.                                            811
            AG_ALPHAN=AG_ALPHAN+1                                         811
            AG_ALPHAK(AG_ALPHAN)=3                                        811
            AG_ALPHAL(AG_ALPHAN)=55.35                                    811
            AG_TITLE = 'SFEB'                                             811
            AG_EXNAME = 'POSITION'                                        811
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   811
            CALL AXPOSITION                                               811
C       *                                                                 811
C       Geant POSITION : SFEB ===> X=+SFJP_SFEB_PX Y=-SFJP_SFEB_PY Z=+(S  811
C       FPA_WPLEN+WAFPCKLEN)/2-(IWAF+8)*WAFPCKLEN-WAFPCKLEN/2.+SFJP_SFEB  811
C       _DZ/2.+SFJP_SFES_PZ/2. ALPHAZ=-55.35 <===                         811
            CALL AGSRESET                                                 814
            AG_X = +SFJP_SFEB_PX                                          814
            AG_Y = -SFJP_SFEB_PY                                          814
            AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/                               814
     *      2-(IWAF+8)*WAFPCKLEN-WAFPCKLEN/2.+SFJP_SFEB_DZ/               814
     *      2.+SFJP_SFES_PZ/2.                                            814
            AG_ALPHAN=AG_ALPHAN+1                                         814
            AG_ALPHAK(AG_ALPHAN)=3                                        814
            AG_ALPHAL(AG_ALPHAN)=-55.35                                   814
            AG_TITLE = 'SFEB'                                             814
            AG_EXNAME = 'POSITION'                                        814
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   814
            CALL AXPOSITION                                               814
C       *                                                                 814
         GO TO 5861                                                       815
5862     CONTINUE                                                         815
5251  CALL AGSPOPB                                                        816
      GO TO BLOCK_SFSM, (5250 ,5260)                                      816
C End of Local Procedure                                                  816
C ----------------------------------------------------------------------  817
C ------------ SFLT                                                       817
C Begin Local Procedure SFLT                                              818
C SFLT IS (HALF) THE TOP CORNER OF THE TRIANGULAR LADDER SKELETON         818
5440  CONTINUE                                                            818
         AG_TITLE='SFLT'                                                  818
         AG_MODULE=MODULE                                                 818
         AG_MARK='V00'                                                    818
         CALL AGSPUSH                                                     818
         AG_MODULE=MODULE                                                 818
C    *                                                                    818
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           819
         CALL AGSRESET                                                    819
         AG_TITLE = 'CARBON'                                              819
         AG_EXNAME = 'MATERIAL'                                           819
         AG_PARLIST = 'NONE'                                              819
         CALL AXMATERIAL                                                  819
C    *                                                                    819
C    Geant ATTRIBUTE : SFLT ===> SEEN=1 COLO=1 <===                       819
         CALL AGSRESET                                                    820
         AG_SEEN = 1                                                      820
         AG_COLO = 1                                                      820
         AG_TITLE = 'SFLT'                                                820
         AG_EXNAME = 'ATTRIBUTE'                                          820
         AG_PARLIST = 'SEEN_COLO'                                         820
         CALL AXATTRIBUTE                                                 820
C    *                                                                    820
C    Geant SHAPE : TRAP ===> DZ=SFJP_SFLU_DZ THET=0 PHI=0, H1=SFJP_SFLU_  820
C    H1 BL1=SFJP_SFLU_BL1 TL1=SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(PI/5), ALP1  820
C    =(PI/2-ATAN(2*TAN(PI/5)))*360/(2*PI), H2=SFJP_SFLU_H1 BL2=SFJP_SFLU  820
C    _BL1, TL2=SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(PI/5), ALP2=(PI/2-ATAN(2*T  820
C    AN(PI/5)))*360/(2*PI) <===                                           820
         CALL AGSRESET                                                    826
         AG_DZ = SFJP_SFLU_DZ                                             826
         AG_THET = 0                                                      826
         AG_PHI = 0                                                       826
         AG_H1 = SFJP_SFLU_H1                                             826
         AG_BL1 = SFJP_SFLU_BL1                                           826
         AG_TL1 = SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(PI/5)                    826
         AG_ALP1 = (PI/2-ATAN(2*TAN(PI/5)))*360/(2*PI)                    826
         AG_H2 = SFJP_SFLU_H1                                             826
         AG_BL2 = SFJP_SFLU_BL1                                           826
         AG_TL2 = SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(PI/5)                    826
         AG_ALP2 = (PI/2-ATAN(2*TAN(PI/5)))*360/(2*PI)                    826
         AG_TITLE = 'TRAP'                                                826
         AG_EXNAME = 'SHAPE'                                              826
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       826
         CALL AXSHAPE                                                     826
C    *                                                                    826
5441  CALL AGSPOPB                                                        827
      GO TO BLOCK_SFLT, (5440 ,5450)                                      827
C End of Local Procedure                                                  827
C ----------------------------------------------------------------------  828
C ------------ SFLU                                                       828
C Begin Local Procedure SFLU                                              829
C SFLU IS (HALF) THE SIDE CORNER OF THE TRIANGULAR LADDER SKELETON        829
5460  CONTINUE                                                            829
         AG_TITLE='SFLU'                                                  829
         AG_MODULE=MODULE                                                 829
         AG_MARK='V00'                                                    829
         CALL AGSPUSH                                                     829
         AG_MODULE=MODULE                                                 829
C    *                                                                    829
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           830
         CALL AGSRESET                                                    830
         AG_TITLE = 'CARBON'                                              830
         AG_EXNAME = 'MATERIAL'                                           830
         AG_PARLIST = 'NONE'                                              830
         CALL AXMATERIAL                                                  830
C    *                                                                    830
C    Geant ATTRIBUTE : SFLU ===> SEEN=1 COLO=1 <===                       830
         CALL AGSRESET                                                    831
         AG_SEEN = 1                                                      831
         AG_COLO = 1                                                      831
         AG_TITLE = 'SFLU'                                                831
         AG_EXNAME = 'ATTRIBUTE'                                          831
         AG_PARLIST = 'SEEN_COLO'                                         831
         CALL AXATTRIBUTE                                                 831
C    *                                                                    831
C    Geant SHAPE : TRAP ===> DZ=SFJP_SFLU_DZ THET=0 PHI=0, H1=SFJP_SFLU_  831
C    H1 BL1=SFJP_SFLU_BL1 TL1=SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(27*2*PI/360  831
C    ), ALP1=(PI/2-ATAN(2*TAN(27*2*PI/360)))*360/(2*PI), H2=SFJP_SFLU_H1  831
C    , BL2=SFJP_SFLU_BL1 TL2=SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(27*2*PI/360)  831
C    , ALP2=(PI/2-ATAN(2*TAN(27*2*PI/360)))*360/(2*PI) <===               831
         CALL AGSRESET                                                    837
         AG_DZ = SFJP_SFLU_DZ                                             837
         AG_THET = 0                                                      837
         AG_PHI = 0                                                       837
         AG_H1 = SFJP_SFLU_H1                                             837
         AG_BL1 = SFJP_SFLU_BL1                                           837
         AG_TL1 = SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(27*2*PI/360)             837
         AG_ALP1 = (PI/2-ATAN(2*TAN(27*2*PI/360)))*360/(2*PI)             837
         AG_H2 = SFJP_SFLU_H1                                             837
         AG_BL2 = SFJP_SFLU_BL1                                           837
         AG_TL2 = SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(27*2*PI/360)             837
         AG_ALP2 = (PI/2-ATAN(2*TAN(27*2*PI/360)))*360/(2*PI)             837
         AG_TITLE = 'TRAP'                                                837
         AG_EXNAME = 'SHAPE'                                              837
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       837
         CALL AXSHAPE                                                     837
C    *                                                                    837
5461  CALL AGSPOPB                                                        838
      GO TO BLOCK_SFLU, (5460 ,5470)                                      838
C End of Local Procedure                                                  838
C ----------------------------------------------------------------------  839
C ------------ SFFK                                                       839
C Begin Local Procedure SFFK                                              840
C SFFK HORIZONTAL PART OF THE LADDER SKELETON CARBON BASE                 840
5480  CONTINUE                                                            840
         AG_TITLE='SFFK'                                                  840
         AG_MODULE=MODULE                                                 840
         AG_MARK='V00'                                                    840
         CALL AGSPUSH                                                     840
         AG_MODULE=MODULE                                                 840
C    *                                                                    840
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           841
         CALL AGSRESET                                                    841
         AG_TITLE = 'CARBON'                                              841
         AG_EXNAME = 'MATERIAL'                                           841
         AG_PARLIST = 'NONE'                                              841
         CALL AXMATERIAL                                                  841
C    *                                                                    841
C    Geant ATTRIBUTE : SFFK ===> SEEN=1 COLO=1 <===                       841
         CALL AGSRESET                                                    842
         AG_SEEN = 1                                                      842
         AG_COLO = 1                                                      842
         AG_TITLE = 'SFFK'                                                842
         AG_EXNAME = 'ATTRIBUTE'                                          842
         AG_PARLIST = 'SEEN_COLO'                                         842
         CALL AXATTRIBUTE                                                 842
C    *                                                                    842
C    Geant SHAPE : BOX ===> DX=SFJP_SFFK_DXE DY=SFJP_SFFK_DY, DZ=SFJP_SF  842
C    FK_DZ/2 <===                                                         842
         CALL AGSRESET                                                    844
         AG_DX = SFJP_SFFK_DXE                                            844
         AG_DY = SFJP_SFFK_DY                                             844
         AG_DZ = SFJP_SFFK_DZ/2                                           844
         AG_TITLE = 'BOX'                                                 844
         AG_EXNAME = 'SHAPE'                                              844
         AG_PARLIST = 'DX_DY_DZ'                                          844
         CALL AXSHAPE                                                     844
C    *                                                                    844
5481  CALL AGSPOPB                                                        845
      GO TO BLOCK_SFFK, (5480 ,5490)                                      845
C End of Local Procedure                                                  845
C ----------------------------------------------------------------------  846
C ------------ SFFL                                                       846
C Begin Local Procedure SFFL                                              847
C SFFL TITLED PART OF THE LADDER SKELETON CARBON BASE                     847
5500  CONTINUE                                                            847
         AG_TITLE='SFFL'                                                  847
         AG_MODULE=MODULE                                                 847
         AG_MARK='V00'                                                    847
         CALL AGSPUSH                                                     847
         AG_MODULE=MODULE                                                 847
C    *                                                                    847
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           848
         CALL AGSRESET                                                    848
         AG_TITLE = 'CARBON'                                              848
         AG_EXNAME = 'MATERIAL'                                           848
         AG_PARLIST = 'NONE'                                              848
         CALL AXMATERIAL                                                  848
C    *                                                                    848
C    Geant ATTRIBUTE : SFFL ===> SEEN=1 COLO=1 <===                       848
         CALL AGSRESET                                                    849
         AG_SEEN = 1                                                      849
         AG_COLO = 1                                                      849
         AG_TITLE = 'SFFL'                                                849
         AG_EXNAME = 'ATTRIBUTE'                                          849
         AG_PARLIST = 'SEEN_COLO'                                         849
         CALL AXATTRIBUTE                                                 849
C    *                                                                    849
C    Geant SHAPE : BOX ===> DX=SFJP_SFFL_DX/COS(15*PI/180), DY=SFJP_SFFK  849
C    _DY DZ=SFJP_SFFK_DZ/2 <===                                           849
         CALL AGSRESET                                                    851
         AG_DX = SFJP_SFFL_DX/COS(15*PI/180)                              851
         AG_DY = SFJP_SFFK_DY                                             851
         AG_DZ = SFJP_SFFK_DZ/2                                           851
         AG_TITLE = 'BOX'                                                 851
         AG_EXNAME = 'SHAPE'                                              851
         AG_PARLIST = 'DX_DY_DZ'                                          851
         CALL AXSHAPE                                                     851
C    *                                                                    851
5501  CALL AGSPOPB                                                        852
      GO TO BLOCK_SFFL, (5500 ,5510)                                      852
C End of Local Procedure                                                  852
C ----------------------------------------------------------------------  853
C ------------ SFKK                                                       853
C Begin Local Procedure SFKK                                              854
C SFKK HORIZONTAL PART OF THE KAPTON FILM UNDER THE LADDER BASE           854
5520  CONTINUE                                                            854
         AG_TITLE='SFKK'                                                  854
         AG_MODULE=MODULE                                                 854
         AG_MARK='V00'                                                    854
         CALL AGSPUSH                                                     854
         AG_MODULE=MODULE                                                 854
C    *                                                                    854
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            855
         CALL AGSRESET                                                    855
         AG_TITLE = 'MYLAR'                                               855
         AG_EXNAME = 'MATERIAL'                                           855
         AG_PARLIST = 'NONE'                                              855
         CALL AXMATERIAL                                                  855
C    *                                                                    855
C    Geant ATTRIBUTE : SFKK ===> SEEN=1 COLO=1 <===                       855
         CALL AGSRESET                                                    856
         AG_SEEN = 1                                                      856
         AG_COLO = 1                                                      856
         AG_TITLE = 'SFKK'                                                856
         AG_EXNAME = 'ATTRIBUTE'                                          856
         AG_PARLIST = 'SEEN_COLO'                                         856
         CALL AXATTRIBUTE                                                 856
C    *                                                                    856
C    Geant SHAPE : BOX ===> DX=SFJP_SFFK_DXZ/2. DY=SFJP_SFKK_DY/2., DZ=S  856
C    FJP_SFFK_DZ/2 <===                                                   856
         CALL AGSRESET                                                    858
         AG_DX = SFJP_SFFK_DXZ/2.                                         858
         AG_DY = SFJP_SFKK_DY/2.                                          858
         AG_DZ = SFJP_SFFK_DZ/2                                           858
         AG_TITLE = 'BOX'                                                 858
         AG_EXNAME = 'SHAPE'                                              858
         AG_PARLIST = 'DX_DY_DZ'                                          858
         CALL AXSHAPE                                                     858
C    *                                                                    858
5521  CALL AGSPOPB                                                        859
      GO TO BLOCK_SFKK, (5520 ,5530)                                      859
C End of Local Procedure                                                  859
C ----------------------------------------------------------------------  860
C ------------ SFKL                                                       860
C Begin Local Procedure SFKL                                              861
C SFKL TITLED PART OF THE KPATON FILM UNDER THE LADDER BASE               861
5540  CONTINUE                                                            861
         AG_TITLE='SFKL'                                                  861
         AG_MODULE=MODULE                                                 861
         AG_MARK='V00'                                                    861
         CALL AGSPUSH                                                     861
         AG_MODULE=MODULE                                                 861
C    *                                                                    861
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            862
         CALL AGSRESET                                                    862
         AG_TITLE = 'MYLAR'                                               862
         AG_EXNAME = 'MATERIAL'                                           862
         AG_PARLIST = 'NONE'                                              862
         CALL AXMATERIAL                                                  862
C    *                                                                    862
C    Geant ATTRIBUTE : SFKL ===> SEEN=1 COLO=1 <===                       862
         CALL AGSRESET                                                    863
         AG_SEEN = 1                                                      863
         AG_COLO = 1                                                      863
         AG_TITLE = 'SFKL'                                                863
         AG_EXNAME = 'ATTRIBUTE'                                          863
         AG_PARLIST = 'SEEN_COLO'                                         863
         CALL AXATTRIBUTE                                                 863
C    *                                                                    863
C    Geant SHAPE : BOX ===> DX=SFJP_SFFL_DX/COS(15*PI/180), DY=SFJP_SFKK  863
C    _DY/2. DZ=SFJP_SFFK_DZ/2 <===                                        863
         CALL AGSRESET                                                    865
         AG_DX = SFJP_SFFL_DX/COS(15*PI/180)                              865
         AG_DY = SFJP_SFKK_DY/2.                                          865
         AG_DZ = SFJP_SFFK_DZ/2                                           865
         AG_TITLE = 'BOX'                                                 865
         AG_EXNAME = 'SHAPE'                                              865
         AG_PARLIST = 'DX_DY_DZ'                                          865
         CALL AXSHAPE                                                     865
C    *                                                                    865
5541  CALL AGSPOPB                                                        866
      GO TO BLOCK_SFKL, (5540 ,5550)                                      866
C End of Local Procedure                                                  866
C ----------------------------------------------------------------------  867
C ------------ SFLA                                                       867
C Begin Local Procedure SFLA                                              869
C SFLA IS THE LONG PART OF THE BUS CABLE LINKING THE MODULES TO THE CONN  869
C ECTION, BOARD                                                           869
5760  CONTINUE                                                            869
         AG_TITLE='SFLA'                                                  869
         AG_MODULE=MODULE                                                 869
         AG_MARK='V00'                                                    869
         CALL AGSPUSH                                                     869
         AG_MODULE=MODULE                                                 869
C    *                                                                    869
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            870
         CALL AGSRESET                                                    870
         AG_TITLE = 'MYLAR'                                               870
         AG_EXNAME = 'MATERIAL'                                           870
         AG_PARLIST = 'NONE'                                              870
         CALL AXMATERIAL                                                  870
C    *                                                                    870
C    Geant ATTRIBUTE : SFLA ===> SEEN=1 COLO=1 <===                       870
         CALL AGSRESET                                                    871
         AG_SEEN = 1                                                      871
         AG_COLO = 1                                                      871
         AG_TITLE = 'SFLA'                                                871
         AG_EXNAME = 'ATTRIBUTE'                                          871
         AG_PARLIST = 'SEEN_COLO'                                         871
         CALL AXATTRIBUTE                                                 871
C    *                                                                    871
C    Geant SHAPE : BOX ===> DX=SFJP_SFLA_DX/2. DY=SFJP_SFLA_DY/2., DZ=SF  871
C    JP_SFLA_DZ/2. <===                                                   871
         CALL AGSRESET                                                    873
         AG_DX = SFJP_SFLA_DX/2.                                          873
         AG_DY = SFJP_SFLA_DY/2.                                          873
         AG_DZ = SFJP_SFLA_DZ/2.                                          873
         AG_TITLE = 'BOX'                                                 873
         AG_EXNAME = 'SHAPE'                                              873
         AG_PARLIST = 'DX_DY_DZ'                                          873
         CALL AXSHAPE                                                     873
C    *                                                                    873
5761  CALL AGSPOPB                                                        874
      GO TO BLOCK_SFLA, (5760 ,5770)                                      874
C End of Local Procedure                                                  874
C ----------------------------------------------------------------------  875
C ------------ SFLB                                                       875
C Begin Local Procedure SFLB                                              876
C SFLB IS THE PART OF THE LONG BUS CABLE ON THE CONNECTION BOARD          876
5780  CONTINUE                                                            876
         AG_TITLE='SFLB'                                                  876
         AG_MODULE=MODULE                                                 876
         AG_MARK='V00'                                                    876
         CALL AGSPUSH                                                     876
         AG_MODULE=MODULE                                                 876
C    *                                                                    876
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            877
         CALL AGSRESET                                                    877
         AG_TITLE = 'MYLAR'                                               877
         AG_EXNAME = 'MATERIAL'                                           877
         AG_PARLIST = 'NONE'                                              877
         CALL AXMATERIAL                                                  877
C    *                                                                    877
C    Geant ATTRIBUTE : SFLB ===> SEEN=1 COLO=1 <===                       877
         CALL AGSRESET                                                    878
         AG_SEEN = 1                                                      878
         AG_COLO = 1                                                      878
         AG_TITLE = 'SFLB'                                                878
         AG_EXNAME = 'ATTRIBUTE'                                          878
         AG_PARLIST = 'SEEN_COLO'                                         878
         CALL AXATTRIBUTE                                                 878
C    *                                                                    878
C    Geant SHAPE : BOX ===> DX=SFJP_SFLA_DX/2. DY=SFJP_SFLA_DY/2., DZ=SF  878
C    JP_SFLB_DZ/2. <===                                                   878
         CALL AGSRESET                                                    880
         AG_DX = SFJP_SFLA_DX/2.                                          880
         AG_DY = SFJP_SFLA_DY/2.                                          880
         AG_DZ = SFJP_SFLB_DZ/2.                                          880
         AG_TITLE = 'BOX'                                                 880
         AG_EXNAME = 'SHAPE'                                              880
         AG_PARLIST = 'DX_DY_DZ'                                          880
         CALL AXSHAPE                                                     880
C    *                                                                    880
5781  CALL AGSPOPB                                                        881
      GO TO BLOCK_SFLB, (5780 ,5790)                                      881
C End of Local Procedure                                                  881
C ----------------------------------------------------------------------  882
C ------------ SFLC                                                       882
C Begin Local Procedure SFLC                                              884
C SFLC IS THE PART OF THE LONG BUS CABLE ON THE CONNECTION BOARD UP TO,   884
C THE CONNECTOR                                                           884
5800  CONTINUE                                                            884
         AG_TITLE='SFLC'                                                  884
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
C    Geant ATTRIBUTE : SFLC ===> SEEN=1 COLO=1 <===                       885
         CALL AGSRESET                                                    886
         AG_SEEN = 1                                                      886
         AG_COLO = 1                                                      886
         AG_TITLE = 'SFLC'                                                886
         AG_EXNAME = 'ATTRIBUTE'                                          886
         AG_PARLIST = 'SEEN_COLO'                                         886
         CALL AXATTRIBUTE                                                 886
C    *                                                                    886
C    Geant SHAPE : BOX ===> DX=SFJP_SFLA_DX/2. DY=SFJP_SFLA_DY/2., DZ=SF  886
C    JP_SFLC_DZ/2. <===                                                   886
         CALL AGSRESET                                                    888
         AG_DX = SFJP_SFLA_DX/2.                                          888
         AG_DY = SFJP_SFLA_DY/2.                                          888
         AG_DZ = SFJP_SFLC_DZ/2.                                          888
         AG_TITLE = 'BOX'                                                 888
         AG_EXNAME = 'SHAPE'                                              888
         AG_PARLIST = 'DX_DY_DZ'                                          888
         CALL AXSHAPE                                                     888
C    *                                                                    888
5801  CALL AGSPOPB                                                        889
      GO TO BLOCK_SFLC, (5800 ,5810)                                      889
C End of Local Procedure                                                  889
C ----------------------------------------------------------------------  890
C ------------ SFEB                                                       890
C Begin Local Procedure SFEB                                              891
C SFEB IS THE BIG BUS ELBOW                                               891
5820  CONTINUE                                                            891
         AG_TITLE='SFEB'                                                  891
         AG_MODULE=MODULE                                                 891
         AG_MARK='V00'                                                    891
         CALL AGSPUSH                                                     891
         AG_MODULE=MODULE                                                 891
C    *                                                                    891
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            892
         CALL AGSRESET                                                    892
         AG_TITLE = 'MYLAR'                                               892
         AG_EXNAME = 'MATERIAL'                                           892
         AG_PARLIST = 'NONE'                                              892
         CALL AXMATERIAL                                                  892
C    *                                                                    892
C    Geant ATTRIBUTE : SFEB ===> SEEN=1 COLO=1 <===                       892
         CALL AGSRESET                                                    893
         AG_SEEN = 1                                                      893
         AG_COLO = 1                                                      893
         AG_TITLE = 'SFEB'                                                893
         AG_EXNAME = 'ATTRIBUTE'                                          893
         AG_PARLIST = 'SEEN_COLO'                                         893
         CALL AXATTRIBUTE                                                 893
C    *                                                                    893
C    Geant SHAPE : BOX ===> DX=SFJP_SFEB_DX/2. DY=SFJP_SFLA_DY/2., DZ=SF  893
C    JP_SFEB_DZ/2. <===                                                   893
         CALL AGSRESET                                                    895
         AG_DX = SFJP_SFEB_DX/2.                                          895
         AG_DY = SFJP_SFLA_DY/2.                                          895
         AG_DZ = SFJP_SFEB_DZ/2.                                          895
         AG_TITLE = 'BOX'                                                 895
         AG_EXNAME = 'SHAPE'                                              895
         AG_PARLIST = 'DX_DY_DZ'                                          895
         CALL AXSHAPE                                                     895
C    *                                                                    895
5821  CALL AGSPOPB                                                        896
      GO TO BLOCK_SFEB, (5820 ,5830)                                      896
C End of Local Procedure                                                  896
C ----------------------------------------------------------------------  897
C ------------ SFES                                                       897
C Begin Local Procedure SFES                                              898
C SFES IS THE SMALL BUS ELBOW                                             898
5840  CONTINUE                                                            898
         AG_TITLE='SFES'                                                  898
         AG_MODULE=MODULE                                                 898
         AG_MARK='V00'                                                    898
         CALL AGSPUSH                                                     898
         AG_MODULE=MODULE                                                 898
C    *                                                                    898
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            899
         CALL AGSRESET                                                    899
         AG_TITLE = 'MYLAR'                                               899
         AG_EXNAME = 'MATERIAL'                                           899
         AG_PARLIST = 'NONE'                                              899
         CALL AXMATERIAL                                                  899
C    *                                                                    899
C    Geant ATTRIBUTE : SFES ===> SEEN=1 COLO=1 <===                       899
         CALL AGSRESET                                                    900
         AG_SEEN = 1                                                      900
         AG_COLO = 1                                                      900
         AG_TITLE = 'SFES'                                                900
         AG_EXNAME = 'ATTRIBUTE'                                          900
         AG_PARLIST = 'SEEN_COLO'                                         900
         CALL AXATTRIBUTE                                                 900
C    *                                                                    900
C    Geant SHAPE : BOX ===> DX=SFJP_SFES_DX/2. DY=SFJP_SFLA_DY/2., DZ=SF  900
C    JP_SFEB_DZ/2. <===                                                   900
         CALL AGSRESET                                                    902
         AG_DX = SFJP_SFES_DX/2.                                          902
         AG_DY = SFJP_SFLA_DY/2.                                          902
         AG_DZ = SFJP_SFEB_DZ/2.                                          902
         AG_TITLE = 'BOX'                                                 902
         AG_EXNAME = 'SHAPE'                                              902
         AG_PARLIST = 'DX_DY_DZ'                                          902
         CALL AXSHAPE                                                     902
C    *                                                                    902
5841  CALL AGSPOPB                                                        903
      GO TO BLOCK_SFES, (5840 ,5850)                                      903
C End of Local Procedure                                                  903
C ----------------------------------------------------------------------  904
C ------------ SFAM                                                       904
C Begin Local Procedure SFAM                                              905
C SFAM IS THE MOTHER VOLUME OF THE ADC BOARD                              905
5580  CONTINUE                                                            905
         AG_TITLE='SFAM'                                                  905
         AG_MODULE=MODULE                                                 905
         AG_MARK='V00'                                                    905
         CALL AGSPUSH                                                     905
         AG_MODULE=MODULE                                                 905
C    *                                                                    905
C    Geant MATERIAL : AIR ===> inherited arguments used <===              906
         CALL AGSRESET                                                    906
         AG_TITLE = 'AIR'                                                 906
         AG_EXNAME = 'MATERIAL'                                           906
         AG_PARLIST = 'NONE'                                              906
         CALL AXMATERIAL                                                  906
C    *                                                                    906
C    Geant ATTRIBUTE : SFAM ===> SEEN=0 COLO=6 <===                       906
         CALL AGSRESET                                                    907
         AG_SEEN = 0                                                      907
         AG_COLO = 6                                                      907
         AG_TITLE = 'SFAM'                                                907
         AG_EXNAME = 'ATTRIBUTE'                                          907
         AG_PARLIST = 'SEEN_COLO'                                         907
         CALL AXATTRIBUTE                                                 907
C    *                                                                    907
C    Geant SHAPE : BOX ===> DX=SFJP_SFAM_DXE/2. DY=SFJP_SFAM_DY/2., DZ=S  907
C    FJP_SFAM_DZ/2 <===                                                   907
         CALL AGSRESET                                                    909
         AG_DX = SFJP_SFAM_DXE/2.                                         909
         AG_DY = SFJP_SFAM_DY/2.                                          909
         AG_DZ = SFJP_SFAM_DZ/2                                           909
         AG_TITLE = 'BOX'                                                 909
         AG_EXNAME = 'SHAPE'                                              909
         AG_PARLIST = 'DX_DY_DZ'                                          909
         CALL AXSHAPE                                                     909
C    *                                                                    909
C    Create SFAB :                                                        910
C    Call To Local Procedure SFAB                                         910
         ASSIGN 5900 TO BLOCK_SFAB                                        910
         GO TO  5890                                                      910
5900     CONTINUE                                                         910
C    *                                                                    910
C    Geant POSITION : SFAB ===> X=0. Y=0. Z=SFJP_SFAM_DZS/2 <===          911
         CALL AGSRESET                                                    911
         AG_X = 0.                                                        911
         AG_Y = 0.                                                        911
         AG_Z = SFJP_SFAM_DZS/2                                           911
         AG_TITLE = 'SFAB'                                                911
         AG_EXNAME = 'POSITION'                                           911
         AG_PARLIST = 'X_Y_Z'                                             911
         CALL AXPOSITION                                                  911
C    *                                                                    911
C    Create SFAS :                                                        912
C    Call To Local Procedure SFAS                                         912
         ASSIGN 5920 TO BLOCK_SFAS                                        912
         GO TO  5910                                                      912
5920     CONTINUE                                                         912
C    *                                                                    912
C    Geant POSITION : SFAS ===> X=(SFJP_SFAM_DXE-SFJP_SFAM_DXZ)/2. Y=0.   914
C    Z=(-SFJP_SFAM_DZ+SFJP_SFAM_DZS)/2 <===                               914
         CALL AGSRESET                                                    914
         AG_X = (SFJP_SFAM_DXE-SFJP_SFAM_DXZ)/2.                          914
         AG_Y = 0.                                                        914
         AG_Z = (-SFJP_SFAM_DZ+SFJP_SFAM_DZS)/2                           914
         AG_TITLE = 'SFAS'                                                914
         AG_EXNAME = 'POSITION'                                           914
         AG_PARLIST = 'X_Y_Z'                                             914
         CALL AXPOSITION                                                  914
C    *                                                                    914
C    Geant POSITION : SFAS ===> X=(-SFJP_SFAM_DXE+SFJP_SFAM_DXZ)/2. Y=0.  914
C     Z=(-SFJP_SFAM_DZ+SFJP_SFAM_DZS)/2 <===                              914
         CALL AGSRESET                                                    916
         AG_X = (-SFJP_SFAM_DXE+SFJP_SFAM_DXZ)/2.                         916
         AG_Y = 0.                                                        916
         AG_Z = (-SFJP_SFAM_DZ+SFJP_SFAM_DZS)/2                           916
         AG_TITLE = 'SFAS'                                                916
         AG_EXNAME = 'POSITION'                                           916
         AG_PARLIST = 'X_Y_Z'                                             916
         CALL AXPOSITION                                                  916
C    *                                                                    916
5581  CALL AGSPOPB                                                        917
      GO TO BLOCK_SFAM, (5580 ,5590)                                      917
C End of Local Procedure                                                  917
C ----------------------------------------------------------------------  918
C ------------ SFAB                                                       918
C Begin Local Procedure SFAB                                              919
C SFAB IS THE BIG VOLUME OF THE ADC BOARD                                 919
5890  CONTINUE                                                            919
         AG_TITLE='SFAB'                                                  919
         AG_MODULE=MODULE                                                 919
         AG_MARK='V00'                                                    919
         CALL AGSPUSH                                                     919
         AG_MODULE=MODULE                                                 919
C    *                                                                    919
C    Geant MATERIAL : G10 ===> inherited arguments used <===              920
         CALL AGSRESET                                                    920
         AG_TITLE = 'G10'                                                 920
         AG_EXNAME = 'MATERIAL'                                           920
         AG_PARLIST = 'NONE'                                              920
         CALL AXMATERIAL                                                  920
C    *                                                                    920
C    Geant ATTRIBUTE : SFAB ===> SEEN=1 COLO=30 <===                      920
         CALL AGSRESET                                                    921
         AG_SEEN = 1                                                      921
         AG_COLO = 30                                                     921
         AG_TITLE = 'SFAB'                                                921
         AG_EXNAME = 'ATTRIBUTE'                                          921
         AG_PARLIST = 'SEEN_COLO'                                         921
         CALL AXATTRIBUTE                                                 921
C    *                                                                    921
C    Geant SHAPE : BOX ===> DX=SFJP_SFAM_DXE/2. DY=SFJP_SFAM_DY/2., DZ=(  921
C    SFJP_SFAM_DZ-SFJP_SFAM_DZS)/2 <===                                   921
         CALL AGSRESET                                                    923
         AG_DX = SFJP_SFAM_DXE/2.                                         923
         AG_DY = SFJP_SFAM_DY/2.                                          923
         AG_DZ = (SFJP_SFAM_DZ-SFJP_SFAM_DZS)/2                           923
         AG_TITLE = 'BOX'                                                 923
         AG_EXNAME = 'SHAPE'                                              923
         AG_PARLIST = 'DX_DY_DZ'                                          923
         CALL AXSHAPE                                                     923
C    *                                                                    923
5891  CALL AGSPOPB                                                        924
      GO TO BLOCK_SFAB, (5890 ,5900)                                      924
C End of Local Procedure                                                  924
C ----------------------------------------------------------------------  925
C ------------ SFAS                                                       925
C Begin Local Procedure SFAS                                              926
C SFAS IS THE SMALL VOLUME OF THE ADC BOARD                               926
5910  CONTINUE                                                            926
         AG_TITLE='SFAS'                                                  926
         AG_MODULE=MODULE                                                 926
         AG_MARK='V00'                                                    926
         CALL AGSPUSH                                                     926
         AG_MODULE=MODULE                                                 926
C    *                                                                    926
C    Geant MATERIAL : G10 ===> inherited arguments used <===              927
         CALL AGSRESET                                                    927
         AG_TITLE = 'G10'                                                 927
         AG_EXNAME = 'MATERIAL'                                           927
         AG_PARLIST = 'NONE'                                              927
         CALL AXMATERIAL                                                  927
C    *                                                                    927
C    Geant ATTRIBUTE : SFAS ===> SEEN=1 COLO=30 <===                      927
         CALL AGSRESET                                                    928
         AG_SEEN = 1                                                      928
         AG_COLO = 30                                                     928
         AG_TITLE = 'SFAS'                                                928
         AG_EXNAME = 'ATTRIBUTE'                                          928
         AG_PARLIST = 'SEEN_COLO'                                         928
         CALL AXATTRIBUTE                                                 928
C    *                                                                    928
C    Geant SHAPE : BOX ===> DX=(SFJP_SFAM_DXZ)/2. DY=(SFJP_SFAM_DY)/2.,   928
C    DZ=(SFJP_SFAM_DZS)/2 <===                                            928
         CALL AGSRESET                                                    930
         AG_DX = (SFJP_SFAM_DXZ)/2.                                       930
         AG_DY = (SFJP_SFAM_DY)/2.                                        930
         AG_DZ = (SFJP_SFAM_DZS)/2                                        930
         AG_TITLE = 'BOX'                                                 930
         AG_EXNAME = 'SHAPE'                                              930
         AG_PARLIST = 'DX_DY_DZ'                                          930
         CALL AXSHAPE                                                     930
C    *                                                                    930
5911  CALL AGSPOPB                                                        931
      GO TO BLOCK_SFAS, (5910 ,5920)                                      931
C End of Local Procedure                                                  931
C ----------------------------------------------------------------------  932
C ------------ SAPP                                                       932
C Begin Local Procedure SAPP                                              933
C SAPP IS THE MOTHER VOLUME OF THE ADC BOARD APPENDICE                    933
5560  CONTINUE                                                            933
         AG_TITLE='SAPP'                                                  933
         AG_MODULE=MODULE                                                 933
         AG_MARK='V00'                                                    933
         CALL AGSPUSH                                                     933
         AG_MODULE=MODULE                                                 933
C    *                                                                    933
C    Geant MATERIAL : AIR ===> inherited arguments used <===              934
         CALL AGSRESET                                                    934
         AG_TITLE = 'AIR'                                                 934
         AG_EXNAME = 'MATERIAL'                                           934
         AG_PARLIST = 'NONE'                                              934
         CALL AXMATERIAL                                                  934
C    *                                                                    934
C    Geant ATTRIBUTE : SAPP ===> SEEN=0 COLO=6 <===                       934
         CALL AGSRESET                                                    935
         AG_SEEN = 0                                                      935
         AG_COLO = 6                                                      935
         AG_TITLE = 'SAPP'                                                935
         AG_EXNAME = 'ATTRIBUTE'                                          935
         AG_PARLIST = 'SEEN_COLO'                                         935
         CALL AXATTRIBUTE                                                 935
C    *                                                                    935
C    Geant SHAPE : BOX ===> DX=(SFJP_SAPP_DXE+2*SFJP_SAPP_DXZ)/2., DY=(S  935
C    FJP_SAPP_DY+2*SFJP_SAPP_DXZ)/2., DZ=SFJP_SAPP_DZ/2. <===             935
         CALL AGSRESET                                                    938
         AG_DX = (SFJP_SAPP_DXE+2*SFJP_SAPP_DXZ)/2.                       938
         AG_DY = (SFJP_SAPP_DY+2*SFJP_SAPP_DXZ)/2.                        938
         AG_DZ = SFJP_SAPP_DZ/2.                                          938
         AG_TITLE = 'BOX'                                                 938
         AG_EXNAME = 'SHAPE'                                              938
         AG_PARLIST = 'DX_DY_DZ'                                          938
         CALL AXSHAPE                                                     938
C    *                                                                    938
C    Create SAPC :                                                        939
C    Call To Local Procedure SAPC                                         939
         ASSIGN 5940 TO BLOCK_SAPC                                        939
         GO TO  5930                                                      939
5940     CONTINUE                                                         939
C    *                                                                    939
C    Geant POSITION : SAPC ===> X=0. Y=0. Z=(-SFJP_SAPP_DZ+SFJP_SAPP_DY)  940
C    /2. <===                                                             940
         CALL AGSRESET                                                    940
         AG_X = 0.                                                        940
         AG_Y = 0.                                                        940
         AG_Z = (-SFJP_SAPP_DZ+SFJP_SAPP_DY)/2.                           940
         AG_TITLE = 'SAPC'                                                940
         AG_EXNAME = 'POSITION'                                           940
         AG_PARLIST = 'X_Y_Z'                                             940
         CALL AXPOSITION                                                  940
C    *                                                                    940
C    Create SAPS :                                                        941
C    Call To Local Procedure SAPS                                         941
         ASSIGN 5960 TO BLOCK_SAPS                                        941
         GO TO  5950                                                      941
5960     CONTINUE                                                         941
C    *                                                                    941
C    Geant POSITION : SAPS ===> X=SFJP_SAPP_DXE/2.+SFJP_SAPP_DXZ/2 Y=0 Z  942
C    =0. <===                                                             942
         CALL AGSRESET                                                    942
         AG_X = SFJP_SAPP_DXE/2.+SFJP_SAPP_DXZ/2                          942
         AG_Y = 0                                                         942
         AG_Z = 0.                                                        942
         AG_TITLE = 'SAPS'                                                942
         AG_EXNAME = 'POSITION'                                           942
         AG_PARLIST = 'X_Y_Z'                                             942
         CALL AXPOSITION                                                  942
C    *                                                                    942
C    Geant POSITION : SAPS ===> X=-(SFJP_SAPP_DXE/2.+SFJP_SAPP_DXZ/2) Y=  942
C    0 Z=0. <===                                                          942
         CALL AGSRESET                                                    943
         AG_X = -(SFJP_SAPP_DXE/2.+SFJP_SAPP_DXZ/2)                       943
         AG_Y = 0                                                         943
         AG_Z = 0.                                                        943
         AG_TITLE = 'SAPS'                                                943
         AG_EXNAME = 'POSITION'                                           943
         AG_PARLIST = 'X_Y_Z'                                             943
         CALL AXPOSITION                                                  943
C    *                                                                    943
C    Create SAPT :                                                        944
C    Call To Local Procedure SAPT                                         944
         ASSIGN 5980 TO BLOCK_SAPT                                        944
         GO TO  5970                                                      944
5980     CONTINUE                                                         944
C    *                                                                    944
C    Geant POSITION : SAPT ===> X=0. Y=(SFJP_SAPP_DY+SFJP_SAPP_DXZ)/2. Z  945
C    =0. <===                                                             945
         CALL AGSRESET                                                    945
         AG_X = 0.                                                        945
         AG_Y = (SFJP_SAPP_DY+SFJP_SAPP_DXZ)/2.                           945
         AG_Z = 0.                                                        945
         AG_TITLE = 'SAPT'                                                945
         AG_EXNAME = 'POSITION'                                           945
         AG_PARLIST = 'X_Y_Z'                                             945
         CALL AXPOSITION                                                  945
C    *                                                                    945
C    Geant POSITION : SAPT ===> X=0. Y=-(SFJP_SAPP_DY+SFJP_SAPP_DXZ)/2.   945
C    Z=0. <===                                                            945
         CALL AGSRESET                                                    946
         AG_X = 0.                                                        946
         AG_Y = -(SFJP_SAPP_DY+SFJP_SAPP_DXZ)/2.                          946
         AG_Z = 0.                                                        946
         AG_TITLE = 'SAPT'                                                946
         AG_EXNAME = 'POSITION'                                           946
         AG_PARLIST = 'X_Y_Z'                                             946
         CALL AXPOSITION                                                  946
C    *                                                                    946
5561  CALL AGSPOPB                                                        947
      GO TO BLOCK_SAPP, (5560 ,5570)                                      947
C End of Local Procedure                                                  947
C ----------------------------------------------------------------------  948
C ------------ SAPC                                                       948
C Begin Local Procedure SAPC                                              949
C SAPC IS THE CORE (EPOXY) OF THE ADC BOARD APPENDICE                     949
5930  CONTINUE                                                            949
         AG_TITLE='SAPC'                                                  949
         AG_MODULE=MODULE                                                 949
         AG_MARK='V00'                                                    949
         CALL AGSPUSH                                                     949
         AG_MODULE=MODULE                                                 949
C    *                                                                    949
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           950
         CALL AGSRESET                                                    950
         AG_TITLE = 'CARBON'                                              950
         AG_EXNAME = 'MATERIAL'                                           950
         AG_PARLIST = 'NONE'                                              950
         CALL AXMATERIAL                                                  950
C    *                                                                    950
C    Geant ATTRIBUTE : SAPC ===> SEEN=1 COLO=1 <===                       950
         CALL AGSRESET                                                    951
         AG_SEEN = 1                                                      951
         AG_COLO = 1                                                      951
         AG_TITLE = 'SAPC'                                                951
         AG_EXNAME = 'ATTRIBUTE'                                          951
         AG_PARLIST = 'SEEN_COLO'                                         951
         CALL AXATTRIBUTE                                                 951
C    *                                                                    951
C    Geant SHAPE : BOX ===> DX=SFJP_SAPP_DXE/2. DY=SFJP_SAPP_DY/2., DZ=S  951
C    FJP_SAPP_DY/2. <===                                                  951
         CALL AGSRESET                                                    953
         AG_DX = SFJP_SAPP_DXE/2.                                         953
         AG_DY = SFJP_SAPP_DY/2.                                          953
         AG_DZ = SFJP_SAPP_DY/2.                                          953
         AG_TITLE = 'BOX'                                                 953
         AG_EXNAME = 'SHAPE'                                              953
         AG_PARLIST = 'DX_DY_DZ'                                          953
         CALL AXSHAPE                                                     953
C    *                                                                    953
5931  CALL AGSPOPB                                                        954
      GO TO BLOCK_SAPC, (5930 ,5940)                                      954
C End of Local Procedure                                                  954
C ----------------------------------------------------------------------  955
C ------------ SAPS                                                       955
C Begin Local Procedure SAPS                                              956
C SAPS IS THE SIDE SHELL (CARBON) OF THE ADC BOARD APPENDICE              956
5950  CONTINUE                                                            956
         AG_TITLE='SAPS'                                                  956
         AG_MODULE=MODULE                                                 956
         AG_MARK='V00'                                                    956
         CALL AGSPUSH                                                     956
         AG_MODULE=MODULE                                                 956
C    *                                                                    956
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           957
         CALL AGSRESET                                                    957
         AG_TITLE = 'CARBON'                                              957
         AG_EXNAME = 'MATERIAL'                                           957
         AG_PARLIST = 'NONE'                                              957
         CALL AXMATERIAL                                                  957
C    *                                                                    957
C    Geant ATTRIBUTE : SAPS ===> SEEN=1 COLO=1 <===                       957
         CALL AGSRESET                                                    958
         AG_SEEN = 1                                                      958
         AG_COLO = 1                                                      958
         AG_TITLE = 'SAPS'                                                958
         AG_EXNAME = 'ATTRIBUTE'                                          958
         AG_PARLIST = 'SEEN_COLO'                                         958
         CALL AXATTRIBUTE                                                 958
C    *                                                                    958
C    Geant SHAPE : BOX ===> DX=SFJP_SAPP_DXZ/2. DY=(SFJP_SAPP_DY+2*SFJP_  958
C    SAPP_DXZ)/2., DZ=SFJP_SAPP_DZ/2. <===                                958
         CALL AGSRESET                                                    960
         AG_DX = SFJP_SAPP_DXZ/2.                                         960
         AG_DY = (SFJP_SAPP_DY+2*SFJP_SAPP_DXZ)/2.                        960
         AG_DZ = SFJP_SAPP_DZ/2.                                          960
         AG_TITLE = 'BOX'                                                 960
         AG_EXNAME = 'SHAPE'                                              960
         AG_PARLIST = 'DX_DY_DZ'                                          960
         CALL AXSHAPE                                                     960
C    *                                                                    960
5951  CALL AGSPOPB                                                        961
      GO TO BLOCK_SAPS, (5950 ,5960)                                      961
C End of Local Procedure                                                  961
C ----------------------------------------------------------------------  962
C ------------ SAPT                                                       962
C Begin Local Procedure SAPT                                              963
C SAPT IS THE TOP-BOTTOM SHELL (CARBON) OF THE ADC BOARD APPENDICE        963
5970  CONTINUE                                                            963
         AG_TITLE='SAPT'                                                  963
         AG_MODULE=MODULE                                                 963
         AG_MARK='V00'                                                    963
         CALL AGSPUSH                                                     963
         AG_MODULE=MODULE                                                 963
C    *                                                                    963
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           964
         CALL AGSRESET                                                    964
         AG_TITLE = 'CARBON'                                              964
         AG_EXNAME = 'MATERIAL'                                           964
         AG_PARLIST = 'NONE'                                              964
         CALL AXMATERIAL                                                  964
C    *                                                                    964
C    Geant ATTRIBUTE : SAPT ===> SEEN=1 COLO=1 <===                       964
         CALL AGSRESET                                                    965
         AG_SEEN = 1                                                      965
         AG_COLO = 1                                                      965
         AG_TITLE = 'SAPT'                                                965
         AG_EXNAME = 'ATTRIBUTE'                                          965
         AG_PARLIST = 'SEEN_COLO'                                         965
         CALL AXATTRIBUTE                                                 965
C    *                                                                    965
C    Geant SHAPE : BOX ===> DX=SFJP_SAPP_DXE/2. DY=SFJP_SAPP_DXZ/2., DZ=  965
C    SFJP_SAPP_DZ/2. <===                                                 965
         CALL AGSRESET                                                    967
         AG_DX = SFJP_SAPP_DXE/2.                                         967
         AG_DY = SFJP_SAPP_DXZ/2.                                         967
         AG_DZ = SFJP_SAPP_DZ/2.                                          967
         AG_TITLE = 'BOX'                                                 967
         AG_EXNAME = 'SHAPE'                                              967
         AG_PARLIST = 'DX_DY_DZ'                                          967
         CALL AXSHAPE                                                     967
C    *                                                                    967
5971  CALL AGSPOPB                                                        968
      GO TO BLOCK_SAPT, (5970 ,5980)                                      968
C End of Local Procedure                                                  968
C ----------------------------------------------------------------------  969
C ------------ SFCO                                                       969
C Begin Local Procedure SFCO                                              970
C SFCO IS THE CONNECTION BOARD (RECTANGULAR WITH HIROSE CONNECTORS)       970
5600  CONTINUE                                                            970
         AG_TITLE='SFCO'                                                  970
         AG_MODULE=MODULE                                                 970
         AG_MARK='V00'                                                    970
         CALL AGSPUSH                                                     970
         AG_MODULE=MODULE                                                 970
C    *                                                                    970
C    Geant MATERIAL : G10 ===> inherited arguments used <===              971
         CALL AGSRESET                                                    971
         AG_TITLE = 'G10'                                                 971
         AG_EXNAME = 'MATERIAL'                                           971
         AG_PARLIST = 'NONE'                                              971
         CALL AXMATERIAL                                                  971
C    *                                                                    971
C    Geant ATTRIBUTE : SFCO ===> SEEN=1 COLO=30 <===                      971
         CALL AGSRESET                                                    972
         AG_SEEN = 1                                                      972
         AG_COLO = 30                                                     972
         AG_TITLE = 'SFCO'                                                972
         AG_EXNAME = 'ATTRIBUTE'                                          972
         AG_PARLIST = 'SEEN_COLO'                                         972
         CALL AXATTRIBUTE                                                 972
C    *                                                                    972
C    Geant SHAPE : BOX ===> DX=SFJP_SFCO_DX/2. DY=SFJP_SFCO_DY/2., DZ=SF  972
C    JP_SFCO_DZ/2 <===                                                    972
         CALL AGSRESET                                                    974
         AG_DX = SFJP_SFCO_DX/2.                                          974
         AG_DY = SFJP_SFCO_DY/2.                                          974
         AG_DZ = SFJP_SFCO_DZ/2                                           974
         AG_TITLE = 'BOX'                                                 974
         AG_EXNAME = 'SHAPE'                                              974
         AG_PARLIST = 'DX_DY_DZ'                                          974
         CALL AXSHAPE                                                     974
C    *                                                                    974
5601  CALL AGSPOPB                                                        975
      GO TO BLOCK_SFCO, (5600 ,5610)                                      975
C End of Local Procedure                                                  975
C ----------------------------------------------------------------------  976
C ------------ SFCM                                                       976
C Begin Local Procedure SFCM                                              977
C SFCM IS THE MOTHER VOLUME OF THE SECOND CONNECTION BOARD                977
5620  CONTINUE                                                            977
         AG_TITLE='SFCM'                                                  977
         AG_MODULE=MODULE                                                 977
         AG_MARK='V00'                                                    977
         CALL AGSPUSH                                                     977
         AG_MODULE=MODULE                                                 977
C    *                                                                    977
C    Geant MATERIAL : AIR ===> inherited arguments used <===              978
         CALL AGSRESET                                                    978
         AG_TITLE = 'AIR'                                                 978
         AG_EXNAME = 'MATERIAL'                                           978
         AG_PARLIST = 'NONE'                                              978
         CALL AXMATERIAL                                                  978
C    *                                                                    978
C    Geant ATTRIBUTE : SFCM ===> SEEN=0 COLO=6 <===                       978
         CALL AGSRESET                                                    979
         AG_SEEN = 0                                                      979
         AG_COLO = 6                                                      979
         AG_TITLE = 'SFCM'                                                979
         AG_EXNAME = 'ATTRIBUTE'                                          979
         AG_PARLIST = 'SEEN_COLO'                                         979
         CALL AXATTRIBUTE                                                 979
C    *                                                                    979
C    Geant SHAPE : BOX ===> DX=SFJP_SFCM_DX/2. DY=SFJP_SFCO_DY/2., DZ=SF  979
C    JP_SFCO_DZ/2 <===                                                    979
         CALL AGSRESET                                                    981
         AG_DX = SFJP_SFCM_DX/2.                                          981
         AG_DY = SFJP_SFCO_DY/2.                                          981
         AG_DZ = SFJP_SFCO_DZ/2                                           981
         AG_TITLE = 'BOX'                                                 981
         AG_EXNAME = 'SHAPE'                                              981
         AG_PARLIST = 'DX_DY_DZ'                                          981
         CALL AXSHAPE                                                     981
C    *                                                                    981
C    Create SFCB :                                                        982
C    Call To Local Procedure SFCB                                         982
         ASSIGN 6000 TO BLOCK_SFCB                                        982
         GO TO  5990                                                      982
6000     CONTINUE                                                         982
C    *                                                                    982
C    Geant POSITION : SFCB ===> X=+0.5 Y=0 Z=0 <===                       983
         CALL AGSRESET                                                    983
         AG_X = +0.5                                                      983
         AG_Y = 0                                                         983
         AG_Z = 0                                                         983
         AG_TITLE = 'SFCB'                                                983
         AG_EXNAME = 'POSITION'                                           983
         AG_PARLIST = 'X_Y_Z'                                             983
         CALL AXPOSITION                                                  983
C    *                                                                    983
C    Create SFCS :                                                        984
C    Call To Local Procedure SFCS                                         984
         ASSIGN 6020 TO BLOCK_SFCS                                        984
         GO TO  6010                                                      984
6020     CONTINUE                                                         984
C    *                                                                    984
C    Geant POSITION : SFCS ===> X=-1.5 Y=0 Z=-SFJP_SFCO_DZ/2+SFJP_SFCS_D  985
C    Z/2 <===                                                             985
         CALL AGSRESET                                                    985
         AG_X = -1.5                                                      985
         AG_Y = 0                                                         985
         AG_Z = -SFJP_SFCO_DZ/2+SFJP_SFCS_DZ/2                            985
         AG_TITLE = 'SFCS'                                                985
         AG_EXNAME = 'POSITION'                                           985
         AG_PARLIST = 'X_Y_Z'                                             985
         CALL AXPOSITION                                                  985
C    *                                                                    985
5621  CALL AGSPOPB                                                        986
      GO TO BLOCK_SFCM, (5620 ,5630)                                      986
C End of Local Procedure                                                  986
C ----------------------------------------------------------------------  987
C ------------ SFCB                                                       987
C Begin Local Procedure SFCB                                              988
C SFCB IS THE BIG PART OF THE SECOND CONNECTION BOARD                     988
5990  CONTINUE                                                            988
         AG_TITLE='SFCB'                                                  988
         AG_MODULE=MODULE                                                 988
         AG_MARK='V00'                                                    988
         CALL AGSPUSH                                                     988
         AG_MODULE=MODULE                                                 988
C    *                                                                    988
C    Geant MATERIAL : G10 ===> inherited arguments used <===              989
         CALL AGSRESET                                                    989
         AG_TITLE = 'G10'                                                 989
         AG_EXNAME = 'MATERIAL'                                           989
         AG_PARLIST = 'NONE'                                              989
         CALL AXMATERIAL                                                  989
C    *                                                                    989
C    Geant ATTRIBUTE : SFCB ===> SEEN=1 COLO=30 <===                      989
         CALL AGSRESET                                                    990
         AG_SEEN = 1                                                      990
         AG_COLO = 30                                                     990
         AG_TITLE = 'SFCB'                                                990
         AG_EXNAME = 'ATTRIBUTE'                                          990
         AG_PARLIST = 'SEEN_COLO'                                         990
         CALL AXATTRIBUTE                                                 990
C    *                                                                    990
C    Geant SHAPE : BOX ===> DX=SFJP_SFCB_DX/2. DY=SFJP_SFCO_DY/2., DZ=SF  990
C    JP_SFCO_DZ/2 <===                                                    990
         CALL AGSRESET                                                    992
         AG_DX = SFJP_SFCB_DX/2.                                          992
         AG_DY = SFJP_SFCO_DY/2.                                          992
         AG_DZ = SFJP_SFCO_DZ/2                                           992
         AG_TITLE = 'BOX'                                                 992
         AG_EXNAME = 'SHAPE'                                              992
         AG_PARLIST = 'DX_DY_DZ'                                          992
         CALL AXSHAPE                                                     992
C    *                                                                    992
5991  CALL AGSPOPB                                                        993
      GO TO BLOCK_SFCB, (5990 ,6000)                                      993
C End of Local Procedure                                                  993
C ----------------------------------------------------------------------  994
C ------------ SFCS                                                       994
C Begin Local Procedure SFCS                                              995
C SFCS IS THE BIG PART OF THE SECOND CONNECTION BOARD                     995
6010  CONTINUE                                                            995
         AG_TITLE='SFCS'                                                  995
         AG_MODULE=MODULE                                                 995
         AG_MARK='V00'                                                    995
         CALL AGSPUSH                                                     995
         AG_MODULE=MODULE                                                 995
C    *                                                                    995
C    Geant MATERIAL : G10 ===> inherited arguments used <===              996
         CALL AGSRESET                                                    996
         AG_TITLE = 'G10'                                                 996
         AG_EXNAME = 'MATERIAL'                                           996
         AG_PARLIST = 'NONE'                                              996
         CALL AXMATERIAL                                                  996
C    *                                                                    996
C    Geant ATTRIBUTE : SFCS ===> SEEN=1 COLO=30 <===                      996
         CALL AGSRESET                                                    997
         AG_SEEN = 1                                                      997
         AG_COLO = 30                                                     997
         AG_TITLE = 'SFCS'                                                997
         AG_EXNAME = 'ATTRIBUTE'                                          997
         AG_PARLIST = 'SEEN_COLO'                                         997
         CALL AXATTRIBUTE                                                 997
C    *                                                                    997
C    Geant SHAPE : BOX ===> DX=(SFJP_SFCM_DX-SFJP_SFCB_DX)/2., DY=SFJP_S  997
C    FCO_DY/2. DZ=SFJP_SFCS_DZ/2 <===                                     997
         CALL AGSRESET                                                    999
         AG_DX = (SFJP_SFCM_DX-SFJP_SFCB_DX)/2.                           999
         AG_DY = SFJP_SFCO_DY/2.                                          999
         AG_DZ = SFJP_SFCS_DZ/2                                           999
         AG_TITLE = 'BOX'                                                 999
         AG_EXNAME = 'SHAPE'                                              999
         AG_PARLIST = 'DX_DY_DZ'                                          999
         CALL AXSHAPE                                                     999
C    *                                                                    999
6011  CALL AGSPOPB                                                        1000
      GO TO BLOCK_SFCS, (6010 ,6020)                                      1000
C End of Local Procedure                                                  1000
C ----------------------------------------------------------------------  1001
C ------------ SFKF                                                       1001
C Begin Local Procedure SFKF                                              1002
C SFKF IS THE FIRST PART OF THE KAPTON FLEX CIRCUIT                       1002
5640  CONTINUE                                                            1002
         AG_TITLE='SFKF'                                                  1002
         AG_MODULE=MODULE                                                 1002
         AG_MARK='V00'                                                    1002
         CALL AGSPUSH                                                     1002
         AG_MODULE=MODULE                                                 1002
C    *                                                                    1002
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1003
         CALL AGSRESET                                                    1003
         AG_TITLE = 'G10'                                                 1003
         AG_EXNAME = 'MATERIAL'                                           1003
         AG_PARLIST = 'NONE'                                              1003
         CALL AXMATERIAL                                                  1003
C    *                                                                    1003
C    Geant ATTRIBUTE : SFKF ===> SEEN=1 COLO=30 <===                      1003
         CALL AGSRESET                                                    1004
         AG_SEEN = 1                                                      1004
         AG_COLO = 30                                                     1004
         AG_TITLE = 'SFKF'                                                1004
         AG_EXNAME = 'ATTRIBUTE'                                          1004
         AG_PARLIST = 'SEEN_COLO'                                         1004
         CALL AXATTRIBUTE                                                 1004
C    *                                                                    1004
C    Geant SHAPE : BOX ===> DX=SFJP_SFKF_DX/2. DY=SFJP_SFKF_DY/2., DZ=SF  1004
C    JP_SFKF_DZ/2 <===                                                    1004
         CALL AGSRESET                                                    1006
         AG_DX = SFJP_SFKF_DX/2.                                          1006
         AG_DY = SFJP_SFKF_DY/2.                                          1006
         AG_DZ = SFJP_SFKF_DZ/2                                           1006
         AG_TITLE = 'BOX'                                                 1006
         AG_EXNAME = 'SHAPE'                                              1006
         AG_PARLIST = 'DX_DY_DZ'                                          1006
         CALL AXSHAPE                                                     1006
C    *                                                                    1006
5641  CALL AGSPOPB                                                        1007
      GO TO BLOCK_SFKF, (5640 ,5650)                                      1007
C End of Local Procedure                                                  1007
C ----------------------------------------------------------------------  1008
C ------------ SFKS                                                       1008
C Begin Local Procedure SFKS                                              1009
C SFKS IS THE SECOND PART OF THE KAPTON FLEX CIRCUIT                      1009
5660  CONTINUE                                                            1009
         AG_TITLE='SFKS'                                                  1009
         AG_MODULE=MODULE                                                 1009
         AG_MARK='V00'                                                    1009
         CALL AGSPUSH                                                     1009
         AG_MODULE=MODULE                                                 1009
C    *                                                                    1009
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1010
         CALL AGSRESET                                                    1010
         AG_TITLE = 'G10'                                                 1010
         AG_EXNAME = 'MATERIAL'                                           1010
         AG_PARLIST = 'NONE'                                              1010
         CALL AXMATERIAL                                                  1010
C    *                                                                    1010
C    Geant ATTRIBUTE : SFKS ===> SEEN=1 COLO=30 <===                      1010
         CALL AGSRESET                                                    1011
         AG_SEEN = 1                                                      1011
         AG_COLO = 30                                                     1011
         AG_TITLE = 'SFKS'                                                1011
         AG_EXNAME = 'ATTRIBUTE'                                          1011
         AG_PARLIST = 'SEEN_COLO'                                         1011
         CALL AXATTRIBUTE                                                 1011
C    *                                                                    1011
C    Geant SHAPE : BOX ===> DX=SFJP_SFKS_DX/2. DY=SFJP_SFKF_DY/2., DZ=SF  1011
C    JP_SFKF_DZ/2 <===                                                    1011
         CALL AGSRESET                                                    1013
         AG_DX = SFJP_SFKS_DX/2.                                          1013
         AG_DY = SFJP_SFKF_DY/2.                                          1013
         AG_DZ = SFJP_SFKF_DZ/2                                           1013
         AG_TITLE = 'BOX'                                                 1013
         AG_EXNAME = 'SHAPE'                                              1013
         AG_PARLIST = 'DX_DY_DZ'                                          1013
         CALL AXSHAPE                                                     1013
C    *                                                                    1013
5661  CALL AGSPOPB                                                        1014
      GO TO BLOCK_SFKS, (5660 ,5670)                                      1014
C End of Local Procedure                                                  1014
C ----------------------------------------------------------------------  1015
C ------------ SFPR                                                       1015
C Begin Local Procedure SFPR                                              1017
C SFPR IS THE LADDER END INSIDE MECHANICAL PART (PRISM WITH G10 WITH, HA  1017
C LF DENSITY)                                                             1017
5680  CONTINUE                                                            1017
         AG_TITLE='SFPR'                                                  1017
         AG_MODULE=MODULE                                                 1017
         AG_MARK='V00'                                                    1017
         CALL AGSPUSH                                                     1017
         AG_MODULE=MODULE                                                 1017
C    *                                                                    1017
C    Geant MATERIAL : G5 ===> inherited arguments used <===               1018
         CALL AGSRESET                                                    1018
         AG_TITLE = 'G5'                                                  1018
         AG_EXNAME = 'MATERIAL'                                           1018
         AG_PARLIST = 'NONE'                                              1018
         CALL AXMATERIAL                                                  1018
C    *                                                                    1018
C    Geant ATTRIBUTE : SFPR ===> SEEN=1 COLO=4 <===                       1018
         CALL AGSRESET                                                    1019
         AG_SEEN = 1                                                      1019
         AG_COLO = 4                                                      1019
         AG_TITLE = 'SFPR'                                                1019
         AG_EXNAME = 'ATTRIBUTE'                                          1019
         AG_PARLIST = 'SEEN_COLO'                                         1019
         CALL AXATTRIBUTE                                                 1019
C    *                                                                    1019
C    Geant SHAPE : PGON ===> PHI1=-126 DPHI=72 NPDIV=1 NZ=2, ZI={0,3.7}   1019
C    RMN={0,0} RMX={3.48,3.48} <===                                       1019
         CALL AGSRESET                                                    1021
         AG_PHI1 = -126                                                   1021
         AG_DPHI = 72                                                     1021
         AG_NPDIV = 1                                                     1021
         AG_NZ = 2                                                        1021
         AG_ZI(1)=0                                                       1021
         AG_ZI(2)=3.7                                                     1021
         AG_RMN(1)=0                                                      1021
         AG_RMN(2)=0                                                      1021
         AG_RMX(1)=3.48                                                   1021
         AG_RMX(2)=3.48                                                   1021
         AG_TITLE = 'PGON'                                                1021
         AG_EXNAME = 'SHAPE'                                              1021
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1021
         CALL AXSHAPE                                                     1021
C    *                                                                    1021
5681  CALL AGSPOPB                                                        1022
      GO TO BLOCK_SFPR, (5680 ,5690)                                      1022
C End of Local Procedure                                                  1022
C ----------------------------------------------------------------------  1023
C ------------ SFPB                                                       1023
C Begin Local Procedure SFPB                                              1024
C SFPB IS THE LADDER END OUTSIDE MECHANICAL PART (RECTANGLE WITH G10)     1024
5700  CONTINUE                                                            1024
         AG_TITLE='SFPB'                                                  1024
         AG_MODULE=MODULE                                                 1024
         AG_MARK='V00'                                                    1024
         CALL AGSPUSH                                                     1024
         AG_MODULE=MODULE                                                 1024
C    *                                                                    1024
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1025
         CALL AGSRESET                                                    1025
         AG_TITLE = 'G10'                                                 1025
         AG_EXNAME = 'MATERIAL'                                           1025
         AG_PARLIST = 'NONE'                                              1025
         CALL AXMATERIAL                                                  1025
C    *                                                                    1025
C    Geant ATTRIBUTE : SFPB ===> SEEN=1 COLO=4 <===                       1025
         CALL AGSRESET                                                    1026
         AG_SEEN = 1                                                      1026
         AG_COLO = 4                                                      1026
         AG_TITLE = 'SFPB'                                                1026
         AG_EXNAME = 'ATTRIBUTE'                                          1026
         AG_PARLIST = 'SEEN_COLO'                                         1026
         CALL AXATTRIBUTE                                                 1026
C    *                                                                    1026
C    Geant SHAPE : BOX ===> DX=SFJP_SFPB_DX/2. DY=SFJP_SFPB_DY/2., DZ=SF  1026
C    JP_SFPBDZ/2. <===                                                    1026
         CALL AGSRESET                                                    1028
         AG_DX = SFJP_SFPB_DX/2.                                          1028
         AG_DY = SFJP_SFPB_DY/2.                                          1028
         AG_DZ = SFJP_SFPBDZ/2.                                           1028
         AG_TITLE = 'BOX'                                                 1028
         AG_EXNAME = 'SHAPE'                                              1028
         AG_PARLIST = 'DX_DY_DZ'                                          1028
         CALL AXSHAPE                                                     1028
C    *                                                                    1028
5701  CALL AGSPOPB                                                        1029
      GO TO BLOCK_SFPB, (5700 ,5710)                                      1029
C End of Local Procedure                                                  1029
C ----------------------------------------------------------------------  1030
C ------------ SSBS                                                       1030
C Begin Local Procedure SSBS                                              1032
C SSBS IS THE SMALL PART OF THE ALUMINIUM PLATE LINKING THE LADDER TO, T  1032
C HE SECTOR                                                               1032
5720  CONTINUE                                                            1032
         AG_TITLE='SSBS'                                                  1032
         AG_MODULE=MODULE                                                 1032
         AG_MARK='V00'                                                    1032
         CALL AGSPUSH                                                     1032
         AG_MODULE=MODULE                                                 1032
C    *                                                                    1032
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1033
         CALL AGSRESET                                                    1033
         AG_TITLE = 'ALUMINIUM'                                           1033
         AG_EXNAME = 'MATERIAL'                                           1033
         AG_PARLIST = 'NONE'                                              1033
         CALL AXMATERIAL                                                  1033
C    *                                                                    1033
C    Geant ATTRIBUTE : SSBS ===> SEEN=1 COLO=1 <===                       1033
         CALL AGSRESET                                                    1034
         AG_SEEN = 1                                                      1034
         AG_COLO = 1                                                      1034
         AG_TITLE = 'SSBS'                                                1034
         AG_EXNAME = 'ATTRIBUTE'                                          1034
         AG_PARLIST = 'SEEN_COLO'                                         1034
         CALL AXATTRIBUTE                                                 1034
C    *                                                                    1034
C    Geant SHAPE : BOX ===> DX=SFJP_SSBS_DX/2. DY=SFJP_SSBS_DY/2., DZ=SF  1034
C    JP_SSBS_DX/2. <===                                                   1034
         CALL AGSRESET                                                    1036
         AG_DX = SFJP_SSBS_DX/2.                                          1036
         AG_DY = SFJP_SSBS_DY/2.                                          1036
         AG_DZ = SFJP_SSBS_DX/2.                                          1036
         AG_TITLE = 'BOX'                                                 1036
         AG_EXNAME = 'SHAPE'                                              1036
         AG_PARLIST = 'DX_DY_DZ'                                          1036
         CALL AXSHAPE                                                     1036
C    *                                                                    1036
5721  CALL AGSPOPB                                                        1037
      GO TO BLOCK_SSBS, (5720 ,5730)                                      1037
C End of Local Procedure                                                  1037
C ----------------------------------------------------------------------  1038
C ------------ SSBB                                                       1038
C Begin Local Procedure SSBB                                              1040
C SSBB IS THE BIG PART OF THE ALUMINIUM PLATE LINKING THE LADDER TO THE,  1040
C  SECTOR                                                                 1040
5740  CONTINUE                                                            1040
         AG_TITLE='SSBB'                                                  1040
         AG_MODULE=MODULE                                                 1040
         AG_MARK='V00'                                                    1040
         CALL AGSPUSH                                                     1040
         AG_MODULE=MODULE                                                 1040
C    *                                                                    1040
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1041
         CALL AGSRESET                                                    1041
         AG_TITLE = 'ALUMINIUM'                                           1041
         AG_EXNAME = 'MATERIAL'                                           1041
         AG_PARLIST = 'NONE'                                              1041
         CALL AXMATERIAL                                                  1041
C    *                                                                    1041
C    Geant ATTRIBUTE : SSBB ===> SEEN=1 COLO=1 <===                       1041
         CALL AGSRESET                                                    1042
         AG_SEEN = 1                                                      1042
         AG_COLO = 1                                                      1042
         AG_TITLE = 'SSBB'                                                1042
         AG_EXNAME = 'ATTRIBUTE'                                          1042
         AG_PARLIST = 'SEEN_COLO'                                         1042
         CALL AXATTRIBUTE                                                 1042
C    *                                                                    1042
C    Geant SHAPE : BOX ===> DX=SFJP_SSBB_DX/2. DY=SFJP_SSBS_DY/2., DZ=SF  1042
C    JP_SSBB_DZ/2. <===                                                   1042
         CALL AGSRESET                                                    1044
         AG_DX = SFJP_SSBB_DX/2.                                          1044
         AG_DY = SFJP_SSBS_DY/2.                                          1044
         AG_DZ = SFJP_SSBB_DZ/2.                                          1044
         AG_TITLE = 'BOX'                                                 1044
         AG_EXNAME = 'SHAPE'                                              1044
         AG_PARLIST = 'DX_DY_DZ'                                          1044
         CALL AXSHAPE                                                     1044
C    *                                                                    1044
5741  CALL AGSPOPB                                                        1045
      GO TO BLOCK_SSBB, (5740 ,5750)                                      1045
C End of Local Procedure                                                  1045
C ----------------------------------------------------------------------  1046
C ------------ SSST                                                       1046
C Begin Local Procedure SSST                                              1047
C SSST IS THE TOP OF THE SMALL SECTOR                                     1047
5070  CONTINUE                                                            1047
         AG_TITLE='SSST'                                                  1047
         AG_MODULE=MODULE                                                 1047
         AG_MARK='V00'                                                    1047
         CALL AGSPUSH                                                     1047
         AG_MODULE=MODULE                                                 1047
C    *                                                                    1047
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1048
         CALL AGSRESET                                                    1048
         AG_TITLE = 'ALUMINIUM'                                           1048
         AG_EXNAME = 'MATERIAL'                                           1048
         AG_PARLIST = 'NONE'                                              1048
         CALL AXMATERIAL                                                  1048
C    *                                                                    1048
C    Geant ATTRIBUTE : SSST ===> SEEN=1 COLO=1 <===                       1048
         CALL AGSRESET                                                    1049
         AG_SEEN = 1                                                      1049
         AG_COLO = 1                                                      1049
         AG_TITLE = 'SSST'                                                1049
         AG_EXNAME = 'ATTRIBUTE'                                          1049
         AG_PARLIST = 'SEEN_COLO'                                         1049
         CALL AXATTRIBUTE                                                 1049
C    *                                                                    1049
C    Geant SHAPE : TUBS ===> RMIN=SFJP_SSST_RMIN RMAX=SFJP_SSST_RMAX, PH  1049
C    I1=SFJP_ALPHAZH PHI2=(180.-SFJP_ALPHAZH), DZ=SFJP_SSST_WIDTH/2. <==  1049
C    =                                                                    1049
         CALL AGSRESET                                                    1052
         AG_RMIN = SFJP_SSST_RMIN                                         1052
         AG_RMAX = SFJP_SSST_RMAX                                         1052
         AG_PHI1 = SFJP_ALPHAZH                                           1052
         AG_PHI2 = (180.-SFJP_ALPHAZH)                                    1052
         AG_DZ = SFJP_SSST_WIDTH/2.                                       1052
         AG_TITLE = 'TUBS'                                                1052
         AG_EXNAME = 'SHAPE'                                              1052
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1052
         CALL AXSHAPE                                                     1052
C    *                                                                    1052
5071  CALL AGSPOPB                                                        1053
      GO TO BLOCK_SSST, (5070 ,5080)                                      1053
C End of Local Procedure                                                  1053
C ----------------------------------------------------------------------  1054
C ------------ SSSS                                                       1054
C Begin Local Procedure SSSS                                              1055
C SSSS IS THE SIDE OF THE SMALL SECTOR                                    1055
5090  CONTINUE                                                            1055
         AG_TITLE='SSSS'                                                  1055
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
C    Geant ATTRIBUTE : SSSS ===> SEEN=1 COLO=1 <===                       1056
         CALL AGSRESET                                                    1057
         AG_SEEN = 1                                                      1057
         AG_COLO = 1                                                      1057
         AG_TITLE = 'SSSS'                                                1057
         AG_EXNAME = 'ATTRIBUTE'                                          1057
         AG_PARLIST = 'SEEN_COLO'                                         1057
         CALL AXATTRIBUTE                                                 1057
C    *                                                                    1057
C    Geant SHAPE : TUBS ===> RMIN=SFJP_SSSS_RMIN RMAX=SFJP_SSST_RMIN, PH  1057
C    I1=SFJP_ALPHAZH PHI2=(180.-SFJP_ALPHAZH), DZ=SFJP_SSSS_WIDTH/2. <==  1057
C    =                                                                    1057
         CALL AGSRESET                                                    1060
         AG_RMIN = SFJP_SSSS_RMIN                                         1060
         AG_RMAX = SFJP_SSST_RMIN                                         1060
         AG_PHI1 = SFJP_ALPHAZH                                           1060
         AG_PHI2 = (180.-SFJP_ALPHAZH)                                    1060
         AG_DZ = SFJP_SSSS_WIDTH/2.                                       1060
         AG_TITLE = 'TUBS'                                                1060
         AG_EXNAME = 'SHAPE'                                              1060
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1060
         CALL AXSHAPE                                                     1060
C    *                                                                    1060
5091  CALL AGSPOPB                                                        1061
      GO TO BLOCK_SSSS, (5090 ,5100)                                      1061
C End of Local Procedure                                                  1061
C ----------------------------------------------------------------------  1062
C ------------ SSRT                                                       1062
C Begin Local Procedure SSRT                                              1063
C SSRT IS THE TOP OF THE SIDE RIB                                         1063
5110  CONTINUE                                                            1063
         AG_TITLE='SSRT'                                                  1063
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
C    Geant ATTRIBUTE : SSRT ===> SEEN=1 COLO=1 <===                       1064
         CALL AGSRESET                                                    1065
         AG_SEEN = 1                                                      1065
         AG_COLO = 1                                                      1065
         AG_TITLE = 'SSRT'                                                1065
         AG_EXNAME = 'ATTRIBUTE'                                          1065
         AG_PARLIST = 'SEEN_COLO'                                         1065
         CALL AXATTRIBUTE                                                 1065
C    *                                                                    1065
C    Geant SHAPE : TUBS ===> RMIN=SFJP_SSST_RMIN RMAX=SFJP_SSST_RMAX, PH  1065
C    I1=-SFJP_ALPHAZH PHI2=+SFJP_ALPHAZH, DZ=SFJP_SSST_WIDTH/2. <===      1065
         CALL AGSRESET                                                    1068
         AG_RMIN = SFJP_SSST_RMIN                                         1068
         AG_RMAX = SFJP_SSST_RMAX                                         1068
         AG_PHI1 = -SFJP_ALPHAZH                                          1068
         AG_PHI2 = +SFJP_ALPHAZH                                          1068
         AG_DZ = SFJP_SSST_WIDTH/2.                                       1068
         AG_TITLE = 'TUBS'                                                1068
         AG_EXNAME = 'SHAPE'                                              1068
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1068
         CALL AXSHAPE                                                     1068
C    *                                                                    1068
5111  CALL AGSPOPB                                                        1069
      GO TO BLOCK_SSRT, (5110 ,5120)                                      1069
C End of Local Procedure                                                  1069
C ----------------------------------------------------------------------  1070
C ------------ SSRS                                                       1070
C Begin Local Procedure SSRS                                              1071
C SSRS IS THE SIDE OF THE SMALL RIB                                       1071
5130  CONTINUE                                                            1071
         AG_TITLE='SSRS'                                                  1071
         AG_MODULE=MODULE                                                 1071
         AG_MARK='V00'                                                    1071
         CALL AGSPUSH                                                     1071
         AG_MODULE=MODULE                                                 1071
C    *                                                                    1071
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1072
         CALL AGSRESET                                                    1072
         AG_TITLE = 'ALUMINIUM'                                           1072
         AG_EXNAME = 'MATERIAL'                                           1072
         AG_PARLIST = 'NONE'                                              1072
         CALL AXMATERIAL                                                  1072
C    *                                                                    1072
C    Geant ATTRIBUTE : SSRS ===> SEEN=1 COLO=1 <===                       1072
         CALL AGSRESET                                                    1073
         AG_SEEN = 1                                                      1073
         AG_COLO = 1                                                      1073
         AG_TITLE = 'SSRS'                                                1073
         AG_EXNAME = 'ATTRIBUTE'                                          1073
         AG_PARLIST = 'SEEN_COLO'                                         1073
         CALL AXATTRIBUTE                                                 1073
C    *                                                                    1073
C    Geant SHAPE : TUBS ===> RMIN=SFJP_SSRS_RMIN RMAX=SFJP_SSST_RMIN, PH  1073
C    I1=-SFJP_ALPHAZH PHI2=+SFJP_ALPHAZH, DZ=SFJP_SSSS_WIDTH/2. <===      1073
         CALL AGSRESET                                                    1076
         AG_RMIN = SFJP_SSRS_RMIN                                         1076
         AG_RMAX = SFJP_SSST_RMIN                                         1076
         AG_PHI1 = -SFJP_ALPHAZH                                          1076
         AG_PHI2 = +SFJP_ALPHAZH                                          1076
         AG_DZ = SFJP_SSSS_WIDTH/2.                                       1076
         AG_TITLE = 'TUBS'                                                1076
         AG_EXNAME = 'SHAPE'                                              1076
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1076
         CALL AXSHAPE                                                     1076
C    *                                                                    1076
5131  CALL AGSPOPB                                                        1077
      GO TO BLOCK_SSRS, (5130 ,5140)                                      1077
C End of Local Procedure                                                  1077
C ----------------------------------------------------------------------  1078
C ------------ SSLB                                                       1078
C Begin Local Procedure SSLB                                              1079
C SSLB IS THE LINKING (SECTOR TO THE CONE) BOX                            1079
5150  CONTINUE                                                            1079
         AG_TITLE='SSLB'                                                  1079
         AG_MODULE=MODULE                                                 1079
         AG_MARK='V00'                                                    1079
         CALL AGSPUSH                                                     1079
         AG_MODULE=MODULE                                                 1079
C    *                                                                    1079
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1080
         CALL AGSRESET                                                    1080
         AG_TITLE = 'AIR'                                                 1080
         AG_EXNAME = 'MATERIAL'                                           1080
         AG_PARLIST = 'NONE'                                              1080
         CALL AXMATERIAL                                                  1080
C    *                                                                    1080
C    Geant ATTRIBUTE : SSLB ===> SEEN=0 COLO=6 <===                       1080
         CALL AGSRESET                                                    1081
         AG_SEEN = 0                                                      1081
         AG_COLO = 6                                                      1081
         AG_TITLE = 'SSLB'                                                1081
         AG_EXNAME = 'ATTRIBUTE'                                          1081
         AG_PARLIST = 'SEEN_COLO'                                         1081
         CALL AXATTRIBUTE                                                 1081
C    *                                                                    1081
C    Geant SHAPE : BOX ===> DX=SFJP_SSLB_DX/2. DY=SFJP_SSLB_DY/2., DZ=SF  1081
C    JP_SSLB_DZ/2. <===                                                   1081
         CALL AGSRESET                                                    1083
         AG_DX = SFJP_SSLB_DX/2.                                          1083
         AG_DY = SFJP_SSLB_DY/2.                                          1083
         AG_DZ = SFJP_SSLB_DZ/2.                                          1083
         AG_TITLE = 'BOX'                                                 1083
         AG_EXNAME = 'SHAPE'                                              1083
         AG_PARLIST = 'DX_DY_DZ'                                          1083
         CALL AXSHAPE                                                     1083
C    *                                                                    1083
C    Create SBCH :                                                        1084
C    Call To Local Procedure SBCH                                         1084
         ASSIGN 6040 TO BLOCK_SBCH                                        1084
         GO TO  6030                                                      1084
6040     CONTINUE                                                         1084
C    *                                                                    1084
C    Geant POSITION : SBCH ===> X=0 Y=0 Z=SFJP_SSLB_DZ/2.-SFJP_SSCR_THD/  1085
C    2. <===                                                              1085
         CALL AGSRESET                                                    1085
         AG_X = 0                                                         1085
         AG_Y = 0                                                         1085
         AG_Z = SFJP_SSLB_DZ/2.-SFJP_SSCR_THD/2.                          1085
         AG_TITLE = 'SBCH'                                                1085
         AG_EXNAME = 'POSITION'                                           1085
         AG_PARLIST = 'X_Y_Z'                                             1085
         CALL AXPOSITION                                                  1085
C    *                                                                    1085
C    Create SBCV :                                                        1086
C    Call To Local Procedure SBCV                                         1086
         ASSIGN 6060 TO BLOCK_SBCV                                        1086
         GO TO  6050                                                      1086
6060     CONTINUE                                                         1086
C    *                                                                    1086
C    Geant POSITION : SBCV ===> X=0 Y=(SFJP_SSCR_WD+SFJP_SSCR_VL)/2 Z=SF  1088
C    JP_SSLB_DZ/2.-SFJP_SSCR_THD/2. <===                                  1088
         CALL AGSRESET                                                    1088
         AG_X = 0                                                         1088
         AG_Y = (SFJP_SSCR_WD+SFJP_SSCR_VL)/2                             1088
         AG_Z = SFJP_SSLB_DZ/2.-SFJP_SSCR_THD/2.                          1088
         AG_TITLE = 'SBCV'                                                1088
         AG_EXNAME = 'POSITION'                                           1088
         AG_PARLIST = 'X_Y_Z'                                             1088
         CALL AXPOSITION                                                  1088
C    *                                                                    1088
C    Geant POSITION : SBCV ===> X=0 Y=-(SFJP_SSCR_WD+SFJP_SSCR_VL)/2 Z=S  1088
C    FJP_SSLB_DZ/2.-SFJP_SSCR_THD/2. <===                                 1088
         CALL AGSRESET                                                    1090
         AG_X = 0                                                         1090
         AG_Y = -(SFJP_SSCR_WD+SFJP_SSCR_VL)/2                            1090
         AG_Z = SFJP_SSLB_DZ/2.-SFJP_SSCR_THD/2.                          1090
         AG_TITLE = 'SBCV'                                                1090
         AG_EXNAME = 'POSITION'                                           1090
         AG_PARLIST = 'X_Y_Z'                                             1090
         CALL AXPOSITION                                                  1090
C    *                                                                    1090
C    Create SBFH :                                                        1091
C    Call To Local Procedure SBFH                                         1091
         ASSIGN 6080 TO BLOCK_SBFH                                        1091
         GO TO  6070                                                      1091
6080     CONTINUE                                                         1091
C    *                                                                    1091
C    Geant POSITION : SBFH ===> X=0 Y=0 Z=SFJP_SSLB_DZ/2.-SFJP_SSCR_THD-  1092
C    SFJP_SSCR_THA/2. <===                                                1092
         CALL AGSRESET                                                    1092
         AG_X = 0                                                         1092
         AG_Y = 0                                                         1092
         AG_Z = SFJP_SSLB_DZ/2.-SFJP_SSCR_THD-SFJP_SSCR_THA/2.            1092
         AG_TITLE = 'SBFH'                                                1092
         AG_EXNAME = 'POSITION'                                           1092
         AG_PARLIST = 'X_Y_Z'                                             1092
         CALL AXPOSITION                                                  1092
C    *                                                                    1092
C    Create SBFV :                                                        1093
C    Call To Local Procedure SBFV                                         1093
         ASSIGN 6100 TO BLOCK_SBFV                                        1093
         GO TO  6090                                                      1093
6100     CONTINUE                                                         1093
C    *                                                                    1093
C    Geant POSITION : SBFV ===> X=0 Y=(SFJP_SSCR_WD+SFJP_SSCR_VL)/2 Z=SF  1095
C    JP_SSLB_DZ/2.-SFJP_SSCR_THD-SFJP_SSCR_THA/2. <===                    1095
         CALL AGSRESET                                                    1095
         AG_X = 0                                                         1095
         AG_Y = (SFJP_SSCR_WD+SFJP_SSCR_VL)/2                             1095
         AG_Z = SFJP_SSLB_DZ/2.-SFJP_SSCR_THD-SFJP_SSCR_THA/2.            1095
         AG_TITLE = 'SBFV'                                                1095
         AG_EXNAME = 'POSITION'                                           1095
         AG_PARLIST = 'X_Y_Z'                                             1095
         CALL AXPOSITION                                                  1095
C    *                                                                    1095
C    Geant POSITION : SBFV ===> X=0 Y=-(SFJP_SSCR_WD+SFJP_SSCR_VL)/2 Z=S  1095
C    FJP_SSLB_DZ/2.-SFJP_SSCR_THD-SFJP_SSCR_THA/2. <===                   1095
         CALL AGSRESET                                                    1097
         AG_X = 0                                                         1097
         AG_Y = -(SFJP_SSCR_WD+SFJP_SSCR_VL)/2                            1097
         AG_Z = SFJP_SSLB_DZ/2.-SFJP_SSCR_THD-SFJP_SSCR_THA/2.            1097
         AG_TITLE = 'SBFV'                                                1097
         AG_EXNAME = 'POSITION'                                           1097
         AG_PARLIST = 'X_Y_Z'                                             1097
         CALL AXPOSITION                                                  1097
C    *                                                                    1097
5151  CALL AGSPOPB                                                        1098
      GO TO BLOCK_SSLB, (5150 ,5160)                                      1098
C End of Local Procedure                                                  1098
C ----------------------------------------------------------------------  1099
C ------------ SBCH                                                       1099
C Begin Local Procedure SBCH                                              1100
C SBCH IS THE HORIZONTAL BRANCH OF THE DELRIN CROSS                       1100
6030  CONTINUE                                                            1100
         AG_TITLE='SBCH'                                                  1100
         AG_MODULE=MODULE                                                 1100
         AG_MARK='V00'                                                    1100
         CALL AGSPUSH                                                     1100
         AG_MODULE=MODULE                                                 1100
C    *                                                                    1100
C    Geant MATERIAL : DELRIN ===> inherited arguments used <===           1101
         CALL AGSRESET                                                    1101
         AG_TITLE = 'DELRIN'                                              1101
         AG_EXNAME = 'MATERIAL'                                           1101
         AG_PARLIST = 'NONE'                                              1101
         CALL AXMATERIAL                                                  1101
C    *                                                                    1101
C    Geant ATTRIBUTE : SBCH ===> SEEN=1 COLO=2 <===                       1101
         CALL AGSRESET                                                    1102
         AG_SEEN = 1                                                      1102
         AG_COLO = 2                                                      1102
         AG_TITLE = 'SBCH'                                                1102
         AG_EXNAME = 'ATTRIBUTE'                                          1102
         AG_PARLIST = 'SEEN_COLO'                                         1102
         CALL AXATTRIBUTE                                                 1102
C    *                                                                    1102
C    Geant SHAPE : BOX ===> DX=SFJP_SSCR_HL/2. DY=SFJP_SSCR_WD/2., DZ=SF  1102
C    JP_SSCR_THD/2. <===                                                  1102
         CALL AGSRESET                                                    1104
         AG_DX = SFJP_SSCR_HL/2.                                          1104
         AG_DY = SFJP_SSCR_WD/2.                                          1104
         AG_DZ = SFJP_SSCR_THD/2.                                         1104
         AG_TITLE = 'BOX'                                                 1104
         AG_EXNAME = 'SHAPE'                                              1104
         AG_PARLIST = 'DX_DY_DZ'                                          1104
         CALL AXSHAPE                                                     1104
C    *                                                                    1104
6031  CALL AGSPOPB                                                        1105
      GO TO BLOCK_SBCH, (6030 ,6040)                                      1105
C End of Local Procedure                                                  1105
C ----------------------------------------------------------------------  1106
C ------------ SBFH                                                       1106
C Begin Local Procedure SBFH                                              1107
C SBFH IS THE HORIZONTAL BRANCH OF THE ALUMIMIUM CROSS                    1107
6070  CONTINUE                                                            1107
         AG_TITLE='SBFH'                                                  1107
         AG_MODULE=MODULE                                                 1107
         AG_MARK='V00'                                                    1107
         CALL AGSPUSH                                                     1107
         AG_MODULE=MODULE                                                 1107
C    *                                                                    1107
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1108
         CALL AGSRESET                                                    1108
         AG_TITLE = 'ALUMINIUM'                                           1108
         AG_EXNAME = 'MATERIAL'                                           1108
         AG_PARLIST = 'NONE'                                              1108
         CALL AXMATERIAL                                                  1108
C    *                                                                    1108
C    Geant ATTRIBUTE : SBFH ===> SEEN=1 COLO=1 <===                       1108
         CALL AGSRESET                                                    1109
         AG_SEEN = 1                                                      1109
         AG_COLO = 1                                                      1109
         AG_TITLE = 'SBFH'                                                1109
         AG_EXNAME = 'ATTRIBUTE'                                          1109
         AG_PARLIST = 'SEEN_COLO'                                         1109
         CALL AXATTRIBUTE                                                 1109
C    *                                                                    1109
C    Geant SHAPE : BOX ===> DX=SFJP_SSCR_HL/2. DY=SFJP_SSCR_WD/2., DZ=SF  1109
C    JP_SSCR_THA/2. <===                                                  1109
         CALL AGSRESET                                                    1111
         AG_DX = SFJP_SSCR_HL/2.                                          1111
         AG_DY = SFJP_SSCR_WD/2.                                          1111
         AG_DZ = SFJP_SSCR_THA/2.                                         1111
         AG_TITLE = 'BOX'                                                 1111
         AG_EXNAME = 'SHAPE'                                              1111
         AG_PARLIST = 'DX_DY_DZ'                                          1111
         CALL AXSHAPE                                                     1111
C    *                                                                    1111
6071  CALL AGSPOPB                                                        1112
      GO TO BLOCK_SBFH, (6070 ,6080)                                      1112
C End of Local Procedure                                                  1112
C ----------------------------------------------------------------------  1113
C ------------ SBCV                                                       1113
C Begin Local Procedure SBCV                                              1114
C SBCV IS THE VERTICAL BRANCH OF THE DELRIN CROSS                         1114
6050  CONTINUE                                                            1114
         AG_TITLE='SBCV'                                                  1114
         AG_MODULE=MODULE                                                 1114
         AG_MARK='V00'                                                    1114
         CALL AGSPUSH                                                     1114
         AG_MODULE=MODULE                                                 1114
C    *                                                                    1114
C    Geant MATERIAL : DELRIN ===> inherited arguments used <===           1115
         CALL AGSRESET                                                    1115
         AG_TITLE = 'DELRIN'                                              1115
         AG_EXNAME = 'MATERIAL'                                           1115
         AG_PARLIST = 'NONE'                                              1115
         CALL AXMATERIAL                                                  1115
C    *                                                                    1115
C    Geant ATTRIBUTE : SBCV ===> SEEN=1 COLO=2 <===                       1115
         CALL AGSRESET                                                    1116
         AG_SEEN = 1                                                      1116
         AG_COLO = 2                                                      1116
         AG_TITLE = 'SBCV'                                                1116
         AG_EXNAME = 'ATTRIBUTE'                                          1116
         AG_PARLIST = 'SEEN_COLO'                                         1116
         CALL AXATTRIBUTE                                                 1116
C    *                                                                    1116
C    Geant SHAPE : BOX ===> DX=SFJP_SSCR_WD/2. DY=SFJP_SSCR_VL/2., DZ=SF  1116
C    JP_SSCR_THD/2. <===                                                  1116
         CALL AGSRESET                                                    1118
         AG_DX = SFJP_SSCR_WD/2.                                          1118
         AG_DY = SFJP_SSCR_VL/2.                                          1118
         AG_DZ = SFJP_SSCR_THD/2.                                         1118
         AG_TITLE = 'BOX'                                                 1118
         AG_EXNAME = 'SHAPE'                                              1118
         AG_PARLIST = 'DX_DY_DZ'                                          1118
         CALL AXSHAPE                                                     1118
C    *                                                                    1118
6051  CALL AGSPOPB                                                        1119
      GO TO BLOCK_SBCV, (6050 ,6060)                                      1119
C End of Local Procedure                                                  1119
C ----------------------------------------------------------------------  1120
C ------------ SBFV                                                       1120
C Begin Local Procedure SBFV                                              1121
C SBFV IS THE VERTICAL BRANCH OF THE ALUMIMIUM CROSS                      1121
6090  CONTINUE                                                            1121
         AG_TITLE='SBFV'                                                  1121
         AG_MODULE=MODULE                                                 1121
         AG_MARK='V00'                                                    1121
         CALL AGSPUSH                                                     1121
         AG_MODULE=MODULE                                                 1121
C    *                                                                    1121
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1122
         CALL AGSRESET                                                    1122
         AG_TITLE = 'ALUMINIUM'                                           1122
         AG_EXNAME = 'MATERIAL'                                           1122
         AG_PARLIST = 'NONE'                                              1122
         CALL AXMATERIAL                                                  1122
C    *                                                                    1122
C    Geant ATTRIBUTE : SBFV ===> SEEN=1 COLO=1 <===                       1122
         CALL AGSRESET                                                    1123
         AG_SEEN = 1                                                      1123
         AG_COLO = 1                                                      1123
         AG_TITLE = 'SBFV'                                                1123
         AG_EXNAME = 'ATTRIBUTE'                                          1123
         AG_PARLIST = 'SEEN_COLO'                                         1123
         CALL AXATTRIBUTE                                                 1123
C    *                                                                    1123
C    Geant SHAPE : BOX ===> DX=SFJP_SSCR_WD/2. DY=SFJP_SSCR_VL/2., DZ=SF  1123
C    JP_SSCR_THA/2. <===                                                  1123
         CALL AGSRESET                                                    1125
         AG_DX = SFJP_SSCR_WD/2.                                          1125
         AG_DY = SFJP_SSCR_VL/2.                                          1125
         AG_DZ = SFJP_SSCR_THA/2.                                         1125
         AG_TITLE = 'BOX'                                                 1125
         AG_EXNAME = 'SHAPE'                                              1125
         AG_PARLIST = 'DX_DY_DZ'                                          1125
         CALL AXSHAPE                                                     1125
C    *                                                                    1125
6091  CALL AGSPOPB                                                        1126
      GO TO BLOCK_SBFV, (6090 ,6100)                                      1126
C End of Local Procedure                                                  1126
C ----------------------------------------------------------------------  1127
C ------------ SSLT                                                       1127
C Begin Local Procedure SSLT                                              1128
C SSLT IS THE LINKING (SECTOR TO THE CONE) TUBE                           1128
5170  CONTINUE                                                            1128
         AG_TITLE='SSLT'                                                  1128
         AG_MODULE=MODULE                                                 1128
         AG_MARK='V00'                                                    1128
         CALL AGSPUSH                                                     1128
         AG_MODULE=MODULE                                                 1128
C    *                                                                    1128
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1129
         CALL AGSRESET                                                    1129
         AG_TITLE = 'ALUMINIUM'                                           1129
         AG_EXNAME = 'MATERIAL'                                           1129
         AG_PARLIST = 'NONE'                                              1129
         CALL AXMATERIAL                                                  1129
C    *                                                                    1129
C    Geant ATTRIBUTE : SSLT ===> SEEN=1 COLO=1 <===                       1129
         CALL AGSRESET                                                    1130
         AG_SEEN = 1                                                      1130
         AG_COLO = 1                                                      1130
         AG_TITLE = 'SSLT'                                                1130
         AG_EXNAME = 'ATTRIBUTE'                                          1130
         AG_PARLIST = 'SEEN_COLO'                                         1130
         CALL AXATTRIBUTE                                                 1130
C    *                                                                    1130
C    Geant SHAPE : TUBE ===> RMIN=0. RMAX=SFJP_SSLT_RMAX DZ=SFJP_SSLT_DZ  1130
C    /2. <===                                                             1130
         CALL AGSRESET                                                    1131
         AG_RMIN = 0.                                                     1131
         AG_RMAX = SFJP_SSLT_RMAX                                         1131
         AG_DZ = SFJP_SSLT_DZ/2.                                          1131
         AG_TITLE = 'TUBE'                                                1131
         AG_EXNAME = 'SHAPE'                                              1131
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1131
         CALL AXSHAPE                                                     1131
C    *                                                                    1131
5171  CALL AGSPOPB                                                        1132
      GO TO BLOCK_SSLT, (5170 ,5180)                                      1132
C End of Local Procedure                                                  1132
C ----------------------------------------------------------------------  1133
C ------------ SCMP                                                       1133
C Begin Local Procedure SCMP                                              1134
C SCMP IS THE MOUNTING PLATE INSERTED IN THE CONES.                       1134
5190  CONTINUE                                                            1134
         AG_TITLE='SCMP'                                                  1134
         AG_MODULE=MODULE                                                 1134
         AG_MARK='V00'                                                    1134
         CALL AGSPUSH                                                     1134
         AG_MODULE=MODULE                                                 1134
C    *                                                                    1134
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1135
         CALL AGSRESET                                                    1135
         AG_TITLE = 'ALUMINIUM'                                           1135
         AG_EXNAME = 'MATERIAL'                                           1135
         AG_PARLIST = 'NONE'                                              1135
         CALL AXMATERIAL                                                  1135
C    *                                                                    1135
C    Geant ATTRIBUTE : SCMP ===> SEEN=1 COLO=1 <===                       1135
         CALL AGSRESET                                                    1136
         AG_SEEN = 1                                                      1136
         AG_COLO = 1                                                      1136
         AG_TITLE = 'SCMP'                                                1136
         AG_EXNAME = 'ATTRIBUTE'                                          1136
         AG_PARLIST = 'SEEN_COLO'                                         1136
         CALL AXATTRIBUTE                                                 1136
C    *                                                                    1136
C    Geant SHAPE : BOX ===> DX=SFJP_SCMP_DX/2. DY=SFJP_SCMP_DY/2., DZ=SF  1136
C    JP_SCMP_DZ/2 <===                                                    1136
         CALL AGSRESET                                                    1138
         AG_DX = SFJP_SCMP_DX/2.                                          1138
         AG_DY = SFJP_SCMP_DY/2.                                          1138
         AG_DZ = SFJP_SCMP_DZ/2                                           1138
         AG_TITLE = 'BOX'                                                 1138
         AG_EXNAME = 'SHAPE'                                              1138
         AG_PARLIST = 'DX_DY_DZ'                                          1138
         CALL AXSHAPE                                                     1138
C    *                                                                    1138
5191  CALL AGSPOPB                                                        1139
      GO TO BLOCK_SCMP, (5190 ,5200)                                      1139
C End of Local Procedure                                                  1139
C ----------------------------------------------------------------------  1140
C ------------ SCVM                                                       1140
C Begin Local Procedure SCVM                                              1141
C SCVM IS THE MOTHER VOLUME OF THE V-SHAPE PIECE                          1141
5210  CONTINUE                                                            1141
         AG_TITLE='SCVM'                                                  1141
         AG_MODULE=MODULE                                                 1141
         AG_MARK='V00'                                                    1141
         CALL AGSPUSH                                                     1141
         AG_MODULE=MODULE                                                 1141
C    *                                                                    1141
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1142
         CALL AGSRESET                                                    1142
         AG_TITLE = 'AIR'                                                 1142
         AG_EXNAME = 'MATERIAL'                                           1142
         AG_PARLIST = 'NONE'                                              1142
         CALL AXMATERIAL                                                  1142
C    *                                                                    1142
C    Geant ATTRIBUTE : SCVM ===> SEEN=0 COLO=6 <===                       1142
         CALL AGSRESET                                                    1143
         AG_SEEN = 0                                                      1143
         AG_COLO = 6                                                      1143
         AG_TITLE = 'SCVM'                                                1143
         AG_EXNAME = 'ATTRIBUTE'                                          1143
         AG_PARLIST = 'SEEN_COLO'                                         1143
         CALL AXATTRIBUTE                                                 1143
C    *                                                                    1143
C    Geant SHAPE : BOX ===> DX=SFJP_SCVM_DX/2. DY=SFJP_SCVM_DY/2., DZ=SF  1143
C    JP_SCVM_DZ/2 <===                                                    1143
         CALL AGSRESET                                                    1145
         AG_DX = SFJP_SCVM_DX/2.                                          1145
         AG_DY = SFJP_SCVM_DY/2.                                          1145
         AG_DZ = SFJP_SCVM_DZ/2                                           1145
         AG_TITLE = 'BOX'                                                 1145
         AG_EXNAME = 'SHAPE'                                              1145
         AG_PARLIST = 'DX_DY_DZ'                                          1145
         CALL AXSHAPE                                                     1145
C    *                                                                    1145
C    Create SCVB :                                                        1146
C    Call To Local Procedure SCVB                                         1146
         ASSIGN 6120 TO BLOCK_SCVB                                        1146
         GO TO  6110                                                      1146
6120     CONTINUE                                                         1146
C    *                                                                    1146
C    Geant POSITION : SCVB ===> X=0. Y=-SFJP_SCVM_DY/2.+SFJP_SCVB_DY/2.   1147
C    Z=0. <===                                                            1147
         CALL AGSRESET                                                    1147
         AG_X = 0.                                                        1147
         AG_Y = -SFJP_SCVM_DY/2.+SFJP_SCVB_DY/2.                          1147
         AG_Z = 0.                                                        1147
         AG_TITLE = 'SCVB'                                                1147
         AG_EXNAME = 'POSITION'                                           1147
         AG_PARLIST = 'X_Y_Z'                                             1147
         CALL AXPOSITION                                                  1147
C    *                                                                    1147
C    Create SCVS :                                                        1148
C    Call To Local Procedure SCVS                                         1148
         ASSIGN 6140 TO BLOCK_SCVS                                        1148
         GO TO  6130                                                      1148
6140     CONTINUE                                                         1148
C    *                                                                    1148
C    Geant POSITION : SCVS ===> X=SFJP_SCVS_PX Y=SFJP_SCVS_PY Z=0.0 ALPH  1149
C    AZ=SFJP_ALPHAZH <===                                                 1149
         CALL AGSRESET                                                    1149
         AG_X = SFJP_SCVS_PX                                              1149
         AG_Y = SFJP_SCVS_PY                                              1149
         AG_Z = 0.0                                                       1149
         AG_ALPHAN=AG_ALPHAN+1                                            1149
         AG_ALPHAK(AG_ALPHAN)=3                                           1149
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                1149
         AG_TITLE = 'SCVS'                                                1149
         AG_EXNAME = 'POSITION'                                           1149
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      1149
         CALL AXPOSITION                                                  1149
C    *                                                                    1149
C    Geant POSITION : SCVS ===> X=-SFJP_SCVS_PX Y=SFJP_SCVS_PY Z=0.0 ALP  1149
C    HAZ=-SFJP_ALPHAZH <===                                               1149
         CALL AGSRESET                                                    1150
         AG_X = -SFJP_SCVS_PX                                             1150
         AG_Y = SFJP_SCVS_PY                                              1150
         AG_Z = 0.0                                                       1150
         AG_ALPHAN=AG_ALPHAN+1                                            1150
         AG_ALPHAK(AG_ALPHAN)=3                                           1150
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               1150
         AG_TITLE = 'SCVS'                                                1150
         AG_EXNAME = 'POSITION'                                           1150
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      1150
         CALL AXPOSITION                                                  1150
C    *                                                                    1150
5211  CALL AGSPOPB                                                        1151
      GO TO BLOCK_SCVM, (5210 ,5220)                                      1151
C End of Local Procedure                                                  1151
C ----------------------------------------------------------------------  1152
C ------------ SCVB                                                       1152
C Begin Local Procedure SCVB                                              1153
C SCVB IS THE BASE PLATE OF THE V-SHAPE PIECE                             1153
6110  CONTINUE                                                            1153
         AG_TITLE='SCVB'                                                  1153
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
C    Geant ATTRIBUTE : SCVB ===> SEEN=1 COLO=1 <===                       1154
         CALL AGSRESET                                                    1155
         AG_SEEN = 1                                                      1155
         AG_COLO = 1                                                      1155
         AG_TITLE = 'SCVB'                                                1155
         AG_EXNAME = 'ATTRIBUTE'                                          1155
         AG_PARLIST = 'SEEN_COLO'                                         1155
         CALL AXATTRIBUTE                                                 1155
C    *                                                                    1155
C    Geant SHAPE : BOX ===> DX=SFJP_SCVM_DX/2. DY=SFJP_SCVB_DY/2., DZ=SF  1155
C    JP_SCVM_DZ/2 <===                                                    1155
         CALL AGSRESET                                                    1157
         AG_DX = SFJP_SCVM_DX/2.                                          1157
         AG_DY = SFJP_SCVB_DY/2.                                          1157
         AG_DZ = SFJP_SCVM_DZ/2                                           1157
         AG_TITLE = 'BOX'                                                 1157
         AG_EXNAME = 'SHAPE'                                              1157
         AG_PARLIST = 'DX_DY_DZ'                                          1157
         CALL AXSHAPE                                                     1157
C    *                                                                    1157
6111  CALL AGSPOPB                                                        1158
      GO TO BLOCK_SCVB, (6110 ,6120)                                      1158
C End of Local Procedure                                                  1158
C ----------------------------------------------------------------------  1159
C ------------ SCVS                                                       1159
C Begin Local Procedure SCVS                                              1160
C SCVS IS THE SIDE PLATE OF THE V-SHAPE PIECE                             1160
6130  CONTINUE                                                            1160
         AG_TITLE='SCVS'                                                  1160
         AG_MODULE=MODULE                                                 1160
         AG_MARK='V00'                                                    1160
         CALL AGSPUSH                                                     1160
         AG_MODULE=MODULE                                                 1160
C    *                                                                    1160
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1161
         CALL AGSRESET                                                    1161
         AG_TITLE = 'ALUMINIUM'                                           1161
         AG_EXNAME = 'MATERIAL'                                           1161
         AG_PARLIST = 'NONE'                                              1161
         CALL AXMATERIAL                                                  1161
C    *                                                                    1161
C    Geant ATTRIBUTE : SCVS ===> SEEN=1 COLO=1 <===                       1161
         CALL AGSRESET                                                    1162
         AG_SEEN = 1                                                      1162
         AG_COLO = 1                                                      1162
         AG_TITLE = 'SCVS'                                                1162
         AG_EXNAME = 'ATTRIBUTE'                                          1162
         AG_PARLIST = 'SEEN_COLO'                                         1162
         CALL AXATTRIBUTE                                                 1162
C    *                                                                    1162
C    Geant SHAPE : BOX ===> DX=SFJP_SCVS_DX/2. DY=SFJP_SCVS_DY/2., DZ=SF  1162
C    JP_SCVM_DZ/2 <===                                                    1162
         CALL AGSRESET                                                    1164
         AG_DX = SFJP_SCVS_DX/2.                                          1164
         AG_DY = SFJP_SCVS_DY/2.                                          1164
         AG_DZ = SFJP_SCVM_DZ/2                                           1164
         AG_TITLE = 'BOX'                                                 1164
         AG_EXNAME = 'SHAPE'                                              1164
         AG_PARLIST = 'DX_DY_DZ'                                          1164
         CALL AXSHAPE                                                     1164
C    *                                                                    1164
6131  CALL AGSPOPB                                                        1165
      GO TO BLOCK_SCVS, (6130 ,6140)                                      1165
C End of Local Procedure                                                  1165
      WRITE(LOUT,6150)'===>>  [End AgML Module SISDGEO4]  <<==='          1165
6150  FORMAT(A32,/,/)                                                     1165
C MODULE SISDGEO4                                                         1167
      END                                                                 1167
