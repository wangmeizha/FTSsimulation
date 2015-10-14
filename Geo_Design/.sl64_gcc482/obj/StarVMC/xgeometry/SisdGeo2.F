C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE SISDGEO2                                                 1
C SISDGEO2 IS THE SILICON STRIP DETECTOR                                  1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'SISDGEO2'/, BANK_TITLE/'*'/                            1
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
C AUTHOR LILIAN MARTIN, JOERG REINNARTH, MAXIM POTEKHIN                   1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'LILIAN MARTIN, JOERG REINNARTH, MAXIM POTEKH  2
     *IN'/                                                                2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C CREATED 27 OCT 04                                                       2
      CHARACTER*40 CREATED /'27 OCT 04'/                                  3
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
C -                                                                       26
      REAL SFJP_ALPHAZ                                                    26
      EQUIVALENCE (BANK_SFJP(3),SFJP_ALPHAZ,RE_SFJP_ALPHAZ),              26
     *(NAMES_SFJP(1),NSFJP_ALPHAZ)                                        26
      EQUIVALENCE (COMMEN_SFJP(1),CSFJP_ALPHAZ),(MAP_SFJP(1,1),           26
     *MSFJP_ALPHAZ)                                                       26
      CHARACTER CSFJP_ALPHAZ*40/' '/                                      26
      CHARACTER NSFJP_ALPHAZ*12/'R.ALPHAZ'/                               26
      INTEGER MSFJP_ALPHAZ(2)/1,1/                                        26
      REAL RE_SFJP_ALPHAZ                                                 26
C -                                                                       26
      REAL SFJP_ALPHAZH                                                   26
      EQUIVALENCE (BANK_SFJP(4),SFJP_ALPHAZH,RE_SFJP_ALPHAZH),            26
     *(NAMES_SFJP(2),NSFJP_ALPHAZH)                                       26
      EQUIVALENCE (COMMEN_SFJP(2),CSFJP_ALPHAZH),(MAP_SFJP(1,2),          26
     *MSFJP_ALPHAZH)                                                      26
      CHARACTER CSFJP_ALPHAZH*40/' '/                                     26
      CHARACTER NSFJP_ALPHAZH*12/'R.ALPHAZH'/                             26
      INTEGER MSFJP_ALPHAZH(2)/1,1/                                       26
      REAL RE_SFJP_ALPHAZH                                                26
C -                                                                       26
      REAL SFJP_SSST_RMIN                                                 26
      EQUIVALENCE (BANK_SFJP(5),SFJP_SSST_RMIN,RE_SFJP_SSST_RMIN),        26
     *(NAMES_SFJP(3),NSFJP_SSST_RMIN)                                     26
      EQUIVALENCE (COMMEN_SFJP(3),CSFJP_SSST_RMIN),(MAP_SFJP(1,3),        26
     *MSFJP_SSST_RMIN)                                                    26
      CHARACTER CSFJP_SSST_RMIN*40/' '/                                   26
      CHARACTER NSFJP_SSST_RMIN*12/'R.SSST_RMIN'/                         26
      INTEGER MSFJP_SSST_RMIN(2)/1,1/                                     26
      REAL RE_SFJP_SSST_RMIN                                              26
C -                                                                       26
      REAL SFJP_SSST_RMAX                                                 26
      EQUIVALENCE (BANK_SFJP(6),SFJP_SSST_RMAX,RE_SFJP_SSST_RMAX),        26
     *(NAMES_SFJP(4),NSFJP_SSST_RMAX)                                     26
      EQUIVALENCE (COMMEN_SFJP(4),CSFJP_SSST_RMAX),(MAP_SFJP(1,4),        26
     *MSFJP_SSST_RMAX)                                                    26
      CHARACTER CSFJP_SSST_RMAX*40/' '/                                   26
      CHARACTER NSFJP_SSST_RMAX*12/'R.SSST_RMAX'/                         26
      INTEGER MSFJP_SSST_RMAX(2)/1,1/                                     26
      REAL RE_SFJP_SSST_RMAX                                              26
C -                                                                       26
      REAL SFJP_SSST_WIDTH                                                26
      EQUIVALENCE (BANK_SFJP(7),SFJP_SSST_WIDTH,RE_SFJP_SSST_WIDTH),      26
     *(NAMES_SFJP(5),NSFJP_SSST_WIDTH)                                    26
      EQUIVALENCE (COMMEN_SFJP(5),CSFJP_SSST_WIDTH),(MAP_SFJP(1,5),       26
     *MSFJP_SSST_WIDTH)                                                   26
      CHARACTER CSFJP_SSST_WIDTH*40/' '/                                  26
      CHARACTER NSFJP_SSST_WIDTH*12/'R.SSST_WIDTH'/                       26
      INTEGER MSFJP_SSST_WIDTH(2)/1,1/                                    26
      REAL RE_SFJP_SSST_WIDTH                                             26
C -                                                                       26
      REAL SFJP_SSST_PZ                                                   26
      EQUIVALENCE (BANK_SFJP(8),SFJP_SSST_PZ,RE_SFJP_SSST_PZ),            26
     *(NAMES_SFJP(6),NSFJP_SSST_PZ)                                       26
      EQUIVALENCE (COMMEN_SFJP(6),CSFJP_SSST_PZ),(MAP_SFJP(1,6),          26
     *MSFJP_SSST_PZ)                                                      26
      CHARACTER CSFJP_SSST_PZ*40/' '/                                     26
      CHARACTER NSFJP_SSST_PZ*12/'R.SSST_PZ'/                             26
      INTEGER MSFJP_SSST_PZ(2)/1,1/                                       26
      REAL RE_SFJP_SSST_PZ                                                26
C -                                                                       26
      REAL SFJP_SSSS_RMIN                                                 26
      EQUIVALENCE (BANK_SFJP(9),SFJP_SSSS_RMIN,RE_SFJP_SSSS_RMIN),        26
     *(NAMES_SFJP(7),NSFJP_SSSS_RMIN)                                     26
      EQUIVALENCE (COMMEN_SFJP(7),CSFJP_SSSS_RMIN),(MAP_SFJP(1,7),        26
     *MSFJP_SSSS_RMIN)                                                    26
      CHARACTER CSFJP_SSSS_RMIN*40/' '/                                   26
      CHARACTER NSFJP_SSSS_RMIN*12/'R.SSSS_RMIN'/                         26
      INTEGER MSFJP_SSSS_RMIN(2)/1,1/                                     26
      REAL RE_SFJP_SSSS_RMIN                                              26
C -                                                                       26
      REAL SFJP_SSSS_WIDTH                                                26
      EQUIVALENCE (BANK_SFJP(10),SFJP_SSSS_WIDTH,RE_SFJP_SSSS_WIDTH),     26
     *(NAMES_SFJP(8),NSFJP_SSSS_WIDTH)                                    26
      EQUIVALENCE (COMMEN_SFJP(8),CSFJP_SSSS_WIDTH),(MAP_SFJP(1,8),       26
     *MSFJP_SSSS_WIDTH)                                                   26
      CHARACTER CSFJP_SSSS_WIDTH*40/' '/                                  26
      CHARACTER NSFJP_SSSS_WIDTH*12/'R.SSSS_WIDTH'/                       26
      INTEGER MSFJP_SSSS_WIDTH(2)/1,1/                                    26
      REAL RE_SFJP_SSSS_WIDTH                                             26
C -                                                                       26
      REAL SFJP_SSRS_RMIN                                                 26
      EQUIVALENCE (BANK_SFJP(11),SFJP_SSRS_RMIN,RE_SFJP_SSRS_RMIN),       26
     *(NAMES_SFJP(9),NSFJP_SSRS_RMIN)                                     26
      EQUIVALENCE (COMMEN_SFJP(9),CSFJP_SSRS_RMIN),(MAP_SFJP(1,9),        26
     *MSFJP_SSRS_RMIN)                                                    26
      CHARACTER CSFJP_SSRS_RMIN*40/' '/                                   26
      CHARACTER NSFJP_SSRS_RMIN*12/'R.SSRS_RMIN'/                         26
      INTEGER MSFJP_SSRS_RMIN(2)/1,1/                                     26
      REAL RE_SFJP_SSRS_RMIN                                              26
C -                                                                       26
      REAL SFJP_SSLB_DX                                                   26
      EQUIVALENCE (BANK_SFJP(12),SFJP_SSLB_DX,RE_SFJP_SSLB_DX),           26
     *(NAMES_SFJP(10),NSFJP_SSLB_DX)                                      26
      EQUIVALENCE (COMMEN_SFJP(10),CSFJP_SSLB_DX),(MAP_SFJP(1,10),        26
     *MSFJP_SSLB_DX)                                                      26
      CHARACTER CSFJP_SSLB_DX*40/' '/                                     26
      CHARACTER NSFJP_SSLB_DX*12/'R.SSLB_DX'/                             26
      INTEGER MSFJP_SSLB_DX(2)/1,1/                                       26
      REAL RE_SFJP_SSLB_DX                                                26
C -                                                                       26
      REAL SFJP_SSLB_PX                                                   26
      EQUIVALENCE (BANK_SFJP(13),SFJP_SSLB_PX,RE_SFJP_SSLB_PX),           26
     *(NAMES_SFJP(11),NSFJP_SSLB_PX)                                      26
      EQUIVALENCE (COMMEN_SFJP(11),CSFJP_SSLB_PX),(MAP_SFJP(1,11),        26
     *MSFJP_SSLB_PX)                                                      26
      CHARACTER CSFJP_SSLB_PX*40/' '/                                     26
      CHARACTER NSFJP_SSLB_PX*12/'R.SSLB_PX'/                             26
      INTEGER MSFJP_SSLB_PX(2)/1,1/                                       26
      REAL RE_SFJP_SSLB_PX                                                26
C -                                                                       26
      REAL SFJP_SSLB_DY                                                   26
      EQUIVALENCE (BANK_SFJP(14),SFJP_SSLB_DY,RE_SFJP_SSLB_DY),           26
     *(NAMES_SFJP(12),NSFJP_SSLB_DY)                                      26
      EQUIVALENCE (COMMEN_SFJP(12),CSFJP_SSLB_DY),(MAP_SFJP(1,12),        26
     *MSFJP_SSLB_DY)                                                      26
      CHARACTER CSFJP_SSLB_DY*40/' '/                                     26
      CHARACTER NSFJP_SSLB_DY*12/'R.SSLB_DY'/                             26
      INTEGER MSFJP_SSLB_DY(2)/1,1/                                       26
      REAL RE_SFJP_SSLB_DY                                                26
C -                                                                       26
      REAL SFJP_SSLB_DZ                                                   26
      EQUIVALENCE (BANK_SFJP(15),SFJP_SSLB_DZ,RE_SFJP_SSLB_DZ),           26
     *(NAMES_SFJP(13),NSFJP_SSLB_DZ)                                      26
      EQUIVALENCE (COMMEN_SFJP(13),CSFJP_SSLB_DZ),(MAP_SFJP(1,13),        26
     *MSFJP_SSLB_DZ)                                                      26
      CHARACTER CSFJP_SSLB_DZ*40/' '/                                     26
      CHARACTER NSFJP_SSLB_DZ*12/'R.SSLB_DZ'/                             26
      INTEGER MSFJP_SSLB_DZ(2)/1,1/                                       26
      REAL RE_SFJP_SSLB_DZ                                                26
C -                                                                       26
      REAL SFJP_SSLT_PX                                                   26
      EQUIVALENCE (BANK_SFJP(16),SFJP_SSLT_PX,RE_SFJP_SSLT_PX),           26
     *(NAMES_SFJP(14),NSFJP_SSLT_PX)                                      26
      EQUIVALENCE (COMMEN_SFJP(14),CSFJP_SSLT_PX),(MAP_SFJP(1,14),        26
     *MSFJP_SSLT_PX)                                                      26
      CHARACTER CSFJP_SSLT_PX*40/' '/                                     26
      CHARACTER NSFJP_SSLT_PX*12/'R.SSLT_PX'/                             26
      INTEGER MSFJP_SSLT_PX(2)/1,1/                                       26
      REAL RE_SFJP_SSLT_PX                                                26
C -                                                                       26
      REAL SFJP_SSLT_RMAX                                                 26
      EQUIVALENCE (BANK_SFJP(17),SFJP_SSLT_RMAX,RE_SFJP_SSLT_RMAX),       26
     *(NAMES_SFJP(15),NSFJP_SSLT_RMAX)                                    26
      EQUIVALENCE (COMMEN_SFJP(15),CSFJP_SSLT_RMAX),(MAP_SFJP(1,15),      26
     *MSFJP_SSLT_RMAX)                                                    26
      CHARACTER CSFJP_SSLT_RMAX*40/' '/                                   26
      CHARACTER NSFJP_SSLT_RMAX*12/'R.SSLT_RMAX'/                         26
      INTEGER MSFJP_SSLT_RMAX(2)/1,1/                                     26
      REAL RE_SFJP_SSLT_RMAX                                              26
C -                                                                       26
      REAL SFJP_SSLT_DZ                                                   26
      EQUIVALENCE (BANK_SFJP(18),SFJP_SSLT_DZ,RE_SFJP_SSLT_DZ),           26
     *(NAMES_SFJP(16),NSFJP_SSLT_DZ)                                      26
      EQUIVALENCE (COMMEN_SFJP(16),CSFJP_SSLT_DZ),(MAP_SFJP(1,16),        26
     *MSFJP_SSLT_DZ)                                                      26
      CHARACTER CSFJP_SSLT_DZ*40/' '/                                     26
      CHARACTER NSFJP_SSLT_DZ*12/'R.SSLT_DZ'/                             26
      INTEGER MSFJP_SSLT_DZ(2)/1,1/                                       26
      REAL RE_SFJP_SSLT_DZ                                                26
C -                                                                       26
      REAL SFJP_SCMP_DX                                                   26
      EQUIVALENCE (BANK_SFJP(19),SFJP_SCMP_DX,RE_SFJP_SCMP_DX),           26
     *(NAMES_SFJP(17),NSFJP_SCMP_DX)                                      26
      EQUIVALENCE (COMMEN_SFJP(17),CSFJP_SCMP_DX),(MAP_SFJP(1,17),        26
     *MSFJP_SCMP_DX)                                                      26
      CHARACTER CSFJP_SCMP_DX*40/' '/                                     26
      CHARACTER NSFJP_SCMP_DX*12/'R.SCMP_DX'/                             26
      INTEGER MSFJP_SCMP_DX(2)/1,1/                                       26
      REAL RE_SFJP_SCMP_DX                                                26
C -                                                                       26
      REAL SFJP_SCMP_DY                                                   26
      EQUIVALENCE (BANK_SFJP(20),SFJP_SCMP_DY,RE_SFJP_SCMP_DY),           26
     *(NAMES_SFJP(18),NSFJP_SCMP_DY)                                      26
      EQUIVALENCE (COMMEN_SFJP(18),CSFJP_SCMP_DY),(MAP_SFJP(1,18),        26
     *MSFJP_SCMP_DY)                                                      26
      CHARACTER CSFJP_SCMP_DY*40/' '/                                     26
      CHARACTER NSFJP_SCMP_DY*12/'R.SCMP_DY'/                             26
      INTEGER MSFJP_SCMP_DY(2)/1,1/                                       26
      REAL RE_SFJP_SCMP_DY                                                26
C -                                                                       26
      REAL SFJP_SCMP_DZ                                                   26
      EQUIVALENCE (BANK_SFJP(21),SFJP_SCMP_DZ,RE_SFJP_SCMP_DZ),           26
     *(NAMES_SFJP(19),NSFJP_SCMP_DZ)                                      26
      EQUIVALENCE (COMMEN_SFJP(19),CSFJP_SCMP_DZ),(MAP_SFJP(1,19),        26
     *MSFJP_SCMP_DZ)                                                      26
      CHARACTER CSFJP_SCMP_DZ*40/' '/                                     26
      CHARACTER NSFJP_SCMP_DZ*12/'R.SCMP_DZ'/                             26
      INTEGER MSFJP_SCMP_DZ(2)/1,1/                                       26
      REAL RE_SFJP_SCMP_DZ                                                26
C -                                                                       26
      REAL SFJP_SCMP_PX                                                   26
      EQUIVALENCE (BANK_SFJP(22),SFJP_SCMP_PX,RE_SFJP_SCMP_PX),           26
     *(NAMES_SFJP(20),NSFJP_SCMP_PX)                                      26
      EQUIVALENCE (COMMEN_SFJP(20),CSFJP_SCMP_PX),(MAP_SFJP(1,20),        26
     *MSFJP_SCMP_PX)                                                      26
      CHARACTER CSFJP_SCMP_PX*40/' '/                                     26
      CHARACTER NSFJP_SCMP_PX*12/'R.SCMP_PX'/                             26
      INTEGER MSFJP_SCMP_PX(2)/1,1/                                       26
      REAL RE_SFJP_SCMP_PX                                                26
C -                                                                       26
      REAL SFJP_SCVM_DZ                                                   26
      EQUIVALENCE (BANK_SFJP(23),SFJP_SCVM_DZ,RE_SFJP_SCVM_DZ),           26
     *(NAMES_SFJP(21),NSFJP_SCVM_DZ)                                      26
      EQUIVALENCE (COMMEN_SFJP(21),CSFJP_SCVM_DZ),(MAP_SFJP(1,21),        26
     *MSFJP_SCVM_DZ)                                                      26
      CHARACTER CSFJP_SCVM_DZ*40/' '/                                     26
      CHARACTER NSFJP_SCVM_DZ*12/'R.SCVM_DZ'/                             26
      INTEGER MSFJP_SCVM_DZ(2)/1,1/                                       26
      REAL RE_SFJP_SCVM_DZ                                                26
C -                                                                       26
      REAL SFJP_SCVM_DX                                                   26
      EQUIVALENCE (BANK_SFJP(24),SFJP_SCVM_DX,RE_SFJP_SCVM_DX),           26
     *(NAMES_SFJP(22),NSFJP_SCVM_DX)                                      26
      EQUIVALENCE (COMMEN_SFJP(22),CSFJP_SCVM_DX),(MAP_SFJP(1,22),        26
     *MSFJP_SCVM_DX)                                                      26
      CHARACTER CSFJP_SCVM_DX*40/' '/                                     26
      CHARACTER NSFJP_SCVM_DX*12/'R.SCVM_DX'/                             26
      INTEGER MSFJP_SCVM_DX(2)/1,1/                                       26
      REAL RE_SFJP_SCVM_DX                                                26
C -                                                                       26
      REAL SFJP_SCVM_PX                                                   26
      EQUIVALENCE (BANK_SFJP(25),SFJP_SCVM_PX,RE_SFJP_SCVM_PX),           26
     *(NAMES_SFJP(23),NSFJP_SCVM_PX)                                      26
      EQUIVALENCE (COMMEN_SFJP(23),CSFJP_SCVM_PX),(MAP_SFJP(1,23),        26
     *MSFJP_SCVM_PX)                                                      26
      CHARACTER CSFJP_SCVM_PX*40/' '/                                     26
      CHARACTER NSFJP_SCVM_PX*12/'R.SCVM_PX'/                             26
      INTEGER MSFJP_SCVM_PX(2)/1,1/                                       26
      REAL RE_SFJP_SCVM_PX                                                26
C -                                                                       26
      REAL SFJP_SCVM_DY                                                   26
      EQUIVALENCE (BANK_SFJP(26),SFJP_SCVM_DY,RE_SFJP_SCVM_DY),           26
     *(NAMES_SFJP(24),NSFJP_SCVM_DY)                                      26
      EQUIVALENCE (COMMEN_SFJP(24),CSFJP_SCVM_DY),(MAP_SFJP(1,24),        26
     *MSFJP_SCVM_DY)                                                      26
      CHARACTER CSFJP_SCVM_DY*40/' '/                                     26
      CHARACTER NSFJP_SCVM_DY*12/'R.SCVM_DY'/                             26
      INTEGER MSFJP_SCVM_DY(2)/1,1/                                       26
      REAL RE_SFJP_SCVM_DY                                                26
C -                                                                       26
      REAL SFJP_SCVM_PZ                                                   26
      EQUIVALENCE (BANK_SFJP(27),SFJP_SCVM_PZ,RE_SFJP_SCVM_PZ),           26
     *(NAMES_SFJP(25),NSFJP_SCVM_PZ)                                      26
      EQUIVALENCE (COMMEN_SFJP(25),CSFJP_SCVM_PZ),(MAP_SFJP(1,25),        26
     *MSFJP_SCVM_PZ)                                                      26
      CHARACTER CSFJP_SCVM_PZ*40/' '/                                     26
      CHARACTER NSFJP_SCVM_PZ*12/'R.SCVM_PZ'/                             26
      INTEGER MSFJP_SCVM_PZ(2)/1,1/                                       26
      REAL RE_SFJP_SCVM_PZ                                                26
C -                                                                       26
      REAL SFJP_SCVB_DY                                                   26
      EQUIVALENCE (BANK_SFJP(28),SFJP_SCVB_DY,RE_SFJP_SCVB_DY),           26
     *(NAMES_SFJP(26),NSFJP_SCVB_DY)                                      26
      EQUIVALENCE (COMMEN_SFJP(26),CSFJP_SCVB_DY),(MAP_SFJP(1,26),        26
     *MSFJP_SCVB_DY)                                                      26
      CHARACTER CSFJP_SCVB_DY*40/' '/                                     26
      CHARACTER NSFJP_SCVB_DY*12/'R.SCVB_DY'/                             26
      INTEGER MSFJP_SCVB_DY(2)/1,1/                                       26
      REAL RE_SFJP_SCVB_DY                                                26
C -                                                                       26
      REAL SFJP_SCVS_DX                                                   26
      EQUIVALENCE (BANK_SFJP(29),SFJP_SCVS_DX,RE_SFJP_SCVS_DX),           26
     *(NAMES_SFJP(27),NSFJP_SCVS_DX)                                      26
      EQUIVALENCE (COMMEN_SFJP(27),CSFJP_SCVS_DX),(MAP_SFJP(1,27),        26
     *MSFJP_SCVS_DX)                                                      26
      CHARACTER CSFJP_SCVS_DX*40/' '/                                     26
      CHARACTER NSFJP_SCVS_DX*12/'R.SCVS_DX'/                             26
      INTEGER MSFJP_SCVS_DX(2)/1,1/                                       26
      REAL RE_SFJP_SCVS_DX                                                26
C -                                                                       26
      REAL SFJP_SCVS_DY                                                   26
      EQUIVALENCE (BANK_SFJP(30),SFJP_SCVS_DY,RE_SFJP_SCVS_DY),           26
     *(NAMES_SFJP(28),NSFJP_SCVS_DY)                                      26
      EQUIVALENCE (COMMEN_SFJP(28),CSFJP_SCVS_DY),(MAP_SFJP(1,28),        26
     *MSFJP_SCVS_DY)                                                      26
      CHARACTER CSFJP_SCVS_DY*40/' '/                                     26
      CHARACTER NSFJP_SCVS_DY*12/'R.SCVS_DY'/                             26
      INTEGER MSFJP_SCVS_DY(2)/1,1/                                       26
      REAL RE_SFJP_SCVS_DY                                                26
C -                                                                       26
      REAL SFJP_SCVS_PX                                                   26
      EQUIVALENCE (BANK_SFJP(31),SFJP_SCVS_PX,RE_SFJP_SCVS_PX),           26
     *(NAMES_SFJP(29),NSFJP_SCVS_PX)                                      26
      EQUIVALENCE (COMMEN_SFJP(29),CSFJP_SCVS_PX),(MAP_SFJP(1,29),        26
     *MSFJP_SCVS_PX)                                                      26
      CHARACTER CSFJP_SCVS_PX*40/' '/                                     26
      CHARACTER NSFJP_SCVS_PX*12/'R.SCVS_PX'/                             26
      INTEGER MSFJP_SCVS_PX(2)/1,1/                                       26
      REAL RE_SFJP_SCVS_PX                                                26
C -                                                                       26
      REAL SFJP_SCVS_PY                                                   26
      EQUIVALENCE (BANK_SFJP(32),SFJP_SCVS_PY,RE_SFJP_SCVS_PY),           26
     *(NAMES_SFJP(30),NSFJP_SCVS_PY)                                      26
      EQUIVALENCE (COMMEN_SFJP(30),CSFJP_SCVS_PY),(MAP_SFJP(1,30),        26
     *MSFJP_SCVS_PY)                                                      26
      CHARACTER CSFJP_SCVS_PY*40/' '/                                     26
      CHARACTER NSFJP_SCVS_PY*12/'R.SCVS_PY'/                             26
      INTEGER MSFJP_SCVS_PY(2)/1,1/                                       26
      REAL RE_SFJP_SCVS_PY                                                26
C -                                                                       26
      REAL SFJP_SFCO_DX                                                   26
      EQUIVALENCE (BANK_SFJP(33),SFJP_SFCO_DX,RE_SFJP_SFCO_DX),           26
     *(NAMES_SFJP(31),NSFJP_SFCO_DX)                                      26
      EQUIVALENCE (COMMEN_SFJP(31),CSFJP_SFCO_DX),(MAP_SFJP(1,31),        26
     *MSFJP_SFCO_DX)                                                      26
      CHARACTER CSFJP_SFCO_DX*40/' '/                                     26
      CHARACTER NSFJP_SFCO_DX*12/'R.SFCO_DX'/                             26
      INTEGER MSFJP_SFCO_DX(2)/1,1/                                       26
      REAL RE_SFJP_SFCO_DX                                                26
C -                                                                       26
      REAL SFJP_SFCO_DY                                                   26
      EQUIVALENCE (BANK_SFJP(34),SFJP_SFCO_DY,RE_SFJP_SFCO_DY),           26
     *(NAMES_SFJP(32),NSFJP_SFCO_DY)                                      26
      EQUIVALENCE (COMMEN_SFJP(32),CSFJP_SFCO_DY),(MAP_SFJP(1,32),        26
     *MSFJP_SFCO_DY)                                                      26
      CHARACTER CSFJP_SFCO_DY*40/' '/                                     26
      CHARACTER NSFJP_SFCO_DY*12/'R.SFCO_DY'/                             26
      INTEGER MSFJP_SFCO_DY(2)/1,1/                                       26
      REAL RE_SFJP_SFCO_DY                                                26
C -                                                                       26
      REAL SFJP_SFCO_DZ                                                   26
      EQUIVALENCE (BANK_SFJP(35),SFJP_SFCO_DZ,RE_SFJP_SFCO_DZ),           26
     *(NAMES_SFJP(33),NSFJP_SFCO_DZ)                                      26
      EQUIVALENCE (COMMEN_SFJP(33),CSFJP_SFCO_DZ),(MAP_SFJP(1,33),        26
     *MSFJP_SFCO_DZ)                                                      26
      CHARACTER CSFJP_SFCO_DZ*40/' '/                                     26
      CHARACTER NSFJP_SFCO_DZ*12/'R.SFCO_DZ'/                             26
      INTEGER MSFJP_SFCO_DZ(2)/1,1/                                       26
      REAL RE_SFJP_SFCO_DZ                                                26
C -                                                                       26
      REAL SFJP_SFCO_PX                                                   26
      EQUIVALENCE (BANK_SFJP(36),SFJP_SFCO_PX,RE_SFJP_SFCO_PX),           26
     *(NAMES_SFJP(34),NSFJP_SFCO_PX)                                      26
      EQUIVALENCE (COMMEN_SFJP(34),CSFJP_SFCO_PX),(MAP_SFJP(1,34),        26
     *MSFJP_SFCO_PX)                                                      26
      CHARACTER CSFJP_SFCO_PX*40/' '/                                     26
      CHARACTER NSFJP_SFCO_PX*12/'R.SFCO_PX'/                             26
      INTEGER MSFJP_SFCO_PX(2)/1,1/                                       26
      REAL RE_SFJP_SFCO_PX                                                26
C -                                                                       26
      REAL SFJP_SFCO_PY                                                   26
      EQUIVALENCE (BANK_SFJP(37),SFJP_SFCO_PY,RE_SFJP_SFCO_PY),           26
     *(NAMES_SFJP(35),NSFJP_SFCO_PY)                                      26
      EQUIVALENCE (COMMEN_SFJP(35),CSFJP_SFCO_PY),(MAP_SFJP(1,35),        26
     *MSFJP_SFCO_PY)                                                      26
      CHARACTER CSFJP_SFCO_PY*40/' '/                                     26
      CHARACTER NSFJP_SFCO_PY*12/'R.SFCO_PY'/                             26
      INTEGER MSFJP_SFCO_PY(2)/1,1/                                       26
      REAL RE_SFJP_SFCO_PY                                                26
C -                                                                       26
      REAL SFJP_SFCM_DX                                                   26
      EQUIVALENCE (BANK_SFJP(38),SFJP_SFCM_DX,RE_SFJP_SFCM_DX),           26
     *(NAMES_SFJP(36),NSFJP_SFCM_DX)                                      26
      EQUIVALENCE (COMMEN_SFJP(36),CSFJP_SFCM_DX),(MAP_SFJP(1,36),        26
     *MSFJP_SFCM_DX)                                                      26
      CHARACTER CSFJP_SFCM_DX*40/' '/                                     26
      CHARACTER NSFJP_SFCM_DX*12/'R.SFCM_DX'/                             26
      INTEGER MSFJP_SFCM_DX(2)/1,1/                                       26
      REAL RE_SFJP_SFCM_DX                                                26
C -                                                                       26
      REAL SFJP_SFCS_DZ                                                   26
      EQUIVALENCE (BANK_SFJP(39),SFJP_SFCS_DZ,RE_SFJP_SFCS_DZ),           26
     *(NAMES_SFJP(37),NSFJP_SFCS_DZ)                                      26
      EQUIVALENCE (COMMEN_SFJP(37),CSFJP_SFCS_DZ),(MAP_SFJP(1,37),        26
     *MSFJP_SFCS_DZ)                                                      26
      CHARACTER CSFJP_SFCS_DZ*40/' '/                                     26
      CHARACTER NSFJP_SFCS_DZ*12/'R.SFCS_DZ'/                             26
      INTEGER MSFJP_SFCS_DZ(2)/1,1/                                       26
      REAL RE_SFJP_SFCS_DZ                                                26
C -                                                                       26
      REAL SFJP_SFKF_DY                                                   26
      EQUIVALENCE (BANK_SFJP(40),SFJP_SFKF_DY,RE_SFJP_SFKF_DY),           26
     *(NAMES_SFJP(38),NSFJP_SFKF_DY)                                      26
      EQUIVALENCE (COMMEN_SFJP(38),CSFJP_SFKF_DY),(MAP_SFJP(1,38),        26
     *MSFJP_SFKF_DY)                                                      26
      CHARACTER CSFJP_SFKF_DY*40/' '/                                     26
      CHARACTER NSFJP_SFKF_DY*12/'R.SFKF_DY'/                             26
      INTEGER MSFJP_SFKF_DY(2)/1,1/                                       26
      REAL RE_SFJP_SFKF_DY                                                26
C -                                                                       26
      REAL SFJP_SFKF_DZ                                                   26
      EQUIVALENCE (BANK_SFJP(41),SFJP_SFKF_DZ,RE_SFJP_SFKF_DZ),           26
     *(NAMES_SFJP(39),NSFJP_SFKF_DZ)                                      26
      EQUIVALENCE (COMMEN_SFJP(39),CSFJP_SFKF_DZ),(MAP_SFJP(1,39),        26
     *MSFJP_SFKF_DZ)                                                      26
      CHARACTER CSFJP_SFKF_DZ*40/' '/                                     26
      CHARACTER NSFJP_SFKF_DZ*12/'R.SFKF_DZ'/                             26
      INTEGER MSFJP_SFKF_DZ(2)/1,1/                                       26
      REAL RE_SFJP_SFKF_DZ                                                26
C -                                                                       26
      REAL SFJP_SFKF_DX                                                   26
      EQUIVALENCE (BANK_SFJP(42),SFJP_SFKF_DX,RE_SFJP_SFKF_DX),           26
     *(NAMES_SFJP(40),NSFJP_SFKF_DX)                                      26
      EQUIVALENCE (COMMEN_SFJP(40),CSFJP_SFKF_DX),(MAP_SFJP(1,40),        26
     *MSFJP_SFKF_DX)                                                      26
      CHARACTER CSFJP_SFKF_DX*40/' '/                                     26
      CHARACTER NSFJP_SFKF_DX*12/'R.SFKF_DX'/                             26
      INTEGER MSFJP_SFKF_DX(2)/1,1/                                       26
      REAL RE_SFJP_SFKF_DX                                                26
C -                                                                       26
      REAL SFJP_SFKF_PX                                                   26
      EQUIVALENCE (BANK_SFJP(43),SFJP_SFKF_PX,RE_SFJP_SFKF_PX),           26
     *(NAMES_SFJP(41),NSFJP_SFKF_PX)                                      26
      EQUIVALENCE (COMMEN_SFJP(41),CSFJP_SFKF_PX),(MAP_SFJP(1,41),        26
     *MSFJP_SFKF_PX)                                                      26
      CHARACTER CSFJP_SFKF_PX*40/' '/                                     26
      CHARACTER NSFJP_SFKF_PX*12/'R.SFKF_PX'/                             26
      INTEGER MSFJP_SFKF_PX(2)/1,1/                                       26
      REAL RE_SFJP_SFKF_PX                                                26
C -                                                                       26
      REAL SFJP_SFKF_PY                                                   26
      EQUIVALENCE (BANK_SFJP(44),SFJP_SFKF_PY,RE_SFJP_SFKF_PY),           26
     *(NAMES_SFJP(42),NSFJP_SFKF_PY)                                      26
      EQUIVALENCE (COMMEN_SFJP(42),CSFJP_SFKF_PY),(MAP_SFJP(1,42),        26
     *MSFJP_SFKF_PY)                                                      26
      CHARACTER CSFJP_SFKF_PY*40/' '/                                     26
      CHARACTER NSFJP_SFKF_PY*12/'R.SFKF_PY'/                             26
      INTEGER MSFJP_SFKF_PY(2)/1,1/                                       26
      REAL RE_SFJP_SFKF_PY                                                26
C -                                                                       26
      REAL SFJP_SFKS_DX                                                   26
      EQUIVALENCE (BANK_SFJP(45),SFJP_SFKS_DX,RE_SFJP_SFKS_DX),           26
     *(NAMES_SFJP(43),NSFJP_SFKS_DX)                                      26
      EQUIVALENCE (COMMEN_SFJP(43),CSFJP_SFKS_DX),(MAP_SFJP(1,43),        26
     *MSFJP_SFKS_DX)                                                      26
      CHARACTER CSFJP_SFKS_DX*40/' '/                                     26
      CHARACTER NSFJP_SFKS_DX*12/'R.SFKS_DX'/                             26
      INTEGER MSFJP_SFKS_DX(2)/1,1/                                       26
      REAL RE_SFJP_SFKS_DX                                                26
C -                                                                       26
      REAL SFJP_SFKS_PX                                                   26
      EQUIVALENCE (BANK_SFJP(46),SFJP_SFKS_PX,RE_SFJP_SFKS_PX),           26
     *(NAMES_SFJP(44),NSFJP_SFKS_PX)                                      26
      EQUIVALENCE (COMMEN_SFJP(44),CSFJP_SFKS_PX),(MAP_SFJP(1,44),        26
     *MSFJP_SFKS_PX)                                                      26
      CHARACTER CSFJP_SFKS_PX*40/' '/                                     26
      CHARACTER NSFJP_SFKS_PX*12/'R.SFKS_PX'/                             26
      INTEGER MSFJP_SFKS_PX(2)/1,1/                                       26
      REAL RE_SFJP_SFKS_PX                                                26
C -                                                                       26
      REAL SFJP_SFKS_PY                                                   26
      EQUIVALENCE (BANK_SFJP(47),SFJP_SFKS_PY,RE_SFJP_SFKS_PY),           26
     *(NAMES_SFJP(45),NSFJP_SFKS_PY)                                      26
      EQUIVALENCE (COMMEN_SFJP(45),CSFJP_SFKS_PY),(MAP_SFJP(1,45),        26
     *MSFJP_SFKS_PY)                                                      26
      CHARACTER CSFJP_SFKS_PY*40/' '/                                     26
      CHARACTER NSFJP_SFKS_PY*12/'R.SFKS_PY'/                             26
      INTEGER MSFJP_SFKS_PY(2)/1,1/                                       26
      REAL RE_SFJP_SFKS_PY                                                26
C -                                                                       26
      REAL SFJP_SFPR_PY                                                   26
      EQUIVALENCE (BANK_SFJP(48),SFJP_SFPR_PY,RE_SFJP_SFPR_PY),           26
     *(NAMES_SFJP(46),NSFJP_SFPR_PY)                                      26
      EQUIVALENCE (COMMEN_SFJP(46),CSFJP_SFPR_PY),(MAP_SFJP(1,46),        26
     *MSFJP_SFPR_PY)                                                      26
      CHARACTER CSFJP_SFPR_PY*40/' '/                                     26
      CHARACTER NSFJP_SFPR_PY*12/'R.SFPR_PY'/                             26
      INTEGER MSFJP_SFPR_PY(2)/1,1/                                       26
      REAL RE_SFJP_SFPR_PY                                                26
C -                                                                       26
      REAL SFJP_SFPB_PY                                                   26
      EQUIVALENCE (BANK_SFJP(49),SFJP_SFPB_PY,RE_SFJP_SFPB_PY),           26
     *(NAMES_SFJP(47),NSFJP_SFPB_PY)                                      26
      EQUIVALENCE (COMMEN_SFJP(47),CSFJP_SFPB_PY),(MAP_SFJP(1,47),        26
     *MSFJP_SFPB_PY)                                                      26
      CHARACTER CSFJP_SFPB_PY*40/' '/                                     26
      CHARACTER NSFJP_SFPB_PY*12/'R.SFPB_PY'/                             26
      INTEGER MSFJP_SFPB_PY(2)/1,1/                                       26
      REAL RE_SFJP_SFPB_PY                                                26
C -                                                                       26
      REAL SFJP_SFPB_PY2                                                  26
      EQUIVALENCE (BANK_SFJP(50),SFJP_SFPB_PY2,RE_SFJP_SFPB_PY2),         26
     *(NAMES_SFJP(48),NSFJP_SFPB_PY2)                                     26
      EQUIVALENCE (COMMEN_SFJP(48),CSFJP_SFPB_PY2),(MAP_SFJP(1,48),       26
     *MSFJP_SFPB_PY2)                                                     26
      CHARACTER CSFJP_SFPB_PY2*40/' '/                                    26
      CHARACTER NSFJP_SFPB_PY2*12/'R.SFPB_PY2'/                           26
      INTEGER MSFJP_SFPB_PY2(2)/1,1/                                      26
      REAL RE_SFJP_SFPB_PY2                                               26
C -                                                                       26
      REAL SFJP_SFCB_DX                                                   26
      EQUIVALENCE (BANK_SFJP(51),SFJP_SFCB_DX,RE_SFJP_SFCB_DX),           26
     *(NAMES_SFJP(49),NSFJP_SFCB_DX)                                      26
      EQUIVALENCE (COMMEN_SFJP(49),CSFJP_SFCB_DX),(MAP_SFJP(1,49),        26
     *MSFJP_SFCB_DX)                                                      26
      CHARACTER CSFJP_SFCB_DX*40/' '/                                     26
      CHARACTER NSFJP_SFCB_DX*12/'R.SFCB_DX'/                             26
      INTEGER MSFJP_SFCB_DX(2)/1,1/                                       26
      REAL RE_SFJP_SFCB_DX                                                26
C -                                                                       26
      REAL SFJP_SSBS_DY                                                   26
      EQUIVALENCE (BANK_SFJP(52),SFJP_SSBS_DY,RE_SFJP_SSBS_DY),           26
     *(NAMES_SFJP(50),NSFJP_SSBS_DY)                                      26
      EQUIVALENCE (COMMEN_SFJP(50),CSFJP_SSBS_DY),(MAP_SFJP(1,50),        26
     *MSFJP_SSBS_DY)                                                      26
      CHARACTER CSFJP_SSBS_DY*40/' '/                                     26
      CHARACTER NSFJP_SSBS_DY*12/'R.SSBS_DY'/                             26
      INTEGER MSFJP_SSBS_DY(2)/1,1/                                       26
      REAL RE_SFJP_SSBS_DY                                                26
C -                                                                       26
      REAL SFJP_SSBS_DX                                                   26
      EQUIVALENCE (BANK_SFJP(53),SFJP_SSBS_DX,RE_SFJP_SSBS_DX),           26
     *(NAMES_SFJP(51),NSFJP_SSBS_DX)                                      26
      EQUIVALENCE (COMMEN_SFJP(51),CSFJP_SSBS_DX),(MAP_SFJP(1,51),        26
     *MSFJP_SSBS_DX)                                                      26
      CHARACTER CSFJP_SSBS_DX*40/' '/                                     26
      CHARACTER NSFJP_SSBS_DX*12/'R.SSBS_DX'/                             26
      INTEGER MSFJP_SSBS_DX(2)/1,1/                                       26
      REAL RE_SFJP_SSBS_DX                                                26
C -                                                                       26
      REAL SFJP_SSBB_DX                                                   26
      EQUIVALENCE (BANK_SFJP(54),SFJP_SSBB_DX,RE_SFJP_SSBB_DX),           26
     *(NAMES_SFJP(52),NSFJP_SSBB_DX)                                      26
      EQUIVALENCE (COMMEN_SFJP(52),CSFJP_SSBB_DX),(MAP_SFJP(1,52),        26
     *MSFJP_SSBB_DX)                                                      26
      CHARACTER CSFJP_SSBB_DX*40/' '/                                     26
      CHARACTER NSFJP_SSBB_DX*12/'R.SSBB_DX'/                             26
      INTEGER MSFJP_SSBB_DX(2)/1,1/                                       26
      REAL RE_SFJP_SSBB_DX                                                26
C -                                                                       26
      REAL SFJP_SSBB_DZ                                                   26
      EQUIVALENCE (BANK_SFJP(55),SFJP_SSBB_DZ,RE_SFJP_SSBB_DZ),           26
     *(NAMES_SFJP(53),NSFJP_SSBB_DZ)                                      26
      EQUIVALENCE (COMMEN_SFJP(53),CSFJP_SSBB_DZ),(MAP_SFJP(1,53),        26
     *MSFJP_SSBB_DZ)                                                      26
      CHARACTER CSFJP_SSBB_DZ*40/' '/                                     26
      CHARACTER NSFJP_SSBB_DZ*12/'R.SSBB_DZ'/                             26
      INTEGER MSFJP_SSBB_DZ(2)/1,1/                                       26
      REAL RE_SFJP_SSBB_DZ                                                26
C -                                                                       26
      REAL SFJP_FLEX_DI                                                   26
      EQUIVALENCE (BANK_SFJP(56),SFJP_FLEX_DI,RE_SFJP_FLEX_DI),           26
     *(NAMES_SFJP(54),NSFJP_FLEX_DI)                                      26
      EQUIVALENCE (COMMEN_SFJP(54),CSFJP_FLEX_DI),(MAP_SFJP(1,54),        26
     *MSFJP_FLEX_DI)                                                      26
      CHARACTER CSFJP_FLEX_DI*40/' '/                                     26
      CHARACTER NSFJP_FLEX_DI*12/'R.FLEX_DI'/                             26
      INTEGER MSFJP_FLEX_DI(2)/1,1/                                       26
      REAL RE_SFJP_FLEX_DI                                                26
C -                                                                       26
      REAL SFJP_SFPB_DX                                                   26
      EQUIVALENCE (BANK_SFJP(57),SFJP_SFPB_DX,RE_SFJP_SFPB_DX),           26
     *(NAMES_SFJP(55),NSFJP_SFPB_DX)                                      26
      EQUIVALENCE (COMMEN_SFJP(55),CSFJP_SFPB_DX),(MAP_SFJP(1,55),        26
     *MSFJP_SFPB_DX)                                                      26
      CHARACTER CSFJP_SFPB_DX*40/' '/                                     26
      CHARACTER NSFJP_SFPB_DX*12/'R.SFPB_DX'/                             26
      INTEGER MSFJP_SFPB_DX(2)/1,1/                                       26
      REAL RE_SFJP_SFPB_DX                                                26
C -                                                                       26
      REAL SFJP_SFPB_DY                                                   26
      EQUIVALENCE (BANK_SFJP(58),SFJP_SFPB_DY,RE_SFJP_SFPB_DY),           26
     *(NAMES_SFJP(56),NSFJP_SFPB_DY)                                      26
      EQUIVALENCE (COMMEN_SFJP(56),CSFJP_SFPB_DY),(MAP_SFJP(1,56),        26
     *MSFJP_SFPB_DY)                                                      26
      CHARACTER CSFJP_SFPB_DY*40/' '/                                     26
      CHARACTER NSFJP_SFPB_DY*12/'R.SFPB_DY'/                             26
      INTEGER MSFJP_SFPB_DY(2)/1,1/                                       26
      REAL RE_SFJP_SFPB_DY                                                26
C -                                                                       26
      REAL SFJP_SFPBDZ                                                    26
      EQUIVALENCE (BANK_SFJP(59),SFJP_SFPBDZ,RE_SFJP_SFPBDZ),             26
     *(NAMES_SFJP(57),NSFJP_SFPBDZ)                                       26
      EQUIVALENCE (COMMEN_SFJP(57),CSFJP_SFPBDZ),(MAP_SFJP(1,57),         26
     *MSFJP_SFPBDZ)                                                       26
      CHARACTER CSFJP_SFPBDZ*40/' '/                                      26
      CHARACTER NSFJP_SFPBDZ*12/'R.SFPBDZ'/                               26
      INTEGER MSFJP_SFPBDZ(2)/1,1/                                        26
      REAL RE_SFJP_SFPBDZ                                                 26
C -                                                                       26
      REAL SFJP_SAPP_DXE                                                  26
      EQUIVALENCE (BANK_SFJP(60),SFJP_SAPP_DXE,RE_SFJP_SAPP_DXE),         26
     *(NAMES_SFJP(58),NSFJP_SAPP_DXE)                                     26
      EQUIVALENCE (COMMEN_SFJP(58),CSFJP_SAPP_DXE),(MAP_SFJP(1,58),       26
     *MSFJP_SAPP_DXE)                                                     26
      CHARACTER CSFJP_SAPP_DXE*40/' '/                                    26
      CHARACTER NSFJP_SAPP_DXE*12/'R.SAPP_DXE'/                           26
      INTEGER MSFJP_SAPP_DXE(2)/1,1/                                      26
      REAL RE_SFJP_SAPP_DXE                                               26
C -                                                                       26
      REAL SFJP_SAPP_DXZ                                                  26
      EQUIVALENCE (BANK_SFJP(61),SFJP_SAPP_DXZ,RE_SFJP_SAPP_DXZ),         26
     *(NAMES_SFJP(59),NSFJP_SAPP_DXZ)                                     26
      EQUIVALENCE (COMMEN_SFJP(59),CSFJP_SAPP_DXZ),(MAP_SFJP(1,59),       26
     *MSFJP_SAPP_DXZ)                                                     26
      CHARACTER CSFJP_SAPP_DXZ*40/' '/                                    26
      CHARACTER NSFJP_SAPP_DXZ*12/'R.SAPP_DXZ'/                           26
      INTEGER MSFJP_SAPP_DXZ(2)/1,1/                                      26
      REAL RE_SFJP_SAPP_DXZ                                               26
C -                                                                       26
      REAL SFJP_SAPP_DY                                                   26
      EQUIVALENCE (BANK_SFJP(62),SFJP_SAPP_DY,RE_SFJP_SAPP_DY),           26
     *(NAMES_SFJP(60),NSFJP_SAPP_DY)                                      26
      EQUIVALENCE (COMMEN_SFJP(60),CSFJP_SAPP_DY),(MAP_SFJP(1,60),        26
     *MSFJP_SAPP_DY)                                                      26
      CHARACTER CSFJP_SAPP_DY*40/' '/                                     26
      CHARACTER NSFJP_SAPP_DY*12/'R.SAPP_DY'/                             26
      INTEGER MSFJP_SAPP_DY(2)/1,1/                                       26
      REAL RE_SFJP_SAPP_DY                                                26
C -                                                                       26
      REAL SFJP_SAPP_DZ                                                   26
      EQUIVALENCE (BANK_SFJP(63),SFJP_SAPP_DZ,RE_SFJP_SAPP_DZ),           26
     *(NAMES_SFJP(61),NSFJP_SAPP_DZ)                                      26
      EQUIVALENCE (COMMEN_SFJP(61),CSFJP_SAPP_DZ),(MAP_SFJP(1,61),        26
     *MSFJP_SAPP_DZ)                                                      26
      CHARACTER CSFJP_SAPP_DZ*40/' '/                                     26
      CHARACTER NSFJP_SAPP_DZ*12/'R.SAPP_DZ'/                             26
      INTEGER MSFJP_SAPP_DZ(2)/1,1/                                       26
      REAL RE_SFJP_SAPP_DZ                                                26
C -                                                                       26
      REAL SFJP_SAPP_PY1                                                  26
      EQUIVALENCE (BANK_SFJP(64),SFJP_SAPP_PY1,RE_SFJP_SAPP_PY1),         26
     *(NAMES_SFJP(62),NSFJP_SAPP_PY1)                                     26
      EQUIVALENCE (COMMEN_SFJP(62),CSFJP_SAPP_PY1),(MAP_SFJP(1,62),       26
     *MSFJP_SAPP_PY1)                                                     26
      CHARACTER CSFJP_SAPP_PY1*40/' '/                                    26
      CHARACTER NSFJP_SAPP_PY1*12/'R.SAPP_PY1'/                           26
      INTEGER MSFJP_SAPP_PY1(2)/1,1/                                      26
      REAL RE_SFJP_SAPP_PY1                                               26
C -                                                                       26
      REAL SFJP_SAPP_PY2                                                  26
      EQUIVALENCE (BANK_SFJP(65),SFJP_SAPP_PY2,RE_SFJP_SAPP_PY2),         26
     *(NAMES_SFJP(63),NSFJP_SAPP_PY2)                                     26
      EQUIVALENCE (COMMEN_SFJP(63),CSFJP_SAPP_PY2),(MAP_SFJP(1,63),       26
     *MSFJP_SAPP_PY2)                                                     26
      CHARACTER CSFJP_SAPP_PY2*40/' '/                                    26
      CHARACTER NSFJP_SAPP_PY2*12/'R.SAPP_PY2'/                           26
      INTEGER MSFJP_SAPP_PY2(2)/1,1/                                      26
      REAL RE_SFJP_SAPP_PY2                                               26
C -                                                                       26
      REAL SFJP_SAPP_PY3                                                  26
      EQUIVALENCE (BANK_SFJP(66),SFJP_SAPP_PY3,RE_SFJP_SAPP_PY3),         26
     *(NAMES_SFJP(64),NSFJP_SAPP_PY3)                                     26
      EQUIVALENCE (COMMEN_SFJP(64),CSFJP_SAPP_PY3),(MAP_SFJP(1,64),       26
     *MSFJP_SAPP_PY3)                                                     26
      CHARACTER CSFJP_SAPP_PY3*40/' '/                                    26
      CHARACTER NSFJP_SAPP_PY3*12/'R.SAPP_PY3'/                           26
      INTEGER MSFJP_SAPP_PY3(2)/1,1/                                      26
      REAL RE_SFJP_SAPP_PY3                                               26
C -                                                                       26
      REAL SFJP_SFAM_DXE                                                  26
      EQUIVALENCE (BANK_SFJP(67),SFJP_SFAM_DXE,RE_SFJP_SFAM_DXE),         26
     *(NAMES_SFJP(65),NSFJP_SFAM_DXE)                                     26
      EQUIVALENCE (COMMEN_SFJP(65),CSFJP_SFAM_DXE),(MAP_SFJP(1,65),       26
     *MSFJP_SFAM_DXE)                                                     26
      CHARACTER CSFJP_SFAM_DXE*40/' '/                                    26
      CHARACTER NSFJP_SFAM_DXE*12/'R.SFAM_DXE'/                           26
      INTEGER MSFJP_SFAM_DXE(2)/1,1/                                      26
      REAL RE_SFJP_SFAM_DXE                                               26
C -                                                                       26
      REAL SFJP_SFAM_DXZ                                                  26
      EQUIVALENCE (BANK_SFJP(68),SFJP_SFAM_DXZ,RE_SFJP_SFAM_DXZ),         26
     *(NAMES_SFJP(66),NSFJP_SFAM_DXZ)                                     26
      EQUIVALENCE (COMMEN_SFJP(66),CSFJP_SFAM_DXZ),(MAP_SFJP(1,66),       26
     *MSFJP_SFAM_DXZ)                                                     26
      CHARACTER CSFJP_SFAM_DXZ*40/' '/                                    26
      CHARACTER NSFJP_SFAM_DXZ*12/'R.SFAM_DXZ'/                           26
      INTEGER MSFJP_SFAM_DXZ(2)/1,1/                                      26
      REAL RE_SFJP_SFAM_DXZ                                               26
C -                                                                       26
      REAL SFJP_SFAM_DY                                                   26
      EQUIVALENCE (BANK_SFJP(69),SFJP_SFAM_DY,RE_SFJP_SFAM_DY),           26
     *(NAMES_SFJP(67),NSFJP_SFAM_DY)                                      26
      EQUIVALENCE (COMMEN_SFJP(67),CSFJP_SFAM_DY),(MAP_SFJP(1,67),        26
     *MSFJP_SFAM_DY)                                                      26
      CHARACTER CSFJP_SFAM_DY*40/' '/                                     26
      CHARACTER NSFJP_SFAM_DY*12/'R.SFAM_DY'/                             26
      INTEGER MSFJP_SFAM_DY(2)/1,1/                                       26
      REAL RE_SFJP_SFAM_DY                                                26
C -                                                                       26
      REAL SFJP_SFAM_DZ                                                   26
      EQUIVALENCE (BANK_SFJP(70),SFJP_SFAM_DZ,RE_SFJP_SFAM_DZ),           26
     *(NAMES_SFJP(68),NSFJP_SFAM_DZ)                                      26
      EQUIVALENCE (COMMEN_SFJP(68),CSFJP_SFAM_DZ),(MAP_SFJP(1,68),        26
     *MSFJP_SFAM_DZ)                                                      26
      CHARACTER CSFJP_SFAM_DZ*40/' '/                                     26
      CHARACTER NSFJP_SFAM_DZ*12/'R.SFAM_DZ'/                             26
      INTEGER MSFJP_SFAM_DZ(2)/1,1/                                       26
      REAL RE_SFJP_SFAM_DZ                                                26
C -                                                                       26
      REAL SFJP_SFAM_DZS                                                  26
      EQUIVALENCE (BANK_SFJP(71),SFJP_SFAM_DZS,RE_SFJP_SFAM_DZS),         26
     *(NAMES_SFJP(69),NSFJP_SFAM_DZS)                                     26
      EQUIVALENCE (COMMEN_SFJP(69),CSFJP_SFAM_DZS),(MAP_SFJP(1,69),       26
     *MSFJP_SFAM_DZS)                                                     26
      CHARACTER CSFJP_SFAM_DZS*40/' '/                                    26
      CHARACTER NSFJP_SFAM_DZS*12/'R.SFAM_DZS'/                           26
      INTEGER MSFJP_SFAM_DZS(2)/1,1/                                      26
      REAL RE_SFJP_SFAM_DZS                                               26
C -                                                                       26
      REAL SFJP_SFLA_DX                                                   26
      EQUIVALENCE (BANK_SFJP(72),SFJP_SFLA_DX,RE_SFJP_SFLA_DX),           26
     *(NAMES_SFJP(70),NSFJP_SFLA_DX)                                      26
      EQUIVALENCE (COMMEN_SFJP(70),CSFJP_SFLA_DX),(MAP_SFJP(1,70),        26
     *MSFJP_SFLA_DX)                                                      26
      CHARACTER CSFJP_SFLA_DX*40/' '/                                     26
      CHARACTER NSFJP_SFLA_DX*12/'R.SFLA_DX'/                             26
      INTEGER MSFJP_SFLA_DX(2)/1,1/                                       26
      REAL RE_SFJP_SFLA_DX                                                26
C -                                                                       26
      REAL SFJP_SFLA_DY                                                   26
      EQUIVALENCE (BANK_SFJP(73),SFJP_SFLA_DY,RE_SFJP_SFLA_DY),           26
     *(NAMES_SFJP(71),NSFJP_SFLA_DY)                                      26
      EQUIVALENCE (COMMEN_SFJP(71),CSFJP_SFLA_DY),(MAP_SFJP(1,71),        26
     *MSFJP_SFLA_DY)                                                      26
      CHARACTER CSFJP_SFLA_DY*40/' '/                                     26
      CHARACTER NSFJP_SFLA_DY*12/'R.SFLA_DY'/                             26
      INTEGER MSFJP_SFLA_DY(2)/1,1/                                       26
      REAL RE_SFJP_SFLA_DY                                                26
C -                                                                       26
      REAL SFJP_SFLA_DZ                                                   26
      EQUIVALENCE (BANK_SFJP(74),SFJP_SFLA_DZ,RE_SFJP_SFLA_DZ),           26
     *(NAMES_SFJP(72),NSFJP_SFLA_DZ)                                      26
      EQUIVALENCE (COMMEN_SFJP(72),CSFJP_SFLA_DZ),(MAP_SFJP(1,72),        26
     *MSFJP_SFLA_DZ)                                                      26
      CHARACTER CSFJP_SFLA_DZ*40/' '/                                     26
      CHARACTER NSFJP_SFLA_DZ*12/'R.SFLA_DZ'/                             26
      INTEGER MSFJP_SFLA_DZ(2)/1,1/                                       26
      REAL RE_SFJP_SFLA_DZ                                                26
C -                                                                       26
      REAL SFJP_SFLB_DZ                                                   26
      EQUIVALENCE (BANK_SFJP(75),SFJP_SFLB_DZ,RE_SFJP_SFLB_DZ),           26
     *(NAMES_SFJP(73),NSFJP_SFLB_DZ)                                      26
      EQUIVALENCE (COMMEN_SFJP(73),CSFJP_SFLB_DZ),(MAP_SFJP(1,73),        26
     *MSFJP_SFLB_DZ)                                                      26
      CHARACTER CSFJP_SFLB_DZ*40/' '/                                     26
      CHARACTER NSFJP_SFLB_DZ*12/'R.SFLB_DZ'/                             26
      INTEGER MSFJP_SFLB_DZ(2)/1,1/                                       26
      REAL RE_SFJP_SFLB_DZ                                                26
C -                                                                       26
      REAL SFJP_SFLC_DZ                                                   26
      EQUIVALENCE (BANK_SFJP(76),SFJP_SFLC_DZ,RE_SFJP_SFLC_DZ),           26
     *(NAMES_SFJP(74),NSFJP_SFLC_DZ)                                      26
      EQUIVALENCE (COMMEN_SFJP(74),CSFJP_SFLC_DZ),(MAP_SFJP(1,74),        26
     *MSFJP_SFLC_DZ)                                                      26
      CHARACTER CSFJP_SFLC_DZ*40/' '/                                     26
      CHARACTER NSFJP_SFLC_DZ*12/'R.SFLC_DZ'/                             26
      INTEGER MSFJP_SFLC_DZ(2)/1,1/                                       26
      REAL RE_SFJP_SFLC_DZ                                                26
C -                                                                       26
      REAL SFJP_SFEB_DX                                                   26
      EQUIVALENCE (BANK_SFJP(77),SFJP_SFEB_DX,RE_SFJP_SFEB_DX),           26
     *(NAMES_SFJP(75),NSFJP_SFEB_DX)                                      26
      EQUIVALENCE (COMMEN_SFJP(75),CSFJP_SFEB_DX),(MAP_SFJP(1,75),        26
     *MSFJP_SFEB_DX)                                                      26
      CHARACTER CSFJP_SFEB_DX*40/' '/                                     26
      CHARACTER NSFJP_SFEB_DX*12/'R.SFEB_DX'/                             26
      INTEGER MSFJP_SFEB_DX(2)/1,1/                                       26
      REAL RE_SFJP_SFEB_DX                                                26
C -                                                                       26
      REAL SFJP_SFEB_DZ                                                   26
      EQUIVALENCE (BANK_SFJP(78),SFJP_SFEB_DZ,RE_SFJP_SFEB_DZ),           26
     *(NAMES_SFJP(76),NSFJP_SFEB_DZ)                                      26
      EQUIVALENCE (COMMEN_SFJP(76),CSFJP_SFEB_DZ),(MAP_SFJP(1,76),        26
     *MSFJP_SFEB_DZ)                                                      26
      CHARACTER CSFJP_SFEB_DZ*40/' '/                                     26
      CHARACTER NSFJP_SFEB_DZ*12/'R.SFEB_DZ'/                             26
      INTEGER MSFJP_SFEB_DZ(2)/1,1/                                       26
      REAL RE_SFJP_SFEB_DZ                                                26
C -                                                                       26
      REAL SFJP_SFES_DX                                                   26
      EQUIVALENCE (BANK_SFJP(79),SFJP_SFES_DX,RE_SFJP_SFES_DX),           26
     *(NAMES_SFJP(77),NSFJP_SFES_DX)                                      26
      EQUIVALENCE (COMMEN_SFJP(77),CSFJP_SFES_DX),(MAP_SFJP(1,77),        26
     *MSFJP_SFES_DX)                                                      26
      CHARACTER CSFJP_SFES_DX*40/' '/                                     26
      CHARACTER NSFJP_SFES_DX*12/'R.SFES_DX'/                             26
      INTEGER MSFJP_SFES_DX(2)/1,1/                                       26
      REAL RE_SFJP_SFES_DX                                                26
C -                                                                       26
      REAL SFJP_SFFK_DXE                                                  26
      EQUIVALENCE (BANK_SFJP(80),SFJP_SFFK_DXE,RE_SFJP_SFFK_DXE),         26
     *(NAMES_SFJP(78),NSFJP_SFFK_DXE)                                     26
      EQUIVALENCE (COMMEN_SFJP(78),CSFJP_SFFK_DXE),(MAP_SFJP(1,78),       26
     *MSFJP_SFFK_DXE)                                                     26
      CHARACTER CSFJP_SFFK_DXE*40/' '/                                    26
      CHARACTER NSFJP_SFFK_DXE*12/'R.SFFK_DXE'/                           26
      INTEGER MSFJP_SFFK_DXE(2)/1,1/                                      26
      REAL RE_SFJP_SFFK_DXE                                               26
C -                                                                       26
      REAL SFJP_SFFK_DY                                                   26
      EQUIVALENCE (BANK_SFJP(81),SFJP_SFFK_DY,RE_SFJP_SFFK_DY),           26
     *(NAMES_SFJP(79),NSFJP_SFFK_DY)                                      26
      EQUIVALENCE (COMMEN_SFJP(79),CSFJP_SFFK_DY),(MAP_SFJP(1,79),        26
     *MSFJP_SFFK_DY)                                                      26
      CHARACTER CSFJP_SFFK_DY*40/' '/                                     26
      CHARACTER NSFJP_SFFK_DY*12/'R.SFFK_DY'/                             26
      INTEGER MSFJP_SFFK_DY(2)/1,1/                                       26
      REAL RE_SFJP_SFFK_DY                                                26
C -                                                                       26
      REAL SFJP_SFFX_DYE                                                  26
      EQUIVALENCE (BANK_SFJP(82),SFJP_SFFX_DYE,RE_SFJP_SFFX_DYE),         26
     *(NAMES_SFJP(80),NSFJP_SFFX_DYE)                                     26
      EQUIVALENCE (COMMEN_SFJP(80),CSFJP_SFFX_DYE),(MAP_SFJP(1,80),       26
     *MSFJP_SFFX_DYE)                                                     26
      CHARACTER CSFJP_SFFX_DYE*40/' '/                                    26
      CHARACTER NSFJP_SFFX_DYE*12/'R.SFFX_DYE'/                           26
      INTEGER MSFJP_SFFX_DYE(2)/1,1/                                      26
      REAL RE_SFJP_SFFX_DYE                                               26
C -                                                                       26
      REAL SFJP_SFFK_DZ                                                   26
      EQUIVALENCE (BANK_SFJP(83),SFJP_SFFK_DZ,RE_SFJP_SFFK_DZ),           26
     *(NAMES_SFJP(81),NSFJP_SFFK_DZ)                                      26
      EQUIVALENCE (COMMEN_SFJP(81),CSFJP_SFFK_DZ),(MAP_SFJP(1,81),        26
     *MSFJP_SFFK_DZ)                                                      26
      CHARACTER CSFJP_SFFK_DZ*40/' '/                                     26
      CHARACTER NSFJP_SFFK_DZ*12/'R.SFFK_DZ'/                             26
      INTEGER MSFJP_SFFK_DZ(2)/1,1/                                       26
      REAL RE_SFJP_SFFK_DZ                                                26
C -                                                                       26
      REAL SFJP_SFFL_DX                                                   26
      EQUIVALENCE (BANK_SFJP(84),SFJP_SFFL_DX,RE_SFJP_SFFL_DX),           26
     *(NAMES_SFJP(82),NSFJP_SFFL_DX)                                      26
      EQUIVALENCE (COMMEN_SFJP(82),CSFJP_SFFL_DX),(MAP_SFJP(1,82),        26
     *MSFJP_SFFL_DX)                                                      26
      CHARACTER CSFJP_SFFL_DX*40/' '/                                     26
      CHARACTER NSFJP_SFFL_DX*12/'R.SFFL_DX'/                             26
      INTEGER MSFJP_SFFL_DX(2)/1,1/                                       26
      REAL RE_SFJP_SFFL_DX                                                26
C -                                                                       26
      REAL SFJP_SFFK_DXZ                                                  26
      EQUIVALENCE (BANK_SFJP(85),SFJP_SFFK_DXZ,RE_SFJP_SFFK_DXZ),         26
     *(NAMES_SFJP(83),NSFJP_SFFK_DXZ)                                     26
      EQUIVALENCE (COMMEN_SFJP(83),CSFJP_SFFK_DXZ),(MAP_SFJP(1,83),       26
     *MSFJP_SFFK_DXZ)                                                     26
      CHARACTER CSFJP_SFFK_DXZ*40/' '/                                    26
      CHARACTER NSFJP_SFFK_DXZ*12/'R.SFFK_DXZ'/                           26
      INTEGER MSFJP_SFFK_DXZ(2)/1,1/                                      26
      REAL RE_SFJP_SFFK_DXZ                                               26
C -                                                                       26
      REAL SFJP_SFFK_PX                                                   26
      EQUIVALENCE (BANK_SFJP(86),SFJP_SFFK_PX,RE_SFJP_SFFK_PX),           26
     *(NAMES_SFJP(84),NSFJP_SFFK_PX)                                      26
      EQUIVALENCE (COMMEN_SFJP(84),CSFJP_SFFK_PX),(MAP_SFJP(1,84),        26
     *MSFJP_SFFK_PX)                                                      26
      CHARACTER CSFJP_SFFK_PX*40/' '/                                     26
      CHARACTER NSFJP_SFFK_PX*12/'R.SFFK_PX'/                             26
      INTEGER MSFJP_SFFK_PX(2)/1,1/                                       26
      REAL RE_SFJP_SFFK_PX                                                26
C -                                                                       26
      REAL SFJP_SFFK_PY1                                                  26
      EQUIVALENCE (BANK_SFJP(87),SFJP_SFFK_PY1,RE_SFJP_SFFK_PY1),         26
     *(NAMES_SFJP(85),NSFJP_SFFK_PY1)                                     26
      EQUIVALENCE (COMMEN_SFJP(85),CSFJP_SFFK_PY1),(MAP_SFJP(1,85),       26
     *MSFJP_SFFK_PY1)                                                     26
      CHARACTER CSFJP_SFFK_PY1*40/' '/                                    26
      CHARACTER NSFJP_SFFK_PY1*12/'R.SFFK_PY1'/                           26
      INTEGER MSFJP_SFFK_PY1(2)/1,1/                                      26
      REAL RE_SFJP_SFFK_PY1                                               26
C -                                                                       26
      REAL SFJP_SFFK_PY2                                                  26
      EQUIVALENCE (BANK_SFJP(88),SFJP_SFFK_PY2,RE_SFJP_SFFK_PY2),         26
     *(NAMES_SFJP(86),NSFJP_SFFK_PY2)                                     26
      EQUIVALENCE (COMMEN_SFJP(86),CSFJP_SFFK_PY2),(MAP_SFJP(1,86),       26
     *MSFJP_SFFK_PY2)                                                     26
      CHARACTER CSFJP_SFFK_PY2*40/' '/                                    26
      CHARACTER NSFJP_SFFK_PY2*12/'R.SFFK_PY2'/                           26
      INTEGER MSFJP_SFFK_PY2(2)/1,1/                                      26
      REAL RE_SFJP_SFFK_PY2                                               26
C -                                                                       26
      REAL SFJP_SFKL_PX                                                   26
      EQUIVALENCE (BANK_SFJP(89),SFJP_SFKL_PX,RE_SFJP_SFKL_PX),           26
     *(NAMES_SFJP(87),NSFJP_SFKL_PX)                                      26
      EQUIVALENCE (COMMEN_SFJP(87),CSFJP_SFKL_PX),(MAP_SFJP(1,87),        26
     *MSFJP_SFKL_PX)                                                      26
      CHARACTER CSFJP_SFKL_PX*40/' '/                                     26
      CHARACTER NSFJP_SFKL_PX*12/'R.SFKL_PX'/                             26
      INTEGER MSFJP_SFKL_PX(2)/1,1/                                       26
      REAL RE_SFJP_SFKL_PX                                                26
C -                                                                       26
      REAL SFJP_SFKK_DY                                                   26
      EQUIVALENCE (BANK_SFJP(90),SFJP_SFKK_DY,RE_SFJP_SFKK_DY),           26
     *(NAMES_SFJP(88),NSFJP_SFKK_DY)                                      26
      EQUIVALENCE (COMMEN_SFJP(88),CSFJP_SFKK_DY),(MAP_SFJP(1,88),        26
     *MSFJP_SFKK_DY)                                                      26
      CHARACTER CSFJP_SFKK_DY*40/' '/                                     26
      CHARACTER NSFJP_SFKK_DY*12/'R.SFKK_DY'/                             26
      INTEGER MSFJP_SFKK_DY(2)/1,1/                                       26
      REAL RE_SFJP_SFKK_DY                                                26
C -                                                                       26
      REAL SFJP_SFLU_DZ                                                   26
      EQUIVALENCE (BANK_SFJP(91),SFJP_SFLU_DZ,RE_SFJP_SFLU_DZ),           26
     *(NAMES_SFJP(89),NSFJP_SFLU_DZ)                                      26
      EQUIVALENCE (COMMEN_SFJP(89),CSFJP_SFLU_DZ),(MAP_SFJP(1,89),        26
     *MSFJP_SFLU_DZ)                                                      26
      CHARACTER CSFJP_SFLU_DZ*40/' '/                                     26
      CHARACTER NSFJP_SFLU_DZ*12/'R.SFLU_DZ'/                             26
      INTEGER MSFJP_SFLU_DZ(2)/1,1/                                       26
      REAL RE_SFJP_SFLU_DZ                                                26
C -                                                                       26
      REAL SFJP_SFLU_H1                                                   26
      EQUIVALENCE (BANK_SFJP(92),SFJP_SFLU_H1,RE_SFJP_SFLU_H1),           26
     *(NAMES_SFJP(90),NSFJP_SFLU_H1)                                      26
      EQUIVALENCE (COMMEN_SFJP(90),CSFJP_SFLU_H1),(MAP_SFJP(1,90),        26
     *MSFJP_SFLU_H1)                                                      26
      CHARACTER CSFJP_SFLU_H1*40/' '/                                     26
      CHARACTER NSFJP_SFLU_H1*12/'R.SFLU_H1'/                             26
      INTEGER MSFJP_SFLU_H1(2)/1,1/                                       26
      REAL RE_SFJP_SFLU_H1                                                26
C -                                                                       26
      REAL SFJP_SFLU_BL1                                                  26
      EQUIVALENCE (BANK_SFJP(93),SFJP_SFLU_BL1,RE_SFJP_SFLU_BL1),         26
     *(NAMES_SFJP(91),NSFJP_SFLU_BL1)                                     26
      EQUIVALENCE (COMMEN_SFJP(91),CSFJP_SFLU_BL1),(MAP_SFJP(1,91),       26
     *MSFJP_SFLU_BL1)                                                     26
      CHARACTER CSFJP_SFLU_BL1*40/' '/                                    26
      CHARACTER NSFJP_SFLU_BL1*12/'R.SFLU_BL1'/                           26
      INTEGER MSFJP_SFLU_BL1(2)/1,1/                                      26
      REAL RE_SFJP_SFLU_BL1                                               26
C -                                                                       26
      REAL SFJP_SFRA_DX                                                   26
      EQUIVALENCE (BANK_SFJP(94),SFJP_SFRA_DX,RE_SFJP_SFRA_DX),           26
     *(NAMES_SFJP(92),NSFJP_SFRA_DX)                                      26
      EQUIVALENCE (COMMEN_SFJP(92),CSFJP_SFRA_DX),(MAP_SFJP(1,92),        26
     *MSFJP_SFRA_DX)                                                      26
      CHARACTER CSFJP_SFRA_DX*40/' '/                                     26
      CHARACTER NSFJP_SFRA_DX*12/'R.SFRA_DX'/                             26
      INTEGER MSFJP_SFRA_DX(2)/1,1/                                       26
      REAL RE_SFJP_SFRA_DX                                                26
C -                                                                       26
      REAL SFJP_SFRA_DY                                                   26
      EQUIVALENCE (BANK_SFJP(95),SFJP_SFRA_DY,RE_SFJP_SFRA_DY),           26
     *(NAMES_SFJP(93),NSFJP_SFRA_DY)                                      26
      EQUIVALENCE (COMMEN_SFJP(93),CSFJP_SFRA_DY),(MAP_SFJP(1,93),        26
     *MSFJP_SFRA_DY)                                                      26
      CHARACTER CSFJP_SFRA_DY*40/' '/                                     26
      CHARACTER NSFJP_SFRA_DY*12/'R.SFRA_DY'/                             26
      INTEGER MSFJP_SFRA_DY(2)/1,1/                                       26
      REAL RE_SFJP_SFRA_DY                                                26
C -                                                                       26
      REAL SFJP_SFRA_PY                                                   26
      EQUIVALENCE (BANK_SFJP(96),SFJP_SFRA_PY,RE_SFJP_SFRA_PY),           26
     *(NAMES_SFJP(94),NSFJP_SFRA_PY)                                      26
      EQUIVALENCE (COMMEN_SFJP(94),CSFJP_SFRA_PY),(MAP_SFJP(1,94),        26
     *MSFJP_SFRA_PY)                                                      26
      CHARACTER CSFJP_SFRA_PY*40/' '/                                     26
      CHARACTER NSFJP_SFRA_PY*12/'R.SFRA_PY'/                             26
      INTEGER MSFJP_SFRA_PY(2)/1,1/                                       26
      REAL RE_SFJP_SFRA_PY                                                26
C -                                                                       26
      REAL SFJP_SFRA_DZ                                                   26
      EQUIVALENCE (BANK_SFJP(97),SFJP_SFRA_DZ,RE_SFJP_SFRA_DZ),           26
     *(NAMES_SFJP(95),NSFJP_SFRA_DZ)                                      26
      EQUIVALENCE (COMMEN_SFJP(95),CSFJP_SFRA_DZ),(MAP_SFJP(1,95),        26
     *MSFJP_SFRA_DZ)                                                      26
      CHARACTER CSFJP_SFRA_DZ*40/' '/                                     26
      CHARACTER NSFJP_SFRA_DZ*12/'R.SFRA_DZ'/                             26
      INTEGER MSFJP_SFRA_DZ(2)/1,1/                                       26
      REAL RE_SFJP_SFRA_DZ                                                26
C -                                                                       26
      REAL SFJP_SFRA_PZ                                                   26
      EQUIVALENCE (BANK_SFJP(98),SFJP_SFRA_PZ,RE_SFJP_SFRA_PZ),           26
     *(NAMES_SFJP(96),NSFJP_SFRA_PZ)                                      26
      EQUIVALENCE (COMMEN_SFJP(96),CSFJP_SFRA_PZ),(MAP_SFJP(1,96),        26
     *MSFJP_SFRA_PZ)                                                      26
      CHARACTER CSFJP_SFRA_PZ*40/' '/                                     26
      CHARACTER NSFJP_SFRA_PZ*12/'R.SFRA_PZ'/                             26
      INTEGER MSFJP_SFRA_PZ(2)/1,1/                                       26
      REAL RE_SFJP_SFRA_PZ                                                26
C -                                                                       26
      REAL SFJP_SFSW_DY                                                   26
      EQUIVALENCE (BANK_SFJP(99),SFJP_SFSW_DY,RE_SFJP_SFSW_DY),           26
     *(NAMES_SFJP(97),NSFJP_SFSW_DY)                                      26
      EQUIVALENCE (COMMEN_SFJP(97),CSFJP_SFSW_DY),(MAP_SFJP(1,97),        26
     *MSFJP_SFSW_DY)                                                      26
      CHARACTER CSFJP_SFSW_DY*40/' '/                                     26
      CHARACTER NSFJP_SFSW_DY*12/'R.SFSW_DY'/                             26
      INTEGER MSFJP_SFSW_DY(2)/1,1/                                       26
      REAL RE_SFJP_SFSW_DY                                                26
C -                                                                       26
      REAL SFJP_SFSW_DZ                                                   26
      EQUIVALENCE (BANK_SFJP(100),SFJP_SFSW_DZ,RE_SFJP_SFSW_DZ),          26
     *(NAMES_SFJP(98),NSFJP_SFSW_DZ)                                      26
      EQUIVALENCE (COMMEN_SFJP(98),CSFJP_SFSW_DZ),(MAP_SFJP(1,98),        26
     *MSFJP_SFSW_DZ)                                                      26
      CHARACTER CSFJP_SFSW_DZ*40/' '/                                     26
      CHARACTER NSFJP_SFSW_DZ*12/'R.SFSW_DZ'/                             26
      INTEGER MSFJP_SFSW_DZ(2)/1,1/                                       26
      REAL RE_SFJP_SFSW_DZ                                                26
C -                                                                       26
      REAL SFJP_SFRS_DX                                                   26
      EQUIVALENCE (BANK_SFJP(101),SFJP_SFRS_DX,RE_SFJP_SFRS_DX),          26
     *(NAMES_SFJP(99),NSFJP_SFRS_DX)                                      26
      EQUIVALENCE (COMMEN_SFJP(99),CSFJP_SFRS_DX),(MAP_SFJP(1,99),        26
     *MSFJP_SFRS_DX)                                                      26
      CHARACTER CSFJP_SFRS_DX*40/' '/                                     26
      CHARACTER NSFJP_SFRS_DX*12/'R.SFRS_DX'/                             26
      INTEGER MSFJP_SFRS_DX(2)/1,1/                                       26
      REAL RE_SFJP_SFRS_DX                                                26
C -                                                                       26
      REAL SFJP_SFRS_DY                                                   26
      EQUIVALENCE (BANK_SFJP(102),SFJP_SFRS_DY,RE_SFJP_SFRS_DY),          26
     *(NAMES_SFJP(100),NSFJP_SFRS_DY)                                     26
      EQUIVALENCE (COMMEN_SFJP(100),CSFJP_SFRS_DY),(MAP_SFJP(1,100),      26
     *MSFJP_SFRS_DY)                                                      26
      CHARACTER CSFJP_SFRS_DY*40/' '/                                     26
      CHARACTER NSFJP_SFRS_DY*12/'R.SFRS_DY'/                             26
      INTEGER MSFJP_SFRS_DY(2)/1,1/                                       26
      REAL RE_SFJP_SFRS_DY                                                26
C -                                                                       26
      REAL SFJP_SFRS_PX                                                   26
      EQUIVALENCE (BANK_SFJP(103),SFJP_SFRS_PX,RE_SFJP_SFRS_PX),          26
     *(NAMES_SFJP(101),NSFJP_SFRS_PX)                                     26
      EQUIVALENCE (COMMEN_SFJP(101),CSFJP_SFRS_PX),(MAP_SFJP(1,101),      26
     *MSFJP_SFRS_PX)                                                      26
      CHARACTER CSFJP_SFRS_PX*40/' '/                                     26
      CHARACTER NSFJP_SFRS_PX*12/'R.SFRS_PX'/                             26
      INTEGER MSFJP_SFRS_PX(2)/1,1/                                       26
      REAL RE_SFJP_SFRS_PX                                                26
C -                                                                       26
      REAL SFJP_SFRS_PY                                                   26
      EQUIVALENCE (BANK_SFJP(104),SFJP_SFRS_PY,RE_SFJP_SFRS_PY),          26
     *(NAMES_SFJP(102),NSFJP_SFRS_PY)                                     26
      EQUIVALENCE (COMMEN_SFJP(102),CSFJP_SFRS_PY),(MAP_SFJP(1,102),      26
     *MSFJP_SFRS_PY)                                                      26
      CHARACTER CSFJP_SFRS_PY*40/' '/                                     26
      CHARACTER NSFJP_SFRS_PY*12/'R.SFRS_PY'/                             26
      INTEGER MSFJP_SFRS_PY(2)/1,1/                                       26
      REAL RE_SFJP_SFRS_PY                                                26
C -                                                                       26
      REAL SFJP_SFSM_LL                                                   26
      EQUIVALENCE (BANK_SFJP(105),SFJP_SFSM_LL,RE_SFJP_SFSM_LL),          26
     *(NAMES_SFJP(103),NSFJP_SFSM_LL)                                     26
      EQUIVALENCE (COMMEN_SFJP(103),CSFJP_SFSM_LL),(MAP_SFJP(1,103),      26
     *MSFJP_SFSM_LL)                                                      26
      CHARACTER CSFJP_SFSM_LL*40/' '/                                     26
      CHARACTER NSFJP_SFSM_LL*12/'R.SFSM_LL'/                             26
      INTEGER MSFJP_SFSM_LL(2)/1,1/                                       26
      REAL RE_SFJP_SFSM_LL                                                26
C -                                                                       26
      REAL SFJP_SFFX_DX                                                   26
      EQUIVALENCE (BANK_SFJP(106),SFJP_SFFX_DX,RE_SFJP_SFFX_DX),          26
     *(NAMES_SFJP(104),NSFJP_SFFX_DX)                                     26
      EQUIVALENCE (COMMEN_SFJP(104),CSFJP_SFFX_DX),(MAP_SFJP(1,104),      26
     *MSFJP_SFFX_DX)                                                      26
      CHARACTER CSFJP_SFFX_DX*40/' '/                                     26
      CHARACTER NSFJP_SFFX_DX*12/'R.SFFX_DX'/                             26
      INTEGER MSFJP_SFFX_DX(2)/1,1/                                       26
      REAL RE_SFJP_SFFX_DX                                                26
C -                                                                       26
      REAL SFJP_SFFX_DYZ                                                  26
      EQUIVALENCE (BANK_SFJP(107),SFJP_SFFX_DYZ,RE_SFJP_SFFX_DYZ),        26
     *(NAMES_SFJP(105),NSFJP_SFFX_DYZ)                                    26
      EQUIVALENCE (COMMEN_SFJP(105),CSFJP_SFFX_DYZ),(MAP_SFJP(1,105),     26
     *MSFJP_SFFX_DYZ)                                                     26
      CHARACTER CSFJP_SFFX_DYZ*40/' '/                                    26
      CHARACTER NSFJP_SFFX_DYZ*12/'R.SFFX_DYZ'/                           26
      INTEGER MSFJP_SFFX_DYZ(2)/1,1/                                      26
      REAL RE_SFJP_SFFX_DYZ                                               26
C -                                                                       26
      REAL SFJP_SFPI_RMIN                                                 26
      EQUIVALENCE (BANK_SFJP(108),SFJP_SFPI_RMIN,RE_SFJP_SFPI_RMIN),      26
     *(NAMES_SFJP(106),NSFJP_SFPI_RMIN)                                   26
      EQUIVALENCE (COMMEN_SFJP(106),CSFJP_SFPI_RMIN),(MAP_SFJP(1,106),    26
     *MSFJP_SFPI_RMIN)                                                    26
      CHARACTER CSFJP_SFPI_RMIN*40/' '/                                   26
      CHARACTER NSFJP_SFPI_RMIN*12/'R.SFPI_RMIN'/                         26
      INTEGER MSFJP_SFPI_RMIN(2)/1,1/                                     26
      REAL RE_SFJP_SFPI_RMIN                                              26
C -                                                                       26
      REAL SFJP_SFPI_RMAX                                                 26
      EQUIVALENCE (BANK_SFJP(109),SFJP_SFPI_RMAX,RE_SFJP_SFPI_RMAX),      26
     *(NAMES_SFJP(107),NSFJP_SFPI_RMAX)                                   26
      EQUIVALENCE (COMMEN_SFJP(107),CSFJP_SFPI_RMAX),(MAP_SFJP(1,107),    26
     *MSFJP_SFPI_RMAX)                                                    26
      CHARACTER CSFJP_SFPI_RMAX*40/' '/                                   26
      CHARACTER NSFJP_SFPI_RMAX*12/'R.SFPI_RMAX'/                         26
      INTEGER MSFJP_SFPI_RMAX(2)/1,1/                                     26
      REAL RE_SFJP_SFPI_RMAX                                              26
C -                                                                       26
      REAL SFJP_SFPI_PX                                                   26
      EQUIVALENCE (BANK_SFJP(110),SFJP_SFPI_PX,RE_SFJP_SFPI_PX),          26
     *(NAMES_SFJP(108),NSFJP_SFPI_PX)                                     26
      EQUIVALENCE (COMMEN_SFJP(108),CSFJP_SFPI_PX),(MAP_SFJP(1,108),      26
     *MSFJP_SFPI_PX)                                                      26
      CHARACTER CSFJP_SFPI_PX*40/' '/                                     26
      CHARACTER NSFJP_SFPI_PX*12/'R.SFPI_PX'/                             26
      INTEGER MSFJP_SFPI_PX(2)/1,1/                                       26
      REAL RE_SFJP_SFPI_PX                                                26
C -                                                                       26
      REAL SFJP_SFPI_PY1                                                  26
      EQUIVALENCE (BANK_SFJP(111),SFJP_SFPI_PY1,RE_SFJP_SFPI_PY1),        26
     *(NAMES_SFJP(109),NSFJP_SFPI_PY1)                                    26
      EQUIVALENCE (COMMEN_SFJP(109),CSFJP_SFPI_PY1),(MAP_SFJP(1,109),     26
     *MSFJP_SFPI_PY1)                                                     26
      CHARACTER CSFJP_SFPI_PY1*40/' '/                                    26
      CHARACTER NSFJP_SFPI_PY1*12/'R.SFPI_PY1'/                           26
      INTEGER MSFJP_SFPI_PY1(2)/1,1/                                      26
      REAL RE_SFJP_SFPI_PY1                                               26
C -                                                                       26
      REAL SFJP_SFPI_PY2                                                  26
      EQUIVALENCE (BANK_SFJP(112),SFJP_SFPI_PY2,RE_SFJP_SFPI_PY2),        26
     *(NAMES_SFJP(110),NSFJP_SFPI_PY2)                                    26
      EQUIVALENCE (COMMEN_SFJP(110),CSFJP_SFPI_PY2),(MAP_SFJP(1,110),     26
     *MSFJP_SFPI_PY2)                                                     26
      CHARACTER CSFJP_SFPI_PY2*40/' '/                                    26
      CHARACTER NSFJP_SFPI_PY2*12/'R.SFPI_PY2'/                           26
      INTEGER MSFJP_SFPI_PY2(2)/1,1/                                      26
      REAL RE_SFJP_SFPI_PY2                                               26
C -                                                                       26
      REAL SFJP_SFPI_PZ                                                   26
      EQUIVALENCE (BANK_SFJP(113),SFJP_SFPI_PZ,RE_SFJP_SFPI_PZ),          26
     *(NAMES_SFJP(111),NSFJP_SFPI_PZ)                                     26
      EQUIVALENCE (COMMEN_SFJP(111),CSFJP_SFPI_PZ),(MAP_SFJP(1,111),      26
     *MSFJP_SFPI_PZ)                                                      26
      CHARACTER CSFJP_SFPI_PZ*40/' '/                                     26
      CHARACTER NSFJP_SFPI_PZ*12/'R.SFPI_PZ'/                             26
      INTEGER MSFJP_SFPI_PZ(2)/1,1/                                       26
      REAL RE_SFJP_SFPI_PZ                                                26
C -                                                                       26
      REAL SFJP_SFPJ_DX1                                                  26
      EQUIVALENCE (BANK_SFJP(114),SFJP_SFPJ_DX1,RE_SFJP_SFPJ_DX1),        26
     *(NAMES_SFJP(112),NSFJP_SFPJ_DX1)                                    26
      EQUIVALENCE (COMMEN_SFJP(112),CSFJP_SFPJ_DX1),(MAP_SFJP(1,112),     26
     *MSFJP_SFPJ_DX1)                                                     26
      CHARACTER CSFJP_SFPJ_DX1*40/' '/                                    26
      CHARACTER NSFJP_SFPJ_DX1*12/'R.SFPJ_DX1'/                           26
      INTEGER MSFJP_SFPJ_DX1(2)/1,1/                                      26
      REAL RE_SFJP_SFPJ_DX1                                               26
C -                                                                       26
      REAL SFJP_SFPJ_DX2                                                  26
      EQUIVALENCE (BANK_SFJP(115),SFJP_SFPJ_DX2,RE_SFJP_SFPJ_DX2),        26
     *(NAMES_SFJP(113),NSFJP_SFPJ_DX2)                                    26
      EQUIVALENCE (COMMEN_SFJP(113),CSFJP_SFPJ_DX2),(MAP_SFJP(1,113),     26
     *MSFJP_SFPJ_DX2)                                                     26
      CHARACTER CSFJP_SFPJ_DX2*40/' '/                                    26
      CHARACTER NSFJP_SFPJ_DX2*12/'R.SFPJ_DX2'/                           26
      INTEGER MSFJP_SFPJ_DX2(2)/1,1/                                      26
      REAL RE_SFJP_SFPJ_DX2                                               26
C -                                                                       26
      REAL SFJP_SFPJ_DY                                                   26
      EQUIVALENCE (BANK_SFJP(116),SFJP_SFPJ_DY,RE_SFJP_SFPJ_DY),          26
     *(NAMES_SFJP(114),NSFJP_SFPJ_DY)                                     26
      EQUIVALENCE (COMMEN_SFJP(114),CSFJP_SFPJ_DY),(MAP_SFJP(1,114),      26
     *MSFJP_SFPJ_DY)                                                      26
      CHARACTER CSFJP_SFPJ_DY*40/' '/                                     26
      CHARACTER NSFJP_SFPJ_DY*12/'R.SFPJ_DY'/                             26
      INTEGER MSFJP_SFPJ_DY(2)/1,1/                                       26
      REAL RE_SFJP_SFPJ_DY                                                26
C -                                                                       26
      REAL SFJP_SFPJ_DZ                                                   26
      EQUIVALENCE (BANK_SFJP(117),SFJP_SFPJ_DZ,RE_SFJP_SFPJ_DZ),          26
     *(NAMES_SFJP(115),NSFJP_SFPJ_DZ)                                     26
      EQUIVALENCE (COMMEN_SFJP(115),CSFJP_SFPJ_DZ),(MAP_SFJP(1,115),      26
     *MSFJP_SFPJ_DZ)                                                      26
      CHARACTER CSFJP_SFPJ_DZ*40/' '/                                     26
      CHARACTER NSFJP_SFPJ_DZ*12/'R.SFPJ_DZ'/                             26
      INTEGER MSFJP_SFPJ_DZ(2)/1,1/                                       26
      REAL RE_SFJP_SFPJ_DZ                                                26
C -                                                                       26
      REAL SFJP_SFAA_DX                                                   26
      EQUIVALENCE (BANK_SFJP(118),SFJP_SFAA_DX,RE_SFJP_SFAA_DX),          26
     *(NAMES_SFJP(116),NSFJP_SFAA_DX)                                     26
      EQUIVALENCE (COMMEN_SFJP(116),CSFJP_SFAA_DX),(MAP_SFJP(1,116),      26
     *MSFJP_SFAA_DX)                                                      26
      CHARACTER CSFJP_SFAA_DX*40/' '/                                     26
      CHARACTER NSFJP_SFAA_DX*12/'R.SFAA_DX'/                             26
      INTEGER MSFJP_SFAA_DX(2)/1,1/                                       26
      REAL RE_SFJP_SFAA_DX                                                26
C -                                                                       26
      REAL SFJP_SFAA_DY                                                   26
      EQUIVALENCE (BANK_SFJP(119),SFJP_SFAA_DY,RE_SFJP_SFAA_DY),          26
     *(NAMES_SFJP(117),NSFJP_SFAA_DY)                                     26
      EQUIVALENCE (COMMEN_SFJP(117),CSFJP_SFAA_DY),(MAP_SFJP(1,117),      26
     *MSFJP_SFAA_DY)                                                      26
      CHARACTER CSFJP_SFAA_DY*40/' '/                                     26
      CHARACTER NSFJP_SFAA_DY*12/'R.SFAA_DY'/                             26
      INTEGER MSFJP_SFAA_DY(2)/1,1/                                       26
      REAL RE_SFJP_SFAA_DY                                                26
C -                                                                       26
      REAL SFJP_SFAA_DZ                                                   26
      EQUIVALENCE (BANK_SFJP(120),SFJP_SFAA_DZ,RE_SFJP_SFAA_DZ),          26
     *(NAMES_SFJP(118),NSFJP_SFAA_DZ)                                     26
      EQUIVALENCE (COMMEN_SFJP(118),CSFJP_SFAA_DZ),(MAP_SFJP(1,118),      26
     *MSFJP_SFAA_DZ)                                                      26
      CHARACTER CSFJP_SFAA_DZ*40/' '/                                     26
      CHARACTER NSFJP_SFAA_DZ*12/'R.SFAA_DZ'/                             26
      INTEGER MSFJP_SFAA_DZ(2)/1,1/                                       26
      REAL RE_SFJP_SFAA_DZ                                                26
C -                                                                       26
      REAL SFJP_SFAA_PX1                                                  26
      EQUIVALENCE (BANK_SFJP(121),SFJP_SFAA_PX1,RE_SFJP_SFAA_PX1),        26
     *(NAMES_SFJP(119),NSFJP_SFAA_PX1)                                    26
      EQUIVALENCE (COMMEN_SFJP(119),CSFJP_SFAA_PX1),(MAP_SFJP(1,119),     26
     *MSFJP_SFAA_PX1)                                                     26
      CHARACTER CSFJP_SFAA_PX1*40/' '/                                    26
      CHARACTER NSFJP_SFAA_PX1*12/'R.SFAA_PX1'/                           26
      INTEGER MSFJP_SFAA_PX1(2)/1,1/                                      26
      REAL RE_SFJP_SFAA_PX1                                               26
C -                                                                       26
      REAL SFJP_SFAA_PX2                                                  26
      EQUIVALENCE (BANK_SFJP(122),SFJP_SFAA_PX2,RE_SFJP_SFAA_PX2),        26
     *(NAMES_SFJP(120),NSFJP_SFAA_PX2)                                    26
      EQUIVALENCE (COMMEN_SFJP(120),CSFJP_SFAA_PX2),(MAP_SFJP(1,120),     26
     *MSFJP_SFAA_PX2)                                                     26
      CHARACTER CSFJP_SFAA_PX2*40/' '/                                    26
      CHARACTER NSFJP_SFAA_PX2*12/'R.SFAA_PX2'/                           26
      INTEGER MSFJP_SFAA_PX2(2)/1,1/                                      26
      REAL RE_SFJP_SFAA_PX2                                               26
C -                                                                       26
      REAL SFJP_SFAA_PX3                                                  26
      EQUIVALENCE (BANK_SFJP(123),SFJP_SFAA_PX3,RE_SFJP_SFAA_PX3),        26
     *(NAMES_SFJP(121),NSFJP_SFAA_PX3)                                    26
      EQUIVALENCE (COMMEN_SFJP(121),CSFJP_SFAA_PX3),(MAP_SFJP(1,121),     26
     *MSFJP_SFAA_PX3)                                                     26
      CHARACTER CSFJP_SFAA_PX3*40/' '/                                    26
      CHARACTER NSFJP_SFAA_PX3*12/'R.SFAA_PX3'/                           26
      INTEGER MSFJP_SFAA_PX3(2)/1,1/                                      26
      REAL RE_SFJP_SFAA_PX3                                               26
C -                                                                       26
      REAL SFJP_SFAA_PZ1                                                  26
      EQUIVALENCE (BANK_SFJP(124),SFJP_SFAA_PZ1,RE_SFJP_SFAA_PZ1),        26
     *(NAMES_SFJP(122),NSFJP_SFAA_PZ1)                                    26
      EQUIVALENCE (COMMEN_SFJP(122),CSFJP_SFAA_PZ1),(MAP_SFJP(1,122),     26
     *MSFJP_SFAA_PZ1)                                                     26
      CHARACTER CSFJP_SFAA_PZ1*40/' '/                                    26
      CHARACTER NSFJP_SFAA_PZ1*12/'R.SFAA_PZ1'/                           26
      INTEGER MSFJP_SFAA_PZ1(2)/1,1/                                      26
      REAL RE_SFJP_SFAA_PZ1                                               26
C -                                                                       26
      REAL SFJP_SFAA_PZ2                                                  26
      EQUIVALENCE (BANK_SFJP(125),SFJP_SFAA_PZ2,RE_SFJP_SFAA_PZ2),        26
     *(NAMES_SFJP(123),NSFJP_SFAA_PZ2)                                    26
      EQUIVALENCE (COMMEN_SFJP(123),CSFJP_SFAA_PZ2),(MAP_SFJP(1,123),     26
     *MSFJP_SFAA_PZ2)                                                     26
      CHARACTER CSFJP_SFAA_PZ2*40/' '/                                    26
      CHARACTER NSFJP_SFAA_PZ2*12/'R.SFAA_PZ2'/                           26
      INTEGER MSFJP_SFAA_PZ2(2)/1,1/                                      26
      REAL RE_SFJP_SFAA_PZ2                                               26
C -                                                                       26
      REAL SFJP_SFAA_PZ3                                                  26
      EQUIVALENCE (BANK_SFJP(126),SFJP_SFAA_PZ3,RE_SFJP_SFAA_PZ3),        26
     *(NAMES_SFJP(124),NSFJP_SFAA_PZ3)                                    26
      EQUIVALENCE (COMMEN_SFJP(124),CSFJP_SFAA_PZ3),(MAP_SFJP(1,124),     26
     *MSFJP_SFAA_PZ3)                                                     26
      CHARACTER CSFJP_SFAA_PZ3*40/' '/                                    26
      CHARACTER NSFJP_SFAA_PZ3*12/'R.SFAA_PZ3'/                           26
      INTEGER MSFJP_SFAA_PZ3(2)/1,1/                                      26
      REAL RE_SFJP_SFAA_PZ3                                               26
C -                                                                       26
      REAL SFJP_SFSD_DX                                                   26
      EQUIVALENCE (BANK_SFJP(127),SFJP_SFSD_DX,RE_SFJP_SFSD_DX),          26
     *(NAMES_SFJP(125),NSFJP_SFSD_DX)                                     26
      EQUIVALENCE (COMMEN_SFJP(125),CSFJP_SFSD_DX),(MAP_SFJP(1,125),      26
     *MSFJP_SFSD_DX)                                                      26
      CHARACTER CSFJP_SFSD_DX*40/' '/                                     26
      CHARACTER NSFJP_SFSD_DX*12/'R.SFSD_DX'/                             26
      INTEGER MSFJP_SFSD_DX(2)/1,1/                                       26
      REAL RE_SFJP_SFSD_DX                                                26
C -                                                                       26
      REAL SFJP_SFSD_DY                                                   26
      EQUIVALENCE (BANK_SFJP(128),SFJP_SFSD_DY,RE_SFJP_SFSD_DY),          26
     *(NAMES_SFJP(126),NSFJP_SFSD_DY)                                     26
      EQUIVALENCE (COMMEN_SFJP(126),CSFJP_SFSD_DY),(MAP_SFJP(1,126),      26
     *MSFJP_SFSD_DY)                                                      26
      CHARACTER CSFJP_SFSD_DY*40/' '/                                     26
      CHARACTER NSFJP_SFSD_DY*12/'R.SFSD_DY'/                             26
      INTEGER MSFJP_SFSD_DY(2)/1,1/                                       26
      REAL RE_SFJP_SFSD_DY                                                26
C -                                                                       26
      REAL SFJP_SFSD_DZ                                                   26
      EQUIVALENCE (BANK_SFJP(129),SFJP_SFSD_DZ,RE_SFJP_SFSD_DZ),          26
     *(NAMES_SFJP(127),NSFJP_SFSD_DZ)                                     26
      EQUIVALENCE (COMMEN_SFJP(127),CSFJP_SFSD_DZ),(MAP_SFJP(1,127),      26
     *MSFJP_SFSD_DZ)                                                      26
      CHARACTER CSFJP_SFSD_DZ*40/' '/                                     26
      CHARACTER NSFJP_SFSD_DZ*12/'R.SFSD_DZ'/                             26
      INTEGER MSFJP_SFSD_DZ(2)/1,1/                                       26
      REAL RE_SFJP_SFSD_DZ                                                26
C -                                                                       26
      REAL SFJP_SFLA_PX                                                   26
      EQUIVALENCE (BANK_SFJP(130),SFJP_SFLA_PX,RE_SFJP_SFLA_PX),          26
     *(NAMES_SFJP(128),NSFJP_SFLA_PX)                                     26
      EQUIVALENCE (COMMEN_SFJP(128),CSFJP_SFLA_PX),(MAP_SFJP(1,128),      26
     *MSFJP_SFLA_PX)                                                      26
      CHARACTER CSFJP_SFLA_PX*40/' '/                                     26
      CHARACTER NSFJP_SFLA_PX*12/'R.SFLA_PX'/                             26
      INTEGER MSFJP_SFLA_PX(2)/1,1/                                       26
      REAL RE_SFJP_SFLA_PX                                                26
C -                                                                       26
      REAL SFJP_SFLA_PY                                                   26
      EQUIVALENCE (BANK_SFJP(131),SFJP_SFLA_PY,RE_SFJP_SFLA_PY),          26
     *(NAMES_SFJP(129),NSFJP_SFLA_PY)                                     26
      EQUIVALENCE (COMMEN_SFJP(129),CSFJP_SFLA_PY),(MAP_SFJP(1,129),      26
     *MSFJP_SFLA_PY)                                                      26
      CHARACTER CSFJP_SFLA_PY*40/' '/                                     26
      CHARACTER NSFJP_SFLA_PY*12/'R.SFLA_PY'/                             26
      INTEGER MSFJP_SFLA_PY(2)/1,1/                                       26
      REAL RE_SFJP_SFLA_PY                                                26
C -                                                                       26
      REAL SFJP_SFLC_PX                                                   26
      EQUIVALENCE (BANK_SFJP(132),SFJP_SFLC_PX,RE_SFJP_SFLC_PX),          26
     *(NAMES_SFJP(130),NSFJP_SFLC_PX)                                     26
      EQUIVALENCE (COMMEN_SFJP(130),CSFJP_SFLC_PX),(MAP_SFJP(1,130),      26
     *MSFJP_SFLC_PX)                                                      26
      CHARACTER CSFJP_SFLC_PX*40/' '/                                     26
      CHARACTER NSFJP_SFLC_PX*12/'R.SFLC_PX'/                             26
      INTEGER MSFJP_SFLC_PX(2)/1,1/                                       26
      REAL RE_SFJP_SFLC_PX                                                26
C -                                                                       26
      REAL SFJP_SFLC_PY                                                   26
      EQUIVALENCE (BANK_SFJP(133),SFJP_SFLC_PY,RE_SFJP_SFLC_PY),          26
     *(NAMES_SFJP(131),NSFJP_SFLC_PY)                                     26
      EQUIVALENCE (COMMEN_SFJP(131),CSFJP_SFLC_PY),(MAP_SFJP(1,131),      26
     *MSFJP_SFLC_PY)                                                      26
      CHARACTER CSFJP_SFLC_PY*40/' '/                                     26
      CHARACTER NSFJP_SFLC_PY*12/'R.SFLC_PY'/                             26
      INTEGER MSFJP_SFLC_PY(2)/1,1/                                       26
      REAL RE_SFJP_SFLC_PY                                                26
C -                                                                       26
      REAL SFJP_SFES_PX                                                   26
      EQUIVALENCE (BANK_SFJP(134),SFJP_SFES_PX,RE_SFJP_SFES_PX),          26
     *(NAMES_SFJP(132),NSFJP_SFES_PX)                                     26
      EQUIVALENCE (COMMEN_SFJP(132),CSFJP_SFES_PX),(MAP_SFJP(1,132),      26
     *MSFJP_SFES_PX)                                                      26
      CHARACTER CSFJP_SFES_PX*40/' '/                                     26
      CHARACTER NSFJP_SFES_PX*12/'R.SFES_PX'/                             26
      INTEGER MSFJP_SFES_PX(2)/1,1/                                       26
      REAL RE_SFJP_SFES_PX                                                26
C -                                                                       26
      REAL SFJP_SFES_PY                                                   26
      EQUIVALENCE (BANK_SFJP(135),SFJP_SFES_PY,RE_SFJP_SFES_PY),          26
     *(NAMES_SFJP(133),NSFJP_SFES_PY)                                     26
      EQUIVALENCE (COMMEN_SFJP(133),CSFJP_SFES_PY),(MAP_SFJP(1,133),      26
     *MSFJP_SFES_PY)                                                      26
      CHARACTER CSFJP_SFES_PY*40/' '/                                     26
      CHARACTER NSFJP_SFES_PY*12/'R.SFES_PY'/                             26
      INTEGER MSFJP_SFES_PY(2)/1,1/                                       26
      REAL RE_SFJP_SFES_PY                                                26
C -                                                                       26
      REAL SFJP_SFES_PZ                                                   26
      EQUIVALENCE (BANK_SFJP(136),SFJP_SFES_PZ,RE_SFJP_SFES_PZ),          26
     *(NAMES_SFJP(134),NSFJP_SFES_PZ)                                     26
      EQUIVALENCE (COMMEN_SFJP(134),CSFJP_SFES_PZ),(MAP_SFJP(1,134),      26
     *MSFJP_SFES_PZ)                                                      26
      CHARACTER CSFJP_SFES_PZ*40/' '/                                     26
      CHARACTER NSFJP_SFES_PZ*12/'R.SFES_PZ'/                             26
      INTEGER MSFJP_SFES_PZ(2)/1,1/                                       26
      REAL RE_SFJP_SFES_PZ                                                26
C -                                                                       26
      REAL SFJP_SFEB_PX                                                   26
      EQUIVALENCE (BANK_SFJP(137),SFJP_SFEB_PX,RE_SFJP_SFEB_PX),          26
     *(NAMES_SFJP(135),NSFJP_SFEB_PX)                                     26
      EQUIVALENCE (COMMEN_SFJP(135),CSFJP_SFEB_PX),(MAP_SFJP(1,135),      26
     *MSFJP_SFEB_PX)                                                      26
      CHARACTER CSFJP_SFEB_PX*40/' '/                                     26
      CHARACTER NSFJP_SFEB_PX*12/'R.SFEB_PX'/                             26
      INTEGER MSFJP_SFEB_PX(2)/1,1/                                       26
      REAL RE_SFJP_SFEB_PX                                                26
C -                                                                       26
      REAL SFJP_SFEB_PY                                                   26
      EQUIVALENCE (BANK_SFJP(138),SFJP_SFEB_PY,RE_SFJP_SFEB_PY),          26
     *(NAMES_SFJP(136),NSFJP_SFEB_PY)                                     26
      EQUIVALENCE (COMMEN_SFJP(136),CSFJP_SFEB_PY),(MAP_SFJP(1,136),      26
     *MSFJP_SFEB_PY)                                                      26
      CHARACTER CSFJP_SFEB_PY*40/' '/                                     26
      CHARACTER NSFJP_SFEB_PY*12/'R.SFEB_PY'/                             26
      INTEGER MSFJP_SFEB_PY(2)/1,1/                                       26
      REAL RE_SFJP_SFEB_PY                                                26
C -                                                                       26
      INTEGER LEN_SFJP,LLEN_SFJP,LINK_SFJP                                26
      PARAMETER (LEN_SFJP=138)                                            26
      PARAMETER (LLEN_SFJP=136)                                           26
      INTEGER FLAG_SFJP/0/,MAP_SFJP(2,LLEN_SFJP),IDNUM_SFJP(10)           26
      CHARACTER NAMES_SFJP(LLEN_SFJP)*12,COMMEN_SFJP(LLEN_SFJP)*40        26
      DATA IDNUM_SFJP/10*0/                                               26
      REAL BANK_SFJP(LEN_SFJP)                                            26
      CHARACTER*60 BPATH_SFJP/' '/                                        26
      SAVE FLAG_SFJP,MAP_SFJP,IDNUM_SFJP,NAMES_SFJP,COMMEN_SFJP,          26
     *BPATH_SFJP                                                          26
      DATA LINK_SFJP/0/,BANK_SFJP/LEN_SFJP*0/                             26
C *                                                                       26
C -                                                                       27
      REAL SFPB_HHIGHT                                                    27
      EQUIVALENCE (BANK_SFPB(3),SFPB_HHIGHT,RE_SFPB_HHIGHT),              27
     *(NAMES_SFPB(1),NSFPB_HHIGHT)                                        27
      EQUIVALENCE (COMMEN_SFPB(1),CSFPB_HHIGHT),(MAP_SFPB(1,1),           27
     *MSFPB_HHIGHT)                                                       27
      CHARACTER CSFPB_HHIGHT*40/' '/                                      27
      CHARACTER NSFPB_HHIGHT*12/'R.HHIGHT'/                               27
      INTEGER MSFPB_HHIGHT(2)/1,1/                                        27
      REAL RE_SFPB_HHIGHT                                                 27
C -                                                                       27
      REAL SFPB_KHIGHT                                                    27
      EQUIVALENCE (BANK_SFPB(4),SFPB_KHIGHT,RE_SFPB_KHIGHT),              27
     *(NAMES_SFPB(2),NSFPB_KHIGHT)                                        27
      EQUIVALENCE (COMMEN_SFPB(2),CSFPB_KHIGHT),(MAP_SFPB(1,2),           27
     *MSFPB_KHIGHT)                                                       27
      CHARACTER CSFPB_KHIGHT*40/' '/                                      27
      CHARACTER NSFPB_KHIGHT*12/'R.KHIGHT'/                               27
      INTEGER MSFPB_KHIGHT(2)/1,1/                                        27
      REAL RE_SFPB_KHIGHT                                                 27
C -                                                                       27
      REAL SFPB_HBASE                                                     27
      EQUIVALENCE (BANK_SFPB(5),SFPB_HBASE,RE_SFPB_HBASE),                27
     *(NAMES_SFPB(3),NSFPB_HBASE)                                         27
      EQUIVALENCE (COMMEN_SFPB(3),CSFPB_HBASE),(MAP_SFPB(1,3),            27
     *MSFPB_HBASE)                                                        27
      CHARACTER CSFPB_HBASE*40/' '/                                       27
      CHARACTER NSFPB_HBASE*12/'R.HBASE'/                                 27
      INTEGER MSFPB_HBASE(2)/1,1/                                         27
      REAL RE_SFPB_HBASE                                                  27
C -                                                                       27
      REAL SFPB_KBASE                                                     27
      EQUIVALENCE (BANK_SFPB(6),SFPB_KBASE,RE_SFPB_KBASE),                27
     *(NAMES_SFPB(4),NSFPB_KBASE)                                         27
      EQUIVALENCE (COMMEN_SFPB(4),CSFPB_KBASE),(MAP_SFPB(1,4),            27
     *MSFPB_KBASE)                                                        27
      CHARACTER CSFPB_KBASE*40/' '/                                       27
      CHARACTER NSFPB_KBASE*12/'R.KBASE'/                                 27
      INTEGER MSFPB_KBASE(2)/1,1/                                         27
      REAL RE_SFPB_KBASE                                                  27
C -                                                                       27
      REAL SFPB_FSIZE                                                     27
      EQUIVALENCE (BANK_SFPB(7),SFPB_FSIZE,RE_SFPB_FSIZE),                27
     *(NAMES_SFPB(5),NSFPB_FSIZE)                                         27
      EQUIVALENCE (COMMEN_SFPB(5),CSFPB_FSIZE),(MAP_SFPB(1,5),            27
     *MSFPB_FSIZE)                                                        27
      CHARACTER CSFPB_FSIZE*40/' '/                                       27
      CHARACTER NSFPB_FSIZE*12/'R.FSIZE'/                                 27
      INTEGER MSFPB_FSIZE(2)/1,1/                                         27
      REAL RE_SFPB_FSIZE                                                  27
C -                                                                       27
      REAL SFPB_ZCOOR                                                     27
      EQUIVALENCE (BANK_SFPB(8),SFPB_ZCOOR,RE_SFPB_ZCOOR),                27
     *(NAMES_SFPB(6),NSFPB_ZCOOR)                                         27
      EQUIVALENCE (COMMEN_SFPB(6),CSFPB_ZCOOR),(MAP_SFPB(1,6),            27
     *MSFPB_ZCOOR)                                                        27
      CHARACTER CSFPB_ZCOOR*40/' '/                                       27
      CHARACTER NSFPB_ZCOOR*12/'R.ZCOOR'/                                 27
      INTEGER MSFPB_ZCOOR(2)/1,1/                                         27
      REAL RE_SFPB_ZCOOR                                                  27
C -                                                                       27
      INTEGER LEN_SFPB,LLEN_SFPB,LINK_SFPB                                27
      PARAMETER (LEN_SFPB=8)                                              27
      PARAMETER (LLEN_SFPB=6)                                             27
      INTEGER FLAG_SFPB/0/,MAP_SFPB(2,LLEN_SFPB),IDNUM_SFPB(10)           27
      CHARACTER NAMES_SFPB(LLEN_SFPB)*12,COMMEN_SFPB(LLEN_SFPB)*40        27
      DATA IDNUM_SFPB/10*0/                                               27
      REAL BANK_SFPB(LEN_SFPB)                                            27
      CHARACTER*60 BPATH_SFPB/' '/                                        27
      SAVE FLAG_SFPB,MAP_SFPB,IDNUM_SFPB,NAMES_SFPB,COMMEN_SFPB,          27
     *BPATH_SFPB                                                          27
      DATA LINK_SFPB/0/,BANK_SFPB/LEN_SFPB*0/                             27
C *                                                                       27
C -                                                                       30
      REAL SFPA_VERSION                                                   30
      EQUIVALENCE (BANK_SFPA(3),SFPA_VERSION,RE_SFPA_VERSION),            30
     *(NAMES_SFPA(1),NSFPA_VERSION)                                       30
      EQUIVALENCE (COMMEN_SFPA(1),CSFPA_VERSION),(MAP_SFPA(1,1),          30
     *MSFPA_VERSION)                                                      30
      CHARACTER CSFPA_VERSION*40/' '/                                     30
      CHARACTER NSFPA_VERSION*12/'R.VERSION'/                             30
      INTEGER MSFPA_VERSION(2)/1,1/                                       30
      REAL RE_SFPA_VERSION                                                30
C -                                                                       30
      REAL SFPA_RMIN                                                      30
      EQUIVALENCE (BANK_SFPA(4),SFPA_RMIN,RE_SFPA_RMIN),(NAMES_SFPA(2),   30
     *NSFPA_RMIN)                                                         30
      EQUIVALENCE (COMMEN_SFPA(2),CSFPA_RMIN),(MAP_SFPA(1,2),             30
     *MSFPA_RMIN)                                                         30
      CHARACTER CSFPA_RMIN*40/' '/                                        30
      CHARACTER NSFPA_RMIN*12/'R.RMIN'/                                   30
      INTEGER MSFPA_RMIN(2)/1,1/                                          30
      REAL RE_SFPA_RMIN                                                   30
C -                                                                       30
      REAL SFPA_RMAX                                                      30
      EQUIVALENCE (BANK_SFPA(5),SFPA_RMAX,RE_SFPA_RMAX),(NAMES_SFPA(3),   30
     *NSFPA_RMAX)                                                         30
      EQUIVALENCE (COMMEN_SFPA(3),CSFPA_RMAX),(MAP_SFPA(1,3),             30
     *MSFPA_RMAX)                                                         30
      CHARACTER CSFPA_RMAX*40/' '/                                        30
      CHARACTER NSFPA_RMAX*12/'R.RMAX'/                                   30
      INTEGER MSFPA_RMAX(2)/1,1/                                          30
      REAL RE_SFPA_RMAX                                                   30
C -                                                                       30
      REAL SFPA_LEN                                                       30
      EQUIVALENCE (BANK_SFPA(6),SFPA_LEN,RE_SFPA_LEN),(NAMES_SFPA(4),     30
     *NSFPA_LEN)                                                          30
      EQUIVALENCE (COMMEN_SFPA(4),CSFPA_LEN),(MAP_SFPA(1,4),MSFPA_LEN)    30
      CHARACTER CSFPA_LEN*40/' '/                                         30
      CHARACTER NSFPA_LEN*12/'R.LEN'/                                     30
      INTEGER MSFPA_LEN(2)/1,1/                                           30
      REAL RE_SFPA_LEN                                                    30
C -                                                                       30
      REAL SFPA_RAD                                                       30
      EQUIVALENCE (BANK_SFPA(7),SFPA_RAD,RE_SFPA_RAD),(NAMES_SFPA(5),     30
     *NSFPA_RAD)                                                          30
      EQUIVALENCE (COMMEN_SFPA(5),CSFPA_RAD),(MAP_SFPA(1,5),MSFPA_RAD)    30
      CHARACTER CSFPA_RAD*40/' '/                                         30
      CHARACTER NSFPA_RAD*12/'R.RAD'/                                     30
      INTEGER MSFPA_RAD(2)/1,1/                                           30
      REAL RE_SFPA_RAD                                                    30
C -                                                                       30
      REAL SFPA_NSSD                                                      30
      EQUIVALENCE (BANK_SFPA(8),SFPA_NSSD,RE_SFPA_NSSD),(NAMES_SFPA(6),   30
     *NSFPA_NSSD)                                                         30
      EQUIVALENCE (COMMEN_SFPA(6),CSFPA_NSSD),(MAP_SFPA(1,6),             30
     *MSFPA_NSSD)                                                         30
      CHARACTER CSFPA_NSSD*40/' '/                                        30
      CHARACTER NSFPA_NSSD*12/'R.NSSD'/                                   30
      INTEGER MSFPA_NSSD(2)/1,1/                                          30
      REAL RE_SFPA_NSSD                                                   30
C -                                                                       30
      REAL SFPA_DMWID                                                     30
      EQUIVALENCE (BANK_SFPA(9),SFPA_DMWID,RE_SFPA_DMWID),                30
     *(NAMES_SFPA(7),NSFPA_DMWID)                                         30
      EQUIVALENCE (COMMEN_SFPA(7),CSFPA_DMWID),(MAP_SFPA(1,7),            30
     *MSFPA_DMWID)                                                        30
      CHARACTER CSFPA_DMWID*40/' '/                                       30
      CHARACTER NSFPA_DMWID*12/'R.DMWID'/                                 30
      INTEGER MSFPA_DMWID(2)/1,1/                                         30
      REAL RE_SFPA_DMWID                                                  30
C -                                                                       30
      REAL SFPA_DMTHK                                                     30
      EQUIVALENCE (BANK_SFPA(10),SFPA_DMTHK,RE_SFPA_DMTHK),               30
     *(NAMES_SFPA(8),NSFPA_DMTHK)                                         30
      EQUIVALENCE (COMMEN_SFPA(8),CSFPA_DMTHK),(MAP_SFPA(1,8),            30
     *MSFPA_DMTHK)                                                        30
      CHARACTER CSFPA_DMTHK*40/' '/                                       30
      CHARACTER NSFPA_DMTHK*12/'R.DMTHK'/                                 30
      INTEGER MSFPA_DMTHK(2)/1,1/                                         30
      REAL RE_SFPA_DMTHK                                                  30
C -                                                                       30
      REAL SFPA_DMLEN                                                     30
      EQUIVALENCE (BANK_SFPA(11),SFPA_DMLEN,RE_SFPA_DMLEN),               30
     *(NAMES_SFPA(9),NSFPA_DMLEN)                                         30
      EQUIVALENCE (COMMEN_SFPA(9),CSFPA_DMLEN),(MAP_SFPA(1,9),            30
     *MSFPA_DMLEN)                                                        30
      CHARACTER CSFPA_DMLEN*40/' '/                                       30
      CHARACTER NSFPA_DMLEN*12/'R.DMLEN'/                                 30
      INTEGER MSFPA_DMLEN(2)/1,1/                                         30
      REAL RE_SFPA_DMLEN                                                  30
C -                                                                       30
      REAL SFPA_SMWID                                                     30
      EQUIVALENCE (BANK_SFPA(12),SFPA_SMWID,RE_SFPA_SMWID),               30
     *(NAMES_SFPA(10),NSFPA_SMWID)                                        30
      EQUIVALENCE (COMMEN_SFPA(10),CSFPA_SMWID),(MAP_SFPA(1,10),          30
     *MSFPA_SMWID)                                                        30
      CHARACTER CSFPA_SMWID*40/' '/                                       30
      CHARACTER NSFPA_SMWID*12/'R.SMWID'/                                 30
      INTEGER MSFPA_SMWID(2)/1,1/                                         30
      REAL RE_SFPA_SMWID                                                  30
C -                                                                       30
      REAL SFPA_SMTHK                                                     30
      EQUIVALENCE (BANK_SFPA(13),SFPA_SMTHK,RE_SFPA_SMTHK),               30
     *(NAMES_SFPA(11),NSFPA_SMTHK)                                        30
      EQUIVALENCE (COMMEN_SFPA(11),CSFPA_SMTHK),(MAP_SFPA(1,11),          30
     *MSFPA_SMTHK)                                                        30
      CHARACTER CSFPA_SMTHK*40/' '/                                       30
      CHARACTER NSFPA_SMTHK*12/'R.SMTHK'/                                 30
      INTEGER MSFPA_SMTHK(2)/1,1/                                         30
      REAL RE_SFPA_SMTHK                                                  30
C -                                                                       30
      REAL SFPA_SMLEN                                                     30
      EQUIVALENCE (BANK_SFPA(14),SFPA_SMLEN,RE_SFPA_SMLEN),               30
     *(NAMES_SFPA(12),NSFPA_SMLEN)                                        30
      EQUIVALENCE (COMMEN_SFPA(12),CSFPA_SMLEN),(MAP_SFPA(1,12),          30
     *MSFPA_SMLEN)                                                        30
      CHARACTER CSFPA_SMLEN*40/' '/                                       30
      CHARACTER NSFPA_SMLEN*12/'R.SMLEN'/                                 30
      INTEGER MSFPA_SMLEN(2)/1,1/                                         30
      REAL RE_SFPA_SMLEN                                                  30
C -                                                                       30
      REAL SFPA_SSLEN                                                     30
      EQUIVALENCE (BANK_SFPA(15),SFPA_SSLEN,RE_SFPA_SSLEN),               30
     *(NAMES_SFPA(13),NSFPA_SSLEN)                                        30
      EQUIVALENCE (COMMEN_SFPA(13),CSFPA_SSLEN),(MAP_SFPA(1,13),          30
     *MSFPA_SSLEN)                                                        30
      CHARACTER CSFPA_SSLEN*40/' '/                                       30
      CHARACTER NSFPA_SSLEN*12/'R.SSLEN'/                                 30
      INTEGER MSFPA_SSLEN(2)/1,1/                                         30
      REAL RE_SFPA_SSLEN                                                  30
C -                                                                       30
      REAL SFPA_WPLEN                                                     30
      EQUIVALENCE (BANK_SFPA(16),SFPA_WPLEN,RE_SFPA_WPLEN),               30
     *(NAMES_SFPA(14),NSFPA_WPLEN)                                        30
      EQUIVALENCE (COMMEN_SFPA(14),CSFPA_WPLEN),(MAP_SFPA(1,14),          30
     *MSFPA_WPLEN)                                                        30
      CHARACTER CSFPA_WPLEN*40/' '/                                       30
      CHARACTER NSFPA_WPLEN*12/'R.WPLEN'/                                 30
      INTEGER MSFPA_WPLEN(2)/1,1/                                         30
      REAL RE_SFPA_WPLEN                                                  30
C -                                                                       30
      REAL SFPA_SDLEN                                                     30
      EQUIVALENCE (BANK_SFPA(17),SFPA_SDLEN,RE_SFPA_SDLEN),               30
     *(NAMES_SFPA(15),NSFPA_SDLEN)                                        30
      EQUIVALENCE (COMMEN_SFPA(15),CSFPA_SDLEN),(MAP_SFPA(1,15),          30
     *MSFPA_SDLEN)                                                        30
      CHARACTER CSFPA_SDLEN*40/' '/                                       30
      CHARACTER NSFPA_SDLEN*12/'R.SDLEN'/                                 30
      INTEGER MSFPA_SDLEN(2)/1,1/                                         30
      REAL RE_SFPA_SDLEN                                                  30
C -                                                                       30
      REAL SFPA_TILT                                                      30
      EQUIVALENCE (BANK_SFPA(18),SFPA_TILT,RE_SFPA_TILT),                 30
     *(NAMES_SFPA(16),NSFPA_TILT)                                         30
      EQUIVALENCE (COMMEN_SFPA(16),CSFPA_TILT),(MAP_SFPA(1,16),           30
     *MSFPA_TILT)                                                         30
      CHARACTER CSFPA_TILT*40/' '/                                        30
      CHARACTER NSFPA_TILT*12/'R.TILT'/                                   30
      INTEGER MSFPA_TILT(2)/1,1/                                          30
      REAL RE_SFPA_TILT                                                   30
C -                                                                       30
      REAL SFPA_CPRAD                                                     30
      EQUIVALENCE (BANK_SFPA(19),SFPA_CPRAD,RE_SFPA_CPRAD),               30
     *(NAMES_SFPA(17),NSFPA_CPRAD)                                        30
      EQUIVALENCE (COMMEN_SFPA(17),CSFPA_CPRAD),(MAP_SFPA(1,17),          30
     *MSFPA_CPRAD)                                                        30
      CHARACTER CSFPA_CPRAD*40/' '/                                       30
      CHARACTER NSFPA_CPRAD*12/'R.CPRAD'/                                 30
      INTEGER MSFPA_CPRAD(2)/1,1/                                         30
      REAL RE_SFPA_CPRAD                                                  30
C -                                                                       30
      REAL SFPA_CPRAL                                                     30
      EQUIVALENCE (BANK_SFPA(20),SFPA_CPRAL,RE_SFPA_CPRAL),               30
     *(NAMES_SFPA(18),NSFPA_CPRAL)                                        30
      EQUIVALENCE (COMMEN_SFPA(18),CSFPA_CPRAL),(MAP_SFPA(1,18),          30
     *MSFPA_CPRAL)                                                        30
      CHARACTER CSFPA_CPRAL*40/' '/                                       30
      CHARACTER NSFPA_CPRAL*12/'R.CPRAL'/                                 30
      INTEGER MSFPA_CPRAL(2)/1,1/                                         30
      REAL RE_SFPA_CPRAL                                                  30
C -                                                                       30
      REAL SFPA_CFRAD                                                     30
      EQUIVALENCE (BANK_SFPA(21),SFPA_CFRAD,RE_SFPA_CFRAD),               30
     *(NAMES_SFPA(19),NSFPA_CFRAD)                                        30
      EQUIVALENCE (COMMEN_SFPA(19),CSFPA_CFRAD),(MAP_SFPA(1,19),          30
     *MSFPA_CFRAD)                                                        30
      CHARACTER CSFPA_CFRAD*40/' '/                                       30
      CHARACTER NSFPA_CFRAD*12/'R.CFRAD'/                                 30
      INTEGER MSFPA_CFRAD(2)/1,1/                                         30
      REAL RE_SFPA_CFRAD                                                  30
C -                                                                       30
      REAL SFPA_GPTHK                                                     30
      EQUIVALENCE (BANK_SFPA(22),SFPA_GPTHK,RE_SFPA_GPTHK),               30
     *(NAMES_SFPA(20),NSFPA_GPTHK)                                        30
      EQUIVALENCE (COMMEN_SFPA(20),CSFPA_GPTHK),(MAP_SFPA(1,20),          30
     *MSFPA_GPTHK)                                                        30
      CHARACTER CSFPA_GPTHK*40/' '/                                       30
      CHARACTER NSFPA_GPTHK*12/'R.GPTHK'/                                 30
      INTEGER MSFPA_GPTHK(2)/1,1/                                         30
      REAL RE_SFPA_GPTHK                                                  30
C -                                                                       30
      INTEGER SFPA_LADDERMAP(20)                                          30
      EQUIVALENCE (BANK_SFPA(23),SFPA_LADDERMAP,RE_SFPA_LADDERMAP),       30
     *(NAMES_SFPA(21),NSFPA_LADDERMAP)                                    30
      EQUIVALENCE (COMMEN_SFPA(21),CSFPA_LADDERMAP),(MAP_SFPA(1,21),      30
     *MSFPA_LADDERMAP)                                                    30
      CHARACTER CSFPA_LADDERMAP*40/' '/                                   30
      CHARACTER NSFPA_LADDERMAP*12/'I.LADDERMAP'/                         30
      INTEGER MSFPA_LADDERMAP(2)/20,1/                                    30
      REAL RE_SFPA_LADDERMAP                                              30
C -                                                                       30
      REAL SFPA_LADDERANGLE(20)                                           30
      EQUIVALENCE (BANK_SFPA(43),SFPA_LADDERANGLE,RE_SFPA_LADDERANGLE),   30
     *(NAMES_SFPA(22),NSFPA_LADDERANGLE)                                  30
      EQUIVALENCE (COMMEN_SFPA(22),CSFPA_LADDERANGLE),(MAP_SFPA(1,22),    30
     *MSFPA_LADDERANGLE)                                                  30
      CHARACTER CSFPA_LADDERANGLE*40/' '/                                 30
      CHARACTER NSFPA_LADDERANGLE*12/'R.LADDERANGLE'/                     30
      INTEGER MSFPA_LADDERANGLE(2)/20,1/                                  30
      REAL RE_SFPA_LADDERANGLE                                            30
C -                                                                       30
      REAL SFPA_LADDERTILT(20)                                            30
      EQUIVALENCE (BANK_SFPA(63),SFPA_LADDERTILT,RE_SFPA_LADDERTILT),     30
     *(NAMES_SFPA(23),NSFPA_LADDERTILT)                                   30
      EQUIVALENCE (COMMEN_SFPA(23),CSFPA_LADDERTILT),(MAP_SFPA(1,23),     30
     *MSFPA_LADDERTILT)                                                   30
      CHARACTER CSFPA_LADDERTILT*40/' '/                                  30
      CHARACTER NSFPA_LADDERTILT*12/'R.LADDERTILT'/                       30
      INTEGER MSFPA_LADDERTILT(2)/20,1/                                   30
      REAL RE_SFPA_LADDERTILT                                             30
C -                                                                       30
      REAL SFPA_LADDERRADIUS(20)                                          30
      EQUIVALENCE (BANK_SFPA(83),SFPA_LADDERRADIUS,                       30
     *RE_SFPA_LADDERRADIUS),(NAMES_SFPA(24),NSFPA_LADDERRADIUS)           30
      EQUIVALENCE (COMMEN_SFPA(24),CSFPA_LADDERRADIUS),(MAP_SFPA(1,24),   30
     *MSFPA_LADDERRADIUS)                                                 30
      CHARACTER CSFPA_LADDERRADIUS*40/' '/                                30
      CHARACTER NSFPA_LADDERRADIUS*12/'R.LADDERRADIUS'/                   30
      INTEGER MSFPA_LADDERRADIUS(2)/20,1/                                 30
      REAL RE_SFPA_LADDERRADIUS                                           30
C -                                                                       30
      INTEGER LEN_SFPA,LLEN_SFPA,LINK_SFPA                                30
      PARAMETER (LEN_SFPA=102)                                            30
      PARAMETER (LLEN_SFPA=24)                                            30
      INTEGER FLAG_SFPA/0/,MAP_SFPA(2,LLEN_SFPA),IDNUM_SFPA(10)           30
      CHARACTER NAMES_SFPA(LLEN_SFPA)*12,COMMEN_SFPA(LLEN_SFPA)*40        30
      DATA IDNUM_SFPA/10*0/                                               30
      REAL BANK_SFPA(LEN_SFPA)                                            30
      CHARACTER*60 BPATH_SFPA/' '/                                        30
      SAVE FLAG_SFPA,MAP_SFPA,IDNUM_SFPA,NAMES_SFPA,COMMEN_SFPA,          30
     *BPATH_SFPA                                                          30
      DATA LINK_SFPA/0/,BANK_SFPA/LEN_SFPA*0/                             30
C *                                                                       30
      INTEGER ILAD,IWAF,JWAF,NC                                           31
      REAL WAFPCKLEN,DTHK,RADTILT,ANG                                     33
      REAL ESSAI,HIGHT,LNG,LENG,YOFFSET                                   35
C *                                                                       37
      CALL AGSBEGM(MODULE,IPRIN)                                          37
      IF (IPRIN.LT.0) RETURN                                              37
C Filling bank SSDP - SILICON STRIPS                                      37
      BANK_TITLE='SILICON STRIPS'                                         37
      BPATH_SSDP='SSDP*'                                                  37
      IDNUM_SSDP(1)=0                                                     38
C VERSION                                                                 38
      SSDP_VERSION=1                                                      38
      CSSDP_VERSION='VERSION'                                             38
C THERE ARE A FEW CONFIGURAIONS POSSIBLE                                  39
      SSDP_CONFIG=1                                                       39
      CSSDP_CONFIG='THERE ARE A FEW CONFIGURAIONS POSSIBLE'               39
C 0=CAVE, 1=SVTT                                                          40
      SSDP_PLACEMENT=0                                                    40
      CSSDP_PLACEMENT='0=CAVE, 1=SVTT'                                    40
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSDP',    41
     *BPATH_SSDP,IDNUM_SSDP,1, MAP_SSDP,NAMES_SSDP,COMMEN_SSDP,           41
     *BANK_SSDP,LEN_SSDP,LLEN_SSDP,LINK_SSDP,FLAG_SSDP)                   41
C *                                                                       41
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSDP',    41
     *BPATH_SSDP,IDNUM_SSDP,1, MAP_SSDP,NAMES_SSDP,COMMEN_SSDP,           41
     *BANK_SSDP,LEN_SSDP,LLEN_SSDP,LINK_SSDP,FLAG_SSDP)                   41
C *                                                                       42
      CALL AGSBEGM(MODULE,IPRIN)                                          42
      IF (IPRIN.LT.0) RETURN                                              42
C Filling bank SFJP - DIFFERENT DIMENSION PARAMETERS                      42
      BANK_TITLE='DIFFERENT DIMENSION PARAMETERS'                         42
      BPATH_SFJP='SFJP*'                                                  42
      IDNUM_SFJP(1)=0                                                     43
C ALPHA OF THE SFLA AND OTHER PARTS                                       43
      SFJP_ALPHAZ=54                                                      43
      CSFJP_ALPHAZ='ALPHA OF THE SFLA AND OTHER PARTS'                    43
C ALPHA OF OTHER PARTS                                                    44
      SFJP_ALPHAZH=45                                                     44
      CSFJP_ALPHAZH='ALPHA OF OTHER PARTS'                                44
C MINIMUM RADIUS OF THE SMALL ALUMINIUM SECTOR (OUTSIDE STRUCTURE)        45
      SFJP_SSST_RMIN=31.285                                               45
      CSFJP_SSST_RMIN='MINIMUM RADIUS OF THE SMALL ALUMINIUM SECTOR (OUT  45
     *SIDE STRUCTURE)'                                                    45
C MAXIMUM RADIOS OF THE SMALL ALUMINIUM SECTOR (OUTSIDE STRUCTURE)        46
      SFJP_SSST_RMAX=31.8                                                 46
      CSFJP_SSST_RMAX='MAXIMUM RADIOS OF THE SMALL ALUMINIUM SECTOR (OUT  46
     *SIDE STRUCTURE)'                                                    46
C WIDTH OF THE SMALL ALUMINIUM SECTOR (OUTSIDE STRUCTURE)                 47
      SFJP_SSST_WIDTH=2.5                                                 47
      CSFJP_SSST_WIDTH='WIDTH OF THE SMALL ALUMINIUM SECTOR (OUTSIDE STR  47
     *UCTURE)'                                                            47
C ADDITIONAL Z-POSITION OF THE ALUMINIUM SECTORS                          48
      SFJP_SSST_PZ=0.2                                                    48
      CSFJP_SSST_PZ='ADDITIONAL Z-POSITION OF THE ALUMINIUM SECTORS'      48
C WIDTH OF THE SMALL INNER ALUMINIUM SECTOR (OUTSIDE STRUCTURE)           49
      SFJP_SSSS_RMIN=23.3                                                 49
      CSFJP_SSSS_RMIN='WIDTH OF THE SMALL INNER ALUMINIUM SECTOR (OUTSID  49
     *E STRUCTURE)'                                                       49
C WIDTH OF THE SMALL INNER ALUMINIUM SECTOR (OUTSIDE STRUCTURE)           50
      SFJP_SSSS_WIDTH=0.5                                                 50
      CSFJP_SSSS_WIDTH='WIDTH OF THE SMALL INNER ALUMINIUM SECTOR (OUTSI  50
     *DE STRUCTURE)'                                                      50
C WIDTH OF THE SIDE OF THE SMALL RIB (OUTSIDE STRUCTURE)                  51
      SFJP_SSRS_RMIN=28.0                                                 51
      CSFJP_SSRS_RMIN='WIDTH OF THE SIDE OF THE SMALL RIB (OUTSIDE STRUC  51
     *TURE)'                                                              51
C LINKING BOX (SECTOR TO THE CONE)                                        52
      SFJP_SSLB_DX=7.8                                                    52
      CSFJP_SSLB_DX='LINKING BOX (SECTOR TO THE CONE)'                    52
C X-POSITION = Y-POSITION OF THE LINKING BOX                              53
      SFJP_SSLB_PX=19.1                                                   53
      CSFJP_SSLB_PX='X-POSITION = Y-POSITION OF THE LINKING BOX'          53
C LINKING BOX (SECTOR TO THE CONE)                                        54
      SFJP_SSLB_DY=7.3                                                    54
      CSFJP_SSLB_DY='LINKING BOX (SECTOR TO THE CONE)'                    54
C LINKING BOX (SECTOR TO THE CONE)                                        55
      SFJP_SSLB_DZ=2.5                                                    55
      CSFJP_SSLB_DZ='LINKING BOX (SECTOR TO THE CONE)'                    55
C X-POSITION = Y-POSITION OF THE LINKING TUBE                             56
      SFJP_SSLT_PX=18.1                                                   56
      CSFJP_SSLT_PX='X-POSITION = Y-POSITION OF THE LINKING TUBE'         56
C RADIUS OF THE LINKING TUBE (SECTOR TO THE CONE)                         57
      SFJP_SSLT_RMAX=0.8                                                  57
      CSFJP_SSLT_RMAX='RADIUS OF THE LINKING TUBE (SECTOR TO THE CONE)'   57
C WIDTH OF THE LINKING TUBE (SECTOR OF THE CONE)                          58
      SFJP_SSLT_DZ=5.05                                                   58
      CSFJP_SSLT_DZ='WIDTH OF THE LINKING TUBE (SECTOR OF THE CONE)'      58
C WIDTH OF THE MOUNTING PLATE INSERTED IN THE CONES                       59
      SFJP_SCMP_DX=6.3                                                    59
      CSFJP_SCMP_DX='WIDTH OF THE MOUNTING PLATE INSERTED IN THE CONES'   59
C WIDTH OF THE MOUNTING PLATE INSERTED IN THE CONES                       60
      SFJP_SCMP_DY=0.4                                                    60
      CSFJP_SCMP_DY='WIDTH OF THE MOUNTING PLATE INSERTED IN THE CONES'   60
C WIDTH OF THE MOUNTING PLATE INSERTED IN THE CONES                       61
      SFJP_SCMP_DZ=3.6                                                    61
      CSFJP_SCMP_DZ='WIDTH OF THE MOUNTING PLATE INSERTED IN THE CONES'   61
C X-POSITION = Y-POSITION OF THE MOUNTING PLATE                           62
      SFJP_SCMP_PX=15.945                                                 62
      CSFJP_SCMP_PX='X-POSITION = Y-POSITION OF THE MOUNTING PLATE'       62
C MOTHER VOLUME OF THE V-SHAPE PIECE LENGTH                               63
      SFJP_SCVM_DZ=3.6                                                    63
      CSFJP_SCVM_DZ='MOTHER VOLUME OF THE V-SHAPE PIECE LENGTH'           63
C MOTHER VOLUME OF THE V-SHAPE PIECE DX                                   64
      SFJP_SCVM_DX=6.4                                                    64
      CSFJP_SCVM_DX='MOTHER VOLUME OF THE V-SHAPE PIECE DX'               64
C X-POSITION = Y-POSITION OF THE V-SHAPE PIECE                            65
      SFJP_SCVM_PX=17.00                                                  65
      CSFJP_SCVM_PX='X-POSITION = Y-POSITION OF THE V-SHAPE PIECE'        65
C MOTHER VOLUME OF THE V-SHAPE PIECE DY                                   66
      SFJP_SCVM_DY=2.6                                                    66
      CSFJP_SCVM_DY='MOTHER VOLUME OF THE V-SHAPE PIECE DY'               66
C Z-POSITION OF THE MOTHER VOLUME                                         67
      SFJP_SCVM_PZ=57.5                                                   67
      CSFJP_SCVM_PZ='Z-POSITION OF THE MOTHER VOLUME'                     67
C HEIGHT OF THE BASE OF THE V-SHAPE PIECE                                 68
      SFJP_SCVB_DY=0.5                                                    68
      CSFJP_SCVB_DY='HEIGHT OF THE BASE OF THE V-SHAPE PIECE'             68
C DX OF THE V-PLATES OF THE V-SHAPE PIECE 45 DEGREES                      69
      SFJP_SCVS_DX=2.3                                                    69
      CSFJP_SCVS_DX='DX OF THE V-PLATES OF THE V-SHAPE PIECE 45           69
     *DEGREES'                                                            69
C MEDIUM HEIGHT OF THE V-PLATES OF THE V-SHAPE PIECE 45 DEGREES           70
      SFJP_SCVS_DY=0.6                                                    70
      CSFJP_SCVS_DY='MEDIUM HEIGHT OF THE V-PLATES OF THE V-SHAPE PIECE   70
     *45 DEGREES'                                                         70
C X-POSITION OF THE V-PLATES OF THE V-SHAPE PIECE 45 DEGREES              71
      SFJP_SCVS_PX=1.0                                                    71
      CSFJP_SCVS_PX='X-POSITION OF THE V-PLATES OF THE V-SHAPE PIECE 45   71
     *DEGREES'                                                            71
C X-POSITION OF THE V-PLATES OF THE V-SHAPE PIECE 45 DEGREES              72
      SFJP_SCVS_PY=0.22                                                   72
      CSFJP_SCVS_PY='X-POSITION OF THE V-PLATES OF THE V-SHAPE PIECE 45   72
     *DEGREES'                                                            72
C DX OF THE CONNECTION BOARD (END OF THE LADDER)                          73
      SFJP_SFCO_DX=4.4                                                    73
      CSFJP_SFCO_DX='DX OF THE CONNECTION BOARD (END OF THE LADDER)'      73
C WIDTH OF THE CONNECTION BOARD (END OF THE LADDER)                       74
      SFJP_SFCO_DY=0.37                                                   74
      CSFJP_SFCO_DY='WIDTH OF THE CONNECTION BOARD (END OF THE LADDER)'   74
C LENGTH OF THE CONNECTION BOARD (END OF THE LADDER)                      75
      SFJP_SFCO_DZ=15.8                                                   75
      CSFJP_SFCO_DZ='LENGTH OF THE CONNECTION BOARD (END OF THE           75
     *LADDER)'                                                            75
C X-POSITION OF THE CONNECTION BOARD (END OF THE LADDER)                  76
      SFJP_SFCO_PX=1.7                                                    76
      CSFJP_SFCO_PX='X-POSITION OF THE CONNECTION BOARD (END OF THE LADD  76
     *ER)'                                                                76
C Y-POSITION OF THE CONNECTION BOARD (END OF THE LADDER)                  77
      SFJP_SFCO_PY=0.2                                                    77
      CSFJP_SFCO_PY='Y-POSITION OF THE CONNECTION BOARD (END OF THE LADD  77
     *ER)'                                                                77
C DX OF THE SECOND CONNECTION BOARD (SMALLER THAN THE ABOVE ONE!!)        78
      SFJP_SFCM_DX=4.0                                                    78
      CSFJP_SFCM_DX='DX OF THE SECOND CONNECTION BOARD (SMALLER THAN THE  78
     * ABOVE ONE!!)'                                                      78
C DX OF THE SMALL PART OF THE SECOND CONNECTION BOARD                     79
      SFJP_SFCS_DZ=7.5                                                    79
      CSFJP_SFCS_DZ='DX OF THE SMALL PART OF THE SECOND CONNECTION BOARD  79
     *'                                                                   79
C WIDTH OF THE KAPTON FLEX CIRCUIT BETWEEN THE CONNECTION BOARDS          80
      SFJP_SFKF_DY=0.065                                                  80
      CSFJP_SFKF_DY='WIDTH OF THE KAPTON FLEX CIRCUIT BETWEEN THE CONNEC  80
     *TION BOARDS'                                                        80
C LENGTH OF THE KAPTON FLEX CIRCUIT BETWEEN THE CONNECTION BOARDS         81
      SFJP_SFKF_DZ=8.1                                                    81
      CSFJP_SFKF_DZ='LENGTH OF THE KAPTON FLEX CIRCUIT BETWEEN THE CONNE  81
     *CTION BOARDS'                                                       81
C DX OF THE FIRST PART OF THE KAPTON FLEX CIRCUIT                         82
      SFJP_SFKF_DX=0.44                                                   82
      CSFJP_SFKF_DX='DX OF THE FIRST PART OF THE KAPTON FLEX CIRCUIT'     82
C X-POSITION OF THE FIRST PART OF THE KAPTON FLEX CIRCUIT                 83
      SFJP_SFKF_PX=0.16                                                   83
      CSFJP_SFKF_PX='X-POSITION OF THE FIRST PART OF THE KAPTON FLEX CIR  83
     *CUIT'                                                               83
C Y-POSITION OF THE FIRST PART OF THE KAPTON FLEX CIRCUIT                 84
      SFJP_SFKF_PY=2.07                                                   84
      CSFJP_SFKF_PY='Y-POSITION OF THE FIRST PART OF THE KAPTON FLEX CIR  84
     *CUIT'                                                               84
C DX OF THE SECOND PART OF THE KAPTON FLEX CIRCUIT                        85
      SFJP_SFKS_DX=1.65                                                   85
      CSFJP_SFKS_DX='DX OF THE SECOND PART OF THE KAPTON FLEX CIRCUIT'    85
C X-POSITION OF THE SECOND PART OT THE KAPTON FLEX                        86
      SFJP_SFKS_PX=0.505                                                  86
      CSFJP_SFKS_PX='X-POSITION OF THE SECOND PART OT THE KAPTON FLEX'    86
C Y-POSITION OF THE SECOND PART OT THE KAPTON FLEX                        87
      SFJP_SFKS_PY=1.585                                                  87
      CSFJP_SFKS_PY='Y-POSITION OF THE SECOND PART OT THE KAPTON FLEX'    87
C Y-POSITION OF THE MECHANICAL PARTS                                      88
      SFJP_SFPR_PY=1.79                                                   88
      CSFJP_SFPR_PY='Y-POSITION OF THE MECHANICAL PARTS'                  88
C Y-POSITION OF THE MECHANICAL PARTS 2                                    89
      SFJP_SFPB_PY=3.48                                                   89
      CSFJP_SFPB_PY='Y-POSITION OF THE MECHANICAL PARTS 2'                89
C Y-POSITION OF THE MECHANICAL PARTS 2                                    90
      SFJP_SFPB_PY2=0.08                                                  90
      CSFJP_SFPB_PY2='Y-POSITION OF THE MECHANICAL PARTS 2'               90
C DX OF THE BIG PART OF THE SECOND CONNECTION BOARD                       91
      SFJP_SFCB_DX=3.0                                                    91
      CSFJP_SFCB_DX='DX OF THE BIG PART OF THE SECOND CONNECTION BOARD'   91
C WIDTH OF THE ALUMINIUM PLATES LINKING THE LADDER TO THE SECTOR          92
      SFJP_SSBS_DY=0.5                                                    92
      CSFJP_SSBS_DY='WIDTH OF THE ALUMINIUM PLATES LINKING THE LADDER TO  92
     * THE SECTOR'                                                        92
C DX = DZ = SMALL PART OF THE ALUMINIUM PLATE LINKING THE LADDER          93
      SFJP_SSBS_DX=2.5                                                    93
      CSFJP_SSBS_DX='DX = DZ = SMALL PART OF THE ALUMINIUM PLATE LINKING  93
     * THE LADDER'                                                        93
C DX OF THE BIG PART OF THE ALUMINIUM PLATE LINKING THE LADDER            94
      SFJP_SSBB_DX=4.3                                                    94
      CSFJP_SSBB_DX='DX OF THE BIG PART OF THE ALUMINIUM PLATE LINKING T  94
     *HE LADDER'                                                          94
C DZ OF THE BIG PART OF THE ALUMINIUM PLATE LINKING THE LADDER            95
      SFJP_SSBB_DZ=1.9                                                    95
      CSFJP_SSBB_DZ='DZ OF THE BIG PART OF THE ALUMINIUM PLATE LINKING T  95
     *HE LADDER'                                                          95
C DISTANZE BETWEEN OVERLAYING FLEXES                                      96
      SFJP_FLEX_DI=0.02                                                   96
      CSFJP_FLEX_DI='DISTANZE BETWEEN OVERLAYING FLEXES'                  96
C DX OF THE OUTSIDE MECHANICAL PART (G10 RECTANGLE - BOX BASE)            97
      SFJP_SFPB_DX=4.3                                                    97
      CSFJP_SFPB_DX='DX OF THE OUTSIDE MECHANICAL PART (G10 RECTANGLE -   97
     *BOX BASE)'                                                          97
C WIDTH OF THE OUTSIDE MECHANICAL PART (G10 RECTANGLE - BOX BASE)         98
      SFJP_SFPB_DY=0.8                                                    98
      CSFJP_SFPB_DY='WIDTH OF THE OUTSIDE MECHANICAL PART (G10 RECTANGLE  98
     * - BOX BASE)'                                                       98
C LENGTH OF THE OUTSIDE MECHANICAL PART (G10 RECTANGLE - BOX BASE)        99
      SFJP_SFPBDZ=0.95                                                    99
      CSFJP_SFPBDZ='LENGTH OF THE OUTSIDE MECHANICAL PART (G10 RECTANGLE  99
     * - BOX BASE)'                                                       99
C DX OF THE EPOXY CORE OF THE ADC BOARD APPENDICE MOTHER VOLUME           100
      SFJP_SAPP_DXE=7.6                                                   100
      CSFJP_SAPP_DXE='DX OF THE EPOXY CORE OF THE ADC BOARD APPENDICE MO  100
     *THER VOLUME'                                                        100
C DX OF THE CARBON SIDE SHELL OF THE ADC BOARD APPENDICE                  101
      SFJP_SAPP_DXZ=0.04                                                  101
      CSFJP_SAPP_DXZ='DX OF THE CARBON SIDE SHELL OF THE ADC BOARD APPEN  101
     *DICE'                                                               101
C WIDTH OF THE EPOXY CORE OF THE ADC BOARD APPENDICE MOTHER VOLUME        102
      SFJP_SAPP_DY=0.5                                                    102
      CSFJP_SAPP_DY='WIDTH OF THE EPOXY CORE OF THE ADC BOARD APPENDICE   102
     *MOTHER VOLUME'                                                      102
C LENGTH OF THE ADC BOARD APPENDICE MOTHER VOLUME                         103
      SFJP_SAPP_DZ=1.275                                                  103
      CSFJP_SAPP_DZ='LENGTH OF THE ADC BOARD APPENDICE MOTHER VOLUME'     103
C 1. Y-POSITION OF THE MOTHER COLUME OF THE ADC BOARD                     104
      SFJP_SAPP_PY1=0.04                                                  104
      CSFJP_SAPP_PY1='1. Y-POSITION OF THE MOTHER COLUME OF THE ADC BOAR  104
     *D'                                                                  104
C 2. Y-POSITION OF THE MOTHER COLUME OF THE ADC BOARD                     105
      SFJP_SAPP_PY2=0.5                                                   105
      CSFJP_SAPP_PY2='2. Y-POSITION OF THE MOTHER COLUME OF THE ADC BOAR  105
     *D'                                                                  105
C 3. Y-POSITION OF THE MOTHER COLUME OF THE ADC BOARD                     106
      SFJP_SAPP_PY3=0.08                                                  106
      CSFJP_SAPP_PY3='3. Y-POSITION OF THE MOTHER COLUME OF THE ADC BOAR  106
     *D'                                                                  106
C DX OF THE MOTHER VOLUME OF THE ADC BOARD                                107
      SFJP_SFAM_DXE=7.2                                                   107
      CSFJP_SFAM_DXE='DX OF THE MOTHER VOLUME OF THE ADC BOARD'           107
C DX OF THE SMALL VOLUME OF THE ADC BOARD                                 108
      SFJP_SFAM_DXZ=2.1                                                   108
      CSFJP_SFAM_DXZ='DX OF THE SMALL VOLUME OF THE ADC BOARD'            108
C WIDTH OF THE MOTHER VOLUME OF THE ADC BOARD                             109
      SFJP_SFAM_DY=0.3                                                    109
      CSFJP_SFAM_DY='WIDTH OF THE MOTHER VOLUME OF THE ADC BOARD'         109
C LENGTH OF THE MOTHER VOLUME OF THE ADC BOARD                            110
      SFJP_SFAM_DZ=12.4                                                   110
      CSFJP_SFAM_DZ='LENGTH OF THE MOTHER VOLUME OF THE ADC BOARD'        110
C LENGTH OF THE SMALL VOLUME OF THE ADC BOARD                             111
      SFJP_SFAM_DZS=2.15                                                  111
      CSFJP_SFAM_DZS='LENGTH OF THE SMALL VOLUME OF THE ADC BOARD'        111
C DX OF THE LONG PART OF THE BUS CABLE LINKING THE MODULES                112
      SFJP_SFLA_DX=1.5                                                    112
      CSFJP_SFLA_DX='DX OF THE LONG PART OF THE BUS CABLE LINKING THE MO  112
     *DULES'                                                              112
C WIDTH OF THE LONG PART OF THE BUS CABLE LINKING THE MODULES             113
      SFJP_SFLA_DY=0.0130                                                 113
      CSFJP_SFLA_DY='WIDTH OF THE LONG PART OF THE BUS CABLE LINKING THE  113
     * MODULES'                                                           113
C LENGTH OF THE LONG PART OF THE BUS CABLE LINKING THE MODULES            114
      SFJP_SFLA_DZ=4.3                                                    114
      CSFJP_SFLA_DZ='LENGTH OF THE LONG PART OF THE BUS CABLE LINKING TH  114
     *E MODULES'                                                          114
C LENGTH OF THE LONG BUS CABLE                                            115
      SFJP_SFLB_DZ=7.5                                                    115
      CSFJP_SFLB_DZ='LENGTH OF THE LONG BUS CABLE'                        115
C LENGTH OF THE LONG BUS CABLE ON THE CB UP TO THE CONNECTOR              116
      SFJP_SFLC_DZ=0.93                                                   116
      CSFJP_SFLC_DZ='LENGTH OF THE LONG BUS CABLE ON THE CB UP TO THE CO  116
     *NNECTOR'                                                            116
C DX OF THE BIG BUS ELBOW                                                 117
      SFJP_SFEB_DX=3.89                                                   117
      CSFJP_SFEB_DX='DX OF THE BIG BUS ELBOW'                             117
C LENGTH OF THE BUS ELBOW                                                 118
      SFJP_SFEB_DZ=1.5                                                    118
      CSFJP_SFEB_DZ='LENGTH OF THE BUS ELBOW'                             118
C DX OF THE BIG BUS ELBOW                                                 119
      SFJP_SFES_DX=1.875                                                  119
      CSFJP_SFES_DX='DX OF THE BIG BUS ELBOW'                             119
C DX OF THE HORIZONTAL PART OF THE LADDER SKELETON CARBON BASE            120
      SFJP_SFFK_DXE=0.4                                                   120
      CSFJP_SFFK_DXE='DX OF THE HORIZONTAL PART OF THE LADDER SKELETON C  120
     *ARBON BASE'                                                         120
C WIDTH OF THE HORIZONTAL PART OF THE LADDER SKELETON CARBON BASE         121
      SFJP_SFFK_DY=0.02                                                   121
      CSFJP_SFFK_DY='WIDTH OF THE HORIZONTAL PART OF THE LADDER SKELETON  121
     * CARBON BASE'                                                       121
C LENGTHOF OF THE HORIZONTAL PART OF THE LADDER SKELETON CARBON BASE      122
      SFJP_SFFK_DZ=69.75                                                  122
      CSFJP_SFFK_DZ='LENGTHOF OF THE HORIZONTAL PART OF THE LADDER SKELE  122
     *TON CARBON BASE'                                                    122
C DX OF THE TILTED PART OF THE LADDER                                     123
      SFJP_SFFL_DX=0.6                                                    123
      CSFJP_SFFL_DX='DX OF THE TILTED PART OF THE LADDER'                 123
C DX OF THE HORIZONTAL PART OF THE KAPTON FILM                            124
      SFJP_SFFK_DXZ=5.1                                                   124
      CSFJP_SFFK_DXZ='DX OF THE HORIZONTAL PART OF THE KAPTON FILM'       124
C X-POSITION OF THE CARBON BASE UNDER THE LADDER                          125
      SFJP_SFFK_PX=3.5                                                    125
      CSFJP_SFFK_PX='X-POSITION OF THE CARBON BASE UNDER THE LADDER'      125
C 1. Y-POSITION OF THE CARBON BASE UNDER THE LADDER                       126
      SFJP_SFFK_PY1=0.02                                                  126
      CSFJP_SFFK_PY1='1. Y-POSITION OF THE CARBON BASE UNDER THE          126
     *LADDER'                                                             126
C 2. Y-POSITION OF THE CARBON BASE UNDER THE LADDER                       127
      SFJP_SFFK_PY2=0.04                                                  127
      CSFJP_SFFK_PY2='2. Y-POSITION OF THE CARBON BASE UNDER THE          127
     *LADDER'                                                             127
C X-POSITION OF THE TILTED KAPTON FILM UNDER THE LADDER                   128
      SFJP_SFKL_PX=2.55                                                   128
      CSFJP_SFKL_PX='X-POSITION OF THE TILTED KAPTON FILM UNDER THE LADD  128
     *ER'                                                                 128
C WIDTH OF THE KAPTON FILM                                                129
      SFJP_SFKK_DY=0.0025                                                 129
      CSFJP_SFKK_DY='WIDTH OF THE KAPTON FILM'                            129
C LENGTH OF THE TRIANGULAR LADDER SKELETON                                130
      SFJP_SFLU_DZ=49.8                                                   130
      CSFJP_SFLU_DZ='LENGTH OF THE TRIANGULAR LADDER SKELETON'            130
C HEIGHT OF THE TRIANGULAR LADDER SKELETON                                131
      SFJP_SFLU_H1=0.02                                                   131
      CSFJP_SFLU_H1='HEIGHT OF THE TRIANGULAR LADDER SKELETON'            131
C BASE LENGTH OF THE TRIANGULAR LADDER SKELETON                           132
      SFJP_SFLU_BL1=0.2                                                   132
      CSFJP_SFLU_BL1='BASE LENGTH OF THE TRIANGULAR LADDER SKELETON'      132
C DX OF THE HYBRID STIFFNER                                               133
      SFJP_SFRA_DX=3.76                                                   133
      CSFJP_SFRA_DX='DX OF THE HYBRID STIFFNER'                           133
C WIDTH OF THE HYBRID STIFFNER                                            134
      SFJP_SFRA_DY=0.019                                                  134
      CSFJP_SFRA_DY='WIDTH OF THE HYBRID STIFFNER'                        134
C Y-POSITION OF THE HYBRID STIFFNER                                       135
      SFJP_SFRA_PY=0.099                                                  135
      CSFJP_SFRA_PY='Y-POSITION OF THE HYBRID STIFFNER'                   135
C LENGTH OF THE HYBRID STIFFNER                                           136
      SFJP_SFRA_DZ=1.0                                                    136
      CSFJP_SFRA_DZ='LENGTH OF THE HYBRID STIFFNER'                       136
C Z-POSITION OF THE HYBRID STIFFNER                                       137
      SFJP_SFRA_PZ=1.1                                                    137
      CSFJP_SFRA_PZ='Z-POSITION OF THE HYBRID STIFFNER'                   137
C WIDTH OF A SINGLE WAFER CONTAINER                                       138
      SFJP_SFSW_DY=1.0                                                    138
      CSFJP_SFSW_DY='WIDTH OF A SINGLE WAFER CONTAINER'                   138
C LENGTHOF OF A SINGLE WAFER CONTAINER                                    139
      SFJP_SFSW_DZ=2.25                                                   139
      CSFJP_SFSW_DZ='LENGTHOF OF A SINGLE WAFER CONTAINER'                139
C DX OF THE TWO SUPPORTS OF THE HYBRID STIFFNER                           140
      SFJP_SFRS_DX=0.04                                                   140
      CSFJP_SFRS_DX='DX OF THE TWO SUPPORTS OF THE HYBRID STIFFNER'       140
C DY OF THE TWO SUPPORTS OF THE HYBRID STIFFNER                           141
      SFJP_SFRS_DY=0.0915                                                 141
      CSFJP_SFRS_DY='DY OF THE TWO SUPPORTS OF THE HYBRID STIFFNER'       141
C X-POSITION OF THE TWO SUPPORTS OF THE HYBRID STIFFNER                   142
      SFJP_SFRS_PX=3.8                                                    142
      CSFJP_SFRS_PX='X-POSITION OF THE TWO SUPPORTS OF THE HYBRID STIFFN  142
     *ER'                                                                 142
C Y-POSITION OF THE TWO SUPPORTS OF THE HYBRID STIFFNER                   143
      SFJP_SFRS_PY=0.0265                                                 143
      CSFJP_SFRS_PY='Y-POSITION OF THE TWO SUPPORTS OF THE HYBRID STIFFN  143
     *ER'                                                                 143
C LENGTH OF THE TRIANGULAR BASE                                           144
      SFJP_SFSM_LL=2.6                                                    144
      CSFJP_SFSM_LL='LENGTH OF THE TRIANGULAR BASE'                       144
C DX OF THE FLEX                                                          145
      SFJP_SFFX_DX=3.84                                                   145
      CSFJP_SFFX_DX='DX OF THE FLEX'                                      145
C WIDTH OF THE FLEX                                                       146
      SFJP_SFFX_DYZ=0.0007                                                146
      CSFJP_SFFX_DYZ='WIDTH OF THE FLEX'                                  146
C RMIN OF THE PIONS                                                       147
      SFJP_SFPI_RMIN=0.15                                                 147
      CSFJP_SFPI_RMIN='RMIN OF THE PIONS'                                 147
C RMAX OF THE PIONS                                                       148
      SFJP_SFPI_RMAX=0.2                                                  148
      CSFJP_SFPI_RMAX='RMAX OF THE PIONS'                                 148
C X-POSITION OF THE FOUR PIONS                                            149
      SFJP_SFPI_PX=3.2                                                    149
      CSFJP_SFPI_PX='X-POSITION OF THE FOUR PIONS'                        149
C 1. Y-POSITION OF THE FOUR PIONS                                         150
      SFJP_SFPI_PY1=0.35                                                  150
      CSFJP_SFPI_PY1='1. Y-POSITION OF THE FOUR PIONS'                    150
C 2. Y-POSITION OF THE FOUR PIONS                                         151
      SFJP_SFPI_PY2=0.025                                                 151
      CSFJP_SFPI_PY2='2. Y-POSITION OF THE FOUR PIONS'                    151
C Z-POSITION OF THE FOUR PIONS                                            152
      SFJP_SFPI_PZ=0.7                                                    152
      CSFJP_SFPI_PZ='Z-POSITION OF THE FOUR PIONS'                        152
C 1. DX OF THE BASE OF THE PIONS                                          153
      SFJP_SFPJ_DX1=0.075                                                 153
      CSFJP_SFPJ_DX1='1. DX OF THE BASE OF THE PIONS'                     153
C 2. DX OF THE BASE OF THE PIONS                                          154
      SFJP_SFPJ_DX2=0.025                                                 154
      CSFJP_SFPJ_DX2='2. DX OF THE BASE OF THE PIONS'                     154
C DY OF THE BASE OF THE PIONS                                             155
      SFJP_SFPJ_DY=0.3                                                    155
      CSFJP_SFPJ_DY='DY OF THE BASE OF THE PIONS'                         155
C DZ OF THE BASE OF THE PIONS                                             156
      SFJP_SFPJ_DZ=0.0125                                                 156
      CSFJP_SFPJ_DZ='DZ OF THE BASE OF THE PIONS'                         156
C DX OF THE A128C CHIP                                                    157
      SFJP_SFAA_DX=0.3                                                    157
      CSFJP_SFAA_DX='DX OF THE A128C CHIP'                                157
C DY OF THE A128C CHIP                                                    158
      SFJP_SFAA_DY=0.015                                                  158
      CSFJP_SFAA_DY='DY OF THE A128C CHIP'                                158
C DZ OF THE A128C CHIP                                                    159
      SFJP_SFAA_DZ=0.4                                                    159
      CSFJP_SFAA_DZ='DZ OF THE A128C CHIP'                                159
C 1. X=^FYLL SFJP 01-0 SFAA_PX1 = 0.325 ! 1. X-POSITION OF THE A128C CHI  160
C PS                                                                      160
      SFJP_SFAA_PX1=0.325                                                 160
      CSFJP_SFAA_PX1='1. X=^FYLL SFJP 01-0 SFAA_PX1 = 0.325 ! 1. X-POSIT  160
     *ION OF THE A128C CHIPS'                                             160
C 2. X=^FYLL SFJP 01-0 SFAA_PX2 = 0.65 ! 2. X-POSITION OF THE A128C CHIP  161
C S                                                                       161
      SFJP_SFAA_PX2=0.65                                                  161
      CSFJP_SFAA_PX2='2. X=^FYLL SFJP 01-0 SFAA_PX2 = 0.65 ! 2. X-POSITI  161
     *ON OF THE A128C CHIPS'                                              161
C 3. X=^FYLL SFJP 01-0 SFAA_PX3 = 0.6 ! 3. X-POSITION OF THE A128C CHIPS  162
      SFJP_SFAA_PX3=0.6                                                   162
      CSFJP_SFAA_PX3='3. X=^FYLL SFJP 01-0 SFAA_PX3 = 0.6 ! 3. X-POSITIO  162
     *N OF THE A128C CHIPS'                                               162
C 1. Z=^FYLL SFJP 01-0 SFAA_PZ1 = 1.1 ! 1. Z-POSITION OF THE A128C CHIPS  163
      SFJP_SFAA_PZ1=1.1                                                   163
      CSFJP_SFAA_PZ1='1. Z=^FYLL SFJP 01-0 SFAA_PZ1 = 1.1 ! 1. Z-POSITIO  163
     *N OF THE A128C CHIPS'                                               163
C 2. Z=^FYLL SFJP 01-0 SFAA_PZ2 = 1.0 ! 2. Z-POSITION OF THE A128C CHIPS  164
      SFJP_SFAA_PZ2=1.0                                                   164
      CSFJP_SFAA_PZ2='2. Z=^FYLL SFJP 01-0 SFAA_PZ2 = 1.0 ! 2. Z-POSITIO  164
     *N OF THE A128C CHIPS'                                               164
C 3. Z=^FYLL SFJP 01-0 SFAA_PZ3 = 0.02 ! 3. Z-POSITION OF THE A128C CHIP  165
C S                                                                       165
      SFJP_SFAA_PZ3=0.02                                                  165
      CSFJP_SFAA_PZ3='3. Z=^FYLL SFJP 01-0 SFAA_PZ3 = 0.02 ! 3. Z-POSITI  165
     *ON OF THE A128C CHIPS'                                              165
C DX OF THE STRIP DETECTOR                                                166
      SFJP_SFSD_DX=3.75                                                   166
      CSFJP_SFSD_DX='DX OF THE STRIP DETECTOR'                            166
C DY OF THE STRIP DETECTOR                                                167
      SFJP_SFSD_DY=0.015                                                  167
      CSFJP_SFSD_DY='DY OF THE STRIP DETECTOR'                            167
C DZ OF THE STRIP DETECTOR                                                168
      SFJP_SFSD_DZ=2.1                                                    168
      CSFJP_SFSD_DZ='DZ OF THE STRIP DETECTOR'                            168
C X-POSITION OF THE TOP FLEX ALONG THE Z-AXIS                             169
      SFJP_SFLA_PX=1.1                                                    169
      CSFJP_SFLA_PX='X-POSITION OF THE TOP FLEX ALONG THE Z-AXIS'         169
C Y-POSITION OF THE TOP FLEX ALONG THE Z-AXIS                             170
      SFJP_SFLA_PY=1.35                                                   170
      CSFJP_SFLA_PY='Y-POSITION OF THE TOP FLEX ALONG THE Z-AXIS'         170
C X-POSITION OF THE BOTTOM FLEX ALONG THE Z-AXIS                          171
      SFJP_SFLC_PX=2.3                                                    171
      CSFJP_SFLC_PX='X-POSITION OF THE BOTTOM FLEX ALONG THE Z-AXIS'      171
C Y-POSITION OF THE BOTTOM FLEX ALONG THE Z-AXIS                          172
      SFJP_SFLC_PY=0.3                                                    172
      CSFJP_SFLC_PY='Y-POSITION OF THE BOTTOM FLEX ALONG THE Z-AXIS'      172
C X-POSITION OF THE SMALL ELBOW BUS                                       173
      SFJP_SFES_PX=3.32                                                   173
      CSFJP_SFES_PX='X-POSITION OF THE SMALL ELBOW BUS'                   173
C Y-POSITION OF THE SMALL ELBOW BUS                                       174
      SFJP_SFES_PY=1.6                                                    174
      CSFJP_SFES_PY='Y-POSITION OF THE SMALL ELBOW BUS'                   174
C Z-POSITION OF THE SMALL ELBOW BUS                                       175
      SFJP_SFES_PZ=0.1                                                    175
      CSFJP_SFES_PZ='Z-POSITION OF THE SMALL ELBOW BUS'                   175
C X-POSITION OF THE BIG ELBOW BUS                                         176
      SFJP_SFEB_PX=2.71                                                   176
      CSFJP_SFEB_PX='X-POSITION OF THE BIG ELBOW BUS'                     176
C Y-POSITION OF THE BIG ELBOW BUS                                         177
      SFJP_SFEB_PY=0.75                                                   177
      CSFJP_SFEB_PY='Y-POSITION OF THE BIG ELBOW BUS'                     177
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFJP',    178
     *BPATH_SFJP,IDNUM_SFJP,1, MAP_SFJP,NAMES_SFJP,COMMEN_SFJP,           178
     *BANK_SFJP,LEN_SFJP,LLEN_SFJP,LINK_SFJP,FLAG_SFJP)                   178
C *                                                                       178
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFJP',    178
     *BPATH_SFJP,IDNUM_SFJP,1, MAP_SFJP,NAMES_SFJP,COMMEN_SFJP,           178
     *BANK_SFJP,LEN_SFJP,LLEN_SFJP,LINK_SFJP,FLAG_SFJP)                   178
C *                                                                       179
      CALL AGSBEGM(MODULE,IPRIN)                                          179
      IF (IPRIN.LT.0) RETURN                                              179
C Filling bank SFPB - SOME SSD SHELL DIMENSIONS                           179
      BANK_TITLE='SOME SSD SHELL DIMENSIONS'                              179
      BPATH_SFPB='SFPB*'                                                  179
      IDNUM_SFPB(1)=0                                                     180
C HAUT 1                                                                  180
      SFPB_HHIGHT=(0.02/TAN(54*PI/180)+0.02/(2*TAN(PI/5))+0.2)*SIN(PI/    180
     *5)                                                                  180
      CSFPB_HHIGHT='HAUT 1'                                               180
C HAUT 2                                                                  181
      SFPB_KHIGHT=0.02/SIN(54*PI/180)                                     181
      CSFPB_KHIGHT='HAUT 2'                                               181
C BAS 1                                                                   182
      SFPB_HBASE=(0.02/TAN(63*PI/180)+0.01/TAN(27*PI/                     182
     *180)+0.2)*SIN(27*PI/180)                                            182
      CSFPB_HBASE='BAS 1'                                                 182
C BAS 2                                                                   183
      SFPB_KBASE=0.02/SIN(63*PI/180)                                      183
      CSFPB_KBASE='BAS 2'                                                 183
C LENGHT OF THE SIDE KAPTON FILM                                          184
      SFPB_FSIZE=0.6/COS(15*PI/180)+0.02*TAN(15*PI/180)                   184
      CSFPB_FSIZE='LENGHT OF THE SIDE KAPTON FILM'                        184
C Z COORD                                                                 185
      SFPB_ZCOOR=5.18462095393834410                                      185
      CSFPB_ZCOOR='Z COORD'                                               185
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPB',    186
     *BPATH_SFPB,IDNUM_SFPB,1, MAP_SFPB,NAMES_SFPB,COMMEN_SFPB,           186
     *BANK_SFPB,LEN_SFPB,LLEN_SFPB,LINK_SFPB,FLAG_SFPB)                   186
C *                                                                       186
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPB',    186
     *BPATH_SFPB,IDNUM_SFPB,1, MAP_SFPB,NAMES_SFPB,COMMEN_SFPB,           186
     *BANK_SFPB,LEN_SFPB,LLEN_SFPB,LINK_SFPB,FLAG_SFPB)                   186
C *                                                                       187
      CALL AGSBEGM(MODULE,IPRIN)                                          187
      IF (IPRIN.LT.0) RETURN                                              187
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   187
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      187
      BPATH_SFPA='SFPA*'                                                  187
      IDNUM_SFPA(1)=0                                                     188
C GEOMETRY VERSION                                                        188
      SFPA_VERSION=1                                                      188
      CSFPA_VERSION='GEOMETRY VERSION'                                    188
C MOTHER RMIN                                                             189
      SFPA_RMIN=21.8                                                      189
      CSFPA_RMIN='MOTHER RMIN'                                            189
C MOTHER RMAX                                                             190
      SFPA_RMAX=29.5                                                      190
      CSFPA_RMAX='MOTHER RMAX'                                            190
C MOTHER LEN ALONG THE Z DIRECTION                                        191
      SFPA_LEN=120.                                                       191
      CSFPA_LEN='MOTHER LEN ALONG THE Z DIRECTION'                        191
C DISTANCE FROM BEAM AXIS TO DETECTOR CENTER                              192
      SFPA_RAD=23.                                                        192
      CSFPA_RAD='DISTANCE FROM BEAM AXIS TO DETECTOR CENTER'              192
C NUMBER OF SILICON STRIP DETECTORS                                       193
      SFPA_NSSD=16                                                        193
      CSFPA_NSSD='NUMBER OF SILICON STRIP DETECTORS'                      193
C DETECTOR MOTHER WIDTH                                                   194
      SFPA_DMWID=7.8                                                      194
      CSFPA_DMWID='DETECTOR MOTHER WIDTH'                                 194
C DETECTOR MOTHER THICKNESS                                               195
      SFPA_DMTHK=2.0                                                      195
      CSFPA_DMTHK='DETECTOR MOTHER THICKNESS'                             195
C DETECTOR MOTHER LENGTH (DETECTORS + ADC BOARD)                          196
      SFPA_DMLEN=90.                                                      196
      CSFPA_DMLEN='DETECTOR MOTHER LENGTH (DETECTORS + ADC BOARD)'        196
C STRUCTURE MOTHER WIDTH                                                  197
      SFPA_SMWID=7.5                                                      197
      CSFPA_SMWID='STRUCTURE MOTHER WIDTH'                                197
C STRUCTURE MOTHER THICKNESS                                              198
      SFPA_SMTHK=5.0                                                      198
      CSFPA_SMTHK='STRUCTURE MOTHER THICKNESS'                            198
C STRUCTURE MOTHER LENGTH                                                 199
      SFPA_SMLEN=101.9                                                    199
      CSFPA_SMLEN='STRUCTURE MOTHER LENGTH'                               199
C LENGTH OF A SUBVOLUME OF THE STRUCTURE                                  200
      SFPA_SSLEN=95./20.                                                  200
      CSFPA_SSLEN='LENGTH OF A SUBVOLUME OF THE STRUCTURE'                200
C LENGTH OF WAFER PACK                                                    201
      SFPA_WPLEN=68.8                                                     201
      CSFPA_WPLEN='LENGTH OF WAFER PACK'                                  201
C LENGTH OF ONE STRIP DETECTOR (ALONG BEAM AXIS)                          202
      SFPA_SDLEN=4.2                                                      202
      CSFPA_SDLEN='LENGTH OF ONE STRIP DETECTOR (ALONG BEAM AXIS)'        202
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
      SFPA_GPTHK=-1.0                                                     207
      CSFPA_GPTHK='GAP BETWEEN STRUCTURE MOTHER AND DETECTOR'             207
C PRESENCE OF LADDERS                                                     209
      SFPA_LADDERMAP(1)=0                                                 209
      SFPA_LADDERMAP(2)=0                                                 209
      SFPA_LADDERMAP(3)=1                                                 209
      SFPA_LADDERMAP(4)=0                                                 209
      SFPA_LADDERMAP(5)=0                                                 209
      SFPA_LADDERMAP(6)=0                                                 209
      SFPA_LADDERMAP(7)=0                                                 209
      SFPA_LADDERMAP(8)=0                                                 209
      SFPA_LADDERMAP(9)=0                                                 209
      SFPA_LADDERMAP(10)=0                                                209
      SFPA_LADDERMAP(11)=0                                                209
      SFPA_LADDERMAP(12)=0                                                209
      SFPA_LADDERMAP(13)=0                                                209
      SFPA_LADDERMAP(14)=0                                                209
      SFPA_LADDERMAP(15)=0                                                209
      SFPA_LADDERMAP(16)=0                                                209
      SFPA_LADDERMAP(17)=0                                                209
      SFPA_LADDERMAP(18)=0                                                209
      SFPA_LADDERMAP(19)=0                                                209
      SFPA_LADDERMAP(20)=0                                                209
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               209
C INDIVIDUAL ANGLES                                                       213
      SFPA_LADDERANGLE(1)=-1.0                                            213
      SFPA_LADDERANGLE(2)=-1.0                                            213
      SFPA_LADDERANGLE(3)=45.0                                            213
      SFPA_LADDERANGLE(4)=-1.0                                            213
      SFPA_LADDERANGLE(5)=-1.0                                            213
      SFPA_LADDERANGLE(6)=-1.0                                            213
      SFPA_LADDERANGLE(7)=-1.0                                            213
      SFPA_LADDERANGLE(8)=-1.0                                            213
      SFPA_LADDERANGLE(9)=-1.0                                            213
      SFPA_LADDERANGLE(10)=-1.0                                           213
      SFPA_LADDERANGLE(11)=-1.0                                           213
      SFPA_LADDERANGLE(12)=-1.0                                           213
      SFPA_LADDERANGLE(13)=-1.0                                           213
      SFPA_LADDERANGLE(14)=-1.0                                           213
      SFPA_LADDERANGLE(15)=-1.0                                           213
      SFPA_LADDERANGLE(16)=-1.0                                           213
      SFPA_LADDERANGLE(17)=-1.0                                           213
      SFPA_LADDERANGLE(18)=-1.0                                           213
      SFPA_LADDERANGLE(19)=-1.0                                           213
      SFPA_LADDERANGLE(20)=-1.0                                           213
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               213
C INDIVIDUAL TILTS                                                        217
      SFPA_LADDERTILT(1)=-1.0                                             217
      SFPA_LADDERTILT(2)=-1.0                                             217
      SFPA_LADDERTILT(3)=0.0                                              217
      SFPA_LADDERTILT(4)=-1.0                                             217
      SFPA_LADDERTILT(5)=-1.0                                             217
      SFPA_LADDERTILT(6)=-1.0                                             217
      SFPA_LADDERTILT(7)=-1.0                                             217
      SFPA_LADDERTILT(8)=-1.0                                             217
      SFPA_LADDERTILT(9)=-1.0                                             217
      SFPA_LADDERTILT(10)=-1.0                                            217
      SFPA_LADDERTILT(11)=-1.0                                            217
      SFPA_LADDERTILT(12)=-1.0                                            217
      SFPA_LADDERTILT(13)=-1.0                                            217
      SFPA_LADDERTILT(14)=-1.0                                            217
      SFPA_LADDERTILT(15)=-1.0                                            217
      SFPA_LADDERTILT(16)=-1.0                                            217
      SFPA_LADDERTILT(17)=-1.0                                            217
      SFPA_LADDERTILT(18)=-1.0                                            217
      SFPA_LADDERTILT(19)=-1.0                                            217
      SFPA_LADDERTILT(20)=-1.0                                            217
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 217
C INDIVIDUAL RADII                                                        222
      SFPA_LADDERRADIUS(1)=-1.0                                           222
      SFPA_LADDERRADIUS(2)=-1.0                                           222
      SFPA_LADDERRADIUS(3)=23.000                                         222
      SFPA_LADDERRADIUS(4)=-1.0                                           222
      SFPA_LADDERRADIUS(5)=-1.0                                           222
      SFPA_LADDERRADIUS(6)=-1.0                                           222
      SFPA_LADDERRADIUS(7)=-1.0                                           222
      SFPA_LADDERRADIUS(8)=-1.0                                           222
      SFPA_LADDERRADIUS(9)=-1.0                                           222
      SFPA_LADDERRADIUS(10)=-1.0                                          222
      SFPA_LADDERRADIUS(11)=-1.0                                          222
      SFPA_LADDERRADIUS(12)=-1.0                                          222
      SFPA_LADDERRADIUS(13)=-1.0                                          222
      SFPA_LADDERRADIUS(14)=-1.0                                          222
      SFPA_LADDERRADIUS(15)=-1.0                                          222
      SFPA_LADDERRADIUS(16)=-1.0                                          222
      SFPA_LADDERRADIUS(17)=-1.0                                          222
      SFPA_LADDERRADIUS(18)=-1.0                                          222
      SFPA_LADDERRADIUS(19)=-1.0                                          222
      SFPA_LADDERRADIUS(20)=-1.0                                          222
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               222
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    223
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           223
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   223
C *                                                                       223
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    223
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           223
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   223
C *                                                                       224
      CALL AGSBEGM(MODULE,IPRIN)                                          224
      IF (IPRIN.LT.0) RETURN                                              224
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   224
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      224
      BPATH_SFPA='SFPA*'                                                  224
      IDNUM_SFPA(1)=0                                                     225
C GEOMETRY VERSION                                                        225
      SFPA_VERSION=2                                                      225
      CSFPA_VERSION='GEOMETRY VERSION'                                    225
C PRESENCE OF LADDERS                                                     227
      SFPA_LADDERMAP(1)=1                                                 227
      SFPA_LADDERMAP(2)=1                                                 227
      SFPA_LADDERMAP(3)=1                                                 227
      SFPA_LADDERMAP(4)=0                                                 227
      SFPA_LADDERMAP(5)=0                                                 227
      SFPA_LADDERMAP(6)=0                                                 227
      SFPA_LADDERMAP(7)=0                                                 227
      SFPA_LADDERMAP(8)=0                                                 227
      SFPA_LADDERMAP(9)=1                                                 227
      SFPA_LADDERMAP(10)=1                                                227
      SFPA_LADDERMAP(11)=1                                                227
      SFPA_LADDERMAP(12)=1                                                227
      SFPA_LADDERMAP(13)=1                                                227
      SFPA_LADDERMAP(14)=0                                                227
      SFPA_LADDERMAP(15)=0                                                227
      SFPA_LADDERMAP(16)=0                                                227
      SFPA_LADDERMAP(17)=0                                                227
      SFPA_LADDERMAP(18)=0                                                227
      SFPA_LADDERMAP(19)=1                                                227
      SFPA_LADDERMAP(20)=1                                                227
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               227
C INDIVIDUAL ANGLES                                                       231
      SFPA_LADDERANGLE(1)=90.0                                            231
      SFPA_LADDERANGLE(2)=108.3                                           231
      SFPA_LADDERANGLE(3)=130.0                                           231
      SFPA_LADDERANGLE(4)=-1.0                                            231
      SFPA_LADDERANGLE(5)=-1.0                                            231
      SFPA_LADDERANGLE(6)=-1.0                                            231
      SFPA_LADDERANGLE(7)=-1.0                                            231
      SFPA_LADDERANGLE(8)=-1.0                                            231
      SFPA_LADDERANGLE(9)=230.0                                           231
      SFPA_LADDERANGLE(10)=251.7                                          231
      SFPA_LADDERANGLE(11)=270.0                                          231
      SFPA_LADDERANGLE(12)=288.3                                          231
      SFPA_LADDERANGLE(13)=310.0                                          231
      SFPA_LADDERANGLE(14)=-1.0                                           231
      SFPA_LADDERANGLE(15)=-1.0                                           231
      SFPA_LADDERANGLE(16)=-1.0                                           231
      SFPA_LADDERANGLE(17)=-1.0                                           231
      SFPA_LADDERANGLE(18)=-1.0                                           231
      SFPA_LADDERANGLE(19)=50.0                                           231
      SFPA_LADDERANGLE(20)=71.7                                           231
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               231
C INDIVIDUAL TILTS                                                        235
      SFPA_LADDERTILT(1)=0.0                                              235
      SFPA_LADDERTILT(2)=-6.0                                             235
      SFPA_LADDERTILT(3)=0.0                                              235
      SFPA_LADDERTILT(4)=-1.0                                             235
      SFPA_LADDERTILT(5)=-1.0                                             235
      SFPA_LADDERTILT(6)=-1.0                                             235
      SFPA_LADDERTILT(7)=-1.0                                             235
      SFPA_LADDERTILT(8)=0.0                                              235
      SFPA_LADDERTILT(9)=0.0                                              235
      SFPA_LADDERTILT(10)=6.0                                             235
      SFPA_LADDERTILT(11)=0.0                                             235
      SFPA_LADDERTILT(12)=-6.0                                            235
      SFPA_LADDERTILT(13)=0.0                                             235
      SFPA_LADDERTILT(14)=-1.0                                            235
      SFPA_LADDERTILT(15)=-1.0                                            235
      SFPA_LADDERTILT(16)=-1.0                                            235
      SFPA_LADDERTILT(17)=-1.0                                            235
      SFPA_LADDERTILT(18)=-1.0                                            235
      SFPA_LADDERTILT(19)=0.0                                             235
      SFPA_LADDERTILT(20)=6.0                                             235
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 235
C INDIVIDUAL RADII                                                        240
      SFPA_LADDERRADIUS(1)=23.174                                         240
      SFPA_LADDERRADIUS(2)=22.800                                         240
      SFPA_LADDERRADIUS(3)=24.600                                         240
      SFPA_LADDERRADIUS(4)=-1.0                                           240
      SFPA_LADDERRADIUS(5)=-1.0                                           240
      SFPA_LADDERRADIUS(6)=-1.0                                           240
      SFPA_LADDERRADIUS(7)=-1.0                                           240
      SFPA_LADDERRADIUS(8)=-1.0                                           240
      SFPA_LADDERRADIUS(9)=24.600                                         240
      SFPA_LADDERRADIUS(10)=22.800                                        240
      SFPA_LADDERRADIUS(11)=23.174                                        240
      SFPA_LADDERRADIUS(12)=22.800                                        240
      SFPA_LADDERRADIUS(13)=24.600                                        240
      SFPA_LADDERRADIUS(14)=-1.0                                          240
      SFPA_LADDERRADIUS(15)=-1.0                                          240
      SFPA_LADDERRADIUS(16)=-1.0                                          240
      SFPA_LADDERRADIUS(17)=-1.0                                          240
      SFPA_LADDERRADIUS(18)=-1.0                                          240
      SFPA_LADDERRADIUS(19)=24.600                                        240
      SFPA_LADDERRADIUS(20)=22.800                                        240
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               240
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    241
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           241
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   241
C *                                                                       241
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    241
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           241
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   241
C *                                                                       242
      CALL AGSBEGM(MODULE,IPRIN)                                          242
      IF (IPRIN.LT.0) RETURN                                              242
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   242
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      242
      BPATH_SFPA='SFPA*'                                                  242
      IDNUM_SFPA(1)=0                                                     243
C GEOMETRY VERSION                                                        243
      SFPA_VERSION=3                                                      243
      CSFPA_VERSION='GEOMETRY VERSION'                                    243
C PRESENCE OF LADDERS                                                     245
      SFPA_LADDERMAP(1)=1                                                 245
      SFPA_LADDERMAP(2)=1                                                 245
      SFPA_LADDERMAP(3)=1                                                 245
      SFPA_LADDERMAP(4)=1                                                 245
      SFPA_LADDERMAP(5)=1                                                 245
      SFPA_LADDERMAP(6)=1                                                 245
      SFPA_LADDERMAP(7)=1                                                 245
      SFPA_LADDERMAP(8)=1                                                 245
      SFPA_LADDERMAP(9)=1                                                 245
      SFPA_LADDERMAP(10)=1                                                245
      SFPA_LADDERMAP(11)=1                                                245
      SFPA_LADDERMAP(12)=1                                                245
      SFPA_LADDERMAP(13)=1                                                245
      SFPA_LADDERMAP(14)=1                                                245
      SFPA_LADDERMAP(15)=1                                                245
      SFPA_LADDERMAP(16)=1                                                245
      SFPA_LADDERMAP(17)=1                                                245
      SFPA_LADDERMAP(18)=1                                                245
      SFPA_LADDERMAP(19)=1                                                245
      SFPA_LADDERMAP(20)=1                                                245
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               245
C INDIVIDUAL ANGLES                                                       249
      SFPA_LADDERANGLE(1)=90.0                                            249
      SFPA_LADDERANGLE(2)=108.3                                           249
      SFPA_LADDERANGLE(3)=126.6                                           249
      SFPA_LADDERANGLE(4)=144.4                                           249
      SFPA_LADDERANGLE(5)=162.2                                           249
      SFPA_LADDERANGLE(6)=180.0                                           249
      SFPA_LADDERANGLE(7)=197.8                                           249
      SFPA_LADDERANGLE(8)=215.6                                           249
      SFPA_LADDERANGLE(9)=233.4                                           249
      SFPA_LADDERANGLE(10)=251.7                                          249
      SFPA_LADDERANGLE(11)=270.0                                          249
      SFPA_LADDERANGLE(12)=288.3                                          249
      SFPA_LADDERANGLE(13)=306.6                                          249
      SFPA_LADDERANGLE(14)=324.4                                          249
      SFPA_LADDERANGLE(15)=342.2                                          249
      SFPA_LADDERANGLE(16)=0.0                                            249
      SFPA_LADDERANGLE(17)=17.8                                           249
      SFPA_LADDERANGLE(18)=35.6                                           249
      SFPA_LADDERANGLE(19)=53.4                                           249
      SFPA_LADDERANGLE(20)=71.7                                           249
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               249
C INDIVIDUAL TILTS                                                        253
      SFPA_LADDERTILT(1)=0.0                                              253
      SFPA_LADDERTILT(2)=-6.0                                             253
      SFPA_LADDERTILT(3)=-7.0                                             253
      SFPA_LADDERTILT(4)=-7.0                                             253
      SFPA_LADDERTILT(5)=-7.0                                             253
      SFPA_LADDERTILT(6)=0.0                                              253
      SFPA_LADDERTILT(7)=7.0                                              253
      SFPA_LADDERTILT(8)=7.0                                              253
      SFPA_LADDERTILT(9)=7.0                                              253
      SFPA_LADDERTILT(10)=6.0                                             253
      SFPA_LADDERTILT(11)=0.0                                             253
      SFPA_LADDERTILT(12)=-6.0                                            253
      SFPA_LADDERTILT(13)=-7.0                                            253
      SFPA_LADDERTILT(14)=-7.0                                            253
      SFPA_LADDERTILT(15)=-7.0                                            253
      SFPA_LADDERTILT(16)=0.0                                             253
      SFPA_LADDERTILT(17)=7.0                                             253
      SFPA_LADDERTILT(18)=7.0                                             253
      SFPA_LADDERTILT(19)=7.0                                             253
      SFPA_LADDERTILT(20)=6.0                                             253
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 253
C INDIVIDUAL RADII                                                        258
      SFPA_LADDERRADIUS(1)=23.177                                         258
      SFPA_LADDERRADIUS(2)=22.800                                         258
      SFPA_LADDERRADIUS(3)=22.800                                         258
      SFPA_LADDERRADIUS(4)=22.800                                         258
      SFPA_LADDERRADIUS(5)=23.800                                         258
      SFPA_LADDERRADIUS(6)=22.500                                         258
      SFPA_LADDERRADIUS(7)=23.800                                         258
      SFPA_LADDERRADIUS(8)=22.800                                         258
      SFPA_LADDERRADIUS(9)=22.800                                         258
      SFPA_LADDERRADIUS(10)=22.800                                        258
      SFPA_LADDERRADIUS(11)=23.177                                        258
      SFPA_LADDERRADIUS(12)=22.800                                        258
      SFPA_LADDERRADIUS(13)=22.800                                        258
      SFPA_LADDERRADIUS(14)=22.800                                        258
      SFPA_LADDERRADIUS(15)=23.800                                        258
      SFPA_LADDERRADIUS(16)=22.500                                        258
      SFPA_LADDERRADIUS(17)=23.800                                        258
      SFPA_LADDERRADIUS(18)=22.800                                        258
      SFPA_LADDERRADIUS(19)=22.800                                        258
      SFPA_LADDERRADIUS(20)=22.800                                        258
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               258
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    259
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           259
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   259
C *                                                                       259
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    259
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           259
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   259
C *                                                                       260
      CALL AGSBEGM(MODULE,IPRIN)                                          260
      IF (IPRIN.LT.0) RETURN                                              260
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   260
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      260
      BPATH_SFPA='SFPA*'                                                  260
      IDNUM_SFPA(1)=0                                                     261
C GEOMETRY VERSION                                                        261
      SFPA_VERSION=4                                                      261
      CSFPA_VERSION='GEOMETRY VERSION'                                    261
C PRESENCE OF LADDERS                                                     263
      SFPA_LADDERMAP(1)=1                                                 263
      SFPA_LADDERMAP(2)=1                                                 263
      SFPA_LADDERMAP(3)=1                                                 263
      SFPA_LADDERMAP(4)=1                                                 263
      SFPA_LADDERMAP(5)=1                                                 263
      SFPA_LADDERMAP(6)=1                                                 263
      SFPA_LADDERMAP(7)=1                                                 263
      SFPA_LADDERMAP(8)=1                                                 263
      SFPA_LADDERMAP(9)=1                                                 263
      SFPA_LADDERMAP(10)=1                                                263
      SFPA_LADDERMAP(11)=1                                                263
      SFPA_LADDERMAP(12)=1                                                263
      SFPA_LADDERMAP(13)=1                                                263
      SFPA_LADDERMAP(14)=1                                                263
      SFPA_LADDERMAP(15)=1                                                263
      SFPA_LADDERMAP(16)=1                                                263
      SFPA_LADDERMAP(17)=1                                                263
      SFPA_LADDERMAP(18)=1                                                263
      SFPA_LADDERMAP(19)=1                                                263
      SFPA_LADDERMAP(20)=1                                                263
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               263
C INDIVIDUAL ANGLES                                                       267
      SFPA_LADDERANGLE(1)=90.0                                            267
      SFPA_LADDERANGLE(2)=108.3                                           267
      SFPA_LADDERANGLE(3)=126.6                                           267
      SFPA_LADDERANGLE(4)=144.4                                           267
      SFPA_LADDERANGLE(5)=162.2                                           267
      SFPA_LADDERANGLE(6)=180.0                                           267
      SFPA_LADDERANGLE(7)=197.8                                           267
      SFPA_LADDERANGLE(8)=215.6                                           267
      SFPA_LADDERANGLE(9)=233.4                                           267
      SFPA_LADDERANGLE(10)=251.7                                          267
      SFPA_LADDERANGLE(11)=270.0                                          267
      SFPA_LADDERANGLE(12)=288.3                                          267
      SFPA_LADDERANGLE(13)=306.6                                          267
      SFPA_LADDERANGLE(14)=324.4                                          267
      SFPA_LADDERANGLE(15)=342.2                                          267
      SFPA_LADDERANGLE(16)=0.0                                            267
      SFPA_LADDERANGLE(17)=17.8                                           267
      SFPA_LADDERANGLE(18)=35.6                                           267
      SFPA_LADDERANGLE(19)=53.4                                           267
      SFPA_LADDERANGLE(20)=71.7                                           267
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               267
C INDIVIDUAL TILTS                                                        271
      SFPA_LADDERTILT(1)=0.0                                              271
      SFPA_LADDERTILT(2)=-6.0                                             271
      SFPA_LADDERTILT(3)=-7.0                                             271
      SFPA_LADDERTILT(4)=-7.0                                             271
      SFPA_LADDERTILT(5)=-7.0                                             271
      SFPA_LADDERTILT(6)=0.0                                              271
      SFPA_LADDERTILT(7)=7.0                                              271
      SFPA_LADDERTILT(8)=7.0                                              271
      SFPA_LADDERTILT(9)=7.0                                              271
      SFPA_LADDERTILT(10)=6.0                                             271
      SFPA_LADDERTILT(11)=0.0                                             271
      SFPA_LADDERTILT(12)=-6.0                                            271
      SFPA_LADDERTILT(13)=-7.0                                            271
      SFPA_LADDERTILT(14)=-7.0                                            271
      SFPA_LADDERTILT(15)=-7.0                                            271
      SFPA_LADDERTILT(16)=0.0                                             271
      SFPA_LADDERTILT(17)=7.0                                             271
      SFPA_LADDERTILT(18)=7.0                                             271
      SFPA_LADDERTILT(19)=7.0                                             271
      SFPA_LADDERTILT(20)=6.0                                             271
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 271
C INDIVIDUAL RADII                                                        276
      SFPA_LADDERRADIUS(1)=23.177                                         276
      SFPA_LADDERRADIUS(2)=22.800                                         276
      SFPA_LADDERRADIUS(3)=22.800                                         276
      SFPA_LADDERRADIUS(4)=22.800                                         276
      SFPA_LADDERRADIUS(5)=22.800                                         276
      SFPA_LADDERRADIUS(6)=22.500                                         276
      SFPA_LADDERRADIUS(7)=22.800                                         276
      SFPA_LADDERRADIUS(8)=22.800                                         276
      SFPA_LADDERRADIUS(9)=22.800                                         276
      SFPA_LADDERRADIUS(10)=22.800                                        276
      SFPA_LADDERRADIUS(11)=23.177                                        276
      SFPA_LADDERRADIUS(12)=22.800                                        276
      SFPA_LADDERRADIUS(13)=22.800                                        276
      SFPA_LADDERRADIUS(14)=22.800                                        276
      SFPA_LADDERRADIUS(15)=22.800                                        276
      SFPA_LADDERRADIUS(16)=22.500                                        276
      SFPA_LADDERRADIUS(17)=22.800                                        276
      SFPA_LADDERRADIUS(18)=22.800                                        276
      SFPA_LADDERRADIUS(19)=22.800                                        276
      SFPA_LADDERRADIUS(20)=22.800                                        276
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               276
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    277
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           277
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   277
C *                                                                       277
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    277
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           277
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   277
C use default SSDP bank                                                   278
      BPATH_SSDP='SSDP*'                                                  278
      IDNUM_SSDP(1)=0                                                     278
      CALL AGDGETP('SISDGEO2','*',-999,'SSDP',' ',BANK_SSDP, BPATH_SSDP,  278
     *IDNUM_SSDP,1,MAP_SSDP,NAMES_SSDP,BANK_SSDP,LEN_SSDP,LLEN_SSDP,      278
     *LINK_SSDP,FLAG_SSDP)                                                278
C use SFPA bank                                                           279
      BPATH_SFPA='SFPA*'                                                  279
      IDNUM_SFPA(1)=0                                                     279
      SFPA_VERSION=SSDP_CONFIG                                            279
      CALL AGDGETP('SISDGEO2','*',-999,'SFPA',NSFPA_VERSION,              279
     *RE_SFPA_VERSION,BPATH_SFPA,IDNUM_SFPA,1,MAP_SFPA,NAMES_SFPA,        279
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   279
C use default SFJP bank                                                   280
      BPATH_SFJP='SFJP*'                                                  280
      IDNUM_SFJP(1)=0                                                     280
      CALL AGDGETP('SISDGEO2','*',-999,'SFJP',' ',BANK_SFJP, BPATH_SFJP,  280
     *IDNUM_SFJP,1,MAP_SFJP,NAMES_SFJP,BANK_SFJP,LEN_SFJP,LLEN_SFJP,      280
     *LINK_SFJP,FLAG_SFJP)                                                280
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             281
      CALL AGSRESET                                                       281
      AG_A = 28.08                                                        281
      AG_Z = 14                                                           281
      AG_W = 0.6*1*28./60.                                                281
      AG_TITLE = 'SI'                                                     281
      AG_EXNAME = 'COMPONENT'                                             281
      AG_PARLIST = 'A_Z_W'                                                281
      CALL AXCOMPONENT                                                    281
C *                                                                       281
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  281
C =                                                                       281
      CALL AGSRESET                                                       282
      AG_A = 16                                                           282
      AG_Z = 8                                                            282
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               282
      AG_TITLE = 'O'                                                      282
      AG_EXNAME = 'COMPONENT'                                             282
      AG_PARLIST = 'A_Z_W'                                                282
      CALL AXCOMPONENT                                                    282
C *                                                                       282
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 282
      CALL AGSRESET                                                       283
      AG_A = 12                                                           283
      AG_Z = 6                                                            283
      AG_W = 0.4*8*12./174.                                               283
      AG_TITLE = 'C'                                                      283
      AG_EXNAME = 'COMPONENT'                                             283
      AG_PARLIST = 'A_Z_W'                                                283
      CALL AXCOMPONENT                                                    283
C *                                                                       283
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  283
      CALL AGSRESET                                                       284
      AG_A = 1                                                            284
      AG_Z = 1                                                            284
      AG_W = 0.4*14*1./174.                                               284
      AG_TITLE = 'H'                                                      284
      AG_EXNAME = 'COMPONENT'                                             284
      AG_PARLIST = 'A_Z_W'                                                284
      CALL AXCOMPONENT                                                    284
C *                                                                       284
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  284
      CALL AGSRESET                                                       285
      AG_DENS = 1.7                                                       285
      AG_TITLE = 'G10'                                                    285
      AG_EXNAME = 'MIXTURE'                                               285
      AG_PARLIST = 'DENS'                                                 285
      CALL AXMIXTURE                                                      285
C *                                                                       285
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             285
      CALL AGSRESET                                                       286
      AG_A = 28.08                                                        286
      AG_Z = 14                                                           286
      AG_W = 0.6*1*28./60.                                                286
      AG_TITLE = 'SI'                                                     286
      AG_EXNAME = 'COMPONENT'                                             286
      AG_PARLIST = 'A_Z_W'                                                286
      CALL AXCOMPONENT                                                    286
C *                                                                       286
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  286
C =                                                                       286
      CALL AGSRESET                                                       287
      AG_A = 16                                                           287
      AG_Z = 8                                                            287
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               287
      AG_TITLE = 'O'                                                      287
      AG_EXNAME = 'COMPONENT'                                             287
      AG_PARLIST = 'A_Z_W'                                                287
      CALL AXCOMPONENT                                                    287
C *                                                                       287
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 287
      CALL AGSRESET                                                       288
      AG_A = 12                                                           288
      AG_Z = 6                                                            288
      AG_W = 0.4*8*12./174.                                               288
      AG_TITLE = 'C'                                                      288
      AG_EXNAME = 'COMPONENT'                                             288
      AG_PARLIST = 'A_Z_W'                                                288
      CALL AXCOMPONENT                                                    288
C *                                                                       288
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  288
      CALL AGSRESET                                                       289
      AG_A = 1                                                            289
      AG_Z = 1                                                            289
      AG_W = 0.4*14*1./174.                                               289
      AG_TITLE = 'H'                                                      289
      AG_EXNAME = 'COMPONENT'                                             289
      AG_PARLIST = 'A_Z_W'                                                289
      CALL AXCOMPONENT                                                    289
C *                                                                       289
C Geant MIXTURE : G5 ===> DENS=0.85 <===                                  289
      CALL AGSRESET                                                       290
      AG_DENS = 0.85                                                      290
      AG_TITLE = 'G5'                                                     290
      AG_EXNAME = 'MIXTURE'                                               290
      AG_PARLIST = 'DENS'                                                 290
      CALL AXMIXTURE                                                      290
C *                                                                       290
C Check SSDP_PLACEMENT==1                                                 290
      IF (SSDP_PLACEMENT.EQ.1) THEN                                       291
      CALL AGSBEGM(MODULE,IPRIN)                                          292
C Check IPRIN<0                                                           292
      IF (IPRIN.LT.0) RETURN                                              292
C Create SFMO :                                                           292
C Call To Local Procedure SFMO                                            292
      ASSIGN 5020 TO BLOCK_SFMO                                           292
      GO TO  5010                                                         292
5020  CONTINUE                                                            292
C *                                                                       292
C Geant POSITION : SFMO ===> MOTHER='SVTT' <===                           293
      CALL AGSRESET                                                       293
      AG_MOTHER = 'SVTT'                                                  293
      AG_TITLE = 'SFMO'                                                   293
      AG_EXNAME = 'POSITION'                                              293
      AG_PARLIST = 'MOTHER'                                               293
      CALL AXPOSITION                                                     293
C *                                                                       293
      ELSE                                                                293
      CALL AGSBEGM(MODULE,IPRIN)                                          295
C Check IPRIN<0                                                           295
      IF (IPRIN.LT.0) RETURN                                              295
C Create SFMO :                                                           295
C Call To Local Procedure SFMO                                            295
      ASSIGN 5030 TO BLOCK_SFMO                                           295
      GO TO  5010                                                         295
5030  CONTINUE                                                            295
C *                                                                       295
C Geant POSITION : SFMO ===> MOTHER='CAVE' <===                           296
      CALL AGSRESET                                                       296
      AG_MOTHER = 'CAVE'                                                  296
      AG_TITLE = 'SFMO'                                                   296
      AG_EXNAME = 'POSITION'                                              296
      AG_PARLIST = 'MOTHER'                                               296
      CALL AXPOSITION                                                     296
C *                                                                       296
      ENDIF                                                               296
C ----------------------------------------------------------------------  298
C ------------ SFMO                                                       298
      CALL AGSENDM                                                        299
      RETURN                                                              299
C Begin Local Procedure SFMO                                              299
C SFMO IS THE MOTHER OF ALL SILICON STRIP DETECTOR VOLUMES                299
5010  CONTINUE                                                            299
         AG_TITLE='SFMO'                                                  299
         AG_MODULE=MODULE                                                 299
         AG_MARK='V00'                                                    299
         CALL AGSPUSH                                                     299
         AG_MODULE=MODULE                                                 299
C    *                                                                    299
C    Geant MATERIAL : AIR ===> inherited arguments used <===              300
         CALL AGSRESET                                                    300
         AG_TITLE = 'AIR'                                                 300
         AG_EXNAME = 'MATERIAL'                                           300
         AG_PARLIST = 'NONE'                                              300
         CALL AXMATERIAL                                                  300
C    *                                                                    300
C    Geant ATTRIBUTE : SFMO ===> SEEN=0 COLO=6 <===                       300
         CALL AGSRESET                                                    301
         AG_SEEN = 0                                                      301
         AG_COLO = 6                                                      301
         AG_TITLE = 'SFMO'                                                301
         AG_EXNAME = 'ATTRIBUTE'                                          301
         AG_PARLIST = 'SEEN_COLO'                                         301
         CALL AXATTRIBUTE                                                 301
C    *                                                                    301
C    Geant SHAPE : TUBE ===> RMIN=SFPA_RMIN RMAX=SFPA_RMAX, DZ=SFPA_LEN/  301
C    2 <===                                                               301
         CALL AGSRESET                                                    303
         AG_RMIN = SFPA_RMIN                                              303
         AG_RMAX = SFPA_RMAX                                              303
         AG_DZ = SFPA_LEN/2                                               303
         AG_TITLE = 'TUBE'                                                303
         AG_EXNAME = 'SHAPE'                                              303
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      303
         CALL AXSHAPE                                                     303
C    *                                                                    303
         DTHK = SFPA_SMTHK + SFPA_GPTHK                                   303
C    Create SFLM :                                                        305
C    Call To Local Procedure SFLM                                         305
         ASSIGN 5050 TO BLOCK_SFLM                                        305
         GO TO  5040                                                      305
5050     CONTINUE                                                         305
C    *                                                                    305
C    *                                                                    306
         ILAD=1                                                           306
            GO TO 5063                                                    306
5061        ILAD=ILAD+1                                                   306
5063        IF(ILAD-(20).GT.0) GO TO 5062                                 306
C       Check SFPA_LADDERMAP(ILAD).GT.0                                   307
            IF (SFPA_LADDERMAP(ILAD).GT.0) THEN                           307
            ANG = (SFPA_LADDERANGLE(ILAD)*PI)/180.0                       308
            RADTILT = (SFPA_LADDERTILT(ILAD) *PI)/180.0                   309
C       Check ILAD.EQ.1                                                   310
            IF (ILAD.EQ.1) THEN                                           310
            NC=1                                                          311
            ELSE                                                          312
            NC=20-ILAD+2                                                  313
            ENDIF                                                         314
C       Geant POSITION : SFLM ===> X=(SFPA_LADDERRADIUS(ILAD)*COS(ANG)+(  317
C       DTHK*COS(ANG+RADTILT))/2.0) Y=(SFPA_LADDERRADIUS(ILAD)*SIN(ANG)+  317
C       (DTHK*SIN(ANG+RADTILT))/2.0) Z=0 ALPHAZ=SFPA_LADDERANGLE(ILAD)-9  317
C       0.0+SFPA_LADDERTILT(ILAD) NCOPY=NC KONLY='MANY' <===              317
            CALL AGSRESET                                                 317
            AG_X =                                                        317
     *      (SFPA_LADDERRADIUS(ILAD)*COS(ANG)+(DTHK*COS(ANG+RADTILT))/    317
     *      2.0)                                                          317
            AG_Y =                                                        317
     *      (SFPA_LADDERRADIUS(ILAD)*SIN(ANG)+(DTHK*SIN(ANG+RADTILT))/    317
     *      2.0)                                                          317
            AG_Z = 0                                                      317
            AG_ALPHAN=AG_ALPHAN+1                                         317
            AG_ALPHAK(AG_ALPHAN)=3                                        317
            AG_ALPHAL(AG_ALPHAN)=                                         317
     *      SFPA_LADDERANGLE(ILAD)-90.0+SFPA_LADDERTILT(ILAD)             317
            AG_NCOPY = NC                                                 317
            AG_KONLY = 'MANY'                                             317
            AG_TITLE = 'SFLM'                                             317
            AG_EXNAME = 'POSITION'                                        317
            AG_PARLIST = 'X_Y_Z_ALPHAZ_NCOPY_KONLY'                       317
            CALL AXPOSITION                                               317
C       *                                                                 317
            ENDIF                                                         317
         GO TO 5061                                                       319
5062     CONTINUE                                                         319
C    Create SSST :                                                        320
C    Call To Local Procedure SSST                                         320
         ASSIGN 5080 TO BLOCK_SSST                                        320
         GO TO  5070                                                      320
5080     CONTINUE                                                         320
C    *                                                                    320
C    Geant POSITION : SSST ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  322
C    JP_SSST_WIDTH/2.-SFJP_SSST_PZ <===                                   322
         CALL AGSRESET                                                    322
         AG_X = 0.                                                        322
         AG_Y = 0.                                                        322
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSST_WIDTH/                322
     *   2.-SFJP_SSST_PZ                                                  322
         AG_TITLE = 'SSST'                                                322
         AG_EXNAME = 'POSITION'                                           322
         AG_PARLIST = 'X_Y_Z'                                             322
         CALL AXPOSITION                                                  322
C    *                                                                    322
C    Geant POSITION : SSST ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  322
C    JP_SSST_WIDTH/2.+SFJP_SSST_PZ <===                                   322
         CALL AGSRESET                                                    324
         AG_X = 0.                                                        324
         AG_Y = 0.                                                        324
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSST_WIDTH/                324
     *   2.+SFJP_SSST_PZ                                                  324
         AG_TITLE = 'SSST'                                                324
         AG_EXNAME = 'POSITION'                                           324
         AG_PARLIST = 'X_Y_Z'                                             324
         CALL AXPOSITION                                                  324
C    *                                                                    324
C    Geant POSITION : SSST ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  324
C    JP_SSST_WIDTH/2.-SFJP_SSST_PZ ALPHAZ=180. <===                       324
         CALL AGSRESET                                                    326
         AG_X = 0.                                                        326
         AG_Y = 0.                                                        326
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSST_WIDTH/                326
     *   2.-SFJP_SSST_PZ                                                  326
         AG_ALPHAN=AG_ALPHAN+1                                            326
         AG_ALPHAK(AG_ALPHAN)=3                                           326
         AG_ALPHAL(AG_ALPHAN)=180.                                        326
         AG_TITLE = 'SSST'                                                326
         AG_EXNAME = 'POSITION'                                           326
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      326
         CALL AXPOSITION                                                  326
C    *                                                                    326
C    Geant POSITION : SSST ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  326
C    JP_SSST_WIDTH/2.+SFJP_SSST_PZ ALPHAZ=180. <===                       326
         CALL AGSRESET                                                    328
         AG_X = 0.                                                        328
         AG_Y = 0.                                                        328
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSST_WIDTH/                328
     *   2.+SFJP_SSST_PZ                                                  328
         AG_ALPHAN=AG_ALPHAN+1                                            328
         AG_ALPHAK(AG_ALPHAN)=3                                           328
         AG_ALPHAL(AG_ALPHAN)=180.                                        328
         AG_TITLE = 'SSST'                                                328
         AG_EXNAME = 'POSITION'                                           328
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      328
         CALL AXPOSITION                                                  328
C    *                                                                    328
C    Create SSSS :                                                        329
C    Call To Local Procedure SSSS                                         329
         ASSIGN 5100 TO BLOCK_SSSS                                        329
         GO TO  5090                                                      329
5100     CONTINUE                                                         329
C    *                                                                    329
C    Geant POSITION : SSSS ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  331
C    JP_SSSS_WIDTH/2.-SFJP_SSST_PZ <===                                   331
         CALL AGSRESET                                                    331
         AG_X = 0.                                                        331
         AG_Y = 0.                                                        331
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH/                331
     *   2.-SFJP_SSST_PZ                                                  331
         AG_TITLE = 'SSSS'                                                331
         AG_EXNAME = 'POSITION'                                           331
         AG_PARLIST = 'X_Y_Z'                                             331
         CALL AXPOSITION                                                  331
C    *                                                                    331
C    Geant POSITION : SSSS ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  331
C    JP_SSSS_WIDTH/2.+SFJP_SSST_PZ <===                                   331
         CALL AGSRESET                                                    333
         AG_X = 0.                                                        333
         AG_Y = 0.                                                        333
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH/                333
     *   2.+SFJP_SSST_PZ                                                  333
         AG_TITLE = 'SSSS'                                                333
         AG_EXNAME = 'POSITION'                                           333
         AG_PARLIST = 'X_Y_Z'                                             333
         CALL AXPOSITION                                                  333
C    *                                                                    333
C    Geant POSITION : SSSS ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  333
C    JP_SSSS_WIDTH/2.-SFJP_SSST_PZ ALPHAZ=180. <===                       333
         CALL AGSRESET                                                    335
         AG_X = 0.                                                        335
         AG_Y = 0.                                                        335
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH/                335
     *   2.-SFJP_SSST_PZ                                                  335
         AG_ALPHAN=AG_ALPHAN+1                                            335
         AG_ALPHAK(AG_ALPHAN)=3                                           335
         AG_ALPHAL(AG_ALPHAN)=180.                                        335
         AG_TITLE = 'SSSS'                                                335
         AG_EXNAME = 'POSITION'                                           335
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      335
         CALL AXPOSITION                                                  335
C    *                                                                    335
C    Geant POSITION : SSSS ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  335
C    JP_SSSS_WIDTH/2.+SFJP_SSST_PZ ALPHAZ=180. <===                       335
         CALL AGSRESET                                                    337
         AG_X = 0.                                                        337
         AG_Y = 0.                                                        337
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH/                337
     *   2.+SFJP_SSST_PZ                                                  337
         AG_ALPHAN=AG_ALPHAN+1                                            337
         AG_ALPHAK(AG_ALPHAN)=3                                           337
         AG_ALPHAL(AG_ALPHAN)=180.                                        337
         AG_TITLE = 'SSSS'                                                337
         AG_EXNAME = 'POSITION'                                           337
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      337
         CALL AXPOSITION                                                  337
C    *                                                                    337
C    Create SSRT :                                                        338
C    Call To Local Procedure SSRT                                         338
         ASSIGN 5120 TO BLOCK_SSRT                                        338
         GO TO  5110                                                      338
5120     CONTINUE                                                         338
C    *                                                                    338
C    Geant POSITION : SSRT ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  340
C    JP_SSST_WIDTH/2.-SFJP_SSST_PZ <===                                   340
         CALL AGSRESET                                                    340
         AG_X = 0.                                                        340
         AG_Y = 0.                                                        340
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSST_WIDTH/                340
     *   2.-SFJP_SSST_PZ                                                  340
         AG_TITLE = 'SSRT'                                                340
         AG_EXNAME = 'POSITION'                                           340
         AG_PARLIST = 'X_Y_Z'                                             340
         CALL AXPOSITION                                                  340
C    *                                                                    340
C    Geant POSITION : SSRT ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  340
C    JP_SSST_WIDTH/2.+SFJP_SSST_PZ <===                                   340
         CALL AGSRESET                                                    342
         AG_X = 0.                                                        342
         AG_Y = 0.                                                        342
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSST_WIDTH/                342
     *   2.+SFJP_SSST_PZ                                                  342
         AG_TITLE = 'SSRT'                                                342
         AG_EXNAME = 'POSITION'                                           342
         AG_PARLIST = 'X_Y_Z'                                             342
         CALL AXPOSITION                                                  342
C    *                                                                    342
C    Geant POSITION : SSRT ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  342
C    JP_SSST_WIDTH/2.-SFJP_SSST_PZ ALPHAZ=180. <===                       342
         CALL AGSRESET                                                    344
         AG_X = 0.                                                        344
         AG_Y = 0.                                                        344
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSST_WIDTH/                344
     *   2.-SFJP_SSST_PZ                                                  344
         AG_ALPHAN=AG_ALPHAN+1                                            344
         AG_ALPHAK(AG_ALPHAN)=3                                           344
         AG_ALPHAL(AG_ALPHAN)=180.                                        344
         AG_TITLE = 'SSRT'                                                344
         AG_EXNAME = 'POSITION'                                           344
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      344
         CALL AXPOSITION                                                  344
C    *                                                                    344
C    Geant POSITION : SSRT ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  344
C    JP_SSST_WIDTH/2.+SFJP_SSST_PZ ALPHAZ=180. <===                       344
         CALL AGSRESET                                                    346
         AG_X = 0.                                                        346
         AG_Y = 0.                                                        346
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSST_WIDTH/                346
     *   2.+SFJP_SSST_PZ                                                  346
         AG_ALPHAN=AG_ALPHAN+1                                            346
         AG_ALPHAK(AG_ALPHAN)=3                                           346
         AG_ALPHAL(AG_ALPHAN)=180.                                        346
         AG_TITLE = 'SSRT'                                                346
         AG_EXNAME = 'POSITION'                                           346
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      346
         CALL AXPOSITION                                                  346
C    *                                                                    346
C    Create SSRS :                                                        347
C    Call To Local Procedure SSRS                                         347
         ASSIGN 5140 TO BLOCK_SSRS                                        347
         GO TO  5130                                                      347
5140     CONTINUE                                                         347
C    *                                                                    347
C    Geant POSITION : SSRS ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  349
C    JP_SSSS_WIDTH/2.-SFJP_SSST_PZ <===                                   349
         CALL AGSRESET                                                    349
         AG_X = 0.                                                        349
         AG_Y = 0.                                                        349
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH/                349
     *   2.-SFJP_SSST_PZ                                                  349
         AG_TITLE = 'SSRS'                                                349
         AG_EXNAME = 'POSITION'                                           349
         AG_PARLIST = 'X_Y_Z'                                             349
         CALL AXPOSITION                                                  349
C    *                                                                    349
C    Geant POSITION : SSRS ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  349
C    JP_SSSS_WIDTH/2.+SFJP_SSST_PZ <===                                   349
         CALL AGSRESET                                                    351
         AG_X = 0.                                                        351
         AG_Y = 0.                                                        351
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH/                351
     *   2.+SFJP_SSST_PZ                                                  351
         AG_TITLE = 'SSRS'                                                351
         AG_EXNAME = 'POSITION'                                           351
         AG_PARLIST = 'X_Y_Z'                                             351
         CALL AXPOSITION                                                  351
C    *                                                                    351
C    Geant POSITION : SSRS ===> X=0. Y=0. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-SF  351
C    JP_SSSS_WIDTH/2.-SFJP_SSST_PZ ALPHAZ=180. <===                       351
         CALL AGSRESET                                                    353
         AG_X = 0.                                                        353
         AG_Y = 0.                                                        353
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH/                353
     *   2.-SFJP_SSST_PZ                                                  353
         AG_ALPHAN=AG_ALPHAN+1                                            353
         AG_ALPHAK(AG_ALPHAN)=3                                           353
         AG_ALPHAL(AG_ALPHAN)=180.                                        353
         AG_TITLE = 'SSRS'                                                353
         AG_EXNAME = 'POSITION'                                           353
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      353
         CALL AXPOSITION                                                  353
C    *                                                                    353
C    Geant POSITION : SSRS ===> X=0. Y=0. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+SF  353
C    JP_SSSS_WIDTH/2.+SFJP_SSST_PZ ALPHAZ=180. <===                       353
         CALL AGSRESET                                                    355
         AG_X = 0.                                                        355
         AG_Y = 0.                                                        355
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH/                355
     *   2.+SFJP_SSST_PZ                                                  355
         AG_ALPHAN=AG_ALPHAN+1                                            355
         AG_ALPHAK(AG_ALPHAN)=3                                           355
         AG_ALPHAL(AG_ALPHAN)=180.                                        355
         AG_TITLE = 'SSRS'                                                355
         AG_EXNAME = 'POSITION'                                           355
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      355
         CALL AXPOSITION                                                  355
C    *                                                                    355
C    Create SSLB :                                                        356
C    Call To Local Procedure SSLB                                         356
         ASSIGN 5160 TO BLOCK_SSLB                                        356
         GO TO  5150                                                      356
5160     CONTINUE                                                         356
C    *                                                                    356
C    Geant POSITION : SSLB ===> X=SFJP_SSLB_PX Y=SFJP_SSLB_PX Z=-SFJP_SF  359
C    LU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ/2. ALPH  359
C    AZ=SFJP_ALPHAZH <===                                                 359
         CALL AGSRESET                                                    359
         AG_X = SFJP_SSLB_PX                                              359
         AG_Y = SFJP_SSLB_PX                                              359
         AG_Z =                                                           359
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          359
     *   SFJP_SSLB_DZ/2.                                                  359
         AG_ALPHAN=AG_ALPHAN+1                                            359
         AG_ALPHAK(AG_ALPHAN)=3                                           359
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                359
         AG_TITLE = 'SSLB'                                                359
         AG_EXNAME = 'POSITION'                                           359
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      359
         CALL AXPOSITION                                                  359
C    *                                                                    359
C    Geant POSITION : SSLB ===> X=-SFJP_SSLB_PX Y=-SFJP_SSLB_PX Z=-SFJP_  359
C    SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ/2. AL  359
C    PHAZ=SFJP_ALPHAZH <===                                               359
         CALL AGSRESET                                                    362
         AG_X = -SFJP_SSLB_PX                                             362
         AG_Y = -SFJP_SSLB_PX                                             362
         AG_Z =                                                           362
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          362
     *   SFJP_SSLB_DZ/2.                                                  362
         AG_ALPHAN=AG_ALPHAN+1                                            362
         AG_ALPHAK(AG_ALPHAN)=3                                           362
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                362
         AG_TITLE = 'SSLB'                                                362
         AG_EXNAME = 'POSITION'                                           362
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      362
         CALL AXPOSITION                                                  362
C    *                                                                    362
C    Geant POSITION : SSLB ===> X=SFJP_SSLB_PX Y=-SFJP_SSLB_PX Z=-SFJP_S  362
C    FLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ/2. ALP  362
C    HAZ=-SFJP_ALPHAZH <===                                               362
         CALL AGSRESET                                                    365
         AG_X = SFJP_SSLB_PX                                              365
         AG_Y = -SFJP_SSLB_PX                                             365
         AG_Z =                                                           365
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          365
     *   SFJP_SSLB_DZ/2.                                                  365
         AG_ALPHAN=AG_ALPHAN+1                                            365
         AG_ALPHAK(AG_ALPHAN)=3                                           365
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               365
         AG_TITLE = 'SSLB'                                                365
         AG_EXNAME = 'POSITION'                                           365
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      365
         CALL AXPOSITION                                                  365
C    *                                                                    365
C    Geant POSITION : SSLB ===> X=-SFJP_SSLB_PX Y=SFJP_SSLB_PX Z=-SFJP_S  365
C    FLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ/2. ALP  365
C    HAZ=-SFJP_ALPHAZH <===                                               365
         CALL AGSRESET                                                    368
         AG_X = -SFJP_SSLB_PX                                             368
         AG_Y = SFJP_SSLB_PX                                              368
         AG_Z =                                                           368
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          368
     *   SFJP_SSLB_DZ/2.                                                  368
         AG_ALPHAN=AG_ALPHAN+1                                            368
         AG_ALPHAK(AG_ALPHAN)=3                                           368
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               368
         AG_TITLE = 'SSLB'                                                368
         AG_EXNAME = 'POSITION'                                           368
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      368
         CALL AXPOSITION                                                  368
C    *                                                                    368
C    Geant POSITION : SSLB ===> X=SFJP_SSLB_PX Y=SFJP_SSLB_PX Z=+SFJP_SF  368
C    LU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ/2. ALPH  368
C    AZ=SFJP_ALPHAZH <===                                                 368
         CALL AGSRESET                                                    371
         AG_X = SFJP_SSLB_PX                                              371
         AG_Y = SFJP_SSLB_PX                                              371
         AG_Z =                                                           371
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          371
     *   SFJP_SSLB_DZ/2.                                                  371
         AG_ALPHAN=AG_ALPHAN+1                                            371
         AG_ALPHAK(AG_ALPHAN)=3                                           371
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                371
         AG_TITLE = 'SSLB'                                                371
         AG_EXNAME = 'POSITION'                                           371
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      371
         CALL AXPOSITION                                                  371
C    *                                                                    371
C    Geant POSITION : SSLB ===> X=-SFJP_SSLB_PX Y=-SFJP_SSLB_PX Z=+SFJP_  371
C    SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ/2. AL  371
C    PHAZ=SFJP_ALPHAZH <===                                               371
         CALL AGSRESET                                                    374
         AG_X = -SFJP_SSLB_PX                                             374
         AG_Y = -SFJP_SSLB_PX                                             374
         AG_Z =                                                           374
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          374
     *   SFJP_SSLB_DZ/2.                                                  374
         AG_ALPHAN=AG_ALPHAN+1                                            374
         AG_ALPHAK(AG_ALPHAN)=3                                           374
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                374
         AG_TITLE = 'SSLB'                                                374
         AG_EXNAME = 'POSITION'                                           374
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      374
         CALL AXPOSITION                                                  374
C    *                                                                    374
C    Geant POSITION : SSLB ===> X=SFJP_SSLB_PX Y=-SFJP_SSLB_PX Z=+SFJP_S  374
C    FLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ/2. ALP  374
C    HAZ=-SFJP_ALPHAZH <===                                               374
         CALL AGSRESET                                                    377
         AG_X = SFJP_SSLB_PX                                              377
         AG_Y = -SFJP_SSLB_PX                                             377
         AG_Z =                                                           377
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          377
     *   SFJP_SSLB_DZ/2.                                                  377
         AG_ALPHAN=AG_ALPHAN+1                                            377
         AG_ALPHAK(AG_ALPHAN)=3                                           377
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               377
         AG_TITLE = 'SSLB'                                                377
         AG_EXNAME = 'POSITION'                                           377
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      377
         CALL AXPOSITION                                                  377
C    *                                                                    377
C    Geant POSITION : SSLB ===> X=-SFJP_SSLB_PX Y=SFJP_SSLB_PX Z=+SFJP_S  377
C    FLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ/2. ALP  377
C    HAZ=-SFJP_ALPHAZH <===                                               377
         CALL AGSRESET                                                    380
         AG_X = -SFJP_SSLB_PX                                             380
         AG_Y = SFJP_SSLB_PX                                              380
         AG_Z =                                                           380
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          380
     *   SFJP_SSLB_DZ/2.                                                  380
         AG_ALPHAN=AG_ALPHAN+1                                            380
         AG_ALPHAK(AG_ALPHAN)=3                                           380
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               380
         AG_TITLE = 'SSLB'                                                380
         AG_EXNAME = 'POSITION'                                           380
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      380
         CALL AXPOSITION                                                  380
C    *                                                                    380
C    Create SSLT :                                                        381
C    Call To Local Procedure SSLT                                         381
         ASSIGN 5180 TO BLOCK_SSLT                                        381
         GO TO  5170                                                      381
5180     CONTINUE                                                         381
C    *                                                                    381
C    Geant POSITION : SSLT ===> X=SFJP_SSLT_PX Y=SFJP_SSLT_PX Z=-SFJP_SF  383
C    LU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ-SFJP_SS  383
C    LT_DZ/2. <===                                                        383
         CALL AGSRESET                                                    383
         AG_X = SFJP_SSLT_PX                                              383
         AG_Y = SFJP_SSLT_PX                                              383
         AG_Z =                                                           383
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          383
     *   SFJP_SSLB_DZ-SFJP_SSLT_DZ/2.                                     383
         AG_TITLE = 'SSLT'                                                383
         AG_EXNAME = 'POSITION'                                           383
         AG_PARLIST = 'X_Y_Z'                                             383
         CALL AXPOSITION                                                  383
C    *                                                                    383
C    Geant POSITION : SSLT ===> X=-SFJP_SSLT_PX Y=-SFJP_SSLT_PX Z=-SFJP_  383
C    SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ-SFJP_  383
C    SSLT_DZ/2. <===                                                      383
         CALL AGSRESET                                                    385
         AG_X = -SFJP_SSLT_PX                                             385
         AG_Y = -SFJP_SSLT_PX                                             385
         AG_Z =                                                           385
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          385
     *   SFJP_SSLB_DZ-SFJP_SSLT_DZ/2.                                     385
         AG_TITLE = 'SSLT'                                                385
         AG_EXNAME = 'POSITION'                                           385
         AG_PARLIST = 'X_Y_Z'                                             385
         CALL AXPOSITION                                                  385
C    *                                                                    385
C    Geant POSITION : SSLT ===> X=SFJP_SSLT_PX Y=-SFJP_SSLT_PX Z=-SFJP_S  385
C    FLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ-SFJP_S  385
C    SLT_DZ/2. <===                                                       385
         CALL AGSRESET                                                    387
         AG_X = SFJP_SSLT_PX                                              387
         AG_Y = -SFJP_SSLT_PX                                             387
         AG_Z =                                                           387
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          387
     *   SFJP_SSLB_DZ-SFJP_SSLT_DZ/2.                                     387
         AG_TITLE = 'SSLT'                                                387
         AG_EXNAME = 'POSITION'                                           387
         AG_PARLIST = 'X_Y_Z'                                             387
         CALL AXPOSITION                                                  387
C    *                                                                    387
C    Geant POSITION : SSLT ===> X=-SFJP_SSLT_PX Y=SFJP_SSLT_PX Z=-SFJP_S  387
C    FLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-SFJP_SSLB_DZ-SFJP_S  387
C    SLT_DZ/2. <===                                                       387
         CALL AGSRESET                                                    389
         AG_X = -SFJP_SSLT_PX                                             389
         AG_Y = SFJP_SSLT_PX                                              389
         AG_Z =                                                           389
     *   -SFJP_SFLU_DZ-SFJP_SFPBDZ-SFJP_SSSS_WIDTH-SFJP_SSST_PZ-          389
     *   SFJP_SSLB_DZ-SFJP_SSLT_DZ/2.                                     389
         AG_TITLE = 'SSLT'                                                389
         AG_EXNAME = 'POSITION'                                           389
         AG_PARLIST = 'X_Y_Z'                                             389
         CALL AXPOSITION                                                  389
C    *                                                                    389
C    Geant POSITION : SSLT ===> X=SFJP_SSLT_PX Y=SFJP_SSLT_PX Z=+SFJP_SF  389
C    LU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ+SFJP_SS  389
C    LT_DZ/2. <===                                                        389
         CALL AGSRESET                                                    391
         AG_X = SFJP_SSLT_PX                                              391
         AG_Y = SFJP_SSLT_PX                                              391
         AG_Z =                                                           391
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          391
     *   SFJP_SSLB_DZ+SFJP_SSLT_DZ/2.                                     391
         AG_TITLE = 'SSLT'                                                391
         AG_EXNAME = 'POSITION'                                           391
         AG_PARLIST = 'X_Y_Z'                                             391
         CALL AXPOSITION                                                  391
C    *                                                                    391
C    Geant POSITION : SSLT ===> X=-SFJP_SSLT_PX Y=-SFJP_SSLT_PX Z=+SFJP_  391
C    SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ+SFJP_  391
C    SSLT_DZ/2. <===                                                      391
         CALL AGSRESET                                                    393
         AG_X = -SFJP_SSLT_PX                                             393
         AG_Y = -SFJP_SSLT_PX                                             393
         AG_Z =                                                           393
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          393
     *   SFJP_SSLB_DZ+SFJP_SSLT_DZ/2.                                     393
         AG_TITLE = 'SSLT'                                                393
         AG_EXNAME = 'POSITION'                                           393
         AG_PARLIST = 'X_Y_Z'                                             393
         CALL AXPOSITION                                                  393
C    *                                                                    393
C    Geant POSITION : SSLT ===> X=SFJP_SSLT_PX Y=-SFJP_SSLT_PX Z=+SFJP_S  393
C    FLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ+SFJP_S  393
C    SLT_DZ/2. <===                                                       393
         CALL AGSRESET                                                    395
         AG_X = SFJP_SSLT_PX                                              395
         AG_Y = -SFJP_SSLT_PX                                             395
         AG_Z =                                                           395
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          395
     *   SFJP_SSLB_DZ+SFJP_SSLT_DZ/2.                                     395
         AG_TITLE = 'SSLT'                                                395
         AG_EXNAME = 'POSITION'                                           395
         AG_PARLIST = 'X_Y_Z'                                             395
         CALL AXPOSITION                                                  395
C    *                                                                    395
C    Geant POSITION : SSLT ===> X=-SFJP_SSLT_PX Y=SFJP_SSLT_PX Z=+SFJP_S  395
C    FLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+SFJP_SSLB_DZ+SFJP_S  395
C    SLT_DZ/2. <===                                                       395
         CALL AGSRESET                                                    397
         AG_X = -SFJP_SSLT_PX                                             397
         AG_Y = SFJP_SSLT_PX                                              397
         AG_Z =                                                           397
     *   +SFJP_SFLU_DZ+SFJP_SFPBDZ+SFJP_SSSS_WIDTH+SFJP_SSST_PZ+          397
     *   SFJP_SSLB_DZ+SFJP_SSLT_DZ/2.                                     397
         AG_TITLE = 'SSLT'                                                397
         AG_EXNAME = 'POSITION'                                           397
         AG_PARLIST = 'X_Y_Z'                                             397
         CALL AXPOSITION                                                  397
C    *                                                                    397
C    Create SCMP :                                                        398
C    Call To Local Procedure SCMP                                         398
         ASSIGN 5200 TO BLOCK_SCMP                                        398
         GO TO  5190                                                      398
5200     CONTINUE                                                         398
C    *                                                                    398
C    Geant POSITION : SCMP ===> X=SFJP_SCMP_PX Y=SFJP_SCMP_PX Z=SFJP_SCV  399
C    M_PZ ALPHAZ=-SFJP_ALPHAZH <===                                       399
         CALL AGSRESET                                                    399
         AG_X = SFJP_SCMP_PX                                              399
         AG_Y = SFJP_SCMP_PX                                              399
         AG_Z = SFJP_SCVM_PZ                                              399
         AG_ALPHAN=AG_ALPHAN+1                                            399
         AG_ALPHAK(AG_ALPHAN)=3                                           399
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               399
         AG_TITLE = 'SCMP'                                                399
         AG_EXNAME = 'POSITION'                                           399
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      399
         CALL AXPOSITION                                                  399
C    *                                                                    399
C    Geant POSITION : SCMP ===> X=-SFJP_SCMP_PX Y=-SFJP_SCMP_PX Z=SFJP_S  399
C    CVM_PZ ALPHAZ=-SFJP_ALPHAZH <===                                     399
         CALL AGSRESET                                                    401
         AG_X = -SFJP_SCMP_PX                                             401
         AG_Y = -SFJP_SCMP_PX                                             401
         AG_Z = SFJP_SCVM_PZ                                              401
         AG_ALPHAN=AG_ALPHAN+1                                            401
         AG_ALPHAK(AG_ALPHAN)=3                                           401
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               401
         AG_TITLE = 'SCMP'                                                401
         AG_EXNAME = 'POSITION'                                           401
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      401
         CALL AXPOSITION                                                  401
C    *                                                                    401
C    Geant POSITION : SCMP ===> X=SFJP_SCMP_PX Y=-SFJP_SCMP_PX Z=SFJP_SC  401
C    VM_PZ ALPHAZ=SFJP_ALPHAZH <===                                       401
         CALL AGSRESET                                                    402
         AG_X = SFJP_SCMP_PX                                              402
         AG_Y = -SFJP_SCMP_PX                                             402
         AG_Z = SFJP_SCVM_PZ                                              402
         AG_ALPHAN=AG_ALPHAN+1                                            402
         AG_ALPHAK(AG_ALPHAN)=3                                           402
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                402
         AG_TITLE = 'SCMP'                                                402
         AG_EXNAME = 'POSITION'                                           402
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      402
         CALL AXPOSITION                                                  402
C    *                                                                    402
C    Geant POSITION : SCMP ===> X=-SFJP_SCMP_PX Y=SFJP_SCMP_PX Z=SFJP_SC  402
C    VM_PZ ALPHAZ=SFJP_ALPHAZH <===                                       402
         CALL AGSRESET                                                    403
         AG_X = -SFJP_SCMP_PX                                             403
         AG_Y = SFJP_SCMP_PX                                              403
         AG_Z = SFJP_SCVM_PZ                                              403
         AG_ALPHAN=AG_ALPHAN+1                                            403
         AG_ALPHAK(AG_ALPHAN)=3                                           403
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                403
         AG_TITLE = 'SCMP'                                                403
         AG_EXNAME = 'POSITION'                                           403
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      403
         CALL AXPOSITION                                                  403
C    *                                                                    403
C    Geant POSITION : SCMP ===> X=SFJP_SCMP_PX Y=SFJP_SCMP_PX Z=-SFJP_SC  403
C    VM_PZ ALPHAZ=-SFJP_ALPHAZH <===                                      403
         CALL AGSRESET                                                    405
         AG_X = SFJP_SCMP_PX                                              405
         AG_Y = SFJP_SCMP_PX                                              405
         AG_Z = -SFJP_SCVM_PZ                                             405
         AG_ALPHAN=AG_ALPHAN+1                                            405
         AG_ALPHAK(AG_ALPHAN)=3                                           405
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               405
         AG_TITLE = 'SCMP'                                                405
         AG_EXNAME = 'POSITION'                                           405
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      405
         CALL AXPOSITION                                                  405
C    *                                                                    405
C    Geant POSITION : SCMP ===> X=-SFJP_SCMP_PX Y=-SFJP_SCMP_PX Z=-SFJP_  405
C    SCVM_PZ ALPHAZ=-SFJP_ALPHAZH <===                                    405
         CALL AGSRESET                                                    407
         AG_X = -SFJP_SCMP_PX                                             407
         AG_Y = -SFJP_SCMP_PX                                             407
         AG_Z = -SFJP_SCVM_PZ                                             407
         AG_ALPHAN=AG_ALPHAN+1                                            407
         AG_ALPHAK(AG_ALPHAN)=3                                           407
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               407
         AG_TITLE = 'SCMP'                                                407
         AG_EXNAME = 'POSITION'                                           407
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      407
         CALL AXPOSITION                                                  407
C    *                                                                    407
C    Geant POSITION : SCMP ===> X=SFJP_SCMP_PX Y=-SFJP_SCMP_PX Z=-SFJP_S  407
C    CVM_PZ ALPHAZ=SFJP_ALPHAZH <===                                      407
         CALL AGSRESET                                                    409
         AG_X = SFJP_SCMP_PX                                              409
         AG_Y = -SFJP_SCMP_PX                                             409
         AG_Z = -SFJP_SCVM_PZ                                             409
         AG_ALPHAN=AG_ALPHAN+1                                            409
         AG_ALPHAK(AG_ALPHAN)=3                                           409
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                409
         AG_TITLE = 'SCMP'                                                409
         AG_EXNAME = 'POSITION'                                           409
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      409
         CALL AXPOSITION                                                  409
C    *                                                                    409
C    Geant POSITION : SCMP ===> X=-SFJP_SCMP_PX Y=SFJP_SCMP_PX Z=-SFJP_S  409
C    CVM_PZ ALPHAZ=SFJP_ALPHAZH <===                                      409
         CALL AGSRESET                                                    411
         AG_X = -SFJP_SCMP_PX                                             411
         AG_Y = SFJP_SCMP_PX                                              411
         AG_Z = -SFJP_SCVM_PZ                                             411
         AG_ALPHAN=AG_ALPHAN+1                                            411
         AG_ALPHAK(AG_ALPHAN)=3                                           411
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                411
         AG_TITLE = 'SCMP'                                                411
         AG_EXNAME = 'POSITION'                                           411
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      411
         CALL AXPOSITION                                                  411
C    *                                                                    411
C    Create SCVM :                                                        412
C    Call To Local Procedure SCVM                                         412
         ASSIGN 5220 TO BLOCK_SCVM                                        412
         GO TO  5210                                                      412
5220     CONTINUE                                                         412
C    *                                                                    412
C    Geant POSITION : SCVM ===> X=SFJP_SCVM_PX Y=SFJP_SCVM_PX Z=SFJP_SCV  413
C    M_PZ ALPHAZ=-SFJP_ALPHAZH <===                                       413
         CALL AGSRESET                                                    413
         AG_X = SFJP_SCVM_PX                                              413
         AG_Y = SFJP_SCVM_PX                                              413
         AG_Z = SFJP_SCVM_PZ                                              413
         AG_ALPHAN=AG_ALPHAN+1                                            413
         AG_ALPHAK(AG_ALPHAN)=3                                           413
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               413
         AG_TITLE = 'SCVM'                                                413
         AG_EXNAME = 'POSITION'                                           413
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      413
         CALL AXPOSITION                                                  413
C    *                                                                    413
C    Geant POSITION : SCVM ===> X=-SFJP_SCVM_PX Y=-SFJP_SCVM_PX Z=SFJP_S  413
C    CVM_PZ ALPHAZ=(180.-SFJP_ALPHAZH) <===                               413
         CALL AGSRESET                                                    415
         AG_X = -SFJP_SCVM_PX                                             415
         AG_Y = -SFJP_SCVM_PX                                             415
         AG_Z = SFJP_SCVM_PZ                                              415
         AG_ALPHAN=AG_ALPHAN+1                                            415
         AG_ALPHAK(AG_ALPHAN)=3                                           415
         AG_ALPHAL(AG_ALPHAN)=(180.-SFJP_ALPHAZH)                         415
         AG_TITLE = 'SCVM'                                                415
         AG_EXNAME = 'POSITION'                                           415
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      415
         CALL AXPOSITION                                                  415
C    *                                                                    415
C    Geant POSITION : SCVM ===> X=SFJP_SCVM_PX Y=-SFJP_SCVM_PX Z=SFJP_SC  415
C    VM_PZ ALPHAZ=-(180.-SFJP_ALPHAZH) <===                               415
         CALL AGSRESET                                                    417
         AG_X = SFJP_SCVM_PX                                              417
         AG_Y = -SFJP_SCVM_PX                                             417
         AG_Z = SFJP_SCVM_PZ                                              417
         AG_ALPHAN=AG_ALPHAN+1                                            417
         AG_ALPHAK(AG_ALPHAN)=3                                           417
         AG_ALPHAL(AG_ALPHAN)=-(180.-SFJP_ALPHAZH)                        417
         AG_TITLE = 'SCVM'                                                417
         AG_EXNAME = 'POSITION'                                           417
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      417
         CALL AXPOSITION                                                  417
C    *                                                                    417
C    Geant POSITION : SCVM ===> X=-SFJP_SCVM_PX Y=SFJP_SCVM_PX Z=SFJP_SC  417
C    VM_PZ ALPHAZ=SFJP_ALPHAZH <===                                       417
         CALL AGSRESET                                                    418
         AG_X = -SFJP_SCVM_PX                                             418
         AG_Y = SFJP_SCVM_PX                                              418
         AG_Z = SFJP_SCVM_PZ                                              418
         AG_ALPHAN=AG_ALPHAN+1                                            418
         AG_ALPHAK(AG_ALPHAN)=3                                           418
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                418
         AG_TITLE = 'SCVM'                                                418
         AG_EXNAME = 'POSITION'                                           418
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      418
         CALL AXPOSITION                                                  418
C    *                                                                    418
C    Geant POSITION : SCVM ===> X=SFJP_SCVM_PX Y=SFJP_SCVM_PX Z=-SFJP_SC  418
C    VM_PZ ALPHAZ=-SFJP_ALPHAZH <===                                      418
         CALL AGSRESET                                                    420
         AG_X = SFJP_SCVM_PX                                              420
         AG_Y = SFJP_SCVM_PX                                              420
         AG_Z = -SFJP_SCVM_PZ                                             420
         AG_ALPHAN=AG_ALPHAN+1                                            420
         AG_ALPHAK(AG_ALPHAN)=3                                           420
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               420
         AG_TITLE = 'SCVM'                                                420
         AG_EXNAME = 'POSITION'                                           420
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      420
         CALL AXPOSITION                                                  420
C    *                                                                    420
C    Geant POSITION : SCVM ===> X=-SFJP_SCVM_PX Y=-SFJP_SCVM_PX Z=-SFJP_  420
C    SCVM_PZ ALPHAZ=(180.-SFJP_ALPHAZH) <===                              420
         CALL AGSRESET                                                    422
         AG_X = -SFJP_SCVM_PX                                             422
         AG_Y = -SFJP_SCVM_PX                                             422
         AG_Z = -SFJP_SCVM_PZ                                             422
         AG_ALPHAN=AG_ALPHAN+1                                            422
         AG_ALPHAK(AG_ALPHAN)=3                                           422
         AG_ALPHAL(AG_ALPHAN)=(180.-SFJP_ALPHAZH)                         422
         AG_TITLE = 'SCVM'                                                422
         AG_EXNAME = 'POSITION'                                           422
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      422
         CALL AXPOSITION                                                  422
C    *                                                                    422
C    Geant POSITION : SCVM ===> X=SFJP_SCVM_PX Y=-SFJP_SCVM_PX Z=-SFJP_S  422
C    CVM_PZ ALPHAZ=-(180.-SFJP_ALPHAZH) <===                              422
         CALL AGSRESET                                                    424
         AG_X = SFJP_SCVM_PX                                              424
         AG_Y = -SFJP_SCVM_PX                                             424
         AG_Z = -SFJP_SCVM_PZ                                             424
         AG_ALPHAN=AG_ALPHAN+1                                            424
         AG_ALPHAK(AG_ALPHAN)=3                                           424
         AG_ALPHAL(AG_ALPHAN)=-(180.-SFJP_ALPHAZH)                        424
         AG_TITLE = 'SCVM'                                                424
         AG_EXNAME = 'POSITION'                                           424
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      424
         CALL AXPOSITION                                                  424
C    *                                                                    424
C    Geant POSITION : SCVM ===> X=-SFJP_SCVM_PX Y=SFJP_SCVM_PX Z=-SFJP_S  424
C    CVM_PZ ALPHAZ=SFJP_ALPHAZH <===                                      424
         CALL AGSRESET                                                    426
         AG_X = -SFJP_SCVM_PX                                             426
         AG_Y = SFJP_SCVM_PX                                              426
         AG_Z = -SFJP_SCVM_PZ                                             426
         AG_ALPHAN=AG_ALPHAN+1                                            426
         AG_ALPHAK(AG_ALPHAN)=3                                           426
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                426
         AG_TITLE = 'SCVM'                                                426
         AG_EXNAME = 'POSITION'                                           426
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      426
         CALL AXPOSITION                                                  426
C    *                                                                    426
5011  CALL AGSPOPB                                                        427
      GO TO BLOCK_SFMO, (5010 ,5020,5030)                                 427
C End of Local Procedure                                                  427
C ----------------------------------------------------------------------  428
C ------------ SFLM                                                       428
C Begin Local Procedure SFLM                                              429
C SFLM IS THE MOTHER OF THE LADDER                                        429
5040  CONTINUE                                                            429
         AG_TITLE='SFLM'                                                  429
         AG_MODULE=MODULE                                                 429
         AG_MARK='V00'                                                    429
         CALL AGSPUSH                                                     429
         AG_MODULE=MODULE                                                 429
C    *                                                                    429
C    Geant MATERIAL : AIR ===> inherited arguments used <===              430
         CALL AGSRESET                                                    430
         AG_TITLE = 'AIR'                                                 430
         AG_EXNAME = 'MATERIAL'                                           430
         AG_PARLIST = 'NONE'                                              430
         CALL AXMATERIAL                                                  430
C    *                                                                    430
C    Geant ATTRIBUTE : SFLM ===> SEEN=0 COLO=6 <===                       430
         CALL AGSRESET                                                    431
         AG_SEEN = 0                                                      431
         AG_COLO = 6                                                      431
         AG_TITLE = 'SFLM'                                                431
         AG_EXNAME = 'ATTRIBUTE'                                          431
         AG_PARLIST = 'SEEN_COLO'                                         431
         CALL AXATTRIBUTE                                                 431
C    *                                                                    431
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=(SFPA_DMTHK+SFPA_GPTHK+SF  431
C    PA_SMTHK)/2, DZ=SFPA_SMLEN/2 <===                                    431
         CALL AGSRESET                                                    433
         AG_DX = SFPA_DMWID/2                                             433
         AG_DY = (SFPA_DMTHK+SFPA_GPTHK+SFPA_SMTHK)/2                     433
         AG_DZ = SFPA_SMLEN/2                                             433
         AG_TITLE = 'BOX'                                                 433
         AG_EXNAME = 'SHAPE'                                              433
         AG_PARLIST = 'DX_DY_DZ'                                          433
         CALL AXSHAPE                                                     433
C    *                                                                    433
C    Create SFDM :                                                        434
C    Call To Local Procedure SFDM                                         434
         ASSIGN 5240 TO BLOCK_SFDM                                        434
         GO TO  5230                                                      434
5240     CONTINUE                                                         434
C    *                                                                    434
C    Geant POSITION : SFDM ===> Y=-(SFPA_SMTHK+SFPA_GPTHK)/2 <===         435
         CALL AGSRESET                                                    435
         AG_Y = -(SFPA_SMTHK+SFPA_GPTHK)/2                                435
         AG_TITLE = 'SFDM'                                                435
         AG_EXNAME = 'POSITION'                                           435
         AG_PARLIST = 'Y'                                                 435
         CALL AXPOSITION                                                  435
C    *                                                                    435
C    Create SFSM :                                                        436
C    Call To Local Procedure SFSM                                         436
         ASSIGN 5260 TO BLOCK_SFSM                                        436
         GO TO  5250                                                      436
5260     CONTINUE                                                         436
C    *                                                                    436
C    Geant POSITION : SFSM ===> Y=(SFPA_DMTHK+SFPA_GPTHK)/2 <===          437
         CALL AGSRESET                                                    437
         AG_Y = (SFPA_DMTHK+SFPA_GPTHK)/2                                 437
         AG_TITLE = 'SFSM'                                                437
         AG_EXNAME = 'POSITION'                                           437
         AG_PARLIST = 'Y'                                                 437
         CALL AXPOSITION                                                  437
C    *                                                                    437
5041  CALL AGSPOPB                                                        438
      GO TO BLOCK_SFLM, (5040 ,5050)                                      438
C End of Local Procedure                                                  438
C ----------------------------------------------------------------------  439
C ------------ SFDM                                                       439
C Begin Local Procedure SFDM                                              440
C SFDM IS THE MOTHER OF THE DETECTORS                                     440
5230  CONTINUE                                                            440
         AG_TITLE='SFDM'                                                  440
         AG_MODULE=MODULE                                                 440
         AG_MARK='V00'                                                    440
         CALL AGSPUSH                                                     440
         AG_MODULE=MODULE                                                 440
C    *                                                                    440
C    Geant MATERIAL : AIR ===> inherited arguments used <===              441
         CALL AGSRESET                                                    441
         AG_TITLE = 'AIR'                                                 441
         AG_EXNAME = 'MATERIAL'                                           441
         AG_PARLIST = 'NONE'                                              441
         CALL AXMATERIAL                                                  441
C    *                                                                    441
C    Geant ATTRIBUTE : SFDM ===> SEEN=0 COLO=6 <===                       441
         CALL AGSRESET                                                    442
         AG_SEEN = 0                                                      442
         AG_COLO = 6                                                      442
         AG_TITLE = 'SFDM'                                                442
         AG_EXNAME = 'ATTRIBUTE'                                          442
         AG_PARLIST = 'SEEN_COLO'                                         442
         CALL AXATTRIBUTE                                                 442
C    *                                                                    442
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=SFPA_DMTHK/2, DZ=SFPA_WPL  442
C    EN/2 <===                                                            442
         CALL AGSRESET                                                    444
         AG_DX = SFPA_DMWID/2                                             444
         AG_DY = SFPA_DMTHK/2                                             444
         AG_DZ = SFPA_WPLEN/2                                             444
         AG_TITLE = 'BOX'                                                 444
         AG_EXNAME = 'SHAPE'                                              444
         AG_PARLIST = 'DX_DY_DZ'                                          444
         CALL AXSHAPE                                                     444
C    *                                                                    444
         WAFPCKLEN=SFPA_WPLEN/(SFPA_NSSD*1.)                              444
C    *                                                                    446
         IWAF=1                                                           446
            GO TO 5273                                                    446
5271        IWAF=IWAF+1                                                   446
5273        IF(IWAF-(SFPA_NSSD).GT.0) GO TO 5272                          446
C       Create SFSW :                                                     447
C       Call To Local Procedure SFSW                                      447
            ASSIGN 5290 TO BLOCK_SFSW                                     447
            GO TO  5280                                                   447
5290        CONTINUE                                                      447
C       *                                                                 447
C       Geant POSITION : SFSW ===> Z=-(SFPA_WPLEN+WAFPCKLEN)/2+IWAF*WAFP  448
C       CKLEN <===                                                        448
            CALL AGSRESET                                                 448
            AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+IWAF*WAFPCKLEN               448
            AG_TITLE = 'SFSW'                                             448
            AG_EXNAME = 'POSITION'                                        448
            AG_PARLIST = 'Z'                                              448
            CALL AXPOSITION                                               448
C       *                                                                 448
         GO TO 5271                                                       449
5272     CONTINUE                                                         449
5231  CALL AGSPOPB                                                        450
      GO TO BLOCK_SFDM, (5230 ,5240)                                      450
C End of Local Procedure                                                  450
C ----------------------------------------------------------------------  451
C ------------ SFSW                                                       451
C Begin Local Procedure SFSW                                              452
C SFSW IS A SINGLE WAFER CONTAINER                                        452
5280  CONTINUE                                                            452
         AG_TITLE='SFSW'                                                  452
         AG_MODULE=MODULE                                                 452
         AG_MARK='V00'                                                    452
         CALL AGSPUSH                                                     452
         AG_MODULE=MODULE                                                 452
C    *                                                                    452
C    Geant MATERIAL : AIR ===> inherited arguments used <===              453
         CALL AGSRESET                                                    453
         AG_TITLE = 'AIR'                                                 453
         AG_EXNAME = 'MATERIAL'                                           453
         AG_PARLIST = 'NONE'                                              453
         CALL AXMATERIAL                                                  453
C    *                                                                    453
C    Geant ATTRIBUTE : SFSW ===> SEEN=0 COLO=6 <===                       453
         CALL AGSRESET                                                    454
         AG_SEEN = 0                                                      454
         AG_COLO = 6                                                      454
         AG_TITLE = 'SFSW'                                                454
         AG_EXNAME = 'ATTRIBUTE'                                          454
         AG_PARLIST = 'SEEN_COLO'                                         454
         CALL AXATTRIBUTE                                                 454
C    *                                                                    454
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=SFJP_SFSW_DY, DZ=SFJP_SFS  454
C    W_DZ <===                                                            454
         CALL AGSRESET                                                    456
         AG_DX = SFPA_DMWID/2                                             456
         AG_DY = SFJP_SFSW_DY                                             456
         AG_DZ = SFJP_SFSW_DZ                                             456
         AG_TITLE = 'BOX'                                                 456
         AG_EXNAME = 'SHAPE'                                              456
         AG_PARLIST = 'DX_DY_DZ'                                          456
         CALL AXSHAPE                                                     456
C    *                                                                    456
C    Create SFSD :                                                        457
C    Call To Local Procedure SFSD                                         457
         ASSIGN 5310 TO BLOCK_SFSD                                        457
         GO TO  5300                                                      457
5310     CONTINUE                                                         457
C    *                                                                    457
C    Geant POSITION : SFSD ===> inherited arguments used <===             458
         CALL AGSRESET                                                    458
         AG_TITLE = 'SFSD'                                                458
         AG_EXNAME = 'POSITION'                                           458
         AG_PARLIST = 'NONE'                                              458
         CALL AXPOSITION                                                  458
C    *                                                                    458
C    Create SFRA :                                                        459
C    Call To Local Procedure SFRA                                         459
         ASSIGN 5330 TO BLOCK_SFRA                                        459
         GO TO  5320                                                      459
5330     CONTINUE                                                         459
C    *                                                                    459
C    Geant POSITION : SFRA ===> X=0.0 Y=SFJP_SFRA_PY Z=SFJP_SFRA_PZ <===  460
         CALL AGSRESET                                                    460
         AG_X = 0.0                                                       460
         AG_Y = SFJP_SFRA_PY                                              460
         AG_Z = SFJP_SFRA_PZ                                              460
         AG_TITLE = 'SFRA'                                                460
         AG_EXNAME = 'POSITION'                                           460
         AG_PARLIST = 'X_Y_Z'                                             460
         CALL AXPOSITION                                                  460
C    *                                                                    460
C    Geant POSITION : SFRA ===> X=0.0 Y=SFJP_SFRA_PY Z=-SFJP_SFRA_PZ <==  460
C    =                                                                    460
         CALL AGSRESET                                                    461
         AG_X = 0.0                                                       461
         AG_Y = SFJP_SFRA_PY                                              461
         AG_Z = -SFJP_SFRA_PZ                                             461
         AG_TITLE = 'SFRA'                                                461
         AG_EXNAME = 'POSITION'                                           461
         AG_PARLIST = 'X_Y_Z'                                             461
         CALL AXPOSITION                                                  461
C    *                                                                    461
C    Create SFRS :                                                        462
C    Call To Local Procedure SFRS                                         462
         ASSIGN 5350 TO BLOCK_SFRS                                        462
         GO TO  5340                                                      462
5350     CONTINUE                                                         462
C    *                                                                    462
C    Geant POSITION : SFRS ===> X=SFJP_SFRS_PX Y=SFJP_SFRS_PY Z=SFJP_SFR  463
C    A_PZ <===                                                            463
         CALL AGSRESET                                                    463
         AG_X = SFJP_SFRS_PX                                              463
         AG_Y = SFJP_SFRS_PY                                              463
         AG_Z = SFJP_SFRA_PZ                                              463
         AG_TITLE = 'SFRS'                                                463
         AG_EXNAME = 'POSITION'                                           463
         AG_PARLIST = 'X_Y_Z'                                             463
         CALL AXPOSITION                                                  463
C    *                                                                    463
C    Geant POSITION : SFRS ===> X=SFJP_SFRS_PX Y=SFJP_SFRS_PY Z=-SFJP_SF  463
C    RA_PZ <===                                                           463
         CALL AGSRESET                                                    464
         AG_X = SFJP_SFRS_PX                                              464
         AG_Y = SFJP_SFRS_PY                                              464
         AG_Z = -SFJP_SFRA_PZ                                             464
         AG_TITLE = 'SFRS'                                                464
         AG_EXNAME = 'POSITION'                                           464
         AG_PARLIST = 'X_Y_Z'                                             464
         CALL AXPOSITION                                                  464
C    *                                                                    464
C    Geant POSITION : SFRS ===> X=-SFJP_SFRS_PX Y=SFJP_SFRS_PY Z=SFJP_SF  464
C    RA_PZ <===                                                           464
         CALL AGSRESET                                                    465
         AG_X = -SFJP_SFRS_PX                                             465
         AG_Y = SFJP_SFRS_PY                                              465
         AG_Z = SFJP_SFRA_PZ                                              465
         AG_TITLE = 'SFRS'                                                465
         AG_EXNAME = 'POSITION'                                           465
         AG_PARLIST = 'X_Y_Z'                                             465
         CALL AXPOSITION                                                  465
C    *                                                                    465
C    Geant POSITION : SFRS ===> X=-SFJP_SFRS_PX Y=SFJP_SFRS_PY Z=-SFJP_S  465
C    FRA_PZ <===                                                          465
         CALL AGSRESET                                                    466
         AG_X = -SFJP_SFRS_PX                                             466
         AG_Y = SFJP_SFRS_PY                                              466
         AG_Z = -SFJP_SFRA_PZ                                             466
         AG_TITLE = 'SFRS'                                                466
         AG_EXNAME = 'POSITION'                                           466
         AG_PARLIST = 'X_Y_Z'                                             466
         CALL AXPOSITION                                                  466
C    *                                                                    466
C    Create SFFX :                                                        467
C    Call To Local Procedure SFFX                                         467
         ASSIGN 5370 TO BLOCK_SFFX                                        467
         GO TO  5360                                                      467
5370     CONTINUE                                                         467
C    *                                                                    467
C    Geant POSITION : SFFX ===> X=0.0 Y=SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_S  468
C    FFX_DYZ Z=SFJP_SFRA_PZ <===                                          468
         CALL AGSRESET                                                    468
         AG_X = 0.0                                                       468
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ                   468
         AG_Z = SFJP_SFRA_PZ                                              468
         AG_TITLE = 'SFFX'                                                468
         AG_EXNAME = 'POSITION'                                           468
         AG_PARLIST = 'X_Y_Z'                                             468
         CALL AXPOSITION                                                  468
C    *                                                                    468
C    Geant POSITION : SFFX ===> X=0.0 Y=SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_S  468
C    FFX_DYZ Z=-SFJP_SFRA_PZ <===                                         468
         CALL AGSRESET                                                    469
         AG_X = 0.0                                                       469
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ                   469
         AG_Z = -SFJP_SFRA_PZ                                             469
         AG_TITLE = 'SFFX'                                                469
         AG_EXNAME = 'POSITION'                                           469
         AG_PARLIST = 'X_Y_Z'                                             469
         CALL AXPOSITION                                                  469
C    *                                                                    469
C    Create SFPI :                                                        470
C    Call To Local Procedure SFPI                                         470
         ASSIGN 5390 TO BLOCK_SFPI                                        470
         GO TO  5380                                                      470
5390     CONTINUE                                                         470
C    *                                                                    470
C    Geant POSITION : SFPI ===> X=SFJP_SFPI_PX Y=SFJP_SFRA_PY+SFJP_SFRA_  473
C    DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2 Z=SFJP_SFPI_PZ ALPHAX=90 <===         473
         CALL AGSRESET                                                    473
         AG_X = SFJP_SFPI_PX                                              473
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2     473
         AG_Z = SFJP_SFPI_PZ                                              473
         AG_ALPHAN=AG_ALPHAN+1                                            473
         AG_ALPHAK(AG_ALPHAN)=1                                           473
         AG_ALPHAL(AG_ALPHAN)=90                                          473
         AG_TITLE = 'SFPI'                                                473
         AG_EXNAME = 'POSITION'                                           473
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      473
         CALL AXPOSITION                                                  473
C    *                                                                    473
C    Geant POSITION : SFPI ===> X=-SFJP_SFPI_PX Y=SFJP_SFRA_PY+SFJP_SFRA  473
C    _DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2 Z=SFJP_SFPI_PZ ALPHAX=90 <===        473
         CALL AGSRESET                                                    476
         AG_X = -SFJP_SFPI_PX                                             476
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2     476
         AG_Z = SFJP_SFPI_PZ                                              476
         AG_ALPHAN=AG_ALPHAN+1                                            476
         AG_ALPHAK(AG_ALPHAN)=1                                           476
         AG_ALPHAL(AG_ALPHAN)=90                                          476
         AG_TITLE = 'SFPI'                                                476
         AG_EXNAME = 'POSITION'                                           476
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      476
         CALL AXPOSITION                                                  476
C    *                                                                    476
C    Geant POSITION : SFPI ===> X=SFJP_SFPI_PX Y=SFJP_SFRA_PY+SFJP_SFRA_  476
C    DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2 Z=-SFJP_SFPI_PZ ALPHAX=90 <===        476
         CALL AGSRESET                                                    479
         AG_X = SFJP_SFPI_PX                                              479
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2     479
         AG_Z = -SFJP_SFPI_PZ                                             479
         AG_ALPHAN=AG_ALPHAN+1                                            479
         AG_ALPHAK(AG_ALPHAN)=1                                           479
         AG_ALPHAL(AG_ALPHAN)=90                                          479
         AG_TITLE = 'SFPI'                                                479
         AG_EXNAME = 'POSITION'                                           479
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      479
         CALL AXPOSITION                                                  479
C    *                                                                    479
C    Geant POSITION : SFPI ===> X=-SFJP_SFPI_PX Y=SFJP_SFRA_PY+SFJP_SFRA  479
C    _DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2 Z=-SFJP_SFPI_PZ ALPHAX=90 <===       479
         CALL AGSRESET                                                    482
         AG_X = -SFJP_SFPI_PX                                             482
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFPI_PY1-SFJP_SFPI_PY2     482
         AG_Z = -SFJP_SFPI_PZ                                             482
         AG_ALPHAN=AG_ALPHAN+1                                            482
         AG_ALPHAK(AG_ALPHAN)=1                                           482
         AG_ALPHAL(AG_ALPHAN)=90                                          482
         AG_TITLE = 'SFPI'                                                482
         AG_EXNAME = 'POSITION'                                           482
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      482
         CALL AXPOSITION                                                  482
C    *                                                                    482
C    Create SFAA :                                                        483
C    Call To Local Procedure SFAA                                         483
         ASSIGN 5410 TO BLOCK_SFAA                                        483
         GO TO  5400                                                      483
5410     CONTINUE                                                         483
C    *                                                                    483
C    Geant POSITION : SFAA ===> X=-SFJP_SFAA_PX1-SFJP_SFAA_DX Y=SFJP_SFR  486
C    A_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=SFJP_SFAA_PZ1+SFJP  486
C    _SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ <===                            486
         CALL AGSRESET                                                    486
         AG_X = -SFJP_SFAA_PX1-SFJP_SFAA_DX                               486
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    486
         AG_Z = SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ    486
         AG_TITLE = 'SFAA'                                                486
         AG_EXNAME = 'POSITION'                                           486
         AG_PARLIST = 'X_Y_Z'                                             486
         CALL AXPOSITION                                                  486
C    *                                                                    486
C    Geant POSITION : SFAA ===> X=-SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_  486
C    PX2-SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_  486
C    SFAA_DY Z=SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ <=  486
C    ==                                                                   486
         CALL AGSRESET                                                    489
         AG_X = -SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_PX2-SFJP_SFAA_PX3   489
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    489
         AG_Z = SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ    489
         AG_TITLE = 'SFAA'                                                489
         AG_EXNAME = 'POSITION'                                           489
         AG_PARLIST = 'X_Y_Z'                                             489
         CALL AXPOSITION                                                  489
C    *                                                                    489
C    Geant POSITION : SFAA ===> X=-SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_  489
C    PX2-SFJP_SFAA_PX3-SFJP_SFAA_PX2-SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_S  489
C    FRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-S  489
C    FJP_SFAA_PZ3-SFJP_SFAA_DZ <===                                       489
         CALL AGSRESET                                                    493
         AG_X =                                                           493
     *   -SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_PX2-SFJP_SFAA_PX3-         493
     *   SFJP_SFAA_PX2-SFJP_SFAA_PX3                                      493
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    493
         AG_Z = SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ    493
         AG_TITLE = 'SFAA'                                                493
         AG_EXNAME = 'POSITION'                                           493
         AG_PARLIST = 'X_Y_Z'                                             493
         CALL AXPOSITION                                                  493
C    *                                                                    493
C    Geant POSITION : SFAA ===> X=+SFJP_SFAA_PX1+SFJP_SFAA_DX Y=SFJP_SFR  493
C    A_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=SFJP_SFAA_PZ1+SFJP  493
C    _SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ <===                            493
         CALL AGSRESET                                                    496
         AG_X = +SFJP_SFAA_PX1+SFJP_SFAA_DX                               496
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    496
         AG_Z = SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ    496
         AG_TITLE = 'SFAA'                                                496
         AG_EXNAME = 'POSITION'                                           496
         AG_PARLIST = 'X_Y_Z'                                             496
         CALL AXPOSITION                                                  496
C    *                                                                    496
C    Geant POSITION : SFAA ===> X=+SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_  496
C    PX2+SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_  496
C    SFAA_DY Z=SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ <=  496
C    ==                                                                   496
         CALL AGSRESET                                                    499
         AG_X = +SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_PX2+SFJP_SFAA_PX3   499
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    499
         AG_Z = SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ    499
         AG_TITLE = 'SFAA'                                                499
         AG_EXNAME = 'POSITION'                                           499
         AG_PARLIST = 'X_Y_Z'                                             499
         CALL AXPOSITION                                                  499
C    *                                                                    499
C    Geant POSITION : SFAA ===> X=+SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_  499
C    PX2+SFJP_SFAA_PX3+SFJP_SFAA_PX2+SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_S  499
C    FRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-S  499
C    FJP_SFAA_PZ3-SFJP_SFAA_DZ <===                                       499
         CALL AGSRESET                                                    503
         AG_X =                                                           503
     *   +SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_PX2+SFJP_SFAA_PX3+         503
     *   SFJP_SFAA_PX2+SFJP_SFAA_PX3                                      503
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    503
         AG_Z = SFJP_SFAA_PZ1+SFJP_SFAA_PZ2-SFJP_SFAA_PZ3-SFJP_SFAA_DZ    503
         AG_TITLE = 'SFAA'                                                503
         AG_EXNAME = 'POSITION'                                           503
         AG_PARLIST = 'X_Y_Z'                                             503
         CALL AXPOSITION                                                  503
C    *                                                                    503
C    Geant POSITION : SFAA ===> X=-SFJP_SFAA_PX1-SFJP_SFAA_DX Y=SFJP_SFR  503
C    A_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=-SFJP_SFAA_PZ1-SFJ  503
C    P_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ <===                           503
         CALL AGSRESET                                                    506
         AG_X = -SFJP_SFAA_PX1-SFJP_SFAA_DX                               506
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    506
         AG_Z = -SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ   506
         AG_TITLE = 'SFAA'                                                506
         AG_EXNAME = 'POSITION'                                           506
         AG_PARLIST = 'X_Y_Z'                                             506
         CALL AXPOSITION                                                  506
C    *                                                                    506
C    Geant POSITION : SFAA ===> X=-SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_  506
C    PX2-SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_  506
C    SFAA_DY Z=-SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ <  506
C    ===                                                                  506
         CALL AGSRESET                                                    509
         AG_X = -SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_PX2-SFJP_SFAA_PX3   509
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    509
         AG_Z = -SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ   509
         AG_TITLE = 'SFAA'                                                509
         AG_EXNAME = 'POSITION'                                           509
         AG_PARLIST = 'X_Y_Z'                                             509
         CALL AXPOSITION                                                  509
C    *                                                                    509
C    Geant POSITION : SFAA ===> X=-SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_  509
C    PX2-SFJP_SFAA_PX3-SFJP_SFAA_PX2-SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_S  509
C    FRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=-SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+  509
C    SFJP_SFAA_PZ3+SFJP_SFAA_DZ <===                                      509
         CALL AGSRESET                                                    513
         AG_X =                                                           513
     *   -SFJP_SFAA_PX1-SFJP_SFAA_DX-SFJP_SFAA_PX2-SFJP_SFAA_PX3-         513
     *   SFJP_SFAA_PX2-SFJP_SFAA_PX3                                      513
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    513
         AG_Z = -SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ   513
         AG_TITLE = 'SFAA'                                                513
         AG_EXNAME = 'POSITION'                                           513
         AG_PARLIST = 'X_Y_Z'                                             513
         CALL AXPOSITION                                                  513
C    *                                                                    513
C    Geant POSITION : SFAA ===> X=+SFJP_SFAA_PX1+SFJP_SFAA_DX Y=SFJP_SFR  513
C    A_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=-SFJP_SFAA_PZ1-SFJ  513
C    P_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ <===                           513
         CALL AGSRESET                                                    516
         AG_X = +SFJP_SFAA_PX1+SFJP_SFAA_DX                               516
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    516
         AG_Z = -SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ   516
         AG_TITLE = 'SFAA'                                                516
         AG_EXNAME = 'POSITION'                                           516
         AG_PARLIST = 'X_Y_Z'                                             516
         CALL AXPOSITION                                                  516
C    *                                                                    516
C    Geant POSITION : SFAA ===> X=+SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_  516
C    PX2+SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_  516
C    SFAA_DY Z=-SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ <  516
C    ===                                                                  516
         CALL AGSRESET                                                    519
         AG_X = +SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_PX2+SFJP_SFAA_PX3   519
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    519
         AG_Z = -SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ   519
         AG_TITLE = 'SFAA'                                                519
         AG_EXNAME = 'POSITION'                                           519
         AG_PARLIST = 'X_Y_Z'                                             519
         CALL AXPOSITION                                                  519
C    *                                                                    519
C    Geant POSITION : SFAA ===> X=+SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_  519
C    PX2+SFJP_SFAA_PX3+SFJP_SFAA_PX2+SFJP_SFAA_PX3 Y=SFJP_SFRA_PY+SFJP_S  519
C    FRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY Z=-SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+  519
C    SFJP_SFAA_PZ3+SFJP_SFAA_DZ <===                                      519
         CALL AGSRESET                                                    523
         AG_X =                                                           523
     *   +SFJP_SFAA_PX1+SFJP_SFAA_DX+SFJP_SFAA_PX2+SFJP_SFAA_PX3+         523
     *   SFJP_SFAA_PX2+SFJP_SFAA_PX3                                      523
         AG_Y = SFJP_SFRA_PY+SFJP_SFRA_DY+SFJP_SFFX_DYZ*2+SFJP_SFAA_DY    523
         AG_Z = -SFJP_SFAA_PZ1-SFJP_SFAA_PZ2+SFJP_SFAA_PZ3+SFJP_SFAA_DZ   523
         AG_TITLE = 'SFAA'                                                523
         AG_EXNAME = 'POSITION'                                           523
         AG_PARLIST = 'X_Y_Z'                                             523
         CALL AXPOSITION                                                  523
C    *                                                                    523
5281  CALL AGSPOPB                                                        524
      GO TO BLOCK_SFSW, (5280 ,5290)                                      524
C End of Local Procedure                                                  524
C ----------------------------------------------------------------------  525
C ------------ SFRA                                                       525
C Begin Local Procedure SFRA                                              526
C SFRA IS THE HYBRID STIFFNEER                                            526
5320  CONTINUE                                                            526
         AG_TITLE='SFRA'                                                  526
         AG_MODULE=MODULE                                                 526
         AG_MARK='V00'                                                    526
         CALL AGSPUSH                                                     526
         AG_MODULE=MODULE                                                 526
C    *                                                                    526
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           527
         CALL AGSRESET                                                    527
         AG_TITLE = 'CARBON'                                              527
         AG_EXNAME = 'MATERIAL'                                           527
         AG_PARLIST = 'NONE'                                              527
         CALL AXMATERIAL                                                  527
C    *                                                                    527
C    Geant ATTRIBUTE : SFRA ===> SEEN=2 COLO=1 <===                       527
         CALL AGSRESET                                                    528
         AG_SEEN = 2                                                      528
         AG_COLO = 1                                                      528
         AG_TITLE = 'SFRA'                                                528
         AG_EXNAME = 'ATTRIBUTE'                                          528
         AG_PARLIST = 'SEEN_COLO'                                         528
         CALL AXATTRIBUTE                                                 528
C    *                                                                    528
C    Geant SHAPE : BOX ===> DX=SFJP_SFRA_DX DY=SFJP_SFRA_DY, DZ=SFJP_SFR  528
C    A_DZ <===                                                            528
         CALL AGSRESET                                                    530
         AG_DX = SFJP_SFRA_DX                                             530
         AG_DY = SFJP_SFRA_DY                                             530
         AG_DZ = SFJP_SFRA_DZ                                             530
         AG_TITLE = 'BOX'                                                 530
         AG_EXNAME = 'SHAPE'                                              530
         AG_PARLIST = 'DX_DY_DZ'                                          530
         CALL AXSHAPE                                                     530
C    *                                                                    530
5321  CALL AGSPOPB                                                        531
      GO TO BLOCK_SFRA, (5320 ,5330)                                      531
C End of Local Procedure                                                  531
C ----------------------------------------------------------------------  532
C ------------ SFRS                                                       532
C Begin Local Procedure SFRS                                              533
C SFRS TWO SUPPORTS OF THE HYBRID STIFFNEER (PIECE OF IT)                 533
5340  CONTINUE                                                            533
         AG_TITLE='SFRS'                                                  533
         AG_MODULE=MODULE                                                 533
         AG_MARK='V00'                                                    533
         CALL AGSPUSH                                                     533
         AG_MODULE=MODULE                                                 533
C    *                                                                    533
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           534
         CALL AGSRESET                                                    534
         AG_TITLE = 'CARBON'                                              534
         AG_EXNAME = 'MATERIAL'                                           534
         AG_PARLIST = 'NONE'                                              534
         CALL AXMATERIAL                                                  534
C    *                                                                    534
C    Geant ATTRIBUTE : SFRS ===> SEEN=2 COLO=1 <===                       534
         CALL AGSRESET                                                    535
         AG_SEEN = 2                                                      535
         AG_COLO = 1                                                      535
         AG_TITLE = 'SFRS'                                                535
         AG_EXNAME = 'ATTRIBUTE'                                          535
         AG_PARLIST = 'SEEN_COLO'                                         535
         CALL AXATTRIBUTE                                                 535
C    *                                                                    535
C    Geant SHAPE : BOX ===> DX=SFJP_SFRS_DX DY=SFJP_SFRS_DY, DZ=SFJP_SFR  535
C    A_DZ <===                                                            535
         CALL AGSRESET                                                    537
         AG_DX = SFJP_SFRS_DX                                             537
         AG_DY = SFJP_SFRS_DY                                             537
         AG_DZ = SFJP_SFRA_DZ                                             537
         AG_TITLE = 'BOX'                                                 537
         AG_EXNAME = 'SHAPE'                                              537
         AG_PARLIST = 'DX_DY_DZ'                                          537
         CALL AXSHAPE                                                     537
C    *                                                                    537
5341  CALL AGSPOPB                                                        538
      GO TO BLOCK_SFRS, (5340 ,5350)                                      538
C End of Local Procedure                                                  538
C ----------------------------------------------------------------------  539
C ------------ SFFX                                                       539
C Begin Local Procedure SFFX                                              540
C SFFX IS THE FLEX                                                        540
5360  CONTINUE                                                            540
         AG_TITLE='SFFX'                                                  540
         AG_MODULE=MODULE                                                 540
         AG_MARK='V00'                                                    540
         CALL AGSPUSH                                                     540
         AG_MODULE=MODULE                                                 540
C    *                                                                    540
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           541
         CALL AGSRESET                                                    541
         AG_TITLE = 'COPPER'                                              541
         AG_EXNAME = 'MATERIAL'                                           541
         AG_PARLIST = 'NONE'                                              541
         CALL AXMATERIAL                                                  541
C    *                                                                    541
C    Geant ATTRIBUTE : SFFX ===> SEEN=1 COLO=42 <===                      541
         CALL AGSRESET                                                    542
         AG_SEEN = 1                                                      542
         AG_COLO = 42                                                     542
         AG_TITLE = 'SFFX'                                                542
         AG_EXNAME = 'ATTRIBUTE'                                          542
         AG_PARLIST = 'SEEN_COLO'                                         542
         CALL AXATTRIBUTE                                                 542
C    *                                                                    542
C    Geant SHAPE : BOX ===> DX=SFJP_SFFX_DX DY=SFJP_SFFX_DYZ, DZ=SFJP_SF  542
C    RA_DZ <===                                                           542
         CALL AGSRESET                                                    544
         AG_DX = SFJP_SFFX_DX                                             544
         AG_DY = SFJP_SFFX_DYZ                                            544
         AG_DZ = SFJP_SFRA_DZ                                             544
         AG_TITLE = 'BOX'                                                 544
         AG_EXNAME = 'SHAPE'                                              544
         AG_PARLIST = 'DX_DY_DZ'                                          544
         CALL AXSHAPE                                                     544
C    *                                                                    544
5361  CALL AGSPOPB                                                        545
      GO TO BLOCK_SFFX, (5360 ,5370)                                      545
C End of Local Procedure                                                  545
C ----------------------------------------------------------------------  546
C ------------ SFAA                                                       546
C Begin Local Procedure SFAA                                              547
C SFAA IS THE A128C CHIP                                                  547
5400  CONTINUE                                                            547
         AG_TITLE='SFAA'                                                  547
         AG_MODULE=MODULE                                                 547
         AG_MARK='V00'                                                    547
         CALL AGSPUSH                                                     547
         AG_MODULE=MODULE                                                 547
C    *                                                                    547
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          548
         CALL AGSRESET                                                    548
         AG_TITLE = 'SILICON'                                             548
         AG_EXNAME = 'MATERIAL'                                           548
         AG_PARLIST = 'NONE'                                              548
         CALL AXMATERIAL                                                  548
C    *                                                                    548
C    Geant ATTRIBUTE : SFAA ===> SEEN=1 COLO=41 <===                      548
         CALL AGSRESET                                                    549
         AG_SEEN = 1                                                      549
         AG_COLO = 41                                                     549
         AG_TITLE = 'SFAA'                                                549
         AG_EXNAME = 'ATTRIBUTE'                                          549
         AG_PARLIST = 'SEEN_COLO'                                         549
         CALL AXATTRIBUTE                                                 549
C    *                                                                    549
C    Geant SHAPE : BOX ===> DX=SFJP_SFAA_DX DY=SFJP_SFAA_DY, DZ=SFJP_SFA  549
C    A_DZ <===                                                            549
         CALL AGSRESET                                                    551
         AG_DX = SFJP_SFAA_DX                                             551
         AG_DY = SFJP_SFAA_DY                                             551
         AG_DZ = SFJP_SFAA_DZ                                             551
         AG_TITLE = 'BOX'                                                 551
         AG_EXNAME = 'SHAPE'                                              551
         AG_PARLIST = 'DX_DY_DZ'                                          551
         CALL AXSHAPE                                                     551
C    *                                                                    551
5401  CALL AGSPOPB                                                        552
      GO TO BLOCK_SFAA, (5400 ,5410)                                      552
C End of Local Procedure                                                  552
C ----------------------------------------------------------------------  553
C ------------ SFPI                                                       553
C Begin Local Procedure SFPI                                              554
C SFPI ARE THE PIONS                                                      554
5380  CONTINUE                                                            554
         AG_TITLE='SFPI'                                                  554
         AG_MODULE=MODULE                                                 554
         AG_MARK='V00'                                                    554
         CALL AGSPUSH                                                     554
         AG_MODULE=MODULE                                                 554
C    *                                                                    554
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        555
         CALL AGSRESET                                                    555
         AG_TITLE = 'ALUMINIUM'                                           555
         AG_EXNAME = 'MATERIAL'                                           555
         AG_PARLIST = 'NONE'                                              555
         CALL AXMATERIAL                                                  555
C    *                                                                    555
C    Geant ATTRIBUTE : SFPI ===> SEEN=2 COLO=33 <===                      555
         CALL AGSRESET                                                    556
         AG_SEEN = 2                                                      556
         AG_COLO = 33                                                     556
         AG_TITLE = 'SFPI'                                                556
         AG_EXNAME = 'ATTRIBUTE'                                          556
         AG_PARLIST = 'SEEN_COLO'                                         556
         CALL AXATTRIBUTE                                                 556
C    *                                                                    556
C    Geant SHAPE : TUBE ===> RMIN=SFJP_SFPI_RMIN/2. RMAX=SFJP_SFPI_RMAX/  556
C    2., DZ=SFJP_SFPI_PZ/2.0 <===                                         556
         CALL AGSRESET                                                    558
         AG_RMIN = SFJP_SFPI_RMIN/2.                                      558
         AG_RMAX = SFJP_SFPI_RMAX/2.                                      558
         AG_DZ = SFJP_SFPI_PZ/2.0                                         558
         AG_TITLE = 'TUBE'                                                558
         AG_EXNAME = 'SHAPE'                                              558
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      558
         CALL AXSHAPE                                                     558
C    *                                                                    558
C    Create SFPJ :                                                        559
C    Call To Local Procedure SFPJ                                         559
         ASSIGN 5430 TO BLOCK_SFPJ                                        559
         GO TO  5420                                                      559
5430     CONTINUE                                                         559
C    *                                                                    559
C    Geant POSITION : SFPJ ===> X=0. Y=0. Z=-SFJP_SFPI_PZ/2.0+SFJP_SFPJ_  560
C    DZ <===                                                              560
         CALL AGSRESET                                                    560
         AG_X = 0.                                                        560
         AG_Y = 0.                                                        560
         AG_Z = -SFJP_SFPI_PZ/2.0+SFJP_SFPJ_DZ                            560
         AG_TITLE = 'SFPJ'                                                560
         AG_EXNAME = 'POSITION'                                           560
         AG_PARLIST = 'X_Y_Z'                                             560
         CALL AXPOSITION                                                  560
C    *                                                                    560
5381  CALL AGSPOPB                                                        561
      GO TO BLOCK_SFPI, (5380 ,5390)                                      561
C End of Local Procedure                                                  561
C ----------------------------------------------------------------------  562
C ------------ SFPJ                                                       562
C Begin Local Procedure SFPJ                                              563
C SFPJ IS THE BASE OF THE PIONS                                           563
5420  CONTINUE                                                            563
         AG_TITLE='SFPJ'                                                  563
         AG_MODULE=MODULE                                                 563
         AG_MARK='V00'                                                    563
         CALL AGSPUSH                                                     563
         AG_MODULE=MODULE                                                 563
C    *                                                                    563
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        564
         CALL AGSRESET                                                    564
         AG_TITLE = 'ALUMINIUM'                                           564
         AG_EXNAME = 'MATERIAL'                                           564
         AG_PARLIST = 'NONE'                                              564
         CALL AXMATERIAL                                                  564
C    *                                                                    564
C    Geant ATTRIBUTE : SFPJ ===> SEEN=2 COLO=33 <===                      564
         CALL AGSRESET                                                    565
         AG_SEEN = 2                                                      565
         AG_COLO = 33                                                     565
         AG_TITLE = 'SFPJ'                                                565
         AG_EXNAME = 'ATTRIBUTE'                                          565
         AG_PARLIST = 'SEEN_COLO'                                         565
         CALL AXATTRIBUTE                                                 565
C    *                                                                    565
C    Geant SHAPE : TUBE ===> RMIN=SFJP_SFPJ_DX1+SFJP_SFPJ_DX2, RMAX=SFJP  565
C    _SFPJ_DY DZ=SFJP_SFPJ_DZ <===                                        565
         CALL AGSRESET                                                    567
         AG_RMIN = SFJP_SFPJ_DX1+SFJP_SFPJ_DX2                            567
         AG_RMAX = SFJP_SFPJ_DY                                           567
         AG_DZ = SFJP_SFPJ_DZ                                             567
         AG_TITLE = 'TUBE'                                                567
         AG_EXNAME = 'SHAPE'                                              567
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      567
         CALL AXSHAPE                                                     567
C    *                                                                    567
5421  CALL AGSPOPB                                                        568
      GO TO BLOCK_SFPJ, (5420 ,5430)                                      568
C End of Local Procedure                                                  568
C ----------------------------------------------------------------------  569
C ------------ SFSD                                                       569
C Begin Local Procedure SFSD                                              570
C SFSD IS THE STRIP DETECTOR                                              570
5300  CONTINUE                                                            570
         AG_TITLE='SFSD'                                                  570
         AG_MODULE=MODULE                                                 570
         AG_MARK='V00'                                                    570
         CALL AGSPUSH                                                     570
         AG_MODULE=MODULE                                                 570
C    *                                                                    570
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          571
         CALL AGSRESET                                                    571
         AG_TITLE = 'SILICON'                                             571
         AG_EXNAME = 'MATERIAL'                                           571
         AG_PARLIST = 'NONE'                                              571
         CALL AXMATERIAL                                                  571
C    *                                                                    571
C    Geant MATERIAL : SENSITIVE ===> ISVOL=1 <===                         571
         CALL AGSRESET                                                    572
         AG_ISVOL = 1                                                     572
         AG_TITLE = 'SENSITIVE'                                           572
         AG_EXNAME = 'MATERIAL'                                           572
         AG_PARLIST = 'ISVOL'                                             572
         CALL AXMATERIAL                                                  572
C    *                                                                    572
C    Geant ATTRIBUTE : SFSD ===> SEEN=2 COLO=41 <===                      572
         CALL AGSRESET                                                    573
         AG_SEEN = 2                                                      573
         AG_COLO = 41                                                     573
         AG_TITLE = 'SFSD'                                                573
         AG_EXNAME = 'ATTRIBUTE'                                          573
         AG_PARLIST = 'SEEN_COLO'                                         573
         CALL AXATTRIBUTE                                                 573
C    *                                                                    573
C    Geant SHAPE : BOX ===> DX=SFJP_SFSD_DX DY=SFJP_SFSD_DY, DZ=SFJP_SFS  573
C    D_DZ <===                                                            573
         CALL AGSRESET                                                    575
         AG_DX = SFJP_SFSD_DX                                             575
         AG_DY = SFJP_SFSD_DY                                             575
         AG_DZ = SFJP_SFSD_DZ                                             575
         AG_TITLE = 'BOX'                                                 575
         AG_EXNAME = 'SHAPE'                                              575
         AG_PARLIST = 'DX_DY_DZ'                                          575
         CALL AXSHAPE                                                     575
C    *                                                                    575
         CALL GSTPAR( AG_IMED,'STRA',1. )                                 575
C    Defining Hits: X:0.001:S Y:0.001: Z:0.001: CX:10: CY:10: CZ:10: STE  578
C    P:0.01: SLENG:16:(0,500), TOF:16:(0,1.0E-6) PTOT:16:(0,100.0) ELOSS  578
C    :16:(0,0.001)                                                        578
         AG_TITLE = 'SFSD'                                                578
         AG_EXNAME = 'HITS'                                               578
         AG_TYPE=AG_MAGIC                                                 578
         CALL AGSDETV('SFSD')                                             578
C    *                                                                    578
         AG_PARLIST='X'                                                   578
         AG_TITLE = 'DETH'                                                578
         AG_NBITS = AG_MAGIC                                              578
         AG_BIN=0.001                                                     578
         AG_OPTION='S'                                                    578
         AG_FHMIN = AG_MAGIC                                              578
         AG_FHMAX = AG_MAGIC                                              578
         CALL AGSDETH (0)                                                 578
C    *                                                                    578
         AG_PARLIST='Y'                                                   578
         AG_TITLE = 'DETH'                                                578
         AG_NBITS = AG_MAGIC                                              578
         AG_BIN=0.001                                                     578
         AG_FHMIN=AG_MAGIC                                                578
         AG_FHMAX=AG_MAGIC                                                578
         AG_OPTION='*'                                                    578
         CALL AGSDETH (0)                                                 578
C    *                                                                    578
         AG_PARLIST='Z'                                                   578
         AG_TITLE = 'DETH'                                                578
         AG_NBITS = AG_MAGIC                                              578
         AG_BIN=0.001                                                     578
         AG_FHMIN=AG_MAGIC                                                578
         AG_FHMAX=AG_MAGIC                                                578
         AG_OPTION='*'                                                    578
         CALL AGSDETH (0)                                                 578
C    *                                                                    578
         AG_PARLIST='CX'                                                  578
         AG_TITLE = 'DETH'                                                578
         AG_NBITS = 10                                                    578
         AG_BIN=AG_MAGIC                                                  578
         AG_FHMIN=AG_MAGIC                                                578
         AG_FHMAX=AG_MAGIC                                                578
         AG_OPTION='*'                                                    578
         CALL AGSDETH (0)                                                 578
C    *                                                                    578
         AG_PARLIST='CY'                                                  578
         AG_TITLE = 'DETH'                                                578
         AG_NBITS = 10                                                    578
         AG_BIN=AG_MAGIC                                                  578
         AG_FHMIN=AG_MAGIC                                                578
         AG_FHMAX=AG_MAGIC                                                578
         AG_OPTION='*'                                                    578
         CALL AGSDETH (0)                                                 578
C    *                                                                    578
         AG_PARLIST='CZ'                                                  578
         AG_TITLE = 'DETH'                                                578
         AG_NBITS = 10                                                    578
         AG_BIN=AG_MAGIC                                                  578
         AG_FHMIN=AG_MAGIC                                                578
         AG_FHMAX=AG_MAGIC                                                578
         AG_OPTION='*'                                                    578
         CALL AGSDETH (0)                                                 578
C    *                                                                    578
         AG_PARLIST='STEP'                                                578
         AG_TITLE = 'DETH'                                                578
         AG_NBITS = AG_MAGIC                                              578
         AG_BIN=0.01                                                      578
         AG_FHMIN=AG_MAGIC                                                578
         AG_FHMAX=AG_MAGIC                                                578
         AG_OPTION='*'                                                    578
         CALL AGSDETH (0)                                                 578
C    *                                                                    578
         AG_PARLIST='SLENG'                                               578
         AG_TITLE = 'DETH'                                                578
         AG_NBITS = 16                                                    578
         AG_BIN=AG_MAGIC                                                  578
         AG_OPTION='*'                                                    578
         AG_FHMIN = 0                                                     578
         AG_FHMAX = 500                                                   578
         CALL AGSDETH (0)                                                 578
C    *                                                                    578
         AG_PARLIST='TOF'                                                 578
         AG_TITLE = 'DETH'                                                578
         AG_NBITS = 16                                                    578
         AG_BIN=AG_MAGIC                                                  578
         AG_OPTION='*'                                                    578
         AG_FHMIN = 0                                                     578
         AG_FHMAX = 1.0E-6                                                578
         CALL AGSDETH (0)                                                 578
C    *                                                                    578
         AG_PARLIST='PTOT'                                                578
         AG_TITLE = 'DETH'                                                578
         AG_NBITS = 16                                                    578
         AG_BIN=AG_MAGIC                                                  578
         AG_OPTION='*'                                                    578
         AG_FHMIN = 0                                                     578
         AG_FHMAX = 100.0                                                 578
         CALL AGSDETH (0)                                                 578
C    *                                                                    578
         AG_PARLIST='ELOSS'                                               578
         AG_TITLE = 'DETH'                                                578
         AG_NBITS = 16                                                    578
         AG_BIN=AG_MAGIC                                                  578
         AG_OPTION='*'                                                    578
         AG_FHMIN = 0                                                     578
         AG_FHMAX = 0.001                                                 578
         CALL AGSDETH (0)                                                 578
C    *                                                                    578
         CALL AGSDETU('SFSD')                                             578
C    *                                                                    578
5301  CALL AGSPOPB                                                        579
      GO TO BLOCK_SFSD, (5300 ,5310)                                      579
C End of Local Procedure                                                  579
C ----------------------------------------------------------------------  580
C ------------ SFSM                                                       580
C Begin Local Procedure SFSM                                              581
C SFSM IS THE MOTHER VOLUME OF THE LADDER (MECHANIC STRUCTURE)            581
5250  CONTINUE                                                            581
         AG_TITLE='SFSM'                                                  581
         AG_MODULE=MODULE                                                 581
         AG_MARK='V00'                                                    581
         CALL AGSPUSH                                                     581
         AG_MODULE=MODULE                                                 581
C    *                                                                    581
C    Geant MATERIAL : AIR ===> inherited arguments used <===              582
         CALL AGSRESET                                                    582
         AG_TITLE = 'AIR'                                                 582
         AG_EXNAME = 'MATERIAL'                                           582
         AG_PARLIST = 'NONE'                                              582
         CALL AXMATERIAL                                                  582
C    *                                                                    582
C    Geant ATTRIBUTE : SFSM ===> SEEN=0 COLO=6 <===                       582
         CALL AGSRESET                                                    583
         AG_SEEN = 0                                                      583
         AG_COLO = 6                                                      583
         AG_TITLE = 'SFSM'                                                583
         AG_EXNAME = 'ATTRIBUTE'                                          583
         AG_PARLIST = 'SEEN_COLO'                                         583
         CALL AXATTRIBUTE                                                 583
C    *                                                                    583
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=SFPA_SMTHK/2, DZ=SFPA_SML  583
C    EN/2. <===                                                           583
         CALL AGSRESET                                                    585
         AG_DX = SFPA_DMWID/2                                             585
         AG_DY = SFPA_SMTHK/2                                             585
         AG_DZ = SFPA_SMLEN/2.                                            585
         AG_TITLE = 'BOX'                                                 585
         AG_EXNAME = 'SHAPE'                                              585
         AG_PARLIST = 'DX_DY_DZ'                                          585
         CALL AXSHAPE                                                     585
C    *                                                                    585
         YOFFSET = - 1.7                                                  585
C    Create SFLT :                                                        587
C    Call To Local Procedure SFLT                                         587
         ASSIGN 5450 TO BLOCK_SFLT                                        587
         GO TO  5440                                                      587
5450     CONTINUE                                                         587
C    *                                                                    587
         HIGHT = SFJP_SFSM_LL*TAN(54*PI/180) - SFPB_HHIGHT/TAN(PI/5) -    588
     *   0.02                                                             588
C    Geant POSITION : SFLT ===> X=-(SFPB_HHIGHT-SFPB_KHIGHT) Y=HIGHT+YOF  589
C    FSET Z=0.0 ALPHAZ=-90-36 <===                                        589
         CALL AGSRESET                                                    589
         AG_X = -(SFPB_HHIGHT-SFPB_KHIGHT)                                589
         AG_Y = HIGHT+YOFFSET                                             589
         AG_Z = 0.0                                                       589
         AG_ALPHAN=AG_ALPHAN+1                                            589
         AG_ALPHAK(AG_ALPHAN)=3                                           589
         AG_ALPHAL(AG_ALPHAN)=-90-36                                      589
         AG_TITLE = 'SFLT'                                                589
         AG_EXNAME = 'POSITION'                                           589
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      589
         CALL AXPOSITION                                                  589
C    *                                                                    589
C    Geant POSITION : SFLT ===> X=(SFPB_HHIGHT-SFPB_KHIGHT) Y=HIGHT+YOFF  589
C    SET Z=0 ALPHAY=180. ALPHAZ=90.+36. <===                              589
         CALL AGSRESET                                                    591
         AG_X = (SFPB_HHIGHT-SFPB_KHIGHT)                                 591
         AG_Y = HIGHT+YOFFSET                                             591
         AG_Z = 0                                                         591
         AG_ALPHAN=AG_ALPHAN+1                                            591
         AG_ALPHAK(AG_ALPHAN)=2                                           591
         AG_ALPHAL(AG_ALPHAN)=180.                                        591
         AG_ALPHAN=AG_ALPHAN+1                                            591
         AG_ALPHAK(AG_ALPHAN)=3                                           591
         AG_ALPHAL(AG_ALPHAN)=90.+36.                                     591
         AG_TITLE = 'SFLT'                                                591
         AG_EXNAME = 'POSITION'                                           591
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               591
         CALL AXPOSITION                                                  591
C    *                                                                    591
C    Create SFLU :                                                        592
C    Call To Local Procedure SFLU                                         592
         ASSIGN 5470 TO BLOCK_SFLU                                        592
         GO TO  5460                                                      592
5470     CONTINUE                                                         592
C    *                                                                    592
C    Geant POSITION : SFLU ===> X=0.-(SFJP_SFSM_LL-(SFJP_SFLU_BL1+SFJP_S  594
C    FLU_H1/2*1/TAN(PI/5))) Y=0.+YOFFSET Z=0. <===                        594
         CALL AGSRESET                                                    594
         AG_X = 0.-(SFJP_SFSM_LL-(SFJP_SFLU_BL1+SFJP_SFLU_H1/2*1/TAN(PI/  594
     *   5)))                                                             594
         AG_Y = 0.+YOFFSET                                                594
         AG_Z = 0.                                                        594
         AG_TITLE = 'SFLU'                                                594
         AG_EXNAME = 'POSITION'                                           594
         AG_PARLIST = 'X_Y_Z'                                             594
         CALL AXPOSITION                                                  594
C    *                                                                    594
         ESSAI = 180                                                      594
         ESSAI = ESSAI + 27 + 27                                          596
C    Geant POSITION : SFLU ===> X=-COS(63*PI/180)*2*(SFPB_HBASE-SFPB_KBA  599
C    SE)-(SFJP_SFSM_LL-(0.2+0.01/TAN(PI/5))) Y=COS(27*PI/180)*2*(SFPB_HB  599
C    ASE-SFPB_KBASE)+YOFFSET Z=0 ALPHAY=180 ALPHAZ=ESSAI <===             599
         CALL AGSRESET                                                    599
         AG_X = -COS(63*PI/                                               599
     *   180)*2*(SFPB_HBASE-SFPB_KBASE)-(SFJP_SFSM_LL-(0.2+0.01/TAN(PI/   599
     *   5)))                                                             599
         AG_Y = COS(27*PI/180)*2*(SFPB_HBASE-SFPB_KBASE)+YOFFSET          599
         AG_Z = 0                                                         599
         AG_ALPHAN=AG_ALPHAN+1                                            599
         AG_ALPHAK(AG_ALPHAN)=2                                           599
         AG_ALPHAL(AG_ALPHAN)=180                                         599
         AG_ALPHAN=AG_ALPHAN+1                                            599
         AG_ALPHAK(AG_ALPHAN)=3                                           599
         AG_ALPHAL(AG_ALPHAN)=ESSAI                                       599
         AG_TITLE = 'SFLU'                                                599
         AG_EXNAME = 'POSITION'                                           599
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               599
         CALL AXPOSITION                                                  599
C    *                                                                    599
C    Geant POSITION : SFLU ===> X=0.+(SFJP_SFSM_LL-(0.2+0.01/TAN(PI/5)))  599
C     Y=0.+YOFFSET Z=0. ALPHAY=180. <===                                  599
         CALL AGSRESET                                                    601
         AG_X = 0.+(SFJP_SFSM_LL-(0.2+0.01/TAN(PI/5)))                    601
         AG_Y = 0.+YOFFSET                                                601
         AG_Z = 0.                                                        601
         AG_ALPHAN=AG_ALPHAN+1                                            601
         AG_ALPHAK(AG_ALPHAN)=2                                           601
         AG_ALPHAL(AG_ALPHAN)=180.                                        601
         AG_TITLE = 'SFLU'                                                601
         AG_EXNAME = 'POSITION'                                           601
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      601
         CALL AXPOSITION                                                  601
C    *                                                                    601
C    Geant POSITION : SFLU ===> X=COS(63*PI/180)*2*(SFPB_HBASE-SFPB_KBAS  601
C    E)+(SFJP_SFSM_LL-(0.2+0.01/TAN(PI/5))) Y=COS(27*PI/180)*2*(SFPB_HBA  601
C    SE-SFPB_KBASE)+YOFFSET Z=0 ALPHAZ=-ESSAI <===                        601
         CALL AGSRESET                                                    604
         AG_X = COS(63*PI/                                                604
     *   180)*2*(SFPB_HBASE-SFPB_KBASE)+(SFJP_SFSM_LL-(0.2+0.01/TAN(PI/   604
     *   5)))                                                             604
         AG_Y = COS(27*PI/180)*2*(SFPB_HBASE-SFPB_KBASE)+YOFFSET          604
         AG_Z = 0                                                         604
         AG_ALPHAN=AG_ALPHAN+1                                            604
         AG_ALPHAK(AG_ALPHAN)=3                                           604
         AG_ALPHAL(AG_ALPHAN)=-ESSAI                                      604
         AG_TITLE = 'SFLU'                                                604
         AG_EXNAME = 'POSITION'                                           604
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      604
         CALL AXPOSITION                                                  604
C    *                                                                    604
C    Create SFFK :                                                        605
C    Call To Local Procedure SFFK                                         605
         ASSIGN 5490 TO BLOCK_SFFK                                        605
         GO TO  5480                                                      605
5490     CONTINUE                                                         605
C    *                                                                    605
C    Geant POSITION : SFFK ===> X=SFJP_SFFK_PX/2+SFJP_SFFK_DXE Y=-SFJP_S  607
C    FFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY+YOFFSET Z=0.0 <===                607
         CALL AGSRESET                                                    607
         AG_X = SFJP_SFFK_PX/2+SFJP_SFFK_DXE                              607
         AG_Y = -SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY+YOFFSET         607
         AG_Z = 0.0                                                       607
         AG_TITLE = 'SFFK'                                                607
         AG_EXNAME = 'POSITION'                                           607
         AG_PARLIST = 'X_Y_Z'                                             607
         CALL AXPOSITION                                                  607
C    *                                                                    607
C    Geant POSITION : SFFK ===> X=-SFJP_SFFK_PX/2-SFJP_SFFK_DXE Y=-SFJP_  607
C    SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY+YOFFSET Z=0.0 <===               607
         CALL AGSRESET                                                    609
         AG_X = -SFJP_SFFK_PX/2-SFJP_SFFK_DXE                             609
         AG_Y = -SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY+YOFFSET         609
         AG_Z = 0.0                                                       609
         AG_TITLE = 'SFFK'                                                609
         AG_EXNAME = 'POSITION'                                           609
         AG_PARLIST = 'X_Y_Z'                                             609
         CALL AXPOSITION                                                  609
C    *                                                                    609
C    Create SFFL :                                                        610
C    Call To Local Procedure SFFL                                         610
         ASSIGN 5510 TO BLOCK_SFFL                                        610
         GO TO  5500                                                      610
5510     CONTINUE                                                         610
C    *                                                                    610
C    Geant POSITION : SFFL ===> X=-SFJP_SFKL_PX-SFPB_FSIZE*COS(15*PI/180  613
C    ) Y=-SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(75*PI/  613
C    180)+YOFFSET Z=0 ALPHAZ=15 <===                                      613
         CALL AGSRESET                                                    613
         AG_X = -SFJP_SFKL_PX-SFPB_FSIZE*COS(15*PI/180)                   613
         AG_Y =                                                           613
     *   -SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(75*PI/  613
     *   180)+YOFFSET                                                     613
         AG_Z = 0                                                         613
         AG_ALPHAN=AG_ALPHAN+1                                            613
         AG_ALPHAK(AG_ALPHAN)=3                                           613
         AG_ALPHAL(AG_ALPHAN)=15                                          613
         AG_TITLE = 'SFFL'                                                613
         AG_EXNAME = 'POSITION'                                           613
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      613
         CALL AXPOSITION                                                  613
C    *                                                                    613
C    Geant POSITION : SFFL ===> X=SFJP_SFKL_PX+SFPB_FSIZE*COS(15*PI/180)  613
C     Y=-SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(75*PI/1  613
C    80)+YOFFSET Z=0 ALPHAZ=-15 <===                                      613
         CALL AGSRESET                                                    616
         AG_X = SFJP_SFKL_PX+SFPB_FSIZE*COS(15*PI/180)                    616
         AG_Y =                                                           616
     *   -SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(75*PI/  616
     *   180)+YOFFSET                                                     616
         AG_Z = 0                                                         616
         AG_ALPHAN=AG_ALPHAN+1                                            616
         AG_ALPHAK(AG_ALPHAN)=3                                           616
         AG_ALPHAL(AG_ALPHAN)=-15                                         616
         AG_TITLE = 'SFFL'                                                616
         AG_EXNAME = 'POSITION'                                           616
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      616
         CALL AXPOSITION                                                  616
C    *                                                                    616
C    Create SFKK :                                                        617
C    Call To Local Procedure SFKK                                         617
         ASSIGN 5530 TO BLOCK_SFKK                                        617
         GO TO  5520                                                      617
5530     CONTINUE                                                         617
C    *                                                                    617
C    Geant POSITION : SFKK ===> X=0. Y=-SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP  619
C    _SFFK_PY2+YOFFSET-SFJP_SFKK_DY/2. Z=0. <===                          619
         CALL AGSRESET                                                    619
         AG_X = 0.                                                        619
         AG_Y =                                                           619
     *   -SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_PY2+YOFFSET-              619
     *   SFJP_SFKK_DY/2.                                                  619
         AG_Z = 0.                                                        619
         AG_TITLE = 'SFKK'                                                619
         AG_EXNAME = 'POSITION'                                           619
         AG_PARLIST = 'X_Y_Z'                                             619
         CALL AXPOSITION                                                  619
C    *                                                                    619
C    Create SFKL :                                                        620
C    Call To Local Procedure SFKL                                         620
         ASSIGN 5550 TO BLOCK_SFKL                                        620
         GO TO  5540                                                      620
5550     CONTINUE                                                         620
C    *                                                                    620
C    Geant POSITION : SFKL ===> X=-SFJP_SFKL_PX-SFPB_FSIZE*COS(15*PI/180  623
C    )+0.005 Y=-SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(  623
C    75*PI/180)+YOFFSET-0.021 Z=0 ALPHAZ=15 <===                          623
         CALL AGSRESET                                                    623
         AG_X = -SFJP_SFKL_PX-SFPB_FSIZE*COS(15*PI/180)+0.005             623
         AG_Y =                                                           623
     *   -SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(75*PI/  623
     *   180)+YOFFSET-0.021                                               623
         AG_Z = 0                                                         623
         AG_ALPHAN=AG_ALPHAN+1                                            623
         AG_ALPHAK(AG_ALPHAN)=3                                           623
         AG_ALPHAL(AG_ALPHAN)=15                                          623
         AG_TITLE = 'SFKL'                                                623
         AG_EXNAME = 'POSITION'                                           623
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      623
         CALL AXPOSITION                                                  623
C    *                                                                    623
C    Geant POSITION : SFKL ===> X=+SFJP_SFKL_PX+SFPB_FSIZE*COS(15*PI/180  623
C    )-0.005 Y=-SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(  623
C    75*PI/180)+YOFFSET-0.021 Z=0 ALPHAZ=-15 <===                         623
         CALL AGSRESET                                                    626
         AG_X = +SFJP_SFKL_PX+SFPB_FSIZE*COS(15*PI/180)-0.005             626
         AG_Y =                                                           626
     *   -SFJP_SFFK_PY1-SFJP_SFFK_PY2-SFJP_SFFK_DY-SFPB_FSIZE*COS(75*PI/  626
     *   180)+YOFFSET-0.021                                               626
         AG_Z = 0                                                         626
         AG_ALPHAN=AG_ALPHAN+1                                            626
         AG_ALPHAK(AG_ALPHAN)=3                                           626
         AG_ALPHAL(AG_ALPHAN)=-15                                         626
         AG_TITLE = 'SFKL'                                                626
         AG_EXNAME = 'POSITION'                                           626
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      626
         CALL AXPOSITION                                                  626
C    *                                                                    626
C    Create SAPP :                                                        627
C    Call To Local Procedure SAPP                                         627
         ASSIGN 5570 TO BLOCK_SAPP                                        627
         GO TO  5560                                                      627
5570     CONTINUE                                                         627
C    *                                                                    627
C    Geant POSITION : SAPP ===> X=0. Y=-SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJ  629
C    P_SAPP_PY3)/2.+YOFFSET Z=SFJP_SFFK_DZ/2+SFJP_SAPP_DZ/2. ALPHAY=180.  629
C     <===                                                                629
         CALL AGSRESET                                                    629
         AG_X = 0.                                                        629
         AG_Y = -SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJP_SAPP_PY3)/2.+YOFFSET   629
         AG_Z = SFJP_SFFK_DZ/2+SFJP_SAPP_DZ/2.                            629
         AG_ALPHAN=AG_ALPHAN+1                                            629
         AG_ALPHAK(AG_ALPHAN)=2                                           629
         AG_ALPHAL(AG_ALPHAN)=180.                                        629
         AG_TITLE = 'SAPP'                                                629
         AG_EXNAME = 'POSITION'                                           629
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      629
         CALL AXPOSITION                                                  629
C    *                                                                    629
C    Geant POSITION : SAPP ===> X=0. Y=-SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJ  629
C    P_SAPP_PY3)/2.+YOFFSET Z=-(SFJP_SFFK_DZ/2+SFJP_SAPP_DZ/2.) <===      629
         CALL AGSRESET                                                    631
         AG_X = 0.                                                        631
         AG_Y = -SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJP_SAPP_PY3)/2.+YOFFSET   631
         AG_Z = -(SFJP_SFFK_DZ/2+SFJP_SAPP_DZ/2.)                         631
         AG_TITLE = 'SAPP'                                                631
         AG_EXNAME = 'POSITION'                                           631
         AG_PARLIST = 'X_Y_Z'                                             631
         CALL AXPOSITION                                                  631
C    *                                                                    631
C    Create SFAM :                                                        632
C    Call To Local Procedure SFAM                                         632
         ASSIGN 5590 TO BLOCK_SFAM                                        632
         GO TO  5580                                                      632
5590     CONTINUE                                                         632
C    *                                                                    632
C    Geant POSITION : SFAM ===> X=0. Y=-SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJ  634
C    P_SAPP_PY3)/2.+YOFFSET Z=-(SFJP_SFFK_DZ/2+SFJP_SAPP_DZ)-SFJP_SFAM_D  634
C    Z/2. <===                                                            634
         CALL AGSRESET                                                    634
         AG_X = 0.                                                        634
         AG_Y = -SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJP_SAPP_PY3)/2.+YOFFSET   634
         AG_Z = -(SFJP_SFFK_DZ/2+SFJP_SAPP_DZ)-SFJP_SFAM_DZ/2.            634
         AG_TITLE = 'SFAM'                                                634
         AG_EXNAME = 'POSITION'                                           634
         AG_PARLIST = 'X_Y_Z'                                             634
         CALL AXPOSITION                                                  634
C    *                                                                    634
C    Geant POSITION : SFAM ===> X=0. Y=-SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJ  634
C    P_SAPP_PY3)/2.+YOFFSET Z=+(SFJP_SFFK_DZ/2+SFJP_SAPP_DZ)+SFJP_SFAM_D  634
C    Z/2. ALPHAY=180. <===                                                634
         CALL AGSRESET                                                    636
         AG_X = 0.                                                        636
         AG_Y = -SFJP_SAPP_PY1-(SFJP_SAPP_PY2+SFJP_SAPP_PY3)/2.+YOFFSET   636
         AG_Z = +(SFJP_SFFK_DZ/2+SFJP_SAPP_DZ)+SFJP_SFAM_DZ/2.            636
         AG_ALPHAN=AG_ALPHAN+1                                            636
         AG_ALPHAK(AG_ALPHAN)=2                                           636
         AG_ALPHAL(AG_ALPHAN)=180.                                        636
         AG_TITLE = 'SFAM'                                                636
         AG_EXNAME = 'POSITION'                                           636
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      636
         CALL AXPOSITION                                                  636
C    *                                                                    636
C    Create SFCO :                                                        637
C    Call To Local Procedure SFCO                                         637
         ASSIGN 5610 TO BLOCK_SFCO                                        637
         GO TO  5600                                                      637
5610     CONTINUE                                                         637
C    *                                                                    637
C    Geant POSITION : SFCO ===> X=-SFJP_SFCO_PX Y=SFJP_SFCO_PY Z=-(SFJP_  640
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFCO_DZ/2. ALPHAZ=+SFJP  640
C    _ALPHAZ <===                                                         640
         CALL AGSRESET                                                    640
         AG_X = -SFJP_SFCO_PX                                             640
         AG_Y = SFJP_SFCO_PY                                              640
         AG_Z = -(SFJP_SFFK_DZ/                                           640
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFCO_DZ/2.                   640
         AG_ALPHAN=AG_ALPHAN+1                                            640
         AG_ALPHAK(AG_ALPHAN)=3                                           640
         AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                                640
         AG_TITLE = 'SFCO'                                                640
         AG_EXNAME = 'POSITION'                                           640
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      640
         CALL AXPOSITION                                                  640
C    *                                                                    640
C    Geant POSITION : SFCO ===> X=+SFJP_SFCO_PX Y=SFJP_SFCO_PY Z=+(SFJP_  640
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFCO_DZ/2. ALPHAZ=-SFJP  640
C    _ALPHAZ <===                                                         640
         CALL AGSRESET                                                    643
         AG_X = +SFJP_SFCO_PX                                             643
         AG_Y = SFJP_SFCO_PY                                              643
         AG_Z = +(SFJP_SFFK_DZ/                                           643
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFCO_DZ/2.                   643
         AG_ALPHAN=AG_ALPHAN+1                                            643
         AG_ALPHAK(AG_ALPHAN)=3                                           643
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                                643
         AG_TITLE = 'SFCO'                                                643
         AG_EXNAME = 'POSITION'                                           643
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      643
         CALL AXPOSITION                                                  643
C    *                                                                    643
C    Create SFCM :                                                        644
C    Call To Local Procedure SFCM                                         644
         ASSIGN 5630 TO BLOCK_SFCM                                        644
         GO TO  5620                                                      644
5630     CONTINUE                                                         644
C    *                                                                    644
C    Geant POSITION : SFCM ===> X=+SFJP_SFCO_PX Y=SFJP_SFCO_PY Z=-(SFJP_  647
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFCO_DZ/2. ALPHAZ=-SFJP  647
C    _ALPHAZ <===                                                         647
         CALL AGSRESET                                                    647
         AG_X = +SFJP_SFCO_PX                                             647
         AG_Y = SFJP_SFCO_PY                                              647
         AG_Z = -(SFJP_SFFK_DZ/                                           647
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFCO_DZ/2.                   647
         AG_ALPHAN=AG_ALPHAN+1                                            647
         AG_ALPHAK(AG_ALPHAN)=3                                           647
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                                647
         AG_TITLE = 'SFCM'                                                647
         AG_EXNAME = 'POSITION'                                           647
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      647
         CALL AXPOSITION                                                  647
C    *                                                                    647
C    Geant POSITION : SFCM ===> X=-SFJP_SFCO_PX Y=SFJP_SFCO_PY Z=+(SFJP_  647
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFCO_DZ/2. ALPHAY=180 A  647
C    LPHAZ=+SFJP_ALPHAZ <===                                              647
         CALL AGSRESET                                                    650
         AG_X = -SFJP_SFCO_PX                                             650
         AG_Y = SFJP_SFCO_PY                                              650
         AG_Z = +(SFJP_SFFK_DZ/                                           650
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFCO_DZ/2.                   650
         AG_ALPHAN=AG_ALPHAN+1                                            650
         AG_ALPHAK(AG_ALPHAN)=2                                           650
         AG_ALPHAL(AG_ALPHAN)=180                                         650
         AG_ALPHAN=AG_ALPHAN+1                                            650
         AG_ALPHAK(AG_ALPHAN)=3                                           650
         AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                                650
         AG_TITLE = 'SFCM'                                                650
         AG_EXNAME = 'POSITION'                                           650
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               650
         CALL AXPOSITION                                                  650
C    *                                                                    650
C    Create SFKF :                                                        651
C    Call To Local Procedure SFKF                                         651
         ASSIGN 5650 TO BLOCK_SFKF                                        651
         GO TO  5640                                                      651
5650     CONTINUE                                                         651
C    *                                                                    651
C    Geant POSITION : SFKF ===> X=-SFJP_SFKF_PX Y=SFJP_SFKF_PY Z=-(SFJP_  654
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFKF_DZ/2-0.2 ALPHAZ=+S  654
C    FJP_ALPHAZ <===                                                      654
         CALL AGSRESET                                                    654
         AG_X = -SFJP_SFKF_PX                                             654
         AG_Y = SFJP_SFKF_PY                                              654
         AG_Z = -(SFJP_SFFK_DZ/                                           654
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFKF_DZ/2-0.2                654
         AG_ALPHAN=AG_ALPHAN+1                                            654
         AG_ALPHAK(AG_ALPHAN)=3                                           654
         AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                                654
         AG_TITLE = 'SFKF'                                                654
         AG_EXNAME = 'POSITION'                                           654
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      654
         CALL AXPOSITION                                                  654
C    *                                                                    654
C    Geant POSITION : SFKF ===> X=+SFJP_SFKF_PX Y=SFJP_SFKF_PY Z=+(SFJP_  654
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFKF_DZ/2+0.2 ALPHAZ=-S  654
C    FJP_ALPHAZ <===                                                      654
         CALL AGSRESET                                                    657
         AG_X = +SFJP_SFKF_PX                                             657
         AG_Y = SFJP_SFKF_PY                                              657
         AG_Z = +(SFJP_SFFK_DZ/                                           657
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFKF_DZ/2+0.2                657
         AG_ALPHAN=AG_ALPHAN+1                                            657
         AG_ALPHAK(AG_ALPHAN)=3                                           657
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                                657
         AG_TITLE = 'SFKF'                                                657
         AG_EXNAME = 'POSITION'                                           657
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      657
         CALL AXPOSITION                                                  657
C    *                                                                    657
C    Create SFKS :                                                        658
C    Call To Local Procedure SFKS                                         658
         ASSIGN 5670 TO BLOCK_SFKS                                        658
         GO TO  5660                                                      658
5670     CONTINUE                                                         658
C    *                                                                    658
C    Geant POSITION : SFKS ===> X=+SFJP_SFKS_PX Y=SFJP_SFKS_PY Z=-(SFJP_  661
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFKF_DZ/2-0.2 ALPHAZ=-S  661
C    FJP_ALPHAZ <===                                                      661
         CALL AGSRESET                                                    661
         AG_X = +SFJP_SFKS_PX                                             661
         AG_Y = SFJP_SFKS_PY                                              661
         AG_Z = -(SFJP_SFFK_DZ/                                           661
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))-SFJP_SFKF_DZ/2-0.2                661
         AG_ALPHAN=AG_ALPHAN+1                                            661
         AG_ALPHAK(AG_ALPHAN)=3                                           661
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                                661
         AG_TITLE = 'SFKS'                                                661
         AG_EXNAME = 'POSITION'                                           661
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      661
         CALL AXPOSITION                                                  661
C    *                                                                    661
C    Geant POSITION : SFKS ===> X=-SFJP_SFKS_PX Y=SFJP_SFKS_PY Z=+(SFJP_  661
C    SFFK_DZ/2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFKF_DZ/2+0.2 ALPHAZ=+S  661
C    FJP_ALPHAZ <===                                                      661
         CALL AGSRESET                                                    664
         AG_X = -SFJP_SFKS_PX                                             664
         AG_Y = SFJP_SFKS_PY                                              664
         AG_Z = +(SFJP_SFFK_DZ/                                           664
     *   2-(SFJP_SFKF_DZ-SFJP_SFCS_DZ))+SFJP_SFKF_DZ/2+0.2                664
         AG_ALPHAN=AG_ALPHAN+1                                            664
         AG_ALPHAK(AG_ALPHAN)=3                                           664
         AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                                664
         AG_TITLE = 'SFKS'                                                664
         AG_EXNAME = 'POSITION'                                           664
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      664
         CALL AXPOSITION                                                  664
C    *                                                                    664
C    Create SFPR :                                                        665
C    Call To Local Procedure SFPR                                         665
         ASSIGN 5690 TO BLOCK_SFPR                                        665
         GO TO  5680                                                      665
5690     CONTINUE                                                         665
C    *                                                                    665
C    Geant POSITION : SFPR ===> X=0 Y=+SFJP_SFPR_PY Z=-SFJP_SFLU_DZ <===  666
         CALL AGSRESET                                                    666
         AG_X = 0                                                         666
         AG_Y = +SFJP_SFPR_PY                                             666
         AG_Z = -SFJP_SFLU_DZ                                             666
         AG_TITLE = 'SFPR'                                                666
         AG_EXNAME = 'POSITION'                                           666
         AG_PARLIST = 'X_Y_Z'                                             666
         CALL AXPOSITION                                                  666
C    *                                                                    666
C    Geant POSITION : SFPR ===> X=0 Y=+SFJP_SFPR_PY Z=+SFJP_SFLU_DZ-3.7   666
C    <===                                                                 666
         CALL AGSRESET                                                    667
         AG_X = 0                                                         667
         AG_Y = +SFJP_SFPR_PY                                             667
         AG_Z = +SFJP_SFLU_DZ-3.7                                         667
         AG_TITLE = 'SFPR'                                                667
         AG_EXNAME = 'POSITION'                                           667
         AG_PARLIST = 'X_Y_Z'                                             667
         CALL AXPOSITION                                                  667
C    *                                                                    667
C    Create SFPB :                                                        668
C    Call To Local Procedure SFPB                                         668
         ASSIGN 5710 TO BLOCK_SFPB                                        668
         GO TO  5700                                                      668
5710     CONTINUE                                                         668
C    *                                                                    668
C    Geant POSITION : SFPB ===> X=0 Y=+SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SF  670
C    PB_PY2 Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ/2. <===                           670
         CALL AGSRESET                                                    670
         AG_X = 0                                                         670
         AG_Y = +SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SFPB_PY2                  670
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ/2.                              670
         AG_TITLE = 'SFPB'                                                670
         AG_EXNAME = 'POSITION'                                           670
         AG_PARLIST = 'X_Y_Z'                                             670
         CALL AXPOSITION                                                  670
C    *                                                                    670
C    Geant POSITION : SFPB ===> X=0 Y=+SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SF  670
C    PB_PY2 Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ/2. <===                           670
         CALL AGSRESET                                                    672
         AG_X = 0                                                         672
         AG_Y = +SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SFPB_PY2                  672
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ/2.                              672
         AG_TITLE = 'SFPB'                                                672
         AG_EXNAME = 'POSITION'                                           672
         AG_PARLIST = 'X_Y_Z'                                             672
         CALL AXPOSITION                                                  672
C    *                                                                    672
C    Create SSBS :                                                        673
C    Call To Local Procedure SSBS                                         673
         ASSIGN 5730 TO BLOCK_SSBS                                        673
         GO TO  5720                                                      673
5730     CONTINUE                                                         673
C    *                                                                    673
C    Geant POSITION : SSBS ===> X=0. Y=+SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_S  676
C    FPB_PY2-0.8/2.-SFJP_SSBS_DY/2. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-0.2+SFJP  676
C    _SSBB_DZ+SFJP_SSBS_DX/2. <===                                        676
         CALL AGSRESET                                                    676
         AG_X = 0.                                                        676
         AG_Y = +SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SFPB_PY2-0.8/             676
     *   2.-SFJP_SSBS_DY/2.                                               676
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-0.2+SFJP_SSBB_DZ+SFJP_SSBS_DX/  676
     *   2.                                                               676
         AG_TITLE = 'SSBS'                                                676
         AG_EXNAME = 'POSITION'                                           676
         AG_PARLIST = 'X_Y_Z'                                             676
         CALL AXPOSITION                                                  676
C    *                                                                    676
C    Geant POSITION : SSBS ===> X=0. Y=+SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_S  676
C    FPB_PY2-0.8/2.-SFJP_SSBS_DY/2. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+0.2-SFJP  676
C    _SSBB_DZ-SFJP_SSBS_DX/2. <===                                        676
         CALL AGSRESET                                                    679
         AG_X = 0.                                                        679
         AG_Y = +SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SFPB_PY2-0.8/             679
     *   2.-SFJP_SSBS_DY/2.                                               679
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+0.2-SFJP_SSBB_DZ-SFJP_SSBS_DX/  679
     *   2.                                                               679
         AG_TITLE = 'SSBS'                                                679
         AG_EXNAME = 'POSITION'                                           679
         AG_PARLIST = 'X_Y_Z'                                             679
         CALL AXPOSITION                                                  679
C    *                                                                    679
C    Create SSBB :                                                        680
C    Call To Local Procedure SSBB                                         680
         ASSIGN 5750 TO BLOCK_SSBB                                        680
         GO TO  5740                                                      680
5750     CONTINUE                                                         680
C    *                                                                    680
C    Geant POSITION : SSBB ===> X=0. Y=+SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_S  683
C    FPB_PY2-0.8/2.-SFJP_SSBS_DY/2. Z=-SFJP_SFLU_DZ-SFJP_SFPBDZ-0.2+SFJP  683
C    _SSBB_DZ/2. <===                                                     683
         CALL AGSRESET                                                    683
         AG_X = 0.                                                        683
         AG_Y = +SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SFPB_PY2-0.8/             683
     *   2.-SFJP_SSBS_DY/2.                                               683
         AG_Z = -SFJP_SFLU_DZ-SFJP_SFPBDZ-0.2+SFJP_SSBB_DZ/2.             683
         AG_TITLE = 'SSBB'                                                683
         AG_EXNAME = 'POSITION'                                           683
         AG_PARLIST = 'X_Y_Z'                                             683
         CALL AXPOSITION                                                  683
C    *                                                                    683
C    Geant POSITION : SSBB ===> X=0. Y=+SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_S  683
C    FPB_PY2-0.8/2.-SFJP_SSBS_DY/2. Z=+SFJP_SFLU_DZ+SFJP_SFPBDZ+0.2-SFJP  683
C    _SSBB_DZ/2. <===                                                     683
         CALL AGSRESET                                                    686
         AG_X = 0.                                                        686
         AG_Y = +SFJP_SFPR_PY-SFJP_SFPB_PY+SFJP_SFPB_PY2-0.8/             686
     *   2.-SFJP_SSBS_DY/2.                                               686
         AG_Z = +SFJP_SFLU_DZ+SFJP_SFPBDZ+0.2-SFJP_SSBB_DZ/2.             686
         AG_TITLE = 'SSBB'                                                686
         AG_EXNAME = 'POSITION'                                           686
         AG_PARLIST = 'X_Y_Z'                                             686
         CALL AXPOSITION                                                  686
C    *                                                                    686
C    Create SFLA :                                                        687
C    Call To Local Procedure SFLA                                         687
         ASSIGN 5770 TO BLOCK_SFLA                                        687
         GO TO  5760                                                      687
5770     CONTINUE                                                         687
C    *                                                                    687
C    Create SFLB :                                                        688
C    Call To Local Procedure SFLB                                         688
         ASSIGN 5790 TO BLOCK_SFLB                                        688
         GO TO  5780                                                      688
5790     CONTINUE                                                         688
C    *                                                                    688
C    Create SFLC :                                                        689
C    Call To Local Procedure SFLC                                         689
         ASSIGN 5810 TO BLOCK_SFLC                                        689
         GO TO  5800                                                      689
5810     CONTINUE                                                         689
C    *                                                                    689
C    Create SFEB :                                                        690
C    Call To Local Procedure SFEB                                         690
         ASSIGN 5830 TO BLOCK_SFEB                                        690
         GO TO  5820                                                      690
5830     CONTINUE                                                         690
C    *                                                                    690
C    Create SFES :                                                        691
C    Call To Local Procedure SFES                                         691
         ASSIGN 5850 TO BLOCK_SFES                                        691
         GO TO  5840                                                      691
5850     CONTINUE                                                         691
C    *                                                                    691
         WAFPCKLEN=SFPA_WPLEN/(SFPA_NSSD*1.)                              692
C    *                                                                    693
         IWAF=1                                                           693
            GO TO 5863                                                    693
5861        IWAF=IWAF+1                                                   693
5863        IF(IWAF-(8).GT.0) GO TO 5862                                  693
C       *                                                                 694
            JWAF=1                                                        694
               GO TO 5873                                                 694
5871           JWAF=JWAF+1                                                694
5873           IF(JWAF-(IWAF).GT.0) GO TO 5872                            694
C          Geant POSITION : SFLA ===> X=+SFJP_SFLA_PX+SFJP_FLEX_DI*JWAF   697
C          Y=SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF Z=-(SFPA_WPLEN+WAFPCKLEN)/2+  697
C          IWAF*WAFPCKLEN ALPHAZ=-SFJP_ALPHAZ <===                        697
               CALL AGSRESET                                              697
               AG_X = +SFJP_SFLA_PX+SFJP_FLEX_DI*JWAF                     697
               AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF                      697
               AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+IWAF*WAFPCKLEN            697
               AG_ALPHAN=AG_ALPHAN+1                                      697
               AG_ALPHAK(AG_ALPHAN)=3                                     697
               AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                          697
               AG_TITLE = 'SFLA'                                          697
               AG_EXNAME = 'POSITION'                                     697
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                697
               CALL AXPOSITION                                            697
C          *                                                              697
C          Geant POSITION : SFLA ===> X=-SFJP_SFLA_PX-SFJP_FLEX_DI*JWAF   697
C          Y=SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF Z=+(SFPA_WPLEN+WAFPCKLEN)/2-  697
C          IWAF*WAFPCKLEN ALPHAZ=+SFJP_ALPHAZ <===                        697
               CALL AGSRESET                                              700
               AG_X = -SFJP_SFLA_PX-SFJP_FLEX_DI*JWAF                     700
               AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF                      700
               AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-IWAF*WAFPCKLEN            700
               AG_ALPHAN=AG_ALPHAN+1                                      700
               AG_ALPHAK(AG_ALPHAN)=3                                     700
               AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                          700
               AG_TITLE = 'SFLA'                                          700
               AG_EXNAME = 'POSITION'                                     700
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                700
               CALL AXPOSITION                                            700
C          *                                                              700
C          Geant POSITION : SFLC ===> X=+SFJP_SFLA_PX+SFJP_FLEX_DI*JWAF   700
C          Y=SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF Z=-(SFPA_WPLEN+WAFPCKLEN)/2.  700
C          +16*WAFPCKLEN+WAFPCKLEN/2.+SFJP_SFLB_DZ+8.5*SFJP_SFLC_DZ-IWAF  700
C          *SFJP_SFLC_DZ ALPHAZ=-SFJP_ALPHAZ <===                         700
               CALL AGSRESET                                              704
               AG_X = +SFJP_SFLA_PX+SFJP_FLEX_DI*JWAF                     704
               AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF                      704
               AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2.+16*WAFPCKLEN+WAFPCKLEN/  704
     *         2.+SFJP_SFLB_DZ+8.5*SFJP_SFLC_DZ-IWAF*SFJP_SFLC_DZ         704
               AG_ALPHAN=AG_ALPHAN+1                                      704
               AG_ALPHAK(AG_ALPHAN)=3                                     704
               AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                          704
               AG_TITLE = 'SFLC'                                          704
               AG_EXNAME = 'POSITION'                                     704
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                704
               CALL AXPOSITION                                            704
C          *                                                              704
C          Geant POSITION : SFLC ===> X=-SFJP_SFLA_PX-SFJP_FLEX_DI*JWAF   704
C          Y=SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF Z=+(SFPA_WPLEN+WAFPCKLEN)/2.  704
C          -16*WAFPCKLEN-WAFPCKLEN/2.-SFJP_SFLB_DZ-8.5*SFJP_SFLC_DZ+IWAF  704
C          *SFJP_SFLC_DZ ALPHAZ=+SFJP_ALPHAZ <===                         704
               CALL AGSRESET                                              708
               AG_X = -SFJP_SFLA_PX-SFJP_FLEX_DI*JWAF                     708
               AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF                      708
               AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2.-16*WAFPCKLEN-WAFPCKLEN/  708
     *         2.-SFJP_SFLB_DZ-8.5*SFJP_SFLC_DZ+IWAF*SFJP_SFLC_DZ         708
               AG_ALPHAN=AG_ALPHAN+1                                      708
               AG_ALPHAK(AG_ALPHAN)=3                                     708
               AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                          708
               AG_TITLE = 'SFLC'                                          708
               AG_EXNAME = 'POSITION'                                     708
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                708
               CALL AXPOSITION                                            708
C          *                                                              708
C          Geant POSITION : SFLA ===> X=+SFJP_SFLC_PX+SFJP_FLEX_DI*JWAF   708
C          Y=-SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF Z=-(SFPA_WPLEN+WAFPCKLEN)/2  708
C          +(IWAF+8)*WAFPCKLEN ALPHAZ=-SFJP_ALPHAZ <===                   708
               CALL AGSRESET                                              711
               AG_X = +SFJP_SFLC_PX+SFJP_FLEX_DI*JWAF                     711
               AG_Y = -SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF                     711
               AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+(IWAF+8)*WAFPCKLEN        711
               AG_ALPHAN=AG_ALPHAN+1                                      711
               AG_ALPHAK(AG_ALPHAN)=3                                     711
               AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                          711
               AG_TITLE = 'SFLA'                                          711
               AG_EXNAME = 'POSITION'                                     711
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                711
               CALL AXPOSITION                                            711
C          *                                                              711
C          Geant POSITION : SFLA ===> X=-SFJP_SFLC_PX-SFJP_FLEX_DI*JWAF   711
C          Y=-SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF Z=+(SFPA_WPLEN+WAFPCKLEN)/2  711
C          -(IWAF+8)*WAFPCKLEN ALPHAZ=+SFJP_ALPHAZ <===                   711
               CALL AGSRESET                                              714
               AG_X = -SFJP_SFLC_PX-SFJP_FLEX_DI*JWAF                     714
               AG_Y = -SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF                     714
               AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-(IWAF+8)*WAFPCKLEN        714
               AG_ALPHAN=AG_ALPHAN+1                                      714
               AG_ALPHAK(AG_ALPHAN)=3                                     714
               AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                          714
               AG_TITLE = 'SFLA'                                          714
               AG_EXNAME = 'POSITION'                                     714
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                714
               CALL AXPOSITION                                            714
C          *                                                              714
C          Geant POSITION : SFLC ===> X=+SFJP_SFLC_PX+SFJP_FLEX_DI*JWAF   714
C          Y=-SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF Z=-(SFPA_WPLEN+WAFPCKLEN)/2  714
C          +16*WAFPCKLEN+WAFPCKLEN/2.+8.5*SFJP_SFLC_DZ-IWAF*SFJP_SFLC_DZ  714
C           ALPHAZ=-SFJP_ALPHAZ <===                                      714
               CALL AGSRESET                                              718
               AG_X = +SFJP_SFLC_PX+SFJP_FLEX_DI*JWAF                     718
               AG_Y = -SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF                     718
               AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+16*WAFPCKLEN+WAFPCKLEN/   718
     *         2.+8.5*SFJP_SFLC_DZ-IWAF*SFJP_SFLC_DZ                      718
               AG_ALPHAN=AG_ALPHAN+1                                      718
               AG_ALPHAK(AG_ALPHAN)=3                                     718
               AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                          718
               AG_TITLE = 'SFLC'                                          718
               AG_EXNAME = 'POSITION'                                     718
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                718
               CALL AXPOSITION                                            718
C          *                                                              718
C          Geant POSITION : SFLC ===> X=-SFJP_SFLC_PX-SFJP_FLEX_DI*JWAF   718
C          Y=-SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF Z=+(SFPA_WPLEN+WAFPCKLEN)/2  718
C          -16*WAFPCKLEN-WAFPCKLEN/2.-8.5*SFJP_SFLC_DZ+IWAF*SFJP_SFLC_DZ  718
C           ALPHAZ=+SFJP_ALPHAZ <===                                      718
               CALL AGSRESET                                              722
               AG_X = -SFJP_SFLC_PX-SFJP_FLEX_DI*JWAF                     722
               AG_Y = -SFJP_SFLC_PY+SFJP_FLEX_DI*JWAF                     722
               AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-16*WAFPCKLEN-WAFPCKLEN/   722
     *         2.-8.5*SFJP_SFLC_DZ+IWAF*SFJP_SFLC_DZ                      722
               AG_ALPHAN=AG_ALPHAN+1                                      722
               AG_ALPHAK(AG_ALPHAN)=3                                     722
               AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                          722
               AG_TITLE = 'SFLC'                                          722
               AG_EXNAME = 'POSITION'                                     722
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                722
               CALL AXPOSITION                                            722
C          *                                                              722
            GO TO 5871                                                    723
5872        CONTINUE                                                      723
C       *                                                                 724
            JWAF=1                                                        724
               GO TO 5883                                                 724
5881           JWAF=JWAF+1                                                724
5883           IF(JWAF-(8).GT.0) GO TO 5882                               724
C          Geant POSITION : SFLA ===> X=+SFJP_SFLA_PX+SFJP_FLEX_DI*JWAF   727
C          Y=SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF Z=-(SFPA_WPLEN+WAFPCKLEN)/2+  727
C          (IWAF+8)*WAFPCKLEN ALPHAZ=-SFJP_ALPHAZ <===                    727
               CALL AGSRESET                                              727
               AG_X = +SFJP_SFLA_PX+SFJP_FLEX_DI*JWAF                     727
               AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF                      727
               AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+(IWAF+8)*WAFPCKLEN        727
               AG_ALPHAN=AG_ALPHAN+1                                      727
               AG_ALPHAK(AG_ALPHAN)=3                                     727
               AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                          727
               AG_TITLE = 'SFLA'                                          727
               AG_EXNAME = 'POSITION'                                     727
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                727
               CALL AXPOSITION                                            727
C          *                                                              727
C          Geant POSITION : SFLA ===> X=-SFJP_SFLA_PX-SFJP_FLEX_DI*JWAF   727
C          Y=SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF Z=+(SFPA_WPLEN+WAFPCKLEN)/2-  727
C          (IWAF+8)*WAFPCKLEN ALPHAZ=+SFJP_ALPHAZ <===                    727
               CALL AGSRESET                                              730
               AG_X = -SFJP_SFLA_PX-SFJP_FLEX_DI*JWAF                     730
               AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*JWAF                      730
               AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-(IWAF+8)*WAFPCKLEN        730
               AG_ALPHAN=AG_ALPHAN+1                                      730
               AG_ALPHAK(AG_ALPHAN)=3                                     730
               AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                          730
               AG_TITLE = 'SFLA'                                          730
               AG_EXNAME = 'POSITION'                                     730
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                730
               CALL AXPOSITION                                            730
C          *                                                              730
            GO TO 5881                                                    731
5882        CONTINUE                                                      731
C       Geant POSITION : SFLB ===> X=+SFJP_SFLA_PX+SFJP_FLEX_DI*IWAF Y=S  735
C       FJP_SFLA_PY+SFJP_FLEX_DI*IWAF Z=-(SFPA_WPLEN+WAFPCKLEN)/2+16*WAF  735
C       PCKLEN+WAFPCKLEN/2.+SFJP_SFLB_DZ/2. ALPHAZ=-SFJP_ALPHAZ <===      735
            CALL AGSRESET                                                 735
            AG_X = +SFJP_SFLA_PX+SFJP_FLEX_DI*IWAF                        735
            AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*IWAF                         735
            AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+16*WAFPCKLEN+WAFPCKLEN/      735
     *      2.+SFJP_SFLB_DZ/2.                                            735
            AG_ALPHAN=AG_ALPHAN+1                                         735
            AG_ALPHAK(AG_ALPHAN)=3                                        735
            AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZ                             735
            AG_TITLE = 'SFLB'                                             735
            AG_EXNAME = 'POSITION'                                        735
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   735
            CALL AXPOSITION                                               735
C       *                                                                 735
C       Geant POSITION : SFLB ===> X=-SFJP_SFLA_PX-SFJP_FLEX_DI*IWAF Y=S  735
C       FJP_SFLA_PY+SFJP_FLEX_DI*IWAF Z=+(SFPA_WPLEN+WAFPCKLEN)/2-16*WAF  735
C       PCKLEN-WAFPCKLEN/2.-SFJP_SFLB_DZ/2. ALPHAZ=+SFJP_ALPHAZ <===      735
            CALL AGSRESET                                                 739
            AG_X = -SFJP_SFLA_PX-SFJP_FLEX_DI*IWAF                        739
            AG_Y = SFJP_SFLA_PY+SFJP_FLEX_DI*IWAF                         739
            AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-16*WAFPCKLEN-WAFPCKLEN/      739
     *      2.-SFJP_SFLB_DZ/2.                                            739
            AG_ALPHAN=AG_ALPHAN+1                                         739
            AG_ALPHAK(AG_ALPHAN)=3                                        739
            AG_ALPHAL(AG_ALPHAN)=+SFJP_ALPHAZ                             739
            AG_TITLE = 'SFLB'                                             739
            AG_EXNAME = 'POSITION'                                        739
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   739
            CALL AXPOSITION                                               739
C       *                                                                 739
C       Geant POSITION : SFES ===> X=-SFJP_SFES_PX Y=-SFJP_SFES_PY Z=-(S  739
C       FPA_WPLEN+WAFPCKLEN)/2+(IWAF)*WAFPCKLEN+WAFPCKLEN/2.-SFJP_SFEB_D  739
C       Z/2.-SFJP_SFES_PZ/2. ALPHAZ=57.66 <===                            739
            CALL AGSRESET                                                 742
            AG_X = -SFJP_SFES_PX                                          742
            AG_Y = -SFJP_SFES_PY                                          742
            AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+(IWAF)*WAFPCKLEN+WAFPCKLEN/  742
     *      2.-SFJP_SFEB_DZ/2.-SFJP_SFES_PZ/2.                            742
            AG_ALPHAN=AG_ALPHAN+1                                         742
            AG_ALPHAK(AG_ALPHAN)=3                                        742
            AG_ALPHAL(AG_ALPHAN)=57.66                                    742
            AG_TITLE = 'SFES'                                             742
            AG_EXNAME = 'POSITION'                                        742
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   742
            CALL AXPOSITION                                               742
C       *                                                                 742
C       Geant POSITION : SFES ===> X=+SFJP_SFES_PX Y=-SFJP_SFES_PY Z=+(S  742
C       FPA_WPLEN+WAFPCKLEN)/2-(IWAF)*WAFPCKLEN-WAFPCKLEN/2.+SFJP_SFEB_D  742
C       Z/2.+SFJP_SFES_PZ/2. ALPHAZ=-57.66 <===                           742
            CALL AGSRESET                                                 745
            AG_X = +SFJP_SFES_PX                                          745
            AG_Y = -SFJP_SFES_PY                                          745
            AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-(IWAF)*WAFPCKLEN-WAFPCKLEN/  745
     *      2.+SFJP_SFEB_DZ/2.+SFJP_SFES_PZ/2.                            745
            AG_ALPHAN=AG_ALPHAN+1                                         745
            AG_ALPHAK(AG_ALPHAN)=3                                        745
            AG_ALPHAL(AG_ALPHAN)=-57.66                                   745
            AG_TITLE = 'SFES'                                             745
            AG_EXNAME = 'POSITION'                                        745
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   745
            CALL AXPOSITION                                               745
C       *                                                                 745
C       Geant POSITION : SFEB ===> X=-SFJP_SFEB_PX Y=-SFJP_SFEB_PY Z=-(S  745
C       FPA_WPLEN+WAFPCKLEN)/2+(IWAF+8)*WAFPCKLEN+WAFPCKLEN/2.-SFJP_SFEB  745
C       _DZ/2.-SFJP_SFES_PZ/2. ALPHAZ=55.35 <===                          745
            CALL AGSRESET                                                 748
            AG_X = -SFJP_SFEB_PX                                          748
            AG_Y = -SFJP_SFEB_PY                                          748
            AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/                               748
     *      2+(IWAF+8)*WAFPCKLEN+WAFPCKLEN/2.-SFJP_SFEB_DZ/               748
     *      2.-SFJP_SFES_PZ/2.                                            748
            AG_ALPHAN=AG_ALPHAN+1                                         748
            AG_ALPHAK(AG_ALPHAN)=3                                        748
            AG_ALPHAL(AG_ALPHAN)=55.35                                    748
            AG_TITLE = 'SFEB'                                             748
            AG_EXNAME = 'POSITION'                                        748
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   748
            CALL AXPOSITION                                               748
C       *                                                                 748
C       Geant POSITION : SFEB ===> X=+SFJP_SFEB_PX Y=-SFJP_SFEB_PY Z=+(S  748
C       FPA_WPLEN+WAFPCKLEN)/2-(IWAF+8)*WAFPCKLEN-WAFPCKLEN/2.+SFJP_SFEB  748
C       _DZ/2.+SFJP_SFES_PZ/2. ALPHAZ=-55.35 <===                         748
            CALL AGSRESET                                                 751
            AG_X = +SFJP_SFEB_PX                                          751
            AG_Y = -SFJP_SFEB_PY                                          751
            AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/                               751
     *      2-(IWAF+8)*WAFPCKLEN-WAFPCKLEN/2.+SFJP_SFEB_DZ/               751
     *      2.+SFJP_SFES_PZ/2.                                            751
            AG_ALPHAN=AG_ALPHAN+1                                         751
            AG_ALPHAK(AG_ALPHAN)=3                                        751
            AG_ALPHAL(AG_ALPHAN)=-55.35                                   751
            AG_TITLE = 'SFEB'                                             751
            AG_EXNAME = 'POSITION'                                        751
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   751
            CALL AXPOSITION                                               751
C       *                                                                 751
         GO TO 5861                                                       752
5862     CONTINUE                                                         752
5251  CALL AGSPOPB                                                        753
      GO TO BLOCK_SFSM, (5250 ,5260)                                      753
C End of Local Procedure                                                  753
C ----------------------------------------------------------------------  754
C ------------ SFLT                                                       754
C Begin Local Procedure SFLT                                              755
C SFLT IS (HALF) THE TOP CORNER OF THE TRIANGULAR LADDER SKELETON         755
5440  CONTINUE                                                            755
         AG_TITLE='SFLT'                                                  755
         AG_MODULE=MODULE                                                 755
         AG_MARK='V00'                                                    755
         CALL AGSPUSH                                                     755
         AG_MODULE=MODULE                                                 755
C    *                                                                    755
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           756
         CALL AGSRESET                                                    756
         AG_TITLE = 'CARBON'                                              756
         AG_EXNAME = 'MATERIAL'                                           756
         AG_PARLIST = 'NONE'                                              756
         CALL AXMATERIAL                                                  756
C    *                                                                    756
C    Geant ATTRIBUTE : SFLT ===> SEEN=1 COLO=1 <===                       756
         CALL AGSRESET                                                    757
         AG_SEEN = 1                                                      757
         AG_COLO = 1                                                      757
         AG_TITLE = 'SFLT'                                                757
         AG_EXNAME = 'ATTRIBUTE'                                          757
         AG_PARLIST = 'SEEN_COLO'                                         757
         CALL AXATTRIBUTE                                                 757
C    *                                                                    757
C    Geant SHAPE : TRAP ===> DZ=SFJP_SFLU_DZ THET=0 PHI=0, H1=SFJP_SFLU_  757
C    H1 BL1=SFJP_SFLU_BL1 TL1=SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(PI/5), ALP1  757
C    =(PI/2-ATAN(2*TAN(PI/5)))*360/(2*PI), H2=SFJP_SFLU_H1 BL2=SFJP_SFLU  757
C    _BL1, TL2=SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(PI/5), ALP2=(PI/2-ATAN(2*T  757
C    AN(PI/5)))*360/(2*PI) <===                                           757
         CALL AGSRESET                                                    763
         AG_DZ = SFJP_SFLU_DZ                                             763
         AG_THET = 0                                                      763
         AG_PHI = 0                                                       763
         AG_H1 = SFJP_SFLU_H1                                             763
         AG_BL1 = SFJP_SFLU_BL1                                           763
         AG_TL1 = SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(PI/5)                    763
         AG_ALP1 = (PI/2-ATAN(2*TAN(PI/5)))*360/(2*PI)                    763
         AG_H2 = SFJP_SFLU_H1                                             763
         AG_BL2 = SFJP_SFLU_BL1                                           763
         AG_TL2 = SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(PI/5)                    763
         AG_ALP2 = (PI/2-ATAN(2*TAN(PI/5)))*360/(2*PI)                    763
         AG_TITLE = 'TRAP'                                                763
         AG_EXNAME = 'SHAPE'                                              763
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       763
         CALL AXSHAPE                                                     763
C    *                                                                    763
5441  CALL AGSPOPB                                                        764
      GO TO BLOCK_SFLT, (5440 ,5450)                                      764
C End of Local Procedure                                                  764
C ----------------------------------------------------------------------  765
C ------------ SFLU                                                       765
C Begin Local Procedure SFLU                                              766
C SFLU IS (HALF) THE SIDE CORNER OF THE TRIANGULAR LADDER SKELETON        766
5460  CONTINUE                                                            766
         AG_TITLE='SFLU'                                                  766
         AG_MODULE=MODULE                                                 766
         AG_MARK='V00'                                                    766
         CALL AGSPUSH                                                     766
         AG_MODULE=MODULE                                                 766
C    *                                                                    766
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           767
         CALL AGSRESET                                                    767
         AG_TITLE = 'CARBON'                                              767
         AG_EXNAME = 'MATERIAL'                                           767
         AG_PARLIST = 'NONE'                                              767
         CALL AXMATERIAL                                                  767
C    *                                                                    767
C    Geant ATTRIBUTE : SFLU ===> SEEN=1 COLO=1 <===                       767
         CALL AGSRESET                                                    768
         AG_SEEN = 1                                                      768
         AG_COLO = 1                                                      768
         AG_TITLE = 'SFLU'                                                768
         AG_EXNAME = 'ATTRIBUTE'                                          768
         AG_PARLIST = 'SEEN_COLO'                                         768
         CALL AXATTRIBUTE                                                 768
C    *                                                                    768
C    Geant SHAPE : TRAP ===> DZ=SFJP_SFLU_DZ THET=0 PHI=0, H1=SFJP_SFLU_  768
C    H1 BL1=SFJP_SFLU_BL1 TL1=SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(27*2*PI/360  768
C    ), ALP1=(PI/2-ATAN(2*TAN(27*2*PI/360)))*360/(2*PI), H2=SFJP_SFLU_H1  768
C    , BL2=SFJP_SFLU_BL1 TL2=SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(27*2*PI/360)  768
C    , ALP2=(PI/2-ATAN(2*TAN(27*2*PI/360)))*360/(2*PI) <===               768
         CALL AGSRESET                                                    774
         AG_DZ = SFJP_SFLU_DZ                                             774
         AG_THET = 0                                                      774
         AG_PHI = 0                                                       774
         AG_H1 = SFJP_SFLU_H1                                             774
         AG_BL1 = SFJP_SFLU_BL1                                           774
         AG_TL1 = SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(27*2*PI/360)             774
         AG_ALP1 = (PI/2-ATAN(2*TAN(27*2*PI/360)))*360/(2*PI)             774
         AG_H2 = SFJP_SFLU_H1                                             774
         AG_BL2 = SFJP_SFLU_BL1                                           774
         AG_TL2 = SFJP_SFLU_BL1-SFJP_SFLU_H1/TAN(27*2*PI/360)             774
         AG_ALP2 = (PI/2-ATAN(2*TAN(27*2*PI/360)))*360/(2*PI)             774
         AG_TITLE = 'TRAP'                                                774
         AG_EXNAME = 'SHAPE'                                              774
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       774
         CALL AXSHAPE                                                     774
C    *                                                                    774
5461  CALL AGSPOPB                                                        775
      GO TO BLOCK_SFLU, (5460 ,5470)                                      775
C End of Local Procedure                                                  775
C ----------------------------------------------------------------------  776
C ------------ SFFK                                                       776
C Begin Local Procedure SFFK                                              777
C SFFK HORIZONTAL PART OF THE LADDER SKELETON CARBON BASE                 777
5480  CONTINUE                                                            777
         AG_TITLE='SFFK'                                                  777
         AG_MODULE=MODULE                                                 777
         AG_MARK='V00'                                                    777
         CALL AGSPUSH                                                     777
         AG_MODULE=MODULE                                                 777
C    *                                                                    777
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           778
         CALL AGSRESET                                                    778
         AG_TITLE = 'CARBON'                                              778
         AG_EXNAME = 'MATERIAL'                                           778
         AG_PARLIST = 'NONE'                                              778
         CALL AXMATERIAL                                                  778
C    *                                                                    778
C    Geant ATTRIBUTE : SFFK ===> SEEN=1 COLO=1 <===                       778
         CALL AGSRESET                                                    779
         AG_SEEN = 1                                                      779
         AG_COLO = 1                                                      779
         AG_TITLE = 'SFFK'                                                779
         AG_EXNAME = 'ATTRIBUTE'                                          779
         AG_PARLIST = 'SEEN_COLO'                                         779
         CALL AXATTRIBUTE                                                 779
C    *                                                                    779
C    Geant SHAPE : BOX ===> DX=SFJP_SFFK_DXE DY=SFJP_SFFK_DY, DZ=SFJP_SF  779
C    FK_DZ/2 <===                                                         779
         CALL AGSRESET                                                    781
         AG_DX = SFJP_SFFK_DXE                                            781
         AG_DY = SFJP_SFFK_DY                                             781
         AG_DZ = SFJP_SFFK_DZ/2                                           781
         AG_TITLE = 'BOX'                                                 781
         AG_EXNAME = 'SHAPE'                                              781
         AG_PARLIST = 'DX_DY_DZ'                                          781
         CALL AXSHAPE                                                     781
C    *                                                                    781
5481  CALL AGSPOPB                                                        782
      GO TO BLOCK_SFFK, (5480 ,5490)                                      782
C End of Local Procedure                                                  782
C ----------------------------------------------------------------------  783
C ------------ SFFL                                                       783
C Begin Local Procedure SFFL                                              784
C SFFL TITLED PART OF THE LADDER SKELETON CARBON BASE                     784
5500  CONTINUE                                                            784
         AG_TITLE='SFFL'                                                  784
         AG_MODULE=MODULE                                                 784
         AG_MARK='V00'                                                    784
         CALL AGSPUSH                                                     784
         AG_MODULE=MODULE                                                 784
C    *                                                                    784
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           785
         CALL AGSRESET                                                    785
         AG_TITLE = 'CARBON'                                              785
         AG_EXNAME = 'MATERIAL'                                           785
         AG_PARLIST = 'NONE'                                              785
         CALL AXMATERIAL                                                  785
C    *                                                                    785
C    Geant ATTRIBUTE : SFFL ===> SEEN=1 COLO=1 <===                       785
         CALL AGSRESET                                                    786
         AG_SEEN = 1                                                      786
         AG_COLO = 1                                                      786
         AG_TITLE = 'SFFL'                                                786
         AG_EXNAME = 'ATTRIBUTE'                                          786
         AG_PARLIST = 'SEEN_COLO'                                         786
         CALL AXATTRIBUTE                                                 786
C    *                                                                    786
C    Geant SHAPE : BOX ===> DX=SFJP_SFFL_DX/COS(15*PI/180), DY=SFJP_SFFK  786
C    _DY DZ=SFJP_SFFK_DZ/2 <===                                           786
         CALL AGSRESET                                                    788
         AG_DX = SFJP_SFFL_DX/COS(15*PI/180)                              788
         AG_DY = SFJP_SFFK_DY                                             788
         AG_DZ = SFJP_SFFK_DZ/2                                           788
         AG_TITLE = 'BOX'                                                 788
         AG_EXNAME = 'SHAPE'                                              788
         AG_PARLIST = 'DX_DY_DZ'                                          788
         CALL AXSHAPE                                                     788
C    *                                                                    788
5501  CALL AGSPOPB                                                        789
      GO TO BLOCK_SFFL, (5500 ,5510)                                      789
C End of Local Procedure                                                  789
C ----------------------------------------------------------------------  790
C ------------ SFKK                                                       790
C Begin Local Procedure SFKK                                              791
C SFKK HORIZONTAL PART OF THE KAPTON FILM UNDER THE LADDER BASE           791
5520  CONTINUE                                                            791
         AG_TITLE='SFKK'                                                  791
         AG_MODULE=MODULE                                                 791
         AG_MARK='V00'                                                    791
         CALL AGSPUSH                                                     791
         AG_MODULE=MODULE                                                 791
C    *                                                                    791
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            792
         CALL AGSRESET                                                    792
         AG_TITLE = 'MYLAR'                                               792
         AG_EXNAME = 'MATERIAL'                                           792
         AG_PARLIST = 'NONE'                                              792
         CALL AXMATERIAL                                                  792
C    *                                                                    792
C    Geant ATTRIBUTE : SFKK ===> SEEN=1 COLO=1 <===                       792
         CALL AGSRESET                                                    793
         AG_SEEN = 1                                                      793
         AG_COLO = 1                                                      793
         AG_TITLE = 'SFKK'                                                793
         AG_EXNAME = 'ATTRIBUTE'                                          793
         AG_PARLIST = 'SEEN_COLO'                                         793
         CALL AXATTRIBUTE                                                 793
C    *                                                                    793
C    Geant SHAPE : BOX ===> DX=SFJP_SFFK_DXZ/2. DY=SFJP_SFKK_DY/2., DZ=S  793
C    FJP_SFFK_DZ/2 <===                                                   793
         CALL AGSRESET                                                    795
         AG_DX = SFJP_SFFK_DXZ/2.                                         795
         AG_DY = SFJP_SFKK_DY/2.                                          795
         AG_DZ = SFJP_SFFK_DZ/2                                           795
         AG_TITLE = 'BOX'                                                 795
         AG_EXNAME = 'SHAPE'                                              795
         AG_PARLIST = 'DX_DY_DZ'                                          795
         CALL AXSHAPE                                                     795
C    *                                                                    795
5521  CALL AGSPOPB                                                        796
      GO TO BLOCK_SFKK, (5520 ,5530)                                      796
C End of Local Procedure                                                  796
C ----------------------------------------------------------------------  797
C ------------ SFKL                                                       797
C Begin Local Procedure SFKL                                              798
C SFKL TITLED PART OF THE KPATON FILM UNDER THE LADDER BASE               798
5540  CONTINUE                                                            798
         AG_TITLE='SFKL'                                                  798
         AG_MODULE=MODULE                                                 798
         AG_MARK='V00'                                                    798
         CALL AGSPUSH                                                     798
         AG_MODULE=MODULE                                                 798
C    *                                                                    798
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            799
         CALL AGSRESET                                                    799
         AG_TITLE = 'MYLAR'                                               799
         AG_EXNAME = 'MATERIAL'                                           799
         AG_PARLIST = 'NONE'                                              799
         CALL AXMATERIAL                                                  799
C    *                                                                    799
C    Geant ATTRIBUTE : SFKL ===> SEEN=1 COLO=1 <===                       799
         CALL AGSRESET                                                    800
         AG_SEEN = 1                                                      800
         AG_COLO = 1                                                      800
         AG_TITLE = 'SFKL'                                                800
         AG_EXNAME = 'ATTRIBUTE'                                          800
         AG_PARLIST = 'SEEN_COLO'                                         800
         CALL AXATTRIBUTE                                                 800
C    *                                                                    800
C    Geant SHAPE : BOX ===> DX=SFJP_SFFL_DX/COS(15*PI/180), DY=SFJP_SFKK  800
C    _DY/2. DZ=SFJP_SFFK_DZ/2 <===                                        800
         CALL AGSRESET                                                    802
         AG_DX = SFJP_SFFL_DX/COS(15*PI/180)                              802
         AG_DY = SFJP_SFKK_DY/2.                                          802
         AG_DZ = SFJP_SFFK_DZ/2                                           802
         AG_TITLE = 'BOX'                                                 802
         AG_EXNAME = 'SHAPE'                                              802
         AG_PARLIST = 'DX_DY_DZ'                                          802
         CALL AXSHAPE                                                     802
C    *                                                                    802
5541  CALL AGSPOPB                                                        803
      GO TO BLOCK_SFKL, (5540 ,5550)                                      803
C End of Local Procedure                                                  803
C ----------------------------------------------------------------------  804
C ------------ SFLA                                                       804
C Begin Local Procedure SFLA                                              806
C SFLA IS THE LONG PART OF THE BUS CABLE LINKING THE MODULES TO THE CONN  806
C ECTION, BOARD                                                           806
5760  CONTINUE                                                            806
         AG_TITLE='SFLA'                                                  806
         AG_MODULE=MODULE                                                 806
         AG_MARK='V00'                                                    806
         CALL AGSPUSH                                                     806
         AG_MODULE=MODULE                                                 806
C    *                                                                    806
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            807
         CALL AGSRESET                                                    807
         AG_TITLE = 'MYLAR'                                               807
         AG_EXNAME = 'MATERIAL'                                           807
         AG_PARLIST = 'NONE'                                              807
         CALL AXMATERIAL                                                  807
C    *                                                                    807
C    Geant ATTRIBUTE : SFLA ===> SEEN=1 COLO=1 <===                       807
         CALL AGSRESET                                                    808
         AG_SEEN = 1                                                      808
         AG_COLO = 1                                                      808
         AG_TITLE = 'SFLA'                                                808
         AG_EXNAME = 'ATTRIBUTE'                                          808
         AG_PARLIST = 'SEEN_COLO'                                         808
         CALL AXATTRIBUTE                                                 808
C    *                                                                    808
C    Geant SHAPE : BOX ===> DX=SFJP_SFLA_DX/2. DY=SFJP_SFLA_DY/2., DZ=SF  808
C    JP_SFLA_DZ/2. <===                                                   808
         CALL AGSRESET                                                    810
         AG_DX = SFJP_SFLA_DX/2.                                          810
         AG_DY = SFJP_SFLA_DY/2.                                          810
         AG_DZ = SFJP_SFLA_DZ/2.                                          810
         AG_TITLE = 'BOX'                                                 810
         AG_EXNAME = 'SHAPE'                                              810
         AG_PARLIST = 'DX_DY_DZ'                                          810
         CALL AXSHAPE                                                     810
C    *                                                                    810
5761  CALL AGSPOPB                                                        811
      GO TO BLOCK_SFLA, (5760 ,5770)                                      811
C End of Local Procedure                                                  811
C ----------------------------------------------------------------------  812
C ------------ SFLB                                                       812
C Begin Local Procedure SFLB                                              813
C SFLB IS THE PART OF THE LONG BUS CABLE ON THE CONNECTION BOARD          813
5780  CONTINUE                                                            813
         AG_TITLE='SFLB'                                                  813
         AG_MODULE=MODULE                                                 813
         AG_MARK='V00'                                                    813
         CALL AGSPUSH                                                     813
         AG_MODULE=MODULE                                                 813
C    *                                                                    813
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            814
         CALL AGSRESET                                                    814
         AG_TITLE = 'MYLAR'                                               814
         AG_EXNAME = 'MATERIAL'                                           814
         AG_PARLIST = 'NONE'                                              814
         CALL AXMATERIAL                                                  814
C    *                                                                    814
C    Geant ATTRIBUTE : SFLB ===> SEEN=1 COLO=1 <===                       814
         CALL AGSRESET                                                    815
         AG_SEEN = 1                                                      815
         AG_COLO = 1                                                      815
         AG_TITLE = 'SFLB'                                                815
         AG_EXNAME = 'ATTRIBUTE'                                          815
         AG_PARLIST = 'SEEN_COLO'                                         815
         CALL AXATTRIBUTE                                                 815
C    *                                                                    815
C    Geant SHAPE : BOX ===> DX=SFJP_SFLA_DX/2. DY=SFJP_SFLA_DY/2., DZ=SF  815
C    JP_SFLB_DZ/2. <===                                                   815
         CALL AGSRESET                                                    817
         AG_DX = SFJP_SFLA_DX/2.                                          817
         AG_DY = SFJP_SFLA_DY/2.                                          817
         AG_DZ = SFJP_SFLB_DZ/2.                                          817
         AG_TITLE = 'BOX'                                                 817
         AG_EXNAME = 'SHAPE'                                              817
         AG_PARLIST = 'DX_DY_DZ'                                          817
         CALL AXSHAPE                                                     817
C    *                                                                    817
5781  CALL AGSPOPB                                                        818
      GO TO BLOCK_SFLB, (5780 ,5790)                                      818
C End of Local Procedure                                                  818
C ----------------------------------------------------------------------  819
C ------------ SFLC                                                       819
C Begin Local Procedure SFLC                                              821
C SFLC IS THE PART OF THE LONG BUS CABLE ON THE CONNECTION BOARD UP TO,   821
C THE CONNECTOR                                                           821
5800  CONTINUE                                                            821
         AG_TITLE='SFLC'                                                  821
         AG_MODULE=MODULE                                                 821
         AG_MARK='V00'                                                    821
         CALL AGSPUSH                                                     821
         AG_MODULE=MODULE                                                 821
C    *                                                                    821
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            822
         CALL AGSRESET                                                    822
         AG_TITLE = 'MYLAR'                                               822
         AG_EXNAME = 'MATERIAL'                                           822
         AG_PARLIST = 'NONE'                                              822
         CALL AXMATERIAL                                                  822
C    *                                                                    822
C    Geant ATTRIBUTE : SFLC ===> SEEN=1 COLO=1 <===                       822
         CALL AGSRESET                                                    823
         AG_SEEN = 1                                                      823
         AG_COLO = 1                                                      823
         AG_TITLE = 'SFLC'                                                823
         AG_EXNAME = 'ATTRIBUTE'                                          823
         AG_PARLIST = 'SEEN_COLO'                                         823
         CALL AXATTRIBUTE                                                 823
C    *                                                                    823
C    Geant SHAPE : BOX ===> DX=SFJP_SFLA_DX/2. DY=SFJP_SFLA_DY/2., DZ=SF  823
C    JP_SFLC_DZ/2. <===                                                   823
         CALL AGSRESET                                                    825
         AG_DX = SFJP_SFLA_DX/2.                                          825
         AG_DY = SFJP_SFLA_DY/2.                                          825
         AG_DZ = SFJP_SFLC_DZ/2.                                          825
         AG_TITLE = 'BOX'                                                 825
         AG_EXNAME = 'SHAPE'                                              825
         AG_PARLIST = 'DX_DY_DZ'                                          825
         CALL AXSHAPE                                                     825
C    *                                                                    825
5801  CALL AGSPOPB                                                        826
      GO TO BLOCK_SFLC, (5800 ,5810)                                      826
C End of Local Procedure                                                  826
C ----------------------------------------------------------------------  827
C ------------ SFEB                                                       827
C Begin Local Procedure SFEB                                              828
C SFEB IS THE BIG BUS ELBOW                                               828
5820  CONTINUE                                                            828
         AG_TITLE='SFEB'                                                  828
         AG_MODULE=MODULE                                                 828
         AG_MARK='V00'                                                    828
         CALL AGSPUSH                                                     828
         AG_MODULE=MODULE                                                 828
C    *                                                                    828
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            829
         CALL AGSRESET                                                    829
         AG_TITLE = 'MYLAR'                                               829
         AG_EXNAME = 'MATERIAL'                                           829
         AG_PARLIST = 'NONE'                                              829
         CALL AXMATERIAL                                                  829
C    *                                                                    829
C    Geant ATTRIBUTE : SFEB ===> SEEN=1 COLO=1 <===                       829
         CALL AGSRESET                                                    830
         AG_SEEN = 1                                                      830
         AG_COLO = 1                                                      830
         AG_TITLE = 'SFEB'                                                830
         AG_EXNAME = 'ATTRIBUTE'                                          830
         AG_PARLIST = 'SEEN_COLO'                                         830
         CALL AXATTRIBUTE                                                 830
C    *                                                                    830
C    Geant SHAPE : BOX ===> DX=SFJP_SFEB_DX/2. DY=SFJP_SFLA_DY/2., DZ=SF  830
C    JP_SFEB_DZ/2. <===                                                   830
         CALL AGSRESET                                                    832
         AG_DX = SFJP_SFEB_DX/2.                                          832
         AG_DY = SFJP_SFLA_DY/2.                                          832
         AG_DZ = SFJP_SFEB_DZ/2.                                          832
         AG_TITLE = 'BOX'                                                 832
         AG_EXNAME = 'SHAPE'                                              832
         AG_PARLIST = 'DX_DY_DZ'                                          832
         CALL AXSHAPE                                                     832
C    *                                                                    832
5821  CALL AGSPOPB                                                        833
      GO TO BLOCK_SFEB, (5820 ,5830)                                      833
C End of Local Procedure                                                  833
C ----------------------------------------------------------------------  834
C ------------ SFES                                                       834
C Begin Local Procedure SFES                                              835
C SFES IS THE SMALL BUS ELBOW                                             835
5840  CONTINUE                                                            835
         AG_TITLE='SFES'                                                  835
         AG_MODULE=MODULE                                                 835
         AG_MARK='V00'                                                    835
         CALL AGSPUSH                                                     835
         AG_MODULE=MODULE                                                 835
C    *                                                                    835
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            836
         CALL AGSRESET                                                    836
         AG_TITLE = 'MYLAR'                                               836
         AG_EXNAME = 'MATERIAL'                                           836
         AG_PARLIST = 'NONE'                                              836
         CALL AXMATERIAL                                                  836
C    *                                                                    836
C    Geant ATTRIBUTE : SFES ===> SEEN=1 COLO=1 <===                       836
         CALL AGSRESET                                                    837
         AG_SEEN = 1                                                      837
         AG_COLO = 1                                                      837
         AG_TITLE = 'SFES'                                                837
         AG_EXNAME = 'ATTRIBUTE'                                          837
         AG_PARLIST = 'SEEN_COLO'                                         837
         CALL AXATTRIBUTE                                                 837
C    *                                                                    837
C    Geant SHAPE : BOX ===> DX=SFJP_SFES_DX/2. DY=SFJP_SFLA_DY/2., DZ=SF  837
C    JP_SFEB_DZ/2. <===                                                   837
         CALL AGSRESET                                                    839
         AG_DX = SFJP_SFES_DX/2.                                          839
         AG_DY = SFJP_SFLA_DY/2.                                          839
         AG_DZ = SFJP_SFEB_DZ/2.                                          839
         AG_TITLE = 'BOX'                                                 839
         AG_EXNAME = 'SHAPE'                                              839
         AG_PARLIST = 'DX_DY_DZ'                                          839
         CALL AXSHAPE                                                     839
C    *                                                                    839
5841  CALL AGSPOPB                                                        840
      GO TO BLOCK_SFES, (5840 ,5850)                                      840
C End of Local Procedure                                                  840
C ----------------------------------------------------------------------  841
C ------------ SFAM                                                       841
C Begin Local Procedure SFAM                                              842
C SFAM IS THE MOTHER VOLUME OF THE ADC BOARD                              842
5580  CONTINUE                                                            842
         AG_TITLE='SFAM'                                                  842
         AG_MODULE=MODULE                                                 842
         AG_MARK='V00'                                                    842
         CALL AGSPUSH                                                     842
         AG_MODULE=MODULE                                                 842
C    *                                                                    842
C    Geant MATERIAL : AIR ===> inherited arguments used <===              843
         CALL AGSRESET                                                    843
         AG_TITLE = 'AIR'                                                 843
         AG_EXNAME = 'MATERIAL'                                           843
         AG_PARLIST = 'NONE'                                              843
         CALL AXMATERIAL                                                  843
C    *                                                                    843
C    Geant ATTRIBUTE : SFAM ===> SEEN=0 COLO=6 <===                       843
         CALL AGSRESET                                                    844
         AG_SEEN = 0                                                      844
         AG_COLO = 6                                                      844
         AG_TITLE = 'SFAM'                                                844
         AG_EXNAME = 'ATTRIBUTE'                                          844
         AG_PARLIST = 'SEEN_COLO'                                         844
         CALL AXATTRIBUTE                                                 844
C    *                                                                    844
C    Geant SHAPE : BOX ===> DX=SFJP_SFAM_DXE/2. DY=SFJP_SFAM_DY/2., DZ=S  844
C    FJP_SFAM_DZ/2 <===                                                   844
         CALL AGSRESET                                                    846
         AG_DX = SFJP_SFAM_DXE/2.                                         846
         AG_DY = SFJP_SFAM_DY/2.                                          846
         AG_DZ = SFJP_SFAM_DZ/2                                           846
         AG_TITLE = 'BOX'                                                 846
         AG_EXNAME = 'SHAPE'                                              846
         AG_PARLIST = 'DX_DY_DZ'                                          846
         CALL AXSHAPE                                                     846
C    *                                                                    846
C    Create SFAB :                                                        847
C    Call To Local Procedure SFAB                                         847
         ASSIGN 5900 TO BLOCK_SFAB                                        847
         GO TO  5890                                                      847
5900     CONTINUE                                                         847
C    *                                                                    847
C    Geant POSITION : SFAB ===> X=0. Y=0. Z=SFJP_SFAM_DZS/2 <===          848
         CALL AGSRESET                                                    848
         AG_X = 0.                                                        848
         AG_Y = 0.                                                        848
         AG_Z = SFJP_SFAM_DZS/2                                           848
         AG_TITLE = 'SFAB'                                                848
         AG_EXNAME = 'POSITION'                                           848
         AG_PARLIST = 'X_Y_Z'                                             848
         CALL AXPOSITION                                                  848
C    *                                                                    848
C    Create SFAS :                                                        849
C    Call To Local Procedure SFAS                                         849
         ASSIGN 5920 TO BLOCK_SFAS                                        849
         GO TO  5910                                                      849
5920     CONTINUE                                                         849
C    *                                                                    849
C    Geant POSITION : SFAS ===> X=(SFJP_SFAM_DXE-SFJP_SFAM_DXZ)/2. Y=0.   851
C    Z=(-SFJP_SFAM_DZ+SFJP_SFAM_DZS)/2 <===                               851
         CALL AGSRESET                                                    851
         AG_X = (SFJP_SFAM_DXE-SFJP_SFAM_DXZ)/2.                          851
         AG_Y = 0.                                                        851
         AG_Z = (-SFJP_SFAM_DZ+SFJP_SFAM_DZS)/2                           851
         AG_TITLE = 'SFAS'                                                851
         AG_EXNAME = 'POSITION'                                           851
         AG_PARLIST = 'X_Y_Z'                                             851
         CALL AXPOSITION                                                  851
C    *                                                                    851
C    Geant POSITION : SFAS ===> X=(-SFJP_SFAM_DXE+SFJP_SFAM_DXZ)/2. Y=0.  851
C     Z=(-SFJP_SFAM_DZ+SFJP_SFAM_DZS)/2 <===                              851
         CALL AGSRESET                                                    853
         AG_X = (-SFJP_SFAM_DXE+SFJP_SFAM_DXZ)/2.                         853
         AG_Y = 0.                                                        853
         AG_Z = (-SFJP_SFAM_DZ+SFJP_SFAM_DZS)/2                           853
         AG_TITLE = 'SFAS'                                                853
         AG_EXNAME = 'POSITION'                                           853
         AG_PARLIST = 'X_Y_Z'                                             853
         CALL AXPOSITION                                                  853
C    *                                                                    853
5581  CALL AGSPOPB                                                        854
      GO TO BLOCK_SFAM, (5580 ,5590)                                      854
C End of Local Procedure                                                  854
C ----------------------------------------------------------------------  855
C ------------ SFAB                                                       855
C Begin Local Procedure SFAB                                              856
C SFAB IS THE BIG VOLUME OF THE ADC BOARD                                 856
5890  CONTINUE                                                            856
         AG_TITLE='SFAB'                                                  856
         AG_MODULE=MODULE                                                 856
         AG_MARK='V00'                                                    856
         CALL AGSPUSH                                                     856
         AG_MODULE=MODULE                                                 856
C    *                                                                    856
C    Geant MATERIAL : G10 ===> inherited arguments used <===              857
         CALL AGSRESET                                                    857
         AG_TITLE = 'G10'                                                 857
         AG_EXNAME = 'MATERIAL'                                           857
         AG_PARLIST = 'NONE'                                              857
         CALL AXMATERIAL                                                  857
C    *                                                                    857
C    Geant ATTRIBUTE : SFAB ===> SEEN=1 COLO=30 <===                      857
         CALL AGSRESET                                                    858
         AG_SEEN = 1                                                      858
         AG_COLO = 30                                                     858
         AG_TITLE = 'SFAB'                                                858
         AG_EXNAME = 'ATTRIBUTE'                                          858
         AG_PARLIST = 'SEEN_COLO'                                         858
         CALL AXATTRIBUTE                                                 858
C    *                                                                    858
C    Geant SHAPE : BOX ===> DX=SFJP_SFAM_DXE/2. DY=SFJP_SFAM_DY/2., DZ=(  858
C    SFJP_SFAM_DZ-SFJP_SFAM_DZS)/2 <===                                   858
         CALL AGSRESET                                                    860
         AG_DX = SFJP_SFAM_DXE/2.                                         860
         AG_DY = SFJP_SFAM_DY/2.                                          860
         AG_DZ = (SFJP_SFAM_DZ-SFJP_SFAM_DZS)/2                           860
         AG_TITLE = 'BOX'                                                 860
         AG_EXNAME = 'SHAPE'                                              860
         AG_PARLIST = 'DX_DY_DZ'                                          860
         CALL AXSHAPE                                                     860
C    *                                                                    860
5891  CALL AGSPOPB                                                        861
      GO TO BLOCK_SFAB, (5890 ,5900)                                      861
C End of Local Procedure                                                  861
C ----------------------------------------------------------------------  862
C ------------ SFAS                                                       862
C Begin Local Procedure SFAS                                              863
C SFAS IS THE SMALL VOLUME OF THE ADC BOARD                               863
5910  CONTINUE                                                            863
         AG_TITLE='SFAS'                                                  863
         AG_MODULE=MODULE                                                 863
         AG_MARK='V00'                                                    863
         CALL AGSPUSH                                                     863
         AG_MODULE=MODULE                                                 863
C    *                                                                    863
C    Geant MATERIAL : G10 ===> inherited arguments used <===              864
         CALL AGSRESET                                                    864
         AG_TITLE = 'G10'                                                 864
         AG_EXNAME = 'MATERIAL'                                           864
         AG_PARLIST = 'NONE'                                              864
         CALL AXMATERIAL                                                  864
C    *                                                                    864
C    Geant ATTRIBUTE : SFAS ===> SEEN=1 COLO=30 <===                      864
         CALL AGSRESET                                                    865
         AG_SEEN = 1                                                      865
         AG_COLO = 30                                                     865
         AG_TITLE = 'SFAS'                                                865
         AG_EXNAME = 'ATTRIBUTE'                                          865
         AG_PARLIST = 'SEEN_COLO'                                         865
         CALL AXATTRIBUTE                                                 865
C    *                                                                    865
C    Geant SHAPE : BOX ===> DX=(SFJP_SFAM_DXZ)/2. DY=(SFJP_SFAM_DY)/2.,   865
C    DZ=(SFJP_SFAM_DZS)/2 <===                                            865
         CALL AGSRESET                                                    867
         AG_DX = (SFJP_SFAM_DXZ)/2.                                       867
         AG_DY = (SFJP_SFAM_DY)/2.                                        867
         AG_DZ = (SFJP_SFAM_DZS)/2                                        867
         AG_TITLE = 'BOX'                                                 867
         AG_EXNAME = 'SHAPE'                                              867
         AG_PARLIST = 'DX_DY_DZ'                                          867
         CALL AXSHAPE                                                     867
C    *                                                                    867
5911  CALL AGSPOPB                                                        868
      GO TO BLOCK_SFAS, (5910 ,5920)                                      868
C End of Local Procedure                                                  868
C ----------------------------------------------------------------------  869
C ------------ SAPP                                                       869
C Begin Local Procedure SAPP                                              870
C SAPP IS THE MOTHER VOLUME OF THE ADC BOARD APPENDICE                    870
5560  CONTINUE                                                            870
         AG_TITLE='SAPP'                                                  870
         AG_MODULE=MODULE                                                 870
         AG_MARK='V00'                                                    870
         CALL AGSPUSH                                                     870
         AG_MODULE=MODULE                                                 870
C    *                                                                    870
C    Geant MATERIAL : AIR ===> inherited arguments used <===              871
         CALL AGSRESET                                                    871
         AG_TITLE = 'AIR'                                                 871
         AG_EXNAME = 'MATERIAL'                                           871
         AG_PARLIST = 'NONE'                                              871
         CALL AXMATERIAL                                                  871
C    *                                                                    871
C    Geant ATTRIBUTE : SAPP ===> SEEN=0 COLO=6 <===                       871
         CALL AGSRESET                                                    872
         AG_SEEN = 0                                                      872
         AG_COLO = 6                                                      872
         AG_TITLE = 'SAPP'                                                872
         AG_EXNAME = 'ATTRIBUTE'                                          872
         AG_PARLIST = 'SEEN_COLO'                                         872
         CALL AXATTRIBUTE                                                 872
C    *                                                                    872
C    Geant SHAPE : BOX ===> DX=(SFJP_SAPP_DXE+2*SFJP_SAPP_DXZ)/2., DY=(S  872
C    FJP_SAPP_DY+2*SFJP_SAPP_DXZ)/2., DZ=SFJP_SAPP_DZ/2. <===             872
         CALL AGSRESET                                                    875
         AG_DX = (SFJP_SAPP_DXE+2*SFJP_SAPP_DXZ)/2.                       875
         AG_DY = (SFJP_SAPP_DY+2*SFJP_SAPP_DXZ)/2.                        875
         AG_DZ = SFJP_SAPP_DZ/2.                                          875
         AG_TITLE = 'BOX'                                                 875
         AG_EXNAME = 'SHAPE'                                              875
         AG_PARLIST = 'DX_DY_DZ'                                          875
         CALL AXSHAPE                                                     875
C    *                                                                    875
C    Create SAPC :                                                        876
C    Call To Local Procedure SAPC                                         876
         ASSIGN 5940 TO BLOCK_SAPC                                        876
         GO TO  5930                                                      876
5940     CONTINUE                                                         876
C    *                                                                    876
C    Geant POSITION : SAPC ===> X=0. Y=0. Z=(-SFJP_SAPP_DZ+SFJP_SAPP_DY)  877
C    /2. <===                                                             877
         CALL AGSRESET                                                    877
         AG_X = 0.                                                        877
         AG_Y = 0.                                                        877
         AG_Z = (-SFJP_SAPP_DZ+SFJP_SAPP_DY)/2.                           877
         AG_TITLE = 'SAPC'                                                877
         AG_EXNAME = 'POSITION'                                           877
         AG_PARLIST = 'X_Y_Z'                                             877
         CALL AXPOSITION                                                  877
C    *                                                                    877
C    Create SAPS :                                                        878
C    Call To Local Procedure SAPS                                         878
         ASSIGN 5960 TO BLOCK_SAPS                                        878
         GO TO  5950                                                      878
5960     CONTINUE                                                         878
C    *                                                                    878
C    Geant POSITION : SAPS ===> X=SFJP_SAPP_DXE/2.+SFJP_SAPP_DXZ/2 Y=0 Z  879
C    =0. <===                                                             879
         CALL AGSRESET                                                    879
         AG_X = SFJP_SAPP_DXE/2.+SFJP_SAPP_DXZ/2                          879
         AG_Y = 0                                                         879
         AG_Z = 0.                                                        879
         AG_TITLE = 'SAPS'                                                879
         AG_EXNAME = 'POSITION'                                           879
         AG_PARLIST = 'X_Y_Z'                                             879
         CALL AXPOSITION                                                  879
C    *                                                                    879
C    Geant POSITION : SAPS ===> X=-(SFJP_SAPP_DXE/2.+SFJP_SAPP_DXZ/2) Y=  879
C    0 Z=0. <===                                                          879
         CALL AGSRESET                                                    880
         AG_X = -(SFJP_SAPP_DXE/2.+SFJP_SAPP_DXZ/2)                       880
         AG_Y = 0                                                         880
         AG_Z = 0.                                                        880
         AG_TITLE = 'SAPS'                                                880
         AG_EXNAME = 'POSITION'                                           880
         AG_PARLIST = 'X_Y_Z'                                             880
         CALL AXPOSITION                                                  880
C    *                                                                    880
C    Create SAPT :                                                        881
C    Call To Local Procedure SAPT                                         881
         ASSIGN 5980 TO BLOCK_SAPT                                        881
         GO TO  5970                                                      881
5980     CONTINUE                                                         881
C    *                                                                    881
C    Geant POSITION : SAPT ===> X=0. Y=(SFJP_SAPP_DY+SFJP_SAPP_DXZ)/2. Z  882
C    =0. <===                                                             882
         CALL AGSRESET                                                    882
         AG_X = 0.                                                        882
         AG_Y = (SFJP_SAPP_DY+SFJP_SAPP_DXZ)/2.                           882
         AG_Z = 0.                                                        882
         AG_TITLE = 'SAPT'                                                882
         AG_EXNAME = 'POSITION'                                           882
         AG_PARLIST = 'X_Y_Z'                                             882
         CALL AXPOSITION                                                  882
C    *                                                                    882
C    Geant POSITION : SAPT ===> X=0. Y=-(SFJP_SAPP_DY+SFJP_SAPP_DXZ)/2.   882
C    Z=0. <===                                                            882
         CALL AGSRESET                                                    883
         AG_X = 0.                                                        883
         AG_Y = -(SFJP_SAPP_DY+SFJP_SAPP_DXZ)/2.                          883
         AG_Z = 0.                                                        883
         AG_TITLE = 'SAPT'                                                883
         AG_EXNAME = 'POSITION'                                           883
         AG_PARLIST = 'X_Y_Z'                                             883
         CALL AXPOSITION                                                  883
C    *                                                                    883
5561  CALL AGSPOPB                                                        884
      GO TO BLOCK_SAPP, (5560 ,5570)                                      884
C End of Local Procedure                                                  884
C ----------------------------------------------------------------------  885
C ------------ SAPC                                                       885
C Begin Local Procedure SAPC                                              886
C SAPC IS THE CORE (EPOXY) OF THE ADC BOARD APPENDICE                     886
5930  CONTINUE                                                            886
         AG_TITLE='SAPC'                                                  886
         AG_MODULE=MODULE                                                 886
         AG_MARK='V00'                                                    886
         CALL AGSPUSH                                                     886
         AG_MODULE=MODULE                                                 886
C    *                                                                    886
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           887
         CALL AGSRESET                                                    887
         AG_TITLE = 'CARBON'                                              887
         AG_EXNAME = 'MATERIAL'                                           887
         AG_PARLIST = 'NONE'                                              887
         CALL AXMATERIAL                                                  887
C    *                                                                    887
C    Geant ATTRIBUTE : SAPC ===> SEEN=1 COLO=1 <===                       887
         CALL AGSRESET                                                    888
         AG_SEEN = 1                                                      888
         AG_COLO = 1                                                      888
         AG_TITLE = 'SAPC'                                                888
         AG_EXNAME = 'ATTRIBUTE'                                          888
         AG_PARLIST = 'SEEN_COLO'                                         888
         CALL AXATTRIBUTE                                                 888
C    *                                                                    888
C    Geant SHAPE : BOX ===> DX=SFJP_SAPP_DXE/2. DY=SFJP_SAPP_DY/2., DZ=S  888
C    FJP_SAPP_DY/2. <===                                                  888
         CALL AGSRESET                                                    890
         AG_DX = SFJP_SAPP_DXE/2.                                         890
         AG_DY = SFJP_SAPP_DY/2.                                          890
         AG_DZ = SFJP_SAPP_DY/2.                                          890
         AG_TITLE = 'BOX'                                                 890
         AG_EXNAME = 'SHAPE'                                              890
         AG_PARLIST = 'DX_DY_DZ'                                          890
         CALL AXSHAPE                                                     890
C    *                                                                    890
5931  CALL AGSPOPB                                                        891
      GO TO BLOCK_SAPC, (5930 ,5940)                                      891
C End of Local Procedure                                                  891
C ----------------------------------------------------------------------  892
C ------------ SAPS                                                       892
C Begin Local Procedure SAPS                                              893
C SAPS IS THE SIDE SHELL (CARBON) OF THE ADC BOARD APPENDICE              893
5950  CONTINUE                                                            893
         AG_TITLE='SAPS'                                                  893
         AG_MODULE=MODULE                                                 893
         AG_MARK='V00'                                                    893
         CALL AGSPUSH                                                     893
         AG_MODULE=MODULE                                                 893
C    *                                                                    893
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           894
         CALL AGSRESET                                                    894
         AG_TITLE = 'CARBON'                                              894
         AG_EXNAME = 'MATERIAL'                                           894
         AG_PARLIST = 'NONE'                                              894
         CALL AXMATERIAL                                                  894
C    *                                                                    894
C    Geant ATTRIBUTE : SAPS ===> SEEN=1 COLO=1 <===                       894
         CALL AGSRESET                                                    895
         AG_SEEN = 1                                                      895
         AG_COLO = 1                                                      895
         AG_TITLE = 'SAPS'                                                895
         AG_EXNAME = 'ATTRIBUTE'                                          895
         AG_PARLIST = 'SEEN_COLO'                                         895
         CALL AXATTRIBUTE                                                 895
C    *                                                                    895
C    Geant SHAPE : BOX ===> DX=SFJP_SAPP_DXZ/2. DY=(SFJP_SAPP_DY+2*SFJP_  895
C    SAPP_DXZ)/2., DZ=SFJP_SAPP_DZ/2. <===                                895
         CALL AGSRESET                                                    897
         AG_DX = SFJP_SAPP_DXZ/2.                                         897
         AG_DY = (SFJP_SAPP_DY+2*SFJP_SAPP_DXZ)/2.                        897
         AG_DZ = SFJP_SAPP_DZ/2.                                          897
         AG_TITLE = 'BOX'                                                 897
         AG_EXNAME = 'SHAPE'                                              897
         AG_PARLIST = 'DX_DY_DZ'                                          897
         CALL AXSHAPE                                                     897
C    *                                                                    897
5951  CALL AGSPOPB                                                        898
      GO TO BLOCK_SAPS, (5950 ,5960)                                      898
C End of Local Procedure                                                  898
C ----------------------------------------------------------------------  899
C ------------ SAPT                                                       899
C Begin Local Procedure SAPT                                              900
C SAPT IS THE TOP-BOTTOM SHELL (CARBON) OF THE ADC BOARD APPENDICE        900
5970  CONTINUE                                                            900
         AG_TITLE='SAPT'                                                  900
         AG_MODULE=MODULE                                                 900
         AG_MARK='V00'                                                    900
         CALL AGSPUSH                                                     900
         AG_MODULE=MODULE                                                 900
C    *                                                                    900
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           901
         CALL AGSRESET                                                    901
         AG_TITLE = 'CARBON'                                              901
         AG_EXNAME = 'MATERIAL'                                           901
         AG_PARLIST = 'NONE'                                              901
         CALL AXMATERIAL                                                  901
C    *                                                                    901
C    Geant ATTRIBUTE : SAPT ===> SEEN=1 COLO=1 <===                       901
         CALL AGSRESET                                                    902
         AG_SEEN = 1                                                      902
         AG_COLO = 1                                                      902
         AG_TITLE = 'SAPT'                                                902
         AG_EXNAME = 'ATTRIBUTE'                                          902
         AG_PARLIST = 'SEEN_COLO'                                         902
         CALL AXATTRIBUTE                                                 902
C    *                                                                    902
C    Geant SHAPE : BOX ===> DX=SFJP_SAPP_DXE/2. DY=SFJP_SAPP_DXZ/2., DZ=  902
C    SFJP_SAPP_DZ/2. <===                                                 902
         CALL AGSRESET                                                    904
         AG_DX = SFJP_SAPP_DXE/2.                                         904
         AG_DY = SFJP_SAPP_DXZ/2.                                         904
         AG_DZ = SFJP_SAPP_DZ/2.                                          904
         AG_TITLE = 'BOX'                                                 904
         AG_EXNAME = 'SHAPE'                                              904
         AG_PARLIST = 'DX_DY_DZ'                                          904
         CALL AXSHAPE                                                     904
C    *                                                                    904
5971  CALL AGSPOPB                                                        905
      GO TO BLOCK_SAPT, (5970 ,5980)                                      905
C End of Local Procedure                                                  905
C ----------------------------------------------------------------------  906
C ------------ SFCO                                                       906
C Begin Local Procedure SFCO                                              907
C SFCO IS THE CONNECTION BOARD (RECTANGULAR WITH HIROSE CONNECTORS)       907
5600  CONTINUE                                                            907
         AG_TITLE='SFCO'                                                  907
         AG_MODULE=MODULE                                                 907
         AG_MARK='V00'                                                    907
         CALL AGSPUSH                                                     907
         AG_MODULE=MODULE                                                 907
C    *                                                                    907
C    Geant MATERIAL : G10 ===> inherited arguments used <===              908
         CALL AGSRESET                                                    908
         AG_TITLE = 'G10'                                                 908
         AG_EXNAME = 'MATERIAL'                                           908
         AG_PARLIST = 'NONE'                                              908
         CALL AXMATERIAL                                                  908
C    *                                                                    908
C    Geant ATTRIBUTE : SFCO ===> SEEN=1 COLO=30 <===                      908
         CALL AGSRESET                                                    909
         AG_SEEN = 1                                                      909
         AG_COLO = 30                                                     909
         AG_TITLE = 'SFCO'                                                909
         AG_EXNAME = 'ATTRIBUTE'                                          909
         AG_PARLIST = 'SEEN_COLO'                                         909
         CALL AXATTRIBUTE                                                 909
C    *                                                                    909
C    Geant SHAPE : BOX ===> DX=SFJP_SFCO_DX/2. DY=SFJP_SFCO_DY/2., DZ=SF  909
C    JP_SFCO_DZ/2 <===                                                    909
         CALL AGSRESET                                                    911
         AG_DX = SFJP_SFCO_DX/2.                                          911
         AG_DY = SFJP_SFCO_DY/2.                                          911
         AG_DZ = SFJP_SFCO_DZ/2                                           911
         AG_TITLE = 'BOX'                                                 911
         AG_EXNAME = 'SHAPE'                                              911
         AG_PARLIST = 'DX_DY_DZ'                                          911
         CALL AXSHAPE                                                     911
C    *                                                                    911
5601  CALL AGSPOPB                                                        912
      GO TO BLOCK_SFCO, (5600 ,5610)                                      912
C End of Local Procedure                                                  912
C ----------------------------------------------------------------------  913
C ------------ SFCM                                                       913
C Begin Local Procedure SFCM                                              914
C SFCM IS THE MOTHER VOLUME OF THE SECOND CONNECTION BOARD                914
5620  CONTINUE                                                            914
         AG_TITLE='SFCM'                                                  914
         AG_MODULE=MODULE                                                 914
         AG_MARK='V00'                                                    914
         CALL AGSPUSH                                                     914
         AG_MODULE=MODULE                                                 914
C    *                                                                    914
C    Geant MATERIAL : AIR ===> inherited arguments used <===              915
         CALL AGSRESET                                                    915
         AG_TITLE = 'AIR'                                                 915
         AG_EXNAME = 'MATERIAL'                                           915
         AG_PARLIST = 'NONE'                                              915
         CALL AXMATERIAL                                                  915
C    *                                                                    915
C    Geant ATTRIBUTE : SFCM ===> SEEN=0 COLO=6 <===                       915
         CALL AGSRESET                                                    916
         AG_SEEN = 0                                                      916
         AG_COLO = 6                                                      916
         AG_TITLE = 'SFCM'                                                916
         AG_EXNAME = 'ATTRIBUTE'                                          916
         AG_PARLIST = 'SEEN_COLO'                                         916
         CALL AXATTRIBUTE                                                 916
C    *                                                                    916
C    Geant SHAPE : BOX ===> DX=SFJP_SFCM_DX/2. DY=SFJP_SFCO_DY/2., DZ=SF  916
C    JP_SFCO_DZ/2 <===                                                    916
         CALL AGSRESET                                                    918
         AG_DX = SFJP_SFCM_DX/2.                                          918
         AG_DY = SFJP_SFCO_DY/2.                                          918
         AG_DZ = SFJP_SFCO_DZ/2                                           918
         AG_TITLE = 'BOX'                                                 918
         AG_EXNAME = 'SHAPE'                                              918
         AG_PARLIST = 'DX_DY_DZ'                                          918
         CALL AXSHAPE                                                     918
C    *                                                                    918
C    Create SFCB :                                                        919
C    Call To Local Procedure SFCB                                         919
         ASSIGN 6000 TO BLOCK_SFCB                                        919
         GO TO  5990                                                      919
6000     CONTINUE                                                         919
C    *                                                                    919
C    Geant POSITION : SFCB ===> X=+0.5 Y=0 Z=0 <===                       920
         CALL AGSRESET                                                    920
         AG_X = +0.5                                                      920
         AG_Y = 0                                                         920
         AG_Z = 0                                                         920
         AG_TITLE = 'SFCB'                                                920
         AG_EXNAME = 'POSITION'                                           920
         AG_PARLIST = 'X_Y_Z'                                             920
         CALL AXPOSITION                                                  920
C    *                                                                    920
C    Create SFCS :                                                        921
C    Call To Local Procedure SFCS                                         921
         ASSIGN 6020 TO BLOCK_SFCS                                        921
         GO TO  6010                                                      921
6020     CONTINUE                                                         921
C    *                                                                    921
C    Geant POSITION : SFCS ===> X=-1.5 Y=0 Z=-SFJP_SFCO_DZ/2+SFJP_SFCS_D  922
C    Z/2 <===                                                             922
         CALL AGSRESET                                                    922
         AG_X = -1.5                                                      922
         AG_Y = 0                                                         922
         AG_Z = -SFJP_SFCO_DZ/2+SFJP_SFCS_DZ/2                            922
         AG_TITLE = 'SFCS'                                                922
         AG_EXNAME = 'POSITION'                                           922
         AG_PARLIST = 'X_Y_Z'                                             922
         CALL AXPOSITION                                                  922
C    *                                                                    922
5621  CALL AGSPOPB                                                        923
      GO TO BLOCK_SFCM, (5620 ,5630)                                      923
C End of Local Procedure                                                  923
C ----------------------------------------------------------------------  924
C ------------ SFCB                                                       924
C Begin Local Procedure SFCB                                              925
C SFCB IS THE BIG PART OF THE SECOND CONNECTION BOARD                     925
5990  CONTINUE                                                            925
         AG_TITLE='SFCB'                                                  925
         AG_MODULE=MODULE                                                 925
         AG_MARK='V00'                                                    925
         CALL AGSPUSH                                                     925
         AG_MODULE=MODULE                                                 925
C    *                                                                    925
C    Geant MATERIAL : G10 ===> inherited arguments used <===              926
         CALL AGSRESET                                                    926
         AG_TITLE = 'G10'                                                 926
         AG_EXNAME = 'MATERIAL'                                           926
         AG_PARLIST = 'NONE'                                              926
         CALL AXMATERIAL                                                  926
C    *                                                                    926
C    Geant ATTRIBUTE : SFCB ===> SEEN=1 COLO=30 <===                      926
         CALL AGSRESET                                                    927
         AG_SEEN = 1                                                      927
         AG_COLO = 30                                                     927
         AG_TITLE = 'SFCB'                                                927
         AG_EXNAME = 'ATTRIBUTE'                                          927
         AG_PARLIST = 'SEEN_COLO'                                         927
         CALL AXATTRIBUTE                                                 927
C    *                                                                    927
C    Geant SHAPE : BOX ===> DX=SFJP_SFCB_DX/2. DY=SFJP_SFCO_DY/2., DZ=SF  927
C    JP_SFCO_DZ/2 <===                                                    927
         CALL AGSRESET                                                    929
         AG_DX = SFJP_SFCB_DX/2.                                          929
         AG_DY = SFJP_SFCO_DY/2.                                          929
         AG_DZ = SFJP_SFCO_DZ/2                                           929
         AG_TITLE = 'BOX'                                                 929
         AG_EXNAME = 'SHAPE'                                              929
         AG_PARLIST = 'DX_DY_DZ'                                          929
         CALL AXSHAPE                                                     929
C    *                                                                    929
5991  CALL AGSPOPB                                                        930
      GO TO BLOCK_SFCB, (5990 ,6000)                                      930
C End of Local Procedure                                                  930
C ----------------------------------------------------------------------  931
C ------------ SFCS                                                       931
C Begin Local Procedure SFCS                                              932
C SFCS IS THE BIG PART OF THE SECOND CONNECTION BOARD                     932
6010  CONTINUE                                                            932
         AG_TITLE='SFCS'                                                  932
         AG_MODULE=MODULE                                                 932
         AG_MARK='V00'                                                    932
         CALL AGSPUSH                                                     932
         AG_MODULE=MODULE                                                 932
C    *                                                                    932
C    Geant MATERIAL : G10 ===> inherited arguments used <===              933
         CALL AGSRESET                                                    933
         AG_TITLE = 'G10'                                                 933
         AG_EXNAME = 'MATERIAL'                                           933
         AG_PARLIST = 'NONE'                                              933
         CALL AXMATERIAL                                                  933
C    *                                                                    933
C    Geant ATTRIBUTE : SFCS ===> SEEN=1 COLO=30 <===                      933
         CALL AGSRESET                                                    934
         AG_SEEN = 1                                                      934
         AG_COLO = 30                                                     934
         AG_TITLE = 'SFCS'                                                934
         AG_EXNAME = 'ATTRIBUTE'                                          934
         AG_PARLIST = 'SEEN_COLO'                                         934
         CALL AXATTRIBUTE                                                 934
C    *                                                                    934
C    Geant SHAPE : BOX ===> DX=(SFJP_SFCM_DX-SFJP_SFCB_DX)/2., DY=SFJP_S  934
C    FCO_DY/2. DZ=SFJP_SFCS_DZ/2 <===                                     934
         CALL AGSRESET                                                    936
         AG_DX = (SFJP_SFCM_DX-SFJP_SFCB_DX)/2.                           936
         AG_DY = SFJP_SFCO_DY/2.                                          936
         AG_DZ = SFJP_SFCS_DZ/2                                           936
         AG_TITLE = 'BOX'                                                 936
         AG_EXNAME = 'SHAPE'                                              936
         AG_PARLIST = 'DX_DY_DZ'                                          936
         CALL AXSHAPE                                                     936
C    *                                                                    936
6011  CALL AGSPOPB                                                        937
      GO TO BLOCK_SFCS, (6010 ,6020)                                      937
C End of Local Procedure                                                  937
C ----------------------------------------------------------------------  938
C ------------ SFKF                                                       938
C Begin Local Procedure SFKF                                              939
C SFKF IS THE FIRST PART OF THE KAPTON FLEX CIRCUIT                       939
5640  CONTINUE                                                            939
         AG_TITLE='SFKF'                                                  939
         AG_MODULE=MODULE                                                 939
         AG_MARK='V00'                                                    939
         CALL AGSPUSH                                                     939
         AG_MODULE=MODULE                                                 939
C    *                                                                    939
C    Geant MATERIAL : G10 ===> inherited arguments used <===              940
         CALL AGSRESET                                                    940
         AG_TITLE = 'G10'                                                 940
         AG_EXNAME = 'MATERIAL'                                           940
         AG_PARLIST = 'NONE'                                              940
         CALL AXMATERIAL                                                  940
C    *                                                                    940
C    Geant ATTRIBUTE : SFKF ===> SEEN=1 COLO=30 <===                      940
         CALL AGSRESET                                                    941
         AG_SEEN = 1                                                      941
         AG_COLO = 30                                                     941
         AG_TITLE = 'SFKF'                                                941
         AG_EXNAME = 'ATTRIBUTE'                                          941
         AG_PARLIST = 'SEEN_COLO'                                         941
         CALL AXATTRIBUTE                                                 941
C    *                                                                    941
C    Geant SHAPE : BOX ===> DX=SFJP_SFKF_DX/2. DY=SFJP_SFKF_DY/2., DZ=SF  941
C    JP_SFKF_DZ/2 <===                                                    941
         CALL AGSRESET                                                    943
         AG_DX = SFJP_SFKF_DX/2.                                          943
         AG_DY = SFJP_SFKF_DY/2.                                          943
         AG_DZ = SFJP_SFKF_DZ/2                                           943
         AG_TITLE = 'BOX'                                                 943
         AG_EXNAME = 'SHAPE'                                              943
         AG_PARLIST = 'DX_DY_DZ'                                          943
         CALL AXSHAPE                                                     943
C    *                                                                    943
5641  CALL AGSPOPB                                                        944
      GO TO BLOCK_SFKF, (5640 ,5650)                                      944
C End of Local Procedure                                                  944
C ----------------------------------------------------------------------  945
C ------------ SFKS                                                       945
C Begin Local Procedure SFKS                                              946
C SFKS IS THE SECOND PART OF THE KAPTON FLEX CIRCUIT                      946
5660  CONTINUE                                                            946
         AG_TITLE='SFKS'                                                  946
         AG_MODULE=MODULE                                                 946
         AG_MARK='V00'                                                    946
         CALL AGSPUSH                                                     946
         AG_MODULE=MODULE                                                 946
C    *                                                                    946
C    Geant MATERIAL : G10 ===> inherited arguments used <===              947
         CALL AGSRESET                                                    947
         AG_TITLE = 'G10'                                                 947
         AG_EXNAME = 'MATERIAL'                                           947
         AG_PARLIST = 'NONE'                                              947
         CALL AXMATERIAL                                                  947
C    *                                                                    947
C    Geant ATTRIBUTE : SFKS ===> SEEN=1 COLO=30 <===                      947
         CALL AGSRESET                                                    948
         AG_SEEN = 1                                                      948
         AG_COLO = 30                                                     948
         AG_TITLE = 'SFKS'                                                948
         AG_EXNAME = 'ATTRIBUTE'                                          948
         AG_PARLIST = 'SEEN_COLO'                                         948
         CALL AXATTRIBUTE                                                 948
C    *                                                                    948
C    Geant SHAPE : BOX ===> DX=SFJP_SFKS_DX/2. DY=SFJP_SFKF_DY/2., DZ=SF  948
C    JP_SFKF_DZ/2 <===                                                    948
         CALL AGSRESET                                                    950
         AG_DX = SFJP_SFKS_DX/2.                                          950
         AG_DY = SFJP_SFKF_DY/2.                                          950
         AG_DZ = SFJP_SFKF_DZ/2                                           950
         AG_TITLE = 'BOX'                                                 950
         AG_EXNAME = 'SHAPE'                                              950
         AG_PARLIST = 'DX_DY_DZ'                                          950
         CALL AXSHAPE                                                     950
C    *                                                                    950
5661  CALL AGSPOPB                                                        951
      GO TO BLOCK_SFKS, (5660 ,5670)                                      951
C End of Local Procedure                                                  951
C ----------------------------------------------------------------------  952
C ------------ SFPR                                                       952
C Begin Local Procedure SFPR                                              954
C SFPR IS THE LADDER END INSIDE MECHANICAL PART (PRISM WITH G10 WITH, HA  954
C LF DENSITY)                                                             954
5680  CONTINUE                                                            954
         AG_TITLE='SFPR'                                                  954
         AG_MODULE=MODULE                                                 954
         AG_MARK='V00'                                                    954
         CALL AGSPUSH                                                     954
         AG_MODULE=MODULE                                                 954
C    *                                                                    954
C    Geant MATERIAL : G5 ===> inherited arguments used <===               955
         CALL AGSRESET                                                    955
         AG_TITLE = 'G5'                                                  955
         AG_EXNAME = 'MATERIAL'                                           955
         AG_PARLIST = 'NONE'                                              955
         CALL AXMATERIAL                                                  955
C    *                                                                    955
C    Geant ATTRIBUTE : SFPR ===> SEEN=1 COLO=4 <===                       955
         CALL AGSRESET                                                    956
         AG_SEEN = 1                                                      956
         AG_COLO = 4                                                      956
         AG_TITLE = 'SFPR'                                                956
         AG_EXNAME = 'ATTRIBUTE'                                          956
         AG_PARLIST = 'SEEN_COLO'                                         956
         CALL AXATTRIBUTE                                                 956
C    *                                                                    956
C    Geant SHAPE : PGON ===> PHI1=-126 DPHI=72 NPDIV=1 NZ=2, ZI={0,3.7}   956
C    RMN={0,0} RMX={3.48,3.48} <===                                       956
         CALL AGSRESET                                                    958
         AG_PHI1 = -126                                                   958
         AG_DPHI = 72                                                     958
         AG_NPDIV = 1                                                     958
         AG_NZ = 2                                                        958
         AG_ZI(1)=0                                                       958
         AG_ZI(2)=3.7                                                     958
         AG_RMN(1)=0                                                      958
         AG_RMN(2)=0                                                      958
         AG_RMX(1)=3.48                                                   958
         AG_RMX(2)=3.48                                                   958
         AG_TITLE = 'PGON'                                                958
         AG_EXNAME = 'SHAPE'                                              958
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     958
         CALL AXSHAPE                                                     958
C    *                                                                    958
5681  CALL AGSPOPB                                                        959
      GO TO BLOCK_SFPR, (5680 ,5690)                                      959
C End of Local Procedure                                                  959
C ----------------------------------------------------------------------  960
C ------------ SFPB                                                       960
C Begin Local Procedure SFPB                                              961
C SFPB IS THE LADDER END OUTSIDE MECHANICAL PART (RECTANGLE WITH G10)     961
5700  CONTINUE                                                            961
         AG_TITLE='SFPB'                                                  961
         AG_MODULE=MODULE                                                 961
         AG_MARK='V00'                                                    961
         CALL AGSPUSH                                                     961
         AG_MODULE=MODULE                                                 961
C    *                                                                    961
C    Geant MATERIAL : G10 ===> inherited arguments used <===              962
         CALL AGSRESET                                                    962
         AG_TITLE = 'G10'                                                 962
         AG_EXNAME = 'MATERIAL'                                           962
         AG_PARLIST = 'NONE'                                              962
         CALL AXMATERIAL                                                  962
C    *                                                                    962
C    Geant ATTRIBUTE : SFPB ===> SEEN=1 COLO=4 <===                       962
         CALL AGSRESET                                                    963
         AG_SEEN = 1                                                      963
         AG_COLO = 4                                                      963
         AG_TITLE = 'SFPB'                                                963
         AG_EXNAME = 'ATTRIBUTE'                                          963
         AG_PARLIST = 'SEEN_COLO'                                         963
         CALL AXATTRIBUTE                                                 963
C    *                                                                    963
C    Geant SHAPE : BOX ===> DX=SFJP_SFPB_DX/2. DY=SFJP_SFPB_DY/2., DZ=SF  963
C    JP_SFPBDZ/2. <===                                                    963
         CALL AGSRESET                                                    965
         AG_DX = SFJP_SFPB_DX/2.                                          965
         AG_DY = SFJP_SFPB_DY/2.                                          965
         AG_DZ = SFJP_SFPBDZ/2.                                           965
         AG_TITLE = 'BOX'                                                 965
         AG_EXNAME = 'SHAPE'                                              965
         AG_PARLIST = 'DX_DY_DZ'                                          965
         CALL AXSHAPE                                                     965
C    *                                                                    965
5701  CALL AGSPOPB                                                        966
      GO TO BLOCK_SFPB, (5700 ,5710)                                      966
C End of Local Procedure                                                  966
C ----------------------------------------------------------------------  967
C ------------ SSBS                                                       967
C Begin Local Procedure SSBS                                              969
C SSBS IS THE SMALL PART OF THE ALUMINIUM PLATE LINKING THE LADDER TO, T  969
C HE SECTOR                                                               969
5720  CONTINUE                                                            969
         AG_TITLE='SSBS'                                                  969
         AG_MODULE=MODULE                                                 969
         AG_MARK='V00'                                                    969
         CALL AGSPUSH                                                     969
         AG_MODULE=MODULE                                                 969
C    *                                                                    969
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        970
         CALL AGSRESET                                                    970
         AG_TITLE = 'ALUMINIUM'                                           970
         AG_EXNAME = 'MATERIAL'                                           970
         AG_PARLIST = 'NONE'                                              970
         CALL AXMATERIAL                                                  970
C    *                                                                    970
C    Geant ATTRIBUTE : SSBS ===> SEEN=1 COLO=1 <===                       970
         CALL AGSRESET                                                    971
         AG_SEEN = 1                                                      971
         AG_COLO = 1                                                      971
         AG_TITLE = 'SSBS'                                                971
         AG_EXNAME = 'ATTRIBUTE'                                          971
         AG_PARLIST = 'SEEN_COLO'                                         971
         CALL AXATTRIBUTE                                                 971
C    *                                                                    971
C    Geant SHAPE : BOX ===> DX=SFJP_SSBS_DX/2. DY=SFJP_SSBS_DY/2., DZ=SF  971
C    JP_SSBS_DX/2. <===                                                   971
         CALL AGSRESET                                                    973
         AG_DX = SFJP_SSBS_DX/2.                                          973
         AG_DY = SFJP_SSBS_DY/2.                                          973
         AG_DZ = SFJP_SSBS_DX/2.                                          973
         AG_TITLE = 'BOX'                                                 973
         AG_EXNAME = 'SHAPE'                                              973
         AG_PARLIST = 'DX_DY_DZ'                                          973
         CALL AXSHAPE                                                     973
C    *                                                                    973
5721  CALL AGSPOPB                                                        974
      GO TO BLOCK_SSBS, (5720 ,5730)                                      974
C End of Local Procedure                                                  974
C ----------------------------------------------------------------------  975
C ------------ SSBB                                                       975
C Begin Local Procedure SSBB                                              977
C SSBB IS THE BIG PART OF THE ALUMINIUM PLATE LINKING THE LADDER TO THE,  977
C  SECTOR                                                                 977
5740  CONTINUE                                                            977
         AG_TITLE='SSBB'                                                  977
         AG_MODULE=MODULE                                                 977
         AG_MARK='V00'                                                    977
         CALL AGSPUSH                                                     977
         AG_MODULE=MODULE                                                 977
C    *                                                                    977
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        978
         CALL AGSRESET                                                    978
         AG_TITLE = 'ALUMINIUM'                                           978
         AG_EXNAME = 'MATERIAL'                                           978
         AG_PARLIST = 'NONE'                                              978
         CALL AXMATERIAL                                                  978
C    *                                                                    978
C    Geant ATTRIBUTE : SSBB ===> SEEN=1 COLO=1 <===                       978
         CALL AGSRESET                                                    979
         AG_SEEN = 1                                                      979
         AG_COLO = 1                                                      979
         AG_TITLE = 'SSBB'                                                979
         AG_EXNAME = 'ATTRIBUTE'                                          979
         AG_PARLIST = 'SEEN_COLO'                                         979
         CALL AXATTRIBUTE                                                 979
C    *                                                                    979
C    Geant SHAPE : BOX ===> DX=SFJP_SSBB_DX/2. DY=SFJP_SSBS_DY/2., DZ=SF  979
C    JP_SSBB_DZ/2. <===                                                   979
         CALL AGSRESET                                                    981
         AG_DX = SFJP_SSBB_DX/2.                                          981
         AG_DY = SFJP_SSBS_DY/2.                                          981
         AG_DZ = SFJP_SSBB_DZ/2.                                          981
         AG_TITLE = 'BOX'                                                 981
         AG_EXNAME = 'SHAPE'                                              981
         AG_PARLIST = 'DX_DY_DZ'                                          981
         CALL AXSHAPE                                                     981
C    *                                                                    981
5741  CALL AGSPOPB                                                        982
      GO TO BLOCK_SSBB, (5740 ,5750)                                      982
C End of Local Procedure                                                  982
C ----------------------------------------------------------------------  983
C ------------ SSST                                                       983
C Begin Local Procedure SSST                                              984
C SSST IS THE TOP OF THE SMALL SECTOR                                     984
5070  CONTINUE                                                            984
         AG_TITLE='SSST'                                                  984
         AG_MODULE=MODULE                                                 984
         AG_MARK='V00'                                                    984
         CALL AGSPUSH                                                     984
         AG_MODULE=MODULE                                                 984
C    *                                                                    984
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        985
         CALL AGSRESET                                                    985
         AG_TITLE = 'ALUMINIUM'                                           985
         AG_EXNAME = 'MATERIAL'                                           985
         AG_PARLIST = 'NONE'                                              985
         CALL AXMATERIAL                                                  985
C    *                                                                    985
C    Geant ATTRIBUTE : SSST ===> SEEN=1 COLO=1 <===                       985
         CALL AGSRESET                                                    986
         AG_SEEN = 1                                                      986
         AG_COLO = 1                                                      986
         AG_TITLE = 'SSST'                                                986
         AG_EXNAME = 'ATTRIBUTE'                                          986
         AG_PARLIST = 'SEEN_COLO'                                         986
         CALL AXATTRIBUTE                                                 986
C    *                                                                    986
C    Geant SHAPE : TUBS ===> RMIN=SFJP_SSST_RMIN RMAX=SFJP_SSST_RMAX, PH  986
C    I1=SFJP_ALPHAZH PHI2=(180.-SFJP_ALPHAZH), DZ=SFJP_SSST_WIDTH/2. <==  986
C    =                                                                    986
         CALL AGSRESET                                                    989
         AG_RMIN = SFJP_SSST_RMIN                                         989
         AG_RMAX = SFJP_SSST_RMAX                                         989
         AG_PHI1 = SFJP_ALPHAZH                                           989
         AG_PHI2 = (180.-SFJP_ALPHAZH)                                    989
         AG_DZ = SFJP_SSST_WIDTH/2.                                       989
         AG_TITLE = 'TUBS'                                                989
         AG_EXNAME = 'SHAPE'                                              989
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            989
         CALL AXSHAPE                                                     989
C    *                                                                    989
5071  CALL AGSPOPB                                                        990
      GO TO BLOCK_SSST, (5070 ,5080)                                      990
C End of Local Procedure                                                  990
C ----------------------------------------------------------------------  991
C ------------ SSSS                                                       991
C Begin Local Procedure SSSS                                              992
C SSSS IS THE SIDE OF THE SMALL SECTOR                                    992
5090  CONTINUE                                                            992
         AG_TITLE='SSSS'                                                  992
         AG_MODULE=MODULE                                                 992
         AG_MARK='V00'                                                    992
         CALL AGSPUSH                                                     992
         AG_MODULE=MODULE                                                 992
C    *                                                                    992
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        993
         CALL AGSRESET                                                    993
         AG_TITLE = 'ALUMINIUM'                                           993
         AG_EXNAME = 'MATERIAL'                                           993
         AG_PARLIST = 'NONE'                                              993
         CALL AXMATERIAL                                                  993
C    *                                                                    993
C    Geant ATTRIBUTE : SSSS ===> SEEN=1 COLO=1 <===                       993
         CALL AGSRESET                                                    994
         AG_SEEN = 1                                                      994
         AG_COLO = 1                                                      994
         AG_TITLE = 'SSSS'                                                994
         AG_EXNAME = 'ATTRIBUTE'                                          994
         AG_PARLIST = 'SEEN_COLO'                                         994
         CALL AXATTRIBUTE                                                 994
C    *                                                                    994
C    Geant SHAPE : TUBS ===> RMIN=SFJP_SSSS_RMIN RMAX=SFJP_SSST_RMIN, PH  994
C    I1=SFJP_ALPHAZH PHI2=(180.-SFJP_ALPHAZH), DZ=SFJP_SSSS_WIDTH/2. <==  994
C    =                                                                    994
         CALL AGSRESET                                                    997
         AG_RMIN = SFJP_SSSS_RMIN                                         997
         AG_RMAX = SFJP_SSST_RMIN                                         997
         AG_PHI1 = SFJP_ALPHAZH                                           997
         AG_PHI2 = (180.-SFJP_ALPHAZH)                                    997
         AG_DZ = SFJP_SSSS_WIDTH/2.                                       997
         AG_TITLE = 'TUBS'                                                997
         AG_EXNAME = 'SHAPE'                                              997
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            997
         CALL AXSHAPE                                                     997
C    *                                                                    997
5091  CALL AGSPOPB                                                        998
      GO TO BLOCK_SSSS, (5090 ,5100)                                      998
C End of Local Procedure                                                  998
C ----------------------------------------------------------------------  999
C ------------ SSRT                                                       999
C Begin Local Procedure SSRT                                              1000
C SSRT IS THE TOP OF THE SIDE RIB                                         1000
5110  CONTINUE                                                            1000
         AG_TITLE='SSRT'                                                  1000
         AG_MODULE=MODULE                                                 1000
         AG_MARK='V00'                                                    1000
         CALL AGSPUSH                                                     1000
         AG_MODULE=MODULE                                                 1000
C    *                                                                    1000
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1001
         CALL AGSRESET                                                    1001
         AG_TITLE = 'ALUMINIUM'                                           1001
         AG_EXNAME = 'MATERIAL'                                           1001
         AG_PARLIST = 'NONE'                                              1001
         CALL AXMATERIAL                                                  1001
C    *                                                                    1001
C    Geant ATTRIBUTE : SSRT ===> SEEN=1 COLO=1 <===                       1001
         CALL AGSRESET                                                    1002
         AG_SEEN = 1                                                      1002
         AG_COLO = 1                                                      1002
         AG_TITLE = 'SSRT'                                                1002
         AG_EXNAME = 'ATTRIBUTE'                                          1002
         AG_PARLIST = 'SEEN_COLO'                                         1002
         CALL AXATTRIBUTE                                                 1002
C    *                                                                    1002
C    Geant SHAPE : TUBS ===> RMIN=SFJP_SSST_RMIN RMAX=SFJP_SSST_RMAX, PH  1002
C    I1=-SFJP_ALPHAZH PHI2=+SFJP_ALPHAZH, DZ=SFJP_SSST_WIDTH/2. <===      1002
         CALL AGSRESET                                                    1005
         AG_RMIN = SFJP_SSST_RMIN                                         1005
         AG_RMAX = SFJP_SSST_RMAX                                         1005
         AG_PHI1 = -SFJP_ALPHAZH                                          1005
         AG_PHI2 = +SFJP_ALPHAZH                                          1005
         AG_DZ = SFJP_SSST_WIDTH/2.                                       1005
         AG_TITLE = 'TUBS'                                                1005
         AG_EXNAME = 'SHAPE'                                              1005
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1005
         CALL AXSHAPE                                                     1005
C    *                                                                    1005
5111  CALL AGSPOPB                                                        1006
      GO TO BLOCK_SSRT, (5110 ,5120)                                      1006
C End of Local Procedure                                                  1006
C ----------------------------------------------------------------------  1007
C ------------ SSRS                                                       1007
C Begin Local Procedure SSRS                                              1008
C SSRS IS THE SIDE OF THE SMALL RIB                                       1008
5130  CONTINUE                                                            1008
         AG_TITLE='SSRS'                                                  1008
         AG_MODULE=MODULE                                                 1008
         AG_MARK='V00'                                                    1008
         CALL AGSPUSH                                                     1008
         AG_MODULE=MODULE                                                 1008
C    *                                                                    1008
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1009
         CALL AGSRESET                                                    1009
         AG_TITLE = 'ALUMINIUM'                                           1009
         AG_EXNAME = 'MATERIAL'                                           1009
         AG_PARLIST = 'NONE'                                              1009
         CALL AXMATERIAL                                                  1009
C    *                                                                    1009
C    Geant ATTRIBUTE : SSRS ===> SEEN=1 COLO=1 <===                       1009
         CALL AGSRESET                                                    1010
         AG_SEEN = 1                                                      1010
         AG_COLO = 1                                                      1010
         AG_TITLE = 'SSRS'                                                1010
         AG_EXNAME = 'ATTRIBUTE'                                          1010
         AG_PARLIST = 'SEEN_COLO'                                         1010
         CALL AXATTRIBUTE                                                 1010
C    *                                                                    1010
C    Geant SHAPE : TUBS ===> RMIN=SFJP_SSRS_RMIN RMAX=SFJP_SSST_RMIN, PH  1010
C    I1=-SFJP_ALPHAZH PHI2=+SFJP_ALPHAZH, DZ=SFJP_SSSS_WIDTH/2. <===      1010
         CALL AGSRESET                                                    1013
         AG_RMIN = SFJP_SSRS_RMIN                                         1013
         AG_RMAX = SFJP_SSST_RMIN                                         1013
         AG_PHI1 = -SFJP_ALPHAZH                                          1013
         AG_PHI2 = +SFJP_ALPHAZH                                          1013
         AG_DZ = SFJP_SSSS_WIDTH/2.                                       1013
         AG_TITLE = 'TUBS'                                                1013
         AG_EXNAME = 'SHAPE'                                              1013
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1013
         CALL AXSHAPE                                                     1013
C    *                                                                    1013
5131  CALL AGSPOPB                                                        1014
      GO TO BLOCK_SSRS, (5130 ,5140)                                      1014
C End of Local Procedure                                                  1014
C ----------------------------------------------------------------------  1015
C ------------ SSLB                                                       1015
C Begin Local Procedure SSLB                                              1016
C SSLB IS THE LINKING (SECTOR TO THE CONE) BOX                            1016
5150  CONTINUE                                                            1016
         AG_TITLE='SSLB'                                                  1016
         AG_MODULE=MODULE                                                 1016
         AG_MARK='V00'                                                    1016
         CALL AGSPUSH                                                     1016
         AG_MODULE=MODULE                                                 1016
C    *                                                                    1016
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1017
         CALL AGSRESET                                                    1017
         AG_TITLE = 'ALUMINIUM'                                           1017
         AG_EXNAME = 'MATERIAL'                                           1017
         AG_PARLIST = 'NONE'                                              1017
         CALL AXMATERIAL                                                  1017
C    *                                                                    1017
C    Geant ATTRIBUTE : SSLB ===> SEEN=1 COLO=1 <===                       1017
         CALL AGSRESET                                                    1018
         AG_SEEN = 1                                                      1018
         AG_COLO = 1                                                      1018
         AG_TITLE = 'SSLB'                                                1018
         AG_EXNAME = 'ATTRIBUTE'                                          1018
         AG_PARLIST = 'SEEN_COLO'                                         1018
         CALL AXATTRIBUTE                                                 1018
C    *                                                                    1018
C    Geant SHAPE : BOX ===> DX=SFJP_SSLB_DX/2. DY=SFJP_SSLB_DY/2., DZ=SF  1018
C    JP_SSLB_DZ/2. <===                                                   1018
         CALL AGSRESET                                                    1020
         AG_DX = SFJP_SSLB_DX/2.                                          1020
         AG_DY = SFJP_SSLB_DY/2.                                          1020
         AG_DZ = SFJP_SSLB_DZ/2.                                          1020
         AG_TITLE = 'BOX'                                                 1020
         AG_EXNAME = 'SHAPE'                                              1020
         AG_PARLIST = 'DX_DY_DZ'                                          1020
         CALL AXSHAPE                                                     1020
C    *                                                                    1020
5151  CALL AGSPOPB                                                        1021
      GO TO BLOCK_SSLB, (5150 ,5160)                                      1021
C End of Local Procedure                                                  1021
C ----------------------------------------------------------------------  1022
C ------------ SSLT                                                       1022
C Begin Local Procedure SSLT                                              1023
C SSLT IS THE LINKING (SECTOR TO THE CONE) TUBE                           1023
5170  CONTINUE                                                            1023
         AG_TITLE='SSLT'                                                  1023
         AG_MODULE=MODULE                                                 1023
         AG_MARK='V00'                                                    1023
         CALL AGSPUSH                                                     1023
         AG_MODULE=MODULE                                                 1023
C    *                                                                    1023
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1024
         CALL AGSRESET                                                    1024
         AG_TITLE = 'ALUMINIUM'                                           1024
         AG_EXNAME = 'MATERIAL'                                           1024
         AG_PARLIST = 'NONE'                                              1024
         CALL AXMATERIAL                                                  1024
C    *                                                                    1024
C    Geant ATTRIBUTE : SSLT ===> SEEN=1 COLO=1 <===                       1024
         CALL AGSRESET                                                    1025
         AG_SEEN = 1                                                      1025
         AG_COLO = 1                                                      1025
         AG_TITLE = 'SSLT'                                                1025
         AG_EXNAME = 'ATTRIBUTE'                                          1025
         AG_PARLIST = 'SEEN_COLO'                                         1025
         CALL AXATTRIBUTE                                                 1025
C    *                                                                    1025
C    Geant SHAPE : TUBE ===> RMIN=0. RMAX=SFJP_SSLT_RMAX DZ=SFJP_SSLT_DZ  1025
C    /2. <===                                                             1025
         CALL AGSRESET                                                    1026
         AG_RMIN = 0.                                                     1026
         AG_RMAX = SFJP_SSLT_RMAX                                         1026
         AG_DZ = SFJP_SSLT_DZ/2.                                          1026
         AG_TITLE = 'TUBE'                                                1026
         AG_EXNAME = 'SHAPE'                                              1026
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1026
         CALL AXSHAPE                                                     1026
C    *                                                                    1026
5171  CALL AGSPOPB                                                        1027
      GO TO BLOCK_SSLT, (5170 ,5180)                                      1027
C End of Local Procedure                                                  1027
C ----------------------------------------------------------------------  1028
C ------------ SCMP                                                       1028
C Begin Local Procedure SCMP                                              1029
C SCMP IS THE MOUNTING PLATE INSERTED IN THE CONES.                       1029
5190  CONTINUE                                                            1029
         AG_TITLE='SCMP'                                                  1029
         AG_MODULE=MODULE                                                 1029
         AG_MARK='V00'                                                    1029
         CALL AGSPUSH                                                     1029
         AG_MODULE=MODULE                                                 1029
C    *                                                                    1029
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1030
         CALL AGSRESET                                                    1030
         AG_TITLE = 'ALUMINIUM'                                           1030
         AG_EXNAME = 'MATERIAL'                                           1030
         AG_PARLIST = 'NONE'                                              1030
         CALL AXMATERIAL                                                  1030
C    *                                                                    1030
C    Geant ATTRIBUTE : SCMP ===> SEEN=1 COLO=1 <===                       1030
         CALL AGSRESET                                                    1031
         AG_SEEN = 1                                                      1031
         AG_COLO = 1                                                      1031
         AG_TITLE = 'SCMP'                                                1031
         AG_EXNAME = 'ATTRIBUTE'                                          1031
         AG_PARLIST = 'SEEN_COLO'                                         1031
         CALL AXATTRIBUTE                                                 1031
C    *                                                                    1031
C    Geant SHAPE : BOX ===> DX=SFJP_SCMP_DX/2. DY=SFJP_SCMP_DY/2., DZ=SF  1031
C    JP_SCMP_DZ/2 <===                                                    1031
         CALL AGSRESET                                                    1033
         AG_DX = SFJP_SCMP_DX/2.                                          1033
         AG_DY = SFJP_SCMP_DY/2.                                          1033
         AG_DZ = SFJP_SCMP_DZ/2                                           1033
         AG_TITLE = 'BOX'                                                 1033
         AG_EXNAME = 'SHAPE'                                              1033
         AG_PARLIST = 'DX_DY_DZ'                                          1033
         CALL AXSHAPE                                                     1033
C    *                                                                    1033
5191  CALL AGSPOPB                                                        1034
      GO TO BLOCK_SCMP, (5190 ,5200)                                      1034
C End of Local Procedure                                                  1034
C ----------------------------------------------------------------------  1035
C ------------ SCVM                                                       1035
C Begin Local Procedure SCVM                                              1036
C SCVM IS THE MOTHER VOLUME OF THE V-SHAPE PIECE                          1036
5210  CONTINUE                                                            1036
         AG_TITLE='SCVM'                                                  1036
         AG_MODULE=MODULE                                                 1036
         AG_MARK='V00'                                                    1036
         CALL AGSPUSH                                                     1036
         AG_MODULE=MODULE                                                 1036
C    *                                                                    1036
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1037
         CALL AGSRESET                                                    1037
         AG_TITLE = 'AIR'                                                 1037
         AG_EXNAME = 'MATERIAL'                                           1037
         AG_PARLIST = 'NONE'                                              1037
         CALL AXMATERIAL                                                  1037
C    *                                                                    1037
C    Geant ATTRIBUTE : SCVM ===> SEEN=0 COLO=6 <===                       1037
         CALL AGSRESET                                                    1038
         AG_SEEN = 0                                                      1038
         AG_COLO = 6                                                      1038
         AG_TITLE = 'SCVM'                                                1038
         AG_EXNAME = 'ATTRIBUTE'                                          1038
         AG_PARLIST = 'SEEN_COLO'                                         1038
         CALL AXATTRIBUTE                                                 1038
C    *                                                                    1038
C    Geant SHAPE : BOX ===> DX=SFJP_SCVM_DX/2. DY=SFJP_SCVM_DY/2., DZ=SF  1038
C    JP_SCVM_DZ/2 <===                                                    1038
         CALL AGSRESET                                                    1040
         AG_DX = SFJP_SCVM_DX/2.                                          1040
         AG_DY = SFJP_SCVM_DY/2.                                          1040
         AG_DZ = SFJP_SCVM_DZ/2                                           1040
         AG_TITLE = 'BOX'                                                 1040
         AG_EXNAME = 'SHAPE'                                              1040
         AG_PARLIST = 'DX_DY_DZ'                                          1040
         CALL AXSHAPE                                                     1040
C    *                                                                    1040
C    Create SCVB :                                                        1041
C    Call To Local Procedure SCVB                                         1041
         ASSIGN 6040 TO BLOCK_SCVB                                        1041
         GO TO  6030                                                      1041
6040     CONTINUE                                                         1041
C    *                                                                    1041
C    Geant POSITION : SCVB ===> X=0. Y=-SFJP_SCVM_DY/2.+SFJP_SCVB_DY/2.   1042
C    Z=0. <===                                                            1042
         CALL AGSRESET                                                    1042
         AG_X = 0.                                                        1042
         AG_Y = -SFJP_SCVM_DY/2.+SFJP_SCVB_DY/2.                          1042
         AG_Z = 0.                                                        1042
         AG_TITLE = 'SCVB'                                                1042
         AG_EXNAME = 'POSITION'                                           1042
         AG_PARLIST = 'X_Y_Z'                                             1042
         CALL AXPOSITION                                                  1042
C    *                                                                    1042
C    Create SCVS :                                                        1043
C    Call To Local Procedure SCVS                                         1043
         ASSIGN 6060 TO BLOCK_SCVS                                        1043
         GO TO  6050                                                      1043
6060     CONTINUE                                                         1043
C    *                                                                    1043
C    Geant POSITION : SCVS ===> X=SFJP_SCVS_PX Y=SFJP_SCVS_PY Z=0.0 ALPH  1044
C    AZ=SFJP_ALPHAZH <===                                                 1044
         CALL AGSRESET                                                    1044
         AG_X = SFJP_SCVS_PX                                              1044
         AG_Y = SFJP_SCVS_PY                                              1044
         AG_Z = 0.0                                                       1044
         AG_ALPHAN=AG_ALPHAN+1                                            1044
         AG_ALPHAK(AG_ALPHAN)=3                                           1044
         AG_ALPHAL(AG_ALPHAN)=SFJP_ALPHAZH                                1044
         AG_TITLE = 'SCVS'                                                1044
         AG_EXNAME = 'POSITION'                                           1044
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      1044
         CALL AXPOSITION                                                  1044
C    *                                                                    1044
C    Geant POSITION : SCVS ===> X=-SFJP_SCVS_PX Y=SFJP_SCVS_PY Z=0.0 ALP  1044
C    HAZ=-SFJP_ALPHAZH <===                                               1044
         CALL AGSRESET                                                    1045
         AG_X = -SFJP_SCVS_PX                                             1045
         AG_Y = SFJP_SCVS_PY                                              1045
         AG_Z = 0.0                                                       1045
         AG_ALPHAN=AG_ALPHAN+1                                            1045
         AG_ALPHAK(AG_ALPHAN)=3                                           1045
         AG_ALPHAL(AG_ALPHAN)=-SFJP_ALPHAZH                               1045
         AG_TITLE = 'SCVS'                                                1045
         AG_EXNAME = 'POSITION'                                           1045
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      1045
         CALL AXPOSITION                                                  1045
C    *                                                                    1045
5211  CALL AGSPOPB                                                        1046
      GO TO BLOCK_SCVM, (5210 ,5220)                                      1046
C End of Local Procedure                                                  1046
C ----------------------------------------------------------------------  1047
C ------------ SCVB                                                       1047
C Begin Local Procedure SCVB                                              1048
C SCVB IS THE BASE PLATE OF THE V-SHAPE PIECE                             1048
6030  CONTINUE                                                            1048
         AG_TITLE='SCVB'                                                  1048
         AG_MODULE=MODULE                                                 1048
         AG_MARK='V00'                                                    1048
         CALL AGSPUSH                                                     1048
         AG_MODULE=MODULE                                                 1048
C    *                                                                    1048
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1049
         CALL AGSRESET                                                    1049
         AG_TITLE = 'ALUMINIUM'                                           1049
         AG_EXNAME = 'MATERIAL'                                           1049
         AG_PARLIST = 'NONE'                                              1049
         CALL AXMATERIAL                                                  1049
C    *                                                                    1049
C    Geant ATTRIBUTE : SCVB ===> SEEN=1 COLO=1 <===                       1049
         CALL AGSRESET                                                    1050
         AG_SEEN = 1                                                      1050
         AG_COLO = 1                                                      1050
         AG_TITLE = 'SCVB'                                                1050
         AG_EXNAME = 'ATTRIBUTE'                                          1050
         AG_PARLIST = 'SEEN_COLO'                                         1050
         CALL AXATTRIBUTE                                                 1050
C    *                                                                    1050
C    Geant SHAPE : BOX ===> DX=SFJP_SCVM_DX/2. DY=SFJP_SCVB_DY/2., DZ=SF  1050
C    JP_SCVM_DZ/2 <===                                                    1050
         CALL AGSRESET                                                    1052
         AG_DX = SFJP_SCVM_DX/2.                                          1052
         AG_DY = SFJP_SCVB_DY/2.                                          1052
         AG_DZ = SFJP_SCVM_DZ/2                                           1052
         AG_TITLE = 'BOX'                                                 1052
         AG_EXNAME = 'SHAPE'                                              1052
         AG_PARLIST = 'DX_DY_DZ'                                          1052
         CALL AXSHAPE                                                     1052
C    *                                                                    1052
6031  CALL AGSPOPB                                                        1053
      GO TO BLOCK_SCVB, (6030 ,6040)                                      1053
C End of Local Procedure                                                  1053
C ----------------------------------------------------------------------  1054
C ------------ SCVS                                                       1054
C Begin Local Procedure SCVS                                              1055
C SCVS IS THE SIDE PLATE OF THE V-SHAPE PIECE                             1055
6050  CONTINUE                                                            1055
         AG_TITLE='SCVS'                                                  1055
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
C    Geant ATTRIBUTE : SCVS ===> SEEN=1 COLO=1 <===                       1056
         CALL AGSRESET                                                    1057
         AG_SEEN = 1                                                      1057
         AG_COLO = 1                                                      1057
         AG_TITLE = 'SCVS'                                                1057
         AG_EXNAME = 'ATTRIBUTE'                                          1057
         AG_PARLIST = 'SEEN_COLO'                                         1057
         CALL AXATTRIBUTE                                                 1057
C    *                                                                    1057
C    Geant SHAPE : BOX ===> DX=SFJP_SCVS_DX/2. DY=SFJP_SCVS_DY/2., DZ=SF  1057
C    JP_SCVM_DZ/2 <===                                                    1057
         CALL AGSRESET                                                    1059
         AG_DX = SFJP_SCVS_DX/2.                                          1059
         AG_DY = SFJP_SCVS_DY/2.                                          1059
         AG_DZ = SFJP_SCVM_DZ/2                                           1059
         AG_TITLE = 'BOX'                                                 1059
         AG_EXNAME = 'SHAPE'                                              1059
         AG_PARLIST = 'DX_DY_DZ'                                          1059
         CALL AXSHAPE                                                     1059
C    *                                                                    1059
6051  CALL AGSPOPB                                                        1060
      GO TO BLOCK_SCVS, (6050 ,6060)                                      1060
C End of Local Procedure                                                  1060
      WRITE(LOUT,6070)'===>>  [End AgML Module SISDGEO2]  <<==='          1060
6070  FORMAT(A32,/,/)                                                     1060
C MODULE SISDGEO2                                                         1062
      END                                                                 1062
