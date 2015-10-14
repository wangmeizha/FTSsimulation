C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE SISDGEO1                                                 1
C SISDGEO1 IS THE SILICON STRIP DETECTOR                                  1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'SISDGEO1'/, BANK_TITLE/'*'/                            1
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
C CREATED 23 APR 04                                                       2
      CHARACTER*40 CREATED /'23 APR 04'/                                  3
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
C -                                                                       12
      REAL SFPB_HHIGHT                                                    12
      EQUIVALENCE (BANK_SFPB(3),SFPB_HHIGHT,RE_SFPB_HHIGHT),              12
     *(NAMES_SFPB(1),NSFPB_HHIGHT)                                        12
      EQUIVALENCE (COMMEN_SFPB(1),CSFPB_HHIGHT),(MAP_SFPB(1,1),           12
     *MSFPB_HHIGHT)                                                       12
      CHARACTER CSFPB_HHIGHT*40/' '/                                      12
      CHARACTER NSFPB_HHIGHT*12/'R.HHIGHT'/                               12
      INTEGER MSFPB_HHIGHT(2)/1,1/                                        12
      REAL RE_SFPB_HHIGHT                                                 12
C -                                                                       12
      REAL SFPB_KHIGHT                                                    12
      EQUIVALENCE (BANK_SFPB(4),SFPB_KHIGHT,RE_SFPB_KHIGHT),              12
     *(NAMES_SFPB(2),NSFPB_KHIGHT)                                        12
      EQUIVALENCE (COMMEN_SFPB(2),CSFPB_KHIGHT),(MAP_SFPB(1,2),           12
     *MSFPB_KHIGHT)                                                       12
      CHARACTER CSFPB_KHIGHT*40/' '/                                      12
      CHARACTER NSFPB_KHIGHT*12/'R.KHIGHT'/                               12
      INTEGER MSFPB_KHIGHT(2)/1,1/                                        12
      REAL RE_SFPB_KHIGHT                                                 12
C -                                                                       12
      REAL SFPB_HBASE                                                     12
      EQUIVALENCE (BANK_SFPB(5),SFPB_HBASE,RE_SFPB_HBASE),                12
     *(NAMES_SFPB(3),NSFPB_HBASE)                                         12
      EQUIVALENCE (COMMEN_SFPB(3),CSFPB_HBASE),(MAP_SFPB(1,3),            12
     *MSFPB_HBASE)                                                        12
      CHARACTER CSFPB_HBASE*40/' '/                                       12
      CHARACTER NSFPB_HBASE*12/'R.HBASE'/                                 12
      INTEGER MSFPB_HBASE(2)/1,1/                                         12
      REAL RE_SFPB_HBASE                                                  12
C -                                                                       12
      REAL SFPB_KBASE                                                     12
      EQUIVALENCE (BANK_SFPB(6),SFPB_KBASE,RE_SFPB_KBASE),                12
     *(NAMES_SFPB(4),NSFPB_KBASE)                                         12
      EQUIVALENCE (COMMEN_SFPB(4),CSFPB_KBASE),(MAP_SFPB(1,4),            12
     *MSFPB_KBASE)                                                        12
      CHARACTER CSFPB_KBASE*40/' '/                                       12
      CHARACTER NSFPB_KBASE*12/'R.KBASE'/                                 12
      INTEGER MSFPB_KBASE(2)/1,1/                                         12
      REAL RE_SFPB_KBASE                                                  12
C -                                                                       12
      REAL SFPB_FSIZE                                                     12
      EQUIVALENCE (BANK_SFPB(7),SFPB_FSIZE,RE_SFPB_FSIZE),                12
     *(NAMES_SFPB(5),NSFPB_FSIZE)                                         12
      EQUIVALENCE (COMMEN_SFPB(5),CSFPB_FSIZE),(MAP_SFPB(1,5),            12
     *MSFPB_FSIZE)                                                        12
      CHARACTER CSFPB_FSIZE*40/' '/                                       12
      CHARACTER NSFPB_FSIZE*12/'R.FSIZE'/                                 12
      INTEGER MSFPB_FSIZE(2)/1,1/                                         12
      REAL RE_SFPB_FSIZE                                                  12
C -                                                                       12
      REAL SFPB_ZCOOR                                                     12
      EQUIVALENCE (BANK_SFPB(8),SFPB_ZCOOR,RE_SFPB_ZCOOR),                12
     *(NAMES_SFPB(6),NSFPB_ZCOOR)                                         12
      EQUIVALENCE (COMMEN_SFPB(6),CSFPB_ZCOOR),(MAP_SFPB(1,6),            12
     *MSFPB_ZCOOR)                                                        12
      CHARACTER CSFPB_ZCOOR*40/' '/                                       12
      CHARACTER NSFPB_ZCOOR*12/'R.ZCOOR'/                                 12
      INTEGER MSFPB_ZCOOR(2)/1,1/                                         12
      REAL RE_SFPB_ZCOOR                                                  12
C -                                                                       12
      INTEGER LEN_SFPB,LLEN_SFPB,LINK_SFPB                                12
      PARAMETER (LEN_SFPB=8)                                              12
      PARAMETER (LLEN_SFPB=6)                                             12
      INTEGER FLAG_SFPB/0/,MAP_SFPB(2,LLEN_SFPB),IDNUM_SFPB(10)           12
      CHARACTER NAMES_SFPB(LLEN_SFPB)*12,COMMEN_SFPB(LLEN_SFPB)*40        12
      DATA IDNUM_SFPB/10*0/                                               12
      REAL BANK_SFPB(LEN_SFPB)                                            12
      CHARACTER*60 BPATH_SFPB/' '/                                        12
      SAVE FLAG_SFPB,MAP_SFPB,IDNUM_SFPB,NAMES_SFPB,COMMEN_SFPB,          12
     *BPATH_SFPB                                                          12
      DATA LINK_SFPB/0/,BANK_SFPB/LEN_SFPB*0/                             12
C *                                                                       12
C -                                                                       15
      REAL SFPA_VERSION                                                   15
      EQUIVALENCE (BANK_SFPA(3),SFPA_VERSION,RE_SFPA_VERSION),            15
     *(NAMES_SFPA(1),NSFPA_VERSION)                                       15
      EQUIVALENCE (COMMEN_SFPA(1),CSFPA_VERSION),(MAP_SFPA(1,1),          15
     *MSFPA_VERSION)                                                      15
      CHARACTER CSFPA_VERSION*40/' '/                                     15
      CHARACTER NSFPA_VERSION*12/'R.VERSION'/                             15
      INTEGER MSFPA_VERSION(2)/1,1/                                       15
      REAL RE_SFPA_VERSION                                                15
C -                                                                       15
      REAL SFPA_RMIN                                                      15
      EQUIVALENCE (BANK_SFPA(4),SFPA_RMIN,RE_SFPA_RMIN),(NAMES_SFPA(2),   15
     *NSFPA_RMIN)                                                         15
      EQUIVALENCE (COMMEN_SFPA(2),CSFPA_RMIN),(MAP_SFPA(1,2),             15
     *MSFPA_RMIN)                                                         15
      CHARACTER CSFPA_RMIN*40/' '/                                        15
      CHARACTER NSFPA_RMIN*12/'R.RMIN'/                                   15
      INTEGER MSFPA_RMIN(2)/1,1/                                          15
      REAL RE_SFPA_RMIN                                                   15
C -                                                                       15
      REAL SFPA_RMAX                                                      15
      EQUIVALENCE (BANK_SFPA(5),SFPA_RMAX,RE_SFPA_RMAX),(NAMES_SFPA(3),   15
     *NSFPA_RMAX)                                                         15
      EQUIVALENCE (COMMEN_SFPA(3),CSFPA_RMAX),(MAP_SFPA(1,3),             15
     *MSFPA_RMAX)                                                         15
      CHARACTER CSFPA_RMAX*40/' '/                                        15
      CHARACTER NSFPA_RMAX*12/'R.RMAX'/                                   15
      INTEGER MSFPA_RMAX(2)/1,1/                                          15
      REAL RE_SFPA_RMAX                                                   15
C -                                                                       15
      REAL SFPA_LEN                                                       15
      EQUIVALENCE (BANK_SFPA(6),SFPA_LEN,RE_SFPA_LEN),(NAMES_SFPA(4),     15
     *NSFPA_LEN)                                                          15
      EQUIVALENCE (COMMEN_SFPA(4),CSFPA_LEN),(MAP_SFPA(1,4),MSFPA_LEN)    15
      CHARACTER CSFPA_LEN*40/' '/                                         15
      CHARACTER NSFPA_LEN*12/'R.LEN'/                                     15
      INTEGER MSFPA_LEN(2)/1,1/                                           15
      REAL RE_SFPA_LEN                                                    15
C -                                                                       15
      REAL SFPA_RAD                                                       15
      EQUIVALENCE (BANK_SFPA(7),SFPA_RAD,RE_SFPA_RAD),(NAMES_SFPA(5),     15
     *NSFPA_RAD)                                                          15
      EQUIVALENCE (COMMEN_SFPA(5),CSFPA_RAD),(MAP_SFPA(1,5),MSFPA_RAD)    15
      CHARACTER CSFPA_RAD*40/' '/                                         15
      CHARACTER NSFPA_RAD*12/'R.RAD'/                                     15
      INTEGER MSFPA_RAD(2)/1,1/                                           15
      REAL RE_SFPA_RAD                                                    15
C -                                                                       15
      REAL SFPA_NSSD                                                      15
      EQUIVALENCE (BANK_SFPA(8),SFPA_NSSD,RE_SFPA_NSSD),(NAMES_SFPA(6),   15
     *NSFPA_NSSD)                                                         15
      EQUIVALENCE (COMMEN_SFPA(6),CSFPA_NSSD),(MAP_SFPA(1,6),             15
     *MSFPA_NSSD)                                                         15
      CHARACTER CSFPA_NSSD*40/' '/                                        15
      CHARACTER NSFPA_NSSD*12/'R.NSSD'/                                   15
      INTEGER MSFPA_NSSD(2)/1,1/                                          15
      REAL RE_SFPA_NSSD                                                   15
C -                                                                       15
      REAL SFPA_DMWID                                                     15
      EQUIVALENCE (BANK_SFPA(9),SFPA_DMWID,RE_SFPA_DMWID),                15
     *(NAMES_SFPA(7),NSFPA_DMWID)                                         15
      EQUIVALENCE (COMMEN_SFPA(7),CSFPA_DMWID),(MAP_SFPA(1,7),            15
     *MSFPA_DMWID)                                                        15
      CHARACTER CSFPA_DMWID*40/' '/                                       15
      CHARACTER NSFPA_DMWID*12/'R.DMWID'/                                 15
      INTEGER MSFPA_DMWID(2)/1,1/                                         15
      REAL RE_SFPA_DMWID                                                  15
C -                                                                       15
      REAL SFPA_DMTHK                                                     15
      EQUIVALENCE (BANK_SFPA(10),SFPA_DMTHK,RE_SFPA_DMTHK),               15
     *(NAMES_SFPA(8),NSFPA_DMTHK)                                         15
      EQUIVALENCE (COMMEN_SFPA(8),CSFPA_DMTHK),(MAP_SFPA(1,8),            15
     *MSFPA_DMTHK)                                                        15
      CHARACTER CSFPA_DMTHK*40/' '/                                       15
      CHARACTER NSFPA_DMTHK*12/'R.DMTHK'/                                 15
      INTEGER MSFPA_DMTHK(2)/1,1/                                         15
      REAL RE_SFPA_DMTHK                                                  15
C -                                                                       15
      REAL SFPA_DMLEN                                                     15
      EQUIVALENCE (BANK_SFPA(11),SFPA_DMLEN,RE_SFPA_DMLEN),               15
     *(NAMES_SFPA(9),NSFPA_DMLEN)                                         15
      EQUIVALENCE (COMMEN_SFPA(9),CSFPA_DMLEN),(MAP_SFPA(1,9),            15
     *MSFPA_DMLEN)                                                        15
      CHARACTER CSFPA_DMLEN*40/' '/                                       15
      CHARACTER NSFPA_DMLEN*12/'R.DMLEN'/                                 15
      INTEGER MSFPA_DMLEN(2)/1,1/                                         15
      REAL RE_SFPA_DMLEN                                                  15
C -                                                                       15
      REAL SFPA_SMWID                                                     15
      EQUIVALENCE (BANK_SFPA(12),SFPA_SMWID,RE_SFPA_SMWID),               15
     *(NAMES_SFPA(10),NSFPA_SMWID)                                        15
      EQUIVALENCE (COMMEN_SFPA(10),CSFPA_SMWID),(MAP_SFPA(1,10),          15
     *MSFPA_SMWID)                                                        15
      CHARACTER CSFPA_SMWID*40/' '/                                       15
      CHARACTER NSFPA_SMWID*12/'R.SMWID'/                                 15
      INTEGER MSFPA_SMWID(2)/1,1/                                         15
      REAL RE_SFPA_SMWID                                                  15
C -                                                                       15
      REAL SFPA_SMTHK                                                     15
      EQUIVALENCE (BANK_SFPA(13),SFPA_SMTHK,RE_SFPA_SMTHK),               15
     *(NAMES_SFPA(11),NSFPA_SMTHK)                                        15
      EQUIVALENCE (COMMEN_SFPA(11),CSFPA_SMTHK),(MAP_SFPA(1,11),          15
     *MSFPA_SMTHK)                                                        15
      CHARACTER CSFPA_SMTHK*40/' '/                                       15
      CHARACTER NSFPA_SMTHK*12/'R.SMTHK'/                                 15
      INTEGER MSFPA_SMTHK(2)/1,1/                                         15
      REAL RE_SFPA_SMTHK                                                  15
C -                                                                       15
      REAL SFPA_SMLEN                                                     15
      EQUIVALENCE (BANK_SFPA(14),SFPA_SMLEN,RE_SFPA_SMLEN),               15
     *(NAMES_SFPA(12),NSFPA_SMLEN)                                        15
      EQUIVALENCE (COMMEN_SFPA(12),CSFPA_SMLEN),(MAP_SFPA(1,12),          15
     *MSFPA_SMLEN)                                                        15
      CHARACTER CSFPA_SMLEN*40/' '/                                       15
      CHARACTER NSFPA_SMLEN*12/'R.SMLEN'/                                 15
      INTEGER MSFPA_SMLEN(2)/1,1/                                         15
      REAL RE_SFPA_SMLEN                                                  15
C -                                                                       15
      REAL SFPA_SSLEN                                                     15
      EQUIVALENCE (BANK_SFPA(15),SFPA_SSLEN,RE_SFPA_SSLEN),               15
     *(NAMES_SFPA(13),NSFPA_SSLEN)                                        15
      EQUIVALENCE (COMMEN_SFPA(13),CSFPA_SSLEN),(MAP_SFPA(1,13),          15
     *MSFPA_SSLEN)                                                        15
      CHARACTER CSFPA_SSLEN*40/' '/                                       15
      CHARACTER NSFPA_SSLEN*12/'R.SSLEN'/                                 15
      INTEGER MSFPA_SSLEN(2)/1,1/                                         15
      REAL RE_SFPA_SSLEN                                                  15
C -                                                                       15
      REAL SFPA_WPLEN                                                     15
      EQUIVALENCE (BANK_SFPA(16),SFPA_WPLEN,RE_SFPA_WPLEN),               15
     *(NAMES_SFPA(14),NSFPA_WPLEN)                                        15
      EQUIVALENCE (COMMEN_SFPA(14),CSFPA_WPLEN),(MAP_SFPA(1,14),          15
     *MSFPA_WPLEN)                                                        15
      CHARACTER CSFPA_WPLEN*40/' '/                                       15
      CHARACTER NSFPA_WPLEN*12/'R.WPLEN'/                                 15
      INTEGER MSFPA_WPLEN(2)/1,1/                                         15
      REAL RE_SFPA_WPLEN                                                  15
C -                                                                       15
      REAL SFPA_SDLEN                                                     15
      EQUIVALENCE (BANK_SFPA(17),SFPA_SDLEN,RE_SFPA_SDLEN),               15
     *(NAMES_SFPA(15),NSFPA_SDLEN)                                        15
      EQUIVALENCE (COMMEN_SFPA(15),CSFPA_SDLEN),(MAP_SFPA(1,15),          15
     *MSFPA_SDLEN)                                                        15
      CHARACTER CSFPA_SDLEN*40/' '/                                       15
      CHARACTER NSFPA_SDLEN*12/'R.SDLEN'/                                 15
      INTEGER MSFPA_SDLEN(2)/1,1/                                         15
      REAL RE_SFPA_SDLEN                                                  15
C -                                                                       15
      REAL SFPA_TILT                                                      15
      EQUIVALENCE (BANK_SFPA(18),SFPA_TILT,RE_SFPA_TILT),                 15
     *(NAMES_SFPA(16),NSFPA_TILT)                                         15
      EQUIVALENCE (COMMEN_SFPA(16),CSFPA_TILT),(MAP_SFPA(1,16),           15
     *MSFPA_TILT)                                                         15
      CHARACTER CSFPA_TILT*40/' '/                                        15
      CHARACTER NSFPA_TILT*12/'R.TILT'/                                   15
      INTEGER MSFPA_TILT(2)/1,1/                                          15
      REAL RE_SFPA_TILT                                                   15
C -                                                                       15
      REAL SFPA_CPRAD                                                     15
      EQUIVALENCE (BANK_SFPA(19),SFPA_CPRAD,RE_SFPA_CPRAD),               15
     *(NAMES_SFPA(17),NSFPA_CPRAD)                                        15
      EQUIVALENCE (COMMEN_SFPA(17),CSFPA_CPRAD),(MAP_SFPA(1,17),          15
     *MSFPA_CPRAD)                                                        15
      CHARACTER CSFPA_CPRAD*40/' '/                                       15
      CHARACTER NSFPA_CPRAD*12/'R.CPRAD'/                                 15
      INTEGER MSFPA_CPRAD(2)/1,1/                                         15
      REAL RE_SFPA_CPRAD                                                  15
C -                                                                       15
      REAL SFPA_CPRAL                                                     15
      EQUIVALENCE (BANK_SFPA(20),SFPA_CPRAL,RE_SFPA_CPRAL),               15
     *(NAMES_SFPA(18),NSFPA_CPRAL)                                        15
      EQUIVALENCE (COMMEN_SFPA(18),CSFPA_CPRAL),(MAP_SFPA(1,18),          15
     *MSFPA_CPRAL)                                                        15
      CHARACTER CSFPA_CPRAL*40/' '/                                       15
      CHARACTER NSFPA_CPRAL*12/'R.CPRAL'/                                 15
      INTEGER MSFPA_CPRAL(2)/1,1/                                         15
      REAL RE_SFPA_CPRAL                                                  15
C -                                                                       15
      REAL SFPA_CFRAD                                                     15
      EQUIVALENCE (BANK_SFPA(21),SFPA_CFRAD,RE_SFPA_CFRAD),               15
     *(NAMES_SFPA(19),NSFPA_CFRAD)                                        15
      EQUIVALENCE (COMMEN_SFPA(19),CSFPA_CFRAD),(MAP_SFPA(1,19),          15
     *MSFPA_CFRAD)                                                        15
      CHARACTER CSFPA_CFRAD*40/' '/                                       15
      CHARACTER NSFPA_CFRAD*12/'R.CFRAD'/                                 15
      INTEGER MSFPA_CFRAD(2)/1,1/                                         15
      REAL RE_SFPA_CFRAD                                                  15
C -                                                                       15
      REAL SFPA_GPTHK                                                     15
      EQUIVALENCE (BANK_SFPA(22),SFPA_GPTHK,RE_SFPA_GPTHK),               15
     *(NAMES_SFPA(20),NSFPA_GPTHK)                                        15
      EQUIVALENCE (COMMEN_SFPA(20),CSFPA_GPTHK),(MAP_SFPA(1,20),          15
     *MSFPA_GPTHK)                                                        15
      CHARACTER CSFPA_GPTHK*40/' '/                                       15
      CHARACTER NSFPA_GPTHK*12/'R.GPTHK'/                                 15
      INTEGER MSFPA_GPTHK(2)/1,1/                                         15
      REAL RE_SFPA_GPTHK                                                  15
C -                                                                       15
      INTEGER SFPA_LADDERMAP(20)                                          15
      EQUIVALENCE (BANK_SFPA(23),SFPA_LADDERMAP,RE_SFPA_LADDERMAP),       15
     *(NAMES_SFPA(21),NSFPA_LADDERMAP)                                    15
      EQUIVALENCE (COMMEN_SFPA(21),CSFPA_LADDERMAP),(MAP_SFPA(1,21),      15
     *MSFPA_LADDERMAP)                                                    15
      CHARACTER CSFPA_LADDERMAP*40/' '/                                   15
      CHARACTER NSFPA_LADDERMAP*12/'I.LADDERMAP'/                         15
      INTEGER MSFPA_LADDERMAP(2)/20,1/                                    15
      REAL RE_SFPA_LADDERMAP                                              15
C -                                                                       15
      REAL SFPA_LADDERANGLE(20)                                           15
      EQUIVALENCE (BANK_SFPA(43),SFPA_LADDERANGLE,RE_SFPA_LADDERANGLE),   15
     *(NAMES_SFPA(22),NSFPA_LADDERANGLE)                                  15
      EQUIVALENCE (COMMEN_SFPA(22),CSFPA_LADDERANGLE),(MAP_SFPA(1,22),    15
     *MSFPA_LADDERANGLE)                                                  15
      CHARACTER CSFPA_LADDERANGLE*40/' '/                                 15
      CHARACTER NSFPA_LADDERANGLE*12/'R.LADDERANGLE'/                     15
      INTEGER MSFPA_LADDERANGLE(2)/20,1/                                  15
      REAL RE_SFPA_LADDERANGLE                                            15
C -                                                                       15
      REAL SFPA_LADDERTILT(20)                                            15
      EQUIVALENCE (BANK_SFPA(63),SFPA_LADDERTILT,RE_SFPA_LADDERTILT),     15
     *(NAMES_SFPA(23),NSFPA_LADDERTILT)                                   15
      EQUIVALENCE (COMMEN_SFPA(23),CSFPA_LADDERTILT),(MAP_SFPA(1,23),     15
     *MSFPA_LADDERTILT)                                                   15
      CHARACTER CSFPA_LADDERTILT*40/' '/                                  15
      CHARACTER NSFPA_LADDERTILT*12/'R.LADDERTILT'/                       15
      INTEGER MSFPA_LADDERTILT(2)/20,1/                                   15
      REAL RE_SFPA_LADDERTILT                                             15
C -                                                                       15
      REAL SFPA_LADDERRADIUS(20)                                          15
      EQUIVALENCE (BANK_SFPA(83),SFPA_LADDERRADIUS,                       15
     *RE_SFPA_LADDERRADIUS),(NAMES_SFPA(24),NSFPA_LADDERRADIUS)           15
      EQUIVALENCE (COMMEN_SFPA(24),CSFPA_LADDERRADIUS),(MAP_SFPA(1,24),   15
     *MSFPA_LADDERRADIUS)                                                 15
      CHARACTER CSFPA_LADDERRADIUS*40/' '/                                15
      CHARACTER NSFPA_LADDERRADIUS*12/'R.LADDERRADIUS'/                   15
      INTEGER MSFPA_LADDERRADIUS(2)/20,1/                                 15
      REAL RE_SFPA_LADDERRADIUS                                           15
C -                                                                       15
      INTEGER LEN_SFPA,LLEN_SFPA,LINK_SFPA                                15
      PARAMETER (LEN_SFPA=102)                                            15
      PARAMETER (LLEN_SFPA=24)                                            15
      INTEGER FLAG_SFPA/0/,MAP_SFPA(2,LLEN_SFPA),IDNUM_SFPA(10)           15
      CHARACTER NAMES_SFPA(LLEN_SFPA)*12,COMMEN_SFPA(LLEN_SFPA)*40        15
      DATA IDNUM_SFPA/10*0/                                               15
      REAL BANK_SFPA(LEN_SFPA)                                            15
      CHARACTER*60 BPATH_SFPA/' '/                                        15
      SAVE FLAG_SFPA,MAP_SFPA,IDNUM_SFPA,NAMES_SFPA,COMMEN_SFPA,          15
     *BPATH_SFPA                                                          15
      DATA LINK_SFPA/0/,BANK_SFPA/LEN_SFPA*0/                             15
C *                                                                       15
      INTEGER ILAD,IWAF,JWAF,NC                                           16
      REAL WAFPCKLEN,DTHK,RADTILT,ANG                                     18
      REAL ESSAI,HIGHT,LNG,LENG,YOFFSET                                   20
      REAL ZS1,ZS2,ZS3,ZS4,ZS5,YS1,YS2,YS3,YS4                            22
C *                                                                       24
      CALL AGSBEGM(MODULE,IPRIN)                                          24
      IF (IPRIN.LT.0) RETURN                                              24
C Filling bank SSDP - SILICON STRIPS                                      24
      BANK_TITLE='SILICON STRIPS'                                         24
      BPATH_SSDP='SSDP*'                                                  24
      IDNUM_SSDP(1)=0                                                     25
C VERSION                                                                 25
      SSDP_VERSION=1                                                      25
      CSSDP_VERSION='VERSION'                                             25
C THERE ARE A FEW CONFIGURAIONS POSSIBLE                                  26
      SSDP_CONFIG=1                                                       26
      CSSDP_CONFIG='THERE ARE A FEW CONFIGURAIONS POSSIBLE'               26
C 0=CAVE, 1=SVTT                                                          27
      SSDP_PLACEMENT=0                                                    27
      CSSDP_PLACEMENT='0=CAVE, 1=SVTT'                                    27
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSDP',    28
     *BPATH_SSDP,IDNUM_SSDP,1, MAP_SSDP,NAMES_SSDP,COMMEN_SSDP,           28
     *BANK_SSDP,LEN_SSDP,LLEN_SSDP,LINK_SSDP,FLAG_SSDP)                   28
C *                                                                       28
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSDP',    28
     *BPATH_SSDP,IDNUM_SSDP,1, MAP_SSDP,NAMES_SSDP,COMMEN_SSDP,           28
     *BANK_SSDP,LEN_SSDP,LLEN_SSDP,LINK_SSDP,FLAG_SSDP)                   28
C *                                                                       29
      CALL AGSBEGM(MODULE,IPRIN)                                          29
      IF (IPRIN.LT.0) RETURN                                              29
C Filling bank SFPB - SOME SSD SHELL DIMENSIONS                           29
      BANK_TITLE='SOME SSD SHELL DIMENSIONS'                              29
      BPATH_SFPB='SFPB*'                                                  29
      IDNUM_SFPB(1)=0                                                     30
C HAUT 1                                                                  30
      SFPB_HHIGHT=(0.02/TAN(54*PI/180)+0.02/(2*TAN(PI/5))+0.2)*SIN(PI/    30
     *5)                                                                  30
      CSFPB_HHIGHT='HAUT 1'                                               30
C HAUT 2                                                                  31
      SFPB_KHIGHT=0.02/SIN(54*PI/180)                                     31
      CSFPB_KHIGHT='HAUT 2'                                               31
C BAS 1                                                                   32
      SFPB_HBASE=(0.02/TAN(63*PI/180)+0.01/TAN(27*PI/                     32
     *180)+0.2)*SIN(27*PI/180)                                            32
      CSFPB_HBASE='BAS 1'                                                 32
C BAS 2                                                                   33
      SFPB_KBASE=0.02/SIN(63*PI/180)                                      33
      CSFPB_KBASE='BAS 2'                                                 33
C LENGHT OF THE SIDE KAPTON FILM                                          34
      SFPB_FSIZE=0.6/COS(15*PI/180)+0.02*TAN(15*PI/180)                   34
      CSFPB_FSIZE='LENGHT OF THE SIDE KAPTON FILM'                        34
C Z COORD                                                                 35
      SFPB_ZCOOR=5.18462095393834410                                      35
      CSFPB_ZCOOR='Z COORD'                                               35
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPB',    36
     *BPATH_SFPB,IDNUM_SFPB,1, MAP_SFPB,NAMES_SFPB,COMMEN_SFPB,           36
     *BANK_SFPB,LEN_SFPB,LLEN_SFPB,LINK_SFPB,FLAG_SFPB)                   36
C *                                                                       36
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPB',    36
     *BPATH_SFPB,IDNUM_SFPB,1, MAP_SFPB,NAMES_SFPB,COMMEN_SFPB,           36
     *BANK_SFPB,LEN_SFPB,LLEN_SFPB,LINK_SFPB,FLAG_SFPB)                   36
C *                                                                       37
      CALL AGSBEGM(MODULE,IPRIN)                                          37
      IF (IPRIN.LT.0) RETURN                                              37
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   37
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      37
      BPATH_SFPA='SFPA*'                                                  37
      IDNUM_SFPA(1)=0                                                     38
C GEOMETRY VERSION                                                        38
      SFPA_VERSION=1                                                      38
      CSFPA_VERSION='GEOMETRY VERSION'                                    38
C MOTHER RMIN                                                             39
      SFPA_RMIN=21.8                                                      39
      CSFPA_RMIN='MOTHER RMIN'                                            39
C MOTHER RMAX                                                             40
      SFPA_RMAX=29.5                                                      40
      CSFPA_RMAX='MOTHER RMAX'                                            40
C MOTHER LEN ALONG THE Z DIRECTION                                        41
      SFPA_LEN=120.                                                       41
      CSFPA_LEN='MOTHER LEN ALONG THE Z DIRECTION'                        41
C DISTANCE FROM BEAM AXIS TO DETECTOR CENTER                              42
      SFPA_RAD=23.                                                        42
      CSFPA_RAD='DISTANCE FROM BEAM AXIS TO DETECTOR CENTER'              42
C NUMBER OF SILICON STRIP DETECTORS                                       43
      SFPA_NSSD=16                                                        43
      CSFPA_NSSD='NUMBER OF SILICON STRIP DETECTORS'                      43
C DETECTOR MOTHER WIDTH                                                   44
      SFPA_DMWID=7.8                                                      44
      CSFPA_DMWID='DETECTOR MOTHER WIDTH'                                 44
C DETECTOR MOTHER THICKNESS                                               45
      SFPA_DMTHK=2.0                                                      45
      CSFPA_DMTHK='DETECTOR MOTHER THICKNESS'                             45
C DETECTOR MOTHER LENGTH (DETECTORS + ADC BOARD)                          46
      SFPA_DMLEN=90.                                                      46
      CSFPA_DMLEN='DETECTOR MOTHER LENGTH (DETECTORS + ADC BOARD)'        46
C STRUCTURE MOTHER WIDTH                                                  47
      SFPA_SMWID=7.5                                                      47
      CSFPA_SMWID='STRUCTURE MOTHER WIDTH'                                47
C STRUCTURE MOTHER THICKNESS                                              48
      SFPA_SMTHK=5.0                                                      48
      CSFPA_SMTHK='STRUCTURE MOTHER THICKNESS'                            48
C STRUCTURE MOTHER LENGTH                                                 49
      SFPA_SMLEN=101.9                                                    49
      CSFPA_SMLEN='STRUCTURE MOTHER LENGTH'                               49
C LENGTH OF A SUBVOLUME OF THE STRUCTURE                                  50
      SFPA_SSLEN=95./20.                                                  50
      CSFPA_SSLEN='LENGTH OF A SUBVOLUME OF THE STRUCTURE'                50
C LENGTH OF WAFER PACK                                                    51
      SFPA_WPLEN=68.8                                                     51
      CSFPA_WPLEN='LENGTH OF WAFER PACK'                                  51
C LENGTH OF ONE STRIP DETECTOR (ALONG BEAM AXIS)                          52
      SFPA_SDLEN=4.2                                                      52
      CSFPA_SDLEN='LENGTH OF ONE STRIP DETECTOR (ALONG BEAM AXIS)'        52
C TILING ANGLE (DEGREES)                                                  53
      SFPA_TILT=5.0                                                       53
      CSFPA_TILT='TILING ANGLE (DEGREES)'                                 53
C COOLING PIPE OUTER RADIUS                                               54
      SFPA_CPRAD=0.1                                                      54
      CSFPA_CPRAD='COOLING PIPE OUTER RADIUS'                             54
C COOLING PIPE INNER RADIUS                                               55
      SFPA_CPRAL=0.09                                                     55
      CSFPA_CPRAL='COOLING PIPE INNER RADIUS'                             55
C CARBON FIBER TUBE RADIUS (SUPPORT STRUCTURE)                            56
      SFPA_CFRAD=0.1                                                      56
      CSFPA_CFRAD='CARBON FIBER TUBE RADIUS (SUPPORT STRUCTURE)'          56
C GAP BETWEEN STRUCTURE MOTHER AND DETECTOR                               57
      SFPA_GPTHK=-1.0                                                     57
      CSFPA_GPTHK='GAP BETWEEN STRUCTURE MOTHER AND DETECTOR'             57
C PRESENCE OF LADDERS                                                     59
      SFPA_LADDERMAP(1)=0                                                 59
      SFPA_LADDERMAP(2)=0                                                 59
      SFPA_LADDERMAP(3)=1                                                 59
      SFPA_LADDERMAP(4)=0                                                 59
      SFPA_LADDERMAP(5)=0                                                 59
      SFPA_LADDERMAP(6)=0                                                 59
      SFPA_LADDERMAP(7)=0                                                 59
      SFPA_LADDERMAP(8)=0                                                 59
      SFPA_LADDERMAP(9)=0                                                 59
      SFPA_LADDERMAP(10)=0                                                59
      SFPA_LADDERMAP(11)=0                                                59
      SFPA_LADDERMAP(12)=0                                                59
      SFPA_LADDERMAP(13)=0                                                59
      SFPA_LADDERMAP(14)=0                                                59
      SFPA_LADDERMAP(15)=0                                                59
      SFPA_LADDERMAP(16)=0                                                59
      SFPA_LADDERMAP(17)=0                                                59
      SFPA_LADDERMAP(18)=0                                                59
      SFPA_LADDERMAP(19)=0                                                59
      SFPA_LADDERMAP(20)=0                                                59
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               59
C INDIVIDUAL ANGLES                                                       63
      SFPA_LADDERANGLE(1)=-1.0                                            63
      SFPA_LADDERANGLE(2)=-1.0                                            63
      SFPA_LADDERANGLE(3)=45.0                                            63
      SFPA_LADDERANGLE(4)=-1.0                                            63
      SFPA_LADDERANGLE(5)=-1.0                                            63
      SFPA_LADDERANGLE(6)=-1.0                                            63
      SFPA_LADDERANGLE(7)=-1.0                                            63
      SFPA_LADDERANGLE(8)=-1.0                                            63
      SFPA_LADDERANGLE(9)=-1.0                                            63
      SFPA_LADDERANGLE(10)=-1.0                                           63
      SFPA_LADDERANGLE(11)=-1.0                                           63
      SFPA_LADDERANGLE(12)=-1.0                                           63
      SFPA_LADDERANGLE(13)=-1.0                                           63
      SFPA_LADDERANGLE(14)=-1.0                                           63
      SFPA_LADDERANGLE(15)=-1.0                                           63
      SFPA_LADDERANGLE(16)=-1.0                                           63
      SFPA_LADDERANGLE(17)=-1.0                                           63
      SFPA_LADDERANGLE(18)=-1.0                                           63
      SFPA_LADDERANGLE(19)=-1.0                                           63
      SFPA_LADDERANGLE(20)=-1.0                                           63
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               63
C INDIVIDUAL TILTS                                                        67
      SFPA_LADDERTILT(1)=-1.0                                             67
      SFPA_LADDERTILT(2)=-1.0                                             67
      SFPA_LADDERTILT(3)=0.0                                              67
      SFPA_LADDERTILT(4)=-1.0                                             67
      SFPA_LADDERTILT(5)=-1.0                                             67
      SFPA_LADDERTILT(6)=-1.0                                             67
      SFPA_LADDERTILT(7)=-1.0                                             67
      SFPA_LADDERTILT(8)=-1.0                                             67
      SFPA_LADDERTILT(9)=-1.0                                             67
      SFPA_LADDERTILT(10)=-1.0                                            67
      SFPA_LADDERTILT(11)=-1.0                                            67
      SFPA_LADDERTILT(12)=-1.0                                            67
      SFPA_LADDERTILT(13)=-1.0                                            67
      SFPA_LADDERTILT(14)=-1.0                                            67
      SFPA_LADDERTILT(15)=-1.0                                            67
      SFPA_LADDERTILT(16)=-1.0                                            67
      SFPA_LADDERTILT(17)=-1.0                                            67
      SFPA_LADDERTILT(18)=-1.0                                            67
      SFPA_LADDERTILT(19)=-1.0                                            67
      SFPA_LADDERTILT(20)=-1.0                                            67
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 67
C INDIVIDUAL RADII                                                        72
      SFPA_LADDERRADIUS(1)=-1.0                                           72
      SFPA_LADDERRADIUS(2)=-1.0                                           72
      SFPA_LADDERRADIUS(3)=23.000                                         72
      SFPA_LADDERRADIUS(4)=-1.0                                           72
      SFPA_LADDERRADIUS(5)=-1.0                                           72
      SFPA_LADDERRADIUS(6)=-1.0                                           72
      SFPA_LADDERRADIUS(7)=-1.0                                           72
      SFPA_LADDERRADIUS(8)=-1.0                                           72
      SFPA_LADDERRADIUS(9)=-1.0                                           72
      SFPA_LADDERRADIUS(10)=-1.0                                          72
      SFPA_LADDERRADIUS(11)=-1.0                                          72
      SFPA_LADDERRADIUS(12)=-1.0                                          72
      SFPA_LADDERRADIUS(13)=-1.0                                          72
      SFPA_LADDERRADIUS(14)=-1.0                                          72
      SFPA_LADDERRADIUS(15)=-1.0                                          72
      SFPA_LADDERRADIUS(16)=-1.0                                          72
      SFPA_LADDERRADIUS(17)=-1.0                                          72
      SFPA_LADDERRADIUS(18)=-1.0                                          72
      SFPA_LADDERRADIUS(19)=-1.0                                          72
      SFPA_LADDERRADIUS(20)=-1.0                                          72
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               72
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    73
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           73
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   73
C *                                                                       73
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    73
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           73
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   73
C *                                                                       74
      CALL AGSBEGM(MODULE,IPRIN)                                          74
      IF (IPRIN.LT.0) RETURN                                              74
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   74
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      74
      BPATH_SFPA='SFPA*'                                                  74
      IDNUM_SFPA(1)=0                                                     75
C GEOMETRY VERSION                                                        75
      SFPA_VERSION=2                                                      75
      CSFPA_VERSION='GEOMETRY VERSION'                                    75
C PRESENCE OF LADDERS                                                     77
      SFPA_LADDERMAP(1)=1                                                 77
      SFPA_LADDERMAP(2)=1                                                 77
      SFPA_LADDERMAP(3)=1                                                 77
      SFPA_LADDERMAP(4)=0                                                 77
      SFPA_LADDERMAP(5)=0                                                 77
      SFPA_LADDERMAP(6)=0                                                 77
      SFPA_LADDERMAP(7)=0                                                 77
      SFPA_LADDERMAP(8)=0                                                 77
      SFPA_LADDERMAP(9)=1                                                 77
      SFPA_LADDERMAP(10)=1                                                77
      SFPA_LADDERMAP(11)=1                                                77
      SFPA_LADDERMAP(12)=1                                                77
      SFPA_LADDERMAP(13)=1                                                77
      SFPA_LADDERMAP(14)=0                                                77
      SFPA_LADDERMAP(15)=0                                                77
      SFPA_LADDERMAP(16)=0                                                77
      SFPA_LADDERMAP(17)=0                                                77
      SFPA_LADDERMAP(18)=0                                                77
      SFPA_LADDERMAP(19)=1                                                77
      SFPA_LADDERMAP(20)=1                                                77
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               77
C INDIVIDUAL ANGLES                                                       81
      SFPA_LADDERANGLE(1)=90.0                                            81
      SFPA_LADDERANGLE(2)=108.3                                           81
      SFPA_LADDERANGLE(3)=130.0                                           81
      SFPA_LADDERANGLE(4)=-1.0                                            81
      SFPA_LADDERANGLE(5)=-1.0                                            81
      SFPA_LADDERANGLE(6)=-1.0                                            81
      SFPA_LADDERANGLE(7)=-1.0                                            81
      SFPA_LADDERANGLE(8)=-1.0                                            81
      SFPA_LADDERANGLE(9)=230.0                                           81
      SFPA_LADDERANGLE(10)=251.7                                          81
      SFPA_LADDERANGLE(11)=270.0                                          81
      SFPA_LADDERANGLE(12)=288.3                                          81
      SFPA_LADDERANGLE(13)=310.0                                          81
      SFPA_LADDERANGLE(14)=-1.0                                           81
      SFPA_LADDERANGLE(15)=-1.0                                           81
      SFPA_LADDERANGLE(16)=-1.0                                           81
      SFPA_LADDERANGLE(17)=-1.0                                           81
      SFPA_LADDERANGLE(18)=-1.0                                           81
      SFPA_LADDERANGLE(19)=50.0                                           81
      SFPA_LADDERANGLE(20)=71.7                                           81
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               81
C INDIVIDUAL TILTS                                                        85
      SFPA_LADDERTILT(1)=0.0                                              85
      SFPA_LADDERTILT(2)=-6.0                                             85
      SFPA_LADDERTILT(3)=0.0                                              85
      SFPA_LADDERTILT(4)=-1.0                                             85
      SFPA_LADDERTILT(5)=-1.0                                             85
      SFPA_LADDERTILT(6)=-1.0                                             85
      SFPA_LADDERTILT(7)=-1.0                                             85
      SFPA_LADDERTILT(8)=0.0                                              85
      SFPA_LADDERTILT(9)=0.0                                              85
      SFPA_LADDERTILT(10)=6.0                                             85
      SFPA_LADDERTILT(11)=0.0                                             85
      SFPA_LADDERTILT(12)=-6.0                                            85
      SFPA_LADDERTILT(13)=0.0                                             85
      SFPA_LADDERTILT(14)=-1.0                                            85
      SFPA_LADDERTILT(15)=-1.0                                            85
      SFPA_LADDERTILT(16)=-1.0                                            85
      SFPA_LADDERTILT(17)=-1.0                                            85
      SFPA_LADDERTILT(18)=-1.0                                            85
      SFPA_LADDERTILT(19)=0.0                                             85
      SFPA_LADDERTILT(20)=6.0                                             85
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 85
C INDIVIDUAL RADII                                                        90
      SFPA_LADDERRADIUS(1)=23.174                                         90
      SFPA_LADDERRADIUS(2)=22.800                                         90
      SFPA_LADDERRADIUS(3)=24.600                                         90
      SFPA_LADDERRADIUS(4)=-1.0                                           90
      SFPA_LADDERRADIUS(5)=-1.0                                           90
      SFPA_LADDERRADIUS(6)=-1.0                                           90
      SFPA_LADDERRADIUS(7)=-1.0                                           90
      SFPA_LADDERRADIUS(8)=-1.0                                           90
      SFPA_LADDERRADIUS(9)=24.600                                         90
      SFPA_LADDERRADIUS(10)=22.800                                        90
      SFPA_LADDERRADIUS(11)=23.174                                        90
      SFPA_LADDERRADIUS(12)=22.800                                        90
      SFPA_LADDERRADIUS(13)=24.600                                        90
      SFPA_LADDERRADIUS(14)=-1.0                                          90
      SFPA_LADDERRADIUS(15)=-1.0                                          90
      SFPA_LADDERRADIUS(16)=-1.0                                          90
      SFPA_LADDERRADIUS(17)=-1.0                                          90
      SFPA_LADDERRADIUS(18)=-1.0                                          90
      SFPA_LADDERRADIUS(19)=24.600                                        90
      SFPA_LADDERRADIUS(20)=22.800                                        90
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               90
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    91
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           91
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   91
C *                                                                       91
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    91
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           91
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   91
C *                                                                       92
      CALL AGSBEGM(MODULE,IPRIN)                                          92
      IF (IPRIN.LT.0) RETURN                                              92
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   92
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      92
      BPATH_SFPA='SFPA*'                                                  92
      IDNUM_SFPA(1)=0                                                     93
C GEOMETRY VERSION                                                        93
      SFPA_VERSION=3                                                      93
      CSFPA_VERSION='GEOMETRY VERSION'                                    93
C PRESENCE OF LADDERS                                                     95
      SFPA_LADDERMAP(1)=1                                                 95
      SFPA_LADDERMAP(2)=1                                                 95
      SFPA_LADDERMAP(3)=1                                                 95
      SFPA_LADDERMAP(4)=1                                                 95
      SFPA_LADDERMAP(5)=1                                                 95
      SFPA_LADDERMAP(6)=1                                                 95
      SFPA_LADDERMAP(7)=1                                                 95
      SFPA_LADDERMAP(8)=1                                                 95
      SFPA_LADDERMAP(9)=1                                                 95
      SFPA_LADDERMAP(10)=1                                                95
      SFPA_LADDERMAP(11)=1                                                95
      SFPA_LADDERMAP(12)=1                                                95
      SFPA_LADDERMAP(13)=1                                                95
      SFPA_LADDERMAP(14)=1                                                95
      SFPA_LADDERMAP(15)=1                                                95
      SFPA_LADDERMAP(16)=1                                                95
      SFPA_LADDERMAP(17)=1                                                95
      SFPA_LADDERMAP(18)=1                                                95
      SFPA_LADDERMAP(19)=1                                                95
      SFPA_LADDERMAP(20)=1                                                95
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               95
C INDIVIDUAL ANGLES                                                       99
      SFPA_LADDERANGLE(1)=90.0                                            99
      SFPA_LADDERANGLE(2)=108.3                                           99
      SFPA_LADDERANGLE(3)=126.6                                           99
      SFPA_LADDERANGLE(4)=144.4                                           99
      SFPA_LADDERANGLE(5)=162.2                                           99
      SFPA_LADDERANGLE(6)=180.0                                           99
      SFPA_LADDERANGLE(7)=197.8                                           99
      SFPA_LADDERANGLE(8)=215.6                                           99
      SFPA_LADDERANGLE(9)=233.4                                           99
      SFPA_LADDERANGLE(10)=251.7                                          99
      SFPA_LADDERANGLE(11)=270.0                                          99
      SFPA_LADDERANGLE(12)=288.3                                          99
      SFPA_LADDERANGLE(13)=306.6                                          99
      SFPA_LADDERANGLE(14)=324.4                                          99
      SFPA_LADDERANGLE(15)=342.2                                          99
      SFPA_LADDERANGLE(16)=0.0                                            99
      SFPA_LADDERANGLE(17)=17.8                                           99
      SFPA_LADDERANGLE(18)=35.6                                           99
      SFPA_LADDERANGLE(19)=53.4                                           99
      SFPA_LADDERANGLE(20)=71.7                                           99
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               99
C INDIVIDUAL TILTS                                                        103
      SFPA_LADDERTILT(1)=0.0                                              103
      SFPA_LADDERTILT(2)=-6.0                                             103
      SFPA_LADDERTILT(3)=-7.0                                             103
      SFPA_LADDERTILT(4)=-7.0                                             103
      SFPA_LADDERTILT(5)=-7.0                                             103
      SFPA_LADDERTILT(6)=0.0                                              103
      SFPA_LADDERTILT(7)=7.0                                              103
      SFPA_LADDERTILT(8)=7.0                                              103
      SFPA_LADDERTILT(9)=7.0                                              103
      SFPA_LADDERTILT(10)=6.0                                             103
      SFPA_LADDERTILT(11)=0.0                                             103
      SFPA_LADDERTILT(12)=-6.0                                            103
      SFPA_LADDERTILT(13)=-7.0                                            103
      SFPA_LADDERTILT(14)=-7.0                                            103
      SFPA_LADDERTILT(15)=-7.0                                            103
      SFPA_LADDERTILT(16)=0.0                                             103
      SFPA_LADDERTILT(17)=7.0                                             103
      SFPA_LADDERTILT(18)=7.0                                             103
      SFPA_LADDERTILT(19)=7.0                                             103
      SFPA_LADDERTILT(20)=6.0                                             103
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 103
C INDIVIDUAL RADII                                                        108
      SFPA_LADDERRADIUS(1)=23.177                                         108
      SFPA_LADDERRADIUS(2)=22.800                                         108
      SFPA_LADDERRADIUS(3)=22.800                                         108
      SFPA_LADDERRADIUS(4)=22.800                                         108
      SFPA_LADDERRADIUS(5)=23.800                                         108
      SFPA_LADDERRADIUS(6)=22.500                                         108
      SFPA_LADDERRADIUS(7)=23.800                                         108
      SFPA_LADDERRADIUS(8)=22.800                                         108
      SFPA_LADDERRADIUS(9)=22.800                                         108
      SFPA_LADDERRADIUS(10)=22.800                                        108
      SFPA_LADDERRADIUS(11)=23.177                                        108
      SFPA_LADDERRADIUS(12)=22.800                                        108
      SFPA_LADDERRADIUS(13)=22.800                                        108
      SFPA_LADDERRADIUS(14)=22.800                                        108
      SFPA_LADDERRADIUS(15)=23.800                                        108
      SFPA_LADDERRADIUS(16)=22.500                                        108
      SFPA_LADDERRADIUS(17)=23.800                                        108
      SFPA_LADDERRADIUS(18)=22.800                                        108
      SFPA_LADDERRADIUS(19)=22.800                                        108
      SFPA_LADDERRADIUS(20)=22.800                                        108
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               108
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    109
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           109
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   109
C *                                                                       109
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    109
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           109
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   109
C use default SSDP bank                                                   110
      BPATH_SSDP='SSDP*'                                                  110
      IDNUM_SSDP(1)=0                                                     110
      CALL AGDGETP('SISDGEO1','*',-999,'SSDP',' ',BANK_SSDP, BPATH_SSDP,  110
     *IDNUM_SSDP,1,MAP_SSDP,NAMES_SSDP,BANK_SSDP,LEN_SSDP,LLEN_SSDP,      110
     *LINK_SSDP,FLAG_SSDP)                                                110
C use SFPA bank                                                           111
      BPATH_SFPA='SFPA*'                                                  111
      IDNUM_SFPA(1)=0                                                     111
      SFPA_VERSION=SSDP_CONFIG                                            111
      CALL AGDGETP('SISDGEO1','*',-999,'SFPA',NSFPA_VERSION,              111
     *RE_SFPA_VERSION,BPATH_SFPA,IDNUM_SFPA,1,MAP_SFPA,NAMES_SFPA,        111
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   111
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             112
      CALL AGSRESET                                                       112
      AG_A = 28.08                                                        112
      AG_Z = 14                                                           112
      AG_W = 0.6*1*28./60.                                                112
      AG_TITLE = 'SI'                                                     112
      AG_EXNAME = 'COMPONENT'                                             112
      AG_PARLIST = 'A_Z_W'                                                112
      CALL AXCOMPONENT                                                    112
C *                                                                       112
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  112
C =                                                                       112
      CALL AGSRESET                                                       113
      AG_A = 16                                                           113
      AG_Z = 8                                                            113
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               113
      AG_TITLE = 'O'                                                      113
      AG_EXNAME = 'COMPONENT'                                             113
      AG_PARLIST = 'A_Z_W'                                                113
      CALL AXCOMPONENT                                                    113
C *                                                                       113
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 113
      CALL AGSRESET                                                       114
      AG_A = 12                                                           114
      AG_Z = 6                                                            114
      AG_W = 0.4*8*12./174.                                               114
      AG_TITLE = 'C'                                                      114
      AG_EXNAME = 'COMPONENT'                                             114
      AG_PARLIST = 'A_Z_W'                                                114
      CALL AXCOMPONENT                                                    114
C *                                                                       114
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  114
      CALL AGSRESET                                                       115
      AG_A = 1                                                            115
      AG_Z = 1                                                            115
      AG_W = 0.4*14*1./174.                                               115
      AG_TITLE = 'H'                                                      115
      AG_EXNAME = 'COMPONENT'                                             115
      AG_PARLIST = 'A_Z_W'                                                115
      CALL AXCOMPONENT                                                    115
C *                                                                       115
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  115
      CALL AGSRESET                                                       116
      AG_DENS = 1.7                                                       116
      AG_TITLE = 'G10'                                                    116
      AG_EXNAME = 'MIXTURE'                                               116
      AG_PARLIST = 'DENS'                                                 116
      CALL AXMIXTURE                                                      116
C *                                                                       116
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             116
      CALL AGSRESET                                                       117
      AG_A = 28.08                                                        117
      AG_Z = 14                                                           117
      AG_W = 0.6*1*28./60.                                                117
      AG_TITLE = 'SI'                                                     117
      AG_EXNAME = 'COMPONENT'                                             117
      AG_PARLIST = 'A_Z_W'                                                117
      CALL AXCOMPONENT                                                    117
C *                                                                       117
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  117
C =                                                                       117
      CALL AGSRESET                                                       118
      AG_A = 16                                                           118
      AG_Z = 8                                                            118
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               118
      AG_TITLE = 'O'                                                      118
      AG_EXNAME = 'COMPONENT'                                             118
      AG_PARLIST = 'A_Z_W'                                                118
      CALL AXCOMPONENT                                                    118
C *                                                                       118
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 118
      CALL AGSRESET                                                       119
      AG_A = 12                                                           119
      AG_Z = 6                                                            119
      AG_W = 0.4*8*12./174.                                               119
      AG_TITLE = 'C'                                                      119
      AG_EXNAME = 'COMPONENT'                                             119
      AG_PARLIST = 'A_Z_W'                                                119
      CALL AXCOMPONENT                                                    119
C *                                                                       119
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  119
      CALL AGSRESET                                                       120
      AG_A = 1                                                            120
      AG_Z = 1                                                            120
      AG_W = 0.4*14*1./174.                                               120
      AG_TITLE = 'H'                                                      120
      AG_EXNAME = 'COMPONENT'                                             120
      AG_PARLIST = 'A_Z_W'                                                120
      CALL AXCOMPONENT                                                    120
C *                                                                       120
C Geant MIXTURE : G5 ===> DENS=0.85 <===                                  120
      CALL AGSRESET                                                       121
      AG_DENS = 0.85                                                      121
      AG_TITLE = 'G5'                                                     121
      AG_EXNAME = 'MIXTURE'                                               121
      AG_PARLIST = 'DENS'                                                 121
      CALL AXMIXTURE                                                      121
C *                                                                       121
C Check SSDP_PLACEMENT==1                                                 121
      IF (SSDP_PLACEMENT.EQ.1) THEN                                       122
      CALL AGSBEGM(MODULE,IPRIN)                                          123
C Check IPRIN<0                                                           123
      IF (IPRIN.LT.0) RETURN                                              123
C Create SFMO :                                                           123
C Call To Local Procedure SFMO                                            123
      ASSIGN 5020 TO BLOCK_SFMO                                           123
      GO TO  5010                                                         123
5020  CONTINUE                                                            123
C *                                                                       123
C Geant POSITION : SFMO ===> MOTHER='SVTT' <===                           124
      CALL AGSRESET                                                       124
      AG_MOTHER = 'SVTT'                                                  124
      AG_TITLE = 'SFMO'                                                   124
      AG_EXNAME = 'POSITION'                                              124
      AG_PARLIST = 'MOTHER'                                               124
      CALL AXPOSITION                                                     124
C *                                                                       124
      ELSE                                                                124
      CALL AGSBEGM(MODULE,IPRIN)                                          126
C Check IPRIN<0                                                           126
      IF (IPRIN.LT.0) RETURN                                              126
C Create SFMO :                                                           126
C Call To Local Procedure SFMO                                            126
      ASSIGN 5030 TO BLOCK_SFMO                                           126
      GO TO  5010                                                         126
5030  CONTINUE                                                            126
C *                                                                       126
C Geant POSITION : SFMO ===> MOTHER='CAVE' <===                           127
      CALL AGSRESET                                                       127
      AG_MOTHER = 'CAVE'                                                  127
      AG_TITLE = 'SFMO'                                                   127
      AG_EXNAME = 'POSITION'                                              127
      AG_PARLIST = 'MOTHER'                                               127
      CALL AXPOSITION                                                     127
C *                                                                       127
      ENDIF                                                               127
C ----------------------------------------------------------------------  129
C ------------ SFMO                                                       129
      CALL AGSENDM                                                        130
      RETURN                                                              130
C Begin Local Procedure SFMO                                              130
C SFMO IS THE MOTHER OF ALL SILICON STRIP DETECTOR VOLUMES                130
5010  CONTINUE                                                            130
         AG_TITLE='SFMO'                                                  130
         AG_MODULE=MODULE                                                 130
         AG_MARK='V00'                                                    130
         CALL AGSPUSH                                                     130
         AG_MODULE=MODULE                                                 130
C    *                                                                    130
C    Geant MATERIAL : AIR ===> inherited arguments used <===              131
         CALL AGSRESET                                                    131
         AG_TITLE = 'AIR'                                                 131
         AG_EXNAME = 'MATERIAL'                                           131
         AG_PARLIST = 'NONE'                                              131
         CALL AXMATERIAL                                                  131
C    *                                                                    131
C    Geant ATTRIBUTE : SFMO ===> SEEN=1 COLO=6 <===                       131
         CALL AGSRESET                                                    132
         AG_SEEN = 1                                                      132
         AG_COLO = 6                                                      132
         AG_TITLE = 'SFMO'                                                132
         AG_EXNAME = 'ATTRIBUTE'                                          132
         AG_PARLIST = 'SEEN_COLO'                                         132
         CALL AXATTRIBUTE                                                 132
C    *                                                                    132
C    Geant SHAPE : TUBE ===> RMIN=SFPA_RMIN RMAX=SFPA_RMAX, DZ=SFPA_LEN/  132
C    2 <===                                                               132
         CALL AGSRESET                                                    134
         AG_RMIN = SFPA_RMIN                                              134
         AG_RMAX = SFPA_RMAX                                              134
         AG_DZ = SFPA_LEN/2                                               134
         AG_TITLE = 'TUBE'                                                134
         AG_EXNAME = 'SHAPE'                                              134
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      134
         CALL AXSHAPE                                                     134
C    *                                                                    134
         DTHK=SFPA_SMTHK + SFPA_GPTHK                                     134
C    Create SFLM :                                                        136
C    Call To Local Procedure SFLM                                         136
         ASSIGN 5050 TO BLOCK_SFLM                                        136
         GO TO  5040                                                      136
5050     CONTINUE                                                         136
C    *                                                                    136
C    *                                                                    137
         ILAD=1                                                           137
            GO TO 5063                                                    137
5061        ILAD=ILAD+1                                                   137
5063        IF(ILAD-(20).GT.0) GO TO 5062                                 137
C       Check SFPA_LADDERMAP(ILAD).GT.0                                   138
            IF (SFPA_LADDERMAP(ILAD).GT.0) THEN                           138
            ANG = (SFPA_LADDERANGLE(ILAD)*PI)/180.0                       139
            RADTILT = (SFPA_LADDERTILT(ILAD) *PI)/180.0                   140
C       Check ILAD.EQ.1                                                   141
            IF (ILAD.EQ.1) THEN                                           141
            NC=1                                                          142
            ELSE                                                          143
            NC=20-ILAD+2                                                  144
            ENDIF                                                         145
C       Geant POSITION : SFLM ===> X=(SFPA_LADDERRADIUS(ILAD)*COS(ANG)+(  148
C       DTHK*COS(ANG+RADTILT))/2.0) Y=(SFPA_LADDERRADIUS(ILAD)*SIN(ANG)+  148
C       (DTHK*SIN(ANG+RADTILT))/2.0) Z=0 ALPHAZ=SFPA_LADDERANGLE(ILAD)-9  148
C       0.0+SFPA_LADDERTILT(ILAD) NCOPY=NC KONLY='MANY' <===              148
            CALL AGSRESET                                                 148
            AG_X =                                                        148
     *      (SFPA_LADDERRADIUS(ILAD)*COS(ANG)+(DTHK*COS(ANG+RADTILT))/    148
     *      2.0)                                                          148
            AG_Y =                                                        148
     *      (SFPA_LADDERRADIUS(ILAD)*SIN(ANG)+(DTHK*SIN(ANG+RADTILT))/    148
     *      2.0)                                                          148
            AG_Z = 0                                                      148
            AG_ALPHAN=AG_ALPHAN+1                                         148
            AG_ALPHAK(AG_ALPHAN)=3                                        148
            AG_ALPHAL(AG_ALPHAN)=                                         148
     *      SFPA_LADDERANGLE(ILAD)-90.0+SFPA_LADDERTILT(ILAD)             148
            AG_NCOPY = NC                                                 148
            AG_KONLY = 'MANY'                                             148
            AG_TITLE = 'SFLM'                                             148
            AG_EXNAME = 'POSITION'                                        148
            AG_PARLIST = 'X_Y_Z_ALPHAZ_NCOPY_KONLY'                       148
            CALL AXPOSITION                                               148
C       *                                                                 148
            ENDIF                                                         148
         GO TO 5061                                                       150
5062     CONTINUE                                                         150
         ZS1=49.8 + 0.95 + 2.5/2. + 0.2                                   151
         ZS2=49.8 + 0.95 + 0.5/2. + 0.2                                   152
         ZS3=49.8 + 0.95 + 0.5 + 0.2 + 2.5/2                              153
         ZS4=49.8 + 0.95 + 0.5 + 0.2 + 2.5 + 5.05/2.                      154
         ZS5=57.5                                                         155
         YS1=19.1                                                         156
         YS2=18.1                                                         157
         YS3=15.945                                                       158
         YS4=17.0                                                         159
C    Create SSST :                                                        160
C    Call To Local Procedure SSST                                         160
         ASSIGN 5080 TO BLOCK_SSST                                        160
         GO TO  5070                                                      160
5080     CONTINUE                                                         160
C    *                                                                    160
C    Geant POSITION : SSST ===> X=0. Y=0. Z=-ZS1 <===                     161
         CALL AGSRESET                                                    161
         AG_X = 0.                                                        161
         AG_Y = 0.                                                        161
         AG_Z = -ZS1                                                      161
         AG_TITLE = 'SSST'                                                161
         AG_EXNAME = 'POSITION'                                           161
         AG_PARLIST = 'X_Y_Z'                                             161
         CALL AXPOSITION                                                  161
C    *                                                                    161
C    Geant POSITION : SSST ===> X=0. Y=0. Z=ZS1 <===                      161
         CALL AGSRESET                                                    162
         AG_X = 0.                                                        162
         AG_Y = 0.                                                        162
         AG_Z = ZS1                                                       162
         AG_TITLE = 'SSST'                                                162
         AG_EXNAME = 'POSITION'                                           162
         AG_PARLIST = 'X_Y_Z'                                             162
         CALL AXPOSITION                                                  162
C    *                                                                    162
C    Geant POSITION : SSST ===> X=0. Y=0. Z=-ZS1 ALPHAZ=180. <===         162
         CALL AGSRESET                                                    163
         AG_X = 0.                                                        163
         AG_Y = 0.                                                        163
         AG_Z = -ZS1                                                      163
         AG_ALPHAN=AG_ALPHAN+1                                            163
         AG_ALPHAK(AG_ALPHAN)=3                                           163
         AG_ALPHAL(AG_ALPHAN)=180.                                        163
         AG_TITLE = 'SSST'                                                163
         AG_EXNAME = 'POSITION'                                           163
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      163
         CALL AXPOSITION                                                  163
C    *                                                                    163
C    Geant POSITION : SSST ===> X=0. Y=0. Z=ZS1 ALPHAZ=180. <===          163
         CALL AGSRESET                                                    164
         AG_X = 0.                                                        164
         AG_Y = 0.                                                        164
         AG_Z = ZS1                                                       164
         AG_ALPHAN=AG_ALPHAN+1                                            164
         AG_ALPHAK(AG_ALPHAN)=3                                           164
         AG_ALPHAL(AG_ALPHAN)=180.                                        164
         AG_TITLE = 'SSST'                                                164
         AG_EXNAME = 'POSITION'                                           164
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      164
         CALL AXPOSITION                                                  164
C    *                                                                    164
C    Create SSSS :                                                        165
C    Call To Local Procedure SSSS                                         165
         ASSIGN 5100 TO BLOCK_SSSS                                        165
         GO TO  5090                                                      165
5100     CONTINUE                                                         165
C    *                                                                    165
C    Geant POSITION : SSSS ===> X=0. Y=0. Z=-ZS2 <===                     166
         CALL AGSRESET                                                    166
         AG_X = 0.                                                        166
         AG_Y = 0.                                                        166
         AG_Z = -ZS2                                                      166
         AG_TITLE = 'SSSS'                                                166
         AG_EXNAME = 'POSITION'                                           166
         AG_PARLIST = 'X_Y_Z'                                             166
         CALL AXPOSITION                                                  166
C    *                                                                    166
C    Geant POSITION : SSSS ===> X=0. Y=0. Z=ZS2 <===                      166
         CALL AGSRESET                                                    167
         AG_X = 0.                                                        167
         AG_Y = 0.                                                        167
         AG_Z = ZS2                                                       167
         AG_TITLE = 'SSSS'                                                167
         AG_EXNAME = 'POSITION'                                           167
         AG_PARLIST = 'X_Y_Z'                                             167
         CALL AXPOSITION                                                  167
C    *                                                                    167
C    Geant POSITION : SSSS ===> X=0. Y=0. Z=-ZS2 ALPHAZ=180. <===         167
         CALL AGSRESET                                                    168
         AG_X = 0.                                                        168
         AG_Y = 0.                                                        168
         AG_Z = -ZS2                                                      168
         AG_ALPHAN=AG_ALPHAN+1                                            168
         AG_ALPHAK(AG_ALPHAN)=3                                           168
         AG_ALPHAL(AG_ALPHAN)=180.                                        168
         AG_TITLE = 'SSSS'                                                168
         AG_EXNAME = 'POSITION'                                           168
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      168
         CALL AXPOSITION                                                  168
C    *                                                                    168
C    Geant POSITION : SSSS ===> X=0. Y=0. Z=ZS2 ALPHAZ=180. <===          168
         CALL AGSRESET                                                    169
         AG_X = 0.                                                        169
         AG_Y = 0.                                                        169
         AG_Z = ZS2                                                       169
         AG_ALPHAN=AG_ALPHAN+1                                            169
         AG_ALPHAK(AG_ALPHAN)=3                                           169
         AG_ALPHAL(AG_ALPHAN)=180.                                        169
         AG_TITLE = 'SSSS'                                                169
         AG_EXNAME = 'POSITION'                                           169
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      169
         CALL AXPOSITION                                                  169
C    *                                                                    169
C    Create SSRT :                                                        170
C    Call To Local Procedure SSRT                                         170
         ASSIGN 5120 TO BLOCK_SSRT                                        170
         GO TO  5110                                                      170
5120     CONTINUE                                                         170
C    *                                                                    170
C    Geant POSITION : SSRT ===> X=0. Y=0. Z=-ZS1 <===                     171
         CALL AGSRESET                                                    171
         AG_X = 0.                                                        171
         AG_Y = 0.                                                        171
         AG_Z = -ZS1                                                      171
         AG_TITLE = 'SSRT'                                                171
         AG_EXNAME = 'POSITION'                                           171
         AG_PARLIST = 'X_Y_Z'                                             171
         CALL AXPOSITION                                                  171
C    *                                                                    171
C    Geant POSITION : SSRT ===> X=0. Y=0. Z=ZS1 <===                      171
         CALL AGSRESET                                                    172
         AG_X = 0.                                                        172
         AG_Y = 0.                                                        172
         AG_Z = ZS1                                                       172
         AG_TITLE = 'SSRT'                                                172
         AG_EXNAME = 'POSITION'                                           172
         AG_PARLIST = 'X_Y_Z'                                             172
         CALL AXPOSITION                                                  172
C    *                                                                    172
C    Geant POSITION : SSRT ===> X=0. Y=0. Z=-ZS1 ALPHAZ=180. <===         172
         CALL AGSRESET                                                    173
         AG_X = 0.                                                        173
         AG_Y = 0.                                                        173
         AG_Z = -ZS1                                                      173
         AG_ALPHAN=AG_ALPHAN+1                                            173
         AG_ALPHAK(AG_ALPHAN)=3                                           173
         AG_ALPHAL(AG_ALPHAN)=180.                                        173
         AG_TITLE = 'SSRT'                                                173
         AG_EXNAME = 'POSITION'                                           173
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      173
         CALL AXPOSITION                                                  173
C    *                                                                    173
C    Geant POSITION : SSRT ===> X=0. Y=0. Z=ZS1 ALPHAZ=180. <===          173
         CALL AGSRESET                                                    174
         AG_X = 0.                                                        174
         AG_Y = 0.                                                        174
         AG_Z = ZS1                                                       174
         AG_ALPHAN=AG_ALPHAN+1                                            174
         AG_ALPHAK(AG_ALPHAN)=3                                           174
         AG_ALPHAL(AG_ALPHAN)=180.                                        174
         AG_TITLE = 'SSRT'                                                174
         AG_EXNAME = 'POSITION'                                           174
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      174
         CALL AXPOSITION                                                  174
C    *                                                                    174
C    Create SSRS :                                                        175
C    Call To Local Procedure SSRS                                         175
         ASSIGN 5140 TO BLOCK_SSRS                                        175
         GO TO  5130                                                      175
5140     CONTINUE                                                         175
C    *                                                                    175
C    Geant POSITION : SSRS ===> X=0. Y=0. Z=-ZS2 <===                     176
         CALL AGSRESET                                                    176
         AG_X = 0.                                                        176
         AG_Y = 0.                                                        176
         AG_Z = -ZS2                                                      176
         AG_TITLE = 'SSRS'                                                176
         AG_EXNAME = 'POSITION'                                           176
         AG_PARLIST = 'X_Y_Z'                                             176
         CALL AXPOSITION                                                  176
C    *                                                                    176
C    Geant POSITION : SSRS ===> X=0. Y=0. Z=ZS2 <===                      176
         CALL AGSRESET                                                    177
         AG_X = 0.                                                        177
         AG_Y = 0.                                                        177
         AG_Z = ZS2                                                       177
         AG_TITLE = 'SSRS'                                                177
         AG_EXNAME = 'POSITION'                                           177
         AG_PARLIST = 'X_Y_Z'                                             177
         CALL AXPOSITION                                                  177
C    *                                                                    177
C    Geant POSITION : SSRS ===> X=0. Y=0. Z=-ZS2 ALPHAZ=180. <===         177
         CALL AGSRESET                                                    178
         AG_X = 0.                                                        178
         AG_Y = 0.                                                        178
         AG_Z = -ZS2                                                      178
         AG_ALPHAN=AG_ALPHAN+1                                            178
         AG_ALPHAK(AG_ALPHAN)=3                                           178
         AG_ALPHAL(AG_ALPHAN)=180.                                        178
         AG_TITLE = 'SSRS'                                                178
         AG_EXNAME = 'POSITION'                                           178
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      178
         CALL AXPOSITION                                                  178
C    *                                                                    178
C    Geant POSITION : SSRS ===> X=0. Y=0. Z=ZS2 ALPHAZ=180. <===          178
         CALL AGSRESET                                                    179
         AG_X = 0.                                                        179
         AG_Y = 0.                                                        179
         AG_Z = ZS2                                                       179
         AG_ALPHAN=AG_ALPHAN+1                                            179
         AG_ALPHAK(AG_ALPHAN)=3                                           179
         AG_ALPHAL(AG_ALPHAN)=180.                                        179
         AG_TITLE = 'SSRS'                                                179
         AG_EXNAME = 'POSITION'                                           179
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      179
         CALL AXPOSITION                                                  179
C    *                                                                    179
C    Create SSLB :                                                        180
C    Call To Local Procedure SSLB                                         180
         ASSIGN 5160 TO BLOCK_SSLB                                        180
         GO TO  5150                                                      180
5160     CONTINUE                                                         180
C    *                                                                    180
C    Geant POSITION : SSLB ===> X=YS1 Y=YS1 Z=-ZS3 ALPHAZ=45. <===        181
         CALL AGSRESET                                                    181
         AG_X = YS1                                                       181
         AG_Y = YS1                                                       181
         AG_Z = -ZS3                                                      181
         AG_ALPHAN=AG_ALPHAN+1                                            181
         AG_ALPHAK(AG_ALPHAN)=3                                           181
         AG_ALPHAL(AG_ALPHAN)=45.                                         181
         AG_TITLE = 'SSLB'                                                181
         AG_EXNAME = 'POSITION'                                           181
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      181
         CALL AXPOSITION                                                  181
C    *                                                                    181
C    Geant POSITION : SSLB ===> X=-YS1 Y=-YS1 Z=-ZS3 ALPHAZ=45. <===      181
         CALL AGSRESET                                                    182
         AG_X = -YS1                                                      182
         AG_Y = -YS1                                                      182
         AG_Z = -ZS3                                                      182
         AG_ALPHAN=AG_ALPHAN+1                                            182
         AG_ALPHAK(AG_ALPHAN)=3                                           182
         AG_ALPHAL(AG_ALPHAN)=45.                                         182
         AG_TITLE = 'SSLB'                                                182
         AG_EXNAME = 'POSITION'                                           182
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      182
         CALL AXPOSITION                                                  182
C    *                                                                    182
C    Geant POSITION : SSLB ===> X=YS1 Y=-YS1 Z=-ZS3 ALPHAZ=-45. <===      182
         CALL AGSRESET                                                    183
         AG_X = YS1                                                       183
         AG_Y = -YS1                                                      183
         AG_Z = -ZS3                                                      183
         AG_ALPHAN=AG_ALPHAN+1                                            183
         AG_ALPHAK(AG_ALPHAN)=3                                           183
         AG_ALPHAL(AG_ALPHAN)=-45.                                        183
         AG_TITLE = 'SSLB'                                                183
         AG_EXNAME = 'POSITION'                                           183
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      183
         CALL AXPOSITION                                                  183
C    *                                                                    183
C    Geant POSITION : SSLB ===> X=-YS1 Y=YS1 Z=-ZS3 ALPHAZ=-45. <===      183
         CALL AGSRESET                                                    184
         AG_X = -YS1                                                      184
         AG_Y = YS1                                                       184
         AG_Z = -ZS3                                                      184
         AG_ALPHAN=AG_ALPHAN+1                                            184
         AG_ALPHAK(AG_ALPHAN)=3                                           184
         AG_ALPHAL(AG_ALPHAN)=-45.                                        184
         AG_TITLE = 'SSLB'                                                184
         AG_EXNAME = 'POSITION'                                           184
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      184
         CALL AXPOSITION                                                  184
C    *                                                                    184
C    Geant POSITION : SSLB ===> X=YS1 Y=YS1 Z=ZS3 ALPHAZ=45. <===         184
         CALL AGSRESET                                                    185
         AG_X = YS1                                                       185
         AG_Y = YS1                                                       185
         AG_Z = ZS3                                                       185
         AG_ALPHAN=AG_ALPHAN+1                                            185
         AG_ALPHAK(AG_ALPHAN)=3                                           185
         AG_ALPHAL(AG_ALPHAN)=45.                                         185
         AG_TITLE = 'SSLB'                                                185
         AG_EXNAME = 'POSITION'                                           185
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      185
         CALL AXPOSITION                                                  185
C    *                                                                    185
C    Geant POSITION : SSLB ===> X=-YS1 Y=-YS1 Z=ZS3 ALPHAZ=45. <===       185
         CALL AGSRESET                                                    186
         AG_X = -YS1                                                      186
         AG_Y = -YS1                                                      186
         AG_Z = ZS3                                                       186
         AG_ALPHAN=AG_ALPHAN+1                                            186
         AG_ALPHAK(AG_ALPHAN)=3                                           186
         AG_ALPHAL(AG_ALPHAN)=45.                                         186
         AG_TITLE = 'SSLB'                                                186
         AG_EXNAME = 'POSITION'                                           186
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      186
         CALL AXPOSITION                                                  186
C    *                                                                    186
C    Geant POSITION : SSLB ===> X=YS1 Y=-YS1 Z=ZS3 ALPHAZ=-45. <===       186
         CALL AGSRESET                                                    187
         AG_X = YS1                                                       187
         AG_Y = -YS1                                                      187
         AG_Z = ZS3                                                       187
         AG_ALPHAN=AG_ALPHAN+1                                            187
         AG_ALPHAK(AG_ALPHAN)=3                                           187
         AG_ALPHAL(AG_ALPHAN)=-45.                                        187
         AG_TITLE = 'SSLB'                                                187
         AG_EXNAME = 'POSITION'                                           187
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      187
         CALL AXPOSITION                                                  187
C    *                                                                    187
C    Geant POSITION : SSLB ===> X=-YS1 Y=YS1 Z=ZS3 ALPHAZ=-45. <===       187
         CALL AGSRESET                                                    188
         AG_X = -YS1                                                      188
         AG_Y = YS1                                                       188
         AG_Z = ZS3                                                       188
         AG_ALPHAN=AG_ALPHAN+1                                            188
         AG_ALPHAK(AG_ALPHAN)=3                                           188
         AG_ALPHAL(AG_ALPHAN)=-45.                                        188
         AG_TITLE = 'SSLB'                                                188
         AG_EXNAME = 'POSITION'                                           188
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      188
         CALL AXPOSITION                                                  188
C    *                                                                    188
C    Create SSLT :                                                        189
C    Call To Local Procedure SSLT                                         189
         ASSIGN 5180 TO BLOCK_SSLT                                        189
         GO TO  5170                                                      189
5180     CONTINUE                                                         189
C    *                                                                    189
C    Geant POSITION : SSLT ===> X=YS2 Y=YS2 Z=-ZS4 <===                   190
         CALL AGSRESET                                                    190
         AG_X = YS2                                                       190
         AG_Y = YS2                                                       190
         AG_Z = -ZS4                                                      190
         AG_TITLE = 'SSLT'                                                190
         AG_EXNAME = 'POSITION'                                           190
         AG_PARLIST = 'X_Y_Z'                                             190
         CALL AXPOSITION                                                  190
C    *                                                                    190
C    Geant POSITION : SSLT ===> X=-YS2 Y=-YS2 Z=-ZS4 <===                 190
         CALL AGSRESET                                                    191
         AG_X = -YS2                                                      191
         AG_Y = -YS2                                                      191
         AG_Z = -ZS4                                                      191
         AG_TITLE = 'SSLT'                                                191
         AG_EXNAME = 'POSITION'                                           191
         AG_PARLIST = 'X_Y_Z'                                             191
         CALL AXPOSITION                                                  191
C    *                                                                    191
C    Geant POSITION : SSLT ===> X=YS2 Y=-YS2 Z=-ZS4 <===                  191
         CALL AGSRESET                                                    192
         AG_X = YS2                                                       192
         AG_Y = -YS2                                                      192
         AG_Z = -ZS4                                                      192
         AG_TITLE = 'SSLT'                                                192
         AG_EXNAME = 'POSITION'                                           192
         AG_PARLIST = 'X_Y_Z'                                             192
         CALL AXPOSITION                                                  192
C    *                                                                    192
C    Geant POSITION : SSLT ===> X=-YS2 Y=YS2 Z=-ZS4 <===                  192
         CALL AGSRESET                                                    193
         AG_X = -YS2                                                      193
         AG_Y = YS2                                                       193
         AG_Z = -ZS4                                                      193
         AG_TITLE = 'SSLT'                                                193
         AG_EXNAME = 'POSITION'                                           193
         AG_PARLIST = 'X_Y_Z'                                             193
         CALL AXPOSITION                                                  193
C    *                                                                    193
C    Geant POSITION : SSLT ===> X=YS2 Y=YS2 Z=ZS4 <===                    193
         CALL AGSRESET                                                    194
         AG_X = YS2                                                       194
         AG_Y = YS2                                                       194
         AG_Z = ZS4                                                       194
         AG_TITLE = 'SSLT'                                                194
         AG_EXNAME = 'POSITION'                                           194
         AG_PARLIST = 'X_Y_Z'                                             194
         CALL AXPOSITION                                                  194
C    *                                                                    194
C    Geant POSITION : SSLT ===> X=-YS2 Y=-YS2 Z=ZS4 <===                  194
         CALL AGSRESET                                                    195
         AG_X = -YS2                                                      195
         AG_Y = -YS2                                                      195
         AG_Z = ZS4                                                       195
         AG_TITLE = 'SSLT'                                                195
         AG_EXNAME = 'POSITION'                                           195
         AG_PARLIST = 'X_Y_Z'                                             195
         CALL AXPOSITION                                                  195
C    *                                                                    195
C    Geant POSITION : SSLT ===> X=YS2 Y=-YS2 Z=ZS4 <===                   195
         CALL AGSRESET                                                    196
         AG_X = YS2                                                       196
         AG_Y = -YS2                                                      196
         AG_Z = ZS4                                                       196
         AG_TITLE = 'SSLT'                                                196
         AG_EXNAME = 'POSITION'                                           196
         AG_PARLIST = 'X_Y_Z'                                             196
         CALL AXPOSITION                                                  196
C    *                                                                    196
C    Geant POSITION : SSLT ===> X=-YS2 Y=YS2 Z=ZS4 <===                   196
         CALL AGSRESET                                                    197
         AG_X = -YS2                                                      197
         AG_Y = YS2                                                       197
         AG_Z = ZS4                                                       197
         AG_TITLE = 'SSLT'                                                197
         AG_EXNAME = 'POSITION'                                           197
         AG_PARLIST = 'X_Y_Z'                                             197
         CALL AXPOSITION                                                  197
C    *                                                                    197
C    Create SCMP :                                                        198
C    Call To Local Procedure SCMP                                         198
         ASSIGN 5200 TO BLOCK_SCMP                                        198
         GO TO  5190                                                      198
5200     CONTINUE                                                         198
C    *                                                                    198
C    Geant POSITION : SCMP ===> X=YS3 Y=YS3 Z=ZS5 ALPHAZ=-45. <===        199
         CALL AGSRESET                                                    199
         AG_X = YS3                                                       199
         AG_Y = YS3                                                       199
         AG_Z = ZS5                                                       199
         AG_ALPHAN=AG_ALPHAN+1                                            199
         AG_ALPHAK(AG_ALPHAN)=3                                           199
         AG_ALPHAL(AG_ALPHAN)=-45.                                        199
         AG_TITLE = 'SCMP'                                                199
         AG_EXNAME = 'POSITION'                                           199
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      199
         CALL AXPOSITION                                                  199
C    *                                                                    199
C    Geant POSITION : SCMP ===> X=-YS3 Y=-YS3 Z=ZS5 ALPHAZ=-45. <===      199
         CALL AGSRESET                                                    200
         AG_X = -YS3                                                      200
         AG_Y = -YS3                                                      200
         AG_Z = ZS5                                                       200
         AG_ALPHAN=AG_ALPHAN+1                                            200
         AG_ALPHAK(AG_ALPHAN)=3                                           200
         AG_ALPHAL(AG_ALPHAN)=-45.                                        200
         AG_TITLE = 'SCMP'                                                200
         AG_EXNAME = 'POSITION'                                           200
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      200
         CALL AXPOSITION                                                  200
C    *                                                                    200
C    Geant POSITION : SCMP ===> X=YS3 Y=-YS3 Z=ZS5 ALPHAZ=45. <===        200
         CALL AGSRESET                                                    201
         AG_X = YS3                                                       201
         AG_Y = -YS3                                                      201
         AG_Z = ZS5                                                       201
         AG_ALPHAN=AG_ALPHAN+1                                            201
         AG_ALPHAK(AG_ALPHAN)=3                                           201
         AG_ALPHAL(AG_ALPHAN)=45.                                         201
         AG_TITLE = 'SCMP'                                                201
         AG_EXNAME = 'POSITION'                                           201
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      201
         CALL AXPOSITION                                                  201
C    *                                                                    201
C    Geant POSITION : SCMP ===> X=-YS3 Y=YS3 Z=ZS5 ALPHAZ=45. <===        201
         CALL AGSRESET                                                    202
         AG_X = -YS3                                                      202
         AG_Y = YS3                                                       202
         AG_Z = ZS5                                                       202
         AG_ALPHAN=AG_ALPHAN+1                                            202
         AG_ALPHAK(AG_ALPHAN)=3                                           202
         AG_ALPHAL(AG_ALPHAN)=45.                                         202
         AG_TITLE = 'SCMP'                                                202
         AG_EXNAME = 'POSITION'                                           202
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      202
         CALL AXPOSITION                                                  202
C    *                                                                    202
C    Geant POSITION : SCMP ===> X=YS3 Y=YS3 Z=-ZS5 ALPHAZ=-45. <===       202
         CALL AGSRESET                                                    203
         AG_X = YS3                                                       203
         AG_Y = YS3                                                       203
         AG_Z = -ZS5                                                      203
         AG_ALPHAN=AG_ALPHAN+1                                            203
         AG_ALPHAK(AG_ALPHAN)=3                                           203
         AG_ALPHAL(AG_ALPHAN)=-45.                                        203
         AG_TITLE = 'SCMP'                                                203
         AG_EXNAME = 'POSITION'                                           203
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      203
         CALL AXPOSITION                                                  203
C    *                                                                    203
C    Geant POSITION : SCMP ===> X=-YS3 Y=-YS3 Z=-ZS5 ALPHAZ=-45. <===     203
         CALL AGSRESET                                                    204
         AG_X = -YS3                                                      204
         AG_Y = -YS3                                                      204
         AG_Z = -ZS5                                                      204
         AG_ALPHAN=AG_ALPHAN+1                                            204
         AG_ALPHAK(AG_ALPHAN)=3                                           204
         AG_ALPHAL(AG_ALPHAN)=-45.                                        204
         AG_TITLE = 'SCMP'                                                204
         AG_EXNAME = 'POSITION'                                           204
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      204
         CALL AXPOSITION                                                  204
C    *                                                                    204
C    Geant POSITION : SCMP ===> X=YS3 Y=-YS3 Z=-ZS5 ALPHAZ=45. <===       204
         CALL AGSRESET                                                    205
         AG_X = YS3                                                       205
         AG_Y = -YS3                                                      205
         AG_Z = -ZS5                                                      205
         AG_ALPHAN=AG_ALPHAN+1                                            205
         AG_ALPHAK(AG_ALPHAN)=3                                           205
         AG_ALPHAL(AG_ALPHAN)=45.                                         205
         AG_TITLE = 'SCMP'                                                205
         AG_EXNAME = 'POSITION'                                           205
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      205
         CALL AXPOSITION                                                  205
C    *                                                                    205
C    Geant POSITION : SCMP ===> X=-YS3 Y=YS3 Z=-ZS5 ALPHAZ=45. <===       205
         CALL AGSRESET                                                    206
         AG_X = -YS3                                                      206
         AG_Y = YS3                                                       206
         AG_Z = -ZS5                                                      206
         AG_ALPHAN=AG_ALPHAN+1                                            206
         AG_ALPHAK(AG_ALPHAN)=3                                           206
         AG_ALPHAL(AG_ALPHAN)=45.                                         206
         AG_TITLE = 'SCMP'                                                206
         AG_EXNAME = 'POSITION'                                           206
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      206
         CALL AXPOSITION                                                  206
C    *                                                                    206
C    Create SCVM :                                                        207
C    Call To Local Procedure SCVM                                         207
         ASSIGN 5220 TO BLOCK_SCVM                                        207
         GO TO  5210                                                      207
5220     CONTINUE                                                         207
C    *                                                                    207
C    Geant POSITION : SCVM ===> X=YS4 Y=YS4 Z=ZS5 ALPHAZ=-45. <===        208
         CALL AGSRESET                                                    208
         AG_X = YS4                                                       208
         AG_Y = YS4                                                       208
         AG_Z = ZS5                                                       208
         AG_ALPHAN=AG_ALPHAN+1                                            208
         AG_ALPHAK(AG_ALPHAN)=3                                           208
         AG_ALPHAL(AG_ALPHAN)=-45.                                        208
         AG_TITLE = 'SCVM'                                                208
         AG_EXNAME = 'POSITION'                                           208
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      208
         CALL AXPOSITION                                                  208
C    *                                                                    208
C    Geant POSITION : SCVM ===> X=-YS4 Y=-YS4 Z=ZS5 ALPHAZ=135. <===      208
         CALL AGSRESET                                                    209
         AG_X = -YS4                                                      209
         AG_Y = -YS4                                                      209
         AG_Z = ZS5                                                       209
         AG_ALPHAN=AG_ALPHAN+1                                            209
         AG_ALPHAK(AG_ALPHAN)=3                                           209
         AG_ALPHAL(AG_ALPHAN)=135.                                        209
         AG_TITLE = 'SCVM'                                                209
         AG_EXNAME = 'POSITION'                                           209
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      209
         CALL AXPOSITION                                                  209
C    *                                                                    209
C    Geant POSITION : SCVM ===> X=YS4 Y=-YS4 Z=ZS5 ALPHAZ=-135. <===      209
         CALL AGSRESET                                                    210
         AG_X = YS4                                                       210
         AG_Y = -YS4                                                      210
         AG_Z = ZS5                                                       210
         AG_ALPHAN=AG_ALPHAN+1                                            210
         AG_ALPHAK(AG_ALPHAN)=3                                           210
         AG_ALPHAL(AG_ALPHAN)=-135.                                       210
         AG_TITLE = 'SCVM'                                                210
         AG_EXNAME = 'POSITION'                                           210
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      210
         CALL AXPOSITION                                                  210
C    *                                                                    210
C    Geant POSITION : SCVM ===> X=-YS4 Y=YS4 Z=ZS5 ALPHAZ=45. <===        210
         CALL AGSRESET                                                    211
         AG_X = -YS4                                                      211
         AG_Y = YS4                                                       211
         AG_Z = ZS5                                                       211
         AG_ALPHAN=AG_ALPHAN+1                                            211
         AG_ALPHAK(AG_ALPHAN)=3                                           211
         AG_ALPHAL(AG_ALPHAN)=45.                                         211
         AG_TITLE = 'SCVM'                                                211
         AG_EXNAME = 'POSITION'                                           211
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      211
         CALL AXPOSITION                                                  211
C    *                                                                    211
C    Geant POSITION : SCVM ===> X=YS4 Y=YS4 Z=-ZS5 ALPHAZ=-45. <===       211
         CALL AGSRESET                                                    212
         AG_X = YS4                                                       212
         AG_Y = YS4                                                       212
         AG_Z = -ZS5                                                      212
         AG_ALPHAN=AG_ALPHAN+1                                            212
         AG_ALPHAK(AG_ALPHAN)=3                                           212
         AG_ALPHAL(AG_ALPHAN)=-45.                                        212
         AG_TITLE = 'SCVM'                                                212
         AG_EXNAME = 'POSITION'                                           212
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      212
         CALL AXPOSITION                                                  212
C    *                                                                    212
C    Geant POSITION : SCVM ===> X=-YS4 Y=-YS4 Z=-ZS5 ALPHAZ=135. <===     212
         CALL AGSRESET                                                    213
         AG_X = -YS4                                                      213
         AG_Y = -YS4                                                      213
         AG_Z = -ZS5                                                      213
         AG_ALPHAN=AG_ALPHAN+1                                            213
         AG_ALPHAK(AG_ALPHAN)=3                                           213
         AG_ALPHAL(AG_ALPHAN)=135.                                        213
         AG_TITLE = 'SCVM'                                                213
         AG_EXNAME = 'POSITION'                                           213
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      213
         CALL AXPOSITION                                                  213
C    *                                                                    213
C    Geant POSITION : SCVM ===> X=YS4 Y=-YS4 Z=-ZS5 ALPHAZ=-135. <===     213
         CALL AGSRESET                                                    214
         AG_X = YS4                                                       214
         AG_Y = -YS4                                                      214
         AG_Z = -ZS5                                                      214
         AG_ALPHAN=AG_ALPHAN+1                                            214
         AG_ALPHAK(AG_ALPHAN)=3                                           214
         AG_ALPHAL(AG_ALPHAN)=-135.                                       214
         AG_TITLE = 'SCVM'                                                214
         AG_EXNAME = 'POSITION'                                           214
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      214
         CALL AXPOSITION                                                  214
C    *                                                                    214
C    Geant POSITION : SCVM ===> X=-YS4 Y=YS4 Z=-ZS5 ALPHAZ=45. <===       214
         CALL AGSRESET                                                    215
         AG_X = -YS4                                                      215
         AG_Y = YS4                                                       215
         AG_Z = -ZS5                                                      215
         AG_ALPHAN=AG_ALPHAN+1                                            215
         AG_ALPHAK(AG_ALPHAN)=3                                           215
         AG_ALPHAL(AG_ALPHAN)=45.                                         215
         AG_TITLE = 'SCVM'                                                215
         AG_EXNAME = 'POSITION'                                           215
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      215
         CALL AXPOSITION                                                  215
C    *                                                                    215
5011  CALL AGSPOPB                                                        216
      GO TO BLOCK_SFMO, (5010 ,5020,5030)                                 216
C End of Local Procedure                                                  216
C ----------------------------------------------------------------------  217
C ------------ SFLM                                                       217
C Begin Local Procedure SFLM                                              218
C SFLM IS THE MOTHER OF THE LADDER                                        218
5040  CONTINUE                                                            218
         AG_TITLE='SFLM'                                                  218
         AG_MODULE=MODULE                                                 218
         AG_MARK='V00'                                                    218
         CALL AGSPUSH                                                     218
         AG_MODULE=MODULE                                                 218
C    *                                                                    218
C    Geant MATERIAL : AIR ===> inherited arguments used <===              219
         CALL AGSRESET                                                    219
         AG_TITLE = 'AIR'                                                 219
         AG_EXNAME = 'MATERIAL'                                           219
         AG_PARLIST = 'NONE'                                              219
         CALL AXMATERIAL                                                  219
C    *                                                                    219
C    Geant ATTRIBUTE : SFLM ===> SEEN=0 COLO=6 <===                       219
         CALL AGSRESET                                                    220
         AG_SEEN = 0                                                      220
         AG_COLO = 6                                                      220
         AG_TITLE = 'SFLM'                                                220
         AG_EXNAME = 'ATTRIBUTE'                                          220
         AG_PARLIST = 'SEEN_COLO'                                         220
         CALL AXATTRIBUTE                                                 220
C    *                                                                    220
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=(SFPA_DMTHK+SFPA_GPTHK+SF  220
C    PA_SMTHK)/2, DZ=SFPA_SMLEN/2 <===                                    220
         CALL AGSRESET                                                    222
         AG_DX = SFPA_DMWID/2                                             222
         AG_DY = (SFPA_DMTHK+SFPA_GPTHK+SFPA_SMTHK)/2                     222
         AG_DZ = SFPA_SMLEN/2                                             222
         AG_TITLE = 'BOX'                                                 222
         AG_EXNAME = 'SHAPE'                                              222
         AG_PARLIST = 'DX_DY_DZ'                                          222
         CALL AXSHAPE                                                     222
C    *                                                                    222
C    Create SFDM :                                                        223
C    Call To Local Procedure SFDM                                         223
         ASSIGN 5240 TO BLOCK_SFDM                                        223
         GO TO  5230                                                      223
5240     CONTINUE                                                         223
C    *                                                                    223
C    Geant POSITION : SFDM ===> Y=-(SFPA_SMTHK+SFPA_GPTHK)/2 <===         224
         CALL AGSRESET                                                    224
         AG_Y = -(SFPA_SMTHK+SFPA_GPTHK)/2                                224
         AG_TITLE = 'SFDM'                                                224
         AG_EXNAME = 'POSITION'                                           224
         AG_PARLIST = 'Y'                                                 224
         CALL AXPOSITION                                                  224
C    *                                                                    224
C    Create SFSM :                                                        225
C    Call To Local Procedure SFSM                                         225
         ASSIGN 5260 TO BLOCK_SFSM                                        225
         GO TO  5250                                                      225
5260     CONTINUE                                                         225
C    *                                                                    225
C    Geant POSITION : SFSM ===> Y=(SFPA_DMTHK+SFPA_GPTHK)/2 <===          226
         CALL AGSRESET                                                    226
         AG_Y = (SFPA_DMTHK+SFPA_GPTHK)/2                                 226
         AG_TITLE = 'SFSM'                                                226
         AG_EXNAME = 'POSITION'                                           226
         AG_PARLIST = 'Y'                                                 226
         CALL AXPOSITION                                                  226
C    *                                                                    226
5041  CALL AGSPOPB                                                        227
      GO TO BLOCK_SFLM, (5040 ,5050)                                      227
C End of Local Procedure                                                  227
C ----------------------------------------------------------------------  228
C ------------ SFDM                                                       228
C Begin Local Procedure SFDM                                              229
C SFDM IS THE MOTHER OF THE DETECTORS                                     229
5230  CONTINUE                                                            229
         AG_TITLE='SFDM'                                                  229
         AG_MODULE=MODULE                                                 229
         AG_MARK='V00'                                                    229
         CALL AGSPUSH                                                     229
         AG_MODULE=MODULE                                                 229
C    *                                                                    229
C    Geant MATERIAL : AIR ===> inherited arguments used <===              230
         CALL AGSRESET                                                    230
         AG_TITLE = 'AIR'                                                 230
         AG_EXNAME = 'MATERIAL'                                           230
         AG_PARLIST = 'NONE'                                              230
         CALL AXMATERIAL                                                  230
C    *                                                                    230
C    Geant ATTRIBUTE : SFDM ===> SEEN=0 COLO=6 <===                       230
         CALL AGSRESET                                                    231
         AG_SEEN = 0                                                      231
         AG_COLO = 6                                                      231
         AG_TITLE = 'SFDM'                                                231
         AG_EXNAME = 'ATTRIBUTE'                                          231
         AG_PARLIST = 'SEEN_COLO'                                         231
         CALL AXATTRIBUTE                                                 231
C    *                                                                    231
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=SFPA_DMTHK/2, DZ=SFPA_WPL  231
C    EN/2 <===                                                            231
         CALL AGSRESET                                                    233
         AG_DX = SFPA_DMWID/2                                             233
         AG_DY = SFPA_DMTHK/2                                             233
         AG_DZ = SFPA_WPLEN/2                                             233
         AG_TITLE = 'BOX'                                                 233
         AG_EXNAME = 'SHAPE'                                              233
         AG_PARLIST = 'DX_DY_DZ'                                          233
         CALL AXSHAPE                                                     233
C    *                                                                    233
         WAFPCKLEN=SFPA_WPLEN/(SFPA_NSSD*1.)                              233
C    *                                                                    235
         IWAF=1                                                           235
            GO TO 5273                                                    235
5271        IWAF=IWAF+1                                                   235
5273        IF(IWAF-(SFPA_NSSD).GT.0) GO TO 5272                          235
C       Create SFSW :                                                     236
C       Call To Local Procedure SFSW                                      236
            ASSIGN 5290 TO BLOCK_SFSW                                     236
            GO TO  5280                                                   236
5290        CONTINUE                                                      236
C       *                                                                 236
C       Geant POSITION : SFSW ===> Z=-(SFPA_WPLEN+WAFPCKLEN)/2+IWAF*WAFP  237
C       CKLEN <===                                                        237
            CALL AGSRESET                                                 237
            AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+IWAF*WAFPCKLEN               237
            AG_TITLE = 'SFSW'                                             237
            AG_EXNAME = 'POSITION'                                        237
            AG_PARLIST = 'Z'                                              237
            CALL AXPOSITION                                               237
C       *                                                                 237
         GO TO 5271                                                       238
5272     CONTINUE                                                         238
5231  CALL AGSPOPB                                                        239
      GO TO BLOCK_SFDM, (5230 ,5240)                                      239
C End of Local Procedure                                                  239
C ----------------------------------------------------------------------  240
C ------------ SFSW                                                       240
C Begin Local Procedure SFSW                                              241
C SFSW IS A SINGLE WAFER CONTAINER                                        241
5280  CONTINUE                                                            241
         AG_TITLE='SFSW'                                                  241
         AG_MODULE=MODULE                                                 241
         AG_MARK='V00'                                                    241
         CALL AGSPUSH                                                     241
         AG_MODULE=MODULE                                                 241
C    *                                                                    241
C    Geant MATERIAL : AIR ===> inherited arguments used <===              242
         CALL AGSRESET                                                    242
         AG_TITLE = 'AIR'                                                 242
         AG_EXNAME = 'MATERIAL'                                           242
         AG_PARLIST = 'NONE'                                              242
         CALL AXMATERIAL                                                  242
C    *                                                                    242
C    Geant ATTRIBUTE : SFSW ===> SEEN=0 COLO=6 <===                       242
         CALL AGSRESET                                                    243
         AG_SEEN = 0                                                      243
         AG_COLO = 6                                                      243
         AG_TITLE = 'SFSW'                                                243
         AG_EXNAME = 'ATTRIBUTE'                                          243
         AG_PARLIST = 'SEEN_COLO'                                         243
         CALL AXATTRIBUTE                                                 243
C    *                                                                    243
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=1. DZ=2.25 <===            243
         CALL AGSRESET                                                    244
         AG_DX = SFPA_DMWID/2                                             244
         AG_DY = 1.                                                       244
         AG_DZ = 2.25                                                     244
         AG_TITLE = 'BOX'                                                 244
         AG_EXNAME = 'SHAPE'                                              244
         AG_PARLIST = 'DX_DY_DZ'                                          244
         CALL AXSHAPE                                                     244
C    *                                                                    244
C    Create SFSD :                                                        245
C    Call To Local Procedure SFSD                                         245
         ASSIGN 5310 TO BLOCK_SFSD                                        245
         GO TO  5300                                                      245
5310     CONTINUE                                                         245
C    *                                                                    245
C    Geant POSITION : SFSD ===> inherited arguments used <===             246
         CALL AGSRESET                                                    246
         AG_TITLE = 'SFSD'                                                246
         AG_EXNAME = 'POSITION'                                           246
         AG_PARLIST = 'NONE'                                              246
         CALL AXPOSITION                                                  246
C    *                                                                    246
C    Create SFRA :                                                        247
C    Call To Local Procedure SFRA                                         247
         ASSIGN 5330 TO BLOCK_SFRA                                        247
         GO TO  5320                                                      247
5330     CONTINUE                                                         247
C    *                                                                    247
C    Geant POSITION : SFRA ===> X=0. Y=0.099 Z=1.1 <===                   248
         CALL AGSRESET                                                    248
         AG_X = 0.                                                        248
         AG_Y = 0.099                                                     248
         AG_Z = 1.1                                                       248
         AG_TITLE = 'SFRA'                                                248
         AG_EXNAME = 'POSITION'                                           248
         AG_PARLIST = 'X_Y_Z'                                             248
         CALL AXPOSITION                                                  248
C    *                                                                    248
C    Geant POSITION : SFRA ===> X=0. Y=0.099 Z=-1.1 <===                  248
         CALL AGSRESET                                                    249
         AG_X = 0.                                                        249
         AG_Y = 0.099                                                     249
         AG_Z = -1.1                                                      249
         AG_TITLE = 'SFRA'                                                249
         AG_EXNAME = 'POSITION'                                           249
         AG_PARLIST = 'X_Y_Z'                                             249
         CALL AXPOSITION                                                  249
C    *                                                                    249
C    Create SFRS :                                                        250
C    Call To Local Procedure SFRS                                         250
         ASSIGN 5350 TO BLOCK_SFRS                                        250
         GO TO  5340                                                      250
5350     CONTINUE                                                         250
C    *                                                                    250
C    Geant POSITION : SFRS ===> X=3.8 Y=0.0265 Z=1.1 <===                 251
         CALL AGSRESET                                                    251
         AG_X = 3.8                                                       251
         AG_Y = 0.0265                                                    251
         AG_Z = 1.1                                                       251
         AG_TITLE = 'SFRS'                                                251
         AG_EXNAME = 'POSITION'                                           251
         AG_PARLIST = 'X_Y_Z'                                             251
         CALL AXPOSITION                                                  251
C    *                                                                    251
C    Geant POSITION : SFRS ===> X=3.8 Y=0.0265 Z=-1.1 <===                251
         CALL AGSRESET                                                    252
         AG_X = 3.8                                                       252
         AG_Y = 0.0265                                                    252
         AG_Z = -1.1                                                      252
         AG_TITLE = 'SFRS'                                                252
         AG_EXNAME = 'POSITION'                                           252
         AG_PARLIST = 'X_Y_Z'                                             252
         CALL AXPOSITION                                                  252
C    *                                                                    252
C    Geant POSITION : SFRS ===> X=-3.8 Y=0.0265 Z=1.1 <===                252
         CALL AGSRESET                                                    253
         AG_X = -3.8                                                      253
         AG_Y = 0.0265                                                    253
         AG_Z = 1.1                                                       253
         AG_TITLE = 'SFRS'                                                253
         AG_EXNAME = 'POSITION'                                           253
         AG_PARLIST = 'X_Y_Z'                                             253
         CALL AXPOSITION                                                  253
C    *                                                                    253
C    Geant POSITION : SFRS ===> X=-3.8 Y=0.0265 Z=-1.1 <===               253
         CALL AGSRESET                                                    254
         AG_X = -3.8                                                      254
         AG_Y = 0.0265                                                    254
         AG_Z = -1.1                                                      254
         AG_TITLE = 'SFRS'                                                254
         AG_EXNAME = 'POSITION'                                           254
         AG_PARLIST = 'X_Y_Z'                                             254
         CALL AXPOSITION                                                  254
C    *                                                                    254
C    Create SFFX :                                                        255
C    Call To Local Procedure SFFX                                         255
         ASSIGN 5370 TO BLOCK_SFFX                                        255
         GO TO  5360                                                      255
5370     CONTINUE                                                         255
C    *                                                                    255
C    Geant POSITION : SFFX ===> X=0. Y=0.099+0.0190+0.0007 Z=1.1 <===     256
         CALL AGSRESET                                                    256
         AG_X = 0.                                                        256
         AG_Y = 0.099+0.0190+0.0007                                       256
         AG_Z = 1.1                                                       256
         AG_TITLE = 'SFFX'                                                256
         AG_EXNAME = 'POSITION'                                           256
         AG_PARLIST = 'X_Y_Z'                                             256
         CALL AXPOSITION                                                  256
C    *                                                                    256
C    Geant POSITION : SFFX ===> X=0. Y=0.099+0.0190+0.0007 Z=-1.1 <===    256
         CALL AGSRESET                                                    257
         AG_X = 0.                                                        257
         AG_Y = 0.099+0.0190+0.0007                                       257
         AG_Z = -1.1                                                      257
         AG_TITLE = 'SFFX'                                                257
         AG_EXNAME = 'POSITION'                                           257
         AG_PARLIST = 'X_Y_Z'                                             257
         CALL AXPOSITION                                                  257
C    *                                                                    257
C    Create SFPI :                                                        258
C    Call To Local Procedure SFPI                                         258
         ASSIGN 5390 TO BLOCK_SFPI                                        258
         GO TO  5380                                                      258
5390     CONTINUE                                                         258
C    *                                                                    258
C    Geant POSITION : SFPI ===> X=3.2 Y=0.099+0.019+0.35-0.0250 Z=0.1+0.  259
C    6 ALPHAX=90 <===                                                     259
         CALL AGSRESET                                                    259
         AG_X = 3.2                                                       259
         AG_Y = 0.099+0.019+0.35-0.0250                                   259
         AG_Z = 0.1+0.6                                                   259
         AG_ALPHAN=AG_ALPHAN+1                                            259
         AG_ALPHAK(AG_ALPHAN)=1                                           259
         AG_ALPHAL(AG_ALPHAN)=90                                          259
         AG_TITLE = 'SFPI'                                                259
         AG_EXNAME = 'POSITION'                                           259
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      259
         CALL AXPOSITION                                                  259
C    *                                                                    259
C    Geant POSITION : SFPI ===> X=-3.2 Y=0.099+0.019+0.35-0.0250 Z=0.7 A  259
C    LPHAX=90 <===                                                        259
         CALL AGSRESET                                                    260
         AG_X = -3.2                                                      260
         AG_Y = 0.099+0.019+0.35-0.0250                                   260
         AG_Z = 0.7                                                       260
         AG_ALPHAN=AG_ALPHAN+1                                            260
         AG_ALPHAK(AG_ALPHAN)=1                                           260
         AG_ALPHAL(AG_ALPHAN)=90                                          260
         AG_TITLE = 'SFPI'                                                260
         AG_EXNAME = 'POSITION'                                           260
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      260
         CALL AXPOSITION                                                  260
C    *                                                                    260
C    Geant POSITION : SFPI ===> X=3.2 Y=0.099+0.019+0.35-0.0250 Z=-0.7 A  260
C    LPHAX=90 <===                                                        260
         CALL AGSRESET                                                    261
         AG_X = 3.2                                                       261
         AG_Y = 0.099+0.019+0.35-0.0250                                   261
         AG_Z = -0.7                                                      261
         AG_ALPHAN=AG_ALPHAN+1                                            261
         AG_ALPHAK(AG_ALPHAN)=1                                           261
         AG_ALPHAL(AG_ALPHAN)=90                                          261
         AG_TITLE = 'SFPI'                                                261
         AG_EXNAME = 'POSITION'                                           261
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      261
         CALL AXPOSITION                                                  261
C    *                                                                    261
C    Geant POSITION : SFPI ===> X=-3.2 Y=0.099+0.019+0.35-0.0250 Z=-0.7   261
C    ALPHAX=90 <===                                                       261
         CALL AGSRESET                                                    262
         AG_X = -3.2                                                      262
         AG_Y = 0.099+0.019+0.35-0.0250                                   262
         AG_Z = -0.7                                                      262
         AG_ALPHAN=AG_ALPHAN+1                                            262
         AG_ALPHAK(AG_ALPHAN)=1                                           262
         AG_ALPHAL(AG_ALPHAN)=90                                          262
         AG_TITLE = 'SFPI'                                                262
         AG_EXNAME = 'POSITION'                                           262
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      262
         CALL AXPOSITION                                                  262
C    *                                                                    262
C    Create SFAA :                                                        263
C    Call To Local Procedure SFAA                                         263
         ASSIGN 5410 TO BLOCK_SFAA                                        263
         GO TO  5400                                                      263
5410     CONTINUE                                                         263
C    *                                                                    263
C    Geant POSITION : SFAA ===> X=-0.325-0.3 Y=0.099+0.0190+0.0007*2+0.0  264
C    150 Z=1.1+1.0-0.02-0.4 <===                                          264
         CALL AGSRESET                                                    264
         AG_X = -0.325-0.3                                                264
         AG_Y = 0.099+0.0190+0.0007*2+0.0150                              264
         AG_Z = 1.1+1.0-0.02-0.4                                          264
         AG_TITLE = 'SFAA'                                                264
         AG_EXNAME = 'POSITION'                                           264
         AG_PARLIST = 'X_Y_Z'                                             264
         CALL AXPOSITION                                                  264
C    *                                                                    264
C    Geant POSITION : SFAA ===> X=-0.325-0.3-0.65-0.6 Y=0.099+0.0190+0.0  264
C    007*2+0.0150 Z=1.1+1.0-0.02-0.4 <===                                 264
         CALL AGSRESET                                                    266
         AG_X = -0.325-0.3-0.65-0.6                                       266
         AG_Y = 0.099+0.0190+0.0007*2+0.0150                              266
         AG_Z = 1.1+1.0-0.02-0.4                                          266
         AG_TITLE = 'SFAA'                                                266
         AG_EXNAME = 'POSITION'                                           266
         AG_PARLIST = 'X_Y_Z'                                             266
         CALL AXPOSITION                                                  266
C    *                                                                    266
C    Geant POSITION : SFAA ===> X=-0.325-0.3-0.65-0.6-0.65-0.6 Y=0.099+0  266
C    .0190+0.0007*2+0.0150 Z=1.1+1.0-0.02-0.4 <===                        266
         CALL AGSRESET                                                    268
         AG_X = -0.325-0.3-0.65-0.6-0.65-0.6                              268
         AG_Y = 0.099+0.0190+0.0007*2+0.0150                              268
         AG_Z = 1.1+1.0-0.02-0.4                                          268
         AG_TITLE = 'SFAA'                                                268
         AG_EXNAME = 'POSITION'                                           268
         AG_PARLIST = 'X_Y_Z'                                             268
         CALL AXPOSITION                                                  268
C    *                                                                    268
C    Geant POSITION : SFAA ===> X=+0.325+0.3 Y=0.099+0.0190+0.0007*2+0.0  268
C    150 Z=1.1+1.0-0.02-0.4 <===                                          268
         CALL AGSRESET                                                    269
         AG_X = +0.325+0.3                                                269
         AG_Y = 0.099+0.0190+0.0007*2+0.0150                              269
         AG_Z = 1.1+1.0-0.02-0.4                                          269
         AG_TITLE = 'SFAA'                                                269
         AG_EXNAME = 'POSITION'                                           269
         AG_PARLIST = 'X_Y_Z'                                             269
         CALL AXPOSITION                                                  269
C    *                                                                    269
C    Geant POSITION : SFAA ===> X=+0.325+0.3+0.65+0.6 Y=0.099+0.0190+0.0  269
C    007*2+0.0150 Z=1.1+1.0-0.02-0.4 <===                                 269
         CALL AGSRESET                                                    271
         AG_X = +0.325+0.3+0.65+0.6                                       271
         AG_Y = 0.099+0.0190+0.0007*2+0.0150                              271
         AG_Z = 1.1+1.0-0.02-0.4                                          271
         AG_TITLE = 'SFAA'                                                271
         AG_EXNAME = 'POSITION'                                           271
         AG_PARLIST = 'X_Y_Z'                                             271
         CALL AXPOSITION                                                  271
C    *                                                                    271
C    Geant POSITION : SFAA ===> X=+0.325+0.3+0.65+0.6+0.65+0.6 Y=0.099+0  271
C    .0190+0.0007*2+0.0150 Z=1.1+1.0-0.02-0.4 <===                        271
         CALL AGSRESET                                                    273
         AG_X = +0.325+0.3+0.65+0.6+0.65+0.6                              273
         AG_Y = 0.099+0.0190+0.0007*2+0.0150                              273
         AG_Z = 1.1+1.0-0.02-0.4                                          273
         AG_TITLE = 'SFAA'                                                273
         AG_EXNAME = 'POSITION'                                           273
         AG_PARLIST = 'X_Y_Z'                                             273
         CALL AXPOSITION                                                  273
C    *                                                                    273
C    Geant POSITION : SFAA ===> X=-0.325-0.3 Y=0.099+0.0190+0.0007*2+0.0  273
C    150 Z=-1.1-1.0+0.02+0.4 <===                                         273
         CALL AGSRESET                                                    274
         AG_X = -0.325-0.3                                                274
         AG_Y = 0.099+0.0190+0.0007*2+0.0150                              274
         AG_Z = -1.1-1.0+0.02+0.4                                         274
         AG_TITLE = 'SFAA'                                                274
         AG_EXNAME = 'POSITION'                                           274
         AG_PARLIST = 'X_Y_Z'                                             274
         CALL AXPOSITION                                                  274
C    *                                                                    274
C    Geant POSITION : SFAA ===> X=-0.325-0.3-0.65-0.6 Y=0.099+0.0190+0.0  274
C    007*2+0.0150 Z=-1.1-1.0+0.02+0.4 <===                                274
         CALL AGSRESET                                                    276
         AG_X = -0.325-0.3-0.65-0.6                                       276
         AG_Y = 0.099+0.0190+0.0007*2+0.0150                              276
         AG_Z = -1.1-1.0+0.02+0.4                                         276
         AG_TITLE = 'SFAA'                                                276
         AG_EXNAME = 'POSITION'                                           276
         AG_PARLIST = 'X_Y_Z'                                             276
         CALL AXPOSITION                                                  276
C    *                                                                    276
C    Geant POSITION : SFAA ===> X=-0.325-0.3-0.65-0.6-0.65-0.6 Y=0.099+0  276
C    .0190+0.0007*2+0.0150 Z=-1.1-1.0+0.02+0.4 <===                       276
         CALL AGSRESET                                                    278
         AG_X = -0.325-0.3-0.65-0.6-0.65-0.6                              278
         AG_Y = 0.099+0.0190+0.0007*2+0.0150                              278
         AG_Z = -1.1-1.0+0.02+0.4                                         278
         AG_TITLE = 'SFAA'                                                278
         AG_EXNAME = 'POSITION'                                           278
         AG_PARLIST = 'X_Y_Z'                                             278
         CALL AXPOSITION                                                  278
C    *                                                                    278
C    Geant POSITION : SFAA ===> X=+0.325+0.3 Y=0.099+0.0190+0.0007*2+0.0  278
C    150 Z=-1.1-1.0+0.02+0.4 <===                                         278
         CALL AGSRESET                                                    279
         AG_X = +0.325+0.3                                                279
         AG_Y = 0.099+0.0190+0.0007*2+0.0150                              279
         AG_Z = -1.1-1.0+0.02+0.4                                         279
         AG_TITLE = 'SFAA'                                                279
         AG_EXNAME = 'POSITION'                                           279
         AG_PARLIST = 'X_Y_Z'                                             279
         CALL AXPOSITION                                                  279
C    *                                                                    279
C    Geant POSITION : SFAA ===> X=+0.325+0.3+0.65+0.6 Y=0.099+0.0190+0.0  279
C    007*2+0.0150 Z=-1.1-1.0+0.02+0.4 <===                                279
         CALL AGSRESET                                                    281
         AG_X = +0.325+0.3+0.65+0.6                                       281
         AG_Y = 0.099+0.0190+0.0007*2+0.0150                              281
         AG_Z = -1.1-1.0+0.02+0.4                                         281
         AG_TITLE = 'SFAA'                                                281
         AG_EXNAME = 'POSITION'                                           281
         AG_PARLIST = 'X_Y_Z'                                             281
         CALL AXPOSITION                                                  281
C    *                                                                    281
C    Geant POSITION : SFAA ===> X=+0.325+0.3+0.65+0.6+0.65+0.6 Y=0.099+0  281
C    .0190+0.0007*2+0.0150 Z=-1.1-1.0+0.02+0.4 <===                       281
         CALL AGSRESET                                                    283
         AG_X = +0.325+0.3+0.65+0.6+0.65+0.6                              283
         AG_Y = 0.099+0.0190+0.0007*2+0.0150                              283
         AG_Z = -1.1-1.0+0.02+0.4                                         283
         AG_TITLE = 'SFAA'                                                283
         AG_EXNAME = 'POSITION'                                           283
         AG_PARLIST = 'X_Y_Z'                                             283
         CALL AXPOSITION                                                  283
C    *                                                                    283
5281  CALL AGSPOPB                                                        284
      GO TO BLOCK_SFSW, (5280 ,5290)                                      284
C End of Local Procedure                                                  284
C ----------------------------------------------------------------------  285
C ------------ SFRA                                                       285
C Begin Local Procedure SFRA                                              286
C SFRA IS THE HYBRID STIFFNEER                                            286
5320  CONTINUE                                                            286
         AG_TITLE='SFRA'                                                  286
         AG_MODULE=MODULE                                                 286
         AG_MARK='V00'                                                    286
         CALL AGSPUSH                                                     286
         AG_MODULE=MODULE                                                 286
C    *                                                                    286
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           287
         CALL AGSRESET                                                    287
         AG_TITLE = 'CARBON'                                              287
         AG_EXNAME = 'MATERIAL'                                           287
         AG_PARLIST = 'NONE'                                              287
         CALL AXMATERIAL                                                  287
C    *                                                                    287
C    Geant ATTRIBUTE : SFRA ===> SEEN=2 COLO=1 <===                       287
         CALL AGSRESET                                                    288
         AG_SEEN = 2                                                      288
         AG_COLO = 1                                                      288
         AG_TITLE = 'SFRA'                                                288
         AG_EXNAME = 'ATTRIBUTE'                                          288
         AG_PARLIST = 'SEEN_COLO'                                         288
         CALL AXATTRIBUTE                                                 288
C    *                                                                    288
C    Geant SHAPE : BOX ===> DX=3.76 DY=0.019 DZ=1. <===                   288
         CALL AGSRESET                                                    289
         AG_DX = 3.76                                                     289
         AG_DY = 0.019                                                    289
         AG_DZ = 1.                                                       289
         AG_TITLE = 'BOX'                                                 289
         AG_EXNAME = 'SHAPE'                                              289
         AG_PARLIST = 'DX_DY_DZ'                                          289
         CALL AXSHAPE                                                     289
C    *                                                                    289
5321  CALL AGSPOPB                                                        290
      GO TO BLOCK_SFRA, (5320 ,5330)                                      290
C End of Local Procedure                                                  290
C ----------------------------------------------------------------------  291
C ------------ SFRS                                                       291
C Begin Local Procedure SFRS                                              292
C SFRS TWO SUPPORTS OF THE HYBRID STIFFNEER (PIECE OF IT)                 292
5340  CONTINUE                                                            292
         AG_TITLE='SFRS'                                                  292
         AG_MODULE=MODULE                                                 292
         AG_MARK='V00'                                                    292
         CALL AGSPUSH                                                     292
         AG_MODULE=MODULE                                                 292
C    *                                                                    292
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           293
         CALL AGSRESET                                                    293
         AG_TITLE = 'CARBON'                                              293
         AG_EXNAME = 'MATERIAL'                                           293
         AG_PARLIST = 'NONE'                                              293
         CALL AXMATERIAL                                                  293
C    *                                                                    293
C    Geant ATTRIBUTE : SFRS ===> SEEN=2 COLO=1 <===                       293
         CALL AGSRESET                                                    294
         AG_SEEN = 2                                                      294
         AG_COLO = 1                                                      294
         AG_TITLE = 'SFRS'                                                294
         AG_EXNAME = 'ATTRIBUTE'                                          294
         AG_PARLIST = 'SEEN_COLO'                                         294
         CALL AXATTRIBUTE                                                 294
C    *                                                                    294
C    Geant SHAPE : BOX ===> DX=0.04 DY=0.0915 DZ=1. <===                  294
         CALL AGSRESET                                                    295
         AG_DX = 0.04                                                     295
         AG_DY = 0.0915                                                   295
         AG_DZ = 1.                                                       295
         AG_TITLE = 'BOX'                                                 295
         AG_EXNAME = 'SHAPE'                                              295
         AG_PARLIST = 'DX_DY_DZ'                                          295
         CALL AXSHAPE                                                     295
C    *                                                                    295
5341  CALL AGSPOPB                                                        296
      GO TO BLOCK_SFRS, (5340 ,5350)                                      296
C End of Local Procedure                                                  296
C ----------------------------------------------------------------------  297
C ------------ SFFX                                                       297
C Begin Local Procedure SFFX                                              298
C SFFX IS THE FLEX                                                        298
5360  CONTINUE                                                            298
         AG_TITLE='SFFX'                                                  298
         AG_MODULE=MODULE                                                 298
         AG_MARK='V00'                                                    298
         CALL AGSPUSH                                                     298
         AG_MODULE=MODULE                                                 298
C    *                                                                    298
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           299
         CALL AGSRESET                                                    299
         AG_TITLE = 'COPPER'                                              299
         AG_EXNAME = 'MATERIAL'                                           299
         AG_PARLIST = 'NONE'                                              299
         CALL AXMATERIAL                                                  299
C    *                                                                    299
C    Geant ATTRIBUTE : SFFX ===> SEEN=1 COLO=42 <===                      299
         CALL AGSRESET                                                    300
         AG_SEEN = 1                                                      300
         AG_COLO = 42                                                     300
         AG_TITLE = 'SFFX'                                                300
         AG_EXNAME = 'ATTRIBUTE'                                          300
         AG_PARLIST = 'SEEN_COLO'                                         300
         CALL AXATTRIBUTE                                                 300
C    *                                                                    300
C    Geant SHAPE : BOX ===> DX=3.84 DY=0.0007 DZ=1. <===                  300
         CALL AGSRESET                                                    301
         AG_DX = 3.84                                                     301
         AG_DY = 0.0007                                                   301
         AG_DZ = 1.                                                       301
         AG_TITLE = 'BOX'                                                 301
         AG_EXNAME = 'SHAPE'                                              301
         AG_PARLIST = 'DX_DY_DZ'                                          301
         CALL AXSHAPE                                                     301
C    *                                                                    301
5361  CALL AGSPOPB                                                        302
      GO TO BLOCK_SFFX, (5360 ,5370)                                      302
C End of Local Procedure                                                  302
C ----------------------------------------------------------------------  303
C ------------ SFAA                                                       303
C Begin Local Procedure SFAA                                              304
C SFAA IS THE A128C CHIP                                                  304
5400  CONTINUE                                                            304
         AG_TITLE='SFAA'                                                  304
         AG_MODULE=MODULE                                                 304
         AG_MARK='V00'                                                    304
         CALL AGSPUSH                                                     304
         AG_MODULE=MODULE                                                 304
C    *                                                                    304
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          305
         CALL AGSRESET                                                    305
         AG_TITLE = 'SILICON'                                             305
         AG_EXNAME = 'MATERIAL'                                           305
         AG_PARLIST = 'NONE'                                              305
         CALL AXMATERIAL                                                  305
C    *                                                                    305
C    Geant ATTRIBUTE : SFAA ===> SEEN=1 COLO=41 <===                      305
         CALL AGSRESET                                                    306
         AG_SEEN = 1                                                      306
         AG_COLO = 41                                                     306
         AG_TITLE = 'SFAA'                                                306
         AG_EXNAME = 'ATTRIBUTE'                                          306
         AG_PARLIST = 'SEEN_COLO'                                         306
         CALL AXATTRIBUTE                                                 306
C    *                                                                    306
C    Geant SHAPE : BOX ===> DX=0.3 DY=0.015 DZ=0.4 <===                   306
         CALL AGSRESET                                                    307
         AG_DX = 0.3                                                      307
         AG_DY = 0.015                                                    307
         AG_DZ = 0.4                                                      307
         AG_TITLE = 'BOX'                                                 307
         AG_EXNAME = 'SHAPE'                                              307
         AG_PARLIST = 'DX_DY_DZ'                                          307
         CALL AXSHAPE                                                     307
C    *                                                                    307
5401  CALL AGSPOPB                                                        308
      GO TO BLOCK_SFAA, (5400 ,5410)                                      308
C End of Local Procedure                                                  308
C ----------------------------------------------------------------------  309
C ------------ SFPI                                                       309
C Begin Local Procedure SFPI                                              310
C SFPI ARE THE PIONS                                                      310
5380  CONTINUE                                                            310
         AG_TITLE='SFPI'                                                  310
         AG_MODULE=MODULE                                                 310
         AG_MARK='V00'                                                    310
         CALL AGSPUSH                                                     310
         AG_MODULE=MODULE                                                 310
C    *                                                                    310
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        311
         CALL AGSRESET                                                    311
         AG_TITLE = 'ALUMINIUM'                                           311
         AG_EXNAME = 'MATERIAL'                                           311
         AG_PARLIST = 'NONE'                                              311
         CALL AXMATERIAL                                                  311
C    *                                                                    311
C    Geant ATTRIBUTE : SFPI ===> SEEN=2 COLO=33 <===                      311
         CALL AGSRESET                                                    312
         AG_SEEN = 2                                                      312
         AG_COLO = 33                                                     312
         AG_TITLE = 'SFPI'                                                312
         AG_EXNAME = 'ATTRIBUTE'                                          312
         AG_PARLIST = 'SEEN_COLO'                                         312
         CALL AXATTRIBUTE                                                 312
C    *                                                                    312
C    Geant SHAPE : TUBE ===> RMIN=0.15/2 RMAX=0.2/2 DZ=0.35 <===          312
         CALL AGSRESET                                                    313
         AG_RMIN = 0.15/2                                                 313
         AG_RMAX = 0.2/2                                                  313
         AG_DZ = 0.35                                                     313
         AG_TITLE = 'TUBE'                                                313
         AG_EXNAME = 'SHAPE'                                              313
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      313
         CALL AXSHAPE                                                     313
C    *                                                                    313
C    Create SFPJ :                                                        314
C    Call To Local Procedure SFPJ                                         314
         ASSIGN 5430 TO BLOCK_SFPJ                                        314
         GO TO  5420                                                      314
5430     CONTINUE                                                         314
C    *                                                                    314
C    Geant POSITION : SFPJ ===> X=0. Y=0 Z=-0.35+0.0125 <===              315
         CALL AGSRESET                                                    315
         AG_X = 0.                                                        315
         AG_Y = 0                                                         315
         AG_Z = -0.35+0.0125                                              315
         AG_TITLE = 'SFPJ'                                                315
         AG_EXNAME = 'POSITION'                                           315
         AG_PARLIST = 'X_Y_Z'                                             315
         CALL AXPOSITION                                                  315
C    *                                                                    315
5381  CALL AGSPOPB                                                        316
      GO TO BLOCK_SFPI, (5380 ,5390)                                      316
C End of Local Procedure                                                  316
C ----------------------------------------------------------------------  317
C ------------ SFPJ                                                       317
C Begin Local Procedure SFPJ                                              318
C SFPJ IS THE BASE OF THE PIONS                                           318
5420  CONTINUE                                                            318
         AG_TITLE='SFPJ'                                                  318
         AG_MODULE=MODULE                                                 318
         AG_MARK='V00'                                                    318
         CALL AGSPUSH                                                     318
         AG_MODULE=MODULE                                                 318
C    *                                                                    318
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        319
         CALL AGSRESET                                                    319
         AG_TITLE = 'ALUMINIUM'                                           319
         AG_EXNAME = 'MATERIAL'                                           319
         AG_PARLIST = 'NONE'                                              319
         CALL AXMATERIAL                                                  319
C    *                                                                    319
C    Geant ATTRIBUTE : SFPJ ===> SEEN=2 COLO=33 <===                      319
         CALL AGSRESET                                                    320
         AG_SEEN = 2                                                      320
         AG_COLO = 33                                                     320
         AG_TITLE = 'SFPJ'                                                320
         AG_EXNAME = 'ATTRIBUTE'                                          320
         AG_PARLIST = 'SEEN_COLO'                                         320
         CALL AXATTRIBUTE                                                 320
C    *                                                                    320
C    Geant SHAPE : TUBE ===> RMIN=0.075+0.025 RMAX=0.3 DZ=0.0125 <===     320
         CALL AGSRESET                                                    321
         AG_RMIN = 0.075+0.025                                            321
         AG_RMAX = 0.3                                                    321
         AG_DZ = 0.0125                                                   321
         AG_TITLE = 'TUBE'                                                321
         AG_EXNAME = 'SHAPE'                                              321
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      321
         CALL AXSHAPE                                                     321
C    *                                                                    321
5421  CALL AGSPOPB                                                        322
      GO TO BLOCK_SFPJ, (5420 ,5430)                                      322
C End of Local Procedure                                                  322
C ----------------------------------------------------------------------  323
C ------------ SFSD                                                       323
C Begin Local Procedure SFSD                                              324
C SFSD IS THE STRIP DETECTOR                                              324
5300  CONTINUE                                                            324
         AG_TITLE='SFSD'                                                  324
         AG_MODULE=MODULE                                                 324
         AG_MARK='V00'                                                    324
         CALL AGSPUSH                                                     324
         AG_MODULE=MODULE                                                 324
C    *                                                                    324
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          325
         CALL AGSRESET                                                    325
         AG_TITLE = 'SILICON'                                             325
         AG_EXNAME = 'MATERIAL'                                           325
         AG_PARLIST = 'NONE'                                              325
         CALL AXMATERIAL                                                  325
C    *                                                                    325
C    Geant MATERIAL : SENSITIVE ===> ISVOL=1 <===                         325
         CALL AGSRESET                                                    326
         AG_ISVOL = 1                                                     326
         AG_TITLE = 'SENSITIVE'                                           326
         AG_EXNAME = 'MATERIAL'                                           326
         AG_PARLIST = 'ISVOL'                                             326
         CALL AXMATERIAL                                                  326
C    *                                                                    326
C    Geant ATTRIBUTE : SFSD ===> SEEN=2 COLO=41 <===                      326
         CALL AGSRESET                                                    327
         AG_SEEN = 2                                                      327
         AG_COLO = 41                                                     327
         AG_TITLE = 'SFSD'                                                327
         AG_EXNAME = 'ATTRIBUTE'                                          327
         AG_PARLIST = 'SEEN_COLO'                                         327
         CALL AXATTRIBUTE                                                 327
C    *                                                                    327
C    Geant SHAPE : BOX ===> DX=3.75 DY=0.015 DZ=2.1 <===                  327
         CALL AGSRESET                                                    328
         AG_DX = 3.75                                                     328
         AG_DY = 0.015                                                    328
         AG_DZ = 2.1                                                      328
         AG_TITLE = 'BOX'                                                 328
         AG_EXNAME = 'SHAPE'                                              328
         AG_PARLIST = 'DX_DY_DZ'                                          328
         CALL AXSHAPE                                                     328
C    *                                                                    328
         CALL GSTPAR( AG_IMED,'STRA',1. )                                 328
C    Defining Hits: X:0.001:S Y:0.001: Z:0.001: CX:10: CY:10: CZ:10: STE  331
C    P:0.01: SLENG:16:(0,500), TOF:16:(0,1.0E-6) PTOT:16:(0,100.0) ELOSS  331
C    :16:(0,0.001)                                                        331
         AG_TITLE = 'SFSD'                                                331
         AG_EXNAME = 'HITS'                                               331
         AG_TYPE=AG_MAGIC                                                 331
         CALL AGSDETV('SFSD')                                             331
C    *                                                                    331
         AG_PARLIST='X'                                                   331
         AG_TITLE = 'DETH'                                                331
         AG_NBITS = AG_MAGIC                                              331
         AG_BIN=0.001                                                     331
         AG_OPTION='S'                                                    331
         AG_FHMIN = AG_MAGIC                                              331
         AG_FHMAX = AG_MAGIC                                              331
         CALL AGSDETH (0)                                                 331
C    *                                                                    331
         AG_PARLIST='Y'                                                   331
         AG_TITLE = 'DETH'                                                331
         AG_NBITS = AG_MAGIC                                              331
         AG_BIN=0.001                                                     331
         AG_FHMIN=AG_MAGIC                                                331
         AG_FHMAX=AG_MAGIC                                                331
         AG_OPTION='*'                                                    331
         CALL AGSDETH (0)                                                 331
C    *                                                                    331
         AG_PARLIST='Z'                                                   331
         AG_TITLE = 'DETH'                                                331
         AG_NBITS = AG_MAGIC                                              331
         AG_BIN=0.001                                                     331
         AG_FHMIN=AG_MAGIC                                                331
         AG_FHMAX=AG_MAGIC                                                331
         AG_OPTION='*'                                                    331
         CALL AGSDETH (0)                                                 331
C    *                                                                    331
         AG_PARLIST='CX'                                                  331
         AG_TITLE = 'DETH'                                                331
         AG_NBITS = 10                                                    331
         AG_BIN=AG_MAGIC                                                  331
         AG_FHMIN=AG_MAGIC                                                331
         AG_FHMAX=AG_MAGIC                                                331
         AG_OPTION='*'                                                    331
         CALL AGSDETH (0)                                                 331
C    *                                                                    331
         AG_PARLIST='CY'                                                  331
         AG_TITLE = 'DETH'                                                331
         AG_NBITS = 10                                                    331
         AG_BIN=AG_MAGIC                                                  331
         AG_FHMIN=AG_MAGIC                                                331
         AG_FHMAX=AG_MAGIC                                                331
         AG_OPTION='*'                                                    331
         CALL AGSDETH (0)                                                 331
C    *                                                                    331
         AG_PARLIST='CZ'                                                  331
         AG_TITLE = 'DETH'                                                331
         AG_NBITS = 10                                                    331
         AG_BIN=AG_MAGIC                                                  331
         AG_FHMIN=AG_MAGIC                                                331
         AG_FHMAX=AG_MAGIC                                                331
         AG_OPTION='*'                                                    331
         CALL AGSDETH (0)                                                 331
C    *                                                                    331
         AG_PARLIST='STEP'                                                331
         AG_TITLE = 'DETH'                                                331
         AG_NBITS = AG_MAGIC                                              331
         AG_BIN=0.01                                                      331
         AG_FHMIN=AG_MAGIC                                                331
         AG_FHMAX=AG_MAGIC                                                331
         AG_OPTION='*'                                                    331
         CALL AGSDETH (0)                                                 331
C    *                                                                    331
         AG_PARLIST='SLENG'                                               331
         AG_TITLE = 'DETH'                                                331
         AG_NBITS = 16                                                    331
         AG_BIN=AG_MAGIC                                                  331
         AG_OPTION='*'                                                    331
         AG_FHMIN = 0                                                     331
         AG_FHMAX = 500                                                   331
         CALL AGSDETH (0)                                                 331
C    *                                                                    331
         AG_PARLIST='TOF'                                                 331
         AG_TITLE = 'DETH'                                                331
         AG_NBITS = 16                                                    331
         AG_BIN=AG_MAGIC                                                  331
         AG_OPTION='*'                                                    331
         AG_FHMIN = 0                                                     331
         AG_FHMAX = 1.0E-6                                                331
         CALL AGSDETH (0)                                                 331
C    *                                                                    331
         AG_PARLIST='PTOT'                                                331
         AG_TITLE = 'DETH'                                                331
         AG_NBITS = 16                                                    331
         AG_BIN=AG_MAGIC                                                  331
         AG_OPTION='*'                                                    331
         AG_FHMIN = 0                                                     331
         AG_FHMAX = 100.0                                                 331
         CALL AGSDETH (0)                                                 331
C    *                                                                    331
         AG_PARLIST='ELOSS'                                               331
         AG_TITLE = 'DETH'                                                331
         AG_NBITS = 16                                                    331
         AG_BIN=AG_MAGIC                                                  331
         AG_OPTION='*'                                                    331
         AG_FHMIN = 0                                                     331
         AG_FHMAX = 0.001                                                 331
         CALL AGSDETH (0)                                                 331
C    *                                                                    331
         CALL AGSDETU('SFSD')                                             331
C    *                                                                    331
5301  CALL AGSPOPB                                                        332
      GO TO BLOCK_SFSD, (5300 ,5310)                                      332
C End of Local Procedure                                                  332
C ----------------------------------------------------------------------  333
C ------------ SFSM                                                       333
C Begin Local Procedure SFSM                                              334
C SFSM IS THE MOTHER VOLUME OF THE LADDER (MECHANIC STRUCTURE)            334
5250  CONTINUE                                                            334
         AG_TITLE='SFSM'                                                  334
         AG_MODULE=MODULE                                                 334
         AG_MARK='V00'                                                    334
         CALL AGSPUSH                                                     334
         AG_MODULE=MODULE                                                 334
C    *                                                                    334
C    Geant MATERIAL : AIR ===> inherited arguments used <===              335
         CALL AGSRESET                                                    335
         AG_TITLE = 'AIR'                                                 335
         AG_EXNAME = 'MATERIAL'                                           335
         AG_PARLIST = 'NONE'                                              335
         CALL AXMATERIAL                                                  335
C    *                                                                    335
C    Geant ATTRIBUTE : SFSM ===> SEEN=0 COLO=6 <===                       335
         CALL AGSRESET                                                    336
         AG_SEEN = 0                                                      336
         AG_COLO = 6                                                      336
         AG_TITLE = 'SFSM'                                                336
         AG_EXNAME = 'ATTRIBUTE'                                          336
         AG_PARLIST = 'SEEN_COLO'                                         336
         CALL AXATTRIBUTE                                                 336
C    *                                                                    336
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=SFPA_SMTHK/2, DZ=SFPA_SML  336
C    EN/2. <===                                                           336
         CALL AGSRESET                                                    338
         AG_DX = SFPA_DMWID/2                                             338
         AG_DY = SFPA_SMTHK/2                                             338
         AG_DZ = SFPA_SMLEN/2.                                            338
         AG_TITLE = 'BOX'                                                 338
         AG_EXNAME = 'SHAPE'                                              338
         AG_PARLIST = 'DX_DY_DZ'                                          338
         CALL AXSHAPE                                                     338
C    *                                                                    338
         YOFFSET= - 1.7                                                   338
C    Create SFLT :                                                        340
C    Call To Local Procedure SFLT                                         340
         ASSIGN 5450 TO BLOCK_SFLT                                        340
         GO TO  5440                                                      340
5450     CONTINUE                                                         340
C    *                                                                    340
         HIGHT = 2.6*TAN(54*PI/180) - SFPB_HHIGHT/TAN(PI/5) - 0.02        341
C    Geant POSITION : SFLT ===> X=-(SFPB_HHIGHT-SFPB_KHIGHT) Y=HIGHT+YOF  342
C    FSET Z=0 ALPHAZ=-90-36 <===                                          342
         CALL AGSRESET                                                    342
         AG_X = -(SFPB_HHIGHT-SFPB_KHIGHT)                                342
         AG_Y = HIGHT+YOFFSET                                             342
         AG_Z = 0                                                         342
         AG_ALPHAN=AG_ALPHAN+1                                            342
         AG_ALPHAK(AG_ALPHAN)=3                                           342
         AG_ALPHAL(AG_ALPHAN)=-90-36                                      342
         AG_TITLE = 'SFLT'                                                342
         AG_EXNAME = 'POSITION'                                           342
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      342
         CALL AXPOSITION                                                  342
C    *                                                                    342
C    Geant POSITION : SFLT ===> X=(SFPB_HHIGHT-SFPB_KHIGHT) Y=HIGHT+YOFF  342
C    SET Z=0 ALPHAY=180. ALPHAZ=90.+36. <===                              342
         CALL AGSRESET                                                    344
         AG_X = (SFPB_HHIGHT-SFPB_KHIGHT)                                 344
         AG_Y = HIGHT+YOFFSET                                             344
         AG_Z = 0                                                         344
         AG_ALPHAN=AG_ALPHAN+1                                            344
         AG_ALPHAK(AG_ALPHAN)=2                                           344
         AG_ALPHAL(AG_ALPHAN)=180.                                        344
         AG_ALPHAN=AG_ALPHAN+1                                            344
         AG_ALPHAK(AG_ALPHAN)=3                                           344
         AG_ALPHAL(AG_ALPHAN)=90.+36.                                     344
         AG_TITLE = 'SFLT'                                                344
         AG_EXNAME = 'POSITION'                                           344
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               344
         CALL AXPOSITION                                                  344
C    *                                                                    344
C    Create SFLU :                                                        345
C    Call To Local Procedure SFLU                                         345
         ASSIGN 5470 TO BLOCK_SFLU                                        345
         GO TO  5460                                                      345
5470     CONTINUE                                                         345
C    *                                                                    345
C    Geant POSITION : SFLU ===> X=0.-(2.6-(0.2+0.01/TAN(PI/5))) Y=0.+YOF  346
C    FSET Z=0. <===                                                       346
         CALL AGSRESET                                                    346
         AG_X = 0.-(2.6-(0.2+0.01/TAN(PI/5)))                             346
         AG_Y = 0.+YOFFSET                                                346
         AG_Z = 0.                                                        346
         AG_TITLE = 'SFLU'                                                346
         AG_EXNAME = 'POSITION'                                           346
         AG_PARLIST = 'X_Y_Z'                                             346
         CALL AXPOSITION                                                  346
C    *                                                                    346
         ESSAI=180                                                        346
         ESSAI=ESSAI + 27 + 27                                            348
C    Geant POSITION : SFLU ===> X=-COS(63*PI/180)*2*(SFPB_HBASE-SFPB_KBA  351
C    SE)-(2.6-(0.2+0.01/TAN(PI/5))) Y=COS(27*PI/180)*2*(SFPB_HBASE-SFPB_  351
C    KBASE)+YOFFSET Z=0 ALPHAY=180 ALPHAZ=ESSAI <===                      351
         CALL AGSRESET                                                    351
         AG_X = -COS(63*PI/                                               351
     *   180)*2*(SFPB_HBASE-SFPB_KBASE)-(2.6-(0.2+0.01/TAN(PI/5)))        351
         AG_Y = COS(27*PI/180)*2*(SFPB_HBASE-SFPB_KBASE)+YOFFSET          351
         AG_Z = 0                                                         351
         AG_ALPHAN=AG_ALPHAN+1                                            351
         AG_ALPHAK(AG_ALPHAN)=2                                           351
         AG_ALPHAL(AG_ALPHAN)=180                                         351
         AG_ALPHAN=AG_ALPHAN+1                                            351
         AG_ALPHAK(AG_ALPHAN)=3                                           351
         AG_ALPHAL(AG_ALPHAN)=ESSAI                                       351
         AG_TITLE = 'SFLU'                                                351
         AG_EXNAME = 'POSITION'                                           351
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               351
         CALL AXPOSITION                                                  351
C    *                                                                    351
C    Geant POSITION : SFLU ===> X=0.+(2.6-(0.2+0.01/TAN(PI/5))) Y=0.+YOF  351
C    FSET Z=0. ALPHAY=180. <===                                           351
         CALL AGSRESET                                                    352
         AG_X = 0.+(2.6-(0.2+0.01/TAN(PI/5)))                             352
         AG_Y = 0.+YOFFSET                                                352
         AG_Z = 0.                                                        352
         AG_ALPHAN=AG_ALPHAN+1                                            352
         AG_ALPHAK(AG_ALPHAN)=2                                           352
         AG_ALPHAL(AG_ALPHAN)=180.                                        352
         AG_TITLE = 'SFLU'                                                352
         AG_EXNAME = 'POSITION'                                           352
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      352
         CALL AXPOSITION                                                  352
C    *                                                                    352
C    Geant POSITION : SFLU ===> X=COS(63*PI/180)*2*(SFPB_HBASE-SFPB_KBAS  352
C    E)+(2.6-(0.2+0.01/TAN(PI/5))) Y=COS(27*PI/180)*2*(SFPB_HBASE-SFPB_K  352
C    BASE)+YOFFSET Z=0 ALPHAZ=-ESSAI <===                                 352
         CALL AGSRESET                                                    355
         AG_X = COS(63*PI/180)*2*(SFPB_HBASE-SFPB_KBASE)+(2.6-(0.2+0.01/  355
     *   TAN(PI/5)))                                                      355
         AG_Y = COS(27*PI/180)*2*(SFPB_HBASE-SFPB_KBASE)+YOFFSET          355
         AG_Z = 0                                                         355
         AG_ALPHAN=AG_ALPHAN+1                                            355
         AG_ALPHAK(AG_ALPHAN)=3                                           355
         AG_ALPHAL(AG_ALPHAN)=-ESSAI                                      355
         AG_TITLE = 'SFLU'                                                355
         AG_EXNAME = 'POSITION'                                           355
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      355
         CALL AXPOSITION                                                  355
C    *                                                                    355
C    Create SFFK :                                                        356
C    Call To Local Procedure SFFK                                         356
         ASSIGN 5490 TO BLOCK_SFFK                                        356
         GO TO  5480                                                      356
5490     CONTINUE                                                         356
C    *                                                                    356
C    Geant POSITION : SFFK ===> X=3.5/2+0.4 Y=-0.02-0.04-0.02+YOFFSET Z=  357
C    0 <===                                                               357
         CALL AGSRESET                                                    357
         AG_X = 3.5/2+0.4                                                 357
         AG_Y = -0.02-0.04-0.02+YOFFSET                                   357
         AG_Z = 0                                                         357
         AG_TITLE = 'SFFK'                                                357
         AG_EXNAME = 'POSITION'                                           357
         AG_PARLIST = 'X_Y_Z'                                             357
         CALL AXPOSITION                                                  357
C    *                                                                    357
C    Geant POSITION : SFFK ===> X=-3.5/2-0.4 Y=-0.02-0.04-0.02+YOFFSET Z  357
C    =0 <===                                                              357
         CALL AGSRESET                                                    358
         AG_X = -3.5/2-0.4                                                358
         AG_Y = -0.02-0.04-0.02+YOFFSET                                   358
         AG_Z = 0                                                         358
         AG_TITLE = 'SFFK'                                                358
         AG_EXNAME = 'POSITION'                                           358
         AG_PARLIST = 'X_Y_Z'                                             358
         CALL AXPOSITION                                                  358
C    *                                                                    358
C    Create SFFL :                                                        359
C    Call To Local Procedure SFFL                                         359
         ASSIGN 5510 TO BLOCK_SFFL                                        359
         GO TO  5500                                                      359
5510     CONTINUE                                                         359
C    *                                                                    359
C    Geant POSITION : SFFL ===> X=-2.55-SFPB_FSIZE*COS(15*PI/180) Y=-0.0  361
C    2-0.04-0.02-SFPB_FSIZE*COS(75*PI/180)+YOFFSET Z=0 ALPHAZ=15 <===     361
         CALL AGSRESET                                                    361
         AG_X = -2.55-SFPB_FSIZE*COS(15*PI/180)                           361
         AG_Y = -0.02-0.04-0.02-SFPB_FSIZE*COS(75*PI/180)+YOFFSET         361
         AG_Z = 0                                                         361
         AG_ALPHAN=AG_ALPHAN+1                                            361
         AG_ALPHAK(AG_ALPHAN)=3                                           361
         AG_ALPHAL(AG_ALPHAN)=15                                          361
         AG_TITLE = 'SFFL'                                                361
         AG_EXNAME = 'POSITION'                                           361
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      361
         CALL AXPOSITION                                                  361
C    *                                                                    361
C    Geant POSITION : SFFL ===> X=2.55+SFPB_FSIZE*COS(15*PI/180) Y=-0.02  361
C    -0.04-0.02-SFPB_FSIZE*COS(75*PI/180)+YOFFSET Z=0 ALPHAZ=-15 <===     361
         CALL AGSRESET                                                    363
         AG_X = 2.55+SFPB_FSIZE*COS(15*PI/180)                            363
         AG_Y = -0.02-0.04-0.02-SFPB_FSIZE*COS(75*PI/180)+YOFFSET         363
         AG_Z = 0                                                         363
         AG_ALPHAN=AG_ALPHAN+1                                            363
         AG_ALPHAK(AG_ALPHAN)=3                                           363
         AG_ALPHAL(AG_ALPHAN)=-15                                         363
         AG_TITLE = 'SFFL'                                                363
         AG_EXNAME = 'POSITION'                                           363
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      363
         CALL AXPOSITION                                                  363
C    *                                                                    363
C    Create SFKK :                                                        364
C    Call To Local Procedure SFKK                                         364
         ASSIGN 5530 TO BLOCK_SFKK                                        364
         GO TO  5520                                                      364
5530     CONTINUE                                                         364
C    *                                                                    364
C    Geant POSITION : SFKK ===> X=0. Y=-0.02-0.04-0.04+YOFFSET-0.0025/2.  365
C     Z=0. <===                                                           365
         CALL AGSRESET                                                    365
         AG_X = 0.                                                        365
         AG_Y = -0.02-0.04-0.04+YOFFSET-0.0025/2.                         365
         AG_Z = 0.                                                        365
         AG_TITLE = 'SFKK'                                                365
         AG_EXNAME = 'POSITION'                                           365
         AG_PARLIST = 'X_Y_Z'                                             365
         CALL AXPOSITION                                                  365
C    *                                                                    365
C    Create SFKL :                                                        366
C    Call To Local Procedure SFKL                                         366
         ASSIGN 5550 TO BLOCK_SFKL                                        366
         GO TO  5540                                                      366
5550     CONTINUE                                                         366
C    *                                                                    366
C    Geant POSITION : SFKL ===> X=-2.55-SFPB_FSIZE*COS(15*PI/180)+0.005   368
C    Y=-0.02-0.04-0.02-SFPB_FSIZE*COS(75*PI/180)+YOFFSET-0.021 Z=0 ALPHA  368
C    Z=15 <===                                                            368
         CALL AGSRESET                                                    368
         AG_X = -2.55-SFPB_FSIZE*COS(15*PI/180)+0.005                     368
         AG_Y = -0.02-0.04-0.02-SFPB_FSIZE*COS(75*PI/180)+YOFFSET-0.021   368
         AG_Z = 0                                                         368
         AG_ALPHAN=AG_ALPHAN+1                                            368
         AG_ALPHAK(AG_ALPHAN)=3                                           368
         AG_ALPHAL(AG_ALPHAN)=15                                          368
         AG_TITLE = 'SFKL'                                                368
         AG_EXNAME = 'POSITION'                                           368
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      368
         CALL AXPOSITION                                                  368
C    *                                                                    368
C    Geant POSITION : SFKL ===> X=+2.55+SFPB_FSIZE*COS(15*PI/180)-0.005   368
C    Y=-0.02-0.04-0.02-SFPB_FSIZE*COS(75*PI/180)+YOFFSET-0.021 Z=0 ALPHA  368
C    Z=-15 <===                                                           368
         CALL AGSRESET                                                    370
         AG_X = +2.55+SFPB_FSIZE*COS(15*PI/180)-0.005                     370
         AG_Y = -0.02-0.04-0.02-SFPB_FSIZE*COS(75*PI/180)+YOFFSET-0.021   370
         AG_Z = 0                                                         370
         AG_ALPHAN=AG_ALPHAN+1                                            370
         AG_ALPHAK(AG_ALPHAN)=3                                           370
         AG_ALPHAL(AG_ALPHAN)=-15                                         370
         AG_TITLE = 'SFKL'                                                370
         AG_EXNAME = 'POSITION'                                           370
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      370
         CALL AXPOSITION                                                  370
C    *                                                                    370
C    Create SAPP :                                                        371
C    Call To Local Procedure SAPP                                         371
         ASSIGN 5570 TO BLOCK_SAPP                                        371
         GO TO  5560                                                      371
5570     CONTINUE                                                         371
C    *                                                                    371
C    Geant POSITION : SAPP ===> X=0. Y=-0.04-(0.5+0.08)/2.+YOFFSET Z=69.  372
C    75/2+1.275/2. ALPHAY=180. <===                                       372
         CALL AGSRESET                                                    372
         AG_X = 0.                                                        372
         AG_Y = -0.04-(0.5+0.08)/2.+YOFFSET                               372
         AG_Z = 69.75/2+1.275/2.                                          372
         AG_ALPHAN=AG_ALPHAN+1                                            372
         AG_ALPHAK(AG_ALPHAN)=2                                           372
         AG_ALPHAL(AG_ALPHAN)=180.                                        372
         AG_TITLE = 'SAPP'                                                372
         AG_EXNAME = 'POSITION'                                           372
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      372
         CALL AXPOSITION                                                  372
C    *                                                                    372
C    Geant POSITION : SAPP ===> X=0. Y=-0.04-(0.5+0.08)/2.+YOFFSET Z=-(6  372
C    9.75/2+1.275/2.) <===                                                372
         CALL AGSRESET                                                    373
         AG_X = 0.                                                        373
         AG_Y = -0.04-(0.5+0.08)/2.+YOFFSET                               373
         AG_Z = -(69.75/2+1.275/2.)                                       373
         AG_TITLE = 'SAPP'                                                373
         AG_EXNAME = 'POSITION'                                           373
         AG_PARLIST = 'X_Y_Z'                                             373
         CALL AXPOSITION                                                  373
C    *                                                                    373
C    Create SFAM :                                                        374
C    Call To Local Procedure SFAM                                         374
         ASSIGN 5590 TO BLOCK_SFAM                                        374
         GO TO  5580                                                      374
5590     CONTINUE                                                         374
C    *                                                                    374
C    Geant POSITION : SFAM ===> X=0. Y=-0.04-(0.5+0.08)/2.+YOFFSET Z=-(6  375
C    9.75/2+1.275)-(12.4)/2. <===                                         375
         CALL AGSRESET                                                    375
         AG_X = 0.                                                        375
         AG_Y = -0.04-(0.5+0.08)/2.+YOFFSET                               375
         AG_Z = -(69.75/2+1.275)-(12.4)/2.                                375
         AG_TITLE = 'SFAM'                                                375
         AG_EXNAME = 'POSITION'                                           375
         AG_PARLIST = 'X_Y_Z'                                             375
         CALL AXPOSITION                                                  375
C    *                                                                    375
C    Geant POSITION : SFAM ===> X=0. Y=-0.04-(0.5+0.08)/2.+YOFFSET Z=+(6  375
C    9.75/2+1.275)+(12.4)/2. ALPHAY=180. <===                             375
         CALL AGSRESET                                                    377
         AG_X = 0.                                                        377
         AG_Y = -0.04-(0.5+0.08)/2.+YOFFSET                               377
         AG_Z = +(69.75/2+1.275)+(12.4)/2.                                377
         AG_ALPHAN=AG_ALPHAN+1                                            377
         AG_ALPHAK(AG_ALPHAN)=2                                           377
         AG_ALPHAL(AG_ALPHAN)=180.                                        377
         AG_TITLE = 'SFAM'                                                377
         AG_EXNAME = 'POSITION'                                           377
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      377
         CALL AXPOSITION                                                  377
C    *                                                                    377
C    Create SFCO :                                                        378
C    Call To Local Procedure SFCO                                         378
         ASSIGN 5610 TO BLOCK_SFCO                                        378
         GO TO  5600                                                      378
5610     CONTINUE                                                         378
C    *                                                                    378
C    Geant POSITION : SFCO ===> X=-1.7 Y=0.2 Z=-(69.75/2-0.6)-15.8/2. AL  379
C    PHAZ=+54. <===                                                       379
         CALL AGSRESET                                                    379
         AG_X = -1.7                                                      379
         AG_Y = 0.2                                                       379
         AG_Z = -(69.75/2-0.6)-15.8/2.                                    379
         AG_ALPHAN=AG_ALPHAN+1                                            379
         AG_ALPHAK(AG_ALPHAN)=3                                           379
         AG_ALPHAL(AG_ALPHAN)=+54.                                        379
         AG_TITLE = 'SFCO'                                                379
         AG_EXNAME = 'POSITION'                                           379
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      379
         CALL AXPOSITION                                                  379
C    *                                                                    379
C    Geant POSITION : SFCO ===> X=+1.7 Y=0.2 Z=+(69.75/2-0.6)+15.8/2. AL  379
C    PHAZ=-54. <===                                                       379
         CALL AGSRESET                                                    380
         AG_X = +1.7                                                      380
         AG_Y = 0.2                                                       380
         AG_Z = +(69.75/2-0.6)+15.8/2.                                    380
         AG_ALPHAN=AG_ALPHAN+1                                            380
         AG_ALPHAK(AG_ALPHAN)=3                                           380
         AG_ALPHAL(AG_ALPHAN)=-54.                                        380
         AG_TITLE = 'SFCO'                                                380
         AG_EXNAME = 'POSITION'                                           380
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      380
         CALL AXPOSITION                                                  380
C    *                                                                    380
C    Create SFCM :                                                        381
C    Call To Local Procedure SFCM                                         381
         ASSIGN 5630 TO BLOCK_SFCM                                        381
         GO TO  5620                                                      381
5630     CONTINUE                                                         381
C    *                                                                    381
C    Geant POSITION : SFCM ===> X=+1.7 Y=0.2 Z=-(69.75/2-0.6)-15.8/2. AL  382
C    PHAZ=-54. <===                                                       382
         CALL AGSRESET                                                    382
         AG_X = +1.7                                                      382
         AG_Y = 0.2                                                       382
         AG_Z = -(69.75/2-0.6)-15.8/2.                                    382
         AG_ALPHAN=AG_ALPHAN+1                                            382
         AG_ALPHAK(AG_ALPHAN)=3                                           382
         AG_ALPHAL(AG_ALPHAN)=-54.                                        382
         AG_TITLE = 'SFCM'                                                382
         AG_EXNAME = 'POSITION'                                           382
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      382
         CALL AXPOSITION                                                  382
C    *                                                                    382
C    Geant POSITION : SFCM ===> X=-1.7 Y=0.2 Z=+(69.75/2-0.6)+15.8/2. AL  382
C    PHAY=180 ALPHAZ=+54. <===                                            382
         CALL AGSRESET                                                    383
         AG_X = -1.7                                                      383
         AG_Y = 0.2                                                       383
         AG_Z = +(69.75/2-0.6)+15.8/2.                                    383
         AG_ALPHAN=AG_ALPHAN+1                                            383
         AG_ALPHAK(AG_ALPHAN)=2                                           383
         AG_ALPHAL(AG_ALPHAN)=180                                         383
         AG_ALPHAN=AG_ALPHAN+1                                            383
         AG_ALPHAK(AG_ALPHAN)=3                                           383
         AG_ALPHAL(AG_ALPHAN)=+54.                                        383
         AG_TITLE = 'SFCM'                                                383
         AG_EXNAME = 'POSITION'                                           383
         AG_PARLIST = 'X_Y_Z_ALPHAY_ALPHAZ'                               383
         CALL AXPOSITION                                                  383
C    *                                                                    383
C    Create SFKF :                                                        384
C    Call To Local Procedure SFKF                                         384
         ASSIGN 5650 TO BLOCK_SFKF                                        384
         GO TO  5640                                                      384
5650     CONTINUE                                                         384
C    *                                                                    384
C    Geant POSITION : SFKF ===> X=-0.16 Y=2.07 Z=-(69.75/2-0.6)-8.1/2-0.  385
C    2 ALPHAZ=+54. <===                                                   385
         CALL AGSRESET                                                    385
         AG_X = -0.16                                                     385
         AG_Y = 2.07                                                      385
         AG_Z = -(69.75/2-0.6)-8.1/2-0.2                                  385
         AG_ALPHAN=AG_ALPHAN+1                                            385
         AG_ALPHAK(AG_ALPHAN)=3                                           385
         AG_ALPHAL(AG_ALPHAN)=+54.                                        385
         AG_TITLE = 'SFKF'                                                385
         AG_EXNAME = 'POSITION'                                           385
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      385
         CALL AXPOSITION                                                  385
C    *                                                                    385
C    Geant POSITION : SFKF ===> X=+0.16 Y=2.07 Z=+(69.75/2-0.6)+8.1/2+0.  385
C    2 ALPHAZ=-54. <===                                                   385
         CALL AGSRESET                                                    386
         AG_X = +0.16                                                     386
         AG_Y = 2.07                                                      386
         AG_Z = +(69.75/2-0.6)+8.1/2+0.2                                  386
         AG_ALPHAN=AG_ALPHAN+1                                            386
         AG_ALPHAK(AG_ALPHAN)=3                                           386
         AG_ALPHAL(AG_ALPHAN)=-54.                                        386
         AG_TITLE = 'SFKF'                                                386
         AG_EXNAME = 'POSITION'                                           386
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      386
         CALL AXPOSITION                                                  386
C    *                                                                    386
C    Create SFKS :                                                        387
C    Call To Local Procedure SFKS                                         387
         ASSIGN 5670 TO BLOCK_SFKS                                        387
         GO TO  5660                                                      387
5670     CONTINUE                                                         387
C    *                                                                    387
C    Geant POSITION : SFKS ===> X=+0.505 Y=1.585 Z=-(69.75/2-0.6)-8.1/2-  388
C    0.2 ALPHAZ=-54. <===                                                 388
         CALL AGSRESET                                                    388
         AG_X = +0.505                                                    388
         AG_Y = 1.585                                                     388
         AG_Z = -(69.75/2-0.6)-8.1/2-0.2                                  388
         AG_ALPHAN=AG_ALPHAN+1                                            388
         AG_ALPHAK(AG_ALPHAN)=3                                           388
         AG_ALPHAL(AG_ALPHAN)=-54.                                        388
         AG_TITLE = 'SFKS'                                                388
         AG_EXNAME = 'POSITION'                                           388
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      388
         CALL AXPOSITION                                                  388
C    *                                                                    388
C    Geant POSITION : SFKS ===> X=-0.505 Y=1.585 Z=+(69.75/2-0.6)+8.1/2+  388
C    0.2 ALPHAZ=+54. <===                                                 388
         CALL AGSRESET                                                    389
         AG_X = -0.505                                                    389
         AG_Y = 1.585                                                     389
         AG_Z = +(69.75/2-0.6)+8.1/2+0.2                                  389
         AG_ALPHAN=AG_ALPHAN+1                                            389
         AG_ALPHAK(AG_ALPHAN)=3                                           389
         AG_ALPHAL(AG_ALPHAN)=+54.                                        389
         AG_TITLE = 'SFKS'                                                389
         AG_EXNAME = 'POSITION'                                           389
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      389
         CALL AXPOSITION                                                  389
C    *                                                                    389
C    Create SFPR :                                                        390
C    Call To Local Procedure SFPR                                         390
         ASSIGN 5690 TO BLOCK_SFPR                                        390
         GO TO  5680                                                      390
5690     CONTINUE                                                         390
C    *                                                                    390
C    Geant POSITION : SFPR ===> X=0 Y=+1.79 Z=-49.8 <===                  391
         CALL AGSRESET                                                    391
         AG_X = 0                                                         391
         AG_Y = +1.79                                                     391
         AG_Z = -49.8                                                     391
         AG_TITLE = 'SFPR'                                                391
         AG_EXNAME = 'POSITION'                                           391
         AG_PARLIST = 'X_Y_Z'                                             391
         CALL AXPOSITION                                                  391
C    *                                                                    391
C    Geant POSITION : SFPR ===> X=0 Y=+1.79 Z=+49.8-3.7 <===              391
         CALL AGSRESET                                                    392
         AG_X = 0                                                         392
         AG_Y = +1.79                                                     392
         AG_Z = +49.8-3.7                                                 392
         AG_TITLE = 'SFPR'                                                392
         AG_EXNAME = 'POSITION'                                           392
         AG_PARLIST = 'X_Y_Z'                                             392
         CALL AXPOSITION                                                  392
C    *                                                                    392
C    Create SFPB :                                                        393
C    Call To Local Procedure SFPB                                         393
         ASSIGN 5710 TO BLOCK_SFPB                                        393
         GO TO  5700                                                      393
5710     CONTINUE                                                         393
C    *                                                                    393
C    Geant POSITION : SFPB ===> X=0 Y=+1.79-3.48+0.08 Z=-49.8-0.95/2. <=  394
C    ==                                                                   394
         CALL AGSRESET                                                    394
         AG_X = 0                                                         394
         AG_Y = +1.79-3.48+0.08                                           394
         AG_Z = -49.8-0.95/2.                                             394
         AG_TITLE = 'SFPB'                                                394
         AG_EXNAME = 'POSITION'                                           394
         AG_PARLIST = 'X_Y_Z'                                             394
         CALL AXPOSITION                                                  394
C    *                                                                    394
C    Geant POSITION : SFPB ===> X=0 Y=+1.79-3.48+0.08 Z=+49.8+0.95/2. <=  394
C    ==                                                                   394
         CALL AGSRESET                                                    395
         AG_X = 0                                                         395
         AG_Y = +1.79-3.48+0.08                                           395
         AG_Z = +49.8+0.95/2.                                             395
         AG_TITLE = 'SFPB'                                                395
         AG_EXNAME = 'POSITION'                                           395
         AG_PARLIST = 'X_Y_Z'                                             395
         CALL AXPOSITION                                                  395
C    *                                                                    395
C    Create SSBS :                                                        396
C    Call To Local Procedure SSBS                                         396
         ASSIGN 5730 TO BLOCK_SSBS                                        396
         GO TO  5720                                                      396
5730     CONTINUE                                                         396
C    *                                                                    396
C    Geant POSITION : SSBS ===> X=0. Y=+1.79-3.48+0.08-0.8/2.-0.5/2. Z=-  397
C    49.8-0.95-0.2+1.9+2.5/2. <===                                        397
         CALL AGSRESET                                                    397
         AG_X = 0.                                                        397
         AG_Y = +1.79-3.48+0.08-0.8/2.-0.5/2.                             397
         AG_Z = -49.8-0.95-0.2+1.9+2.5/2.                                 397
         AG_TITLE = 'SSBS'                                                397
         AG_EXNAME = 'POSITION'                                           397
         AG_PARLIST = 'X_Y_Z'                                             397
         CALL AXPOSITION                                                  397
C    *                                                                    397
C    Geant POSITION : SSBS ===> X=0. Y=+1.79-3.48+0.08-0.8/2.-0.5/2. Z=+  397
C    49.8+0.95+0.2-1.9-2.5/2. <===                                        397
         CALL AGSRESET                                                    398
         AG_X = 0.                                                        398
         AG_Y = +1.79-3.48+0.08-0.8/2.-0.5/2.                             398
         AG_Z = +49.8+0.95+0.2-1.9-2.5/2.                                 398
         AG_TITLE = 'SSBS'                                                398
         AG_EXNAME = 'POSITION'                                           398
         AG_PARLIST = 'X_Y_Z'                                             398
         CALL AXPOSITION                                                  398
C    *                                                                    398
C    Create SSBB :                                                        399
C    Call To Local Procedure SSBB                                         399
         ASSIGN 5750 TO BLOCK_SSBB                                        399
         GO TO  5740                                                      399
5750     CONTINUE                                                         399
C    *                                                                    399
C    Geant POSITION : SSBB ===> X=0. Y=+1.79-3.48+0.08-0.8/2.-0.5/2. Z=-  400
C    49.8-0.95-0.2+1.9/2. <===                                            400
         CALL AGSRESET                                                    400
         AG_X = 0.                                                        400
         AG_Y = +1.79-3.48+0.08-0.8/2.-0.5/2.                             400
         AG_Z = -49.8-0.95-0.2+1.9/2.                                     400
         AG_TITLE = 'SSBB'                                                400
         AG_EXNAME = 'POSITION'                                           400
         AG_PARLIST = 'X_Y_Z'                                             400
         CALL AXPOSITION                                                  400
C    *                                                                    400
C    Geant POSITION : SSBB ===> X=0. Y=+1.79-3.48+0.08-0.8/2.-0.5/2. Z=+  400
C    49.8+0.95+0.2-1.9/2. <===                                            400
         CALL AGSRESET                                                    401
         AG_X = 0.                                                        401
         AG_Y = +1.79-3.48+0.08-0.8/2.-0.5/2.                             401
         AG_Z = +49.8+0.95+0.2-1.9/2.                                     401
         AG_TITLE = 'SSBB'                                                401
         AG_EXNAME = 'POSITION'                                           401
         AG_PARLIST = 'X_Y_Z'                                             401
         CALL AXPOSITION                                                  401
C    *                                                                    401
C    Create SFLA :                                                        402
C    Call To Local Procedure SFLA                                         402
         ASSIGN 5770 TO BLOCK_SFLA                                        402
         GO TO  5760                                                      402
5770     CONTINUE                                                         402
C    *                                                                    402
C    Create SFLB :                                                        403
C    Call To Local Procedure SFLB                                         403
         ASSIGN 5790 TO BLOCK_SFLB                                        403
         GO TO  5780                                                      403
5790     CONTINUE                                                         403
C    *                                                                    403
C    Create SFLC :                                                        404
C    Call To Local Procedure SFLC                                         404
         ASSIGN 5810 TO BLOCK_SFLC                                        404
         GO TO  5800                                                      404
5810     CONTINUE                                                         404
C    *                                                                    404
C    Create SFEB :                                                        405
C    Call To Local Procedure SFEB                                         405
         ASSIGN 5830 TO BLOCK_SFEB                                        405
         GO TO  5820                                                      405
5830     CONTINUE                                                         405
C    *                                                                    405
C    Create SFES :                                                        406
C    Call To Local Procedure SFES                                         406
         ASSIGN 5850 TO BLOCK_SFES                                        406
         GO TO  5840                                                      406
5850     CONTINUE                                                         406
C    *                                                                    406
         WAFPCKLEN=SFPA_WPLEN/(SFPA_NSSD*1.)                              407
C    *                                                                    408
         IWAF=1                                                           408
            GO TO 5863                                                    408
5861        IWAF=IWAF+1                                                   408
5863        IF(IWAF-(8).GT.0) GO TO 5862                                  408
C       *                                                                 409
            JWAF=1                                                        409
               GO TO 5873                                                 409
5871           JWAF=JWAF+1                                                409
5873           IF(JWAF-(IWAF).GT.0) GO TO 5872                            409
C          Geant POSITION : SFLA ===> X=+1.1+0.02*JWAF Y=1.35+0.02*JWAF   411
C          Z=-(SFPA_WPLEN+WAFPCKLEN)/2+IWAF*WAFPCKLEN ALPHAZ=-54 <===     411
               CALL AGSRESET                                              411
               AG_X = +1.1+0.02*JWAF                                      411
               AG_Y = 1.35+0.02*JWAF                                      411
               AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+IWAF*WAFPCKLEN            411
               AG_ALPHAN=AG_ALPHAN+1                                      411
               AG_ALPHAK(AG_ALPHAN)=3                                     411
               AG_ALPHAL(AG_ALPHAN)=-54                                   411
               AG_TITLE = 'SFLA'                                          411
               AG_EXNAME = 'POSITION'                                     411
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                411
               CALL AXPOSITION                                            411
C          *                                                              411
C          Geant POSITION : SFLA ===> X=-1.1-0.02*JWAF Y=1.35+0.02*JWAF   411
C          Z=+(SFPA_WPLEN+WAFPCKLEN)/2-IWAF*WAFPCKLEN ALPHAZ=+54 <===     411
               CALL AGSRESET                                              413
               AG_X = -1.1-0.02*JWAF                                      413
               AG_Y = 1.35+0.02*JWAF                                      413
               AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-IWAF*WAFPCKLEN            413
               AG_ALPHAN=AG_ALPHAN+1                                      413
               AG_ALPHAK(AG_ALPHAN)=3                                     413
               AG_ALPHAL(AG_ALPHAN)=+54                                   413
               AG_TITLE = 'SFLA'                                          413
               AG_EXNAME = 'POSITION'                                     413
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                413
               CALL AXPOSITION                                            413
C          *                                                              413
C          Geant POSITION : SFLC ===> X=+1.1+0.02*JWAF Y=1.35+0.02*JWAF   413
C          Z=-(SFPA_WPLEN+WAFPCKLEN)/2+16*WAFPCKLEN+WAFPCKLEN/2.+7.5+8.5  413
C          *0.93-IWAF*0.93 ALPHAZ=-54 <===                                413
               CALL AGSRESET                                              416
               AG_X = +1.1+0.02*JWAF                                      416
               AG_Y = 1.35+0.02*JWAF                                      416
               AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+16*WAFPCKLEN+WAFPCKLEN/   416
     *         2.+7.5+8.5*0.93-IWAF*0.93                                  416
               AG_ALPHAN=AG_ALPHAN+1                                      416
               AG_ALPHAK(AG_ALPHAN)=3                                     416
               AG_ALPHAL(AG_ALPHAN)=-54                                   416
               AG_TITLE = 'SFLC'                                          416
               AG_EXNAME = 'POSITION'                                     416
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                416
               CALL AXPOSITION                                            416
C          *                                                              416
C          Geant POSITION : SFLC ===> X=-1.1-0.02*JWAF Y=1.35+0.02*JWAF   416
C          Z=+(SFPA_WPLEN+WAFPCKLEN)/2-16*WAFPCKLEN-WAFPCKLEN/2.-7.5-8.5  416
C          *0.93+IWAF*0.93 ALPHAZ=+54 <===                                416
               CALL AGSRESET                                              419
               AG_X = -1.1-0.02*JWAF                                      419
               AG_Y = 1.35+0.02*JWAF                                      419
               AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-16*WAFPCKLEN-WAFPCKLEN/   419
     *         2.-7.5-8.5*0.93+IWAF*0.93                                  419
               AG_ALPHAN=AG_ALPHAN+1                                      419
               AG_ALPHAK(AG_ALPHAN)=3                                     419
               AG_ALPHAL(AG_ALPHAN)=+54                                   419
               AG_TITLE = 'SFLC'                                          419
               AG_EXNAME = 'POSITION'                                     419
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                419
               CALL AXPOSITION                                            419
C          *                                                              419
C          Geant POSITION : SFLA ===> X=+2.3+0.02*JWAF Y=-0.3+0.02*JWAF   419
C          Z=-(SFPA_WPLEN+WAFPCKLEN)/2+(IWAF+8)*WAFPCKLEN ALPHAZ=-54 <==  419
C          =                                                              419
               CALL AGSRESET                                              421
               AG_X = +2.3+0.02*JWAF                                      421
               AG_Y = -0.3+0.02*JWAF                                      421
               AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+(IWAF+8)*WAFPCKLEN        421
               AG_ALPHAN=AG_ALPHAN+1                                      421
               AG_ALPHAK(AG_ALPHAN)=3                                     421
               AG_ALPHAL(AG_ALPHAN)=-54                                   421
               AG_TITLE = 'SFLA'                                          421
               AG_EXNAME = 'POSITION'                                     421
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                421
               CALL AXPOSITION                                            421
C          *                                                              421
C          Geant POSITION : SFLA ===> X=-2.3-0.02*JWAF Y=-0.3+0.02*JWAF   421
C          Z=+(SFPA_WPLEN+WAFPCKLEN)/2-(IWAF+8)*WAFPCKLEN ALPHAZ=+54 <==  421
C          =                                                              421
               CALL AGSRESET                                              423
               AG_X = -2.3-0.02*JWAF                                      423
               AG_Y = -0.3+0.02*JWAF                                      423
               AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-(IWAF+8)*WAFPCKLEN        423
               AG_ALPHAN=AG_ALPHAN+1                                      423
               AG_ALPHAK(AG_ALPHAN)=3                                     423
               AG_ALPHAL(AG_ALPHAN)=+54                                   423
               AG_TITLE = 'SFLA'                                          423
               AG_EXNAME = 'POSITION'                                     423
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                423
               CALL AXPOSITION                                            423
C          *                                                              423
C          Geant POSITION : SFLC ===> X=+2.3+0.02*JWAF Y=-0.3+0.02*JWAF   423
C          Z=-(SFPA_WPLEN+WAFPCKLEN)/2+16*WAFPCKLEN+WAFPCKLEN/2.+8.5*0.9  423
C          3-IWAF*0.93 ALPHAZ=-54 <===                                    423
               CALL AGSRESET                                              426
               AG_X = +2.3+0.02*JWAF                                      426
               AG_Y = -0.3+0.02*JWAF                                      426
               AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+16*WAFPCKLEN+WAFPCKLEN/   426
     *         2.+8.5*0.93-IWAF*0.93                                      426
               AG_ALPHAN=AG_ALPHAN+1                                      426
               AG_ALPHAK(AG_ALPHAN)=3                                     426
               AG_ALPHAL(AG_ALPHAN)=-54                                   426
               AG_TITLE = 'SFLC'                                          426
               AG_EXNAME = 'POSITION'                                     426
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                426
               CALL AXPOSITION                                            426
C          *                                                              426
C          Geant POSITION : SFLC ===> X=-2.3-0.02*JWAF Y=-0.3+0.02*JWAF   426
C          Z=+(SFPA_WPLEN+WAFPCKLEN)/2-16*WAFPCKLEN-WAFPCKLEN/2.-8.5*0.9  426
C          3+IWAF*0.93 ALPHAZ=+54 <===                                    426
               CALL AGSRESET                                              429
               AG_X = -2.3-0.02*JWAF                                      429
               AG_Y = -0.3+0.02*JWAF                                      429
               AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-16*WAFPCKLEN-WAFPCKLEN/   429
     *         2.-8.5*0.93+IWAF*0.93                                      429
               AG_ALPHAN=AG_ALPHAN+1                                      429
               AG_ALPHAK(AG_ALPHAN)=3                                     429
               AG_ALPHAL(AG_ALPHAN)=+54                                   429
               AG_TITLE = 'SFLC'                                          429
               AG_EXNAME = 'POSITION'                                     429
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                429
               CALL AXPOSITION                                            429
C          *                                                              429
            GO TO 5871                                                    430
5872        CONTINUE                                                      430
C       *                                                                 431
            JWAF=1                                                        431
               GO TO 5883                                                 431
5881           JWAF=JWAF+1                                                431
5883           IF(JWAF-(8).GT.0) GO TO 5882                               431
C          Geant POSITION : SFLA ===> X=+1.1+0.02*JWAF Y=1.35+0.02*JWAF   433
C          Z=-(SFPA_WPLEN+WAFPCKLEN)/2+(IWAF+8)*WAFPCKLEN ALPHAZ=-54 <==  433
C          =                                                              433
               CALL AGSRESET                                              433
               AG_X = +1.1+0.02*JWAF                                      433
               AG_Y = 1.35+0.02*JWAF                                      433
               AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+(IWAF+8)*WAFPCKLEN        433
               AG_ALPHAN=AG_ALPHAN+1                                      433
               AG_ALPHAK(AG_ALPHAN)=3                                     433
               AG_ALPHAL(AG_ALPHAN)=-54                                   433
               AG_TITLE = 'SFLA'                                          433
               AG_EXNAME = 'POSITION'                                     433
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                433
               CALL AXPOSITION                                            433
C          *                                                              433
C          Geant POSITION : SFLA ===> X=-1.1-0.02*JWAF Y=1.35+0.02*JWAF   433
C          Z=+(SFPA_WPLEN+WAFPCKLEN)/2-(IWAF+8)*WAFPCKLEN ALPHAZ=+54 <==  433
C          =                                                              433
               CALL AGSRESET                                              435
               AG_X = -1.1-0.02*JWAF                                      435
               AG_Y = 1.35+0.02*JWAF                                      435
               AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-(IWAF+8)*WAFPCKLEN        435
               AG_ALPHAN=AG_ALPHAN+1                                      435
               AG_ALPHAK(AG_ALPHAN)=3                                     435
               AG_ALPHAL(AG_ALPHAN)=+54                                   435
               AG_TITLE = 'SFLA'                                          435
               AG_EXNAME = 'POSITION'                                     435
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                435
               CALL AXPOSITION                                            435
C          *                                                              435
            GO TO 5881                                                    436
5882        CONTINUE                                                      436
C       Geant POSITION : SFLB ===> X=+1.1+0.02*IWAF Y=1.35+0.02*IWAF Z=-  438
C       (SFPA_WPLEN+WAFPCKLEN)/2+16*WAFPCKLEN+WAFPCKLEN/2.+7.5/2. ALPHAZ  438
C       =-54 <===                                                         438
            CALL AGSRESET                                                 438
            AG_X = +1.1+0.02*IWAF                                         438
            AG_Y = 1.35+0.02*IWAF                                         438
            AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+16*WAFPCKLEN+WAFPCKLEN/      438
     *      2.+7.5/2.                                                     438
            AG_ALPHAN=AG_ALPHAN+1                                         438
            AG_ALPHAK(AG_ALPHAN)=3                                        438
            AG_ALPHAL(AG_ALPHAN)=-54                                      438
            AG_TITLE = 'SFLB'                                             438
            AG_EXNAME = 'POSITION'                                        438
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   438
            CALL AXPOSITION                                               438
C       *                                                                 438
C       Geant POSITION : SFLB ===> X=-1.1-0.02*IWAF Y=1.35+0.02*IWAF Z=+  438
C       (SFPA_WPLEN+WAFPCKLEN)/2-16*WAFPCKLEN-WAFPCKLEN/2.-7.5/2. ALPHAZ  438
C       =+54 <===                                                         438
            CALL AGSRESET                                                 440
            AG_X = -1.1-0.02*IWAF                                         440
            AG_Y = 1.35+0.02*IWAF                                         440
            AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-16*WAFPCKLEN-WAFPCKLEN/      440
     *      2.-7.5/2.                                                     440
            AG_ALPHAN=AG_ALPHAN+1                                         440
            AG_ALPHAK(AG_ALPHAN)=3                                        440
            AG_ALPHAL(AG_ALPHAN)=+54                                      440
            AG_TITLE = 'SFLB'                                             440
            AG_EXNAME = 'POSITION'                                        440
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   440
            CALL AXPOSITION                                               440
C       *                                                                 440
C       Geant POSITION : SFES ===> X=-3.32 Y=-1.6 Z=-(SFPA_WPLEN+WAFPCKL  440
C       EN)/2+(IWAF)*WAFPCKLEN+WAFPCKLEN/2.-1.5/2.-0.1/2. ALPHAZ=57.66 <  440
C       ===                                                               440
            CALL AGSRESET                                                 443
            AG_X = -3.32                                                  443
            AG_Y = -1.6                                                   443
            AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+(IWAF)*WAFPCKLEN+WAFPCKLEN/  443
     *      2.-1.5/2.-0.1/2.                                              443
            AG_ALPHAN=AG_ALPHAN+1                                         443
            AG_ALPHAK(AG_ALPHAN)=3                                        443
            AG_ALPHAL(AG_ALPHAN)=57.66                                    443
            AG_TITLE = 'SFES'                                             443
            AG_EXNAME = 'POSITION'                                        443
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   443
            CALL AXPOSITION                                               443
C       *                                                                 443
C       Geant POSITION : SFES ===> X=+3.32 Y=-1.6 Z=+(SFPA_WPLEN+WAFPCKL  443
C       EN)/2-(IWAF)*WAFPCKLEN-WAFPCKLEN/2.+1.5/2.+0.1/2. ALPHAZ=-57.66   443
C       <===                                                              443
            CALL AGSRESET                                                 446
            AG_X = +3.32                                                  446
            AG_Y = -1.6                                                   446
            AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/2-(IWAF)*WAFPCKLEN-WAFPCKLEN/  446
     *      2.+1.5/2.+0.1/2.                                              446
            AG_ALPHAN=AG_ALPHAN+1                                         446
            AG_ALPHAK(AG_ALPHAN)=3                                        446
            AG_ALPHAL(AG_ALPHAN)=-57.66                                   446
            AG_TITLE = 'SFES'                                             446
            AG_EXNAME = 'POSITION'                                        446
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   446
            CALL AXPOSITION                                               446
C       *                                                                 446
C       Geant POSITION : SFEB ===> X=-2.71 Y=-0.75 Z=-(SFPA_WPLEN+WAFPCK  446
C       LEN)/2+(IWAF+8)*WAFPCKLEN+WAFPCKLEN/2.-1.5/2.-0.1/2. ALPHAZ=55.3  446
C       5 <===                                                            446
            CALL AGSRESET                                                 449
            AG_X = -2.71                                                  449
            AG_Y = -0.75                                                  449
            AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/                               449
     *      2+(IWAF+8)*WAFPCKLEN+WAFPCKLEN/2.-1.5/2.-0.1/2.               449
            AG_ALPHAN=AG_ALPHAN+1                                         449
            AG_ALPHAK(AG_ALPHAN)=3                                        449
            AG_ALPHAL(AG_ALPHAN)=55.35                                    449
            AG_TITLE = 'SFEB'                                             449
            AG_EXNAME = 'POSITION'                                        449
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   449
            CALL AXPOSITION                                               449
C       *                                                                 449
C       Geant POSITION : SFEB ===> X=+2.71 Y=-0.75 Z=+(SFPA_WPLEN+WAFPCK  449
C       LEN)/2-(IWAF+8)*WAFPCKLEN-WAFPCKLEN/2.+1.5/2.+0.1/2. ALPHAZ=-55.  449
C       35 <===                                                           449
            CALL AGSRESET                                                 452
            AG_X = +2.71                                                  452
            AG_Y = -0.75                                                  452
            AG_Z = +(SFPA_WPLEN+WAFPCKLEN)/                               452
     *      2-(IWAF+8)*WAFPCKLEN-WAFPCKLEN/2.+1.5/2.+0.1/2.               452
            AG_ALPHAN=AG_ALPHAN+1                                         452
            AG_ALPHAK(AG_ALPHAN)=3                                        452
            AG_ALPHAL(AG_ALPHAN)=-55.35                                   452
            AG_TITLE = 'SFEB'                                             452
            AG_EXNAME = 'POSITION'                                        452
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   452
            CALL AXPOSITION                                               452
C       *                                                                 452
         GO TO 5861                                                       453
5862     CONTINUE                                                         453
5251  CALL AGSPOPB                                                        454
      GO TO BLOCK_SFSM, (5250 ,5260)                                      454
C End of Local Procedure                                                  454
C ----------------------------------------------------------------------  455
C ------------ SFLT                                                       455
C Begin Local Procedure SFLT                                              456
C SFLT IS (HALF) THE TOP CORNER OF THE TRIANGULAR LADDER SKELETON         456
5440  CONTINUE                                                            456
         AG_TITLE='SFLT'                                                  456
         AG_MODULE=MODULE                                                 456
         AG_MARK='V00'                                                    456
         CALL AGSPUSH                                                     456
         AG_MODULE=MODULE                                                 456
C    *                                                                    456
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           457
         CALL AGSRESET                                                    457
         AG_TITLE = 'CARBON'                                              457
         AG_EXNAME = 'MATERIAL'                                           457
         AG_PARLIST = 'NONE'                                              457
         CALL AXMATERIAL                                                  457
C    *                                                                    457
C    Geant ATTRIBUTE : SFLT ===> SEEN=1 COLO=1 <===                       457
         CALL AGSRESET                                                    458
         AG_SEEN = 1                                                      458
         AG_COLO = 1                                                      458
         AG_TITLE = 'SFLT'                                                458
         AG_EXNAME = 'ATTRIBUTE'                                          458
         AG_PARLIST = 'SEEN_COLO'                                         458
         CALL AXATTRIBUTE                                                 458
C    *                                                                    458
C    Geant SHAPE : TRAP ===> DZ=49.8 THET=0 PHI=0 H1=0.02, BL1=0.2 TL1=0  458
C    .2-0.02/TAN(PI/5) ALP1=(PI/2-ATAN(2*TAN(PI/5)))*360/(2*PI), H2=0.02  458
C     BL2=0.2 TL2=0.2-0.02/TAN(PI/5), ALP2=(PI/2-ATAN(2*TAN(PI/5)))*360/  458
C    (2*PI) <===                                                          458
         CALL AGSRESET                                                    462
         AG_DZ = 49.8                                                     462
         AG_THET = 0                                                      462
         AG_PHI = 0                                                       462
         AG_H1 = 0.02                                                     462
         AG_BL1 = 0.2                                                     462
         AG_TL1 = 0.2-0.02/TAN(PI/5)                                      462
         AG_ALP1 = (PI/2-ATAN(2*TAN(PI/5)))*360/(2*PI)                    462
         AG_H2 = 0.02                                                     462
         AG_BL2 = 0.2                                                     462
         AG_TL2 = 0.2-0.02/TAN(PI/5)                                      462
         AG_ALP2 = (PI/2-ATAN(2*TAN(PI/5)))*360/(2*PI)                    462
         AG_TITLE = 'TRAP'                                                462
         AG_EXNAME = 'SHAPE'                                              462
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       462
         CALL AXSHAPE                                                     462
C    *                                                                    462
5441  CALL AGSPOPB                                                        463
      GO TO BLOCK_SFLT, (5440 ,5450)                                      463
C End of Local Procedure                                                  463
C ----------------------------------------------------------------------  464
C ------------ SFLU                                                       464
C Begin Local Procedure SFLU                                              465
C SFLU IS (HALF) THE SIDE CORNER OF THE TRIANGULAR LADDER SKELETON        465
5460  CONTINUE                                                            465
         AG_TITLE='SFLU'                                                  465
         AG_MODULE=MODULE                                                 465
         AG_MARK='V00'                                                    465
         CALL AGSPUSH                                                     465
         AG_MODULE=MODULE                                                 465
C    *                                                                    465
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           466
         CALL AGSRESET                                                    466
         AG_TITLE = 'CARBON'                                              466
         AG_EXNAME = 'MATERIAL'                                           466
         AG_PARLIST = 'NONE'                                              466
         CALL AXMATERIAL                                                  466
C    *                                                                    466
C    Geant ATTRIBUTE : SFLU ===> SEEN=1 COLO=1 <===                       466
         CALL AGSRESET                                                    467
         AG_SEEN = 1                                                      467
         AG_COLO = 1                                                      467
         AG_TITLE = 'SFLU'                                                467
         AG_EXNAME = 'ATTRIBUTE'                                          467
         AG_PARLIST = 'SEEN_COLO'                                         467
         CALL AXATTRIBUTE                                                 467
C    *                                                                    467
C    Geant SHAPE : TRAP ===> DZ=49.8 THET=0 PHI=0 H1=0.02, BL1=0.2 TL1=0  467
C    .2-0.02/TAN(27*2*PI/360) ALP1=(PI/2-ATAN(2*TAN(27*2*PI/360)))*360/(  467
C    2*PI), H2=0.02, BL2=0.2 TL2=0.2-0.02/TAN(27*2*PI/360) ALP2=(PI/2-AT  467
C    AN(2*TAN(27*2*PI/360)))*360/(2*PI) <===                              467
         CALL AGSRESET                                                    471
         AG_DZ = 49.8                                                     471
         AG_THET = 0                                                      471
         AG_PHI = 0                                                       471
         AG_H1 = 0.02                                                     471
         AG_BL1 = 0.2                                                     471
         AG_TL1 = 0.2-0.02/TAN(27*2*PI/360)                               471
         AG_ALP1 = (PI/2-ATAN(2*TAN(27*2*PI/360)))*360/(2*PI)             471
         AG_H2 = 0.02                                                     471
         AG_BL2 = 0.2                                                     471
         AG_TL2 = 0.2-0.02/TAN(27*2*PI/360)                               471
         AG_ALP2 = (PI/2-ATAN(2*TAN(27*2*PI/360)))*360/(2*PI)             471
         AG_TITLE = 'TRAP'                                                471
         AG_EXNAME = 'SHAPE'                                              471
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       471
         CALL AXSHAPE                                                     471
C    *                                                                    471
5461  CALL AGSPOPB                                                        472
      GO TO BLOCK_SFLU, (5460 ,5470)                                      472
C End of Local Procedure                                                  472
C ----------------------------------------------------------------------  473
C ------------ SFFK                                                       473
C Begin Local Procedure SFFK                                              474
C SFFK HORIZONTAL PART OF THE LADDER SKELETON CARBON BASE                 474
5480  CONTINUE                                                            474
         AG_TITLE='SFFK'                                                  474
         AG_MODULE=MODULE                                                 474
         AG_MARK='V00'                                                    474
         CALL AGSPUSH                                                     474
         AG_MODULE=MODULE                                                 474
C    *                                                                    474
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           475
         CALL AGSRESET                                                    475
         AG_TITLE = 'CARBON'                                              475
         AG_EXNAME = 'MATERIAL'                                           475
         AG_PARLIST = 'NONE'                                              475
         CALL AXMATERIAL                                                  475
C    *                                                                    475
C    Geant ATTRIBUTE : SFFK ===> SEEN=1 COLO=1 <===                       475
         CALL AGSRESET                                                    476
         AG_SEEN = 1                                                      476
         AG_COLO = 1                                                      476
         AG_TITLE = 'SFFK'                                                476
         AG_EXNAME = 'ATTRIBUTE'                                          476
         AG_PARLIST = 'SEEN_COLO'                                         476
         CALL AXATTRIBUTE                                                 476
C    *                                                                    476
C    Geant SHAPE : BOX ===> DX=0.4 DY=0.02 DZ=69.75/2 <===                476
         CALL AGSRESET                                                    477
         AG_DX = 0.4                                                      477
         AG_DY = 0.02                                                     477
         AG_DZ = 69.75/2                                                  477
         AG_TITLE = 'BOX'                                                 477
         AG_EXNAME = 'SHAPE'                                              477
         AG_PARLIST = 'DX_DY_DZ'                                          477
         CALL AXSHAPE                                                     477
C    *                                                                    477
5481  CALL AGSPOPB                                                        478
      GO TO BLOCK_SFFK, (5480 ,5490)                                      478
C End of Local Procedure                                                  478
C ----------------------------------------------------------------------  479
C ------------ SFFL                                                       479
C Begin Local Procedure SFFL                                              480
C SFFL TITLED PART OF THE LADDER SKELETON CARBON BASE                     480
5500  CONTINUE                                                            480
         AG_TITLE='SFFL'                                                  480
         AG_MODULE=MODULE                                                 480
         AG_MARK='V00'                                                    480
         CALL AGSPUSH                                                     480
         AG_MODULE=MODULE                                                 480
C    *                                                                    480
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           481
         CALL AGSRESET                                                    481
         AG_TITLE = 'CARBON'                                              481
         AG_EXNAME = 'MATERIAL'                                           481
         AG_PARLIST = 'NONE'                                              481
         CALL AXMATERIAL                                                  481
C    *                                                                    481
C    Geant ATTRIBUTE : SFFL ===> SEEN=1 COLO=1 <===                       481
         CALL AGSRESET                                                    482
         AG_SEEN = 1                                                      482
         AG_COLO = 1                                                      482
         AG_TITLE = 'SFFL'                                                482
         AG_EXNAME = 'ATTRIBUTE'                                          482
         AG_PARLIST = 'SEEN_COLO'                                         482
         CALL AXATTRIBUTE                                                 482
C    *                                                                    482
C    Geant SHAPE : BOX ===> DX=0.6/COS(15*PI/180) DY=0.02, DZ=69.75/2 <=  482
C    ==                                                                   482
         CALL AGSRESET                                                    484
         AG_DX = 0.6/COS(15*PI/180)                                       484
         AG_DY = 0.02                                                     484
         AG_DZ = 69.75/2                                                  484
         AG_TITLE = 'BOX'                                                 484
         AG_EXNAME = 'SHAPE'                                              484
         AG_PARLIST = 'DX_DY_DZ'                                          484
         CALL AXSHAPE                                                     484
C    *                                                                    484
5501  CALL AGSPOPB                                                        485
      GO TO BLOCK_SFFL, (5500 ,5510)                                      485
C End of Local Procedure                                                  485
C ----------------------------------------------------------------------  486
C ------------ SFKK                                                       486
C Begin Local Procedure SFKK                                              487
C SFKK HORIZONTAL PART OF THE KAPTON FILM UNDER THE LADDER BASE           487
5520  CONTINUE                                                            487
         AG_TITLE='SFKK'                                                  487
         AG_MODULE=MODULE                                                 487
         AG_MARK='V00'                                                    487
         CALL AGSPUSH                                                     487
         AG_MODULE=MODULE                                                 487
C    *                                                                    487
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            488
         CALL AGSRESET                                                    488
         AG_TITLE = 'MYLAR'                                               488
         AG_EXNAME = 'MATERIAL'                                           488
         AG_PARLIST = 'NONE'                                              488
         CALL AXMATERIAL                                                  488
C    *                                                                    488
C    Geant ATTRIBUTE : SFKK ===> SEEN=1 COLO=1 <===                       488
         CALL AGSRESET                                                    489
         AG_SEEN = 1                                                      489
         AG_COLO = 1                                                      489
         AG_TITLE = 'SFKK'                                                489
         AG_EXNAME = 'ATTRIBUTE'                                          489
         AG_PARLIST = 'SEEN_COLO'                                         489
         CALL AXATTRIBUTE                                                 489
C    *                                                                    489
C    Geant SHAPE : BOX ===> DX=5.1/2. DY=0.0025/2. DZ=69.75/2 <===        489
         CALL AGSRESET                                                    490
         AG_DX = 5.1/2.                                                   490
         AG_DY = 0.0025/2.                                                490
         AG_DZ = 69.75/2                                                  490
         AG_TITLE = 'BOX'                                                 490
         AG_EXNAME = 'SHAPE'                                              490
         AG_PARLIST = 'DX_DY_DZ'                                          490
         CALL AXSHAPE                                                     490
C    *                                                                    490
5521  CALL AGSPOPB                                                        491
      GO TO BLOCK_SFKK, (5520 ,5530)                                      491
C End of Local Procedure                                                  491
C ----------------------------------------------------------------------  492
C ------------ SFKL                                                       492
C Begin Local Procedure SFKL                                              493
C SFKL TITLED PART OF THE KPATON FILM UNDER THE LADDER BASE               493
5540  CONTINUE                                                            493
         AG_TITLE='SFKL'                                                  493
         AG_MODULE=MODULE                                                 493
         AG_MARK='V00'                                                    493
         CALL AGSPUSH                                                     493
         AG_MODULE=MODULE                                                 493
C    *                                                                    493
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            494
         CALL AGSRESET                                                    494
         AG_TITLE = 'MYLAR'                                               494
         AG_EXNAME = 'MATERIAL'                                           494
         AG_PARLIST = 'NONE'                                              494
         CALL AXMATERIAL                                                  494
C    *                                                                    494
C    Geant ATTRIBUTE : SFKL ===> SEEN=1 COLO=1 <===                       494
         CALL AGSRESET                                                    495
         AG_SEEN = 1                                                      495
         AG_COLO = 1                                                      495
         AG_TITLE = 'SFKL'                                                495
         AG_EXNAME = 'ATTRIBUTE'                                          495
         AG_PARLIST = 'SEEN_COLO'                                         495
         CALL AXATTRIBUTE                                                 495
C    *                                                                    495
C    Geant SHAPE : BOX ===> DX=0.6/COS(15*PI/180) DY=0.0025/2., DZ=69.75  495
C    /2 <===                                                              495
         CALL AGSRESET                                                    497
         AG_DX = 0.6/COS(15*PI/180)                                       497
         AG_DY = 0.0025/2.                                                497
         AG_DZ = 69.75/2                                                  497
         AG_TITLE = 'BOX'                                                 497
         AG_EXNAME = 'SHAPE'                                              497
         AG_PARLIST = 'DX_DY_DZ'                                          497
         CALL AXSHAPE                                                     497
C    *                                                                    497
5541  CALL AGSPOPB                                                        498
      GO TO BLOCK_SFKL, (5540 ,5550)                                      498
C End of Local Procedure                                                  498
C ----------------------------------------------------------------------  499
C ------------ SFLA                                                       499
C Begin Local Procedure SFLA                                              501
C SFLA IS THE LONG PART OF THE BUS CABLE LINKING THE MODULES TO THE CONN  501
C ECTION, BOARD                                                           501
5760  CONTINUE                                                            501
         AG_TITLE='SFLA'                                                  501
         AG_MODULE=MODULE                                                 501
         AG_MARK='V00'                                                    501
         CALL AGSPUSH                                                     501
         AG_MODULE=MODULE                                                 501
C    *                                                                    501
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            502
         CALL AGSRESET                                                    502
         AG_TITLE = 'MYLAR'                                               502
         AG_EXNAME = 'MATERIAL'                                           502
         AG_PARLIST = 'NONE'                                              502
         CALL AXMATERIAL                                                  502
C    *                                                                    502
C    Geant ATTRIBUTE : SFLA ===> SEEN=1 COLO=1 <===                       502
         CALL AGSRESET                                                    503
         AG_SEEN = 1                                                      503
         AG_COLO = 1                                                      503
         AG_TITLE = 'SFLA'                                                503
         AG_EXNAME = 'ATTRIBUTE'                                          503
         AG_PARLIST = 'SEEN_COLO'                                         503
         CALL AXATTRIBUTE                                                 503
C    *                                                                    503
C    Geant SHAPE : BOX ===> DX=1.5/2. DY=0.0130/2. DZ=4.3/2. <===         503
         CALL AGSRESET                                                    504
         AG_DX = 1.5/2.                                                   504
         AG_DY = 0.0130/2.                                                504
         AG_DZ = 4.3/2.                                                   504
         AG_TITLE = 'BOX'                                                 504
         AG_EXNAME = 'SHAPE'                                              504
         AG_PARLIST = 'DX_DY_DZ'                                          504
         CALL AXSHAPE                                                     504
C    *                                                                    504
5761  CALL AGSPOPB                                                        505
      GO TO BLOCK_SFLA, (5760 ,5770)                                      505
C End of Local Procedure                                                  505
C ----------------------------------------------------------------------  506
C ------------ SFLB                                                       506
C Begin Local Procedure SFLB                                              507
C SFLB IS THE PART OF THE LONG BUS CABLE ON THE CONNECTION BOARD          507
5780  CONTINUE                                                            507
         AG_TITLE='SFLB'                                                  507
         AG_MODULE=MODULE                                                 507
         AG_MARK='V00'                                                    507
         CALL AGSPUSH                                                     507
         AG_MODULE=MODULE                                                 507
C    *                                                                    507
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            508
         CALL AGSRESET                                                    508
         AG_TITLE = 'MYLAR'                                               508
         AG_EXNAME = 'MATERIAL'                                           508
         AG_PARLIST = 'NONE'                                              508
         CALL AXMATERIAL                                                  508
C    *                                                                    508
C    Geant ATTRIBUTE : SFLB ===> SEEN=1 COLO=1 <===                       508
         CALL AGSRESET                                                    509
         AG_SEEN = 1                                                      509
         AG_COLO = 1                                                      509
         AG_TITLE = 'SFLB'                                                509
         AG_EXNAME = 'ATTRIBUTE'                                          509
         AG_PARLIST = 'SEEN_COLO'                                         509
         CALL AXATTRIBUTE                                                 509
C    *                                                                    509
C    Geant SHAPE : BOX ===> DX=1.5/2. DY=0.0130/2. DZ=7.5/2. <===         509
         CALL AGSRESET                                                    510
         AG_DX = 1.5/2.                                                   510
         AG_DY = 0.0130/2.                                                510
         AG_DZ = 7.5/2.                                                   510
         AG_TITLE = 'BOX'                                                 510
         AG_EXNAME = 'SHAPE'                                              510
         AG_PARLIST = 'DX_DY_DZ'                                          510
         CALL AXSHAPE                                                     510
C    *                                                                    510
5781  CALL AGSPOPB                                                        511
      GO TO BLOCK_SFLB, (5780 ,5790)                                      511
C End of Local Procedure                                                  511
C ----------------------------------------------------------------------  512
C ------------ SFLC                                                       512
C Begin Local Procedure SFLC                                              514
C SFLC IS THE PART OF THE LONG BUS CABLE ON THE CONNECTION BOARD UP TO,   514
C THE CONNECTOR                                                           514
5800  CONTINUE                                                            514
         AG_TITLE='SFLC'                                                  514
         AG_MODULE=MODULE                                                 514
         AG_MARK='V00'                                                    514
         CALL AGSPUSH                                                     514
         AG_MODULE=MODULE                                                 514
C    *                                                                    514
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            515
         CALL AGSRESET                                                    515
         AG_TITLE = 'MYLAR'                                               515
         AG_EXNAME = 'MATERIAL'                                           515
         AG_PARLIST = 'NONE'                                              515
         CALL AXMATERIAL                                                  515
C    *                                                                    515
C    Geant ATTRIBUTE : SFLC ===> SEEN=1 COLO=1 <===                       515
         CALL AGSRESET                                                    516
         AG_SEEN = 1                                                      516
         AG_COLO = 1                                                      516
         AG_TITLE = 'SFLC'                                                516
         AG_EXNAME = 'ATTRIBUTE'                                          516
         AG_PARLIST = 'SEEN_COLO'                                         516
         CALL AXATTRIBUTE                                                 516
C    *                                                                    516
C    Geant SHAPE : BOX ===> DX=1.5/2. DY=0.0130/2. DZ=0.93/2. <===        516
         CALL AGSRESET                                                    517
         AG_DX = 1.5/2.                                                   517
         AG_DY = 0.0130/2.                                                517
         AG_DZ = 0.93/2.                                                  517
         AG_TITLE = 'BOX'                                                 517
         AG_EXNAME = 'SHAPE'                                              517
         AG_PARLIST = 'DX_DY_DZ'                                          517
         CALL AXSHAPE                                                     517
C    *                                                                    517
5801  CALL AGSPOPB                                                        518
      GO TO BLOCK_SFLC, (5800 ,5810)                                      518
C End of Local Procedure                                                  518
C ----------------------------------------------------------------------  519
C ------------ SFEB                                                       519
C Begin Local Procedure SFEB                                              520
C SFEB IS THE BIG BUS ELBOW                                               520
5820  CONTINUE                                                            520
         AG_TITLE='SFEB'                                                  520
         AG_MODULE=MODULE                                                 520
         AG_MARK='V00'                                                    520
         CALL AGSPUSH                                                     520
         AG_MODULE=MODULE                                                 520
C    *                                                                    520
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            521
         CALL AGSRESET                                                    521
         AG_TITLE = 'MYLAR'                                               521
         AG_EXNAME = 'MATERIAL'                                           521
         AG_PARLIST = 'NONE'                                              521
         CALL AXMATERIAL                                                  521
C    *                                                                    521
C    Geant ATTRIBUTE : SFEB ===> SEEN=1 COLO=1 <===                       521
         CALL AGSRESET                                                    522
         AG_SEEN = 1                                                      522
         AG_COLO = 1                                                      522
         AG_TITLE = 'SFEB'                                                522
         AG_EXNAME = 'ATTRIBUTE'                                          522
         AG_PARLIST = 'SEEN_COLO'                                         522
         CALL AXATTRIBUTE                                                 522
C    *                                                                    522
C    Geant SHAPE : BOX ===> DX=3.89/2. DY=0.0130/2. DZ=1.5/2. <===        522
         CALL AGSRESET                                                    523
         AG_DX = 3.89/2.                                                  523
         AG_DY = 0.0130/2.                                                523
         AG_DZ = 1.5/2.                                                   523
         AG_TITLE = 'BOX'                                                 523
         AG_EXNAME = 'SHAPE'                                              523
         AG_PARLIST = 'DX_DY_DZ'                                          523
         CALL AXSHAPE                                                     523
C    *                                                                    523
5821  CALL AGSPOPB                                                        524
      GO TO BLOCK_SFEB, (5820 ,5830)                                      524
C End of Local Procedure                                                  524
C ----------------------------------------------------------------------  525
C ------------ SFES                                                       525
C Begin Local Procedure SFES                                              526
C SFES IS THE SMALL BUS ELBOW                                             526
5840  CONTINUE                                                            526
         AG_TITLE='SFES'                                                  526
         AG_MODULE=MODULE                                                 526
         AG_MARK='V00'                                                    526
         CALL AGSPUSH                                                     526
         AG_MODULE=MODULE                                                 526
C    *                                                                    526
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            527
         CALL AGSRESET                                                    527
         AG_TITLE = 'MYLAR'                                               527
         AG_EXNAME = 'MATERIAL'                                           527
         AG_PARLIST = 'NONE'                                              527
         CALL AXMATERIAL                                                  527
C    *                                                                    527
C    Geant ATTRIBUTE : SFES ===> SEEN=1 COLO=1 <===                       527
         CALL AGSRESET                                                    528
         AG_SEEN = 1                                                      528
         AG_COLO = 1                                                      528
         AG_TITLE = 'SFES'                                                528
         AG_EXNAME = 'ATTRIBUTE'                                          528
         AG_PARLIST = 'SEEN_COLO'                                         528
         CALL AXATTRIBUTE                                                 528
C    *                                                                    528
C    Geant SHAPE : BOX ===> DX=1.875/2. DY=0.0130/2. DZ=1.5/2. <===       528
         CALL AGSRESET                                                    529
         AG_DX = 1.875/2.                                                 529
         AG_DY = 0.0130/2.                                                529
         AG_DZ = 1.5/2.                                                   529
         AG_TITLE = 'BOX'                                                 529
         AG_EXNAME = 'SHAPE'                                              529
         AG_PARLIST = 'DX_DY_DZ'                                          529
         CALL AXSHAPE                                                     529
C    *                                                                    529
5841  CALL AGSPOPB                                                        530
      GO TO BLOCK_SFES, (5840 ,5850)                                      530
C End of Local Procedure                                                  530
C ----------------------------------------------------------------------  531
C ------------ SFAM                                                       531
C Begin Local Procedure SFAM                                              532
C SFAM IS THE MOTHER VOLUME OF THE ADC BOARD                              532
5580  CONTINUE                                                            532
         AG_TITLE='SFAM'                                                  532
         AG_MODULE=MODULE                                                 532
         AG_MARK='V00'                                                    532
         CALL AGSPUSH                                                     532
         AG_MODULE=MODULE                                                 532
C    *                                                                    532
C    Geant MATERIAL : AIR ===> inherited arguments used <===              533
         CALL AGSRESET                                                    533
         AG_TITLE = 'AIR'                                                 533
         AG_EXNAME = 'MATERIAL'                                           533
         AG_PARLIST = 'NONE'                                              533
         CALL AXMATERIAL                                                  533
C    *                                                                    533
C    Geant ATTRIBUTE : SFAM ===> SEEN=0 COLO=6 <===                       533
         CALL AGSRESET                                                    534
         AG_SEEN = 0                                                      534
         AG_COLO = 6                                                      534
         AG_TITLE = 'SFAM'                                                534
         AG_EXNAME = 'ATTRIBUTE'                                          534
         AG_PARLIST = 'SEEN_COLO'                                         534
         CALL AXATTRIBUTE                                                 534
C    *                                                                    534
C    Geant SHAPE : BOX ===> DX=7.2/2. DY=0.3/2. DZ=12.4/2 <===            534
         CALL AGSRESET                                                    535
         AG_DX = 7.2/2.                                                   535
         AG_DY = 0.3/2.                                                   535
         AG_DZ = 12.4/2                                                   535
         AG_TITLE = 'BOX'                                                 535
         AG_EXNAME = 'SHAPE'                                              535
         AG_PARLIST = 'DX_DY_DZ'                                          535
         CALL AXSHAPE                                                     535
C    *                                                                    535
C    Create SFAB :                                                        536
C    Call To Local Procedure SFAB                                         536
         ASSIGN 5900 TO BLOCK_SFAB                                        536
         GO TO  5890                                                      536
5900     CONTINUE                                                         536
C    *                                                                    536
C    Geant POSITION : SFAB ===> X=0. Y=0. Z=12.4/2-10.25/2 <===           537
         CALL AGSRESET                                                    537
         AG_X = 0.                                                        537
         AG_Y = 0.                                                        537
         AG_Z = 12.4/2-10.25/2                                            537
         AG_TITLE = 'SFAB'                                                537
         AG_EXNAME = 'POSITION'                                           537
         AG_PARLIST = 'X_Y_Z'                                             537
         CALL AXPOSITION                                                  537
C    *                                                                    537
C    Create SFAS :                                                        538
C    Call To Local Procedure SFAS                                         538
         ASSIGN 5920 TO BLOCK_SFAS                                        538
         GO TO  5910                                                      538
5920     CONTINUE                                                         538
C    *                                                                    538
C    Geant POSITION : SFAS ===> X=7.2/2.-2.1/2. Y=0. Z=-12.4/2+2.15/2 <=  539
C    ==                                                                   539
         CALL AGSRESET                                                    539
         AG_X = 7.2/2.-2.1/2.                                             539
         AG_Y = 0.                                                        539
         AG_Z = -12.4/2+2.15/2                                            539
         AG_TITLE = 'SFAS'                                                539
         AG_EXNAME = 'POSITION'                                           539
         AG_PARLIST = 'X_Y_Z'                                             539
         CALL AXPOSITION                                                  539
C    *                                                                    539
C    Geant POSITION : SFAS ===> X=-7.2/2.+2.1/2. Y=0. Z=-12.4/2+2.15/2 <  539
C    ===                                                                  539
         CALL AGSRESET                                                    540
         AG_X = -7.2/2.+2.1/2.                                            540
         AG_Y = 0.                                                        540
         AG_Z = -12.4/2+2.15/2                                            540
         AG_TITLE = 'SFAS'                                                540
         AG_EXNAME = 'POSITION'                                           540
         AG_PARLIST = 'X_Y_Z'                                             540
         CALL AXPOSITION                                                  540
C    *                                                                    540
5581  CALL AGSPOPB                                                        541
      GO TO BLOCK_SFAM, (5580 ,5590)                                      541
C End of Local Procedure                                                  541
C ----------------------------------------------------------------------  542
C ------------ SFAB                                                       542
C Begin Local Procedure SFAB                                              543
C SFAB IS THE BIG VOLUME OF THE ADC BOARD                                 543
5890  CONTINUE                                                            543
         AG_TITLE='SFAB'                                                  543
         AG_MODULE=MODULE                                                 543
         AG_MARK='V00'                                                    543
         CALL AGSPUSH                                                     543
         AG_MODULE=MODULE                                                 543
C    *                                                                    543
C    Geant MATERIAL : G10 ===> inherited arguments used <===              544
         CALL AGSRESET                                                    544
         AG_TITLE = 'G10'                                                 544
         AG_EXNAME = 'MATERIAL'                                           544
         AG_PARLIST = 'NONE'                                              544
         CALL AXMATERIAL                                                  544
C    *                                                                    544
C    Geant ATTRIBUTE : SFAB ===> SEEN=1 COLO=30 <===                      544
         CALL AGSRESET                                                    545
         AG_SEEN = 1                                                      545
         AG_COLO = 30                                                     545
         AG_TITLE = 'SFAB'                                                545
         AG_EXNAME = 'ATTRIBUTE'                                          545
         AG_PARLIST = 'SEEN_COLO'                                         545
         CALL AXATTRIBUTE                                                 545
C    *                                                                    545
C    Geant SHAPE : BOX ===> DX=7.2/2. DY=0.3/2. DZ=10.25/2 <===           545
         CALL AGSRESET                                                    546
         AG_DX = 7.2/2.                                                   546
         AG_DY = 0.3/2.                                                   546
         AG_DZ = 10.25/2                                                  546
         AG_TITLE = 'BOX'                                                 546
         AG_EXNAME = 'SHAPE'                                              546
         AG_PARLIST = 'DX_DY_DZ'                                          546
         CALL AXSHAPE                                                     546
C    *                                                                    546
5891  CALL AGSPOPB                                                        547
      GO TO BLOCK_SFAB, (5890 ,5900)                                      547
C End of Local Procedure                                                  547
C ----------------------------------------------------------------------  548
C ------------ SFAS                                                       548
C Begin Local Procedure SFAS                                              549
C SFAS IS THE SMALL VOLUME OF THE ADC BOARD                               549
5910  CONTINUE                                                            549
         AG_TITLE='SFAS'                                                  549
         AG_MODULE=MODULE                                                 549
         AG_MARK='V00'                                                    549
         CALL AGSPUSH                                                     549
         AG_MODULE=MODULE                                                 549
C    *                                                                    549
C    Geant MATERIAL : G10 ===> inherited arguments used <===              550
         CALL AGSRESET                                                    550
         AG_TITLE = 'G10'                                                 550
         AG_EXNAME = 'MATERIAL'                                           550
         AG_PARLIST = 'NONE'                                              550
         CALL AXMATERIAL                                                  550
C    *                                                                    550
C    Geant ATTRIBUTE : SFAS ===> SEEN=1 COLO=30 <===                      550
         CALL AGSRESET                                                    551
         AG_SEEN = 1                                                      551
         AG_COLO = 30                                                     551
         AG_TITLE = 'SFAS'                                                551
         AG_EXNAME = 'ATTRIBUTE'                                          551
         AG_PARLIST = 'SEEN_COLO'                                         551
         CALL AXATTRIBUTE                                                 551
C    *                                                                    551
C    Geant SHAPE : BOX ===> DX=2.1/2. DY=0.3/2. DZ=2.15/2 <===            551
         CALL AGSRESET                                                    552
         AG_DX = 2.1/2.                                                   552
         AG_DY = 0.3/2.                                                   552
         AG_DZ = 2.15/2                                                   552
         AG_TITLE = 'BOX'                                                 552
         AG_EXNAME = 'SHAPE'                                              552
         AG_PARLIST = 'DX_DY_DZ'                                          552
         CALL AXSHAPE                                                     552
C    *                                                                    552
5911  CALL AGSPOPB                                                        553
      GO TO BLOCK_SFAS, (5910 ,5920)                                      553
C End of Local Procedure                                                  553
C ----------------------------------------------------------------------  554
C ------------ SAPP                                                       554
C Begin Local Procedure SAPP                                              555
C SAPP IS THE MOTHER VOLUME OF THE ADC BOARD APPENDICE                    555
5560  CONTINUE                                                            555
         AG_TITLE='SAPP'                                                  555
         AG_MODULE=MODULE                                                 555
         AG_MARK='V00'                                                    555
         CALL AGSPUSH                                                     555
         AG_MODULE=MODULE                                                 555
C    *                                                                    555
C    Geant MATERIAL : AIR ===> inherited arguments used <===              556
         CALL AGSRESET                                                    556
         AG_TITLE = 'AIR'                                                 556
         AG_EXNAME = 'MATERIAL'                                           556
         AG_PARLIST = 'NONE'                                              556
         CALL AXMATERIAL                                                  556
C    *                                                                    556
C    Geant ATTRIBUTE : SAPP ===> SEEN=0 COLO=6 <===                       556
         CALL AGSRESET                                                    557
         AG_SEEN = 0                                                      557
         AG_COLO = 6                                                      557
         AG_TITLE = 'SAPP'                                                557
         AG_EXNAME = 'ATTRIBUTE'                                          557
         AG_PARLIST = 'SEEN_COLO'                                         557
         CALL AXATTRIBUTE                                                 557
C    *                                                                    557
C    Geant SHAPE : BOX ===> DX=(7.6+0.08)/2. DY=(0.5+0.08)/2., DZ=1.275/  557
C    2. <===                                                              557
         CALL AGSRESET                                                    559
         AG_DX = (7.6+0.08)/2.                                            559
         AG_DY = (0.5+0.08)/2.                                            559
         AG_DZ = 1.275/2.                                                 559
         AG_TITLE = 'BOX'                                                 559
         AG_EXNAME = 'SHAPE'                                              559
         AG_PARLIST = 'DX_DY_DZ'                                          559
         CALL AXSHAPE                                                     559
C    *                                                                    559
C    Create SAPC :                                                        560
C    Call To Local Procedure SAPC                                         560
         ASSIGN 5940 TO BLOCK_SAPC                                        560
         GO TO  5930                                                      560
5940     CONTINUE                                                         560
C    *                                                                    560
C    Geant POSITION : SAPC ===> X=0. Y=0. Z=(-1.275+0.5)/2. <===          561
         CALL AGSRESET                                                    561
         AG_X = 0.                                                        561
         AG_Y = 0.                                                        561
         AG_Z = (-1.275+0.5)/2.                                           561
         AG_TITLE = 'SAPC'                                                561
         AG_EXNAME = 'POSITION'                                           561
         AG_PARLIST = 'X_Y_Z'                                             561
         CALL AXPOSITION                                                  561
C    *                                                                    561
C    Create SAPS :                                                        562
C    Call To Local Procedure SAPS                                         562
         ASSIGN 5960 TO BLOCK_SAPS                                        562
         GO TO  5950                                                      562
5960     CONTINUE                                                         562
C    *                                                                    562
C    Geant POSITION : SAPS ===> X=7.6/2.+0.04/2 Y=0 Z=0. <===             563
         CALL AGSRESET                                                    563
         AG_X = 7.6/2.+0.04/2                                             563
         AG_Y = 0                                                         563
         AG_Z = 0.                                                        563
         AG_TITLE = 'SAPS'                                                563
         AG_EXNAME = 'POSITION'                                           563
         AG_PARLIST = 'X_Y_Z'                                             563
         CALL AXPOSITION                                                  563
C    *                                                                    563
C    Geant POSITION : SAPS ===> X=-(7.6/2.+0.04/2) Y=0 Z=0. <===          563
         CALL AGSRESET                                                    564
         AG_X = -(7.6/2.+0.04/2)                                          564
         AG_Y = 0                                                         564
         AG_Z = 0.                                                        564
         AG_TITLE = 'SAPS'                                                564
         AG_EXNAME = 'POSITION'                                           564
         AG_PARLIST = 'X_Y_Z'                                             564
         CALL AXPOSITION                                                  564
C    *                                                                    564
C    Create SAPT :                                                        565
C    Call To Local Procedure SAPT                                         565
         ASSIGN 5980 TO BLOCK_SAPT                                        565
         GO TO  5970                                                      565
5980     CONTINUE                                                         565
C    *                                                                    565
C    Geant POSITION : SAPT ===> X=0. Y=(0.5+0.04)/2. Z=0. <===            566
         CALL AGSRESET                                                    566
         AG_X = 0.                                                        566
         AG_Y = (0.5+0.04)/2.                                             566
         AG_Z = 0.                                                        566
         AG_TITLE = 'SAPT'                                                566
         AG_EXNAME = 'POSITION'                                           566
         AG_PARLIST = 'X_Y_Z'                                             566
         CALL AXPOSITION                                                  566
C    *                                                                    566
C    Geant POSITION : SAPT ===> X=0. Y=-(0.5+0.04)/2. Z=0. <===           566
         CALL AGSRESET                                                    567
         AG_X = 0.                                                        567
         AG_Y = -(0.5+0.04)/2.                                            567
         AG_Z = 0.                                                        567
         AG_TITLE = 'SAPT'                                                567
         AG_EXNAME = 'POSITION'                                           567
         AG_PARLIST = 'X_Y_Z'                                             567
         CALL AXPOSITION                                                  567
C    *                                                                    567
5561  CALL AGSPOPB                                                        568
      GO TO BLOCK_SAPP, (5560 ,5570)                                      568
C End of Local Procedure                                                  568
C ----------------------------------------------------------------------  569
C ------------ SAPC                                                       569
C Begin Local Procedure SAPC                                              570
C SAPC IS THE CORE (EPOXY) OF THE ADC BOARD APPENDICE                     570
5930  CONTINUE                                                            570
         AG_TITLE='SAPC'                                                  570
         AG_MODULE=MODULE                                                 570
         AG_MARK='V00'                                                    570
         CALL AGSPUSH                                                     570
         AG_MODULE=MODULE                                                 570
C    *                                                                    570
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           571
         CALL AGSRESET                                                    571
         AG_TITLE = 'CARBON'                                              571
         AG_EXNAME = 'MATERIAL'                                           571
         AG_PARLIST = 'NONE'                                              571
         CALL AXMATERIAL                                                  571
C    *                                                                    571
C    Geant ATTRIBUTE : SAPC ===> SEEN=1 COLO=1 <===                       571
         CALL AGSRESET                                                    572
         AG_SEEN = 1                                                      572
         AG_COLO = 1                                                      572
         AG_TITLE = 'SAPC'                                                572
         AG_EXNAME = 'ATTRIBUTE'                                          572
         AG_PARLIST = 'SEEN_COLO'                                         572
         CALL AXATTRIBUTE                                                 572
C    *                                                                    572
C    Geant SHAPE : BOX ===> DX=7.6/2. DY=0.5/2. DZ=0.5/2. <===            572
         CALL AGSRESET                                                    573
         AG_DX = 7.6/2.                                                   573
         AG_DY = 0.5/2.                                                   573
         AG_DZ = 0.5/2.                                                   573
         AG_TITLE = 'BOX'                                                 573
         AG_EXNAME = 'SHAPE'                                              573
         AG_PARLIST = 'DX_DY_DZ'                                          573
         CALL AXSHAPE                                                     573
C    *                                                                    573
5931  CALL AGSPOPB                                                        574
      GO TO BLOCK_SAPC, (5930 ,5940)                                      574
C End of Local Procedure                                                  574
C ----------------------------------------------------------------------  575
C ------------ SAPS                                                       575
C Begin Local Procedure SAPS                                              576
C SAPS IS THE SIDE SHELL (CARBON) OF THE ADC BOARD APPENDICE              576
5950  CONTINUE                                                            576
         AG_TITLE='SAPS'                                                  576
         AG_MODULE=MODULE                                                 576
         AG_MARK='V00'                                                    576
         CALL AGSPUSH                                                     576
         AG_MODULE=MODULE                                                 576
C    *                                                                    576
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           577
         CALL AGSRESET                                                    577
         AG_TITLE = 'CARBON'                                              577
         AG_EXNAME = 'MATERIAL'                                           577
         AG_PARLIST = 'NONE'                                              577
         CALL AXMATERIAL                                                  577
C    *                                                                    577
C    Geant ATTRIBUTE : SAPS ===> SEEN=1 COLO=1 <===                       577
         CALL AGSRESET                                                    578
         AG_SEEN = 1                                                      578
         AG_COLO = 1                                                      578
         AG_TITLE = 'SAPS'                                                578
         AG_EXNAME = 'ATTRIBUTE'                                          578
         AG_PARLIST = 'SEEN_COLO'                                         578
         CALL AXATTRIBUTE                                                 578
C    *                                                                    578
C    Geant SHAPE : BOX ===> DX=0.04/2. DY=(0.5+0.08)/2. DZ=1.275/2. <===  578
         CALL AGSRESET                                                    579
         AG_DX = 0.04/2.                                                  579
         AG_DY = (0.5+0.08)/2.                                            579
         AG_DZ = 1.275/2.                                                 579
         AG_TITLE = 'BOX'                                                 579
         AG_EXNAME = 'SHAPE'                                              579
         AG_PARLIST = 'DX_DY_DZ'                                          579
         CALL AXSHAPE                                                     579
C    *                                                                    579
5951  CALL AGSPOPB                                                        580
      GO TO BLOCK_SAPS, (5950 ,5960)                                      580
C End of Local Procedure                                                  580
C ----------------------------------------------------------------------  581
C ------------ SAPT                                                       581
C Begin Local Procedure SAPT                                              582
C SAPT IS THE TOP-BOTTOM SHELL (CARBON) OF THE ADC BOARD APPENDICE        582
5970  CONTINUE                                                            582
         AG_TITLE='SAPT'                                                  582
         AG_MODULE=MODULE                                                 582
         AG_MARK='V00'                                                    582
         CALL AGSPUSH                                                     582
         AG_MODULE=MODULE                                                 582
C    *                                                                    582
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           583
         CALL AGSRESET                                                    583
         AG_TITLE = 'CARBON'                                              583
         AG_EXNAME = 'MATERIAL'                                           583
         AG_PARLIST = 'NONE'                                              583
         CALL AXMATERIAL                                                  583
C    *                                                                    583
C    Geant ATTRIBUTE : SAPT ===> SEEN=1 COLO=1 <===                       583
         CALL AGSRESET                                                    584
         AG_SEEN = 1                                                      584
         AG_COLO = 1                                                      584
         AG_TITLE = 'SAPT'                                                584
         AG_EXNAME = 'ATTRIBUTE'                                          584
         AG_PARLIST = 'SEEN_COLO'                                         584
         CALL AXATTRIBUTE                                                 584
C    *                                                                    584
C    Geant SHAPE : BOX ===> DX=7.6/2. DY=0.04/2. DZ=1.275/2. <===         584
         CALL AGSRESET                                                    585
         AG_DX = 7.6/2.                                                   585
         AG_DY = 0.04/2.                                                  585
         AG_DZ = 1.275/2.                                                 585
         AG_TITLE = 'BOX'                                                 585
         AG_EXNAME = 'SHAPE'                                              585
         AG_PARLIST = 'DX_DY_DZ'                                          585
         CALL AXSHAPE                                                     585
C    *                                                                    585
5971  CALL AGSPOPB                                                        586
      GO TO BLOCK_SAPT, (5970 ,5980)                                      586
C End of Local Procedure                                                  586
C ----------------------------------------------------------------------  587
C ------------ SFCO                                                       587
C Begin Local Procedure SFCO                                              588
C SFCO IS THE CONNECTION BOARD (RECTANGULAR WITH HIROSE CONNECTORS)       588
5600  CONTINUE                                                            588
         AG_TITLE='SFCO'                                                  588
         AG_MODULE=MODULE                                                 588
         AG_MARK='V00'                                                    588
         CALL AGSPUSH                                                     588
         AG_MODULE=MODULE                                                 588
C    *                                                                    588
C    Geant MATERIAL : G10 ===> inherited arguments used <===              589
         CALL AGSRESET                                                    589
         AG_TITLE = 'G10'                                                 589
         AG_EXNAME = 'MATERIAL'                                           589
         AG_PARLIST = 'NONE'                                              589
         CALL AXMATERIAL                                                  589
C    *                                                                    589
C    Geant ATTRIBUTE : SFCO ===> SEEN=1 COLO=30 <===                      589
         CALL AGSRESET                                                    590
         AG_SEEN = 1                                                      590
         AG_COLO = 30                                                     590
         AG_TITLE = 'SFCO'                                                590
         AG_EXNAME = 'ATTRIBUTE'                                          590
         AG_PARLIST = 'SEEN_COLO'                                         590
         CALL AXATTRIBUTE                                                 590
C    *                                                                    590
C    Geant SHAPE : BOX ===> DX=4.4/2. DY=0.37/2. DZ=15.8/2 <===           590
         CALL AGSRESET                                                    591
         AG_DX = 4.4/2.                                                   591
         AG_DY = 0.37/2.                                                  591
         AG_DZ = 15.8/2                                                   591
         AG_TITLE = 'BOX'                                                 591
         AG_EXNAME = 'SHAPE'                                              591
         AG_PARLIST = 'DX_DY_DZ'                                          591
         CALL AXSHAPE                                                     591
C    *                                                                    591
5601  CALL AGSPOPB                                                        592
      GO TO BLOCK_SFCO, (5600 ,5610)                                      592
C End of Local Procedure                                                  592
C ----------------------------------------------------------------------  593
C ------------ SFCM                                                       593
C Begin Local Procedure SFCM                                              594
C SFCM IS THE MOTHER VOLUME OF THE SECOND CONNECTION BOARD                594
5620  CONTINUE                                                            594
         AG_TITLE='SFCM'                                                  594
         AG_MODULE=MODULE                                                 594
         AG_MARK='V00'                                                    594
         CALL AGSPUSH                                                     594
         AG_MODULE=MODULE                                                 594
C    *                                                                    594
C    Geant MATERIAL : AIR ===> inherited arguments used <===              595
         CALL AGSRESET                                                    595
         AG_TITLE = 'AIR'                                                 595
         AG_EXNAME = 'MATERIAL'                                           595
         AG_PARLIST = 'NONE'                                              595
         CALL AXMATERIAL                                                  595
C    *                                                                    595
C    Geant ATTRIBUTE : SFCM ===> SEEN=0 COLO=6 <===                       595
         CALL AGSRESET                                                    596
         AG_SEEN = 0                                                      596
         AG_COLO = 6                                                      596
         AG_TITLE = 'SFCM'                                                596
         AG_EXNAME = 'ATTRIBUTE'                                          596
         AG_PARLIST = 'SEEN_COLO'                                         596
         CALL AXATTRIBUTE                                                 596
C    *                                                                    596
C    Geant SHAPE : BOX ===> DX=4.0/2. DY=0.37/2. DZ=15.8/2 <===           596
         CALL AGSRESET                                                    597
         AG_DX = 4.0/2.                                                   597
         AG_DY = 0.37/2.                                                  597
         AG_DZ = 15.8/2                                                   597
         AG_TITLE = 'BOX'                                                 597
         AG_EXNAME = 'SHAPE'                                              597
         AG_PARLIST = 'DX_DY_DZ'                                          597
         CALL AXSHAPE                                                     597
C    *                                                                    597
C    Create SFCB :                                                        598
C    Call To Local Procedure SFCB                                         598
         ASSIGN 6000 TO BLOCK_SFCB                                        598
         GO TO  5990                                                      598
6000     CONTINUE                                                         598
C    *                                                                    598
C    Geant POSITION : SFCB ===> X=+0.5 Y=0 Z=0 <===                       599
         CALL AGSRESET                                                    599
         AG_X = +0.5                                                      599
         AG_Y = 0                                                         599
         AG_Z = 0                                                         599
         AG_TITLE = 'SFCB'                                                599
         AG_EXNAME = 'POSITION'                                           599
         AG_PARLIST = 'X_Y_Z'                                             599
         CALL AXPOSITION                                                  599
C    *                                                                    599
C    Create SFCS :                                                        600
C    Call To Local Procedure SFCS                                         600
         ASSIGN 6020 TO BLOCK_SFCS                                        600
         GO TO  6010                                                      600
6020     CONTINUE                                                         600
C    *                                                                    600
C    Geant POSITION : SFCS ===> X=-1.5 Y=0 Z=-15.8/2+7.5/2 <===           601
         CALL AGSRESET                                                    601
         AG_X = -1.5                                                      601
         AG_Y = 0                                                         601
         AG_Z = -15.8/2+7.5/2                                             601
         AG_TITLE = 'SFCS'                                                601
         AG_EXNAME = 'POSITION'                                           601
         AG_PARLIST = 'X_Y_Z'                                             601
         CALL AXPOSITION                                                  601
C    *                                                                    601
5621  CALL AGSPOPB                                                        602
      GO TO BLOCK_SFCM, (5620 ,5630)                                      602
C End of Local Procedure                                                  602
C ----------------------------------------------------------------------  603
C ------------ SFCB                                                       603
C Begin Local Procedure SFCB                                              604
C SFCB IS THE BIG PART OF THE SECOND CONNECTION BOARD                     604
5990  CONTINUE                                                            604
         AG_TITLE='SFCB'                                                  604
         AG_MODULE=MODULE                                                 604
         AG_MARK='V00'                                                    604
         CALL AGSPUSH                                                     604
         AG_MODULE=MODULE                                                 604
C    *                                                                    604
C    Geant MATERIAL : G10 ===> inherited arguments used <===              605
         CALL AGSRESET                                                    605
         AG_TITLE = 'G10'                                                 605
         AG_EXNAME = 'MATERIAL'                                           605
         AG_PARLIST = 'NONE'                                              605
         CALL AXMATERIAL                                                  605
C    *                                                                    605
C    Geant ATTRIBUTE : SFCB ===> SEEN=1 COLO=30 <===                      605
         CALL AGSRESET                                                    606
         AG_SEEN = 1                                                      606
         AG_COLO = 30                                                     606
         AG_TITLE = 'SFCB'                                                606
         AG_EXNAME = 'ATTRIBUTE'                                          606
         AG_PARLIST = 'SEEN_COLO'                                         606
         CALL AXATTRIBUTE                                                 606
C    *                                                                    606
C    Geant SHAPE : BOX ===> DX=3.0/2. DY=0.37/2. DZ=15.8/2 <===           606
         CALL AGSRESET                                                    607
         AG_DX = 3.0/2.                                                   607
         AG_DY = 0.37/2.                                                  607
         AG_DZ = 15.8/2                                                   607
         AG_TITLE = 'BOX'                                                 607
         AG_EXNAME = 'SHAPE'                                              607
         AG_PARLIST = 'DX_DY_DZ'                                          607
         CALL AXSHAPE                                                     607
C    *                                                                    607
5991  CALL AGSPOPB                                                        608
      GO TO BLOCK_SFCB, (5990 ,6000)                                      608
C End of Local Procedure                                                  608
C ----------------------------------------------------------------------  609
C ------------ SFCS                                                       609
C Begin Local Procedure SFCS                                              610
C SFCS IS THE BIG PART OF THE SECOND CONNECTION BOARD                     610
6010  CONTINUE                                                            610
         AG_TITLE='SFCS'                                                  610
         AG_MODULE=MODULE                                                 610
         AG_MARK='V00'                                                    610
         CALL AGSPUSH                                                     610
         AG_MODULE=MODULE                                                 610
C    *                                                                    610
C    Geant MATERIAL : G10 ===> inherited arguments used <===              611
         CALL AGSRESET                                                    611
         AG_TITLE = 'G10'                                                 611
         AG_EXNAME = 'MATERIAL'                                           611
         AG_PARLIST = 'NONE'                                              611
         CALL AXMATERIAL                                                  611
C    *                                                                    611
C    Geant ATTRIBUTE : SFCS ===> SEEN=1 COLO=30 <===                      611
         CALL AGSRESET                                                    612
         AG_SEEN = 1                                                      612
         AG_COLO = 30                                                     612
         AG_TITLE = 'SFCS'                                                612
         AG_EXNAME = 'ATTRIBUTE'                                          612
         AG_PARLIST = 'SEEN_COLO'                                         612
         CALL AXATTRIBUTE                                                 612
C    *                                                                    612
C    Geant SHAPE : BOX ===> DX=1.0/2. DY=0.37/2. DZ=7.5/2 <===            612
         CALL AGSRESET                                                    613
         AG_DX = 1.0/2.                                                   613
         AG_DY = 0.37/2.                                                  613
         AG_DZ = 7.5/2                                                    613
         AG_TITLE = 'BOX'                                                 613
         AG_EXNAME = 'SHAPE'                                              613
         AG_PARLIST = 'DX_DY_DZ'                                          613
         CALL AXSHAPE                                                     613
C    *                                                                    613
6011  CALL AGSPOPB                                                        614
      GO TO BLOCK_SFCS, (6010 ,6020)                                      614
C End of Local Procedure                                                  614
C ----------------------------------------------------------------------  615
C ------------ SFKF                                                       615
C Begin Local Procedure SFKF                                              616
C SFKF IS THE FIRST PART OF THE KAPTON FLEX CIRCUIT                       616
5640  CONTINUE                                                            616
         AG_TITLE='SFKF'                                                  616
         AG_MODULE=MODULE                                                 616
         AG_MARK='V00'                                                    616
         CALL AGSPUSH                                                     616
         AG_MODULE=MODULE                                                 616
C    *                                                                    616
C    Geant MATERIAL : G10 ===> inherited arguments used <===              617
         CALL AGSRESET                                                    617
         AG_TITLE = 'G10'                                                 617
         AG_EXNAME = 'MATERIAL'                                           617
         AG_PARLIST = 'NONE'                                              617
         CALL AXMATERIAL                                                  617
C    *                                                                    617
C    Geant ATTRIBUTE : SFKF ===> SEEN=1 COLO=30 <===                      617
         CALL AGSRESET                                                    618
         AG_SEEN = 1                                                      618
         AG_COLO = 30                                                     618
         AG_TITLE = 'SFKF'                                                618
         AG_EXNAME = 'ATTRIBUTE'                                          618
         AG_PARLIST = 'SEEN_COLO'                                         618
         CALL AXATTRIBUTE                                                 618
C    *                                                                    618
C    Geant SHAPE : BOX ===> DX=0.44/2. DY=0.065/2. DZ=8.1/2 <===          618
         CALL AGSRESET                                                    619
         AG_DX = 0.44/2.                                                  619
         AG_DY = 0.065/2.                                                 619
         AG_DZ = 8.1/2                                                    619
         AG_TITLE = 'BOX'                                                 619
         AG_EXNAME = 'SHAPE'                                              619
         AG_PARLIST = 'DX_DY_DZ'                                          619
         CALL AXSHAPE                                                     619
C    *                                                                    619
5641  CALL AGSPOPB                                                        620
      GO TO BLOCK_SFKF, (5640 ,5650)                                      620
C End of Local Procedure                                                  620
C ----------------------------------------------------------------------  621
C ------------ SFKS                                                       621
C Begin Local Procedure SFKS                                              622
C SFKS IS THE SECOND PART OF THE KAPTON FLEX CIRCUIT                      622
5660  CONTINUE                                                            622
         AG_TITLE='SFKS'                                                  622
         AG_MODULE=MODULE                                                 622
         AG_MARK='V00'                                                    622
         CALL AGSPUSH                                                     622
         AG_MODULE=MODULE                                                 622
C    *                                                                    622
C    Geant MATERIAL : G10 ===> inherited arguments used <===              623
         CALL AGSRESET                                                    623
         AG_TITLE = 'G10'                                                 623
         AG_EXNAME = 'MATERIAL'                                           623
         AG_PARLIST = 'NONE'                                              623
         CALL AXMATERIAL                                                  623
C    *                                                                    623
C    Geant ATTRIBUTE : SFKS ===> SEEN=1 COLO=30 <===                      623
         CALL AGSRESET                                                    624
         AG_SEEN = 1                                                      624
         AG_COLO = 30                                                     624
         AG_TITLE = 'SFKS'                                                624
         AG_EXNAME = 'ATTRIBUTE'                                          624
         AG_PARLIST = 'SEEN_COLO'                                         624
         CALL AXATTRIBUTE                                                 624
C    *                                                                    624
C    Geant SHAPE : BOX ===> DX=1.65/2. DY=0.065/2. DZ=8.1/2 <===          624
         CALL AGSRESET                                                    625
         AG_DX = 1.65/2.                                                  625
         AG_DY = 0.065/2.                                                 625
         AG_DZ = 8.1/2                                                    625
         AG_TITLE = 'BOX'                                                 625
         AG_EXNAME = 'SHAPE'                                              625
         AG_PARLIST = 'DX_DY_DZ'                                          625
         CALL AXSHAPE                                                     625
C    *                                                                    625
5661  CALL AGSPOPB                                                        626
      GO TO BLOCK_SFKS, (5660 ,5670)                                      626
C End of Local Procedure                                                  626
C ----------------------------------------------------------------------  627
C ------------ SFPR                                                       627
C Begin Local Procedure SFPR                                              629
C SFPR IS THE LADDER END INSIDE MECHANICAL PART (PRISM WITH G10 WITH, HA  629
C LF DENSITY)                                                             629
5680  CONTINUE                                                            629
         AG_TITLE='SFPR'                                                  629
         AG_MODULE=MODULE                                                 629
         AG_MARK='V00'                                                    629
         CALL AGSPUSH                                                     629
         AG_MODULE=MODULE                                                 629
C    *                                                                    629
C    Geant MATERIAL : G5 ===> inherited arguments used <===               630
         CALL AGSRESET                                                    630
         AG_TITLE = 'G5'                                                  630
         AG_EXNAME = 'MATERIAL'                                           630
         AG_PARLIST = 'NONE'                                              630
         CALL AXMATERIAL                                                  630
C    *                                                                    630
C    Geant ATTRIBUTE : SFPR ===> SEEN=1 COLO=4 <===                       630
         CALL AGSRESET                                                    631
         AG_SEEN = 1                                                      631
         AG_COLO = 4                                                      631
         AG_TITLE = 'SFPR'                                                631
         AG_EXNAME = 'ATTRIBUTE'                                          631
         AG_PARLIST = 'SEEN_COLO'                                         631
         CALL AXATTRIBUTE                                                 631
C    *                                                                    631
C    Geant SHAPE : PGON ===> PHI1=-126 DPHI=72 NPDIV=1 NZ=2, ZI={0,3.7}   631
C    RMN={0,0} RMX={3.48,3.48} <===                                       631
         CALL AGSRESET                                                    633
         AG_PHI1 = -126                                                   633
         AG_DPHI = 72                                                     633
         AG_NPDIV = 1                                                     633
         AG_NZ = 2                                                        633
         AG_ZI(1)=0                                                       633
         AG_ZI(2)=3.7                                                     633
         AG_RMN(1)=0                                                      633
         AG_RMN(2)=0                                                      633
         AG_RMX(1)=3.48                                                   633
         AG_RMX(2)=3.48                                                   633
         AG_TITLE = 'PGON'                                                633
         AG_EXNAME = 'SHAPE'                                              633
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     633
         CALL AXSHAPE                                                     633
C    *                                                                    633
5681  CALL AGSPOPB                                                        634
      GO TO BLOCK_SFPR, (5680 ,5690)                                      634
C End of Local Procedure                                                  634
C ----------------------------------------------------------------------  635
C ------------ SFPB                                                       635
C Begin Local Procedure SFPB                                              636
C SFPB IS THE LADDER END OUTSIDE MECHANICAL PART (RECTANGLE WITH G10)     636
5700  CONTINUE                                                            636
         AG_TITLE='SFPB'                                                  636
         AG_MODULE=MODULE                                                 636
         AG_MARK='V00'                                                    636
         CALL AGSPUSH                                                     636
         AG_MODULE=MODULE                                                 636
C    *                                                                    636
C    Geant MATERIAL : G10 ===> inherited arguments used <===              637
         CALL AGSRESET                                                    637
         AG_TITLE = 'G10'                                                 637
         AG_EXNAME = 'MATERIAL'                                           637
         AG_PARLIST = 'NONE'                                              637
         CALL AXMATERIAL                                                  637
C    *                                                                    637
C    Geant ATTRIBUTE : SFPB ===> SEEN=1 COLO=4 <===                       637
         CALL AGSRESET                                                    638
         AG_SEEN = 1                                                      638
         AG_COLO = 4                                                      638
         AG_TITLE = 'SFPB'                                                638
         AG_EXNAME = 'ATTRIBUTE'                                          638
         AG_PARLIST = 'SEEN_COLO'                                         638
         CALL AXATTRIBUTE                                                 638
C    *                                                                    638
C    Geant SHAPE : BOX ===> DX=4.3/2. DY=0.8/2. DZ=0.95/2. <===           638
         CALL AGSRESET                                                    639
         AG_DX = 4.3/2.                                                   639
         AG_DY = 0.8/2.                                                   639
         AG_DZ = 0.95/2.                                                  639
         AG_TITLE = 'BOX'                                                 639
         AG_EXNAME = 'SHAPE'                                              639
         AG_PARLIST = 'DX_DY_DZ'                                          639
         CALL AXSHAPE                                                     639
C    *                                                                    639
5701  CALL AGSPOPB                                                        640
      GO TO BLOCK_SFPB, (5700 ,5710)                                      640
C End of Local Procedure                                                  640
C ----------------------------------------------------------------------  641
C ------------ SSBS                                                       641
C Begin Local Procedure SSBS                                              643
C SSBS IS THE SMALL PART OF THE ALUMINIUM PLATE LINKING THE LADDER TO, T  643
C HE SECTOR                                                               643
5720  CONTINUE                                                            643
         AG_TITLE='SSBS'                                                  643
         AG_MODULE=MODULE                                                 643
         AG_MARK='V00'                                                    643
         CALL AGSPUSH                                                     643
         AG_MODULE=MODULE                                                 643
C    *                                                                    643
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        644
         CALL AGSRESET                                                    644
         AG_TITLE = 'ALUMINIUM'                                           644
         AG_EXNAME = 'MATERIAL'                                           644
         AG_PARLIST = 'NONE'                                              644
         CALL AXMATERIAL                                                  644
C    *                                                                    644
C    Geant ATTRIBUTE : SSBS ===> SEEN=1 COLO=1 <===                       644
         CALL AGSRESET                                                    645
         AG_SEEN = 1                                                      645
         AG_COLO = 1                                                      645
         AG_TITLE = 'SSBS'                                                645
         AG_EXNAME = 'ATTRIBUTE'                                          645
         AG_PARLIST = 'SEEN_COLO'                                         645
         CALL AXATTRIBUTE                                                 645
C    *                                                                    645
C    Geant SHAPE : BOX ===> DX=2.5/2. DY=0.5/2. DZ=2.5/2. <===            645
         CALL AGSRESET                                                    646
         AG_DX = 2.5/2.                                                   646
         AG_DY = 0.5/2.                                                   646
         AG_DZ = 2.5/2.                                                   646
         AG_TITLE = 'BOX'                                                 646
         AG_EXNAME = 'SHAPE'                                              646
         AG_PARLIST = 'DX_DY_DZ'                                          646
         CALL AXSHAPE                                                     646
C    *                                                                    646
5721  CALL AGSPOPB                                                        647
      GO TO BLOCK_SSBS, (5720 ,5730)                                      647
C End of Local Procedure                                                  647
C ----------------------------------------------------------------------  648
C ------------ SSBB                                                       648
C Begin Local Procedure SSBB                                              650
C SSBB IS THE BIG PART OF THE ALUMINIUM PLATE LINKING THE LADDER TO THE,  650
C  SECTOR                                                                 650
5740  CONTINUE                                                            650
         AG_TITLE='SSBB'                                                  650
         AG_MODULE=MODULE                                                 650
         AG_MARK='V00'                                                    650
         CALL AGSPUSH                                                     650
         AG_MODULE=MODULE                                                 650
C    *                                                                    650
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        651
         CALL AGSRESET                                                    651
         AG_TITLE = 'ALUMINIUM'                                           651
         AG_EXNAME = 'MATERIAL'                                           651
         AG_PARLIST = 'NONE'                                              651
         CALL AXMATERIAL                                                  651
C    *                                                                    651
C    Geant ATTRIBUTE : SSBB ===> SEEN=1 COLO=1 <===                       651
         CALL AGSRESET                                                    652
         AG_SEEN = 1                                                      652
         AG_COLO = 1                                                      652
         AG_TITLE = 'SSBB'                                                652
         AG_EXNAME = 'ATTRIBUTE'                                          652
         AG_PARLIST = 'SEEN_COLO'                                         652
         CALL AXATTRIBUTE                                                 652
C    *                                                                    652
C    Geant SHAPE : BOX ===> DX=4.3/2. DY=0.5/2. DZ=1.9/2. <===            652
         CALL AGSRESET                                                    653
         AG_DX = 4.3/2.                                                   653
         AG_DY = 0.5/2.                                                   653
         AG_DZ = 1.9/2.                                                   653
         AG_TITLE = 'BOX'                                                 653
         AG_EXNAME = 'SHAPE'                                              653
         AG_PARLIST = 'DX_DY_DZ'                                          653
         CALL AXSHAPE                                                     653
C    *                                                                    653
5741  CALL AGSPOPB                                                        654
      GO TO BLOCK_SSBB, (5740 ,5750)                                      654
C End of Local Procedure                                                  654
C ----------------------------------------------------------------------  655
C ------------ SSST                                                       655
C Begin Local Procedure SSST                                              656
C SSST IS THE TOP OF THE SMALL SECTOR                                     656
5070  CONTINUE                                                            656
         AG_TITLE='SSST'                                                  656
         AG_MODULE=MODULE                                                 656
         AG_MARK='V00'                                                    656
         CALL AGSPUSH                                                     656
         AG_MODULE=MODULE                                                 656
C    *                                                                    656
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        657
         CALL AGSRESET                                                    657
         AG_TITLE = 'ALUMINIUM'                                           657
         AG_EXNAME = 'MATERIAL'                                           657
         AG_PARLIST = 'NONE'                                              657
         CALL AXMATERIAL                                                  657
C    *                                                                    657
C    Geant ATTRIBUTE : SSST ===> SEEN=1 COLO=1 <===                       657
         CALL AGSRESET                                                    658
         AG_SEEN = 1                                                      658
         AG_COLO = 1                                                      658
         AG_TITLE = 'SSST'                                                658
         AG_EXNAME = 'ATTRIBUTE'                                          658
         AG_PARLIST = 'SEEN_COLO'                                         658
         CALL AXATTRIBUTE                                                 658
C    *                                                                    658
C    Geant SHAPE : TUBS ===> RMIN=31.285 RMAX=31.8 PHI1=45., PHI2=135. D  658
C    Z=2.5/2. <===                                                        658
         CALL AGSRESET                                                    660
         AG_RMIN = 31.285                                                 660
         AG_RMAX = 31.8                                                   660
         AG_PHI1 = 45.                                                    660
         AG_PHI2 = 135.                                                   660
         AG_DZ = 2.5/2.                                                   660
         AG_TITLE = 'TUBS'                                                660
         AG_EXNAME = 'SHAPE'                                              660
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            660
         CALL AXSHAPE                                                     660
C    *                                                                    660
5071  CALL AGSPOPB                                                        661
      GO TO BLOCK_SSST, (5070 ,5080)                                      661
C End of Local Procedure                                                  661
C ----------------------------------------------------------------------  662
C ------------ SSSS                                                       662
C Begin Local Procedure SSSS                                              663
C SSSS IS THE SIDE OF THE SMALL SECTOR                                    663
5090  CONTINUE                                                            663
         AG_TITLE='SSSS'                                                  663
         AG_MODULE=MODULE                                                 663
         AG_MARK='V00'                                                    663
         CALL AGSPUSH                                                     663
         AG_MODULE=MODULE                                                 663
C    *                                                                    663
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        664
         CALL AGSRESET                                                    664
         AG_TITLE = 'ALUMINIUM'                                           664
         AG_EXNAME = 'MATERIAL'                                           664
         AG_PARLIST = 'NONE'                                              664
         CALL AXMATERIAL                                                  664
C    *                                                                    664
C    Geant ATTRIBUTE : SSSS ===> SEEN=1 COLO=1 <===                       664
         CALL AGSRESET                                                    665
         AG_SEEN = 1                                                      665
         AG_COLO = 1                                                      665
         AG_TITLE = 'SSSS'                                                665
         AG_EXNAME = 'ATTRIBUTE'                                          665
         AG_PARLIST = 'SEEN_COLO'                                         665
         CALL AXATTRIBUTE                                                 665
C    *                                                                    665
C    Geant SHAPE : TUBS ===> RMIN=23.3 RMAX=31.285 PHI1=45., PHI2=135. D  665
C    Z=0.5/2. <===                                                        665
         CALL AGSRESET                                                    667
         AG_RMIN = 23.3                                                   667
         AG_RMAX = 31.285                                                 667
         AG_PHI1 = 45.                                                    667
         AG_PHI2 = 135.                                                   667
         AG_DZ = 0.5/2.                                                   667
         AG_TITLE = 'TUBS'                                                667
         AG_EXNAME = 'SHAPE'                                              667
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            667
         CALL AXSHAPE                                                     667
C    *                                                                    667
5091  CALL AGSPOPB                                                        668
      GO TO BLOCK_SSSS, (5090 ,5100)                                      668
C End of Local Procedure                                                  668
C ----------------------------------------------------------------------  669
C ------------ SSRT                                                       669
C Begin Local Procedure SSRT                                              670
C SSRT IS THE TOP OF THE SIDE RIB                                         670
5110  CONTINUE                                                            670
         AG_TITLE='SSRT'                                                  670
         AG_MODULE=MODULE                                                 670
         AG_MARK='V00'                                                    670
         CALL AGSPUSH                                                     670
         AG_MODULE=MODULE                                                 670
C    *                                                                    670
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        671
         CALL AGSRESET                                                    671
         AG_TITLE = 'ALUMINIUM'                                           671
         AG_EXNAME = 'MATERIAL'                                           671
         AG_PARLIST = 'NONE'                                              671
         CALL AXMATERIAL                                                  671
C    *                                                                    671
C    Geant ATTRIBUTE : SSRT ===> SEEN=1 COLO=1 <===                       671
         CALL AGSRESET                                                    672
         AG_SEEN = 1                                                      672
         AG_COLO = 1                                                      672
         AG_TITLE = 'SSRT'                                                672
         AG_EXNAME = 'ATTRIBUTE'                                          672
         AG_PARLIST = 'SEEN_COLO'                                         672
         CALL AXATTRIBUTE                                                 672
C    *                                                                    672
C    Geant SHAPE : TUBS ===> RMIN=31.285 RMAX=31.8 PHI1=-45., PHI2=+45.   672
C    DZ=2.5/2. <===                                                       672
         CALL AGSRESET                                                    674
         AG_RMIN = 31.285                                                 674
         AG_RMAX = 31.8                                                   674
         AG_PHI1 = -45.                                                   674
         AG_PHI2 = +45.                                                   674
         AG_DZ = 2.5/2.                                                   674
         AG_TITLE = 'TUBS'                                                674
         AG_EXNAME = 'SHAPE'                                              674
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            674
         CALL AXSHAPE                                                     674
C    *                                                                    674
5111  CALL AGSPOPB                                                        675
      GO TO BLOCK_SSRT, (5110 ,5120)                                      675
C End of Local Procedure                                                  675
C ----------------------------------------------------------------------  676
C ------------ SSRS                                                       676
C Begin Local Procedure SSRS                                              677
C SSRS IS THE SIDE OF THE SMALL RIB                                       677
5130  CONTINUE                                                            677
         AG_TITLE='SSRS'                                                  677
         AG_MODULE=MODULE                                                 677
         AG_MARK='V00'                                                    677
         CALL AGSPUSH                                                     677
         AG_MODULE=MODULE                                                 677
C    *                                                                    677
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        678
         CALL AGSRESET                                                    678
         AG_TITLE = 'ALUMINIUM'                                           678
         AG_EXNAME = 'MATERIAL'                                           678
         AG_PARLIST = 'NONE'                                              678
         CALL AXMATERIAL                                                  678
C    *                                                                    678
C    Geant ATTRIBUTE : SSRS ===> SEEN=1 COLO=1 <===                       678
         CALL AGSRESET                                                    679
         AG_SEEN = 1                                                      679
         AG_COLO = 1                                                      679
         AG_TITLE = 'SSRS'                                                679
         AG_EXNAME = 'ATTRIBUTE'                                          679
         AG_PARLIST = 'SEEN_COLO'                                         679
         CALL AXATTRIBUTE                                                 679
C    *                                                                    679
C    Geant SHAPE : TUBS ===> RMIN=28.0 RMAX=31.285 PHI1=-45., PHI2=+45.   679
C    DZ=0.5/2. <===                                                       679
         CALL AGSRESET                                                    681
         AG_RMIN = 28.0                                                   681
         AG_RMAX = 31.285                                                 681
         AG_PHI1 = -45.                                                   681
         AG_PHI2 = +45.                                                   681
         AG_DZ = 0.5/2.                                                   681
         AG_TITLE = 'TUBS'                                                681
         AG_EXNAME = 'SHAPE'                                              681
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            681
         CALL AXSHAPE                                                     681
C    *                                                                    681
5131  CALL AGSPOPB                                                        682
      GO TO BLOCK_SSRS, (5130 ,5140)                                      682
C End of Local Procedure                                                  682
C ----------------------------------------------------------------------  683
C ------------ SSLB                                                       683
C Begin Local Procedure SSLB                                              684
C SSLB IS THE LINKING (SECTOR TO THE CONE) BOX                            684
5150  CONTINUE                                                            684
         AG_TITLE='SSLB'                                                  684
         AG_MODULE=MODULE                                                 684
         AG_MARK='V00'                                                    684
         CALL AGSPUSH                                                     684
         AG_MODULE=MODULE                                                 684
C    *                                                                    684
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        685
         CALL AGSRESET                                                    685
         AG_TITLE = 'ALUMINIUM'                                           685
         AG_EXNAME = 'MATERIAL'                                           685
         AG_PARLIST = 'NONE'                                              685
         CALL AXMATERIAL                                                  685
C    *                                                                    685
C    Geant ATTRIBUTE : SSLB ===> SEEN=1 COLO=1 <===                       685
         CALL AGSRESET                                                    686
         AG_SEEN = 1                                                      686
         AG_COLO = 1                                                      686
         AG_TITLE = 'SSLB'                                                686
         AG_EXNAME = 'ATTRIBUTE'                                          686
         AG_PARLIST = 'SEEN_COLO'                                         686
         CALL AXATTRIBUTE                                                 686
C    *                                                                    686
C    Geant SHAPE : BOX ===> DX=7.8/2. DY=7.3/2. DZ=2.5/2. <===            686
         CALL AGSRESET                                                    687
         AG_DX = 7.8/2.                                                   687
         AG_DY = 7.3/2.                                                   687
         AG_DZ = 2.5/2.                                                   687
         AG_TITLE = 'BOX'                                                 687
         AG_EXNAME = 'SHAPE'                                              687
         AG_PARLIST = 'DX_DY_DZ'                                          687
         CALL AXSHAPE                                                     687
C    *                                                                    687
5151  CALL AGSPOPB                                                        688
      GO TO BLOCK_SSLB, (5150 ,5160)                                      688
C End of Local Procedure                                                  688
C ----------------------------------------------------------------------  689
C ------------ SSLT                                                       689
C Begin Local Procedure SSLT                                              690
C SSLT IS THE LINKING (SECTOR TO THE CONE) TUBE                           690
5170  CONTINUE                                                            690
         AG_TITLE='SSLT'                                                  690
         AG_MODULE=MODULE                                                 690
         AG_MARK='V00'                                                    690
         CALL AGSPUSH                                                     690
         AG_MODULE=MODULE                                                 690
C    *                                                                    690
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        691
         CALL AGSRESET                                                    691
         AG_TITLE = 'ALUMINIUM'                                           691
         AG_EXNAME = 'MATERIAL'                                           691
         AG_PARLIST = 'NONE'                                              691
         CALL AXMATERIAL                                                  691
C    *                                                                    691
C    Geant ATTRIBUTE : SSLT ===> SEEN=1 COLO=1 <===                       691
         CALL AGSRESET                                                    692
         AG_SEEN = 1                                                      692
         AG_COLO = 1                                                      692
         AG_TITLE = 'SSLT'                                                692
         AG_EXNAME = 'ATTRIBUTE'                                          692
         AG_PARLIST = 'SEEN_COLO'                                         692
         CALL AXATTRIBUTE                                                 692
C    *                                                                    692
C    Geant SHAPE : TUBE ===> RMIN=0. RMAX=0.8 DZ=5.05/2. <===             692
         CALL AGSRESET                                                    693
         AG_RMIN = 0.                                                     693
         AG_RMAX = 0.8                                                    693
         AG_DZ = 5.05/2.                                                  693
         AG_TITLE = 'TUBE'                                                693
         AG_EXNAME = 'SHAPE'                                              693
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      693
         CALL AXSHAPE                                                     693
C    *                                                                    693
5171  CALL AGSPOPB                                                        694
      GO TO BLOCK_SSLT, (5170 ,5180)                                      694
C End of Local Procedure                                                  694
C ----------------------------------------------------------------------  695
C ------------ SCMP                                                       695
C Begin Local Procedure SCMP                                              696
C SCMP IS THE MOUNTING PLATE INSERTED IN THE CONES.                       696
5190  CONTINUE                                                            696
         AG_TITLE='SCMP'                                                  696
         AG_MODULE=MODULE                                                 696
         AG_MARK='V00'                                                    696
         CALL AGSPUSH                                                     696
         AG_MODULE=MODULE                                                 696
C    *                                                                    696
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        697
         CALL AGSRESET                                                    697
         AG_TITLE = 'ALUMINIUM'                                           697
         AG_EXNAME = 'MATERIAL'                                           697
         AG_PARLIST = 'NONE'                                              697
         CALL AXMATERIAL                                                  697
C    *                                                                    697
C    Geant ATTRIBUTE : SCMP ===> SEEN=1 COLO=1 <===                       697
         CALL AGSRESET                                                    698
         AG_SEEN = 1                                                      698
         AG_COLO = 1                                                      698
         AG_TITLE = 'SCMP'                                                698
         AG_EXNAME = 'ATTRIBUTE'                                          698
         AG_PARLIST = 'SEEN_COLO'                                         698
         CALL AXATTRIBUTE                                                 698
C    *                                                                    698
C    Geant SHAPE : BOX ===> DX=6.3/2. DY=0.4/2. DZ=3.6/2 <===             698
         CALL AGSRESET                                                    699
         AG_DX = 6.3/2.                                                   699
         AG_DY = 0.4/2.                                                   699
         AG_DZ = 3.6/2                                                    699
         AG_TITLE = 'BOX'                                                 699
         AG_EXNAME = 'SHAPE'                                              699
         AG_PARLIST = 'DX_DY_DZ'                                          699
         CALL AXSHAPE                                                     699
C    *                                                                    699
5191  CALL AGSPOPB                                                        700
      GO TO BLOCK_SCMP, (5190 ,5200)                                      700
C End of Local Procedure                                                  700
C ----------------------------------------------------------------------  701
C ------------ SCVM                                                       701
C Begin Local Procedure SCVM                                              702
C SCVM IS THE MOTHER VOLUME OF THE V-SHAPE PIECE                          702
5210  CONTINUE                                                            702
         AG_TITLE='SCVM'                                                  702
         AG_MODULE=MODULE                                                 702
         AG_MARK='V00'                                                    702
         CALL AGSPUSH                                                     702
         AG_MODULE=MODULE                                                 702
C    *                                                                    702
C    Geant MATERIAL : AIR ===> inherited arguments used <===              703
         CALL AGSRESET                                                    703
         AG_TITLE = 'AIR'                                                 703
         AG_EXNAME = 'MATERIAL'                                           703
         AG_PARLIST = 'NONE'                                              703
         CALL AXMATERIAL                                                  703
C    *                                                                    703
C    Geant ATTRIBUTE : SCVM ===> SEEN=0 COLO=6 <===                       703
         CALL AGSRESET                                                    704
         AG_SEEN = 0                                                      704
         AG_COLO = 6                                                      704
         AG_TITLE = 'SCVM'                                                704
         AG_EXNAME = 'ATTRIBUTE'                                          704
         AG_PARLIST = 'SEEN_COLO'                                         704
         CALL AXATTRIBUTE                                                 704
C    *                                                                    704
C    Geant SHAPE : BOX ===> DX=6.4/2. DY=2.6/2. DZ=3.6/2 <===             704
         CALL AGSRESET                                                    705
         AG_DX = 6.4/2.                                                   705
         AG_DY = 2.6/2.                                                   705
         AG_DZ = 3.6/2                                                    705
         AG_TITLE = 'BOX'                                                 705
         AG_EXNAME = 'SHAPE'                                              705
         AG_PARLIST = 'DX_DY_DZ'                                          705
         CALL AXSHAPE                                                     705
C    *                                                                    705
C    Create SCVB :                                                        706
C    Call To Local Procedure SCVB                                         706
         ASSIGN 6040 TO BLOCK_SCVB                                        706
         GO TO  6030                                                      706
6040     CONTINUE                                                         706
C    *                                                                    706
C    Geant POSITION : SCVB ===> X=0. Y=-2.6/2.+0.5/2. Z=0. <===           707
         CALL AGSRESET                                                    707
         AG_X = 0.                                                        707
         AG_Y = -2.6/2.+0.5/2.                                            707
         AG_Z = 0.                                                        707
         AG_TITLE = 'SCVB'                                                707
         AG_EXNAME = 'POSITION'                                           707
         AG_PARLIST = 'X_Y_Z'                                             707
         CALL AXPOSITION                                                  707
C    *                                                                    707
C    Create SCVS :                                                        708
C    Call To Local Procedure SCVS                                         708
         ASSIGN 6060 TO BLOCK_SCVS                                        708
         GO TO  6050                                                      708
6060     CONTINUE                                                         708
C    *                                                                    708
C    Geant POSITION : SCVS ===> X=1.0 Y=0.22 Z=0. ALPHAZ=45. <===         709
         CALL AGSRESET                                                    709
         AG_X = 1.0                                                       709
         AG_Y = 0.22                                                      709
         AG_Z = 0.                                                        709
         AG_ALPHAN=AG_ALPHAN+1                                            709
         AG_ALPHAK(AG_ALPHAN)=3                                           709
         AG_ALPHAL(AG_ALPHAN)=45.                                         709
         AG_TITLE = 'SCVS'                                                709
         AG_EXNAME = 'POSITION'                                           709
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      709
         CALL AXPOSITION                                                  709
C    *                                                                    709
C    Geant POSITION : SCVS ===> X=-1.0 Y=0.22 Z=0. ALPHAZ=-45. <===       709
         CALL AGSRESET                                                    710
         AG_X = -1.0                                                      710
         AG_Y = 0.22                                                      710
         AG_Z = 0.                                                        710
         AG_ALPHAN=AG_ALPHAN+1                                            710
         AG_ALPHAK(AG_ALPHAN)=3                                           710
         AG_ALPHAL(AG_ALPHAN)=-45.                                        710
         AG_TITLE = 'SCVS'                                                710
         AG_EXNAME = 'POSITION'                                           710
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      710
         CALL AXPOSITION                                                  710
C    *                                                                    710
5211  CALL AGSPOPB                                                        711
      GO TO BLOCK_SCVM, (5210 ,5220)                                      711
C End of Local Procedure                                                  711
C ----------------------------------------------------------------------  712
C ------------ SCVB                                                       712
C Begin Local Procedure SCVB                                              713
C SCVB IS THE BASE PLATE OF THE V-SHAPE PIECE                             713
6030  CONTINUE                                                            713
         AG_TITLE='SCVB'                                                  713
         AG_MODULE=MODULE                                                 713
         AG_MARK='V00'                                                    713
         CALL AGSPUSH                                                     713
         AG_MODULE=MODULE                                                 713
C    *                                                                    713
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        714
         CALL AGSRESET                                                    714
         AG_TITLE = 'ALUMINIUM'                                           714
         AG_EXNAME = 'MATERIAL'                                           714
         AG_PARLIST = 'NONE'                                              714
         CALL AXMATERIAL                                                  714
C    *                                                                    714
C    Geant ATTRIBUTE : SCVB ===> SEEN=1 COLO=1 <===                       714
         CALL AGSRESET                                                    715
         AG_SEEN = 1                                                      715
         AG_COLO = 1                                                      715
         AG_TITLE = 'SCVB'                                                715
         AG_EXNAME = 'ATTRIBUTE'                                          715
         AG_PARLIST = 'SEEN_COLO'                                         715
         CALL AXATTRIBUTE                                                 715
C    *                                                                    715
C    Geant SHAPE : BOX ===> DX=6.4/2. DY=0.5/2. DZ=3.6/2 <===             715
         CALL AGSRESET                                                    716
         AG_DX = 6.4/2.                                                   716
         AG_DY = 0.5/2.                                                   716
         AG_DZ = 3.6/2                                                    716
         AG_TITLE = 'BOX'                                                 716
         AG_EXNAME = 'SHAPE'                                              716
         AG_PARLIST = 'DX_DY_DZ'                                          716
         CALL AXSHAPE                                                     716
C    *                                                                    716
6031  CALL AGSPOPB                                                        717
      GO TO BLOCK_SCVB, (6030 ,6040)                                      717
C End of Local Procedure                                                  717
C ----------------------------------------------------------------------  718
C ------------ SCVS                                                       718
C Begin Local Procedure SCVS                                              719
C SCVS IS THE SIDE PLATE OF THE V-SHAPE PIECE                             719
6050  CONTINUE                                                            719
         AG_TITLE='SCVS'                                                  719
         AG_MODULE=MODULE                                                 719
         AG_MARK='V00'                                                    719
         CALL AGSPUSH                                                     719
         AG_MODULE=MODULE                                                 719
C    *                                                                    719
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        720
         CALL AGSRESET                                                    720
         AG_TITLE = 'ALUMINIUM'                                           720
         AG_EXNAME = 'MATERIAL'                                           720
         AG_PARLIST = 'NONE'                                              720
         CALL AXMATERIAL                                                  720
C    *                                                                    720
C    Geant ATTRIBUTE : SCVS ===> SEEN=1 COLO=1 <===                       720
         CALL AGSRESET                                                    721
         AG_SEEN = 1                                                      721
         AG_COLO = 1                                                      721
         AG_TITLE = 'SCVS'                                                721
         AG_EXNAME = 'ATTRIBUTE'                                          721
         AG_PARLIST = 'SEEN_COLO'                                         721
         CALL AXATTRIBUTE                                                 721
C    *                                                                    721
C    Geant SHAPE : BOX ===> DX=2.3/2. DY=0.6/2. DZ=3.6/2 <===             721
         CALL AGSRESET                                                    722
         AG_DX = 2.3/2.                                                   722
         AG_DY = 0.6/2.                                                   722
         AG_DZ = 3.6/2                                                    722
         AG_TITLE = 'BOX'                                                 722
         AG_EXNAME = 'SHAPE'                                              722
         AG_PARLIST = 'DX_DY_DZ'                                          722
         CALL AXSHAPE                                                     722
C    *                                                                    722
6051  CALL AGSPOPB                                                        723
      GO TO BLOCK_SCVS, (6050 ,6060)                                      723
C End of Local Procedure                                                  723
      WRITE(LOUT,6070)'===>>  [End AgML Module SISDGEO1]  <<==='          723
6070  FORMAT(A32,/,/)                                                     723
C MODULE SISDGEO1                                                         725
      END                                                                 725
