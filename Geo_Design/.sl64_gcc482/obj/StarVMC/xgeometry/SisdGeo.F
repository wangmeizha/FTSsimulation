C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE SISDGEO                                                  1
C SISDGEO IS THE SILICON STRIP DETECTOR                                   1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'SISDGEO'/, BANK_TITLE/'*'/                             1
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
C AUTHOR MAXIM POTEKHIN                                                   1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'MAXIM POTEKHIN'/                              2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C CREATED 17 NOV 03                                                       2
      CHARACTER*40 CREATED /'17 NOV 03'/                                  3
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
      INTEGER BLOCK_SFLM                                                  7
      INTEGER BLOCK_SFDM                                                  7
      INTEGER BLOCK_SFSW                                                  7
      INTEGER BLOCK_SFSD                                                  7
      INTEGER BLOCK_SFSM                                                  7
      INTEGER BLOCK_SFSS                                                  7
      INTEGER BLOCK_SFCP                                                  7
      INTEGER BLOCK_SFCW                                                  7
      INTEGER BLOCK_SFCF                                                  7
      INTEGER BLOCK_SFCT                                                  7
      INTEGER BLOCK_SFCX                                                  7
C -                                                                       8
      REAL SSDP_VERSION                                                   8
      EQUIVALENCE (BANK_SSDP(3),SSDP_VERSION,RE_SSDP_VERSION),            8
     *(NAMES_SSDP(1),NSSDP_VERSION)                                       8
      EQUIVALENCE (COMMEN_SSDP(1),CSSDP_VERSION),(MAP_SSDP(1,1),          8
     *MSSDP_VERSION)                                                      8
      CHARACTER CSSDP_VERSION*40/' '/                                     8
      CHARACTER NSSDP_VERSION*12/'R.VERSION'/                             8
      INTEGER MSSDP_VERSION(2)/1,1/                                       8
      REAL RE_SSDP_VERSION                                                8
C -                                                                       8
      REAL SSDP_CONFIG                                                    8
      EQUIVALENCE (BANK_SSDP(4),SSDP_CONFIG,RE_SSDP_CONFIG),              8
     *(NAMES_SSDP(2),NSSDP_CONFIG)                                        8
      EQUIVALENCE (COMMEN_SSDP(2),CSSDP_CONFIG),(MAP_SSDP(1,2),           8
     *MSSDP_CONFIG)                                                       8
      CHARACTER CSSDP_CONFIG*40/' '/                                      8
      CHARACTER NSSDP_CONFIG*12/'R.CONFIG'/                               8
      INTEGER MSSDP_CONFIG(2)/1,1/                                        8
      REAL RE_SSDP_CONFIG                                                 8
C -                                                                       8
      INTEGER SSDP_PLACEMENT                                              8
      EQUIVALENCE (BANK_SSDP(5),SSDP_PLACEMENT,RE_SSDP_PLACEMENT),        8
     *(NAMES_SSDP(3),NSSDP_PLACEMENT)                                     8
      EQUIVALENCE (COMMEN_SSDP(3),CSSDP_PLACEMENT),(MAP_SSDP(1,3),        8
     *MSSDP_PLACEMENT)                                                    8
      CHARACTER CSSDP_PLACEMENT*40/' '/                                   8
      CHARACTER NSSDP_PLACEMENT*12/'I.PLACEMENT'/                         8
      INTEGER MSSDP_PLACEMENT(2)/1,1/                                     8
      REAL RE_SSDP_PLACEMENT                                              8
C -                                                                       8
      INTEGER LEN_SSDP,LLEN_SSDP,LINK_SSDP                                8
      PARAMETER (LEN_SSDP=5)                                              8
      PARAMETER (LLEN_SSDP=3)                                             8
      INTEGER FLAG_SSDP/0/,MAP_SSDP(2,LLEN_SSDP),IDNUM_SSDP(10)           8
      CHARACTER NAMES_SSDP(LLEN_SSDP)*12,COMMEN_SSDP(LLEN_SSDP)*40        8
      DATA IDNUM_SSDP/10*0/                                               8
      REAL BANK_SSDP(LEN_SSDP)                                            8
      CHARACTER*60 BPATH_SSDP/' '/                                        8
      SAVE FLAG_SSDP,MAP_SSDP,IDNUM_SSDP,NAMES_SSDP,COMMEN_SSDP,          8
     *BPATH_SSDP                                                          8
      DATA LINK_SSDP/0/,BANK_SSDP/LEN_SSDP*0/                             8
C *                                                                       8
C -                                                                       11
      REAL SFPA_VERSION                                                   11
      EQUIVALENCE (BANK_SFPA(3),SFPA_VERSION,RE_SFPA_VERSION),            11
     *(NAMES_SFPA(1),NSFPA_VERSION)                                       11
      EQUIVALENCE (COMMEN_SFPA(1),CSFPA_VERSION),(MAP_SFPA(1,1),          11
     *MSFPA_VERSION)                                                      11
      CHARACTER CSFPA_VERSION*40/' '/                                     11
      CHARACTER NSFPA_VERSION*12/'R.VERSION'/                             11
      INTEGER MSFPA_VERSION(2)/1,1/                                       11
      REAL RE_SFPA_VERSION                                                11
C -                                                                       11
      REAL SFPA_RMIN                                                      11
      EQUIVALENCE (BANK_SFPA(4),SFPA_RMIN,RE_SFPA_RMIN),(NAMES_SFPA(2),   11
     *NSFPA_RMIN)                                                         11
      EQUIVALENCE (COMMEN_SFPA(2),CSFPA_RMIN),(MAP_SFPA(1,2),             11
     *MSFPA_RMIN)                                                         11
      CHARACTER CSFPA_RMIN*40/' '/                                        11
      CHARACTER NSFPA_RMIN*12/'R.RMIN'/                                   11
      INTEGER MSFPA_RMIN(2)/1,1/                                          11
      REAL RE_SFPA_RMIN                                                   11
C -                                                                       11
      REAL SFPA_RMAX                                                      11
      EQUIVALENCE (BANK_SFPA(5),SFPA_RMAX,RE_SFPA_RMAX),(NAMES_SFPA(3),   11
     *NSFPA_RMAX)                                                         11
      EQUIVALENCE (COMMEN_SFPA(3),CSFPA_RMAX),(MAP_SFPA(1,3),             11
     *MSFPA_RMAX)                                                         11
      CHARACTER CSFPA_RMAX*40/' '/                                        11
      CHARACTER NSFPA_RMAX*12/'R.RMAX'/                                   11
      INTEGER MSFPA_RMAX(2)/1,1/                                          11
      REAL RE_SFPA_RMAX                                                   11
C -                                                                       11
      REAL SFPA_LEN                                                       11
      EQUIVALENCE (BANK_SFPA(6),SFPA_LEN,RE_SFPA_LEN),(NAMES_SFPA(4),     11
     *NSFPA_LEN)                                                          11
      EQUIVALENCE (COMMEN_SFPA(4),CSFPA_LEN),(MAP_SFPA(1,4),MSFPA_LEN)    11
      CHARACTER CSFPA_LEN*40/' '/                                         11
      CHARACTER NSFPA_LEN*12/'R.LEN'/                                     11
      INTEGER MSFPA_LEN(2)/1,1/                                           11
      REAL RE_SFPA_LEN                                                    11
C -                                                                       11
      REAL SFPA_RAD                                                       11
      EQUIVALENCE (BANK_SFPA(7),SFPA_RAD,RE_SFPA_RAD),(NAMES_SFPA(5),     11
     *NSFPA_RAD)                                                          11
      EQUIVALENCE (COMMEN_SFPA(5),CSFPA_RAD),(MAP_SFPA(1,5),MSFPA_RAD)    11
      CHARACTER CSFPA_RAD*40/' '/                                         11
      CHARACTER NSFPA_RAD*12/'R.RAD'/                                     11
      INTEGER MSFPA_RAD(2)/1,1/                                           11
      REAL RE_SFPA_RAD                                                    11
C -                                                                       11
      REAL SFPA_NSSD                                                      11
      EQUIVALENCE (BANK_SFPA(8),SFPA_NSSD,RE_SFPA_NSSD),(NAMES_SFPA(6),   11
     *NSFPA_NSSD)                                                         11
      EQUIVALENCE (COMMEN_SFPA(6),CSFPA_NSSD),(MAP_SFPA(1,6),             11
     *MSFPA_NSSD)                                                         11
      CHARACTER CSFPA_NSSD*40/' '/                                        11
      CHARACTER NSFPA_NSSD*12/'R.NSSD'/                                   11
      INTEGER MSFPA_NSSD(2)/1,1/                                          11
      REAL RE_SFPA_NSSD                                                   11
C -                                                                       11
      REAL SFPA_DMWID                                                     11
      EQUIVALENCE (BANK_SFPA(9),SFPA_DMWID,RE_SFPA_DMWID),                11
     *(NAMES_SFPA(7),NSFPA_DMWID)                                         11
      EQUIVALENCE (COMMEN_SFPA(7),CSFPA_DMWID),(MAP_SFPA(1,7),            11
     *MSFPA_DMWID)                                                        11
      CHARACTER CSFPA_DMWID*40/' '/                                       11
      CHARACTER NSFPA_DMWID*12/'R.DMWID'/                                 11
      INTEGER MSFPA_DMWID(2)/1,1/                                         11
      REAL RE_SFPA_DMWID                                                  11
C -                                                                       11
      REAL SFPA_DMTHK                                                     11
      EQUIVALENCE (BANK_SFPA(10),SFPA_DMTHK,RE_SFPA_DMTHK),               11
     *(NAMES_SFPA(8),NSFPA_DMTHK)                                         11
      EQUIVALENCE (COMMEN_SFPA(8),CSFPA_DMTHK),(MAP_SFPA(1,8),            11
     *MSFPA_DMTHK)                                                        11
      CHARACTER CSFPA_DMTHK*40/' '/                                       11
      CHARACTER NSFPA_DMTHK*12/'R.DMTHK'/                                 11
      INTEGER MSFPA_DMTHK(2)/1,1/                                         11
      REAL RE_SFPA_DMTHK                                                  11
C -                                                                       11
      REAL SFPA_DMLEN                                                     11
      EQUIVALENCE (BANK_SFPA(11),SFPA_DMLEN,RE_SFPA_DMLEN),               11
     *(NAMES_SFPA(9),NSFPA_DMLEN)                                         11
      EQUIVALENCE (COMMEN_SFPA(9),CSFPA_DMLEN),(MAP_SFPA(1,9),            11
     *MSFPA_DMLEN)                                                        11
      CHARACTER CSFPA_DMLEN*40/' '/                                       11
      CHARACTER NSFPA_DMLEN*12/'R.DMLEN'/                                 11
      INTEGER MSFPA_DMLEN(2)/1,1/                                         11
      REAL RE_SFPA_DMLEN                                                  11
C -                                                                       11
      REAL SFPA_SMWID                                                     11
      EQUIVALENCE (BANK_SFPA(12),SFPA_SMWID,RE_SFPA_SMWID),               11
     *(NAMES_SFPA(10),NSFPA_SMWID)                                        11
      EQUIVALENCE (COMMEN_SFPA(10),CSFPA_SMWID),(MAP_SFPA(1,10),          11
     *MSFPA_SMWID)                                                        11
      CHARACTER CSFPA_SMWID*40/' '/                                       11
      CHARACTER NSFPA_SMWID*12/'R.SMWID'/                                 11
      INTEGER MSFPA_SMWID(2)/1,1/                                         11
      REAL RE_SFPA_SMWID                                                  11
C -                                                                       11
      REAL SFPA_SMTHK                                                     11
      EQUIVALENCE (BANK_SFPA(13),SFPA_SMTHK,RE_SFPA_SMTHK),               11
     *(NAMES_SFPA(11),NSFPA_SMTHK)                                        11
      EQUIVALENCE (COMMEN_SFPA(11),CSFPA_SMTHK),(MAP_SFPA(1,11),          11
     *MSFPA_SMTHK)                                                        11
      CHARACTER CSFPA_SMTHK*40/' '/                                       11
      CHARACTER NSFPA_SMTHK*12/'R.SMTHK'/                                 11
      INTEGER MSFPA_SMTHK(2)/1,1/                                         11
      REAL RE_SFPA_SMTHK                                                  11
C -                                                                       11
      REAL SFPA_SMLEN                                                     11
      EQUIVALENCE (BANK_SFPA(14),SFPA_SMLEN,RE_SFPA_SMLEN),               11
     *(NAMES_SFPA(12),NSFPA_SMLEN)                                        11
      EQUIVALENCE (COMMEN_SFPA(12),CSFPA_SMLEN),(MAP_SFPA(1,12),          11
     *MSFPA_SMLEN)                                                        11
      CHARACTER CSFPA_SMLEN*40/' '/                                       11
      CHARACTER NSFPA_SMLEN*12/'R.SMLEN'/                                 11
      INTEGER MSFPA_SMLEN(2)/1,1/                                         11
      REAL RE_SFPA_SMLEN                                                  11
C -                                                                       11
      REAL SFPA_SSLEN                                                     11
      EQUIVALENCE (BANK_SFPA(15),SFPA_SSLEN,RE_SFPA_SSLEN),               11
     *(NAMES_SFPA(13),NSFPA_SSLEN)                                        11
      EQUIVALENCE (COMMEN_SFPA(13),CSFPA_SSLEN),(MAP_SFPA(1,13),          11
     *MSFPA_SSLEN)                                                        11
      CHARACTER CSFPA_SSLEN*40/' '/                                       11
      CHARACTER NSFPA_SSLEN*12/'R.SSLEN'/                                 11
      INTEGER MSFPA_SSLEN(2)/1,1/                                         11
      REAL RE_SFPA_SSLEN                                                  11
C -                                                                       11
      REAL SFPA_WPLEN                                                     11
      EQUIVALENCE (BANK_SFPA(16),SFPA_WPLEN,RE_SFPA_WPLEN),               11
     *(NAMES_SFPA(14),NSFPA_WPLEN)                                        11
      EQUIVALENCE (COMMEN_SFPA(14),CSFPA_WPLEN),(MAP_SFPA(1,14),          11
     *MSFPA_WPLEN)                                                        11
      CHARACTER CSFPA_WPLEN*40/' '/                                       11
      CHARACTER NSFPA_WPLEN*12/'R.WPLEN'/                                 11
      INTEGER MSFPA_WPLEN(2)/1,1/                                         11
      REAL RE_SFPA_WPLEN                                                  11
C -                                                                       11
      REAL SFPA_SDLEN                                                     11
      EQUIVALENCE (BANK_SFPA(17),SFPA_SDLEN,RE_SFPA_SDLEN),               11
     *(NAMES_SFPA(15),NSFPA_SDLEN)                                        11
      EQUIVALENCE (COMMEN_SFPA(15),CSFPA_SDLEN),(MAP_SFPA(1,15),          11
     *MSFPA_SDLEN)                                                        11
      CHARACTER CSFPA_SDLEN*40/' '/                                       11
      CHARACTER NSFPA_SDLEN*12/'R.SDLEN'/                                 11
      INTEGER MSFPA_SDLEN(2)/1,1/                                         11
      REAL RE_SFPA_SDLEN                                                  11
C -                                                                       11
      REAL SFPA_TILT                                                      11
      EQUIVALENCE (BANK_SFPA(18),SFPA_TILT,RE_SFPA_TILT),                 11
     *(NAMES_SFPA(16),NSFPA_TILT)                                         11
      EQUIVALENCE (COMMEN_SFPA(16),CSFPA_TILT),(MAP_SFPA(1,16),           11
     *MSFPA_TILT)                                                         11
      CHARACTER CSFPA_TILT*40/' '/                                        11
      CHARACTER NSFPA_TILT*12/'R.TILT'/                                   11
      INTEGER MSFPA_TILT(2)/1,1/                                          11
      REAL RE_SFPA_TILT                                                   11
C -                                                                       11
      REAL SFPA_CPRAD                                                     11
      EQUIVALENCE (BANK_SFPA(19),SFPA_CPRAD,RE_SFPA_CPRAD),               11
     *(NAMES_SFPA(17),NSFPA_CPRAD)                                        11
      EQUIVALENCE (COMMEN_SFPA(17),CSFPA_CPRAD),(MAP_SFPA(1,17),          11
     *MSFPA_CPRAD)                                                        11
      CHARACTER CSFPA_CPRAD*40/' '/                                       11
      CHARACTER NSFPA_CPRAD*12/'R.CPRAD'/                                 11
      INTEGER MSFPA_CPRAD(2)/1,1/                                         11
      REAL RE_SFPA_CPRAD                                                  11
C -                                                                       11
      REAL SFPA_CPRAL                                                     11
      EQUIVALENCE (BANK_SFPA(20),SFPA_CPRAL,RE_SFPA_CPRAL),               11
     *(NAMES_SFPA(18),NSFPA_CPRAL)                                        11
      EQUIVALENCE (COMMEN_SFPA(18),CSFPA_CPRAL),(MAP_SFPA(1,18),          11
     *MSFPA_CPRAL)                                                        11
      CHARACTER CSFPA_CPRAL*40/' '/                                       11
      CHARACTER NSFPA_CPRAL*12/'R.CPRAL'/                                 11
      INTEGER MSFPA_CPRAL(2)/1,1/                                         11
      REAL RE_SFPA_CPRAL                                                  11
C -                                                                       11
      REAL SFPA_CFRAD                                                     11
      EQUIVALENCE (BANK_SFPA(21),SFPA_CFRAD,RE_SFPA_CFRAD),               11
     *(NAMES_SFPA(19),NSFPA_CFRAD)                                        11
      EQUIVALENCE (COMMEN_SFPA(19),CSFPA_CFRAD),(MAP_SFPA(1,19),          11
     *MSFPA_CFRAD)                                                        11
      CHARACTER CSFPA_CFRAD*40/' '/                                       11
      CHARACTER NSFPA_CFRAD*12/'R.CFRAD'/                                 11
      INTEGER MSFPA_CFRAD(2)/1,1/                                         11
      REAL RE_SFPA_CFRAD                                                  11
C -                                                                       11
      REAL SFPA_GPTHK                                                     11
      EQUIVALENCE (BANK_SFPA(22),SFPA_GPTHK,RE_SFPA_GPTHK),               11
     *(NAMES_SFPA(20),NSFPA_GPTHK)                                        11
      EQUIVALENCE (COMMEN_SFPA(20),CSFPA_GPTHK),(MAP_SFPA(1,20),          11
     *MSFPA_GPTHK)                                                        11
      CHARACTER CSFPA_GPTHK*40/' '/                                       11
      CHARACTER NSFPA_GPTHK*12/'R.GPTHK'/                                 11
      INTEGER MSFPA_GPTHK(2)/1,1/                                         11
      REAL RE_SFPA_GPTHK                                                  11
C -                                                                       11
      INTEGER SFPA_LADDERMAP(20)                                          11
      EQUIVALENCE (BANK_SFPA(23),SFPA_LADDERMAP,RE_SFPA_LADDERMAP),       11
     *(NAMES_SFPA(21),NSFPA_LADDERMAP)                                    11
      EQUIVALENCE (COMMEN_SFPA(21),CSFPA_LADDERMAP),(MAP_SFPA(1,21),      11
     *MSFPA_LADDERMAP)                                                    11
      CHARACTER CSFPA_LADDERMAP*40/' '/                                   11
      CHARACTER NSFPA_LADDERMAP*12/'I.LADDERMAP'/                         11
      INTEGER MSFPA_LADDERMAP(2)/20,1/                                    11
      REAL RE_SFPA_LADDERMAP                                              11
C -                                                                       11
      REAL SFPA_LADDERANGLE(20)                                           11
      EQUIVALENCE (BANK_SFPA(43),SFPA_LADDERANGLE,RE_SFPA_LADDERANGLE),   11
     *(NAMES_SFPA(22),NSFPA_LADDERANGLE)                                  11
      EQUIVALENCE (COMMEN_SFPA(22),CSFPA_LADDERANGLE),(MAP_SFPA(1,22),    11
     *MSFPA_LADDERANGLE)                                                  11
      CHARACTER CSFPA_LADDERANGLE*40/' '/                                 11
      CHARACTER NSFPA_LADDERANGLE*12/'R.LADDERANGLE'/                     11
      INTEGER MSFPA_LADDERANGLE(2)/20,1/                                  11
      REAL RE_SFPA_LADDERANGLE                                            11
C -                                                                       11
      REAL SFPA_LADDERTILT(20)                                            11
      EQUIVALENCE (BANK_SFPA(63),SFPA_LADDERTILT,RE_SFPA_LADDERTILT),     11
     *(NAMES_SFPA(23),NSFPA_LADDERTILT)                                   11
      EQUIVALENCE (COMMEN_SFPA(23),CSFPA_LADDERTILT),(MAP_SFPA(1,23),     11
     *MSFPA_LADDERTILT)                                                   11
      CHARACTER CSFPA_LADDERTILT*40/' '/                                  11
      CHARACTER NSFPA_LADDERTILT*12/'R.LADDERTILT'/                       11
      INTEGER MSFPA_LADDERTILT(2)/20,1/                                   11
      REAL RE_SFPA_LADDERTILT                                             11
C -                                                                       11
      REAL SFPA_LADDERRADIUS(20)                                          11
      EQUIVALENCE (BANK_SFPA(83),SFPA_LADDERRADIUS,                       11
     *RE_SFPA_LADDERRADIUS),(NAMES_SFPA(24),NSFPA_LADDERRADIUS)           11
      EQUIVALENCE (COMMEN_SFPA(24),CSFPA_LADDERRADIUS),(MAP_SFPA(1,24),   11
     *MSFPA_LADDERRADIUS)                                                 11
      CHARACTER CSFPA_LADDERRADIUS*40/' '/                                11
      CHARACTER NSFPA_LADDERRADIUS*12/'R.LADDERRADIUS'/                   11
      INTEGER MSFPA_LADDERRADIUS(2)/20,1/                                 11
      REAL RE_SFPA_LADDERRADIUS                                           11
C -                                                                       11
      INTEGER LEN_SFPA,LLEN_SFPA,LINK_SFPA                                11
      PARAMETER (LEN_SFPA=102)                                            11
      PARAMETER (LLEN_SFPA=24)                                            11
      INTEGER FLAG_SFPA/0/,MAP_SFPA(2,LLEN_SFPA),IDNUM_SFPA(10)           11
      CHARACTER NAMES_SFPA(LLEN_SFPA)*12,COMMEN_SFPA(LLEN_SFPA)*40        11
      DATA IDNUM_SFPA/10*0/                                               11
      REAL BANK_SFPA(LEN_SFPA)                                            11
      CHARACTER*60 BPATH_SFPA/' '/                                        11
      SAVE FLAG_SFPA,MAP_SFPA,IDNUM_SFPA,NAMES_SFPA,COMMEN_SFPA,          11
     *BPATH_SFPA                                                          11
      DATA LINK_SFPA/0/,BANK_SFPA/LEN_SFPA*0/                             11
C *                                                                       11
      INTEGER ILAD,IWAF,NC                                                12
      REAL WAFPCKLEN,DTHK,RADTILT,ANG                                     14
C *                                                                       16
      CALL AGSBEGM(MODULE,IPRIN)                                          16
      IF (IPRIN.LT.0) RETURN                                              16
C Filling bank SSDP - SILICON STRIPS                                      16
      BANK_TITLE='SILICON STRIPS'                                         16
      BPATH_SSDP='SSDP*'                                                  16
      IDNUM_SSDP(1)=0                                                     17
C VERSION                                                                 17
      SSDP_VERSION=1                                                      17
      CSSDP_VERSION='VERSION'                                             17
C THERE ARE A FEW CONFIGURAIONS POSSIBLE                                  18
      SSDP_CONFIG=1                                                       18
      CSSDP_CONFIG='THERE ARE A FEW CONFIGURAIONS POSSIBLE'               18
C 0=CAVE, 1=SVTT                                                          19
      SSDP_PLACEMENT=0                                                    19
      CSSDP_PLACEMENT='0=CAVE, 1=SVTT'                                    19
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSDP',    20
     *BPATH_SSDP,IDNUM_SSDP,1, MAP_SSDP,NAMES_SSDP,COMMEN_SSDP,           20
     *BANK_SSDP,LEN_SSDP,LLEN_SSDP,LINK_SSDP,FLAG_SSDP)                   20
C *                                                                       20
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSDP',    20
     *BPATH_SSDP,IDNUM_SSDP,1, MAP_SSDP,NAMES_SSDP,COMMEN_SSDP,           20
     *BANK_SSDP,LEN_SSDP,LLEN_SSDP,LINK_SSDP,FLAG_SSDP)                   20
C *                                                                       21
      CALL AGSBEGM(MODULE,IPRIN)                                          21
      IF (IPRIN.LT.0) RETURN                                              21
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   21
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      21
      BPATH_SFPA='SFPA*'                                                  21
      IDNUM_SFPA(1)=0                                                     22
C GEOMETRY VERSION                                                        22
      SFPA_VERSION=1                                                      22
      CSFPA_VERSION='GEOMETRY VERSION'                                    22
C MOTHER RMIN                                                             23
      SFPA_RMIN=21.8                                                      23
      CSFPA_RMIN='MOTHER RMIN'                                            23
C MOTHER RMAX                                                             24
      SFPA_RMAX=29.5                                                      24
      CSFPA_RMAX='MOTHER RMAX'                                            24
C MOTHER LEN                                                              25
      SFPA_LEN=100.                                                       25
      CSFPA_LEN='MOTHER LEN'                                              25
C DISTANCE FROM BEAM AXIS TO DETECTOR CENTER                              26
      SFPA_RAD=23.                                                        26
      CSFPA_RAD='DISTANCE FROM BEAM AXIS TO DETECTOR CENTER'              26
C NUMBER OF SILICON STRIP DETECTORS                                       27
      SFPA_NSSD=16                                                        27
      CSFPA_NSSD='NUMBER OF SILICON STRIP DETECTORS'                      27
C DETECTOR MOTHER WIDTH                                                   28
      SFPA_DMWID=7.5                                                      28
      CSFPA_DMWID='DETECTOR MOTHER WIDTH'                                 28
C DETECTOR MOTHER THICKNESS                                               29
      SFPA_DMTHK=0.03                                                     29
      CSFPA_DMTHK='DETECTOR MOTHER THICKNESS'                             29
C DETECTOR MOTHER LENGTH (DETECTORS + ADC BOARD)                          30
      SFPA_DMLEN=90.                                                      30
      CSFPA_DMLEN='DETECTOR MOTHER LENGTH (DETECTORS + ADC BOARD)'        30
C STRUCTURE MOTHER WIDTH                                                  31
      SFPA_SMWID=7.5                                                      31
      CSFPA_SMWID='STRUCTURE MOTHER WIDTH'                                31
C STRUCTURE MOTHER THICKNESS                                              32
      SFPA_SMTHK=3.5                                                      32
      CSFPA_SMTHK='STRUCTURE MOTHER THICKNESS'                            32
C STRUCTURE MOTHER LENGTH (COOL. PIPE+CARBON FIBER)                       33
      SFPA_SMLEN=95.                                                      33
      CSFPA_SMLEN='STRUCTURE MOTHER LENGTH (COOL. PIPE+CARBON FIBER)'     33
C LENGTH OF A SUBVOLUME OF THE STRUCTURE                                  34
      SFPA_SSLEN=95./20.                                                  34
      CSFPA_SSLEN='LENGTH OF A SUBVOLUME OF THE STRUCTURE'                34
C LENGTH OF WAFER PACK                                                    35
      SFPA_WPLEN=68.8                                                     35
      CSFPA_WPLEN='LENGTH OF WAFER PACK'                                  35
C LENGHT OF ONE STRIP DETECTOR (ALONG BEAM AXIS)                          36
      SFPA_SDLEN=4.2                                                      36
      CSFPA_SDLEN='LENGHT OF ONE STRIP DETECTOR (ALONG BEAM AXIS)'        36
C TILING ANGLE (DEGREES)                                                  37
      SFPA_TILT=5.0                                                       37
      CSFPA_TILT='TILING ANGLE (DEGREES)'                                 37
C COOLING PIPE OUTER RADIUS                                               38
      SFPA_CPRAD=0.1                                                      38
      CSFPA_CPRAD='COOLING PIPE OUTER RADIUS'                             38
C COOLING PIPE INNER RADIUS                                               39
      SFPA_CPRAL=0.09                                                     39
      CSFPA_CPRAL='COOLING PIPE INNER RADIUS'                             39
C CARBON FIBER TUBE RADIUS (SUPPORT STRUCTURE)                            40
      SFPA_CFRAD=0.1                                                      40
      CSFPA_CFRAD='CARBON FIBER TUBE RADIUS (SUPPORT STRUCTURE)'          40
C GAP BETWEEN STRUCTURE MOTHER AND DETECTOR                               41
      SFPA_GPTHK=0.5                                                      41
      CSFPA_GPTHK='GAP BETWEEN STRUCTURE MOTHER AND DETECTOR'             41
C PRESENCE OF LADDERS                                                     43
      SFPA_LADDERMAP(1)=0                                                 43
      SFPA_LADDERMAP(2)=0                                                 43
      SFPA_LADDERMAP(3)=1                                                 43
      SFPA_LADDERMAP(4)=0                                                 43
      SFPA_LADDERMAP(5)=0                                                 43
      SFPA_LADDERMAP(6)=0                                                 43
      SFPA_LADDERMAP(7)=0                                                 43
      SFPA_LADDERMAP(8)=0                                                 43
      SFPA_LADDERMAP(9)=0                                                 43
      SFPA_LADDERMAP(10)=0                                                43
      SFPA_LADDERMAP(11)=0                                                43
      SFPA_LADDERMAP(12)=0                                                43
      SFPA_LADDERMAP(13)=0                                                43
      SFPA_LADDERMAP(14)=0                                                43
      SFPA_LADDERMAP(15)=0                                                43
      SFPA_LADDERMAP(16)=0                                                43
      SFPA_LADDERMAP(17)=0                                                43
      SFPA_LADDERMAP(18)=0                                                43
      SFPA_LADDERMAP(19)=0                                                43
      SFPA_LADDERMAP(20)=0                                                43
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               43
C INDIVIDUAL ANGLES                                                       47
      SFPA_LADDERANGLE(1)=-1.0                                            47
      SFPA_LADDERANGLE(2)=-1.0                                            47
      SFPA_LADDERANGLE(3)=45.0                                            47
      SFPA_LADDERANGLE(4)=-1.0                                            47
      SFPA_LADDERANGLE(5)=-1.0                                            47
      SFPA_LADDERANGLE(6)=-1.0                                            47
      SFPA_LADDERANGLE(7)=-1.0                                            47
      SFPA_LADDERANGLE(8)=-1.0                                            47
      SFPA_LADDERANGLE(9)=-1.0                                            47
      SFPA_LADDERANGLE(10)=-1.0                                           47
      SFPA_LADDERANGLE(11)=-1.0                                           47
      SFPA_LADDERANGLE(12)=-1.0                                           47
      SFPA_LADDERANGLE(13)=-1.0                                           47
      SFPA_LADDERANGLE(14)=-1.0                                           47
      SFPA_LADDERANGLE(15)=-1.0                                           47
      SFPA_LADDERANGLE(16)=-1.0                                           47
      SFPA_LADDERANGLE(17)=-1.0                                           47
      SFPA_LADDERANGLE(18)=-1.0                                           47
      SFPA_LADDERANGLE(19)=-1.0                                           47
      SFPA_LADDERANGLE(20)=-1.0                                           47
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               47
C INDIVIDUAL TILTS                                                        51
      SFPA_LADDERTILT(1)=-1.0                                             51
      SFPA_LADDERTILT(2)=-1.0                                             51
      SFPA_LADDERTILT(3)=0.0                                              51
      SFPA_LADDERTILT(4)=-1.0                                             51
      SFPA_LADDERTILT(5)=-1.0                                             51
      SFPA_LADDERTILT(6)=-1.0                                             51
      SFPA_LADDERTILT(7)=-1.0                                             51
      SFPA_LADDERTILT(8)=-1.0                                             51
      SFPA_LADDERTILT(9)=-1.0                                             51
      SFPA_LADDERTILT(10)=-1.0                                            51
      SFPA_LADDERTILT(11)=-1.0                                            51
      SFPA_LADDERTILT(12)=-1.0                                            51
      SFPA_LADDERTILT(13)=-1.0                                            51
      SFPA_LADDERTILT(14)=-1.0                                            51
      SFPA_LADDERTILT(15)=-1.0                                            51
      SFPA_LADDERTILT(16)=-1.0                                            51
      SFPA_LADDERTILT(17)=-1.0                                            51
      SFPA_LADDERTILT(18)=-1.0                                            51
      SFPA_LADDERTILT(19)=-1.0                                            51
      SFPA_LADDERTILT(20)=-1.0                                            51
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 51
C INDIVIDUAL RADII                                                        56
      SFPA_LADDERRADIUS(1)=-1.0                                           56
      SFPA_LADDERRADIUS(2)=-1.0                                           56
      SFPA_LADDERRADIUS(3)=23.000                                         56
      SFPA_LADDERRADIUS(4)=-1.0                                           56
      SFPA_LADDERRADIUS(5)=-1.0                                           56
      SFPA_LADDERRADIUS(6)=-1.0                                           56
      SFPA_LADDERRADIUS(7)=-1.0                                           56
      SFPA_LADDERRADIUS(8)=-1.0                                           56
      SFPA_LADDERRADIUS(9)=-1.0                                           56
      SFPA_LADDERRADIUS(10)=-1.0                                          56
      SFPA_LADDERRADIUS(11)=-1.0                                          56
      SFPA_LADDERRADIUS(12)=-1.0                                          56
      SFPA_LADDERRADIUS(13)=-1.0                                          56
      SFPA_LADDERRADIUS(14)=-1.0                                          56
      SFPA_LADDERRADIUS(15)=-1.0                                          56
      SFPA_LADDERRADIUS(16)=-1.0                                          56
      SFPA_LADDERRADIUS(17)=-1.0                                          56
      SFPA_LADDERRADIUS(18)=-1.0                                          56
      SFPA_LADDERRADIUS(19)=-1.0                                          56
      SFPA_LADDERRADIUS(20)=-1.0                                          56
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               56
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    57
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           57
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   57
C *                                                                       57
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    57
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           57
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   57
C *                                                                       58
      CALL AGSBEGM(MODULE,IPRIN)                                          58
      IF (IPRIN.LT.0) RETURN                                              58
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   58
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      58
      BPATH_SFPA='SFPA*'                                                  58
      IDNUM_SFPA(1)=0                                                     59
C GEOMETRY VERSION                                                        59
      SFPA_VERSION=2                                                      59
      CSFPA_VERSION='GEOMETRY VERSION'                                    59
C PRESENCE OF LADDERS                                                     61
      SFPA_LADDERMAP(1)=1                                                 61
      SFPA_LADDERMAP(2)=1                                                 61
      SFPA_LADDERMAP(3)=1                                                 61
      SFPA_LADDERMAP(4)=0                                                 61
      SFPA_LADDERMAP(5)=0                                                 61
      SFPA_LADDERMAP(6)=0                                                 61
      SFPA_LADDERMAP(7)=0                                                 61
      SFPA_LADDERMAP(8)=0                                                 61
      SFPA_LADDERMAP(9)=1                                                 61
      SFPA_LADDERMAP(10)=1                                                61
      SFPA_LADDERMAP(11)=1                                                61
      SFPA_LADDERMAP(12)=1                                                61
      SFPA_LADDERMAP(13)=1                                                61
      SFPA_LADDERMAP(14)=0                                                61
      SFPA_LADDERMAP(15)=0                                                61
      SFPA_LADDERMAP(16)=0                                                61
      SFPA_LADDERMAP(17)=0                                                61
      SFPA_LADDERMAP(18)=0                                                61
      SFPA_LADDERMAP(19)=1                                                61
      SFPA_LADDERMAP(20)=1                                                61
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               61
C INDIVIDUAL ANGLES                                                       65
      SFPA_LADDERANGLE(1)=90.0                                            65
      SFPA_LADDERANGLE(2)=108.3                                           65
      SFPA_LADDERANGLE(3)=130.0                                           65
      SFPA_LADDERANGLE(4)=-1.0                                            65
      SFPA_LADDERANGLE(5)=-1.0                                            65
      SFPA_LADDERANGLE(6)=-1.0                                            65
      SFPA_LADDERANGLE(7)=-1.0                                            65
      SFPA_LADDERANGLE(8)=-1.0                                            65
      SFPA_LADDERANGLE(9)=230.0                                           65
      SFPA_LADDERANGLE(10)=251.7                                          65
      SFPA_LADDERANGLE(11)=270.0                                          65
      SFPA_LADDERANGLE(12)=288.3                                          65
      SFPA_LADDERANGLE(13)=310.0                                          65
      SFPA_LADDERANGLE(14)=-1.0                                           65
      SFPA_LADDERANGLE(15)=-1.0                                           65
      SFPA_LADDERANGLE(16)=-1.0                                           65
      SFPA_LADDERANGLE(17)=-1.0                                           65
      SFPA_LADDERANGLE(18)=-1.0                                           65
      SFPA_LADDERANGLE(19)=50.0                                           65
      SFPA_LADDERANGLE(20)=71.7                                           65
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               65
C INDIVIDUAL TILTS                                                        69
      SFPA_LADDERTILT(1)=0.0                                              69
      SFPA_LADDERTILT(2)=-6.0                                             69
      SFPA_LADDERTILT(3)=0.0                                              69
      SFPA_LADDERTILT(4)=-1.0                                             69
      SFPA_LADDERTILT(5)=-1.0                                             69
      SFPA_LADDERTILT(6)=-1.0                                             69
      SFPA_LADDERTILT(7)=-1.0                                             69
      SFPA_LADDERTILT(8)=0.0                                              69
      SFPA_LADDERTILT(9)=0.0                                              69
      SFPA_LADDERTILT(10)=6.0                                             69
      SFPA_LADDERTILT(11)=0.0                                             69
      SFPA_LADDERTILT(12)=-6.0                                            69
      SFPA_LADDERTILT(13)=0.0                                             69
      SFPA_LADDERTILT(14)=-1.0                                            69
      SFPA_LADDERTILT(15)=-1.0                                            69
      SFPA_LADDERTILT(16)=-1.0                                            69
      SFPA_LADDERTILT(17)=-1.0                                            69
      SFPA_LADDERTILT(18)=-1.0                                            69
      SFPA_LADDERTILT(19)=0.0                                             69
      SFPA_LADDERTILT(20)=6.0                                             69
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 69
C INDIVIDUAL RADII                                                        74
      SFPA_LADDERRADIUS(1)=23.174                                         74
      SFPA_LADDERRADIUS(2)=22.800                                         74
      SFPA_LADDERRADIUS(3)=24.600                                         74
      SFPA_LADDERRADIUS(4)=-1.0                                           74
      SFPA_LADDERRADIUS(5)=-1.0                                           74
      SFPA_LADDERRADIUS(6)=-1.0                                           74
      SFPA_LADDERRADIUS(7)=-1.0                                           74
      SFPA_LADDERRADIUS(8)=-1.0                                           74
      SFPA_LADDERRADIUS(9)=24.600                                         74
      SFPA_LADDERRADIUS(10)=22.800                                        74
      SFPA_LADDERRADIUS(11)=23.174                                        74
      SFPA_LADDERRADIUS(12)=22.800                                        74
      SFPA_LADDERRADIUS(13)=24.600                                        74
      SFPA_LADDERRADIUS(14)=-1.0                                          74
      SFPA_LADDERRADIUS(15)=-1.0                                          74
      SFPA_LADDERRADIUS(16)=-1.0                                          74
      SFPA_LADDERRADIUS(17)=-1.0                                          74
      SFPA_LADDERRADIUS(18)=-1.0                                          74
      SFPA_LADDERRADIUS(19)=24.600                                        74
      SFPA_LADDERRADIUS(20)=22.800                                        74
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               74
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    75
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           75
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   75
C *                                                                       75
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    75
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           75
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   75
C *                                                                       76
      CALL AGSBEGM(MODULE,IPRIN)                                          76
      IF (IPRIN.LT.0) RETURN                                              76
C Filling bank SFPA - SILICON STRIP DETECTOR PARAMETERS                   76
      BANK_TITLE='SILICON STRIP DETECTOR PARAMETERS'                      76
      BPATH_SFPA='SFPA*'                                                  76
      IDNUM_SFPA(1)=0                                                     77
C GEOMETRY VERSION                                                        77
      SFPA_VERSION=3                                                      77
      CSFPA_VERSION='GEOMETRY VERSION'                                    77
C PRESENCE OF LADDERS                                                     79
      SFPA_LADDERMAP(1)=1                                                 79
      SFPA_LADDERMAP(2)=1                                                 79
      SFPA_LADDERMAP(3)=1                                                 79
      SFPA_LADDERMAP(4)=1                                                 79
      SFPA_LADDERMAP(5)=1                                                 79
      SFPA_LADDERMAP(6)=1                                                 79
      SFPA_LADDERMAP(7)=1                                                 79
      SFPA_LADDERMAP(8)=1                                                 79
      SFPA_LADDERMAP(9)=1                                                 79
      SFPA_LADDERMAP(10)=1                                                79
      SFPA_LADDERMAP(11)=1                                                79
      SFPA_LADDERMAP(12)=1                                                79
      SFPA_LADDERMAP(13)=1                                                79
      SFPA_LADDERMAP(14)=1                                                79
      SFPA_LADDERMAP(15)=1                                                79
      SFPA_LADDERMAP(16)=1                                                79
      SFPA_LADDERMAP(17)=1                                                79
      SFPA_LADDERMAP(18)=1                                                79
      SFPA_LADDERMAP(19)=1                                                79
      SFPA_LADDERMAP(20)=1                                                79
      CSFPA_LADDERMAP='PRESENCE OF LADDERS'                               79
C INDIVIDUAL ANGLES                                                       83
      SFPA_LADDERANGLE(1)=90.0                                            83
      SFPA_LADDERANGLE(2)=108.3                                           83
      SFPA_LADDERANGLE(3)=126.6                                           83
      SFPA_LADDERANGLE(4)=144.4                                           83
      SFPA_LADDERANGLE(5)=162.2                                           83
      SFPA_LADDERANGLE(6)=180.0                                           83
      SFPA_LADDERANGLE(7)=197.8                                           83
      SFPA_LADDERANGLE(8)=215.6                                           83
      SFPA_LADDERANGLE(9)=233.4                                           83
      SFPA_LADDERANGLE(10)=251.7                                          83
      SFPA_LADDERANGLE(11)=270.0                                          83
      SFPA_LADDERANGLE(12)=288.3                                          83
      SFPA_LADDERANGLE(13)=306.6                                          83
      SFPA_LADDERANGLE(14)=324.4                                          83
      SFPA_LADDERANGLE(15)=342.2                                          83
      SFPA_LADDERANGLE(16)=0.0                                            83
      SFPA_LADDERANGLE(17)=17.8                                           83
      SFPA_LADDERANGLE(18)=35.6                                           83
      SFPA_LADDERANGLE(19)=53.4                                           83
      SFPA_LADDERANGLE(20)=71.7                                           83
      CSFPA_LADDERANGLE='INDIVIDUAL ANGLES'                               83
C INDIVIDUAL TILTS                                                        87
      SFPA_LADDERTILT(1)=0.0                                              87
      SFPA_LADDERTILT(2)=-6.0                                             87
      SFPA_LADDERTILT(3)=-7.0                                             87
      SFPA_LADDERTILT(4)=-7.0                                             87
      SFPA_LADDERTILT(5)=-7.0                                             87
      SFPA_LADDERTILT(6)=0.0                                              87
      SFPA_LADDERTILT(7)=7.0                                              87
      SFPA_LADDERTILT(8)=7.0                                              87
      SFPA_LADDERTILT(9)=7.0                                              87
      SFPA_LADDERTILT(10)=6.0                                             87
      SFPA_LADDERTILT(11)=0.0                                             87
      SFPA_LADDERTILT(12)=-6.0                                            87
      SFPA_LADDERTILT(13)=-7.0                                            87
      SFPA_LADDERTILT(14)=-7.0                                            87
      SFPA_LADDERTILT(15)=-7.0                                            87
      SFPA_LADDERTILT(16)=0.0                                             87
      SFPA_LADDERTILT(17)=7.0                                             87
      SFPA_LADDERTILT(18)=7.0                                             87
      SFPA_LADDERTILT(19)=7.0                                             87
      SFPA_LADDERTILT(20)=6.0                                             87
      CSFPA_LADDERTILT='INDIVIDUAL TILTS'                                 87
C INDIVIDUAL RADII                                                        92
      SFPA_LADDERRADIUS(1)=23.177                                         92
      SFPA_LADDERRADIUS(2)=22.800                                         92
      SFPA_LADDERRADIUS(3)=22.800                                         92
      SFPA_LADDERRADIUS(4)=22.800                                         92
      SFPA_LADDERRADIUS(5)=23.800                                         92
      SFPA_LADDERRADIUS(6)=22.500                                         92
      SFPA_LADDERRADIUS(7)=23.800                                         92
      SFPA_LADDERRADIUS(8)=22.800                                         92
      SFPA_LADDERRADIUS(9)=22.800                                         92
      SFPA_LADDERRADIUS(10)=22.800                                        92
      SFPA_LADDERRADIUS(11)=23.177                                        92
      SFPA_LADDERRADIUS(12)=22.800                                        92
      SFPA_LADDERRADIUS(13)=22.800                                        92
      SFPA_LADDERRADIUS(14)=22.800                                        92
      SFPA_LADDERRADIUS(15)=23.800                                        92
      SFPA_LADDERRADIUS(16)=22.500                                        92
      SFPA_LADDERRADIUS(17)=23.800                                        92
      SFPA_LADDERRADIUS(18)=22.800                                        92
      SFPA_LADDERRADIUS(19)=22.800                                        92
      SFPA_LADDERRADIUS(20)=22.800                                        92
      CSFPA_LADDERRADIUS='INDIVIDUAL RADII'                               92
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    93
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           93
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   93
C *                                                                       93
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFPA',    93
     *BPATH_SFPA,IDNUM_SFPA,1, MAP_SFPA,NAMES_SFPA,COMMEN_SFPA,           93
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   93
C use default SSDP bank                                                   94
      BPATH_SSDP='SSDP*'                                                  94
      IDNUM_SSDP(1)=0                                                     94
      CALL AGDGETP('SISDGEO','*',-999,'SSDP',' ',BANK_SSDP, BPATH_SSDP,   94
     *IDNUM_SSDP,1,MAP_SSDP,NAMES_SSDP,BANK_SSDP,LEN_SSDP,LLEN_SSDP,      94
     *LINK_SSDP,FLAG_SSDP)                                                94
C use SFPA bank                                                           95
      BPATH_SFPA='SFPA*'                                                  95
      IDNUM_SFPA(1)=0                                                     95
      SFPA_VERSION=SSDP_CONFIG                                            95
      CALL AGDGETP('SISDGEO','*',-999,'SFPA',NSFPA_VERSION,               95
     *RE_SFPA_VERSION,BPATH_SFPA,IDNUM_SFPA,1,MAP_SFPA,NAMES_SFPA,        95
     *BANK_SFPA,LEN_SFPA,LLEN_SFPA,LINK_SFPA,FLAG_SFPA)                   95
C Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                              96
      CALL AGSRESET                                                       96
      AG_A = 1                                                            96
      AG_Z = 1                                                            96
      AG_W = 2                                                            96
      AG_TITLE = 'H2'                                                     96
      AG_EXNAME = 'COMPONENT'                                             96
      AG_PARLIST = 'A_Z_W'                                                96
      CALL AXCOMPONENT                                                    96
C *                                                                       96
C Geant COMPONENT : O ===> A=16 Z=8 W=1 <===                              96
      CALL AGSRESET                                                       97
      AG_A = 16                                                           97
      AG_Z = 8                                                            97
      AG_W = 1                                                            97
      AG_TITLE = 'O'                                                      97
      AG_EXNAME = 'COMPONENT'                                             97
      AG_PARLIST = 'A_Z_W'                                                97
      CALL AXCOMPONENT                                                    97
C *                                                                       97
C Geant MIXTURE : WATER ===> DENS=1.0 <===                                97
      CALL AGSRESET                                                       98
      AG_DENS = 1.0                                                       98
      AG_TITLE = 'WATER'                                                  98
      AG_EXNAME = 'MIXTURE'                                               98
      AG_PARLIST = 'DENS'                                                 98
      CALL AXMIXTURE                                                      98
C *                                                                       98
C Check SSDP_PLACEMENT==1                                                 98
      IF (SSDP_PLACEMENT.EQ.1) THEN                                       99
      CALL AGSBEGM(MODULE,IPRIN)                                          100
C Check IPRIN<0                                                           100
      IF (IPRIN.LT.0) RETURN                                              100
C Create SFMO :                                                           100
C Call To Local Procedure SFMO                                            100
      ASSIGN 5020 TO BLOCK_SFMO                                           100
      GO TO  5010                                                         100
5020  CONTINUE                                                            100
C *                                                                       100
C Geant POSITION : SFMO ===> MOTHER='SVTT' <===                           101
      CALL AGSRESET                                                       101
      AG_MOTHER = 'SVTT'                                                  101
      AG_TITLE = 'SFMO'                                                   101
      AG_EXNAME = 'POSITION'                                              101
      AG_PARLIST = 'MOTHER'                                               101
      CALL AXPOSITION                                                     101
C *                                                                       101
      ELSE                                                                101
      CALL AGSBEGM(MODULE,IPRIN)                                          103
C Check IPRIN<0                                                           103
      IF (IPRIN.LT.0) RETURN                                              103
C Create SFMO :                                                           103
C Call To Local Procedure SFMO                                            103
      ASSIGN 5030 TO BLOCK_SFMO                                           103
      GO TO  5010                                                         103
5030  CONTINUE                                                            103
C *                                                                       103
C Geant POSITION : SFMO ===> MOTHER='CAVE' <===                           104
      CALL AGSRESET                                                       104
      AG_MOTHER = 'CAVE'                                                  104
      AG_TITLE = 'SFMO'                                                   104
      AG_EXNAME = 'POSITION'                                              104
      AG_PARLIST = 'MOTHER'                                               104
      CALL AXPOSITION                                                     104
C *                                                                       104
      ENDIF                                                               104
C ----------------------------------------------------------------------  106
C ------------ SFMO                                                       106
      CALL AGSENDM                                                        107
      RETURN                                                              107
C Begin Local Procedure SFMO                                              107
C SFMO IS THE MOTHER OF ALL SILICON STRIP DETECTOR VOLUMES                107
5010  CONTINUE                                                            107
         AG_TITLE='SFMO'                                                  107
         AG_MODULE=MODULE                                                 107
         AG_MARK='V00'                                                    107
         CALL AGSPUSH                                                     107
         AG_MODULE=MODULE                                                 107
C    *                                                                    107
C    Geant MATERIAL : AIR ===> inherited arguments used <===              108
         CALL AGSRESET                                                    108
         AG_TITLE = 'AIR'                                                 108
         AG_EXNAME = 'MATERIAL'                                           108
         AG_PARLIST = 'NONE'                                              108
         CALL AXMATERIAL                                                  108
C    *                                                                    108
C    Geant ATTRIBUTE : SISD ===> SEEN=1 COLO=1 <===                       108
         CALL AGSRESET                                                    109
         AG_SEEN = 1                                                      109
         AG_COLO = 1                                                      109
         AG_TITLE = 'SISD'                                                109
         AG_EXNAME = 'ATTRIBUTE'                                          109
         AG_PARLIST = 'SEEN_COLO'                                         109
         CALL AXATTRIBUTE                                                 109
C    *                                                                    109
C    Geant SHAPE : TUBE ===> RMIN=SFPA_RMIN RMAX=SFPA_RMAX, DZ=SFPA_LEN/  109
C    2 <===                                                               109
         CALL AGSRESET                                                    111
         AG_RMIN = SFPA_RMIN                                              111
         AG_RMAX = SFPA_RMAX                                              111
         AG_DZ = SFPA_LEN/2                                               111
         AG_TITLE = 'TUBE'                                                111
         AG_EXNAME = 'SHAPE'                                              111
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      111
         CALL AXSHAPE                                                     111
C    *                                                                    111
         DTHK=SFPA_SMTHK + SFPA_GPTHK                                     111
C    Create SFLM :                                                        113
C    Call To Local Procedure SFLM                                         113
         ASSIGN 5050 TO BLOCK_SFLM                                        113
         GO TO  5040                                                      113
5050     CONTINUE                                                         113
C    *                                                                    113
C    *                                                                    114
         ILAD=1                                                           114
            GO TO 5063                                                    114
5061        ILAD=ILAD+1                                                   114
5063        IF(ILAD-(20).GT.0) GO TO 5062                                 114
C       Check SFPA_LADDERMAP(ILAD).GT.0                                   115
            IF (SFPA_LADDERMAP(ILAD).GT.0) THEN                           115
            ANG = (SFPA_LADDERANGLE(ILAD)*PI)/180.0                       116
            RADTILT = (SFPA_LADDERTILT(ILAD) *PI)/180.0                   117
C       Check ILAD.EQ.1                                                   118
            IF (ILAD.EQ.1) THEN                                           118
            NC=1                                                          119
            ELSE                                                          120
            NC=20-ILAD+2                                                  121
            ENDIF                                                         122
C       Geant POSITION : SFLM ===> X=(SFPA_LADDERRADIUS(ILAD)*COS(ANG)+(  125
C       DTHK*COS(ANG+RADTILT))/2.0) Y=(SFPA_LADDERRADIUS(ILAD)*SIN(ANG)+  125
C       (DTHK*SIN(ANG+RADTILT))/2.0) Z=0 ALPHAZ=SFPA_LADDERANGLE(ILAD)-9  125
C       0.0+SFPA_LADDERTILT(ILAD) NCOPY=NC KONLY='MANY' <===              125
            CALL AGSRESET                                                 125
            AG_X =                                                        125
     *      (SFPA_LADDERRADIUS(ILAD)*COS(ANG)+(DTHK*COS(ANG+RADTILT))/    125
     *      2.0)                                                          125
            AG_Y =                                                        125
     *      (SFPA_LADDERRADIUS(ILAD)*SIN(ANG)+(DTHK*SIN(ANG+RADTILT))/    125
     *      2.0)                                                          125
            AG_Z = 0                                                      125
            AG_ALPHAN=AG_ALPHAN+1                                         125
            AG_ALPHAK(AG_ALPHAN)=3                                        125
            AG_ALPHAL(AG_ALPHAN)=                                         125
     *      SFPA_LADDERANGLE(ILAD)-90.0+SFPA_LADDERTILT(ILAD)             125
            AG_NCOPY = NC                                                 125
            AG_KONLY = 'MANY'                                             125
            AG_TITLE = 'SFLM'                                             125
            AG_EXNAME = 'POSITION'                                        125
            AG_PARLIST = 'X_Y_Z_ALPHAZ_NCOPY_KONLY'                       125
            CALL AXPOSITION                                               125
C       *                                                                 125
            ENDIF                                                         125
         GO TO 5061                                                       127
5062     CONTINUE                                                         127
5011  CALL AGSPOPB                                                        128
      GO TO BLOCK_SFMO, (5010 ,5020,5030)                                 128
C End of Local Procedure                                                  128
C ----------------------------------------------------------------------  129
C ------------ SFLM                                                       129
C Begin Local Procedure SFLM                                              130
C SFLM IS THE MOTHER OF THE LADDER                                        130
5040  CONTINUE                                                            130
         AG_TITLE='SFLM'                                                  130
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
C    Geant ATTRIBUTE : SFLM ===> SEEN=1 COLO=1 <===                       131
         CALL AGSRESET                                                    132
         AG_SEEN = 1                                                      132
         AG_COLO = 1                                                      132
         AG_TITLE = 'SFLM'                                                132
         AG_EXNAME = 'ATTRIBUTE'                                          132
         AG_PARLIST = 'SEEN_COLO'                                         132
         CALL AXATTRIBUTE                                                 132
C    *                                                                    132
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=(SFPA_DMTHK+SFPA_GPTHK+SF  132
C    PA_SMTHK)/2, DZ=SFPA_SMLEN/2 <===                                    132
         CALL AGSRESET                                                    134
         AG_DX = SFPA_DMWID/2                                             134
         AG_DY = (SFPA_DMTHK+SFPA_GPTHK+SFPA_SMTHK)/2                     134
         AG_DZ = SFPA_SMLEN/2                                             134
         AG_TITLE = 'BOX'                                                 134
         AG_EXNAME = 'SHAPE'                                              134
         AG_PARLIST = 'DX_DY_DZ'                                          134
         CALL AXSHAPE                                                     134
C    *                                                                    134
C    Create SFDM :                                                        135
C    Call To Local Procedure SFDM                                         135
         ASSIGN 5080 TO BLOCK_SFDM                                        135
         GO TO  5070                                                      135
5080     CONTINUE                                                         135
C    *                                                                    135
C    Geant POSITION : SFDM ===> Y=-(SFPA_SMTHK+SFPA_GPTHK)/2 <===         136
         CALL AGSRESET                                                    136
         AG_Y = -(SFPA_SMTHK+SFPA_GPTHK)/2                                136
         AG_TITLE = 'SFDM'                                                136
         AG_EXNAME = 'POSITION'                                           136
         AG_PARLIST = 'Y'                                                 136
         CALL AXPOSITION                                                  136
C    *                                                                    136
C    Create SFSM :                                                        137
C    Call To Local Procedure SFSM                                         137
         ASSIGN 5100 TO BLOCK_SFSM                                        137
         GO TO  5090                                                      137
5100     CONTINUE                                                         137
C    *                                                                    137
C    Geant POSITION : SFSM ===> Y=(SFPA_DMTHK+SFPA_GPTHK)/2 <===          138
         CALL AGSRESET                                                    138
         AG_Y = (SFPA_DMTHK+SFPA_GPTHK)/2                                 138
         AG_TITLE = 'SFSM'                                                138
         AG_EXNAME = 'POSITION'                                           138
         AG_PARLIST = 'Y'                                                 138
         CALL AXPOSITION                                                  138
C    *                                                                    138
5041  CALL AGSPOPB                                                        139
      GO TO BLOCK_SFLM, (5040 ,5050)                                      139
C End of Local Procedure                                                  139
C ----------------------------------------------------------------------  140
C ------------ SFDM                                                       140
C Begin Local Procedure SFDM                                              141
C SFDM IS THE MOTHER OF THE DETECTORS                                     141
5070  CONTINUE                                                            141
         AG_TITLE='SFDM'                                                  141
         AG_MODULE=MODULE                                                 141
         AG_MARK='V00'                                                    141
         CALL AGSPUSH                                                     141
         AG_MODULE=MODULE                                                 141
C    *                                                                    141
C    Geant MATERIAL : AIR ===> inherited arguments used <===              142
         CALL AGSRESET                                                    142
         AG_TITLE = 'AIR'                                                 142
         AG_EXNAME = 'MATERIAL'                                           142
         AG_PARLIST = 'NONE'                                              142
         CALL AXMATERIAL                                                  142
C    *                                                                    142
C    Geant ATTRIBUTE : SFDM ===> SEEN=0 COLO=1 <===                       142
         CALL AGSRESET                                                    143
         AG_SEEN = 0                                                      143
         AG_COLO = 1                                                      143
         AG_TITLE = 'SFDM'                                                143
         AG_EXNAME = 'ATTRIBUTE'                                          143
         AG_PARLIST = 'SEEN_COLO'                                         143
         CALL AXATTRIBUTE                                                 143
C    *                                                                    143
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=SFPA_DMTHK/2, DZ=SFPA_WPL  143
C    EN/2 <===                                                            143
         CALL AGSRESET                                                    145
         AG_DX = SFPA_DMWID/2                                             145
         AG_DY = SFPA_DMTHK/2                                             145
         AG_DZ = SFPA_WPLEN/2                                             145
         AG_TITLE = 'BOX'                                                 145
         AG_EXNAME = 'SHAPE'                                              145
         AG_PARLIST = 'DX_DY_DZ'                                          145
         CALL AXSHAPE                                                     145
C    *                                                                    145
         WAFPCKLEN=SFPA_WPLEN/(SFPA_NSSD*1.)                              145
C    *                                                                    147
         IWAF=1                                                           147
            GO TO 5113                                                    147
5111        IWAF=IWAF+1                                                   147
5113        IF(IWAF-(SFPA_NSSD).GT.0) GO TO 5112                          147
C       Create SFSW :                                                     148
C       Call To Local Procedure SFSW                                      148
            ASSIGN 5130 TO BLOCK_SFSW                                     148
            GO TO  5120                                                   148
5130        CONTINUE                                                      148
C       *                                                                 148
C       Geant POSITION : SFSW ===> Z=-(SFPA_WPLEN+WAFPCKLEN)/2+IWAF*WAFP  149
C       CKLEN <===                                                        149
            CALL AGSRESET                                                 149
            AG_Z = -(SFPA_WPLEN+WAFPCKLEN)/2+IWAF*WAFPCKLEN               149
            AG_TITLE = 'SFSW'                                             149
            AG_EXNAME = 'POSITION'                                        149
            AG_PARLIST = 'Z'                                              149
            CALL AXPOSITION                                               149
C       *                                                                 149
         GO TO 5111                                                       150
5112     CONTINUE                                                         150
5071  CALL AGSPOPB                                                        151
      GO TO BLOCK_SFDM, (5070 ,5080)                                      151
C End of Local Procedure                                                  151
C ----------------------------------------------------------------------  152
C ------------ SFSW                                                       152
C Begin Local Procedure SFSW                                              153
C SFSW IS A SINGLE WAFER CONTAINER                                        153
5120  CONTINUE                                                            153
         AG_TITLE='SFSW'                                                  153
         AG_MODULE=MODULE                                                 153
         AG_MARK='V00'                                                    153
         CALL AGSPUSH                                                     153
         AG_MODULE=MODULE                                                 153
C    *                                                                    153
C    Geant ATTRIBUTE : SFSW ===> SEEN=0 COLO=1 <===                       154
         CALL AGSRESET                                                    154
         AG_SEEN = 0                                                      154
         AG_COLO = 1                                                      154
         AG_TITLE = 'SFSW'                                                154
         AG_EXNAME = 'ATTRIBUTE'                                          154
         AG_PARLIST = 'SEEN_COLO'                                         154
         CALL AXATTRIBUTE                                                 154
C    *                                                                    154
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=SFPA_DMTHK/2, DZ=WAFPCKLE  154
C    N/2 <===                                                             154
         CALL AGSRESET                                                    156
         AG_DX = SFPA_DMWID/2                                             156
         AG_DY = SFPA_DMTHK/2                                             156
         AG_DZ = WAFPCKLEN/2                                              156
         AG_TITLE = 'BOX'                                                 156
         AG_EXNAME = 'SHAPE'                                              156
         AG_PARLIST = 'DX_DY_DZ'                                          156
         CALL AXSHAPE                                                     156
C    *                                                                    156
C    Create SFSD :                                                        157
C    Call To Local Procedure SFSD                                         157
         ASSIGN 5150 TO BLOCK_SFSD                                        157
         GO TO  5140                                                      157
5150     CONTINUE                                                         157
C    *                                                                    157
C    Geant POSITION : SFSD ===> inherited arguments used <===             158
         CALL AGSRESET                                                    158
         AG_TITLE = 'SFSD'                                                158
         AG_EXNAME = 'POSITION'                                           158
         AG_PARLIST = 'NONE'                                              158
         CALL AXPOSITION                                                  158
C    *                                                                    158
5121  CALL AGSPOPB                                                        159
      GO TO BLOCK_SFSW, (5120 ,5130)                                      159
C End of Local Procedure                                                  159
C ----------------------------------------------------------------------  160
C ------------ SFSD                                                       160
C Begin Local Procedure SFSD                                              161
C SFSD IS THE STRIP DETECTOR                                              161
5140  CONTINUE                                                            161
         AG_TITLE='SFSD'                                                  161
         AG_MODULE=MODULE                                                 161
         AG_MARK='V00'                                                    161
         CALL AGSPUSH                                                     161
         AG_MODULE=MODULE                                                 161
C    *                                                                    161
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          162
         CALL AGSRESET                                                    162
         AG_TITLE = 'SILICON'                                             162
         AG_EXNAME = 'MATERIAL'                                           162
         AG_PARLIST = 'NONE'                                              162
         CALL AXMATERIAL                                                  162
C    *                                                                    162
C    Geant MATERIAL : SENSITIVE ===> ISVOL=1 <===                         162
         CALL AGSRESET                                                    163
         AG_ISVOL = 1                                                     163
         AG_TITLE = 'SENSITIVE'                                           163
         AG_EXNAME = 'MATERIAL'                                           163
         AG_PARLIST = 'ISVOL'                                             163
         CALL AXMATERIAL                                                  163
C    *                                                                    163
C    Geant ATTRIBUTE : SFSD ===> SEEN=2 COLO=4 <===                       163
         CALL AGSRESET                                                    164
         AG_SEEN = 2                                                      164
         AG_COLO = 4                                                      164
         AG_TITLE = 'SFSD'                                                164
         AG_EXNAME = 'ATTRIBUTE'                                          164
         AG_PARLIST = 'SEEN_COLO'                                         164
         CALL AXATTRIBUTE                                                 164
C    *                                                                    164
C    Geant SHAPE : BOX ===> DX=SFPA_DMWID/2 DY=SFPA_DMTHK/2, DZ=SFPA_SDL  164
C    EN/2 <===                                                            164
         CALL AGSRESET                                                    166
         AG_DX = SFPA_DMWID/2                                             166
         AG_DY = SFPA_DMTHK/2                                             166
         AG_DZ = SFPA_SDLEN/2                                             166
         AG_TITLE = 'BOX'                                                 166
         AG_EXNAME = 'SHAPE'                                              166
         AG_PARLIST = 'DX_DY_DZ'                                          166
         CALL AXSHAPE                                                     166
C    *                                                                    166
         CALL GSTPAR( AG_IMED,'STRA',1. )                                 166
C    Defining Hits: X:0.001:S Y:0.001: Z:0.001: CX:10: CY:10: CZ:10: STE  169
C    P:0.01: SLENG:16:(0,500), TOF:16:(0,1.0E-6) PTOT:16:(0,100.0) ELOSS  169
C    :16:(0,0.001)                                                        169
         AG_TITLE = 'SFSD'                                                169
         AG_EXNAME = 'HITS'                                               169
         AG_TYPE=AG_MAGIC                                                 169
         CALL AGSDETV('SFSD')                                             169
C    *                                                                    169
         AG_PARLIST='X'                                                   169
         AG_TITLE = 'DETH'                                                169
         AG_NBITS = AG_MAGIC                                              169
         AG_BIN=0.001                                                     169
         AG_OPTION='S'                                                    169
         AG_FHMIN = AG_MAGIC                                              169
         AG_FHMAX = AG_MAGIC                                              169
         CALL AGSDETH (0)                                                 169
C    *                                                                    169
         AG_PARLIST='Y'                                                   169
         AG_TITLE = 'DETH'                                                169
         AG_NBITS = AG_MAGIC                                              169
         AG_BIN=0.001                                                     169
         AG_FHMIN=AG_MAGIC                                                169
         AG_FHMAX=AG_MAGIC                                                169
         AG_OPTION='*'                                                    169
         CALL AGSDETH (0)                                                 169
C    *                                                                    169
         AG_PARLIST='Z'                                                   169
         AG_TITLE = 'DETH'                                                169
         AG_NBITS = AG_MAGIC                                              169
         AG_BIN=0.001                                                     169
         AG_FHMIN=AG_MAGIC                                                169
         AG_FHMAX=AG_MAGIC                                                169
         AG_OPTION='*'                                                    169
         CALL AGSDETH (0)                                                 169
C    *                                                                    169
         AG_PARLIST='CX'                                                  169
         AG_TITLE = 'DETH'                                                169
         AG_NBITS = 10                                                    169
         AG_BIN=AG_MAGIC                                                  169
         AG_FHMIN=AG_MAGIC                                                169
         AG_FHMAX=AG_MAGIC                                                169
         AG_OPTION='*'                                                    169
         CALL AGSDETH (0)                                                 169
C    *                                                                    169
         AG_PARLIST='CY'                                                  169
         AG_TITLE = 'DETH'                                                169
         AG_NBITS = 10                                                    169
         AG_BIN=AG_MAGIC                                                  169
         AG_FHMIN=AG_MAGIC                                                169
         AG_FHMAX=AG_MAGIC                                                169
         AG_OPTION='*'                                                    169
         CALL AGSDETH (0)                                                 169
C    *                                                                    169
         AG_PARLIST='CZ'                                                  169
         AG_TITLE = 'DETH'                                                169
         AG_NBITS = 10                                                    169
         AG_BIN=AG_MAGIC                                                  169
         AG_FHMIN=AG_MAGIC                                                169
         AG_FHMAX=AG_MAGIC                                                169
         AG_OPTION='*'                                                    169
         CALL AGSDETH (0)                                                 169
C    *                                                                    169
         AG_PARLIST='STEP'                                                169
         AG_TITLE = 'DETH'                                                169
         AG_NBITS = AG_MAGIC                                              169
         AG_BIN=0.01                                                      169
         AG_FHMIN=AG_MAGIC                                                169
         AG_FHMAX=AG_MAGIC                                                169
         AG_OPTION='*'                                                    169
         CALL AGSDETH (0)                                                 169
C    *                                                                    169
         AG_PARLIST='SLENG'                                               169
         AG_TITLE = 'DETH'                                                169
         AG_NBITS = 16                                                    169
         AG_BIN=AG_MAGIC                                                  169
         AG_OPTION='*'                                                    169
         AG_FHMIN = 0                                                     169
         AG_FHMAX = 500                                                   169
         CALL AGSDETH (0)                                                 169
C    *                                                                    169
         AG_PARLIST='TOF'                                                 169
         AG_TITLE = 'DETH'                                                169
         AG_NBITS = 16                                                    169
         AG_BIN=AG_MAGIC                                                  169
         AG_OPTION='*'                                                    169
         AG_FHMIN = 0                                                     169
         AG_FHMAX = 1.0E-6                                                169
         CALL AGSDETH (0)                                                 169
C    *                                                                    169
         AG_PARLIST='PTOT'                                                169
         AG_TITLE = 'DETH'                                                169
         AG_NBITS = 16                                                    169
         AG_BIN=AG_MAGIC                                                  169
         AG_OPTION='*'                                                    169
         AG_FHMIN = 0                                                     169
         AG_FHMAX = 100.0                                                 169
         CALL AGSDETH (0)                                                 169
C    *                                                                    169
         AG_PARLIST='ELOSS'                                               169
         AG_TITLE = 'DETH'                                                169
         AG_NBITS = 16                                                    169
         AG_BIN=AG_MAGIC                                                  169
         AG_OPTION='*'                                                    169
         AG_FHMIN = 0                                                     169
         AG_FHMAX = 0.001                                                 169
         CALL AGSDETH (0)                                                 169
C    *                                                                    169
         CALL AGSDETU('SFSD')                                             169
C    *                                                                    169
5141  CALL AGSPOPB                                                        170
      GO TO BLOCK_SFSD, (5140 ,5150)                                      170
C End of Local Procedure                                                  170
C ----------------------------------------------------------------------  171
C ------------ SFSM                                                       171
C Begin Local Procedure SFSM                                              172
C SFSM IS THE MOTHER OF THE LADDER STRUCT.                                172
5090  CONTINUE                                                            172
         AG_TITLE='SFSM'                                                  172
         AG_MODULE=MODULE                                                 172
         AG_MARK='V00'                                                    172
         CALL AGSPUSH                                                     172
         AG_MODULE=MODULE                                                 172
C    *                                                                    172
C    Geant MATERIAL : AIR ===> inherited arguments used <===              173
         CALL AGSRESET                                                    173
         AG_TITLE = 'AIR'                                                 173
         AG_EXNAME = 'MATERIAL'                                           173
         AG_PARLIST = 'NONE'                                              173
         CALL AXMATERIAL                                                  173
C    *                                                                    173
C    Geant ATTRIBUTE : SFSM ===> SEEN=0 COLO=1 <===                       173
         CALL AGSRESET                                                    174
         AG_SEEN = 0                                                      174
         AG_COLO = 1                                                      174
         AG_TITLE = 'SFSM'                                                174
         AG_EXNAME = 'ATTRIBUTE'                                          174
         AG_PARLIST = 'SEEN_COLO'                                         174
         CALL AXATTRIBUTE                                                 174
C    *                                                                    174
C    Geant SHAPE : BOX ===> DX=SFPA_SMWID/2 DY=SFPA_SMTHK/2, DZ=SFPA_SML  174
C    EN/2 <===                                                            174
         CALL AGSRESET                                                    176
         AG_DX = SFPA_SMWID/2                                             176
         AG_DY = SFPA_SMTHK/2                                             176
         AG_DZ = SFPA_SMLEN/2                                             176
         AG_TITLE = 'BOX'                                                 176
         AG_EXNAME = 'SHAPE'                                              176
         AG_PARLIST = 'DX_DY_DZ'                                          176
         CALL AXSHAPE                                                     176
C    *                                                                    176
C    Create SFSS :                                                        177
C    Call To Local Procedure SFSS                                         177
         ASSIGN 5170 TO BLOCK_SFSS                                        177
         GO TO  5160                                                      177
5170     CONTINUE                                                         177
C    *                                                                    177
5091  CALL AGSPOPB                                                        178
      GO TO BLOCK_SFSM, (5090 ,5100)                                      178
C End of Local Procedure                                                  178
C ----------------------------------------------------------------------  179
C ------------ SFSS                                                       179
C Begin Local Procedure SFSS                                              180
C SFSS IS THE SUBVOLUME OF THE MOTHER STRUCT.                             180
5160  CONTINUE                                                            180
         AG_TITLE='SFSS'                                                  180
         AG_MODULE=MODULE                                                 180
         AG_MARK='V00'                                                    180
         CALL AGSPUSH                                                     180
         AG_MODULE=MODULE                                                 180
C    *                                                                    180
C    Geant MATERIAL : AIR ===> inherited arguments used <===              181
         CALL AGSRESET                                                    181
         AG_TITLE = 'AIR'                                                 181
         AG_EXNAME = 'MATERIAL'                                           181
         AG_PARLIST = 'NONE'                                              181
         CALL AXMATERIAL                                                  181
C    *                                                                    181
C    Geant ATTRIBUTE : SFSS ===> SEEN=0 COLO=1 <===                       181
         CALL AGSRESET                                                    182
         AG_SEEN = 0                                                      182
         AG_COLO = 1                                                      182
         AG_TITLE = 'SFSS'                                                182
         AG_EXNAME = 'ATTRIBUTE'                                          182
         AG_PARLIST = 'SEEN_COLO'                                         182
         CALL AXATTRIBUTE                                                 182
C    *                                                                    182
C    Geant SHAPE : DIVISION ===> NDIV=20 IAXIS=3 <===                     182
         CALL AGSRESET                                                    183
         AG_NDIV = 20                                                     183
         AG_IAXIS = 3                                                     183
         AG_TITLE = 'DIVISION'                                            183
         AG_EXNAME = 'SHAPE'                                              183
         AG_PARLIST = 'NDIV_IAXIS'                                        183
         CALL AXSHAPE                                                     183
C    *                                                                    183
C    Create SFCP :                                                        184
C    Call To Local Procedure SFCP                                         184
         ASSIGN 5190 TO BLOCK_SFCP                                        184
         GO TO  5180                                                      184
5190     CONTINUE                                                         184
C    *                                                                    184
C    Geant POSITION : SFCP ===> X=SFPA_SMWID/2-5.*SFPA_CPRAD Y=-SFPA_SMT  185
C    HK/2+SFPA_CPRAD <===                                                 185
         CALL AGSRESET                                                    185
         AG_X = SFPA_SMWID/2-5.*SFPA_CPRAD                                185
         AG_Y = -SFPA_SMTHK/2+SFPA_CPRAD                                  185
         AG_TITLE = 'SFCP'                                                185
         AG_EXNAME = 'POSITION'                                           185
         AG_PARLIST = 'X_Y'                                               185
         CALL AXPOSITION                                                  185
C    *                                                                    185
C    Geant POSITION : SFCP ===> X=-SFPA_SMWID/2+5.*SFPA_CPRAD Y=-SFPA_SM  185
C    THK/2+SFPA_CPRAD <===                                                185
         CALL AGSRESET                                                    186
         AG_X = -SFPA_SMWID/2+5.*SFPA_CPRAD                               186
         AG_Y = -SFPA_SMTHK/2+SFPA_CPRAD                                  186
         AG_TITLE = 'SFCP'                                                186
         AG_EXNAME = 'POSITION'                                           186
         AG_PARLIST = 'X_Y'                                               186
         CALL AXPOSITION                                                  186
C    *                                                                    186
C    Create SFCF :                                                        187
C    Call To Local Procedure SFCF                                         187
         ASSIGN 5210 TO BLOCK_SFCF                                        187
         GO TO  5200                                                      187
5210     CONTINUE                                                         187
C    *                                                                    187
C    Geant POSITION : SFCF ===> inherited arguments used <===             188
         CALL AGSRESET                                                    188
         AG_TITLE = 'SFCF'                                                188
         AG_EXNAME = 'POSITION'                                           188
         AG_PARLIST = 'NONE'                                              188
         CALL AXPOSITION                                                  188
C    *                                                                    188
5161  CALL AGSPOPB                                                        189
      GO TO BLOCK_SFSS, (5160 ,5170)                                      189
C End of Local Procedure                                                  189
C ----------------------------------------------------------------------  190
C ------------ SFCP                                                       190
C Begin Local Procedure SFCP                                              191
C SFCP IS THE COOLING PIPE                                                191
5180  CONTINUE                                                            191
         AG_TITLE='SFCP'                                                  191
         AG_MODULE=MODULE                                                 191
         AG_MARK='V00'                                                    191
         CALL AGSPUSH                                                     191
         AG_MODULE=MODULE                                                 191
C    *                                                                    191
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           192
         CALL AGSRESET                                                    192
         AG_TITLE = 'CARBON'                                              192
         AG_EXNAME = 'MATERIAL'                                           192
         AG_PARLIST = 'NONE'                                              192
         CALL AXMATERIAL                                                  192
C    *                                                                    192
C    Geant ATTRIBUTE : SFCP ===> SEEN=1 COLO=6 <===                       192
         CALL AGSRESET                                                    193
         AG_SEEN = 1                                                      193
         AG_COLO = 6                                                      193
         AG_TITLE = 'SFCP'                                                193
         AG_EXNAME = 'ATTRIBUTE'                                          193
         AG_PARLIST = 'SEEN_COLO'                                         193
         CALL AXATTRIBUTE                                                 193
C    *                                                                    193
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=SFPA_CPRAD DZ=SFPA_SSLEN/2 <===  193
         CALL AGSRESET                                                    194
         AG_RMIN = 0                                                      194
         AG_RMAX = SFPA_CPRAD                                             194
         AG_DZ = SFPA_SSLEN/2                                             194
         AG_TITLE = 'TUBE'                                                194
         AG_EXNAME = 'SHAPE'                                              194
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      194
         CALL AXSHAPE                                                     194
C    *                                                                    194
C    Create SFCW :                                                        195
C    Call To Local Procedure SFCW                                         195
         ASSIGN 5230 TO BLOCK_SFCW                                        195
         GO TO  5220                                                      195
5230     CONTINUE                                                         195
C    *                                                                    195
C    Geant POSITION : SFCW ===> inherited arguments used <===             196
         CALL AGSRESET                                                    196
         AG_TITLE = 'SFCW'                                                196
         AG_EXNAME = 'POSITION'                                           196
         AG_PARLIST = 'NONE'                                              196
         CALL AXPOSITION                                                  196
C    *                                                                    196
5181  CALL AGSPOPB                                                        197
      GO TO BLOCK_SFCP, (5180 ,5190)                                      197
C End of Local Procedure                                                  197
C ----------------------------------------------------------------------  198
C ------------ SFCW                                                       198
C Begin Local Procedure SFCW                                              199
C SFCW IS THE WATER CYLINDER IN THE COOLING PIPE                          199
5220  CONTINUE                                                            199
         AG_TITLE='SFCW'                                                  199
         AG_MODULE=MODULE                                                 199
         AG_MARK='V00'                                                    199
         CALL AGSPUSH                                                     199
         AG_MODULE=MODULE                                                 199
C    *                                                                    199
C    Geant ATTRIBUTE : SFCW ===> SEEN=1 COLO=6 <===                       200
         CALL AGSRESET                                                    200
         AG_SEEN = 1                                                      200
         AG_COLO = 6                                                      200
         AG_TITLE = 'SFCW'                                                200
         AG_EXNAME = 'ATTRIBUTE'                                          200
         AG_PARLIST = 'SEEN_COLO'                                         200
         CALL AXATTRIBUTE                                                 200
C    *                                                                    200
C    Geant MATERIAL : WATER ===> inherited arguments used <===            200
         CALL AGSRESET                                                    201
         AG_TITLE = 'WATER'                                               201
         AG_EXNAME = 'MATERIAL'                                           201
         AG_PARLIST = 'NONE'                                              201
         CALL AXMATERIAL                                                  201
C    *                                                                    201
C    Geant SHAPE : TUBE ===> RMAX=SFPA_CPRAL <===                         201
         CALL AGSRESET                                                    202
         AG_RMAX = SFPA_CPRAL                                             202
         AG_TITLE = 'TUBE'                                                202
         AG_EXNAME = 'SHAPE'                                              202
         AG_PARLIST = 'RMAX'                                              202
         CALL AXSHAPE                                                     202
C    *                                                                    202
5221  CALL AGSPOPB                                                        203
      GO TO BLOCK_SFCW, (5220 ,5230)                                      203
C End of Local Procedure                                                  203
C ----------------------------------------------------------------------  204
C ------------ SFCF                                                       204
C Begin Local Procedure SFCF                                              205
C SFCF IS THE CARBON FIBER STRUCTURE CONTAINER                            205
5200  CONTINUE                                                            205
         AG_TITLE='SFCF'                                                  205
         AG_MODULE=MODULE                                                 205
         AG_MARK='V00'                                                    205
         CALL AGSPUSH                                                     205
         AG_MODULE=MODULE                                                 205
C    *                                                                    205
C    Geant MATERIAL : AIR ===> inherited arguments used <===              206
         CALL AGSRESET                                                    206
         AG_TITLE = 'AIR'                                                 206
         AG_EXNAME = 'MATERIAL'                                           206
         AG_PARLIST = 'NONE'                                              206
         CALL AXMATERIAL                                                  206
C    *                                                                    206
C    Geant ATTRIBUTE : SFCF ===> SEEN=0 COLO=3 <===                       206
         CALL AGSRESET                                                    207
         AG_SEEN = 0                                                      207
         AG_COLO = 3                                                      207
         AG_TITLE = 'SFCF'                                                207
         AG_EXNAME = 'ATTRIBUTE'                                          207
         AG_PARLIST = 'SEEN_COLO'                                         207
         CALL AXATTRIBUTE                                                 207
C    *                                                                    207
C    Geant SHAPE : BOX ===> DX=SFPA_SMTHK*TAN(PI/6.) DY=SFPA_SMTHK/2., D  207
C    Z=SFPA_SSLEN/2 <===                                                  207
         CALL AGSRESET                                                    209
         AG_DX = SFPA_SMTHK*TAN(PI/6.)                                    209
         AG_DY = SFPA_SMTHK/2.                                            209
         AG_DZ = SFPA_SSLEN/2                                             209
         AG_TITLE = 'BOX'                                                 209
         AG_EXNAME = 'SHAPE'                                              209
         AG_PARLIST = 'DX_DY_DZ'                                          209
         CALL AXSHAPE                                                     209
C    *                                                                    209
C    Create SFCT :                                                        210
C    Call To Local Procedure SFCT                                         210
         ASSIGN 5250 TO BLOCK_SFCT                                        210
         GO TO  5240                                                      210
5250     CONTINUE                                                         210
C    *                                                                    210
C    Geant POSITION : SFCT ===> Y=SFPA_SMTHK/2.-SFPA_CFRAD <===           211
         CALL AGSRESET                                                    211
         AG_Y = SFPA_SMTHK/2.-SFPA_CFRAD                                  211
         AG_TITLE = 'SFCT'                                                211
         AG_EXNAME = 'POSITION'                                           211
         AG_PARLIST = 'Y'                                                 211
         CALL AXPOSITION                                                  211
C    *                                                                    211
C    Geant POSITION : SFCT ===> X=SFPA_SMTHK*TAN(PI/6.)-SFPA_CFRAD Y=-SF  211
C    PA_SMTHK/2.+SFPA_CFRAD <===                                          211
         CALL AGSRESET                                                    212
         AG_X = SFPA_SMTHK*TAN(PI/6.)-SFPA_CFRAD                          212
         AG_Y = -SFPA_SMTHK/2.+SFPA_CFRAD                                 212
         AG_TITLE = 'SFCT'                                                212
         AG_EXNAME = 'POSITION'                                           212
         AG_PARLIST = 'X_Y'                                               212
         CALL AXPOSITION                                                  212
C    *                                                                    212
C    Geant POSITION : SFCT ===> X=-SFPA_SMTHK*TAN(PI/6.)+SFPA_CFRAD Y=-S  212
C    FPA_SMTHK/2.+SFPA_CFRAD <===                                         212
         CALL AGSRESET                                                    213
         AG_X = -SFPA_SMTHK*TAN(PI/6.)+SFPA_CFRAD                         213
         AG_Y = -SFPA_SMTHK/2.+SFPA_CFRAD                                 213
         AG_TITLE = 'SFCT'                                                213
         AG_EXNAME = 'POSITION'                                           213
         AG_PARLIST = 'X_Y'                                               213
         CALL AXPOSITION                                                  213
C    *                                                                    213
C    Create SFCX :                                                        214
C    Call To Local Procedure SFCX                                         214
         ASSIGN 5270 TO BLOCK_SFCX                                        214
         GO TO  5260                                                      214
5270     CONTINUE                                                         214
C    *                                                                    214
C    Geant POSITION : SFCX ===> Y=-SFPA_SMTHK/2.+SFPA_CFRAD ORT=YZX <===  215
         CALL AGSRESET                                                    215
         AG_Y = -SFPA_SMTHK/2.+SFPA_CFRAD                                 215
         AG_ALPHAN=AG_ALPHAN+1                                            215
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 215
         AG_TITLE = 'SFCX'                                                215
         AG_EXNAME = 'POSITION'                                           215
         AG_PARLIST = 'Y_ORT'                                             215
         CALL AXPOSITION                                                  215
C    *                                                                    215
C    Geant POSITION : SFCX ===> X=SFPA_SMTHK*TAN(PI/6.)/2.-SFPA_CFRAD/3.  215
C     ORT=YZX ALPHAZ=-60 <===                                             215
         CALL AGSRESET                                                    216
         AG_X = SFPA_SMTHK*TAN(PI/6.)/2.-SFPA_CFRAD/3.                    216
         AG_ALPHAN=AG_ALPHAN+1                                            216
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 216
         AG_ALPHAN=AG_ALPHAN+1                                            216
         AG_ALPHAK(AG_ALPHAN)=3                                           216
         AG_ALPHAL(AG_ALPHAN)=-60                                         216
         AG_TITLE = 'SFCX'                                                216
         AG_EXNAME = 'POSITION'                                           216
         AG_PARLIST = 'X_ORT_ALPHAZ'                                      216
         CALL AXPOSITION                                                  216
C    *                                                                    216
C    Geant POSITION : SFCX ===> X=-SFPA_SMTHK*TAN(PI/6.)/2.+SFPA_CFRAD/3  216
C    . ORT=YZX ALPHAZ=+60 <===                                            216
         CALL AGSRESET                                                    217
         AG_X = -SFPA_SMTHK*TAN(PI/6.)/2.+SFPA_CFRAD/3.                   217
         AG_ALPHAN=AG_ALPHAN+1                                            217
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 217
         AG_ALPHAN=AG_ALPHAN+1                                            217
         AG_ALPHAK(AG_ALPHAN)=3                                           217
         AG_ALPHAL(AG_ALPHAN)=+60                                         217
         AG_TITLE = 'SFCX'                                                217
         AG_EXNAME = 'POSITION'                                           217
         AG_PARLIST = 'X_ORT_ALPHAZ'                                      217
         CALL AXPOSITION                                                  217
C    *                                                                    217
5201  CALL AGSPOPB                                                        218
      GO TO BLOCK_SFCF, (5200 ,5210)                                      218
C End of Local Procedure                                                  218
C ----------------------------------------------------------------------  219
C ------------ SFCT                                                       219
C Begin Local Procedure SFCT                                              220
C SFCT IS THE CARBON FIBER TUBE                                           220
5240  CONTINUE                                                            220
         AG_TITLE='SFCT'                                                  220
         AG_MODULE=MODULE                                                 220
         AG_MARK='V00'                                                    220
         CALL AGSPUSH                                                     220
         AG_MODULE=MODULE                                                 220
C    *                                                                    220
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           221
         CALL AGSRESET                                                    221
         AG_TITLE = 'CARBON'                                              221
         AG_EXNAME = 'MATERIAL'                                           221
         AG_PARLIST = 'NONE'                                              221
         CALL AXMATERIAL                                                  221
C    *                                                                    221
C    Geant ATTRIBUTE : SFCT ===> SEEN=1 COLO=6 <===                       221
         CALL AGSRESET                                                    222
         AG_SEEN = 1                                                      222
         AG_COLO = 6                                                      222
         AG_TITLE = 'SFCT'                                                222
         AG_EXNAME = 'ATTRIBUTE'                                          222
         AG_PARLIST = 'SEEN_COLO'                                         222
         CALL AXATTRIBUTE                                                 222
C    *                                                                    222
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=SFPA_CFRAD DZ=SFPA_SSLEN/2 <===  222
         CALL AGSRESET                                                    223
         AG_RMIN = 0                                                      223
         AG_RMAX = SFPA_CFRAD                                             223
         AG_DZ = SFPA_SSLEN/2                                             223
         AG_TITLE = 'TUBE'                                                223
         AG_EXNAME = 'SHAPE'                                              223
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      223
         CALL AXSHAPE                                                     223
C    *                                                                    223
5241  CALL AGSPOPB                                                        224
      GO TO BLOCK_SFCT, (5240 ,5250)                                      224
C End of Local Procedure                                                  224
C ----------------------------------------------------------------------  225
C ------------ SFCX                                                       225
C Begin Local Procedure SFCX                                              226
C SFCX IS THE CARBON FIBER TUBE                                           226
5260  CONTINUE                                                            226
         AG_TITLE='SFCX'                                                  226
         AG_MODULE=MODULE                                                 226
         AG_MARK='V00'                                                    226
         CALL AGSPUSH                                                     226
         AG_MODULE=MODULE                                                 226
C    *                                                                    226
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           227
         CALL AGSRESET                                                    227
         AG_TITLE = 'CARBON'                                              227
         AG_EXNAME = 'MATERIAL'                                           227
         AG_PARLIST = 'NONE'                                              227
         CALL AXMATERIAL                                                  227
C    *                                                                    227
C    Geant ATTRIBUTE : SFCX ===> SEEN=1 COLO=7 <===                       227
         CALL AGSRESET                                                    228
         AG_SEEN = 1                                                      228
         AG_COLO = 7                                                      228
         AG_TITLE = 'SFCX'                                                228
         AG_EXNAME = 'ATTRIBUTE'                                          228
         AG_PARLIST = 'SEEN_COLO'                                         228
         CALL AXATTRIBUTE                                                 228
C    *                                                                    228
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=SFPA_CFRAD DZ=SFPA_SMTHK*TAN(PI  228
C    /6.)-SFPA_CFRAD <===                                                 228
         CALL AGSRESET                                                    229
         AG_RMIN = 0                                                      229
         AG_RMAX = SFPA_CFRAD                                             229
         AG_DZ = SFPA_SMTHK*TAN(PI/6.)-SFPA_CFRAD                         229
         AG_TITLE = 'TUBE'                                                229
         AG_EXNAME = 'SHAPE'                                              229
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      229
         CALL AXSHAPE                                                     229
C    *                                                                    229
5261  CALL AGSPOPB                                                        230
      GO TO BLOCK_SFCX, (5260 ,5270)                                      230
C End of Local Procedure                                                  230
      WRITE(LOUT,5280)'===>>  [End AgML Module SISDGEO]  <<==='           230
5280  FORMAT(A32,/,/)                                                     230
C MODULE SISDGEO                                                          232
      END                                                                 232
