C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE TPCEGEO1                                                 1
C TPCEGEO1 IS THE TPC SYSTEM IN GSTAR                                     1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'TPCEGEO1'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'IS THE TPC SYSTEM IN GSTAR'/                         1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C AUTHOR DAVID UNDERWOOD AND MAXIM POTEKHIN                               1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'DAVID UNDERWOOD AND MAXIM POTEKHIN'/          2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C CREATED MARCH 7, 2005                                                   2
      CHARACTER*40 CREATED /'MARCH 7, 2005'/                              3
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
C - GEANT description of logical units                                    4
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            5
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      5
      COMMON/GCMAIL/ CHMAIL                                               5
      CHARACTER*132 CHMAIL                                                5
C - Natural constants (or considered as such)                             5
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   6
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        6
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       6
      INTEGER BLOCK_TPCE                                                  6
      INTEGER BLOCK_TOFC                                                  9
      INTEGER BLOCK_TOFS                                                  9
      INTEGER BLOCK_TOST                                                  9
      INTEGER BLOCK_TOKA                                                  9
      INTEGER BLOCK_TONX                                                  9
      INTEGER BLOCK_TOAD                                                  9
      INTEGER BLOCK_TOHA                                                  9
      INTEGER BLOCK_TPGV                                                  9
      INTEGER BLOCK_TPSS                                                  9
      INTEGER BLOCK_TIFC                                                  9
      INTEGER BLOCK_TIAL                                                  9
      INTEGER BLOCK_TIKA                                                  9
      INTEGER BLOCK_TINX                                                  9
      INTEGER BLOCK_TPCW                                                  9
      INTEGER BLOCK_TWSS                                                  9
      INTEGER BLOCK_TWGI                                                  9
      INTEGER BLOCK_TPCM                                                  9
      INTEGER BLOCK_TPEA                                                  9
      INTEGER BLOCK_TESS                                                  9
      INTEGER BLOCK_TSEC                                                  9
      INTEGER BLOCK_TMWC                                                  9
      INTEGER BLOCK_TMEA                                                  9
      INTEGER BLOCK_TMSE                                                  9
      INTEGER BLOCK_TIAG                                                  9
      INTEGER BLOCK_TOAE                                                  9
      INTEGER BLOCK_TPAD                                                  9
      INTEGER BLOCK_THOL                                                  9
      INTEGER BLOCK_THRA                                                  9
      INTEGER BLOCK_THLA                                                  9
      INTEGER BLOCK_TALC                                                  9
      INTEGER BLOCK_TAEC                                                  9
      INTEGER BLOCK_TCEX                                                  9
      INTEGER BLOCK_TCRX                                                  9
      INTEGER BLOCK_TWGC                                                  9
      INTEGER BLOCK_TWGB                                                  9
      INTEGER BLOCK_TPIP                                                  9
      INTEGER BLOCK_TMAN                                                  9
      INTEGER BLOCK_TRDV                                                  9
      INTEGER BLOCK_TRDS                                                  9
      INTEGER BLOCK_TRDC                                                  9
C -                                                                       12
      REAL TPCG_VERSION                                                   12
      EQUIVALENCE (BANK_TPCG(3),TPCG_VERSION,RE_TPCG_VERSION),            12
     *(NAMES_TPCG(1),NTPCG_VERSION)                                       12
      EQUIVALENCE (COMMEN_TPCG(1),CTPCG_VERSION),(MAP_TPCG(1,1),          12
     *MTPCG_VERSION)                                                      12
      CHARACTER CTPCG_VERSION*40/' '/                                     12
      CHARACTER NTPCG_VERSION*12/'R.VERSION'/                             12
      INTEGER MTPCG_VERSION(2)/1,1/                                       12
      REAL RE_TPCG_VERSION                                                12
C -                                                                       12
      REAL TPCG_RMIN                                                      12
      EQUIVALENCE (BANK_TPCG(4),TPCG_RMIN,RE_TPCG_RMIN),(NAMES_TPCG(2),   12
     *NTPCG_RMIN)                                                         12
      EQUIVALENCE (COMMEN_TPCG(2),CTPCG_RMIN),(MAP_TPCG(1,2),             12
     *MTPCG_RMIN)                                                         12
      CHARACTER CTPCG_RMIN*40/' '/                                        12
      CHARACTER NTPCG_RMIN*12/'R.RMIN'/                                   12
      INTEGER MTPCG_RMIN(2)/1,1/                                          12
      REAL RE_TPCG_RMIN                                                   12
C -                                                                       12
      REAL TPCG_RMAX                                                      12
      EQUIVALENCE (BANK_TPCG(5),TPCG_RMAX,RE_TPCG_RMAX),(NAMES_TPCG(3),   12
     *NTPCG_RMAX)                                                         12
      EQUIVALENCE (COMMEN_TPCG(3),CTPCG_RMAX),(MAP_TPCG(1,3),             12
     *MTPCG_RMAX)                                                         12
      CHARACTER CTPCG_RMAX*40/' '/                                        12
      CHARACTER NTPCG_RMAX*12/'R.RMAX'/                                   12
      INTEGER MTPCG_RMAX(2)/1,1/                                          12
      REAL RE_TPCG_RMAX                                                   12
C -                                                                       12
      REAL TPCG_LENGTH                                                    12
      EQUIVALENCE (BANK_TPCG(6),TPCG_LENGTH,RE_TPCG_LENGTH),              12
     *(NAMES_TPCG(4),NTPCG_LENGTH)                                        12
      EQUIVALENCE (COMMEN_TPCG(4),CTPCG_LENGTH),(MAP_TPCG(1,4),           12
     *MTPCG_LENGTH)                                                       12
      CHARACTER CTPCG_LENGTH*40/' '/                                      12
      CHARACTER NTPCG_LENGTH*12/'R.LENGTH'/                               12
      INTEGER MTPCG_LENGTH(2)/1,1/                                        12
      REAL RE_TPCG_LENGTH                                                 12
C -                                                                       12
      REAL TPCG_WHEELIR                                                   12
      EQUIVALENCE (BANK_TPCG(7),TPCG_WHEELIR,RE_TPCG_WHEELIR),            12
     *(NAMES_TPCG(5),NTPCG_WHEELIR)                                       12
      EQUIVALENCE (COMMEN_TPCG(5),CTPCG_WHEELIR),(MAP_TPCG(1,5),          12
     *MTPCG_WHEELIR)                                                      12
      CHARACTER CTPCG_WHEELIR*40/' '/                                     12
      CHARACTER NTPCG_WHEELIR*12/'R.WHEELIR'/                             12
      INTEGER MTPCG_WHEELIR(2)/1,1/                                       12
      REAL RE_TPCG_WHEELIR                                                12
C -                                                                       12
      REAL TPCG_WHEELOR                                                   12
      EQUIVALENCE (BANK_TPCG(8),TPCG_WHEELOR,RE_TPCG_WHEELOR),            12
     *(NAMES_TPCG(6),NTPCG_WHEELOR)                                       12
      EQUIVALENCE (COMMEN_TPCG(6),CTPCG_WHEELOR),(MAP_TPCG(1,6),          12
     *MTPCG_WHEELOR)                                                      12
      CHARACTER CTPCG_WHEELOR*40/' '/                                     12
      CHARACTER NTPCG_WHEELOR*12/'R.WHEELOR'/                             12
      INTEGER MTPCG_WHEELOR(2)/1,1/                                       12
      REAL RE_TPCG_WHEELOR                                                12
C -                                                                       12
      REAL TPCG_WHEELTHK                                                  12
      EQUIVALENCE (BANK_TPCG(9),TPCG_WHEELTHK,RE_TPCG_WHEELTHK),          12
     *(NAMES_TPCG(7),NTPCG_WHEELTHK)                                      12
      EQUIVALENCE (COMMEN_TPCG(7),CTPCG_WHEELTHK),(MAP_TPCG(1,7),         12
     *MTPCG_WHEELTHK)                                                     12
      CHARACTER CTPCG_WHEELTHK*40/' '/                                    12
      CHARACTER NTPCG_WHEELTHK*12/'R.WHEELTHK'/                           12
      INTEGER MTPCG_WHEELTHK(2)/1,1/                                      12
      REAL RE_TPCG_WHEELTHK                                               12
C -                                                                       12
      REAL TPCG_SENGASOR                                                  12
      EQUIVALENCE (BANK_TPCG(10),TPCG_SENGASOR,RE_TPCG_SENGASOR),         12
     *(NAMES_TPCG(8),NTPCG_SENGASOR)                                      12
      EQUIVALENCE (COMMEN_TPCG(8),CTPCG_SENGASOR),(MAP_TPCG(1,8),         12
     *MTPCG_SENGASOR)                                                     12
      CHARACTER CTPCG_SENGASOR*40/' '/                                    12
      CHARACTER NTPCG_SENGASOR*12/'R.SENGASOR'/                           12
      INTEGER MTPCG_SENGASOR(2)/1,1/                                      12
      REAL RE_TPCG_SENGASOR                                               12
C -                                                                       12
      REAL TPCG_TPEATHK                                                   12
      EQUIVALENCE (BANK_TPCG(11),TPCG_TPEATHK,RE_TPCG_TPEATHK),           12
     *(NAMES_TPCG(9),NTPCG_TPEATHK)                                       12
      EQUIVALENCE (COMMEN_TPCG(9),CTPCG_TPEATHK),(MAP_TPCG(1,9),          12
     *MTPCG_TPEATHK)                                                      12
      CHARACTER CTPCG_TPEATHK*40/' '/                                     12
      CHARACTER NTPCG_TPEATHK*12/'R.TPEATHK'/                             12
      INTEGER MTPCG_TPEATHK(2)/1,1/                                       12
      REAL RE_TPCG_TPEATHK                                                12
C -                                                                       12
      REAL TPCG_MEMBTHK                                                   12
      EQUIVALENCE (BANK_TPCG(12),TPCG_MEMBTHK,RE_TPCG_MEMBTHK),           12
     *(NAMES_TPCG(10),NTPCG_MEMBTHK)                                      12
      EQUIVALENCE (COMMEN_TPCG(10),CTPCG_MEMBTHK),(MAP_TPCG(1,10),        12
     *MTPCG_MEMBTHK)                                                      12
      CHARACTER CTPCG_MEMBTHK*40/' '/                                     12
      CHARACTER NTPCG_MEMBTHK*12/'R.MEMBTHK'/                             12
      INTEGER MTPCG_MEMBTHK(2)/1,1/                                       12
      REAL RE_TPCG_MEMBTHK                                                12
C -                                                                       12
      REAL TPCG_TIADDR                                                    12
      EQUIVALENCE (BANK_TPCG(13),TPCG_TIADDR,RE_TPCG_TIADDR),             12
     *(NAMES_TPCG(11),NTPCG_TIADDR)                                       12
      EQUIVALENCE (COMMEN_TPCG(11),CTPCG_TIADDR),(MAP_TPCG(1,11),         12
     *MTPCG_TIADDR)                                                       12
      CHARACTER CTPCG_TIADDR*40/' '/                                      12
      CHARACTER NTPCG_TIADDR*12/'R.TIADDR'/                               12
      INTEGER MTPCG_TIADDR(2)/1,1/                                        12
      REAL RE_TPCG_TIADDR                                                 12
C -                                                                       12
      REAL TPCG_TINXDR                                                    12
      EQUIVALENCE (BANK_TPCG(14),TPCG_TINXDR,RE_TPCG_TINXDR),             12
     *(NAMES_TPCG(12),NTPCG_TINXDR)                                       12
      EQUIVALENCE (COMMEN_TPCG(12),CTPCG_TINXDR),(MAP_TPCG(1,12),         12
     *MTPCG_TINXDR)                                                       12
      CHARACTER CTPCG_TINXDR*40/' '/                                      12
      CHARACTER NTPCG_TINXDR*12/'R.TINXDR'/                               12
      INTEGER MTPCG_TINXDR(2)/1,1/                                        12
      REAL RE_TPCG_TINXDR                                                 12
C -                                                                       12
      REAL TPCG_TIKADR                                                    12
      EQUIVALENCE (BANK_TPCG(15),TPCG_TIKADR,RE_TPCG_TIKADR),             12
     *(NAMES_TPCG(13),NTPCG_TIKADR)                                       12
      EQUIVALENCE (COMMEN_TPCG(13),CTPCG_TIKADR),(MAP_TPCG(1,13),         12
     *MTPCG_TIKADR)                                                       12
      CHARACTER CTPCG_TIKADR*40/' '/                                      12
      CHARACTER NTPCG_TIKADR*12/'R.TIKADR'/                               12
      INTEGER MTPCG_TIKADR(2)/1,1/                                        12
      REAL RE_TPCG_TIKADR                                                 12
C -                                                                       12
      REAL TPCG_TIALDR                                                    12
      EQUIVALENCE (BANK_TPCG(16),TPCG_TIALDR,RE_TPCG_TIALDR),             12
     *(NAMES_TPCG(14),NTPCG_TIALDR)                                       12
      EQUIVALENCE (COMMEN_TPCG(14),CTPCG_TIALDR),(MAP_TPCG(1,14),         12
     *MTPCG_TIALDR)                                                       12
      CHARACTER CTPCG_TIALDR*40/' '/                                      12
      CHARACTER NTPCG_TIALDR*12/'R.TIALDR'/                               12
      INTEGER MTPCG_TIALDR(2)/1,1/                                        12
      REAL RE_TPCG_TIALDR                                                 12
C -                                                                       12
      REAL TPCG_TOCSDR                                                    12
      EQUIVALENCE (BANK_TPCG(17),TPCG_TOCSDR,RE_TPCG_TOCSDR),             12
     *(NAMES_TPCG(15),NTPCG_TOCSDR)                                       12
      EQUIVALENCE (COMMEN_TPCG(15),CTPCG_TOCSDR),(MAP_TPCG(1,15),         12
     *MTPCG_TOCSDR)                                                       12
      CHARACTER CTPCG_TOCSDR*40/' '/                                      12
      CHARACTER NTPCG_TOCSDR*12/'R.TOCSDR'/                               12
      INTEGER MTPCG_TOCSDR(2)/1,1/                                        12
      REAL RE_TPCG_TOCSDR                                                 12
C -                                                                       12
      REAL TPCG_TOKADR                                                    12
      EQUIVALENCE (BANK_TPCG(18),TPCG_TOKADR,RE_TPCG_TOKADR),             12
     *(NAMES_TPCG(16),NTPCG_TOKADR)                                       12
      EQUIVALENCE (COMMEN_TPCG(16),CTPCG_TOKADR),(MAP_TPCG(1,16),         12
     *MTPCG_TOKADR)                                                       12
      CHARACTER CTPCG_TOKADR*40/' '/                                      12
      CHARACTER NTPCG_TOKADR*12/'R.TOKADR'/                               12
      INTEGER MTPCG_TOKADR(2)/1,1/                                        12
      REAL RE_TPCG_TOKADR                                                 12
C -                                                                       12
      REAL TPCG_TONXDR                                                    12
      EQUIVALENCE (BANK_TPCG(19),TPCG_TONXDR,RE_TPCG_TONXDR),             12
     *(NAMES_TPCG(17),NTPCG_TONXDR)                                       12
      EQUIVALENCE (COMMEN_TPCG(17),CTPCG_TONXDR),(MAP_TPCG(1,17),         12
     *MTPCG_TONXDR)                                                       12
      CHARACTER CTPCG_TONXDR*40/' '/                                      12
      CHARACTER NTPCG_TONXDR*12/'R.TONXDR'/                               12
      INTEGER MTPCG_TONXDR(2)/1,1/                                        12
      REAL RE_TPCG_TONXDR                                                 12
C -                                                                       12
      REAL TPCG_TOADDR                                                    12
      EQUIVALENCE (BANK_TPCG(20),TPCG_TOADDR,RE_TPCG_TOADDR),             12
     *(NAMES_TPCG(18),NTPCG_TOADDR)                                       12
      EQUIVALENCE (COMMEN_TPCG(18),CTPCG_TOADDR),(MAP_TPCG(1,18),         12
     *MTPCG_TOADDR)                                                       12
      CHARACTER CTPCG_TOADDR*40/' '/                                      12
      CHARACTER NTPCG_TOADDR*12/'R.TOADDR'/                               12
      INTEGER MTPCG_TOADDR(2)/1,1/                                        12
      REAL RE_TPCG_TOADDR                                                 12
C -                                                                       12
      REAL TPCG_TOIGDR                                                    12
      EQUIVALENCE (BANK_TPCG(21),TPCG_TOIGDR,RE_TPCG_TOIGDR),             12
     *(NAMES_TPCG(19),NTPCG_TOIGDR)                                       12
      EQUIVALENCE (COMMEN_TPCG(19),CTPCG_TOIGDR),(MAP_TPCG(1,19),         12
     *MTPCG_TOIGDR)                                                       12
      CHARACTER CTPCG_TOIGDR*40/' '/                                      12
      CHARACTER NTPCG_TOIGDR*12/'R.TOIGDR'/                               12
      INTEGER MTPCG_TOIGDR(2)/1,1/                                        12
      REAL RE_TPCG_TOIGDR                                                 12
C -                                                                       12
      REAL TPCG_TOALDR                                                    12
      EQUIVALENCE (BANK_TPCG(22),TPCG_TOALDR,RE_TPCG_TOALDR),             12
     *(NAMES_TPCG(20),NTPCG_TOALDR)                                       12
      EQUIVALENCE (COMMEN_TPCG(20),CTPCG_TOALDR),(MAP_TPCG(1,20),         12
     *MTPCG_TOALDR)                                                       12
      CHARACTER CTPCG_TOALDR*40/' '/                                      12
      CHARACTER NTPCG_TOALDR*12/'R.TOALDR'/                               12
      INTEGER MTPCG_TOALDR(2)/1,1/                                        12
      REAL RE_TPCG_TOALDR                                                 12
C -                                                                       12
      REAL TPCG_TOHADR                                                    12
      EQUIVALENCE (BANK_TPCG(23),TPCG_TOHADR,RE_TPCG_TOHADR),             12
     *(NAMES_TPCG(21),NTPCG_TOHADR)                                       12
      EQUIVALENCE (COMMEN_TPCG(21),CTPCG_TOHADR),(MAP_TPCG(1,21),         12
     *MTPCG_TOHADR)                                                       12
      CHARACTER CTPCG_TOHADR*40/' '/                                      12
      CHARACTER NTPCG_TOHADR*12/'R.TOHADR'/                               12
      INTEGER MTPCG_TOHADR(2)/1,1/                                        12
      REAL RE_TPCG_TOHADR                                                 12
C -                                                                       12
      REAL TPCG_MWCREAD                                                   12
      EQUIVALENCE (BANK_TPCG(24),TPCG_MWCREAD,RE_TPCG_MWCREAD),           12
     *(NAMES_TPCG(22),NTPCG_MWCREAD)                                      12
      EQUIVALENCE (COMMEN_TPCG(22),CTPCG_MWCREAD),(MAP_TPCG(1,22),        12
     *MTPCG_MWCREAD)                                                      12
      CHARACTER CTPCG_MWCREAD*40/' '/                                     12
      CHARACTER NTPCG_MWCREAD*12/'R.MWCREAD'/                             12
      INTEGER MTPCG_MWCREAD(2)/1,1/                                       12
      REAL RE_TPCG_MWCREAD                                                12
C -                                                                       12
      REAL TPCG_GASCORR                                                   12
      EQUIVALENCE (BANK_TPCG(25),TPCG_GASCORR,RE_TPCG_GASCORR),           12
     *(NAMES_TPCG(23),NTPCG_GASCORR)                                      12
      EQUIVALENCE (COMMEN_TPCG(23),CTPCG_GASCORR),(MAP_TPCG(1,23),        12
     *MTPCG_GASCORR)                                                      12
      CHARACTER CTPCG_GASCORR*40/' '/                                     12
      CHARACTER NTPCG_GASCORR*12/'R.GASCORR'/                             12
      INTEGER MTPCG_GASCORR(2)/1,1/                                       12
      REAL RE_TPCG_GASCORR                                                12
C -                                                                       12
      INTEGER LEN_TPCG,LLEN_TPCG,LINK_TPCG                                12
      PARAMETER (LEN_TPCG=25)                                             12
      PARAMETER (LLEN_TPCG=23)                                            12
      INTEGER FLAG_TPCG/0/,MAP_TPCG(2,LLEN_TPCG),IDNUM_TPCG(10)           12
      CHARACTER NAMES_TPCG(LLEN_TPCG)*12,COMMEN_TPCG(LLEN_TPCG)*40        12
      DATA IDNUM_TPCG/10*0/                                               12
      REAL BANK_TPCG(LEN_TPCG)                                            12
      CHARACTER*60 BPATH_TPCG/' '/                                        12
      SAVE FLAG_TPCG,MAP_TPCG,IDNUM_TPCG,NAMES_TPCG,COMMEN_TPCG,          12
     *BPATH_TPCG                                                          12
      DATA LINK_TPCG/0/,BANK_TPCG/LEN_TPCG*0/                             12
C *                                                                       12
C -                                                                       17
      REAL TECW_SEC                                                       17
      EQUIVALENCE (BANK_TECW(3),TECW_SEC,RE_TECW_SEC),(NAMES_TECW(1),     17
     *NTECW_SEC)                                                          17
      EQUIVALENCE (COMMEN_TECW(1),CTECW_SEC),(MAP_TECW(1,1),MTECW_SEC)    17
      CHARACTER CTECW_SEC*40/' '/                                         17
      CHARACTER NTECW_SEC*12/'R.SEC'/                                     17
      INTEGER MTECW_SEC(2)/1,1/                                           17
      REAL RE_TECW_SEC                                                    17
C -                                                                       17
      REAL TECW_GAPRAD                                                    17
      EQUIVALENCE (BANK_TECW(4),TECW_GAPRAD,RE_TECW_GAPRAD),              17
     *(NAMES_TECW(2),NTECW_GAPRAD)                                        17
      EQUIVALENCE (COMMEN_TECW(2),CTECW_GAPRAD),(MAP_TECW(1,2),           17
     *MTECW_GAPRAD)                                                       17
      CHARACTER CTECW_GAPRAD*40/' '/                                      17
      CHARACTER NTECW_GAPRAD*12/'R.GAPRAD'/                               17
      INTEGER MTECW_GAPRAD(2)/1,1/                                        17
      REAL RE_TECW_GAPRAD                                                 17
C -                                                                       17
      REAL TECW_GAPHEIT                                                   17
      EQUIVALENCE (BANK_TECW(5),TECW_GAPHEIT,RE_TECW_GAPHEIT),            17
     *(NAMES_TECW(3),NTECW_GAPHEIT)                                       17
      EQUIVALENCE (COMMEN_TECW(3),CTECW_GAPHEIT),(MAP_TECW(1,3),          17
     *MTECW_GAPHEIT)                                                      17
      CHARACTER CTECW_GAPHEIT*40/' '/                                     17
      CHARACTER NTECW_GAPHEIT*12/'R.GAPHEIT'/                             17
      INTEGER MTECW_GAPHEIT(2)/1,1/                                       17
      REAL RE_TECW_GAPHEIT                                                17
C -                                                                       17
      REAL TECW_GAPWIDI                                                   17
      EQUIVALENCE (BANK_TECW(6),TECW_GAPWIDI,RE_TECW_GAPWIDI),            17
     *(NAMES_TECW(4),NTECW_GAPWIDI)                                       17
      EQUIVALENCE (COMMEN_TECW(4),CTECW_GAPWIDI),(MAP_TECW(1,4),          17
     *MTECW_GAPWIDI)                                                      17
      CHARACTER CTECW_GAPWIDI*40/' '/                                     17
      CHARACTER NTECW_GAPWIDI*12/'R.GAPWIDI'/                             17
      INTEGER MTECW_GAPWIDI(2)/1,1/                                       17
      REAL RE_TECW_GAPWIDI                                                17
C -                                                                       17
      REAL TECW_GAPWIDO                                                   17
      EQUIVALENCE (BANK_TECW(7),TECW_GAPWIDO,RE_TECW_GAPWIDO),            17
     *(NAMES_TECW(5),NTECW_GAPWIDO)                                       17
      EQUIVALENCE (COMMEN_TECW(5),CTECW_GAPWIDO),(MAP_TECW(1,5),          17
     *MTECW_GAPWIDO)                                                      17
      CHARACTER CTECW_GAPWIDO*40/' '/                                     17
      CHARACTER NTECW_GAPWIDO*12/'R.GAPWIDO'/                             17
      INTEGER MTECW_GAPWIDO(2)/1,1/                                       17
      REAL RE_TECW_GAPWIDO                                                17
C -                                                                       17
      REAL TECW_GAPSHFT                                                   17
      EQUIVALENCE (BANK_TECW(8),TECW_GAPSHFT,RE_TECW_GAPSHFT),            17
     *(NAMES_TECW(6),NTECW_GAPSHFT)                                       17
      EQUIVALENCE (COMMEN_TECW(6),CTECW_GAPSHFT),(MAP_TECW(1,6),          17
     *MTECW_GAPSHFT)                                                      17
      CHARACTER CTECW_GAPSHFT*40/' '/                                     17
      CHARACTER NTECW_GAPSHFT*12/'R.GAPSHFT'/                             17
      INTEGER MTECW_GAPSHFT(2)/1,1/                                       17
      REAL RE_TECW_GAPSHFT                                                17
C -                                                                       17
      REAL TECW_INWIDTH                                                   17
      EQUIVALENCE (BANK_TECW(9),TECW_INWIDTH,RE_TECW_INWIDTH),            17
     *(NAMES_TECW(7),NTECW_INWIDTH)                                       17
      EQUIVALENCE (COMMEN_TECW(7),CTECW_INWIDTH),(MAP_TECW(1,7),          17
     *MTECW_INWIDTH)                                                      17
      CHARACTER CTECW_INWIDTH*40/' '/                                     17
      CHARACTER NTECW_INWIDTH*12/'R.INWIDTH'/                             17
      INTEGER MTECW_INWIDTH(2)/1,1/                                       17
      REAL RE_TECW_INWIDTH                                                17
C -                                                                       17
      REAL TECW_OUWIDTH                                                   17
      EQUIVALENCE (BANK_TECW(10),TECW_OUWIDTH,RE_TECW_OUWIDTH),           17
     *(NAMES_TECW(8),NTECW_OUWIDTH)                                       17
      EQUIVALENCE (COMMEN_TECW(8),CTECW_OUWIDTH),(MAP_TECW(1,8),          17
     *MTECW_OUWIDTH)                                                      17
      CHARACTER CTECW_OUWIDTH*40/' '/                                     17
      CHARACTER NTECW_OUWIDTH*12/'R.OUWIDTH'/                             17
      INTEGER MTECW_OUWIDTH(2)/1,1/                                       17
      REAL RE_TECW_OUWIDTH                                                17
C -                                                                       17
      REAL TECW_HEIGHT                                                    17
      EQUIVALENCE (BANK_TECW(11),TECW_HEIGHT,RE_TECW_HEIGHT),             17
     *(NAMES_TECW(9),NTECW_HEIGHT)                                        17
      EQUIVALENCE (COMMEN_TECW(9),CTECW_HEIGHT),(MAP_TECW(1,9),           17
     *MTECW_HEIGHT)                                                       17
      CHARACTER CTECW_HEIGHT*40/' '/                                      17
      CHARACTER NTECW_HEIGHT*12/'R.HEIGHT'/                               17
      INTEGER MTECW_HEIGHT(2)/1,1/                                        17
      REAL RE_TECW_HEIGHT                                                 17
C -                                                                       17
      REAL TECW_PPDEPTH                                                   17
      EQUIVALENCE (BANK_TECW(12),TECW_PPDEPTH,RE_TECW_PPDEPTH),           17
     *(NAMES_TECW(10),NTECW_PPDEPTH)                                      17
      EQUIVALENCE (COMMEN_TECW(10),CTECW_PPDEPTH),(MAP_TECW(1,10),        17
     *MTECW_PPDEPTH)                                                      17
      CHARACTER CTECW_PPDEPTH*40/' '/                                     17
      CHARACTER NTECW_PPDEPTH*12/'R.PPDEPTH'/                             17
      INTEGER MTECW_PPDEPTH(2)/1,1/                                       17
      REAL RE_TECW_PPDEPTH                                                17
C -                                                                       17
      REAL TECW_ASDEPTH                                                   17
      EQUIVALENCE (BANK_TECW(13),TECW_ASDEPTH,RE_TECW_ASDEPTH),           17
     *(NAMES_TECW(11),NTECW_ASDEPTH)                                      17
      EQUIVALENCE (COMMEN_TECW(11),CTECW_ASDEPTH),(MAP_TECW(1,11),        17
     *MTECW_ASDEPTH)                                                      17
      CHARACTER CTECW_ASDEPTH*40/' '/                                     17
      CHARACTER NTECW_ASDEPTH*12/'R.ASDEPTH'/                             17
      INTEGER MTECW_ASDEPTH(2)/1,1/                                       17
      REAL RE_TECW_ASDEPTH                                                17
C -                                                                       17
      REAL TECW_GGDEPTH                                                   17
      EQUIVALENCE (BANK_TECW(14),TECW_GGDEPTH,RE_TECW_GGDEPTH),           17
     *(NAMES_TECW(12),NTECW_GGDEPTH)                                      17
      EQUIVALENCE (COMMEN_TECW(12),CTECW_GGDEPTH),(MAP_TECW(1,12),        17
     *MTECW_GGDEPTH)                                                      17
      CHARACTER CTECW_GGDEPTH*40/' '/                                     17
      CHARACTER NTECW_GGDEPTH*12/'R.GGDEPTH'/                             17
      INTEGER MTECW_GGDEPTH(2)/1,1/                                       17
      REAL RE_TECW_GGDEPTH                                                17
C -                                                                       17
      REAL TECW_MWCDEPTH                                                  17
      EQUIVALENCE (BANK_TECW(15),TECW_MWCDEPTH,RE_TECW_MWCDEPTH),         17
     *(NAMES_TECW(13),NTECW_MWCDEPTH)                                     17
      EQUIVALENCE (COMMEN_TECW(13),CTECW_MWCDEPTH),(MAP_TECW(1,13),       17
     *MTECW_MWCDEPTH)                                                     17
      CHARACTER CTECW_MWCDEPTH*40/' '/                                    17
      CHARACTER NTECW_MWCDEPTH*12/'R.MWCDEPTH'/                           17
      INTEGER MTECW_MWCDEPTH(2)/1,1/                                      17
      REAL RE_TECW_MWCDEPTH                                               17
C -                                                                       17
      REAL TECW_BOUNDARY                                                  17
      EQUIVALENCE (BANK_TECW(16),TECW_BOUNDARY,RE_TECW_BOUNDARY),         17
     *(NAMES_TECW(14),NTECW_BOUNDARY)                                     17
      EQUIVALENCE (COMMEN_TECW(14),CTECW_BOUNDARY),(MAP_TECW(1,14),       17
     *MTECW_BOUNDARY)                                                     17
      CHARACTER CTECW_BOUNDARY*40/' '/                                    17
      CHARACTER NTECW_BOUNDARY*12/'R.BOUNDARY'/                           17
      INTEGER MTECW_BOUNDARY(2)/1,1/                                      17
      REAL RE_TECW_BOUNDARY                                               17
C -                                                                       17
      REAL TECW_RCENTER                                                   17
      EQUIVALENCE (BANK_TECW(17),TECW_RCENTER,RE_TECW_RCENTER),           17
     *(NAMES_TECW(15),NTECW_RCENTER)                                      17
      EQUIVALENCE (COMMEN_TECW(15),CTECW_RCENTER),(MAP_TECW(1,15),        17
     *MTECW_RCENTER)                                                      17
      CHARACTER CTECW_RCENTER*40/' '/                                     17
      CHARACTER NTECW_RCENTER*12/'R.RCENTER'/                             17
      INTEGER MTECW_RCENTER(2)/1,1/                                       17
      REAL RE_TECW_RCENTER                                                17
C -                                                                       17
      REAL TECW_MWCINN                                                    17
      EQUIVALENCE (BANK_TECW(18),TECW_MWCINN,RE_TECW_MWCINN),             17
     *(NAMES_TECW(16),NTECW_MWCINN)                                       17
      EQUIVALENCE (COMMEN_TECW(16),CTECW_MWCINN),(MAP_TECW(1,16),         17
     *MTECW_MWCINN)                                                       17
      CHARACTER CTECW_MWCINN*40/' '/                                      17
      CHARACTER NTECW_MWCINN*12/'R.MWCINN'/                               17
      INTEGER MTECW_MWCINN(2)/1,1/                                        17
      REAL RE_TECW_MWCINN                                                 17
C -                                                                       17
      REAL TECW_MWCOUT                                                    17
      EQUIVALENCE (BANK_TECW(19),TECW_MWCOUT,RE_TECW_MWCOUT),             17
     *(NAMES_TECW(17),NTECW_MWCOUT)                                       17
      EQUIVALENCE (COMMEN_TECW(17),CTECW_MWCOUT),(MAP_TECW(1,17),         17
     *MTECW_MWCOUT)                                                       17
      CHARACTER CTECW_MWCOUT*40/' '/                                      17
      CHARACTER NTECW_MWCOUT*12/'R.MWCOUT'/                               17
      INTEGER MTECW_MWCOUT(2)/1,1/                                        17
      REAL RE_TECW_MWCOUT                                                 17
C -                                                                       17
      REAL TECW_MWCHEI                                                    17
      EQUIVALENCE (BANK_TECW(20),TECW_MWCHEI,RE_TECW_MWCHEI),             17
     *(NAMES_TECW(18),NTECW_MWCHEI)                                       17
      EQUIVALENCE (COMMEN_TECW(18),CTECW_MWCHEI),(MAP_TECW(1,18),         17
     *MTECW_MWCHEI)                                                       17
      CHARACTER CTECW_MWCHEI*40/' '/                                      17
      CHARACTER NTECW_MWCHEI*12/'R.MWCHEI'/                               17
      INTEGER MTECW_MWCHEI(2)/1,1/                                        17
      REAL RE_TECW_MWCHEI                                                 17
C -                                                                       17
      REAL TECW_MWCCENT                                                   17
      EQUIVALENCE (BANK_TECW(21),TECW_MWCCENT,RE_TECW_MWCCENT),           17
     *(NAMES_TECW(19),NTECW_MWCCENT)                                      17
      EQUIVALENCE (COMMEN_TECW(19),CTECW_MWCCENT),(MAP_TECW(1,19),        17
     *MTECW_MWCCENT)                                                      17
      CHARACTER CTECW_MWCCENT*40/' '/                                     17
      CHARACTER NTECW_MWCCENT*12/'R.MWCCENT'/                             17
      INTEGER MTECW_MWCCENT(2)/1,1/                                       17
      REAL RE_TECW_MWCCENT                                                17
C -                                                                       17
      REAL TECW_MWCNWIR                                                   17
      EQUIVALENCE (BANK_TECW(22),TECW_MWCNWIR,RE_TECW_MWCNWIR),           17
     *(NAMES_TECW(20),NTECW_MWCNWIR)                                      17
      EQUIVALENCE (COMMEN_TECW(20),CTECW_MWCNWIR),(MAP_TECW(1,20),        17
     *MTECW_MWCNWIR)                                                      17
      CHARACTER CTECW_MWCNWIR*40/' '/                                     17
      CHARACTER NTECW_MWCNWIR*12/'R.MWCNWIR'/                             17
      INTEGER MTECW_MWCNWIR(2)/1,1/                                       17
      REAL RE_TECW_MWCNWIR                                                17
C -                                                                       17
      REAL TECW_N                                                         17
      EQUIVALENCE (BANK_TECW(23),TECW_N,RE_TECW_N),(NAMES_TECW(21),       17
     *NTECW_N)                                                            17
      EQUIVALENCE (COMMEN_TECW(21),CTECW_N),(MAP_TECW(1,21),MTECW_N)      17
      CHARACTER CTECW_N*40/' '/                                           17
      CHARACTER NTECW_N*12/'R.N'/                                         17
      INTEGER MTECW_N(2)/1,1/                                             17
      REAL RE_TECW_N                                                      17
C -                                                                       17
      REAL TECW_NEX                                                       17
      EQUIVALENCE (BANK_TECW(24),TECW_NEX,RE_TECW_NEX),(NAMES_TECW(22),   17
     *NTECW_NEX)                                                          17
      EQUIVALENCE (COMMEN_TECW(22),CTECW_NEX),(MAP_TECW(1,22),            17
     *MTECW_NEX)                                                          17
      CHARACTER CTECW_NEX*40/' '/                                         17
      CHARACTER NTECW_NEX*12/'R.NEX'/                                     17
      INTEGER MTECW_NEX(2)/1,1/                                           17
      REAL RE_TECW_NEX                                                    17
C -                                                                       17
      REAL TECW_Z(8)                                                      17
      EQUIVALENCE (BANK_TECW(25),TECW_Z,RE_TECW_Z),(NAMES_TECW(23),       17
     *NTECW_Z)                                                            17
      EQUIVALENCE (COMMEN_TECW(23),CTECW_Z),(MAP_TECW(1,23),MTECW_Z)      17
      CHARACTER CTECW_Z*40/' '/                                           17
      CHARACTER NTECW_Z*12/'R.Z'/                                         17
      INTEGER MTECW_Z(2)/8,1/                                             17
      REAL RE_TECW_Z                                                      17
C -                                                                       17
      REAL TECW_DZ(8)                                                     17
      EQUIVALENCE (BANK_TECW(33),TECW_DZ,RE_TECW_DZ),(NAMES_TECW(24),     17
     *NTECW_DZ)                                                           17
      EQUIVALENCE (COMMEN_TECW(24),CTECW_DZ),(MAP_TECW(1,24),MTECW_DZ)    17
      CHARACTER CTECW_DZ*40/' '/                                          17
      CHARACTER NTECW_DZ*12/'R.DZ'/                                       17
      INTEGER MTECW_DZ(2)/8,1/                                            17
      REAL RE_TECW_DZ                                                     17
C -                                                                       17
      REAL TECW_XEX(5)                                                    17
      EQUIVALENCE (BANK_TECW(41),TECW_XEX,RE_TECW_XEX),(NAMES_TECW(25),   17
     *NTECW_XEX)                                                          17
      EQUIVALENCE (COMMEN_TECW(25),CTECW_XEX),(MAP_TECW(1,25),            17
     *MTECW_XEX)                                                          17
      CHARACTER CTECW_XEX*40/' '/                                         17
      CHARACTER NTECW_XEX*12/'R.XEX'/                                     17
      INTEGER MTECW_XEX(2)/5,1/                                           17
      REAL RE_TECW_XEX                                                    17
C -                                                                       17
      REAL TECW_ZEX(5)                                                    17
      EQUIVALENCE (BANK_TECW(46),TECW_ZEX,RE_TECW_ZEX),(NAMES_TECW(26),   17
     *NTECW_ZEX)                                                          17
      EQUIVALENCE (COMMEN_TECW(26),CTECW_ZEX),(MAP_TECW(1,26),            17
     *MTECW_ZEX)                                                          17
      CHARACTER CTECW_ZEX*40/' '/                                         17
      CHARACTER NTECW_ZEX*12/'R.ZEX'/                                     17
      INTEGER MTECW_ZEX(2)/5,1/                                           17
      REAL RE_TECW_ZEX                                                    17
C -                                                                       17
      REAL TECW_DXEX(5)                                                   17
      EQUIVALENCE (BANK_TECW(51),TECW_DXEX,RE_TECW_DXEX),                 17
     *(NAMES_TECW(27),NTECW_DXEX)                                         17
      EQUIVALENCE (COMMEN_TECW(27),CTECW_DXEX),(MAP_TECW(1,27),           17
     *MTECW_DXEX)                                                         17
      CHARACTER CTECW_DXEX*40/' '/                                        17
      CHARACTER NTECW_DXEX*12/'R.DXEX'/                                   17
      INTEGER MTECW_DXEX(2)/5,1/                                          17
      REAL RE_TECW_DXEX                                                   17
C -                                                                       17
      REAL TECW_DZEX(5)                                                   17
      EQUIVALENCE (BANK_TECW(56),TECW_DZEX,RE_TECW_DZEX),                 17
     *(NAMES_TECW(28),NTECW_DZEX)                                         17
      EQUIVALENCE (COMMEN_TECW(28),CTECW_DZEX),(MAP_TECW(1,28),           17
     *MTECW_DZEX)                                                         17
      CHARACTER CTECW_DZEX*40/' '/                                        17
      CHARACTER NTECW_DZEX*12/'R.DZEX'/                                   17
      INTEGER MTECW_DZEX(2)/5,1/                                          17
      REAL RE_TECW_DZEX                                                   17
C -                                                                       17
      REAL TECW_NHPLANE(8)                                                17
      EQUIVALENCE (BANK_TECW(61),TECW_NHPLANE,RE_TECW_NHPLANE),           17
     *(NAMES_TECW(29),NTECW_NHPLANE)                                      17
      EQUIVALENCE (COMMEN_TECW(29),CTECW_NHPLANE),(MAP_TECW(1,29),        17
     *MTECW_NHPLANE)                                                      17
      CHARACTER CTECW_NHPLANE*40/' '/                                     17
      CHARACTER NTECW_NHPLANE*12/'R.NHPLANE'/                             17
      INTEGER MTECW_NHPLANE(2)/8,1/                                       17
      REAL RE_TECW_NHPLANE                                                17
C -                                                                       17
      REAL TECW_CARDW                                                     17
      EQUIVALENCE (BANK_TECW(69),TECW_CARDW,RE_TECW_CARDW),               17
     *(NAMES_TECW(30),NTECW_CARDW)                                        17
      EQUIVALENCE (COMMEN_TECW(30),CTECW_CARDW),(MAP_TECW(1,30),          17
     *MTECW_CARDW)                                                        17
      CHARACTER CTECW_CARDW*40/' '/                                       17
      CHARACTER NTECW_CARDW*12/'R.CARDW'/                                 17
      INTEGER MTECW_CARDW(2)/1,1/                                         17
      REAL RE_TECW_CARDW                                                  17
C -                                                                       17
      REAL TECW_CARDTH                                                    17
      EQUIVALENCE (BANK_TECW(70),TECW_CARDTH,RE_TECW_CARDTH),             17
     *(NAMES_TECW(31),NTECW_CARDTH)                                       17
      EQUIVALENCE (COMMEN_TECW(31),CTECW_CARDTH),(MAP_TECW(1,31),         17
     *MTECW_CARDTH)                                                       17
      CHARACTER CTECW_CARDTH*40/' '/                                      17
      CHARACTER NTECW_CARDTH*12/'R.CARDTH'/                               17
      INTEGER MTECW_CARDTH(2)/1,1/                                        17
      REAL RE_TECW_CARDTH                                                 17
C -                                                                       17
      REAL TECW_COOLW                                                     17
      EQUIVALENCE (BANK_TECW(71),TECW_COOLW,RE_TECW_COOLW),               17
     *(NAMES_TECW(32),NTECW_COOLW)                                        17
      EQUIVALENCE (COMMEN_TECW(32),CTECW_COOLW),(MAP_TECW(1,32),          17
     *MTECW_COOLW)                                                        17
      CHARACTER CTECW_COOLW*40/' '/                                       17
      CHARACTER NTECW_COOLW*12/'R.COOLW'/                                 17
      INTEGER MTECW_COOLW(2)/1,1/                                         17
      REAL RE_TECW_COOLW                                                  17
C -                                                                       17
      REAL TECW_COOLTH                                                    17
      EQUIVALENCE (BANK_TECW(72),TECW_COOLTH,RE_TECW_COOLTH),             17
     *(NAMES_TECW(33),NTECW_COOLTH)                                       17
      EQUIVALENCE (COMMEN_TECW(33),CTECW_COOLTH),(MAP_TECW(1,33),         17
     *MTECW_COOLTH)                                                       17
      CHARACTER CTECW_COOLTH*40/' '/                                      17
      CHARACTER NTECW_COOLTH*12/'R.COOLTH'/                               17
      INTEGER MTECW_COOLTH(2)/1,1/                                        17
      REAL RE_TECW_COOLTH                                                 17
C -                                                                       17
      REAL TECW_CARDOFF                                                   17
      EQUIVALENCE (BANK_TECW(73),TECW_CARDOFF,RE_TECW_CARDOFF),           17
     *(NAMES_TECW(34),NTECW_CARDOFF)                                      17
      EQUIVALENCE (COMMEN_TECW(34),CTECW_CARDOFF),(MAP_TECW(1,34),        17
     *MTECW_CARDOFF)                                                      17
      CHARACTER CTECW_CARDOFF*40/' '/                                     17
      CHARACTER NTECW_CARDOFF*12/'R.CARDOFF'/                             17
      INTEGER MTECW_CARDOFF(2)/1,1/                                       17
      REAL RE_TECW_CARDOFF                                                17
C -                                                                       17
      REAL TECW_COOLOFF                                                   17
      EQUIVALENCE (BANK_TECW(74),TECW_COOLOFF,RE_TECW_COOLOFF),           17
     *(NAMES_TECW(35),NTECW_COOLOFF)                                      17
      EQUIVALENCE (COMMEN_TECW(35),CTECW_COOLOFF),(MAP_TECW(1,35),        17
     *MTECW_COOLOFF)                                                      17
      CHARACTER CTECW_COOLOFF*40/' '/                                     17
      CHARACTER NTECW_COOLOFF*12/'R.COOLOFF'/                             17
      INTEGER MTECW_COOLOFF(2)/1,1/                                       17
      REAL RE_TECW_COOLOFF                                                17
C -                                                                       17
      REAL TECW_SLOTW                                                     17
      EQUIVALENCE (BANK_TECW(75),TECW_SLOTW,RE_TECW_SLOTW),               17
     *(NAMES_TECW(36),NTECW_SLOTW)                                        17
      EQUIVALENCE (COMMEN_TECW(36),CTECW_SLOTW),(MAP_TECW(1,36),          17
     *MTECW_SLOTW)                                                        17
      CHARACTER CTECW_SLOTW*40/' '/                                       17
      CHARACTER NTECW_SLOTW*12/'R.SLOTW'/                                 17
      INTEGER MTECW_SLOTW(2)/1,1/                                         17
      REAL RE_TECW_SLOTW                                                  17
C -                                                                       17
      REAL TECW_SLOTRAD                                                   17
      EQUIVALENCE (BANK_TECW(76),TECW_SLOTRAD,RE_TECW_SLOTRAD),           17
     *(NAMES_TECW(37),NTECW_SLOTRAD)                                      17
      EQUIVALENCE (COMMEN_TECW(37),CTECW_SLOTRAD),(MAP_TECW(1,37),        17
     *MTECW_SLOTRAD)                                                      17
      CHARACTER CTECW_SLOTRAD*40/' '/                                     17
      CHARACTER NTECW_SLOTRAD*12/'R.SLOTRAD'/                             17
      INTEGER MTECW_SLOTRAD(2)/1,1/                                       17
      REAL RE_TECW_SLOTRAD                                                17
C -                                                                       17
      REAL TECW_PIPETHK                                                   17
      EQUIVALENCE (BANK_TECW(77),TECW_PIPETHK,RE_TECW_PIPETHK),           17
     *(NAMES_TECW(38),NTECW_PIPETHK)                                      17
      EQUIVALENCE (COMMEN_TECW(38),CTECW_PIPETHK),(MAP_TECW(1,38),        17
     *MTECW_PIPETHK)                                                      17
      CHARACTER CTECW_PIPETHK*40/' '/                                     17
      CHARACTER NTECW_PIPETHK*12/'R.PIPETHK'/                             17
      INTEGER MTECW_PIPETHK(2)/1,1/                                       17
      REAL RE_TECW_PIPETHK                                                17
C -                                                                       17
      REAL TECW_PIPEHT                                                    17
      EQUIVALENCE (BANK_TECW(78),TECW_PIPEHT,RE_TECW_PIPEHT),             17
     *(NAMES_TECW(39),NTECW_PIPEHT)                                       17
      EQUIVALENCE (COMMEN_TECW(39),CTECW_PIPEHT),(MAP_TECW(1,39),         17
     *MTECW_PIPEHT)                                                       17
      CHARACTER CTECW_PIPEHT*40/' '/                                      17
      CHARACTER NTECW_PIPEHT*12/'R.PIPEHT'/                               17
      INTEGER MTECW_PIPEHT(2)/1,1/                                        17
      REAL RE_TECW_PIPEHT                                                 17
C -                                                                       17
      REAL TECW_MANITHK                                                   17
      EQUIVALENCE (BANK_TECW(79),TECW_MANITHK,RE_TECW_MANITHK),           17
     *(NAMES_TECW(40),NTECW_MANITHK)                                      17
      EQUIVALENCE (COMMEN_TECW(40),CTECW_MANITHK),(MAP_TECW(1,40),        17
     *MTECW_MANITHK)                                                      17
      CHARACTER CTECW_MANITHK*40/' '/                                     17
      CHARACTER NTECW_MANITHK*12/'R.MANITHK'/                             17
      INTEGER MTECW_MANITHK(2)/1,1/                                       17
      REAL RE_TECW_MANITHK                                                17
C -                                                                       17
      REAL TECW_MANIWID                                                   17
      EQUIVALENCE (BANK_TECW(80),TECW_MANIWID,RE_TECW_MANIWID),           17
     *(NAMES_TECW(41),NTECW_MANIWID)                                      17
      EQUIVALENCE (COMMEN_TECW(41),CTECW_MANIWID),(MAP_TECW(1,41),        17
     *MTECW_MANIWID)                                                      17
      CHARACTER CTECW_MANIWID*40/' '/                                     17
      CHARACTER NTECW_MANIWID*12/'R.MANIWID'/                             17
      INTEGER MTECW_MANIWID(2)/1,1/                                       17
      REAL RE_TECW_MANIWID                                                17
C -                                                                       17
      REAL TECW_TAN15                                                     17
      EQUIVALENCE (BANK_TECW(81),TECW_TAN15,RE_TECW_TAN15),               17
     *(NAMES_TECW(42),NTECW_TAN15)                                        17
      EQUIVALENCE (COMMEN_TECW(42),CTECW_TAN15),(MAP_TECW(1,42),          17
     *MTECW_TAN15)                                                        17
      CHARACTER CTECW_TAN15*40/' '/                                       17
      CHARACTER NTECW_TAN15*12/'R.TAN15'/                                 17
      INTEGER MTECW_TAN15(2)/1,1/                                         17
      REAL RE_TECW_TAN15                                                  17
C -                                                                       17
      REAL TECW_CLEARANCE                                                 17
      EQUIVALENCE (BANK_TECW(82),TECW_CLEARANCE,RE_TECW_CLEARANCE),       17
     *(NAMES_TECW(43),NTECW_CLEARANCE)                                    17
      EQUIVALENCE (COMMEN_TECW(43),CTECW_CLEARANCE),(MAP_TECW(1,43),      17
     *MTECW_CLEARANCE)                                                    17
      CHARACTER CTECW_CLEARANCE*40/' '/                                   17
      CHARACTER NTECW_CLEARANCE*12/'R.CLEARANCE'/                         17
      INTEGER MTECW_CLEARANCE(2)/1,1/                                     17
      REAL RE_TECW_CLEARANCE                                              17
C -                                                                       17
      REAL TECW_WHLIPTHK                                                  17
      EQUIVALENCE (BANK_TECW(83),TECW_WHLIPTHK,RE_TECW_WHLIPTHK),         17
     *(NAMES_TECW(44),NTECW_WHLIPTHK)                                     17
      EQUIVALENCE (COMMEN_TECW(44),CTECW_WHLIPTHK),(MAP_TECW(1,44),       17
     *MTECW_WHLIPTHK)                                                     17
      CHARACTER CTECW_WHLIPTHK*40/' '/                                    17
      CHARACTER NTECW_WHLIPTHK*12/'R.WHLIPTHK'/                           17
      INTEGER MTECW_WHLIPTHK(2)/1,1/                                      17
      REAL RE_TECW_WHLIPTHK                                               17
C -                                                                       17
      REAL TECW_WHLIPWID                                                  17
      EQUIVALENCE (BANK_TECW(84),TECW_WHLIPWID,RE_TECW_WHLIPWID),         17
     *(NAMES_TECW(45),NTECW_WHLIPWID)                                     17
      EQUIVALENCE (COMMEN_TECW(45),CTECW_WHLIPWID),(MAP_TECW(1,45),       17
     *MTECW_WHLIPWID)                                                     17
      CHARACTER CTECW_WHLIPWID*40/' '/                                    17
      CHARACTER NTECW_WHLIPWID*12/'R.WHLIPWID'/                           17
      INTEGER MTECW_WHLIPWID(2)/1,1/                                      17
      REAL RE_TECW_WHLIPWID                                               17
C -                                                                       17
      REAL TECW_WHBLKLEN                                                  17
      EQUIVALENCE (BANK_TECW(85),TECW_WHBLKLEN,RE_TECW_WHBLKLEN),         17
     *(NAMES_TECW(46),NTECW_WHBLKLEN)                                     17
      EQUIVALENCE (COMMEN_TECW(46),CTECW_WHBLKLEN),(MAP_TECW(1,46),       17
     *MTECW_WHBLKLEN)                                                     17
      CHARACTER CTECW_WHBLKLEN*40/' '/                                    17
      CHARACTER NTECW_WHBLKLEN*12/'R.WHBLKLEN'/                           17
      INTEGER MTECW_WHBLKLEN(2)/1,1/                                      17
      REAL RE_TECW_WHBLKLEN                                               17
C -                                                                       17
      REAL TECW_WHBLKPOS                                                  17
      EQUIVALENCE (BANK_TECW(86),TECW_WHBLKPOS,RE_TECW_WHBLKPOS),         17
     *(NAMES_TECW(47),NTECW_WHBLKPOS)                                     17
      EQUIVALENCE (COMMEN_TECW(47),CTECW_WHBLKPOS),(MAP_TECW(1,47),       17
     *MTECW_WHBLKPOS)                                                     17
      CHARACTER CTECW_WHBLKPOS*40/' '/                                    17
      CHARACTER NTECW_WHBLKPOS*12/'R.WHBLKPOS'/                           17
      INTEGER MTECW_WHBLKPOS(2)/1,1/                                      17
      REAL RE_TECW_WHBLKPOS                                               17
C -                                                                       17
      REAL TECW_WHBLKIN                                                   17
      EQUIVALENCE (BANK_TECW(87),TECW_WHBLKIN,RE_TECW_WHBLKIN),           17
     *(NAMES_TECW(48),NTECW_WHBLKIN)                                      17
      EQUIVALENCE (COMMEN_TECW(48),CTECW_WHBLKIN),(MAP_TECW(1,48),        17
     *MTECW_WHBLKIN)                                                      17
      CHARACTER CTECW_WHBLKIN*40/' '/                                     17
      CHARACTER NTECW_WHBLKIN*12/'R.WHBLKIN'/                             17
      INTEGER MTECW_WHBLKIN(2)/1,1/                                       17
      REAL RE_TECW_WHBLKIN                                                17
C -                                                                       17
      INTEGER LEN_TECW,LLEN_TECW,LINK_TECW                                17
      PARAMETER (LEN_TECW=87)                                             17
      PARAMETER (LLEN_TECW=48)                                            17
      INTEGER FLAG_TECW/0/,MAP_TECW(2,LLEN_TECW),IDNUM_TECW(10)           17
      CHARACTER NAMES_TECW(LLEN_TECW)*12,COMMEN_TECW(LLEN_TECW)*40        17
      DATA IDNUM_TECW/10*0/                                               17
      REAL BANK_TECW(LEN_TECW)                                            17
      CHARACTER*60 BPATH_TECW/' '/                                        17
      SAVE FLAG_TECW,MAP_TECW,IDNUM_TECW,NAMES_TECW,COMMEN_TECW,          17
     *BPATH_TECW                                                          17
      DATA LINK_TECW/0/,BANK_TECW/LEN_TECW*0/                             17
C *                                                                       17
C -                                                                       18
      REAL TROV_SEC                                                       18
      EQUIVALENCE (BANK_TROV(3),TROV_SEC,RE_TROV_SEC),(NAMES_TROV(1),     18
     *NTROV_SEC)                                                          18
      EQUIVALENCE (COMMEN_TROV(1),CTROV_SEC),(MAP_TROV(1,1),MTROV_SEC)    18
      CHARACTER CTROV_SEC*40/' '/                                         18
      CHARACTER NTROV_SEC*12/'R.SEC'/                                     18
      INTEGER MTROV_SEC(2)/1,1/                                           18
      REAL RE_TROV_SEC                                                    18
C -                                                                       18
      REAL TROV_NHP(16)                                                   18
      EQUIVALENCE (BANK_TROV(4),TROV_NHP,RE_TROV_NHP),(NAMES_TROV(2),     18
     *NTROV_NHP)                                                          18
      EQUIVALENCE (COMMEN_TROV(2),CTROV_NHP),(MAP_TROV(1,2),MTROV_NHP)    18
      CHARACTER CTROV_NHP*40/' '/                                         18
      CHARACTER NTROV_NHP*12/'R.NHP'/                                     18
      INTEGER MTROV_NHP(2)/16,1/                                          18
      REAL RE_TROV_NHP                                                    18
C -                                                                       18
      REAL TROV_HX(9,16)                                                  18
      EQUIVALENCE (BANK_TROV(20),TROV_HX,RE_TROV_HX),(NAMES_TROV(3),      18
     *NTROV_HX)                                                           18
      EQUIVALENCE (COMMEN_TROV(3),CTROV_HX),(MAP_TROV(1,3),MTROV_HX)      18
      CHARACTER CTROV_HX*40/' '/                                          18
      CHARACTER NTROV_HX*12/'R.HX'/                                       18
      INTEGER MTROV_HX(2)/9,16/                                           18
      REAL RE_TROV_HX                                                     18
C -                                                                       18
      REAL TROV_OFFCARDINOUT                                              18
      EQUIVALENCE (BANK_TROV(164),TROV_OFFCARDINOUT,                      18
     *RE_TROV_OFFCARDINOUT),(NAMES_TROV(4),NTROV_OFFCARDINOUT)            18
      EQUIVALENCE (COMMEN_TROV(4),CTROV_OFFCARDINOUT),(MAP_TROV(1,4),     18
     *MTROV_OFFCARDINOUT)                                                 18
      CHARACTER CTROV_OFFCARDINOUT*40/' '/                                18
      CHARACTER NTROV_OFFCARDINOUT*12/'R.OFFCARDINOUT'/                   18
      INTEGER MTROV_OFFCARDINOUT(2)/1,1/                                  18
      REAL RE_TROV_OFFCARDINOUT                                           18
C -                                                                       18
      INTEGER LEN_TROV,LLEN_TROV,LINK_TROV                                18
      PARAMETER (LEN_TROV=164)                                            18
      PARAMETER (LLEN_TROV=4)                                             18
      INTEGER FLAG_TROV/0/,MAP_TROV(2,LLEN_TROV),IDNUM_TROV(10)           18
      CHARACTER NAMES_TROV(LLEN_TROV)*12,COMMEN_TROV(LLEN_TROV)*40        18
      DATA IDNUM_TROV/10*0/                                               18
      REAL BANK_TROV(LEN_TROV)                                            18
      CHARACTER*60 BPATH_TROV/' '/                                        18
      SAVE FLAG_TROV,MAP_TROV,IDNUM_TROV,NAMES_TROV,COMMEN_TROV,          18
     *BPATH_TROV                                                          18
      DATA LINK_TROV/0/,BANK_TROV/LEN_TROV*0/                             18
C *                                                                       18
C -                                                                       19
      REAL TPRS_SEC                                                       19
      EQUIVALENCE (BANK_TPRS(3),TPRS_SEC,RE_TPRS_SEC),(NAMES_TPRS(1),     19
     *NTPRS_SEC)                                                          19
      EQUIVALENCE (COMMEN_TPRS(1),CTPRS_SEC),(MAP_TPRS(1,1),MTPRS_SEC)    19
      CHARACTER CTPRS_SEC*40/' '/                                         19
      CHARACTER NTPRS_SEC*12/'R.SEC'/                                     19
      INTEGER MTPRS_SEC(2)/1,1/                                           19
      REAL RE_TPRS_SEC                                                    19
C -                                                                       19
      REAL TPRS_NROW                                                      19
      EQUIVALENCE (BANK_TPRS(4),TPRS_NROW,RE_TPRS_NROW),(NAMES_TPRS(2),   19
     *NTPRS_NROW)                                                         19
      EQUIVALENCE (COMMEN_TPRS(2),CTPRS_NROW),(MAP_TPRS(1,2),             19
     *MTPRS_NROW)                                                         19
      CHARACTER CTPRS_NROW*40/' '/                                        19
      CHARACTER NTPRS_NROW*12/'R.NROW'/                                   19
      INTEGER MTPRS_NROW(2)/1,1/                                          19
      REAL RE_TPRS_NROW                                                   19
C -                                                                       19
      REAL TPRS_PITCH                                                     19
      EQUIVALENCE (BANK_TPRS(5),TPRS_PITCH,RE_TPRS_PITCH),                19
     *(NAMES_TPRS(3),NTPRS_PITCH)                                         19
      EQUIVALENCE (COMMEN_TPRS(3),CTPRS_PITCH),(MAP_TPRS(1,3),            19
     *MTPRS_PITCH)                                                        19
      CHARACTER CTPRS_PITCH*40/' '/                                       19
      CHARACTER NTPRS_PITCH*12/'R.PITCH'/                                 19
      INTEGER MTPRS_PITCH(2)/1,1/                                         19
      REAL RE_TPRS_PITCH                                                  19
C -                                                                       19
      REAL TPRS_WIDTH                                                     19
      EQUIVALENCE (BANK_TPRS(6),TPRS_WIDTH,RE_TPRS_WIDTH),                19
     *(NAMES_TPRS(4),NTPRS_WIDTH)                                         19
      EQUIVALENCE (COMMEN_TPRS(4),CTPRS_WIDTH),(MAP_TPRS(1,4),            19
     *MTPRS_WIDTH)                                                        19
      CHARACTER CTPRS_WIDTH*40/' '/                                       19
      CHARACTER NTPRS_WIDTH*12/'R.WIDTH'/                                 19
      INTEGER MTPRS_WIDTH(2)/1,1/                                         19
      REAL RE_TPRS_WIDTH                                                  19
C -                                                                       19
      REAL TPRS_SUPER                                                     19
      EQUIVALENCE (BANK_TPRS(7),TPRS_SUPER,RE_TPRS_SUPER),                19
     *(NAMES_TPRS(5),NTPRS_SUPER)                                         19
      EQUIVALENCE (COMMEN_TPRS(5),CTPRS_SUPER),(MAP_TPRS(1,5),            19
     *MTPRS_SUPER)                                                        19
      CHARACTER CTPRS_SUPER*40/' '/                                       19
      CHARACTER NTPRS_SUPER*12/'R.SUPER'/                                 19
      INTEGER MTPRS_SUPER(2)/1,1/                                         19
      REAL RE_TPRS_SUPER                                                  19
C -                                                                       19
      REAL TPRS_RPADS(40)                                                 19
      EQUIVALENCE (BANK_TPRS(8),TPRS_RPADS,RE_TPRS_RPADS),                19
     *(NAMES_TPRS(6),NTPRS_RPADS)                                         19
      EQUIVALENCE (COMMEN_TPRS(6),CTPRS_RPADS),(MAP_TPRS(1,6),            19
     *MTPRS_RPADS)                                                        19
      CHARACTER CTPRS_RPADS*40/' '/                                       19
      CHARACTER NTPRS_RPADS*12/'R.RPADS'/                                 19
      INTEGER MTPRS_RPADS(2)/40,1/                                        19
      REAL RE_TPRS_RPADS                                                  19
C -                                                                       19
      REAL TPRS_NPADS(40)                                                 19
      EQUIVALENCE (BANK_TPRS(48),TPRS_NPADS,RE_TPRS_NPADS),               19
     *(NAMES_TPRS(7),NTPRS_NPADS)                                         19
      EQUIVALENCE (COMMEN_TPRS(7),CTPRS_NPADS),(MAP_TPRS(1,7),            19
     *MTPRS_NPADS)                                                        19
      CHARACTER CTPRS_NPADS*40/' '/                                       19
      CHARACTER NTPRS_NPADS*12/'R.NPADS'/                                 19
      INTEGER MTPRS_NPADS(2)/40,1/                                        19
      REAL RE_TPRS_NPADS                                                  19
C -                                                                       19
      INTEGER LEN_TPRS,LLEN_TPRS,LINK_TPRS                                19
      PARAMETER (LEN_TPRS=87)                                             19
      PARAMETER (LLEN_TPRS=7)                                             19
      INTEGER FLAG_TPRS/0/,MAP_TPRS(2,LLEN_TPRS),IDNUM_TPRS(10)           19
      CHARACTER NAMES_TPRS(LLEN_TPRS)*12,COMMEN_TPRS(LLEN_TPRS)*40        19
      DATA IDNUM_TPRS/10*0/                                               19
      REAL BANK_TPRS(LEN_TPRS)                                            19
      CHARACTER*60 BPATH_TPRS/' '/                                        19
      SAVE FLAG_TPRS,MAP_TPRS,IDNUM_TPRS,NAMES_TPRS,COMMEN_TPRS,          19
     *BPATH_TPRS                                                          19
      DATA LINK_TPRS/0/,BANK_TPRS/LEN_TPRS*0/                             19
C *                                                                       19
C -                                                                       20
      REAL TRDO_RDOVTHK                                                   20
      EQUIVALENCE (BANK_TRDO(3),TRDO_RDOVTHK,RE_TRDO_RDOVTHK),            20
     *(NAMES_TRDO(1),NTRDO_RDOVTHK)                                       20
      EQUIVALENCE (COMMEN_TRDO(1),CTRDO_RDOVTHK),(MAP_TRDO(1,1),          20
     *MTRDO_RDOVTHK)                                                      20
      CHARACTER CTRDO_RDOVTHK*40/' '/                                     20
      CHARACTER NTRDO_RDOVTHK*12/'R.RDOVTHK'/                             20
      INTEGER MTRDO_RDOVTHK(2)/1,1/                                       20
      REAL RE_TRDO_RDOVTHK                                                20
C -                                                                       20
      REAL TRDO_RDOTHK                                                    20
      EQUIVALENCE (BANK_TRDO(4),TRDO_RDOTHK,RE_TRDO_RDOTHK),              20
     *(NAMES_TRDO(2),NTRDO_RDOTHK)                                        20
      EQUIVALENCE (COMMEN_TRDO(2),CTRDO_RDOTHK),(MAP_TRDO(1,2),           20
     *MTRDO_RDOTHK)                                                       20
      CHARACTER CTRDO_RDOTHK*40/' '/                                      20
      CHARACTER NTRDO_RDOTHK*12/'R.RDOTHK'/                               20
      INTEGER MTRDO_RDOTHK(2)/1,1/                                        20
      REAL RE_TRDO_RDOTHK                                                 20
C -                                                                       20
      REAL TRDO_RDOLEN                                                    20
      EQUIVALENCE (BANK_TRDO(5),TRDO_RDOLEN,RE_TRDO_RDOLEN),              20
     *(NAMES_TRDO(3),NTRDO_RDOLEN)                                        20
      EQUIVALENCE (COMMEN_TRDO(3),CTRDO_RDOLEN),(MAP_TRDO(1,3),           20
     *MTRDO_RDOLEN)                                                       20
      CHARACTER CTRDO_RDOLEN*40/' '/                                      20
      CHARACTER NTRDO_RDOLEN*12/'R.RDOLEN'/                               20
      INTEGER MTRDO_RDOLEN(2)/1,1/                                        20
      REAL RE_TRDO_RDOLEN                                                 20
C -                                                                       20
      REAL TRDO_NRDOBRD                                                   20
      EQUIVALENCE (BANK_TRDO(6),TRDO_NRDOBRD,RE_TRDO_NRDOBRD),            20
     *(NAMES_TRDO(4),NTRDO_NRDOBRD)                                       20
      EQUIVALENCE (COMMEN_TRDO(4),CTRDO_NRDOBRD),(MAP_TRDO(1,4),          20
     *MTRDO_NRDOBRD)                                                      20
      CHARACTER CTRDO_NRDOBRD*40/' '/                                     20
      CHARACTER NTRDO_NRDOBRD*12/'R.NRDOBRD'/                             20
      INTEGER MTRDO_NRDOBRD(2)/1,1/                                       20
      REAL RE_TRDO_NRDOBRD                                                20
C -                                                                       20
      REAL TRDO_RDOHT(9)                                                  20
      EQUIVALENCE (BANK_TRDO(7),TRDO_RDOHT,RE_TRDO_RDOHT),                20
     *(NAMES_TRDO(5),NTRDO_RDOHT)                                         20
      EQUIVALENCE (COMMEN_TRDO(5),CTRDO_RDOHT),(MAP_TRDO(1,5),            20
     *MTRDO_RDOHT)                                                        20
      CHARACTER CTRDO_RDOHT*40/' '/                                       20
      CHARACTER NTRDO_RDOHT*12/'R.RDOHT'/                                 20
      INTEGER MTRDO_RDOHT(2)/9,1/                                         20
      REAL RE_TRDO_RDOHT                                                  20
C -                                                                       20
      INTEGER LEN_TRDO,LLEN_TRDO,LINK_TRDO                                20
      PARAMETER (LEN_TRDO=15)                                             20
      PARAMETER (LLEN_TRDO=5)                                             20
      INTEGER FLAG_TRDO/0/,MAP_TRDO(2,LLEN_TRDO),IDNUM_TRDO(10)           20
      CHARACTER NAMES_TRDO(LLEN_TRDO)*12,COMMEN_TRDO(LLEN_TRDO)*40        20
      DATA IDNUM_TRDO/10*0/                                               20
      REAL BANK_TRDO(LEN_TRDO)                                            20
      CHARACTER*60 BPATH_TRDO/' '/                                        20
      SAVE FLAG_TRDO,MAP_TRDO,IDNUM_TRDO,NAMES_TRDO,COMMEN_TRDO,          20
     *BPATH_TRDO                                                          20
      DATA LINK_TRDO/0/,BANK_TRDO/LEN_TRDO*0/                             20
C *                                                                       20
      REAL TOCSIR,TOCSOR,TOKAIR,TOKAOR,TONXIR,TONXOR,TOADIR,TOADOR,       24
     *TOIGIR,TOIGOR, TOALIR,TOALOR,TOHAIR,TOHAOR,TOFSIR,TOFSOR,TOFCIR,    24
     *TOFCOR,TIADIR,TIADOR,TINXIR,TINXOR, TIKAIR,TIKAOR,TIALIR,TIALOR,    24
     *TIFCIR,TIFCOR,TPGVIR,TPGVLENG,TOFCLENG,TPCWZ,TPGVZ, TPEAZ,TPRDOZ,   24
     *DX_DZ,DXB,DEL,A,B,RADOFF,ZVALUE                                     24
      REAL PIPEOFF                                                        26
      REAL ZTEMP,XTEMP,HPLA,XBOTTOM                                       28
      REAL MNTCYLID,MNTCYLOD,MNTCYLHT,XLIP                                30
      REAL T,P,PATM,RHO,DENSITY                                           32
      INTEGER I_ROW,I_SEC,I,JJ,KK,I_NHP,J_NHO                             34
      EXTERNAL TPADSTEP                                                   36
      EXTERNAL TPAISTEP                                                   37
      EXTERNAL TPAOSTEP                                                   38
      EXTERNAL TPCELASER                                                  39
C *                                                                       40
      CALL AGSBEGM(MODULE,IPRIN)                                          40
      IF (IPRIN.LT.0) RETURN                                              40
C Filling bank TPCG - TPC BASIC DIMENSIONS                                40
      BANK_TITLE='TPC BASIC DIMENSIONS'                                   40
      BPATH_TPCG='TPCG*'                                                  40
      IDNUM_TPCG(1)=0                                                     41
C CURRENT VERSION                                                         41
      TPCG_VERSION=2                                                      41
      CTPCG_VERSION='CURRENT VERSION'                                     41
C TPC INNER RADIUS                                                        42
      TPCG_RMIN=46.107                                                    42
      CTPCG_RMIN='TPC INNER RADIUS'                                       42
C TPC OUTER RADIUS                                                        43
      TPCG_RMAX=207.750                                                   43
      CTPCG_RMAX='TPC OUTER RADIUS'                                       43
C TPC FULL LENGTH                                                         44
      TPCG_LENGTH=519.37                                                  44
      CTPCG_LENGTH='TPC FULL LENGTH'                                      44
C SUPPORT WHEEL INNER RADIUS                                              45
      TPCG_WHEELIR=49.60                                                  45
      CTPCG_WHEELIR='SUPPORT WHEEL INNER RADIUS'                          45
C SUPPORT WHEEL OUTER RADIUS                                              46
      TPCG_WHEELOR=206.75                                                 46
      CTPCG_WHEELOR='SUPPORT WHEEL OUTER RADIUS'                          46
C SUPPORT WHEEL LENGTH                                                    47
      TPCG_WHEELTHK=11.43                                                 47
      CTPCG_WHEELTHK='SUPPORT WHEEL LENGTH'                               47
C TPC SENSITIVE GAS OUTER RADIUS                                          48
      TPCG_SENGASOR=200                                                   48
      CTPCG_SENGASOR='TPC SENSITIVE GAS OUTER RADIUS'                     48
C ENDCAP MWC + SECTOR THICKNESS                                           49
      TPCG_TPEATHK=9.7                                                    49
      CTPCG_TPEATHK='ENDCAP MWC + SECTOR THICKNESS'                       49
C CENTRAL MEMBRANE THICKNESS                                              50
      TPCG_MEMBTHK=.00762                                                 50
      CTPCG_MEMBTHK='CENTRAL MEMBRANE THICKNESS'                          50
C OUTER COPPER THICKNESS                                                  51
      TPCG_TOCSDR=.013                                                    51
      CTPCG_TOCSDR='OUTER COPPER THICKNESS'                               51
C OUTER KAPTON THICKNESS                                                  52
      TPCG_TOKADR=.015                                                    52
      CTPCG_TOKADR='OUTER KAPTON THICKNESS'                               52
C OUTER NOMEX THICKNESS                                                   53
      TPCG_TONXDR=1.00                                                    53
      CTPCG_TONXDR='OUTER NOMEX THICKNESS'                                53
C OUTER ADHESIVE THICKNESS                                                54
      TPCG_TOADDR=0.02                                                    54
      CTPCG_TOADDR='OUTER ADHESIVE THICKNESS'                             54
C OUTER ISOLATING GAS THICKNESS                                           55
      TPCG_TOIGDR=5.70                                                    55
      CTPCG_TOIGDR='OUTER ISOLATING GAS THICKNESS'                        55
C OUTER ALUMINUM THICKNESS                                                56
      TPCG_TOALDR=0.40                                                    56
      CTPCG_TOALDR='OUTER ALUMINUM THICKNESS'                             56
C OUTER HONEYUCOMB AL THICKNESS                                           57
      TPCG_TOHADR=0.60                                                    57
      CTPCG_TOHADR='OUTER HONEYUCOMB AL THICKNESS'                        57
C INNER ADHESIVE LAYER THICKNESS                                          58
      TPCG_TIADDR=0.080                                                   58
      CTPCG_TIADDR='INNER ADHESIVE LAYER THICKNESS'                       58
C INNER NOMEX STRUCTURE THICKNESS                                         59
      TPCG_TINXDR=1.270                                                   59
      CTPCG_TINXDR='INNER NOMEX STRUCTURE THICKNESS'                      59
C INNER KAPTON LAYER THICKNESS                                            60
      TPCG_TIKADR=0.015                                                   60
      CTPCG_TIKADR='INNER KAPTON LAYER THICKNESS'                         60
C INNER ALUMINUM LAYER THICKNESS                                          61
      TPCG_TIALDR=0.004                                                   61
      CTPCG_TIALDR='INNER ALUMINUM LAYER THICKNESS'                       61
C MWC READOUT FLAG                                                        62
      TPCG_MWCREAD=2                                                      62
      CTPCG_MWCREAD='MWC READOUT FLAG'                                    62
C GAS DENSITY CORRECTION VERSION                                          63
      TPCG_GASCORR=1                                                      63
      CTPCG_GASCORR='GAS DENSITY CORRECTION VERSION'                      63
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCG',    64
     *BPATH_TPCG,IDNUM_TPCG,1, MAP_TPCG,NAMES_TPCG,COMMEN_TPCG,           64
     *BANK_TPCG,LEN_TPCG,LLEN_TPCG,LINK_TPCG,FLAG_TPCG)                   64
C *                                                                       64
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCG',    64
     *BPATH_TPCG,IDNUM_TPCG,1, MAP_TPCG,NAMES_TPCG,COMMEN_TPCG,           64
     *BANK_TPCG,LEN_TPCG,LLEN_TPCG,LINK_TPCG,FLAG_TPCG)                   64
C *                                                                       65
      CALL AGSBEGM(MODULE,IPRIN)                                          65
      IF (IPRIN.LT.0) RETURN                                              65
C Filling bank TRDO - VOLUME FOR TPC READOUT BOARDS                       65
      BANK_TITLE='VOLUME FOR TPC READOUT BOARDS'                          65
      BPATH_TRDO='TRDO*'                                                  65
      IDNUM_TRDO(1)=0                                                     66
C LENGTH OF RDO VLOUME                                                    66
      TRDO_RDOVTHK=30.                                                    66
      CTRDO_RDOVTHK='LENGTH OF RDO VLOUME'                                66
C THICKNESS OF RDO CARD                                                   67
      TRDO_RDOTHK=.25                                                     67
      CTRDO_RDOTHK='THICKNESS OF RDO CARD'                                67
C CARD LENGTH ALONG BEAM DIRECTION                                        68
      TRDO_RDOLEN=27                                                      68
      CTRDO_RDOLEN='CARD LENGTH ALONG BEAM DIRECTION'                     68
C NUMBER OF RDO BOARDS                                                    69
      TRDO_NRDOBRD=9                                                      69
      CTRDO_NRDOBRD='NUMBER OF RDO BOARDS'                                69
C RADIAL POS OF RDO BOARDS                                                71
      TRDO_RDOHT(1)=60.0                                                  71
      TRDO_RDOHT(2)=74.0                                                  71
      TRDO_RDOHT(3)=84.0                                                  71
      TRDO_RDOHT(4)=101.0                                                 71
      TRDO_RDOHT(5)=106.0                                                 71
      TRDO_RDOHT(6)=126.0                                                 71
      TRDO_RDOHT(7)=146.0                                                 71
      TRDO_RDOHT(8)=166.0                                                 71
      TRDO_RDOHT(9)=186.0                                                 71
      CTRDO_RDOHT='RADIAL POS OF RDO BOARDS'                              71
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TRDO',    72
     *BPATH_TRDO,IDNUM_TRDO,1, MAP_TRDO,NAMES_TRDO,COMMEN_TRDO,           72
     *BANK_TRDO,LEN_TRDO,LLEN_TRDO,LINK_TRDO,FLAG_TRDO)                   72
C *                                                                       72
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TRDO',    72
     *BPATH_TRDO,IDNUM_TRDO,1, MAP_TRDO,NAMES_TRDO,COMMEN_TRDO,           72
     *BANK_TRDO,LEN_TRDO,LLEN_TRDO,LINK_TRDO,FLAG_TRDO)                   72
C *                                                                       73
      CALL AGSBEGM(MODULE,IPRIN)                                          73
      IF (IPRIN.LT.0) RETURN                                              73
C Filling bank TPRS - SECTOR OF PADROWS                                   73
      BANK_TITLE='SECTOR OF PADROWS'                                      73
      BPATH_TPRS='TPRS*'                                                  73
      IDNUM_TPRS(1)=0                                                     74
C SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER                                 74
      TPRS_SEC=1                                                          74
      CTPRS_SEC='SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER'                 74
C NUMBER OF PADROWS IN THE SECTOR                                         75
      TPRS_NROW=13                                                        75
      CTPRS_NROW='NUMBER OF PADROWS IN THE SECTOR'                        75
C TPC PADROW PITCH WIDTH                                                  76
      TPRS_PITCH=0.335                                                    76
      CTPRS_PITCH='TPC PADROW PITCH WIDTH'                                76
C TPC PADROW THICKNESS                                                    77
      TPRS_WIDTH=1.15                                                     77
      CTPRS_WIDTH='TPC PADROW THICKNESS'                                  77
C NUMBER OF PADRAWS IN A SUPERPADROW                                      78
      TPRS_SUPER=3                                                        78
      CTPRS_SUPER='NUMBER OF PADRAWS IN A SUPERPADROW'                    78
C NUMBER OF PADS IN ROW                                                   80
      TPRS_NPADS(1)=88                                                    80
      TPRS_NPADS(2)=96                                                    80
      TPRS_NPADS(3)=104                                                   80
      TPRS_NPADS(4)=112                                                   80
      TPRS_NPADS(5)=118                                                   80
      TPRS_NPADS(6)=126                                                   80
      TPRS_NPADS(7)=134                                                   80
      TPRS_NPADS(8)=142                                                   80
      TPRS_NPADS(9)=150                                                   80
      TPRS_NPADS(10)=158                                                  80
      TPRS_NPADS(11)=166                                                  80
      TPRS_NPADS(12)=174                                                  80
      TPRS_NPADS(13)=182                                                  80
      CTPRS_NPADS='NUMBER OF PADS IN ROW'                                 80
C TPC PADROW RADII                                                        83
      TPRS_RPADS(1)=60.0                                                  83
      TPRS_RPADS(2)=64.8                                                  83
      TPRS_RPADS(3)=69.6                                                  83
      TPRS_RPADS(4)=74.4                                                  83
      TPRS_RPADS(5)=79.2                                                  83
      TPRS_RPADS(6)=84.0                                                  83
      TPRS_RPADS(7)=88.8                                                  83
      TPRS_RPADS(8)=93.6                                                  83
      TPRS_RPADS(9)=98.8                                                  83
      TPRS_RPADS(10)=104.0                                                83
      TPRS_RPADS(11)=109.2                                                83
      TPRS_RPADS(12)=114.4                                                83
      TPRS_RPADS(13)=119.6                                                83
      CTPRS_RPADS='TPC PADROW RADII'                                      83
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPRS',    84
     *BPATH_TPRS,IDNUM_TPRS,1, MAP_TPRS,NAMES_TPRS,COMMEN_TPRS,           84
     *BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)                   84
C *                                                                       84
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPRS',    84
     *BPATH_TPRS,IDNUM_TPRS,1, MAP_TPRS,NAMES_TPRS,COMMEN_TPRS,           84
     *BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)                   84
C *                                                                       85
      CALL AGSBEGM(MODULE,IPRIN)                                          85
      IF (IPRIN.LT.0) RETURN                                              85
C Filling bank TPRS - SECTOR OF PADROWS                                   85
      BANK_TITLE='SECTOR OF PADROWS'                                      85
      BPATH_TPRS='TPRS*'                                                  85
      IDNUM_TPRS(1)=0                                                     86
C SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER                                 86
      TPRS_SEC=2                                                          86
      CTPRS_SEC='SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER'                 86
C NUMBER OF PADROWS IN OUTER SECTOR                                       87
      TPRS_NROW=32                                                        87
      CTPRS_NROW='NUMBER OF PADROWS IN OUTER SECTOR'                      87
C OUTER TPC PADROW PITCH WIDTH                                            88
      TPRS_PITCH=0.67                                                     88
      CTPRS_PITCH='OUTER TPC PADROW PITCH WIDTH'                          88
C OUTER TPC PADROW THICKNESS                                              89
      TPRS_WIDTH=1.95                                                     89
      CTPRS_WIDTH='OUTER TPC PADROW THICKNESS'                            89
C NUMBER OF PADROWS IN A SUPERPADROW                                      90
      TPRS_SUPER=1                                                        90
      CTPRS_SUPER='NUMBER OF PADROWS IN A SUPERPADROW'                    90
C NUMBER OF PADS IN ROW                                                   94
      TPRS_NPADS(1)=98                                                    94
      TPRS_NPADS(2)=100                                                   94
      TPRS_NPADS(3)=102                                                   94
      TPRS_NPADS(4)=104                                                   94
      TPRS_NPADS(5)=106                                                   94
      TPRS_NPADS(6)=106                                                   94
      TPRS_NPADS(7)=108                                                   94
      TPRS_NPADS(8)=110                                                   94
      TPRS_NPADS(9)=112                                                   94
      TPRS_NPADS(10)=112                                                  94
      TPRS_NPADS(11)=114                                                  94
      TPRS_NPADS(12)=116                                                  94
      TPRS_NPADS(13)=118                                                  94
      TPRS_NPADS(14)=120                                                  94
      TPRS_NPADS(15)=122                                                  94
      TPRS_NPADS(16)=122                                                  94
      TPRS_NPADS(17)=124                                                  94
      TPRS_NPADS(18)=126                                                  94
      TPRS_NPADS(19)=128                                                  94
      TPRS_NPADS(20)=128                                                  94
      TPRS_NPADS(21)=130                                                  94
      TPRS_NPADS(22)=132                                                  94
      TPRS_NPADS(23)=134                                                  94
      TPRS_NPADS(24)=136                                                  94
      TPRS_NPADS(25)=138                                                  94
      TPRS_NPADS(26)=138                                                  94
      TPRS_NPADS(27)=140                                                  94
      TPRS_NPADS(28)=142                                                  94
      TPRS_NPADS(29)=144                                                  94
      TPRS_NPADS(30)=144                                                  94
      TPRS_NPADS(31)=144                                                  94
      TPRS_NPADS(32)=144                                                  94
      CTPRS_NPADS='NUMBER OF PADS IN ROW'                                 94
C TPC PADROW RADII                                                        102
      TPRS_RPADS(1)=127.195                                               102
      TPRS_RPADS(2)=129.195                                               102
      TPRS_RPADS(3)=131.195                                               102
      TPRS_RPADS(4)=133.195                                               102
      TPRS_RPADS(5)=135.195                                               102
      TPRS_RPADS(6)=137.195                                               102
      TPRS_RPADS(7)=139.195                                               102
      TPRS_RPADS(8)=141.195                                               102
      TPRS_RPADS(9)=143.195                                               102
      TPRS_RPADS(10)=145.195                                              102
      TPRS_RPADS(11)=147.195                                              102
      TPRS_RPADS(12)=149.195                                              102
      TPRS_RPADS(13)=151.195                                              102
      TPRS_RPADS(14)=153.195                                              102
      TPRS_RPADS(15)=155.195                                              102
      TPRS_RPADS(16)=157.195                                              102
      TPRS_RPADS(17)=159.195                                              102
      TPRS_RPADS(18)=161.195                                              102
      TPRS_RPADS(19)=163.195                                              102
      TPRS_RPADS(20)=165.195                                              102
      TPRS_RPADS(21)=167.195                                              102
      TPRS_RPADS(22)=169.195                                              102
      TPRS_RPADS(23)=171.195                                              102
      TPRS_RPADS(24)=173.195                                              102
      TPRS_RPADS(25)=175.195                                              102
      TPRS_RPADS(26)=177.195                                              102
      TPRS_RPADS(27)=179.195                                              102
      TPRS_RPADS(28)=181.195                                              102
      TPRS_RPADS(29)=183.195                                              102
      TPRS_RPADS(30)=185.195                                              102
      TPRS_RPADS(31)=187.195                                              102
      TPRS_RPADS(32)=189.195                                              102
      CTPRS_RPADS='TPC PADROW RADII'                                      102
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPRS',    103
     *BPATH_TPRS,IDNUM_TPRS,1, MAP_TPRS,NAMES_TPRS,COMMEN_TPRS,           103
     *BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)                   103
C *                                                                       103
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPRS',    103
     *BPATH_TPRS,IDNUM_TPRS,1, MAP_TPRS,NAMES_TPRS,COMMEN_TPRS,           103
     *BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)                   103
C *                                                                       104
      CALL AGSBEGM(MODULE,IPRIN)                                          104
      IF (IPRIN.LT.0) RETURN                                              104
C Filling bank TECW - EC TRAPEZOID AND SUPPORT WHEEL                      104
      BANK_TITLE='EC TRAPEZOID AND SUPPORT WHEEL'                         104
      BPATH_TECW='TECW*'                                                  104
      IDNUM_TECW(1)=0                                                     105
C SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER                                 105
      TECW_SEC=1                                                          105
      CTECW_SEC='SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER'                 105
C AIR IN SUPPORT WHEEL - INNER WIDTH                                      106
      TECW_GAPWIDI=2.* 10.91                                              106
      CTECW_GAPWIDI='AIR IN SUPPORT WHEEL - INNER WIDTH'                  106
C AIR IN SUPPORT WHEEL - OUTER WIDTH                                      107
      TECW_GAPWIDO=2.* 27.56                                              107
      CTECW_GAPWIDO='AIR IN SUPPORT WHEEL - OUTER WIDTH'                  107
C AIR IN SUPPORT WHEEL - HEIGHT (DR)                                      108
      TECW_GAPHEIT=62.15                                                  108
      CTECW_GAPHEIT='AIR IN SUPPORT WHEEL - HEIGHT (DR)'                  108
C AIR IN SUPPORT WHEEL - CENTER RADIUS                                    109
      TECW_GAPRAD=87.0                                                    109
      CTECW_GAPRAD='AIR IN SUPPORT WHEEL - CENTER RADIUS'                 109
C OFFSET AIR GAP TO MAKE LIP                                              110
      TECW_GAPSHFT=1.4                                                    110
      CTECW_GAPSHFT='OFFSET AIR GAP TO MAKE LIP'                          110
C SECTOR WIDTH AT INNER RADIUS                                            111
      TECW_INWIDTH=2.* 13.75                                              111
      CTECW_INWIDTH='SECTOR WIDTH AT INNER RADIUS'                        111
C SECTOR WIDTH AT OUTER RADIUS                                            112
      TECW_OUWIDTH=2.* 32.38                                              112
      CTECW_OUWIDTH='SECTOR WIDTH AT OUTER RADIUS'                        112
C SECTOR RADIAL HEIGHT                                                    113
      TECW_HEIGHT=69.52                                                   113
      CTECW_HEIGHT='SECTOR RADIAL HEIGHT'                                 113
C PADPLANE THICKNESS (BOTH AL AND PCB)                                    114
      TECW_PPDEPTH=1.35+0.32                                              114
      CTECW_PPDEPTH='PADPLANE THICKNESS (BOTH AL AND PCB)'                114
C DEPTH OF OPENINGS IN ALUMINUM STRUCTURE                                 115
      TECW_ASDEPTH=6.99                                                   115
      CTECW_ASDEPTH='DEPTH OF OPENINGS IN ALUMINUM STRUCTURE'             115
C MWC GAP FROM GATED GRID TO PAD PLANE                                    116
      TECW_GGDEPTH=1.0                                                    116
      CTECW_GGDEPTH='MWC GAP FROM GATED GRID TO PAD PLANE'                116
C SENSITIVE MWC GAS GAP FULL THICKNESS                                    117
      TECW_MWCDEPTH=1.0                                                   117
      CTECW_MWCDEPTH='SENSITIVE MWC GAS GAP FULL THICKNESS'               117
C AL FRAME - BOUNDARY WIDTH                                               118
      TECW_BOUNDARY=3.74                                                  118
      CTECW_BOUNDARY='AL FRAME - BOUNDARY WIDTH'                          118
C SECTOR CENTER RADIUS (SET BY PRECISION HOLES)                           119
      TECW_RCENTER=86.669                                                 119
      CTECW_RCENTER='SECTOR CENTER RADIUS (SET BY PRECISION HOLES)'       119
C MWC SENSITIVE REGION INNER SIZE                                         120
      TECW_MWCINN=2*12.804                                                120
      CTECW_MWCINN='MWC SENSITIVE REGION INNER SIZE'                      120
C MWC SENSITIVE REGION OUTER SIZE                                         121
      TECW_MWCOUT=2*29.953                                                121
      CTECW_MWCOUT='MWC SENSITIVE REGION OUTER SIZE'                      121
C MWC SENSITIVE REGION HEIGHT (RADIAL)                                    122
      TECW_MWCHEI=64                                                      122
      CTECW_MWCHEI='MWC SENSITIVE REGION HEIGHT (RADIAL)'                 122
C SENSITIVE REGION CENTER POSITION                                        123
      TECW_MWCCENT=-1.669                                                 123
      CTECW_MWCCENT='SENSITIVE REGION CENTER POSITION'                    123
C NUMBER OF MWC SENSITIVE WIRES                                           124
      TECW_MWCNWIR=160                                                    124
      CTECW_MWCNWIR='NUMBER OF MWC SENSITIVE WIRES'                       124
C NUMBER OF AIR GAPS IN AL                                                125
      TECW_N=5                                                            125
      CTECW_N='NUMBER OF AIR GAPS IN AL'                                  125
C NUMBER OF EXTRA ALUMINUM SUPPORT PIECES                                 126
      TECW_NEX=0                                                          126
      CTECW_NEX='NUMBER OF EXTRA ALUMINUM SUPPORT PIECES'                 126
C POSITIONS OF AIR GAPS                                                   127
      TECW_Z(1)=6.67                                                      127
      TECW_Z(2)=18.22                                                     127
      TECW_Z(3)=34.22                                                     127
      TECW_Z(4)=50.22                                                     127
      TECW_Z(5)=62.65                                                     127
      CTECW_Z='POSITIONS OF AIR GAPS'                                     127
C SIZE OF AIR GAPS                                                        128
      TECW_DZ(1)=6.13                                                     128
      TECW_DZ(2)=15.05                                                    128
      TECW_DZ(3)=15.05                                                    128
      TECW_DZ(4)=15.05                                                    128
      TECW_DZ(5)=7.09                                                     128
      CTECW_DZ='SIZE OF AIR GAPS'                                         128
C X POSITIONS OF EXTRA ALUMINUM                                           129
      TECW_XEX(1)=0                                                       129
      TECW_XEX(2)=0                                                       129
      TECW_XEX(3)=0                                                       129
      TECW_XEX(4)=0                                                       129
      TECW_XEX(5)=0                                                       129
      CTECW_XEX='X POSITIONS OF EXTRA ALUMINUM'                           129
C Z POSITIONS OF EXTRA ALUMINUM                                           130
      TECW_ZEX(1)=0                                                       130
      TECW_ZEX(2)=0                                                       130
      TECW_ZEX(3)=0                                                       130
      TECW_ZEX(4)=0                                                       130
      TECW_ZEX(5)=0                                                       130
      CTECW_ZEX='Z POSITIONS OF EXTRA ALUMINUM'                           130
C X-THICKNESS OF EXTRA ALUMINUM                                           131
      TECW_DXEX(1)=0                                                      131
      TECW_DXEX(2)=0                                                      131
      TECW_DXEX(3)=0                                                      131
      TECW_DXEX(4)=0                                                      131
      TECW_DXEX(5)=0                                                      131
      CTECW_DXEX='X-THICKNESS OF EXTRA ALUMINUM'                          131
C Z-THICKNESS OF EXTRA ALUMINUM                                           132
      TECW_DZEX(1)=0                                                      132
      TECW_DZEX(2)=0                                                      132
      TECW_DZEX(3)=0                                                      132
      TECW_DZEX(4)=0                                                      132
      TECW_DZEX(5)=0                                                      132
      CTECW_DZEX='Z-THICKNESS OF EXTRA ALUMINUM'                          132
C NUMBER OF PC PLANES PER AIR GAP                                         133
      TECW_NHPLANE(1)=1                                                   133
      TECW_NHPLANE(2)=4                                                   133
      TECW_NHPLANE(3)=4                                                   133
      TECW_NHPLANE(4)=4                                                   133
      TECW_NHPLANE(5)=2                                                   133
      CTECW_NHPLANE='NUMBER OF PC PLANES PER AIR GAP'                     133
C WHEEL BOLT LIP ALONG BEAM                                               134
      TECW_WHLIPTHK=1.0                                                   134
      CTECW_WHLIPTHK='WHEEL BOLT LIP ALONG BEAM'                          134
C WHEEL LIP IN PHI                                                        135
      TECW_WHLIPWID=2.8                                                   135
      CTECW_WHLIPWID='WHEEL LIP IN PHI'                                   135
C BLOCKS ON WHEEL RADIAL SIZE                                             136
      TECW_WHBLKLEN=5.5                                                   136
      CTECW_WHBLKLEN='BLOCKS ON WHEEL RADIAL SIZE'                        136
C BOLT BLOCKS AT +- 18 IN WHL                                             137
      TECW_WHBLKPOS=18.0                                                  137
      CTECW_WHBLKPOS='BOLT BLOCKS AT +- 18 IN WHL'                        137
C NO BLOCK BOTTOM INNER WHEEL                                             138
      TECW_WHBLKIN=0                                                      138
      CTECW_WHBLKIN='NO BLOCK BOTTOM INNER WHEEL'                         138
C PC CARD WIDTH AND THICKNESS                                             139
      TECW_CARDW=7.33                                                     139
      CTECW_CARDW='PC CARD WIDTH AND THICKNESS'                           139
C PC CARD WIDTH AND THICKNESS                                             140
      TECW_CARDTH=0.19                                                    140
      CTECW_CARDTH='PC CARD WIDTH AND THICKNESS'                          140
C COOLING AL WIDTH AND THICKNESS                                          141
      TECW_COOLW=5.7                                                      141
      CTECW_COOLW='COOLING AL WIDTH AND THICKNESS'                        141
C COOLING AL WIDTH AND THICKNESS                                          142
      TECW_COOLTH=0.33                                                    142
      CTECW_COOLTH='COOLING AL WIDTH AND THICKNESS'                       142
C CARD AND COOLING OFFSETS FROM                                           143
      TECW_CARDOFF=1.77                                                   143
      CTECW_CARDOFF='CARD AND COOLING OFFSETS FROM'                       143
C CARD AND COOLING OFFSETS FROM                                           144
      TECW_COOLOFF=1.50                                                   144
      CTECW_COOLOFF='CARD AND COOLING OFFSETS FROM'                       144
C SLOT WIDTH AND RADIUS                                                   145
      TECW_SLOTW=4.70                                                     145
      CTECW_SLOTW='SLOT WIDTH AND RADIUS'                                 145
C SLOT WIDTH AND RADIUS                                                   146
      TECW_SLOTRAD=0.95                                                   146
      CTECW_SLOTRAD='SLOT WIDTH AND RADIUS'                               146
C WATER PIPE ALONG BEAM DIR                                               147
      TECW_PIPETHK=1.26                                                   147
      CTECW_PIPETHK='WATER PIPE ALONG BEAM DIR'                           147
C WATER PIPE RADIAL                                                       148
      TECW_PIPEHT=1.9                                                     148
      CTECW_PIPEHT='WATER PIPE RADIAL'                                    148
C WATER MANIFOLD ALONG BEAM                                               149
      TECW_MANITHK=4.5                                                    149
      CTECW_MANITHK='WATER MANIFOLD ALONG BEAM'                           149
C WATER MANIFOLD IN PHI                                                   150
      TECW_MANIWID=2.2                                                    150
      CTECW_MANIWID='WATER MANIFOLD IN PHI'                               150
C TANGENT OF 15 DEG                                                       151
      TECW_TAN15=.26795                                                   151
      CTECW_TAN15='TANGENT OF 15 DEG'                                     151
C CLEARANCE FOR RDO BOARDSIN PHI                                          152
      TECW_CLEARANCE=1.0                                                  152
      CTECW_CLEARANCE='CLEARANCE FOR RDO BOARDSIN PHI'                    152
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TECW',    153
     *BPATH_TECW,IDNUM_TECW,1, MAP_TECW,NAMES_TECW,COMMEN_TECW,           153
     *BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)                   153
C *                                                                       153
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TECW',    153
     *BPATH_TECW,IDNUM_TECW,1, MAP_TECW,NAMES_TECW,COMMEN_TECW,           153
     *BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)                   153
C *                                                                       154
      CALL AGSBEGM(MODULE,IPRIN)                                          154
      IF (IPRIN.LT.0) RETURN                                              154
C Filling bank TECW - ENDCAP TRAPEZOID AND SUPPORT WHEEL                  154
      BANK_TITLE='ENDCAP TRAPEZOID AND SUPPORT WHEEL'                     154
      BPATH_TECW='TECW*'                                                  154
      IDNUM_TECW(1)=0                                                     155
C SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER                                 155
      TECW_SEC=2                                                          155
      CTECW_SEC='SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER'                 155
C AIR IN SUPPORT WHEEL - INNER WIDTH                                      156
      TECW_GAPWIDI=2.* 28.92                                              156
      CTECW_GAPWIDI='AIR IN SUPPORT WHEEL - INNER WIDTH'                  156
C AIR IN SUPPORT WHEEL - OUTER WIDTH                                      157
      TECW_GAPWIDO=2.* 46.74                                              157
      CTECW_GAPWIDO='AIR IN SUPPORT WHEEL - OUTER WIDTH'                  157
C AIR IN SUPPORT WHEEL - HEIGHT (DR)                                      158
      TECW_GAPHEIT=65.0                                                   158
      CTECW_GAPHEIT='AIR IN SUPPORT WHEEL - HEIGHT (DR)'                  158
C AIR IN SUPPORT WHEEL - RADIUS                                           159
      TECW_GAPRAD=158.0                                                   159
      CTECW_GAPRAD='AIR IN SUPPORT WHEEL - RADIUS'                        159
C OFFSET OF AIR GAP LIP                                                   160
      TECW_GAPSHFT=0.0                                                    160
      CTECW_GAPSHFT='OFFSET OF AIR GAP LIP'                               160
C SECTOR WIDTH AT INNER RADIUS                                            161
      TECW_INWIDTH=64.92                                                  161
      CTECW_INWIDTH='SECTOR WIDTH AT INNER RADIUS'                        161
C SECTOR WIDTH AT OUTER RADIUS                                            162
      TECW_OUWIDTH=103.25                                                 162
      CTECW_OUWIDTH='SECTOR WIDTH AT OUTER RADIUS'                        162
C SECTOR RADIAL HEIGHT                                                    163
      TECW_HEIGHT=71.51                                                   163
      CTECW_HEIGHT='SECTOR RADIAL HEIGHT'                                 163
C PADPLANE THICKNESS (BOTH AL AND PCB)                                    164
      TECW_PPDEPTH=0.95+0.32                                              164
      CTECW_PPDEPTH='PADPLANE THICKNESS (BOTH AL AND PCB)'                164
C DEPTH OF OPENINGS IN ALUMINUM STRUCTURE                                 165
      TECW_ASDEPTH=6.99                                                   165
      CTECW_ASDEPTH='DEPTH OF OPENINGS IN ALUMINUM STRUCTURE'             165
C MWC GAP FROM GATED GRID TO PAD PLANE                                    166
      TECW_GGDEPTH=1.4                                                    166
      CTECW_GGDEPTH='MWC GAP FROM GATED GRID TO PAD PLANE'                166
C SENSITIVE MWC GAS GAP FULL THICKNESS                                    167
      TECW_MWCDEPTH=1.4                                                   167
      CTECW_MWCDEPTH='SENSITIVE MWC GAS GAP FULL THICKNESS'               167
C AL FRAME - BOUNDARY WIDTH                                               168
      TECW_BOUNDARY=3.74                                                  168
      CTECW_BOUNDARY='AL FRAME - BOUNDARY WIDTH'                          168
C SECTOR CENTER RADIUS (SET BY PRECISION HOLES)                           169
      TECW_RCENTER=157.488                                                169
      CTECW_RCENTER='SECTOR CENTER RADIUS (SET BY PRECISION HOLES)'       169
C MWC SENSITIVE REGION INNER SIZE                                         170
      TECW_MWCINN=2*32.202                                                170
      CTECW_MWCINN='MWC SENSITIVE REGION INNER SIZE'                      170
C MWC SENSITIVE REGION OUTER SIZE                                         171
      TECW_MWCOUT=2*49.351                                                171
      CTECW_MWCOUT='MWC SENSITIVE REGION OUTER SIZE'                      171
C MWC SENSITIVE REGION HEIGHT (RADIAL)                                    172
      TECW_MWCHEI=64                                                      172
      CTECW_MWCHEI='MWC SENSITIVE REGION HEIGHT (RADIAL)'                 172
C SENSITIVE REGION CENTER POSITION                                        173
      TECW_MWCCENT=-0.093                                                 173
      CTECW_MWCCENT='SENSITIVE REGION CENTER POSITION'                    173
C NUMBER OF MWC SENSITIVE WIRES                                           174
      TECW_MWCNWIR=160                                                    174
      CTECW_MWCNWIR='NUMBER OF MWC SENSITIVE WIRES'                       174
C NUMBER OF AIR GAPS IN AL                                                175
      TECW_N=8                                                            175
      CTECW_N='NUMBER OF AIR GAPS IN AL'                                  175
C NUMBER OF EXTRA ALUMINUM SUPPORT PIECES                                 176
      TECW_NEX=5                                                          176
      CTECW_NEX='NUMBER OF EXTRA ALUMINUM SUPPORT PIECES'                 176
C OPENING POSITIONS                                                       178
      TECW_Z(1)=8.216                                                     178
      TECW_Z(2)=16.217                                                    178
      TECW_Z(3)=24.218                                                    178
      TECW_Z(4)=32.219                                                    178
      TECW_Z(5)=40.220                                                    178
      TECW_Z(6)=48.221                                                    178
      TECW_Z(7)=56.222                                                    178
      TECW_Z(8)=64.143                                                    178
      CTECW_Z='OPENING POSITIONS'                                         178
C OPENING SIZE                                                            180
      TECW_DZ(1)=7.05                                                     180
      TECW_DZ(2)=7.05                                                     180
      TECW_DZ(3)=7.05                                                     180
      TECW_DZ(4)=7.05                                                     180
      TECW_DZ(5)=7.05                                                     180
      TECW_DZ(6)=7.05                                                     180
      TECW_DZ(7)=7.05                                                     180
      TECW_DZ(8)=6.89                                                     180
      CTECW_DZ='OPENING SIZE'                                             180
C X POSITIONS                                                             181
      TECW_XEX(1)=0.                                                      181
      TECW_XEX(2)=0.                                                      181
      TECW_XEX(3)=0.                                                      181
      TECW_XEX(4)=-21.907                                                 181
      TECW_XEX(5)=21.907                                                  181
      CTECW_XEX='X POSITIONS'                                             181
C Z POSITIONS                                                             182
      TECW_ZEX(1)=6.69                                                    182
      TECW_ZEX(2)=32.22                                                   182
      TECW_ZEX(3)=40.22                                                   182
      TECW_ZEX(4)=56.22                                                   182
      TECW_ZEX(5)=56.22                                                   182
      CTECW_ZEX='Z POSITIONS'                                             182
C X-THICKNESS                                                             183
      TECW_DXEX(1)=2.54                                                   183
      TECW_DXEX(2)=0.476                                                  183
      TECW_DXEX(3)=0.476                                                  183
      TECW_DXEX(4)=3.175                                                  183
      TECW_DXEX(5)=3.175                                                  183
      CTECW_DXEX='X-THICKNESS'                                            183
C Z-THICKNESS                                                             184
      TECW_DZEX(1)=4.00                                                   184
      TECW_DZEX(2)=7.05                                                   184
      TECW_DZEX(3)=7.05                                                   184
      TECW_DZEX(4)=7.05                                                   184
      TECW_DZEX(5)=7.05                                                   184
      CTECW_DZEX='Z-THICKNESS'                                            184
C NUMBER OF PC PLANES PER AIR GAP                                         185
      TECW_NHPLANE(1)=2                                                   185
      TECW_NHPLANE(2)=2                                                   185
      TECW_NHPLANE(3)=2                                                   185
      TECW_NHPLANE(4)=2                                                   185
      TECW_NHPLANE(5)=2                                                   185
      TECW_NHPLANE(6)=2                                                   185
      TECW_NHPLANE(7)=2                                                   185
      TECW_NHPLANE(8)=2                                                   185
      CTECW_NHPLANE='NUMBER OF PC PLANES PER AIR GAP'                     185
C WHEEL BOLT LIP ALONG BEAM                                               186
      TECW_WHLIPTHK=1.0                                                   186
      CTECW_WHLIPTHK='WHEEL BOLT LIP ALONG BEAM'                          186
C WHEEL LIP IN PHI                                                        187
      TECW_WHLIPWID=2.8                                                   187
      CTECW_WHLIPWID='WHEEL LIP IN PHI'                                   187
C BLOCKS ON WHEEL RADIAL SIZE                                             188
      TECW_WHBLKLEN=5.5                                                   188
      CTECW_WHBLKLEN='BLOCKS ON WHEEL RADIAL SIZE'                        188
C BOLT BLOCKS AT +- 18 IN WHL                                             189
      TECW_WHBLKPOS=18.0                                                  189
      CTECW_WHBLKPOS='BOLT BLOCKS AT +- 18 IN WHL'                        189
C IS BLOCK BOTTOM OUTER WHEEL                                             190
      TECW_WHBLKIN=1                                                      190
      CTECW_WHBLKIN='IS BLOCK BOTTOM OUTER WHEEL'                         190
C PC CARD WIDTH AND THICKNESS                                             191
      TECW_CARDW=7.33                                                     191
      CTECW_CARDW='PC CARD WIDTH AND THICKNESS'                           191
C PC CARD WIDTH AND THICKNESS                                             192
      TECW_CARDTH=0.19                                                    192
      CTECW_CARDTH='PC CARD WIDTH AND THICKNESS'                          192
C COOLING AL WIDTH AND THICKNESS                                          193
      TECW_COOLW=5.7                                                      193
      CTECW_COOLW='COOLING AL WIDTH AND THICKNESS'                        193
C COOLING AL WIDTH AND THICKNESS                                          194
      TECW_COOLTH=0.33                                                    194
      CTECW_COOLTH='COOLING AL WIDTH AND THICKNESS'                       194
C CARD AND COOLING OFFSETS FROM                                           195
      TECW_CARDOFF=1.77                                                   195
      CTECW_CARDOFF='CARD AND COOLING OFFSETS FROM'                       195
C CARD AND COOLING OFFSETS FROM                                           196
      TECW_COOLOFF=1.50                                                   196
      CTECW_COOLOFF='CARD AND COOLING OFFSETS FROM'                       196
C SLOT WIDTH AND RADIUS                                                   197
      TECW_SLOTW=4.70                                                     197
      CTECW_SLOTW='SLOT WIDTH AND RADIUS'                                 197
C SLOT WIDTH AND RADIUS                                                   198
      TECW_SLOTRAD=0.95                                                   198
      CTECW_SLOTRAD='SLOT WIDTH AND RADIUS'                               198
C WATER PIPE ALONG BEAM DIR                                               199
      TECW_PIPETHK=1.26                                                   199
      CTECW_PIPETHK='WATER PIPE ALONG BEAM DIR'                           199
C WATER PIPE RADIAL                                                       200
      TECW_PIPEHT=1.9                                                     200
      CTECW_PIPEHT='WATER PIPE RADIAL'                                    200
C WATER MANIFOLD ALONG BEAM                                               201
      TECW_MANITHK=4.5                                                    201
      CTECW_MANITHK='WATER MANIFOLD ALONG BEAM'                           201
C WATER MANIFOLD IN PHI                                                   202
      TECW_MANIWID=2.2                                                    202
      CTECW_MANIWID='WATER MANIFOLD IN PHI'                               202
C TANGENT OF 15 DEG                                                       203
      TECW_TAN15=.26795                                                   203
      CTECW_TAN15='TANGENT OF 15 DEG'                                     203
C CLEARANCE FOR RDO BOARDSIN PHI                                          204
      TECW_CLEARANCE=1.0                                                  204
      CTECW_CLEARANCE='CLEARANCE FOR RDO BOARDSIN PHI'                    204
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TECW',    205
     *BPATH_TECW,IDNUM_TECW,1, MAP_TECW,NAMES_TECW,COMMEN_TECW,           205
     *BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)                   205
C *                                                                       205
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TECW',    205
     *BPATH_TECW,IDNUM_TECW,1, MAP_TECW,NAMES_TECW,COMMEN_TECW,           205
     *BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)                   205
C *                                                                       206
      CALL AGSBEGM(MODULE,IPRIN)                                          206
      IF (IPRIN.LT.0) RETURN                                              206
C Filling bank TROV - CARDS IN ROW 1                                      206
      BANK_TITLE='CARDS IN ROW 1'                                         206
      BPATH_TROV='TROV*'                                                  206
      IDNUM_TROV(1)=0                                                     207
C INNER                                                                   207
      TROV_SEC=1                                                          207
      CTROV_SEC='INNER'                                                   207
C NO OF HOLES/PLANE                                                       209
      TROV_NHP(1)=0                                                       209
      TROV_NHP(2)=2                                                       209
      TROV_NHP(3)=2                                                       209
      TROV_NHP(4)=3                                                       209
      TROV_NHP(5)=3                                                       209
      TROV_NHP(6)=3                                                       209
      TROV_NHP(7)=3                                                       209
      TROV_NHP(8)=4                                                       209
      TROV_NHP(9)=4                                                       209
      TROV_NHP(10)=4                                                      209
      TROV_NHP(11)=4                                                      209
      TROV_NHP(12)=5                                                      209
      TROV_NHP(13)=2                                                      209
      TROV_NHP(14)=5                                                      209
      TROV_NHP(15)=5                                                      209
      TROV_NHP(16)=6                                                      209
      CTROV_NHP='NO OF HOLES/PLANE'                                       209
      TROV_HX(1,1)=9*0.0                                                  210
      TROV_HX(1,2)=-5.17                                                  210
      TROV_HX(2,2)=5.17                                                   211
      TROV_HX(3,2)=7*0.0                                                  211
      TROV_HX(1,3)=-6.21                                                  211
      TROV_HX(2,3)=6.21                                                   212
      TROV_HX(3,3)=7*0.0                                                  212
      TROV_HX(1,4)=-7.79                                                  212
      TROV_HX(2,4)=0.0                                                    213
      TROV_HX(3,4)=7.79                                                   213
      TROV_HX(4,4)=6*0.0                                                  213
      TROV_HX(1,5)=-8.34                                                  213
      TROV_HX(2,5)=0.0                                                    214
      TROV_HX(3,5)=8.34                                                   214
      TROV_HX(4,5)=6*0.0                                                  214
      TROV_HX(1,6)=-9.38                                                  214
      TROV_HX(2,6)=0.0                                                    215
      TROV_HX(3,6)=9.38                                                   215
      TROV_HX(4,6)=6*0.0                                                  215
      TROV_HX(1,7)=-10.50                                                 215
      TROV_HX(2,7)=0.0                                                    217
      TROV_HX(3,7)=10.50                                                  217
      TROV_HX(4,7)=6*0.0                                                  217
      TROV_HX(1,8)=-11.97                                                 217
      TROV_HX(2,8)=-4.05                                                  219
      TROV_HX(3,8)=4.05                                                   219
      TROV_HX(4,8)=11.97                                                  219
      TROV_HX(5,8)=5*0.0                                                  219
      TROV_HX(1,9)=-12.70                                                 219
      TROV_HX(2,9)=-4.32                                                  221
      TROV_HX(3,9)=4.32                                                   221
      TROV_HX(4,9)=12.70                                                  221
      TROV_HX(5,9)=5*0.0                                                  221
      TROV_HX(1,10)=-13.70                                                221
      TROV_HX(2,10)=-4.63                                                 223
      TROV_HX(3,10)=4.63                                                  223
      TROV_HX(4,10)=13.70                                                 223
      TROV_HX(5,10)=5*0.0                                                 223
      TROV_HX(1,11)=-14.67                                                223
      TROV_HX(2,11)=-4.98                                                 225
      TROV_HX(3,11)=4.98                                                  225
      TROV_HX(4,11)=14.67                                                 225
      TROV_HX(5,11)=5*0.0                                                 225
      TROV_HX(1,12)=-15.86                                                225
      TROV_HX(2,12)=-7.91                                                 226
      TROV_HX(3,12)=0.0                                                   226
      TROV_HX(4,12)=7.91                                                  226
      TROV_HX(5,12)=15.86                                                 226
      TROV_HX(6,12)=4*0.0                                                 226
      TROV_HX(1,13)=-16.95                                                226
      TROV_HX(2,13)=16.95                                                 228
      TROV_HX(3,13)=7*0.0                                                 228
      TROV_HX(1,14)=-18.37                                                228
      TROV_HX(2,14)=-8.99                                                 230
      TROV_HX(3,14)=0.0                                                   230
      TROV_HX(4,14)=8.99                                                  230
      TROV_HX(5,14)=18.37                                                 230
      TROV_HX(6,14)=4*0.0                                                 230
      TROV_HX(1,15)=-19.11                                                230
      TROV_HX(2,15)=-9.76                                                 233
      TROV_HX(3,15)=0.0                                                   233
      TROV_HX(4,15)=9.76                                                  233
      TROV_HX(5,15)=19.11                                                 233
      TROV_HX(6,15)=4*0.0                                                 233
      TROV_HX(1,16)=-20.15                                                233
      TROV_HX(2,16)=-12.08                                                233
      TROV_HX(3,16)=-4.05                                                 233
      TROV_HX(4,16)=4.05                                                  233
      TROV_HX(5,16)=12.08                                                 233
      TROV_HX(6,16)=20.15                                                 233
      TROV_HX(7,16)=3*0.0                                                 233
C X OF HOLES                                                              233
      CTROV_HX='X OF HOLES'                                               233
C CARDS, FIRST ROW INNER SECTOR                                           234
      TROV_OFFCARDINOUT=-0.9                                              234
      CTROV_OFFCARDINOUT='CARDS, FIRST ROW INNER SECTOR'                  234
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TROV',    235
     *BPATH_TROV,IDNUM_TROV,1, MAP_TROV,NAMES_TROV,COMMEN_TROV,           235
     *BANK_TROV,LEN_TROV,LLEN_TROV,LINK_TROV,FLAG_TROV)                   235
C *                                                                       235
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TROV',    235
     *BPATH_TROV,IDNUM_TROV,1, MAP_TROV,NAMES_TROV,COMMEN_TROV,           235
     *BANK_TROV,LEN_TROV,LLEN_TROV,LINK_TROV,FLAG_TROV)                   235
C *                                                                       236
      CALL AGSBEGM(MODULE,IPRIN)                                          236
      IF (IPRIN.LT.0) RETURN                                              236
C Filling bank TROV - HOLES AND CARDS IN ROW 1                            236
      BANK_TITLE='HOLES AND CARDS IN ROW 1'                               236
      BPATH_TROV='TROV*'                                                  236
      IDNUM_TROV(1)=0                                                     237
C OUTER                                                                   237
      TROV_SEC=2                                                          237
      CTROV_SEC='OUTER'                                                   237
C NUMBER OF HOLES / PLANE                                                 239
      TROV_NHP(1)=6                                                       239
      TROV_NHP(2)=7                                                       239
      TROV_NHP(3)=7                                                       239
      TROV_NHP(4)=7                                                       239
      TROV_NHP(5)=7                                                       239
      TROV_NHP(6)=7                                                       239
      TROV_NHP(7)=8                                                       239
      TROV_NHP(8)=8                                                       239
      TROV_NHP(9)=8                                                       239
      TROV_NHP(10)=8                                                      239
      TROV_NHP(11)=8                                                      239
      TROV_NHP(12)=9                                                      239
      TROV_NHP(13)=9                                                      239
      TROV_NHP(14)=9                                                      239
      TROV_NHP(15)=9                                                      239
      TROV_NHP(16)=9                                                      239
      CTROV_NHP='NUMBER OF HOLES / PLANE'                                 239
      TROV_HX(1,1)=-23.70                                                 242
      TROV_HX(2,1)=-15.77                                                 242
      TROV_HX(3,1)=-7.72                                                  242
      TROV_HX(4,1)=7.72                                                   242
      TROV_HX(5,1)=15.77                                                  242
      TROV_HX(6,1)=23.70                                                  242
      TROV_HX(7,1)=3*0.0                                                  242
      TROV_HX(1,2)=-24.15                                                 242
      TROV_HX(2,2)=-16.25                                                 244
      TROV_HX(3,2)=-8.10                                                  244
      TROV_HX(4,2)=0.00                                                   244
      TROV_HX(5,2)=8.10                                                   244
      TROV_HX(6,2)=16.25                                                  244
      TROV_HX(7,2)=24.15                                                  244
      TROV_HX(8,2)=2*0.0                                                  244
      TROV_HX(1,3)=-25.48                                                 244
      TROV_HX(2,3)=-17.47                                                 246
      TROV_HX(3,3)=-8.71                                                  246
      TROV_HX(4,3)=0.00                                                   246
      TROV_HX(5,3)=8.71                                                   246
      TROV_HX(6,3)=17.47                                                  246
      TROV_HX(7,3)=25.48                                                  246
      TROV_HX(8,3)=2*0.0                                                  246
      TROV_HX(1,4)=-25.48                                                 246
      TROV_HX(2,4)=-17.47                                                 248
      TROV_HX(3,4)=-8.71                                                  248
      TROV_HX(4,4)=0.00                                                   248
      TROV_HX(5,4)=8.71                                                   248
      TROV_HX(6,4)=17.47                                                  248
      TROV_HX(7,4)=25.48                                                  248
      TROV_HX(8,4)=2*0.0                                                  248
      TROV_HX(1,5)=-28.19                                                 248
      TROV_HX(2,5)=-19.43                                                 250
      TROV_HX(3,5)=-10.81                                                 250
      TROV_HX(4,5)=0.00                                                   250
      TROV_HX(5,5)=10.81                                                  250
      TROV_HX(6,5)=19.43                                                  250
      TROV_HX(7,5)=28.19                                                  250
      TROV_HX(8,5)=2*0.0                                                  250
      TROV_HX(1,6)=-28.19                                                 250
      TROV_HX(2,6)=-19.43                                                 252
      TROV_HX(3,6)=-10.81                                                 252
      TROV_HX(4,6)=0.00                                                   252
      TROV_HX(5,6)=10.81                                                  252
      TROV_HX(6,6)=19.43                                                  252
      TROV_HX(7,6)=28.19                                                  252
      TROV_HX(8,6)=2*0.0                                                  252
      TROV_HX(1,7)=-30.24                                                 252
      TROV_HX(2,7)=-21.51                                                 254
      TROV_HX(3,7)=-13.54                                                 254
      TROV_HX(4,7)=-5.37                                                  254
      TROV_HX(5,7)=5.37                                                   254
      TROV_HX(6,7)=13.54                                                  254
      TROV_HX(7,7)=21.51                                                  254
      TROV_HX(8,7)=30.24                                                  254
      TROV_HX(9,7)=0.0                                                    254
      TROV_HX(1,8)=-30.24                                                 254
      TROV_HX(2,8)=-21.51                                                 256
      TROV_HX(3,8)=-13.54                                                 256
      TROV_HX(4,8)=-5.37                                                  256
      TROV_HX(5,8)=5.37                                                   256
      TROV_HX(6,8)=13.54                                                  256
      TROV_HX(7,8)=21.51                                                  256
      TROV_HX(8,8)=30.24                                                  256
      TROV_HX(9,8)=1*0.0                                                  256
      TROV_HX(1,9)=-32.23                                                 256
      TROV_HX(2,9)=-24.17                                                 258
      TROV_HX(3,9)=-16.09                                                 258
      TROV_HX(4,9)=-5.42                                                  258
      TROV_HX(5,9)=5.42                                                   258
      TROV_HX(6,9)=16.09                                                  258
      TROV_HX(7,9)=24.17                                                  258
      TROV_HX(8,9)=32.23                                                  258
      TROV_HX(9,9)=0.0                                                    258
      TROV_HX(1,10)=-32.23                                                258
      TROV_HX(2,10)=-24.17                                                260
      TROV_HX(3,10)=-16.09                                                260
      TROV_HX(4,10)=-5.42                                                 260
      TROV_HX(5,10)=5.42                                                  260
      TROV_HX(6,10)=16.09                                                 260
      TROV_HX(7,10)=24.17                                                 260
      TROV_HX(8,10)=32.23                                                 260
      TROV_HX(9,10)=1*0.0                                                 260
      TROV_HX(1,11)=-34.30                                                260
      TROV_HX(2,11)=-25.86                                                263
      TROV_HX(3,11)=-16.14                                                263
      TROV_HX(4,11)=-8.10                                                 263
      TROV_HX(5,11)=8.10                                                  263
      TROV_HX(6,11)=16.14                                                 263
      TROV_HX(7,11)=25.86                                                 263
      TROV_HX(8,11)=34.30                                                 263
      TROV_HX(9,11)=0.0                                                   263
      TROV_HX(1,12)=-34.30                                                263
      TROV_HX(2,12)=-25.86                                                265
      TROV_HX(3,12)=-16.14                                                265
      TROV_HX(4,12)=-8.10                                                 265
      TROV_HX(5,12)=0.00                                                  265
      TROV_HX(6,12)=8.10                                                  265
      TROV_HX(7,12)=16.14                                                 265
      TROV_HX(8,12)=25.86                                                 265
      TROV_HX(9,12)=34.30                                                 265
      TROV_HX(10,12)=1*0.0                                                265
      TROV_HX(1,13)=-36.16                                                265
      TROV_HX(2,13)=-28.12                                                268
      TROV_HX(3,13)=-16.14                                                268
      TROV_HX(4,13)=-8.10                                                 268
      TROV_HX(5,13)=0.00                                                  268
      TROV_HX(6,13)=8.10                                                  268
      TROV_HX(7,13)=16.14                                                 268
      TROV_HX(8,13)=28.12                                                 268
      TROV_HX(9,13)=36.16                                                 268
      TROV_HX(1,14)=-36.16                                                268
      TROV_HX(2,14)=-28.12                                                270
      TROV_HX(3,14)=-16.14                                                270
      TROV_HX(4,14)=-8.10                                                 270
      TROV_HX(5,14)=0.00                                                  270
      TROV_HX(6,14)=8.10                                                  270
      TROV_HX(7,14)=16.14                                                 270
      TROV_HX(8,14)=28.12                                                 270
      TROV_HX(9,14)=36.16                                                 270
      TROV_HX(10,14)=18*0.0                                               270
      TROV_HX(1,15)=-38.37                                                270
      TROV_HX(2,15)=-29.47                                                273
      TROV_HX(3,15)=-20.76                                                273
      TROV_HX(4,15)=-10.61                                                273
      TROV_HX(5,15)=0.00                                                  273
      TROV_HX(6,15)=10.61                                                 273
      TROV_HX(7,15)=20.76                                                 273
      TROV_HX(8,15)=29.47                                                 273
      TROV_HX(9,15)=38.37                                                 273
      TROV_HX(1,16)=-38.37                                                273
      TROV_HX(2,16)=-29.47                                                273
      TROV_HX(3,16)=-20.76                                                273
      TROV_HX(4,16)=-10.61                                                273
      TROV_HX(5,16)=0.00                                                  273
      TROV_HX(6,16)=10.61                                                 273
      TROV_HX(7,16)=20.76                                                 273
      TROV_HX(8,16)=29.47                                                 273
      TROV_HX(9,16)=38.37                                                 273
C X OF HOLES                                                              273
      CTROV_HX='X OF HOLES'                                               273
C CARD NO SPECIAL CASE                                                    274
      TROV_OFFCARDINOUT=0.0                                               274
      CTROV_OFFCARDINOUT='CARD NO SPECIAL CASE'                           274
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TROV',    275
     *BPATH_TROV,IDNUM_TROV,1, MAP_TROV,NAMES_TROV,COMMEN_TROV,           275
     *BANK_TROV,LEN_TROV,LLEN_TROV,LINK_TROV,FLAG_TROV)                   275
C *                                                                       275
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TROV',    275
     *BPATH_TROV,IDNUM_TROV,1, MAP_TROV,NAMES_TROV,COMMEN_TROV,           275
     *BANK_TROV,LEN_TROV,LLEN_TROV,LINK_TROV,FLAG_TROV)                   275
C use default TPCG bank                                                   276
      BPATH_TPCG='TPCG*'                                                  276
      IDNUM_TPCG(1)=0                                                     276
      CALL AGDGETP('TPCEGEO1','*',-999,'TPCG',' ',BANK_TPCG, BPATH_TPCG,  276
     *IDNUM_TPCG,1,MAP_TPCG,NAMES_TPCG,BANK_TPCG,LEN_TPCG,LLEN_TPCG,      276
     *LINK_TPCG,FLAG_TPCG)                                                276
C GAS PLUS ENDCAPS                                                        277
      TOFCLENG = TPCG_LENGTH-2*TPCG_WHEELTHK-2*TRDO_RDOVTHK               277
C ACTIVE GAS                                                              278
      TPGVLENG = (TOFCLENG-TPCG_MEMBTHK-2*TPCG_TPEATHK)/2                 278
      TOCSIR = TPCG_SENGASOR                                              278
      TOCSOR = TOCSIR + TPCG_TOCSDR                                       279
      TOKAIR = TOCSOR                                                     279
      TOKAOR = TOKAIR + TPCG_TOKADR                                       280
      TONXIR = TOKAOR                                                     280
      TONXOR = TONXIR + TPCG_TONXDR                                       281
      TOADIR = TONXOR                                                     281
      TOADOR = TOADIR + TPCG_TOADDR                                       282
      TOIGIR = TOADOR                                                     282
      TOIGOR = TOIGIR + TPCG_TOIGDR                                       283
      TOALIR = TOIGOR                                                     283
      TOALOR = TOALIR + TPCG_TOALDR                                       284
      TOHAIR = TOALOR                                                     284
      TOHAOR = TOHAIR + TPCG_TOHADR                                       285
C INNER FIELD CAGE INNER RADIUS                                           286
      TIFCIR = TPCG_RMIN                                                  286
      TIADIR = TIFCIR                                                     286
      TIADOR = TIADIR + TPCG_TIADDR                                       287
      TINXIR = TIADOR                                                     287
      TINXOR = TINXIR + TPCG_TINXDR                                       288
      TIKAIR = TINXOR                                                     288
      TIKAOR = TIKAIR + TPCG_TIKADR                                       289
      TIALIR = TIKAOR                                                     289
      TIALOR = TIALIR + TPCG_TIALDR                                       290
      TOFCIR = TOCSIR                                                     290
      TOFCOR = TOHAOR                                                     291
      TOFSIR = TOCSIR                                                     291
      TOFSOR = TOADOR                                                     292
      TIFCIR = TIADIR                                                     292
      TIFCOR = TIALOR                                                     293
C TPC GAS INNER RADIUS                                                    294
      TPGVIR = TIFCOR                                                     294
      DATA MNTCYLHT/30./                                                  295
      DATA MNTCYLID/16./                                                  296
      DATA MNTCYLOD/20./                                                  297
      DEL = TPCG_RMAX-TOFCOR                                              298
C Check IPRIN>=1                                                          298
      IF (IPRIN.GE.1) WRITE(LOUT,5010)TOFCOR,DEL                          298
5010  FORMAT(' TPCEgeo: maximum TPC Radius is ',F10.4,' clearance is ',   300
     *F8.4)                                                               300
C Check DEL<0                                                             301
      IF (DEL.LT.0) THEN                                                  301
      WRITE(LOUT,5020)DEL                                                 301
5020  FORMAT(' *** TPCEgeo ERROR : outer clearance negative ',F9.3)       303
      ENDIF                                                               304
C Check IPRIN>=1                                                          304
      IF (IPRIN.GE.1) WRITE(LOUT,5030)TPGVIR                              304
5030  FORMAT(' TPCEgeo: senset. gas inner radius',F10.4)                  306
      CALL AGSSTEP( TPCELASER )                                           307
      CALL AGSBEGM(MODULE,IPRIN)                                          308
C Check IPRIN<0                                                           308
      IF (IPRIN.LT.0) RETURN                                              308
C Create TPCE :                                                           308
C Call To Local Procedure TPCE                                            308
      ASSIGN 5050 TO BLOCK_TPCE                                           308
      GO TO  5040                                                         308
5050  CONTINUE                                                            308
C *                                                                       308
C Geant POSITION : TPCE ===> MOTHER='CAVE' <===                           309
      CALL AGSRESET                                                       309
      AG_MOTHER = 'CAVE'                                                  309
      AG_TITLE = 'TPCE'                                                   309
      AG_EXNAME = 'POSITION'                                              309
      AG_PARLIST = 'MOTHER'                                               309
      CALL AXPOSITION                                                     309
C *                                                                       309
C ----------------------------------------------------------------------  309
C ------------ TPCE                                                       309
      CALL AGSENDM                                                        311
      RETURN                                                              311
C Begin Local Procedure TPCE                                              311
C TPCE IS THE TPC ENVELOPE                                                311
5040  CONTINUE                                                            311
         AG_TITLE='TPCE'                                                  311
         AG_MODULE=MODULE                                                 311
         AG_MARK='V00'                                                    311
         CALL AGSPUSH                                                     311
         AG_MODULE=MODULE                                                 311
C    *                                                                    311
C    Geant MATERIAL : AIR ===> inherited arguments used <===              312
         CALL AGSRESET                                                    312
         AG_TITLE = 'AIR'                                                 312
         AG_EXNAME = 'MATERIAL'                                           312
         AG_PARLIST = 'NONE'                                              312
         CALL AXMATERIAL                                                  312
C    *                                                                    312
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           312
         CALL AGSRESET                                                    313
         AG_TITLE = 'STANDARD'                                            313
         AG_EXNAME = 'MEDIUM'                                             313
         AG_PARLIST = 'NONE'                                              313
         CALL AXMEDIUM                                                    313
C    *                                                                    313
C    Geant ATTRIBUTE : TPCE ===> SEEN=0 COLO=2 <===                       313
         CALL AGSRESET                                                    314
         AG_SEEN = 0                                                      314
         AG_COLO = 2                                                      314
         AG_TITLE = 'TPCE'                                                314
         AG_EXNAME = 'ATTRIBUTE'                                          314
         AG_PARLIST = 'SEEN_COLO'                                         314
         CALL AXATTRIBUTE                                                 314
C    *                                                                    314
C    Geant SHAPE : TUBE ===> RMIN=TPCG_RMIN RMAX=TPCG_RMAX, DZ=TPCG_LENG  314
C    TH/2 <===                                                            314
         CALL AGSRESET                                                    316
         AG_RMIN = TPCG_RMIN                                              316
         AG_RMAX = TPCG_RMAX                                              316
         AG_DZ = TPCG_LENGTH/2                                            316
         AG_TITLE = 'TUBE'                                                316
         AG_EXNAME = 'SHAPE'                                              316
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      316
         CALL AXSHAPE                                                     316
C    *                                                                    316
         TPGVZ =(TPCG_MEMBTHK + TPGVLENG)/2                               316
C    Create TPGV :                                                        318
C    Call To Local Procedure TPGV                                         318
         ASSIGN 5070 TO BLOCK_TPGV                                        318
         GO TO  5060                                                      318
5070     CONTINUE                                                         318
C    *                                                                    318
C    Geant POSITION : TPGV ===> Z=+TPGVZ THETAX=90 PHIX=75 THETAY=90 PHI  319
C    Y=-15 THETAZ=0 PHIZ=0 <===                                           319
         CALL AGSRESET                                                    319
         AG_Z = +TPGVZ                                                    319
         AG_THETAX = 90                                                   319
         AG_PHIX = 75                                                     319
         AG_THETAY = 90                                                   319
         AG_PHIY = -15                                                    319
         AG_THETAZ = 0                                                    319
         AG_PHIZ = 0                                                      319
         AG_TITLE = 'TPGV'                                                319
         AG_EXNAME = 'POSITION'                                           319
         AG_PARLIST = 'Z_THETAX_PHIX_THETAY_PHIY_THETAZ_PHIZ'             319
         CALL AXPOSITION                                                  319
C    *                                                                    319
C    Geant POSITION : TPGV ===> Z=-TPGVZ THETAX=90 PHIX=105 THETAY=90 PH  319
C    IY=195 THETAZ=180 PHIZ=0 <===                                        319
         CALL AGSRESET                                                    320
         AG_Z = -TPGVZ                                                    320
         AG_THETAX = 90                                                   320
         AG_PHIX = 105                                                    320
         AG_THETAY = 90                                                   320
         AG_PHIY = 195                                                    320
         AG_THETAZ = 180                                                  320
         AG_PHIZ = 0                                                      320
         AG_TITLE = 'TPGV'                                                320
         AG_EXNAME = 'POSITION'                                           320
         AG_PARLIST = 'Z_THETAX_PHIX_THETAY_PHIY_THETAZ_PHIZ'             320
         CALL AXPOSITION                                                  320
C    *                                                                    320
C    Create TIFC :                                                        321
C    Call To Local Procedure TIFC                                         321
         ASSIGN 5090 TO BLOCK_TIFC                                        321
         GO TO  5080                                                      321
5090     CONTINUE                                                         321
C    *                                                                    321
C    Geant POSITION : TIFC ===> inherited arguments used <===             322
         CALL AGSRESET                                                    322
         AG_TITLE = 'TIFC'                                                322
         AG_EXNAME = 'POSITION'                                           322
         AG_PARLIST = 'NONE'                                              322
         CALL AXPOSITION                                                  322
C    *                                                                    322
C    Create TOFC :                                                        323
C    Call To Local Procedure TOFC                                         323
         ASSIGN 5110 TO BLOCK_TOFC                                        323
         GO TO  5100                                                      323
5110     CONTINUE                                                         323
C    *                                                                    323
C    Geant POSITION : TOFC ===> inherited arguments used <===             324
         CALL AGSRESET                                                    324
         AG_TITLE = 'TOFC'                                                324
         AG_EXNAME = 'POSITION'                                           324
         AG_PARLIST = 'NONE'                                              324
         CALL AXPOSITION                                                  324
C    *                                                                    324
C    Create TPCM :                                                        325
C    Call To Local Procedure TPCM                                         325
         ASSIGN 5130 TO BLOCK_TPCM                                        325
         GO TO  5120                                                      325
5130     CONTINUE                                                         325
C    *                                                                    325
C    Geant POSITION : TPCM ===> inherited arguments used <===             326
         CALL AGSRESET                                                    326
         AG_TITLE = 'TPCM'                                                326
         AG_EXNAME = 'POSITION'                                           326
         AG_PARLIST = 'NONE'                                              326
         CALL AXPOSITION                                                  326
C    *                                                                    326
         TPEAZ =(TOFCLENG - TPCG_TPEATHK)/2                               326
C    Create TPEA :                                                        328
C    Call To Local Procedure TPEA                                         328
         ASSIGN 5150 TO BLOCK_TPEA                                        328
         GO TO  5140                                                      328
5150     CONTINUE                                                         328
C    *                                                                    328
C    Geant POSITION : TPEA ===> Z=+TPEAZ THETAX=90 PHIX=75 THETAY=90 PHI  329
C    Y=-15 THETAZ=0 PHIZ=0 <===                                           329
         CALL AGSRESET                                                    329
         AG_Z = +TPEAZ                                                    329
         AG_THETAX = 90                                                   329
         AG_PHIX = 75                                                     329
         AG_THETAY = 90                                                   329
         AG_PHIY = -15                                                    329
         AG_THETAZ = 0                                                    329
         AG_PHIZ = 0                                                      329
         AG_TITLE = 'TPEA'                                                329
         AG_EXNAME = 'POSITION'                                           329
         AG_PARLIST = 'Z_THETAX_PHIX_THETAY_PHIY_THETAZ_PHIZ'             329
         CALL AXPOSITION                                                  329
C    *                                                                    329
C    Geant POSITION : TPEA ===> Z=-TPEAZ THETAX=90 PHIX=105 THETAY=90 PH  329
C    IY=195 THETAZ=180 PHIZ=0 <===                                        329
         CALL AGSRESET                                                    330
         AG_Z = -TPEAZ                                                    330
         AG_THETAX = 90                                                   330
         AG_PHIX = 105                                                    330
         AG_THETAY = 90                                                   330
         AG_PHIY = 195                                                    330
         AG_THETAZ = 180                                                  330
         AG_PHIZ = 0                                                      330
         AG_TITLE = 'TPEA'                                                330
         AG_EXNAME = 'POSITION'                                           330
         AG_PARLIST = 'Z_THETAX_PHIX_THETAY_PHIY_THETAZ_PHIZ'             330
         CALL AXPOSITION                                                  330
C    *                                                                    330
         TPCWZ =(TPCG_LENGTH - 2*TRDO_RDOVTHK - TPCG_WHEELTHK)/2          330
C    Create TPCW :                                                        332
C    Call To Local Procedure TPCW                                         332
         ASSIGN 5170 TO BLOCK_TPCW                                        332
         GO TO  5160                                                      332
5170     CONTINUE                                                         332
C    *                                                                    332
C    Geant POSITION : TPCW ===> Z=+TPCWZ THETAX=90 PHIX=75 THETAY=90 PHI  333
C    Y=-15 THETAZ=0 PHIZ=0 <===                                           333
         CALL AGSRESET                                                    333
         AG_Z = +TPCWZ                                                    333
         AG_THETAX = 90                                                   333
         AG_PHIX = 75                                                     333
         AG_THETAY = 90                                                   333
         AG_PHIY = -15                                                    333
         AG_THETAZ = 0                                                    333
         AG_PHIZ = 0                                                      333
         AG_TITLE = 'TPCW'                                                333
         AG_EXNAME = 'POSITION'                                           333
         AG_PARLIST = 'Z_THETAX_PHIX_THETAY_PHIY_THETAZ_PHIZ'             333
         CALL AXPOSITION                                                  333
C    *                                                                    333
C    Geant POSITION : TPCW ===> Z=-TPCWZ THETAX=90 PHIX=105 THETAY=90 PH  333
C    IY=195 THETAZ=180 PHIZ=0 <===                                        333
         CALL AGSRESET                                                    334
         AG_Z = -TPCWZ                                                    334
         AG_THETAX = 90                                                   334
         AG_PHIX = 105                                                    334
         AG_THETAY = 90                                                   334
         AG_PHIY = 195                                                    334
         AG_THETAZ = 180                                                  334
         AG_PHIZ = 0                                                      334
         AG_TITLE = 'TPCW'                                                334
         AG_EXNAME = 'POSITION'                                           334
         AG_PARLIST = 'Z_THETAX_PHIX_THETAY_PHIY_THETAZ_PHIZ'             334
         CALL AXPOSITION                                                  334
C    *                                                                    334
         TPRDOZ =(TPCG_LENGTH - TRDO_RDOVTHK)/2                           334
C    Create TRDV :                                                        336
C    Call To Local Procedure TRDV                                         336
         ASSIGN 5190 TO BLOCK_TRDV                                        336
         GO TO  5180                                                      336
5190     CONTINUE                                                         336
C    *                                                                    336
C    Geant POSITION : TRDV ===> Z=+TPRDOZ THETAX=90 PHIX=75 THETAY=90 PH  337
C    IY=-15 THETAZ=0 PHIZ=0 <===                                          337
         CALL AGSRESET                                                    337
         AG_Z = +TPRDOZ                                                   337
         AG_THETAX = 90                                                   337
         AG_PHIX = 75                                                     337
         AG_THETAY = 90                                                   337
         AG_PHIY = -15                                                    337
         AG_THETAZ = 0                                                    337
         AG_PHIZ = 0                                                      337
         AG_TITLE = 'TRDV'                                                337
         AG_EXNAME = 'POSITION'                                           337
         AG_PARLIST = 'Z_THETAX_PHIX_THETAY_PHIY_THETAZ_PHIZ'             337
         CALL AXPOSITION                                                  337
C    *                                                                    337
C    Geant POSITION : TRDV ===> Z=-TPRDOZ THETAX=90 PHIX=105 THETAY=90 P  337
C    HIY=195 THETAZ=180 PHIZ=0 <===                                       337
         CALL AGSRESET                                                    338
         AG_Z = -TPRDOZ                                                   338
         AG_THETAX = 90                                                   338
         AG_PHIX = 105                                                    338
         AG_THETAY = 90                                                   338
         AG_PHIY = 195                                                    338
         AG_THETAZ = 180                                                  338
         AG_PHIZ = 0                                                      338
         AG_TITLE = 'TRDV'                                                338
         AG_EXNAME = 'POSITION'                                           338
         AG_PARLIST = 'Z_THETAX_PHIX_THETAY_PHIY_THETAZ_PHIZ'             338
         CALL AXPOSITION                                                  338
C    *                                                                    338
5041  CALL AGSPOPB                                                        339
      GO TO BLOCK_TPCE, (5040 ,5050)                                      339
C End of Local Procedure                                                  339
C ----------------------------------------------------------------------  340
C ------------ TPGV                                                       340
C Begin Local Procedure TPGV                                              341
C TPGV IS THE GAS VOLUME PLACED IN TPC                                    341
5060  CONTINUE                                                            341
         AG_TITLE='TPGV'                                                  341
         AG_MODULE=MODULE                                                 341
         AG_MARK='V00'                                                    341
         CALL AGSPUSH                                                     341
         AG_MODULE=MODULE                                                 341
C    *                                                                    341
C    Check TPCG_GASCORR==1                                                342
         IF (TPCG_GASCORR.EQ.1) THEN                                      342
         DENSITY=0.9*0.001782+0.1*0.000667                                343
         ELSE                                                             344
         T = 298.06                                                       345
         P = 1016.59                                                      346
         PATM = 0.001*P*750.062/760.                                      347
         RHO =(0.9*0.001782+0.1*0.000717)                                 348
         DENSITY=RHO*(273.15/T)*PATM                                      349
         ENDIF                                                            350
C    Geant COMPONENT : AR ===> A=40 Z=18 W=9 <===                         351
         CALL AGSRESET                                                    351
         AG_A = 40                                                        351
         AG_Z = 18                                                        351
         AG_W = 9                                                         351
         AG_TITLE = 'AR'                                                  351
         AG_EXNAME = 'COMPONENT'                                          351
         AG_PARLIST = 'A_Z_W'                                             351
         CALL AXCOMPONENT                                                 351
C    *                                                                    351
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           351
         CALL AGSRESET                                                    352
         AG_A = 12                                                        352
         AG_Z = 6                                                         352
         AG_W = 1                                                         352
         AG_TITLE = 'C'                                                   352
         AG_EXNAME = 'COMPONENT'                                          352
         AG_PARLIST = 'A_Z_W'                                             352
         CALL AXCOMPONENT                                                 352
C    *                                                                    352
C    Geant COMPONENT : H ===> A=1 Z=1 W=4 <===                            352
         CALL AGSRESET                                                    353
         AG_A = 1                                                         353
         AG_Z = 1                                                         353
         AG_W = 4                                                         353
         AG_TITLE = 'H'                                                   353
         AG_EXNAME = 'COMPONENT'                                          353
         AG_PARLIST = 'A_Z_W'                                             353
         CALL AXCOMPONENT                                                 353
C    *                                                                    353
C    Geant MIXTURE : P10 ===> DENS=DENSITY <===                           353
         CALL AGSRESET                                                    354
         AG_DENS = DENSITY                                                354
         AG_TITLE = 'P10'                                                 354
         AG_EXNAME = 'MIXTURE'                                            354
         AG_PARLIST = 'DENS'                                              354
         CALL AXMIXTURE                                                   354
C    *                                                                    354
C    Geant SHAPE : TUBE ===> RMIN=TPGVIR RMAX=TPCG_SENGASOR, DZ=TPGVLENG  354
C    /2 <===                                                              354
         CALL AGSRESET                                                    356
         AG_RMIN = TPGVIR                                                 356
         AG_RMAX = TPCG_SENGASOR                                          356
         AG_DZ = TPGVLENG/2                                               356
         AG_TITLE = 'TUBE'                                                356
         AG_EXNAME = 'SHAPE'                                              356
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      356
         CALL AXSHAPE                                                     356
C    *                                                                    356
C    Create TPSS :                                                        357
C    Call To Local Procedure TPSS                                         357
         ASSIGN 5210 TO BLOCK_TPSS                                        357
         GO TO  5200                                                      357
5210     CONTINUE                                                         357
C    *                                                                    357
5061  CALL AGSPOPB                                                        358
      GO TO BLOCK_TPGV, (5060 ,5070)                                      358
C End of Local Procedure                                                  358
C ----------------------------------------------------------------------  359
C ------------ TPSS                                                       359
C Begin Local Procedure TPSS                                              360
C TPSS IS A DIVISION OF GAS VOLUME CORRESPONDING TO A SUPERSECTORS        360
5200  CONTINUE                                                            360
         AG_TITLE='TPSS'                                                  360
         AG_MODULE=MODULE                                                 360
         AG_MARK='V00'                                                    360
         CALL AGSPUSH                                                     360
         AG_MODULE=MODULE                                                 360
C    *                                                                    360
C    Geant ATTRIBUTE : TPSS ===> SEEN=0 <===                              361
         CALL AGSRESET                                                    361
         AG_SEEN = 0                                                      361
         AG_TITLE = 'TPSS'                                                361
         AG_EXNAME = 'ATTRIBUTE'                                          361
         AG_PARLIST = 'SEEN'                                              361
         CALL AXATTRIBUTE                                                 361
C    *                                                                    361
C    Geant SHAPE : DIVISION ===> NDIV=12 IAXIS=2 <===                     361
         CALL AGSRESET                                                    362
         AG_NDIV = 12                                                     362
         AG_IAXIS = 2                                                     362
         AG_TITLE = 'DIVISION'                                            362
         AG_EXNAME = 'SHAPE'                                              362
         AG_PARLIST = 'NDIV_IAXIS'                                        362
         CALL AXSHAPE                                                     362
C    *                                                                    362
C    *                                                                    362
         I_SEC=1                                                          363
            GO TO 5223                                                    363
5221        I_SEC=I_SEC+1                                                 363
5223        IF(I_SEC-(2).GT.0) GO TO 5222                                 363
C       use TPRS bank                                                     364
            BPATH_TPRS='TPRS*'                                            364
            IDNUM_TPRS(1)=0                                               364
            TPRS_SEC=I_SEC                                                364
            CALL AGDGETP('TPCEGEO1','*',-999,'TPRS',NTPRS_SEC,            364
     *      RE_TPRS_SEC,BPATH_TPRS,IDNUM_TPRS,1,MAP_TPRS,NAMES_TPRS,      364
     *      BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)             364
C       *                                                                 365
            I_ROW=1                                                       365
               GO TO 5233                                                 365
5231           I_ROW=I_ROW+1                                              365
5233           IF(I_ROW-(NINT(TPRS_NROW)).GT.0) GO TO 5232                365
C          Check NINT(TPRS_SUPER)==3|I_ROW==1                             366
               IF (NINT(TPRS_SUPER).EQ.3.OR.I_ROW.EQ.1) THEN              366
C          Create TPAD :                                                  367
C          Call To Local Procedure TPAD                                   367
               ASSIGN 5250 TO BLOCK_TPAD                                  367
               GO TO  5240                                                367
5250           CONTINUE                                                   367
C          *                                                              367
C          Geant POSITION : TPAD ===> X=TPRS_RPADS(I_ROW)-TPRS_WIDTH DX=  369
C          TPRS_WIDTH/2 DY=TPRS_NPADS(I_ROW)*TPRS_PITCH/2 <===            369
               CALL AGSRESET                                              369
               AG_X = TPRS_RPADS(I_ROW)-TPRS_WIDTH                        369
               AG_DX = TPRS_WIDTH/2                                       369
               AG_DY = TPRS_NPADS(I_ROW)*TPRS_PITCH/2                     369
               AG_TITLE = 'TPAD'                                          369
               AG_EXNAME = 'POSITION'                                     369
               AG_PARLIST = 'X_DX_DY'                                     369
               CALL AXPOSITION                                            369
C          *                                                              369
               ENDIF                                                      369
C          Check NINT(TPRS_SUPER)>0                                       371
               IF (NINT(TPRS_SUPER).GT.0) THEN                            371
C          Create TPAD :                                                  372
C          Call To Local Procedure TPAD                                   372
               ASSIGN 5260 TO BLOCK_TPAD                                  372
               GO TO  5240                                                372
5260           CONTINUE                                                   372
C          *                                                              372
C          Geant POSITION : TPAD ===> X=TPRS_RPADS(I_ROW) DX=TPRS_WIDTH/  374
C          2 DY=TPRS_NPADS(I_ROW)*TPRS_PITCH/2 <===                       374
               CALL AGSRESET                                              374
               AG_X = TPRS_RPADS(I_ROW)                                   374
               AG_DX = TPRS_WIDTH/2                                       374
               AG_DY = TPRS_NPADS(I_ROW)*TPRS_PITCH/2                     374
               AG_TITLE = 'TPAD'                                          374
               AG_EXNAME = 'POSITION'                                     374
               AG_PARLIST = 'X_DX_DY'                                     374
               CALL AXPOSITION                                            374
C          *                                                              374
               ENDIF                                                      374
C          Check NINT(TPRS_SUPER)==3|I_ROW==NINT(TPRS_NROW)               376
               IF (NINT(TPRS_SUPER).EQ.3.OR.I_ROW.EQ.NINT(TPRS_NROW))     376
     *         THEN                                                       376
C          Create TPAD :                                                  377
C          Call To Local Procedure TPAD                                   377
               ASSIGN 5270 TO BLOCK_TPAD                                  377
               GO TO  5240                                                377
5270           CONTINUE                                                   377
C          *                                                              377
C          Geant POSITION : TPAD ===> X=TPRS_RPADS(I_ROW)+TPRS_WIDTH DX=  379
C          TPRS_WIDTH/2 DY=TPRS_NPADS(I_ROW)*TPRS_PITCH/2 <===            379
               CALL AGSRESET                                              379
               AG_X = TPRS_RPADS(I_ROW)+TPRS_WIDTH                        379
               AG_DX = TPRS_WIDTH/2                                       379
               AG_DY = TPRS_NPADS(I_ROW)*TPRS_PITCH/2                     379
               AG_TITLE = 'TPAD'                                          379
               AG_EXNAME = 'POSITION'                                     379
               AG_PARLIST = 'X_DX_DY'                                     379
               CALL AXPOSITION                                            379
C          *                                                              379
               ENDIF                                                      379
            GO TO 5231                                                    381
5232        CONTINUE                                                      381
         GO TO 5221                                                       382
5222     CONTINUE                                                         382
5201  CALL AGSPOPB                                                        383
      GO TO BLOCK_TPSS, (5200 ,5210)                                      383
C End of Local Procedure                                                  383
C ----------------------------------------------------------------------  384
C ------------ TPAD                                                       384
C Begin Local Procedure TPAD                                              385
C TPAD IS A REAL PADROW WITH DIMENSIONS DEFINED AT POSITIONING TIME       385
5240  CONTINUE                                                            385
         AG_TITLE='TPAD'                                                  385
         AG_MODULE=MODULE                                                 385
         AG_MARK='V00'                                                    385
         CALL AGSPUSH                                                     385
         AG_MODULE=MODULE                                                 385
C    *                                                                    385
C    *                                                                    386
C       Check TPRS_WIDTH<1.5                                              386
            IF (TPRS_WIDTH.LT.1.5) THEN                                   386
C       Geant ATTRIBUTE : TPAD ===> SEEN=0 COLO=2 SERIAL=1 <===           387
            CALL AGSRESET                                                 387
            AG_SEEN = 0                                                   387
            AG_COLO = 2                                                   387
            AG_SERIAL = 1                                                 387
            AG_TITLE = 'TPAD'                                             387
            AG_EXNAME = 'ATTRIBUTE'                                       387
            AG_PARLIST = 'SEEN_COLO_SERIAL'                               387
            CALL AXATTRIBUTE                                              387
C       *                                                                 387
         END IF                                                           388
C    *                                                                    389
C       Check TPRS_WIDTH>1.5                                              389
            IF (TPRS_WIDTH.GT.1.5) THEN                                   389
C       Geant ATTRIBUTE : TPAD ===> SEEN=0 COLO=2 SERIAL=2 <===           390
            CALL AGSRESET                                                 390
            AG_SEEN = 0                                                   390
            AG_COLO = 2                                                   390
            AG_SERIAL = 2                                                 390
            AG_TITLE = 'TPAD'                                             390
            AG_EXNAME = 'ATTRIBUTE'                                       390
            AG_PARLIST = 'SEEN_COLO_SERIAL'                               390
            CALL AXATTRIBUTE                                              390
C       *                                                                 390
         END IF                                                           391
C    Geant MATERIAL : P10 ===> inherited arguments used <===              392
         CALL AGSRESET                                                    392
         AG_TITLE = 'P10'                                                 392
         AG_EXNAME = 'MATERIAL'                                           392
         AG_PARLIST = 'NONE'                                              392
         CALL AXMATERIAL                                                  392
C    *                                                                    392
C    Geant MATERIAL : SENSITIVE_GAS ===> ISVOL=1 STEMAX=2.5*TPRS_WIDTH <  392
C    ===                                                                  392
         CALL AGSRESET                                                    393
         AG_ISVOL = 1                                                     393
         AG_STEMAX = 2.5*TPRS_WIDTH                                       393
         AG_TITLE = 'SENSITIVE_GAS'                                       393
         AG_EXNAME = 'MATERIAL'                                           393
         AG_PARLIST = 'ISVOL_STEMAX'                                      393
         CALL AXMATERIAL                                                  393
C    *                                                                    393
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           393
         CALL AGSRESET                                                    394
         AG_DX = 0                                                        394
         AG_DY = 0                                                        394
         AG_DZ = 0                                                        394
         AG_TITLE = 'BOX'                                                 394
         AG_EXNAME = 'SHAPE'                                              394
         AG_PARLIST = 'DX_DY_DZ'                                          394
         CALL AXSHAPE                                                     394
C    *                                                                    394
         CALL GSTPAR(AG_IMED, 'STRA', 1.)                                 394
C    Defining Hits: Z:0.0005:S Y:0.0005: Z:0.0005: CX:10: CY:10: CZ:10:   397
C    LPTOT:18:(-3,2), SLENG:0.1:(0,800) TOF:16:(0,1.0E-6) LGAM:16:(-2,+2  397
C    ) STEP:11:(0,10) USER:21:(-0.01,0.01)                                397
         AG_TITLE = 'TPAD'                                                397
         AG_EXNAME = 'HITS'                                               397
         AG_TYPE=AG_MAGIC                                                 397
         CALL AGSDETV('TPAD')                                             397
C    *                                                                    397
         AG_PARLIST='Z'                                                   397
         AG_TITLE = 'DETH'                                                397
         AG_NBITS = AG_MAGIC                                              397
         AG_BIN=0.0005                                                    397
         AG_OPTION='S'                                                    397
         AG_FHMIN = AG_MAGIC                                              397
         AG_FHMAX = AG_MAGIC                                              397
         CALL AGSDETH (0)                                                 397
C    *                                                                    397
         AG_PARLIST='Y'                                                   397
         AG_TITLE = 'DETH'                                                397
         AG_NBITS = AG_MAGIC                                              397
         AG_BIN=0.0005                                                    397
         AG_FHMIN=AG_MAGIC                                                397
         AG_FHMAX=AG_MAGIC                                                397
         AG_OPTION='*'                                                    397
         CALL AGSDETH (0)                                                 397
C    *                                                                    397
         AG_PARLIST='Z'                                                   397
         AG_TITLE = 'DETH'                                                397
         AG_NBITS = AG_MAGIC                                              397
         AG_BIN=0.0005                                                    397
         AG_FHMIN=AG_MAGIC                                                397
         AG_FHMAX=AG_MAGIC                                                397
         AG_OPTION='*'                                                    397
         CALL AGSDETH (0)                                                 397
C    *                                                                    397
         AG_PARLIST='CX'                                                  397
         AG_TITLE = 'DETH'                                                397
         AG_NBITS = 10                                                    397
         AG_BIN=AG_MAGIC                                                  397
         AG_FHMIN=AG_MAGIC                                                397
         AG_FHMAX=AG_MAGIC                                                397
         AG_OPTION='*'                                                    397
         CALL AGSDETH (0)                                                 397
C    *                                                                    397
         AG_PARLIST='CY'                                                  397
         AG_TITLE = 'DETH'                                                397
         AG_NBITS = 10                                                    397
         AG_BIN=AG_MAGIC                                                  397
         AG_FHMIN=AG_MAGIC                                                397
         AG_FHMAX=AG_MAGIC                                                397
         AG_OPTION='*'                                                    397
         CALL AGSDETH (0)                                                 397
C    *                                                                    397
         AG_PARLIST='CZ'                                                  397
         AG_TITLE = 'DETH'                                                397
         AG_NBITS = 10                                                    397
         AG_BIN=AG_MAGIC                                                  397
         AG_FHMIN=AG_MAGIC                                                397
         AG_FHMAX=AG_MAGIC                                                397
         AG_OPTION='*'                                                    397
         CALL AGSDETH (0)                                                 397
C    *                                                                    397
         AG_PARLIST='LPTOT'                                               397
         AG_TITLE = 'DETH'                                                397
         AG_NBITS = 18                                                    397
         AG_BIN=AG_MAGIC                                                  397
         AG_OPTION='*'                                                    397
         AG_FHMIN = -3                                                    397
         AG_FHMAX = 2                                                     397
         CALL AGSDETH (0)                                                 397
C    *                                                                    397
         AG_PARLIST='SLENG'                                               397
         AG_TITLE = 'DETH'                                                397
         AG_NBITS = AG_MAGIC                                              397
         AG_BIN=0.1                                                       397
         AG_OPTION='*'                                                    397
         AG_FHMIN = 0                                                     397
         AG_FHMAX = 800                                                   397
         CALL AGSDETH (0)                                                 397
C    *                                                                    397
         AG_PARLIST='TOF'                                                 397
         AG_TITLE = 'DETH'                                                397
         AG_NBITS = 16                                                    397
         AG_BIN=AG_MAGIC                                                  397
         AG_OPTION='*'                                                    397
         AG_FHMIN = 0                                                     397
         AG_FHMAX = 1.0E-6                                                397
         CALL AGSDETH (0)                                                 397
C    *                                                                    397
         AG_PARLIST='LGAM'                                                397
         AG_TITLE = 'DETH'                                                397
         AG_NBITS = 16                                                    397
         AG_BIN=AG_MAGIC                                                  397
         AG_OPTION='*'                                                    397
         AG_FHMIN = -2                                                    397
         AG_FHMAX = +2                                                    397
         CALL AGSDETH (0)                                                 397
C    *                                                                    397
         AG_PARLIST='STEP'                                                397
         AG_TITLE = 'DETH'                                                397
         AG_NBITS = 11                                                    397
         AG_BIN=AG_MAGIC                                                  397
         AG_OPTION='*'                                                    397
         AG_FHMIN = 0                                                     397
         AG_FHMAX = 10                                                    397
         CALL AGSDETH (0)                                                 397
C    *                                                                    397
         AG_PARLIST='USER'                                                397
         AG_TITLE = 'DETH'                                                397
         AG_NBITS = 21                                                    397
         AG_BIN=AG_MAGIC                                                  397
         AG_OPTION='*'                                                    397
         AG_FHMIN = -0.01                                                 397
         AG_FHMAX = 0.01                                                  397
         CALL AGSDETH (JUMPAD(TPADSTEP))                                  397
C    *                                                                    397
         CALL AGSDETU('TPAD')                                             397
C    *                                                                    397
5241  CALL AGSPOPB                                                        398
      GO TO BLOCK_TPAD, (5240 ,5250,5260,5270)                            398
C End of Local Procedure                                                  398
C ----------------------------------------------------------------------  399
C ------------ TIFC                                                       399
C Begin Local Procedure TIFC                                              400
C TIFC DEFINES THE INNER FIELD CAGE PLACED IN TPC                         400
5080  CONTINUE                                                            400
         AG_TITLE='TIFC'                                                  400
         AG_MODULE=MODULE                                                 400
         AG_MARK='V00'                                                    400
         CALL AGSPUSH                                                     400
         AG_MODULE=MODULE                                                 400
C    *                                                                    400
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            401
         CALL AGSRESET                                                    401
         AG_TITLE = 'MYLAR'                                               401
         AG_EXNAME = 'MATERIAL'                                           401
         AG_PARLIST = 'NONE'                                              401
         CALL AXMATERIAL                                                  401
C    *                                                                    401
C    Geant ATTRIBUTE : TIFC ===> SEEN=1 COLO=4 <===                       401
         CALL AGSRESET                                                    402
         AG_SEEN = 1                                                      402
         AG_COLO = 4                                                      402
         AG_TITLE = 'TIFC'                                                402
         AG_EXNAME = 'ATTRIBUTE'                                          402
         AG_PARLIST = 'SEEN_COLO'                                         402
         CALL AXATTRIBUTE                                                 402
C    *                                                                    402
C    Geant SHAPE : TUBE ===> RMIN=TIADIR RMAX=TIFCOR DZ=TOFCLENG/2 <===   402
         CALL AGSRESET                                                    403
         AG_RMIN = TIADIR                                                 403
         AG_RMAX = TIFCOR                                                 403
         AG_DZ = TOFCLENG/2                                               403
         AG_TITLE = 'TUBE'                                                403
         AG_EXNAME = 'SHAPE'                                              403
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      403
         CALL AXSHAPE                                                     403
C    *                                                                    403
C    Create TINX :                                                        404
C    Call To Local Procedure TINX                                         404
         ASSIGN 5290 TO BLOCK_TINX                                        404
         GO TO  5280                                                      404
5290     CONTINUE                                                         404
C    *                                                                    404
C    Geant POSITION : TINX ===> inherited arguments used <===             405
         CALL AGSRESET                                                    405
         AG_TITLE = 'TINX'                                                405
         AG_EXNAME = 'POSITION'                                           405
         AG_PARLIST = 'NONE'                                              405
         CALL AXPOSITION                                                  405
C    *                                                                    405
5081  CALL AGSPOPB                                                        406
      GO TO BLOCK_TIFC, (5080 ,5090)                                      406
C End of Local Procedure                                                  406
C ----------------------------------------------------------------------  407
C ------------ TINX                                                       407
C Begin Local Procedure TINX                                              408
C TINX IS THE INNER NOMEX STRUCTURE                                       408
5280  CONTINUE                                                            408
         AG_TITLE='TINX'                                                  408
         AG_MODULE=MODULE                                                 408
         AG_MARK='V00'                                                    408
         CALL AGSPUSH                                                     408
         AG_MODULE=MODULE                                                 408
C    *                                                                    408
C    Geant COMPONENT : C ===> A=12 Z=6 W=5 <===                           409
         CALL AGSRESET                                                    409
         AG_A = 12                                                        409
         AG_Z = 6                                                         409
         AG_W = 5                                                         409
         AG_TITLE = 'C'                                                   409
         AG_EXNAME = 'COMPONENT'                                          409
         AG_PARLIST = 'A_Z_W'                                             409
         CALL AXCOMPONENT                                                 409
C    *                                                                    409
C    Geant COMPONENT : H ===> A=1 Z=1 W=8 <===                            409
         CALL AGSRESET                                                    410
         AG_A = 1                                                         410
         AG_Z = 1                                                         410
         AG_W = 8                                                         410
         AG_TITLE = 'H'                                                   410
         AG_EXNAME = 'COMPONENT'                                          410
         AG_PARLIST = 'A_Z_W'                                             410
         CALL AXCOMPONENT                                                 410
C    *                                                                    410
C    Geant COMPONENT : O ===> A=16 Z=8 W=2 <===                           410
         CALL AGSRESET                                                    411
         AG_A = 16                                                        411
         AG_Z = 8                                                         411
         AG_W = 2                                                         411
         AG_TITLE = 'O'                                                   411
         AG_EXNAME = 'COMPONENT'                                          411
         AG_PARLIST = 'A_Z_W'                                             411
         CALL AXCOMPONENT                                                 411
C    *                                                                    411
C    Geant MIXTURE : NOMEX ===> DENS=0.048 <===                           411
         CALL AGSRESET                                                    412
         AG_DENS = 0.048                                                  412
         AG_TITLE = 'NOMEX'                                               412
         AG_EXNAME = 'MIXTURE'                                            412
         AG_PARLIST = 'DENS'                                              412
         CALL AXMIXTURE                                                   412
C    *                                                                    412
C    Geant ATTRIBUTE : TINX ===> SEEN=1 COLO=5 <===                       412
         CALL AGSRESET                                                    413
         AG_SEEN = 1                                                      413
         AG_COLO = 5                                                      413
         AG_TITLE = 'TINX'                                                413
         AG_EXNAME = 'ATTRIBUTE'                                          413
         AG_PARLIST = 'SEEN_COLO'                                         413
         CALL AXATTRIBUTE                                                 413
C    *                                                                    413
C    Geant SHAPE : TUBE ===> RMIN=TINXIR <===                             413
         CALL AGSRESET                                                    414
         AG_RMIN = TINXIR                                                 414
         AG_TITLE = 'TUBE'                                                414
         AG_EXNAME = 'SHAPE'                                              414
         AG_PARLIST = 'RMIN'                                              414
         CALL AXSHAPE                                                     414
C    *                                                                    414
C    Create TIKA :                                                        415
C    Call To Local Procedure TIKA                                         415
         ASSIGN 5310 TO BLOCK_TIKA                                        415
         GO TO  5300                                                      415
5310     CONTINUE                                                         415
C    *                                                                    415
C    Geant POSITION : TIKA ===> inherited arguments used <===             416
         CALL AGSRESET                                                    416
         AG_TITLE = 'TIKA'                                                416
         AG_EXNAME = 'POSITION'                                           416
         AG_PARLIST = 'NONE'                                              416
         CALL AXPOSITION                                                  416
C    *                                                                    416
5281  CALL AGSPOPB                                                        417
      GO TO BLOCK_TINX, (5280 ,5290)                                      417
C End of Local Procedure                                                  417
C ----------------------------------------------------------------------  418
C ------------ TIKA                                                       418
C Begin Local Procedure TIKA                                              419
C TIKA IS THE KAPTON FILM OF THE INNER FIELD CAGE                         419
5300  CONTINUE                                                            419
         AG_TITLE='TIKA'                                                  419
         AG_MODULE=MODULE                                                 419
         AG_MARK='V00'                                                    419
         CALL AGSPUSH                                                     419
         AG_MODULE=MODULE                                                 419
C    *                                                                    419
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            420
         CALL AGSRESET                                                    420
         AG_TITLE = 'MYLAR'                                               420
         AG_EXNAME = 'MATERIAL'                                           420
         AG_PARLIST = 'NONE'                                              420
         CALL AXMATERIAL                                                  420
C    *                                                                    420
C    Geant ATTRIBUTE : TIKA ===> SEEN=1 COLO=6 <===                       420
         CALL AGSRESET                                                    421
         AG_SEEN = 1                                                      421
         AG_COLO = 6                                                      421
         AG_TITLE = 'TIKA'                                                421
         AG_EXNAME = 'ATTRIBUTE'                                          421
         AG_PARLIST = 'SEEN_COLO'                                         421
         CALL AXATTRIBUTE                                                 421
C    *                                                                    421
C    Geant SHAPE : TUBE ===> RMIN=TIKAIR <===                             421
         CALL AGSRESET                                                    422
         AG_RMIN = TIKAIR                                                 422
         AG_TITLE = 'TUBE'                                                422
         AG_EXNAME = 'SHAPE'                                              422
         AG_PARLIST = 'RMIN'                                              422
         CALL AXSHAPE                                                     422
C    *                                                                    422
C    Create TIAL :                                                        423
C    Call To Local Procedure TIAL                                         423
         ASSIGN 5330 TO BLOCK_TIAL                                        423
         GO TO  5320                                                      423
5330     CONTINUE                                                         423
C    *                                                                    423
C    Geant POSITION : TIAL ===> inherited arguments used <===             424
         CALL AGSRESET                                                    424
         AG_TITLE = 'TIAL'                                                424
         AG_EXNAME = 'POSITION'                                           424
         AG_PARLIST = 'NONE'                                              424
         CALL AXPOSITION                                                  424
C    *                                                                    424
5301  CALL AGSPOPB                                                        425
      GO TO BLOCK_TIKA, (5300 ,5310)                                      425
C End of Local Procedure                                                  425
C ----------------------------------------------------------------------  426
C ------------ TIAL                                                       426
C Begin Local Procedure TIAL                                              427
C TIAL IS THE INNER ALUMINUM CYLINDER                                     427
5320  CONTINUE                                                            427
         AG_TITLE='TIAL'                                                  427
         AG_MODULE=MODULE                                                 427
         AG_MARK='V00'                                                    427
         CALL AGSPUSH                                                     427
         AG_MODULE=MODULE                                                 427
C    *                                                                    427
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        428
         CALL AGSRESET                                                    428
         AG_TITLE = 'ALUMINIUM'                                           428
         AG_EXNAME = 'MATERIAL'                                           428
         AG_PARLIST = 'NONE'                                              428
         CALL AXMATERIAL                                                  428
C    *                                                                    428
C    Geant ATTRIBUTE : TIAL ===> SEEN=1 COLO=7 <===                       428
         CALL AGSRESET                                                    429
         AG_SEEN = 1                                                      429
         AG_COLO = 7                                                      429
         AG_TITLE = 'TIAL'                                                429
         AG_EXNAME = 'ATTRIBUTE'                                          429
         AG_PARLIST = 'SEEN_COLO'                                         429
         CALL AXATTRIBUTE                                                 429
C    *                                                                    429
C    Geant SHAPE : TUBE ===> RMIN=TIALIR <===                             429
         CALL AGSRESET                                                    430
         AG_RMIN = TIALIR                                                 430
         AG_TITLE = 'TUBE'                                                430
         AG_EXNAME = 'SHAPE'                                              430
         AG_PARLIST = 'RMIN'                                              430
         CALL AXSHAPE                                                     430
C    *                                                                    430
5321  CALL AGSPOPB                                                        431
      GO TO BLOCK_TIAL, (5320 ,5330)                                      431
C End of Local Procedure                                                  431
C ----------------------------------------------------------------------  432
C ------------ TOFC                                                       432
C Begin Local Procedure TOFC                                              433
C TOFC DEFINES OUTER FIELD CAGE - FILL IT WITH INSULATING GAS ALREADY     433
5100  CONTINUE                                                            433
         AG_TITLE='TOFC'                                                  433
         AG_MODULE=MODULE                                                 433
         AG_MARK='V00'                                                    433
         CALL AGSPUSH                                                     433
         AG_MODULE=MODULE                                                 433
C    *                                                                    433
C    Geant MATERIAL : NITROGEN_GAS ===> inherited arguments used <===     434
         CALL AGSRESET                                                    434
         AG_TITLE = 'NITROGEN_GAS'                                        434
         AG_EXNAME = 'MATERIAL'                                           434
         AG_PARLIST = 'NONE'                                              434
         CALL AXMATERIAL                                                  434
C    *                                                                    434
C    Geant ATTRIBUTE : TOFC ===> SEEN=1 COLO=3 <===                       434
         CALL AGSRESET                                                    435
         AG_SEEN = 1                                                      435
         AG_COLO = 3                                                      435
         AG_TITLE = 'TOFC'                                                435
         AG_EXNAME = 'ATTRIBUTE'                                          435
         AG_PARLIST = 'SEEN_COLO'                                         435
         CALL AXATTRIBUTE                                                 435
C    *                                                                    435
C    Geant SHAPE : TUBE ===> RMIN=TOFCIR RMAX=TOFCOR DZ=TOFCLENG/2 <===   435
         CALL AGSRESET                                                    436
         AG_RMIN = TOFCIR                                                 436
         AG_RMAX = TOFCOR                                                 436
         AG_DZ = TOFCLENG/2                                               436
         AG_TITLE = 'TUBE'                                                436
         AG_EXNAME = 'SHAPE'                                              436
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      436
         CALL AXSHAPE                                                     436
C    *                                                                    436
C    Create TOFS :                                                        437
C    Call To Local Procedure TOFS                                         437
         ASSIGN 5350 TO BLOCK_TOFS                                        437
         GO TO  5340                                                      437
5350     CONTINUE                                                         437
C    *                                                                    437
C    Geant POSITION : TOFS ===> inherited arguments used <===             438
         CALL AGSRESET                                                    438
         AG_TITLE = 'TOFS'                                                438
         AG_EXNAME = 'POSITION'                                           438
         AG_PARLIST = 'NONE'                                              438
         CALL AXPOSITION                                                  438
C    *                                                                    438
C    Create TOST :                                                        439
C    Call To Local Procedure TOST                                         439
         ASSIGN 5370 TO BLOCK_TOST                                        439
         GO TO  5360                                                      439
5370     CONTINUE                                                         439
C    *                                                                    439
C    Geant POSITION : TOST ===> inherited arguments used <===             440
         CALL AGSRESET                                                    440
         AG_TITLE = 'TOST'                                                440
         AG_EXNAME = 'POSITION'                                           440
         AG_PARLIST = 'NONE'                                              440
         CALL AXPOSITION                                                  440
C    *                                                                    440
5101  CALL AGSPOPB                                                        441
      GO TO BLOCK_TOFC, (5100 ,5110)                                      441
C End of Local Procedure                                                  441
C ----------------------------------------------------------------------  442
C ------------ TOFS                                                       442
C Begin Local Procedure TOFS                                              443
C TOFS IS THE OUTER FIELD CAGE STRUCTURE                                  443
5340  CONTINUE                                                            443
         AG_TITLE='TOFS'                                                  443
         AG_MODULE=MODULE                                                 443
         AG_MARK='V00'                                                    443
         CALL AGSPUSH                                                     443
         AG_MODULE=MODULE                                                 443
C    *                                                                    443
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           444
         CALL AGSRESET                                                    444
         AG_TITLE = 'COPPER'                                              444
         AG_EXNAME = 'MATERIAL'                                           444
         AG_PARLIST = 'NONE'                                              444
         CALL AXMATERIAL                                                  444
C    *                                                                    444
C    Geant ATTRIBUTE : TOFS ===> SEEN=1 COLO=4 <===                       444
         CALL AGSRESET                                                    445
         AG_SEEN = 1                                                      445
         AG_COLO = 4                                                      445
         AG_TITLE = 'TOFS'                                                445
         AG_EXNAME = 'ATTRIBUTE'                                          445
         AG_PARLIST = 'SEEN_COLO'                                         445
         CALL AXATTRIBUTE                                                 445
C    *                                                                    445
C    Geant SHAPE : TUBE ===> RMAX=TOFSOR <===                             445
         CALL AGSRESET                                                    446
         AG_RMAX = TOFSOR                                                 446
         AG_TITLE = 'TUBE'                                                446
         AG_EXNAME = 'SHAPE'                                              446
         AG_PARLIST = 'RMAX'                                              446
         CALL AXSHAPE                                                     446
C    *                                                                    446
C    Create TOKA :                                                        447
C    Call To Local Procedure TOKA                                         447
         ASSIGN 5390 TO BLOCK_TOKA                                        447
         GO TO  5380                                                      447
5390     CONTINUE                                                         447
C    *                                                                    447
C    Geant POSITION : TOKA ===> inherited arguments used <===             448
         CALL AGSRESET                                                    448
         AG_TITLE = 'TOKA'                                                448
         AG_EXNAME = 'POSITION'                                           448
         AG_PARLIST = 'NONE'                                              448
         CALL AXPOSITION                                                  448
C    *                                                                    448
5341  CALL AGSPOPB                                                        449
      GO TO BLOCK_TOFS, (5340 ,5350)                                      449
C End of Local Procedure                                                  449
C ----------------------------------------------------------------------  450
C ------------ TOKA                                                       450
C Begin Local Procedure TOKA                                              451
C TOKA IS KAPTON LAYER                                                    451
5380  CONTINUE                                                            451
         AG_TITLE='TOKA'                                                  451
         AG_MODULE=MODULE                                                 451
         AG_MARK='V00'                                                    451
         CALL AGSPUSH                                                     451
         AG_MODULE=MODULE                                                 451
C    *                                                                    451
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            452
         CALL AGSRESET                                                    452
         AG_TITLE = 'MYLAR'                                               452
         AG_EXNAME = 'MATERIAL'                                           452
         AG_PARLIST = 'NONE'                                              452
         CALL AXMATERIAL                                                  452
C    *                                                                    452
C    Geant ATTRIBUTE : TOKA ===> SEEN=1 COLO=3 <===                       452
         CALL AGSRESET                                                    453
         AG_SEEN = 1                                                      453
         AG_COLO = 3                                                      453
         AG_TITLE = 'TOKA'                                                453
         AG_EXNAME = 'ATTRIBUTE'                                          453
         AG_PARLIST = 'SEEN_COLO'                                         453
         CALL AXATTRIBUTE                                                 453
C    *                                                                    453
C    Geant SHAPE : TUBE ===> RMIN=TOKAIR <===                             453
         CALL AGSRESET                                                    454
         AG_RMIN = TOKAIR                                                 454
         AG_TITLE = 'TUBE'                                                454
         AG_EXNAME = 'SHAPE'                                              454
         AG_PARLIST = 'RMIN'                                              454
         CALL AXSHAPE                                                     454
C    *                                                                    454
C    Create TONX :                                                        455
C    Call To Local Procedure TONX                                         455
         ASSIGN 5410 TO BLOCK_TONX                                        455
         GO TO  5400                                                      455
5410     CONTINUE                                                         455
C    *                                                                    455
C    Geant POSITION : TONX ===> inherited arguments used <===             456
         CALL AGSRESET                                                    456
         AG_TITLE = 'TONX'                                                456
         AG_EXNAME = 'POSITION'                                           456
         AG_PARLIST = 'NONE'                                              456
         CALL AXPOSITION                                                  456
C    *                                                                    456
5381  CALL AGSPOPB                                                        457
      GO TO BLOCK_TOKA, (5380 ,5390)                                      457
C End of Local Procedure                                                  457
C ----------------------------------------------------------------------  458
C ------------ TONX                                                       458
C Begin Local Procedure TONX                                              459
C TONX IS NOMEX SUPPORT                                                   459
5400  CONTINUE                                                            459
         AG_TITLE='TONX'                                                  459
         AG_MODULE=MODULE                                                 459
         AG_MARK='V00'                                                    459
         CALL AGSPUSH                                                     459
         AG_MODULE=MODULE                                                 459
C    *                                                                    459
C    Geant MATERIAL : NOMEX ===> inherited arguments used <===            460
         CALL AGSRESET                                                    460
         AG_TITLE = 'NOMEX'                                               460
         AG_EXNAME = 'MATERIAL'                                           460
         AG_PARLIST = 'NONE'                                              460
         CALL AXMATERIAL                                                  460
C    *                                                                    460
C    Geant ATTRIBUTE : TONX ===> SEEN=1 COLO=2 <===                       460
         CALL AGSRESET                                                    461
         AG_SEEN = 1                                                      461
         AG_COLO = 2                                                      461
         AG_TITLE = 'TONX'                                                461
         AG_EXNAME = 'ATTRIBUTE'                                          461
         AG_PARLIST = 'SEEN_COLO'                                         461
         CALL AXATTRIBUTE                                                 461
C    *                                                                    461
C    Geant SHAPE : TUBE ===> RMIN=TONXIR <===                             461
         CALL AGSRESET                                                    462
         AG_RMIN = TONXIR                                                 462
         AG_TITLE = 'TUBE'                                                462
         AG_EXNAME = 'SHAPE'                                              462
         AG_PARLIST = 'RMIN'                                              462
         CALL AXSHAPE                                                     462
C    *                                                                    462
C    Create TOAD :                                                        463
C    Call To Local Procedure TOAD                                         463
         ASSIGN 5430 TO BLOCK_TOAD                                        463
         GO TO  5420                                                      463
5430     CONTINUE                                                         463
C    *                                                                    463
C    Geant POSITION : TOAD ===> inherited arguments used <===             464
         CALL AGSRESET                                                    464
         AG_TITLE = 'TOAD'                                                464
         AG_EXNAME = 'POSITION'                                           464
         AG_PARLIST = 'NONE'                                              464
         CALL AXPOSITION                                                  464
C    *                                                                    464
5401  CALL AGSPOPB                                                        465
      GO TO BLOCK_TONX, (5400 ,5410)                                      465
C End of Local Procedure                                                  465
C ----------------------------------------------------------------------  466
C ------------ TOAD                                                       466
C Begin Local Procedure TOAD                                              467
C TOAD IS ADHESIVE LAYER                                                  467
5420  CONTINUE                                                            467
         AG_TITLE='TOAD'                                                  467
         AG_MODULE=MODULE                                                 467
         AG_MARK='V00'                                                    467
         CALL AGSPUSH                                                     467
         AG_MODULE=MODULE                                                 467
C    *                                                                    467
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            468
         CALL AGSRESET                                                    468
         AG_TITLE = 'MYLAR'                                               468
         AG_EXNAME = 'MATERIAL'                                           468
         AG_PARLIST = 'NONE'                                              468
         CALL AXMATERIAL                                                  468
C    *                                                                    468
C    Geant ATTRIBUTE : TOAD ===> SEEN=1 COLO=7 <===                       468
         CALL AGSRESET                                                    469
         AG_SEEN = 1                                                      469
         AG_COLO = 7                                                      469
         AG_TITLE = 'TOAD'                                                469
         AG_EXNAME = 'ATTRIBUTE'                                          469
         AG_PARLIST = 'SEEN_COLO'                                         469
         CALL AXATTRIBUTE                                                 469
C    *                                                                    469
C    Geant SHAPE : TUBE ===> RMIN=TOADIR <===                             469
         CALL AGSRESET                                                    470
         AG_RMIN = TOADIR                                                 470
         AG_TITLE = 'TUBE'                                                470
         AG_EXNAME = 'SHAPE'                                              470
         AG_PARLIST = 'RMIN'                                              470
         CALL AXSHAPE                                                     470
C    *                                                                    470
5421  CALL AGSPOPB                                                        471
      GO TO BLOCK_TOAD, (5420 ,5430)                                      471
C End of Local Procedure                                                  471
C ----------------------------------------------------------------------  472
C ------------ TOST                                                       472
C Begin Local Procedure TOST                                              473
C TOST IS THE OUTER FIELD CAGE SUPPORT                                    473
5360  CONTINUE                                                            473
         AG_TITLE='TOST'                                                  473
         AG_MODULE=MODULE                                                 473
         AG_MARK='V00'                                                    473
         CALL AGSPUSH                                                     473
         AG_MODULE=MODULE                                                 473
C    *                                                                    473
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        474
         CALL AGSRESET                                                    474
         AG_TITLE = 'ALUMINIUM'                                           474
         AG_EXNAME = 'MATERIAL'                                           474
         AG_PARLIST = 'NONE'                                              474
         CALL AXMATERIAL                                                  474
C    *                                                                    474
C    Geant ATTRIBUTE : TOST ===> SEEN=1 COLO=2 <===                       474
         CALL AGSRESET                                                    475
         AG_SEEN = 1                                                      475
         AG_COLO = 2                                                      475
         AG_TITLE = 'TOST'                                                475
         AG_EXNAME = 'ATTRIBUTE'                                          475
         AG_PARLIST = 'SEEN_COLO'                                         475
         CALL AXATTRIBUTE                                                 475
C    *                                                                    475
C    Geant SHAPE : TUBE ===> RMIN=TOALIR <===                             475
         CALL AGSRESET                                                    476
         AG_RMIN = TOALIR                                                 476
         AG_TITLE = 'TUBE'                                                476
         AG_EXNAME = 'SHAPE'                                              476
         AG_PARLIST = 'RMIN'                                              476
         CALL AXSHAPE                                                     476
C    *                                                                    476
C    Create TOHA :                                                        477
C    Call To Local Procedure TOHA                                         477
         ASSIGN 5450 TO BLOCK_TOHA                                        477
         GO TO  5440                                                      477
5450     CONTINUE                                                         477
C    *                                                                    477
C    Geant POSITION : TOHA ===> inherited arguments used <===             478
         CALL AGSRESET                                                    478
         AG_TITLE = 'TOHA'                                                478
         AG_EXNAME = 'POSITION'                                           478
         AG_PARLIST = 'NONE'                                              478
         CALL AXPOSITION                                                  478
C    *                                                                    478
5361  CALL AGSPOPB                                                        479
      GO TO BLOCK_TOST, (5360 ,5370)                                      479
C End of Local Procedure                                                  479
C ----------------------------------------------------------------------  480
C ------------ TOHA                                                       480
C Begin Local Procedure TOHA                                              481
C TOHA IS HONEYCOMB/ADHESIVE MIXTURE                                      481
5440  CONTINUE                                                            481
         AG_TITLE='TOHA'                                                  481
         AG_MODULE=MODULE                                                 481
         AG_MARK='V00'                                                    481
         CALL AGSPUSH                                                     481
         AG_MODULE=MODULE                                                 481
C    *                                                                    481
C    Geant COMPONENT : AL ===> A=27 Z=13 W=0.0105 <===                    482
         CALL AGSRESET                                                    482
         AG_A = 27                                                        482
         AG_Z = 13                                                        482
         AG_W = 0.0105                                                    482
         AG_TITLE = 'AL'                                                  482
         AG_EXNAME = 'COMPONENT'                                          482
         AG_PARLIST = 'A_Z_W'                                             482
         CALL AXCOMPONENT                                                 482
C    *                                                                    482
C    Geant COMPONENT : N ===> A=14 Z=7 W=0.7395 <===                      482
         CALL AGSRESET                                                    483
         AG_A = 14                                                        483
         AG_Z = 7                                                         483
         AG_W = 0.7395                                                    483
         AG_TITLE = 'N'                                                   483
         AG_EXNAME = 'COMPONENT'                                          483
         AG_PARLIST = 'A_Z_W'                                             483
         CALL AXCOMPONENT                                                 483
C    *                                                                    483
C    Geant COMPONENT : ADHESIVE ===> A=9 Z=4.5 W=0.2500 <===              483
         CALL AGSRESET                                                    484
         AG_A = 9                                                         484
         AG_Z = 4.5                                                       484
         AG_W = 0.2500                                                    484
         AG_TITLE = 'ADHESIVE'                                            484
         AG_EXNAME = 'COMPONENT'                                          484
         AG_PARLIST = 'A_Z_W'                                             484
         CALL AXCOMPONENT                                                 484
C    *                                                                    484
C    Geant MIXTURE : AL_HONEYCOMB ===> DENS=0.282 <===                    484
         CALL AGSRESET                                                    485
         AG_DENS = 0.282                                                  485
         AG_TITLE = 'AL_HONEYCOMB'                                        485
         AG_EXNAME = 'MIXTURE'                                            485
         AG_PARLIST = 'DENS'                                              485
         CALL AXMIXTURE                                                   485
C    *                                                                    485
C    Geant ATTRIBUTE : TOHA ===> SEEN=1 COLO=7 <===                       485
         CALL AGSRESET                                                    486
         AG_SEEN = 1                                                      486
         AG_COLO = 7                                                      486
         AG_TITLE = 'TOHA'                                                486
         AG_EXNAME = 'ATTRIBUTE'                                          486
         AG_PARLIST = 'SEEN_COLO'                                         486
         CALL AXATTRIBUTE                                                 486
C    *                                                                    486
C    Geant SHAPE : TUBE ===> RMIN=TOHAIR <===                             486
         CALL AGSRESET                                                    487
         AG_RMIN = TOHAIR                                                 487
         AG_TITLE = 'TUBE'                                                487
         AG_EXNAME = 'SHAPE'                                              487
         AG_PARLIST = 'RMIN'                                              487
         CALL AXSHAPE                                                     487
C    *                                                                    487
5441  CALL AGSPOPB                                                        488
      GO TO BLOCK_TOHA, (5440 ,5450)                                      488
C End of Local Procedure                                                  488
C ----------------------------------------------------------------------  489
C ------------ TPCM                                                       489
C Begin Local Procedure TPCM                                              490
C TPCM IS THE CENTRAL MEMBRANE PLACED IN TPC                              490
5120  CONTINUE                                                            490
         AG_TITLE='TPCM'                                                  490
         AG_MODULE=MODULE                                                 490
         AG_MARK='V00'                                                    490
         CALL AGSPUSH                                                     490
         AG_MODULE=MODULE                                                 490
C    *                                                                    490
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            491
         CALL AGSRESET                                                    491
         AG_TITLE = 'MYLAR'                                               491
         AG_EXNAME = 'MATERIAL'                                           491
         AG_PARLIST = 'NONE'                                              491
         CALL AXMATERIAL                                                  491
C    *                                                                    491
C    Geant ATTRIBUTE : TPCM ===> SEEN=1 COLO=4 <===                       491
         CALL AGSRESET                                                    492
         AG_SEEN = 1                                                      492
         AG_COLO = 4                                                      492
         AG_TITLE = 'TPCM'                                                492
         AG_EXNAME = 'ATTRIBUTE'                                          492
         AG_PARLIST = 'SEEN_COLO'                                         492
         CALL AXATTRIBUTE                                                 492
C    *                                                                    492
C    Geant SHAPE : TUBE ===> RMIN=TPGVIR RMAX=TPCG_SENGASOR, DZ=TPCG_MEM  492
C    BTHK/2 <===                                                          492
         CALL AGSRESET                                                    494
         AG_RMIN = TPGVIR                                                 494
         AG_RMAX = TPCG_SENGASOR                                          494
         AG_DZ = TPCG_MEMBTHK/2                                           494
         AG_TITLE = 'TUBE'                                                494
         AG_EXNAME = 'SHAPE'                                              494
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      494
         CALL AXSHAPE                                                     494
C    *                                                                    494
5121  CALL AGSPOPB                                                        495
      GO TO BLOCK_TPCM, (5120 ,5130)                                      495
C End of Local Procedure                                                  495
C ----------------------------------------------------------------------  496
C ------------ TPEA                                                       496
C Begin Local Procedure TPEA                                              497
C TPEA IS ONE ENDCAP PLACED IN TPC                                        497
5140  CONTINUE                                                            497
         AG_TITLE='TPEA'                                                  497
         AG_MODULE=MODULE                                                 497
         AG_MARK='V00'                                                    497
         CALL AGSPUSH                                                     497
         AG_MODULE=MODULE                                                 497
C    *                                                                    497
C    Geant MATERIAL : P10 ===> inherited arguments used <===              498
         CALL AGSRESET                                                    498
         AG_TITLE = 'P10'                                                 498
         AG_EXNAME = 'MATERIAL'                                           498
         AG_PARLIST = 'NONE'                                              498
         CALL AXMATERIAL                                                  498
C    *                                                                    498
C    Geant ATTRIBUTE : TPEA ===> SEEN=0 <===                              498
         CALL AGSRESET                                                    499
         AG_SEEN = 0                                                      499
         AG_TITLE = 'TPEA'                                                499
         AG_EXNAME = 'ATTRIBUTE'                                          499
         AG_PARLIST = 'SEEN'                                              499
         CALL AXATTRIBUTE                                                 499
C    *                                                                    499
C    Geant SHAPE : TUBE ===> RMIN=TPGVIR RMAX=TPCG_SENGASOR, DZ=TPCG_TPE  499
C    ATHK/2 <===                                                          499
         CALL AGSRESET                                                    501
         AG_RMIN = TPGVIR                                                 501
         AG_RMAX = TPCG_SENGASOR                                          501
         AG_DZ = TPCG_TPEATHK/2                                           501
         AG_TITLE = 'TUBE'                                                501
         AG_EXNAME = 'SHAPE'                                              501
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      501
         CALL AXSHAPE                                                     501
C    *                                                                    501
C    Create TESS :                                                        502
C    Call To Local Procedure TESS                                         502
         ASSIGN 5470 TO BLOCK_TESS                                        502
         GO TO  5460                                                      502
5470     CONTINUE                                                         502
C    *                                                                    502
5141  CALL AGSPOPB                                                        503
      GO TO BLOCK_TPEA, (5140 ,5150)                                      503
C End of Local Procedure                                                  503
C ----------------------------------------------------------------------  504
C ------------ TESS                                                       504
C Begin Local Procedure TESS                                              505
C TESS IS A DIVISION OF ENDCAP VOLUME CORRESPONDING TO ONE SUPERSECTOR    505
5460  CONTINUE                                                            505
         AG_TITLE='TESS'                                                  505
         AG_MODULE=MODULE                                                 505
         AG_MARK='V00'                                                    505
         CALL AGSPUSH                                                     505
         AG_MODULE=MODULE                                                 505
C    *                                                                    505
C    Geant ATTRIBUTE : TESS ===> SEEN=1 <===                              506
         CALL AGSRESET                                                    506
         AG_SEEN = 1                                                      506
         AG_TITLE = 'TESS'                                                506
         AG_EXNAME = 'ATTRIBUTE'                                          506
         AG_PARLIST = 'SEEN'                                              506
         CALL AXATTRIBUTE                                                 506
C    *                                                                    506
C    Geant SHAPE : DIVISION ===> NDIV=12 IAXIS=2 <===                     506
         CALL AGSRESET                                                    507
         AG_NDIV = 12                                                     507
         AG_IAXIS = 2                                                     507
         AG_TITLE = 'DIVISION'                                            507
         AG_EXNAME = 'SHAPE'                                              507
         AG_PARLIST = 'NDIV_IAXIS'                                        507
         CALL AXSHAPE                                                     507
C    *                                                                    507
C    *                                                                    507
         I_SEC=1                                                          508
            GO TO 5483                                                    508
5481        I_SEC=I_SEC+1                                                 508
5483        IF(I_SEC-(2).GT.0) GO TO 5482                                 508
C       use TECW bank                                                     509
            BPATH_TECW='TECW*'                                            509
            IDNUM_TECW(1)=0                                               509
            TECW_SEC=I_SEC                                                509
            CALL AGDGETP('TPCEGEO1','*',-999,'TECW',NTECW_SEC,            509
     *      RE_TECW_SEC,BPATH_TECW,IDNUM_TECW,1,MAP_TECW,NAMES_TECW,      509
     *      BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)             509
C       use TROV bank                                                     510
            BPATH_TROV='TROV*'                                            510
            IDNUM_TROV(1)=0                                               510
            TROV_SEC=I_SEC                                                510
            CALL AGDGETP('TPCEGEO1','*',-999,'TROV',NTROV_SEC,            510
     *      RE_TROV_SEC,BPATH_TROV,IDNUM_TROV,1,MAP_TROV,NAMES_TROV,      510
     *      BANK_TROV,LEN_TROV,LLEN_TROV,LINK_TROV,FLAG_TROV)             510
C       Create TSEC :                                                     511
C       Call To Local Procedure TSEC                                      511
            ASSIGN 5500 TO BLOCK_TSEC                                     511
            GO TO  5490                                                   511
5500        CONTINUE                                                      511
C       *                                                                 511
C       Geant POSITION : TSEC ===> X=TECW_RCENTER Z=(TPCG_TPEATHK-TECW_A  513
C       SDEPTH-TECW_PPDEPTH-TECW_GGDEPTH)/2 ORT=YZX <===                  513
            CALL AGSRESET                                                 513
            AG_X = TECW_RCENTER                                           513
            AG_Z =                                                        513
     *      (TPCG_TPEATHK-TECW_ASDEPTH-TECW_PPDEPTH-TECW_GGDEPTH)/2       513
            AG_ALPHAN=AG_ALPHAN+1                                         513
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              513
            AG_TITLE = 'TSEC'                                             513
            AG_EXNAME = 'POSITION'                                        513
            AG_PARLIST = 'X_Z_ORT'                                        513
            CALL AXPOSITION                                               513
C       *                                                                 513
         GO TO 5481                                                       514
5482     CONTINUE                                                         514
5461  CALL AGSPOPB                                                        515
      GO TO BLOCK_TESS, (5460 ,5470)                                      515
C End of Local Procedure                                                  515
C ----------------------------------------------------------------------  516
C ------------ TSEC                                                       516
C Begin Local Procedure TSEC                                              517
C TSEC IS A SUPERSECTOR CONTAINING AL SECTOR, PCB AND MWC VOLUME          517
5490  CONTINUE                                                            517
         AG_TITLE='TSEC'                                                  517
         AG_MODULE=MODULE                                                 517
         AG_MARK='V00'                                                    517
         CALL AGSPUSH                                                     517
         AG_MODULE=MODULE                                                 517
C    *                                                                    517
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        518
         CALL AGSRESET                                                    518
         AG_TITLE = 'ALUMINIUM'                                           518
         AG_EXNAME = 'MATERIAL'                                           518
         AG_PARLIST = 'NONE'                                              518
         CALL AXMATERIAL                                                  518
C    *                                                                    518
C    Geant SHAPE : TRD1 ===> DX1=TECW_INWIDTH/2 DX2=TECW_OUWIDTH/2, DY=(  518
C    TECW_PPDEPTH+TECW_ASDEPTH+TECW_GGDEPTH)/2, DZ=TECW_HEIGHT/2 <===     518
         CALL AGSRESET                                                    521
         AG_DX1 = TECW_INWIDTH/2                                          521
         AG_DX2 = TECW_OUWIDTH/2                                          521
         AG_DY = (TECW_PPDEPTH+TECW_ASDEPTH+TECW_GGDEPTH)/2               521
         AG_DZ = TECW_HEIGHT/2                                            521
         AG_TITLE = 'TRD1'                                                521
         AG_EXNAME = 'SHAPE'                                              521
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     521
         CALL AXSHAPE                                                     521
C    *                                                                    521
C    Create TMWC :                                                        522
C    Call To Local Procedure TMWC                                         522
         ASSIGN 5520 TO BLOCK_TMWC                                        522
         GO TO  5510                                                      522
5520     CONTINUE                                                         522
C    *                                                                    522
C    Geant POSITION : TMWC ===> Y=-(TECW_PPDEPTH+TECW_ASDEPTH)/2 <===     523
         CALL AGSRESET                                                    523
         AG_Y = -(TECW_PPDEPTH+TECW_ASDEPTH)/2                            523
         AG_TITLE = 'TMWC'                                                523
         AG_EXNAME = 'POSITION'                                           523
         AG_PARLIST = 'Y'                                                 523
         CALL AXPOSITION                                                  523
C    *                                                                    523
C    Create TIAG :                                                        524
C    Call To Local Procedure TIAG                                         524
         ASSIGN 5540 TO BLOCK_TIAG                                        524
         GO TO  5530                                                      524
5540     CONTINUE                                                         524
C    *                                                                    524
         DX_DZ =(TECW_OUWIDTH - TECW_INWIDTH)/(2*TECW_HEIGHT)             525
         DXB = TECW_BOUNDARY*SQRT(1. + DX_DZ*DX_DZ)                       526
C    *                                                                    527
         I=1                                                              527
            GO TO 5553                                                    527
5551        I=I+1                                                         527
5553        IF(I-(NINT(TECW_N)).GT.0) GO TO 5552                          527
C       Geant POSITION : TIAG ===> Y=+TECW_PPDEPTH/2+TECW_GGDEPTH/2 Z=-T  531
C       ECW_HEIGHT/2+TECW_Z(I) DX2=TECW_INWIDTH/2-DXB+(TECW_Z(I)+TECW_DZ  531
C       (I)/2)*DX_DZ DX1=TECW_INWIDTH/2-DXB+(TECW_Z(I)-TECW_DZ(I)/2)*DX_  531
C       DZ DZ=TECW_DZ(I)/2 DY=TECW_ASDEPTH/2 KONLY='MANY' <===            531
            CALL AGSRESET                                                 531
            AG_Y = +TECW_PPDEPTH/2+TECW_GGDEPTH/2                         531
            AG_Z = -TECW_HEIGHT/2+TECW_Z(I)                               531
            AG_DX2 = TECW_INWIDTH/2-DXB+(TECW_Z(I)+TECW_DZ(I)/2)*DX_DZ    531
            AG_DX1 = TECW_INWIDTH/2-DXB+(TECW_Z(I)-TECW_DZ(I)/2)*DX_DZ    531
            AG_DZ = TECW_DZ(I)/2                                          531
            AG_DY = TECW_ASDEPTH/2                                        531
            AG_KONLY = 'MANY'                                             531
            AG_TITLE = 'TIAG'                                             531
            AG_EXNAME = 'POSITION'                                        531
            AG_PARLIST = 'Y_Z_DX2_DX1_DZ_DY_KONLY'                        531
            CALL AXPOSITION                                               531
C       *                                                                 531
         GO TO 5551                                                       532
5552     CONTINUE                                                         532
C    Create TCEX :                                                        533
C    Call To Local Procedure TCEX                                         533
         ASSIGN 5570 TO BLOCK_TCEX                                        533
         GO TO  5560                                                      533
5570     CONTINUE                                                         533
C    *                                                                    533
C    Create TAEC :                                                        534
C    Call To Local Procedure TAEC                                         534
         ASSIGN 5590 TO BLOCK_TAEC                                        534
         GO TO  5580                                                      534
5590     CONTINUE                                                         534
C    *                                                                    534
C    Create THOL :                                                        535
C    Call To Local Procedure THOL                                         535
         ASSIGN 5610 TO BLOCK_THOL                                        535
         GO TO  5600                                                      535
5610     CONTINUE                                                         535
C    *                                                                    535
C    Create THRA :                                                        536
C    Call To Local Procedure THRA                                         536
         ASSIGN 5630 TO BLOCK_THRA                                        536
         GO TO  5620                                                      536
5630     CONTINUE                                                         536
C    *                                                                    536
C    Create THLA :                                                        537
C    Call To Local Procedure THLA                                         537
         ASSIGN 5650 TO BLOCK_THLA                                        537
         GO TO  5640                                                      537
5650     CONTINUE                                                         537
C    *                                                                    537
C    *                                                                    538
         I_NHP=1                                                          538
            GO TO 5663                                                    538
5661        I_NHP=I_NHP+1                                                 538
5663        IF(I_NHP-(16).GT.0) GO TO 5662                                538
C       FIRST PL                                                          539
            HPLA = -TECW_HEIGHT/2+TECW_Z(1)-TECW_DZ(1)/                   539
     *      2+TROV_OFFCARDINOUT                                           539
            ZVALUE = HPLA+4.0*(I_NHP-1)                                   540
C       Check TROV_NHP(I_NHP)>0                                           541
            IF (TROV_NHP(I_NHP).GT.0) THEN                                541
C       *                                                                 542
            J_NHO=1                                                       542
               GO TO 5673                                                 542
5671           J_NHO=J_NHO+1                                              542
5673           IF(J_NHO-(TROV_NHP(I_NHP)).GT.0) GO TO 5672                542
C          Geant POSITION : TCEX ===> X=TROV_HX(J_NHO,I_NHP) Y=+TECW_PPD  544
C          EPTH/2+TECW_GGDEPTH/2 Z=ZVALUE+TECW_CARDOFF <===               544
               CALL AGSRESET                                              544
               AG_X = TROV_HX(J_NHO,I_NHP)                                544
               AG_Y = +TECW_PPDEPTH/2+TECW_GGDEPTH/2                      544
               AG_Z = ZVALUE+TECW_CARDOFF                                 544
               AG_TITLE = 'TCEX'                                          544
               AG_EXNAME = 'POSITION'                                     544
               AG_PARLIST = 'X_Y_Z'                                       544
               CALL AXPOSITION                                            544
C          *                                                              544
C          Geant POSITION : TAEC ===> X=TROV_HX(J_NHO,I_NHP) Y=+TECW_PPD  544
C          EPTH/2+TECW_GGDEPTH/2 Z=ZVALUE+TECW_COOLOFF <===               544
               CALL AGSRESET                                              546
               AG_X = TROV_HX(J_NHO,I_NHP)                                546
               AG_Y = +TECW_PPDEPTH/2+TECW_GGDEPTH/2                      546
               AG_Z = ZVALUE+TECW_COOLOFF                                 546
               AG_TITLE = 'TAEC'                                          546
               AG_EXNAME = 'POSITION'                                     546
               AG_PARLIST = 'X_Y_Z'                                       546
               CALL AXPOSITION                                            546
C          *                                                              546
C          Geant POSITION : THOL ===> X=TROV_HX(J_NHO,I_NHP) Y=+TECW_GGD  546
C          EPTH/2-TECW_ASDEPTH/2 Z=ZVALUE+TECW_CARDOFF <===               546
               CALL AGSRESET                                              548
               AG_X = TROV_HX(J_NHO,I_NHP)                                548
               AG_Y = +TECW_GGDEPTH/2-TECW_ASDEPTH/2                      548
               AG_Z = ZVALUE+TECW_CARDOFF                                 548
               AG_TITLE = 'THOL'                                          548
               AG_EXNAME = 'POSITION'                                     548
               AG_PARLIST = 'X_Y_Z'                                       548
               CALL AXPOSITION                                            548
C          *                                                              548
C          Geant POSITION : THRA ===> X=TROV_HX(J_NHO,I_NHP)+TECW_SLOTW/  548
C          2 Y=+TECW_GGDEPTH/2-TECW_ASDEPTH/2 Z=ZVALUE+TECW_CARDOFF ORT=  548
C          ZXY <===                                                       548
               CALL AGSRESET                                              550
               AG_X = TROV_HX(J_NHO,I_NHP)+TECW_SLOTW/2                   550
               AG_Y = +TECW_GGDEPTH/2-TECW_ASDEPTH/2                      550
               AG_Z = ZVALUE+TECW_CARDOFF                                 550
               AG_ALPHAN=AG_ALPHAN+1                                      550
               AG_ALPHAK(AG_ALPHAN)=IGOR('ZXY')                           550
               AG_TITLE = 'THRA'                                          550
               AG_EXNAME = 'POSITION'                                     550
               AG_PARLIST = 'X_Y_Z_ORT'                                   550
               CALL AXPOSITION                                            550
C          *                                                              550
C          Geant POSITION : THLA ===> X=TROV_HX(J_NHO,I_NHP)-TECW_SLOTW/  550
C          2 Y=+TECW_GGDEPTH/2-TECW_ASDEPTH/2 Z=ZVALUE+TECW_CARDOFF ORT=  550
C          ZXY <===                                                       550
               CALL AGSRESET                                              552
               AG_X = TROV_HX(J_NHO,I_NHP)-TECW_SLOTW/2                   552
               AG_Y = +TECW_GGDEPTH/2-TECW_ASDEPTH/2                      552
               AG_Z = ZVALUE+TECW_CARDOFF                                 552
               AG_ALPHAN=AG_ALPHAN+1                                      552
               AG_ALPHAK(AG_ALPHAN)=IGOR('ZXY')                           552
               AG_TITLE = 'THLA'                                          552
               AG_EXNAME = 'POSITION'                                     552
               AG_PARLIST = 'X_Y_Z_ORT'                                   552
               CALL AXPOSITION                                            552
C          *                                                              552
            GO TO 5671                                                    553
5672        CONTINUE                                                      553
            ENDIF                                                         554
         GO TO 5661                                                       555
5662     CONTINUE                                                         555
C    Create TOAE :                                                        556
C    Call To Local Procedure TOAE                                         556
         ASSIGN 5690 TO BLOCK_TOAE                                        556
         GO TO  5680                                                      556
5690     CONTINUE                                                         556
C    *                                                                    556
C    *                                                                    557
         I=1                                                              557
            GO TO 5703                                                    557
5701        I=I+1                                                         557
5703        IF(I-(NINT(TECW_NEX)).GT.0) GO TO 5702                        557
C       Geant POSITION : TOAE ===> X=TECW_XEX(I) Y=+TECW_PPDEPTH/2+TECW_  560
C       GGDEPTH/2 Z=TECW_ZEX(I)-TECW_HEIGHT/2 DZ=TECW_DZEX(I)/2 DX=TECW_  560
C       DXEX(I)/2 DY=TECW_ASDEPTH/2 <===                                  560
            CALL AGSRESET                                                 560
            AG_X = TECW_XEX(I)                                            560
            AG_Y = +TECW_PPDEPTH/2+TECW_GGDEPTH/2                         560
            AG_Z = TECW_ZEX(I)-TECW_HEIGHT/2                              560
            AG_DZ = TECW_DZEX(I)/2                                        560
            AG_DX = TECW_DXEX(I)/2                                        560
            AG_DY = TECW_ASDEPTH/2                                        560
            AG_TITLE = 'TOAE'                                             560
            AG_EXNAME = 'POSITION'                                        560
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 560
            CALL AXPOSITION                                               560
C       *                                                                 560
         GO TO 5701                                                       561
5702     CONTINUE                                                         561
5491  CALL AGSPOPB                                                        562
      GO TO BLOCK_TSEC, (5490 ,5500)                                      562
C End of Local Procedure                                                  562
C ----------------------------------------------------------------------  563
C ------------ TMWC                                                       563
C Begin Local Procedure TMWC                                              564
C TMWC IS A WIRE CHAMBER VOLUME WITH BOTH GATED AND SENSITIVE VOLUMES     564
5510  CONTINUE                                                            564
         AG_TITLE='TMWC'                                                  564
         AG_MODULE=MODULE                                                 564
         AG_MARK='V00'                                                    564
         CALL AGSPUSH                                                     564
         AG_MODULE=MODULE                                                 564
C    *                                                                    564
C    Geant MATERIAL : P10 ===> inherited arguments used <===              565
         CALL AGSRESET                                                    565
         AG_TITLE = 'P10'                                                 565
         AG_EXNAME = 'MATERIAL'                                           565
         AG_PARLIST = 'NONE'                                              565
         CALL AXMATERIAL                                                  565
C    *                                                                    565
C    Geant ATTRIBUTE : TMWC ===> SEEN=1 COLO=3 <===                       565
         CALL AGSRESET                                                    566
         AG_SEEN = 1                                                      566
         AG_COLO = 3                                                      566
         AG_TITLE = 'TMWC'                                                566
         AG_EXNAME = 'ATTRIBUTE'                                          566
         AG_PARLIST = 'SEEN_COLO'                                         566
         CALL AXATTRIBUTE                                                 566
C    *                                                                    566
C    Geant SHAPE : TRD1 ===> DX1=TECW_INWIDTH/2 DX2=TECW_OUWIDTH/2, DY=T  566
C    ECW_GGDEPTH/2 DZ=TECW_HEIGHT/2 <===                                  566
         CALL AGSRESET                                                    568
         AG_DX1 = TECW_INWIDTH/2                                          568
         AG_DX2 = TECW_OUWIDTH/2                                          568
         AG_DY = TECW_GGDEPTH/2                                           568
         AG_DZ = TECW_HEIGHT/2                                            568
         AG_TITLE = 'TRD1'                                                568
         AG_EXNAME = 'SHAPE'                                              568
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     568
         CALL AXSHAPE                                                     568
C    *                                                                    568
C    Skip Unless TPCG_MWCREAD>0                                           568
         IF (TPCG_MWCREAD.LE.0)GO TO 5511                                 569
C    Create TMEA :                                                        570
C    Call To Local Procedure TMEA                                         570
         ASSIGN 5720 TO BLOCK_TMEA                                        570
         GO TO  5710                                                      570
5720     CONTINUE                                                         570
C    *                                                                    570
C    Geant POSITION : TMEA ===> Y=+(TECW_GGDEPTH-TECW_MWCDEPTH)/2 Z=TECW  571
C    _MWCCENT <===                                                        571
         CALL AGSRESET                                                    571
         AG_Y = +(TECW_GGDEPTH-TECW_MWCDEPTH)/2                           571
         AG_Z = TECW_MWCCENT                                              571
         AG_TITLE = 'TMEA'                                                571
         AG_EXNAME = 'POSITION'                                           571
         AG_PARLIST = 'Y_Z'                                               571
         CALL AXPOSITION                                                  571
C    *                                                                    571
5511  CALL AGSPOPB                                                        572
      GO TO BLOCK_TMWC, (5510 ,5520)                                      572
C End of Local Procedure                                                  572
C ----------------------------------------------------------------------  573
C ------------ TMEA                                                       573
C Begin Local Procedure TMEA                                              574
C TMEA IS A DOUBLE SENSITIVE LAYER AROUND GATING GRID                     574
5710  CONTINUE                                                            574
         AG_TITLE='TMEA'                                                  574
         AG_MODULE=MODULE                                                 574
         AG_MARK='V00'                                                    574
         CALL AGSPUSH                                                     574
         AG_MODULE=MODULE                                                 574
C    *                                                                    574
C    Geant MATERIAL : P10 ===> inherited arguments used <===              575
         CALL AGSRESET                                                    575
         AG_TITLE = 'P10'                                                 575
         AG_EXNAME = 'MATERIAL'                                           575
         AG_PARLIST = 'NONE'                                              575
         CALL AXMATERIAL                                                  575
C    *                                                                    575
C    Geant MATERIAL : SENSITIVE_GAS ===> ISVOL=1 STEMAX=5 <===            575
         CALL AGSRESET                                                    576
         AG_ISVOL = 1                                                     576
         AG_STEMAX = 5                                                    576
         AG_TITLE = 'SENSITIVE_GAS'                                       576
         AG_EXNAME = 'MATERIAL'                                           576
         AG_PARLIST = 'ISVOL_STEMAX'                                      576
         CALL AXMATERIAL                                                  576
C    *                                                                    576
C    Geant ATTRIBUTE : TMEA ===> SEEN=1 COLO=4 <===                       576
         CALL AGSRESET                                                    577
         AG_SEEN = 1                                                      577
         AG_COLO = 4                                                      577
         AG_TITLE = 'TMEA'                                                577
         AG_EXNAME = 'ATTRIBUTE'                                          577
         AG_PARLIST = 'SEEN_COLO'                                         577
         CALL AXATTRIBUTE                                                 577
C    *                                                                    577
C    Geant SHAPE : TRD1 ===> DX1=TECW_MWCINN/2 DX2=TECW_MWCOUT/2, DY=TEC  577
C    W_MWCDEPTH/2 DZ=TECW_MWCHEI/2 <===                                   577
         CALL AGSRESET                                                    579
         AG_DX1 = TECW_MWCINN/2                                           579
         AG_DX2 = TECW_MWCOUT/2                                           579
         AG_DY = TECW_MWCDEPTH/2                                          579
         AG_DZ = TECW_MWCHEI/2                                            579
         AG_TITLE = 'TRD1'                                                579
         AG_EXNAME = 'SHAPE'                                              579
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     579
         CALL AXSHAPE                                                     579
C    *                                                                    579
C    Create TMSE :                                                        580
C    Call To Local Procedure TMSE                                         580
         ASSIGN 5740 TO BLOCK_TMSE                                        580
         GO TO  5730                                                      580
5740     CONTINUE                                                         580
C    *                                                                    580
         CALL GSTPAR(AG_IMED, 'STRA', 1.)                                 581
5711  CALL AGSPOPB                                                        582
      GO TO BLOCK_TMEA, (5710 ,5720)                                      582
C End of Local Procedure                                                  582
C ----------------------------------------------------------------------  583
C ------------ TMSE                                                       583
C Begin Local Procedure TMSE                                              584
C TMSE IS A SINGLE SENSITIVE VOLUME                                       584
5730  CONTINUE                                                            584
         AG_TITLE='TMSE'                                                  584
         AG_MODULE=MODULE                                                 584
         AG_MARK='V00'                                                    584
         CALL AGSPUSH                                                     584
         AG_MODULE=MODULE                                                 584
C    *                                                                    584
C    Geant SHAPE : DIVISION ===> NDIV=TECW_MWCNWIR IAXIS=3 <===           585
         CALL AGSRESET                                                    585
         AG_NDIV = TECW_MWCNWIR                                           585
         AG_IAXIS = 3                                                     585
         AG_TITLE = 'DIVISION'                                            585
         AG_EXNAME = 'SHAPE'                                              585
         AG_PARLIST = 'NDIV_IAXIS'                                        585
         CALL AXSHAPE                                                     585
C    *                                                                    585
C    Check TPCG_MWCREAD==1                                                585
         IF (TPCG_MWCREAD.EQ.1) THEN                                      586
C    Defining Hits: Z:0.01:S Y:0.01: Z:0.01:(-TECW_MWCOUT/2,+TECW_MWCOUT  589
C    /2) CX:10: CY:10:, CZ:10: SLENG:0.1:(0,800) STEP:0.1: TOF:16:(0,1.0  589
C    E-6) PTOT:16:(0,100) LGAM:16:(-2,+2), ELOS:16:(0.,0.01)              589
         AG_TITLE = 'TMSE'                                                589
         AG_EXNAME = 'HITS'                                               589
         AG_TYPE=AG_MAGIC                                                 589
         CALL AGSDETV('TMSE')                                             589
C    *                                                                    589
         AG_PARLIST='Z'                                                   589
         AG_TITLE = 'DETH'                                                589
         AG_NBITS = AG_MAGIC                                              589
         AG_BIN=0.01                                                      589
         AG_OPTION='S'                                                    589
         AG_FHMIN = AG_MAGIC                                              589
         AG_FHMAX = AG_MAGIC                                              589
         CALL AGSDETH (0)                                                 589
C    *                                                                    589
         AG_PARLIST='Y'                                                   589
         AG_TITLE = 'DETH'                                                589
         AG_NBITS = AG_MAGIC                                              589
         AG_BIN=0.01                                                      589
         AG_FHMIN=AG_MAGIC                                                589
         AG_FHMAX=AG_MAGIC                                                589
         AG_OPTION='*'                                                    589
         CALL AGSDETH (0)                                                 589
C    *                                                                    589
         AG_PARLIST='Z'                                                   589
         AG_TITLE = 'DETH'                                                589
         AG_NBITS = AG_MAGIC                                              589
         AG_BIN=0.01                                                      589
         AG_OPTION='*'                                                    589
         AG_FHMIN = -TECW_MWCOUT/2                                        589
         AG_FHMAX = +TECW_MWCOUT/2                                        589
         CALL AGSDETH (0)                                                 589
C    *                                                                    589
         AG_PARLIST='CX'                                                  589
         AG_TITLE = 'DETH'                                                589
         AG_NBITS = 10                                                    589
         AG_BIN=AG_MAGIC                                                  589
         AG_FHMIN=AG_MAGIC                                                589
         AG_FHMAX=AG_MAGIC                                                589
         AG_OPTION='*'                                                    589
         CALL AGSDETH (0)                                                 589
C    *                                                                    589
         AG_PARLIST='CY'                                                  589
         AG_TITLE = 'DETH'                                                589
         AG_NBITS = 10                                                    589
         AG_BIN=AG_MAGIC                                                  589
         AG_OPTION='*'                                                    589
         AG_FHMIN = AG_MAGIC                                              589
         AG_FHMAX = AG_MAGIC                                              589
         CALL AGSDETH (0)                                                 589
C    *                                                                    589
         AG_PARLIST='CZ'                                                  589
         AG_TITLE = 'DETH'                                                589
         AG_NBITS = 10                                                    589
         AG_BIN=AG_MAGIC                                                  589
         AG_FHMIN=AG_MAGIC                                                589
         AG_FHMAX=AG_MAGIC                                                589
         AG_OPTION='*'                                                    589
         CALL AGSDETH (0)                                                 589
C    *                                                                    589
         AG_PARLIST='SLENG'                                               589
         AG_TITLE = 'DETH'                                                589
         AG_NBITS = AG_MAGIC                                              589
         AG_BIN=0.1                                                       589
         AG_OPTION='*'                                                    589
         AG_FHMIN = 0                                                     589
         AG_FHMAX = 800                                                   589
         CALL AGSDETH (0)                                                 589
C    *                                                                    589
         AG_PARLIST='STEP'                                                589
         AG_TITLE = 'DETH'                                                589
         AG_NBITS = AG_MAGIC                                              589
         AG_BIN=0.1                                                       589
         AG_FHMIN=AG_MAGIC                                                589
         AG_FHMAX=AG_MAGIC                                                589
         AG_OPTION='*'                                                    589
         CALL AGSDETH (0)                                                 589
C    *                                                                    589
         AG_PARLIST='TOF'                                                 589
         AG_TITLE = 'DETH'                                                589
         AG_NBITS = 16                                                    589
         AG_BIN=AG_MAGIC                                                  589
         AG_OPTION='*'                                                    589
         AG_FHMIN = 0                                                     589
         AG_FHMAX = 1.0E-6                                                589
         CALL AGSDETH (0)                                                 589
C    *                                                                    589
         AG_PARLIST='PTOT'                                                589
         AG_TITLE = 'DETH'                                                589
         AG_NBITS = 16                                                    589
         AG_BIN=AG_MAGIC                                                  589
         AG_OPTION='*'                                                    589
         AG_FHMIN = 0                                                     589
         AG_FHMAX = 100                                                   589
         CALL AGSDETH (0)                                                 589
C    *                                                                    589
         AG_PARLIST='LGAM'                                                589
         AG_TITLE = 'DETH'                                                589
         AG_NBITS = 16                                                    589
         AG_BIN=AG_MAGIC                                                  589
         AG_OPTION='*'                                                    589
         AG_FHMIN = -2                                                    589
         AG_FHMAX = +2                                                    589
         CALL AGSDETH (0)                                                 589
C    *                                                                    589
         AG_PARLIST='ELOS'                                                589
         AG_TITLE = 'DETH'                                                589
         AG_NBITS = 16                                                    589
         AG_BIN=AG_MAGIC                                                  589
         AG_OPTION='*'                                                    589
         AG_FHMIN = 0.                                                    589
         AG_FHMAX = 0.01                                                  589
         CALL AGSDETH (0)                                                 589
C    *                                                                    589
         CALL AGSDETU('TMSE')                                             589
C    *                                                                    589
         ELSE                                                             590
C    Defining Hits: Z:0.01:S Y:0.01: Z:0.01: CX:10: CY:10: CZ:10: SLENG:  592
C    0.1:(0,800) STEP:0.1:, TOF:16:(0,1.0E-6) PTOT:16:(0,100) LGAM:16:(-  592
C    2,+2) ELOS:16:(0.,0.01)                                              592
         AG_TITLE = 'TMSE'                                                592
         AG_EXNAME = 'HITS'                                               592
         AG_TYPE=AG_MAGIC                                                 592
         CALL AGSDETV('TMSE')                                             592
C    *                                                                    592
         AG_PARLIST='Z'                                                   592
         AG_TITLE = 'DETH'                                                592
         AG_NBITS = AG_MAGIC                                              592
         AG_BIN=0.01                                                      592
         AG_OPTION='S'                                                    592
         AG_FHMIN = AG_MAGIC                                              592
         AG_FHMAX = AG_MAGIC                                              592
         CALL AGSDETH (0)                                                 592
C    *                                                                    592
         AG_PARLIST='Y'                                                   592
         AG_TITLE = 'DETH'                                                592
         AG_NBITS = AG_MAGIC                                              592
         AG_BIN=0.01                                                      592
         AG_FHMIN=AG_MAGIC                                                592
         AG_FHMAX=AG_MAGIC                                                592
         AG_OPTION='*'                                                    592
         CALL AGSDETH (0)                                                 592
C    *                                                                    592
         AG_PARLIST='Z'                                                   592
         AG_TITLE = 'DETH'                                                592
         AG_NBITS = AG_MAGIC                                              592
         AG_BIN=0.01                                                      592
         AG_FHMIN=AG_MAGIC                                                592
         AG_FHMAX=AG_MAGIC                                                592
         AG_OPTION='*'                                                    592
         CALL AGSDETH (0)                                                 592
C    *                                                                    592
         AG_PARLIST='CX'                                                  592
         AG_TITLE = 'DETH'                                                592
         AG_NBITS = 10                                                    592
         AG_BIN=AG_MAGIC                                                  592
         AG_FHMIN=AG_MAGIC                                                592
         AG_FHMAX=AG_MAGIC                                                592
         AG_OPTION='*'                                                    592
         CALL AGSDETH (0)                                                 592
C    *                                                                    592
         AG_PARLIST='CY'                                                  592
         AG_TITLE = 'DETH'                                                592
         AG_NBITS = 10                                                    592
         AG_BIN=AG_MAGIC                                                  592
         AG_FHMIN=AG_MAGIC                                                592
         AG_FHMAX=AG_MAGIC                                                592
         AG_OPTION='*'                                                    592
         CALL AGSDETH (0)                                                 592
C    *                                                                    592
         AG_PARLIST='CZ'                                                  592
         AG_TITLE = 'DETH'                                                592
         AG_NBITS = 10                                                    592
         AG_BIN=AG_MAGIC                                                  592
         AG_FHMIN=AG_MAGIC                                                592
         AG_FHMAX=AG_MAGIC                                                592
         AG_OPTION='*'                                                    592
         CALL AGSDETH (0)                                                 592
C    *                                                                    592
         AG_PARLIST='SLENG'                                               592
         AG_TITLE = 'DETH'                                                592
         AG_NBITS = AG_MAGIC                                              592
         AG_BIN=0.1                                                       592
         AG_OPTION='*'                                                    592
         AG_FHMIN = 0                                                     592
         AG_FHMAX = 800                                                   592
         CALL AGSDETH (0)                                                 592
C    *                                                                    592
         AG_PARLIST='STEP'                                                592
         AG_TITLE = 'DETH'                                                592
         AG_NBITS = AG_MAGIC                                              592
         AG_BIN=0.1                                                       592
         AG_OPTION='*'                                                    592
         AG_FHMIN = AG_MAGIC                                              592
         AG_FHMAX = AG_MAGIC                                              592
         CALL AGSDETH (0)                                                 592
C    *                                                                    592
         AG_PARLIST='TOF'                                                 592
         AG_TITLE = 'DETH'                                                592
         AG_NBITS = 16                                                    592
         AG_BIN=AG_MAGIC                                                  592
         AG_OPTION='*'                                                    592
         AG_FHMIN = 0                                                     592
         AG_FHMAX = 1.0E-6                                                592
         CALL AGSDETH (0)                                                 592
C    *                                                                    592
         AG_PARLIST='PTOT'                                                592
         AG_TITLE = 'DETH'                                                592
         AG_NBITS = 16                                                    592
         AG_BIN=AG_MAGIC                                                  592
         AG_OPTION='*'                                                    592
         AG_FHMIN = 0                                                     592
         AG_FHMAX = 100                                                   592
         CALL AGSDETH (0)                                                 592
C    *                                                                    592
         AG_PARLIST='LGAM'                                                592
         AG_TITLE = 'DETH'                                                592
         AG_NBITS = 16                                                    592
         AG_BIN=AG_MAGIC                                                  592
         AG_OPTION='*'                                                    592
         AG_FHMIN = -2                                                    592
         AG_FHMAX = +2                                                    592
         CALL AGSDETH (0)                                                 592
C    *                                                                    592
         AG_PARLIST='ELOS'                                                592
         AG_TITLE = 'DETH'                                                592
         AG_NBITS = 16                                                    592
         AG_BIN=AG_MAGIC                                                  592
         AG_OPTION='*'                                                    592
         AG_FHMIN = 0.                                                    592
         AG_FHMAX = 0.01                                                  592
         CALL AGSDETH (0)                                                 592
C    *                                                                    592
         CALL AGSDETU('TMSE')                                             592
C    *                                                                    592
         ENDIF                                                            593
5731  CALL AGSPOPB                                                        594
      GO TO BLOCK_TMSE, (5730 ,5740)                                      594
C End of Local Procedure                                                  594
C ----------------------------------------------------------------------  595
C ------------ THOL                                                       595
C Begin Local Procedure THOL                                              596
C THOL IS PART OF A HOLE FOR PC BOARDS                                    596
5600  CONTINUE                                                            596
         AG_TITLE='THOL'                                                  596
         AG_MODULE=MODULE                                                 596
         AG_MARK='V00'                                                    596
         CALL AGSPUSH                                                     596
         AG_MODULE=MODULE                                                 596
C    *                                                                    596
C    Geant MATERIAL : AIR ===> inherited arguments used <===              597
         CALL AGSRESET                                                    597
         AG_TITLE = 'AIR'                                                 597
         AG_EXNAME = 'MATERIAL'                                           597
         AG_PARLIST = 'NONE'                                              597
         CALL AXMATERIAL                                                  597
C    *                                                                    597
C    Geant SHAPE : BOX ===> DX=TECW_SLOTW/2 DY=TECW_PPDEPTH/2, DZ=TECW_S  597
C    LOTRAD <===                                                          597
         CALL AGSRESET                                                    599
         AG_DX = TECW_SLOTW/2                                             599
         AG_DY = TECW_PPDEPTH/2                                           599
         AG_DZ = TECW_SLOTRAD                                             599
         AG_TITLE = 'BOX'                                                 599
         AG_EXNAME = 'SHAPE'                                              599
         AG_PARLIST = 'DX_DY_DZ'                                          599
         CALL AXSHAPE                                                     599
C    *                                                                    599
5601  CALL AGSPOPB                                                        600
      GO TO BLOCK_THOL, (5600 ,5610)                                      600
C End of Local Procedure                                                  600
C ----------------------------------------------------------------------  601
C ------------ THRA                                                       601
C Begin Local Procedure THRA                                              602
C THRA IS PART OF A HOLE FOR PC BOARDS                                    602
5620  CONTINUE                                                            602
         AG_TITLE='THRA'                                                  602
         AG_MODULE=MODULE                                                 602
         AG_MARK='V00'                                                    602
         CALL AGSPUSH                                                     602
         AG_MODULE=MODULE                                                 602
C    *                                                                    602
C    Geant MATERIAL : AIR ===> inherited arguments used <===              603
         CALL AGSRESET                                                    603
         AG_TITLE = 'AIR'                                                 603
         AG_EXNAME = 'MATERIAL'                                           603
         AG_PARLIST = 'NONE'                                              603
         CALL AXMATERIAL                                                  603
C    *                                                                    603
C    Geant SHAPE : TUBS ===> RMIN=0.0 RMAX=TECW_SLOTRAD PHI1=0.0, PHI2=1  603
C    80.0 DZ=TECW_PPDEPTH/2 <===                                          603
         CALL AGSRESET                                                    605
         AG_RMIN = 0.0                                                    605
         AG_RMAX = TECW_SLOTRAD                                           605
         AG_PHI1 = 0.0                                                    605
         AG_PHI2 = 180.0                                                  605
         AG_DZ = TECW_PPDEPTH/2                                           605
         AG_TITLE = 'TUBS'                                                605
         AG_EXNAME = 'SHAPE'                                              605
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            605
         CALL AXSHAPE                                                     605
C    *                                                                    605
5621  CALL AGSPOPB                                                        606
      GO TO BLOCK_THRA, (5620 ,5630)                                      606
C End of Local Procedure                                                  606
C ----------------------------------------------------------------------  607
C ------------ THLA                                                       607
C Begin Local Procedure THLA                                              608
C THLA IS PART OF A HOLE FOR PC BOARDS                                    608
5640  CONTINUE                                                            608
         AG_TITLE='THLA'                                                  608
         AG_MODULE=MODULE                                                 608
         AG_MARK='V00'                                                    608
         CALL AGSPUSH                                                     608
         AG_MODULE=MODULE                                                 608
C    *                                                                    608
C    Geant MATERIAL : AIR ===> inherited arguments used <===              609
         CALL AGSRESET                                                    609
         AG_TITLE = 'AIR'                                                 609
         AG_EXNAME = 'MATERIAL'                                           609
         AG_PARLIST = 'NONE'                                              609
         CALL AXMATERIAL                                                  609
C    *                                                                    609
C    Geant SHAPE : TUBS ===> RMIN=0.0 RMAX=TECW_SLOTRAD PHI1=180.0, PHI2  609
C    =360.0 DZ=TECW_PPDEPTH/2 <===                                        609
         CALL AGSRESET                                                    611
         AG_RMIN = 0.0                                                    611
         AG_RMAX = TECW_SLOTRAD                                           611
         AG_PHI1 = 180.0                                                  611
         AG_PHI2 = 360.0                                                  611
         AG_DZ = TECW_PPDEPTH/2                                           611
         AG_TITLE = 'TUBS'                                                611
         AG_EXNAME = 'SHAPE'                                              611
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            611
         CALL AXSHAPE                                                     611
C    *                                                                    611
5641  CALL AGSPOPB                                                        612
      GO TO BLOCK_THLA, (5640 ,5650)                                      612
C End of Local Procedure                                                  612
C ----------------------------------------------------------------------  613
C ------------ TIAG                                                       613
C Begin Local Procedure TIAG                                              614
C TIAG IS AN AIR GAP IN INNER SECTOR ALUMINUM STRUCTURE                   614
5530  CONTINUE                                                            614
         AG_TITLE='TIAG'                                                  614
         AG_MODULE=MODULE                                                 614
         AG_MARK='V00'                                                    614
         CALL AGSPUSH                                                     614
         AG_MODULE=MODULE                                                 614
C    *                                                                    614
C    Geant MATERIAL : AIR ===> inherited arguments used <===              615
         CALL AGSRESET                                                    615
         AG_TITLE = 'AIR'                                                 615
         AG_EXNAME = 'MATERIAL'                                           615
         AG_PARLIST = 'NONE'                                              615
         CALL AXMATERIAL                                                  615
C    *                                                                    615
C    Geant SHAPE : TRD1 ===> DX1=0 DX2=0 DY=0 DZ=0 <===                   615
         CALL AGSRESET                                                    616
         AG_DX1 = 0                                                       616
         AG_DX2 = 0                                                       616
         AG_DY = 0                                                        616
         AG_DZ = 0                                                        616
         AG_TITLE = 'TRD1'                                                616
         AG_EXNAME = 'SHAPE'                                              616
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     616
         CALL AXSHAPE                                                     616
C    *                                                                    616
5531  CALL AGSPOPB                                                        617
      GO TO BLOCK_TIAG, (5530 ,5540)                                      617
C End of Local Procedure                                                  617
C ----------------------------------------------------------------------  618
C ------------ TOAE                                                       618
C Begin Local Procedure TOAE                                              619
C TOAE IS EXTRA ALUMINUM SUPPORTS IN THE AIR OPENNINGS                    619
5680  CONTINUE                                                            619
         AG_TITLE='TOAE'                                                  619
         AG_MODULE=MODULE                                                 619
         AG_MARK='V00'                                                    619
         CALL AGSPUSH                                                     619
         AG_MODULE=MODULE                                                 619
C    *                                                                    619
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        620
         CALL AGSRESET                                                    620
         AG_TITLE = 'ALUMINIUM'                                           620
         AG_EXNAME = 'MATERIAL'                                           620
         AG_PARLIST = 'NONE'                                              620
         CALL AXMATERIAL                                                  620
C    *                                                                    620
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           620
         CALL AGSRESET                                                    621
         AG_DX = 0                                                        621
         AG_DY = 0                                                        621
         AG_DZ = 0                                                        621
         AG_TITLE = 'BOX'                                                 621
         AG_EXNAME = 'SHAPE'                                              621
         AG_PARLIST = 'DX_DY_DZ'                                          621
         CALL AXSHAPE                                                     621
C    *                                                                    621
5681  CALL AGSPOPB                                                        622
      GO TO BLOCK_TOAE, (5680 ,5690)                                      622
C End of Local Procedure                                                  622
C ----------------------------------------------------------------------  623
C ------------ TCEX                                                       623
C Begin Local Procedure TCEX                                              624
C TCEX IS PART OF THE G10 FOR THE PC BOARDS                               624
5560  CONTINUE                                                            624
         AG_TITLE='TCEX'                                                  624
         AG_MODULE=MODULE                                                 624
         AG_MARK='V00'                                                    624
         CALL AGSPUSH                                                     624
         AG_MODULE=MODULE                                                 624
C    *                                                                    624
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===          625
         CALL AGSRESET                                                    625
         AG_A = 28.08                                                     625
         AG_Z = 14                                                        625
         AG_W = 0.6*1*28./60.                                             625
         AG_TITLE = 'SI'                                                  625
         AG_EXNAME = 'COMPONENT'                                          625
         AG_PARLIST = 'A_Z_W'                                             625
         CALL AXCOMPONENT                                                 625
C    *                                                                    625
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174.   625
C    <===                                                                 625
         CALL AGSRESET                                                    626
         AG_A = 16                                                        626
         AG_Z = 8                                                         626
         AG_W = 0.6*2*16./60. + 0.4*4*16./174.                            626
         AG_TITLE = 'O'                                                   626
         AG_EXNAME = 'COMPONENT'                                          626
         AG_PARLIST = 'A_Z_W'                                             626
         CALL AXCOMPONENT                                                 626
C    *                                                                    626
C    Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===              626
         CALL AGSRESET                                                    627
         AG_A = 12                                                        627
         AG_Z = 6                                                         627
         AG_W = 0.4*8*12./174.                                            627
         AG_TITLE = 'C'                                                   627
         AG_EXNAME = 'COMPONENT'                                          627
         AG_PARLIST = 'A_Z_W'                                             627
         CALL AXCOMPONENT                                                 627
C    *                                                                    627
C    Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===               627
         CALL AGSRESET                                                    628
         AG_A = 1                                                         628
         AG_Z = 1                                                         628
         AG_W = 0.4*14*1./174.                                            628
         AG_TITLE = 'H'                                                   628
         AG_EXNAME = 'COMPONENT'                                          628
         AG_PARLIST = 'A_Z_W'                                             628
         CALL AXCOMPONENT                                                 628
C    *                                                                    628
C    Geant MIXTURE : G10 ===> DENS=1.7 <===                               628
         CALL AGSRESET                                                    629
         AG_DENS = 1.7                                                    629
         AG_TITLE = 'G10'                                                 629
         AG_EXNAME = 'MIXTURE'                                            629
         AG_PARLIST = 'DENS'                                              629
         CALL AXMIXTURE                                                   629
C    *                                                                    629
C    Geant MATERIAL : G10 ===> inherited arguments used <===              629
         CALL AGSRESET                                                    630
         AG_TITLE = 'G10'                                                 630
         AG_EXNAME = 'MATERIAL'                                           630
         AG_PARLIST = 'NONE'                                              630
         CALL AXMATERIAL                                                  630
C    *                                                                    630
C    Geant SHAPE : BOX ===> DX=TECW_CARDW/2 DY=TECW_ASDEPTH/2, DZ=TECW_C  630
C    ARDTH/2 <===                                                         630
         CALL AGSRESET                                                    632
         AG_DX = TECW_CARDW/2                                             632
         AG_DY = TECW_ASDEPTH/2                                           632
         AG_DZ = TECW_CARDTH/2                                            632
         AG_TITLE = 'BOX'                                                 632
         AG_EXNAME = 'SHAPE'                                              632
         AG_PARLIST = 'DX_DY_DZ'                                          632
         CALL AXSHAPE                                                     632
C    *                                                                    632
5561  CALL AGSPOPB                                                        633
      GO TO BLOCK_TCEX, (5560 ,5570)                                      633
C End of Local Procedure                                                  633
C ----------------------------------------------------------------------  634
C ------------ TAEC                                                       634
C Begin Local Procedure TAEC                                              635
C TAEC IS PART OF THE HEAT SHIELD FOR THE PC BOARDS                       635
5580  CONTINUE                                                            635
         AG_TITLE='TAEC'                                                  635
         AG_MODULE=MODULE                                                 635
         AG_MARK='V00'                                                    635
         CALL AGSPUSH                                                     635
         AG_MODULE=MODULE                                                 635
C    *                                                                    635
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        636
         CALL AGSRESET                                                    636
         AG_TITLE = 'ALUMINIUM'                                           636
         AG_EXNAME = 'MATERIAL'                                           636
         AG_PARLIST = 'NONE'                                              636
         CALL AXMATERIAL                                                  636
C    *                                                                    636
C    Geant SHAPE : BOX ===> DX=TECW_COOLW/2 DY=TECW_ASDEPTH/2, DZ=TECW_C  636
C    OOLTH/2 <===                                                         636
         CALL AGSRESET                                                    638
         AG_DX = TECW_COOLW/2                                             638
         AG_DY = TECW_ASDEPTH/2                                           638
         AG_DZ = TECW_COOLTH/2                                            638
         AG_TITLE = 'BOX'                                                 638
         AG_EXNAME = 'SHAPE'                                              638
         AG_PARLIST = 'DX_DY_DZ'                                          638
         CALL AXSHAPE                                                     638
C    *                                                                    638
5581  CALL AGSPOPB                                                        639
      GO TO BLOCK_TAEC, (5580 ,5590)                                      639
C End of Local Procedure                                                  639
C ----------------------------------------------------------------------  640
C ------------ TPCW                                                       640
C Begin Local Procedure TPCW                                              641
C TPCW IS THE TPC SUPPORTING ENDCAP WHEEL                                 641
5160  CONTINUE                                                            641
         AG_TITLE='TPCW'                                                  641
         AG_MODULE=MODULE                                                 641
         AG_MARK='V00'                                                    641
         CALL AGSPUSH                                                     641
         AG_MODULE=MODULE                                                 641
C    *                                                                    641
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        642
         CALL AGSRESET                                                    642
         AG_TITLE = 'ALUMINIUM'                                           642
         AG_EXNAME = 'MATERIAL'                                           642
         AG_PARLIST = 'NONE'                                              642
         CALL AXMATERIAL                                                  642
C    *                                                                    642
C    Geant ATTRIBUTE : TPEA ===> SEEN=1 <===                              642
         CALL AGSRESET                                                    643
         AG_SEEN = 1                                                      643
         AG_TITLE = 'TPEA'                                                643
         AG_EXNAME = 'ATTRIBUTE'                                          643
         AG_PARLIST = 'SEEN'                                              643
         CALL AXATTRIBUTE                                                 643
C    *                                                                    643
C    Geant SHAPE : TUBE ===> RMIN=TPCG_WHEELIR RMAX=TPCG_WHEELOR, DZ=TPC  643
C    G_WHEELTHK/2 <===                                                    643
         CALL AGSRESET                                                    645
         AG_RMIN = TPCG_WHEELIR                                           645
         AG_RMAX = TPCG_WHEELOR                                           645
         AG_DZ = TPCG_WHEELTHK/2                                          645
         AG_TITLE = 'TUBE'                                                645
         AG_EXNAME = 'SHAPE'                                              645
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      645
         CALL AXSHAPE                                                     645
C    *                                                                    645
C    Create TWSS :                                                        646
C    Call To Local Procedure TWSS                                         646
         ASSIGN 5760 TO BLOCK_TWSS                                        646
         GO TO  5750                                                      646
5760     CONTINUE                                                         646
C    *                                                                    646
5161  CALL AGSPOPB                                                        647
      GO TO BLOCK_TPCW, (5160 ,5170)                                      647
C End of Local Procedure                                                  647
C ----------------------------------------------------------------------  648
C ------------ TWSS                                                       648
C Begin Local Procedure TWSS                                              649
C TWSS IS A DIVISION OF WHEEL CORRESPONDING TO SUPERSECTORS               649
5750  CONTINUE                                                            649
         AG_TITLE='TWSS'                                                  649
         AG_MODULE=MODULE                                                 649
         AG_MARK='V00'                                                    649
         CALL AGSPUSH                                                     649
         AG_MODULE=MODULE                                                 649
C    *                                                                    649
C    Geant ATTRIBUTE : TWSS ===> SEEN=0 <===                              650
         CALL AGSRESET                                                    650
         AG_SEEN = 0                                                      650
         AG_TITLE = 'TWSS'                                                650
         AG_EXNAME = 'ATTRIBUTE'                                          650
         AG_PARLIST = 'SEEN'                                              650
         CALL AXATTRIBUTE                                                 650
C    *                                                                    650
C    Geant SHAPE : DIVISION ===> NDIV=12 IAXIS=2 <===                     650
         CALL AGSRESET                                                    651
         AG_NDIV = 12                                                     651
         AG_IAXIS = 2                                                     651
         AG_TITLE = 'DIVISION'                                            651
         AG_EXNAME = 'SHAPE'                                              651
         AG_PARLIST = 'NDIV_IAXIS'                                        651
         CALL AXSHAPE                                                     651
C    *                                                                    651
C    *                                                                    651
         I_SEC=1                                                          652
            GO TO 5773                                                    652
5771        I_SEC=I_SEC+1                                                 652
5773        IF(I_SEC-(2).GT.0) GO TO 5772                                 652
C       use TECW bank                                                     653
            BPATH_TECW='TECW*'                                            653
            IDNUM_TECW(1)=0                                               653
            TECW_SEC=I_SEC                                                653
            CALL AGDGETP('TPCEGEO1','*',-999,'TECW',NTECW_SEC,            653
     *      RE_TECW_SEC,BPATH_TECW,IDNUM_TECW,1,MAP_TECW,NAMES_TECW,      653
     *      BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)             653
C       use TROV bank                                                     654
            BPATH_TROV='TROV*'                                            654
            IDNUM_TROV(1)=0                                               654
            TROV_SEC=I_SEC                                                654
            CALL AGDGETP('TPCEGEO1','*',-999,'TROV',NTROV_SEC,            654
     *      RE_TROV_SEC,BPATH_TROV,IDNUM_TROV,1,MAP_TROV,NAMES_TROV,      654
     *      BANK_TROV,LEN_TROV,LLEN_TROV,LINK_TROV,FLAG_TROV)             654
C       Create TWGI :                                                     655
C       Call To Local Procedure TWGI                                      655
            ASSIGN 5790 TO BLOCK_TWGI                                     655
            GO TO  5780                                                   655
5790        CONTINUE                                                      655
C       *                                                                 655
C       Geant POSITION : TWGI ===> X=TECW_GAPRAD ORT=YZX <===             656
            CALL AGSRESET                                                 656
            AG_X = TECW_GAPRAD                                            656
            AG_ALPHAN=AG_ALPHAN+1                                         656
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              656
            AG_TITLE = 'TWGI'                                             656
            AG_EXNAME = 'POSITION'                                        656
            AG_PARLIST = 'X_ORT'                                          656
            CALL AXPOSITION                                               656
C       *                                                                 656
         GO TO 5771                                                       657
5772     CONTINUE                                                         657
5751  CALL AGSPOPB                                                        658
      GO TO BLOCK_TWSS, (5750 ,5760)                                      658
C End of Local Procedure                                                  658
C ----------------------------------------------------------------------  659
C ------------ TWGI                                                       659
C Begin Local Procedure TWGI                                              660
C TWGI IS THE INNER AIR GAP IN WHEEL                                      660
5780  CONTINUE                                                            660
         AG_TITLE='TWGI'                                                  660
         AG_MODULE=MODULE                                                 660
         AG_MARK='V00'                                                    660
         CALL AGSPUSH                                                     660
         AG_MODULE=MODULE                                                 660
C    *                                                                    660
C    Geant MATERIAL : AIR ===> inherited arguments used <===              661
         CALL AGSRESET                                                    661
         AG_TITLE = 'AIR'                                                 661
         AG_EXNAME = 'MATERIAL'                                           661
         AG_PARLIST = 'NONE'                                              661
         CALL AXMATERIAL                                                  661
C    *                                                                    661
C    Geant ATTRIBUTE : TWGI ===> SEEN=1 <===                              661
         CALL AGSRESET                                                    662
         AG_SEEN = 1                                                      662
         AG_TITLE = 'TWGI'                                                662
         AG_EXNAME = 'ATTRIBUTE'                                          662
         AG_PARLIST = 'SEEN'                                              662
         CALL AXATTRIBUTE                                                 662
C    *                                                                    662
C    Geant SHAPE : TRD1 ===> DX1=TECW_GAPWIDI/2 DX2=TECW_GAPWIDO/2, DY=T  662
C    PCG_WHEELTHK/2 DZ=TECW_GAPHEIT/2 <===                                662
         CALL AGSRESET                                                    664
         AG_DX1 = TECW_GAPWIDI/2                                          664
         AG_DX2 = TECW_GAPWIDO/2                                          664
         AG_DY = TPCG_WHEELTHK/2                                          664
         AG_DZ = TECW_GAPHEIT/2                                           664
         AG_TITLE = 'TRD1'                                                664
         AG_EXNAME = 'SHAPE'                                              664
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     664
         CALL AXSHAPE                                                     664
C    *                                                                    664
         DX_DZ=(TECW_GAPWIDO/2 - TECW_GAPWIDI/2)/TECW_GAPHEIT             664
         XLIP=TECW_WHLIPWID*SQRT(1 + DX_DZ*DX_DZ)                         666
         XBOTTOM =TECW_GAPWIDI/2 + XLIP - DX_DZ*(TECW_WHLIPWID -          667
     *   TECW_GAPSHFT)                                                    667
C    Create TWGC :                                                        668
C    Call To Local Procedure TWGC                                         668
         ASSIGN 5810 TO BLOCK_TWGC                                        668
         GO TO  5800                                                      668
5810     CONTINUE                                                         668
C    *                                                                    668
C    Geant POSITION : TWGC ===> Y=TECW_WHLIPTHK/2 Z=TECW_GAPSHFT DX2=TEC  672
C    W_GAPWIDO/2+XLIP+DX_DZ*TECW_WHLIPWID DX1=XBOTTOM DZ=TECW_GAPHEIT/2+  672
C    TECW_WHLIPWID-TECW_GAPSHFT DY=(TPCG_WHEELTHK-TECW_WHLIPTHK)/2 <===   672
         CALL AGSRESET                                                    672
         AG_Y = TECW_WHLIPTHK/2                                           672
         AG_Z = TECW_GAPSHFT                                              672
         AG_DX2 = TECW_GAPWIDO/2+XLIP+DX_DZ*TECW_WHLIPWID                 672
         AG_DX1 = XBOTTOM                                                 672
         AG_DZ = TECW_GAPHEIT/2+TECW_WHLIPWID-TECW_GAPSHFT                672
         AG_DY = (TPCG_WHEELTHK-TECW_WHLIPTHK)/2                          672
         AG_TITLE = 'TWGC'                                                672
         AG_EXNAME = 'POSITION'                                           672
         AG_PARLIST = 'Y_Z_DX2_DX1_DZ_DY'                                 672
         CALL AXPOSITION                                                  672
C    *                                                                    672
C    Create TWGB :                                                        673
C    Call To Local Procedure TWGB                                         673
         ASSIGN 5830 TO BLOCK_TWGB                                        673
         GO TO  5820                                                      673
5830     CONTINUE                                                         673
C    *                                                                    673
C    Geant POSITION : TWGB ===> X=0 Y=TECW_WHLIPTHK/2 Z=(TECW_GAPHEIT/2+  675
C    TECW_WHLIPWID/2) ALPHAY=90 <===                                      675
         CALL AGSRESET                                                    675
         AG_X = 0                                                         675
         AG_Y = TECW_WHLIPTHK/2                                           675
         AG_Z = (TECW_GAPHEIT/2+TECW_WHLIPWID/2)                          675
         AG_ALPHAN=AG_ALPHAN+1                                            675
         AG_ALPHAK(AG_ALPHAN)=2                                           675
         AG_ALPHAL(AG_ALPHAN)=90                                          675
         AG_TITLE = 'TWGB'                                                675
         AG_EXNAME = 'POSITION'                                           675
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      675
         CALL AXPOSITION                                                  675
C    *                                                                    675
C    Check TECW_WHBLKIN==1                                                675
         IF (TECW_WHBLKIN.EQ.1) THEN                                      676
C    Geant POSITION : TWGB ===> X=0 Y=TECW_WHLIPTHK/2 Z=-(TECW_GAPHEIT/2  678
C    +TECW_WHLIPWID/2) ALPHAY=90 <===                                     678
         CALL AGSRESET                                                    678
         AG_X = 0                                                         678
         AG_Y = TECW_WHLIPTHK/2                                           678
         AG_Z = -(TECW_GAPHEIT/2+TECW_WHLIPWID/2)                         678
         AG_ALPHAN=AG_ALPHAN+1                                            678
         AG_ALPHAK(AG_ALPHAN)=2                                           678
         AG_ALPHAL(AG_ALPHAN)=90                                          678
         AG_TITLE = 'TWGB'                                                678
         AG_EXNAME = 'POSITION'                                           678
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      678
         CALL AXPOSITION                                                  678
C    *                                                                    678
         ENDIF                                                            678
C    Geant POSITION : TWGB ===> X=TECW_GAPWIDI/2+XLIP/2+DX_DZ*(TECW_WHBL  681
C    KPOS+TECW_GAPHEIT/2) Y=TECW_WHLIPTHK/2 Z=TECW_WHBLKPOS ALPHAY=15 <=  681
C    ==                                                                   681
         CALL AGSRESET                                                    681
         AG_X = TECW_GAPWIDI/2+XLIP/2+DX_DZ*(TECW_WHBLKPOS+TECW_GAPHEIT/  681
     *   2)                                                               681
         AG_Y = TECW_WHLIPTHK/2                                           681
         AG_Z = TECW_WHBLKPOS                                             681
         AG_ALPHAN=AG_ALPHAN+1                                            681
         AG_ALPHAK(AG_ALPHAN)=2                                           681
         AG_ALPHAL(AG_ALPHAN)=15                                          681
         AG_TITLE = 'TWGB'                                                681
         AG_EXNAME = 'POSITION'                                           681
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      681
         CALL AXPOSITION                                                  681
C    *                                                                    681
C    Geant POSITION : TWGB ===> X=TECW_GAPWIDI/2+XLIP/2+DX_DZ*(-TECW_WHB  681
C    LKPOS+TECW_GAPHEIT/2) Y=TECW_WHLIPTHK/2 Z=-TECW_WHBLKPOS ALPHAY=15   681
C    <===                                                                 681
         CALL AGSRESET                                                    683
         AG_X = TECW_GAPWIDI/2+XLIP/                                      683
     *   2+DX_DZ*(-TECW_WHBLKPOS+TECW_GAPHEIT/2)                          683
         AG_Y = TECW_WHLIPTHK/2                                           683
         AG_Z = -TECW_WHBLKPOS                                            683
         AG_ALPHAN=AG_ALPHAN+1                                            683
         AG_ALPHAK(AG_ALPHAN)=2                                           683
         AG_ALPHAL(AG_ALPHAN)=15                                          683
         AG_TITLE = 'TWGB'                                                683
         AG_EXNAME = 'POSITION'                                           683
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      683
         CALL AXPOSITION                                                  683
C    *                                                                    683
C    Geant POSITION : TWGB ===> X=-TECW_GAPWIDI/2-XLIP/2-DX_DZ*(TECW_WHB  683
C    LKPOS+TECW_GAPHEIT/2) Y=TECW_WHLIPTHK/2 Z=TECW_WHBLKPOS ALPHAY=-15   683
C    <===                                                                 683
         CALL AGSRESET                                                    685
         AG_X = -TECW_GAPWIDI/2-XLIP/                                     685
     *   2-DX_DZ*(TECW_WHBLKPOS+TECW_GAPHEIT/2)                           685
         AG_Y = TECW_WHLIPTHK/2                                           685
         AG_Z = TECW_WHBLKPOS                                             685
         AG_ALPHAN=AG_ALPHAN+1                                            685
         AG_ALPHAK(AG_ALPHAN)=2                                           685
         AG_ALPHAL(AG_ALPHAN)=-15                                         685
         AG_TITLE = 'TWGB'                                                685
         AG_EXNAME = 'POSITION'                                           685
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      685
         CALL AXPOSITION                                                  685
C    *                                                                    685
C    Geant POSITION : TWGB ===> X=-TECW_GAPWIDI/2-XLIP/2-DX_DZ*(-TECW_WH  685
C    BLKPOS+TECW_GAPHEIT/2) Y=TECW_WHLIPTHK/2 Z=-TECW_WHBLKPOS ALPHAY=-1  685
C    5 <===                                                               685
         CALL AGSRESET                                                    687
         AG_X = -TECW_GAPWIDI/2-XLIP/                                     687
     *   2-DX_DZ*(-TECW_WHBLKPOS+TECW_GAPHEIT/2)                          687
         AG_Y = TECW_WHLIPTHK/2                                           687
         AG_Z = -TECW_WHBLKPOS                                            687
         AG_ALPHAN=AG_ALPHAN+1                                            687
         AG_ALPHAK(AG_ALPHAN)=2                                           687
         AG_ALPHAL(AG_ALPHAN)=-15                                         687
         AG_TITLE = 'TWGB'                                                687
         AG_EXNAME = 'POSITION'                                           687
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      687
         CALL AXPOSITION                                                  687
C    *                                                                    687
         RADOFF = TECW_RCENTER - TECW_GAPRAD - TECW_HEIGHT/2 +            687
     *   TECW_GAPHEIT/2                                                   687
C    *                                                                    689
         I_NHP=1                                                          689
            GO TO 5843                                                    689
5841        I_NHP=I_NHP+1                                                 689
5843        IF(I_NHP-(16).GT.0) GO TO 5842                                689
C       Check TROV_NHP(I_NHP)>0                                           690
            IF (TROV_NHP(I_NHP).GT.0) THEN                                690
C       FIRST PL                                                          691
            HPLA = -TECW_GAPHEIT/2+RADOFF+TECW_Z(1)-TECW_DZ(1)/           691
     *      2+TROV_OFFCARDINOUT                                           691
            ZVALUE = HPLA+4.0*(I_NHP-1)                                   692
            PIPEOFF=TECW_PIPEHT/2+TECW_COOLTH+TECW_CARDTH/2               693
            XTEMP=TECW_GAPWIDI/2                                          694
            ZTEMP=ZVALUE+TECW_CARDOFF-PIPEOFF                             695
C       Create TPIP :                                                     696
C       Call To Local Procedure TPIP                                      696
            ASSIGN 5860 TO BLOCK_TPIP                                     696
            GO TO  5850                                                   696
5860        CONTINUE                                                      696
C       *                                                                 696
C       Geant POSITION : TPIP ===> Y=-TPCG_WHEELTHK/2+TECW_PIPETHK/2 Z=Z  700
C       TEMP DX2=XTEMP+DX_DZ*(ZTEMP+TECW_GAPHEIT/2+TECW_PIPEHT/2) DX1=XT  700
C       EMP+DX_DZ*(ZTEMP+TECW_GAPHEIT/2-TECW_PIPEHT/2) DZ=TECW_PIPEHT/2   700
C       DY=TECW_PIPETHK/2 <===                                            700
            CALL AGSRESET                                                 700
            AG_Y = -TPCG_WHEELTHK/2+TECW_PIPETHK/2                        700
            AG_Z = ZTEMP                                                  700
            AG_DX2 = XTEMP+DX_DZ*(ZTEMP+TECW_GAPHEIT/2+TECW_PIPEHT/2)     700
            AG_DX1 = XTEMP+DX_DZ*(ZTEMP+TECW_GAPHEIT/2-TECW_PIPEHT/2)     700
            AG_DZ = TECW_PIPEHT/2                                         700
            AG_DY = TECW_PIPETHK/2                                        700
            AG_TITLE = 'TPIP'                                             700
            AG_EXNAME = 'POSITION'                                        700
            AG_PARLIST = 'Y_Z_DX2_DX1_DZ_DY'                              700
            CALL AXPOSITION                                               700
C       *                                                                 700
            ENDIF                                                         700
         GO TO 5841                                                       702
5842     CONTINUE                                                         702
C    Create TMAN :                                                        703
C    Call To Local Procedure TMAN                                         703
         ASSIGN 5880 TO BLOCK_TMAN                                        703
         GO TO  5870                                                      703
5880     CONTINUE                                                         703
C    *                                                                    703
C    Geant POSITION : TMAN ===> X=TECW_GAPWIDI/2.+DX_DZ*(TECW_GAPHEIT/2)  708
C    -(1+DX_DZ*DX_DZ)*TECW_MANIWID/2 Y=-TPCG_WHEELTHK/2+TECW_PIPETHK+TEC  708
C    W_MANITHK/2 Z=0.0 DZ=(1.+DX_DZ*DX_DZ)*(TECW_GAPHEIT/2-TECW_MANIWID)  708
C     DX=TECW_MANIWID/2 DY=TECW_MANITHK/2 ALPHAY=15 <===                  708
         CALL AGSRESET                                                    708
         AG_X = TECW_GAPWIDI/2.+DX_DZ*(TECW_GAPHEIT/                      708
     *   2)-(1+DX_DZ*DX_DZ)*TECW_MANIWID/2                                708
         AG_Y = -TPCG_WHEELTHK/2+TECW_PIPETHK+TECW_MANITHK/2              708
         AG_Z = 0.0                                                       708
         AG_DZ = (1.+DX_DZ*DX_DZ)*(TECW_GAPHEIT/2-TECW_MANIWID)           708
         AG_DX = TECW_MANIWID/2                                           708
         AG_DY = TECW_MANITHK/2                                           708
         AG_ALPHAN=AG_ALPHAN+1                                            708
         AG_ALPHAK(AG_ALPHAN)=2                                           708
         AG_ALPHAL(AG_ALPHAN)=15                                          708
         AG_TITLE = 'TMAN'                                                708
         AG_EXNAME = 'POSITION'                                           708
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY_ALPHAY'                             708
         CALL AXPOSITION                                                  708
C    *                                                                    708
C    Geant POSITION : TMAN ===> X=-TECW_GAPWIDI/2.-DX_DZ*(TECW_GAPHEIT/2  708
C    )+(1+DX_DZ*DX_DZ)*TECW_MANIWID/2 Y=-TPCG_WHEELTHK/2+TECW_PIPETHK+TE  708
C    CW_MANITHK/2 Z=0.0 DZ=(1.+DX_DZ*DX_DZ)*(TECW_GAPHEIT/2-TECW_MANIWID  708
C    ) DX=TECW_MANIWID/2 DY=TECW_MANITHK/2 ALPHAY=-15 <===                708
         CALL AGSRESET                                                    713
         AG_X = -TECW_GAPWIDI/2.-DX_DZ*(TECW_GAPHEIT/                     713
     *   2)+(1+DX_DZ*DX_DZ)*TECW_MANIWID/2                                713
         AG_Y = -TPCG_WHEELTHK/2+TECW_PIPETHK+TECW_MANITHK/2              713
         AG_Z = 0.0                                                       713
         AG_DZ = (1.+DX_DZ*DX_DZ)*(TECW_GAPHEIT/2-TECW_MANIWID)           713
         AG_DX = TECW_MANIWID/2                                           713
         AG_DY = TECW_MANITHK/2                                           713
         AG_ALPHAN=AG_ALPHAN+1                                            713
         AG_ALPHAK(AG_ALPHAN)=2                                           713
         AG_ALPHAL(AG_ALPHAN)=-15                                         713
         AG_TITLE = 'TMAN'                                                713
         AG_EXNAME = 'POSITION'                                           713
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY_ALPHAY'                             713
         CALL AXPOSITION                                                  713
C    *                                                                    713
C    Create TCRX :                                                        714
C    Call To Local Procedure TCRX                                         714
         ASSIGN 5900 TO BLOCK_TCRX                                        714
         GO TO  5890                                                      714
5900     CONTINUE                                                         714
C    *                                                                    714
C    Create TALC :                                                        715
C    Call To Local Procedure TALC                                         715
         ASSIGN 5920 TO BLOCK_TALC                                        715
         GO TO  5910                                                      715
5920     CONTINUE                                                         715
C    *                                                                    715
C    *                                                                    716
         I_NHP=1                                                          716
            GO TO 5933                                                    716
5931        I_NHP=I_NHP+1                                                 716
5933        IF(I_NHP-(16).GT.0) GO TO 5932                                716
C       Check TROV_NHP(I_NHP)>0                                           717
            IF (TROV_NHP(I_NHP).GT.0) THEN                                717
C       FIRST PL                                                          718
            HPLA = -TECW_GAPHEIT/2+RADOFF+TECW_Z(1)-TECW_DZ(1)/           718
     *      2+TROV_OFFCARDINOUT                                           718
            ZVALUE = HPLA+4.0*(I_NHP-1)                                   719
C       *                                                                 720
            J_NHO=1                                                       720
               GO TO 5943                                                 720
5941           J_NHO=J_NHO+1                                              720
5943           IF(J_NHO-(TROV_NHP(I_NHP)).GT.0) GO TO 5942                720
C          Geant POSITION : TCRX ===> X=TROV_HX(J_NHO,I_NHP) Z=ZVALUE+TE  721
C          CW_CARDOFF <===                                                721
               CALL AGSRESET                                              721
               AG_X = TROV_HX(J_NHO,I_NHP)                                721
               AG_Z = ZVALUE+TECW_CARDOFF                                 721
               AG_TITLE = 'TCRX'                                          721
               AG_EXNAME = 'POSITION'                                     721
               AG_PARLIST = 'X_Z'                                         721
               CALL AXPOSITION                                            721
C          *                                                              721
C          Geant POSITION : TALC ===> X=TROV_HX(J_NHO,I_NHP) Z=ZVALUE+TE  721
C          CW_COOLOFF <===                                                721
               CALL AGSRESET                                              722
               AG_X = TROV_HX(J_NHO,I_NHP)                                722
               AG_Z = ZVALUE+TECW_COOLOFF                                 722
               AG_TITLE = 'TALC'                                          722
               AG_EXNAME = 'POSITION'                                     722
               AG_PARLIST = 'X_Z'                                         722
               CALL AXPOSITION                                            722
C          *                                                              722
            GO TO 5941                                                    723
5942        CONTINUE                                                      723
            ENDIF                                                         724
         GO TO 5931                                                       725
5932     CONTINUE                                                         725
5781  CALL AGSPOPB                                                        726
      GO TO BLOCK_TWGI, (5780 ,5790)                                      726
C End of Local Procedure                                                  726
C ----------------------------------------------------------------------  727
C ------------ TWGC                                                       727
C Begin Local Procedure TWGC                                              728
C TWGC IS THE LARGER INNER AIR GAP IN WHEEL                               728
5800  CONTINUE                                                            728
         AG_TITLE='TWGC'                                                  728
         AG_MODULE=MODULE                                                 728
         AG_MARK='V00'                                                    728
         CALL AGSPUSH                                                     728
         AG_MODULE=MODULE                                                 728
C    *                                                                    728
C    Geant MATERIAL : AIR ===> inherited arguments used <===              729
         CALL AGSRESET                                                    729
         AG_TITLE = 'AIR'                                                 729
         AG_EXNAME = 'MATERIAL'                                           729
         AG_PARLIST = 'NONE'                                              729
         CALL AXMATERIAL                                                  729
C    *                                                                    729
C    Geant ATTRIBUTE : TWGC ===> SEEN=1 COLO=7 <===                       729
         CALL AGSRESET                                                    730
         AG_SEEN = 1                                                      730
         AG_COLO = 7                                                      730
         AG_TITLE = 'TWGC'                                                730
         AG_EXNAME = 'ATTRIBUTE'                                          730
         AG_PARLIST = 'SEEN_COLO'                                         730
         CALL AXATTRIBUTE                                                 730
C    *                                                                    730
C    Geant SHAPE : TRD1 ===> DX1=0 DX2=0 DY=0 DZ=0 <===                   730
         CALL AGSRESET                                                    731
         AG_DX1 = 0                                                       731
         AG_DX2 = 0                                                       731
         AG_DY = 0                                                        731
         AG_DZ = 0                                                        731
         AG_TITLE = 'TRD1'                                                731
         AG_EXNAME = 'SHAPE'                                              731
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     731
         CALL AXSHAPE                                                     731
C    *                                                                    731
5801  CALL AGSPOPB                                                        732
      GO TO BLOCK_TWGC, (5800 ,5810)                                      732
C End of Local Procedure                                                  732
C ----------------------------------------------------------------------  733
C ------------ TWGB                                                       733
C Begin Local Procedure TWGB                                              734
C TWGB IS ADDED ALUM BLOCKSIN WHEEL                                       734
5820  CONTINUE                                                            734
         AG_TITLE='TWGB'                                                  734
         AG_MODULE=MODULE                                                 734
         AG_MARK='V00'                                                    734
         CALL AGSPUSH                                                     734
         AG_MODULE=MODULE                                                 734
C    *                                                                    734
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        735
         CALL AGSRESET                                                    735
         AG_TITLE = 'ALUMINIUM'                                           735
         AG_EXNAME = 'MATERIAL'                                           735
         AG_PARLIST = 'NONE'                                              735
         CALL AXMATERIAL                                                  735
C    *                                                                    735
C    Geant ATTRIBUTE : TWGB ===> SEEN=1 <===                              735
         CALL AGSRESET                                                    736
         AG_SEEN = 1                                                      736
         AG_TITLE = 'TWGB'                                                736
         AG_EXNAME = 'ATTRIBUTE'                                          736
         AG_PARLIST = 'SEEN'                                              736
         CALL AXATTRIBUTE                                                 736
C    *                                                                    736
C    Geant SHAPE : BOX ===> DX=TECW_WHLIPWID/2 DY=(TPCG_WHEELTHK-TECW_WH  736
C    LIPTHK)/2, DZ=TECW_WHBLKLEN/2 <===                                   736
         CALL AGSRESET                                                    738
         AG_DX = TECW_WHLIPWID/2                                          738
         AG_DY = (TPCG_WHEELTHK-TECW_WHLIPTHK)/2                          738
         AG_DZ = TECW_WHBLKLEN/2                                          738
         AG_TITLE = 'BOX'                                                 738
         AG_EXNAME = 'SHAPE'                                              738
         AG_PARLIST = 'DX_DY_DZ'                                          738
         CALL AXSHAPE                                                     738
C    *                                                                    738
5821  CALL AGSPOPB                                                        739
      GO TO BLOCK_TWGB, (5820 ,5830)                                      739
C End of Local Procedure                                                  739
C ----------------------------------------------------------------------  740
C ------------ TMAN                                                       740
C Begin Local Procedure TMAN                                              741
C TMAN IS ALUMINUM WATER MANIFOLD                                         741
5870  CONTINUE                                                            741
         AG_TITLE='TMAN'                                                  741
         AG_MODULE=MODULE                                                 741
         AG_MARK='V00'                                                    741
         CALL AGSPUSH                                                     741
         AG_MODULE=MODULE                                                 741
C    *                                                                    741
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        742
         CALL AGSRESET                                                    742
         AG_TITLE = 'ALUMINIUM'                                           742
         AG_EXNAME = 'MATERIAL'                                           742
         AG_PARLIST = 'NONE'                                              742
         CALL AXMATERIAL                                                  742
C    *                                                                    742
C    Geant ATTRIBUTE : TWGB ===> SEEN=1 <===                              742
         CALL AGSRESET                                                    743
         AG_SEEN = 1                                                      743
         AG_TITLE = 'TWGB'                                                743
         AG_EXNAME = 'ATTRIBUTE'                                          743
         AG_PARLIST = 'SEEN'                                              743
         CALL AXATTRIBUTE                                                 743
C    *                                                                    743
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           743
         CALL AGSRESET                                                    744
         AG_DX = 0                                                        744
         AG_DY = 0                                                        744
         AG_DZ = 0                                                        744
         AG_TITLE = 'BOX'                                                 744
         AG_EXNAME = 'SHAPE'                                              744
         AG_PARLIST = 'DX_DY_DZ'                                          744
         CALL AXSHAPE                                                     744
C    *                                                                    744
5871  CALL AGSPOPB                                                        745
      GO TO BLOCK_TMAN, (5870 ,5880)                                      745
C End of Local Procedure                                                  745
C ----------------------------------------------------------------------  746
C ------------ TPIP                                                       746
C Begin Local Procedure TPIP                                              747
C TPIP IS A WATER PIPE THERE ARE LOTS PER SECT                            747
5850  CONTINUE                                                            747
         AG_TITLE='TPIP'                                                  747
         AG_MODULE=MODULE                                                 747
         AG_MARK='V00'                                                    747
         CALL AGSPUSH                                                     747
         AG_MODULE=MODULE                                                 747
C    *                                                                    747
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        748
         CALL AGSRESET                                                    748
         AG_TITLE = 'ALUMINIUM'                                           748
         AG_EXNAME = 'MATERIAL'                                           748
         AG_PARLIST = 'NONE'                                              748
         CALL AXMATERIAL                                                  748
C    *                                                                    748
C    Geant ATTRIBUTE : TPIP ===> SEEN=1 COLO=4 <===                       748
         CALL AGSRESET                                                    749
         AG_SEEN = 1                                                      749
         AG_COLO = 4                                                      749
         AG_TITLE = 'TPIP'                                                749
         AG_EXNAME = 'ATTRIBUTE'                                          749
         AG_PARLIST = 'SEEN_COLO'                                         749
         CALL AXATTRIBUTE                                                 749
C    *                                                                    749
C    Geant SHAPE : TRD1 ===> DX1=0 DX2=0 DY=0 DZ=0 <===                   749
         CALL AGSRESET                                                    750
         AG_DX1 = 0                                                       750
         AG_DX2 = 0                                                       750
         AG_DY = 0                                                        750
         AG_DZ = 0                                                        750
         AG_TITLE = 'TRD1'                                                750
         AG_EXNAME = 'SHAPE'                                              750
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     750
         CALL AXSHAPE                                                     750
C    *                                                                    750
5851  CALL AGSPOPB                                                        751
      GO TO BLOCK_TPIP, (5850 ,5860)                                      751
C End of Local Procedure                                                  751
C ----------------------------------------------------------------------  752
C ------------ TCRX                                                       752
C Begin Local Procedure TCRX                                              753
C TCRX IS PART OF THE G10 FOR THE PC BOARDS                               753
5890  CONTINUE                                                            753
         AG_TITLE='TCRX'                                                  753
         AG_MODULE=MODULE                                                 753
         AG_MARK='V00'                                                    753
         CALL AGSPUSH                                                     753
         AG_MODULE=MODULE                                                 753
C    *                                                                    753
C    Geant MATERIAL : G10 ===> inherited arguments used <===              754
         CALL AGSRESET                                                    754
         AG_TITLE = 'G10'                                                 754
         AG_EXNAME = 'MATERIAL'                                           754
         AG_PARLIST = 'NONE'                                              754
         CALL AXMATERIAL                                                  754
C    *                                                                    754
C    Geant ATTRIBUTE : TCRX ===> SEEN=1 COLO=4 <===                       754
         CALL AGSRESET                                                    755
         AG_SEEN = 1                                                      755
         AG_COLO = 4                                                      755
         AG_TITLE = 'TCRX'                                                755
         AG_EXNAME = 'ATTRIBUTE'                                          755
         AG_PARLIST = 'SEEN_COLO'                                         755
         CALL AXATTRIBUTE                                                 755
C    *                                                                    755
C    Geant SHAPE : BOX ===> DX=TECW_CARDW/2 DY=TPCG_WHEELTHK/2, DZ=TECW_  755
C    CARDTH/2 <===                                                        755
         CALL AGSRESET                                                    757
         AG_DX = TECW_CARDW/2                                             757
         AG_DY = TPCG_WHEELTHK/2                                          757
         AG_DZ = TECW_CARDTH/2                                            757
         AG_TITLE = 'BOX'                                                 757
         AG_EXNAME = 'SHAPE'                                              757
         AG_PARLIST = 'DX_DY_DZ'                                          757
         CALL AXSHAPE                                                     757
C    *                                                                    757
5891  CALL AGSPOPB                                                        758
      GO TO BLOCK_TCRX, (5890 ,5900)                                      758
C End of Local Procedure                                                  758
C ----------------------------------------------------------------------  759
C ------------ TALC                                                       759
C Begin Local Procedure TALC                                              760
C TALC IS PART OF THE HEAT SHIELD FOR THE PC BOARDS                       760
5910  CONTINUE                                                            760
         AG_TITLE='TALC'                                                  760
         AG_MODULE=MODULE                                                 760
         AG_MARK='V00'                                                    760
         CALL AGSPUSH                                                     760
         AG_MODULE=MODULE                                                 760
C    *                                                                    760
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        761
         CALL AGSRESET                                                    761
         AG_TITLE = 'ALUMINIUM'                                           761
         AG_EXNAME = 'MATERIAL'                                           761
         AG_PARLIST = 'NONE'                                              761
         CALL AXMATERIAL                                                  761
C    *                                                                    761
C    Geant ATTRIBUTE : TALC ===> SEEN=1 COLO=6 <===                       761
         CALL AGSRESET                                                    762
         AG_SEEN = 1                                                      762
         AG_COLO = 6                                                      762
         AG_TITLE = 'TALC'                                                762
         AG_EXNAME = 'ATTRIBUTE'                                          762
         AG_PARLIST = 'SEEN_COLO'                                         762
         CALL AXATTRIBUTE                                                 762
C    *                                                                    762
C    Geant SHAPE : BOX ===> DX=TECW_COOLW/2 DY=TPCG_WHEELTHK/2, DZ=TECW_  762
C    COOLTH/2 <===                                                        762
         CALL AGSRESET                                                    764
         AG_DX = TECW_COOLW/2                                             764
         AG_DY = TPCG_WHEELTHK/2                                          764
         AG_DZ = TECW_COOLTH/2                                            764
         AG_TITLE = 'BOX'                                                 764
         AG_EXNAME = 'SHAPE'                                              764
         AG_PARLIST = 'DX_DY_DZ'                                          764
         CALL AXSHAPE                                                     764
C    *                                                                    764
5911  CALL AGSPOPB                                                        765
      GO TO BLOCK_TALC, (5910 ,5920)                                      765
C End of Local Procedure                                                  765
C ----------------------------------------------------------------------  766
C ------------ TRDV                                                       766
C Begin Local Procedure TRDV                                              767
C TRDV IS THE RDO BOARD VOLUME                                            767
5180  CONTINUE                                                            767
         AG_TITLE='TRDV'                                                  767
         AG_MODULE=MODULE                                                 767
         AG_MARK='V00'                                                    767
         CALL AGSPUSH                                                     767
         AG_MODULE=MODULE                                                 767
C    *                                                                    767
C    Geant MATERIAL : AIR ===> inherited arguments used <===              768
         CALL AGSRESET                                                    768
         AG_TITLE = 'AIR'                                                 768
         AG_EXNAME = 'MATERIAL'                                           768
         AG_PARLIST = 'NONE'                                              768
         CALL AXMATERIAL                                                  768
C    *                                                                    768
C    Geant ATTRIBUTE : TRDV ===> SEEN=1 COLO=2 <===                       768
         CALL AGSRESET                                                    769
         AG_SEEN = 1                                                      769
         AG_COLO = 2                                                      769
         AG_TITLE = 'TRDV'                                                769
         AG_EXNAME = 'ATTRIBUTE'                                          769
         AG_PARLIST = 'SEEN_COLO'                                         769
         CALL AXATTRIBUTE                                                 769
C    *                                                                    769
C    Geant SHAPE : TUBE ===> RMIN=TPCG_WHEELIR RMAX=TPCG_WHEELOR, DZ=TRD  769
C    O_RDOVTHK/2 <===                                                     769
         CALL AGSRESET                                                    771
         AG_RMIN = TPCG_WHEELIR                                           771
         AG_RMAX = TPCG_WHEELOR                                           771
         AG_DZ = TRDO_RDOVTHK/2                                           771
         AG_TITLE = 'TUBE'                                                771
         AG_EXNAME = 'SHAPE'                                              771
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      771
         CALL AXSHAPE                                                     771
C    *                                                                    771
C    Create TRDS :                                                        772
C    Call To Local Procedure TRDS                                         772
         ASSIGN 5960 TO BLOCK_TRDS                                        772
         GO TO  5950                                                      772
5960     CONTINUE                                                         772
C    *                                                                    772
5181  CALL AGSPOPB                                                        773
      GO TO BLOCK_TRDV, (5180 ,5190)                                      773
C End of Local Procedure                                                  773
C ----------------------------------------------------------------------  774
C ------------ TRDS                                                       774
C Begin Local Procedure TRDS                                              775
C TRDS IS A DIVISION OF RDO BOARD VOLUME CORRESPONDING TO ONE SUPERSECTO  775
C R                                                                       775
5950  CONTINUE                                                            775
         AG_TITLE='TRDS'                                                  775
         AG_MODULE=MODULE                                                 775
         AG_MARK='V00'                                                    775
         CALL AGSPUSH                                                     775
         AG_MODULE=MODULE                                                 775
C    *                                                                    775
C    Geant ATTRIBUTE : TRDS ===> SEEN=1 <===                              776
         CALL AGSRESET                                                    776
         AG_SEEN = 1                                                      776
         AG_TITLE = 'TRDS'                                                776
         AG_EXNAME = 'ATTRIBUTE'                                          776
         AG_PARLIST = 'SEEN'                                              776
         CALL AXATTRIBUTE                                                 776
C    *                                                                    776
C    Geant SHAPE : DIVISION ===> NDIV=12 IAXIS=2 <===                     776
         CALL AGSRESET                                                    777
         AG_NDIV = 12                                                     777
         AG_IAXIS = 2                                                     777
         AG_TITLE = 'DIVISION'                                            777
         AG_EXNAME = 'SHAPE'                                              777
         AG_PARLIST = 'NDIV_IAXIS'                                        777
         CALL AXSHAPE                                                     777
C    *                                                                    777
C    use default TRDO bank                                                777
         BPATH_TRDO='TRDO*'                                               778
         IDNUM_TRDO(1)=0                                                  778
         CALL AGDGETP('TPCEGEO1','*',-999,'TRDO',' ',BANK_TRDO,           778
     *   BPATH_TRDO,IDNUM_TRDO,1,MAP_TRDO,NAMES_TRDO,BANK_TRDO,LEN_TRDO,  778
     *   LLEN_TRDO,LINK_TRDO,FLAG_TRDO)                                   778
C    *                                                                    779
         KK=1                                                             779
            GO TO 5973                                                    779
5971        KK=KK+1                                                       779
5973        IF(KK-(TRDO_NRDOBRD).GT.0) GO TO 5972                         779
C       Create TRDC :                                                     780
C       Call To Local Procedure TRDC                                      780
            ASSIGN 5990 TO BLOCK_TRDC                                     780
            GO TO  5980                                                   780
5990        CONTINUE                                                      780
C       *                                                                 780
C       Geant POSITION : TRDC ===> X=TRDO_RDOHT(KK) DZ=TRDO_RDOTHK/2 DX=  782
C       (TRDO_RDOHT(KK)*TECW_TAN15)-TECW_CLEARANCE DY=TRDO_RDOLEN/2 ORT=  782
C       YZX <===                                                          782
            CALL AGSRESET                                                 782
            AG_X = TRDO_RDOHT(KK)                                         782
            AG_DZ = TRDO_RDOTHK/2                                         782
            AG_DX = (TRDO_RDOHT(KK)*TECW_TAN15)-TECW_CLEARANCE            782
            AG_DY = TRDO_RDOLEN/2                                         782
            AG_ALPHAN=AG_ALPHAN+1                                         782
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              782
            AG_TITLE = 'TRDC'                                             782
            AG_EXNAME = 'POSITION'                                        782
            AG_PARLIST = 'X_DZ_DX_DY_ORT'                                 782
            CALL AXPOSITION                                               782
C       *                                                                 782
         GO TO 5971                                                       783
5972     CONTINUE                                                         783
5951  CALL AGSPOPB                                                        784
      GO TO BLOCK_TRDS, (5950 ,5960)                                      784
C End of Local Procedure                                                  784
C ----------------------------------------------------------------------  785
C ------------ TRDC                                                       785
C Begin Local Procedure TRDC                                              786
C TRDC IS AN RDO CARD                                                     786
5980  CONTINUE                                                            786
         AG_TITLE='TRDC'                                                  786
         AG_MODULE=MODULE                                                 786
         AG_MARK='V00'                                                    786
         CALL AGSPUSH                                                     786
         AG_MODULE=MODULE                                                 786
C    *                                                                    786
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===          787
         CALL AGSRESET                                                    787
         AG_A = 28.08                                                     787
         AG_Z = 14                                                        787
         AG_W = 0.6*1*28./60.                                             787
         AG_TITLE = 'SI'                                                  787
         AG_EXNAME = 'COMPONENT'                                          787
         AG_PARLIST = 'A_Z_W'                                             787
         CALL AXCOMPONENT                                                 787
C    *                                                                    787
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. <===               787
         CALL AGSRESET                                                    788
         AG_A = 16                                                        788
         AG_Z = 8                                                         788
         AG_W = 0.6*2*16./60.                                             788
         AG_TITLE = 'O'                                                   788
         AG_EXNAME = 'COMPONENT'                                          788
         AG_PARLIST = 'A_Z_W'                                             788
         CALL AXCOMPONENT                                                 788
C    *                                                                    788
C    Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===              788
         CALL AGSRESET                                                    789
         AG_A = 12                                                        789
         AG_Z = 6                                                         789
         AG_W = 0.4*8*12./174.                                            789
         AG_TITLE = 'C'                                                   789
         AG_EXNAME = 'COMPONENT'                                          789
         AG_PARLIST = 'A_Z_W'                                             789
         CALL AXCOMPONENT                                                 789
C    *                                                                    789
C    Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===               789
         CALL AGSRESET                                                    790
         AG_A = 1                                                         790
         AG_Z = 1                                                         790
         AG_W = 0.4*14*1./174.                                            790
         AG_TITLE = 'H'                                                   790
         AG_EXNAME = 'COMPONENT'                                          790
         AG_PARLIST = 'A_Z_W'                                             790
         CALL AXCOMPONENT                                                 790
C    *                                                                    790
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.4*4*16./174. <===              790
         CALL AGSRESET                                                    791
         AG_A = 16                                                        791
         AG_Z = 8                                                         791
         AG_W = 0.4*4*16./174.                                            791
         AG_TITLE = 'O'                                                   791
         AG_EXNAME = 'COMPONENT'                                          791
         AG_PARLIST = 'A_Z_W'                                             791
         CALL AXCOMPONENT                                                 791
C    *                                                                    791
C    Geant MIXTURE : G10 ===> DENS=1.7 <===                               791
         CALL AGSRESET                                                    792
         AG_DENS = 1.7                                                    792
         AG_TITLE = 'G10'                                                 792
         AG_EXNAME = 'MIXTURE'                                            792
         AG_PARLIST = 'DENS'                                              792
         CALL AXMIXTURE                                                   792
C    *                                                                    792
C    Geant MATERIAL : G10 ===> inherited arguments used <===              792
         CALL AGSRESET                                                    793
         AG_TITLE = 'G10'                                                 793
         AG_EXNAME = 'MATERIAL'                                           793
         AG_PARLIST = 'NONE'                                              793
         CALL AXMATERIAL                                                  793
C    *                                                                    793
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           793
         CALL AGSRESET                                                    794
         AG_DX = 0                                                        794
         AG_DY = 0                                                        794
         AG_DZ = 0                                                        794
         AG_TITLE = 'BOX'                                                 794
         AG_EXNAME = 'SHAPE'                                              794
         AG_PARLIST = 'DX_DY_DZ'                                          794
         CALL AXSHAPE                                                     794
C    *                                                                    794
5981  CALL AGSPOPB                                                        795
      GO TO BLOCK_TRDC, (5980 ,5990)                                      795
C End of Local Procedure                                                  795
      WRITE(LOUT,6000)'===>>  [End AgML Module TPCEGEO1]  <<==='          795
6000  FORMAT(A32,/,/)                                                     795
C MODULE TPCEGEO1                                                         797
1     CONTINUE                                                            797
      END                                                                 797
