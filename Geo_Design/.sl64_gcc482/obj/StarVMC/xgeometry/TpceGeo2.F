C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE TPCEGEO2                                                 1
C TPCEGEO2 IS THE UPDATED TPC                                             1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'TPCEGEO2'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'IS THE UPDATED TPC'/                                 1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C AUTHOR DAVID UNDERWOOD CORRECTED BY MAXIM POTEKHIN                      1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'DAVID UNDERWOOD CORRECTED BY MAXIM            2
     *POTEKHIN'/                                                          2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C CREATED DEC 7, 2005                                                     2
      CHARACTER*40 CREATED /'DEC 7, 2005'/                                3
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
      INTEGER BLOCK_TOFC                                                  11
      INTEGER BLOCK_TOFS                                                  11
      INTEGER BLOCK_TOST                                                  11
      INTEGER BLOCK_TOKA                                                  11
      INTEGER BLOCK_TONX                                                  11
      INTEGER BLOCK_TOAD                                                  11
      INTEGER BLOCK_TOHA                                                  11
      INTEGER BLOCK_TPGV                                                  11
      INTEGER BLOCK_TPSS                                                  11
      INTEGER BLOCK_TIFC                                                  11
      INTEGER BLOCK_TIAL                                                  11
      INTEGER BLOCK_TIKA                                                  11
      INTEGER BLOCK_TINX                                                  11
      INTEGER BLOCK_TPCW                                                  11
      INTEGER BLOCK_TWSS                                                  11
      INTEGER BLOCK_TWGI                                                  11
      INTEGER BLOCK_TPCM                                                  11
      INTEGER BLOCK_TPEA                                                  11
      INTEGER BLOCK_TESS                                                  11
      INTEGER BLOCK_TSEC                                                  11
      INTEGER BLOCK_TMWC                                                  11
      INTEGER BLOCK_TMEA                                                  11
      INTEGER BLOCK_TMSE                                                  11
      INTEGER BLOCK_TIAG                                                  11
      INTEGER BLOCK_TOAE                                                  11
      INTEGER BLOCK_TPAD                                                  11
      INTEGER BLOCK_THOL                                                  11
      INTEGER BLOCK_THRA                                                  11
      INTEGER BLOCK_THLA                                                  11
      INTEGER BLOCK_TALC                                                  11
      INTEGER BLOCK_TAEC                                                  11
      INTEGER BLOCK_TCEX                                                  11
      INTEGER BLOCK_TCRX                                                  11
      INTEGER BLOCK_TWGC                                                  11
      INTEGER BLOCK_TWGB                                                  11
      INTEGER BLOCK_TPIP                                                  11
      INTEGER BLOCK_TMAN                                                  11
      INTEGER BLOCK_TRDV                                                  11
      INTEGER BLOCK_TRDS                                                  11
      INTEGER BLOCK_TRDC                                                  11
C -                                                                       14
      REAL TPCG_VERSION                                                   14
      EQUIVALENCE (BANK_TPCG(3),TPCG_VERSION,RE_TPCG_VERSION),            14
     *(NAMES_TPCG(1),NTPCG_VERSION)                                       14
      EQUIVALENCE (COMMEN_TPCG(1),CTPCG_VERSION),(MAP_TPCG(1,1),          14
     *MTPCG_VERSION)                                                      14
      CHARACTER CTPCG_VERSION*40/' '/                                     14
      CHARACTER NTPCG_VERSION*12/'R.VERSION'/                             14
      INTEGER MTPCG_VERSION(2)/1,1/                                       14
      REAL RE_TPCG_VERSION                                                14
C -                                                                       14
      REAL TPCG_RMIN                                                      14
      EQUIVALENCE (BANK_TPCG(4),TPCG_RMIN,RE_TPCG_RMIN),(NAMES_TPCG(2),   14
     *NTPCG_RMIN)                                                         14
      EQUIVALENCE (COMMEN_TPCG(2),CTPCG_RMIN),(MAP_TPCG(1,2),             14
     *MTPCG_RMIN)                                                         14
      CHARACTER CTPCG_RMIN*40/' '/                                        14
      CHARACTER NTPCG_RMIN*12/'R.RMIN'/                                   14
      INTEGER MTPCG_RMIN(2)/1,1/                                          14
      REAL RE_TPCG_RMIN                                                   14
C -                                                                       14
      REAL TPCG_RMAX                                                      14
      EQUIVALENCE (BANK_TPCG(5),TPCG_RMAX,RE_TPCG_RMAX),(NAMES_TPCG(3),   14
     *NTPCG_RMAX)                                                         14
      EQUIVALENCE (COMMEN_TPCG(3),CTPCG_RMAX),(MAP_TPCG(1,3),             14
     *MTPCG_RMAX)                                                         14
      CHARACTER CTPCG_RMAX*40/' '/                                        14
      CHARACTER NTPCG_RMAX*12/'R.RMAX'/                                   14
      INTEGER MTPCG_RMAX(2)/1,1/                                          14
      REAL RE_TPCG_RMAX                                                   14
C -                                                                       14
      REAL TPCG_LENGTH                                                    14
      EQUIVALENCE (BANK_TPCG(6),TPCG_LENGTH,RE_TPCG_LENGTH),              14
     *(NAMES_TPCG(4),NTPCG_LENGTH)                                        14
      EQUIVALENCE (COMMEN_TPCG(4),CTPCG_LENGTH),(MAP_TPCG(1,4),           14
     *MTPCG_LENGTH)                                                       14
      CHARACTER CTPCG_LENGTH*40/' '/                                      14
      CHARACTER NTPCG_LENGTH*12/'R.LENGTH'/                               14
      INTEGER MTPCG_LENGTH(2)/1,1/                                        14
      REAL RE_TPCG_LENGTH                                                 14
C -                                                                       14
      REAL TPCG_WHEELIR                                                   14
      EQUIVALENCE (BANK_TPCG(7),TPCG_WHEELIR,RE_TPCG_WHEELIR),            14
     *(NAMES_TPCG(5),NTPCG_WHEELIR)                                       14
      EQUIVALENCE (COMMEN_TPCG(5),CTPCG_WHEELIR),(MAP_TPCG(1,5),          14
     *MTPCG_WHEELIR)                                                      14
      CHARACTER CTPCG_WHEELIR*40/' '/                                     14
      CHARACTER NTPCG_WHEELIR*12/'R.WHEELIR'/                             14
      INTEGER MTPCG_WHEELIR(2)/1,1/                                       14
      REAL RE_TPCG_WHEELIR                                                14
C -                                                                       14
      REAL TPCG_WHEELOR                                                   14
      EQUIVALENCE (BANK_TPCG(8),TPCG_WHEELOR,RE_TPCG_WHEELOR),            14
     *(NAMES_TPCG(6),NTPCG_WHEELOR)                                       14
      EQUIVALENCE (COMMEN_TPCG(6),CTPCG_WHEELOR),(MAP_TPCG(1,6),          14
     *MTPCG_WHEELOR)                                                      14
      CHARACTER CTPCG_WHEELOR*40/' '/                                     14
      CHARACTER NTPCG_WHEELOR*12/'R.WHEELOR'/                             14
      INTEGER MTPCG_WHEELOR(2)/1,1/                                       14
      REAL RE_TPCG_WHEELOR                                                14
C -                                                                       14
      REAL TPCG_WHEELTHK                                                  14
      EQUIVALENCE (BANK_TPCG(9),TPCG_WHEELTHK,RE_TPCG_WHEELTHK),          14
     *(NAMES_TPCG(7),NTPCG_WHEELTHK)                                      14
      EQUIVALENCE (COMMEN_TPCG(7),CTPCG_WHEELTHK),(MAP_TPCG(1,7),         14
     *MTPCG_WHEELTHK)                                                     14
      CHARACTER CTPCG_WHEELTHK*40/' '/                                    14
      CHARACTER NTPCG_WHEELTHK*12/'R.WHEELTHK'/                           14
      INTEGER MTPCG_WHEELTHK(2)/1,1/                                      14
      REAL RE_TPCG_WHEELTHK                                               14
C -                                                                       14
      REAL TPCG_SENGASOR                                                  14
      EQUIVALENCE (BANK_TPCG(10),TPCG_SENGASOR,RE_TPCG_SENGASOR),         14
     *(NAMES_TPCG(8),NTPCG_SENGASOR)                                      14
      EQUIVALENCE (COMMEN_TPCG(8),CTPCG_SENGASOR),(MAP_TPCG(1,8),         14
     *MTPCG_SENGASOR)                                                     14
      CHARACTER CTPCG_SENGASOR*40/' '/                                    14
      CHARACTER NTPCG_SENGASOR*12/'R.SENGASOR'/                           14
      INTEGER MTPCG_SENGASOR(2)/1,1/                                      14
      REAL RE_TPCG_SENGASOR                                               14
C -                                                                       14
      REAL TPCG_TPEATHK                                                   14
      EQUIVALENCE (BANK_TPCG(11),TPCG_TPEATHK,RE_TPCG_TPEATHK),           14
     *(NAMES_TPCG(9),NTPCG_TPEATHK)                                       14
      EQUIVALENCE (COMMEN_TPCG(9),CTPCG_TPEATHK),(MAP_TPCG(1,9),          14
     *MTPCG_TPEATHK)                                                      14
      CHARACTER CTPCG_TPEATHK*40/' '/                                     14
      CHARACTER NTPCG_TPEATHK*12/'R.TPEATHK'/                             14
      INTEGER MTPCG_TPEATHK(2)/1,1/                                       14
      REAL RE_TPCG_TPEATHK                                                14
C -                                                                       14
      REAL TPCG_MEMBTHK                                                   14
      EQUIVALENCE (BANK_TPCG(12),TPCG_MEMBTHK,RE_TPCG_MEMBTHK),           14
     *(NAMES_TPCG(10),NTPCG_MEMBTHK)                                      14
      EQUIVALENCE (COMMEN_TPCG(10),CTPCG_MEMBTHK),(MAP_TPCG(1,10),        14
     *MTPCG_MEMBTHK)                                                      14
      CHARACTER CTPCG_MEMBTHK*40/' '/                                     14
      CHARACTER NTPCG_MEMBTHK*12/'R.MEMBTHK'/                             14
      INTEGER MTPCG_MEMBTHK(2)/1,1/                                       14
      REAL RE_TPCG_MEMBTHK                                                14
C -                                                                       14
      REAL TPCG_TIADDR                                                    14
      EQUIVALENCE (BANK_TPCG(13),TPCG_TIADDR,RE_TPCG_TIADDR),             14
     *(NAMES_TPCG(11),NTPCG_TIADDR)                                       14
      EQUIVALENCE (COMMEN_TPCG(11),CTPCG_TIADDR),(MAP_TPCG(1,11),         14
     *MTPCG_TIADDR)                                                       14
      CHARACTER CTPCG_TIADDR*40/' '/                                      14
      CHARACTER NTPCG_TIADDR*12/'R.TIADDR'/                               14
      INTEGER MTPCG_TIADDR(2)/1,1/                                        14
      REAL RE_TPCG_TIADDR                                                 14
C -                                                                       14
      REAL TPCG_TINXDR                                                    14
      EQUIVALENCE (BANK_TPCG(14),TPCG_TINXDR,RE_TPCG_TINXDR),             14
     *(NAMES_TPCG(12),NTPCG_TINXDR)                                       14
      EQUIVALENCE (COMMEN_TPCG(12),CTPCG_TINXDR),(MAP_TPCG(1,12),         14
     *MTPCG_TINXDR)                                                       14
      CHARACTER CTPCG_TINXDR*40/' '/                                      14
      CHARACTER NTPCG_TINXDR*12/'R.TINXDR'/                               14
      INTEGER MTPCG_TINXDR(2)/1,1/                                        14
      REAL RE_TPCG_TINXDR                                                 14
C -                                                                       14
      REAL TPCG_TIKADR                                                    14
      EQUIVALENCE (BANK_TPCG(15),TPCG_TIKADR,RE_TPCG_TIKADR),             14
     *(NAMES_TPCG(13),NTPCG_TIKADR)                                       14
      EQUIVALENCE (COMMEN_TPCG(13),CTPCG_TIKADR),(MAP_TPCG(1,13),         14
     *MTPCG_TIKADR)                                                       14
      CHARACTER CTPCG_TIKADR*40/' '/                                      14
      CHARACTER NTPCG_TIKADR*12/'R.TIKADR'/                               14
      INTEGER MTPCG_TIKADR(2)/1,1/                                        14
      REAL RE_TPCG_TIKADR                                                 14
C -                                                                       14
      REAL TPCG_TIALDR                                                    14
      EQUIVALENCE (BANK_TPCG(16),TPCG_TIALDR,RE_TPCG_TIALDR),             14
     *(NAMES_TPCG(14),NTPCG_TIALDR)                                       14
      EQUIVALENCE (COMMEN_TPCG(14),CTPCG_TIALDR),(MAP_TPCG(1,14),         14
     *MTPCG_TIALDR)                                                       14
      CHARACTER CTPCG_TIALDR*40/' '/                                      14
      CHARACTER NTPCG_TIALDR*12/'R.TIALDR'/                               14
      INTEGER MTPCG_TIALDR(2)/1,1/                                        14
      REAL RE_TPCG_TIALDR                                                 14
C -                                                                       14
      REAL TPCG_TOCSDR                                                    14
      EQUIVALENCE (BANK_TPCG(17),TPCG_TOCSDR,RE_TPCG_TOCSDR),             14
     *(NAMES_TPCG(15),NTPCG_TOCSDR)                                       14
      EQUIVALENCE (COMMEN_TPCG(15),CTPCG_TOCSDR),(MAP_TPCG(1,15),         14
     *MTPCG_TOCSDR)                                                       14
      CHARACTER CTPCG_TOCSDR*40/' '/                                      14
      CHARACTER NTPCG_TOCSDR*12/'R.TOCSDR'/                               14
      INTEGER MTPCG_TOCSDR(2)/1,1/                                        14
      REAL RE_TPCG_TOCSDR                                                 14
C -                                                                       14
      REAL TPCG_TOKADR                                                    14
      EQUIVALENCE (BANK_TPCG(18),TPCG_TOKADR,RE_TPCG_TOKADR),             14
     *(NAMES_TPCG(16),NTPCG_TOKADR)                                       14
      EQUIVALENCE (COMMEN_TPCG(16),CTPCG_TOKADR),(MAP_TPCG(1,16),         14
     *MTPCG_TOKADR)                                                       14
      CHARACTER CTPCG_TOKADR*40/' '/                                      14
      CHARACTER NTPCG_TOKADR*12/'R.TOKADR'/                               14
      INTEGER MTPCG_TOKADR(2)/1,1/                                        14
      REAL RE_TPCG_TOKADR                                                 14
C -                                                                       14
      REAL TPCG_TONXDR                                                    14
      EQUIVALENCE (BANK_TPCG(19),TPCG_TONXDR,RE_TPCG_TONXDR),             14
     *(NAMES_TPCG(17),NTPCG_TONXDR)                                       14
      EQUIVALENCE (COMMEN_TPCG(17),CTPCG_TONXDR),(MAP_TPCG(1,17),         14
     *MTPCG_TONXDR)                                                       14
      CHARACTER CTPCG_TONXDR*40/' '/                                      14
      CHARACTER NTPCG_TONXDR*12/'R.TONXDR'/                               14
      INTEGER MTPCG_TONXDR(2)/1,1/                                        14
      REAL RE_TPCG_TONXDR                                                 14
C -                                                                       14
      REAL TPCG_TOADDR                                                    14
      EQUIVALENCE (BANK_TPCG(20),TPCG_TOADDR,RE_TPCG_TOADDR),             14
     *(NAMES_TPCG(18),NTPCG_TOADDR)                                       14
      EQUIVALENCE (COMMEN_TPCG(18),CTPCG_TOADDR),(MAP_TPCG(1,18),         14
     *MTPCG_TOADDR)                                                       14
      CHARACTER CTPCG_TOADDR*40/' '/                                      14
      CHARACTER NTPCG_TOADDR*12/'R.TOADDR'/                               14
      INTEGER MTPCG_TOADDR(2)/1,1/                                        14
      REAL RE_TPCG_TOADDR                                                 14
C -                                                                       14
      REAL TPCG_TOIGDR                                                    14
      EQUIVALENCE (BANK_TPCG(21),TPCG_TOIGDR,RE_TPCG_TOIGDR),             14
     *(NAMES_TPCG(19),NTPCG_TOIGDR)                                       14
      EQUIVALENCE (COMMEN_TPCG(19),CTPCG_TOIGDR),(MAP_TPCG(1,19),         14
     *MTPCG_TOIGDR)                                                       14
      CHARACTER CTPCG_TOIGDR*40/' '/                                      14
      CHARACTER NTPCG_TOIGDR*12/'R.TOIGDR'/                               14
      INTEGER MTPCG_TOIGDR(2)/1,1/                                        14
      REAL RE_TPCG_TOIGDR                                                 14
C -                                                                       14
      REAL TPCG_TOALDR                                                    14
      EQUIVALENCE (BANK_TPCG(22),TPCG_TOALDR,RE_TPCG_TOALDR),             14
     *(NAMES_TPCG(20),NTPCG_TOALDR)                                       14
      EQUIVALENCE (COMMEN_TPCG(20),CTPCG_TOALDR),(MAP_TPCG(1,20),         14
     *MTPCG_TOALDR)                                                       14
      CHARACTER CTPCG_TOALDR*40/' '/                                      14
      CHARACTER NTPCG_TOALDR*12/'R.TOALDR'/                               14
      INTEGER MTPCG_TOALDR(2)/1,1/                                        14
      REAL RE_TPCG_TOALDR                                                 14
C -                                                                       14
      REAL TPCG_TOHADR                                                    14
      EQUIVALENCE (BANK_TPCG(23),TPCG_TOHADR,RE_TPCG_TOHADR),             14
     *(NAMES_TPCG(21),NTPCG_TOHADR)                                       14
      EQUIVALENCE (COMMEN_TPCG(21),CTPCG_TOHADR),(MAP_TPCG(1,21),         14
     *MTPCG_TOHADR)                                                       14
      CHARACTER CTPCG_TOHADR*40/' '/                                      14
      CHARACTER NTPCG_TOHADR*12/'R.TOHADR'/                               14
      INTEGER MTPCG_TOHADR(2)/1,1/                                        14
      REAL RE_TPCG_TOHADR                                                 14
C -                                                                       14
      REAL TPCG_MWCREAD                                                   14
      EQUIVALENCE (BANK_TPCG(24),TPCG_MWCREAD,RE_TPCG_MWCREAD),           14
     *(NAMES_TPCG(22),NTPCG_MWCREAD)                                      14
      EQUIVALENCE (COMMEN_TPCG(22),CTPCG_MWCREAD),(MAP_TPCG(1,22),        14
     *MTPCG_MWCREAD)                                                      14
      CHARACTER CTPCG_MWCREAD*40/' '/                                     14
      CHARACTER NTPCG_MWCREAD*12/'R.MWCREAD'/                             14
      INTEGER MTPCG_MWCREAD(2)/1,1/                                       14
      REAL RE_TPCG_MWCREAD                                                14
C -                                                                       14
      REAL TPCG_GASCORR                                                   14
      EQUIVALENCE (BANK_TPCG(25),TPCG_GASCORR,RE_TPCG_GASCORR),           14
     *(NAMES_TPCG(23),NTPCG_GASCORR)                                      14
      EQUIVALENCE (COMMEN_TPCG(23),CTPCG_GASCORR),(MAP_TPCG(1,23),        14
     *MTPCG_GASCORR)                                                      14
      CHARACTER CTPCG_GASCORR*40/' '/                                     14
      CHARACTER NTPCG_GASCORR*12/'R.GASCORR'/                             14
      INTEGER MTPCG_GASCORR(2)/1,1/                                       14
      REAL RE_TPCG_GASCORR                                                14
C -                                                                       14
      INTEGER LEN_TPCG,LLEN_TPCG,LINK_TPCG                                14
      PARAMETER (LEN_TPCG=25)                                             14
      PARAMETER (LLEN_TPCG=23)                                            14
      INTEGER FLAG_TPCG/0/,MAP_TPCG(2,LLEN_TPCG),IDNUM_TPCG(10)           14
      CHARACTER NAMES_TPCG(LLEN_TPCG)*12,COMMEN_TPCG(LLEN_TPCG)*40        14
      DATA IDNUM_TPCG/10*0/                                               14
      REAL BANK_TPCG(LEN_TPCG)                                            14
      CHARACTER*60 BPATH_TPCG/' '/                                        14
      SAVE FLAG_TPCG,MAP_TPCG,IDNUM_TPCG,NAMES_TPCG,COMMEN_TPCG,          14
     *BPATH_TPCG                                                          14
      DATA LINK_TPCG/0/,BANK_TPCG/LEN_TPCG*0/                             14
C *                                                                       14
C -                                                                       19
      REAL TECW_SEC                                                       19
      EQUIVALENCE (BANK_TECW(3),TECW_SEC,RE_TECW_SEC),(NAMES_TECW(1),     19
     *NTECW_SEC)                                                          19
      EQUIVALENCE (COMMEN_TECW(1),CTECW_SEC),(MAP_TECW(1,1),MTECW_SEC)    19
      CHARACTER CTECW_SEC*40/' '/                                         19
      CHARACTER NTECW_SEC*12/'R.SEC'/                                     19
      INTEGER MTECW_SEC(2)/1,1/                                           19
      REAL RE_TECW_SEC                                                    19
C -                                                                       19
      REAL TECW_GAPRAD                                                    19
      EQUIVALENCE (BANK_TECW(4),TECW_GAPRAD,RE_TECW_GAPRAD),              19
     *(NAMES_TECW(2),NTECW_GAPRAD)                                        19
      EQUIVALENCE (COMMEN_TECW(2),CTECW_GAPRAD),(MAP_TECW(1,2),           19
     *MTECW_GAPRAD)                                                       19
      CHARACTER CTECW_GAPRAD*40/' '/                                      19
      CHARACTER NTECW_GAPRAD*12/'R.GAPRAD'/                               19
      INTEGER MTECW_GAPRAD(2)/1,1/                                        19
      REAL RE_TECW_GAPRAD                                                 19
C -                                                                       19
      REAL TECW_GAPHEIT                                                   19
      EQUIVALENCE (BANK_TECW(5),TECW_GAPHEIT,RE_TECW_GAPHEIT),            19
     *(NAMES_TECW(3),NTECW_GAPHEIT)                                       19
      EQUIVALENCE (COMMEN_TECW(3),CTECW_GAPHEIT),(MAP_TECW(1,3),          19
     *MTECW_GAPHEIT)                                                      19
      CHARACTER CTECW_GAPHEIT*40/' '/                                     19
      CHARACTER NTECW_GAPHEIT*12/'R.GAPHEIT'/                             19
      INTEGER MTECW_GAPHEIT(2)/1,1/                                       19
      REAL RE_TECW_GAPHEIT                                                19
C -                                                                       19
      REAL TECW_GAPWIDI                                                   19
      EQUIVALENCE (BANK_TECW(6),TECW_GAPWIDI,RE_TECW_GAPWIDI),            19
     *(NAMES_TECW(4),NTECW_GAPWIDI)                                       19
      EQUIVALENCE (COMMEN_TECW(4),CTECW_GAPWIDI),(MAP_TECW(1,4),          19
     *MTECW_GAPWIDI)                                                      19
      CHARACTER CTECW_GAPWIDI*40/' '/                                     19
      CHARACTER NTECW_GAPWIDI*12/'R.GAPWIDI'/                             19
      INTEGER MTECW_GAPWIDI(2)/1,1/                                       19
      REAL RE_TECW_GAPWIDI                                                19
C -                                                                       19
      REAL TECW_GAPWIDO                                                   19
      EQUIVALENCE (BANK_TECW(7),TECW_GAPWIDO,RE_TECW_GAPWIDO),            19
     *(NAMES_TECW(5),NTECW_GAPWIDO)                                       19
      EQUIVALENCE (COMMEN_TECW(5),CTECW_GAPWIDO),(MAP_TECW(1,5),          19
     *MTECW_GAPWIDO)                                                      19
      CHARACTER CTECW_GAPWIDO*40/' '/                                     19
      CHARACTER NTECW_GAPWIDO*12/'R.GAPWIDO'/                             19
      INTEGER MTECW_GAPWIDO(2)/1,1/                                       19
      REAL RE_TECW_GAPWIDO                                                19
C -                                                                       19
      REAL TECW_GAPSHFT                                                   19
      EQUIVALENCE (BANK_TECW(8),TECW_GAPSHFT,RE_TECW_GAPSHFT),            19
     *(NAMES_TECW(6),NTECW_GAPSHFT)                                       19
      EQUIVALENCE (COMMEN_TECW(6),CTECW_GAPSHFT),(MAP_TECW(1,6),          19
     *MTECW_GAPSHFT)                                                      19
      CHARACTER CTECW_GAPSHFT*40/' '/                                     19
      CHARACTER NTECW_GAPSHFT*12/'R.GAPSHFT'/                             19
      INTEGER MTECW_GAPSHFT(2)/1,1/                                       19
      REAL RE_TECW_GAPSHFT                                                19
C -                                                                       19
      REAL TECW_INWIDTH                                                   19
      EQUIVALENCE (BANK_TECW(9),TECW_INWIDTH,RE_TECW_INWIDTH),            19
     *(NAMES_TECW(7),NTECW_INWIDTH)                                       19
      EQUIVALENCE (COMMEN_TECW(7),CTECW_INWIDTH),(MAP_TECW(1,7),          19
     *MTECW_INWIDTH)                                                      19
      CHARACTER CTECW_INWIDTH*40/' '/                                     19
      CHARACTER NTECW_INWIDTH*12/'R.INWIDTH'/                             19
      INTEGER MTECW_INWIDTH(2)/1,1/                                       19
      REAL RE_TECW_INWIDTH                                                19
C -                                                                       19
      REAL TECW_OUWIDTH                                                   19
      EQUIVALENCE (BANK_TECW(10),TECW_OUWIDTH,RE_TECW_OUWIDTH),           19
     *(NAMES_TECW(8),NTECW_OUWIDTH)                                       19
      EQUIVALENCE (COMMEN_TECW(8),CTECW_OUWIDTH),(MAP_TECW(1,8),          19
     *MTECW_OUWIDTH)                                                      19
      CHARACTER CTECW_OUWIDTH*40/' '/                                     19
      CHARACTER NTECW_OUWIDTH*12/'R.OUWIDTH'/                             19
      INTEGER MTECW_OUWIDTH(2)/1,1/                                       19
      REAL RE_TECW_OUWIDTH                                                19
C -                                                                       19
      REAL TECW_HEIGHT                                                    19
      EQUIVALENCE (BANK_TECW(11),TECW_HEIGHT,RE_TECW_HEIGHT),             19
     *(NAMES_TECW(9),NTECW_HEIGHT)                                        19
      EQUIVALENCE (COMMEN_TECW(9),CTECW_HEIGHT),(MAP_TECW(1,9),           19
     *MTECW_HEIGHT)                                                       19
      CHARACTER CTECW_HEIGHT*40/' '/                                      19
      CHARACTER NTECW_HEIGHT*12/'R.HEIGHT'/                               19
      INTEGER MTECW_HEIGHT(2)/1,1/                                        19
      REAL RE_TECW_HEIGHT                                                 19
C -                                                                       19
      REAL TECW_PPDEPTH                                                   19
      EQUIVALENCE (BANK_TECW(12),TECW_PPDEPTH,RE_TECW_PPDEPTH),           19
     *(NAMES_TECW(10),NTECW_PPDEPTH)                                      19
      EQUIVALENCE (COMMEN_TECW(10),CTECW_PPDEPTH),(MAP_TECW(1,10),        19
     *MTECW_PPDEPTH)                                                      19
      CHARACTER CTECW_PPDEPTH*40/' '/                                     19
      CHARACTER NTECW_PPDEPTH*12/'R.PPDEPTH'/                             19
      INTEGER MTECW_PPDEPTH(2)/1,1/                                       19
      REAL RE_TECW_PPDEPTH                                                19
C -                                                                       19
      REAL TECW_ASDEPTH                                                   19
      EQUIVALENCE (BANK_TECW(13),TECW_ASDEPTH,RE_TECW_ASDEPTH),           19
     *(NAMES_TECW(11),NTECW_ASDEPTH)                                      19
      EQUIVALENCE (COMMEN_TECW(11),CTECW_ASDEPTH),(MAP_TECW(1,11),        19
     *MTECW_ASDEPTH)                                                      19
      CHARACTER CTECW_ASDEPTH*40/' '/                                     19
      CHARACTER NTECW_ASDEPTH*12/'R.ASDEPTH'/                             19
      INTEGER MTECW_ASDEPTH(2)/1,1/                                       19
      REAL RE_TECW_ASDEPTH                                                19
C -                                                                       19
      REAL TECW_GGDEPTH                                                   19
      EQUIVALENCE (BANK_TECW(14),TECW_GGDEPTH,RE_TECW_GGDEPTH),           19
     *(NAMES_TECW(12),NTECW_GGDEPTH)                                      19
      EQUIVALENCE (COMMEN_TECW(12),CTECW_GGDEPTH),(MAP_TECW(1,12),        19
     *MTECW_GGDEPTH)                                                      19
      CHARACTER CTECW_GGDEPTH*40/' '/                                     19
      CHARACTER NTECW_GGDEPTH*12/'R.GGDEPTH'/                             19
      INTEGER MTECW_GGDEPTH(2)/1,1/                                       19
      REAL RE_TECW_GGDEPTH                                                19
C -                                                                       19
      REAL TECW_MWCDEPTH                                                  19
      EQUIVALENCE (BANK_TECW(15),TECW_MWCDEPTH,RE_TECW_MWCDEPTH),         19
     *(NAMES_TECW(13),NTECW_MWCDEPTH)                                     19
      EQUIVALENCE (COMMEN_TECW(13),CTECW_MWCDEPTH),(MAP_TECW(1,13),       19
     *MTECW_MWCDEPTH)                                                     19
      CHARACTER CTECW_MWCDEPTH*40/' '/                                    19
      CHARACTER NTECW_MWCDEPTH*12/'R.MWCDEPTH'/                           19
      INTEGER MTECW_MWCDEPTH(2)/1,1/                                      19
      REAL RE_TECW_MWCDEPTH                                               19
C -                                                                       19
      REAL TECW_BOUNDARY                                                  19
      EQUIVALENCE (BANK_TECW(16),TECW_BOUNDARY,RE_TECW_BOUNDARY),         19
     *(NAMES_TECW(14),NTECW_BOUNDARY)                                     19
      EQUIVALENCE (COMMEN_TECW(14),CTECW_BOUNDARY),(MAP_TECW(1,14),       19
     *MTECW_BOUNDARY)                                                     19
      CHARACTER CTECW_BOUNDARY*40/' '/                                    19
      CHARACTER NTECW_BOUNDARY*12/'R.BOUNDARY'/                           19
      INTEGER MTECW_BOUNDARY(2)/1,1/                                      19
      REAL RE_TECW_BOUNDARY                                               19
C -                                                                       19
      REAL TECW_RCENTER                                                   19
      EQUIVALENCE (BANK_TECW(17),TECW_RCENTER,RE_TECW_RCENTER),           19
     *(NAMES_TECW(15),NTECW_RCENTER)                                      19
      EQUIVALENCE (COMMEN_TECW(15),CTECW_RCENTER),(MAP_TECW(1,15),        19
     *MTECW_RCENTER)                                                      19
      CHARACTER CTECW_RCENTER*40/' '/                                     19
      CHARACTER NTECW_RCENTER*12/'R.RCENTER'/                             19
      INTEGER MTECW_RCENTER(2)/1,1/                                       19
      REAL RE_TECW_RCENTER                                                19
C -                                                                       19
      REAL TECW_MWCINN                                                    19
      EQUIVALENCE (BANK_TECW(18),TECW_MWCINN,RE_TECW_MWCINN),             19
     *(NAMES_TECW(16),NTECW_MWCINN)                                       19
      EQUIVALENCE (COMMEN_TECW(16),CTECW_MWCINN),(MAP_TECW(1,16),         19
     *MTECW_MWCINN)                                                       19
      CHARACTER CTECW_MWCINN*40/' '/                                      19
      CHARACTER NTECW_MWCINN*12/'R.MWCINN'/                               19
      INTEGER MTECW_MWCINN(2)/1,1/                                        19
      REAL RE_TECW_MWCINN                                                 19
C -                                                                       19
      REAL TECW_MWCOUT                                                    19
      EQUIVALENCE (BANK_TECW(19),TECW_MWCOUT,RE_TECW_MWCOUT),             19
     *(NAMES_TECW(17),NTECW_MWCOUT)                                       19
      EQUIVALENCE (COMMEN_TECW(17),CTECW_MWCOUT),(MAP_TECW(1,17),         19
     *MTECW_MWCOUT)                                                       19
      CHARACTER CTECW_MWCOUT*40/' '/                                      19
      CHARACTER NTECW_MWCOUT*12/'R.MWCOUT'/                               19
      INTEGER MTECW_MWCOUT(2)/1,1/                                        19
      REAL RE_TECW_MWCOUT                                                 19
C -                                                                       19
      REAL TECW_MWCHEI                                                    19
      EQUIVALENCE (BANK_TECW(20),TECW_MWCHEI,RE_TECW_MWCHEI),             19
     *(NAMES_TECW(18),NTECW_MWCHEI)                                       19
      EQUIVALENCE (COMMEN_TECW(18),CTECW_MWCHEI),(MAP_TECW(1,18),         19
     *MTECW_MWCHEI)                                                       19
      CHARACTER CTECW_MWCHEI*40/' '/                                      19
      CHARACTER NTECW_MWCHEI*12/'R.MWCHEI'/                               19
      INTEGER MTECW_MWCHEI(2)/1,1/                                        19
      REAL RE_TECW_MWCHEI                                                 19
C -                                                                       19
      REAL TECW_MWCCENT                                                   19
      EQUIVALENCE (BANK_TECW(21),TECW_MWCCENT,RE_TECW_MWCCENT),           19
     *(NAMES_TECW(19),NTECW_MWCCENT)                                      19
      EQUIVALENCE (COMMEN_TECW(19),CTECW_MWCCENT),(MAP_TECW(1,19),        19
     *MTECW_MWCCENT)                                                      19
      CHARACTER CTECW_MWCCENT*40/' '/                                     19
      CHARACTER NTECW_MWCCENT*12/'R.MWCCENT'/                             19
      INTEGER MTECW_MWCCENT(2)/1,1/                                       19
      REAL RE_TECW_MWCCENT                                                19
C -                                                                       19
      REAL TECW_MWCNWIR                                                   19
      EQUIVALENCE (BANK_TECW(22),TECW_MWCNWIR,RE_TECW_MWCNWIR),           19
     *(NAMES_TECW(20),NTECW_MWCNWIR)                                      19
      EQUIVALENCE (COMMEN_TECW(20),CTECW_MWCNWIR),(MAP_TECW(1,20),        19
     *MTECW_MWCNWIR)                                                      19
      CHARACTER CTECW_MWCNWIR*40/' '/                                     19
      CHARACTER NTECW_MWCNWIR*12/'R.MWCNWIR'/                             19
      INTEGER MTECW_MWCNWIR(2)/1,1/                                       19
      REAL RE_TECW_MWCNWIR                                                19
C -                                                                       19
      REAL TECW_N                                                         19
      EQUIVALENCE (BANK_TECW(23),TECW_N,RE_TECW_N),(NAMES_TECW(21),       19
     *NTECW_N)                                                            19
      EQUIVALENCE (COMMEN_TECW(21),CTECW_N),(MAP_TECW(1,21),MTECW_N)      19
      CHARACTER CTECW_N*40/' '/                                           19
      CHARACTER NTECW_N*12/'R.N'/                                         19
      INTEGER MTECW_N(2)/1,1/                                             19
      REAL RE_TECW_N                                                      19
C -                                                                       19
      REAL TECW_NEX                                                       19
      EQUIVALENCE (BANK_TECW(24),TECW_NEX,RE_TECW_NEX),(NAMES_TECW(22),   19
     *NTECW_NEX)                                                          19
      EQUIVALENCE (COMMEN_TECW(22),CTECW_NEX),(MAP_TECW(1,22),            19
     *MTECW_NEX)                                                          19
      CHARACTER CTECW_NEX*40/' '/                                         19
      CHARACTER NTECW_NEX*12/'R.NEX'/                                     19
      INTEGER MTECW_NEX(2)/1,1/                                           19
      REAL RE_TECW_NEX                                                    19
C -                                                                       19
      REAL TECW_Z(8)                                                      19
      EQUIVALENCE (BANK_TECW(25),TECW_Z,RE_TECW_Z),(NAMES_TECW(23),       19
     *NTECW_Z)                                                            19
      EQUIVALENCE (COMMEN_TECW(23),CTECW_Z),(MAP_TECW(1,23),MTECW_Z)      19
      CHARACTER CTECW_Z*40/' '/                                           19
      CHARACTER NTECW_Z*12/'R.Z'/                                         19
      INTEGER MTECW_Z(2)/8,1/                                             19
      REAL RE_TECW_Z                                                      19
C -                                                                       19
      REAL TECW_DZ(8)                                                     19
      EQUIVALENCE (BANK_TECW(33),TECW_DZ,RE_TECW_DZ),(NAMES_TECW(24),     19
     *NTECW_DZ)                                                           19
      EQUIVALENCE (COMMEN_TECW(24),CTECW_DZ),(MAP_TECW(1,24),MTECW_DZ)    19
      CHARACTER CTECW_DZ*40/' '/                                          19
      CHARACTER NTECW_DZ*12/'R.DZ'/                                       19
      INTEGER MTECW_DZ(2)/8,1/                                            19
      REAL RE_TECW_DZ                                                     19
C -                                                                       19
      REAL TECW_XEX(5)                                                    19
      EQUIVALENCE (BANK_TECW(41),TECW_XEX,RE_TECW_XEX),(NAMES_TECW(25),   19
     *NTECW_XEX)                                                          19
      EQUIVALENCE (COMMEN_TECW(25),CTECW_XEX),(MAP_TECW(1,25),            19
     *MTECW_XEX)                                                          19
      CHARACTER CTECW_XEX*40/' '/                                         19
      CHARACTER NTECW_XEX*12/'R.XEX'/                                     19
      INTEGER MTECW_XEX(2)/5,1/                                           19
      REAL RE_TECW_XEX                                                    19
C -                                                                       19
      REAL TECW_ZEX(5)                                                    19
      EQUIVALENCE (BANK_TECW(46),TECW_ZEX,RE_TECW_ZEX),(NAMES_TECW(26),   19
     *NTECW_ZEX)                                                          19
      EQUIVALENCE (COMMEN_TECW(26),CTECW_ZEX),(MAP_TECW(1,26),            19
     *MTECW_ZEX)                                                          19
      CHARACTER CTECW_ZEX*40/' '/                                         19
      CHARACTER NTECW_ZEX*12/'R.ZEX'/                                     19
      INTEGER MTECW_ZEX(2)/5,1/                                           19
      REAL RE_TECW_ZEX                                                    19
C -                                                                       19
      REAL TECW_DXEX(5)                                                   19
      EQUIVALENCE (BANK_TECW(51),TECW_DXEX,RE_TECW_DXEX),                 19
     *(NAMES_TECW(27),NTECW_DXEX)                                         19
      EQUIVALENCE (COMMEN_TECW(27),CTECW_DXEX),(MAP_TECW(1,27),           19
     *MTECW_DXEX)                                                         19
      CHARACTER CTECW_DXEX*40/' '/                                        19
      CHARACTER NTECW_DXEX*12/'R.DXEX'/                                   19
      INTEGER MTECW_DXEX(2)/5,1/                                          19
      REAL RE_TECW_DXEX                                                   19
C -                                                                       19
      REAL TECW_DZEX(5)                                                   19
      EQUIVALENCE (BANK_TECW(56),TECW_DZEX,RE_TECW_DZEX),                 19
     *(NAMES_TECW(28),NTECW_DZEX)                                         19
      EQUIVALENCE (COMMEN_TECW(28),CTECW_DZEX),(MAP_TECW(1,28),           19
     *MTECW_DZEX)                                                         19
      CHARACTER CTECW_DZEX*40/' '/                                        19
      CHARACTER NTECW_DZEX*12/'R.DZEX'/                                   19
      INTEGER MTECW_DZEX(2)/5,1/                                          19
      REAL RE_TECW_DZEX                                                   19
C -                                                                       19
      REAL TECW_NHPLANE(8)                                                19
      EQUIVALENCE (BANK_TECW(61),TECW_NHPLANE,RE_TECW_NHPLANE),           19
     *(NAMES_TECW(29),NTECW_NHPLANE)                                      19
      EQUIVALENCE (COMMEN_TECW(29),CTECW_NHPLANE),(MAP_TECW(1,29),        19
     *MTECW_NHPLANE)                                                      19
      CHARACTER CTECW_NHPLANE*40/' '/                                     19
      CHARACTER NTECW_NHPLANE*12/'R.NHPLANE'/                             19
      INTEGER MTECW_NHPLANE(2)/8,1/                                       19
      REAL RE_TECW_NHPLANE                                                19
C -                                                                       19
      REAL TECW_CARDW                                                     19
      EQUIVALENCE (BANK_TECW(69),TECW_CARDW,RE_TECW_CARDW),               19
     *(NAMES_TECW(30),NTECW_CARDW)                                        19
      EQUIVALENCE (COMMEN_TECW(30),CTECW_CARDW),(MAP_TECW(1,30),          19
     *MTECW_CARDW)                                                        19
      CHARACTER CTECW_CARDW*40/' '/                                       19
      CHARACTER NTECW_CARDW*12/'R.CARDW'/                                 19
      INTEGER MTECW_CARDW(2)/1,1/                                         19
      REAL RE_TECW_CARDW                                                  19
C -                                                                       19
      REAL TECW_CARDTH                                                    19
      EQUIVALENCE (BANK_TECW(70),TECW_CARDTH,RE_TECW_CARDTH),             19
     *(NAMES_TECW(31),NTECW_CARDTH)                                       19
      EQUIVALENCE (COMMEN_TECW(31),CTECW_CARDTH),(MAP_TECW(1,31),         19
     *MTECW_CARDTH)                                                       19
      CHARACTER CTECW_CARDTH*40/' '/                                      19
      CHARACTER NTECW_CARDTH*12/'R.CARDTH'/                               19
      INTEGER MTECW_CARDTH(2)/1,1/                                        19
      REAL RE_TECW_CARDTH                                                 19
C -                                                                       19
      REAL TECW_COOLW                                                     19
      EQUIVALENCE (BANK_TECW(71),TECW_COOLW,RE_TECW_COOLW),               19
     *(NAMES_TECW(32),NTECW_COOLW)                                        19
      EQUIVALENCE (COMMEN_TECW(32),CTECW_COOLW),(MAP_TECW(1,32),          19
     *MTECW_COOLW)                                                        19
      CHARACTER CTECW_COOLW*40/' '/                                       19
      CHARACTER NTECW_COOLW*12/'R.COOLW'/                                 19
      INTEGER MTECW_COOLW(2)/1,1/                                         19
      REAL RE_TECW_COOLW                                                  19
C -                                                                       19
      REAL TECW_COOLTH                                                    19
      EQUIVALENCE (BANK_TECW(72),TECW_COOLTH,RE_TECW_COOLTH),             19
     *(NAMES_TECW(33),NTECW_COOLTH)                                       19
      EQUIVALENCE (COMMEN_TECW(33),CTECW_COOLTH),(MAP_TECW(1,33),         19
     *MTECW_COOLTH)                                                       19
      CHARACTER CTECW_COOLTH*40/' '/                                      19
      CHARACTER NTECW_COOLTH*12/'R.COOLTH'/                               19
      INTEGER MTECW_COOLTH(2)/1,1/                                        19
      REAL RE_TECW_COOLTH                                                 19
C -                                                                       19
      REAL TECW_CARDOFF                                                   19
      EQUIVALENCE (BANK_TECW(73),TECW_CARDOFF,RE_TECW_CARDOFF),           19
     *(NAMES_TECW(34),NTECW_CARDOFF)                                      19
      EQUIVALENCE (COMMEN_TECW(34),CTECW_CARDOFF),(MAP_TECW(1,34),        19
     *MTECW_CARDOFF)                                                      19
      CHARACTER CTECW_CARDOFF*40/' '/                                     19
      CHARACTER NTECW_CARDOFF*12/'R.CARDOFF'/                             19
      INTEGER MTECW_CARDOFF(2)/1,1/                                       19
      REAL RE_TECW_CARDOFF                                                19
C -                                                                       19
      REAL TECW_COOLOFF                                                   19
      EQUIVALENCE (BANK_TECW(74),TECW_COOLOFF,RE_TECW_COOLOFF),           19
     *(NAMES_TECW(35),NTECW_COOLOFF)                                      19
      EQUIVALENCE (COMMEN_TECW(35),CTECW_COOLOFF),(MAP_TECW(1,35),        19
     *MTECW_COOLOFF)                                                      19
      CHARACTER CTECW_COOLOFF*40/' '/                                     19
      CHARACTER NTECW_COOLOFF*12/'R.COOLOFF'/                             19
      INTEGER MTECW_COOLOFF(2)/1,1/                                       19
      REAL RE_TECW_COOLOFF                                                19
C -                                                                       19
      REAL TECW_SLOTW                                                     19
      EQUIVALENCE (BANK_TECW(75),TECW_SLOTW,RE_TECW_SLOTW),               19
     *(NAMES_TECW(36),NTECW_SLOTW)                                        19
      EQUIVALENCE (COMMEN_TECW(36),CTECW_SLOTW),(MAP_TECW(1,36),          19
     *MTECW_SLOTW)                                                        19
      CHARACTER CTECW_SLOTW*40/' '/                                       19
      CHARACTER NTECW_SLOTW*12/'R.SLOTW'/                                 19
      INTEGER MTECW_SLOTW(2)/1,1/                                         19
      REAL RE_TECW_SLOTW                                                  19
C -                                                                       19
      REAL TECW_SLOTRAD                                                   19
      EQUIVALENCE (BANK_TECW(76),TECW_SLOTRAD,RE_TECW_SLOTRAD),           19
     *(NAMES_TECW(37),NTECW_SLOTRAD)                                      19
      EQUIVALENCE (COMMEN_TECW(37),CTECW_SLOTRAD),(MAP_TECW(1,37),        19
     *MTECW_SLOTRAD)                                                      19
      CHARACTER CTECW_SLOTRAD*40/' '/                                     19
      CHARACTER NTECW_SLOTRAD*12/'R.SLOTRAD'/                             19
      INTEGER MTECW_SLOTRAD(2)/1,1/                                       19
      REAL RE_TECW_SLOTRAD                                                19
C -                                                                       19
      REAL TECW_PIPETHK                                                   19
      EQUIVALENCE (BANK_TECW(77),TECW_PIPETHK,RE_TECW_PIPETHK),           19
     *(NAMES_TECW(38),NTECW_PIPETHK)                                      19
      EQUIVALENCE (COMMEN_TECW(38),CTECW_PIPETHK),(MAP_TECW(1,38),        19
     *MTECW_PIPETHK)                                                      19
      CHARACTER CTECW_PIPETHK*40/' '/                                     19
      CHARACTER NTECW_PIPETHK*12/'R.PIPETHK'/                             19
      INTEGER MTECW_PIPETHK(2)/1,1/                                       19
      REAL RE_TECW_PIPETHK                                                19
C -                                                                       19
      REAL TECW_PIPEHT                                                    19
      EQUIVALENCE (BANK_TECW(78),TECW_PIPEHT,RE_TECW_PIPEHT),             19
     *(NAMES_TECW(39),NTECW_PIPEHT)                                       19
      EQUIVALENCE (COMMEN_TECW(39),CTECW_PIPEHT),(MAP_TECW(1,39),         19
     *MTECW_PIPEHT)                                                       19
      CHARACTER CTECW_PIPEHT*40/' '/                                      19
      CHARACTER NTECW_PIPEHT*12/'R.PIPEHT'/                               19
      INTEGER MTECW_PIPEHT(2)/1,1/                                        19
      REAL RE_TECW_PIPEHT                                                 19
C -                                                                       19
      REAL TECW_MANITHK                                                   19
      EQUIVALENCE (BANK_TECW(79),TECW_MANITHK,RE_TECW_MANITHK),           19
     *(NAMES_TECW(40),NTECW_MANITHK)                                      19
      EQUIVALENCE (COMMEN_TECW(40),CTECW_MANITHK),(MAP_TECW(1,40),        19
     *MTECW_MANITHK)                                                      19
      CHARACTER CTECW_MANITHK*40/' '/                                     19
      CHARACTER NTECW_MANITHK*12/'R.MANITHK'/                             19
      INTEGER MTECW_MANITHK(2)/1,1/                                       19
      REAL RE_TECW_MANITHK                                                19
C -                                                                       19
      REAL TECW_MANIWID                                                   19
      EQUIVALENCE (BANK_TECW(80),TECW_MANIWID,RE_TECW_MANIWID),           19
     *(NAMES_TECW(41),NTECW_MANIWID)                                      19
      EQUIVALENCE (COMMEN_TECW(41),CTECW_MANIWID),(MAP_TECW(1,41),        19
     *MTECW_MANIWID)                                                      19
      CHARACTER CTECW_MANIWID*40/' '/                                     19
      CHARACTER NTECW_MANIWID*12/'R.MANIWID'/                             19
      INTEGER MTECW_MANIWID(2)/1,1/                                       19
      REAL RE_TECW_MANIWID                                                19
C -                                                                       19
      REAL TECW_TAN15                                                     19
      EQUIVALENCE (BANK_TECW(81),TECW_TAN15,RE_TECW_TAN15),               19
     *(NAMES_TECW(42),NTECW_TAN15)                                        19
      EQUIVALENCE (COMMEN_TECW(42),CTECW_TAN15),(MAP_TECW(1,42),          19
     *MTECW_TAN15)                                                        19
      CHARACTER CTECW_TAN15*40/' '/                                       19
      CHARACTER NTECW_TAN15*12/'R.TAN15'/                                 19
      INTEGER MTECW_TAN15(2)/1,1/                                         19
      REAL RE_TECW_TAN15                                                  19
C -                                                                       19
      REAL TECW_CLEARANCE                                                 19
      EQUIVALENCE (BANK_TECW(82),TECW_CLEARANCE,RE_TECW_CLEARANCE),       19
     *(NAMES_TECW(43),NTECW_CLEARANCE)                                    19
      EQUIVALENCE (COMMEN_TECW(43),CTECW_CLEARANCE),(MAP_TECW(1,43),      19
     *MTECW_CLEARANCE)                                                    19
      CHARACTER CTECW_CLEARANCE*40/' '/                                   19
      CHARACTER NTECW_CLEARANCE*12/'R.CLEARANCE'/                         19
      INTEGER MTECW_CLEARANCE(2)/1,1/                                     19
      REAL RE_TECW_CLEARANCE                                              19
C -                                                                       19
      REAL TECW_WHLIPTHK                                                  19
      EQUIVALENCE (BANK_TECW(83),TECW_WHLIPTHK,RE_TECW_WHLIPTHK),         19
     *(NAMES_TECW(44),NTECW_WHLIPTHK)                                     19
      EQUIVALENCE (COMMEN_TECW(44),CTECW_WHLIPTHK),(MAP_TECW(1,44),       19
     *MTECW_WHLIPTHK)                                                     19
      CHARACTER CTECW_WHLIPTHK*40/' '/                                    19
      CHARACTER NTECW_WHLIPTHK*12/'R.WHLIPTHK'/                           19
      INTEGER MTECW_WHLIPTHK(2)/1,1/                                      19
      REAL RE_TECW_WHLIPTHK                                               19
C -                                                                       19
      REAL TECW_WHLIPWID                                                  19
      EQUIVALENCE (BANK_TECW(84),TECW_WHLIPWID,RE_TECW_WHLIPWID),         19
     *(NAMES_TECW(45),NTECW_WHLIPWID)                                     19
      EQUIVALENCE (COMMEN_TECW(45),CTECW_WHLIPWID),(MAP_TECW(1,45),       19
     *MTECW_WHLIPWID)                                                     19
      CHARACTER CTECW_WHLIPWID*40/' '/                                    19
      CHARACTER NTECW_WHLIPWID*12/'R.WHLIPWID'/                           19
      INTEGER MTECW_WHLIPWID(2)/1,1/                                      19
      REAL RE_TECW_WHLIPWID                                               19
C -                                                                       19
      REAL TECW_WHBLKLEN                                                  19
      EQUIVALENCE (BANK_TECW(85),TECW_WHBLKLEN,RE_TECW_WHBLKLEN),         19
     *(NAMES_TECW(46),NTECW_WHBLKLEN)                                     19
      EQUIVALENCE (COMMEN_TECW(46),CTECW_WHBLKLEN),(MAP_TECW(1,46),       19
     *MTECW_WHBLKLEN)                                                     19
      CHARACTER CTECW_WHBLKLEN*40/' '/                                    19
      CHARACTER NTECW_WHBLKLEN*12/'R.WHBLKLEN'/                           19
      INTEGER MTECW_WHBLKLEN(2)/1,1/                                      19
      REAL RE_TECW_WHBLKLEN                                               19
C -                                                                       19
      REAL TECW_WHBLKPOS                                                  19
      EQUIVALENCE (BANK_TECW(86),TECW_WHBLKPOS,RE_TECW_WHBLKPOS),         19
     *(NAMES_TECW(47),NTECW_WHBLKPOS)                                     19
      EQUIVALENCE (COMMEN_TECW(47),CTECW_WHBLKPOS),(MAP_TECW(1,47),       19
     *MTECW_WHBLKPOS)                                                     19
      CHARACTER CTECW_WHBLKPOS*40/' '/                                    19
      CHARACTER NTECW_WHBLKPOS*12/'R.WHBLKPOS'/                           19
      INTEGER MTECW_WHBLKPOS(2)/1,1/                                      19
      REAL RE_TECW_WHBLKPOS                                               19
C -                                                                       19
      REAL TECW_WHBLKIN                                                   19
      EQUIVALENCE (BANK_TECW(87),TECW_WHBLKIN,RE_TECW_WHBLKIN),           19
     *(NAMES_TECW(48),NTECW_WHBLKIN)                                      19
      EQUIVALENCE (COMMEN_TECW(48),CTECW_WHBLKIN),(MAP_TECW(1,48),        19
     *MTECW_WHBLKIN)                                                      19
      CHARACTER CTECW_WHBLKIN*40/' '/                                     19
      CHARACTER NTECW_WHBLKIN*12/'R.WHBLKIN'/                             19
      INTEGER MTECW_WHBLKIN(2)/1,1/                                       19
      REAL RE_TECW_WHBLKIN                                                19
C -                                                                       19
      INTEGER LEN_TECW,LLEN_TECW,LINK_TECW                                19
      PARAMETER (LEN_TECW=87)                                             19
      PARAMETER (LLEN_TECW=48)                                            19
      INTEGER FLAG_TECW/0/,MAP_TECW(2,LLEN_TECW),IDNUM_TECW(10)           19
      CHARACTER NAMES_TECW(LLEN_TECW)*12,COMMEN_TECW(LLEN_TECW)*40        19
      DATA IDNUM_TECW/10*0/                                               19
      REAL BANK_TECW(LEN_TECW)                                            19
      CHARACTER*60 BPATH_TECW/' '/                                        19
      SAVE FLAG_TECW,MAP_TECW,IDNUM_TECW,NAMES_TECW,COMMEN_TECW,          19
     *BPATH_TECW                                                          19
      DATA LINK_TECW/0/,BANK_TECW/LEN_TECW*0/                             19
C *                                                                       19
C -                                                                       20
      REAL TROV_SEC                                                       20
      EQUIVALENCE (BANK_TROV(3),TROV_SEC,RE_TROV_SEC),(NAMES_TROV(1),     20
     *NTROV_SEC)                                                          20
      EQUIVALENCE (COMMEN_TROV(1),CTROV_SEC),(MAP_TROV(1,1),MTROV_SEC)    20
      CHARACTER CTROV_SEC*40/' '/                                         20
      CHARACTER NTROV_SEC*12/'R.SEC'/                                     20
      INTEGER MTROV_SEC(2)/1,1/                                           20
      REAL RE_TROV_SEC                                                    20
C -                                                                       20
      REAL TROV_NHP(16)                                                   20
      EQUIVALENCE (BANK_TROV(4),TROV_NHP,RE_TROV_NHP),(NAMES_TROV(2),     20
     *NTROV_NHP)                                                          20
      EQUIVALENCE (COMMEN_TROV(2),CTROV_NHP),(MAP_TROV(1,2),MTROV_NHP)    20
      CHARACTER CTROV_NHP*40/' '/                                         20
      CHARACTER NTROV_NHP*12/'R.NHP'/                                     20
      INTEGER MTROV_NHP(2)/16,1/                                          20
      REAL RE_TROV_NHP                                                    20
C -                                                                       20
      REAL TROV_HX(9,16)                                                  20
      EQUIVALENCE (BANK_TROV(20),TROV_HX,RE_TROV_HX),(NAMES_TROV(3),      20
     *NTROV_HX)                                                           20
      EQUIVALENCE (COMMEN_TROV(3),CTROV_HX),(MAP_TROV(1,3),MTROV_HX)      20
      CHARACTER CTROV_HX*40/' '/                                          20
      CHARACTER NTROV_HX*12/'R.HX'/                                       20
      INTEGER MTROV_HX(2)/9,16/                                           20
      REAL RE_TROV_HX                                                     20
C -                                                                       20
      REAL TROV_OFFCRDIO                                                  20
      EQUIVALENCE (BANK_TROV(164),TROV_OFFCRDIO,RE_TROV_OFFCRDIO),        20
     *(NAMES_TROV(4),NTROV_OFFCRDIO)                                      20
      EQUIVALENCE (COMMEN_TROV(4),CTROV_OFFCRDIO),(MAP_TROV(1,4),         20
     *MTROV_OFFCRDIO)                                                     20
      CHARACTER CTROV_OFFCRDIO*40/' '/                                    20
      CHARACTER NTROV_OFFCRDIO*12/'R.OFFCRDIO'/                           20
      INTEGER MTROV_OFFCRDIO(2)/1,1/                                      20
      REAL RE_TROV_OFFCRDIO                                               20
C -                                                                       20
      INTEGER LEN_TROV,LLEN_TROV,LINK_TROV                                20
      PARAMETER (LEN_TROV=164)                                            20
      PARAMETER (LLEN_TROV=4)                                             20
      INTEGER FLAG_TROV/0/,MAP_TROV(2,LLEN_TROV),IDNUM_TROV(10)           20
      CHARACTER NAMES_TROV(LLEN_TROV)*12,COMMEN_TROV(LLEN_TROV)*40        20
      DATA IDNUM_TROV/10*0/                                               20
      REAL BANK_TROV(LEN_TROV)                                            20
      CHARACTER*60 BPATH_TROV/' '/                                        20
      SAVE FLAG_TROV,MAP_TROV,IDNUM_TROV,NAMES_TROV,COMMEN_TROV,          20
     *BPATH_TROV                                                          20
      DATA LINK_TROV/0/,BANK_TROV/LEN_TROV*0/                             20
C *                                                                       20
C -                                                                       21
      REAL TPRS_SEC                                                       21
      EQUIVALENCE (BANK_TPRS(3),TPRS_SEC,RE_TPRS_SEC),(NAMES_TPRS(1),     21
     *NTPRS_SEC)                                                          21
      EQUIVALENCE (COMMEN_TPRS(1),CTPRS_SEC),(MAP_TPRS(1,1),MTPRS_SEC)    21
      CHARACTER CTPRS_SEC*40/' '/                                         21
      CHARACTER NTPRS_SEC*12/'R.SEC'/                                     21
      INTEGER MTPRS_SEC(2)/1,1/                                           21
      REAL RE_TPRS_SEC                                                    21
C -                                                                       21
      REAL TPRS_NROW                                                      21
      EQUIVALENCE (BANK_TPRS(4),TPRS_NROW,RE_TPRS_NROW),(NAMES_TPRS(2),   21
     *NTPRS_NROW)                                                         21
      EQUIVALENCE (COMMEN_TPRS(2),CTPRS_NROW),(MAP_TPRS(1,2),             21
     *MTPRS_NROW)                                                         21
      CHARACTER CTPRS_NROW*40/' '/                                        21
      CHARACTER NTPRS_NROW*12/'R.NROW'/                                   21
      INTEGER MTPRS_NROW(2)/1,1/                                          21
      REAL RE_TPRS_NROW                                                   21
C -                                                                       21
      REAL TPRS_PITCH                                                     21
      EQUIVALENCE (BANK_TPRS(5),TPRS_PITCH,RE_TPRS_PITCH),                21
     *(NAMES_TPRS(3),NTPRS_PITCH)                                         21
      EQUIVALENCE (COMMEN_TPRS(3),CTPRS_PITCH),(MAP_TPRS(1,3),            21
     *MTPRS_PITCH)                                                        21
      CHARACTER CTPRS_PITCH*40/' '/                                       21
      CHARACTER NTPRS_PITCH*12/'R.PITCH'/                                 21
      INTEGER MTPRS_PITCH(2)/1,1/                                         21
      REAL RE_TPRS_PITCH                                                  21
C -                                                                       21
      REAL TPRS_WIDTH                                                     21
      EQUIVALENCE (BANK_TPRS(6),TPRS_WIDTH,RE_TPRS_WIDTH),                21
     *(NAMES_TPRS(4),NTPRS_WIDTH)                                         21
      EQUIVALENCE (COMMEN_TPRS(4),CTPRS_WIDTH),(MAP_TPRS(1,4),            21
     *MTPRS_WIDTH)                                                        21
      CHARACTER CTPRS_WIDTH*40/' '/                                       21
      CHARACTER NTPRS_WIDTH*12/'R.WIDTH'/                                 21
      INTEGER MTPRS_WIDTH(2)/1,1/                                         21
      REAL RE_TPRS_WIDTH                                                  21
C -                                                                       21
      REAL TPRS_SUPER                                                     21
      EQUIVALENCE (BANK_TPRS(7),TPRS_SUPER,RE_TPRS_SUPER),                21
     *(NAMES_TPRS(5),NTPRS_SUPER)                                         21
      EQUIVALENCE (COMMEN_TPRS(5),CTPRS_SUPER),(MAP_TPRS(1,5),            21
     *MTPRS_SUPER)                                                        21
      CHARACTER CTPRS_SUPER*40/' '/                                       21
      CHARACTER NTPRS_SUPER*12/'R.SUPER'/                                 21
      INTEGER MTPRS_SUPER(2)/1,1/                                         21
      REAL RE_TPRS_SUPER                                                  21
C -                                                                       21
      REAL TPRS_RPADS(40)                                                 21
      EQUIVALENCE (BANK_TPRS(8),TPRS_RPADS,RE_TPRS_RPADS),                21
     *(NAMES_TPRS(6),NTPRS_RPADS)                                         21
      EQUIVALENCE (COMMEN_TPRS(6),CTPRS_RPADS),(MAP_TPRS(1,6),            21
     *MTPRS_RPADS)                                                        21
      CHARACTER CTPRS_RPADS*40/' '/                                       21
      CHARACTER NTPRS_RPADS*12/'R.RPADS'/                                 21
      INTEGER MTPRS_RPADS(2)/40,1/                                        21
      REAL RE_TPRS_RPADS                                                  21
C -                                                                       21
      REAL TPRS_NPADS(40)                                                 21
      EQUIVALENCE (BANK_TPRS(48),TPRS_NPADS,RE_TPRS_NPADS),               21
     *(NAMES_TPRS(7),NTPRS_NPADS)                                         21
      EQUIVALENCE (COMMEN_TPRS(7),CTPRS_NPADS),(MAP_TPRS(1,7),            21
     *MTPRS_NPADS)                                                        21
      CHARACTER CTPRS_NPADS*40/' '/                                       21
      CHARACTER NTPRS_NPADS*12/'R.NPADS'/                                 21
      INTEGER MTPRS_NPADS(2)/40,1/                                        21
      REAL RE_TPRS_NPADS                                                  21
C -                                                                       21
      INTEGER LEN_TPRS,LLEN_TPRS,LINK_TPRS                                21
      PARAMETER (LEN_TPRS=87)                                             21
      PARAMETER (LLEN_TPRS=7)                                             21
      INTEGER FLAG_TPRS/0/,MAP_TPRS(2,LLEN_TPRS),IDNUM_TPRS(10)           21
      CHARACTER NAMES_TPRS(LLEN_TPRS)*12,COMMEN_TPRS(LLEN_TPRS)*40        21
      DATA IDNUM_TPRS/10*0/                                               21
      REAL BANK_TPRS(LEN_TPRS)                                            21
      CHARACTER*60 BPATH_TPRS/' '/                                        21
      SAVE FLAG_TPRS,MAP_TPRS,IDNUM_TPRS,NAMES_TPRS,COMMEN_TPRS,          21
     *BPATH_TPRS                                                          21
      DATA LINK_TPRS/0/,BANK_TPRS/LEN_TPRS*0/                             21
C *                                                                       21
C -                                                                       22
      REAL TRDO_RDOVTHK                                                   22
      EQUIVALENCE (BANK_TRDO(3),TRDO_RDOVTHK,RE_TRDO_RDOVTHK),            22
     *(NAMES_TRDO(1),NTRDO_RDOVTHK)                                       22
      EQUIVALENCE (COMMEN_TRDO(1),CTRDO_RDOVTHK),(MAP_TRDO(1,1),          22
     *MTRDO_RDOVTHK)                                                      22
      CHARACTER CTRDO_RDOVTHK*40/' '/                                     22
      CHARACTER NTRDO_RDOVTHK*12/'R.RDOVTHK'/                             22
      INTEGER MTRDO_RDOVTHK(2)/1,1/                                       22
      REAL RE_TRDO_RDOVTHK                                                22
C -                                                                       22
      REAL TRDO_RDOTHK                                                    22
      EQUIVALENCE (BANK_TRDO(4),TRDO_RDOTHK,RE_TRDO_RDOTHK),              22
     *(NAMES_TRDO(2),NTRDO_RDOTHK)                                        22
      EQUIVALENCE (COMMEN_TRDO(2),CTRDO_RDOTHK),(MAP_TRDO(1,2),           22
     *MTRDO_RDOTHK)                                                       22
      CHARACTER CTRDO_RDOTHK*40/' '/                                      22
      CHARACTER NTRDO_RDOTHK*12/'R.RDOTHK'/                               22
      INTEGER MTRDO_RDOTHK(2)/1,1/                                        22
      REAL RE_TRDO_RDOTHK                                                 22
C -                                                                       22
      REAL TRDO_RDOLEN                                                    22
      EQUIVALENCE (BANK_TRDO(5),TRDO_RDOLEN,RE_TRDO_RDOLEN),              22
     *(NAMES_TRDO(3),NTRDO_RDOLEN)                                        22
      EQUIVALENCE (COMMEN_TRDO(3),CTRDO_RDOLEN),(MAP_TRDO(1,3),           22
     *MTRDO_RDOLEN)                                                       22
      CHARACTER CTRDO_RDOLEN*40/' '/                                      22
      CHARACTER NTRDO_RDOLEN*12/'R.RDOLEN'/                               22
      INTEGER MTRDO_RDOLEN(2)/1,1/                                        22
      REAL RE_TRDO_RDOLEN                                                 22
C -                                                                       22
      REAL TRDO_NRDOBRD                                                   22
      EQUIVALENCE (BANK_TRDO(6),TRDO_NRDOBRD,RE_TRDO_NRDOBRD),            22
     *(NAMES_TRDO(4),NTRDO_NRDOBRD)                                       22
      EQUIVALENCE (COMMEN_TRDO(4),CTRDO_NRDOBRD),(MAP_TRDO(1,4),          22
     *MTRDO_NRDOBRD)                                                      22
      CHARACTER CTRDO_NRDOBRD*40/' '/                                     22
      CHARACTER NTRDO_NRDOBRD*12/'R.NRDOBRD'/                             22
      INTEGER MTRDO_NRDOBRD(2)/1,1/                                       22
      REAL RE_TRDO_NRDOBRD                                                22
C -                                                                       22
      REAL TRDO_RDOHT(9)                                                  22
      EQUIVALENCE (BANK_TRDO(7),TRDO_RDOHT,RE_TRDO_RDOHT),                22
     *(NAMES_TRDO(5),NTRDO_RDOHT)                                         22
      EQUIVALENCE (COMMEN_TRDO(5),CTRDO_RDOHT),(MAP_TRDO(1,5),            22
     *MTRDO_RDOHT)                                                        22
      CHARACTER CTRDO_RDOHT*40/' '/                                       22
      CHARACTER NTRDO_RDOHT*12/'R.RDOHT'/                                 22
      INTEGER MTRDO_RDOHT(2)/9,1/                                         22
      REAL RE_TRDO_RDOHT                                                  22
C -                                                                       22
      INTEGER LEN_TRDO,LLEN_TRDO,LINK_TRDO                                22
      PARAMETER (LEN_TRDO=15)                                             22
      PARAMETER (LLEN_TRDO=5)                                             22
      INTEGER FLAG_TRDO/0/,MAP_TRDO(2,LLEN_TRDO),IDNUM_TRDO(10)           22
      CHARACTER NAMES_TRDO(LLEN_TRDO)*12,COMMEN_TRDO(LLEN_TRDO)*40        22
      DATA IDNUM_TRDO/10*0/                                               22
      REAL BANK_TRDO(LEN_TRDO)                                            22
      CHARACTER*60 BPATH_TRDO/' '/                                        22
      SAVE FLAG_TRDO,MAP_TRDO,IDNUM_TRDO,NAMES_TRDO,COMMEN_TRDO,          22
     *BPATH_TRDO                                                          22
      DATA LINK_TRDO/0/,BANK_TRDO/LEN_TRDO*0/                             22
C *                                                                       22
      REAL TOCSIR,TOCSOR,TOKAIR,TOKAOR,TONXIR,TONXOR,TOADIR,TOADOR,       26
     *TOIGIR,TOIGOR, TOALIR,TOALOR,TOHAIR,TOHAOR,TOFSIR,TOFSOR,TOFCIR,    26
     *TOFCOR,TIADIR,TIADOR,TINXIR,TINXOR, TIKAIR,TIKAOR,TIALIR,TIALOR,    26
     *TIFCIR,TIFCOR,TPGVIR,TPGVLENG,TOFCLENG,TPCWZ,TPGVZ, TPEAZ,TPRDOZ,   26
     *DX_DZ,DXB,DEL,A,B,RADOFF,ZVALUE                                     26
      REAL PIPEOFF                                                        28
      REAL ZTEMP,XTEMP,HPLA,XBOTTOM                                       30
      REAL MNTCYLID,MNTCYLOD,MNTCYLHT,XLIP                                32
      REAL T,P,PATM,RHO,DENSITY                                           34
      INTEGER I_ROW,I_SEC,I,JJ,KK,I_NHP,J_NHO                             36
      EXTERNAL TPADSTEP                                                   38
      EXTERNAL TPAISTEP                                                   39
      EXTERNAL TPAOSTEP                                                   40
      EXTERNAL TPCELASER                                                  41
C *                                                                       42
      CALL AGSBEGM(MODULE,IPRIN)                                          42
      IF (IPRIN.LT.0) RETURN                                              42
C Filling bank TPCG - TPC BASIC DIMENSIONS                                42
      BANK_TITLE='TPC BASIC DIMENSIONS'                                   42
      BPATH_TPCG='TPCG*'                                                  42
      IDNUM_TPCG(1)=0                                                     43
C CURRENT VERSION                                                         43
      TPCG_VERSION=2                                                      43
      CTPCG_VERSION='CURRENT VERSION'                                     43
C TPC INNER RADIUS                                                        44
      TPCG_RMIN=46.107                                                    44
      CTPCG_RMIN='TPC INNER RADIUS'                                       44
C TPC OUTER RADIUS                                                        45
      TPCG_RMAX=207.750                                                   45
      CTPCG_RMAX='TPC OUTER RADIUS'                                       45
C TPC FULL LENGTH                                                         46
      TPCG_LENGTH=519.37                                                  46
      CTPCG_LENGTH='TPC FULL LENGTH'                                      46
C SUPPORT WHEEL INNER RADIUS                                              47
      TPCG_WHEELIR=49.60                                                  47
      CTPCG_WHEELIR='SUPPORT WHEEL INNER RADIUS'                          47
C SUPPORT WHEEL OUTER RADIUS                                              48
      TPCG_WHEELOR=206.75                                                 48
      CTPCG_WHEELOR='SUPPORT WHEEL OUTER RADIUS'                          48
C SUPPORT WHEEL LENGTH                                                    49
      TPCG_WHEELTHK=11.43                                                 49
      CTPCG_WHEELTHK='SUPPORT WHEEL LENGTH'                               49
C TPC SENSITIVE GAS OUTER RADIUS                                          50
      TPCG_SENGASOR=200                                                   50
      CTPCG_SENGASOR='TPC SENSITIVE GAS OUTER RADIUS'                     50
C ENDCAP MWC + SECTOR THICKNESS                                           51
      TPCG_TPEATHK=9.7                                                    51
      CTPCG_TPEATHK='ENDCAP MWC + SECTOR THICKNESS'                       51
C CENTRAL MEMBRANE THICKNESS                                              52
      TPCG_MEMBTHK=.00762                                                 52
      CTPCG_MEMBTHK='CENTRAL MEMBRANE THICKNESS'                          52
C OUTER COPPER THICKNESS                                                  53
      TPCG_TOCSDR=.013                                                    53
      CTPCG_TOCSDR='OUTER COPPER THICKNESS'                               53
C OUTER KAPTON THICKNESS                                                  54
      TPCG_TOKADR=.015                                                    54
      CTPCG_TOKADR='OUTER KAPTON THICKNESS'                               54
C OUTER NOMEX THICKNESS                                                   55
      TPCG_TONXDR=1.00                                                    55
      CTPCG_TONXDR='OUTER NOMEX THICKNESS'                                55
C OUTER ADHESIVE THICKNESS                                                56
      TPCG_TOADDR=0.02                                                    56
      CTPCG_TOADDR='OUTER ADHESIVE THICKNESS'                             56
C OUTER ISOLATING GAS THICKNESS                                           57
      TPCG_TOIGDR=5.70                                                    57
      CTPCG_TOIGDR='OUTER ISOLATING GAS THICKNESS'                        57
C OUTER ALUMINUM THICKNESS                                                58
      TPCG_TOALDR=0.40                                                    58
      CTPCG_TOALDR='OUTER ALUMINUM THICKNESS'                             58
C OUTER HONEYUCOMB AL THICKNESS                                           59
      TPCG_TOHADR=0.60                                                    59
      CTPCG_TOHADR='OUTER HONEYUCOMB AL THICKNESS'                        59
C INNER ADHESIVE LAYER THICKNESS                                          60
      TPCG_TIADDR=0.080                                                   60
      CTPCG_TIADDR='INNER ADHESIVE LAYER THICKNESS'                       60
C INNER NOMEX STRUCTURE THICKNESS                                         61
      TPCG_TINXDR=1.270                                                   61
      CTPCG_TINXDR='INNER NOMEX STRUCTURE THICKNESS'                      61
C INNER KAPTON LAYER THICKNESS                                            62
      TPCG_TIKADR=0.015                                                   62
      CTPCG_TIKADR='INNER KAPTON LAYER THICKNESS'                         62
C INNER ALUMINUM LAYER THICKNESS                                          63
      TPCG_TIALDR=0.004                                                   63
      CTPCG_TIALDR='INNER ALUMINUM LAYER THICKNESS'                       63
C MWC READOUT FLAG                                                        64
      TPCG_MWCREAD=2                                                      64
      CTPCG_MWCREAD='MWC READOUT FLAG'                                    64
C GAS DENSITY CORRECTION VERSION                                          65
      TPCG_GASCORR=1                                                      65
      CTPCG_GASCORR='GAS DENSITY CORRECTION VERSION'                      65
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCG',    66
     *BPATH_TPCG,IDNUM_TPCG,1, MAP_TPCG,NAMES_TPCG,COMMEN_TPCG,           66
     *BANK_TPCG,LEN_TPCG,LLEN_TPCG,LINK_TPCG,FLAG_TPCG)                   66
C *                                                                       66
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCG',    66
     *BPATH_TPCG,IDNUM_TPCG,1, MAP_TPCG,NAMES_TPCG,COMMEN_TPCG,           66
     *BANK_TPCG,LEN_TPCG,LLEN_TPCG,LINK_TPCG,FLAG_TPCG)                   66
C *                                                                       67
      CALL AGSBEGM(MODULE,IPRIN)                                          67
      IF (IPRIN.LT.0) RETURN                                              67
C Filling bank TRDO - VOLUME FOR TPC READOUT BOARDS                       67
      BANK_TITLE='VOLUME FOR TPC READOUT BOARDS'                          67
      BPATH_TRDO='TRDO*'                                                  67
      IDNUM_TRDO(1)=0                                                     68
C LENGTH OF RDO VLOUME                                                    68
      TRDO_RDOVTHK=30.                                                    68
      CTRDO_RDOVTHK='LENGTH OF RDO VLOUME'                                68
C THICKNESS OF RDO CARD                                                   69
      TRDO_RDOTHK=.25                                                     69
      CTRDO_RDOTHK='THICKNESS OF RDO CARD'                                69
C CARD LENGTH ALONG BEAM DIRECTION                                        70
      TRDO_RDOLEN=27                                                      70
      CTRDO_RDOLEN='CARD LENGTH ALONG BEAM DIRECTION'                     70
C NUMBER OF RDO BOARDS                                                    71
      TRDO_NRDOBRD=9                                                      71
      CTRDO_NRDOBRD='NUMBER OF RDO BOARDS'                                71
C RADIAL POS OF RDO BOARDS                                                73
      TRDO_RDOHT(1)=60.0                                                  73
      TRDO_RDOHT(2)=74.0                                                  73
      TRDO_RDOHT(3)=84.0                                                  73
      TRDO_RDOHT(4)=101.0                                                 73
      TRDO_RDOHT(5)=106.0                                                 73
      TRDO_RDOHT(6)=126.0                                                 73
      TRDO_RDOHT(7)=146.0                                                 73
      TRDO_RDOHT(8)=166.0                                                 73
      TRDO_RDOHT(9)=186.0                                                 73
      CTRDO_RDOHT='RADIAL POS OF RDO BOARDS'                              73
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TRDO',    74
     *BPATH_TRDO,IDNUM_TRDO,1, MAP_TRDO,NAMES_TRDO,COMMEN_TRDO,           74
     *BANK_TRDO,LEN_TRDO,LLEN_TRDO,LINK_TRDO,FLAG_TRDO)                   74
C *                                                                       74
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TRDO',    74
     *BPATH_TRDO,IDNUM_TRDO,1, MAP_TRDO,NAMES_TRDO,COMMEN_TRDO,           74
     *BANK_TRDO,LEN_TRDO,LLEN_TRDO,LINK_TRDO,FLAG_TRDO)                   74
C *                                                                       75
      CALL AGSBEGM(MODULE,IPRIN)                                          75
      IF (IPRIN.LT.0) RETURN                                              75
C Filling bank TPRS - SECTOR OF PADROWS                                   75
      BANK_TITLE='SECTOR OF PADROWS'                                      75
      BPATH_TPRS='TPRS*'                                                  75
      IDNUM_TPRS(1)=0                                                     76
C SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER                                 76
      TPRS_SEC=1                                                          76
      CTPRS_SEC='SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER'                 76
C NUMBER OF PADROWS IN THE SECTOR                                         77
      TPRS_NROW=13                                                        77
      CTPRS_NROW='NUMBER OF PADROWS IN THE SECTOR'                        77
C TPC PADROW PITCH WIDTH                                                  78
      TPRS_PITCH=0.335                                                    78
      CTPRS_PITCH='TPC PADROW PITCH WIDTH'                                78
C TPC PADROW THICKNESS                                                    79
      TPRS_WIDTH=1.15                                                     79
      CTPRS_WIDTH='TPC PADROW THICKNESS'                                  79
C NUMBER OF PADRAWS IN A SUPERPADROW                                      80
      TPRS_SUPER=3                                                        80
      CTPRS_SUPER='NUMBER OF PADRAWS IN A SUPERPADROW'                    80
C NUMBER OF PADS IN ROW                                                   82
      TPRS_NPADS(1)=88                                                    82
      TPRS_NPADS(2)=96                                                    82
      TPRS_NPADS(3)=104                                                   82
      TPRS_NPADS(4)=112                                                   82
      TPRS_NPADS(5)=118                                                   82
      TPRS_NPADS(6)=126                                                   82
      TPRS_NPADS(7)=134                                                   82
      TPRS_NPADS(8)=142                                                   82
      TPRS_NPADS(9)=150                                                   82
      TPRS_NPADS(10)=158                                                  82
      TPRS_NPADS(11)=166                                                  82
      TPRS_NPADS(12)=174                                                  82
      TPRS_NPADS(13)=182                                                  82
      CTPRS_NPADS='NUMBER OF PADS IN ROW'                                 82
C TPC PADROW RADII                                                        85
      TPRS_RPADS(1)=60.0                                                  85
      TPRS_RPADS(2)=64.8                                                  85
      TPRS_RPADS(3)=69.6                                                  85
      TPRS_RPADS(4)=74.4                                                  85
      TPRS_RPADS(5)=79.2                                                  85
      TPRS_RPADS(6)=84.0                                                  85
      TPRS_RPADS(7)=88.8                                                  85
      TPRS_RPADS(8)=93.6                                                  85
      TPRS_RPADS(9)=98.8                                                  85
      TPRS_RPADS(10)=104.0                                                85
      TPRS_RPADS(11)=109.2                                                85
      TPRS_RPADS(12)=114.4                                                85
      TPRS_RPADS(13)=119.6                                                85
      CTPRS_RPADS='TPC PADROW RADII'                                      85
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPRS',    86
     *BPATH_TPRS,IDNUM_TPRS,1, MAP_TPRS,NAMES_TPRS,COMMEN_TPRS,           86
     *BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)                   86
C *                                                                       86
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPRS',    86
     *BPATH_TPRS,IDNUM_TPRS,1, MAP_TPRS,NAMES_TPRS,COMMEN_TPRS,           86
     *BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)                   86
C *                                                                       87
      CALL AGSBEGM(MODULE,IPRIN)                                          87
      IF (IPRIN.LT.0) RETURN                                              87
C Filling bank TPRS - SECTOR OF PADROWS                                   87
      BANK_TITLE='SECTOR OF PADROWS'                                      87
      BPATH_TPRS='TPRS*'                                                  87
      IDNUM_TPRS(1)=0                                                     88
C SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER                                 88
      TPRS_SEC=2                                                          88
      CTPRS_SEC='SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER'                 88
C NUMBER OF PADROWS IN OUTER SECTOR                                       89
      TPRS_NROW=32                                                        89
      CTPRS_NROW='NUMBER OF PADROWS IN OUTER SECTOR'                      89
C OUTER TPC PADROW PITCH WIDTH                                            90
      TPRS_PITCH=0.67                                                     90
      CTPRS_PITCH='OUTER TPC PADROW PITCH WIDTH'                          90
C OUTER TPC PADROW THICKNESS                                              91
      TPRS_WIDTH=1.95                                                     91
      CTPRS_WIDTH='OUTER TPC PADROW THICKNESS'                            91
C NUMBER OF PADROWS IN A SUPERPADROW                                      92
      TPRS_SUPER=1                                                        92
      CTPRS_SUPER='NUMBER OF PADROWS IN A SUPERPADROW'                    92
C NUMBER OF PADS IN ROW                                                   96
      TPRS_NPADS(1)=98                                                    96
      TPRS_NPADS(2)=100                                                   96
      TPRS_NPADS(3)=102                                                   96
      TPRS_NPADS(4)=104                                                   96
      TPRS_NPADS(5)=106                                                   96
      TPRS_NPADS(6)=106                                                   96
      TPRS_NPADS(7)=108                                                   96
      TPRS_NPADS(8)=110                                                   96
      TPRS_NPADS(9)=112                                                   96
      TPRS_NPADS(10)=112                                                  96
      TPRS_NPADS(11)=114                                                  96
      TPRS_NPADS(12)=116                                                  96
      TPRS_NPADS(13)=118                                                  96
      TPRS_NPADS(14)=120                                                  96
      TPRS_NPADS(15)=122                                                  96
      TPRS_NPADS(16)=122                                                  96
      TPRS_NPADS(17)=124                                                  96
      TPRS_NPADS(18)=126                                                  96
      TPRS_NPADS(19)=128                                                  96
      TPRS_NPADS(20)=128                                                  96
      TPRS_NPADS(21)=130                                                  96
      TPRS_NPADS(22)=132                                                  96
      TPRS_NPADS(23)=134                                                  96
      TPRS_NPADS(24)=136                                                  96
      TPRS_NPADS(25)=138                                                  96
      TPRS_NPADS(26)=138                                                  96
      TPRS_NPADS(27)=140                                                  96
      TPRS_NPADS(28)=142                                                  96
      TPRS_NPADS(29)=144                                                  96
      TPRS_NPADS(30)=144                                                  96
      TPRS_NPADS(31)=144                                                  96
      TPRS_NPADS(32)=144                                                  96
      CTPRS_NPADS='NUMBER OF PADS IN ROW'                                 96
C TPC PADROW RADII                                                        104
      TPRS_RPADS(1)=127.195                                               104
      TPRS_RPADS(2)=129.195                                               104
      TPRS_RPADS(3)=131.195                                               104
      TPRS_RPADS(4)=133.195                                               104
      TPRS_RPADS(5)=135.195                                               104
      TPRS_RPADS(6)=137.195                                               104
      TPRS_RPADS(7)=139.195                                               104
      TPRS_RPADS(8)=141.195                                               104
      TPRS_RPADS(9)=143.195                                               104
      TPRS_RPADS(10)=145.195                                              104
      TPRS_RPADS(11)=147.195                                              104
      TPRS_RPADS(12)=149.195                                              104
      TPRS_RPADS(13)=151.195                                              104
      TPRS_RPADS(14)=153.195                                              104
      TPRS_RPADS(15)=155.195                                              104
      TPRS_RPADS(16)=157.195                                              104
      TPRS_RPADS(17)=159.195                                              104
      TPRS_RPADS(18)=161.195                                              104
      TPRS_RPADS(19)=163.195                                              104
      TPRS_RPADS(20)=165.195                                              104
      TPRS_RPADS(21)=167.195                                              104
      TPRS_RPADS(22)=169.195                                              104
      TPRS_RPADS(23)=171.195                                              104
      TPRS_RPADS(24)=173.195                                              104
      TPRS_RPADS(25)=175.195                                              104
      TPRS_RPADS(26)=177.195                                              104
      TPRS_RPADS(27)=179.195                                              104
      TPRS_RPADS(28)=181.195                                              104
      TPRS_RPADS(29)=183.195                                              104
      TPRS_RPADS(30)=185.195                                              104
      TPRS_RPADS(31)=187.195                                              104
      TPRS_RPADS(32)=189.195                                              104
      CTPRS_RPADS='TPC PADROW RADII'                                      104
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPRS',    105
     *BPATH_TPRS,IDNUM_TPRS,1, MAP_TPRS,NAMES_TPRS,COMMEN_TPRS,           105
     *BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)                   105
C *                                                                       105
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPRS',    105
     *BPATH_TPRS,IDNUM_TPRS,1, MAP_TPRS,NAMES_TPRS,COMMEN_TPRS,           105
     *BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)                   105
C *                                                                       106
      CALL AGSBEGM(MODULE,IPRIN)                                          106
      IF (IPRIN.LT.0) RETURN                                              106
C Filling bank TECW - EC TRAPEZOID AND SUPPORT WHEEL                      106
      BANK_TITLE='EC TRAPEZOID AND SUPPORT WHEEL'                         106
      BPATH_TECW='TECW*'                                                  106
      IDNUM_TECW(1)=0                                                     107
C SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER                                 107
      TECW_SEC=1                                                          107
      CTECW_SEC='SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER'                 107
C AIR IN SUPPORT WHEEL - INNER WIDTH                                      108
      TECW_GAPWIDI=2.* 10.91                                              108
      CTECW_GAPWIDI='AIR IN SUPPORT WHEEL - INNER WIDTH'                  108
C AIR IN SUPPORT WHEEL - OUTER WIDTH                                      109
      TECW_GAPWIDO=2.* 27.56                                              109
      CTECW_GAPWIDO='AIR IN SUPPORT WHEEL - OUTER WIDTH'                  109
C AIR IN SUPPORT WHEEL - HEIGHT (DR)                                      110
      TECW_GAPHEIT=62.15                                                  110
      CTECW_GAPHEIT='AIR IN SUPPORT WHEEL - HEIGHT (DR)'                  110
C AIR IN SUPPORT WHEEL - CENTER RADIUS                                    111
      TECW_GAPRAD=87.0                                                    111
      CTECW_GAPRAD='AIR IN SUPPORT WHEEL - CENTER RADIUS'                 111
C OFFSET AIR GAP TO MAKE LIP                                              112
      TECW_GAPSHFT=1.4                                                    112
      CTECW_GAPSHFT='OFFSET AIR GAP TO MAKE LIP'                          112
C SECTOR WIDTH AT INNER RADIUS                                            113
      TECW_INWIDTH=2.* 13.75                                              113
      CTECW_INWIDTH='SECTOR WIDTH AT INNER RADIUS'                        113
C SECTOR WIDTH AT OUTER RADIUS                                            114
      TECW_OUWIDTH=2.* 32.38                                              114
      CTECW_OUWIDTH='SECTOR WIDTH AT OUTER RADIUS'                        114
C SECTOR RADIAL HEIGHT                                                    115
      TECW_HEIGHT=69.52                                                   115
      CTECW_HEIGHT='SECTOR RADIAL HEIGHT'                                 115
C PADPLANE THICKNESS (BOTH AL AND PCB)                                    116
      TECW_PPDEPTH=1.35+0.32                                              116
      CTECW_PPDEPTH='PADPLANE THICKNESS (BOTH AL AND PCB)'                116
C DEPTH OF OPENINGS IN ALUMINUM STRUCTURE                                 117
      TECW_ASDEPTH=6.99                                                   117
      CTECW_ASDEPTH='DEPTH OF OPENINGS IN ALUMINUM STRUCTURE'             117
C MWC GAP FROM GATED GRID TO PAD PLANE                                    118
      TECW_GGDEPTH=1.0                                                    118
      CTECW_GGDEPTH='MWC GAP FROM GATED GRID TO PAD PLANE'                118
C SENSITIVE MWC GAS GAP FULL THICKNESS                                    119
      TECW_MWCDEPTH=1.0                                                   119
      CTECW_MWCDEPTH='SENSITIVE MWC GAS GAP FULL THICKNESS'               119
C AL FRAME - BOUNDARY WIDTH                                               120
      TECW_BOUNDARY=3.74                                                  120
      CTECW_BOUNDARY='AL FRAME - BOUNDARY WIDTH'                          120
C SECTOR CENTER RADIUS (SET BY PRECISION HOLES)                           121
      TECW_RCENTER=86.669                                                 121
      CTECW_RCENTER='SECTOR CENTER RADIUS (SET BY PRECISION HOLES)'       121
C MWC SENSITIVE REGION INNER SIZE                                         122
      TECW_MWCINN=2*12.804                                                122
      CTECW_MWCINN='MWC SENSITIVE REGION INNER SIZE'                      122
C MWC SENSITIVE REGION OUTER SIZE                                         123
      TECW_MWCOUT=2*29.953                                                123
      CTECW_MWCOUT='MWC SENSITIVE REGION OUTER SIZE'                      123
C MWC SENSITIVE REGION HEIGHT (RADIAL)                                    124
      TECW_MWCHEI=64                                                      124
      CTECW_MWCHEI='MWC SENSITIVE REGION HEIGHT (RADIAL)'                 124
C SENSITIVE REGION CENTER POSITION                                        125
      TECW_MWCCENT=-1.669                                                 125
      CTECW_MWCCENT='SENSITIVE REGION CENTER POSITION'                    125
C NUMBER OF MWC SENSITIVE WIRES                                           126
      TECW_MWCNWIR=160                                                    126
      CTECW_MWCNWIR='NUMBER OF MWC SENSITIVE WIRES'                       126
C NUMBER OF AIR GAPS IN AL                                                127
      TECW_N=5                                                            127
      CTECW_N='NUMBER OF AIR GAPS IN AL'                                  127
C NUMBER OF EXTRA ALUMINUM SUPPORT PIECES                                 128
      TECW_NEX=0                                                          128
      CTECW_NEX='NUMBER OF EXTRA ALUMINUM SUPPORT PIECES'                 128
C POSITIONS OF AIR GAPS                                                   129
      TECW_Z(1)=6.67                                                      129
      TECW_Z(2)=18.22                                                     129
      TECW_Z(3)=34.22                                                     129
      TECW_Z(4)=50.22                                                     129
      TECW_Z(5)=62.65                                                     129
      CTECW_Z='POSITIONS OF AIR GAPS'                                     129
C SIZE OF AIR GAPS                                                        130
      TECW_DZ(1)=6.13                                                     130
      TECW_DZ(2)=15.05                                                    130
      TECW_DZ(3)=15.05                                                    130
      TECW_DZ(4)=15.05                                                    130
      TECW_DZ(5)=7.09                                                     130
      CTECW_DZ='SIZE OF AIR GAPS'                                         130
C X POSITIONS OF EXTRA ALUMINUM                                           131
      TECW_XEX(1)=0                                                       131
      TECW_XEX(2)=0                                                       131
      TECW_XEX(3)=0                                                       131
      TECW_XEX(4)=0                                                       131
      TECW_XEX(5)=0                                                       131
      CTECW_XEX='X POSITIONS OF EXTRA ALUMINUM'                           131
C Z POSITIONS OF EXTRA ALUMINUM                                           132
      TECW_ZEX(1)=0                                                       132
      TECW_ZEX(2)=0                                                       132
      TECW_ZEX(3)=0                                                       132
      TECW_ZEX(4)=0                                                       132
      TECW_ZEX(5)=0                                                       132
      CTECW_ZEX='Z POSITIONS OF EXTRA ALUMINUM'                           132
C X-THICKNESS OF EXTRA ALUMINUM                                           133
      TECW_DXEX(1)=0                                                      133
      TECW_DXEX(2)=0                                                      133
      TECW_DXEX(3)=0                                                      133
      TECW_DXEX(4)=0                                                      133
      TECW_DXEX(5)=0                                                      133
      CTECW_DXEX='X-THICKNESS OF EXTRA ALUMINUM'                          133
C Z-THICKNESS OF EXTRA ALUMINUM                                           134
      TECW_DZEX(1)=0                                                      134
      TECW_DZEX(2)=0                                                      134
      TECW_DZEX(3)=0                                                      134
      TECW_DZEX(4)=0                                                      134
      TECW_DZEX(5)=0                                                      134
      CTECW_DZEX='Z-THICKNESS OF EXTRA ALUMINUM'                          134
C NUMBER OF PC PLANES PER AIR GAP                                         135
      TECW_NHPLANE(1)=1                                                   135
      TECW_NHPLANE(2)=4                                                   135
      TECW_NHPLANE(3)=4                                                   135
      TECW_NHPLANE(4)=4                                                   135
      TECW_NHPLANE(5)=2                                                   135
      CTECW_NHPLANE='NUMBER OF PC PLANES PER AIR GAP'                     135
C WHEEL BOLT LIP ALONG BEAM                                               136
      TECW_WHLIPTHK=1.0                                                   136
      CTECW_WHLIPTHK='WHEEL BOLT LIP ALONG BEAM'                          136
C WHEEL LIP IN PHI                                                        137
      TECW_WHLIPWID=2.8                                                   137
      CTECW_WHLIPWID='WHEEL LIP IN PHI'                                   137
C BLOCKS ON WHEEL RADIAL SIZE                                             138
      TECW_WHBLKLEN=5.5                                                   138
      CTECW_WHBLKLEN='BLOCKS ON WHEEL RADIAL SIZE'                        138
C BOLT BLOCKS AT +- 18 IN WHL                                             139
      TECW_WHBLKPOS=18.0                                                  139
      CTECW_WHBLKPOS='BOLT BLOCKS AT +- 18 IN WHL'                        139
C NO BLOCK BOTTOM INNER WHEEL                                             140
      TECW_WHBLKIN=0                                                      140
      CTECW_WHBLKIN='NO BLOCK BOTTOM INNER WHEEL'                         140
C PC CARD WIDTH AND THICKNESS                                             141
      TECW_CARDW=7.33                                                     141
      CTECW_CARDW='PC CARD WIDTH AND THICKNESS'                           141
C PC CARD WIDTH AND THICKNESS                                             142
      TECW_CARDTH=0.19                                                    142
      CTECW_CARDTH='PC CARD WIDTH AND THICKNESS'                          142
C COOLING AL WIDTH AND THICKNESS                                          143
      TECW_COOLW=5.7                                                      143
      CTECW_COOLW='COOLING AL WIDTH AND THICKNESS'                        143
C COOLING AL WIDTH AND THICKNESS                                          144
      TECW_COOLTH=0.33                                                    144
      CTECW_COOLTH='COOLING AL WIDTH AND THICKNESS'                       144
C CARD AND COOLING OFFSETS FROM                                           145
      TECW_CARDOFF=1.77                                                   145
      CTECW_CARDOFF='CARD AND COOLING OFFSETS FROM'                       145
C CARD AND COOLING OFFSETS FROM                                           146
      TECW_COOLOFF=1.50                                                   146
      CTECW_COOLOFF='CARD AND COOLING OFFSETS FROM'                       146
C SLOT WIDTH AND RADIUS                                                   147
      TECW_SLOTW=4.70                                                     147
      CTECW_SLOTW='SLOT WIDTH AND RADIUS'                                 147
C SLOT WIDTH AND RADIUS                                                   148
      TECW_SLOTRAD=0.95                                                   148
      CTECW_SLOTRAD='SLOT WIDTH AND RADIUS'                               148
C WATER PIPE ALONG BEAM DIR                                               149
      TECW_PIPETHK=1.26                                                   149
      CTECW_PIPETHK='WATER PIPE ALONG BEAM DIR'                           149
C WATER PIPE RADIAL                                                       150
      TECW_PIPEHT=1.9                                                     150
      CTECW_PIPEHT='WATER PIPE RADIAL'                                    150
C WATER MANIFOLD ALONG BEAM                                               151
      TECW_MANITHK=4.5                                                    151
      CTECW_MANITHK='WATER MANIFOLD ALONG BEAM'                           151
C WATER MANIFOLD IN PHI                                                   152
      TECW_MANIWID=2.2                                                    152
      CTECW_MANIWID='WATER MANIFOLD IN PHI'                               152
C TANGENT OF 15 DEG                                                       153
      TECW_TAN15=.26795                                                   153
      CTECW_TAN15='TANGENT OF 15 DEG'                                     153
C CLEARANCE FOR RDO BOARDSIN PHI                                          154
      TECW_CLEARANCE=1.0                                                  154
      CTECW_CLEARANCE='CLEARANCE FOR RDO BOARDSIN PHI'                    154
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TECW',    155
     *BPATH_TECW,IDNUM_TECW,1, MAP_TECW,NAMES_TECW,COMMEN_TECW,           155
     *BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)                   155
C *                                                                       155
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TECW',    155
     *BPATH_TECW,IDNUM_TECW,1, MAP_TECW,NAMES_TECW,COMMEN_TECW,           155
     *BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)                   155
C *                                                                       156
      CALL AGSBEGM(MODULE,IPRIN)                                          156
      IF (IPRIN.LT.0) RETURN                                              156
C Filling bank TECW - ENDCAP TRAPEZOID AND SUPPORT WHEEL                  156
      BANK_TITLE='ENDCAP TRAPEZOID AND SUPPORT WHEEL'                     156
      BPATH_TECW='TECW*'                                                  156
      IDNUM_TECW(1)=0                                                     157
C SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER                                 157
      TECW_SEC=2                                                          157
      CTECW_SEC='SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER'                 157
C AIR IN SUPPORT WHEEL - INNER WIDTH                                      158
      TECW_GAPWIDI=2.* 28.92                                              158
      CTECW_GAPWIDI='AIR IN SUPPORT WHEEL - INNER WIDTH'                  158
C AIR IN SUPPORT WHEEL - OUTER WIDTH                                      159
      TECW_GAPWIDO=2.* 46.74                                              159
      CTECW_GAPWIDO='AIR IN SUPPORT WHEEL - OUTER WIDTH'                  159
C AIR IN SUPPORT WHEEL - HEIGHT (DR)                                      160
      TECW_GAPHEIT=65.0                                                   160
      CTECW_GAPHEIT='AIR IN SUPPORT WHEEL - HEIGHT (DR)'                  160
C AIR IN SUPPORT WHEEL - RADIUS                                           161
      TECW_GAPRAD=158.0                                                   161
      CTECW_GAPRAD='AIR IN SUPPORT WHEEL - RADIUS'                        161
C OFFSET OF AIR GAP LIP                                                   162
      TECW_GAPSHFT=0.0                                                    162
      CTECW_GAPSHFT='OFFSET OF AIR GAP LIP'                               162
C SECTOR WIDTH AT INNER RADIUS                                            163
      TECW_INWIDTH=64.92                                                  163
      CTECW_INWIDTH='SECTOR WIDTH AT INNER RADIUS'                        163
C SECTOR WIDTH AT OUTER RADIUS                                            164
      TECW_OUWIDTH=103.25                                                 164
      CTECW_OUWIDTH='SECTOR WIDTH AT OUTER RADIUS'                        164
C SECTOR RADIAL HEIGHT                                                    165
      TECW_HEIGHT=71.51                                                   165
      CTECW_HEIGHT='SECTOR RADIAL HEIGHT'                                 165
C PADPLANE THICKNESS (BOTH AL AND PCB)                                    166
      TECW_PPDEPTH=0.95+0.32                                              166
      CTECW_PPDEPTH='PADPLANE THICKNESS (BOTH AL AND PCB)'                166
C DEPTH OF OPENINGS IN ALUMINUM STRUCTURE                                 167
      TECW_ASDEPTH=6.99                                                   167
      CTECW_ASDEPTH='DEPTH OF OPENINGS IN ALUMINUM STRUCTURE'             167
C MWC GAP FROM GATED GRID TO PAD PLANE                                    168
      TECW_GGDEPTH=1.4                                                    168
      CTECW_GGDEPTH='MWC GAP FROM GATED GRID TO PAD PLANE'                168
C SENSITIVE MWC GAS GAP FULL THICKNESS                                    169
      TECW_MWCDEPTH=1.4                                                   169
      CTECW_MWCDEPTH='SENSITIVE MWC GAS GAP FULL THICKNESS'               169
C AL FRAME - BOUNDARY WIDTH                                               170
      TECW_BOUNDARY=3.74                                                  170
      CTECW_BOUNDARY='AL FRAME - BOUNDARY WIDTH'                          170
C SECTOR CENTER RADIUS (SET BY PRECISION HOLES)                           171
      TECW_RCENTER=157.488                                                171
      CTECW_RCENTER='SECTOR CENTER RADIUS (SET BY PRECISION HOLES)'       171
C MWC SENSITIVE REGION INNER SIZE                                         172
      TECW_MWCINN=2*32.202                                                172
      CTECW_MWCINN='MWC SENSITIVE REGION INNER SIZE'                      172
C MWC SENSITIVE REGION OUTER SIZE                                         173
      TECW_MWCOUT=2*49.351                                                173
      CTECW_MWCOUT='MWC SENSITIVE REGION OUTER SIZE'                      173
C MWC SENSITIVE REGION HEIGHT (RADIAL)                                    174
      TECW_MWCHEI=64                                                      174
      CTECW_MWCHEI='MWC SENSITIVE REGION HEIGHT (RADIAL)'                 174
C SENSITIVE REGION CENTER POSITION                                        175
      TECW_MWCCENT=-0.093                                                 175
      CTECW_MWCCENT='SENSITIVE REGION CENTER POSITION'                    175
C NUMBER OF MWC SENSITIVE WIRES                                           176
      TECW_MWCNWIR=160                                                    176
      CTECW_MWCNWIR='NUMBER OF MWC SENSITIVE WIRES'                       176
C NUMBER OF AIR GAPS IN AL                                                177
      TECW_N=8                                                            177
      CTECW_N='NUMBER OF AIR GAPS IN AL'                                  177
C NUMBER OF EXTRA ALUMINUM SUPPORT PIECES                                 178
      TECW_NEX=5                                                          178
      CTECW_NEX='NUMBER OF EXTRA ALUMINUM SUPPORT PIECES'                 178
C OPENING POSITIONS                                                       180
      TECW_Z(1)=8.216                                                     180
      TECW_Z(2)=16.217                                                    180
      TECW_Z(3)=24.218                                                    180
      TECW_Z(4)=32.219                                                    180
      TECW_Z(5)=40.220                                                    180
      TECW_Z(6)=48.221                                                    180
      TECW_Z(7)=56.222                                                    180
      TECW_Z(8)=64.143                                                    180
      CTECW_Z='OPENING POSITIONS'                                         180
C OPENING SIZE                                                            182
      TECW_DZ(1)=7.05                                                     182
      TECW_DZ(2)=7.05                                                     182
      TECW_DZ(3)=7.05                                                     182
      TECW_DZ(4)=7.05                                                     182
      TECW_DZ(5)=7.05                                                     182
      TECW_DZ(6)=7.05                                                     182
      TECW_DZ(7)=7.05                                                     182
      TECW_DZ(8)=6.89                                                     182
      CTECW_DZ='OPENING SIZE'                                             182
C X POSITIONS                                                             183
      TECW_XEX(1)=0.                                                      183
      TECW_XEX(2)=0.                                                      183
      TECW_XEX(3)=0.                                                      183
      TECW_XEX(4)=-21.907                                                 183
      TECW_XEX(5)=21.907                                                  183
      CTECW_XEX='X POSITIONS'                                             183
C Z POSITIONS                                                             184
      TECW_ZEX(1)=6.69                                                    184
      TECW_ZEX(2)=32.22                                                   184
      TECW_ZEX(3)=40.22                                                   184
      TECW_ZEX(4)=56.22                                                   184
      TECW_ZEX(5)=56.22                                                   184
      CTECW_ZEX='Z POSITIONS'                                             184
C X-THICKNESS                                                             185
      TECW_DXEX(1)=2.54                                                   185
      TECW_DXEX(2)=0.476                                                  185
      TECW_DXEX(3)=0.476                                                  185
      TECW_DXEX(4)=3.175                                                  185
      TECW_DXEX(5)=3.175                                                  185
      CTECW_DXEX='X-THICKNESS'                                            185
C Z-THICKNESS                                                             186
      TECW_DZEX(1)=4.00                                                   186
      TECW_DZEX(2)=7.05                                                   186
      TECW_DZEX(3)=7.05                                                   186
      TECW_DZEX(4)=7.05                                                   186
      TECW_DZEX(5)=7.05                                                   186
      CTECW_DZEX='Z-THICKNESS'                                            186
C NUMBER OF PC PLANES PER AIR GAP                                         187
      TECW_NHPLANE(1)=2                                                   187
      TECW_NHPLANE(2)=2                                                   187
      TECW_NHPLANE(3)=2                                                   187
      TECW_NHPLANE(4)=2                                                   187
      TECW_NHPLANE(5)=2                                                   187
      TECW_NHPLANE(6)=2                                                   187
      TECW_NHPLANE(7)=2                                                   187
      TECW_NHPLANE(8)=2                                                   187
      CTECW_NHPLANE='NUMBER OF PC PLANES PER AIR GAP'                     187
C WHEEL BOLT LIP ALONG BEAM                                               188
      TECW_WHLIPTHK=1.0                                                   188
      CTECW_WHLIPTHK='WHEEL BOLT LIP ALONG BEAM'                          188
C WHEEL LIP IN PHI                                                        189
      TECW_WHLIPWID=2.8                                                   189
      CTECW_WHLIPWID='WHEEL LIP IN PHI'                                   189
C BLOCKS ON WHEEL RADIAL SIZE                                             190
      TECW_WHBLKLEN=5.5                                                   190
      CTECW_WHBLKLEN='BLOCKS ON WHEEL RADIAL SIZE'                        190
C BOLT BLOCKS AT +- 18 IN WHL                                             191
      TECW_WHBLKPOS=18.0                                                  191
      CTECW_WHBLKPOS='BOLT BLOCKS AT +- 18 IN WHL'                        191
C IS BLOCK BOTTOM OUTER WHEEL                                             192
      TECW_WHBLKIN=1                                                      192
      CTECW_WHBLKIN='IS BLOCK BOTTOM OUTER WHEEL'                         192
C PC CARD WIDTH AND THICKNESS                                             193
      TECW_CARDW=7.33                                                     193
      CTECW_CARDW='PC CARD WIDTH AND THICKNESS'                           193
C PC CARD WIDTH AND THICKNESS                                             194
      TECW_CARDTH=0.19                                                    194
      CTECW_CARDTH='PC CARD WIDTH AND THICKNESS'                          194
C COOLING AL WIDTH AND THICKNESS                                          195
      TECW_COOLW=5.7                                                      195
      CTECW_COOLW='COOLING AL WIDTH AND THICKNESS'                        195
C COOLING AL WIDTH AND THICKNESS                                          196
      TECW_COOLTH=0.33                                                    196
      CTECW_COOLTH='COOLING AL WIDTH AND THICKNESS'                       196
C CARD AND COOLING OFFSETS FROM                                           197
      TECW_CARDOFF=1.77                                                   197
      CTECW_CARDOFF='CARD AND COOLING OFFSETS FROM'                       197
C CARD AND COOLING OFFSETS FROM                                           198
      TECW_COOLOFF=1.50                                                   198
      CTECW_COOLOFF='CARD AND COOLING OFFSETS FROM'                       198
C SLOT WIDTH AND RADIUS                                                   199
      TECW_SLOTW=4.70                                                     199
      CTECW_SLOTW='SLOT WIDTH AND RADIUS'                                 199
C SLOT WIDTH AND RADIUS                                                   200
      TECW_SLOTRAD=0.95                                                   200
      CTECW_SLOTRAD='SLOT WIDTH AND RADIUS'                               200
C WATER PIPE ALONG BEAM DIR                                               201
      TECW_PIPETHK=1.26                                                   201
      CTECW_PIPETHK='WATER PIPE ALONG BEAM DIR'                           201
C WATER PIPE RADIAL                                                       202
      TECW_PIPEHT=1.9                                                     202
      CTECW_PIPEHT='WATER PIPE RADIAL'                                    202
C WATER MANIFOLD ALONG BEAM                                               203
      TECW_MANITHK=4.5                                                    203
      CTECW_MANITHK='WATER MANIFOLD ALONG BEAM'                           203
C WATER MANIFOLD IN PHI                                                   204
      TECW_MANIWID=2.2                                                    204
      CTECW_MANIWID='WATER MANIFOLD IN PHI'                               204
C TANGENT OF 15 DEG                                                       205
      TECW_TAN15=.26795                                                   205
      CTECW_TAN15='TANGENT OF 15 DEG'                                     205
C CLEARANCE FOR RDO BOARDSIN PHI                                          206
      TECW_CLEARANCE=1.0                                                  206
      CTECW_CLEARANCE='CLEARANCE FOR RDO BOARDSIN PHI'                    206
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TECW',    207
     *BPATH_TECW,IDNUM_TECW,1, MAP_TECW,NAMES_TECW,COMMEN_TECW,           207
     *BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)                   207
C *                                                                       207
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TECW',    207
     *BPATH_TECW,IDNUM_TECW,1, MAP_TECW,NAMES_TECW,COMMEN_TECW,           207
     *BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)                   207
C *                                                                       208
      CALL AGSBEGM(MODULE,IPRIN)                                          208
      IF (IPRIN.LT.0) RETURN                                              208
C Filling bank TROV - CARDS IN ROW 1                                      208
      BANK_TITLE='CARDS IN ROW 1'                                         208
      BPATH_TROV='TROV*'                                                  208
      IDNUM_TROV(1)=0                                                     209
C INNER                                                                   209
      TROV_SEC=1                                                          209
      CTROV_SEC='INNER'                                                   209
C NO OF HOLES/PLANE                                                       211
      TROV_NHP(1)=0                                                       211
      TROV_NHP(2)=2                                                       211
      TROV_NHP(3)=2                                                       211
      TROV_NHP(4)=3                                                       211
      TROV_NHP(5)=3                                                       211
      TROV_NHP(6)=3                                                       211
      TROV_NHP(7)=3                                                       211
      TROV_NHP(8)=4                                                       211
      TROV_NHP(9)=4                                                       211
      TROV_NHP(10)=4                                                      211
      TROV_NHP(11)=4                                                      211
      TROV_NHP(12)=5                                                      211
      TROV_NHP(13)=2                                                      211
      TROV_NHP(14)=5                                                      211
      TROV_NHP(15)=5                                                      211
      TROV_NHP(16)=6                                                      211
      CTROV_NHP='NO OF HOLES/PLANE'                                       211
      TROV_HX(1,1)=0.0                                                    214
      TROV_HX(2,1)=0.0                                                    214
      TROV_HX(3,1)=0.0                                                    214
      TROV_HX(4,1)=0.0                                                    214
      TROV_HX(5,1)=0.0                                                    214
      TROV_HX(6,1)=0.0                                                    214
      TROV_HX(7,1)=0.0                                                    214
      TROV_HX(8,1)=0.0                                                    214
      TROV_HX(9,1)=0.0                                                    214
      TROV_HX(1,2)=0.0                                                    214
      TROV_HX(2,2)=0.0                                                    216
      TROV_HX(3,2)=0.0                                                    216
      TROV_HX(4,2)=0.0                                                    216
      TROV_HX(5,2)=0.0                                                    216
      TROV_HX(6,2)=-5.17                                                  216
      TROV_HX(7,2)=5.17                                                   216
      TROV_HX(8,2)=0.0                                                    216
      TROV_HX(9,2)=0.0                                                    216
      TROV_HX(1,3)=-6.21                                                  216
      TROV_HX(2,3)=6.21                                                   218
      TROV_HX(3,3)=0.0                                                    218
      TROV_HX(4,3)=0.0                                                    218
      TROV_HX(5,3)=0.0                                                    218
      TROV_HX(6,3)=0.0                                                    218
      TROV_HX(7,3)=0.0                                                    218
      TROV_HX(8,3)=0.0                                                    218
      TROV_HX(9,3)=0.0                                                    218
      TROV_HX(1,4)=-7.79                                                  218
      TROV_HX(2,4)=0.0                                                    220
      TROV_HX(3,4)=7.79                                                   220
      TROV_HX(4,4)=0.0                                                    220
      TROV_HX(5,4)=0.0                                                    220
      TROV_HX(6,4)=0.0                                                    220
      TROV_HX(7,4)=0.0                                                    220
      TROV_HX(8,4)=0.0                                                    220
      TROV_HX(9,4)=0.0                                                    220
      TROV_HX(1,5)=-8.34                                                  220
      TROV_HX(2,5)=0.0                                                    222
      TROV_HX(3,5)=8.34                                                   222
      TROV_HX(4,5)=0.0                                                    222
      TROV_HX(5,5)=0.0                                                    222
      TROV_HX(6,5)=0.0                                                    222
      TROV_HX(7,5)=0.0                                                    222
      TROV_HX(8,5)=0.0                                                    222
      TROV_HX(9,5)=0.0                                                    222
      TROV_HX(1,6)=-9.38                                                  222
      TROV_HX(2,6)=0.0                                                    224
      TROV_HX(3,6)=9.38                                                   224
      TROV_HX(4,6)=0.0                                                    224
      TROV_HX(5,6)=0.0                                                    224
      TROV_HX(6,6)=0.0                                                    224
      TROV_HX(7,6)=0.0                                                    224
      TROV_HX(8,6)=0.0                                                    224
      TROV_HX(9,6)=0.0                                                    224
      TROV_HX(1,7)=-10.50                                                 224
      TROV_HX(2,7)=0.0                                                    226
      TROV_HX(3,7)=10.50                                                  226
      TROV_HX(4,7)=0.0                                                    226
      TROV_HX(5,7)=0.0                                                    226
      TROV_HX(6,7)=0.0                                                    226
      TROV_HX(7,7)=0.0                                                    226
      TROV_HX(8,7)=0.0                                                    226
      TROV_HX(9,7)=0.0                                                    226
      TROV_HX(1,8)=-11.97                                                 226
      TROV_HX(2,8)=-4.05                                                  228
      TROV_HX(3,8)=4.05                                                   228
      TROV_HX(4,8)=11.97                                                  228
      TROV_HX(5,8)=0.0                                                    228
      TROV_HX(6,8)=0.0                                                    228
      TROV_HX(7,8)=0.0                                                    228
      TROV_HX(8,8)=0.0                                                    228
      TROV_HX(9,8)=0.0                                                    228
      TROV_HX(1,9)=-12.70                                                 228
      TROV_HX(2,9)=-4.32                                                  230
      TROV_HX(3,9)=4.32                                                   230
      TROV_HX(4,9)=12.70                                                  230
      TROV_HX(5,9)=0.0                                                    230
      TROV_HX(6,9)=0.0                                                    230
      TROV_HX(7,9)=0.0                                                    230
      TROV_HX(8,9)=0.0                                                    230
      TROV_HX(9,9)=0.0                                                    230
      TROV_HX(1,10)=-13.70                                                230
      TROV_HX(2,10)=-4.63                                                 232
      TROV_HX(3,10)=4.63                                                  232
      TROV_HX(4,10)=13.70                                                 232
      TROV_HX(5,10)=0.0                                                   232
      TROV_HX(6,10)=0.0                                                   232
      TROV_HX(7,10)=0.0                                                   232
      TROV_HX(8,10)=0.0                                                   232
      TROV_HX(9,10)=0.0                                                   232
      TROV_HX(1,11)=-14.67                                                232
      TROV_HX(2,11)=-4.98                                                 234
      TROV_HX(3,11)=4.98                                                  234
      TROV_HX(4,11)=14.67                                                 234
      TROV_HX(5,11)=0.0                                                   234
      TROV_HX(6,11)=0.0                                                   234
      TROV_HX(7,11)=0.0                                                   234
      TROV_HX(8,11)=0.0                                                   234
      TROV_HX(9,11)=0.0                                                   234
      TROV_HX(1,12)=-15.86                                                234
      TROV_HX(2,12)=-7.91                                                 236
      TROV_HX(3,12)=0.0                                                   236
      TROV_HX(4,12)=7.91                                                  236
      TROV_HX(5,12)=15.86                                                 236
      TROV_HX(6,12)=0.0                                                   236
      TROV_HX(7,12)=0.0                                                   236
      TROV_HX(8,12)=0.0                                                   236
      TROV_HX(9,12)=0.0                                                   236
      TROV_HX(1,13)=-16.95                                                236
      TROV_HX(2,13)=16.95                                                 238
      TROV_HX(3,13)=0.0                                                   238
      TROV_HX(4,13)=0.0                                                   238
      TROV_HX(5,13)=0.0                                                   238
      TROV_HX(6,13)=0.0                                                   238
      TROV_HX(7,13)=0.0                                                   238
      TROV_HX(8,13)=0.0                                                   238
      TROV_HX(9,13)=0.0                                                   238
      TROV_HX(1,14)=-18.37                                                238
      TROV_HX(2,14)=-8.99                                                 240
      TROV_HX(3,14)=0.0                                                   240
      TROV_HX(4,14)=8.99                                                  240
      TROV_HX(5,14)=18.37                                                 240
      TROV_HX(6,14)=0.0                                                   240
      TROV_HX(7,14)=0.0                                                   240
      TROV_HX(8,14)=0.0                                                   240
      TROV_HX(9,14)=0.0                                                   240
      TROV_HX(1,15)=-19.11                                                240
      TROV_HX(2,15)=-9.76                                                 243
      TROV_HX(3,15)=0.0                                                   243
      TROV_HX(4,15)=9.76                                                  243
      TROV_HX(5,15)=19.11                                                 243
      TROV_HX(6,15)=0.0                                                   243
      TROV_HX(7,15)=0.0                                                   243
      TROV_HX(8,15)=0.0                                                   243
      TROV_HX(9,15)=0.0                                                   243
      TROV_HX(1,16)=-20.15                                                243
      TROV_HX(2,16)=-12.08                                                243
      TROV_HX(3,16)=-4.05                                                 243
      TROV_HX(4,16)=4.05                                                  243
      TROV_HX(5,16)=12.08                                                 243
      TROV_HX(6,16)=20.15                                                 243
      TROV_HX(7,16)=0.0                                                   243
      TROV_HX(8,16)=0.0                                                   243
      TROV_HX(9,16)=0.0                                                   243
C X OF HOLES                                                              243
      CTROV_HX='X OF HOLES'                                               243
C CARDS, FIRST ROW INNER SECTOR                                           244
      TROV_OFFCRDIO=-0.9                                                  244
      CTROV_OFFCRDIO='CARDS, FIRST ROW INNER SECTOR'                      244
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TROV',    245
     *BPATH_TROV,IDNUM_TROV,1, MAP_TROV,NAMES_TROV,COMMEN_TROV,           245
     *BANK_TROV,LEN_TROV,LLEN_TROV,LINK_TROV,FLAG_TROV)                   245
C *                                                                       245
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TROV',    245
     *BPATH_TROV,IDNUM_TROV,1, MAP_TROV,NAMES_TROV,COMMEN_TROV,           245
     *BANK_TROV,LEN_TROV,LLEN_TROV,LINK_TROV,FLAG_TROV)                   245
C *                                                                       246
      CALL AGSBEGM(MODULE,IPRIN)                                          246
      IF (IPRIN.LT.0) RETURN                                              246
C Filling bank TROV - HOLES AND CARDS IN ROW 2                            246
      BANK_TITLE='HOLES AND CARDS IN ROW 2'                               246
      BPATH_TROV='TROV*'                                                  246
      IDNUM_TROV(1)=0                                                     247
C OUTER                                                                   247
      TROV_SEC=2                                                          247
      CTROV_SEC='OUTER'                                                   247
C NUMBER OF HOLES IN PLANE                                                249
      TROV_NHP(1)=6                                                       249
      TROV_NHP(2)=7                                                       249
      TROV_NHP(3)=7                                                       249
      TROV_NHP(4)=7                                                       249
      TROV_NHP(5)=7                                                       249
      TROV_NHP(6)=7                                                       249
      TROV_NHP(7)=8                                                       249
      TROV_NHP(8)=8                                                       249
      TROV_NHP(9)=8                                                       249
      TROV_NHP(10)=8                                                      249
      TROV_NHP(11)=8                                                      249
      TROV_NHP(12)=9                                                      249
      TROV_NHP(13)=9                                                      249
      TROV_NHP(14)=9                                                      249
      TROV_NHP(15)=9                                                      249
      TROV_NHP(16)=9                                                      249
      CTROV_NHP='NUMBER OF HOLES IN PLANE'                                249
      TROV_HX(1,1)=-23.70                                                 252
      TROV_HX(2,1)=-15.77                                                 252
      TROV_HX(3,1)=-7.72                                                  252
      TROV_HX(4,1)=7.72                                                   252
      TROV_HX(5,1)=15.77                                                  252
      TROV_HX(6,1)=23.70                                                  252
      TROV_HX(7,1)=0.0                                                    252
      TROV_HX(8,1)=0.0                                                    252
      TROV_HX(9,1)=0.0                                                    252
      TROV_HX(1,2)=-24.15                                                 252
      TROV_HX(2,2)=-16.25                                                 254
      TROV_HX(3,2)=-8.10                                                  254
      TROV_HX(4,2)=0.00                                                   254
      TROV_HX(5,2)=8.10                                                   254
      TROV_HX(6,2)=16.25                                                  254
      TROV_HX(7,2)=24.15                                                  254
      TROV_HX(8,2)=0.0                                                    254
      TROV_HX(9,2)=0.0                                                    254
      TROV_HX(1,3)=-25.48                                                 254
      TROV_HX(2,3)=-17.47                                                 256
      TROV_HX(3,3)=-8.71                                                  256
      TROV_HX(4,3)=0.00                                                   256
      TROV_HX(5,3)=8.71                                                   256
      TROV_HX(6,3)=17.47                                                  256
      TROV_HX(7,3)=25.48                                                  256
      TROV_HX(8,3)=0.0                                                    256
      TROV_HX(9,3)=0.0                                                    256
      TROV_HX(1,4)=-25.48                                                 256
      TROV_HX(2,4)=-17.47                                                 258
      TROV_HX(3,4)=-8.71                                                  258
      TROV_HX(4,4)=0.00                                                   258
      TROV_HX(5,4)=8.71                                                   258
      TROV_HX(6,4)=17.47                                                  258
      TROV_HX(7,4)=25.48                                                  258
      TROV_HX(8,4)=0.0                                                    258
      TROV_HX(9,4)=0.0                                                    258
      TROV_HX(1,5)=-28.19                                                 258
      TROV_HX(2,5)=-19.43                                                 260
      TROV_HX(3,5)=-10.81                                                 260
      TROV_HX(4,5)=0.00                                                   260
      TROV_HX(5,5)=10.81                                                  260
      TROV_HX(6,5)=19.43                                                  260
      TROV_HX(7,5)=28.19                                                  260
      TROV_HX(8,5)=0.0                                                    260
      TROV_HX(9,5)=0.0                                                    260
      TROV_HX(1,6)=-28.19                                                 260
      TROV_HX(2,6)=-19.43                                                 262
      TROV_HX(3,6)=-10.81                                                 262
      TROV_HX(4,6)=0.00                                                   262
      TROV_HX(5,6)=10.81                                                  262
      TROV_HX(6,6)=19.43                                                  262
      TROV_HX(7,6)=28.19                                                  262
      TROV_HX(8,6)=0.0                                                    262
      TROV_HX(9,6)=0.0                                                    262
      TROV_HX(1,7)=-30.24                                                 262
      TROV_HX(2,7)=-21.51                                                 264
      TROV_HX(3,7)=-13.54                                                 264
      TROV_HX(4,7)=-5.37                                                  264
      TROV_HX(5,7)=5.37                                                   264
      TROV_HX(6,7)=13.54                                                  264
      TROV_HX(7,7)=21.51                                                  264
      TROV_HX(8,7)=30.24                                                  264
      TROV_HX(9,7)=0.0                                                    264
      TROV_HX(1,8)=-30.24                                                 264
      TROV_HX(2,8)=-21.51                                                 266
      TROV_HX(3,8)=-13.54                                                 266
      TROV_HX(4,8)=-5.37                                                  266
      TROV_HX(5,8)=5.37                                                   266
      TROV_HX(6,8)=13.54                                                  266
      TROV_HX(7,8)=21.51                                                  266
      TROV_HX(8,8)=30.24                                                  266
      TROV_HX(9,8)=0.0                                                    266
      TROV_HX(1,9)=-32.23                                                 266
      TROV_HX(2,9)=-24.17                                                 268
      TROV_HX(3,9)=-16.09                                                 268
      TROV_HX(4,9)=-5.42                                                  268
      TROV_HX(5,9)=5.42                                                   268
      TROV_HX(6,9)=16.09                                                  268
      TROV_HX(7,9)=24.17                                                  268
      TROV_HX(8,9)=32.23                                                  268
      TROV_HX(9,9)=0.0                                                    268
      TROV_HX(1,10)=-32.23                                                268
      TROV_HX(2,10)=-24.17                                                270
      TROV_HX(3,10)=-16.09                                                270
      TROV_HX(4,10)=-5.42                                                 270
      TROV_HX(5,10)=5.42                                                  270
      TROV_HX(6,10)=16.09                                                 270
      TROV_HX(7,10)=24.17                                                 270
      TROV_HX(8,10)=32.23                                                 270
      TROV_HX(9,10)=0.0                                                   270
      TROV_HX(1,11)=-34.30                                                270
      TROV_HX(2,11)=-25.86                                                272
      TROV_HX(3,11)=-16.14                                                272
      TROV_HX(4,11)=-8.10                                                 272
      TROV_HX(5,11)=8.10                                                  272
      TROV_HX(6,11)=16.14                                                 272
      TROV_HX(7,11)=25.86                                                 272
      TROV_HX(8,11)=34.30                                                 272
      TROV_HX(9,11)=0.0                                                   272
      TROV_HX(1,12)=-34.30                                                272
      TROV_HX(2,12)=-25.86                                                274
      TROV_HX(3,12)=-16.14                                                274
      TROV_HX(4,12)=-8.10                                                 274
      TROV_HX(5,12)=0.00                                                  274
      TROV_HX(6,12)=8.10                                                  274
      TROV_HX(7,12)=16.14                                                 274
      TROV_HX(8,12)=25.86                                                 274
      TROV_HX(9,12)=34.30                                                 274
      TROV_HX(1,13)=-36.16                                                274
      TROV_HX(2,13)=-28.12                                                276
      TROV_HX(3,13)=-16.14                                                276
      TROV_HX(4,13)=-8.10                                                 276
      TROV_HX(5,13)=0.00                                                  276
      TROV_HX(6,13)=8.10                                                  276
      TROV_HX(7,13)=16.14                                                 276
      TROV_HX(8,13)=28.12                                                 276
      TROV_HX(9,13)=36.16                                                 276
      TROV_HX(1,14)=-36.16                                                276
      TROV_HX(2,14)=-28.12                                                278
      TROV_HX(3,14)=-16.14                                                278
      TROV_HX(4,14)=-8.10                                                 278
      TROV_HX(5,14)=0.00                                                  278
      TROV_HX(6,14)=8.10                                                  278
      TROV_HX(7,14)=16.14                                                 278
      TROV_HX(8,14)=28.12                                                 278
      TROV_HX(9,14)=36.16                                                 278
      TROV_HX(1,15)=-38.37                                                278
      TROV_HX(2,15)=-29.47                                                281
      TROV_HX(3,15)=-20.76                                                281
      TROV_HX(4,15)=-10.61                                                281
      TROV_HX(5,15)=0.00                                                  281
      TROV_HX(6,15)=10.61                                                 281
      TROV_HX(7,15)=20.76                                                 281
      TROV_HX(8,15)=29.47                                                 281
      TROV_HX(9,15)=38.37                                                 281
      TROV_HX(1,16)=-38.37                                                281
      TROV_HX(2,16)=-29.47                                                281
      TROV_HX(3,16)=-20.76                                                281
      TROV_HX(4,16)=-10.61                                                281
      TROV_HX(5,16)=0.00                                                  281
      TROV_HX(6,16)=10.61                                                 281
      TROV_HX(7,16)=20.76                                                 281
      TROV_HX(8,16)=29.47                                                 281
      TROV_HX(9,16)=38.37                                                 281
C X OF HOLES                                                              281
      CTROV_HX='X OF HOLES'                                               281
C CARD NO SPECIAL CASE                                                    282
      TROV_OFFCRDIO=0.0                                                   282
      CTROV_OFFCRDIO='CARD NO SPECIAL CASE'                               282
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TROV',    283
     *BPATH_TROV,IDNUM_TROV,1, MAP_TROV,NAMES_TROV,COMMEN_TROV,           283
     *BANK_TROV,LEN_TROV,LLEN_TROV,LINK_TROV,FLAG_TROV)                   283
C *                                                                       283
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TROV',    283
     *BPATH_TROV,IDNUM_TROV,1, MAP_TROV,NAMES_TROV,COMMEN_TROV,           283
     *BANK_TROV,LEN_TROV,LLEN_TROV,LINK_TROV,FLAG_TROV)                   283
C use default TPCG bank                                                   284
      BPATH_TPCG='TPCG*'                                                  284
      IDNUM_TPCG(1)=0                                                     284
      CALL AGDGETP('TPCEGEO2','*',-999,'TPCG',' ',BANK_TPCG, BPATH_TPCG,  284
     *IDNUM_TPCG,1,MAP_TPCG,NAMES_TPCG,BANK_TPCG,LEN_TPCG,LLEN_TPCG,      284
     *LINK_TPCG,FLAG_TPCG)                                                284
C GAS PLUS ENDCAPS                                                        285
      TOFCLENG = TPCG_LENGTH-2*TPCG_WHEELTHK-2*TRDO_RDOVTHK               285
C ACTIVE GAS                                                              286
      TPGVLENG = (TOFCLENG-TPCG_MEMBTHK-2*TPCG_TPEATHK)/2                 286
      TOCSIR = TPCG_SENGASOR                                              286
      TOCSOR = TOCSIR + TPCG_TOCSDR                                       287
      TOKAIR = TOCSOR                                                     287
      TOKAOR = TOKAIR + TPCG_TOKADR                                       288
      TONXIR = TOKAOR                                                     288
      TONXOR = TONXIR + TPCG_TONXDR                                       289
      TOADIR = TONXOR                                                     289
      TOADOR = TOADIR + TPCG_TOADDR                                       290
      TOIGIR = TOADOR                                                     290
      TOIGOR = TOIGIR + TPCG_TOIGDR                                       291
      TOALIR = TOIGOR                                                     291
      TOALOR = TOALIR + TPCG_TOALDR                                       292
      TOHAIR = TOALOR                                                     292
      TOHAOR = TOHAIR + TPCG_TOHADR                                       293
C INNER FIELD CAGE INNER RADIUS                                           294
      TIFCIR = TPCG_RMIN                                                  294
      TIADIR = TIFCIR                                                     294
      TIADOR = TIADIR + TPCG_TIADDR                                       295
      TINXIR = TIADOR                                                     295
      TINXOR = TINXIR + TPCG_TINXDR                                       296
      TIKAIR = TINXOR                                                     296
      TIKAOR = TIKAIR + TPCG_TIKADR                                       297
      TIALIR = TIKAOR                                                     297
      TIALOR = TIALIR + TPCG_TIALDR                                       298
      TOFCIR = TOCSIR                                                     298
      TOFCOR = TOHAOR                                                     299
      TOFSIR = TOCSIR                                                     299
      TOFSOR = TOADOR                                                     300
      TIFCIR = TIADIR                                                     300
      TIFCOR = TIALOR                                                     301
C TPC GAS INNER RADIUS                                                    302
      TPGVIR = TIFCOR                                                     302
      DATA MNTCYLHT/30./                                                  303
      DATA MNTCYLID/16./                                                  304
      DATA MNTCYLOD/20./                                                  305
      DEL = TPCG_RMAX-TOFCOR                                              306
C Check IPRIN>=1                                                          306
      IF (IPRIN.GE.1) WRITE(LOUT,5010)TOFCOR,DEL                          306
5010  FORMAT(' TPCEgeo: maximum TPC Radius is ',F10.4,' clearance is ',   308
     *F8.4)                                                               308
C Check DEL<0                                                             309
      IF (DEL.LT.0) THEN                                                  309
      WRITE(LOUT,5020)DEL                                                 309
5020  FORMAT(' *** TPCEgeo ERROR : outer clearance negative ',F8.4)       311
      ENDIF                                                               312
C Check IPRIN>=1                                                          312
      IF (IPRIN.GE.1) WRITE(LOUT,5030)TPGVIR                              312
5030  FORMAT(' TPCEgeo: senset. gas inner radius',F10.4)                  314
      CALL AGSSTEP( TPCELASER )                                           315
      CALL AGSBEGM(MODULE,IPRIN)                                          316
C Check IPRIN<0                                                           316
      IF (IPRIN.LT.0) RETURN                                              316
C Create TPCE :                                                           316
C Call To Local Procedure TPCE                                            316
      ASSIGN 5050 TO BLOCK_TPCE                                           316
      GO TO  5040                                                         316
5050  CONTINUE                                                            316
C *                                                                       316
C Geant POSITION : TPCE ===> MOTHER='CAVE' <===                           317
      CALL AGSRESET                                                       317
      AG_MOTHER = 'CAVE'                                                  317
      AG_TITLE = 'TPCE'                                                   317
      AG_EXNAME = 'POSITION'                                              317
      AG_PARLIST = 'MOTHER'                                               317
      CALL AXPOSITION                                                     317
C *                                                                       317
C ----------------------------------------------------------------------  317
C ------------ TPCE                                                       317
      CALL AGSENDM                                                        319
      RETURN                                                              319
C Begin Local Procedure TPCE                                              319
C TPCE IS THE TPC ENVELOPE                                                319
5040  CONTINUE                                                            319
         AG_TITLE='TPCE'                                                  319
         AG_MODULE=MODULE                                                 319
         AG_MARK='V00'                                                    319
         CALL AGSPUSH                                                     319
         AG_MODULE=MODULE                                                 319
C    *                                                                    319
C    Geant MATERIAL : AIR ===> inherited arguments used <===              320
         CALL AGSRESET                                                    320
         AG_TITLE = 'AIR'                                                 320
         AG_EXNAME = 'MATERIAL'                                           320
         AG_PARLIST = 'NONE'                                              320
         CALL AXMATERIAL                                                  320
C    *                                                                    320
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           320
         CALL AGSRESET                                                    321
         AG_TITLE = 'STANDARD'                                            321
         AG_EXNAME = 'MEDIUM'                                             321
         AG_PARLIST = 'NONE'                                              321
         CALL AXMEDIUM                                                    321
C    *                                                                    321
C    Geant ATTRIBUTE : TPCE ===> SEEN=0 COLO=2 <===                       321
         CALL AGSRESET                                                    322
         AG_SEEN = 0                                                      322
         AG_COLO = 2                                                      322
         AG_TITLE = 'TPCE'                                                322
         AG_EXNAME = 'ATTRIBUTE'                                          322
         AG_PARLIST = 'SEEN_COLO'                                         322
         CALL AXATTRIBUTE                                                 322
C    *                                                                    322
C    Geant SHAPE : TUBE ===> RMIN=TPCG_RMIN RMAX=TPCG_RMAX, DZ=TPCG_LENG  322
C    TH/2 <===                                                            322
         CALL AGSRESET                                                    324
         AG_RMIN = TPCG_RMIN                                              324
         AG_RMAX = TPCG_RMAX                                              324
         AG_DZ = TPCG_LENGTH/2                                            324
         AG_TITLE = 'TUBE'                                                324
         AG_EXNAME = 'SHAPE'                                              324
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      324
         CALL AXSHAPE                                                     324
C    *                                                                    324
         TPGVZ =(TPCG_MEMBTHK + TPGVLENG)/2                               324
C    Create TPGV :                                                        326
C    Call To Local Procedure TPGV                                         326
         ASSIGN 5070 TO BLOCK_TPGV                                        326
         GO TO  5060                                                      326
5070     CONTINUE                                                         326
C    *                                                                    326
C    Geant POSITION : TPGV ===> Z=+TPGVZ THETAX=90 PHIX=75 THETAY=90 PHI  327
C    Y=-15 THETAZ=0 PHIZ=0 <===                                           327
         CALL AGSRESET                                                    327
         AG_Z = +TPGVZ                                                    327
         AG_THETAX = 90                                                   327
         AG_PHIX = 75                                                     327
         AG_THETAY = 90                                                   327
         AG_PHIY = -15                                                    327
         AG_THETAZ = 0                                                    327
         AG_PHIZ = 0                                                      327
         AG_TITLE = 'TPGV'                                                327
         AG_EXNAME = 'POSITION'                                           327
         AG_PARLIST = 'Z_THETAX_PHIX_THETAY_PHIY_THETAZ_PHIZ'             327
         CALL AXPOSITION                                                  327
C    *                                                                    327
C    Geant POSITION : TPGV ===> Z=-TPGVZ THETAX=90 PHIX=105 THETAY=90 PH  327
C    IY=195 THETAZ=180 PHIZ=0 <===                                        327
         CALL AGSRESET                                                    328
         AG_Z = -TPGVZ                                                    328
         AG_THETAX = 90                                                   328
         AG_PHIX = 105                                                    328
         AG_THETAY = 90                                                   328
         AG_PHIY = 195                                                    328
         AG_THETAZ = 180                                                  328
         AG_PHIZ = 0                                                      328
         AG_TITLE = 'TPGV'                                                328
         AG_EXNAME = 'POSITION'                                           328
         AG_PARLIST = 'Z_THETAX_PHIX_THETAY_PHIY_THETAZ_PHIZ'             328
         CALL AXPOSITION                                                  328
C    *                                                                    328
C    Create TIFC :                                                        329
C    Call To Local Procedure TIFC                                         329
         ASSIGN 5090 TO BLOCK_TIFC                                        329
         GO TO  5080                                                      329
5090     CONTINUE                                                         329
C    *                                                                    329
C    Geant POSITION : TIFC ===> inherited arguments used <===             330
         CALL AGSRESET                                                    330
         AG_TITLE = 'TIFC'                                                330
         AG_EXNAME = 'POSITION'                                           330
         AG_PARLIST = 'NONE'                                              330
         CALL AXPOSITION                                                  330
C    *                                                                    330
C    Create TOFC :                                                        331
C    Call To Local Procedure TOFC                                         331
         ASSIGN 5110 TO BLOCK_TOFC                                        331
         GO TO  5100                                                      331
5110     CONTINUE                                                         331
C    *                                                                    331
C    Geant POSITION : TOFC ===> inherited arguments used <===             332
         CALL AGSRESET                                                    332
         AG_TITLE = 'TOFC'                                                332
         AG_EXNAME = 'POSITION'                                           332
         AG_PARLIST = 'NONE'                                              332
         CALL AXPOSITION                                                  332
C    *                                                                    332
C    Create TPCM :                                                        333
C    Call To Local Procedure TPCM                                         333
         ASSIGN 5130 TO BLOCK_TPCM                                        333
         GO TO  5120                                                      333
5130     CONTINUE                                                         333
C    *                                                                    333
C    Geant POSITION : TPCM ===> inherited arguments used <===             334
         CALL AGSRESET                                                    334
         AG_TITLE = 'TPCM'                                                334
         AG_EXNAME = 'POSITION'                                           334
         AG_PARLIST = 'NONE'                                              334
         CALL AXPOSITION                                                  334
C    *                                                                    334
         TPEAZ =(TOFCLENG - TPCG_TPEATHK)/2                               334
C    Create TPEA :                                                        336
C    Call To Local Procedure TPEA                                         336
         ASSIGN 5150 TO BLOCK_TPEA                                        336
         GO TO  5140                                                      336
5150     CONTINUE                                                         336
C    *                                                                    336
C    Geant POSITION : TPEA ===> Z=+TPEAZ THETAX=90 PHIX=75 THETAY=90 PHI  337
C    Y=-15 THETAZ=0 PHIZ=0 <===                                           337
         CALL AGSRESET                                                    337
         AG_Z = +TPEAZ                                                    337
         AG_THETAX = 90                                                   337
         AG_PHIX = 75                                                     337
         AG_THETAY = 90                                                   337
         AG_PHIY = -15                                                    337
         AG_THETAZ = 0                                                    337
         AG_PHIZ = 0                                                      337
         AG_TITLE = 'TPEA'                                                337
         AG_EXNAME = 'POSITION'                                           337
         AG_PARLIST = 'Z_THETAX_PHIX_THETAY_PHIY_THETAZ_PHIZ'             337
         CALL AXPOSITION                                                  337
C    *                                                                    337
C    Geant POSITION : TPEA ===> Z=-TPEAZ THETAX=90 PHIX=105 THETAY=90 PH  337
C    IY=195 THETAZ=180 PHIZ=0 <===                                        337
         CALL AGSRESET                                                    338
         AG_Z = -TPEAZ                                                    338
         AG_THETAX = 90                                                   338
         AG_PHIX = 105                                                    338
         AG_THETAY = 90                                                   338
         AG_PHIY = 195                                                    338
         AG_THETAZ = 180                                                  338
         AG_PHIZ = 0                                                      338
         AG_TITLE = 'TPEA'                                                338
         AG_EXNAME = 'POSITION'                                           338
         AG_PARLIST = 'Z_THETAX_PHIX_THETAY_PHIY_THETAZ_PHIZ'             338
         CALL AXPOSITION                                                  338
C    *                                                                    338
         TPCWZ =(TPCG_LENGTH - 2*TRDO_RDOVTHK - TPCG_WHEELTHK)/2          338
C    Create TPCW :                                                        340
C    Call To Local Procedure TPCW                                         340
         ASSIGN 5170 TO BLOCK_TPCW                                        340
         GO TO  5160                                                      340
5170     CONTINUE                                                         340
C    *                                                                    340
C    Geant POSITION : TPCW ===> Z=+TPCWZ THETAX=90 PHIX=75 THETAY=90 PHI  341
C    Y=-15 THETAZ=0 PHIZ=0 <===                                           341
         CALL AGSRESET                                                    341
         AG_Z = +TPCWZ                                                    341
         AG_THETAX = 90                                                   341
         AG_PHIX = 75                                                     341
         AG_THETAY = 90                                                   341
         AG_PHIY = -15                                                    341
         AG_THETAZ = 0                                                    341
         AG_PHIZ = 0                                                      341
         AG_TITLE = 'TPCW'                                                341
         AG_EXNAME = 'POSITION'                                           341
         AG_PARLIST = 'Z_THETAX_PHIX_THETAY_PHIY_THETAZ_PHIZ'             341
         CALL AXPOSITION                                                  341
C    *                                                                    341
C    Geant POSITION : TPCW ===> Z=-TPCWZ THETAX=90 PHIX=105 THETAY=90 PH  341
C    IY=195 THETAZ=180 PHIZ=0 <===                                        341
         CALL AGSRESET                                                    342
         AG_Z = -TPCWZ                                                    342
         AG_THETAX = 90                                                   342
         AG_PHIX = 105                                                    342
         AG_THETAY = 90                                                   342
         AG_PHIY = 195                                                    342
         AG_THETAZ = 180                                                  342
         AG_PHIZ = 0                                                      342
         AG_TITLE = 'TPCW'                                                342
         AG_EXNAME = 'POSITION'                                           342
         AG_PARLIST = 'Z_THETAX_PHIX_THETAY_PHIY_THETAZ_PHIZ'             342
         CALL AXPOSITION                                                  342
C    *                                                                    342
         TPRDOZ =(TPCG_LENGTH - TRDO_RDOVTHK)/2                           342
C    Create TRDV :                                                        344
C    Call To Local Procedure TRDV                                         344
         ASSIGN 5190 TO BLOCK_TRDV                                        344
         GO TO  5180                                                      344
5190     CONTINUE                                                         344
C    *                                                                    344
C    Geant POSITION : TRDV ===> Z=+TPRDOZ THETAX=90 PHIX=75 THETAY=90 PH  345
C    IY=-15 THETAZ=0 PHIZ=0 <===                                          345
         CALL AGSRESET                                                    345
         AG_Z = +TPRDOZ                                                   345
         AG_THETAX = 90                                                   345
         AG_PHIX = 75                                                     345
         AG_THETAY = 90                                                   345
         AG_PHIY = -15                                                    345
         AG_THETAZ = 0                                                    345
         AG_PHIZ = 0                                                      345
         AG_TITLE = 'TRDV'                                                345
         AG_EXNAME = 'POSITION'                                           345
         AG_PARLIST = 'Z_THETAX_PHIX_THETAY_PHIY_THETAZ_PHIZ'             345
         CALL AXPOSITION                                                  345
C    *                                                                    345
C    Geant POSITION : TRDV ===> Z=-TPRDOZ THETAX=90 PHIX=105 THETAY=90 P  345
C    HIY=195 THETAZ=180 PHIZ=0 <===                                       345
         CALL AGSRESET                                                    346
         AG_Z = -TPRDOZ                                                   346
         AG_THETAX = 90                                                   346
         AG_PHIX = 105                                                    346
         AG_THETAY = 90                                                   346
         AG_PHIY = 195                                                    346
         AG_THETAZ = 180                                                  346
         AG_PHIZ = 0                                                      346
         AG_TITLE = 'TRDV'                                                346
         AG_EXNAME = 'POSITION'                                           346
         AG_PARLIST = 'Z_THETAX_PHIX_THETAY_PHIY_THETAZ_PHIZ'             346
         CALL AXPOSITION                                                  346
C    *                                                                    346
5041  CALL AGSPOPB                                                        347
      GO TO BLOCK_TPCE, (5040 ,5050)                                      347
C End of Local Procedure                                                  347
C ----------------------------------------------------------------------  348
C ------------ TPGV                                                       348
C Begin Local Procedure TPGV                                              349
C TPGV IS THE GAS VOLUME PLACED IN TPC                                    349
5060  CONTINUE                                                            349
         AG_TITLE='TPGV'                                                  349
         AG_MODULE=MODULE                                                 349
         AG_MARK='V00'                                                    349
         CALL AGSPUSH                                                     349
         AG_MODULE=MODULE                                                 349
C    *                                                                    349
C    Check TPCG_GASCORR==1                                                350
         IF (TPCG_GASCORR.EQ.1) THEN                                      350
         DENSITY=0.9*0.001782+0.1*0.000667                                351
         ELSE                                                             352
         T = 298.06                                                       353
         P = 1016.59                                                      354
         PATM = 0.001*P*750.062/760.                                      355
         RHO =(0.9*0.001782+0.1*0.000717)                                 356
         DENSITY=RHO*(273.15/T)*PATM                                      357
         ENDIF                                                            358
C    Geant COMPONENT : AR ===> A=40 Z=18 W=9 <===                         359
         CALL AGSRESET                                                    359
         AG_A = 40                                                        359
         AG_Z = 18                                                        359
         AG_W = 9                                                         359
         AG_TITLE = 'AR'                                                  359
         AG_EXNAME = 'COMPONENT'                                          359
         AG_PARLIST = 'A_Z_W'                                             359
         CALL AXCOMPONENT                                                 359
C    *                                                                    359
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           359
         CALL AGSRESET                                                    360
         AG_A = 12                                                        360
         AG_Z = 6                                                         360
         AG_W = 1                                                         360
         AG_TITLE = 'C'                                                   360
         AG_EXNAME = 'COMPONENT'                                          360
         AG_PARLIST = 'A_Z_W'                                             360
         CALL AXCOMPONENT                                                 360
C    *                                                                    360
C    Geant COMPONENT : H ===> A=1 Z=1 W=4 <===                            360
         CALL AGSRESET                                                    361
         AG_A = 1                                                         361
         AG_Z = 1                                                         361
         AG_W = 4                                                         361
         AG_TITLE = 'H'                                                   361
         AG_EXNAME = 'COMPONENT'                                          361
         AG_PARLIST = 'A_Z_W'                                             361
         CALL AXCOMPONENT                                                 361
C    *                                                                    361
C    Geant MIXTURE : P10 ===> DENS=DENSITY <===                           361
         CALL AGSRESET                                                    362
         AG_DENS = DENSITY                                                362
         AG_TITLE = 'P10'                                                 362
         AG_EXNAME = 'MIXTURE'                                            362
         AG_PARLIST = 'DENS'                                              362
         CALL AXMIXTURE                                                   362
C    *                                                                    362
C    Geant SHAPE : TUBE ===> RMIN=TPGVIR RMAX=TPCG_SENGASOR, DZ=TPGVLENG  362
C    /2 <===                                                              362
         CALL AGSRESET                                                    364
         AG_RMIN = TPGVIR                                                 364
         AG_RMAX = TPCG_SENGASOR                                          364
         AG_DZ = TPGVLENG/2                                               364
         AG_TITLE = 'TUBE'                                                364
         AG_EXNAME = 'SHAPE'                                              364
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      364
         CALL AXSHAPE                                                     364
C    *                                                                    364
C    Create TPSS :                                                        365
C    Call To Local Procedure TPSS                                         365
         ASSIGN 5210 TO BLOCK_TPSS                                        365
         GO TO  5200                                                      365
5210     CONTINUE                                                         365
C    *                                                                    365
5061  CALL AGSPOPB                                                        366
      GO TO BLOCK_TPGV, (5060 ,5070)                                      366
C End of Local Procedure                                                  366
C ----------------------------------------------------------------------  367
C ------------ TPSS                                                       367
C Begin Local Procedure TPSS                                              368
C TPSS IS A DIVISION OF GAS VOLUME CORRESPONDING TO A SUPERSECTORS        368
5200  CONTINUE                                                            368
         AG_TITLE='TPSS'                                                  368
         AG_MODULE=MODULE                                                 368
         AG_MARK='V00'                                                    368
         CALL AGSPUSH                                                     368
         AG_MODULE=MODULE                                                 368
C    *                                                                    368
C    Geant ATTRIBUTE : TPSS ===> SEEN=0 <===                              369
         CALL AGSRESET                                                    369
         AG_SEEN = 0                                                      369
         AG_TITLE = 'TPSS'                                                369
         AG_EXNAME = 'ATTRIBUTE'                                          369
         AG_PARLIST = 'SEEN'                                              369
         CALL AXATTRIBUTE                                                 369
C    *                                                                    369
C    Geant SHAPE : DIVISION ===> NDIV=12 IAXIS=2 <===                     369
         CALL AGSRESET                                                    370
         AG_NDIV = 12                                                     370
         AG_IAXIS = 2                                                     370
         AG_TITLE = 'DIVISION'                                            370
         AG_EXNAME = 'SHAPE'                                              370
         AG_PARLIST = 'NDIV_IAXIS'                                        370
         CALL AXSHAPE                                                     370
C    *                                                                    370
C    *                                                                    370
         I_SEC=1                                                          371
            GO TO 5223                                                    371
5221        I_SEC=I_SEC+1                                                 371
5223        IF(I_SEC-(2).GT.0) GO TO 5222                                 371
C       use TPRS bank                                                     372
            BPATH_TPRS='TPRS*'                                            372
            IDNUM_TPRS(1)=0                                               372
            TPRS_SEC=I_SEC                                                372
            CALL AGDGETP('TPCEGEO2','*',-999,'TPRS',NTPRS_SEC,            372
     *      RE_TPRS_SEC,BPATH_TPRS,IDNUM_TPRS,1,MAP_TPRS,NAMES_TPRS,      372
     *      BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)             372
C       *                                                                 373
            I_ROW=1                                                       373
               GO TO 5233                                                 373
5231           I_ROW=I_ROW+1                                              373
5233           IF(I_ROW-(NINT(TPRS_NROW)).GT.0) GO TO 5232                373
C          Check NINT(TPRS_SUPER)==3|I_ROW==1                             374
               IF (NINT(TPRS_SUPER).EQ.3.OR.I_ROW.EQ.1) THEN              374
C          Create TPAD :                                                  375
C          Call To Local Procedure TPAD                                   375
               ASSIGN 5250 TO BLOCK_TPAD                                  375
               GO TO  5240                                                375
5250           CONTINUE                                                   375
C          *                                                              375
C          Geant POSITION : TPAD ===> X=TPRS_RPADS(I_ROW)-TPRS_WIDTH DX=  377
C          TPRS_WIDTH/2 DY=TPRS_NPADS(I_ROW)*TPRS_PITCH/2 <===            377
               CALL AGSRESET                                              377
               AG_X = TPRS_RPADS(I_ROW)-TPRS_WIDTH                        377
               AG_DX = TPRS_WIDTH/2                                       377
               AG_DY = TPRS_NPADS(I_ROW)*TPRS_PITCH/2                     377
               AG_TITLE = 'TPAD'                                          377
               AG_EXNAME = 'POSITION'                                     377
               AG_PARLIST = 'X_DX_DY'                                     377
               CALL AXPOSITION                                            377
C          *                                                              377
               ENDIF                                                      377
C          Check NINT(TPRS_SUPER)>0                                       379
               IF (NINT(TPRS_SUPER).GT.0) THEN                            379
C          Create TPAD :                                                  380
C          Call To Local Procedure TPAD                                   380
               ASSIGN 5260 TO BLOCK_TPAD                                  380
               GO TO  5240                                                380
5260           CONTINUE                                                   380
C          *                                                              380
C          Geant POSITION : TPAD ===> X=TPRS_RPADS(I_ROW) DX=TPRS_WIDTH/  382
C          2 DY=TPRS_NPADS(I_ROW)*TPRS_PITCH/2 <===                       382
               CALL AGSRESET                                              382
               AG_X = TPRS_RPADS(I_ROW)                                   382
               AG_DX = TPRS_WIDTH/2                                       382
               AG_DY = TPRS_NPADS(I_ROW)*TPRS_PITCH/2                     382
               AG_TITLE = 'TPAD'                                          382
               AG_EXNAME = 'POSITION'                                     382
               AG_PARLIST = 'X_DX_DY'                                     382
               CALL AXPOSITION                                            382
C          *                                                              382
               ENDIF                                                      382
C          Check NINT(TPRS_SUPER)==3|I_ROW==NINT(TPRS_NROW)               384
               IF (NINT(TPRS_SUPER).EQ.3.OR.I_ROW.EQ.NINT(TPRS_NROW))     384
     *         THEN                                                       384
C          Create TPAD :                                                  385
C          Call To Local Procedure TPAD                                   385
               ASSIGN 5270 TO BLOCK_TPAD                                  385
               GO TO  5240                                                385
5270           CONTINUE                                                   385
C          *                                                              385
C          Geant POSITION : TPAD ===> X=TPRS_RPADS(I_ROW)+TPRS_WIDTH DX=  387
C          TPRS_WIDTH/2 DY=TPRS_NPADS(I_ROW)*TPRS_PITCH/2 <===            387
               CALL AGSRESET                                              387
               AG_X = TPRS_RPADS(I_ROW)+TPRS_WIDTH                        387
               AG_DX = TPRS_WIDTH/2                                       387
               AG_DY = TPRS_NPADS(I_ROW)*TPRS_PITCH/2                     387
               AG_TITLE = 'TPAD'                                          387
               AG_EXNAME = 'POSITION'                                     387
               AG_PARLIST = 'X_DX_DY'                                     387
               CALL AXPOSITION                                            387
C          *                                                              387
               ENDIF                                                      387
            GO TO 5231                                                    389
5232        CONTINUE                                                      389
         GO TO 5221                                                       390
5222     CONTINUE                                                         390
5201  CALL AGSPOPB                                                        391
      GO TO BLOCK_TPSS, (5200 ,5210)                                      391
C End of Local Procedure                                                  391
C ----------------------------------------------------------------------  392
C ------------ TPAD                                                       392
C Begin Local Procedure TPAD                                              393
C TPAD IS A REAL PADROW WITH DIMENSIONS DEFINED AT POSITIONING TIME       393
5240  CONTINUE                                                            393
         AG_TITLE='TPAD'                                                  393
         AG_MODULE=MODULE                                                 393
         AG_MARK='V00'                                                    393
         CALL AGSPUSH                                                     393
         AG_MODULE=MODULE                                                 393
C    *                                                                    393
C    *                                                                    394
C       Check TPRS_WIDTH<1.5                                              394
            IF (TPRS_WIDTH.LT.1.5) THEN                                   394
C       Geant ATTRIBUTE : TPAD ===> SEEN=0 COLO=2 SERIAL=1 <===           395
            CALL AGSRESET                                                 395
            AG_SEEN = 0                                                   395
            AG_COLO = 2                                                   395
            AG_SERIAL = 1                                                 395
            AG_TITLE = 'TPAD'                                             395
            AG_EXNAME = 'ATTRIBUTE'                                       395
            AG_PARLIST = 'SEEN_COLO_SERIAL'                               395
            CALL AXATTRIBUTE                                              395
C       *                                                                 395
         END IF                                                           396
C    *                                                                    397
C       Check TPRS_WIDTH>1.5                                              397
            IF (TPRS_WIDTH.GT.1.5) THEN                                   397
C       Geant ATTRIBUTE : TPAD ===> SEEN=0 COLO=2 SERIAL=2 <===           398
            CALL AGSRESET                                                 398
            AG_SEEN = 0                                                   398
            AG_COLO = 2                                                   398
            AG_SERIAL = 2                                                 398
            AG_TITLE = 'TPAD'                                             398
            AG_EXNAME = 'ATTRIBUTE'                                       398
            AG_PARLIST = 'SEEN_COLO_SERIAL'                               398
            CALL AXATTRIBUTE                                              398
C       *                                                                 398
         END IF                                                           399
C    Geant MATERIAL : P10 ===> inherited arguments used <===              400
         CALL AGSRESET                                                    400
         AG_TITLE = 'P10'                                                 400
         AG_EXNAME = 'MATERIAL'                                           400
         AG_PARLIST = 'NONE'                                              400
         CALL AXMATERIAL                                                  400
C    *                                                                    400
C    Geant MATERIAL : SENSITIVE_GAS ===> ISVOL=1 STEMAX=2.5*TPRS_WIDTH <  400
C    ===                                                                  400
         CALL AGSRESET                                                    401
         AG_ISVOL = 1                                                     401
         AG_STEMAX = 2.5*TPRS_WIDTH                                       401
         AG_TITLE = 'SENSITIVE_GAS'                                       401
         AG_EXNAME = 'MATERIAL'                                           401
         AG_PARLIST = 'ISVOL_STEMAX'                                      401
         CALL AXMATERIAL                                                  401
C    *                                                                    401
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           401
         CALL AGSRESET                                                    402
         AG_DX = 0                                                        402
         AG_DY = 0                                                        402
         AG_DZ = 0                                                        402
         AG_TITLE = 'BOX'                                                 402
         AG_EXNAME = 'SHAPE'                                              402
         AG_PARLIST = 'DX_DY_DZ'                                          402
         CALL AXSHAPE                                                     402
C    *                                                                    402
         CALL GSTPAR(AG_IMED, 'STRA', 1.)                                 402
C    Defining Hits: Z:0.0005:S Y:0.0005: Z:0.0005: CX:10: CY:10: CZ:10:   405
C    LPTOT:18:(-3,2), SLENG:0.1:(0,800) TOF:16:(0,1.0E-6) LGAM:16:(-2,+2  405
C    ) STEP:11:(0,10) USER:21:(-0.01,0.01)                                405
         AG_TITLE = 'TPAD'                                                405
         AG_EXNAME = 'HITS'                                               405
         AG_TYPE=AG_MAGIC                                                 405
         CALL AGSDETV('TPAD')                                             405
C    *                                                                    405
         AG_PARLIST='Z'                                                   405
         AG_TITLE = 'DETH'                                                405
         AG_NBITS = AG_MAGIC                                              405
         AG_BIN=0.0005                                                    405
         AG_OPTION='S'                                                    405
         AG_FHMIN = AG_MAGIC                                              405
         AG_FHMAX = AG_MAGIC                                              405
         CALL AGSDETH (0)                                                 405
C    *                                                                    405
         AG_PARLIST='Y'                                                   405
         AG_TITLE = 'DETH'                                                405
         AG_NBITS = AG_MAGIC                                              405
         AG_BIN=0.0005                                                    405
         AG_FHMIN=AG_MAGIC                                                405
         AG_FHMAX=AG_MAGIC                                                405
         AG_OPTION='*'                                                    405
         CALL AGSDETH (0)                                                 405
C    *                                                                    405
         AG_PARLIST='Z'                                                   405
         AG_TITLE = 'DETH'                                                405
         AG_NBITS = AG_MAGIC                                              405
         AG_BIN=0.0005                                                    405
         AG_FHMIN=AG_MAGIC                                                405
         AG_FHMAX=AG_MAGIC                                                405
         AG_OPTION='*'                                                    405
         CALL AGSDETH (0)                                                 405
C    *                                                                    405
         AG_PARLIST='CX'                                                  405
         AG_TITLE = 'DETH'                                                405
         AG_NBITS = 10                                                    405
         AG_BIN=AG_MAGIC                                                  405
         AG_FHMIN=AG_MAGIC                                                405
         AG_FHMAX=AG_MAGIC                                                405
         AG_OPTION='*'                                                    405
         CALL AGSDETH (0)                                                 405
C    *                                                                    405
         AG_PARLIST='CY'                                                  405
         AG_TITLE = 'DETH'                                                405
         AG_NBITS = 10                                                    405
         AG_BIN=AG_MAGIC                                                  405
         AG_FHMIN=AG_MAGIC                                                405
         AG_FHMAX=AG_MAGIC                                                405
         AG_OPTION='*'                                                    405
         CALL AGSDETH (0)                                                 405
C    *                                                                    405
         AG_PARLIST='CZ'                                                  405
         AG_TITLE = 'DETH'                                                405
         AG_NBITS = 10                                                    405
         AG_BIN=AG_MAGIC                                                  405
         AG_FHMIN=AG_MAGIC                                                405
         AG_FHMAX=AG_MAGIC                                                405
         AG_OPTION='*'                                                    405
         CALL AGSDETH (0)                                                 405
C    *                                                                    405
         AG_PARLIST='LPTOT'                                               405
         AG_TITLE = 'DETH'                                                405
         AG_NBITS = 18                                                    405
         AG_BIN=AG_MAGIC                                                  405
         AG_OPTION='*'                                                    405
         AG_FHMIN = -3                                                    405
         AG_FHMAX = 2                                                     405
         CALL AGSDETH (0)                                                 405
C    *                                                                    405
         AG_PARLIST='SLENG'                                               405
         AG_TITLE = 'DETH'                                                405
         AG_NBITS = AG_MAGIC                                              405
         AG_BIN=0.1                                                       405
         AG_OPTION='*'                                                    405
         AG_FHMIN = 0                                                     405
         AG_FHMAX = 800                                                   405
         CALL AGSDETH (0)                                                 405
C    *                                                                    405
         AG_PARLIST='TOF'                                                 405
         AG_TITLE = 'DETH'                                                405
         AG_NBITS = 16                                                    405
         AG_BIN=AG_MAGIC                                                  405
         AG_OPTION='*'                                                    405
         AG_FHMIN = 0                                                     405
         AG_FHMAX = 1.0E-6                                                405
         CALL AGSDETH (0)                                                 405
C    *                                                                    405
         AG_PARLIST='LGAM'                                                405
         AG_TITLE = 'DETH'                                                405
         AG_NBITS = 16                                                    405
         AG_BIN=AG_MAGIC                                                  405
         AG_OPTION='*'                                                    405
         AG_FHMIN = -2                                                    405
         AG_FHMAX = +2                                                    405
         CALL AGSDETH (0)                                                 405
C    *                                                                    405
         AG_PARLIST='STEP'                                                405
         AG_TITLE = 'DETH'                                                405
         AG_NBITS = 11                                                    405
         AG_BIN=AG_MAGIC                                                  405
         AG_OPTION='*'                                                    405
         AG_FHMIN = 0                                                     405
         AG_FHMAX = 10                                                    405
         CALL AGSDETH (0)                                                 405
C    *                                                                    405
         AG_PARLIST='USER'                                                405
         AG_TITLE = 'DETH'                                                405
         AG_NBITS = 21                                                    405
         AG_BIN=AG_MAGIC                                                  405
         AG_OPTION='*'                                                    405
         AG_FHMIN = -0.01                                                 405
         AG_FHMAX = 0.01                                                  405
         CALL AGSDETH (JUMPAD(TPADSTEP))                                  405
C    *                                                                    405
         CALL AGSDETU('TPAD')                                             405
C    *                                                                    405
5241  CALL AGSPOPB                                                        406
      GO TO BLOCK_TPAD, (5240 ,5250,5260,5270)                            406
C End of Local Procedure                                                  406
C ----------------------------------------------------------------------  407
C ------------ TIFC                                                       407
C Begin Local Procedure TIFC                                              408
C TIFC DEFINES THE INNER FIELD CAGE PLACED IN TPC                         408
5080  CONTINUE                                                            408
         AG_TITLE='TIFC'                                                  408
         AG_MODULE=MODULE                                                 408
         AG_MARK='V00'                                                    408
         CALL AGSPUSH                                                     408
         AG_MODULE=MODULE                                                 408
C    *                                                                    408
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            409
         CALL AGSRESET                                                    409
         AG_TITLE = 'MYLAR'                                               409
         AG_EXNAME = 'MATERIAL'                                           409
         AG_PARLIST = 'NONE'                                              409
         CALL AXMATERIAL                                                  409
C    *                                                                    409
C    Geant ATTRIBUTE : TIFC ===> SEEN=1 COLO=4 <===                       409
         CALL AGSRESET                                                    410
         AG_SEEN = 1                                                      410
         AG_COLO = 4                                                      410
         AG_TITLE = 'TIFC'                                                410
         AG_EXNAME = 'ATTRIBUTE'                                          410
         AG_PARLIST = 'SEEN_COLO'                                         410
         CALL AXATTRIBUTE                                                 410
C    *                                                                    410
C    Geant SHAPE : TUBE ===> RMIN=TIADIR RMAX=TIFCOR DZ=TOFCLENG/2 <===   410
         CALL AGSRESET                                                    411
         AG_RMIN = TIADIR                                                 411
         AG_RMAX = TIFCOR                                                 411
         AG_DZ = TOFCLENG/2                                               411
         AG_TITLE = 'TUBE'                                                411
         AG_EXNAME = 'SHAPE'                                              411
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      411
         CALL AXSHAPE                                                     411
C    *                                                                    411
C    Create TINX :                                                        412
C    Call To Local Procedure TINX                                         412
         ASSIGN 5290 TO BLOCK_TINX                                        412
         GO TO  5280                                                      412
5290     CONTINUE                                                         412
C    *                                                                    412
C    Geant POSITION : TINX ===> inherited arguments used <===             413
         CALL AGSRESET                                                    413
         AG_TITLE = 'TINX'                                                413
         AG_EXNAME = 'POSITION'                                           413
         AG_PARLIST = 'NONE'                                              413
         CALL AXPOSITION                                                  413
C    *                                                                    413
5081  CALL AGSPOPB                                                        414
      GO TO BLOCK_TIFC, (5080 ,5090)                                      414
C End of Local Procedure                                                  414
C ----------------------------------------------------------------------  415
C ------------ TINX                                                       415
C Begin Local Procedure TINX                                              416
C TINX IS THE INNER NOMEX STRUCTURE                                       416
5280  CONTINUE                                                            416
         AG_TITLE='TINX'                                                  416
         AG_MODULE=MODULE                                                 416
         AG_MARK='V00'                                                    416
         CALL AGSPUSH                                                     416
         AG_MODULE=MODULE                                                 416
C    *                                                                    416
C    Geant COMPONENT : C ===> A=12 Z=6 W=5 <===                           417
         CALL AGSRESET                                                    417
         AG_A = 12                                                        417
         AG_Z = 6                                                         417
         AG_W = 5                                                         417
         AG_TITLE = 'C'                                                   417
         AG_EXNAME = 'COMPONENT'                                          417
         AG_PARLIST = 'A_Z_W'                                             417
         CALL AXCOMPONENT                                                 417
C    *                                                                    417
C    Geant COMPONENT : H ===> A=1 Z=1 W=8 <===                            417
         CALL AGSRESET                                                    418
         AG_A = 1                                                         418
         AG_Z = 1                                                         418
         AG_W = 8                                                         418
         AG_TITLE = 'H'                                                   418
         AG_EXNAME = 'COMPONENT'                                          418
         AG_PARLIST = 'A_Z_W'                                             418
         CALL AXCOMPONENT                                                 418
C    *                                                                    418
C    Geant COMPONENT : O ===> A=16 Z=8 W=2 <===                           418
         CALL AGSRESET                                                    419
         AG_A = 16                                                        419
         AG_Z = 8                                                         419
         AG_W = 2                                                         419
         AG_TITLE = 'O'                                                   419
         AG_EXNAME = 'COMPONENT'                                          419
         AG_PARLIST = 'A_Z_W'                                             419
         CALL AXCOMPONENT                                                 419
C    *                                                                    419
C    Geant MIXTURE : NOMEX ===> DENS=0.048 <===                           419
         CALL AGSRESET                                                    420
         AG_DENS = 0.048                                                  420
         AG_TITLE = 'NOMEX'                                               420
         AG_EXNAME = 'MIXTURE'                                            420
         AG_PARLIST = 'DENS'                                              420
         CALL AXMIXTURE                                                   420
C    *                                                                    420
C    Geant ATTRIBUTE : TINX ===> SEEN=1 COLO=5 <===                       420
         CALL AGSRESET                                                    421
         AG_SEEN = 1                                                      421
         AG_COLO = 5                                                      421
         AG_TITLE = 'TINX'                                                421
         AG_EXNAME = 'ATTRIBUTE'                                          421
         AG_PARLIST = 'SEEN_COLO'                                         421
         CALL AXATTRIBUTE                                                 421
C    *                                                                    421
C    Geant SHAPE : TUBE ===> RMIN=TINXIR <===                             421
         CALL AGSRESET                                                    422
         AG_RMIN = TINXIR                                                 422
         AG_TITLE = 'TUBE'                                                422
         AG_EXNAME = 'SHAPE'                                              422
         AG_PARLIST = 'RMIN'                                              422
         CALL AXSHAPE                                                     422
C    *                                                                    422
C    Create TIKA :                                                        423
C    Call To Local Procedure TIKA                                         423
         ASSIGN 5310 TO BLOCK_TIKA                                        423
         GO TO  5300                                                      423
5310     CONTINUE                                                         423
C    *                                                                    423
C    Geant POSITION : TIKA ===> inherited arguments used <===             424
         CALL AGSRESET                                                    424
         AG_TITLE = 'TIKA'                                                424
         AG_EXNAME = 'POSITION'                                           424
         AG_PARLIST = 'NONE'                                              424
         CALL AXPOSITION                                                  424
C    *                                                                    424
5281  CALL AGSPOPB                                                        425
      GO TO BLOCK_TINX, (5280 ,5290)                                      425
C End of Local Procedure                                                  425
C ----------------------------------------------------------------------  426
C ------------ TIKA                                                       426
C Begin Local Procedure TIKA                                              427
C TIKA IS THE KAPTON FILM OF THE INNER FIELD CAGE                         427
5300  CONTINUE                                                            427
         AG_TITLE='TIKA'                                                  427
         AG_MODULE=MODULE                                                 427
         AG_MARK='V00'                                                    427
         CALL AGSPUSH                                                     427
         AG_MODULE=MODULE                                                 427
C    *                                                                    427
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            428
         CALL AGSRESET                                                    428
         AG_TITLE = 'MYLAR'                                               428
         AG_EXNAME = 'MATERIAL'                                           428
         AG_PARLIST = 'NONE'                                              428
         CALL AXMATERIAL                                                  428
C    *                                                                    428
C    Geant ATTRIBUTE : TIKA ===> SEEN=1 COLO=6 <===                       428
         CALL AGSRESET                                                    429
         AG_SEEN = 1                                                      429
         AG_COLO = 6                                                      429
         AG_TITLE = 'TIKA'                                                429
         AG_EXNAME = 'ATTRIBUTE'                                          429
         AG_PARLIST = 'SEEN_COLO'                                         429
         CALL AXATTRIBUTE                                                 429
C    *                                                                    429
C    Geant SHAPE : TUBE ===> RMIN=TIKAIR RMAX=TIKAOR <===                 429
         CALL AGSRESET                                                    430
         AG_RMIN = TIKAIR                                                 430
         AG_RMAX = TIKAOR                                                 430
         AG_TITLE = 'TUBE'                                                430
         AG_EXNAME = 'SHAPE'                                              430
         AG_PARLIST = 'RMIN_RMAX'                                         430
         CALL AXSHAPE                                                     430
C    *                                                                    430
C    Create TIAL :                                                        431
C    Call To Local Procedure TIAL                                         431
         ASSIGN 5330 TO BLOCK_TIAL                                        431
         GO TO  5320                                                      431
5330     CONTINUE                                                         431
C    *                                                                    431
C    Geant POSITION : TIAL ===> inherited arguments used <===             432
         CALL AGSRESET                                                    432
         AG_TITLE = 'TIAL'                                                432
         AG_EXNAME = 'POSITION'                                           432
         AG_PARLIST = 'NONE'                                              432
         CALL AXPOSITION                                                  432
C    *                                                                    432
5301  CALL AGSPOPB                                                        433
      GO TO BLOCK_TIKA, (5300 ,5310)                                      433
C End of Local Procedure                                                  433
C ----------------------------------------------------------------------  434
C ------------ TIAL                                                       434
C Begin Local Procedure TIAL                                              435
C TIAL IS THE INNER ALUMINUM CYLINDER                                     435
5320  CONTINUE                                                            435
         AG_TITLE='TIAL'                                                  435
         AG_MODULE=MODULE                                                 435
         AG_MARK='V00'                                                    435
         CALL AGSPUSH                                                     435
         AG_MODULE=MODULE                                                 435
C    *                                                                    435
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        436
         CALL AGSRESET                                                    436
         AG_TITLE = 'ALUMINIUM'                                           436
         AG_EXNAME = 'MATERIAL'                                           436
         AG_PARLIST = 'NONE'                                              436
         CALL AXMATERIAL                                                  436
C    *                                                                    436
C    Geant ATTRIBUTE : TIAL ===> SEEN=1 COLO=7 <===                       436
         CALL AGSRESET                                                    437
         AG_SEEN = 1                                                      437
         AG_COLO = 7                                                      437
         AG_TITLE = 'TIAL'                                                437
         AG_EXNAME = 'ATTRIBUTE'                                          437
         AG_PARLIST = 'SEEN_COLO'                                         437
         CALL AXATTRIBUTE                                                 437
C    *                                                                    437
C    Geant SHAPE : TUBE ===> RMIN=TIALIR RMAX=TIALOR <===                 437
         CALL AGSRESET                                                    438
         AG_RMIN = TIALIR                                                 438
         AG_RMAX = TIALOR                                                 438
         AG_TITLE = 'TUBE'                                                438
         AG_EXNAME = 'SHAPE'                                              438
         AG_PARLIST = 'RMIN_RMAX'                                         438
         CALL AXSHAPE                                                     438
C    *                                                                    438
5321  CALL AGSPOPB                                                        439
      GO TO BLOCK_TIAL, (5320 ,5330)                                      439
C End of Local Procedure                                                  439
C ----------------------------------------------------------------------  440
C ------------ TOFC                                                       440
C Begin Local Procedure TOFC                                              441
C TOFC DEFINES OUTER FIELD CAGE - FILL IT WITH INSULATING GAS ALREADY     441
5100  CONTINUE                                                            441
         AG_TITLE='TOFC'                                                  441
         AG_MODULE=MODULE                                                 441
         AG_MARK='V00'                                                    441
         CALL AGSPUSH                                                     441
         AG_MODULE=MODULE                                                 441
C    *                                                                    441
C    Geant MATERIAL : NITROGEN_GAS ===> inherited arguments used <===     442
         CALL AGSRESET                                                    442
         AG_TITLE = 'NITROGEN_GAS'                                        442
         AG_EXNAME = 'MATERIAL'                                           442
         AG_PARLIST = 'NONE'                                              442
         CALL AXMATERIAL                                                  442
C    *                                                                    442
C    Geant ATTRIBUTE : TOFC ===> SEEN=1 COLO=3 <===                       442
         CALL AGSRESET                                                    443
         AG_SEEN = 1                                                      443
         AG_COLO = 3                                                      443
         AG_TITLE = 'TOFC'                                                443
         AG_EXNAME = 'ATTRIBUTE'                                          443
         AG_PARLIST = 'SEEN_COLO'                                         443
         CALL AXATTRIBUTE                                                 443
C    *                                                                    443
C    Geant SHAPE : TUBE ===> RMIN=TOFCIR RMAX=TOFCOR DZ=TOFCLENG/2 <===   443
         CALL AGSRESET                                                    444
         AG_RMIN = TOFCIR                                                 444
         AG_RMAX = TOFCOR                                                 444
         AG_DZ = TOFCLENG/2                                               444
         AG_TITLE = 'TUBE'                                                444
         AG_EXNAME = 'SHAPE'                                              444
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      444
         CALL AXSHAPE                                                     444
C    *                                                                    444
C    Create TOFS :                                                        445
C    Call To Local Procedure TOFS                                         445
         ASSIGN 5350 TO BLOCK_TOFS                                        445
         GO TO  5340                                                      445
5350     CONTINUE                                                         445
C    *                                                                    445
C    Geant POSITION : TOFS ===> inherited arguments used <===             446
         CALL AGSRESET                                                    446
         AG_TITLE = 'TOFS'                                                446
         AG_EXNAME = 'POSITION'                                           446
         AG_PARLIST = 'NONE'                                              446
         CALL AXPOSITION                                                  446
C    *                                                                    446
C    Create TOST :                                                        447
C    Call To Local Procedure TOST                                         447
         ASSIGN 5370 TO BLOCK_TOST                                        447
         GO TO  5360                                                      447
5370     CONTINUE                                                         447
C    *                                                                    447
C    Geant POSITION : TOST ===> inherited arguments used <===             448
         CALL AGSRESET                                                    448
         AG_TITLE = 'TOST'                                                448
         AG_EXNAME = 'POSITION'                                           448
         AG_PARLIST = 'NONE'                                              448
         CALL AXPOSITION                                                  448
C    *                                                                    448
5101  CALL AGSPOPB                                                        449
      GO TO BLOCK_TOFC, (5100 ,5110)                                      449
C End of Local Procedure                                                  449
C ----------------------------------------------------------------------  450
C ------------ TOFS                                                       450
C Begin Local Procedure TOFS                                              451
C TOFS IS THE OUTER FIELD CAGE STRUCTURE                                  451
5340  CONTINUE                                                            451
         AG_TITLE='TOFS'                                                  451
         AG_MODULE=MODULE                                                 451
         AG_MARK='V00'                                                    451
         CALL AGSPUSH                                                     451
         AG_MODULE=MODULE                                                 451
C    *                                                                    451
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           452
         CALL AGSRESET                                                    452
         AG_TITLE = 'COPPER'                                              452
         AG_EXNAME = 'MATERIAL'                                           452
         AG_PARLIST = 'NONE'                                              452
         CALL AXMATERIAL                                                  452
C    *                                                                    452
C    Geant ATTRIBUTE : TOFS ===> SEEN=1 COLO=4 <===                       452
         CALL AGSRESET                                                    453
         AG_SEEN = 1                                                      453
         AG_COLO = 4                                                      453
         AG_TITLE = 'TOFS'                                                453
         AG_EXNAME = 'ATTRIBUTE'                                          453
         AG_PARLIST = 'SEEN_COLO'                                         453
         CALL AXATTRIBUTE                                                 453
C    *                                                                    453
C    Geant SHAPE : TUBE ===> RMAX=TOFSOR <===                             453
         CALL AGSRESET                                                    454
         AG_RMAX = TOFSOR                                                 454
         AG_TITLE = 'TUBE'                                                454
         AG_EXNAME = 'SHAPE'                                              454
         AG_PARLIST = 'RMAX'                                              454
         CALL AXSHAPE                                                     454
C    *                                                                    454
C    Create TOKA :                                                        455
C    Call To Local Procedure TOKA                                         455
         ASSIGN 5390 TO BLOCK_TOKA                                        455
         GO TO  5380                                                      455
5390     CONTINUE                                                         455
C    *                                                                    455
C    Geant POSITION : TOKA ===> inherited arguments used <===             456
         CALL AGSRESET                                                    456
         AG_TITLE = 'TOKA'                                                456
         AG_EXNAME = 'POSITION'                                           456
         AG_PARLIST = 'NONE'                                              456
         CALL AXPOSITION                                                  456
C    *                                                                    456
5341  CALL AGSPOPB                                                        457
      GO TO BLOCK_TOFS, (5340 ,5350)                                      457
C End of Local Procedure                                                  457
C ----------------------------------------------------------------------  458
C ------------ TOKA                                                       458
C Begin Local Procedure TOKA                                              459
C TOKA IS KAPTON LAYER                                                    459
5380  CONTINUE                                                            459
         AG_TITLE='TOKA'                                                  459
         AG_MODULE=MODULE                                                 459
         AG_MARK='V00'                                                    459
         CALL AGSPUSH                                                     459
         AG_MODULE=MODULE                                                 459
C    *                                                                    459
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            460
         CALL AGSRESET                                                    460
         AG_TITLE = 'MYLAR'                                               460
         AG_EXNAME = 'MATERIAL'                                           460
         AG_PARLIST = 'NONE'                                              460
         CALL AXMATERIAL                                                  460
C    *                                                                    460
C    Geant ATTRIBUTE : TOKA ===> SEEN=1 COLO=3 <===                       460
         CALL AGSRESET                                                    461
         AG_SEEN = 1                                                      461
         AG_COLO = 3                                                      461
         AG_TITLE = 'TOKA'                                                461
         AG_EXNAME = 'ATTRIBUTE'                                          461
         AG_PARLIST = 'SEEN_COLO'                                         461
         CALL AXATTRIBUTE                                                 461
C    *                                                                    461
C    Geant SHAPE : TUBE ===> RMIN=TOKAIR <===                             461
         CALL AGSRESET                                                    462
         AG_RMIN = TOKAIR                                                 462
         AG_TITLE = 'TUBE'                                                462
         AG_EXNAME = 'SHAPE'                                              462
         AG_PARLIST = 'RMIN'                                              462
         CALL AXSHAPE                                                     462
C    *                                                                    462
C    Create TONX :                                                        463
C    Call To Local Procedure TONX                                         463
         ASSIGN 5410 TO BLOCK_TONX                                        463
         GO TO  5400                                                      463
5410     CONTINUE                                                         463
C    *                                                                    463
C    Geant POSITION : TONX ===> inherited arguments used <===             464
         CALL AGSRESET                                                    464
         AG_TITLE = 'TONX'                                                464
         AG_EXNAME = 'POSITION'                                           464
         AG_PARLIST = 'NONE'                                              464
         CALL AXPOSITION                                                  464
C    *                                                                    464
5381  CALL AGSPOPB                                                        465
      GO TO BLOCK_TOKA, (5380 ,5390)                                      465
C End of Local Procedure                                                  465
C ----------------------------------------------------------------------  466
C ------------ TONX                                                       466
C Begin Local Procedure TONX                                              467
C TONX IS NOMEX SUPPORT                                                   467
5400  CONTINUE                                                            467
         AG_TITLE='TONX'                                                  467
         AG_MODULE=MODULE                                                 467
         AG_MARK='V00'                                                    467
         CALL AGSPUSH                                                     467
         AG_MODULE=MODULE                                                 467
C    *                                                                    467
C    Geant MATERIAL : NOMEX ===> inherited arguments used <===            468
         CALL AGSRESET                                                    468
         AG_TITLE = 'NOMEX'                                               468
         AG_EXNAME = 'MATERIAL'                                           468
         AG_PARLIST = 'NONE'                                              468
         CALL AXMATERIAL                                                  468
C    *                                                                    468
C    Geant ATTRIBUTE : TONX ===> SEEN=1 COLO=2 <===                       468
         CALL AGSRESET                                                    469
         AG_SEEN = 1                                                      469
         AG_COLO = 2                                                      469
         AG_TITLE = 'TONX'                                                469
         AG_EXNAME = 'ATTRIBUTE'                                          469
         AG_PARLIST = 'SEEN_COLO'                                         469
         CALL AXATTRIBUTE                                                 469
C    *                                                                    469
C    Geant SHAPE : TUBE ===> RMIN=TONXIR <===                             469
         CALL AGSRESET                                                    470
         AG_RMIN = TONXIR                                                 470
         AG_TITLE = 'TUBE'                                                470
         AG_EXNAME = 'SHAPE'                                              470
         AG_PARLIST = 'RMIN'                                              470
         CALL AXSHAPE                                                     470
C    *                                                                    470
C    Create TOAD :                                                        471
C    Call To Local Procedure TOAD                                         471
         ASSIGN 5430 TO BLOCK_TOAD                                        471
         GO TO  5420                                                      471
5430     CONTINUE                                                         471
C    *                                                                    471
C    Geant POSITION : TOAD ===> inherited arguments used <===             472
         CALL AGSRESET                                                    472
         AG_TITLE = 'TOAD'                                                472
         AG_EXNAME = 'POSITION'                                           472
         AG_PARLIST = 'NONE'                                              472
         CALL AXPOSITION                                                  472
C    *                                                                    472
5401  CALL AGSPOPB                                                        473
      GO TO BLOCK_TONX, (5400 ,5410)                                      473
C End of Local Procedure                                                  473
C ----------------------------------------------------------------------  474
C ------------ TOAD                                                       474
C Begin Local Procedure TOAD                                              475
C TOAD IS ADHESIVE LAYER                                                  475
5420  CONTINUE                                                            475
         AG_TITLE='TOAD'                                                  475
         AG_MODULE=MODULE                                                 475
         AG_MARK='V00'                                                    475
         CALL AGSPUSH                                                     475
         AG_MODULE=MODULE                                                 475
C    *                                                                    475
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            476
         CALL AGSRESET                                                    476
         AG_TITLE = 'MYLAR'                                               476
         AG_EXNAME = 'MATERIAL'                                           476
         AG_PARLIST = 'NONE'                                              476
         CALL AXMATERIAL                                                  476
C    *                                                                    476
C    Geant ATTRIBUTE : TOAD ===> SEEN=1 COLO=7 <===                       476
         CALL AGSRESET                                                    477
         AG_SEEN = 1                                                      477
         AG_COLO = 7                                                      477
         AG_TITLE = 'TOAD'                                                477
         AG_EXNAME = 'ATTRIBUTE'                                          477
         AG_PARLIST = 'SEEN_COLO'                                         477
         CALL AXATTRIBUTE                                                 477
C    *                                                                    477
C    Geant SHAPE : TUBE ===> RMIN=TOADIR <===                             477
         CALL AGSRESET                                                    478
         AG_RMIN = TOADIR                                                 478
         AG_TITLE = 'TUBE'                                                478
         AG_EXNAME = 'SHAPE'                                              478
         AG_PARLIST = 'RMIN'                                              478
         CALL AXSHAPE                                                     478
C    *                                                                    478
5421  CALL AGSPOPB                                                        479
      GO TO BLOCK_TOAD, (5420 ,5430)                                      479
C End of Local Procedure                                                  479
C ----------------------------------------------------------------------  480
C ------------ TOST                                                       480
C Begin Local Procedure TOST                                              481
C TOST IS THE OUTER FIELD CAGE SUPPORT                                    481
5360  CONTINUE                                                            481
         AG_TITLE='TOST'                                                  481
         AG_MODULE=MODULE                                                 481
         AG_MARK='V00'                                                    481
         CALL AGSPUSH                                                     481
         AG_MODULE=MODULE                                                 481
C    *                                                                    481
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        482
         CALL AGSRESET                                                    482
         AG_TITLE = 'ALUMINIUM'                                           482
         AG_EXNAME = 'MATERIAL'                                           482
         AG_PARLIST = 'NONE'                                              482
         CALL AXMATERIAL                                                  482
C    *                                                                    482
C    Geant ATTRIBUTE : TOST ===> SEEN=1 COLO=2 <===                       482
         CALL AGSRESET                                                    483
         AG_SEEN = 1                                                      483
         AG_COLO = 2                                                      483
         AG_TITLE = 'TOST'                                                483
         AG_EXNAME = 'ATTRIBUTE'                                          483
         AG_PARLIST = 'SEEN_COLO'                                         483
         CALL AXATTRIBUTE                                                 483
C    *                                                                    483
C    Geant SHAPE : TUBE ===> RMIN=TOALIR <===                             483
         CALL AGSRESET                                                    484
         AG_RMIN = TOALIR                                                 484
         AG_TITLE = 'TUBE'                                                484
         AG_EXNAME = 'SHAPE'                                              484
         AG_PARLIST = 'RMIN'                                              484
         CALL AXSHAPE                                                     484
C    *                                                                    484
C    Create TOHA :                                                        485
C    Call To Local Procedure TOHA                                         485
         ASSIGN 5450 TO BLOCK_TOHA                                        485
         GO TO  5440                                                      485
5450     CONTINUE                                                         485
C    *                                                                    485
C    Geant POSITION : TOHA ===> inherited arguments used <===             486
         CALL AGSRESET                                                    486
         AG_TITLE = 'TOHA'                                                486
         AG_EXNAME = 'POSITION'                                           486
         AG_PARLIST = 'NONE'                                              486
         CALL AXPOSITION                                                  486
C    *                                                                    486
5361  CALL AGSPOPB                                                        487
      GO TO BLOCK_TOST, (5360 ,5370)                                      487
C End of Local Procedure                                                  487
C ----------------------------------------------------------------------  488
C ------------ TOHA                                                       488
C Begin Local Procedure TOHA                                              489
C TOHA IS HONEYCOMB/ADHESIVE MIXTURE                                      489
5440  CONTINUE                                                            489
         AG_TITLE='TOHA'                                                  489
         AG_MODULE=MODULE                                                 489
         AG_MARK='V00'                                                    489
         CALL AGSPUSH                                                     489
         AG_MODULE=MODULE                                                 489
C    *                                                                    489
C    Geant COMPONENT : AL ===> A=27 Z=13 W=0.0105 <===                    490
         CALL AGSRESET                                                    490
         AG_A = 27                                                        490
         AG_Z = 13                                                        490
         AG_W = 0.0105                                                    490
         AG_TITLE = 'AL'                                                  490
         AG_EXNAME = 'COMPONENT'                                          490
         AG_PARLIST = 'A_Z_W'                                             490
         CALL AXCOMPONENT                                                 490
C    *                                                                    490
C    Geant COMPONENT : N ===> A=14 Z=7 W=0.7395 <===                      490
         CALL AGSRESET                                                    491
         AG_A = 14                                                        491
         AG_Z = 7                                                         491
         AG_W = 0.7395                                                    491
         AG_TITLE = 'N'                                                   491
         AG_EXNAME = 'COMPONENT'                                          491
         AG_PARLIST = 'A_Z_W'                                             491
         CALL AXCOMPONENT                                                 491
C    *                                                                    491
C    Geant COMPONENT : ADHESIVE ===> A=9 Z=4.5 W=0.2500 <===              491
         CALL AGSRESET                                                    492
         AG_A = 9                                                         492
         AG_Z = 4.5                                                       492
         AG_W = 0.2500                                                    492
         AG_TITLE = 'ADHESIVE'                                            492
         AG_EXNAME = 'COMPONENT'                                          492
         AG_PARLIST = 'A_Z_W'                                             492
         CALL AXCOMPONENT                                                 492
C    *                                                                    492
C    Geant MIXTURE : AL_HONEYCOMB ===> DENS=0.282 <===                    492
         CALL AGSRESET                                                    493
         AG_DENS = 0.282                                                  493
         AG_TITLE = 'AL_HONEYCOMB'                                        493
         AG_EXNAME = 'MIXTURE'                                            493
         AG_PARLIST = 'DENS'                                              493
         CALL AXMIXTURE                                                   493
C    *                                                                    493
C    Geant ATTRIBUTE : TOHA ===> SEEN=1 COLO=7 <===                       493
         CALL AGSRESET                                                    494
         AG_SEEN = 1                                                      494
         AG_COLO = 7                                                      494
         AG_TITLE = 'TOHA'                                                494
         AG_EXNAME = 'ATTRIBUTE'                                          494
         AG_PARLIST = 'SEEN_COLO'                                         494
         CALL AXATTRIBUTE                                                 494
C    *                                                                    494
C    Geant SHAPE : TUBE ===> RMIN=TOHAIR <===                             494
         CALL AGSRESET                                                    495
         AG_RMIN = TOHAIR                                                 495
         AG_TITLE = 'TUBE'                                                495
         AG_EXNAME = 'SHAPE'                                              495
         AG_PARLIST = 'RMIN'                                              495
         CALL AXSHAPE                                                     495
C    *                                                                    495
5441  CALL AGSPOPB                                                        496
      GO TO BLOCK_TOHA, (5440 ,5450)                                      496
C End of Local Procedure                                                  496
C ----------------------------------------------------------------------  497
C ------------ TPCM                                                       497
C Begin Local Procedure TPCM                                              498
C TPCM IS THE CENTRAL MEMBRANE PLACED IN TPC                              498
5120  CONTINUE                                                            498
         AG_TITLE='TPCM'                                                  498
         AG_MODULE=MODULE                                                 498
         AG_MARK='V00'                                                    498
         CALL AGSPUSH                                                     498
         AG_MODULE=MODULE                                                 498
C    *                                                                    498
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            499
         CALL AGSRESET                                                    499
         AG_TITLE = 'MYLAR'                                               499
         AG_EXNAME = 'MATERIAL'                                           499
         AG_PARLIST = 'NONE'                                              499
         CALL AXMATERIAL                                                  499
C    *                                                                    499
C    Geant ATTRIBUTE : TPCM ===> SEEN=1 COLO=4 <===                       499
         CALL AGSRESET                                                    500
         AG_SEEN = 1                                                      500
         AG_COLO = 4                                                      500
         AG_TITLE = 'TPCM'                                                500
         AG_EXNAME = 'ATTRIBUTE'                                          500
         AG_PARLIST = 'SEEN_COLO'                                         500
         CALL AXATTRIBUTE                                                 500
C    *                                                                    500
C    Geant SHAPE : TUBE ===> RMIN=TPGVIR RMAX=TPCG_SENGASOR, DZ=TPCG_MEM  500
C    BTHK/2 <===                                                          500
         CALL AGSRESET                                                    502
         AG_RMIN = TPGVIR                                                 502
         AG_RMAX = TPCG_SENGASOR                                          502
         AG_DZ = TPCG_MEMBTHK/2                                           502
         AG_TITLE = 'TUBE'                                                502
         AG_EXNAME = 'SHAPE'                                              502
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      502
         CALL AXSHAPE                                                     502
C    *                                                                    502
5121  CALL AGSPOPB                                                        503
      GO TO BLOCK_TPCM, (5120 ,5130)                                      503
C End of Local Procedure                                                  503
C ----------------------------------------------------------------------  504
C ------------ TPEA                                                       504
C Begin Local Procedure TPEA                                              505
C TPEA IS ONE ENDCAP PLACED IN TPC                                        505
5140  CONTINUE                                                            505
         AG_TITLE='TPEA'                                                  505
         AG_MODULE=MODULE                                                 505
         AG_MARK='V00'                                                    505
         CALL AGSPUSH                                                     505
         AG_MODULE=MODULE                                                 505
C    *                                                                    505
C    Geant MATERIAL : P10 ===> inherited arguments used <===              506
         CALL AGSRESET                                                    506
         AG_TITLE = 'P10'                                                 506
         AG_EXNAME = 'MATERIAL'                                           506
         AG_PARLIST = 'NONE'                                              506
         CALL AXMATERIAL                                                  506
C    *                                                                    506
C    Geant ATTRIBUTE : TPEA ===> SEEN=0 <===                              506
         CALL AGSRESET                                                    507
         AG_SEEN = 0                                                      507
         AG_TITLE = 'TPEA'                                                507
         AG_EXNAME = 'ATTRIBUTE'                                          507
         AG_PARLIST = 'SEEN'                                              507
         CALL AXATTRIBUTE                                                 507
C    *                                                                    507
C    Geant SHAPE : TUBE ===> RMIN=TPGVIR RMAX=TPCG_SENGASOR, DZ=TPCG_TPE  507
C    ATHK/2 <===                                                          507
         CALL AGSRESET                                                    509
         AG_RMIN = TPGVIR                                                 509
         AG_RMAX = TPCG_SENGASOR                                          509
         AG_DZ = TPCG_TPEATHK/2                                           509
         AG_TITLE = 'TUBE'                                                509
         AG_EXNAME = 'SHAPE'                                              509
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      509
         CALL AXSHAPE                                                     509
C    *                                                                    509
C    Create TESS :                                                        510
C    Call To Local Procedure TESS                                         510
         ASSIGN 5470 TO BLOCK_TESS                                        510
         GO TO  5460                                                      510
5470     CONTINUE                                                         510
C    *                                                                    510
5141  CALL AGSPOPB                                                        511
      GO TO BLOCK_TPEA, (5140 ,5150)                                      511
C End of Local Procedure                                                  511
C ----------------------------------------------------------------------  512
C ------------ TESS                                                       512
C Begin Local Procedure TESS                                              513
C TESS IS A DIVISION OF ENDCAP VOLUME CORRESPONDING TO ONE SUPERSECTOR    513
5460  CONTINUE                                                            513
         AG_TITLE='TESS'                                                  513
         AG_MODULE=MODULE                                                 513
         AG_MARK='V00'                                                    513
         CALL AGSPUSH                                                     513
         AG_MODULE=MODULE                                                 513
C    *                                                                    513
C    Geant ATTRIBUTE : TESS ===> SEEN=1 <===                              514
         CALL AGSRESET                                                    514
         AG_SEEN = 1                                                      514
         AG_TITLE = 'TESS'                                                514
         AG_EXNAME = 'ATTRIBUTE'                                          514
         AG_PARLIST = 'SEEN'                                              514
         CALL AXATTRIBUTE                                                 514
C    *                                                                    514
C    Geant SHAPE : DIVISION ===> NDIV=12 IAXIS=2 <===                     514
         CALL AGSRESET                                                    515
         AG_NDIV = 12                                                     515
         AG_IAXIS = 2                                                     515
         AG_TITLE = 'DIVISION'                                            515
         AG_EXNAME = 'SHAPE'                                              515
         AG_PARLIST = 'NDIV_IAXIS'                                        515
         CALL AXSHAPE                                                     515
C    *                                                                    515
C    *                                                                    515
         I_SEC=1                                                          516
            GO TO 5483                                                    516
5481        I_SEC=I_SEC+1                                                 516
5483        IF(I_SEC-(2).GT.0) GO TO 5482                                 516
C       use TECW bank                                                     517
            BPATH_TECW='TECW*'                                            517
            IDNUM_TECW(1)=0                                               517
            TECW_SEC=I_SEC                                                517
            CALL AGDGETP('TPCEGEO2','*',-999,'TECW',NTECW_SEC,            517
     *      RE_TECW_SEC,BPATH_TECW,IDNUM_TECW,1,MAP_TECW,NAMES_TECW,      517
     *      BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)             517
C       use TROV bank                                                     518
            BPATH_TROV='TROV*'                                            518
            IDNUM_TROV(1)=0                                               518
            TROV_SEC=I_SEC                                                518
            CALL AGDGETP('TPCEGEO2','*',-999,'TROV',NTROV_SEC,            518
     *      RE_TROV_SEC,BPATH_TROV,IDNUM_TROV,1,MAP_TROV,NAMES_TROV,      518
     *      BANK_TROV,LEN_TROV,LLEN_TROV,LINK_TROV,FLAG_TROV)             518
C       Create TSEC :                                                     519
C       Call To Local Procedure TSEC                                      519
            ASSIGN 5500 TO BLOCK_TSEC                                     519
            GO TO  5490                                                   519
5500        CONTINUE                                                      519
C       *                                                                 519
C       Geant POSITION : TSEC ===> X=TECW_RCENTER Z=(TPCG_TPEATHK-TECW_A  521
C       SDEPTH-TECW_PPDEPTH-TECW_GGDEPTH)/2 ORT=YZX <===                  521
            CALL AGSRESET                                                 521
            AG_X = TECW_RCENTER                                           521
            AG_Z =                                                        521
     *      (TPCG_TPEATHK-TECW_ASDEPTH-TECW_PPDEPTH-TECW_GGDEPTH)/2       521
            AG_ALPHAN=AG_ALPHAN+1                                         521
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              521
            AG_TITLE = 'TSEC'                                             521
            AG_EXNAME = 'POSITION'                                        521
            AG_PARLIST = 'X_Z_ORT'                                        521
            CALL AXPOSITION                                               521
C       *                                                                 521
         GO TO 5481                                                       522
5482     CONTINUE                                                         522
5461  CALL AGSPOPB                                                        523
      GO TO BLOCK_TESS, (5460 ,5470)                                      523
C End of Local Procedure                                                  523
C ----------------------------------------------------------------------  524
C ------------ TSEC                                                       524
C Begin Local Procedure TSEC                                              525
C TSEC IS A SUPERSECTOR CONTAINING AL SECTOR, PCB AND MWC VOLUME          525
5490  CONTINUE                                                            525
         AG_TITLE='TSEC'                                                  525
         AG_MODULE=MODULE                                                 525
         AG_MARK='V00'                                                    525
         CALL AGSPUSH                                                     525
         AG_MODULE=MODULE                                                 525
C    *                                                                    525
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        526
         CALL AGSRESET                                                    526
         AG_TITLE = 'ALUMINIUM'                                           526
         AG_EXNAME = 'MATERIAL'                                           526
         AG_PARLIST = 'NONE'                                              526
         CALL AXMATERIAL                                                  526
C    *                                                                    526
C    Geant SHAPE : TRD1 ===> DX1=TECW_INWIDTH/2 DX2=TECW_OUWIDTH/2, DY=(  526
C    TECW_PPDEPTH+TECW_ASDEPTH+TECW_GGDEPTH)/2, DZ=TECW_HEIGHT/2 <===     526
         CALL AGSRESET                                                    529
         AG_DX1 = TECW_INWIDTH/2                                          529
         AG_DX2 = TECW_OUWIDTH/2                                          529
         AG_DY = (TECW_PPDEPTH+TECW_ASDEPTH+TECW_GGDEPTH)/2               529
         AG_DZ = TECW_HEIGHT/2                                            529
         AG_TITLE = 'TRD1'                                                529
         AG_EXNAME = 'SHAPE'                                              529
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     529
         CALL AXSHAPE                                                     529
C    *                                                                    529
C    Create TMWC :                                                        530
C    Call To Local Procedure TMWC                                         530
         ASSIGN 5520 TO BLOCK_TMWC                                        530
         GO TO  5510                                                      530
5520     CONTINUE                                                         530
C    *                                                                    530
C    Geant POSITION : TMWC ===> Y=-(TECW_PPDEPTH+TECW_ASDEPTH)/2 <===     531
         CALL AGSRESET                                                    531
         AG_Y = -(TECW_PPDEPTH+TECW_ASDEPTH)/2                            531
         AG_TITLE = 'TMWC'                                                531
         AG_EXNAME = 'POSITION'                                           531
         AG_PARLIST = 'Y'                                                 531
         CALL AXPOSITION                                                  531
C    *                                                                    531
C    Create TIAG :                                                        532
C    Call To Local Procedure TIAG                                         532
         ASSIGN 5540 TO BLOCK_TIAG                                        532
         GO TO  5530                                                      532
5540     CONTINUE                                                         532
C    *                                                                    532
         DX_DZ =(TECW_OUWIDTH - TECW_INWIDTH)/(2*TECW_HEIGHT)             533
         DXB = TECW_BOUNDARY*SQRT(1. + DX_DZ*DX_DZ)                       534
C    *                                                                    535
         I=1                                                              535
            GO TO 5553                                                    535
5551        I=I+1                                                         535
5553        IF(I-(NINT(TECW_N)).GT.0) GO TO 5552                          535
C       Geant POSITION : TIAG ===> Y=+TECW_PPDEPTH/2+TECW_GGDEPTH/2 Z=-T  539
C       ECW_HEIGHT/2+TECW_Z(I) DX2=TECW_INWIDTH/2-DXB+(TECW_Z(I)+TECW_DZ  539
C       (I)/2)*DX_DZ DX1=TECW_INWIDTH/2-DXB+(TECW_Z(I)-TECW_DZ(I)/2)*DX_  539
C       DZ DZ=TECW_DZ(I)/2 DY=TECW_ASDEPTH/2 KONLY='MANY' <===            539
            CALL AGSRESET                                                 539
            AG_Y = +TECW_PPDEPTH/2+TECW_GGDEPTH/2                         539
            AG_Z = -TECW_HEIGHT/2+TECW_Z(I)                               539
            AG_DX2 = TECW_INWIDTH/2-DXB+(TECW_Z(I)+TECW_DZ(I)/2)*DX_DZ    539
            AG_DX1 = TECW_INWIDTH/2-DXB+(TECW_Z(I)-TECW_DZ(I)/2)*DX_DZ    539
            AG_DZ = TECW_DZ(I)/2                                          539
            AG_DY = TECW_ASDEPTH/2                                        539
            AG_KONLY = 'MANY'                                             539
            AG_TITLE = 'TIAG'                                             539
            AG_EXNAME = 'POSITION'                                        539
            AG_PARLIST = 'Y_Z_DX2_DX1_DZ_DY_KONLY'                        539
            CALL AXPOSITION                                               539
C       *                                                                 539
         GO TO 5551                                                       540
5552     CONTINUE                                                         540
C    Create TCEX :                                                        541
C    Call To Local Procedure TCEX                                         541
         ASSIGN 5570 TO BLOCK_TCEX                                        541
         GO TO  5560                                                      541
5570     CONTINUE                                                         541
C    *                                                                    541
C    Create TAEC :                                                        542
C    Call To Local Procedure TAEC                                         542
         ASSIGN 5590 TO BLOCK_TAEC                                        542
         GO TO  5580                                                      542
5590     CONTINUE                                                         542
C    *                                                                    542
C    Create THOL :                                                        543
C    Call To Local Procedure THOL                                         543
         ASSIGN 5610 TO BLOCK_THOL                                        543
         GO TO  5600                                                      543
5610     CONTINUE                                                         543
C    *                                                                    543
C    Create THRA :                                                        544
C    Call To Local Procedure THRA                                         544
         ASSIGN 5630 TO BLOCK_THRA                                        544
         GO TO  5620                                                      544
5630     CONTINUE                                                         544
C    *                                                                    544
C    Create THLA :                                                        545
C    Call To Local Procedure THLA                                         545
         ASSIGN 5650 TO BLOCK_THLA                                        545
         GO TO  5640                                                      545
5650     CONTINUE                                                         545
C    *                                                                    545
C    *                                                                    546
         I_NHP=1                                                          546
            GO TO 5663                                                    546
5661        I_NHP=I_NHP+1                                                 546
5663        IF(I_NHP-(16).GT.0) GO TO 5662                                546
C       FIRST PL                                                          547
            HPLA = -TECW_HEIGHT/2+TECW_Z(1)-TECW_DZ(1)/2+TROV_OFFCRDIO    547
            ZVALUE = HPLA+4.0*(I_NHP-1)                                   548
C       Check TROV_NHP(I_NHP)>0                                           549
            IF (TROV_NHP(I_NHP).GT.0) THEN                                549
C       *                                                                 550
            J_NHO=1                                                       550
               GO TO 5673                                                 550
5671           J_NHO=J_NHO+1                                              550
5673           IF(J_NHO-(TROV_NHP(I_NHP)).GT.0) GO TO 5672                550
C          Geant POSITION : TCEX ===> X=TROV_HX(J_NHO,I_NHP) Y=+TECW_PPD  552
C          EPTH/2+TECW_GGDEPTH/2 Z=ZVALUE+TECW_CARDOFF <===               552
               CALL AGSRESET                                              552
               AG_X = TROV_HX(J_NHO,I_NHP)                                552
               AG_Y = +TECW_PPDEPTH/2+TECW_GGDEPTH/2                      552
               AG_Z = ZVALUE+TECW_CARDOFF                                 552
               AG_TITLE = 'TCEX'                                          552
               AG_EXNAME = 'POSITION'                                     552
               AG_PARLIST = 'X_Y_Z'                                       552
               CALL AXPOSITION                                            552
C          *                                                              552
C          Geant POSITION : TAEC ===> X=TROV_HX(J_NHO,I_NHP) Y=+TECW_PPD  552
C          EPTH/2+TECW_GGDEPTH/2 Z=ZVALUE+TECW_COOLOFF <===               552
               CALL AGSRESET                                              554
               AG_X = TROV_HX(J_NHO,I_NHP)                                554
               AG_Y = +TECW_PPDEPTH/2+TECW_GGDEPTH/2                      554
               AG_Z = ZVALUE+TECW_COOLOFF                                 554
               AG_TITLE = 'TAEC'                                          554
               AG_EXNAME = 'POSITION'                                     554
               AG_PARLIST = 'X_Y_Z'                                       554
               CALL AXPOSITION                                            554
C          *                                                              554
C          Geant POSITION : THOL ===> X=TROV_HX(J_NHO,I_NHP) Y=+TECW_GGD  554
C          EPTH/2-TECW_ASDEPTH/2 Z=ZVALUE+TECW_CARDOFF <===               554
               CALL AGSRESET                                              556
               AG_X = TROV_HX(J_NHO,I_NHP)                                556
               AG_Y = +TECW_GGDEPTH/2-TECW_ASDEPTH/2                      556
               AG_Z = ZVALUE+TECW_CARDOFF                                 556
               AG_TITLE = 'THOL'                                          556
               AG_EXNAME = 'POSITION'                                     556
               AG_PARLIST = 'X_Y_Z'                                       556
               CALL AXPOSITION                                            556
C          *                                                              556
C          Geant POSITION : THRA ===> X=TROV_HX(J_NHO,I_NHP)+TECW_SLOTW/  556
C          2 Y=+TECW_GGDEPTH/2-TECW_ASDEPTH/2 Z=ZVALUE+TECW_CARDOFF ORT=  556
C          ZXY <===                                                       556
               CALL AGSRESET                                              558
               AG_X = TROV_HX(J_NHO,I_NHP)+TECW_SLOTW/2                   558
               AG_Y = +TECW_GGDEPTH/2-TECW_ASDEPTH/2                      558
               AG_Z = ZVALUE+TECW_CARDOFF                                 558
               AG_ALPHAN=AG_ALPHAN+1                                      558
               AG_ALPHAK(AG_ALPHAN)=IGOR('ZXY')                           558
               AG_TITLE = 'THRA'                                          558
               AG_EXNAME = 'POSITION'                                     558
               AG_PARLIST = 'X_Y_Z_ORT'                                   558
               CALL AXPOSITION                                            558
C          *                                                              558
C          Geant POSITION : THLA ===> X=TROV_HX(J_NHO,I_NHP)-TECW_SLOTW/  558
C          2 Y=+TECW_GGDEPTH/2-TECW_ASDEPTH/2 Z=ZVALUE+TECW_CARDOFF ORT=  558
C          ZXY <===                                                       558
               CALL AGSRESET                                              560
               AG_X = TROV_HX(J_NHO,I_NHP)-TECW_SLOTW/2                   560
               AG_Y = +TECW_GGDEPTH/2-TECW_ASDEPTH/2                      560
               AG_Z = ZVALUE+TECW_CARDOFF                                 560
               AG_ALPHAN=AG_ALPHAN+1                                      560
               AG_ALPHAK(AG_ALPHAN)=IGOR('ZXY')                           560
               AG_TITLE = 'THLA'                                          560
               AG_EXNAME = 'POSITION'                                     560
               AG_PARLIST = 'X_Y_Z_ORT'                                   560
               CALL AXPOSITION                                            560
C          *                                                              560
            GO TO 5671                                                    561
5672        CONTINUE                                                      561
            ENDIF                                                         562
         GO TO 5661                                                       563
5662     CONTINUE                                                         563
C    Create TOAE :                                                        564
C    Call To Local Procedure TOAE                                         564
         ASSIGN 5690 TO BLOCK_TOAE                                        564
         GO TO  5680                                                      564
5690     CONTINUE                                                         564
C    *                                                                    564
C    *                                                                    565
         I=1                                                              565
            GO TO 5703                                                    565
5701        I=I+1                                                         565
5703        IF(I-(NINT(TECW_NEX)).GT.0) GO TO 5702                        565
C       Geant POSITION : TOAE ===> X=TECW_XEX(I) Y=+TECW_PPDEPTH/2+TECW_  568
C       GGDEPTH/2 Z=TECW_ZEX(I)-TECW_HEIGHT/2 DZ=TECW_DZEX(I)/2 DX=TECW_  568
C       DXEX(I)/2 DY=TECW_ASDEPTH/2 <===                                  568
            CALL AGSRESET                                                 568
            AG_X = TECW_XEX(I)                                            568
            AG_Y = +TECW_PPDEPTH/2+TECW_GGDEPTH/2                         568
            AG_Z = TECW_ZEX(I)-TECW_HEIGHT/2                              568
            AG_DZ = TECW_DZEX(I)/2                                        568
            AG_DX = TECW_DXEX(I)/2                                        568
            AG_DY = TECW_ASDEPTH/2                                        568
            AG_TITLE = 'TOAE'                                             568
            AG_EXNAME = 'POSITION'                                        568
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 568
            CALL AXPOSITION                                               568
C       *                                                                 568
         GO TO 5701                                                       569
5702     CONTINUE                                                         569
5491  CALL AGSPOPB                                                        570
      GO TO BLOCK_TSEC, (5490 ,5500)                                      570
C End of Local Procedure                                                  570
C ----------------------------------------------------------------------  571
C ------------ TMWC                                                       571
C Begin Local Procedure TMWC                                              572
C TMWC IS A WIRE CHAMBER VOLUME WITH BOTH GATED AND SENSITIVE VOLUMES     572
5510  CONTINUE                                                            572
         AG_TITLE='TMWC'                                                  572
         AG_MODULE=MODULE                                                 572
         AG_MARK='V00'                                                    572
         CALL AGSPUSH                                                     572
         AG_MODULE=MODULE                                                 572
C    *                                                                    572
C    Geant MATERIAL : P10 ===> inherited arguments used <===              573
         CALL AGSRESET                                                    573
         AG_TITLE = 'P10'                                                 573
         AG_EXNAME = 'MATERIAL'                                           573
         AG_PARLIST = 'NONE'                                              573
         CALL AXMATERIAL                                                  573
C    *                                                                    573
C    Geant ATTRIBUTE : TMWC ===> SEEN=1 COLO=3 <===                       573
         CALL AGSRESET                                                    574
         AG_SEEN = 1                                                      574
         AG_COLO = 3                                                      574
         AG_TITLE = 'TMWC'                                                574
         AG_EXNAME = 'ATTRIBUTE'                                          574
         AG_PARLIST = 'SEEN_COLO'                                         574
         CALL AXATTRIBUTE                                                 574
C    *                                                                    574
C    Geant SHAPE : TRD1 ===> DX1=TECW_INWIDTH/2 DX2=TECW_OUWIDTH/2, DY=T  574
C    ECW_GGDEPTH/2 DZ=TECW_HEIGHT/2 <===                                  574
         CALL AGSRESET                                                    576
         AG_DX1 = TECW_INWIDTH/2                                          576
         AG_DX2 = TECW_OUWIDTH/2                                          576
         AG_DY = TECW_GGDEPTH/2                                           576
         AG_DZ = TECW_HEIGHT/2                                            576
         AG_TITLE = 'TRD1'                                                576
         AG_EXNAME = 'SHAPE'                                              576
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     576
         CALL AXSHAPE                                                     576
C    *                                                                    576
C    Skip Unless TPCG_MWCREAD>0                                           576
         IF (TPCG_MWCREAD.LE.0)GO TO 5511                                 577
C    Create TMEA :                                                        578
C    Call To Local Procedure TMEA                                         578
         ASSIGN 5720 TO BLOCK_TMEA                                        578
         GO TO  5710                                                      578
5720     CONTINUE                                                         578
C    *                                                                    578
C    Geant POSITION : TMEA ===> Y=+(TECW_GGDEPTH-TECW_MWCDEPTH)/2 Z=TECW  579
C    _MWCCENT <===                                                        579
         CALL AGSRESET                                                    579
         AG_Y = +(TECW_GGDEPTH-TECW_MWCDEPTH)/2                           579
         AG_Z = TECW_MWCCENT                                              579
         AG_TITLE = 'TMEA'                                                579
         AG_EXNAME = 'POSITION'                                           579
         AG_PARLIST = 'Y_Z'                                               579
         CALL AXPOSITION                                                  579
C    *                                                                    579
5511  CALL AGSPOPB                                                        580
      GO TO BLOCK_TMWC, (5510 ,5520)                                      580
C End of Local Procedure                                                  580
C ----------------------------------------------------------------------  581
C ------------ TMEA                                                       581
C Begin Local Procedure TMEA                                              582
C TMEA IS A DOUBLE SENSITIVE LAYER AROUND GATING GRID                     582
5710  CONTINUE                                                            582
         AG_TITLE='TMEA'                                                  582
         AG_MODULE=MODULE                                                 582
         AG_MARK='V00'                                                    582
         CALL AGSPUSH                                                     582
         AG_MODULE=MODULE                                                 582
C    *                                                                    582
C    Geant MATERIAL : P10 ===> inherited arguments used <===              583
         CALL AGSRESET                                                    583
         AG_TITLE = 'P10'                                                 583
         AG_EXNAME = 'MATERIAL'                                           583
         AG_PARLIST = 'NONE'                                              583
         CALL AXMATERIAL                                                  583
C    *                                                                    583
C    Geant MATERIAL : SENSITIVE_GAS ===> ISVOL=1 STEMAX=5 <===            583
         CALL AGSRESET                                                    584
         AG_ISVOL = 1                                                     584
         AG_STEMAX = 5                                                    584
         AG_TITLE = 'SENSITIVE_GAS'                                       584
         AG_EXNAME = 'MATERIAL'                                           584
         AG_PARLIST = 'ISVOL_STEMAX'                                      584
         CALL AXMATERIAL                                                  584
C    *                                                                    584
C    Geant ATTRIBUTE : TMEA ===> SEEN=1 COLO=4 <===                       584
         CALL AGSRESET                                                    585
         AG_SEEN = 1                                                      585
         AG_COLO = 4                                                      585
         AG_TITLE = 'TMEA'                                                585
         AG_EXNAME = 'ATTRIBUTE'                                          585
         AG_PARLIST = 'SEEN_COLO'                                         585
         CALL AXATTRIBUTE                                                 585
C    *                                                                    585
C    Geant SHAPE : TRD1 ===> DX1=TECW_MWCINN/2 DX2=TECW_MWCOUT/2, DY=TEC  585
C    W_MWCDEPTH/2 DZ=TECW_MWCHEI/2 <===                                   585
         CALL AGSRESET                                                    587
         AG_DX1 = TECW_MWCINN/2                                           587
         AG_DX2 = TECW_MWCOUT/2                                           587
         AG_DY = TECW_MWCDEPTH/2                                          587
         AG_DZ = TECW_MWCHEI/2                                            587
         AG_TITLE = 'TRD1'                                                587
         AG_EXNAME = 'SHAPE'                                              587
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     587
         CALL AXSHAPE                                                     587
C    *                                                                    587
C    Create TMSE :                                                        588
C    Call To Local Procedure TMSE                                         588
         ASSIGN 5740 TO BLOCK_TMSE                                        588
         GO TO  5730                                                      588
5740     CONTINUE                                                         588
C    *                                                                    588
         CALL GSTPAR(AG_IMED, 'STRA', 1.)                                 589
5711  CALL AGSPOPB                                                        590
      GO TO BLOCK_TMEA, (5710 ,5720)                                      590
C End of Local Procedure                                                  590
C ----------------------------------------------------------------------  591
C ------------ TMSE                                                       591
C Begin Local Procedure TMSE                                              592
C TMSE IS A SINGLE SENSITIVE VOLUME                                       592
5730  CONTINUE                                                            592
         AG_TITLE='TMSE'                                                  592
         AG_MODULE=MODULE                                                 592
         AG_MARK='V00'                                                    592
         CALL AGSPUSH                                                     592
         AG_MODULE=MODULE                                                 592
C    *                                                                    592
C    Geant SHAPE : DIVISION ===> NDIV=TECW_MWCNWIR IAXIS=3 <===           593
         CALL AGSRESET                                                    593
         AG_NDIV = TECW_MWCNWIR                                           593
         AG_IAXIS = 3                                                     593
         AG_TITLE = 'DIVISION'                                            593
         AG_EXNAME = 'SHAPE'                                              593
         AG_PARLIST = 'NDIV_IAXIS'                                        593
         CALL AXSHAPE                                                     593
C    *                                                                    593
C    Check TPCG_MWCREAD==1                                                593
         IF (TPCG_MWCREAD.EQ.1) THEN                                      594
C    Defining Hits: Z:0.01:S Y:0.01: Z:0.01:(-TECW_MWCOUT/2,+TECW_MWCOUT  597
C    /2) CX:10: CY:10:, CZ:10: SLENG:0.1:(0,800) STEP:0.1: TOF:16:(0,1.0  597
C    E-6) PTOT:16:(0,100) LGAM:16:(-2,+2), ELOS:16:(0.,0.01)              597
         AG_TITLE = 'TMSE'                                                597
         AG_EXNAME = 'HITS'                                               597
         AG_TYPE=AG_MAGIC                                                 597
         CALL AGSDETV('TMSE')                                             597
C    *                                                                    597
         AG_PARLIST='Z'                                                   597
         AG_TITLE = 'DETH'                                                597
         AG_NBITS = AG_MAGIC                                              597
         AG_BIN=0.01                                                      597
         AG_OPTION='S'                                                    597
         AG_FHMIN = AG_MAGIC                                              597
         AG_FHMAX = AG_MAGIC                                              597
         CALL AGSDETH (0)                                                 597
C    *                                                                    597
         AG_PARLIST='Y'                                                   597
         AG_TITLE = 'DETH'                                                597
         AG_NBITS = AG_MAGIC                                              597
         AG_BIN=0.01                                                      597
         AG_FHMIN=AG_MAGIC                                                597
         AG_FHMAX=AG_MAGIC                                                597
         AG_OPTION='*'                                                    597
         CALL AGSDETH (0)                                                 597
C    *                                                                    597
         AG_PARLIST='Z'                                                   597
         AG_TITLE = 'DETH'                                                597
         AG_NBITS = AG_MAGIC                                              597
         AG_BIN=0.01                                                      597
         AG_OPTION='*'                                                    597
         AG_FHMIN = -TECW_MWCOUT/2                                        597
         AG_FHMAX = +TECW_MWCOUT/2                                        597
         CALL AGSDETH (0)                                                 597
C    *                                                                    597
         AG_PARLIST='CX'                                                  597
         AG_TITLE = 'DETH'                                                597
         AG_NBITS = 10                                                    597
         AG_BIN=AG_MAGIC                                                  597
         AG_FHMIN=AG_MAGIC                                                597
         AG_FHMAX=AG_MAGIC                                                597
         AG_OPTION='*'                                                    597
         CALL AGSDETH (0)                                                 597
C    *                                                                    597
         AG_PARLIST='CY'                                                  597
         AG_TITLE = 'DETH'                                                597
         AG_NBITS = 10                                                    597
         AG_BIN=AG_MAGIC                                                  597
         AG_OPTION='*'                                                    597
         AG_FHMIN = AG_MAGIC                                              597
         AG_FHMAX = AG_MAGIC                                              597
         CALL AGSDETH (0)                                                 597
C    *                                                                    597
         AG_PARLIST='CZ'                                                  597
         AG_TITLE = 'DETH'                                                597
         AG_NBITS = 10                                                    597
         AG_BIN=AG_MAGIC                                                  597
         AG_FHMIN=AG_MAGIC                                                597
         AG_FHMAX=AG_MAGIC                                                597
         AG_OPTION='*'                                                    597
         CALL AGSDETH (0)                                                 597
C    *                                                                    597
         AG_PARLIST='SLENG'                                               597
         AG_TITLE = 'DETH'                                                597
         AG_NBITS = AG_MAGIC                                              597
         AG_BIN=0.1                                                       597
         AG_OPTION='*'                                                    597
         AG_FHMIN = 0                                                     597
         AG_FHMAX = 800                                                   597
         CALL AGSDETH (0)                                                 597
C    *                                                                    597
         AG_PARLIST='STEP'                                                597
         AG_TITLE = 'DETH'                                                597
         AG_NBITS = AG_MAGIC                                              597
         AG_BIN=0.1                                                       597
         AG_FHMIN=AG_MAGIC                                                597
         AG_FHMAX=AG_MAGIC                                                597
         AG_OPTION='*'                                                    597
         CALL AGSDETH (0)                                                 597
C    *                                                                    597
         AG_PARLIST='TOF'                                                 597
         AG_TITLE = 'DETH'                                                597
         AG_NBITS = 16                                                    597
         AG_BIN=AG_MAGIC                                                  597
         AG_OPTION='*'                                                    597
         AG_FHMIN = 0                                                     597
         AG_FHMAX = 1.0E-6                                                597
         CALL AGSDETH (0)                                                 597
C    *                                                                    597
         AG_PARLIST='PTOT'                                                597
         AG_TITLE = 'DETH'                                                597
         AG_NBITS = 16                                                    597
         AG_BIN=AG_MAGIC                                                  597
         AG_OPTION='*'                                                    597
         AG_FHMIN = 0                                                     597
         AG_FHMAX = 100                                                   597
         CALL AGSDETH (0)                                                 597
C    *                                                                    597
         AG_PARLIST='LGAM'                                                597
         AG_TITLE = 'DETH'                                                597
         AG_NBITS = 16                                                    597
         AG_BIN=AG_MAGIC                                                  597
         AG_OPTION='*'                                                    597
         AG_FHMIN = -2                                                    597
         AG_FHMAX = +2                                                    597
         CALL AGSDETH (0)                                                 597
C    *                                                                    597
         AG_PARLIST='ELOS'                                                597
         AG_TITLE = 'DETH'                                                597
         AG_NBITS = 16                                                    597
         AG_BIN=AG_MAGIC                                                  597
         AG_OPTION='*'                                                    597
         AG_FHMIN = 0.                                                    597
         AG_FHMAX = 0.01                                                  597
         CALL AGSDETH (0)                                                 597
C    *                                                                    597
         CALL AGSDETU('TMSE')                                             597
C    *                                                                    597
         ELSE                                                             598
C    Defining Hits: Z:0.01:S Y:0.01: Z:0.01: CX:10: CY:10: CZ:10: SLENG:  600
C    0.1:(0,800) STEP:0.1:, TOF:16:(0,1.0E-6) PTOT:16:(0,100) LGAM:16:(-  600
C    2,+2) ELOS:16:(0.,0.01)                                              600
         AG_TITLE = 'TMSE'                                                600
         AG_EXNAME = 'HITS'                                               600
         AG_TYPE=AG_MAGIC                                                 600
         CALL AGSDETV('TMSE')                                             600
C    *                                                                    600
         AG_PARLIST='Z'                                                   600
         AG_TITLE = 'DETH'                                                600
         AG_NBITS = AG_MAGIC                                              600
         AG_BIN=0.01                                                      600
         AG_OPTION='S'                                                    600
         AG_FHMIN = AG_MAGIC                                              600
         AG_FHMAX = AG_MAGIC                                              600
         CALL AGSDETH (0)                                                 600
C    *                                                                    600
         AG_PARLIST='Y'                                                   600
         AG_TITLE = 'DETH'                                                600
         AG_NBITS = AG_MAGIC                                              600
         AG_BIN=0.01                                                      600
         AG_FHMIN=AG_MAGIC                                                600
         AG_FHMAX=AG_MAGIC                                                600
         AG_OPTION='*'                                                    600
         CALL AGSDETH (0)                                                 600
C    *                                                                    600
         AG_PARLIST='Z'                                                   600
         AG_TITLE = 'DETH'                                                600
         AG_NBITS = AG_MAGIC                                              600
         AG_BIN=0.01                                                      600
         AG_FHMIN=AG_MAGIC                                                600
         AG_FHMAX=AG_MAGIC                                                600
         AG_OPTION='*'                                                    600
         CALL AGSDETH (0)                                                 600
C    *                                                                    600
         AG_PARLIST='CX'                                                  600
         AG_TITLE = 'DETH'                                                600
         AG_NBITS = 10                                                    600
         AG_BIN=AG_MAGIC                                                  600
         AG_FHMIN=AG_MAGIC                                                600
         AG_FHMAX=AG_MAGIC                                                600
         AG_OPTION='*'                                                    600
         CALL AGSDETH (0)                                                 600
C    *                                                                    600
         AG_PARLIST='CY'                                                  600
         AG_TITLE = 'DETH'                                                600
         AG_NBITS = 10                                                    600
         AG_BIN=AG_MAGIC                                                  600
         AG_FHMIN=AG_MAGIC                                                600
         AG_FHMAX=AG_MAGIC                                                600
         AG_OPTION='*'                                                    600
         CALL AGSDETH (0)                                                 600
C    *                                                                    600
         AG_PARLIST='CZ'                                                  600
         AG_TITLE = 'DETH'                                                600
         AG_NBITS = 10                                                    600
         AG_BIN=AG_MAGIC                                                  600
         AG_FHMIN=AG_MAGIC                                                600
         AG_FHMAX=AG_MAGIC                                                600
         AG_OPTION='*'                                                    600
         CALL AGSDETH (0)                                                 600
C    *                                                                    600
         AG_PARLIST='SLENG'                                               600
         AG_TITLE = 'DETH'                                                600
         AG_NBITS = AG_MAGIC                                              600
         AG_BIN=0.1                                                       600
         AG_OPTION='*'                                                    600
         AG_FHMIN = 0                                                     600
         AG_FHMAX = 800                                                   600
         CALL AGSDETH (0)                                                 600
C    *                                                                    600
         AG_PARLIST='STEP'                                                600
         AG_TITLE = 'DETH'                                                600
         AG_NBITS = AG_MAGIC                                              600
         AG_BIN=0.1                                                       600
         AG_OPTION='*'                                                    600
         AG_FHMIN = AG_MAGIC                                              600
         AG_FHMAX = AG_MAGIC                                              600
         CALL AGSDETH (0)                                                 600
C    *                                                                    600
         AG_PARLIST='TOF'                                                 600
         AG_TITLE = 'DETH'                                                600
         AG_NBITS = 16                                                    600
         AG_BIN=AG_MAGIC                                                  600
         AG_OPTION='*'                                                    600
         AG_FHMIN = 0                                                     600
         AG_FHMAX = 1.0E-6                                                600
         CALL AGSDETH (0)                                                 600
C    *                                                                    600
         AG_PARLIST='PTOT'                                                600
         AG_TITLE = 'DETH'                                                600
         AG_NBITS = 16                                                    600
         AG_BIN=AG_MAGIC                                                  600
         AG_OPTION='*'                                                    600
         AG_FHMIN = 0                                                     600
         AG_FHMAX = 100                                                   600
         CALL AGSDETH (0)                                                 600
C    *                                                                    600
         AG_PARLIST='LGAM'                                                600
         AG_TITLE = 'DETH'                                                600
         AG_NBITS = 16                                                    600
         AG_BIN=AG_MAGIC                                                  600
         AG_OPTION='*'                                                    600
         AG_FHMIN = -2                                                    600
         AG_FHMAX = +2                                                    600
         CALL AGSDETH (0)                                                 600
C    *                                                                    600
         AG_PARLIST='ELOS'                                                600
         AG_TITLE = 'DETH'                                                600
         AG_NBITS = 16                                                    600
         AG_BIN=AG_MAGIC                                                  600
         AG_OPTION='*'                                                    600
         AG_FHMIN = 0.                                                    600
         AG_FHMAX = 0.01                                                  600
         CALL AGSDETH (0)                                                 600
C    *                                                                    600
         CALL AGSDETU('TMSE')                                             600
C    *                                                                    600
         ENDIF                                                            601
5731  CALL AGSPOPB                                                        602
      GO TO BLOCK_TMSE, (5730 ,5740)                                      602
C End of Local Procedure                                                  602
C ----------------------------------------------------------------------  603
C ------------ THOL                                                       603
C Begin Local Procedure THOL                                              604
C THOL IS PART OF A HOLE FOR PC BOARDS                                    604
5600  CONTINUE                                                            604
         AG_TITLE='THOL'                                                  604
         AG_MODULE=MODULE                                                 604
         AG_MARK='V00'                                                    604
         CALL AGSPUSH                                                     604
         AG_MODULE=MODULE                                                 604
C    *                                                                    604
C    Geant MATERIAL : AIR ===> inherited arguments used <===              605
         CALL AGSRESET                                                    605
         AG_TITLE = 'AIR'                                                 605
         AG_EXNAME = 'MATERIAL'                                           605
         AG_PARLIST = 'NONE'                                              605
         CALL AXMATERIAL                                                  605
C    *                                                                    605
C    Geant SHAPE : BOX ===> DX=TECW_SLOTW/2 DY=TECW_PPDEPTH/2, DZ=TECW_S  605
C    LOTRAD <===                                                          605
         CALL AGSRESET                                                    607
         AG_DX = TECW_SLOTW/2                                             607
         AG_DY = TECW_PPDEPTH/2                                           607
         AG_DZ = TECW_SLOTRAD                                             607
         AG_TITLE = 'BOX'                                                 607
         AG_EXNAME = 'SHAPE'                                              607
         AG_PARLIST = 'DX_DY_DZ'                                          607
         CALL AXSHAPE                                                     607
C    *                                                                    607
5601  CALL AGSPOPB                                                        608
      GO TO BLOCK_THOL, (5600 ,5610)                                      608
C End of Local Procedure                                                  608
C ----------------------------------------------------------------------  609
C ------------ THRA                                                       609
C Begin Local Procedure THRA                                              610
C THRA IS PART OF A HOLE FOR PC BOARDS                                    610
5620  CONTINUE                                                            610
         AG_TITLE='THRA'                                                  610
         AG_MODULE=MODULE                                                 610
         AG_MARK='V00'                                                    610
         CALL AGSPUSH                                                     610
         AG_MODULE=MODULE                                                 610
C    *                                                                    610
C    Geant MATERIAL : AIR ===> inherited arguments used <===              611
         CALL AGSRESET                                                    611
         AG_TITLE = 'AIR'                                                 611
         AG_EXNAME = 'MATERIAL'                                           611
         AG_PARLIST = 'NONE'                                              611
         CALL AXMATERIAL                                                  611
C    *                                                                    611
C    Geant SHAPE : TUBS ===> RMIN=0.0 RMAX=TECW_SLOTRAD PHI1=0.0, PHI2=1  611
C    80.0 DZ=TECW_PPDEPTH/2 <===                                          611
         CALL AGSRESET                                                    613
         AG_RMIN = 0.0                                                    613
         AG_RMAX = TECW_SLOTRAD                                           613
         AG_PHI1 = 0.0                                                    613
         AG_PHI2 = 180.0                                                  613
         AG_DZ = TECW_PPDEPTH/2                                           613
         AG_TITLE = 'TUBS'                                                613
         AG_EXNAME = 'SHAPE'                                              613
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            613
         CALL AXSHAPE                                                     613
C    *                                                                    613
5621  CALL AGSPOPB                                                        614
      GO TO BLOCK_THRA, (5620 ,5630)                                      614
C End of Local Procedure                                                  614
C ----------------------------------------------------------------------  615
C ------------ THLA                                                       615
C Begin Local Procedure THLA                                              616
C THLA IS PART OF A HOLE FOR PC BOARDS                                    616
5640  CONTINUE                                                            616
         AG_TITLE='THLA'                                                  616
         AG_MODULE=MODULE                                                 616
         AG_MARK='V00'                                                    616
         CALL AGSPUSH                                                     616
         AG_MODULE=MODULE                                                 616
C    *                                                                    616
C    Geant MATERIAL : AIR ===> inherited arguments used <===              617
         CALL AGSRESET                                                    617
         AG_TITLE = 'AIR'                                                 617
         AG_EXNAME = 'MATERIAL'                                           617
         AG_PARLIST = 'NONE'                                              617
         CALL AXMATERIAL                                                  617
C    *                                                                    617
C    Geant SHAPE : TUBS ===> RMIN=0.0 RMAX=TECW_SLOTRAD PHI1=180.0, PHI2  617
C    =360.0 DZ=TECW_PPDEPTH/2 <===                                        617
         CALL AGSRESET                                                    619
         AG_RMIN = 0.0                                                    619
         AG_RMAX = TECW_SLOTRAD                                           619
         AG_PHI1 = 180.0                                                  619
         AG_PHI2 = 360.0                                                  619
         AG_DZ = TECW_PPDEPTH/2                                           619
         AG_TITLE = 'TUBS'                                                619
         AG_EXNAME = 'SHAPE'                                              619
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            619
         CALL AXSHAPE                                                     619
C    *                                                                    619
5641  CALL AGSPOPB                                                        620
      GO TO BLOCK_THLA, (5640 ,5650)                                      620
C End of Local Procedure                                                  620
C ----------------------------------------------------------------------  621
C ------------ TIAG                                                       621
C Begin Local Procedure TIAG                                              622
C TIAG IS AN AIR GAP IN INNER SECTOR ALUMINUM STRUCTURE                   622
5530  CONTINUE                                                            622
         AG_TITLE='TIAG'                                                  622
         AG_MODULE=MODULE                                                 622
         AG_MARK='V00'                                                    622
         CALL AGSPUSH                                                     622
         AG_MODULE=MODULE                                                 622
C    *                                                                    622
C    Geant MATERIAL : AIR ===> inherited arguments used <===              623
         CALL AGSRESET                                                    623
         AG_TITLE = 'AIR'                                                 623
         AG_EXNAME = 'MATERIAL'                                           623
         AG_PARLIST = 'NONE'                                              623
         CALL AXMATERIAL                                                  623
C    *                                                                    623
C    Geant SHAPE : TRD1 ===> DX1=0 DX2=0 DY=0 DZ=0 <===                   623
         CALL AGSRESET                                                    624
         AG_DX1 = 0                                                       624
         AG_DX2 = 0                                                       624
         AG_DY = 0                                                        624
         AG_DZ = 0                                                        624
         AG_TITLE = 'TRD1'                                                624
         AG_EXNAME = 'SHAPE'                                              624
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     624
         CALL AXSHAPE                                                     624
C    *                                                                    624
5531  CALL AGSPOPB                                                        625
      GO TO BLOCK_TIAG, (5530 ,5540)                                      625
C End of Local Procedure                                                  625
C ----------------------------------------------------------------------  626
C ------------ TOAE                                                       626
C Begin Local Procedure TOAE                                              627
C TOAE IS EXTRA ALUMINUM SUPPORTS IN THE AIR OPENNINGS                    627
5680  CONTINUE                                                            627
         AG_TITLE='TOAE'                                                  627
         AG_MODULE=MODULE                                                 627
         AG_MARK='V00'                                                    627
         CALL AGSPUSH                                                     627
         AG_MODULE=MODULE                                                 627
C    *                                                                    627
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        628
         CALL AGSRESET                                                    628
         AG_TITLE = 'ALUMINIUM'                                           628
         AG_EXNAME = 'MATERIAL'                                           628
         AG_PARLIST = 'NONE'                                              628
         CALL AXMATERIAL                                                  628
C    *                                                                    628
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           628
         CALL AGSRESET                                                    629
         AG_DX = 0                                                        629
         AG_DY = 0                                                        629
         AG_DZ = 0                                                        629
         AG_TITLE = 'BOX'                                                 629
         AG_EXNAME = 'SHAPE'                                              629
         AG_PARLIST = 'DX_DY_DZ'                                          629
         CALL AXSHAPE                                                     629
C    *                                                                    629
5681  CALL AGSPOPB                                                        630
      GO TO BLOCK_TOAE, (5680 ,5690)                                      630
C End of Local Procedure                                                  630
C ----------------------------------------------------------------------  631
C ------------ TCEX                                                       631
C Begin Local Procedure TCEX                                              632
C TCEX IS PART OF THE G10 FOR THE PC BOARDS                               632
5560  CONTINUE                                                            632
         AG_TITLE='TCEX'                                                  632
         AG_MODULE=MODULE                                                 632
         AG_MARK='V00'                                                    632
         CALL AGSPUSH                                                     632
         AG_MODULE=MODULE                                                 632
C    *                                                                    632
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===          633
         CALL AGSRESET                                                    633
         AG_A = 28.08                                                     633
         AG_Z = 14                                                        633
         AG_W = 0.6*1*28./60.                                             633
         AG_TITLE = 'SI'                                                  633
         AG_EXNAME = 'COMPONENT'                                          633
         AG_PARLIST = 'A_Z_W'                                             633
         CALL AXCOMPONENT                                                 633
C    *                                                                    633
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174.   633
C    <===                                                                 633
         CALL AGSRESET                                                    634
         AG_A = 16                                                        634
         AG_Z = 8                                                         634
         AG_W = 0.6*2*16./60. + 0.4*4*16./174.                            634
         AG_TITLE = 'O'                                                   634
         AG_EXNAME = 'COMPONENT'                                          634
         AG_PARLIST = 'A_Z_W'                                             634
         CALL AXCOMPONENT                                                 634
C    *                                                                    634
C    Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===              634
         CALL AGSRESET                                                    635
         AG_A = 12                                                        635
         AG_Z = 6                                                         635
         AG_W = 0.4*8*12./174.                                            635
         AG_TITLE = 'C'                                                   635
         AG_EXNAME = 'COMPONENT'                                          635
         AG_PARLIST = 'A_Z_W'                                             635
         CALL AXCOMPONENT                                                 635
C    *                                                                    635
C    Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===               635
         CALL AGSRESET                                                    636
         AG_A = 1                                                         636
         AG_Z = 1                                                         636
         AG_W = 0.4*14*1./174.                                            636
         AG_TITLE = 'H'                                                   636
         AG_EXNAME = 'COMPONENT'                                          636
         AG_PARLIST = 'A_Z_W'                                             636
         CALL AXCOMPONENT                                                 636
C    *                                                                    636
C    Geant MIXTURE : G10 ===> DENS=1.7 <===                               636
         CALL AGSRESET                                                    637
         AG_DENS = 1.7                                                    637
         AG_TITLE = 'G10'                                                 637
         AG_EXNAME = 'MIXTURE'                                            637
         AG_PARLIST = 'DENS'                                              637
         CALL AXMIXTURE                                                   637
C    *                                                                    637
C    Geant MATERIAL : G10 ===> inherited arguments used <===              637
         CALL AGSRESET                                                    638
         AG_TITLE = 'G10'                                                 638
         AG_EXNAME = 'MATERIAL'                                           638
         AG_PARLIST = 'NONE'                                              638
         CALL AXMATERIAL                                                  638
C    *                                                                    638
C    Geant SHAPE : BOX ===> DX=TECW_CARDW/2 DY=TECW_ASDEPTH/2, DZ=TECW_C  638
C    ARDTH/2 <===                                                         638
         CALL AGSRESET                                                    640
         AG_DX = TECW_CARDW/2                                             640
         AG_DY = TECW_ASDEPTH/2                                           640
         AG_DZ = TECW_CARDTH/2                                            640
         AG_TITLE = 'BOX'                                                 640
         AG_EXNAME = 'SHAPE'                                              640
         AG_PARLIST = 'DX_DY_DZ'                                          640
         CALL AXSHAPE                                                     640
C    *                                                                    640
5561  CALL AGSPOPB                                                        641
      GO TO BLOCK_TCEX, (5560 ,5570)                                      641
C End of Local Procedure                                                  641
C ----------------------------------------------------------------------  642
C ------------ TAEC                                                       642
C Begin Local Procedure TAEC                                              643
C TAEC IS PART OF THE HEAT SHIELD FOR THE PC BOARDS                       643
5580  CONTINUE                                                            643
         AG_TITLE='TAEC'                                                  643
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
C    Geant SHAPE : BOX ===> DX=TECW_COOLW/2 DY=TECW_ASDEPTH/2, DZ=TECW_C  644
C    OOLTH/2 <===                                                         644
         CALL AGSRESET                                                    646
         AG_DX = TECW_COOLW/2                                             646
         AG_DY = TECW_ASDEPTH/2                                           646
         AG_DZ = TECW_COOLTH/2                                            646
         AG_TITLE = 'BOX'                                                 646
         AG_EXNAME = 'SHAPE'                                              646
         AG_PARLIST = 'DX_DY_DZ'                                          646
         CALL AXSHAPE                                                     646
C    *                                                                    646
5581  CALL AGSPOPB                                                        647
      GO TO BLOCK_TAEC, (5580 ,5590)                                      647
C End of Local Procedure                                                  647
C ----------------------------------------------------------------------  648
C ------------ TPCW                                                       648
C Begin Local Procedure TPCW                                              649
C TPCW IS THE TPC SUPPORTING ENDCAP WHEEL                                 649
5160  CONTINUE                                                            649
         AG_TITLE='TPCW'                                                  649
         AG_MODULE=MODULE                                                 649
         AG_MARK='V00'                                                    649
         CALL AGSPUSH                                                     649
         AG_MODULE=MODULE                                                 649
C    *                                                                    649
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        650
         CALL AGSRESET                                                    650
         AG_TITLE = 'ALUMINIUM'                                           650
         AG_EXNAME = 'MATERIAL'                                           650
         AG_PARLIST = 'NONE'                                              650
         CALL AXMATERIAL                                                  650
C    *                                                                    650
C    Geant ATTRIBUTE : TPEA ===> SEEN=1 <===                              650
         CALL AGSRESET                                                    651
         AG_SEEN = 1                                                      651
         AG_TITLE = 'TPEA'                                                651
         AG_EXNAME = 'ATTRIBUTE'                                          651
         AG_PARLIST = 'SEEN'                                              651
         CALL AXATTRIBUTE                                                 651
C    *                                                                    651
C    Geant SHAPE : TUBE ===> RMIN=TPCG_WHEELIR RMAX=TPCG_WHEELOR, DZ=TPC  651
C    G_WHEELTHK/2 <===                                                    651
         CALL AGSRESET                                                    653
         AG_RMIN = TPCG_WHEELIR                                           653
         AG_RMAX = TPCG_WHEELOR                                           653
         AG_DZ = TPCG_WHEELTHK/2                                          653
         AG_TITLE = 'TUBE'                                                653
         AG_EXNAME = 'SHAPE'                                              653
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      653
         CALL AXSHAPE                                                     653
C    *                                                                    653
C    Create TWSS :                                                        654
C    Call To Local Procedure TWSS                                         654
         ASSIGN 5760 TO BLOCK_TWSS                                        654
         GO TO  5750                                                      654
5760     CONTINUE                                                         654
C    *                                                                    654
5161  CALL AGSPOPB                                                        655
      GO TO BLOCK_TPCW, (5160 ,5170)                                      655
C End of Local Procedure                                                  655
C ----------------------------------------------------------------------  656
C ------------ TWSS                                                       656
C Begin Local Procedure TWSS                                              657
C TWSS IS A DIVISION OF WHEEL CORRESPONDING TO SUPERSECTORS               657
5750  CONTINUE                                                            657
         AG_TITLE='TWSS'                                                  657
         AG_MODULE=MODULE                                                 657
         AG_MARK='V00'                                                    657
         CALL AGSPUSH                                                     657
         AG_MODULE=MODULE                                                 657
C    *                                                                    657
C    Geant ATTRIBUTE : TWSS ===> SEEN=0 <===                              658
         CALL AGSRESET                                                    658
         AG_SEEN = 0                                                      658
         AG_TITLE = 'TWSS'                                                658
         AG_EXNAME = 'ATTRIBUTE'                                          658
         AG_PARLIST = 'SEEN'                                              658
         CALL AXATTRIBUTE                                                 658
C    *                                                                    658
C    Geant SHAPE : DIVISION ===> NDIV=12 IAXIS=2 <===                     658
         CALL AGSRESET                                                    659
         AG_NDIV = 12                                                     659
         AG_IAXIS = 2                                                     659
         AG_TITLE = 'DIVISION'                                            659
         AG_EXNAME = 'SHAPE'                                              659
         AG_PARLIST = 'NDIV_IAXIS'                                        659
         CALL AXSHAPE                                                     659
C    *                                                                    659
C    *                                                                    659
         I_SEC=1                                                          660
            GO TO 5773                                                    660
5771        I_SEC=I_SEC+1                                                 660
5773        IF(I_SEC-(2).GT.0) GO TO 5772                                 660
C       use TECW bank                                                     661
            BPATH_TECW='TECW*'                                            661
            IDNUM_TECW(1)=0                                               661
            TECW_SEC=I_SEC                                                661
            CALL AGDGETP('TPCEGEO2','*',-999,'TECW',NTECW_SEC,            661
     *      RE_TECW_SEC,BPATH_TECW,IDNUM_TECW,1,MAP_TECW,NAMES_TECW,      661
     *      BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)             661
C       use TROV bank                                                     662
            BPATH_TROV='TROV*'                                            662
            IDNUM_TROV(1)=0                                               662
            TROV_SEC=I_SEC                                                662
            CALL AGDGETP('TPCEGEO2','*',-999,'TROV',NTROV_SEC,            662
     *      RE_TROV_SEC,BPATH_TROV,IDNUM_TROV,1,MAP_TROV,NAMES_TROV,      662
     *      BANK_TROV,LEN_TROV,LLEN_TROV,LINK_TROV,FLAG_TROV)             662
C       Create TWGI :                                                     663
C       Call To Local Procedure TWGI                                      663
            ASSIGN 5790 TO BLOCK_TWGI                                     663
            GO TO  5780                                                   663
5790        CONTINUE                                                      663
C       *                                                                 663
C       Geant POSITION : TWGI ===> X=TECW_GAPRAD ORT=YZX <===             664
            CALL AGSRESET                                                 664
            AG_X = TECW_GAPRAD                                            664
            AG_ALPHAN=AG_ALPHAN+1                                         664
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              664
            AG_TITLE = 'TWGI'                                             664
            AG_EXNAME = 'POSITION'                                        664
            AG_PARLIST = 'X_ORT'                                          664
            CALL AXPOSITION                                               664
C       *                                                                 664
         GO TO 5771                                                       665
5772     CONTINUE                                                         665
5751  CALL AGSPOPB                                                        666
      GO TO BLOCK_TWSS, (5750 ,5760)                                      666
C End of Local Procedure                                                  666
C ----------------------------------------------------------------------  667
C ------------ TWGI                                                       667
C Begin Local Procedure TWGI                                              668
C TWGI IS THE INNER AIR GAP IN WHEEL                                      668
5780  CONTINUE                                                            668
         AG_TITLE='TWGI'                                                  668
         AG_MODULE=MODULE                                                 668
         AG_MARK='V00'                                                    668
         CALL AGSPUSH                                                     668
         AG_MODULE=MODULE                                                 668
C    *                                                                    668
C    Geant MATERIAL : AIR ===> inherited arguments used <===              669
         CALL AGSRESET                                                    669
         AG_TITLE = 'AIR'                                                 669
         AG_EXNAME = 'MATERIAL'                                           669
         AG_PARLIST = 'NONE'                                              669
         CALL AXMATERIAL                                                  669
C    *                                                                    669
C    Geant ATTRIBUTE : TWGI ===> SEEN=1 <===                              669
         CALL AGSRESET                                                    670
         AG_SEEN = 1                                                      670
         AG_TITLE = 'TWGI'                                                670
         AG_EXNAME = 'ATTRIBUTE'                                          670
         AG_PARLIST = 'SEEN'                                              670
         CALL AXATTRIBUTE                                                 670
C    *                                                                    670
C    Geant SHAPE : TRD1 ===> DX1=TECW_GAPWIDI/2 DX2=TECW_GAPWIDO/2, DY=T  670
C    PCG_WHEELTHK/2 DZ=TECW_GAPHEIT/2 <===                                670
         CALL AGSRESET                                                    672
         AG_DX1 = TECW_GAPWIDI/2                                          672
         AG_DX2 = TECW_GAPWIDO/2                                          672
         AG_DY = TPCG_WHEELTHK/2                                          672
         AG_DZ = TECW_GAPHEIT/2                                           672
         AG_TITLE = 'TRD1'                                                672
         AG_EXNAME = 'SHAPE'                                              672
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     672
         CALL AXSHAPE                                                     672
C    *                                                                    672
         DX_DZ=(TECW_GAPWIDO/2 - TECW_GAPWIDI/2)/TECW_GAPHEIT             672
         XLIP=TECW_WHLIPWID*SQRT(1 + DX_DZ*DX_DZ)                         674
         XBOTTOM =TECW_GAPWIDI/2 + XLIP - DX_DZ*(TECW_WHLIPWID -          675
     *   TECW_GAPSHFT)                                                    675
C    Create TWGC :                                                        676
C    Call To Local Procedure TWGC                                         676
         ASSIGN 5810 TO BLOCK_TWGC                                        676
         GO TO  5800                                                      676
5810     CONTINUE                                                         676
C    *                                                                    676
C    Geant POSITION : TWGC ===> Y=TECW_WHLIPTHK/2 Z=TECW_GAPSHFT DX2=TEC  680
C    W_GAPWIDO/2+XLIP+DX_DZ*TECW_WHLIPWID DX1=XBOTTOM DZ=TECW_GAPHEIT/2+  680
C    TECW_WHLIPWID-TECW_GAPSHFT DY=(TPCG_WHEELTHK-TECW_WHLIPTHK)/2 <===   680
         CALL AGSRESET                                                    680
         AG_Y = TECW_WHLIPTHK/2                                           680
         AG_Z = TECW_GAPSHFT                                              680
         AG_DX2 = TECW_GAPWIDO/2+XLIP+DX_DZ*TECW_WHLIPWID                 680
         AG_DX1 = XBOTTOM                                                 680
         AG_DZ = TECW_GAPHEIT/2+TECW_WHLIPWID-TECW_GAPSHFT                680
         AG_DY = (TPCG_WHEELTHK-TECW_WHLIPTHK)/2                          680
         AG_TITLE = 'TWGC'                                                680
         AG_EXNAME = 'POSITION'                                           680
         AG_PARLIST = 'Y_Z_DX2_DX1_DZ_DY'                                 680
         CALL AXPOSITION                                                  680
C    *                                                                    680
C    Create TWGB :                                                        681
C    Call To Local Procedure TWGB                                         681
         ASSIGN 5830 TO BLOCK_TWGB                                        681
         GO TO  5820                                                      681
5830     CONTINUE                                                         681
C    *                                                                    681
C    Geant POSITION : TWGB ===> X=0 Y=TECW_WHLIPTHK/2 Z=(TECW_GAPHEIT/2+  683
C    TECW_WHLIPWID/2) ALPHAY=90 <===                                      683
         CALL AGSRESET                                                    683
         AG_X = 0                                                         683
         AG_Y = TECW_WHLIPTHK/2                                           683
         AG_Z = (TECW_GAPHEIT/2+TECW_WHLIPWID/2)                          683
         AG_ALPHAN=AG_ALPHAN+1                                            683
         AG_ALPHAK(AG_ALPHAN)=2                                           683
         AG_ALPHAL(AG_ALPHAN)=90                                          683
         AG_TITLE = 'TWGB'                                                683
         AG_EXNAME = 'POSITION'                                           683
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      683
         CALL AXPOSITION                                                  683
C    *                                                                    683
C    Check TECW_WHBLKIN==1                                                683
         IF (TECW_WHBLKIN.EQ.1) THEN                                      684
C    Geant POSITION : TWGB ===> X=0 Y=TECW_WHLIPTHK/2 Z=-(TECW_GAPHEIT/2  686
C    +TECW_WHLIPWID/2) ALPHAY=90 <===                                     686
         CALL AGSRESET                                                    686
         AG_X = 0                                                         686
         AG_Y = TECW_WHLIPTHK/2                                           686
         AG_Z = -(TECW_GAPHEIT/2+TECW_WHLIPWID/2)                         686
         AG_ALPHAN=AG_ALPHAN+1                                            686
         AG_ALPHAK(AG_ALPHAN)=2                                           686
         AG_ALPHAL(AG_ALPHAN)=90                                          686
         AG_TITLE = 'TWGB'                                                686
         AG_EXNAME = 'POSITION'                                           686
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      686
         CALL AXPOSITION                                                  686
C    *                                                                    686
         ENDIF                                                            686
C    Geant POSITION : TWGB ===> X=TECW_GAPWIDI/2+XLIP/2+DX_DZ*(TECW_WHBL  689
C    KPOS+TECW_GAPHEIT/2) Y=TECW_WHLIPTHK/2 Z=TECW_WHBLKPOS ALPHAY=15 <=  689
C    ==                                                                   689
         CALL AGSRESET                                                    689
         AG_X = TECW_GAPWIDI/2+XLIP/2+DX_DZ*(TECW_WHBLKPOS+TECW_GAPHEIT/  689
     *   2)                                                               689
         AG_Y = TECW_WHLIPTHK/2                                           689
         AG_Z = TECW_WHBLKPOS                                             689
         AG_ALPHAN=AG_ALPHAN+1                                            689
         AG_ALPHAK(AG_ALPHAN)=2                                           689
         AG_ALPHAL(AG_ALPHAN)=15                                          689
         AG_TITLE = 'TWGB'                                                689
         AG_EXNAME = 'POSITION'                                           689
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      689
         CALL AXPOSITION                                                  689
C    *                                                                    689
C    Geant POSITION : TWGB ===> X=TECW_GAPWIDI/2+XLIP/2+DX_DZ*(-TECW_WHB  689
C    LKPOS+TECW_GAPHEIT/2) Y=TECW_WHLIPTHK/2 Z=-TECW_WHBLKPOS ALPHAY=15   689
C    <===                                                                 689
         CALL AGSRESET                                                    691
         AG_X = TECW_GAPWIDI/2+XLIP/                                      691
     *   2+DX_DZ*(-TECW_WHBLKPOS+TECW_GAPHEIT/2)                          691
         AG_Y = TECW_WHLIPTHK/2                                           691
         AG_Z = -TECW_WHBLKPOS                                            691
         AG_ALPHAN=AG_ALPHAN+1                                            691
         AG_ALPHAK(AG_ALPHAN)=2                                           691
         AG_ALPHAL(AG_ALPHAN)=15                                          691
         AG_TITLE = 'TWGB'                                                691
         AG_EXNAME = 'POSITION'                                           691
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      691
         CALL AXPOSITION                                                  691
C    *                                                                    691
C    Geant POSITION : TWGB ===> X=-TECW_GAPWIDI/2-XLIP/2-DX_DZ*(TECW_WHB  691
C    LKPOS+TECW_GAPHEIT/2) Y=TECW_WHLIPTHK/2 Z=TECW_WHBLKPOS ALPHAY=-15   691
C    <===                                                                 691
         CALL AGSRESET                                                    693
         AG_X = -TECW_GAPWIDI/2-XLIP/                                     693
     *   2-DX_DZ*(TECW_WHBLKPOS+TECW_GAPHEIT/2)                           693
         AG_Y = TECW_WHLIPTHK/2                                           693
         AG_Z = TECW_WHBLKPOS                                             693
         AG_ALPHAN=AG_ALPHAN+1                                            693
         AG_ALPHAK(AG_ALPHAN)=2                                           693
         AG_ALPHAL(AG_ALPHAN)=-15                                         693
         AG_TITLE = 'TWGB'                                                693
         AG_EXNAME = 'POSITION'                                           693
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      693
         CALL AXPOSITION                                                  693
C    *                                                                    693
C    Geant POSITION : TWGB ===> X=-TECW_GAPWIDI/2-XLIP/2-DX_DZ*(-TECW_WH  693
C    BLKPOS+TECW_GAPHEIT/2) Y=TECW_WHLIPTHK/2 Z=-TECW_WHBLKPOS ALPHAY=-1  693
C    5 <===                                                               693
         CALL AGSRESET                                                    695
         AG_X = -TECW_GAPWIDI/2-XLIP/                                     695
     *   2-DX_DZ*(-TECW_WHBLKPOS+TECW_GAPHEIT/2)                          695
         AG_Y = TECW_WHLIPTHK/2                                           695
         AG_Z = -TECW_WHBLKPOS                                            695
         AG_ALPHAN=AG_ALPHAN+1                                            695
         AG_ALPHAK(AG_ALPHAN)=2                                           695
         AG_ALPHAL(AG_ALPHAN)=-15                                         695
         AG_TITLE = 'TWGB'                                                695
         AG_EXNAME = 'POSITION'                                           695
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      695
         CALL AXPOSITION                                                  695
C    *                                                                    695
         RADOFF = TECW_RCENTER - TECW_GAPRAD - TECW_HEIGHT/2 +            695
     *   TECW_GAPHEIT/2                                                   695
C    *                                                                    697
         I_NHP=1                                                          697
            GO TO 5843                                                    697
5841        I_NHP=I_NHP+1                                                 697
5843        IF(I_NHP-(16).GT.0) GO TO 5842                                697
C       Check TROV_NHP(I_NHP)>0                                           698
            IF (TROV_NHP(I_NHP).GT.0) THEN                                698
C       FIRST PL                                                          699
            HPLA = -TECW_GAPHEIT/2+RADOFF+TECW_Z(1)-TECW_DZ(1)/           699
     *      2+TROV_OFFCRDIO                                               699
            ZVALUE = HPLA+4.0*(I_NHP-1)                                   700
            PIPEOFF=TECW_PIPEHT/2+TECW_COOLTH+TECW_CARDTH/2               701
            XTEMP=TECW_GAPWIDI/2                                          702
            ZTEMP=ZVALUE+TECW_CARDOFF-PIPEOFF                             703
C       Create TPIP :                                                     704
C       Call To Local Procedure TPIP                                      704
            ASSIGN 5860 TO BLOCK_TPIP                                     704
            GO TO  5850                                                   704
5860        CONTINUE                                                      704
C       *                                                                 704
C       Geant POSITION : TPIP ===> Y=-TPCG_WHEELTHK/2+TECW_PIPETHK/2 Z=Z  708
C       TEMP DX2=XTEMP+DX_DZ*(ZTEMP+TECW_GAPHEIT/2+TECW_PIPEHT/2) DX1=XT  708
C       EMP+DX_DZ*(ZTEMP+TECW_GAPHEIT/2-TECW_PIPEHT/2) DZ=TECW_PIPEHT/2   708
C       DY=TECW_PIPETHK/2 <===                                            708
            CALL AGSRESET                                                 708
            AG_Y = -TPCG_WHEELTHK/2+TECW_PIPETHK/2                        708
            AG_Z = ZTEMP                                                  708
            AG_DX2 = XTEMP+DX_DZ*(ZTEMP+TECW_GAPHEIT/2+TECW_PIPEHT/2)     708
            AG_DX1 = XTEMP+DX_DZ*(ZTEMP+TECW_GAPHEIT/2-TECW_PIPEHT/2)     708
            AG_DZ = TECW_PIPEHT/2                                         708
            AG_DY = TECW_PIPETHK/2                                        708
            AG_TITLE = 'TPIP'                                             708
            AG_EXNAME = 'POSITION'                                        708
            AG_PARLIST = 'Y_Z_DX2_DX1_DZ_DY'                              708
            CALL AXPOSITION                                               708
C       *                                                                 708
            ENDIF                                                         708
         GO TO 5841                                                       710
5842     CONTINUE                                                         710
C    Create TMAN :                                                        711
C    Call To Local Procedure TMAN                                         711
         ASSIGN 5880 TO BLOCK_TMAN                                        711
         GO TO  5870                                                      711
5880     CONTINUE                                                         711
C    *                                                                    711
C    Geant POSITION : TMAN ===> X=TECW_GAPWIDI/2.+DX_DZ*(TECW_GAPHEIT/2)  716
C    -(1+DX_DZ*DX_DZ)*TECW_MANIWID/2 Y=-TPCG_WHEELTHK/2+TECW_PIPETHK+TEC  716
C    W_MANITHK/2 Z=0.0 DZ=(1.+DX_DZ*DX_DZ)*(TECW_GAPHEIT/2-TECW_MANIWID)  716
C     DX=TECW_MANIWID/2 DY=TECW_MANITHK/2 ALPHAY=15 <===                  716
         CALL AGSRESET                                                    716
         AG_X = TECW_GAPWIDI/2.+DX_DZ*(TECW_GAPHEIT/                      716
     *   2)-(1+DX_DZ*DX_DZ)*TECW_MANIWID/2                                716
         AG_Y = -TPCG_WHEELTHK/2+TECW_PIPETHK+TECW_MANITHK/2              716
         AG_Z = 0.0                                                       716
         AG_DZ = (1.+DX_DZ*DX_DZ)*(TECW_GAPHEIT/2-TECW_MANIWID)           716
         AG_DX = TECW_MANIWID/2                                           716
         AG_DY = TECW_MANITHK/2                                           716
         AG_ALPHAN=AG_ALPHAN+1                                            716
         AG_ALPHAK(AG_ALPHAN)=2                                           716
         AG_ALPHAL(AG_ALPHAN)=15                                          716
         AG_TITLE = 'TMAN'                                                716
         AG_EXNAME = 'POSITION'                                           716
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY_ALPHAY'                             716
         CALL AXPOSITION                                                  716
C    *                                                                    716
C    Geant POSITION : TMAN ===> X=-TECW_GAPWIDI/2.-DX_DZ*(TECW_GAPHEIT/2  716
C    )+(1+DX_DZ*DX_DZ)*TECW_MANIWID/2 Y=-TPCG_WHEELTHK/2+TECW_PIPETHK+TE  716
C    CW_MANITHK/2 Z=0.0 DZ=(1.+DX_DZ*DX_DZ)*(TECW_GAPHEIT/2-TECW_MANIWID  716
C    ) DX=TECW_MANIWID/2 DY=TECW_MANITHK/2 ALPHAY=-15 <===                716
         CALL AGSRESET                                                    721
         AG_X = -TECW_GAPWIDI/2.-DX_DZ*(TECW_GAPHEIT/                     721
     *   2)+(1+DX_DZ*DX_DZ)*TECW_MANIWID/2                                721
         AG_Y = -TPCG_WHEELTHK/2+TECW_PIPETHK+TECW_MANITHK/2              721
         AG_Z = 0.0                                                       721
         AG_DZ = (1.+DX_DZ*DX_DZ)*(TECW_GAPHEIT/2-TECW_MANIWID)           721
         AG_DX = TECW_MANIWID/2                                           721
         AG_DY = TECW_MANITHK/2                                           721
         AG_ALPHAN=AG_ALPHAN+1                                            721
         AG_ALPHAK(AG_ALPHAN)=2                                           721
         AG_ALPHAL(AG_ALPHAN)=-15                                         721
         AG_TITLE = 'TMAN'                                                721
         AG_EXNAME = 'POSITION'                                           721
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY_ALPHAY'                             721
         CALL AXPOSITION                                                  721
C    *                                                                    721
C    Create TCRX :                                                        722
C    Call To Local Procedure TCRX                                         722
         ASSIGN 5900 TO BLOCK_TCRX                                        722
         GO TO  5890                                                      722
5900     CONTINUE                                                         722
C    *                                                                    722
C    Create TALC :                                                        723
C    Call To Local Procedure TALC                                         723
         ASSIGN 5920 TO BLOCK_TALC                                        723
         GO TO  5910                                                      723
5920     CONTINUE                                                         723
C    *                                                                    723
C    *                                                                    724
         I_NHP=1                                                          724
            GO TO 5933                                                    724
5931        I_NHP=I_NHP+1                                                 724
5933        IF(I_NHP-(16).GT.0) GO TO 5932                                724
C       Check TROV_NHP(I_NHP)>0                                           725
            IF (TROV_NHP(I_NHP).GT.0) THEN                                725
C       FIRST PL                                                          726
            HPLA = -TECW_GAPHEIT/2+RADOFF+TECW_Z(1)-TECW_DZ(1)/           726
     *      2+TROV_OFFCRDIO                                               726
            ZVALUE = HPLA+4.0*(I_NHP-1)                                   727
C       *                                                                 728
            J_NHO=1                                                       728
               GO TO 5943                                                 728
5941           J_NHO=J_NHO+1                                              728
5943           IF(J_NHO-(TROV_NHP(I_NHP)).GT.0) GO TO 5942                728
C          Geant POSITION : TCRX ===> X=TROV_HX(J_NHO,I_NHP) Z=ZVALUE+TE  729
C          CW_CARDOFF <===                                                729
               CALL AGSRESET                                              729
               AG_X = TROV_HX(J_NHO,I_NHP)                                729
               AG_Z = ZVALUE+TECW_CARDOFF                                 729
               AG_TITLE = 'TCRX'                                          729
               AG_EXNAME = 'POSITION'                                     729
               AG_PARLIST = 'X_Z'                                         729
               CALL AXPOSITION                                            729
C          *                                                              729
C          Geant POSITION : TALC ===> X=TROV_HX(J_NHO,I_NHP) Z=ZVALUE+TE  729
C          CW_COOLOFF <===                                                729
               CALL AGSRESET                                              730
               AG_X = TROV_HX(J_NHO,I_NHP)                                730
               AG_Z = ZVALUE+TECW_COOLOFF                                 730
               AG_TITLE = 'TALC'                                          730
               AG_EXNAME = 'POSITION'                                     730
               AG_PARLIST = 'X_Z'                                         730
               CALL AXPOSITION                                            730
C          *                                                              730
            GO TO 5941                                                    731
5942        CONTINUE                                                      731
            ENDIF                                                         732
         GO TO 5931                                                       733
5932     CONTINUE                                                         733
5781  CALL AGSPOPB                                                        734
      GO TO BLOCK_TWGI, (5780 ,5790)                                      734
C End of Local Procedure                                                  734
C ----------------------------------------------------------------------  735
C ------------ TWGC                                                       735
C Begin Local Procedure TWGC                                              736
C TWGC IS THE LARGER INNER AIR GAP IN WHEEL                               736
5800  CONTINUE                                                            736
         AG_TITLE='TWGC'                                                  736
         AG_MODULE=MODULE                                                 736
         AG_MARK='V00'                                                    736
         CALL AGSPUSH                                                     736
         AG_MODULE=MODULE                                                 736
C    *                                                                    736
C    Geant MATERIAL : AIR ===> inherited arguments used <===              737
         CALL AGSRESET                                                    737
         AG_TITLE = 'AIR'                                                 737
         AG_EXNAME = 'MATERIAL'                                           737
         AG_PARLIST = 'NONE'                                              737
         CALL AXMATERIAL                                                  737
C    *                                                                    737
C    Geant ATTRIBUTE : TWGC ===> SEEN=1 COLO=7 <===                       737
         CALL AGSRESET                                                    738
         AG_SEEN = 1                                                      738
         AG_COLO = 7                                                      738
         AG_TITLE = 'TWGC'                                                738
         AG_EXNAME = 'ATTRIBUTE'                                          738
         AG_PARLIST = 'SEEN_COLO'                                         738
         CALL AXATTRIBUTE                                                 738
C    *                                                                    738
C    Geant SHAPE : TRD1 ===> DX1=0 DX2=0 DY=0 DZ=0 <===                   738
         CALL AGSRESET                                                    739
         AG_DX1 = 0                                                       739
         AG_DX2 = 0                                                       739
         AG_DY = 0                                                        739
         AG_DZ = 0                                                        739
         AG_TITLE = 'TRD1'                                                739
         AG_EXNAME = 'SHAPE'                                              739
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     739
         CALL AXSHAPE                                                     739
C    *                                                                    739
5801  CALL AGSPOPB                                                        740
      GO TO BLOCK_TWGC, (5800 ,5810)                                      740
C End of Local Procedure                                                  740
C ----------------------------------------------------------------------  741
C ------------ TWGB                                                       741
C Begin Local Procedure TWGB                                              742
C TWGB IS ADDED ALUM BLOCKSIN WHEEL                                       742
5820  CONTINUE                                                            742
         AG_TITLE='TWGB'                                                  742
         AG_MODULE=MODULE                                                 742
         AG_MARK='V00'                                                    742
         CALL AGSPUSH                                                     742
         AG_MODULE=MODULE                                                 742
C    *                                                                    742
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        743
         CALL AGSRESET                                                    743
         AG_TITLE = 'ALUMINIUM'                                           743
         AG_EXNAME = 'MATERIAL'                                           743
         AG_PARLIST = 'NONE'                                              743
         CALL AXMATERIAL                                                  743
C    *                                                                    743
C    Geant ATTRIBUTE : TWGB ===> SEEN=1 <===                              743
         CALL AGSRESET                                                    744
         AG_SEEN = 1                                                      744
         AG_TITLE = 'TWGB'                                                744
         AG_EXNAME = 'ATTRIBUTE'                                          744
         AG_PARLIST = 'SEEN'                                              744
         CALL AXATTRIBUTE                                                 744
C    *                                                                    744
C    Geant SHAPE : BOX ===> DX=TECW_WHLIPWID/2 DY=(TPCG_WHEELTHK-TECW_WH  744
C    LIPTHK)/2, DZ=TECW_WHBLKLEN/2 <===                                   744
         CALL AGSRESET                                                    746
         AG_DX = TECW_WHLIPWID/2                                          746
         AG_DY = (TPCG_WHEELTHK-TECW_WHLIPTHK)/2                          746
         AG_DZ = TECW_WHBLKLEN/2                                          746
         AG_TITLE = 'BOX'                                                 746
         AG_EXNAME = 'SHAPE'                                              746
         AG_PARLIST = 'DX_DY_DZ'                                          746
         CALL AXSHAPE                                                     746
C    *                                                                    746
5821  CALL AGSPOPB                                                        747
      GO TO BLOCK_TWGB, (5820 ,5830)                                      747
C End of Local Procedure                                                  747
C ----------------------------------------------------------------------  748
C ------------ TMAN                                                       748
C Begin Local Procedure TMAN                                              749
C TMAN IS ALUMINUM WATER MANIFOLD                                         749
5870  CONTINUE                                                            749
         AG_TITLE='TMAN'                                                  749
         AG_MODULE=MODULE                                                 749
         AG_MARK='V00'                                                    749
         CALL AGSPUSH                                                     749
         AG_MODULE=MODULE                                                 749
C    *                                                                    749
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        750
         CALL AGSRESET                                                    750
         AG_TITLE = 'ALUMINIUM'                                           750
         AG_EXNAME = 'MATERIAL'                                           750
         AG_PARLIST = 'NONE'                                              750
         CALL AXMATERIAL                                                  750
C    *                                                                    750
C    Geant ATTRIBUTE : TWGB ===> SEEN=1 <===                              750
         CALL AGSRESET                                                    751
         AG_SEEN = 1                                                      751
         AG_TITLE = 'TWGB'                                                751
         AG_EXNAME = 'ATTRIBUTE'                                          751
         AG_PARLIST = 'SEEN'                                              751
         CALL AXATTRIBUTE                                                 751
C    *                                                                    751
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           751
         CALL AGSRESET                                                    752
         AG_DX = 0                                                        752
         AG_DY = 0                                                        752
         AG_DZ = 0                                                        752
         AG_TITLE = 'BOX'                                                 752
         AG_EXNAME = 'SHAPE'                                              752
         AG_PARLIST = 'DX_DY_DZ'                                          752
         CALL AXSHAPE                                                     752
C    *                                                                    752
5871  CALL AGSPOPB                                                        753
      GO TO BLOCK_TMAN, (5870 ,5880)                                      753
C End of Local Procedure                                                  753
C ----------------------------------------------------------------------  754
C ------------ TPIP                                                       754
C Begin Local Procedure TPIP                                              755
C TPIP IS A WATER PIPE THERE ARE LOTS PER SECT                            755
5850  CONTINUE                                                            755
         AG_TITLE='TPIP'                                                  755
         AG_MODULE=MODULE                                                 755
         AG_MARK='V00'                                                    755
         CALL AGSPUSH                                                     755
         AG_MODULE=MODULE                                                 755
C    *                                                                    755
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        756
         CALL AGSRESET                                                    756
         AG_TITLE = 'ALUMINIUM'                                           756
         AG_EXNAME = 'MATERIAL'                                           756
         AG_PARLIST = 'NONE'                                              756
         CALL AXMATERIAL                                                  756
C    *                                                                    756
C    Geant ATTRIBUTE : TPIP ===> SEEN=1 COLO=4 <===                       756
         CALL AGSRESET                                                    757
         AG_SEEN = 1                                                      757
         AG_COLO = 4                                                      757
         AG_TITLE = 'TPIP'                                                757
         AG_EXNAME = 'ATTRIBUTE'                                          757
         AG_PARLIST = 'SEEN_COLO'                                         757
         CALL AXATTRIBUTE                                                 757
C    *                                                                    757
C    Geant SHAPE : TRD1 ===> DX1=0 DX2=0 DY=0 DZ=0 <===                   757
         CALL AGSRESET                                                    758
         AG_DX1 = 0                                                       758
         AG_DX2 = 0                                                       758
         AG_DY = 0                                                        758
         AG_DZ = 0                                                        758
         AG_TITLE = 'TRD1'                                                758
         AG_EXNAME = 'SHAPE'                                              758
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     758
         CALL AXSHAPE                                                     758
C    *                                                                    758
5851  CALL AGSPOPB                                                        759
      GO TO BLOCK_TPIP, (5850 ,5860)                                      759
C End of Local Procedure                                                  759
C ----------------------------------------------------------------------  760
C ------------ TCRX                                                       760
C Begin Local Procedure TCRX                                              761
C TCRX IS PART OF THE G10 FOR THE PC BOARDS                               761
5890  CONTINUE                                                            761
         AG_TITLE='TCRX'                                                  761
         AG_MODULE=MODULE                                                 761
         AG_MARK='V00'                                                    761
         CALL AGSPUSH                                                     761
         AG_MODULE=MODULE                                                 761
C    *                                                                    761
C    Geant MATERIAL : G10 ===> inherited arguments used <===              762
         CALL AGSRESET                                                    762
         AG_TITLE = 'G10'                                                 762
         AG_EXNAME = 'MATERIAL'                                           762
         AG_PARLIST = 'NONE'                                              762
         CALL AXMATERIAL                                                  762
C    *                                                                    762
C    Geant ATTRIBUTE : TCRX ===> SEEN=1 COLO=4 <===                       762
         CALL AGSRESET                                                    763
         AG_SEEN = 1                                                      763
         AG_COLO = 4                                                      763
         AG_TITLE = 'TCRX'                                                763
         AG_EXNAME = 'ATTRIBUTE'                                          763
         AG_PARLIST = 'SEEN_COLO'                                         763
         CALL AXATTRIBUTE                                                 763
C    *                                                                    763
C    Geant SHAPE : BOX ===> DX=TECW_CARDW/2 DY=TPCG_WHEELTHK/2, DZ=TECW_  763
C    CARDTH/2 <===                                                        763
         CALL AGSRESET                                                    765
         AG_DX = TECW_CARDW/2                                             765
         AG_DY = TPCG_WHEELTHK/2                                          765
         AG_DZ = TECW_CARDTH/2                                            765
         AG_TITLE = 'BOX'                                                 765
         AG_EXNAME = 'SHAPE'                                              765
         AG_PARLIST = 'DX_DY_DZ'                                          765
         CALL AXSHAPE                                                     765
C    *                                                                    765
5891  CALL AGSPOPB                                                        766
      GO TO BLOCK_TCRX, (5890 ,5900)                                      766
C End of Local Procedure                                                  766
C ----------------------------------------------------------------------  767
C ------------ TALC                                                       767
C Begin Local Procedure TALC                                              768
C TALC IS PART OF THE HEAT SHIELD FOR THE PC BOARDS                       768
5910  CONTINUE                                                            768
         AG_TITLE='TALC'                                                  768
         AG_MODULE=MODULE                                                 768
         AG_MARK='V00'                                                    768
         CALL AGSPUSH                                                     768
         AG_MODULE=MODULE                                                 768
C    *                                                                    768
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        769
         CALL AGSRESET                                                    769
         AG_TITLE = 'ALUMINIUM'                                           769
         AG_EXNAME = 'MATERIAL'                                           769
         AG_PARLIST = 'NONE'                                              769
         CALL AXMATERIAL                                                  769
C    *                                                                    769
C    Geant ATTRIBUTE : TALC ===> SEEN=1 COLO=6 <===                       769
         CALL AGSRESET                                                    770
         AG_SEEN = 1                                                      770
         AG_COLO = 6                                                      770
         AG_TITLE = 'TALC'                                                770
         AG_EXNAME = 'ATTRIBUTE'                                          770
         AG_PARLIST = 'SEEN_COLO'                                         770
         CALL AXATTRIBUTE                                                 770
C    *                                                                    770
C    Geant SHAPE : BOX ===> DX=TECW_COOLW/2 DY=TPCG_WHEELTHK/2, DZ=TECW_  770
C    COOLTH/2 <===                                                        770
         CALL AGSRESET                                                    772
         AG_DX = TECW_COOLW/2                                             772
         AG_DY = TPCG_WHEELTHK/2                                          772
         AG_DZ = TECW_COOLTH/2                                            772
         AG_TITLE = 'BOX'                                                 772
         AG_EXNAME = 'SHAPE'                                              772
         AG_PARLIST = 'DX_DY_DZ'                                          772
         CALL AXSHAPE                                                     772
C    *                                                                    772
5911  CALL AGSPOPB                                                        773
      GO TO BLOCK_TALC, (5910 ,5920)                                      773
C End of Local Procedure                                                  773
C ----------------------------------------------------------------------  774
C ------------ TRDV                                                       774
C Begin Local Procedure TRDV                                              775
C TRDV IS THE RDO BOARD VOLUME                                            775
5180  CONTINUE                                                            775
         AG_TITLE='TRDV'                                                  775
         AG_MODULE=MODULE                                                 775
         AG_MARK='V00'                                                    775
         CALL AGSPUSH                                                     775
         AG_MODULE=MODULE                                                 775
C    *                                                                    775
C    Geant MATERIAL : AIR ===> inherited arguments used <===              776
         CALL AGSRESET                                                    776
         AG_TITLE = 'AIR'                                                 776
         AG_EXNAME = 'MATERIAL'                                           776
         AG_PARLIST = 'NONE'                                              776
         CALL AXMATERIAL                                                  776
C    *                                                                    776
C    Geant ATTRIBUTE : TRDV ===> SEEN=1 COLO=2 <===                       776
         CALL AGSRESET                                                    777
         AG_SEEN = 1                                                      777
         AG_COLO = 2                                                      777
         AG_TITLE = 'TRDV'                                                777
         AG_EXNAME = 'ATTRIBUTE'                                          777
         AG_PARLIST = 'SEEN_COLO'                                         777
         CALL AXATTRIBUTE                                                 777
C    *                                                                    777
C    Geant SHAPE : TUBE ===> RMIN=TPCG_WHEELIR RMAX=TPCG_WHEELOR, DZ=TRD  777
C    O_RDOVTHK/2 <===                                                     777
         CALL AGSRESET                                                    779
         AG_RMIN = TPCG_WHEELIR                                           779
         AG_RMAX = TPCG_WHEELOR                                           779
         AG_DZ = TRDO_RDOVTHK/2                                           779
         AG_TITLE = 'TUBE'                                                779
         AG_EXNAME = 'SHAPE'                                              779
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      779
         CALL AXSHAPE                                                     779
C    *                                                                    779
C    Create TRDS :                                                        780
C    Call To Local Procedure TRDS                                         780
         ASSIGN 5960 TO BLOCK_TRDS                                        780
         GO TO  5950                                                      780
5960     CONTINUE                                                         780
C    *                                                                    780
5181  CALL AGSPOPB                                                        781
      GO TO BLOCK_TRDV, (5180 ,5190)                                      781
C End of Local Procedure                                                  781
C ----------------------------------------------------------------------  782
C ------------ TRDS                                                       782
C Begin Local Procedure TRDS                                              783
C TRDS IS A DIVISION OF RDO BOARD VOLUME CORRESPONDING TO ONE SUPERSECTO  783
C R                                                                       783
5950  CONTINUE                                                            783
         AG_TITLE='TRDS'                                                  783
         AG_MODULE=MODULE                                                 783
         AG_MARK='V00'                                                    783
         CALL AGSPUSH                                                     783
         AG_MODULE=MODULE                                                 783
C    *                                                                    783
C    Geant ATTRIBUTE : TRDS ===> SEEN=1 <===                              784
         CALL AGSRESET                                                    784
         AG_SEEN = 1                                                      784
         AG_TITLE = 'TRDS'                                                784
         AG_EXNAME = 'ATTRIBUTE'                                          784
         AG_PARLIST = 'SEEN'                                              784
         CALL AXATTRIBUTE                                                 784
C    *                                                                    784
C    Geant SHAPE : DIVISION ===> NDIV=12 IAXIS=2 <===                     784
         CALL AGSRESET                                                    785
         AG_NDIV = 12                                                     785
         AG_IAXIS = 2                                                     785
         AG_TITLE = 'DIVISION'                                            785
         AG_EXNAME = 'SHAPE'                                              785
         AG_PARLIST = 'NDIV_IAXIS'                                        785
         CALL AXSHAPE                                                     785
C    *                                                                    785
C    use default TRDO bank                                                785
         BPATH_TRDO='TRDO*'                                               786
         IDNUM_TRDO(1)=0                                                  786
         CALL AGDGETP('TPCEGEO2','*',-999,'TRDO',' ',BANK_TRDO,           786
     *   BPATH_TRDO,IDNUM_TRDO,1,MAP_TRDO,NAMES_TRDO,BANK_TRDO,LEN_TRDO,  786
     *   LLEN_TRDO,LINK_TRDO,FLAG_TRDO)                                   786
C    *                                                                    787
         KK=1                                                             787
            GO TO 5973                                                    787
5971        KK=KK+1                                                       787
5973        IF(KK-(TRDO_NRDOBRD).GT.0) GO TO 5972                         787
C       Create TRDC :                                                     788
C       Call To Local Procedure TRDC                                      788
            ASSIGN 5990 TO BLOCK_TRDC                                     788
            GO TO  5980                                                   788
5990        CONTINUE                                                      788
C       *                                                                 788
C       Geant POSITION : TRDC ===> X=TRDO_RDOHT(KK) DZ=TRDO_RDOTHK/2 DX=  790
C       (TRDO_RDOHT(KK)*TECW_TAN15)-TECW_CLEARANCE DY=TRDO_RDOLEN/2 ORT=  790
C       YZX <===                                                          790
            CALL AGSRESET                                                 790
            AG_X = TRDO_RDOHT(KK)                                         790
            AG_DZ = TRDO_RDOTHK/2                                         790
            AG_DX = (TRDO_RDOHT(KK)*TECW_TAN15)-TECW_CLEARANCE            790
            AG_DY = TRDO_RDOLEN/2                                         790
            AG_ALPHAN=AG_ALPHAN+1                                         790
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              790
            AG_TITLE = 'TRDC'                                             790
            AG_EXNAME = 'POSITION'                                        790
            AG_PARLIST = 'X_DZ_DX_DY_ORT'                                 790
            CALL AXPOSITION                                               790
C       *                                                                 790
         GO TO 5971                                                       791
5972     CONTINUE                                                         791
5951  CALL AGSPOPB                                                        792
      GO TO BLOCK_TRDS, (5950 ,5960)                                      792
C End of Local Procedure                                                  792
C ----------------------------------------------------------------------  793
C ------------ TRDC                                                       793
C Begin Local Procedure TRDC                                              794
C TRDC IS AN RDO CARD                                                     794
5980  CONTINUE                                                            794
         AG_TITLE='TRDC'                                                  794
         AG_MODULE=MODULE                                                 794
         AG_MARK='V00'                                                    794
         CALL AGSPUSH                                                     794
         AG_MODULE=MODULE                                                 794
C    *                                                                    794
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===          795
         CALL AGSRESET                                                    795
         AG_A = 28.08                                                     795
         AG_Z = 14                                                        795
         AG_W = 0.6*1*28./60.                                             795
         AG_TITLE = 'SI'                                                  795
         AG_EXNAME = 'COMPONENT'                                          795
         AG_PARLIST = 'A_Z_W'                                             795
         CALL AXCOMPONENT                                                 795
C    *                                                                    795
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. <===               795
         CALL AGSRESET                                                    796
         AG_A = 16                                                        796
         AG_Z = 8                                                         796
         AG_W = 0.6*2*16./60.                                             796
         AG_TITLE = 'O'                                                   796
         AG_EXNAME = 'COMPONENT'                                          796
         AG_PARLIST = 'A_Z_W'                                             796
         CALL AXCOMPONENT                                                 796
C    *                                                                    796
C    Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===              796
         CALL AGSRESET                                                    797
         AG_A = 12                                                        797
         AG_Z = 6                                                         797
         AG_W = 0.4*8*12./174.                                            797
         AG_TITLE = 'C'                                                   797
         AG_EXNAME = 'COMPONENT'                                          797
         AG_PARLIST = 'A_Z_W'                                             797
         CALL AXCOMPONENT                                                 797
C    *                                                                    797
C    Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===               797
         CALL AGSRESET                                                    798
         AG_A = 1                                                         798
         AG_Z = 1                                                         798
         AG_W = 0.4*14*1./174.                                            798
         AG_TITLE = 'H'                                                   798
         AG_EXNAME = 'COMPONENT'                                          798
         AG_PARLIST = 'A_Z_W'                                             798
         CALL AXCOMPONENT                                                 798
C    *                                                                    798
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.4*4*16./174. <===              798
         CALL AGSRESET                                                    799
         AG_A = 16                                                        799
         AG_Z = 8                                                         799
         AG_W = 0.4*4*16./174.                                            799
         AG_TITLE = 'O'                                                   799
         AG_EXNAME = 'COMPONENT'                                          799
         AG_PARLIST = 'A_Z_W'                                             799
         CALL AXCOMPONENT                                                 799
C    *                                                                    799
C    Geant MIXTURE : G10 ===> DENS=1.7 <===                               799
         CALL AGSRESET                                                    800
         AG_DENS = 1.7                                                    800
         AG_TITLE = 'G10'                                                 800
         AG_EXNAME = 'MIXTURE'                                            800
         AG_PARLIST = 'DENS'                                              800
         CALL AXMIXTURE                                                   800
C    *                                                                    800
C    Geant MATERIAL : G10 ===> inherited arguments used <===              800
         CALL AGSRESET                                                    801
         AG_TITLE = 'G10'                                                 801
         AG_EXNAME = 'MATERIAL'                                           801
         AG_PARLIST = 'NONE'                                              801
         CALL AXMATERIAL                                                  801
C    *                                                                    801
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           801
         CALL AGSRESET                                                    802
         AG_DX = 0                                                        802
         AG_DY = 0                                                        802
         AG_DZ = 0                                                        802
         AG_TITLE = 'BOX'                                                 802
         AG_EXNAME = 'SHAPE'                                              802
         AG_PARLIST = 'DX_DY_DZ'                                          802
         CALL AXSHAPE                                                     802
C    *                                                                    802
5981  CALL AGSPOPB                                                        803
      GO TO BLOCK_TRDC, (5980 ,5990)                                      803
C End of Local Procedure                                                  803
      WRITE(LOUT,6000)'===>>  [End AgML Module TPCEGEO2]  <<==='          803
6000  FORMAT(A32,/,/)                                                     803
C MODULE TPCEGEO2                                                         805
1     CONTINUE                                                            805
      END                                                                 805
