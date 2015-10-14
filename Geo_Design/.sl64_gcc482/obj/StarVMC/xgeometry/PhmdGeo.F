C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE PHMDGEO                                                  1
C PHMDGEO IS THE GEOMETRY OF PHOTON MULTIPLICITY DETECTOR                 1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'PHMDGEO'/, BANK_TITLE/'*'/                             1
      DATA MCOMMENT/'IS THE GEOMETRY OF PHOTON MULTIPLICITY DETECTOR'/    1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
      DATA AG_I/0/,AG_J/0/                                                1
C - Advanced Geant interface                                              2
      CHARACTER*20 AG_MODULE,AG_TITLE,AG_EXNAME,AG_PARLIST,AG_CHDIR,      2
     *AG_MATERIAL,AG_MIXTURE,AG_COMPONENT,AG_MEDIUM                       2
      CHARACTER*4 AG_VOLUME,AG_MOTHER,AG_SHAPE,AG_CNICK,AG_KONLY,         2
     *AG_OPTION,AG_ORT,AG_MARK                                            2
      INTEGER AG_BEGCOM,AG_IVOLUME,AG_IMOTHER,AG_IGNUM,AG_ISHAPE,         2
     *AG_IMED,AG_IMAT,AG_IFIELD, AG_NLMAT,AG_IERROR,AG_NWBUF,AG_NPAR,     2
     *AG_ISTATUS,AG_IROT,AG_NBITS,AG_SERIAL, AG_ATTRIBUTE(6),AG_WORK,     2
     *AG_SEEN,AG_LSTY,AG_LWID,AG_COLO,AG_FILL,AG_JDU,AG_ORTI, AG_NDIV,    2
     *AG_IAXIS,AG_NDVMAX,AG_IPRIN,AG_RESET1,AG_RESET2,AG_BEGSCR,          2
     *AG_ENDSCR, AG_IDTYPE,AG_ISET,AG_IDET,AG_ISVOL,AG_LEVEL,AG_IRESER,   2
     *AG_IGEOM,AG_STANDALONE, AG_LSTACK,AG_NWUHIT,AG_NWUVOL,AG_MAGIC,     2
     *AG_LDETU,AG_NPDIV,AG_NPDV,AG_NZ,AG_NCOPY, AG_IDEBU,AG_IGRAP,        2
     *AG_IHIST,AG_IMFLD,AG_ISIMU,AG_ENDSAVE,AG_CODE,AG_TRKTYP, AG_MODE,   2
     *AG_ECODE,AG_PDG,IPRIN, AG_ALPHAN,AG_ALPHAK(10),IGOR                 2
      REAL AG_FIELDM,AG_TMAXFD,AG_STEMAX,AG_DEEMAX,AG_EPSIL,AG_STMIN,     2
     *AG_DENS,AG_RADL,AG_ABSL, AG_THETAX,AG_PHIX,AG_THETAY,AG_PHIY,       2
     *AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,AG_ALFAZ, AG_ALPHAL(10),        2
     *AG_ALPHAX,AG_ALPHAY,AG_ALPHAZ,AG_TYPE, AG_DX, AG_DX1, AG_DX2,       2
     *AG_DY, AG_DY1, AG_DY2, AG_DMAXMS, AG_TWIST, AG_THET, AG_THE1,       2
     *AG_THE2, AG_PHI, AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2,       2
     *AG_RMIN, AG_RMAX, AG_RMN, AG_RMX, AG_ZI , AG_RMN1, AG_RMN2,         2
     *AG_RMX1, AG_RMX2, AG_H1, AG_H2, AG_BL1, AG_BL2, AG_TL1, AG_TL2,     2
     *AG_DPHI, AG_DZ, AG_TWIS, AG_X, AG_Y, AG_Z, AG_A, AG_ZA, AG_W,       2
     *AG_STEP, AG_C0, AG_PAR, AG_AA, AG_ZZ, AG_WW, AG_LX, AG_LY, AG_LZ,   2
     *AG_HX, AG_HY, AG_HZ, AG_P1, AG_P2, AG_STACK,AG_UBUF,AG_XHMAX,       2
     *AG_YHMAX,AG_ZHMAX,AG_RHMAX,AG_FHMAX,AG_FHMIN,AG_BIN, AG_CHARGE,     2
     *AG_MASS,AG_TLIFE,AG_BRATIO                                          2
      PARAMETER (AG_LSTACK=130,AG_NWUHIT=10,AG_NWUVOL=3,AG_MAGIC=         2
     *-696969,AG_LDETU=250)                                               2
      COMMON/AGCGLOB/ AG_MODULE,AG_CHDIR,AG_LEVEL,AG_IDTYPE,AG_IERROR,    2
     *AG_STANDALONE,IPRIN, AG_IPRIN,AG_IGEOM,AG_IDEBU,AG_IGRAP,AG_IHIST,  2
     *AG_IMFLD,AG_ISIMU                                                   2
C Inherited variables a: saved during internal calls                      2
C b: reset at the entry in a block and then saved                         2
      COMMON/AGCPARA/ AG_BEGCOM, AG_IVOLUME, AG_IMOTHER, AG_IGNUM,        2
     *AG_ISHAPE, AG_IMED, AG_IMAT, AG_IFIELD, AG_FIELDM, AG_TMAXFD,       2
     *AG_STEMAX, AG_DEEMAX, AG_EPSIL, AG_STMIN, AG_DENS, AG_RADL,         2
     *AG_ABSL, AG_DX, AG_DX1, AG_DX2, AG_DY, AG_DY1, AG_DY2, AG_RMN1,     2
     *AG_RMN2, AG_RMX1, AG_RMX2, AG_THET, AG_THE1, AG_THE2, AG_PHI,       2
     *AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2, AG_H1, AG_BL1,         2
     *AG_TL1, AG_H2, AG_BL2, AG_TL2, AG_RMIN, AG_RMAX, AG_DPHI,           2
     *AG_NPDIV, AG_NZ, AG_DZ, AG_TWIS, AG_LX, AG_LY, AG_LZ, AG_HX,        2
     *AG_HY, AG_HZ, AG_A, AG_ZA, AG_W, AG_NLMAT, AG_WORK, AG_SEEN,        2
     *AG_LSTY, AG_LWID, AG_COLO, AG_FILL, AG_SERIAL, AG_ISVOL,            2
     *AG_ISTATUS, AG_ZI(16), AG_RMN(16), AG_RMX(16), AG_VOLUME,           2
     *AG_MOTHER, AG_SHAPE, AG_CNICK, AG_ENDSAVE, AG_RESET1, AG_THETAX,    2
     *AG_PHIX,AG_THETAY,AG_PHIY,AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,      2
     *AG_ALFAZ, AG_X,AG_Y,AG_Z, AG_STEP,AG_C0,AG_NDIV,AG_IAXIS,           2
     *AG_NDVMAX,AG_ORTI,AG_NCOPY, AG_ALPHAN,AG_ALPHAK,AG_ALPHAL,          2
     *AG_RESET2, AG_KONLY, AG_ORT, AG_MARK                                2
C local variables valid inside same block                                 2
      COMMON/AGCLOCA/ AG_BEGSCR,AG_UBUF(100),AG_PAR(100),AG_AA(20),       2
     *AG_ZZ(20),AG_WW(20),AG_NWBUF, AG_XHMAX,AG_YHMAX,AG_ZHMAX,AG_RHMAX,  2
     *AG_FHMAX,AG_FHMIN,AG_NBITS,AG_BIN,AG_TYPE, AG_IROT,AG_NPAR,         2
     *AG_ISET,AG_IDET,AG_JDU,AG_IRESER, AG_ENDSCR, AG_TITLE,AG_EXNAME,    2
     *AG_PARLIST,AG_MATERIAL,AG_MIXTURE, AG_COMPONENT,AG_MEDIUM,          2
     *AG_OPTION                                                           2
      COMMON/AGCPART/AG_CODE,AG_TRKTYP,AG_MASS,AG_CHARGE,AG_TLIFE,        2
     *AG_BRATIO(6),AG_MODE(6),AG_PDG,AG_ECODE                             2
      COMMON/AGCSTAC/ AG_STACK(AG_LSTACK,15)                              2
C temporarly                                                              2
      EQUIVALENCE (AG_ATTRIBUTE,AG_WORK),(AG_STEMAX,AG_DMAXMS),           2
     *(AG_ALPHAX,AG_ALFAX), (AG_ALPHAY,AG_ALFAY),(AG_ALPHAZ,AG_ALFAZ),    2
     *(AG_TWIST,AG_TWIS),(AG_NPDIV,AG_NPDV),  (AG_P1,AG_HX),(AG_P2,       2
     *AG_HY)                                                              2
      REAL MIL_P,INCH_P                                                   2
      PARAMETER (INCH_P=2.54,MIL_P=0.00254)                               2
C - GEANT description of logical units                                    2
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            3
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      3
      COMMON/GCMAIL/ CHMAIL                                               3
      CHARACTER*132 CHMAIL                                                3
C - Natural constants (or considered as such)                             3
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   4
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        4
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       4
C AUTHOR SUBHASIS, VIYOGI, BEDANGA,TAPAN AND DIPAK                        4
      CHARACTER*40 AUTHOR /'SUBHASIS, VIYOGI, BEDANGA,TAPAN AND DIPAK'/   5
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  5
C CREATED 03-JULY-2001                                                    5
      CHARACTER*40 CREATED /'03-JULY-2001'/                               6
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  6
      INTEGER BLOCK_PHMD                                                  7
      INTEGER BLOCK_PHMS                                                  7
      INTEGER BLOCK_PHSR                                                  7
      INTEGER BLOCK_PMDA                                                  7
      INTEGER BLOCK_AIRA                                                  7
      INTEGER BLOCK_PHCA                                                  7
      INTEGER BLOCK_PPBA                                                  7
      INTEGER BLOCK_PFEA                                                  7
      INTEGER BLOCK_PCBA                                                  7
      INTEGER BLOCK_BASA                                                  7
      INTEGER BLOCK_ASTR                                                  7
      INTEGER BLOCK_PSTR                                                  7
      INTEGER BLOCK_PDCU                                                  7
      INTEGER BLOCK_PDGS                                                  7
C -                                                                       8
      REAL PMVR_VERSION                                                   8
      EQUIVALENCE (BANK_PMVR(3),PMVR_VERSION,RE_PMVR_VERSION),            8
     *(NAMES_PMVR(1),NPMVR_VERSION)                                       8
      EQUIVALENCE (COMMEN_PMVR(1),CPMVR_VERSION),(MAP_PMVR(1,1),          8
     *MPMVR_VERSION)                                                      8
      CHARACTER CPMVR_VERSION*40/' '/                                     8
      CHARACTER NPMVR_VERSION*12/'R.VERSION'/                             8
      INTEGER MPMVR_VERSION(2)/1,1/                                       8
      REAL RE_PMVR_VERSION                                                8
C -                                                                       8
      INTEGER PMVR_CONFIG                                                 8
      EQUIVALENCE (BANK_PMVR(4),PMVR_CONFIG,RE_PMVR_CONFIG),              8
     *(NAMES_PMVR(2),NPMVR_CONFIG)                                        8
      EQUIVALENCE (COMMEN_PMVR(2),CPMVR_CONFIG),(MAP_PMVR(1,2),           8
     *MPMVR_CONFIG)                                                       8
      CHARACTER CPMVR_CONFIG*40/' '/                                      8
      CHARACTER NPMVR_CONFIG*12/'I.CONFIG'/                               8
      INTEGER MPMVR_CONFIG(2)/1,1/                                        8
      REAL RE_PMVR_CONFIG                                                 8
C -                                                                       8
      INTEGER LEN_PMVR,LLEN_PMVR,LINK_PMVR                                8
      PARAMETER (LEN_PMVR=4)                                              8
      PARAMETER (LLEN_PMVR=2)                                             8
      INTEGER FLAG_PMVR/0/,MAP_PMVR(2,LLEN_PMVR),IDNUM_PMVR(10)           8
      CHARACTER NAMES_PMVR(LLEN_PMVR)*12,COMMEN_PMVR(LLEN_PMVR)*40        8
      DATA IDNUM_PMVR/10*0/                                               8
      REAL BANK_PMVR(LEN_PMVR)                                            8
      CHARACTER*60 BPATH_PMVR/' '/                                        8
      SAVE FLAG_PMVR,MAP_PMVR,IDNUM_PMVR,NAMES_PMVR,COMMEN_PMVR,          8
     *BPATH_PMVR                                                          8
      DATA LINK_PMVR/0/,BANK_PMVR/LEN_PMVR*0/                             8
C *                                                                       8
C -                                                                       11
      REAL PMDG_VERSION                                                   11
      EQUIVALENCE (BANK_PMDG(3),PMDG_VERSION,RE_PMDG_VERSION),            11
     *(NAMES_PMDG(1),NPMDG_VERSION)                                       11
      EQUIVALENCE (COMMEN_PMDG(1),CPMDG_VERSION),(MAP_PMDG(1,1),          11
     *MPMDG_VERSION)                                                      11
      CHARACTER CPMDG_VERSION*40/' '/                                     11
      CHARACTER NPMDG_VERSION*12/'R.VERSION'/                             11
      INTEGER MPMDG_VERSION(2)/1,1/                                       11
      REAL RE_PMDG_VERSION                                                11
C -                                                                       11
      REAL PMDG_M_MAX                                                     11
      EQUIVALENCE (BANK_PMDG(4),PMDG_M_MAX,RE_PMDG_M_MAX),                11
     *(NAMES_PMDG(2),NPMDG_M_MAX)                                         11
      EQUIVALENCE (COMMEN_PMDG(2),CPMDG_M_MAX),(MAP_PMDG(1,2),            11
     *MPMDG_M_MAX)                                                        11
      CHARACTER CPMDG_M_MAX*40/' '/                                       11
      CHARACTER NPMDG_M_MAX*12/'R.M_MAX'/                                 11
      INTEGER MPMDG_M_MAX(2)/1,1/                                         11
      REAL RE_PMDG_M_MAX                                                  11
C -                                                                       11
      REAL PMDG_M_MIN                                                     11
      EQUIVALENCE (BANK_PMDG(5),PMDG_M_MIN,RE_PMDG_M_MIN),                11
     *(NAMES_PMDG(3),NPMDG_M_MIN)                                         11
      EQUIVALENCE (COMMEN_PMDG(3),CPMDG_M_MIN),(MAP_PMDG(1,3),            11
     *MPMDG_M_MIN)                                                        11
      CHARACTER CPMDG_M_MIN*40/' '/                                       11
      CHARACTER NPMDG_M_MIN*12/'R.M_MIN'/                                 11
      INTEGER MPMDG_M_MIN(2)/1,1/                                         11
      REAL RE_PMDG_M_MIN                                                  11
C -                                                                       11
      REAL PMDG_ZDIST(2)                                                  11
      EQUIVALENCE (BANK_PMDG(6),PMDG_ZDIST,RE_PMDG_ZDIST),                11
     *(NAMES_PMDG(4),NPMDG_ZDIST)                                         11
      EQUIVALENCE (COMMEN_PMDG(4),CPMDG_ZDIST),(MAP_PMDG(1,4),            11
     *MPMDG_ZDIST)                                                        11
      CHARACTER CPMDG_ZDIST*40/' '/                                       11
      CHARACTER NPMDG_ZDIST*12/'R.ZDIST'/                                 11
      INTEGER MPMDG_ZDIST(2)/2,1/                                         11
      REAL RE_PMDG_ZDIST                                                  11
C -                                                                       11
      REAL PMDG_DPMDX                                                     11
      EQUIVALENCE (BANK_PMDG(8),PMDG_DPMDX,RE_PMDG_DPMDX),                11
     *(NAMES_PMDG(5),NPMDG_DPMDX)                                         11
      EQUIVALENCE (COMMEN_PMDG(5),CPMDG_DPMDX),(MAP_PMDG(1,5),            11
     *MPMDG_DPMDX)                                                        11
      CHARACTER CPMDG_DPMDX*40/' '/                                       11
      CHARACTER NPMDG_DPMDX*12/'R.DPMDX'/                                 11
      INTEGER MPMDG_DPMDX(2)/1,1/                                         11
      REAL RE_PMDG_DPMDX                                                  11
C -                                                                       11
      REAL PMDG_DPMDY                                                     11
      EQUIVALENCE (BANK_PMDG(9),PMDG_DPMDY,RE_PMDG_DPMDY),                11
     *(NAMES_PMDG(6),NPMDG_DPMDY)                                         11
      EQUIVALENCE (COMMEN_PMDG(6),CPMDG_DPMDY),(MAP_PMDG(1,6),            11
     *MPMDG_DPMDY)                                                        11
      CHARACTER CPMDG_DPMDY*40/' '/                                       11
      CHARACTER NPMDG_DPMDY*12/'R.DPMDY'/                                 11
      INTEGER MPMDG_DPMDY(2)/1,1/                                         11
      REAL RE_PMDG_DPMDY                                                  11
C -                                                                       11
      REAL PMDG_DPMDZ                                                     11
      EQUIVALENCE (BANK_PMDG(10),PMDG_DPMDZ,RE_PMDG_DPMDZ),               11
     *(NAMES_PMDG(7),NPMDG_DPMDZ)                                         11
      EQUIVALENCE (COMMEN_PMDG(7),CPMDG_DPMDZ),(MAP_PMDG(1,7),            11
     *MPMDG_DPMDZ)                                                        11
      CHARACTER CPMDG_DPMDZ*40/' '/                                       11
      CHARACTER NPMDG_DPMDZ*12/'R.DPMDZ'/                                 11
      INTEGER MPMDG_DPMDZ(2)/1,1/                                         11
      REAL RE_PMDG_DPMDZ                                                  11
C -                                                                       11
      REAL PMDG_PARGCZ                                                    11
      EQUIVALENCE (BANK_PMDG(11),PMDG_PARGCZ,RE_PMDG_PARGCZ),             11
     *(NAMES_PMDG(8),NPMDG_PARGCZ)                                        11
      EQUIVALENCE (COMMEN_PMDG(8),CPMDG_PARGCZ),(MAP_PMDG(1,8),           11
     *MPMDG_PARGCZ)                                                       11
      CHARACTER CPMDG_PARGCZ*40/' '/                                      11
      CHARACTER NPMDG_PARGCZ*12/'R.PARGCZ'/                               11
      INTEGER MPMDG_PARGCZ(2)/1,1/                                        11
      REAL RE_PMDG_PARGCZ                                                 11
C -                                                                       11
      REAL PMDG_PARSCZ                                                    11
      EQUIVALENCE (BANK_PMDG(12),PMDG_PARSCZ,RE_PMDG_PARSCZ),             11
     *(NAMES_PMDG(9),NPMDG_PARSCZ)                                        11
      EQUIVALENCE (COMMEN_PMDG(9),CPMDG_PARSCZ),(MAP_PMDG(1,9),           11
     *MPMDG_PARSCZ)                                                       11
      CHARACTER CPMDG_PARSCZ*40/' '/                                      11
      CHARACTER NPMDG_PARSCZ*12/'R.PARSCZ'/                               11
      INTEGER MPMDG_PARSCZ(2)/1,1/                                        11
      REAL RE_PMDG_PARSCZ                                                 11
C -                                                                       11
      REAL PMDG_PARFEZ                                                    11
      EQUIVALENCE (BANK_PMDG(13),PMDG_PARFEZ,RE_PMDG_PARFEZ),             11
     *(NAMES_PMDG(10),NPMDG_PARFEZ)                                       11
      EQUIVALENCE (COMMEN_PMDG(10),CPMDG_PARFEZ),(MAP_PMDG(1,10),         11
     *MPMDG_PARFEZ)                                                       11
      CHARACTER CPMDG_PARFEZ*40/' '/                                      11
      CHARACTER NPMDG_PARFEZ*12/'R.PARFEZ'/                               11
      INTEGER MPMDG_PARFEZ(2)/1,1/                                        11
      REAL RE_PMDG_PARFEZ                                                 11
C -                                                                       11
      REAL PMDG_PARPBZ                                                    11
      EQUIVALENCE (BANK_PMDG(14),PMDG_PARPBZ,RE_PMDG_PARPBZ),             11
     *(NAMES_PMDG(11),NPMDG_PARPBZ)                                       11
      EQUIVALENCE (COMMEN_PMDG(11),CPMDG_PARPBZ),(MAP_PMDG(1,11),         11
     *MPMDG_PARPBZ)                                                       11
      CHARACTER CPMDG_PARPBZ*40/' '/                                      11
      CHARACTER NPMDG_PARPBZ*12/'R.PARPBZ'/                               11
      INTEGER MPMDG_PARPBZ(2)/1,1/                                        11
      REAL RE_PMDG_PARPBZ                                                 11
C -                                                                       11
      REAL PMDG_CELL_RADIUS                                               11
      EQUIVALENCE (BANK_PMDG(15),PMDG_CELL_RADIUS,RE_PMDG_CELL_RADIUS),   11
     *(NAMES_PMDG(12),NPMDG_CELL_RADIUS)                                  11
      EQUIVALENCE (COMMEN_PMDG(12),CPMDG_CELL_RADIUS),(MAP_PMDG(1,12),    11
     *MPMDG_CELL_RADIUS)                                                  11
      CHARACTER CPMDG_CELL_RADIUS*40/' '/                                 11
      CHARACTER NPMDG_CELL_RADIUS*12/'R.CELL_RADIUS'/                     11
      INTEGER MPMDG_CELL_RADIUS(2)/1,1/                                   11
      REAL RE_PMDG_CELL_RADIUS                                            11
C -                                                                       11
      REAL PMDG_CELL_DEPTH                                                11
      EQUIVALENCE (BANK_PMDG(16),PMDG_CELL_DEPTH,RE_PMDG_CELL_DEPTH),     11
     *(NAMES_PMDG(13),NPMDG_CELL_DEPTH)                                   11
      EQUIVALENCE (COMMEN_PMDG(13),CPMDG_CELL_DEPTH),(MAP_PMDG(1,13),     11
     *MPMDG_CELL_DEPTH)                                                   11
      CHARACTER CPMDG_CELL_DEPTH*40/' '/                                  11
      CHARACTER NPMDG_CELL_DEPTH*12/'R.CELL_DEPTH'/                       11
      INTEGER MPMDG_CELL_DEPTH(2)/1,1/                                    11
      REAL RE_PMDG_CELL_DEPTH                                             11
C -                                                                       11
      REAL PMDG_CELL_WALL                                                 11
      EQUIVALENCE (BANK_PMDG(17),PMDG_CELL_WALL,RE_PMDG_CELL_WALL),       11
     *(NAMES_PMDG(14),NPMDG_CELL_WALL)                                    11
      EQUIVALENCE (COMMEN_PMDG(14),CPMDG_CELL_WALL),(MAP_PMDG(1,14),      11
     *MPMDG_CELL_WALL)                                                    11
      CHARACTER CPMDG_CELL_WALL*40/' '/                                   11
      CHARACTER NPMDG_CELL_WALL*12/'R.CELL_WALL'/                         11
      INTEGER MPMDG_CELL_WALL(2)/1,1/                                     11
      REAL RE_PMDG_CELL_WALL                                              11
C -                                                                       11
      REAL PMDG_BOUNDARY                                                  11
      EQUIVALENCE (BANK_PMDG(18),PMDG_BOUNDARY,RE_PMDG_BOUNDARY),         11
     *(NAMES_PMDG(15),NPMDG_BOUNDARY)                                     11
      EQUIVALENCE (COMMEN_PMDG(15),CPMDG_BOUNDARY),(MAP_PMDG(1,15),       11
     *MPMDG_BOUNDARY)                                                     11
      CHARACTER CPMDG_BOUNDARY*40/' '/                                    11
      CHARACTER NPMDG_BOUNDARY*12/'R.BOUNDARY'/                           11
      INTEGER MPMDG_BOUNDARY(2)/1,1/                                      11
      REAL RE_PMDG_BOUNDARY                                               11
C -                                                                       11
      REAL PMDG_TH_BASE                                                   11
      EQUIVALENCE (BANK_PMDG(19),PMDG_TH_BASE,RE_PMDG_TH_BASE),           11
     *(NAMES_PMDG(16),NPMDG_TH_BASE)                                      11
      EQUIVALENCE (COMMEN_PMDG(16),CPMDG_TH_BASE),(MAP_PMDG(1,16),        11
     *MPMDG_TH_BASE)                                                      11
      CHARACTER CPMDG_TH_BASE*40/' '/                                     11
      CHARACTER NPMDG_TH_BASE*12/'R.TH_BASE'/                             11
      INTEGER MPMDG_TH_BASE(2)/1,1/                                       11
      REAL RE_PMDG_TH_BASE                                                11
C -                                                                       11
      REAL PMDG_TH_AIR                                                    11
      EQUIVALENCE (BANK_PMDG(20),PMDG_TH_AIR,RE_PMDG_TH_AIR),             11
     *(NAMES_PMDG(17),NPMDG_TH_AIR)                                       11
      EQUIVALENCE (COMMEN_PMDG(17),CPMDG_TH_AIR),(MAP_PMDG(1,17),         11
     *MPMDG_TH_AIR)                                                       11
      CHARACTER CPMDG_TH_AIR*40/' '/                                      11
      CHARACTER NPMDG_TH_AIR*12/'R.TH_AIR'/                               11
      INTEGER MPMDG_TH_AIR(2)/1,1/                                        11
      REAL RE_PMDG_TH_AIR                                                 11
C -                                                                       11
      REAL PMDG_TH_PCB                                                    11
      EQUIVALENCE (BANK_PMDG(21),PMDG_TH_PCB,RE_PMDG_TH_PCB),             11
     *(NAMES_PMDG(18),NPMDG_TH_PCB)                                       11
      EQUIVALENCE (COMMEN_PMDG(18),CPMDG_TH_PCB),(MAP_PMDG(1,18),         11
     *MPMDG_TH_PCB)                                                       11
      CHARACTER CPMDG_TH_PCB*40/' '/                                      11
      CHARACTER NPMDG_TH_PCB*12/'R.TH_PCB'/                               11
      INTEGER MPMDG_TH_PCB(2)/1,1/                                        11
      REAL RE_PMDG_TH_PCB                                                 11
C -                                                                       11
      REAL PMDG_TH_LEAD                                                   11
      EQUIVALENCE (BANK_PMDG(22),PMDG_TH_LEAD,RE_PMDG_TH_LEAD),           11
     *(NAMES_PMDG(19),NPMDG_TH_LEAD)                                      11
      EQUIVALENCE (COMMEN_PMDG(19),CPMDG_TH_LEAD),(MAP_PMDG(1,19),        11
     *MPMDG_TH_LEAD)                                                      11
      CHARACTER CPMDG_TH_LEAD*40/' '/                                     11
      CHARACTER NPMDG_TH_LEAD*12/'R.TH_LEAD'/                             11
      INTEGER MPMDG_TH_LEAD(2)/1,1/                                       11
      REAL RE_PMDG_TH_LEAD                                                11
C -                                                                       11
      REAL PMDG_TH_STEEL                                                  11
      EQUIVALENCE (BANK_PMDG(23),PMDG_TH_STEEL,RE_PMDG_TH_STEEL),         11
     *(NAMES_PMDG(20),NPMDG_TH_STEEL)                                     11
      EQUIVALENCE (COMMEN_PMDG(20),CPMDG_TH_STEEL),(MAP_PMDG(1,20),       11
     *MPMDG_TH_STEEL)                                                     11
      CHARACTER CPMDG_TH_STEEL*40/' '/                                    11
      CHARACTER NPMDG_TH_STEEL*12/'R.TH_STEEL'/                           11
      INTEGER MPMDG_TH_STEEL(2)/1,1/                                      11
      REAL RE_PMDG_TH_STEEL                                               11
C -                                                                       11
      INTEGER PMDG_NX(5)                                                  11
      EQUIVALENCE (BANK_PMDG(24),PMDG_NX,RE_PMDG_NX),(NAMES_PMDG(21),     11
     *NPMDG_NX)                                                           11
      EQUIVALENCE (COMMEN_PMDG(21),CPMDG_NX),(MAP_PMDG(1,21),MPMDG_NX)    11
      CHARACTER CPMDG_NX*40/' '/                                          11
      CHARACTER NPMDG_NX*12/'I.NX'/                                       11
      INTEGER MPMDG_NX(2)/5,1/                                            11
      REAL RE_PMDG_NX                                                     11
C -                                                                       11
      INTEGER PMDG_NY(5)                                                  11
      EQUIVALENCE (BANK_PMDG(29),PMDG_NY,RE_PMDG_NY),(NAMES_PMDG(22),     11
     *NPMDG_NY)                                                           11
      EQUIVALENCE (COMMEN_PMDG(22),CPMDG_NY),(MAP_PMDG(1,22),MPMDG_NY)    11
      CHARACTER CPMDG_NY*40/' '/                                          11
      CHARACTER NPMDG_NY*12/'I.NY'/                                       11
      INTEGER MPMDG_NY(2)/5,1/                                            11
      REAL RE_PMDG_NY                                                     11
C -                                                                       11
      INTEGER PMDG_MX(7)                                                  11
      EQUIVALENCE (BANK_PMDG(34),PMDG_MX,RE_PMDG_MX),(NAMES_PMDG(23),     11
     *NPMDG_MX)                                                           11
      EQUIVALENCE (COMMEN_PMDG(23),CPMDG_MX),(MAP_PMDG(1,23),MPMDG_MX)    11
      CHARACTER CPMDG_MX*40/' '/                                          11
      CHARACTER NPMDG_MX*12/'I.MX'/                                       11
      INTEGER MPMDG_MX(2)/7,1/                                            11
      REAL RE_PMDG_MX                                                     11
C -                                                                       11
      INTEGER PMDG_MY(7)                                                  11
      EQUIVALENCE (BANK_PMDG(41),PMDG_MY,RE_PMDG_MY),(NAMES_PMDG(24),     11
     *NPMDG_MY)                                                           11
      EQUIVALENCE (COMMEN_PMDG(24),CPMDG_MY),(MAP_PMDG(1,24),MPMDG_MY)    11
      CHARACTER CPMDG_MY*40/' '/                                          11
      CHARACTER NPMDG_MY*12/'I.MY'/                                       11
      INTEGER MPMDG_MY(2)/7,1/                                            11
      REAL RE_PMDG_MY                                                     11
C -                                                                       11
      REAL PMDG_HEXD2(10)                                                 11
      EQUIVALENCE (BANK_PMDG(48),PMDG_HEXD2,RE_PMDG_HEXD2),               11
     *(NAMES_PMDG(25),NPMDG_HEXD2)                                        11
      EQUIVALENCE (COMMEN_PMDG(25),CPMDG_HEXD2),(MAP_PMDG(1,25),          11
     *MPMDG_HEXD2)                                                        11
      CHARACTER CPMDG_HEXD2*40/' '/                                       11
      CHARACTER NPMDG_HEXD2*12/'R.HEXD2'/                                 11
      INTEGER MPMDG_HEXD2(2)/10,1/                                        11
      REAL RE_PMDG_HEXD2                                                  11
C -                                                                       11
      REAL PMDG_HEXD1(10)                                                 11
      EQUIVALENCE (BANK_PMDG(58),PMDG_HEXD1,RE_PMDG_HEXD1),               11
     *(NAMES_PMDG(26),NPMDG_HEXD1)                                        11
      EQUIVALENCE (COMMEN_PMDG(26),CPMDG_HEXD1),(MAP_PMDG(1,26),          11
     *MPMDG_HEXD1)                                                        11
      CHARACTER CPMDG_HEXD1*40/' '/                                       11
      CHARACTER NPMDG_HEXD1*12/'R.HEXD1'/                                 11
      INTEGER MPMDG_HEXD1(2)/10,1/                                        11
      REAL RE_PMDG_HEXD1                                                  11
C -                                                                       11
      REAL PMDG_DPARA(6)                                                  11
      EQUIVALENCE (BANK_PMDG(68),PMDG_DPARA,RE_PMDG_DPARA),               11
     *(NAMES_PMDG(27),NPMDG_DPARA)                                        11
      EQUIVALENCE (COMMEN_PMDG(27),CPMDG_DPARA),(MAP_PMDG(1,27),          11
     *MPMDG_DPARA)                                                        11
      CHARACTER CPMDG_DPARA*40/' '/                                       11
      CHARACTER NPMDG_DPARA*12/'R.DPARA'/                                 11
      INTEGER MPMDG_DPARA(2)/6,1/                                         11
      REAL RE_PMDG_DPARA                                                  11
C -                                                                       11
      INTEGER LEN_PMDG,LLEN_PMDG,LINK_PMDG                                11
      PARAMETER (LEN_PMDG=73)                                             11
      PARAMETER (LLEN_PMDG=27)                                            11
      INTEGER FLAG_PMDG/0/,MAP_PMDG(2,LLEN_PMDG),IDNUM_PMDG(10)           11
      CHARACTER NAMES_PMDG(LLEN_PMDG)*12,COMMEN_PMDG(LLEN_PMDG)*40        11
      DATA IDNUM_PMDG/10*0/                                               11
      REAL BANK_PMDG(LEN_PMDG)                                            11
      CHARACTER*60 BPATH_PMDG/' '/                                        11
      SAVE FLAG_PMDG,MAP_PMDG,IDNUM_PMDG,NAMES_PMDG,COMMEN_PMDG,          11
     *BPATH_PMDG                                                          11
      DATA LINK_PMDG/0/,BANK_PMDG/LEN_PMDG*0/                             11
C *                                                                       11
      INTEGER J,ITYPE,NCELLX,NCELLY,N,MCELLX,IPMS                         12
      REAL XB,YB,ZB,XLEN,XLEN0,YLEN,YLEN0,PHI,PHIDEG,XPOS,YPOS,XSIZE,     14
     *YSIZE                                                               14
      REAL SM_THICK,ZZ,ROOT32,ROOT34,XLEN1,XLEN2,XSIZE1,XLEN3,YLEN3       16
      REAL PHIDEG1,PHIDEG2,PHIDEG3,PHI1,PHI2,PHI3,XPOS1,YPOS1,YLEN1,      18
     *YLEN2                                                               18
      REAL ZLEN,ZLEN0,ZLEN1,SM_THICK_A,XX,ZLEN2                           20
C * LAMBDA FUNCTION SIZEN                                                 22
      REAL SIZEN                                                          23
      SIZEN(N) = ((N + 1./3.)*PMDG_CELL_RADIUS)*2 + PMDG_BOUNDARY*2.*2./  24
     *SQRT(3.)                                                            24
C *                                                                       25
      CALL AGSBEGM(MODULE,IPRIN)                                          25
      IF (IPRIN.LT.0) RETURN                                              25
C Filling bank PMVR - PMD GEOMETRY                                        25
      BANK_TITLE='PMD GEOMETRY'                                           25
      BPATH_PMVR='PMVR*'                                                  25
      IDNUM_PMVR(1)=0                                                     26
C GEOMETRY VERSION                                                        26
      PMVR_VERSION=1                                                      26
      CPMVR_VERSION='GEOMETRY VERSION'                                    26
C GENERAL CONFIGURATION OF THE DETECTOR                                   27
      PMVR_CONFIG=1                                                       27
      CPMVR_CONFIG='GENERAL CONFIGURATION OF THE DETECTOR'                27
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PMVR',    28
     *BPATH_PMVR,IDNUM_PMVR,1, MAP_PMVR,NAMES_PMVR,COMMEN_PMVR,           28
     *BANK_PMVR,LEN_PMVR,LLEN_PMVR,LINK_PMVR,FLAG_PMVR)                   28
C *                                                                       28
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PMVR',    28
     *BPATH_PMVR,IDNUM_PMVR,1, MAP_PMVR,NAMES_PMVR,COMMEN_PMVR,           28
     *BANK_PMVR,LEN_PMVR,LLEN_PMVR,LINK_PMVR,FLAG_PMVR)                   28
C *                                                                       29
      CALL AGSBEGM(MODULE,IPRIN)                                          29
      IF (IPRIN.LT.0) RETURN                                              29
C Filling bank PMDG - PMD GEOMETRY                                        29
      BANK_TITLE='PMD GEOMETRY'                                           29
      BPATH_PMDG='PMDG*'                                                  29
      IDNUM_PMDG(1)=0                                                     30
C GEOMETRY VERSION                                                        30
      PMDG_VERSION=1                                                      30
      CPMDG_VERSION='GEOMETRY VERSION'                                    30
C MOTHER VOLUME MAX RADIUS                                                31
      PMDG_M_MAX=135.0                                                    31
      CPMDG_M_MAX='MOTHER VOLUME MAX RADIUS'                              31
C MOTHER VOLUME MIN RADIUS                                                32
      PMDG_M_MIN=22.0                                                     32
      CPMDG_M_MIN='MOTHER VOLUME MIN RADIUS'                              32
C PMD PLACED AT 5.39 METRE FROM THE INTERACTION POINT                     33
      PMDG_ZDIST(1)=-539.                                                 33
      PMDG_ZDIST(2)=-550.                                                 33
      CPMDG_ZDIST='PMD PLACED AT 5.39 METRE FROM THE INTERACTION POINT'   33
C (X-HALFWIDTH OF THE PMD BOX,WAS 190 EARLIER)                            34
      PMDG_DPMDX=270                                                      34
      CPMDG_DPMDX='(X-HALFWIDTH OF THE PMD BOX,WAS 190 EARLIER)'          34
C Y-HALFWIDTH OF THE PMD BOX.                                             35
      PMDG_DPMDY=270                                                      35
      CPMDG_DPMDY='Y-HALFWIDTH OF THE PMD BOX.'                           35
C TOTAL Z HALF-WIDTH OF THE BOX.                                          36
      PMDG_DPMDZ=10.                                                      36
      CPMDG_DPMDZ='TOTAL Z HALF-WIDTH OF THE BOX.'                        36
C HALF THICKNESS OF GAS (CPV SENSITIVE)                                   37
      PMDG_PARGCZ=0.40                                                    37
      CPMDG_PARGCZ='HALF THICKNESS OF GAS (CPV SENSITIVE)'                37
C (8 MM THICK GAS)                                                        38
      PMDG_PARSCZ=0.40                                                    38
      CPMDG_PARSCZ='(8 MM THICK GAS)'                                     38
C (IRON FRAME)                                                            39
      PMDG_PARFEZ=0.25                                                    39
      CPMDG_PARFEZ='(IRON FRAME)'                                         39
C (3 X0 OF LEAD CONVERTER)                                                40
      PMDG_PARPBZ=0.75                                                    40
      CPMDG_PARPBZ='(3 X0 OF LEAD CONVERTER)'                             40
C X-DIMENSIONS OF MODULES                                                 41
      PMDG_NX(1)=48                                                       41
      PMDG_NX(2)=72                                                       41
      PMDG_NX(3)=72                                                       41
      PMDG_NX(4)=48                                                       41
      PMDG_NX(5)=48                                                       41
      CPMDG_NX='X-DIMENSIONS OF MODULES'                                  41
C Y-DIMENSIONS OF MODULES                                                 42
      PMDG_NY(1)=24                                                       42
      PMDG_NY(2)=48                                                       42
      PMDG_NY(3)=48                                                       42
      PMDG_NY(4)=72                                                       42
      PMDG_NY(5)=48                                                       42
      CPMDG_NY='Y-DIMENSIONS OF MODULES'                                  42
C X-DIMENSIONS OF MODULES                                                 43
      PMDG_MX(1)=24                                                       43
      PMDG_MX(2)=48                                                       43
      PMDG_MX(3)=72                                                       43
      PMDG_MX(4)=72                                                       43
      PMDG_MX(5)=24                                                       43
      PMDG_MX(6)=48                                                       43
      PMDG_MX(7)=48                                                       43
      CPMDG_MX='X-DIMENSIONS OF MODULES'                                  43
C Y-DIMENSIONS OF MODULES                                                 44
      PMDG_MY(1)=24                                                       44
      PMDG_MY(2)=24                                                       44
      PMDG_MY(3)=24                                                       44
      PMDG_MY(4)=48                                                       44
      PMDG_MY(5)=48                                                       44
      PMDG_MY(6)=72                                                       44
      PMDG_MY(7)=48                                                       44
      CPMDG_MY='Y-DIMENSIONS OF MODULES'                                  44
C INNER HEX                                                               46
      PMDG_HEXD2(1)=0.                                                    46
      PMDG_HEXD2(2)=360.                                                  46
      PMDG_HEXD2(3)=6                                                     46
      PMDG_HEXD2(4)=2                                                     46
      PMDG_HEXD2(5)=-0.4                                                  46
      PMDG_HEXD2(6)=0.                                                    46
      PMDG_HEXD2(7)=0.51                                                  46
      PMDG_HEXD2(8)=0.4                                                   46
      PMDG_HEXD2(9)=0.                                                    46
      PMDG_HEXD2(10)=0.51                                                 46
      CPMDG_HEXD2='INNER HEX'                                             46
C OUTER HEX                                                               48
      PMDG_HEXD1(1)=0.                                                    48
      PMDG_HEXD1(2)=360.                                                  48
      PMDG_HEXD1(3)=6                                                     48
      PMDG_HEXD1(4)=2                                                     48
      PMDG_HEXD1(5)=-0.4                                                  48
      PMDG_HEXD1(6)=0.                                                    48
      PMDG_HEXD1(7)=0.53                                                  48
      PMDG_HEXD1(8)=0.4                                                   48
      PMDG_HEXD1(9)=0.                                                    48
      PMDG_HEXD1(10)=0.53                                                 48
      CPMDG_HEXD1='OUTER HEX'                                             48
C SUPERMODULE                                                             49
      PMDG_DPARA(1)=38.29                                                 49
      PMDG_DPARA(2)=33.16                                                 49
      PMDG_DPARA(3)=0.4                                                   49
      PMDG_DPARA(4)=30.                                                   49
      PMDG_DPARA(5)=0.                                                    49
      PMDG_DPARA(6)=0.                                                    49
      CPMDG_DPARA='SUPERMODULE'                                           49
C RADIUS OF A CELL                                                        50
      PMDG_CELL_RADIUS=0.5282                                             50
      CPMDG_CELL_RADIUS='RADIUS OF A CELL'                                50
C GAS DEPTH                                                               51
      PMDG_CELL_DEPTH=0.8                                                 51
      CPMDG_CELL_DEPTH='GAS DEPTH'                                        51
C CELL WALL THICKNESS                                                     52
      PMDG_CELL_WALL=0.2                                                  52
      CPMDG_CELL_WALL='CELL WALL THICKNESS'                               52
C BOUNDARY                                                                53
      PMDG_BOUNDARY=0.8                                                   53
      CPMDG_BOUNDARY='BOUNDARY'                                           53
C THICKNESS OF THE BASE PLATE                                             54
      PMDG_TH_BASE=0.3                                                    54
      CPMDG_TH_BASE='THICKNESS OF THE BASE PLATE'                         54
C AIR GAP                                                                 55
      PMDG_TH_AIR=0.1                                                     55
      CPMDG_TH_AIR='AIR GAP'                                              55
C THICKNESS OF THE PCB                                                    56
      PMDG_TH_PCB=0.16                                                    56
      CPMDG_TH_PCB='THICKNESS OF THE PCB'                                 56
C THICKNESS OF THE LEAD                                                   57
      PMDG_TH_LEAD=1.5                                                    57
      CPMDG_TH_LEAD='THICKNESS OF THE LEAD'                               57
C THICKNESS OF THE STEEL SUPPORT                                          58
      PMDG_TH_STEEL=0.5                                                   58
      CPMDG_TH_STEEL='THICKNESS OF THE STEEL SUPPORT'                     58
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PMDG',    59
     *BPATH_PMDG,IDNUM_PMDG,1, MAP_PMDG,NAMES_PMDG,COMMEN_PMDG,           59
     *BANK_PMDG,LEN_PMDG,LLEN_PMDG,LINK_PMDG,FLAG_PMDG)                   59
C *                                                                       59
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PMDG',    59
     *BPATH_PMDG,IDNUM_PMDG,1, MAP_PMDG,NAMES_PMDG,COMMEN_PMDG,           59
     *BANK_PMDG,LEN_PMDG,LLEN_PMDG,LINK_PMDG,FLAG_PMDG)                   59
C use default PMVR bank                                                   60
      BPATH_PMVR='PMVR*'                                                  60
      IDNUM_PMVR(1)=0                                                     60
      CALL AGDGETP('PHMDGEO','*',-999,'PMVR',' ',BANK_PMVR, BPATH_PMVR,   60
     *IDNUM_PMVR,1,MAP_PMVR,NAMES_PMVR,BANK_PMVR,LEN_PMVR,LLEN_PMVR,      60
     *LINK_PMVR,FLAG_PMVR)                                                60
C use default PMDG bank                                                   61
      BPATH_PMDG='PMDG*'                                                  61
      IDNUM_PMDG(1)=0                                                     61
      CALL AGDGETP('PHMDGEO','*',-999,'PMDG',' ',BANK_PMDG, BPATH_PMDG,   61
     *IDNUM_PMDG,1,MAP_PMDG,NAMES_PMDG,BANK_PMDG,LEN_PMDG,LLEN_PMDG,      61
     *LINK_PMDG,FLAG_PMDG)                                                61
      ROOT32=SQRT(3.0)/2.0                                                62
      ROOT34=ROOT32/2.0                                                   63
      XLEN3=(SIZEN(72)+SIZEN(48))/4.0                                     64
      YLEN3=-(SIZEN(72)+SIZEN(48))*ROOT34                                 65
      ZLEN2=(SIZEN(72)+SIZEN(48)+4.5*PMDG_TH_AIR)/4.                      66
      ZLEN1=-(SIZEN(72)+SIZEN(48)+(2.0*PMDG_BOUNDARY+3.75*PMDG_TH_AIR)/   67
     *ROOT32)*ROOT34                                                      67
      SM_THICK_A = (PMDG_TH_BASE + 4.0*PMDG_TH_AIR + 3.0*PMDG_TH_PCB +    68
     *PMDG_CELL_DEPTH)                                                    68
      SM_THICK = 2.0*SM_THICK_A + PMDG_TH_LEAD + PMDG_TH_STEEL            69
      CALL AGSBEGM(MODULE,IPRIN)                                          70
C Check IPRIN<0                                                           70
      IF (IPRIN.LT.0) RETURN                                              70
C Create PHMD :                                                           70
C Call To Local Procedure PHMD                                            70
      ASSIGN 5020 TO BLOCK_PHMD                                           70
      GO TO  5010                                                         70
5020  CONTINUE                                                            70
C *                                                                       70
C Geant POSITION : PHMD ===> MOTHER='CAVE' Z=PMDG_ZDIST(PMDG_VERSION) <=  71
C ==                                                                      71
      CALL AGSRESET                                                       71
      AG_MOTHER = 'CAVE'                                                  71
      AG_Z = PMDG_ZDIST(PMDG_VERSION)                                     71
      AG_TITLE = 'PHMD'                                                   71
      AG_EXNAME = 'POSITION'                                              71
      AG_PARLIST = 'MOTHER_Z'                                             71
      CALL AXPOSITION                                                     71
C *                                                                       71
C ----------------------------------------------------------------------  71
C ------------ PHMD                                                       71
      CALL AGSENDM                                                        73
      RETURN                                                              73
C Begin Local Procedure PHMD                                              73
C PHMD THE PMD BOX VOLUME AND FILL WITH AIR                               73
5010  CONTINUE                                                            73
         AG_TITLE='PHMD'                                                  73
         AG_MODULE=MODULE                                                 73
         AG_MARK='V00'                                                    73
         CALL AGSPUSH                                                     73
         AG_MODULE=MODULE                                                 73
C    *                                                                    73
C    Geant MATERIAL : AIR ===> inherited arguments used <===              74
         CALL AGSRESET                                                    74
         AG_TITLE = 'AIR'                                                 74
         AG_EXNAME = 'MATERIAL'                                           74
         AG_PARLIST = 'NONE'                                              74
         CALL AXMATERIAL                                                  74
C    *                                                                    74
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           74
         CALL AGSRESET                                                    75
         AG_TITLE = 'STANDARD'                                            75
         AG_EXNAME = 'MEDIUM'                                             75
         AG_PARLIST = 'NONE'                                              75
         CALL AXMEDIUM                                                    75
C    *                                                                    75
C    Geant ATTRIBUTE : PHMD ===> SEEN=1 COLO=2 <===                       75
         CALL AGSRESET                                                    76
         AG_SEEN = 1                                                      76
         AG_COLO = 2                                                      76
         AG_TITLE = 'PHMD'                                                76
         AG_EXNAME = 'ATTRIBUTE'                                          76
         AG_PARLIST = 'SEEN_COLO'                                         76
         CALL AXATTRIBUTE                                                 76
C    *                                                                    76
C    Geant SHAPE : TUBE ===> RMIN=PMDG_M_MIN RMAX=PMDG_M_MAX, DZ=SM_THIC  76
C    K/2. <===                                                            76
         CALL AGSRESET                                                    78
         AG_RMIN = PMDG_M_MIN                                             78
         AG_RMAX = PMDG_M_MAX                                             78
         AG_DZ = SM_THICK/2.                                              78
         AG_TITLE = 'TUBE'                                                78
         AG_EXNAME = 'SHAPE'                                              78
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      78
         CALL AXSHAPE                                                     78
C    *                                                                    78
         PHIDEG1=90.                                                      78
         PHIDEG2=210.                                                     80
         PHIDEG3=330.                                                     81
         PHI1=PHIDEG1*DEGRAD                                              82
         PHI2=PHIDEG2*DEGRAD                                              83
         PHI3=PHIDEG3*DEGRAD                                              84
         IPMS = 1                                                         85
C    Create PHMS :                                                        86
C    Call To Local Procedure PHMS                                         86
         ASSIGN 5040 TO BLOCK_PHMS                                        86
         GO TO  5030                                                      86
5040     CONTINUE                                                         86
C    *                                                                    86
C    Geant POSITION : PHMS ===> X=XLEN3*COS(PHI1)-YLEN3*SIN(PHI1)+5*PMDG  88
C    _TH_AIR Y=XLEN3*SIN(PHI1)+YLEN3*COS(PHI1) Z=0 ALPHAZ=PHIDEG1 NCOPY=  88
C    1 <===                                                               88
         CALL AGSRESET                                                    88
         AG_X = XLEN3*COS(PHI1)-YLEN3*SIN(PHI1)+5*PMDG_TH_AIR             88
         AG_Y = XLEN3*SIN(PHI1)+YLEN3*COS(PHI1)                           88
         AG_Z = 0                                                         88
         AG_ALPHAN=AG_ALPHAN+1                                            88
         AG_ALPHAK(AG_ALPHAN)=3                                           88
         AG_ALPHAL(AG_ALPHAN)=PHIDEG1                                     88
         AG_NCOPY = 1                                                     88
         AG_TITLE = 'PHMS'                                                88
         AG_EXNAME = 'POSITION'                                           88
         AG_PARLIST = 'X_Y_Z_ALPHAZ_NCOPY'                                88
         CALL AXPOSITION                                                  88
C    *                                                                    88
         IPMS = 2                                                         88
C    Create PHMS :                                                        90
C    Call To Local Procedure PHMS                                         90
         ASSIGN 5050 TO BLOCK_PHMS                                        90
         GO TO  5030                                                      90
5050     CONTINUE                                                         90
C    *                                                                    90
C    Geant POSITION : PHMS ===> X=XLEN3*COS(PHI2)-YLEN3*SIN(PHI2)-5*PMDG  92
C    _TH_AIR Y=XLEN3*SIN(PHI2)+YLEN3*COS(PHI2) Z=0 ALPHAZ=PHIDEG2 NCOPY=  92
C    2 <===                                                               92
         CALL AGSRESET                                                    92
         AG_X = XLEN3*COS(PHI2)-YLEN3*SIN(PHI2)-5*PMDG_TH_AIR             92
         AG_Y = XLEN3*SIN(PHI2)+YLEN3*COS(PHI2)                           92
         AG_Z = 0                                                         92
         AG_ALPHAN=AG_ALPHAN+1                                            92
         AG_ALPHAK(AG_ALPHAN)=3                                           92
         AG_ALPHAL(AG_ALPHAN)=PHIDEG2                                     92
         AG_NCOPY = 2                                                     92
         AG_TITLE = 'PHMS'                                                92
         AG_EXNAME = 'POSITION'                                           92
         AG_PARLIST = 'X_Y_Z_ALPHAZ_NCOPY'                                92
         CALL AXPOSITION                                                  92
C    *                                                                    92
         IPMS = 3                                                         92
C    Create PHMS :                                                        94
C    Call To Local Procedure PHMS                                         94
         ASSIGN 5060 TO BLOCK_PHMS                                        94
         GO TO  5030                                                      94
5060     CONTINUE                                                         94
C    *                                                                    94
C    Geant POSITION : PHMS ===> X=ZLEN2*COS(PHI3)-ZLEN1*SIN(PHI3) Y=ZLEN  96
C    2*SIN(PHI3)+ZLEN1*COS(PHI3) Z=0 ALPHAZ=PHIDEG3 NCOPY=3 <===          96
         CALL AGSRESET                                                    96
         AG_X = ZLEN2*COS(PHI3)-ZLEN1*SIN(PHI3)                           96
         AG_Y = ZLEN2*SIN(PHI3)+ZLEN1*COS(PHI3)                           96
         AG_Z = 0                                                         96
         AG_ALPHAN=AG_ALPHAN+1                                            96
         AG_ALPHAK(AG_ALPHAN)=3                                           96
         AG_ALPHAL(AG_ALPHAN)=PHIDEG3                                     96
         AG_NCOPY = 3                                                     96
         AG_TITLE = 'PHMS'                                                96
         AG_EXNAME = 'POSITION'                                           96
         AG_PARLIST = 'X_Y_Z_ALPHAZ_NCOPY'                                96
         CALL AXPOSITION                                                  96
C    *                                                                    96
5011  CALL AGSPOPB                                                        97
      GO TO BLOCK_PHMD, (5010 ,5020)                                      97
C End of Local Procedure                                                  97
C ----------------------------------------------------------------------  98
C ------------ PHMS                                                       98
C Begin Local Procedure PHMS                                              99
C PHMS THE PMD SECTOR VOLUME - 1/3RD OF PHMD                              99
5030  CONTINUE                                                            99
         AG_TITLE='PHMS'                                                  99
         AG_MODULE=MODULE                                                 99
         AG_MARK='V00'                                                    99
         CALL AGSPUSH                                                     99
         AG_MODULE=MODULE                                                 99
C    *                                                                    99
C    Geant MATERIAL : AIR ===> inherited arguments used <===              100
         CALL AGSRESET                                                    100
         AG_TITLE = 'AIR'                                                 100
         AG_EXNAME = 'MATERIAL'                                           100
         AG_PARLIST = 'NONE'                                              100
         CALL AXMATERIAL                                                  100
C    *                                                                    100
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           100
         CALL AGSRESET                                                    101
         AG_TITLE = 'STANDARD'                                            101
         AG_EXNAME = 'MEDIUM'                                             101
         AG_PARLIST = 'NONE'                                              101
         CALL AXMEDIUM                                                    101
C    *                                                                    101
C    Geant ATTRIBUTE : PHMS ===> SEEN=1 COLO=2 <===                       101
         CALL AGSRESET                                                    102
         AG_SEEN = 1                                                      102
         AG_COLO = 2                                                      102
         AG_TITLE = 'PHMS'                                                102
         AG_EXNAME = 'ATTRIBUTE'                                          102
         AG_PARLIST = 'SEEN_COLO'                                         102
         CALL AXATTRIBUTE                                                 102
C    *                                                                    102
C    Check IPMS!=3                                                        102
         IF (IPMS.NE.3) THEN                                              103
C    Geant SHAPE : PARA ===> DX=XLEN3*2. DY=-YLEN3 DZ=SM_THICK/2., ALPH=  105
C    30. THET=0 PHI=0 <===                                                105
         CALL AGSRESET                                                    105
         AG_DX = XLEN3*2.                                                 105
         AG_DY = -YLEN3                                                   105
         AG_DZ = SM_THICK/2.                                              105
         AG_ALPH = 30.                                                    105
         AG_THET = 0                                                      105
         AG_PHI = 0                                                       105
         AG_TITLE = 'PARA'                                                105
         AG_EXNAME = 'SHAPE'                                              105
         AG_PARLIST = 'DX_DY_DZ_ALPH_THET_PHI'                            105
         CALL AXSHAPE                                                     105
C    *                                                                    105
         PHIDEG =0                                                        105
         YPOS = (SIZEN(72)+SIZEN(48))*ROOT34                              107
         XLEN2 = -(SIZEN(72)+SIZEN(48))/4.                                108
         XSIZE1 = (SIZEN(72)+SIZEN(48))/2.                                109
C    *                                                                    110
         ITYPE=1                                                          110
            GO TO 5073                                                    110
5071        ITYPE=ITYPE+1                                                 110
5073        IF(ITYPE-(5).GT.0) GO TO 5072                                 110
            XSIZE = SIZEN(PMDG_NX(ITYPE))                                 111
            YSIZE = SIZEN(PMDG_NY(ITYPE))-PMDG_BOUNDARY/ROOT32            112
C       Check (ITYPE>2)                                                   113
            IF ((ITYPE.GT.2)) THEN                                        113
            YSIZE= SIZEN(PMDG_NY(ITYPE))                                  114
            ENDIF                                                         115
            XLEN0 = XSIZE/2                                               116
            YLEN0 = (YSIZE)*ROOT34                                        117
            YLEN = (YSIZE-PMDG_BOUNDARY/ROOT32)*ROOT34                    118
            XPOS = XLEN2 -XSIZE1 +XSIZE/2.+ SIZEN(PMDG_NY(ITYPE))/4.      119
C       Check (ITYPE>2)                                                   120
            IF ((ITYPE.GT.2)) THEN                                        120
            YLEN = (YSIZE-PMDG_BOUNDARY/ROOT34)*ROOT34                    121
            ENDIF                                                         122
            YLEN1=0.                                                      123
            YLEN2=0.                                                      124
            XLEN1=1.                                                      125
C       Check (ITYPE==2)                                                  126
            IF ((ITYPE.EQ.2)) THEN                                        126
            XLEN1=-1.                                                     127
            ENDIF                                                         128
C       Check (ITYPE>2)                                                   129
            IF ((ITYPE.GT.2)) THEN                                        129
            XLEN1=0.                                                      130
            ENDIF                                                         131
            ZLEN=1.                                                       132
C       Check (ITYPE==2)                                                  133
            IF ((ITYPE.EQ.2)) THEN                                        133
            ZLEN=-1.                                                      134
            ENDIF                                                         135
C       Check (ITYPE>2)                                                   136
            IF ((ITYPE.GT.2)) THEN                                        136
            ZLEN=0.                                                       137
            ENDIF                                                         138
            ZB = 0                                                        139
            NCELLX = PMDG_NX(ITYPE)                                       140
            NCELLY = PMDG_NY(ITYPE)                                       141
C       Check (ITYPE==1)                                                  142
            IF ((ITYPE.EQ.1)) THEN                                        142
            XPOS=XPOS + XSIZE + SIZEN(PMDG_NY(ITYPE))-                    143
     *      11.*PMDG_BOUNDARY/(4.*ROOT32)-PMDG_CELL_RADIUS*2.             143
            ENDIF                                                         144
C       Check (ITYPE==2)                                                  145
            IF ((ITYPE.EQ.2)) THEN                                        145
            XPOS=XPOS + SIZEN(PMDG_NY(ITYPE))/2.-PMDG_BOUNDARY/           146
     *      (4.*ROOT32)                                                   146
            ENDIF                                                         147
C       Check (ITYPE==3)                                                  148
            IF ((ITYPE.EQ.3)) THEN                                        148
            XPOS1=XPOS1 + SIZEN(PMDG_MX(ITYPE-1))/2 -                     149
     *      PMDG_BOUNDARY-2.25*PMDG_TH_AIR                                149
            ENDIF                                                         150
C       Check (ITYPE==4)                                                  151
            IF ((ITYPE.EQ.4)) THEN                                        151
            XPOS=XPOS + 2.*XSIZE1-XSIZE/2.                                152
            ENDIF                                                         153
C       Check (ITYPE==5)                                                  154
            IF ((ITYPE.EQ.5)) THEN                                        154
            XPOS1 = XPOS1+2.*XSIZE1-XSIZE+ SIZEN(PMDG_MX(ITYPE-1))/       155
     *      2.-YSIZE +3*PMDG_BOUNDARY*ROOT32                              155
            ENDIF                                                         156
            YPOS = YPOS-YLEN0                                             157
C       Create PHSR :                                                     158
C       Call To Local Procedure PHSR                                      158
            ASSIGN 5090 TO BLOCK_PHSR                                     158
            GO TO  5080                                                   158
5090        CONTINUE                                                      158
C       *                                                                 158
C       Geant POSITION : PHSR ===> X=XPOS Y=YPOS Z=0. ALPHAZ=PHIDEG <===  159
            CALL AGSRESET                                                 159
            AG_X = XPOS                                                   159
            AG_Y = YPOS                                                   159
            AG_Z = 0.                                                     159
            AG_ALPHAN=AG_ALPHAN+1                                         159
            AG_ALPHAK(AG_ALPHAN)=3                                        159
            AG_ALPHAL(AG_ALPHAN)=PHIDEG                                   159
            AG_TITLE = 'PHSR'                                             159
            AG_EXNAME = 'POSITION'                                        159
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   159
            CALL AXPOSITION                                               159
C       *                                                                 159
            YPOS = YPOS-YLEN0                                             159
C       Check (ITYPE==3)                                                  161
            IF ((ITYPE.EQ.3)) THEN                                        161
            YPOS=(SIZEN(72)+SIZEN(48))*ROOT34                             162
            ENDIF                                                         163
         GO TO 5071                                                       164
5072     CONTINUE                                                         164
         ELSE                                                             165
C    Geant SHAPE : PARA ===> DX=ZLEN2*2. DY=-ZLEN1 DZ=SM_THICK/2., ALPH=  167
C    30. THET=0 PHI=0 <===                                                167
         CALL AGSRESET                                                    167
         AG_DX = ZLEN2*2.                                                 167
         AG_DY = -ZLEN1                                                   167
         AG_DZ = SM_THICK/2.                                              167
         AG_ALPH = 30.                                                    167
         AG_THET = 0                                                      167
         AG_PHI = 0                                                       167
         AG_TITLE = 'PARA'                                                167
         AG_EXNAME = 'SHAPE'                                              167
         AG_PARLIST = 'DX_DY_DZ_ALPH_THET_PHI'                            167
         CALL AXSHAPE                                                     167
C    *                                                                    167
         PHIDEG =0                                                        167
         YPOS1 = (SIZEN(72)+SIZEN(48)+2.*PMDG_BOUNDARY/                   169
     *   ROOT32+.375)*ROOT34                                              169
         XLEN2 = -(SIZEN(72)+SIZEN(48)+3.75*PMDG_TH_AIR)/4.               170
         XSIZE1 = (SIZEN(72)+SIZEN(48)+4.5*PMDG_TH_AIR)/2.                171
C    *                                                                    172
         ITYPE=1                                                          172
            GO TO 5103                                                    172
5101        ITYPE=ITYPE+1                                                 172
5103        IF(ITYPE-(7).GT.0) GO TO 5102                                 172
            XSIZE = SIZEN(PMDG_MX(ITYPE))                                 173
            YSIZE = SIZEN(PMDG_MY(ITYPE))                                 174
            XLEN0 = XSIZE/2                                               175
            YLEN0 = (YSIZE)*ROOT34                                        176
            ZLEN0 = (YSIZE)*ROOT34                                        177
C       Check (ITYPE==2)                                                  178
            IF ((ITYPE.EQ.2)) THEN                                        178
            ZLEN0=(YSIZE-PMDG_BOUNDARY/ROOT32)*ROOT34+3.75*PMDG_TH_AIR    179
            ENDIF                                                         180
C       Check (ITYPE==3)                                                  181
            IF ((ITYPE.EQ.3)) THEN                                        181
            ZLEN0=(YSIZE-PMDG_BOUNDARY/ROOT32)*ROOT34-3.75*PMDG_TH_AIR    182
            ENDIF                                                         183
C       Check (ITYPE==4)                                                  184
            IF ((ITYPE.EQ.4)) THEN                                        184
            ZLEN0=(YSIZE)*ROOT34+3.75*PMDG_TH_AIR                         185
            ENDIF                                                         186
C       Check (ITYPE==5)                                                  187
            IF ((ITYPE.EQ.5)) THEN                                        187
            ZLEN0=(YSIZE+PMDG_BOUNDARY/ROOT32)*ROOT34                     188
            ENDIF                                                         189
            XPOS1 = XLEN2 -XSIZE1 + XSIZE/2.+ YSIZE/4.                    190
            YLEN = (YSIZE-2*PMDG_BOUNDARY)*ROOT34                         191
            YLEN1 = 0.                                                    192
C       Check (ITYPE==1)                                                  193
            IF ((ITYPE.EQ.1)) THEN                                        193
            YLEN1=1.                                                      194
            ENDIF                                                         195
C       Check (ITYPE==2)                                                  196
            IF ((ITYPE.EQ.2)) THEN                                        196
            YLEN1=-1.                                                     197
            ENDIF                                                         198
C       Check (ITYPE==3)                                                  199
            IF ((ITYPE.EQ.3)) THEN                                        199
            YLEN1=-1.                                                     200
            ENDIF                                                         201
C       Check (ITYPE==5)                                                  202
            IF ((ITYPE.EQ.5)) THEN                                        202
            YLEN1=2.                                                      203
            ENDIF                                                         204
C       Check (ITYPE==6)                                                  205
            IF ((ITYPE.EQ.6)) THEN                                        205
            YLEN1=1.                                                      206
            ENDIF                                                         207
            YLEN2 = 0.                                                    208
C       Check (ITYPE==2)                                                  209
            IF ((ITYPE.EQ.2)) THEN                                        209
            YLEN2=1.                                                      210
            ENDIF                                                         211
C       Check (ITYPE==3)                                                  212
            IF ((ITYPE.EQ.3)) THEN                                        212
            YLEN2=1.                                                      213
            ENDIF                                                         214
C       Check (ITYPE==5)                                                  215
            IF ((ITYPE.EQ.5)) THEN                                        215
            YLEN2=-1.                                                     216
            ENDIF                                                         217
            XLEN1=0.                                                      218
C       Check (ITYPE==1)                                                  219
            IF ((ITYPE.EQ.1)) THEN                                        219
            XLEN1=-2.                                                     220
            ENDIF                                                         221
C       Check (ITYPE==2)                                                  222
            IF ((ITYPE.EQ.2)) THEN                                        222
            XLEN1=3.                                                      223
            ENDIF                                                         224
C       Check (ITYPE==3)                                                  225
            IF ((ITYPE.EQ.3)) THEN                                        225
            XLEN1=1.                                                      226
            ENDIF                                                         227
C       Check (ITYPE==5)                                                  228
            IF ((ITYPE.EQ.5)) THEN                                        228
            XLEN1=-1.                                                     229
            ENDIF                                                         230
C       Check (ITYPE==6)                                                  231
            IF ((ITYPE.EQ.6)) THEN                                        231
            XLEN1=2.                                                      232
            ENDIF                                                         233
            ZLEN=0.                                                       234
C       Check (ITYPE==2)                                                  235
            IF ((ITYPE.EQ.2)) THEN                                        235
            ZLEN=0.75                                                     236
            ENDIF                                                         237
C       Check (ITYPE==3)                                                  238
            IF ((ITYPE.EQ.3)) THEN                                        238
            ZLEN=-0.75                                                    239
            ENDIF                                                         240
C       Check (ITYPE==5)                                                  241
            IF ((ITYPE.EQ.5)) THEN                                        241
            ZLEN=-1.                                                      242
            ENDIF                                                         243
            ZB = 0                                                        244
            NCELLX = PMDG_MX(ITYPE)                                       245
            NCELLY = PMDG_MY(ITYPE)                                       246
C       Check (ITYPE==1)                                                  247
            IF ((ITYPE.EQ.1)) THEN                                        247
            XPOS1=XPOS1 + SIZEN(PMDG_MX(ITYPE+1))/2.+2.*YSIZE-2*          248
     *      PMDG_BOUNDARY/ROOT32-PMDG_BOUNDARY/3.                         248
            ENDIF                                                         249
C       Check (ITYPE==2)                                                  250
            IF ((ITYPE.EQ.2)) THEN                                        250
            XPOS1=XPOS1 + XSIZE/2.+ YSIZE/                                251
     *      2.-PMDG_BOUNDARY*ROOT32-PMDG_BOUNDARY-2.25*PMDG_TH_AIR        251
            ENDIF                                                         252
C       Check (ITYPE==3)                                                  253
            IF ((ITYPE.EQ.3)) THEN                                        253
            XPOS1=XPOS1 + SIZEN(PMDG_MX(ITYPE-1))/2 -                     254
     *      PMDG_BOUNDARY-2.25*PMDG_TH_AIR                                254
            ENDIF                                                         255
C       Check (ITYPE==4)                                                  256
            IF ((ITYPE.EQ.4)) THEN                                        256
            XPOS1 = XPOS1 -PMDG_BOUNDARY*ROOT32                           257
            ENDIF                                                         258
C       Check (ITYPE==5)                                                  259
            IF ((ITYPE.EQ.5)) THEN                                        259
            XPOS1 = XPOS1+2.*XSIZE1-XSIZE+ SIZEN(PMDG_MX(ITYPE-1))/       260
     *      2.-YSIZE +3*PMDG_BOUNDARY*ROOT32+2.2                          260
            ENDIF                                                         261
C       Check (ITYPE==6)                                                  262
            IF ((ITYPE.EQ.6)) THEN                                        262
            XPOS1=XPOS1 + 2.*XSIZE1-XSIZE/2.+PMDG_BOUNDARY/               263
     *      ROOT32+2.25*PMDG_TH_AIR                                       263
            ENDIF                                                         264
C       Check (ITYPE==7)                                                  265
            IF ((ITYPE.EQ.7)) THEN                                        265
            XPOS1=XPOS1 + 2.*XSIZE1-XSIZE-PMDG_BOUNDARY/                  266
     *      (2.*ROOT32)+PMDG_BOUNDARY                                     266
            ENDIF                                                         267
            YPOS1 = YPOS1-ZLEN0                                           268
C       Create PHSR :                                                     269
C       Call To Local Procedure PHSR                                      269
            ASSIGN 5110 TO BLOCK_PHSR                                     269
            GO TO  5080                                                   269
5110        CONTINUE                                                      269
C       *                                                                 269
C       Geant POSITION : PHSR ===> X=XPOS1 Y=YPOS1 Z=0. ALPHAZ=PHIDEG <=  270
C       ==                                                                270
            CALL AGSRESET                                                 270
            AG_X = XPOS1                                                  270
            AG_Y = YPOS1                                                  270
            AG_Z = 0.                                                     270
            AG_ALPHAN=AG_ALPHAN+1                                         270
            AG_ALPHAK(AG_ALPHAN)=3                                        270
            AG_ALPHAL(AG_ALPHAN)=PHIDEG                                   270
            AG_TITLE = 'PHSR'                                             270
            AG_EXNAME = 'POSITION'                                        270
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   270
            CALL AXPOSITION                                               270
C       *                                                                 270
            YPOS1 = YPOS1-ZLEN0                                           270
C       Check (ITYPE==4)                                                  272
            IF ((ITYPE.EQ.4)) THEN                                        272
            YPOS1=(SIZEN(72)+SIZEN(48)+2.*PMDG_BOUNDARY/                  273
     *      ROOT32+.375)*ROOT34                                           273
            ENDIF                                                         274
C       Check (ITYPE==5)                                                  275
            IF ((ITYPE.EQ.5)) THEN                                        275
            YPOS1=(SIZEN(72)+SIZEN(48)+2.*PMDG_BOUNDARY/                  276
     *      ROOT32+.375)*ROOT34                                           276
            ENDIF                                                         277
C       Check (ITYPE==6)                                                  278
            IF ((ITYPE.EQ.6)) THEN                                        278
            YPOS1=YPOS1-4.*PMDG_TH_AIR                                    279
            ENDIF                                                         280
         GO TO 5101                                                       281
5102     CONTINUE                                                         281
         ENDIF                                                            282
5031  CALL AGSPOPB                                                        283
      GO TO BLOCK_PHMS, (5030 ,5040,5050,5060)                            283
C End of Local Procedure                                                  283
C ----------------------------------------------------------------------  284
C ------------ PHSR                                                       284
C Begin Local Procedure PHSR                                              285
C PHSR IS A DETECTOR BOX MADE IN AIR                                      285
5080  CONTINUE                                                            285
         AG_TITLE='PHSR'                                                  285
         AG_MODULE=MODULE                                                 285
         AG_MARK='V00'                                                    285
         CALL AGSPUSH                                                     285
         AG_MODULE=MODULE                                                 285
C    *                                                                    285
C    Geant MATERIAL : AIR ===> inherited arguments used <===              286
         CALL AGSRESET                                                    286
         AG_TITLE = 'AIR'                                                 286
         AG_EXNAME = 'MATERIAL'                                           286
         AG_PARLIST = 'NONE'                                              286
         CALL AXMATERIAL                                                  286
C    *                                                                    286
C    Geant ATTRIBUTE : PHSR ===> SEEN=1 COLO=6 SERIAL=0 <===              286
         CALL AGSRESET                                                    287
         AG_SEEN = 1                                                      287
         AG_COLO = 6                                                      287
         AG_SERIAL = 0                                                    287
         AG_TITLE = 'PHSR'                                                287
         AG_EXNAME = 'ATTRIBUTE'                                          287
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  287
         CALL AXATTRIBUTE                                                 287
C    *                                                                    287
C    Geant SHAPE : PARA ===> DX=XLEN0-YLEN1*PMDG_BOUNDARY/(2.*ROOT32), D  287
C    Y=YLEN0-YLEN2*PMDG_BOUNDARY*ROOT34/ROOT32, DZ=SM_THICK/2 ALPH=30 TH  287
C    ET=0 PHI=0 <===                                                      287
         CALL AGSRESET                                                    290
         AG_DX = XLEN0-YLEN1*PMDG_BOUNDARY/(2.*ROOT32)                    290
         AG_DY = YLEN0-YLEN2*PMDG_BOUNDARY*ROOT34/ROOT32                  290
         AG_DZ = SM_THICK/2                                               290
         AG_ALPH = 30                                                     290
         AG_THET = 0                                                      290
         AG_PHI = 0                                                       290
         AG_TITLE = 'PARA'                                                290
         AG_EXNAME = 'SHAPE'                                              290
         AG_PARLIST = 'DX_DY_DZ_ALPH_THET_PHI'                            290
         CALL AXSHAPE                                                     290
C    *                                                                    290
         XX = - SM_THICK/2. + SM_THICK_A/2.                               290
C    Create PMDA :                                                        292
C    Call To Local Procedure PMDA                                         292
         ASSIGN 5130 TO BLOCK_PMDA                                        292
         GO TO  5120                                                      292
5130     CONTINUE                                                         292
C    *                                                                    292
C    Geant POSITION : PMDA ===> Z=XX <===                                 293
         CALL AGSRESET                                                    293
         AG_Z = XX                                                        293
         AG_TITLE = 'PMDA'                                                293
         AG_EXNAME = 'POSITION'                                           293
         AG_PARLIST = 'Z'                                                 293
         CALL AXPOSITION                                                  293
C    *                                                                    293
         XX = - SM_THICK/2. + SM_THICK_A + PMDG_TH_LEAD/2.                293
C    Create PPBA :                                                        295
C    Call To Local Procedure PPBA                                         295
         ASSIGN 5150 TO BLOCK_PPBA                                        295
         GO TO  5140                                                      295
5150     CONTINUE                                                         295
C    *                                                                    295
C    Geant POSITION : PPBA ===> Z=XX <===                                 296
         CALL AGSRESET                                                    296
         AG_Z = XX                                                        296
         AG_TITLE = 'PPBA'                                                296
         AG_EXNAME = 'POSITION'                                           296
         AG_PARLIST = 'Z'                                                 296
         CALL AXPOSITION                                                  296
C    *                                                                    296
         XX = XX + PMDG_TH_LEAD/2. + PMDG_TH_STEEL/2.                     296
C    Create PFEA :                                                        298
C    Call To Local Procedure PFEA                                         298
         ASSIGN 5170 TO BLOCK_PFEA                                        298
         GO TO  5160                                                      298
5170     CONTINUE                                                         298
C    *                                                                    298
C    Geant POSITION : PFEA ===> Z=XX <===                                 299
         CALL AGSRESET                                                    299
         AG_Z = XX                                                        299
         AG_TITLE = 'PFEA'                                                299
         AG_EXNAME = 'POSITION'                                           299
         AG_PARLIST = 'Z'                                                 299
         CALL AXPOSITION                                                  299
C    *                                                                    299
         XX = XX + PMDG_TH_STEEL/2. + SM_THICK_A/2.                       299
C    Create PMDA :                                                        301
C    Call To Local Procedure PMDA                                         301
         ASSIGN 5180 TO BLOCK_PMDA                                        301
         GO TO  5120                                                      301
5180     CONTINUE                                                         301
C    *                                                                    301
C    Geant POSITION : PMDA ===> Z=XX THETAZ=180 <===                      302
         CALL AGSRESET                                                    302
         AG_Z = XX                                                        302
         AG_THETAZ = 180                                                  302
         AG_TITLE = 'PMDA'                                                302
         AG_EXNAME = 'POSITION'                                           302
         AG_PARLIST = 'Z_THETAZ'                                          302
         CALL AXPOSITION                                                  302
C    *                                                                    302
5081  CALL AGSPOPB                                                        303
      GO TO BLOCK_PHSR, (5080 ,5090,5110)                                 303
C End of Local Procedure                                                  303
C ----------------------------------------------------------------------  304
C ------------ PMDA                                                       304
C Begin Local Procedure PMDA                                              305
C PMDA IS A DETECTOR BOX MADE IN ALUMINIUM                                305
5120  CONTINUE                                                            305
         AG_TITLE='PMDA'                                                  305
         AG_MODULE=MODULE                                                 305
         AG_MARK='V00'                                                    305
         CALL AGSPUSH                                                     305
         AG_MODULE=MODULE                                                 305
C    *                                                                    305
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        306
         CALL AGSRESET                                                    306
         AG_TITLE = 'ALUMINIUM'                                           306
         AG_EXNAME = 'MATERIAL'                                           306
         AG_PARLIST = 'NONE'                                              306
         CALL AXMATERIAL                                                  306
C    *                                                                    306
C    Geant ATTRIBUTE : PMDA ===> SEEN=1 COLO=6 <===                       306
         CALL AGSRESET                                                    307
         AG_SEEN = 1                                                      307
         AG_COLO = 6                                                      307
         AG_TITLE = 'PMDA'                                                307
         AG_EXNAME = 'ATTRIBUTE'                                          307
         AG_PARLIST = 'SEEN_COLO'                                         307
         CALL AXATTRIBUTE                                                 307
C    *                                                                    307
C    Geant SHAPE : PARA ===> DX=XLEN0-YLEN1*PMDG_BOUNDARY/(2.*ROOT32), D  307
C    Y=YLEN0-YLEN2*PMDG_BOUNDARY*ROOT34/ROOT32, DZ=SM_THICK_A/2. ALPH=30  307
C     THET=0 PHI=0 <===                                                   307
         CALL AGSRESET                                                    310
         AG_DX = XLEN0-YLEN1*PMDG_BOUNDARY/(2.*ROOT32)                    310
         AG_DY = YLEN0-YLEN2*PMDG_BOUNDARY*ROOT34/ROOT32                  310
         AG_DZ = SM_THICK_A/2.                                            310
         AG_ALPH = 30                                                     310
         AG_THET = 0                                                      310
         AG_PHI = 0                                                       310
         AG_TITLE = 'PARA'                                                310
         AG_EXNAME = 'SHAPE'                                              310
         AG_PARLIST = 'DX_DY_DZ_ALPH_THET_PHI'                            310
         CALL AXSHAPE                                                     310
C    *                                                                    310
C    Create AIRA :                                                        311
C    Call To Local Procedure AIRA                                         311
         ASSIGN 5200 TO BLOCK_AIRA                                        311
         GO TO  5190                                                      311
5200     CONTINUE                                                         311
C    *                                                                    311
C    Geant POSITION : AIRA ===> X=-PMDG_BOUNDARY/(4.*ROOT32)*XLEN1 Y=-PM  312
C    DG_BOUNDARY/2.*ZLEN <===                                             312
         CALL AGSRESET                                                    312
         AG_X = -PMDG_BOUNDARY/(4.*ROOT32)*XLEN1                          312
         AG_Y = -PMDG_BOUNDARY/2.*ZLEN                                    312
         AG_TITLE = 'AIRA'                                                312
         AG_EXNAME = 'POSITION'                                           312
         AG_PARLIST = 'X_Y'                                               312
         CALL AXPOSITION                                                  312
C    *                                                                    312
5121  CALL AGSPOPB                                                        313
      GO TO BLOCK_PMDA, (5120 ,5130,5180)                                 313
C End of Local Procedure                                                  313
C ----------------------------------------------------------------------  314
C ------------ AIRA                                                       314
C Begin Local Procedure AIRA                                              315
C AIRA IS A DETECTOR MADE IN AIR                                          315
5190  CONTINUE                                                            315
         AG_TITLE='AIRA'                                                  315
         AG_MODULE=MODULE                                                 315
         AG_MARK='V00'                                                    315
         CALL AGSPUSH                                                     315
         AG_MODULE=MODULE                                                 315
C    *                                                                    315
C    Geant MATERIAL : AIR ===> inherited arguments used <===              316
         CALL AGSRESET                                                    316
         AG_TITLE = 'AIR'                                                 316
         AG_EXNAME = 'MATERIAL'                                           316
         AG_PARLIST = 'NONE'                                              316
         CALL AXMATERIAL                                                  316
C    *                                                                    316
C    Geant ATTRIBUTE : AIRA ===> SEEN=1 COLO=4 <===                       316
         CALL AGSRESET                                                    317
         AG_SEEN = 1                                                      317
         AG_COLO = 4                                                      317
         AG_TITLE = 'AIRA'                                                317
         AG_EXNAME = 'ATTRIBUTE'                                          317
         AG_PARLIST = 'SEEN_COLO'                                         317
         CALL AXATTRIBUTE                                                 317
C    *                                                                    317
C    Geant SHAPE : PARA ===> DX=XLEN0-PMDG_BOUNDARY/ROOT32, DY=YLEN DZ=S  317
C    M_THICK_A/2. <===                                                    317
         CALL AGSRESET                                                    319
         AG_DX = XLEN0-PMDG_BOUNDARY/ROOT32                               319
         AG_DY = YLEN                                                     319
         AG_DZ = SM_THICK_A/2.                                            319
         AG_TITLE = 'PARA'                                                319
         AG_EXNAME = 'SHAPE'                                              319
         AG_PARLIST = 'DX_DY_DZ'                                          319
         CALL AXSHAPE                                                     319
C    *                                                                    319
         ZZ = - SM_THICK_A/2. + PMDG_TH_PCB/2.                            319
C    Create PCBA :                                                        321
C    Call To Local Procedure PCBA                                         321
         ASSIGN 5220 TO BLOCK_PCBA                                        321
         GO TO  5210                                                      321
5220     CONTINUE                                                         321
C    *                                                                    321
C    Geant POSITION : PCBA ===> Z=ZZ <===                                 322
         CALL AGSRESET                                                    322
         AG_Z = ZZ                                                        322
         AG_TITLE = 'PCBA'                                                322
         AG_EXNAME = 'POSITION'                                           322
         AG_PARLIST = 'Z'                                                 322
         CALL AXPOSITION                                                  322
C    *                                                                    322
         ZZ = ZZ + PMDG_TH_PCB/2 + 3.*PMDG_TH_AIR + PMDG_TH_PCB/2.        322
C    Create PCBA :                                                        324
C    Call To Local Procedure PCBA                                         324
         ASSIGN 5230 TO BLOCK_PCBA                                        324
         GO TO  5210                                                      324
5230     CONTINUE                                                         324
C    *                                                                    324
C    Geant POSITION : PCBA ===> Z=ZZ <===                                 325
         CALL AGSRESET                                                    325
         AG_Z = ZZ                                                        325
         AG_TITLE = 'PCBA'                                                325
         AG_EXNAME = 'POSITION'                                           325
         AG_PARLIST = 'Z'                                                 325
         CALL AXPOSITION                                                  325
C    *                                                                    325
         ZZ = ZZ + PMDG_TH_PCB/2. + PMDG_CELL_DEPTH/2.                    325
C    Create PHCA :                                                        327
C    Call To Local Procedure PHCA                                         327
         ASSIGN 5250 TO BLOCK_PHCA                                        327
         GO TO  5240                                                      327
5250     CONTINUE                                                         327
C    *                                                                    327
C    Geant POSITION : PHCA ===> Z=ZZ <===                                 328
         CALL AGSRESET                                                    328
         AG_Z = ZZ                                                        328
         AG_TITLE = 'PHCA'                                                328
         AG_EXNAME = 'POSITION'                                           328
         AG_PARLIST = 'Z'                                                 328
         CALL AXPOSITION                                                  328
C    *                                                                    328
         ZZ = ZZ + PMDG_CELL_DEPTH/2. + PMDG_TH_PCB/2.                    328
C    Create PCBA :                                                        330
C    Call To Local Procedure PCBA                                         330
         ASSIGN 5260 TO BLOCK_PCBA                                        330
         GO TO  5210                                                      330
5260     CONTINUE                                                         330
C    *                                                                    330
C    Geant POSITION : PCBA ===> Z=ZZ <===                                 331
         CALL AGSRESET                                                    331
         AG_Z = ZZ                                                        331
         AG_TITLE = 'PCBA'                                                331
         AG_EXNAME = 'POSITION'                                           331
         AG_PARLIST = 'Z'                                                 331
         CALL AXPOSITION                                                  331
C    *                                                                    331
         ZZ = ZZ + PMDG_TH_PCB/2. + PMDG_TH_AIR + PMDG_TH_BASE/2.         331
C    Create BASA :                                                        333
C    Call To Local Procedure BASA                                         333
         ASSIGN 5280 TO BLOCK_BASA                                        333
         GO TO  5270                                                      333
5280     CONTINUE                                                         333
C    *                                                                    333
C    Geant POSITION : BASA ===> Z=ZZ <===                                 334
         CALL AGSRESET                                                    334
         AG_Z = ZZ                                                        334
         AG_TITLE = 'BASA'                                                334
         AG_EXNAME = 'POSITION'                                           334
         AG_PARLIST = 'Z'                                                 334
         CALL AXPOSITION                                                  334
C    *                                                                    334
5191  CALL AGSPOPB                                                        335
      GO TO BLOCK_AIRA, (5190 ,5200)                                      335
C End of Local Procedure                                                  335
C ----------------------------------------------------------------------  336
C ------------ PHCA                                                       336
C Begin Local Procedure PHCA                                              337
C PHCA IS THE DETECTOR MADE IN AIR                                        337
5240  CONTINUE                                                            337
         AG_TITLE='PHCA'                                                  337
         AG_MODULE=MODULE                                                 337
         AG_MARK='V00'                                                    337
         CALL AGSPUSH                                                     337
         AG_MODULE=MODULE                                                 337
C    *                                                                    337
C    Geant MATERIAL : AIR ===> inherited arguments used <===              338
         CALL AGSRESET                                                    338
         AG_TITLE = 'AIR'                                                 338
         AG_EXNAME = 'MATERIAL'                                           338
         AG_PARLIST = 'NONE'                                              338
         CALL AXMATERIAL                                                  338
C    *                                                                    338
C    Geant ATTRIBUTE : PHCA ===> SEEN=1 COLO=4 <===                       338
         CALL AGSRESET                                                    339
         AG_SEEN = 1                                                      339
         AG_COLO = 4                                                      339
         AG_TITLE = 'PHCA'                                                339
         AG_EXNAME = 'ATTRIBUTE'                                          339
         AG_PARLIST = 'SEEN_COLO'                                         339
         CALL AXATTRIBUTE                                                 339
C    *                                                                    339
C    Geant SHAPE : PARA ===> DX=XLEN0-PMDG_BOUNDARY/ROOT32, DY=YLEN DZ=P  339
C    MDG_CELL_DEPTH/2. <===                                               339
         CALL AGSRESET                                                    341
         AG_DX = XLEN0-PMDG_BOUNDARY/ROOT32                               341
         AG_DY = YLEN                                                     341
         AG_DZ = PMDG_CELL_DEPTH/2.                                       341
         AG_TITLE = 'PARA'                                                341
         AG_EXNAME = 'SHAPE'                                              341
         AG_PARLIST = 'DX_DY_DZ'                                          341
         CALL AXSHAPE                                                     341
C    *                                                                    341
C    Create ASTR :                                                        342
C    Call To Local Procedure ASTR                                         342
         ASSIGN 5300 TO BLOCK_ASTR                                        342
         GO TO  5290                                                      342
5300     CONTINUE                                                         342
C    *                                                                    342
C    *                                                                    343
         J=1                                                              343
            GO TO 5313                                                    343
5311        J=J+1                                                         343
5313        IF(J-(NCELLY).GT.0) GO TO 5312                                343
            XB=-YLEN*(1/(2*ROOT32))+PMDG_HEXD1(7)*2./3.                   344
     *      +(J-1)*PMDG_HEXD1(7)                                          344
            YB=-YLEN+(2./                                                 345
     *      SQRT(3.))*PMDG_HEXD1(7)+(J-1)*PMDG_HEXD1(7)*SQRT(3.)          345
C       Geant POSITION : ASTR ===> X=XB Y=YB Z=ZB KONLY='MANY' <===       346
            CALL AGSRESET                                                 346
            AG_X = XB                                                     346
            AG_Y = YB                                                     346
            AG_Z = ZB                                                     346
            AG_KONLY = 'MANY'                                             346
            AG_TITLE = 'ASTR'                                             346
            AG_EXNAME = 'POSITION'                                        346
            AG_PARLIST = 'X_Y_Z_KONLY'                                    346
            CALL AXPOSITION                                               346
C       *                                                                 346
         GO TO 5311                                                       347
5312     CONTINUE                                                         347
5241  CALL AGSPOPB                                                        348
      GO TO BLOCK_PHCA, (5240 ,5250)                                      348
C End of Local Procedure                                                  348
C ----------------------------------------------------------------------  349
C ------------ ASTR                                                       349
C Begin Local Procedure ASTR                                              350
C ASTR IS THE STRIP                                                       350
5290  CONTINUE                                                            350
         AG_TITLE='ASTR'                                                  350
         AG_MODULE=MODULE                                                 350
         AG_MARK='V00'                                                    350
         CALL AGSPUSH                                                     350
         AG_MODULE=MODULE                                                 350
C    *                                                                    350
C    Geant MATERIAL : AIR ===> inherited arguments used <===              351
         CALL AGSRESET                                                    351
         AG_TITLE = 'AIR'                                                 351
         AG_EXNAME = 'MATERIAL'                                           351
         AG_PARLIST = 'NONE'                                              351
         CALL AXMATERIAL                                                  351
C    *                                                                    351
C    Geant ATTRIBUTE : ASTR ===> SEEN=0 COLO=6 <===                       351
         CALL AGSRESET                                                    352
         AG_SEEN = 0                                                      352
         AG_COLO = 6                                                      352
         AG_TITLE = 'ASTR'                                                352
         AG_EXNAME = 'ATTRIBUTE'                                          352
         AG_PARLIST = 'SEEN_COLO'                                         352
         CALL AXATTRIBUTE                                                 352
C    *                                                                    352
         XLEN=NCELLX*PMDG_CELL_RADIUS                                     352
C    Geant SHAPE : PARA ===> DX=XLEN DY=PMDG_CELL_RADIUS/ROOT32, DZ=PMDG  355
C    _CELL_DEPTH/2 ALPH=0 <===                                            355
         CALL AGSRESET                                                    355
         AG_DX = XLEN                                                     355
         AG_DY = PMDG_CELL_RADIUS/ROOT32                                  355
         AG_DZ = PMDG_CELL_DEPTH/2                                        355
         AG_ALPH = 0                                                      355
         AG_TITLE = 'PARA'                                                355
         AG_EXNAME = 'SHAPE'                                              355
         AG_PARLIST = 'DX_DY_DZ_ALPH'                                     355
         CALL AXSHAPE                                                     355
C    *                                                                    355
C    Create PSTR :                                                        356
C    Call To Local Procedure PSTR                                         356
         ASSIGN 5330 TO BLOCK_PSTR                                        356
         GO TO  5320                                                      356
5330     CONTINUE                                                         356
C    *                                                                    356
5291  CALL AGSPOPB                                                        357
      GO TO BLOCK_ASTR, (5290 ,5300)                                      357
C End of Local Procedure                                                  357
C ----------------------------------------------------------------------  358
C ------------ PSTR                                                       358
C Begin Local Procedure PSTR                                              359
C PSTR IS ONE PSEUDO-CELL                                                 359
5320  CONTINUE                                                            359
         AG_TITLE='PSTR'                                                  359
         AG_MODULE=MODULE                                                 359
         AG_MARK='V00'                                                    359
         CALL AGSPUSH                                                     359
         AG_MODULE=MODULE                                                 359
C    *                                                                    359
C    Geant SHAPE : DIVISION ===> NDIV=NCELLX IAXIS=1 <===                 360
         CALL AGSRESET                                                    360
         AG_NDIV = NCELLX                                                 360
         AG_IAXIS = 1                                                     360
         AG_TITLE = 'DIVISION'                                            360
         AG_EXNAME = 'SHAPE'                                              360
         AG_PARLIST = 'NDIV_IAXIS'                                        360
         CALL AXSHAPE                                                     360
C    *                                                                    360
C    Create PDCU :                                                        361
C    Call To Local Procedure PDCU                                         361
         ASSIGN 5350 TO BLOCK_PDCU                                        361
         GO TO  5340                                                      361
5350     CONTINUE                                                         361
C    *                                                                    361
C    Geant POSITION : PDCU ===> ALPHAZ=90 <===                            362
         CALL AGSRESET                                                    362
         AG_ALPHAN=AG_ALPHAN+1                                            362
         AG_ALPHAK(AG_ALPHAN)=3                                           362
         AG_ALPHAL(AG_ALPHAN)=90                                          362
         AG_TITLE = 'PDCU'                                                362
         AG_EXNAME = 'POSITION'                                           362
         AG_PARLIST = 'ALPHAZ'                                            362
         CALL AXPOSITION                                                  362
C    *                                                                    362
5321  CALL AGSPOPB                                                        363
      GO TO BLOCK_PSTR, (5320 ,5330)                                      363
C End of Local Procedure                                                  363
C ----------------------------------------------------------------------  364
C ------------ PPBA                                                       364
C Begin Local Procedure PPBA                                              365
C PPBA IS THE LEAD PLATES FOR DIFFERENT MODULES                           365
5140  CONTINUE                                                            365
         AG_TITLE='PPBA'                                                  365
         AG_MODULE=MODULE                                                 365
         AG_MARK='V00'                                                    365
         CALL AGSPUSH                                                     365
         AG_MODULE=MODULE                                                 365
C    *                                                                    365
C    Geant MATERIAL : LEAD ===> inherited arguments used <===             366
         CALL AGSRESET                                                    366
         AG_TITLE = 'LEAD'                                                366
         AG_EXNAME = 'MATERIAL'                                           366
         AG_PARLIST = 'NONE'                                              366
         CALL AXMATERIAL                                                  366
C    *                                                                    366
C    Geant MATERIAL : LEAD_PPBA ===> ISVOL=0 <===                         366
         CALL AGSRESET                                                    367
         AG_ISVOL = 0                                                     367
         AG_TITLE = 'LEAD_PPBA'                                           367
         AG_EXNAME = 'MATERIAL'                                           367
         AG_PARLIST = 'ISVOL'                                             367
         CALL AXMATERIAL                                                  367
C    *                                                                    367
C    Geant ATTRIBUTE : PPBA ===> SEEN=1 COLO=7 <===                       367
         CALL AGSRESET                                                    368
         AG_SEEN = 1                                                      368
         AG_COLO = 7                                                      368
         AG_TITLE = 'PPBA'                                                368
         AG_EXNAME = 'ATTRIBUTE'                                          368
         AG_PARLIST = 'SEEN_COLO'                                         368
         CALL AXATTRIBUTE                                                 368
C    *                                                                    368
C    Geant SHAPE : PARA ===> DX=XLEN0-YLEN1*PMDG_BOUNDARY/(2.*ROOT32), D  368
C    Y=YLEN0-YLEN2*PMDG_BOUNDARY*ROOT34/ROOT32, DZ=PMDG_TH_LEAD/2. <===   368
         CALL AGSRESET                                                    371
         AG_DX = XLEN0-YLEN1*PMDG_BOUNDARY/(2.*ROOT32)                    371
         AG_DY = YLEN0-YLEN2*PMDG_BOUNDARY*ROOT34/ROOT32                  371
         AG_DZ = PMDG_TH_LEAD/2.                                          371
         AG_TITLE = 'PARA'                                                371
         AG_EXNAME = 'SHAPE'                                              371
         AG_PARLIST = 'DX_DY_DZ'                                          371
         CALL AXSHAPE                                                     371
C    *                                                                    371
C    Check PMVR_CONFIG.EQ.1                                               371
         IF (PMVR_CONFIG.EQ.1) THEN                                       372
         CALL GSTPAR(AG_IMED, 'CUTGAM',.0001)                             373
         CALL GSTPAR(AG_IMED, 'CUTELE',.0001)                             374
         ENDIF                                                            375
5141  CALL AGSPOPB                                                        376
      GO TO BLOCK_PPBA, (5140 ,5150)                                      376
C End of Local Procedure                                                  376
C ----------------------------------------------------------------------  377
C ------------ PFEA                                                       377
C Begin Local Procedure PFEA                                              378
C PFEA IS THE IRON PLATES FOR DIFFERENT MODULES                           378
5160  CONTINUE                                                            378
         AG_TITLE='PFEA'                                                  378
         AG_MODULE=MODULE                                                 378
         AG_MARK='V00'                                                    378
         CALL AGSPUSH                                                     378
         AG_MODULE=MODULE                                                 378
C    *                                                                    378
C    Geant MATERIAL : IRON ===> inherited arguments used <===             379
         CALL AGSRESET                                                    379
         AG_TITLE = 'IRON'                                                379
         AG_EXNAME = 'MATERIAL'                                           379
         AG_PARLIST = 'NONE'                                              379
         CALL AXMATERIAL                                                  379
C    *                                                                    379
C    Geant MATERIAL : IRON_PFEA ===> ISVOL=0 <===                         379
         CALL AGSRESET                                                    380
         AG_ISVOL = 0                                                     380
         AG_TITLE = 'IRON_PFEA'                                           380
         AG_EXNAME = 'MATERIAL'                                           380
         AG_PARLIST = 'ISVOL'                                             380
         CALL AXMATERIAL                                                  380
C    *                                                                    380
C    Geant ATTRIBUTE : PFEA ===> SEEN=1 COLO=2 <===                       380
         CALL AGSRESET                                                    381
         AG_SEEN = 1                                                      381
         AG_COLO = 2                                                      381
         AG_TITLE = 'PFEA'                                                381
         AG_EXNAME = 'ATTRIBUTE'                                          381
         AG_PARLIST = 'SEEN_COLO'                                         381
         CALL AXATTRIBUTE                                                 381
C    *                                                                    381
C    Geant SHAPE : PARA ===> DX=XLEN0-YLEN1*PMDG_BOUNDARY/(2.*ROOT32), D  381
C    Y=YLEN0-YLEN2*PMDG_BOUNDARY*ROOT34/ROOT32, DZ=PMDG_TH_STEEL/2. <===  381
         CALL AGSRESET                                                    384
         AG_DX = XLEN0-YLEN1*PMDG_BOUNDARY/(2.*ROOT32)                    384
         AG_DY = YLEN0-YLEN2*PMDG_BOUNDARY*ROOT34/ROOT32                  384
         AG_DZ = PMDG_TH_STEEL/2.                                         384
         AG_TITLE = 'PARA'                                                384
         AG_EXNAME = 'SHAPE'                                              384
         AG_PARLIST = 'DX_DY_DZ'                                          384
         CALL AXSHAPE                                                     384
C    *                                                                    384
C    Check PMVR_CONFIG.EQ.1                                               384
         IF (PMVR_CONFIG.EQ.1) THEN                                       385
         CALL GSTPAR(AG_IMED, 'CUTGAM',.0001)                             386
         CALL GSTPAR(AG_IMED, 'CUTELE',.0001)                             387
         ENDIF                                                            388
5161  CALL AGSPOPB                                                        389
      GO TO BLOCK_PFEA, (5160 ,5170)                                      389
C End of Local Procedure                                                  389
C ----------------------------------------------------------------------  390
C ------------ BASA                                                       390
C Begin Local Procedure BASA                                              391
C BASA IS THE G10 BASE PLATE                                              391
5270  CONTINUE                                                            391
         AG_TITLE='BASA'                                                  391
         AG_MODULE=MODULE                                                 391
         AG_MARK='V00'                                                    391
         CALL AGSPUSH                                                     391
         AG_MODULE=MODULE                                                 391
C    *                                                                    391
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===          392
         CALL AGSRESET                                                    392
         AG_A = 28.08                                                     392
         AG_Z = 14                                                        392
         AG_W = 0.6*1*28./60.                                             392
         AG_TITLE = 'SI'                                                  392
         AG_EXNAME = 'COMPONENT'                                          392
         AG_PARLIST = 'A_Z_W'                                             392
         CALL AXCOMPONENT                                                 392
C    *                                                                    392
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174.   392
C    <===                                                                 392
         CALL AGSRESET                                                    393
         AG_A = 16                                                        393
         AG_Z = 8                                                         393
         AG_W = 0.6*2*16./60. + 0.4*4*16./174.                            393
         AG_TITLE = 'O'                                                   393
         AG_EXNAME = 'COMPONENT'                                          393
         AG_PARLIST = 'A_Z_W'                                             393
         CALL AXCOMPONENT                                                 393
C    *                                                                    393
C    Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===              393
         CALL AGSRESET                                                    394
         AG_A = 12                                                        394
         AG_Z = 6                                                         394
         AG_W = 0.4*8*12./174.                                            394
         AG_TITLE = 'C'                                                   394
         AG_EXNAME = 'COMPONENT'                                          394
         AG_PARLIST = 'A_Z_W'                                             394
         CALL AXCOMPONENT                                                 394
C    *                                                                    394
C    Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===               394
         CALL AGSRESET                                                    395
         AG_A = 1                                                         395
         AG_Z = 1                                                         395
         AG_W = 0.4*14*1./174.                                            395
         AG_TITLE = 'H'                                                   395
         AG_EXNAME = 'COMPONENT'                                          395
         AG_PARLIST = 'A_Z_W'                                             395
         CALL AXCOMPONENT                                                 395
C    *                                                                    395
C    Geant MIXTURE : G10 ===> DENS=1.7 <===                               395
         CALL AGSRESET                                                    396
         AG_DENS = 1.7                                                    396
         AG_TITLE = 'G10'                                                 396
         AG_EXNAME = 'MIXTURE'                                            396
         AG_PARLIST = 'DENS'                                              396
         CALL AXMIXTURE                                                   396
C    *                                                                    396
C    Geant ATTRIBUTE : BASA ===> SEEN=1 COLO=6 <===                       396
         CALL AGSRESET                                                    397
         AG_SEEN = 1                                                      397
         AG_COLO = 6                                                      397
         AG_TITLE = 'BASA'                                                397
         AG_EXNAME = 'ATTRIBUTE'                                          397
         AG_PARLIST = 'SEEN_COLO'                                         397
         CALL AXATTRIBUTE                                                 397
C    *                                                                    397
C    Geant SHAPE : PARA ===> DX=XLEN0-PMDG_BOUNDARY/ROOT32, DY=YLEN DZ=P  397
C    MDG_TH_BASE/2. <===                                                  397
         CALL AGSRESET                                                    399
         AG_DX = XLEN0-PMDG_BOUNDARY/ROOT32                               399
         AG_DY = YLEN                                                     399
         AG_DZ = PMDG_TH_BASE/2.                                          399
         AG_TITLE = 'PARA'                                                399
         AG_EXNAME = 'SHAPE'                                              399
         AG_PARLIST = 'DX_DY_DZ'                                          399
         CALL AXSHAPE                                                     399
C    *                                                                    399
C    Check PMVR_CONFIG.EQ.1                                               399
         IF (PMVR_CONFIG.EQ.1) THEN                                       400
         CALL GSTPAR(AG_IMED, 'CUTGAM',.0001)                             401
         CALL GSTPAR(AG_IMED, 'CUTELE',.0001)                             402
         ENDIF                                                            403
5271  CALL AGSPOPB                                                        404
      GO TO BLOCK_BASA, (5270 ,5280)                                      404
C End of Local Procedure                                                  404
C ----------------------------------------------------------------------  405
C ------------ PCBA                                                       405
C Begin Local Procedure PCBA                                              406
C PCBA IS THE CHAMBER PCB                                                 406
5210  CONTINUE                                                            406
         AG_TITLE='PCBA'                                                  406
         AG_MODULE=MODULE                                                 406
         AG_MARK='V00'                                                    406
         CALL AGSPUSH                                                     406
         AG_MODULE=MODULE                                                 406
C    *                                                                    406
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===          407
         CALL AGSRESET                                                    407
         AG_A = 28.08                                                     407
         AG_Z = 14                                                        407
         AG_W = 0.6*1*28./60.                                             407
         AG_TITLE = 'SI'                                                  407
         AG_EXNAME = 'COMPONENT'                                          407
         AG_PARLIST = 'A_Z_W'                                             407
         CALL AXCOMPONENT                                                 407
C    *                                                                    407
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174.   407
C    <===                                                                 407
         CALL AGSRESET                                                    408
         AG_A = 16                                                        408
         AG_Z = 8                                                         408
         AG_W = 0.6*2*16./60. + 0.4*4*16./174.                            408
         AG_TITLE = 'O'                                                   408
         AG_EXNAME = 'COMPONENT'                                          408
         AG_PARLIST = 'A_Z_W'                                             408
         CALL AXCOMPONENT                                                 408
C    *                                                                    408
C    Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===              408
         CALL AGSRESET                                                    409
         AG_A = 12                                                        409
         AG_Z = 6                                                         409
         AG_W = 0.4*8*12./174.                                            409
         AG_TITLE = 'C'                                                   409
         AG_EXNAME = 'COMPONENT'                                          409
         AG_PARLIST = 'A_Z_W'                                             409
         CALL AXCOMPONENT                                                 409
C    *                                                                    409
C    Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===               409
         CALL AGSRESET                                                    410
         AG_A = 1                                                         410
         AG_Z = 1                                                         410
         AG_W = 0.4*14*1./174.                                            410
         AG_TITLE = 'H'                                                   410
         AG_EXNAME = 'COMPONENT'                                          410
         AG_PARLIST = 'A_Z_W'                                             410
         CALL AXCOMPONENT                                                 410
C    *                                                                    410
C    Geant MIXTURE : G10 ===> DENS=1.7 <===                               410
         CALL AGSRESET                                                    411
         AG_DENS = 1.7                                                    411
         AG_TITLE = 'G10'                                                 411
         AG_EXNAME = 'MIXTURE'                                            411
         AG_PARLIST = 'DENS'                                              411
         CALL AXMIXTURE                                                   411
C    *                                                                    411
C    Geant ATTRIBUTE : PCBA ===> SEEN=1 COLO=4 <===                       411
         CALL AGSRESET                                                    412
         AG_SEEN = 1                                                      412
         AG_COLO = 4                                                      412
         AG_TITLE = 'PCBA'                                                412
         AG_EXNAME = 'ATTRIBUTE'                                          412
         AG_PARLIST = 'SEEN_COLO'                                         412
         CALL AXATTRIBUTE                                                 412
C    *                                                                    412
C    Geant SHAPE : PARA ===> DX=XLEN0-PMDG_BOUNDARY/ROOT32, DY=YLEN DZ=P  412
C    MDG_TH_PCB/2. <===                                                   412
         CALL AGSRESET                                                    414
         AG_DX = XLEN0-PMDG_BOUNDARY/ROOT32                               414
         AG_DY = YLEN                                                     414
         AG_DZ = PMDG_TH_PCB/2.                                           414
         AG_TITLE = 'PARA'                                                414
         AG_EXNAME = 'SHAPE'                                              414
         AG_PARLIST = 'DX_DY_DZ'                                          414
         CALL AXSHAPE                                                     414
C    *                                                                    414
C    Check PMVR_CONFIG.EQ.1                                               414
         IF (PMVR_CONFIG.EQ.1) THEN                                       415
         CALL GSTPAR(AG_IMED, 'CUTGAM',.0001)                             416
         CALL GSTPAR(AG_IMED, 'CUTELE',.0001)                             417
         ENDIF                                                            418
5211  CALL AGSPOPB                                                        419
      GO TO BLOCK_PCBA, (5210 ,5220,5230,5260)                            419
C End of Local Procedure                                                  419
C ----------------------------------------------------------------------  420
C ------------ PDCU                                                       420
C Begin Local Procedure PDCU                                              421
C PDCU IS THE OUTER CELL IN THE PMD MODULE                                421
5340  CONTINUE                                                            421
         AG_TITLE='PDCU'                                                  421
         AG_MODULE=MODULE                                                 421
         AG_MARK='V00'                                                    421
         CALL AGSPUSH                                                     421
         AG_MODULE=MODULE                                                 421
C    *                                                                    421
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           422
         CALL AGSRESET                                                    422
         AG_TITLE = 'COPPER'                                              422
         AG_EXNAME = 'MATERIAL'                                           422
         AG_PARLIST = 'NONE'                                              422
         CALL AXMATERIAL                                                  422
C    *                                                                    422
C    Geant ATTRIBUTE : PDCU ===> SEEN=0 COLO=3 <===                       422
         CALL AGSRESET                                                    423
         AG_SEEN = 0                                                      423
         AG_COLO = 3                                                      423
         AG_TITLE = 'PDCU'                                                423
         AG_EXNAME = 'ATTRIBUTE'                                          423
         AG_PARLIST = 'SEEN_COLO'                                         423
         CALL AXATTRIBUTE                                                 423
C    *                                                                    423
C    Geant SHAPE : PGON ===> PHI1=PMDG_HEXD1(1) DPHI=PMDG_HEXD1(2), NPDI  423
C    V=PMDG_HEXD1(3) NZ=PMDG_HEXD1(4), ZI={PMDG_HEXD1(5),PMDG_HEXD1(8)}   423
C    RMN={PMDG_HEXD1(6), PMDG_HEXD1(9)} RMX={PMDG_HEXD1(7),PMDG_HEXD1(10  423
C    )} <===                                                              423
         CALL AGSRESET                                                    427
         AG_PHI1 = PMDG_HEXD1(1)                                          427
         AG_DPHI = PMDG_HEXD1(2)                                          427
         AG_NPDIV = PMDG_HEXD1(3)                                         427
         AG_NZ = PMDG_HEXD1(4)                                            427
         AG_ZI(1)=PMDG_HEXD1(5)                                           427
         AG_ZI(2)=PMDG_HEXD1(8)                                           427
         AG_RMN(1)=PMDG_HEXD1(6)                                          427
         AG_RMN(2)=PMDG_HEXD1(9)                                          427
         AG_RMX(1)=PMDG_HEXD1(7)                                          427
         AG_RMX(2)=PMDG_HEXD1(10)                                         427
         AG_TITLE = 'PGON'                                                427
         AG_EXNAME = 'SHAPE'                                              427
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     427
         CALL AXSHAPE                                                     427
C    *                                                                    427
C    Create PDGS :                                                        428
C    Call To Local Procedure PDGS                                         428
         ASSIGN 5370 TO BLOCK_PDGS                                        428
         GO TO  5360                                                      428
5370     CONTINUE                                                         428
C    *                                                                    428
C    Geant POSITION : PDGS ===> X=0.0 Y=0.0 Z=0.0 <===                    429
         CALL AGSRESET                                                    429
         AG_X = 0.0                                                       429
         AG_Y = 0.0                                                       429
         AG_Z = 0.0                                                       429
         AG_TITLE = 'PDGS'                                                429
         AG_EXNAME = 'POSITION'                                           429
         AG_PARLIST = 'X_Y_Z'                                             429
         CALL AXPOSITION                                                  429
C    *                                                                    429
5341  CALL AGSPOPB                                                        430
      GO TO BLOCK_PDCU, (5340 ,5350)                                      430
C End of Local Procedure                                                  430
C ----------------------------------------------------------------------  431
C ------------ PDGS                                                       431
C Begin Local Procedure PDGS                                              432
C PDGS IS THE INNER CELL IN THE PMD MODULE                                432
5360  CONTINUE                                                            432
         AG_TITLE='PDGS'                                                  432
         AG_MODULE=MODULE                                                 432
         AG_MARK='V00'                                                    432
         CALL AGSPUSH                                                     432
         AG_MODULE=MODULE                                                 432
C    *                                                                    432
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           433
         CALL AGSRESET                                                    433
         AG_ISVOL = 1                                                     433
         AG_TITLE = 'SENSITIVE'                                           433
         AG_EXNAME = 'MEDIUM'                                             433
         AG_PARLIST = 'ISVOL'                                             433
         CALL AXMEDIUM                                                    433
C    *                                                                    433
C    Geant COMPONENT : AR ===> A=40 Z=18 W=.7 <===                        433
         CALL AGSRESET                                                    434
         AG_A = 40                                                        434
         AG_Z = 18                                                        434
         AG_W = .7                                                        434
         AG_TITLE = 'AR'                                                  434
         AG_EXNAME = 'COMPONENT'                                          434
         AG_PARLIST = 'A_Z_W'                                             434
         CALL AXCOMPONENT                                                 434
C    *                                                                    434
C    Geant COMPONENT : C ===> A=12 Z=6 W=.3*12/44. <===                   434
         CALL AGSRESET                                                    435
         AG_A = 12                                                        435
         AG_Z = 6                                                         435
         AG_W = .3*12/44.                                                 435
         AG_TITLE = 'C'                                                   435
         AG_EXNAME = 'COMPONENT'                                          435
         AG_PARLIST = 'A_Z_W'                                             435
         CALL AXCOMPONENT                                                 435
C    *                                                                    435
C    Geant COMPONENT : O ===> A=16 Z=8 W=.3*32/44. <===                   435
         CALL AGSRESET                                                    436
         AG_A = 16                                                        436
         AG_Z = 8                                                         436
         AG_W = .3*32/44.                                                 436
         AG_TITLE = 'O'                                                   436
         AG_EXNAME = 'COMPONENT'                                          436
         AG_PARLIST = 'A_Z_W'                                             436
         CALL AXCOMPONENT                                                 436
C    *                                                                    436
C    Geant MIXTURE : AR_CO2 ===> DENS=0.0018405 <===                      436
         CALL AGSRESET                                                    437
         AG_DENS = 0.0018405                                              437
         AG_TITLE = 'AR_CO2'                                              437
         AG_EXNAME = 'MIXTURE'                                            437
         AG_PARLIST = 'DENS'                                              437
         CALL AXMIXTURE                                                   437
C    *                                                                    437
C    Geant ATTRIBUTE : PDGS ===> SEEN=0 COLO=3 <===                       437
         CALL AGSRESET                                                    438
         AG_SEEN = 0                                                      438
         AG_COLO = 3                                                      438
         AG_TITLE = 'PDGS'                                                438
         AG_EXNAME = 'ATTRIBUTE'                                          438
         AG_PARLIST = 'SEEN_COLO'                                         438
         CALL AXATTRIBUTE                                                 438
C    *                                                                    438
C    Geant SHAPE : PGON ===> PHI1=PMDG_HEXD2(1) DPHI=PMDG_HEXD2(2), NPDI  438
C    V=PMDG_HEXD2(3) NZ=PMDG_HEXD2(4), ZI={PMDG_HEXD2(5),PMDG_HEXD2(8)}   438
C    RMN={PMDG_HEXD2(6), PMDG_HEXD2(9)} RMX={PMDG_HEXD2(7),PMDG_HEXD2(10  438
C    )} <===                                                              438
         CALL AGSRESET                                                    442
         AG_PHI1 = PMDG_HEXD2(1)                                          442
         AG_DPHI = PMDG_HEXD2(2)                                          442
         AG_NPDIV = PMDG_HEXD2(3)                                         442
         AG_NZ = PMDG_HEXD2(4)                                            442
         AG_ZI(1)=PMDG_HEXD2(5)                                           442
         AG_ZI(2)=PMDG_HEXD2(8)                                           442
         AG_RMN(1)=PMDG_HEXD2(6)                                          442
         AG_RMN(2)=PMDG_HEXD2(9)                                          442
         AG_RMX(1)=PMDG_HEXD2(7)                                          442
         AG_RMX(2)=PMDG_HEXD2(10)                                         442
         AG_TITLE = 'PGON'                                                442
         AG_EXNAME = 'SHAPE'                                              442
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     442
         CALL AXSHAPE                                                     442
C    *                                                                    442
C    Check PMVR_CONFIG.EQ.1                                               442
         IF (PMVR_CONFIG.EQ.1) THEN                                       443
         CALL GSTPAR(AG_IMED, 'CUTGAM',.0001)                             444
         CALL GSTPAR(AG_IMED, 'CUTELE',.00001)                            445
         ENDIF                                                            446
C    Defining Hits: ELOSS:0:(0,1)                                         447
         AG_TITLE = 'PDGS'                                                447
         AG_EXNAME = 'HITS'                                               447
         AG_TYPE=AG_MAGIC                                                 447
         CALL AGSDETV('PDGS')                                             447
C    *                                                                    447
         AG_PARLIST='ELOSS'                                               447
         AG_TITLE = 'DETH'                                                447
         AG_NBITS = 0                                                     447
         AG_BIN=AG_MAGIC                                                  447
         AG_OPTION='*'                                                    447
         AG_FHMIN = 0                                                     447
         AG_FHMAX = 1                                                     447
         CALL AGSDETH (0)                                                 447
C    *                                                                    447
         CALL AGSDETU('PDGS')                                             447
C    *                                                                    447
5361  CALL AGSPOPB                                                        448
      GO TO BLOCK_PDGS, (5360 ,5370)                                      448
C End of Local Procedure                                                  448
      WRITE(LOUT,5380)'===>>  [End AgML Module PHMDGEO]  <<==='           448
5380  FORMAT(A32,/,/)                                                     448
C MODULE PHMDGEO                                                          450
      END                                                                 450
