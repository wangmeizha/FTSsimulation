C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE MUTDGEO4                                                 1
C MUTDGEO4 IS THE GEOMETRY OF THE STAR MTD, WMRPC VERSION                 1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'MUTDGEO4'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'IS THE GEOMETRY OF THE STAR MTD, WMRPC VERSION'/     1
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
C AUTHOR W.J. LLOPE                                                       3
      CHARACTER*40 AUTHOR /'W.J. LLOPE'/                                  4
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  4
C CREATED 21 JULY 2010                                                    4
      CHARACTER*40 CREATED /'21 JULY 2010'/                               5
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  5
      INTEGER BLOCK_MUTD                                                  7
      INTEGER BLOCK_MTMT                                                  7
      INTEGER BLOCK_MTMF                                                  7
      INTEGER BLOCK_MTEB                                                  7
      INTEGER BLOCK_MTTF                                                  7
      INTEGER BLOCK_MTTT                                                  7
      INTEGER BLOCK_MTRA                                                  7
      INTEGER BLOCK_MTBP                                                  7
      INTEGER BLOCK_MTTP                                                  7
      INTEGER BLOCK_MCHL                                                  7
      INTEGER BLOCK_MCAL                                                  7
      INTEGER BLOCK_MCHS                                                  7
      INTEGER BLOCK_MCAS                                                  7
      INTEGER BLOCK_MIGS                                                  7
      INTEGER BLOCK_MIGG                                                  7
C -                                                                       8
      REAL MTDG_VERSION                                                   8
      EQUIVALENCE (BANK_MTDG(3),MTDG_VERSION,RE_MTDG_VERSION),            8
     *(NAMES_MTDG(1),NMTDG_VERSION)                                       8
      EQUIVALENCE (COMMEN_MTDG(1),CMTDG_VERSION),(MAP_MTDG(1,1),          8
     *MMTDG_VERSION)                                                      8
      CHARACTER CMTDG_VERSION*40/' '/                                     8
      CHARACTER NMTDG_VERSION*12/'R.VERSION'/                             8
      INTEGER MMTDG_VERSION(2)/1,1/                                       8
      REAL RE_MTDG_VERSION                                                8
C -                                                                       8
      REAL MTDG_CONFIG                                                    8
      EQUIVALENCE (BANK_MTDG(4),MTDG_CONFIG,RE_MTDG_CONFIG),              8
     *(NAMES_MTDG(2),NMTDG_CONFIG)                                        8
      EQUIVALENCE (COMMEN_MTDG(2),CMTDG_CONFIG),(MAP_MTDG(1,2),           8
     *MMTDG_CONFIG)                                                       8
      CHARACTER CMTDG_CONFIG*40/' '/                                      8
      CHARACTER NMTDG_CONFIG*12/'R.CONFIG'/                               8
      INTEGER MMTDG_CONFIG(2)/1,1/                                        8
      REAL RE_MTDG_CONFIG                                                 8
C -                                                                       8
      INTEGER LEN_MTDG,LLEN_MTDG,LINK_MTDG                                8
      PARAMETER (LEN_MTDG=4)                                              8
      PARAMETER (LLEN_MTDG=2)                                             8
      INTEGER FLAG_MTDG/0/,MAP_MTDG(2,LLEN_MTDG),IDNUM_MTDG(10)           8
      CHARACTER NAMES_MTDG(LLEN_MTDG)*12,COMMEN_MTDG(LLEN_MTDG)*40        8
      DATA IDNUM_MTDG/10*0/                                               8
      REAL BANK_MTDG(LEN_MTDG)                                            8
      CHARACTER*60 BPATH_MTDG/' '/                                        8
      SAVE FLAG_MTDG,MAP_MTDG,IDNUM_MTDG,NAMES_MTDG,COMMEN_MTDG,          8
     *BPATH_MTDG                                                          8
      DATA LINK_MTDG/0/,BANK_MTDG/LEN_MTDG*0/                             8
C *                                                                       8
C -                                                                       12
      REAL MTDD_RMIN                                                      12
      EQUIVALENCE (BANK_MTDD(3),MTDD_RMIN,RE_MTDD_RMIN),(NAMES_MTDD(1),   12
     *NMTDD_RMIN)                                                         12
      EQUIVALENCE (COMMEN_MTDD(1),CMTDD_RMIN),(MAP_MTDD(1,1),             12
     *MMTDD_RMIN)                                                         12
      CHARACTER CMTDD_RMIN*40/' '/                                        12
      CHARACTER NMTDD_RMIN*12/'R.RMIN'/                                   12
      INTEGER MMTDD_RMIN(2)/1,1/                                          12
      REAL RE_MTDD_RMIN                                                   12
C -                                                                       12
      REAL MTDD_RMAX                                                      12
      EQUIVALENCE (BANK_MTDD(4),MTDD_RMAX,RE_MTDD_RMAX),(NAMES_MTDD(2),   12
     *NMTDD_RMAX)                                                         12
      EQUIVALENCE (COMMEN_MTDD(2),CMTDD_RMAX),(MAP_MTDD(1,2),             12
     *MMTDD_RMAX)                                                         12
      CHARACTER CMTDD_RMAX*40/' '/                                        12
      CHARACTER NMTDD_RMAX*12/'R.RMAX'/                                   12
      INTEGER MMTDD_RMAX(2)/1,1/                                          12
      REAL RE_MTDD_RMAX                                                   12
C -                                                                       12
      REAL MTDD_DZMOTHER                                                  12
      EQUIVALENCE (BANK_MTDD(5),MTDD_DZMOTHER,RE_MTDD_DZMOTHER),          12
     *(NAMES_MTDD(3),NMTDD_DZMOTHER)                                      12
      EQUIVALENCE (COMMEN_MTDD(3),CMTDD_DZMOTHER),(MAP_MTDD(1,3),         12
     *MMTDD_DZMOTHER)                                                     12
      CHARACTER CMTDD_DZMOTHER*40/' '/                                    12
      CHARACTER NMTDD_DZMOTHER*12/'R.DZMOTHER'/                           12
      INTEGER MMTDD_DZMOTHER(2)/1,1/                                      12
      REAL RE_MTDD_DZMOTHER                                               12
C -                                                                       12
      REAL MTDD_BACKLEGR                                                  12
      EQUIVALENCE (BANK_MTDD(6),MTDD_BACKLEGR,RE_MTDD_BACKLEGR),          12
     *(NAMES_MTDD(4),NMTDD_BACKLEGR)                                      12
      EQUIVALENCE (COMMEN_MTDD(4),CMTDD_BACKLEGR),(MAP_MTDD(1,4),         12
     *MMTDD_BACKLEGR)                                                     12
      CHARACTER CMTDD_BACKLEGR*40/' '/                                    12
      CHARACTER NMTDD_BACKLEGR*12/'R.BACKLEGR'/                           12
      INTEGER MMTDD_BACKLEGR(2)/1,1/                                      12
      REAL RE_MTDD_BACKLEGR                                               12
C -                                                                       12
      REAL MTDD_BEMCELECTBOXDX                                            12
      EQUIVALENCE (BANK_MTDD(7),MTDD_BEMCELECTBOXDX,                      12
     *RE_MTDD_BEMCELECTBOXDX),(NAMES_MTDD(5),NMTDD_BEMCELECTBOXDX)        12
      EQUIVALENCE (COMMEN_MTDD(5),CMTDD_BEMCELECTBOXDX),(MAP_MTDD(1,5),   12
     *MMTDD_BEMCELECTBOXDX)                                               12
      CHARACTER CMTDD_BEMCELECTBOXDX*40/' '/                              12
      CHARACTER NMTDD_BEMCELECTBOXDX*12/'R.BEMCELECTBOXDX'/               12
      INTEGER MMTDD_BEMCELECTBOXDX(2)/1,1/                                12
      REAL RE_MTDD_BEMCELECTBOXDX                                         12
C -                                                                       12
      REAL MTDD_BEMCELECTBOXDY                                            12
      EQUIVALENCE (BANK_MTDD(8),MTDD_BEMCELECTBOXDY,                      12
     *RE_MTDD_BEMCELECTBOXDY),(NAMES_MTDD(6),NMTDD_BEMCELECTBOXDY)        12
      EQUIVALENCE (COMMEN_MTDD(6),CMTDD_BEMCELECTBOXDY),(MAP_MTDD(1,6),   12
     *MMTDD_BEMCELECTBOXDY)                                               12
      CHARACTER CMTDD_BEMCELECTBOXDY*40/' '/                              12
      CHARACTER NMTDD_BEMCELECTBOXDY*12/'R.BEMCELECTBOXDY'/               12
      INTEGER MMTDD_BEMCELECTBOXDY(2)/1,1/                                12
      REAL RE_MTDD_BEMCELECTBOXDY                                         12
C -                                                                       12
      REAL MTDD_BEMCELECTBOXDZ3                                           12
      EQUIVALENCE (BANK_MTDD(9),MTDD_BEMCELECTBOXDZ3,                     12
     *RE_MTDD_BEMCELECTBOXDZ3),(NAMES_MTDD(7),NMTDD_BEMCELECTBOXDZ3)      12
      EQUIVALENCE (COMMEN_MTDD(7),CMTDD_BEMCELECTBOXDZ3),(MAP_MTDD(1,7),  12
     *MMTDD_BEMCELECTBOXDZ3)                                              12
      CHARACTER CMTDD_BEMCELECTBOXDZ3*40/' '/                             12
      CHARACTER NMTDD_BEMCELECTBOXDZ3*12/'R.BEMCELECTBOXDZ3'/             12
      INTEGER MMTDD_BEMCELECTBOXDZ3(2)/1,1/                               12
      REAL RE_MTDD_BEMCELECTBOXDZ3                                        12
C -                                                                       12
      REAL MTDD_BEMCELECTBOXDZ5                                           12
      EQUIVALENCE (BANK_MTDD(10),MTDD_BEMCELECTBOXDZ5,                    12
     *RE_MTDD_BEMCELECTBOXDZ5),(NAMES_MTDD(8),NMTDD_BEMCELECTBOXDZ5)      12
      EQUIVALENCE (COMMEN_MTDD(8),CMTDD_BEMCELECTBOXDZ5),(MAP_MTDD(1,8),  12
     *MMTDD_BEMCELECTBOXDZ5)                                              12
      CHARACTER CMTDD_BEMCELECTBOXDZ5*40/' '/                             12
      CHARACTER NMTDD_BEMCELECTBOXDZ5*12/'R.BEMCELECTBOXDZ5'/             12
      INTEGER MMTDD_BEMCELECTBOXDZ5(2)/1,1/                               12
      REAL RE_MTDD_BEMCELECTBOXDZ5                                        12
C -                                                                       12
      REAL MTDD_RGAP                                                      12
      EQUIVALENCE (BANK_MTDD(11),MTDD_RGAP,RE_MTDD_RGAP),(NAMES_MTDD(9),  12
     *NMTDD_RGAP)                                                         12
      EQUIVALENCE (COMMEN_MTDD(9),CMTDD_RGAP),(MAP_MTDD(1,9),             12
     *MMTDD_RGAP)                                                         12
      CHARACTER CMTDD_RGAP*40/' '/                                        12
      CHARACTER NMTDD_RGAP*12/'R.RGAP'/                                   12
      INTEGER MMTDD_RGAP(2)/1,1/                                          12
      REAL RE_MTDD_RGAP                                                   12
C -                                                                       12
      REAL MTDD_MTDMOTHERDX                                               12
      EQUIVALENCE (BANK_MTDD(12),MTDD_MTDMOTHERDX,RE_MTDD_MTDMOTHERDX),   12
     *(NAMES_MTDD(10),NMTDD_MTDMOTHERDX)                                  12
      EQUIVALENCE (COMMEN_MTDD(10),CMTDD_MTDMOTHERDX),(MAP_MTDD(1,10),    12
     *MMTDD_MTDMOTHERDX)                                                  12
      CHARACTER CMTDD_MTDMOTHERDX*40/' '/                                 12
      CHARACTER NMTDD_MTDMOTHERDX*12/'R.MTDMOTHERDX'/                     12
      INTEGER MMTDD_MTDMOTHERDX(2)/1,1/                                   12
      REAL RE_MTDD_MTDMOTHERDX                                            12
C -                                                                       12
      REAL MTDD_MTDMOTHERDY                                               12
      EQUIVALENCE (BANK_MTDD(13),MTDD_MTDMOTHERDY,RE_MTDD_MTDMOTHERDY),   12
     *(NAMES_MTDD(11),NMTDD_MTDMOTHERDY)                                  12
      EQUIVALENCE (COMMEN_MTDD(11),CMTDD_MTDMOTHERDY),(MAP_MTDD(1,11),    12
     *MMTDD_MTDMOTHERDY)                                                  12
      CHARACTER CMTDD_MTDMOTHERDY*40/' '/                                 12
      CHARACTER NMTDD_MTDMOTHERDY*12/'R.MTDMOTHERDY'/                     12
      INTEGER MMTDD_MTDMOTHERDY(2)/1,1/                                   12
      REAL RE_MTDD_MTDMOTHERDY                                            12
C -                                                                       12
      REAL MTDD_MTDMOTHERDZ3                                              12
      EQUIVALENCE (BANK_MTDD(14),MTDD_MTDMOTHERDZ3,                       12
     *RE_MTDD_MTDMOTHERDZ3),(NAMES_MTDD(12),NMTDD_MTDMOTHERDZ3)           12
      EQUIVALENCE (COMMEN_MTDD(12),CMTDD_MTDMOTHERDZ3),(MAP_MTDD(1,12),   12
     *MMTDD_MTDMOTHERDZ3)                                                 12
      CHARACTER CMTDD_MTDMOTHERDZ3*40/' '/                                12
      CHARACTER NMTDD_MTDMOTHERDZ3*12/'R.MTDMOTHERDZ3'/                   12
      INTEGER MMTDD_MTDMOTHERDZ3(2)/1,1/                                  12
      REAL RE_MTDD_MTDMOTHERDZ3                                           12
C -                                                                       12
      REAL MTDD_MTDMOTHERDZ5                                              12
      EQUIVALENCE (BANK_MTDD(15),MTDD_MTDMOTHERDZ5,                       12
     *RE_MTDD_MTDMOTHERDZ5),(NAMES_MTDD(13),NMTDD_MTDMOTHERDZ5)           12
      EQUIVALENCE (COMMEN_MTDD(13),CMTDD_MTDMOTHERDZ5),(MAP_MTDD(1,13),   12
     *MMTDD_MTDMOTHERDZ5)                                                 12
      CHARACTER CMTDD_MTDMOTHERDZ5*40/' '/                                12
      CHARACTER NMTDD_MTDMOTHERDZ5*12/'R.MTDMOTHERDZ5'/                   12
      INTEGER MMTDD_MTDMOTHERDZ5(2)/1,1/                                  12
      REAL RE_MTDD_MTDMOTHERDZ5                                           12
C -                                                                       12
      REAL MTDD_MTDTRAYDX                                                 12
      EQUIVALENCE (BANK_MTDD(16),MTDD_MTDTRAYDX,RE_MTDD_MTDTRAYDX),       12
     *(NAMES_MTDD(14),NMTDD_MTDTRAYDX)                                    12
      EQUIVALENCE (COMMEN_MTDD(14),CMTDD_MTDTRAYDX),(MAP_MTDD(1,14),      12
     *MMTDD_MTDTRAYDX)                                                    12
      CHARACTER CMTDD_MTDTRAYDX*40/' '/                                   12
      CHARACTER NMTDD_MTDTRAYDX*12/'R.MTDTRAYDX'/                         12
      INTEGER MMTDD_MTDTRAYDX(2)/1,1/                                     12
      REAL RE_MTDD_MTDTRAYDX                                              12
C -                                                                       12
      REAL MTDD_MTDTRAYDY                                                 12
      EQUIVALENCE (BANK_MTDD(17),MTDD_MTDTRAYDY,RE_MTDD_MTDTRAYDY),       12
     *(NAMES_MTDD(15),NMTDD_MTDTRAYDY)                                    12
      EQUIVALENCE (COMMEN_MTDD(15),CMTDD_MTDTRAYDY),(MAP_MTDD(1,15),      12
     *MMTDD_MTDTRAYDY)                                                    12
      CHARACTER CMTDD_MTDTRAYDY*40/' '/                                   12
      CHARACTER NMTDD_MTDTRAYDY*12/'R.MTDTRAYDY'/                         12
      INTEGER MMTDD_MTDTRAYDY(2)/1,1/                                     12
      REAL RE_MTDD_MTDTRAYDY                                              12
C -                                                                       12
      REAL MTDD_MTDTRAYDZ                                                 12
      EQUIVALENCE (BANK_MTDD(18),MTDD_MTDTRAYDZ,RE_MTDD_MTDTRAYDZ),       12
     *(NAMES_MTDD(16),NMTDD_MTDTRAYDZ)                                    12
      EQUIVALENCE (COMMEN_MTDD(16),CMTDD_MTDTRAYDZ),(MAP_MTDD(1,16),      12
     *MMTDD_MTDTRAYDZ)                                                    12
      CHARACTER CMTDD_MTDTRAYDZ*40/' '/                                   12
      CHARACTER NMTDD_MTDTRAYDZ*12/'R.MTDTRAYDZ'/                         12
      INTEGER MMTDD_MTDTRAYDZ(2)/1,1/                                     12
      REAL RE_MTDD_MTDTRAYDZ                                              12
C -                                                                       12
      REAL MTDD_MTDBPLATE                                                 12
      EQUIVALENCE (BANK_MTDD(19),MTDD_MTDBPLATE,RE_MTDD_MTDBPLATE),       12
     *(NAMES_MTDD(17),NMTDD_MTDBPLATE)                                    12
      EQUIVALENCE (COMMEN_MTDD(17),CMTDD_MTDBPLATE),(MAP_MTDD(1,17),      12
     *MMTDD_MTDBPLATE)                                                    12
      CHARACTER CMTDD_MTDBPLATE*40/' '/                                   12
      CHARACTER NMTDD_MTDBPLATE*12/'R.MTDBPLATE'/                         12
      INTEGER MMTDD_MTDBPLATE(2)/1,1/                                     12
      REAL RE_MTDD_MTDBPLATE                                              12
C -                                                                       12
      REAL MTDD_MTDTPLATE                                                 12
      EQUIVALENCE (BANK_MTDD(20),MTDD_MTDTPLATE,RE_MTDD_MTDTPLATE),       12
     *(NAMES_MTDD(18),NMTDD_MTDTPLATE)                                    12
      EQUIVALENCE (COMMEN_MTDD(18),CMTDD_MTDTPLATE),(MAP_MTDD(1,18),      12
     *MMTDD_MTDTPLATE)                                                    12
      CHARACTER CMTDD_MTDTPLATE*40/' '/                                   12
      CHARACTER NMTDD_MTDTPLATE*12/'R.MTDTPLATE'/                         12
      INTEGER MMTDD_MTDTPLATE(2)/1,1/                                     12
      REAL RE_MTDD_MTDTPLATE                                              12
C -                                                                       12
      REAL MTDD_MTDIGSTACKDY                                              12
      EQUIVALENCE (BANK_MTDD(21),MTDD_MTDIGSTACKDY,                       12
     *RE_MTDD_MTDIGSTACKDY),(NAMES_MTDD(19),NMTDD_MTDIGSTACKDY)           12
      EQUIVALENCE (COMMEN_MTDD(19),CMTDD_MTDIGSTACKDY),(MAP_MTDD(1,19),   12
     *MMTDD_MTDIGSTACKDY)                                                 12
      CHARACTER CMTDD_MTDIGSTACKDY*40/' '/                                12
      CHARACTER NMTDD_MTDIGSTACKDY*12/'R.MTDIGSTACKDY'/                   12
      INTEGER MMTDD_MTDIGSTACKDY(2)/1,1/                                  12
      REAL RE_MTDD_MTDIGSTACKDY                                           12
C -                                                                       12
      REAL MTDD_MTDIGSTACKDZ                                              12
      EQUIVALENCE (BANK_MTDD(22),MTDD_MTDIGSTACKDZ,                       12
     *RE_MTDD_MTDIGSTACKDZ),(NAMES_MTDD(20),NMTDD_MTDIGSTACKDZ)           12
      EQUIVALENCE (COMMEN_MTDD(20),CMTDD_MTDIGSTACKDZ),(MAP_MTDD(1,20),   12
     *MMTDD_MTDIGSTACKDZ)                                                 12
      CHARACTER CMTDD_MTDIGSTACKDZ*40/' '/                                12
      CHARACTER NMTDD_MTDIGSTACKDZ*12/'R.MTDIGSTACKDZ'/                   12
      INTEGER MMTDD_MTDIGSTACKDZ(2)/1,1/                                  12
      REAL RE_MTDD_MTDIGSTACKDZ                                           12
C -                                                                       12
      REAL MTDD_MTDIGSTACKNGAPS                                           12
      EQUIVALENCE (BANK_MTDD(23),MTDD_MTDIGSTACKNGAPS,                    12
     *RE_MTDD_MTDIGSTACKNGAPS),(NAMES_MTDD(21),NMTDD_MTDIGSTACKNGAPS)     12
      EQUIVALENCE (COMMEN_MTDD(21),CMTDD_MTDIGSTACKNGAPS),(MAP_MTDD(1,    12
     *21),MMTDD_MTDIGSTACKNGAPS)                                          12
      CHARACTER CMTDD_MTDIGSTACKNGAPS*40/' '/                             12
      CHARACTER NMTDD_MTDIGSTACKNGAPS*12/'R.MTDIGSTACKNGAPS'/             12
      INTEGER MMTDD_MTDIGSTACKNGAPS(2)/1,1/                               12
      REAL RE_MTDD_MTDIGSTACKNGAPS                                        12
C -                                                                       12
      REAL MTDD_MTDIGSTACKGAPTHICK                                        12
      EQUIVALENCE (BANK_MTDD(24),MTDD_MTDIGSTACKGAPTHICK,                 12
     *RE_MTDD_MTDIGSTACKGAPTHICK),(NAMES_MTDD(22),                        12
     *NMTDD_MTDIGSTACKGAPTHICK)                                           12
      EQUIVALENCE (COMMEN_MTDD(22),CMTDD_MTDIGSTACKGAPTHICK),             12
     *(MAP_MTDD(1,22),MMTDD_MTDIGSTACKGAPTHICK)                           12
      CHARACTER CMTDD_MTDIGSTACKGAPTHICK*40/' '/                          12
      CHARACTER NMTDD_MTDIGSTACKGAPTHICK*12/'R.MTDIGSTACKGAPTHICK'/       12
      INTEGER MMTDD_MTDIGSTACKGAPTHICK(2)/1,1/                            12
      REAL RE_MTDD_MTDIGSTACKGAPTHICK                                     12
C -                                                                       12
      REAL MTDD_MTDIGSTACKTHINGLASSTHICK                                  12
      EQUIVALENCE (BANK_MTDD(25),MTDD_MTDIGSTACKTHINGLASSTHICK,           12
     *RE_MTDD_MTDIGSTACKTHINGLASSTHICK),(NAMES_MTDD(23),                  12
     *NMTDD_MTDIGSTACKTHINGLASSTHICK)                                     12
      EQUIVALENCE (COMMEN_MTDD(23),CMTDD_MTDIGSTACKTHINGLASSTHICK),       12
     *(MAP_MTDD(1,23),MMTDD_MTDIGSTACKTHINGLASSTHICK)                     12
      CHARACTER CMTDD_MTDIGSTACKTHINGLASSTHICK*40/' '/                    12
      CHARACTER NMTDD_MTDIGSTACKTHINGLASSTHICK*12/'R.MTDIGSTACKTHINGLASS  12
     *THICK'/                                                             12
      INTEGER MMTDD_MTDIGSTACKTHINGLASSTHICK(2)/1,1/                      12
      REAL RE_MTDD_MTDIGSTACKTHINGLASSTHICK                               12
C -                                                                       12
      INTEGER LEN_MTDD,LLEN_MTDD,LINK_MTDD                                12
      PARAMETER (LEN_MTDD=25)                                             12
      PARAMETER (LLEN_MTDD=23)                                            12
      INTEGER FLAG_MTDD/0/,MAP_MTDD(2,LLEN_MTDD),IDNUM_MTDD(10)           12
      CHARACTER NAMES_MTDD(LLEN_MTDD)*12,COMMEN_MTDD(LLEN_MTDD)*40        12
      DATA IDNUM_MTDD/10*0/                                               12
      REAL BANK_MTDD(LEN_MTDD)                                            12
      CHARACTER*60 BPATH_MTDD/' '/                                        12
      SAVE FLAG_MTDD,MAP_MTDD,IDNUM_MTDD,NAMES_MTDD,COMMEN_MTDD,          12
     *BPATH_MTDD                                                          12
      DATA LINK_MTDD/0/,BANK_MTDD/LEN_MTDD*0/                             12
C *                                                                       12
      INTEGER KTRAYCONFIG,IPHI,IBL,NTRAYPLACED,IGAP,NGAPS                 13
      REAL THISPHI,THISX,CHANDIM,THISH                                    15
C *                                                                       17
      CALL AGSBEGM(MODULE,IPRIN)                                          17
      IF (IPRIN.LT.0) RETURN                                              17
C Filling bank MTDG - MUON SYSTEM CONFIGURATION PARAMETERS                17
      BANK_TITLE='MUON SYSTEM CONFIGURATION PARAMETERS'                   17
      BPATH_MTDG='MTDG*'                                                  17
      IDNUM_MTDG(1)=0                                                     18
C VERSION NUMBER                                                          18
      MTDG_VERSION=1                                                      18
      CMTDG_VERSION='VERSION NUMBER'                                      18
C =1 SINGLE BACKLEG (RUN11), =2 FULL SYSTEM (RUN13)                       19
      MTDG_CONFIG=1                                                       19
      CMTDG_CONFIG='=1 SINGLE BACKLEG (RUN11), =2 FULL SYSTEM (RUN13)'    19
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MTDG',    20
     *BPATH_MTDG,IDNUM_MTDG,1, MAP_MTDG,NAMES_MTDG,COMMEN_MTDG,           20
     *BANK_MTDG,LEN_MTDG,LLEN_MTDG,LINK_MTDG,FLAG_MTDG)                   20
C *                                                                       20
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MTDG',    20
     *BPATH_MTDG,IDNUM_MTDG,1, MAP_MTDG,NAMES_MTDG,COMMEN_MTDG,           20
     *BANK_MTDG,LEN_MTDG,LLEN_MTDG,LINK_MTDG,FLAG_MTDG)                   20
C *                                                                       21
      CALL AGSBEGM(MODULE,IPRIN)                                          21
      IF (IPRIN.LT.0) RETURN                                              21
C Filling bank MTDD - MUON SYSTEM GEOMETRY PARAMETERS                     21
      BANK_TITLE='MUON SYSTEM GEOMETRY PARAMETERS'                        21
      BPATH_MTDD='MTDD*'                                                  21
      IDNUM_MTDD(1)=0                                                     22
C INTEGRATION RADIUS LOWER                                                22
      MTDD_RMIN=364.25                                                    22
      CMTDD_RMIN='INTEGRATION RADIUS LOWER'                               22
C INTEGRATION RADIUS UPPER                                                23
      MTDD_RMAX=415.0                                                     23
      CMTDD_RMAX='INTEGRATION RADIUS UPPER'                               23
C INTEGRATION HALF LENGTH IN Z                                            24
      MTDD_DZMOTHER=300.0                                                 24
      CMTDD_DZMOTHER='INTEGRATION HALF LENGTH IN Z'                       24
C RADIUS OF A BACKLEG AT PHI-CENTER                                       25
      MTDD_BACKLEGR=365.0                                                 25
      CMTDD_BACKLEGR='RADIUS OF A BACKLEG AT PHI-CENTER'                  25
C BEMC ELECTRONICS BOX HALF-LENGTH X (HEIGHT)                             26
      MTDD_BEMCELECTBOXDX=11.035                                          26
      CMTDD_BEMCELECTBOXDX='BEMC ELECTRONICS BOX HALF-LENGTH X            26
     *(HEIGHT)'                                                           26
C BEMC ELECTRONICS BOX HALF-LENGTH Y (WIDTH)                              27
      MTDD_BEMCELECTBOXDY=30.65                                           27
      CMTDD_BEMCELECTBOXDY='BEMC ELECTRONICS BOX HALF-LENGTH Y (WIDTH)'   27
C BEMC ELECTRONICS BOX HALF-LENGTH Z (LENGTH 5-PACK)                      28
      MTDD_BEMCELECTBOXDZ5=290.0                                          28
      CMTDD_BEMCELECTBOXDZ5='BEMC ELECTRONICS BOX HALF-LENGTH Z (LENGTH   28
     *5-PACK)'                                                            28
C BEMC ELECTRONICS BOX HALF-LENGTH Z (LENGTH 3-PACK)                      29
      MTDD_BEMCELECTBOXDZ3=200.0                                          29
      CMTDD_BEMCELECTBOXDZ3='BEMC ELECTRONICS BOX HALF-LENGTH Z (LENGTH   29
     *3-PACK)'                                                            29
C DISTANCE TOP OF BEMC BOX TO UNDERSIDE OF LOWEST TRAY                    30
      MTDD_RGAP=3.81                                                      30
      CMTDD_RGAP='DISTANCE TOP OF BEMC BOX TO UNDERSIDE OF LOWEST TRAY'   30
C TRAY MOTHER HALF-LENGTH X (HEIGHT)                                      31
      MTDD_MTDMOTHERDX=5.5                                                31
      CMTDD_MTDMOTHERDX='TRAY MOTHER HALF-LENGTH X (HEIGHT)'              31
C TRAY MOTHER HALF-LENGTH Y (WIDTH)                                       32
      MTDD_MTDMOTHERDY=32.0                                               32
      CMTDD_MTDMOTHERDY='TRAY MOTHER HALF-LENGTH Y (WIDTH)'               32
C TRAY MOTHER HALF-LENGTH Z (LENGTH LONG)                                 33
      MTDD_MTDMOTHERDZ5=285.0                                             33
      CMTDD_MTDMOTHERDZ5='TRAY MOTHER HALF-LENGTH Z (LENGTH LONG)'        33
C TRAY MOTHER HALF-LENGTH Z (LENGTH SHORT)                                34
      MTDD_MTDMOTHERDZ3=195.0                                             34
      CMTDD_MTDMOTHERDZ3='TRAY MOTHER HALF-LENGTH Z (LENGTH SHORT)'       34
C TRAY HALF-HEIGHT                                                        35
      MTDD_MTDTRAYDX=2.4956                                               35
      CMTDD_MTDTRAYDX='TRAY HALF-HEIGHT'                                  35
C TRAY HALF-WIDTH                                                         36
      MTDD_MTDTRAYDY=33.765                                               36
      CMTDD_MTDTRAYDY='TRAY HALF-WIDTH'                                   36
C TRAY HALF-LENGTH                                                        37
      MTDD_MTDTRAYDZ=54.005                                               37
      CMTDD_MTDTRAYDZ='TRAY HALF-LENGTH'                                  37
C TRAY BOTTOM PLATE THICKNESS                                             38
      MTDD_MTDBPLATE=0.3175                                               38
      CMTDD_MTDBPLATE='TRAY BOTTOM PLATE THICKNESS'                       38
C TRAY TOP PLATE THICKNESS                                                39
      MTDD_MTDTPLATE=0.2286                                               39
      CMTDD_MTDTPLATE='TRAY TOP PLATE THICKNESS'                          39
C WMRPC INNER GLASS STACK VOLUME HALF-LENGTH Y                            40
      MTDD_MTDIGSTACKDY=26.1                                              40
      CMTDD_MTDIGSTACKDY='WMRPC INNER GLASS STACK VOLUME HALF-LENGTH Y'   40
C WMRPC INNER GLASS STACK VOLUME HALF-LENGTH Y                            41
      MTDD_MTDIGSTACKDZ=43.5                                              41
      CMTDD_MTDIGSTACKDZ='WMRPC INNER GLASS STACK VOLUME HALF-LENGTH Y'   41
C WMPRC NUMBER OF GAS GAPS                                                42
      MTDD_MTDIGSTACKNGAPS=5.0                                            42
      CMTDD_MTDIGSTACKNGAPS='WMPRC NUMBER OF GAS GAPS'                    42
C WMRPC GAS THICKNESS (0.25MM)                                            43
      MTDD_MTDIGSTACKGAPTHICK=0.025                                       43
      CMTDD_MTDIGSTACKGAPTHICK='WMRPC GAS THICKNESS (0.25MM)'             43
C WMRPC THIN GLASS THICKNESS (0.7MM)                                      44
      MTDD_MTDIGSTACKTHINGLASSTHICK=0.07                                  44
      CMTDD_MTDIGSTACKTHINGLASSTHICK='WMRPC THIN GLASS THICKNESS (0.7MM)  44
     *'                                                                   44
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MTDD',    45
     *BPATH_MTDD,IDNUM_MTDD,1, MAP_MTDD,NAMES_MTDD,COMMEN_MTDD,           45
     *BANK_MTDD,LEN_MTDD,LLEN_MTDD,LINK_MTDD,FLAG_MTDD)                   45
C *                                                                       45
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MTDD',    45
     *BPATH_MTDD,IDNUM_MTDD,1, MAP_MTDD,NAMES_MTDD,COMMEN_MTDD,           45
     *BANK_MTDD,LEN_MTDD,LLEN_MTDD,LINK_MTDD,FLAG_MTDD)                   45
C use default MTDG bank                                                   46
      BPATH_MTDG='MTDG*'                                                  46
      IDNUM_MTDG(1)=0                                                     46
      CALL AGDGETP('MUTDGEO4','*',-999,'MTDG',' ',BANK_MTDG, BPATH_MTDG,  46
     *IDNUM_MTDG,1,MAP_MTDG,NAMES_MTDG,BANK_MTDG,LEN_MTDG,LLEN_MTDG,      46
     *LINK_MTDG,FLAG_MTDG)                                                46
C use default MTDD bank                                                   47
      BPATH_MTDD='MTDD*'                                                  47
      IDNUM_MTDD(1)=0                                                     47
      CALL AGDGETP('MUTDGEO4','*',-999,'MTDD',' ',BANK_MTDD, BPATH_MTDD,  47
     *IDNUM_MTDD,1,MAP_MTDD,NAMES_MTDD,BANK_MTDD,LEN_MTDD,LLEN_MTDD,      47
     *LINK_MTDD,FLAG_MTDD)                                                47
      WRITE(LOUT,5010)MTDG_VERSION                                        47
5010  FORMAT(' MuTD: MTDG_Version = ', F5.3)                              49
      WRITE(LOUT,5020)MTDG_CONFIG                                         49
5020  FORMAT(' MuTD: MTDG_Config = ', F5.3)                               51
      KTRAYCONFIG = NINT(MTDG_CONFIG)                                     52
      NGAPS = MTDD_MTDIGSTACKNGAPS                                        53
C Geant COMPONENT : H ===> A=1 Z=1 W=0.90*2*1./102.+0.+0.05*10*1./58. <=  54
C ==                                                                      54
      CALL AGSRESET                                                       54
      AG_A = 1                                                            54
      AG_Z = 1                                                            54
      AG_W = 0.90*2*1./102.+0.+0.05*10*1./58.                             54
      AG_TITLE = 'H'                                                      54
      AG_EXNAME = 'COMPONENT'                                             54
      AG_PARLIST = 'A_Z_W'                                                54
      CALL AXCOMPONENT                                                    54
C *                                                                       54
C Geant COMPONENT : C ===> A=12 Z=6 W=0.90*2*12./102.+0.+0.05*4*12./58.   54
C <===                                                                    54
      CALL AGSRESET                                                       55
      AG_A = 12                                                           55
      AG_Z = 6                                                            55
      AG_W = 0.90*2*12./102.+0.+0.05*4*12./58.                            55
      AG_TITLE = 'C'                                                      55
      AG_EXNAME = 'COMPONENT'                                             55
      AG_PARLIST = 'A_Z_W'                                                55
      CALL AXCOMPONENT                                                    55
C *                                                                       55
C Geant COMPONENT : F ===> A=19 Z=9 W=0.90*4*19./102.+0.05*6*19./146.+0.  55
C  <===                                                                   55
      CALL AGSRESET                                                       56
      AG_A = 19                                                           56
      AG_Z = 9                                                            56
      AG_W = 0.90*4*19./102.+0.05*6*19./146.+0.                           56
      AG_TITLE = 'F'                                                      56
      AG_EXNAME = 'COMPONENT'                                             56
      AG_PARLIST = 'A_Z_W'                                                56
      CALL AXCOMPONENT                                                    56
C *                                                                       56
C Geant COMPONENT : S ===> A=32 Z=16 W=0.+0.05*1*32./146.+0. <===         56
      CALL AGSRESET                                                       57
      AG_A = 32                                                           57
      AG_Z = 16                                                           57
      AG_W = 0.+0.05*1*32./146.+0.                                        57
      AG_TITLE = 'S'                                                      57
      AG_EXNAME = 'COMPONENT'                                             57
      AG_PARLIST = 'A_Z_W'                                                57
      CALL AXCOMPONENT                                                    57
C *                                                                       57
C Geant MIXTURE : RPCGAS ===> DENS=4.55E-3 <===                           57
      CALL AGSRESET                                                       58
      AG_DENS = 4.55E-3                                                   58
      AG_TITLE = 'RPCGAS'                                                 58
      AG_EXNAME = 'MIXTURE'                                               58
      AG_PARLIST = 'DENS'                                                 58
      CALL AXMIXTURE                                                      58
C *                                                                       58
C Geant COMPONENT : SI ===> A=28 Z=14 W=1. <===                           58
      CALL AGSRESET                                                       59
      AG_A = 28                                                           59
      AG_Z = 14                                                           59
      AG_W = 1.                                                           59
      AG_TITLE = 'SI'                                                     59
      AG_EXNAME = 'COMPONENT'                                             59
      AG_PARLIST = 'A_Z_W'                                                59
      CALL AXCOMPONENT                                                    59
C *                                                                       59
C Geant COMPONENT : O ===> A=16 Z=8 W=2. <===                             59
      CALL AGSRESET                                                       60
      AG_A = 16                                                           60
      AG_Z = 8                                                            60
      AG_W = 2.                                                           60
      AG_TITLE = 'O'                                                      60
      AG_EXNAME = 'COMPONENT'                                             60
      AG_PARLIST = 'A_Z_W'                                                60
      CALL AXCOMPONENT                                                    60
C *                                                                       60
C Geant MIXTURE : GLASS ===> DENS=2.5 <===                                60
      CALL AGSRESET                                                       61
      AG_DENS = 2.5                                                       61
      AG_TITLE = 'GLASS'                                                  61
      AG_EXNAME = 'MIXTURE'                                               61
      AG_PARLIST = 'DENS'                                                 61
      CALL AXMIXTURE                                                      61
C *                                                                       61
      CALL AGSBEGM(MODULE,IPRIN)                                          61
C Check IPRIN<0                                                           62
      IF (IPRIN.LT.0) RETURN                                              62
C Create MUTD :                                                           62
C Call To Local Procedure MUTD                                            62
      ASSIGN 5040 TO BLOCK_MUTD                                           62
      GO TO  5030                                                         62
5040  CONTINUE                                                            62
C *                                                                       62
C Geant POSITION : MUTD ===> MOTHER='CAVE' <===                           63
      CALL AGSRESET                                                       63
      AG_MOTHER = 'CAVE'                                                  63
      AG_TITLE = 'MUTD'                                                   63
      AG_EXNAME = 'POSITION'                                              63
      AG_PARLIST = 'MOTHER'                                               63
      CALL AXPOSITION                                                     63
C *                                                                       63
C ----------------------------------------------------------------------  63
C ------------ MUTD                                                       63
      CALL AGSENDM                                                        65
      RETURN                                                              65
C Begin Local Procedure MUTD                                              65
C MUTD IS THE MUON DETECTOR MOTHER                                        65
5030  CONTINUE                                                            65
         AG_TITLE='MUTD'                                                  65
         AG_MODULE=MODULE                                                 65
         AG_MARK='V00'                                                    65
         CALL AGSPUSH                                                     65
         AG_MODULE=MODULE                                                 65
C    *                                                                    65
C    Geant MATERIAL : AIR ===> inherited arguments used <===              66
         CALL AGSRESET                                                    66
         AG_TITLE = 'AIR'                                                 66
         AG_EXNAME = 'MATERIAL'                                           66
         AG_PARLIST = 'NONE'                                              66
         CALL AXMATERIAL                                                  66
C    *                                                                    66
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           66
         CALL AGSRESET                                                    67
         AG_TITLE = 'STANDARD'                                            67
         AG_EXNAME = 'MEDIUM'                                             67
         AG_PARLIST = 'NONE'                                              67
         CALL AXMEDIUM                                                    67
C    *                                                                    67
C    Geant ATTRIBUTE : MUTD ===> SEEN=0 COLO=1 <===                       67
         CALL AGSRESET                                                    68
         AG_SEEN = 0                                                      68
         AG_COLO = 1                                                      68
         AG_TITLE = 'MUTD'                                                68
         AG_EXNAME = 'ATTRIBUTE'                                          68
         AG_PARLIST = 'SEEN_COLO'                                         68
         CALL AXATTRIBUTE                                                 68
C    *                                                                    68
C    Geant SHAPE : TUBE ===> RMIN=MTDD_RMIN RMAX=MTDD_RMAX, DZ=MTDD_DZMO  68
C    THER <===                                                            68
         CALL AGSRESET                                                    70
         AG_RMIN = MTDD_RMIN                                              70
         AG_RMAX = MTDD_RMAX                                              70
         AG_DZ = MTDD_DZMOTHER                                            70
         AG_TITLE = 'TUBE'                                                70
         AG_EXNAME = 'SHAPE'                                              70
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      70
         CALL AXSHAPE                                                     70
C    *                                                                    70
         NTRAYPLACED = 0                                                  70
C    Create MTMT :                                                        72
C    Call To Local Procedure MTMT                                         72
         ASSIGN 5060 TO BLOCK_MTMT                                        72
         GO TO  5050                                                      72
5060     CONTINUE                                                         72
C    *                                                                    72
C    Check KTRAYCONFIG.EQ.4                                               73
         IF (KTRAYCONFIG.EQ.4) THEN                                       73
         THISPHI = 156                                                    74
C    Geant POSITION : MTMT ===> Z=0 ALPHAZ=150 <===                       75
         CALL AGSRESET                                                    75
         AG_Z = 0                                                         75
         AG_ALPHAN=AG_ALPHAN+1                                            75
         AG_ALPHAK(AG_ALPHAN)=3                                           75
         AG_ALPHAL(AG_ALPHAN)=150                                         75
         AG_TITLE = 'MTMT'                                                75
         AG_EXNAME = 'POSITION'                                           75
         AG_PARLIST = 'Z_ALPHAZ'                                          75
         CALL AXPOSITION                                                  75
C    *                                                                    75
         NTRAYPLACED = NTRAYPLACED + 3                                    75
         ELSEIF (KTRAYCONFIG.EQ.5) THEN                                   77
C    Create MTMF :                                                        78
C    Call To Local Procedure MTMF                                         78
         ASSIGN 5080 TO BLOCK_MTMF                                        78
         GO TO  5070                                                      78
5080     CONTINUE                                                         78
C    *                                                                    78
C    *                                                                    79
         IBL=1                                                            79
            GO TO 5093                                                    79
5091        IBL=IBL+1                                                     79
5093        IF(IBL-(30).GT.0) GO TO 5092                                  79
            THISPHI = 90.0 + (IBL-1)*(-12.0)                              80
C       Check IBL.NE.8.AND.IBL.NE.9.AND.IBL.NE.23.AND.IBL.NE.24           81
            IF (IBL.NE.8.AND.IBL.NE.9.AND.IBL.NE.23.AND.IBL.NE.24) THEN   81
C       Check IBL.GE.12.AND.IBL.LE.20                                     82
            IF (IBL.GE.12.AND.IBL.LE.20) THEN                             82
C       Geant POSITION : MTMT ===> Z=0 ALPHAZ=THISPHI <===                83
            CALL AGSRESET                                                 83
            AG_Z = 0                                                      83
            AG_ALPHAN=AG_ALPHAN+1                                         83
            AG_ALPHAK(AG_ALPHAN)=3                                        83
            AG_ALPHAL(AG_ALPHAN)=THISPHI                                  83
            AG_TITLE = 'MTMT'                                             83
            AG_EXNAME = 'POSITION'                                        83
            AG_PARLIST = 'Z_ALPHAZ'                                       83
            CALL AXPOSITION                                               83
C       *                                                                 83
            NTRAYPLACED = NTRAYPLACED + 3                                 83
            ELSE                                                          85
C       Geant POSITION : MTMF ===> Z=0 ALPHAZ=THISPHI <===                86
            CALL AGSRESET                                                 86
            AG_Z = 0                                                      86
            AG_ALPHAN=AG_ALPHAN+1                                         86
            AG_ALPHAK(AG_ALPHAN)=3                                        86
            AG_ALPHAL(AG_ALPHAN)=THISPHI                                  86
            AG_TITLE = 'MTMF'                                             86
            AG_EXNAME = 'POSITION'                                        86
            AG_PARLIST = 'Z_ALPHAZ'                                       86
            CALL AXPOSITION                                               86
C       *                                                                 86
            NTRAYPLACED = NTRAYPLACED + 5                                 86
            ENDIF                                                         88
            ENDIF                                                         89
         GO TO 5091                                                       90
5092     CONTINUE                                                         90
         ELSEIF (KTRAYCONFIG.EQ.12) THEN                                  91
C    Create MTMF :                                                        92
C    Call To Local Procedure MTMF                                         92
         ASSIGN 5100 TO BLOCK_MTMF                                        92
         GO TO  5070                                                      92
5100     CONTINUE                                                         92
C    *                                                                    92
C    *                                                                    93
         IBL=26                                                           93
            GO TO 5113                                                    93
5111        IBL=IBL+1                                                     93
5113        IF(IBL-(28).GT.0) GO TO 5112                                  93
            THISPHI = 90.0 + (IBL-1)*(-12.0)                              94
C       Check IBL.EQ.26                                                   95
            IF (IBL.EQ.26) THEN                                           95
C       Geant POSITION : MTMT ===> Z=0 ALPHAZ=THISPHI <===                96
            CALL AGSRESET                                                 96
            AG_Z = 0                                                      96
            AG_ALPHAN=AG_ALPHAN+1                                         96
            AG_ALPHAK(AG_ALPHAN)=3                                        96
            AG_ALPHAL(AG_ALPHAN)=THISPHI                                  96
            AG_TITLE = 'MTMT'                                             96
            AG_EXNAME = 'POSITION'                                        96
            AG_PARLIST = 'Z_ALPHAZ'                                       96
            CALL AXPOSITION                                               96
C       *                                                                 96
            NTRAYPLACED = NTRAYPLACED + 3                                 96
            ELSE                                                          98
C       Geant POSITION : MTMF ===> Z=0 ALPHAZ=THISPHI <===                99
            CALL AGSRESET                                                 99
            AG_Z = 0                                                      99
            AG_ALPHAN=AG_ALPHAN+1                                         99
            AG_ALPHAK(AG_ALPHAN)=3                                        99
            AG_ALPHAL(AG_ALPHAN)=THISPHI                                  99
            AG_TITLE = 'MTMF'                                             99
            AG_EXNAME = 'POSITION'                                        99
            AG_PARLIST = 'Z_ALPHAZ'                                       99
            CALL AXPOSITION                                               99
C       *                                                                 99
            NTRAYPLACED = NTRAYPLACED + 5                                 99
            ENDIF                                                         101
         GO TO 5111                                                       102
5112     CONTINUE                                                         102
         ELSEIF (KTRAYCONFIG.EQ.13) THEN                                  103
         WRITE(LOUT,5120)KTRAYCONFIG                                      103
5120     FORMAT('Creating MTD geometry with kTrayConfig=', I3)            103
C    Create MTMF :                                                        105
C    Call To Local Procedure MTMF                                         105
         ASSIGN 5130 TO BLOCK_MTMF                                        105
         GO TO  5070                                                      105
5130     CONTINUE                                                         105
C    *                                                                    105
C    *                                                                    106
         IBL=1                                                            106
            GO TO 5143                                                    106
5141        IBL=IBL+1                                                     106
5143        IF(IBL-(30).GT.0) GO TO 5142                                  106
            THISPHI = 90.0 + (IBL-1)*(-12.0)                              107
C       Check (IBL.NE.8.AND.IBL.NE.9.AND.IBL.NE.23.AND.IBL.NE.24)         108
            IF ((IBL.NE.8.AND.IBL.NE.9.AND.IBL.NE.23.AND.IBL.NE.24))      108
     *      THEN                                                          108
C       Check IBL.LE.10.OR.IBL.GE.22                                      109
            IF (IBL.LE.10.OR.IBL.GE.22) THEN                              109
C       Geant POSITION : MTMF ===> Z=0 ALPHAZ=THISPHI <===                110
            CALL AGSRESET                                                 110
            AG_Z = 0                                                      110
            AG_ALPHAN=AG_ALPHAN+1                                         110
            AG_ALPHAK(AG_ALPHAN)=3                                        110
            AG_ALPHAL(AG_ALPHAN)=THISPHI                                  110
            AG_TITLE = 'MTMF'                                             110
            AG_EXNAME = 'POSITION'                                        110
            AG_PARLIST = 'Z_ALPHAZ'                                       110
            CALL AXPOSITION                                               110
C       *                                                                 110
            NTRAYPLACED = NTRAYPLACED + 5                                 110
            ENDIF                                                         112
            ENDIF                                                         113
         GO TO 5141                                                       114
5142     CONTINUE                                                         114
         ENDIF                                                            115
5031  CALL AGSPOPB                                                        116
      GO TO BLOCK_MUTD, (5030 ,5040)                                      116
C End of Local Procedure                                                  116
C ----------------------------------------------------------------------  117
C ------------ MTMF                                                       117
C Begin Local Procedure MTMF                                              118
C MTMF IS THE BACKLEG MOTHER THAT ENCLOSES FIVE TRAYS                     118
5070  CONTINUE                                                            118
         AG_TITLE='MTMF'                                                  118
         AG_MODULE=MODULE                                                 118
         AG_MARK='V00'                                                    118
         CALL AGSPUSH                                                     118
         AG_MODULE=MODULE                                                 118
C    *                                                                    118
C    Geant MATERIAL : AIR ===> inherited arguments used <===              119
         CALL AGSRESET                                                    119
         AG_TITLE = 'AIR'                                                 119
         AG_EXNAME = 'MATERIAL'                                           119
         AG_PARLIST = 'NONE'                                              119
         CALL AXMATERIAL                                                  119
C    *                                                                    119
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           119
         CALL AGSRESET                                                    120
         AG_TITLE = 'STANDARD'                                            120
         AG_EXNAME = 'MEDIUM'                                             120
         AG_PARLIST = 'NONE'                                              120
         CALL AXMEDIUM                                                    120
C    *                                                                    120
C    Geant ATTRIBUTE : MTMF ===> SEEN=1 COLO=6 <===                       120
         CALL AGSRESET                                                    121
         AG_SEEN = 1                                                      121
         AG_COLO = 6                                                      121
         AG_TITLE = 'MTMF'                                                121
         AG_EXNAME = 'ATTRIBUTE'                                          121
         AG_PARLIST = 'SEEN_COLO'                                         121
         CALL AXATTRIBUTE                                                 121
C    *                                                                    121
C    Geant SHAPE : TUBS ===> PHI1=-6.0 PHI2=6.0 <===                      121
         CALL AGSRESET                                                    122
         AG_PHI1 = -6.0                                                   122
         AG_PHI2 = 6.0                                                    122
         AG_TITLE = 'TUBS'                                                122
         AG_EXNAME = 'SHAPE'                                              122
         AG_PARLIST = 'PHI1_PHI2'                                         122
         CALL AXSHAPE                                                     122
C    *                                                                    122
         THISX = MTDD_BACKLEGR + MTDD_BEMCELECTBOXDX                      122
         AG_DZ = MTDD_BEMCELECTBOXDZ5                                     124
C    Create MTEB :                                                        124
C    Call To Local Procedure MTEB                                         124
         ASSIGN 5160 TO BLOCK_MTEB                                        124
         GO TO  5150                                                      124
5160     CONTINUE                                                         124
C    *                                                                    124
C    Geant POSITION : MTEB ===> X=THISX <===                              125
         CALL AGSRESET                                                    125
         AG_X = THISX                                                     125
         AG_TITLE = 'MTEB'                                                125
         AG_EXNAME = 'POSITION'                                           125
         AG_PARLIST = 'X'                                                 125
         CALL AXPOSITION                                                  125
C    *                                                                    125
         THISX = MTDD_BACKLEGR + 2.*(MTDD_BEMCELECTBOXDX + MTDD_RGAP) +   125
     *   MTDD_MTDMOTHERDX                                                 125
         AG_DZ = MTDD_MTDMOTHERDZ5                                        127
C    Create MTTF :                                                        127
C    Call To Local Procedure MTTF                                         127
         ASSIGN 5180 TO BLOCK_MTTF                                        127
         GO TO  5170                                                      127
5180     CONTINUE                                                         127
C    *                                                                    127
C    Geant POSITION : MTTF ===> X=THISX <===                              128
         CALL AGSRESET                                                    128
         AG_X = THISX                                                     128
         AG_TITLE = 'MTTF'                                                128
         AG_EXNAME = 'POSITION'                                           128
         AG_PARLIST = 'X'                                                 128
         CALL AXPOSITION                                                  128
C    *                                                                    128
5071  CALL AGSPOPB                                                        129
      GO TO BLOCK_MTMF, (5070 ,5080,5100,5130)                            129
C End of Local Procedure                                                  129
C ----------------------------------------------------------------------  130
C ------------ MTMT                                                       130
C Begin Local Procedure MTMT                                              131
C MTMT IS THE BACKLEG MOTHER THAT ENCLOSES THREE TRAYS                    131
5050  CONTINUE                                                            131
         AG_TITLE='MTMT'                                                  131
         AG_MODULE=MODULE                                                 131
         AG_MARK='V00'                                                    131
         CALL AGSPUSH                                                     131
         AG_MODULE=MODULE                                                 131
C    *                                                                    131
C    Geant MATERIAL : AIR ===> inherited arguments used <===              132
         CALL AGSRESET                                                    132
         AG_TITLE = 'AIR'                                                 132
         AG_EXNAME = 'MATERIAL'                                           132
         AG_PARLIST = 'NONE'                                              132
         CALL AXMATERIAL                                                  132
C    *                                                                    132
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           132
         CALL AGSRESET                                                    133
         AG_TITLE = 'STANDARD'                                            133
         AG_EXNAME = 'MEDIUM'                                             133
         AG_PARLIST = 'NONE'                                              133
         CALL AXMEDIUM                                                    133
C    *                                                                    133
C    Geant ATTRIBUTE : MTMT ===> SEEN=1 COLO=4 <===                       133
         CALL AGSRESET                                                    134
         AG_SEEN = 1                                                      134
         AG_COLO = 4                                                      134
         AG_TITLE = 'MTMT'                                                134
         AG_EXNAME = 'ATTRIBUTE'                                          134
         AG_PARLIST = 'SEEN_COLO'                                         134
         CALL AXATTRIBUTE                                                 134
C    *                                                                    134
C    Geant SHAPE : TUBS ===> PHI1=-6.0 PHI2=6.0 <===                      134
         CALL AGSRESET                                                    135
         AG_PHI1 = -6.0                                                   135
         AG_PHI2 = 6.0                                                    135
         AG_TITLE = 'TUBS'                                                135
         AG_EXNAME = 'SHAPE'                                              135
         AG_PARLIST = 'PHI1_PHI2'                                         135
         CALL AXSHAPE                                                     135
C    *                                                                    135
         THISX = MTDD_BACKLEGR + MTDD_BEMCELECTBOXDX                      135
         AG_DZ = MTDD_BEMCELECTBOXDZ3                                     137
C    Create MTEB :                                                        137
C    Call To Local Procedure MTEB                                         137
         ASSIGN 5190 TO BLOCK_MTEB                                        137
         GO TO  5150                                                      137
5190     CONTINUE                                                         137
C    *                                                                    137
C    Geant POSITION : MTEB ===> X=THISX <===                              138
         CALL AGSRESET                                                    138
         AG_X = THISX                                                     138
         AG_TITLE = 'MTEB'                                                138
         AG_EXNAME = 'POSITION'                                           138
         AG_PARLIST = 'X'                                                 138
         CALL AXPOSITION                                                  138
C    *                                                                    138
         THISX = MTDD_BACKLEGR + 2.*(MTDD_BEMCELECTBOXDX + MTDD_RGAP) +   138
     *   MTDD_MTDMOTHERDX                                                 138
         AG_DZ = MTDD_MTDMOTHERDZ3                                        140
C    Create MTTT :                                                        140
C    Call To Local Procedure MTTT                                         140
         ASSIGN 5210 TO BLOCK_MTTT                                        140
         GO TO  5200                                                      140
5210     CONTINUE                                                         140
C    *                                                                    140
C    Geant POSITION : MTTT ===> X=THISX <===                              141
         CALL AGSRESET                                                    141
         AG_X = THISX                                                     141
         AG_TITLE = 'MTTT'                                                141
         AG_EXNAME = 'POSITION'                                           141
         AG_PARLIST = 'X'                                                 141
         CALL AXPOSITION                                                  141
C    *                                                                    141
5051  CALL AGSPOPB                                                        142
      GO TO BLOCK_MTMT, (5050 ,5060)                                      142
C End of Local Procedure                                                  142
C ----------------------------------------------------------------------  143
C ------------ MTEB                                                       143
C Begin Local Procedure MTEB                                              144
C MTEB IS THE BEMC ELECTRONICS BOX                                        144
5150  CONTINUE                                                            144
         AG_TITLE='MTEB'                                                  144
         AG_MODULE=MODULE                                                 144
         AG_MARK='V00'                                                    144
         CALL AGSPUSH                                                     144
         AG_MODULE=MODULE                                                 144
C    *                                                                    144
C    Geant MATERIAL : AIR ===> inherited arguments used <===              145
         CALL AGSRESET                                                    145
         AG_TITLE = 'AIR'                                                 145
         AG_EXNAME = 'MATERIAL'                                           145
         AG_PARLIST = 'NONE'                                              145
         CALL AXMATERIAL                                                  145
C    *                                                                    145
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           145
         CALL AGSRESET                                                    146
         AG_TITLE = 'STANDARD'                                            146
         AG_EXNAME = 'MEDIUM'                                             146
         AG_PARLIST = 'NONE'                                              146
         CALL AXMEDIUM                                                    146
C    *                                                                    146
C    Geant ATTRIBUTE : MTEB ===> SEEN=1 COLO=1 <===                       146
         CALL AGSRESET                                                    147
         AG_SEEN = 1                                                      147
         AG_COLO = 1                                                      147
         AG_TITLE = 'MTEB'                                                147
         AG_EXNAME = 'ATTRIBUTE'                                          147
         AG_PARLIST = 'SEEN_COLO'                                         147
         CALL AXATTRIBUTE                                                 147
C    *                                                                    147
C    Geant SHAPE : BOX ===> DX=MTDD_BEMCELECTBOXDX DY=MTDD_BEMCELECTBOXD  147
C    Y <===                                                               147
         CALL AGSRESET                                                    148
         AG_DX = MTDD_BEMCELECTBOXDX                                      148
         AG_DY = MTDD_BEMCELECTBOXDY                                      148
         AG_TITLE = 'BOX'                                                 148
         AG_EXNAME = 'SHAPE'                                              148
         AG_PARLIST = 'DX_DY'                                             148
         CALL AXSHAPE                                                     148
C    *                                                                    148
5151  CALL AGSPOPB                                                        149
      GO TO BLOCK_MTEB, (5150 ,5160,5190)                                 149
C End of Local Procedure                                                  149
C ----------------------------------------------------------------------  150
C ------------ MTTF                                                       150
C Begin Local Procedure MTTF                                              151
C MTTF IS THE MTD11 5-TRAY GROUP MOTHER                                   151
5170  CONTINUE                                                            151
         AG_TITLE='MTTF'                                                  151
         AG_MODULE=MODULE                                                 151
         AG_MARK='V00'                                                    151
         CALL AGSPUSH                                                     151
         AG_MODULE=MODULE                                                 151
C    *                                                                    151
C    Geant MATERIAL : AIR ===> inherited arguments used <===              152
         CALL AGSRESET                                                    152
         AG_TITLE = 'AIR'                                                 152
         AG_EXNAME = 'MATERIAL'                                           152
         AG_PARLIST = 'NONE'                                              152
         CALL AXMATERIAL                                                  152
C    *                                                                    152
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           152
         CALL AGSRESET                                                    153
         AG_TITLE = 'STANDARD'                                            153
         AG_EXNAME = 'MEDIUM'                                             153
         AG_PARLIST = 'NONE'                                              153
         CALL AXMEDIUM                                                    153
C    *                                                                    153
C    Geant ATTRIBUTE : MTTF ===> SEEN=1 COLO=3 <===                       153
         CALL AGSRESET                                                    154
         AG_SEEN = 1                                                      154
         AG_COLO = 3                                                      154
         AG_TITLE = 'MTTF'                                                154
         AG_EXNAME = 'ATTRIBUTE'                                          154
         AG_PARLIST = 'SEEN_COLO'                                         154
         CALL AXATTRIBUTE                                                 154
C    *                                                                    154
C    Geant SHAPE : BOX ===> DX=MTDD_MTDMOTHERDX DY=MTDD_MTDMOTHERDY <===  154
         CALL AGSRESET                                                    155
         AG_DX = MTDD_MTDMOTHERDX                                         155
         AG_DY = MTDD_MTDMOTHERDY                                         155
         AG_TITLE = 'BOX'                                                 155
         AG_EXNAME = 'SHAPE'                                              155
         AG_PARLIST = 'DX_DY'                                             155
         CALL AXSHAPE                                                     155
C    *                                                                    155
C    Create MTRA :                                                        156
C    Call To Local Procedure MTRA                                         156
         ASSIGN 5230 TO BLOCK_MTRA                                        156
         GO TO  5220                                                      156
5230     CONTINUE                                                         156
C    *                                                                    156
C    Geant POSITION : MTRA ===> X=MTDD_MTDTRAYDX Z=-174 <===              157
         CALL AGSRESET                                                    157
         AG_X = MTDD_MTDTRAYDX                                            157
         AG_Z = -174                                                      157
         AG_TITLE = 'MTRA'                                                157
         AG_EXNAME = 'POSITION'                                           157
         AG_PARLIST = 'X_Z'                                               157
         CALL AXPOSITION                                                  157
C    *                                                                    157
C    Geant POSITION : MTRA ===> X=-MTDD_MTDTRAYDX Z=-87 <===              157
         CALL AGSRESET                                                    158
         AG_X = -MTDD_MTDTRAYDX                                           158
         AG_Z = -87                                                       158
         AG_TITLE = 'MTRA'                                                158
         AG_EXNAME = 'POSITION'                                           158
         AG_PARLIST = 'X_Z'                                               158
         CALL AXPOSITION                                                  158
C    *                                                                    158
C    Geant POSITION : MTRA ===> X=MTDD_MTDTRAYDX Z=0 <===                 158
         CALL AGSRESET                                                    159
         AG_X = MTDD_MTDTRAYDX                                            159
         AG_Z = 0                                                         159
         AG_TITLE = 'MTRA'                                                159
         AG_EXNAME = 'POSITION'                                           159
         AG_PARLIST = 'X_Z'                                               159
         CALL AXPOSITION                                                  159
C    *                                                                    159
C    Geant POSITION : MTRA ===> X=-MTDD_MTDTRAYDX Z=87 <===               159
         CALL AGSRESET                                                    160
         AG_X = -MTDD_MTDTRAYDX                                           160
         AG_Z = 87                                                        160
         AG_TITLE = 'MTRA'                                                160
         AG_EXNAME = 'POSITION'                                           160
         AG_PARLIST = 'X_Z'                                               160
         CALL AXPOSITION                                                  160
C    *                                                                    160
C    Geant POSITION : MTRA ===> X=MTDD_MTDTRAYDX Z=174 <===               160
         CALL AGSRESET                                                    161
         AG_X = MTDD_MTDTRAYDX                                            161
         AG_Z = 174                                                       161
         AG_TITLE = 'MTRA'                                                161
         AG_EXNAME = 'POSITION'                                           161
         AG_PARLIST = 'X_Z'                                               161
         CALL AXPOSITION                                                  161
C    *                                                                    161
5171  CALL AGSPOPB                                                        162
      GO TO BLOCK_MTTF, (5170 ,5180)                                      162
C End of Local Procedure                                                  162
C ----------------------------------------------------------------------  163
C ------------ MTTT                                                       163
C Begin Local Procedure MTTT                                              164
C MTTT IS THE MTD11 3-TRAY GROUP MOTHER                                   164
5200  CONTINUE                                                            164
         AG_TITLE='MTTT'                                                  164
         AG_MODULE=MODULE                                                 164
         AG_MARK='V00'                                                    164
         CALL AGSPUSH                                                     164
         AG_MODULE=MODULE                                                 164
C    *                                                                    164
C    Geant MATERIAL : AIR ===> inherited arguments used <===              165
         CALL AGSRESET                                                    165
         AG_TITLE = 'AIR'                                                 165
         AG_EXNAME = 'MATERIAL'                                           165
         AG_PARLIST = 'NONE'                                              165
         CALL AXMATERIAL                                                  165
C    *                                                                    165
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           165
         CALL AGSRESET                                                    166
         AG_TITLE = 'STANDARD'                                            166
         AG_EXNAME = 'MEDIUM'                                             166
         AG_PARLIST = 'NONE'                                              166
         CALL AXMEDIUM                                                    166
C    *                                                                    166
C    Geant ATTRIBUTE : MTTT ===> SEEN=1 COLO=3 <===                       166
         CALL AGSRESET                                                    167
         AG_SEEN = 1                                                      167
         AG_COLO = 3                                                      167
         AG_TITLE = 'MTTT'                                                167
         AG_EXNAME = 'ATTRIBUTE'                                          167
         AG_PARLIST = 'SEEN_COLO'                                         167
         CALL AXATTRIBUTE                                                 167
C    *                                                                    167
C    Geant SHAPE : BOX ===> DX=MTDD_MTDMOTHERDX DY=MTDD_MTDMOTHERDY <===  167
         CALL AGSRESET                                                    168
         AG_DX = MTDD_MTDMOTHERDX                                         168
         AG_DY = MTDD_MTDMOTHERDY                                         168
         AG_TITLE = 'BOX'                                                 168
         AG_EXNAME = 'SHAPE'                                              168
         AG_PARLIST = 'DX_DY'                                             168
         CALL AXSHAPE                                                     168
C    *                                                                    168
C    Create MTRA :                                                        169
C    Call To Local Procedure MTRA                                         169
         ASSIGN 5240 TO BLOCK_MTRA                                        169
         GO TO  5220                                                      169
5240     CONTINUE                                                         169
C    *                                                                    169
C    Geant POSITION : MTRA ===> X=-MTDD_MTDTRAYDX Z=-87 <===              170
         CALL AGSRESET                                                    170
         AG_X = -MTDD_MTDTRAYDX                                           170
         AG_Z = -87                                                       170
         AG_TITLE = 'MTRA'                                                170
         AG_EXNAME = 'POSITION'                                           170
         AG_PARLIST = 'X_Z'                                               170
         CALL AXPOSITION                                                  170
C    *                                                                    170
C    Create MTRA :                                                        171
C    Call To Local Procedure MTRA                                         171
         ASSIGN 5250 TO BLOCK_MTRA                                        171
         GO TO  5220                                                      171
5250     CONTINUE                                                         171
C    *                                                                    171
C    Geant POSITION : MTRA ===> X=MTDD_MTDTRAYDX Z=0 <===                 172
         CALL AGSRESET                                                    172
         AG_X = MTDD_MTDTRAYDX                                            172
         AG_Z = 0                                                         172
         AG_TITLE = 'MTRA'                                                172
         AG_EXNAME = 'POSITION'                                           172
         AG_PARLIST = 'X_Z'                                               172
         CALL AXPOSITION                                                  172
C    *                                                                    172
C    Create MTRA :                                                        173
C    Call To Local Procedure MTRA                                         173
         ASSIGN 5260 TO BLOCK_MTRA                                        173
         GO TO  5220                                                      173
5260     CONTINUE                                                         173
C    *                                                                    173
C    Geant POSITION : MTRA ===> X=-MTDD_MTDTRAYDX Z=87 <===               174
         CALL AGSRESET                                                    174
         AG_X = -MTDD_MTDTRAYDX                                           174
         AG_Z = 87                                                        174
         AG_TITLE = 'MTRA'                                                174
         AG_EXNAME = 'POSITION'                                           174
         AG_PARLIST = 'X_Z'                                               174
         CALL AXPOSITION                                                  174
C    *                                                                    174
5201  CALL AGSPOPB                                                        175
      GO TO BLOCK_MTTT, (5200 ,5210)                                      175
C End of Local Procedure                                                  175
C ----------------------------------------------------------------------  176
C ------------ MTRA                                                       176
C Begin Local Procedure MTRA                                              177
C MTRA IS AN MTD11-STYLE TRAY                                             177
5220  CONTINUE                                                            177
         AG_TITLE='MTRA'                                                  177
         AG_MODULE=MODULE                                                 177
         AG_MARK='V00'                                                    177
         CALL AGSPUSH                                                     177
         AG_MODULE=MODULE                                                 177
C    *                                                                    177
C    Geant MATERIAL : RPCGAS ===> inherited arguments used <===           178
         CALL AGSRESET                                                    178
         AG_TITLE = 'RPCGAS'                                              178
         AG_EXNAME = 'MATERIAL'                                           178
         AG_PARLIST = 'NONE'                                              178
         CALL AXMATERIAL                                                  178
C    *                                                                    178
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           178
         CALL AGSRESET                                                    179
         AG_TITLE = 'STANDARD'                                            179
         AG_EXNAME = 'MEDIUM'                                             179
         AG_PARLIST = 'NONE'                                              179
         CALL AXMEDIUM                                                    179
C    *                                                                    179
C    Geant ATTRIBUTE : MTRA ===> SEEN=1 COLO=2 <===                       179
         CALL AGSRESET                                                    180
         AG_SEEN = 1                                                      180
         AG_COLO = 2                                                      180
         AG_TITLE = 'MTRA'                                                180
         AG_EXNAME = 'ATTRIBUTE'                                          180
         AG_PARLIST = 'SEEN_COLO'                                         180
         CALL AXATTRIBUTE                                                 180
C    *                                                                    180
C    Geant SHAPE : BOX ===> DX=MTDD_MTDTRAYDX DY=MTDD_MTDTRAYDY, DZ=MTDD  180
C    _MTDTRAYDZ <===                                                      180
         CALL AGSRESET                                                    182
         AG_DX = MTDD_MTDTRAYDX                                           182
         AG_DY = MTDD_MTDTRAYDY                                           182
         AG_DZ = MTDD_MTDTRAYDZ                                           182
         AG_TITLE = 'BOX'                                                 182
         AG_EXNAME = 'SHAPE'                                              182
         AG_PARLIST = 'DX_DY_DZ'                                          182
         CALL AXSHAPE                                                     182
C    *                                                                    182
         AG_DX = MTDD_MTDBPLATE/2.                                        182
C    Create MTBP :                                                        183
C    Call To Local Procedure MTBP                                         183
         ASSIGN 5280 TO BLOCK_MTBP                                        183
         GO TO  5270                                                      183
5280     CONTINUE                                                         183
C    *                                                                    183
C    Geant POSITION : MTBP ===> X=-MTDD_MTDTRAYDX+(MTDD_MTDBPLATE/2.) <=  184
C    ==                                                                   184
         CALL AGSRESET                                                    184
         AG_X = -MTDD_MTDTRAYDX+(MTDD_MTDBPLATE/2.)                       184
         AG_TITLE = 'MTBP'                                                184
         AG_EXNAME = 'POSITION'                                           184
         AG_PARLIST = 'X'                                                 184
         CALL AXPOSITION                                                  184
C    *                                                                    184
         AG_DX = MTDD_MTDTPLATE/2.                                        184
C    Create MTTP :                                                        185
C    Call To Local Procedure MTTP                                         185
         ASSIGN 5300 TO BLOCK_MTTP                                        185
         GO TO  5290                                                      185
5300     CONTINUE                                                         185
C    *                                                                    185
C    Geant POSITION : MTTP ===> X=MTDD_MTDTRAYDX-(MTDD_MTDTPLATE/2.) <==  186
C    =                                                                    186
         CALL AGSRESET                                                    186
         AG_X = MTDD_MTDTRAYDX-(MTDD_MTDTPLATE/2.)                        186
         AG_TITLE = 'MTTP'                                                186
         AG_EXNAME = 'POSITION'                                           186
         AG_PARLIST = 'X'                                                 186
         CALL AXPOSITION                                                  186
C    *                                                                    186
         CHANDIM = MTDD_MTDTRAYDX - MTDD_MTDBPLATE/2. - MTDD_MTDBPLATE/   186
     *   2.                                                               186
         AG_DZ = MTDD_MTDTRAYDZ-2.*CHANDIM                                188
         AG_DX = CHANDIM                                                  188
         AG_DY = CHANDIM                                                  188
C    Create MCHL :                                                        188
C    Call To Local Procedure MCHL                                         188
         ASSIGN 5320 TO BLOCK_MCHL                                        188
         GO TO  5310                                                      188
5320     CONTINUE                                                         188
C    *                                                                    188
C    Geant POSITION : MCHL ===> X=-MTDD_MTDTRAYDX+MTDD_MTDBPLATE+CHANDIM  190
C     Y=MTDD_MTDTRAYDY-CHANDIM Z=0 <===                                   190
         CALL AGSRESET                                                    190
         AG_X = -MTDD_MTDTRAYDX+MTDD_MTDBPLATE+CHANDIM                    190
         AG_Y = MTDD_MTDTRAYDY-CHANDIM                                    190
         AG_Z = 0                                                         190
         AG_TITLE = 'MCHL'                                                190
         AG_EXNAME = 'POSITION'                                           190
         AG_PARLIST = 'X_Y_Z'                                             190
         CALL AXPOSITION                                                  190
C    *                                                                    190
C    Geant POSITION : MCHL ===> X=-MTDD_MTDTRAYDX+MTDD_MTDBPLATE+CHANDIM  190
C     Y=-MTDD_MTDTRAYDY+CHANDIM Z=0 ALPHAZ=180 <===                       190
         CALL AGSRESET                                                    192
         AG_X = -MTDD_MTDTRAYDX+MTDD_MTDBPLATE+CHANDIM                    192
         AG_Y = -MTDD_MTDTRAYDY+CHANDIM                                   192
         AG_Z = 0                                                         192
         AG_ALPHAN=AG_ALPHAN+1                                            192
         AG_ALPHAK(AG_ALPHAN)=3                                           192
         AG_ALPHAL(AG_ALPHAN)=180                                         192
         AG_TITLE = 'MCHL'                                                192
         AG_EXNAME = 'POSITION'                                           192
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      192
         CALL AXPOSITION                                                  192
C    *                                                                    192
         AG_DZ = CHANDIM                                                  192
         AG_DX = CHANDIM                                                  193
         AG_DY = MTDD_MTDTRAYDY                                           193
C    Create MCHS :                                                        193
C    Call To Local Procedure MCHS                                         193
         ASSIGN 5340 TO BLOCK_MCHS                                        193
         GO TO  5330                                                      193
5340     CONTINUE                                                         193
C    *                                                                    193
C    Geant POSITION : MCHS ===> X=-MTDD_MTDTRAYDX+MTDD_MTDBPLATE+CHANDIM  195
C     Y=0 Z=MTDD_MTDTRAYDZ-CHANDIM <===                                   195
         CALL AGSRESET                                                    195
         AG_X = -MTDD_MTDTRAYDX+MTDD_MTDBPLATE+CHANDIM                    195
         AG_Y = 0                                                         195
         AG_Z = MTDD_MTDTRAYDZ-CHANDIM                                    195
         AG_TITLE = 'MCHS'                                                195
         AG_EXNAME = 'POSITION'                                           195
         AG_PARLIST = 'X_Y_Z'                                             195
         CALL AXPOSITION                                                  195
C    *                                                                    195
C    Geant POSITION : MCHS ===> X=-MTDD_MTDTRAYDX+MTDD_MTDBPLATE+CHANDIM  195
C     Y=0 Z=-MTDD_MTDTRAYDZ+CHANDIM ALPHAY=180 <===                       195
         CALL AGSRESET                                                    197
         AG_X = -MTDD_MTDTRAYDX+MTDD_MTDBPLATE+CHANDIM                    197
         AG_Y = 0                                                         197
         AG_Z = -MTDD_MTDTRAYDZ+CHANDIM                                   197
         AG_ALPHAN=AG_ALPHAN+1                                            197
         AG_ALPHAK(AG_ALPHAN)=2                                           197
         AG_ALPHAL(AG_ALPHAN)=180                                         197
         AG_TITLE = 'MCHS'                                                197
         AG_EXNAME = 'POSITION'                                           197
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      197
         CALL AXPOSITION                                                  197
C    *                                                                    197
         THISH = MTDD_MTDIGSTACKNGAPS*MTDD_MTDIGSTACKGAPTHICK +           197
     *   (MTDD_MTDIGSTACKNGAPS - 1.0)*MTDD_MTDIGSTACKTHINGLASSTHICK       197
         AG_DZ = MTDD_MTDIGSTACKDZ                                        200
         AG_DX = THISH/2.0                                                200
         AG_DY = MTDD_MTDIGSTACKDY                                        200
C    Create MIGS :                                                        200
C    Call To Local Procedure MIGS                                         200
         ASSIGN 5360 TO BLOCK_MIGS                                        200
         GO TO  5350                                                      200
5360     CONTINUE                                                         200
C    *                                                                    200
C    Geant POSITION : MIGS ===> X=0 Y=0 Z=0 <===                          201
         CALL AGSRESET                                                    201
         AG_X = 0                                                         201
         AG_Y = 0                                                         201
         AG_Z = 0                                                         201
         AG_TITLE = 'MIGS'                                                201
         AG_EXNAME = 'POSITION'                                           201
         AG_PARLIST = 'X_Y_Z'                                             201
         CALL AXPOSITION                                                  201
C    *                                                                    201
5221  CALL AGSPOPB                                                        202
      GO TO BLOCK_MTRA, (5220 ,5230,5240,5250,5260)                       202
C End of Local Procedure                                                  202
C ----------------------------------------------------------------------  203
C ------------ MIGS                                                       203
C Begin Local Procedure MIGS                                              204
C MIGS IS THE INNER GLASS STACK                                           204
5350  CONTINUE                                                            204
         AG_TITLE='MIGS'                                                  204
         AG_MODULE=MODULE                                                 204
         AG_MARK='V00'                                                    204
         CALL AGSPUSH                                                     204
         AG_MODULE=MODULE                                                 204
C    *                                                                    204
C    Geant ATTRIBUTE : MIGS ===> SEEN=1 COLO=4 <===                       205
         CALL AGSRESET                                                    205
         AG_SEEN = 1                                                      205
         AG_COLO = 4                                                      205
         AG_TITLE = 'MIGS'                                                205
         AG_EXNAME = 'ATTRIBUTE'                                          205
         AG_PARLIST = 'SEEN_COLO'                                         205
         CALL AXATTRIBUTE                                                 205
C    *                                                                    205
C    Geant MATERIAL : GLASS ===> inherited arguments used <===            205
         CALL AGSRESET                                                    206
         AG_TITLE = 'GLASS'                                               206
         AG_EXNAME = 'MATERIAL'                                           206
         AG_PARLIST = 'NONE'                                              206
         CALL AXMATERIAL                                                  206
C    *                                                                    206
C    Geant SHAPE : BOX ===> inherited arguments used <===                 206
         CALL AGSRESET                                                    207
         AG_TITLE = 'BOX'                                                 207
         AG_EXNAME = 'SHAPE'                                              207
         AG_PARLIST = 'NONE'                                              207
         CALL AXSHAPE                                                     207
C    *                                                                    207
         AG_DZ = MTDD_MTDIGSTACKDZ                                        207
         AG_DX = MTDD_MTDIGSTACKGAPTHICK/2.0                              208
         AG_DY = MTDD_MTDIGSTACKDY                                        208
C    Create MIGG :                                                        208
C    Call To Local Procedure MIGG                                         208
         ASSIGN 5380 TO BLOCK_MIGG                                        208
         GO TO  5370                                                      208
5380     CONTINUE                                                         208
C    *                                                                    208
C    Check NGAPS.EQ.6                                                     209
         IF (NGAPS.EQ.6) THEN                                             209
C    Geant POSITION : MIGG ===> X=-2.5*MTDD_MTDIGSTACKTHINGLASSTHICK-2.5  210
C    *MTDD_MTDIGSTACKGAPTHICK <===                                        210
         CALL AGSRESET                                                    210
         AG_X =                                                           210
     *   -2.5*MTDD_MTDIGSTACKTHINGLASSTHICK-2.5*MTDD_MTDIGSTACKGAPTHICK   210
         AG_TITLE = 'MIGG'                                                210
         AG_EXNAME = 'POSITION'                                           210
         AG_PARLIST = 'X'                                                 210
         CALL AXPOSITION                                                  210
C    *                                                                    210
C    Geant POSITION : MIGG ===> X=-1.5*MTDD_MTDIGSTACKTHINGLASSTHICK-1.5  210
C    *MTDD_MTDIGSTACKGAPTHICK <===                                        210
         CALL AGSRESET                                                    211
         AG_X =                                                           211
     *   -1.5*MTDD_MTDIGSTACKTHINGLASSTHICK-1.5*MTDD_MTDIGSTACKGAPTHICK   211
         AG_TITLE = 'MIGG'                                                211
         AG_EXNAME = 'POSITION'                                           211
         AG_PARLIST = 'X'                                                 211
         CALL AXPOSITION                                                  211
C    *                                                                    211
C    Geant POSITION : MIGG ===> X=-0.5*MTDD_MTDIGSTACKTHINGLASSTHICK-0.5  211
C    *MTDD_MTDIGSTACKGAPTHICK <===                                        211
         CALL AGSRESET                                                    212
         AG_X =                                                           212
     *   -0.5*MTDD_MTDIGSTACKTHINGLASSTHICK-0.5*MTDD_MTDIGSTACKGAPTHICK   212
         AG_TITLE = 'MIGG'                                                212
         AG_EXNAME = 'POSITION'                                           212
         AG_PARLIST = 'X'                                                 212
         CALL AXPOSITION                                                  212
C    *                                                                    212
C    Geant POSITION : MIGG ===> X=0.5*MTDD_MTDIGSTACKTHINGLASSTHICK+0.5*  212
C    MTDD_MTDIGSTACKGAPTHICK <===                                         212
         CALL AGSRESET                                                    213
         AG_X =                                                           213
     *   0.5*MTDD_MTDIGSTACKTHINGLASSTHICK+0.5*MTDD_MTDIGSTACKGAPTHICK    213
         AG_TITLE = 'MIGG'                                                213
         AG_EXNAME = 'POSITION'                                           213
         AG_PARLIST = 'X'                                                 213
         CALL AXPOSITION                                                  213
C    *                                                                    213
C    Geant POSITION : MIGG ===> X=1.5*MTDD_MTDIGSTACKTHINGLASSTHICK+1.5*  213
C    MTDD_MTDIGSTACKGAPTHICK <===                                         213
         CALL AGSRESET                                                    214
         AG_X =                                                           214
     *   1.5*MTDD_MTDIGSTACKTHINGLASSTHICK+1.5*MTDD_MTDIGSTACKGAPTHICK    214
         AG_TITLE = 'MIGG'                                                214
         AG_EXNAME = 'POSITION'                                           214
         AG_PARLIST = 'X'                                                 214
         CALL AXPOSITION                                                  214
C    *                                                                    214
C    Geant POSITION : MIGG ===> X=2.5*MTDD_MTDIGSTACKTHINGLASSTHICK+2.5*  214
C    MTDD_MTDIGSTACKGAPTHICK <===                                         214
         CALL AGSRESET                                                    215
         AG_X =                                                           215
     *   2.5*MTDD_MTDIGSTACKTHINGLASSTHICK+2.5*MTDD_MTDIGSTACKGAPTHICK    215
         AG_TITLE = 'MIGG'                                                215
         AG_EXNAME = 'POSITION'                                           215
         AG_PARLIST = 'X'                                                 215
         CALL AXPOSITION                                                  215
C    *                                                                    215
         ELSEIF (NGAPS.EQ.5) THEN                                         215
C    Geant POSITION : MIGG ===> X=-2.0*MTDD_MTDIGSTACKTHINGLASSTHICK-2.0  217
C    *MTDD_MTDIGSTACKGAPTHICK <===                                        217
         CALL AGSRESET                                                    217
         AG_X =                                                           217
     *   -2.0*MTDD_MTDIGSTACKTHINGLASSTHICK-2.0*MTDD_MTDIGSTACKGAPTHICK   217
         AG_TITLE = 'MIGG'                                                217
         AG_EXNAME = 'POSITION'                                           217
         AG_PARLIST = 'X'                                                 217
         CALL AXPOSITION                                                  217
C    *                                                                    217
C    Geant POSITION : MIGG ===> X=-1.0*MTDD_MTDIGSTACKTHINGLASSTHICK-1.0  217
C    *MTDD_MTDIGSTACKGAPTHICK <===                                        217
         CALL AGSRESET                                                    218
         AG_X =                                                           218
     *   -1.0*MTDD_MTDIGSTACKTHINGLASSTHICK-1.0*MTDD_MTDIGSTACKGAPTHICK   218
         AG_TITLE = 'MIGG'                                                218
         AG_EXNAME = 'POSITION'                                           218
         AG_PARLIST = 'X'                                                 218
         CALL AXPOSITION                                                  218
C    *                                                                    218
C    Geant POSITION : MIGG ===> X=0.0*MTDD_MTDIGSTACKTHINGLASSTHICK+0.0*  218
C    MTDD_MTDIGSTACKGAPTHICK <===                                         218
         CALL AGSRESET                                                    219
         AG_X =                                                           219
     *   0.0*MTDD_MTDIGSTACKTHINGLASSTHICK+0.0*MTDD_MTDIGSTACKGAPTHICK    219
         AG_TITLE = 'MIGG'                                                219
         AG_EXNAME = 'POSITION'                                           219
         AG_PARLIST = 'X'                                                 219
         CALL AXPOSITION                                                  219
C    *                                                                    219
C    Geant POSITION : MIGG ===> X=1.0*MTDD_MTDIGSTACKTHINGLASSTHICK+1.0*  219
C    MTDD_MTDIGSTACKGAPTHICK <===                                         219
         CALL AGSRESET                                                    220
         AG_X =                                                           220
     *   1.0*MTDD_MTDIGSTACKTHINGLASSTHICK+1.0*MTDD_MTDIGSTACKGAPTHICK    220
         AG_TITLE = 'MIGG'                                                220
         AG_EXNAME = 'POSITION'                                           220
         AG_PARLIST = 'X'                                                 220
         CALL AXPOSITION                                                  220
C    *                                                                    220
C    Geant POSITION : MIGG ===> X=2.0*MTDD_MTDIGSTACKTHINGLASSTHICK+2.0*  220
C    MTDD_MTDIGSTACKGAPTHICK <===                                         220
         CALL AGSRESET                                                    221
         AG_X =                                                           221
     *   2.0*MTDD_MTDIGSTACKTHINGLASSTHICK+2.0*MTDD_MTDIGSTACKGAPTHICK    221
         AG_TITLE = 'MIGG'                                                221
         AG_EXNAME = 'POSITION'                                           221
         AG_PARLIST = 'X'                                                 221
         CALL AXPOSITION                                                  221
C    *                                                                    221
         ENDIF                                                            221
5351  CALL AGSPOPB                                                        223
      GO TO BLOCK_MIGS, (5350 ,5360)                                      223
C End of Local Procedure                                                  223
C ----------------------------------------------------------------------  224
C ------------ MIGG                                                       224
C Begin Local Procedure MIGG                                              225
C MIGG IS A SINGLE GAS GAP                                                225
5370  CONTINUE                                                            225
         AG_TITLE='MIGG'                                                  225
         AG_MODULE=MODULE                                                 225
         AG_MARK='V00'                                                    225
         CALL AGSPUSH                                                     225
         AG_MODULE=MODULE                                                 225
C    *                                                                    225
C    Geant ATTRIBUTE : MIGG ===> SEEN=1 COLO=7 <===                       226
         CALL AGSRESET                                                    226
         AG_SEEN = 1                                                      226
         AG_COLO = 7                                                      226
         AG_TITLE = 'MIGG'                                                226
         AG_EXNAME = 'ATTRIBUTE'                                          226
         AG_PARLIST = 'SEEN_COLO'                                         226
         CALL AXATTRIBUTE                                                 226
C    *                                                                    226
C    Geant MATERIAL : RPCGAS ===> inherited arguments used <===           226
         CALL AGSRESET                                                    227
         AG_TITLE = 'RPCGAS'                                              227
         AG_EXNAME = 'MATERIAL'                                           227
         AG_PARLIST = 'NONE'                                              227
         CALL AXMATERIAL                                                  227
C    *                                                                    227
C    Geant MEDIUM : RPCGAS_SENSITIVE ===> ISVOL=1 <===                    227
         CALL AGSRESET                                                    228
         AG_ISVOL = 1                                                     228
         AG_TITLE = 'RPCGAS_SENSITIVE'                                    228
         AG_EXNAME = 'MEDIUM'                                             228
         AG_PARLIST = 'ISVOL'                                             228
         CALL AXMEDIUM                                                    228
C    *                                                                    228
C    Geant SHAPE : BOX ===> inherited arguments used <===                 228
         CALL AGSRESET                                                    229
         AG_TITLE = 'BOX'                                                 229
         AG_EXNAME = 'SHAPE'                                              229
         AG_PARLIST = 'NONE'                                              229
         CALL AXSHAPE                                                     229
C    *                                                                    229
C    Defining Hits: X:.01:S Y:.01: Z:.01: PTOT:18:(0,100) CX:10: CY:10:   229
C    CZ:10: SLENG:.1:(0,500), TOF:16:(0,1.0E-7) STEP:.01: ELOSS:16:(0,1.  229
C    0E-6)                                                                229
         AG_TITLE = 'MIGG'                                                231
         AG_EXNAME = 'HITS'                                               231
         AG_TYPE=AG_MAGIC                                                 231
         CALL AGSDETV('MIGG')                                             231
C    *                                                                    231
         AG_PARLIST='X'                                                   231
         AG_TITLE = 'DETH'                                                231
         AG_NBITS = AG_MAGIC                                              231
         AG_BIN=.01                                                       231
         AG_OPTION='S'                                                    231
         AG_FHMIN = AG_MAGIC                                              231
         AG_FHMAX = AG_MAGIC                                              231
         CALL AGSDETH (0)                                                 231
C    *                                                                    231
         AG_PARLIST='Y'                                                   231
         AG_TITLE = 'DETH'                                                231
         AG_NBITS = AG_MAGIC                                              231
         AG_BIN=.01                                                       231
         AG_FHMIN=AG_MAGIC                                                231
         AG_FHMAX=AG_MAGIC                                                231
         AG_OPTION='*'                                                    231
         CALL AGSDETH (0)                                                 231
C    *                                                                    231
         AG_PARLIST='Z'                                                   231
         AG_TITLE = 'DETH'                                                231
         AG_NBITS = AG_MAGIC                                              231
         AG_BIN=.01                                                       231
         AG_FHMIN=AG_MAGIC                                                231
         AG_FHMAX=AG_MAGIC                                                231
         AG_OPTION='*'                                                    231
         CALL AGSDETH (0)                                                 231
C    *                                                                    231
         AG_PARLIST='PTOT'                                                231
         AG_TITLE = 'DETH'                                                231
         AG_NBITS = 18                                                    231
         AG_BIN=AG_MAGIC                                                  231
         AG_OPTION='*'                                                    231
         AG_FHMIN = 0                                                     231
         AG_FHMAX = 100                                                   231
         CALL AGSDETH (0)                                                 231
C    *                                                                    231
         AG_PARLIST='CX'                                                  231
         AG_TITLE = 'DETH'                                                231
         AG_NBITS = 10                                                    231
         AG_BIN=AG_MAGIC                                                  231
         AG_FHMIN=AG_MAGIC                                                231
         AG_FHMAX=AG_MAGIC                                                231
         AG_OPTION='*'                                                    231
         CALL AGSDETH (0)                                                 231
C    *                                                                    231
         AG_PARLIST='CY'                                                  231
         AG_TITLE = 'DETH'                                                231
         AG_NBITS = 10                                                    231
         AG_BIN=AG_MAGIC                                                  231
         AG_FHMIN=AG_MAGIC                                                231
         AG_FHMAX=AG_MAGIC                                                231
         AG_OPTION='*'                                                    231
         CALL AGSDETH (0)                                                 231
C    *                                                                    231
         AG_PARLIST='CZ'                                                  231
         AG_TITLE = 'DETH'                                                231
         AG_NBITS = 10                                                    231
         AG_BIN=AG_MAGIC                                                  231
         AG_FHMIN=AG_MAGIC                                                231
         AG_FHMAX=AG_MAGIC                                                231
         AG_OPTION='*'                                                    231
         CALL AGSDETH (0)                                                 231
C    *                                                                    231
         AG_PARLIST='SLENG'                                               231
         AG_TITLE = 'DETH'                                                231
         AG_NBITS = AG_MAGIC                                              231
         AG_BIN=.1                                                        231
         AG_OPTION='*'                                                    231
         AG_FHMIN = 0                                                     231
         AG_FHMAX = 500                                                   231
         CALL AGSDETH (0)                                                 231
C    *                                                                    231
         AG_PARLIST='TOF'                                                 231
         AG_TITLE = 'DETH'                                                231
         AG_NBITS = 16                                                    231
         AG_BIN=AG_MAGIC                                                  231
         AG_OPTION='*'                                                    231
         AG_FHMIN = 0                                                     231
         AG_FHMAX = 1.0E-7                                                231
         CALL AGSDETH (0)                                                 231
C    *                                                                    231
         AG_PARLIST='STEP'                                                231
         AG_TITLE = 'DETH'                                                231
         AG_NBITS = AG_MAGIC                                              231
         AG_BIN=.01                                                       231
         AG_FHMIN=AG_MAGIC                                                231
         AG_FHMAX=AG_MAGIC                                                231
         AG_OPTION='*'                                                    231
         CALL AGSDETH (0)                                                 231
C    *                                                                    231
         AG_PARLIST='ELOSS'                                               231
         AG_TITLE = 'DETH'                                                231
         AG_NBITS = 16                                                    231
         AG_BIN=AG_MAGIC                                                  231
         AG_OPTION='*'                                                    231
         AG_FHMIN = 0                                                     231
         AG_FHMAX = 1.0E-6                                                231
         CALL AGSDETH (0)                                                 231
C    *                                                                    231
         CALL AGSDETU('MIGG')                                             231
C    *                                                                    231
5371  CALL AGSPOPB                                                        232
      GO TO BLOCK_MIGG, (5370 ,5380)                                      232
C End of Local Procedure                                                  232
C ----------------------------------------------------------------------  233
C ------------ MTBP                                                       233
C Begin Local Procedure MTBP                                              234
C MTBP IS THE MTD11 BOTTOM PLATE                                          234
5270  CONTINUE                                                            234
         AG_TITLE='MTBP'                                                  234
         AG_MODULE=MODULE                                                 234
         AG_MARK='V00'                                                    234
         CALL AGSPUSH                                                     234
         AG_MODULE=MODULE                                                 234
C    *                                                                    234
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        235
         CALL AGSRESET                                                    235
         AG_TITLE = 'ALUMINIUM'                                           235
         AG_EXNAME = 'MATERIAL'                                           235
         AG_PARLIST = 'NONE'                                              235
         CALL AXMATERIAL                                                  235
C    *                                                                    235
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           235
         CALL AGSRESET                                                    236
         AG_TITLE = 'STANDARD'                                            236
         AG_EXNAME = 'MEDIUM'                                             236
         AG_PARLIST = 'NONE'                                              236
         CALL AXMEDIUM                                                    236
C    *                                                                    236
C    Geant ATTRIBUTE : MTBP ===> SEEN=1 COLO=1 <===                       236
         CALL AGSRESET                                                    237
         AG_SEEN = 1                                                      237
         AG_COLO = 1                                                      237
         AG_TITLE = 'MTBP'                                                237
         AG_EXNAME = 'ATTRIBUTE'                                          237
         AG_PARLIST = 'SEEN_COLO'                                         237
         CALL AXATTRIBUTE                                                 237
C    *                                                                    237
C    Geant SHAPE : BOX ===> inherited arguments used <===                 237
         CALL AGSRESET                                                    238
         AG_TITLE = 'BOX'                                                 238
         AG_EXNAME = 'SHAPE'                                              238
         AG_PARLIST = 'NONE'                                              238
         CALL AXSHAPE                                                     238
C    *                                                                    238
5271  CALL AGSPOPB                                                        239
      GO TO BLOCK_MTBP, (5270 ,5280)                                      239
C End of Local Procedure                                                  239
C ----------------------------------------------------------------------  240
C ------------ MTTP                                                       240
C Begin Local Procedure MTTP                                              241
C MTTP IS THE MTD11 TOP PLATE                                             241
5290  CONTINUE                                                            241
         AG_TITLE='MTTP'                                                  241
         AG_MODULE=MODULE                                                 241
         AG_MARK='V00'                                                    241
         CALL AGSPUSH                                                     241
         AG_MODULE=MODULE                                                 241
C    *                                                                    241
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        242
         CALL AGSRESET                                                    242
         AG_TITLE = 'ALUMINIUM'                                           242
         AG_EXNAME = 'MATERIAL'                                           242
         AG_PARLIST = 'NONE'                                              242
         CALL AXMATERIAL                                                  242
C    *                                                                    242
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           242
         CALL AGSRESET                                                    243
         AG_TITLE = 'STANDARD'                                            243
         AG_EXNAME = 'MEDIUM'                                             243
         AG_PARLIST = 'NONE'                                              243
         CALL AXMEDIUM                                                    243
C    *                                                                    243
C    Geant ATTRIBUTE : MTTP ===> SEEN=1 COLO=1 <===                       243
         CALL AGSRESET                                                    244
         AG_SEEN = 1                                                      244
         AG_COLO = 1                                                      244
         AG_TITLE = 'MTTP'                                                244
         AG_EXNAME = 'ATTRIBUTE'                                          244
         AG_PARLIST = 'SEEN_COLO'                                         244
         CALL AXATTRIBUTE                                                 244
C    *                                                                    244
C    Geant SHAPE : BOX ===> inherited arguments used <===                 244
         CALL AGSRESET                                                    245
         AG_TITLE = 'BOX'                                                 245
         AG_EXNAME = 'SHAPE'                                              245
         AG_PARLIST = 'NONE'                                              245
         CALL AXSHAPE                                                     245
C    *                                                                    245
5291  CALL AGSPOPB                                                        246
      GO TO BLOCK_MTTP, (5290 ,5300)                                      246
C End of Local Procedure                                                  246
C ----------------------------------------------------------------------  247
C ------------ MCHL                                                       247
C Begin Local Procedure MCHL                                              248
C MCHL IS THE MTD11 ARCHITECTURAL CHANNEL LONG SIDE                       248
5310  CONTINUE                                                            248
         AG_TITLE='MCHL'                                                  248
         AG_MODULE=MODULE                                                 248
         AG_MARK='V00'                                                    248
         CALL AGSPUSH                                                     248
         AG_MODULE=MODULE                                                 248
C    *                                                                    248
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        249
         CALL AGSRESET                                                    249
         AG_TITLE = 'ALUMINIUM'                                           249
         AG_EXNAME = 'MATERIAL'                                           249
         AG_PARLIST = 'NONE'                                              249
         CALL AXMATERIAL                                                  249
C    *                                                                    249
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           249
         CALL AGSRESET                                                    250
         AG_TITLE = 'STANDARD'                                            250
         AG_EXNAME = 'MEDIUM'                                             250
         AG_PARLIST = 'NONE'                                              250
         CALL AXMEDIUM                                                    250
C    *                                                                    250
C    Geant ATTRIBUTE : MCHL ===> SEEN=1 COLO=1 <===                       250
         CALL AGSRESET                                                    251
         AG_SEEN = 1                                                      251
         AG_COLO = 1                                                      251
         AG_TITLE = 'MCHL'                                                251
         AG_EXNAME = 'ATTRIBUTE'                                          251
         AG_PARLIST = 'SEEN_COLO'                                         251
         CALL AXATTRIBUTE                                                 251
C    *                                                                    251
C    Geant SHAPE : BOX ===> inherited arguments used <===                 251
         CALL AGSRESET                                                    252
         AG_TITLE = 'BOX'                                                 252
         AG_EXNAME = 'SHAPE'                                              252
         AG_PARLIST = 'NONE'                                              252
         CALL AXSHAPE                                                     252
C    *                                                                    252
         AG_DZ = MTDD_MTDTRAYDZ-2.*CHANDIM                                252
         AG_DX = CHANDIM-0.3175                                           253
         AG_DY = CHANDIM-0.3175/2.                                        253
C    Create MCAL :                                                        253
C    Call To Local Procedure MCAL                                         253
         ASSIGN 5400 TO BLOCK_MCAL                                        253
         GO TO  5390                                                      253
5400     CONTINUE                                                         253
C    *                                                                    253
C    Geant POSITION : MCAL ===> X=0 Y=0.3175/2. Z=0 <===                  254
         CALL AGSRESET                                                    254
         AG_X = 0                                                         254
         AG_Y = 0.3175/2.                                                 254
         AG_Z = 0                                                         254
         AG_TITLE = 'MCAL'                                                254
         AG_EXNAME = 'POSITION'                                           254
         AG_PARLIST = 'X_Y_Z'                                             254
         CALL AXPOSITION                                                  254
C    *                                                                    254
5311  CALL AGSPOPB                                                        255
      GO TO BLOCK_MCHL, (5310 ,5320)                                      255
C End of Local Procedure                                                  255
C ----------------------------------------------------------------------  256
C ------------ MCAL                                                       256
C Begin Local Procedure MCAL                                              257
C MCAL IS THE AIR IN THE MTD11 ARCHITECTURAL CHANNEL LONG SIDE            257
5390  CONTINUE                                                            257
         AG_TITLE='MCAL'                                                  257
         AG_MODULE=MODULE                                                 257
         AG_MARK='V00'                                                    257
         CALL AGSPUSH                                                     257
         AG_MODULE=MODULE                                                 257
C    *                                                                    257
C    Geant MATERIAL : AIR ===> inherited arguments used <===              258
         CALL AGSRESET                                                    258
         AG_TITLE = 'AIR'                                                 258
         AG_EXNAME = 'MATERIAL'                                           258
         AG_PARLIST = 'NONE'                                              258
         CALL AXMATERIAL                                                  258
C    *                                                                    258
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           258
         CALL AGSRESET                                                    259
         AG_TITLE = 'STANDARD'                                            259
         AG_EXNAME = 'MEDIUM'                                             259
         AG_PARLIST = 'NONE'                                              259
         CALL AXMEDIUM                                                    259
C    *                                                                    259
C    Geant ATTRIBUTE : MCAL ===> SEEN=1 COLO=7 <===                       259
         CALL AGSRESET                                                    260
         AG_SEEN = 1                                                      260
         AG_COLO = 7                                                      260
         AG_TITLE = 'MCAL'                                                260
         AG_EXNAME = 'ATTRIBUTE'                                          260
         AG_PARLIST = 'SEEN_COLO'                                         260
         CALL AXATTRIBUTE                                                 260
C    *                                                                    260
C    Geant SHAPE : BOX ===> inherited arguments used <===                 260
         CALL AGSRESET                                                    261
         AG_TITLE = 'BOX'                                                 261
         AG_EXNAME = 'SHAPE'                                              261
         AG_PARLIST = 'NONE'                                              261
         CALL AXSHAPE                                                     261
C    *                                                                    261
5391  CALL AGSPOPB                                                        262
      GO TO BLOCK_MCAL, (5390 ,5400)                                      262
C End of Local Procedure                                                  262
C ----------------------------------------------------------------------  263
C ------------ MCHS                                                       263
C Begin Local Procedure MCHS                                              264
C MCHS IS THE MTD11 ARCHITECTURAL CHANNEL SHORT SIDE                      264
5330  CONTINUE                                                            264
         AG_TITLE='MCHS'                                                  264
         AG_MODULE=MODULE                                                 264
         AG_MARK='V00'                                                    264
         CALL AGSPUSH                                                     264
         AG_MODULE=MODULE                                                 264
C    *                                                                    264
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        265
         CALL AGSRESET                                                    265
         AG_TITLE = 'ALUMINIUM'                                           265
         AG_EXNAME = 'MATERIAL'                                           265
         AG_PARLIST = 'NONE'                                              265
         CALL AXMATERIAL                                                  265
C    *                                                                    265
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           265
         CALL AGSRESET                                                    266
         AG_TITLE = 'STANDARD'                                            266
         AG_EXNAME = 'MEDIUM'                                             266
         AG_PARLIST = 'NONE'                                              266
         CALL AXMEDIUM                                                    266
C    *                                                                    266
C    Geant ATTRIBUTE : MCHS ===> SEEN=1 COLO=1 <===                       266
         CALL AGSRESET                                                    267
         AG_SEEN = 1                                                      267
         AG_COLO = 1                                                      267
         AG_TITLE = 'MCHS'                                                267
         AG_EXNAME = 'ATTRIBUTE'                                          267
         AG_PARLIST = 'SEEN_COLO'                                         267
         CALL AXATTRIBUTE                                                 267
C    *                                                                    267
C    Geant SHAPE : BOX ===> inherited arguments used <===                 267
         CALL AGSRESET                                                    268
         AG_TITLE = 'BOX'                                                 268
         AG_EXNAME = 'SHAPE'                                              268
         AG_PARLIST = 'NONE'                                              268
         CALL AXSHAPE                                                     268
C    *                                                                    268
         AG_DZ = CHANDIM-0.3175/2.                                        268
         AG_DX = CHANDIM-0.3175                                           269
         AG_DY = MTDD_MTDTRAYDY                                           269
C    Create MCAS :                                                        269
C    Call To Local Procedure MCAS                                         269
         ASSIGN 5420 TO BLOCK_MCAS                                        269
         GO TO  5410                                                      269
5420     CONTINUE                                                         269
C    *                                                                    269
C    Geant POSITION : MCAS ===> X=0 Y=0 Z=+0.3175/2. <===                 270
         CALL AGSRESET                                                    270
         AG_X = 0                                                         270
         AG_Y = 0                                                         270
         AG_Z = +0.3175/2.                                                270
         AG_TITLE = 'MCAS'                                                270
         AG_EXNAME = 'POSITION'                                           270
         AG_PARLIST = 'X_Y_Z'                                             270
         CALL AXPOSITION                                                  270
C    *                                                                    270
5331  CALL AGSPOPB                                                        271
      GO TO BLOCK_MCHS, (5330 ,5340)                                      271
C End of Local Procedure                                                  271
C ----------------------------------------------------------------------  272
C ------------ MCAS                                                       272
C Begin Local Procedure MCAS                                              273
C MCAS IS THE AIR IN THE MTD11 ARCHITECTURAL CHANNEL SHORT SIDE           273
5410  CONTINUE                                                            273
         AG_TITLE='MCAS'                                                  273
         AG_MODULE=MODULE                                                 273
         AG_MARK='V00'                                                    273
         CALL AGSPUSH                                                     273
         AG_MODULE=MODULE                                                 273
C    *                                                                    273
C    Geant MATERIAL : AIR ===> inherited arguments used <===              274
         CALL AGSRESET                                                    274
         AG_TITLE = 'AIR'                                                 274
         AG_EXNAME = 'MATERIAL'                                           274
         AG_PARLIST = 'NONE'                                              274
         CALL AXMATERIAL                                                  274
C    *                                                                    274
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           274
         CALL AGSRESET                                                    275
         AG_TITLE = 'STANDARD'                                            275
         AG_EXNAME = 'MEDIUM'                                             275
         AG_PARLIST = 'NONE'                                              275
         CALL AXMEDIUM                                                    275
C    *                                                                    275
C    Geant ATTRIBUTE : MCAS ===> SEEN=1 COLO=7 <===                       275
         CALL AGSRESET                                                    276
         AG_SEEN = 1                                                      276
         AG_COLO = 7                                                      276
         AG_TITLE = 'MCAS'                                                276
         AG_EXNAME = 'ATTRIBUTE'                                          276
         AG_PARLIST = 'SEEN_COLO'                                         276
         CALL AXATTRIBUTE                                                 276
C    *                                                                    276
C    Geant SHAPE : BOX ===> inherited arguments used <===                 276
         CALL AGSRESET                                                    277
         AG_TITLE = 'BOX'                                                 277
         AG_EXNAME = 'SHAPE'                                              277
         AG_PARLIST = 'NONE'                                              277
         CALL AXSHAPE                                                     277
C    *                                                                    277
5411  CALL AGSPOPB                                                        278
      GO TO BLOCK_MCAS, (5410 ,5420)                                      278
C End of Local Procedure                                                  278
      WRITE(LOUT,5430)'===>>  [End AgML Module MUTDGEO4]  <<==='          278
5430  FORMAT(A32,/,/)                                                     278
C MODULE MUTDGEO4                                                         280
      END                                                                 280
