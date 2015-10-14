C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE MUTDGEO5                                                 1
C MUTDGEO5 IS THE GEOMETRY OF THE STAR MTD                                1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'MUTDGEO5'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'IS THE GEOMETRY OF THE STAR MTD'/                    1
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
C CREATED 13 DEC 2013                                                     4
      CHARACTER*40 CREATED /'13 DEC 2013'/                                5
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  5
      INTEGER BLOCK_MUTD                                                  8
      INTEGER BLOCK_MTTG                                                  8
      INTEGER BLOCK_MTRA                                                  8
      INTEGER BLOCK_MTBP                                                  8
      INTEGER BLOCK_MTTP                                                  8
      INTEGER BLOCK_MCHL                                                  8
      INTEGER BLOCK_MCAL                                                  8
      INTEGER BLOCK_MCHS                                                  8
      INTEGER BLOCK_MCAS                                                  8
      INTEGER BLOCK_MIGS                                                  8
      INTEGER BLOCK_MIGG                                                  8
C -                                                                       9
      REAL MTDG_VERSION                                                   9
      EQUIVALENCE (BANK_MTDG(3),MTDG_VERSION,RE_MTDG_VERSION),            9
     *(NAMES_MTDG(1),NMTDG_VERSION)                                       9
      EQUIVALENCE (COMMEN_MTDG(1),CMTDG_VERSION),(MAP_MTDG(1,1),          9
     *MMTDG_VERSION)                                                      9
      CHARACTER CMTDG_VERSION*40/' '/                                     9
      CHARACTER NMTDG_VERSION*12/'R.VERSION'/                             9
      INTEGER MMTDG_VERSION(2)/1,1/                                       9
      REAL RE_MTDG_VERSION                                                9
C -                                                                       9
      REAL MTDG_CONFIG                                                    9
      EQUIVALENCE (BANK_MTDG(4),MTDG_CONFIG,RE_MTDG_CONFIG),              9
     *(NAMES_MTDG(2),NMTDG_CONFIG)                                        9
      EQUIVALENCE (COMMEN_MTDG(2),CMTDG_CONFIG),(MAP_MTDG(1,2),           9
     *MMTDG_CONFIG)                                                       9
      CHARACTER CMTDG_CONFIG*40/' '/                                      9
      CHARACTER NMTDG_CONFIG*12/'R.CONFIG'/                               9
      INTEGER MMTDG_CONFIG(2)/1,1/                                        9
      REAL RE_MTDG_CONFIG                                                 9
C -                                                                       9
      INTEGER LEN_MTDG,LLEN_MTDG,LINK_MTDG                                9
      PARAMETER (LEN_MTDG=4)                                              9
      PARAMETER (LLEN_MTDG=2)                                             9
      INTEGER FLAG_MTDG/0/,MAP_MTDG(2,LLEN_MTDG),IDNUM_MTDG(10)           9
      CHARACTER NAMES_MTDG(LLEN_MTDG)*12,COMMEN_MTDG(LLEN_MTDG)*40        9
      DATA IDNUM_MTDG/10*0/                                               9
      REAL BANK_MTDG(LEN_MTDG)                                            9
      CHARACTER*60 BPATH_MTDG/' '/                                        9
      SAVE FLAG_MTDG,MAP_MTDG,IDNUM_MTDG,NAMES_MTDG,COMMEN_MTDG,          9
     *BPATH_MTDG                                                          9
      DATA LINK_MTDG/0/,BANK_MTDG/LEN_MTDG*0/                             9
C *                                                                       9
C -                                                                       13
      REAL MTDD_RMIN                                                      13
      EQUIVALENCE (BANK_MTDD(3),MTDD_RMIN,RE_MTDD_RMIN),(NAMES_MTDD(1),   13
     *NMTDD_RMIN)                                                         13
      EQUIVALENCE (COMMEN_MTDD(1),CMTDD_RMIN),(MAP_MTDD(1,1),             13
     *MMTDD_RMIN)                                                         13
      CHARACTER CMTDD_RMIN*40/' '/                                        13
      CHARACTER NMTDD_RMIN*12/'R.RMIN'/                                   13
      INTEGER MMTDD_RMIN(2)/1,1/                                          13
      REAL RE_MTDD_RMIN                                                   13
C -                                                                       13
      REAL MTDD_RMAX                                                      13
      EQUIVALENCE (BANK_MTDD(4),MTDD_RMAX,RE_MTDD_RMAX),(NAMES_MTDD(2),   13
     *NMTDD_RMAX)                                                         13
      EQUIVALENCE (COMMEN_MTDD(2),CMTDD_RMAX),(MAP_MTDD(1,2),             13
     *MMTDD_RMAX)                                                         13
      CHARACTER CMTDD_RMAX*40/' '/                                        13
      CHARACTER NMTDD_RMAX*12/'R.RMAX'/                                   13
      INTEGER MMTDD_RMAX(2)/1,1/                                          13
      REAL RE_MTDD_RMAX                                                   13
C -                                                                       13
      REAL MTDD_RI(30)                                                    13
      EQUIVALENCE (BANK_MTDD(5),MTDD_RI,RE_MTDD_RI),(NAMES_MTDD(3),       13
     *NMTDD_RI)                                                           13
      EQUIVALENCE (COMMEN_MTDD(3),CMTDD_RI),(MAP_MTDD(1,3),MMTDD_RI)      13
      CHARACTER CMTDD_RI*40/' '/                                          13
      CHARACTER NMTDD_RI*12/'R.RI'/                                       13
      INTEGER MMTDD_RI(2)/30,1/                                           13
      REAL RE_MTDD_RI                                                     13
C -                                                                       13
      REAL MTDD_YSHIFT08                                                  13
      EQUIVALENCE (BANK_MTDD(35),MTDD_YSHIFT08,RE_MTDD_YSHIFT08),         13
     *(NAMES_MTDD(4),NMTDD_YSHIFT08)                                      13
      EQUIVALENCE (COMMEN_MTDD(4),CMTDD_YSHIFT08),(MAP_MTDD(1,4),         13
     *MMTDD_YSHIFT08)                                                     13
      CHARACTER CMTDD_YSHIFT08*40/' '/                                    13
      CHARACTER NMTDD_YSHIFT08*12/'R.YSHIFT08'/                           13
      INTEGER MMTDD_YSHIFT08(2)/1,1/                                      13
      REAL RE_MTDD_YSHIFT08                                               13
C -                                                                       13
      REAL MTDD_YSHIFT24                                                  13
      EQUIVALENCE (BANK_MTDD(36),MTDD_YSHIFT24,RE_MTDD_YSHIFT24),         13
     *(NAMES_MTDD(5),NMTDD_YSHIFT24)                                      13
      EQUIVALENCE (COMMEN_MTDD(5),CMTDD_YSHIFT24),(MAP_MTDD(1,5),         13
     *MMTDD_YSHIFT24)                                                     13
      CHARACTER CMTDD_YSHIFT24*40/' '/                                    13
      CHARACTER NMTDD_YSHIFT24*12/'R.YSHIFT24'/                           13
      INTEGER MMTDD_YSHIFT24(2)/1,1/                                      13
      REAL RE_MTDD_YSHIFT24                                               13
C -                                                                       13
      REAL MTDD_DZMOTHER                                                  13
      EQUIVALENCE (BANK_MTDD(37),MTDD_DZMOTHER,RE_MTDD_DZMOTHER),         13
     *(NAMES_MTDD(6),NMTDD_DZMOTHER)                                      13
      EQUIVALENCE (COMMEN_MTDD(6),CMTDD_DZMOTHER),(MAP_MTDD(1,6),         13
     *MMTDD_DZMOTHER)                                                     13
      CHARACTER CMTDD_DZMOTHER*40/' '/                                    13
      CHARACTER NMTDD_DZMOTHER*12/'R.DZMOTHER'/                           13
      INTEGER MMTDD_DZMOTHER(2)/1,1/                                      13
      REAL RE_MTDD_DZMOTHER                                               13
C -                                                                       13
      REAL MTDD_BACKLEGR                                                  13
      EQUIVALENCE (BANK_MTDD(38),MTDD_BACKLEGR,RE_MTDD_BACKLEGR),         13
     *(NAMES_MTDD(7),NMTDD_BACKLEGR)                                      13
      EQUIVALENCE (COMMEN_MTDD(7),CMTDD_BACKLEGR),(MAP_MTDD(1,7),         13
     *MMTDD_BACKLEGR)                                                     13
      CHARACTER CMTDD_BACKLEGR*40/' '/                                    13
      CHARACTER NMTDD_BACKLEGR*12/'R.BACKLEGR'/                           13
      INTEGER MMTDD_BACKLEGR(2)/1,1/                                      13
      REAL RE_MTDD_BACKLEGR                                               13
C -                                                                       13
      REAL MTDD_BEMCELECTBOXDX                                            13
      EQUIVALENCE (BANK_MTDD(39),MTDD_BEMCELECTBOXDX,                     13
     *RE_MTDD_BEMCELECTBOXDX),(NAMES_MTDD(8),NMTDD_BEMCELECTBOXDX)        13
      EQUIVALENCE (COMMEN_MTDD(8),CMTDD_BEMCELECTBOXDX),(MAP_MTDD(1,8),   13
     *MMTDD_BEMCELECTBOXDX)                                               13
      CHARACTER CMTDD_BEMCELECTBOXDX*40/' '/                              13
      CHARACTER NMTDD_BEMCELECTBOXDX*12/'R.BEMCELECTBOXDX'/               13
      INTEGER MMTDD_BEMCELECTBOXDX(2)/1,1/                                13
      REAL RE_MTDD_BEMCELECTBOXDX                                         13
C -                                                                       13
      REAL MTDD_BEMCELECTBOXDY                                            13
      EQUIVALENCE (BANK_MTDD(40),MTDD_BEMCELECTBOXDY,                     13
     *RE_MTDD_BEMCELECTBOXDY),(NAMES_MTDD(9),NMTDD_BEMCELECTBOXDY)        13
      EQUIVALENCE (COMMEN_MTDD(9),CMTDD_BEMCELECTBOXDY),(MAP_MTDD(1,9),   13
     *MMTDD_BEMCELECTBOXDY)                                               13
      CHARACTER CMTDD_BEMCELECTBOXDY*40/' '/                              13
      CHARACTER NMTDD_BEMCELECTBOXDY*12/'R.BEMCELECTBOXDY'/               13
      INTEGER MMTDD_BEMCELECTBOXDY(2)/1,1/                                13
      REAL RE_MTDD_BEMCELECTBOXDY                                         13
C -                                                                       13
      REAL MTDD_BEMCELECTBOXDZ3                                           13
      EQUIVALENCE (BANK_MTDD(41),MTDD_BEMCELECTBOXDZ3,                    13
     *RE_MTDD_BEMCELECTBOXDZ3),(NAMES_MTDD(10),NMTDD_BEMCELECTBOXDZ3)     13
      EQUIVALENCE (COMMEN_MTDD(10),CMTDD_BEMCELECTBOXDZ3),(MAP_MTDD(1,    13
     *10),MMTDD_BEMCELECTBOXDZ3)                                          13
      CHARACTER CMTDD_BEMCELECTBOXDZ3*40/' '/                             13
      CHARACTER NMTDD_BEMCELECTBOXDZ3*12/'R.BEMCELECTBOXDZ3'/             13
      INTEGER MMTDD_BEMCELECTBOXDZ3(2)/1,1/                               13
      REAL RE_MTDD_BEMCELECTBOXDZ3                                        13
C -                                                                       13
      REAL MTDD_BEMCELECTBOXDZ5                                           13
      EQUIVALENCE (BANK_MTDD(42),MTDD_BEMCELECTBOXDZ5,                    13
     *RE_MTDD_BEMCELECTBOXDZ5),(NAMES_MTDD(11),NMTDD_BEMCELECTBOXDZ5)     13
      EQUIVALENCE (COMMEN_MTDD(11),CMTDD_BEMCELECTBOXDZ5),(MAP_MTDD(1,    13
     *11),MMTDD_BEMCELECTBOXDZ5)                                          13
      CHARACTER CMTDD_BEMCELECTBOXDZ5*40/' '/                             13
      CHARACTER NMTDD_BEMCELECTBOXDZ5*12/'R.BEMCELECTBOXDZ5'/             13
      INTEGER MMTDD_BEMCELECTBOXDZ5(2)/1,1/                               13
      REAL RE_MTDD_BEMCELECTBOXDZ5                                        13
C -                                                                       13
      REAL MTDD_RGAP                                                      13
      EQUIVALENCE (BANK_MTDD(43),MTDD_RGAP,RE_MTDD_RGAP),                 13
     *(NAMES_MTDD(12),NMTDD_RGAP)                                         13
      EQUIVALENCE (COMMEN_MTDD(12),CMTDD_RGAP),(MAP_MTDD(1,12),           13
     *MMTDD_RGAP)                                                         13
      CHARACTER CMTDD_RGAP*40/' '/                                        13
      CHARACTER NMTDD_RGAP*12/'R.RGAP'/                                   13
      INTEGER MMTDD_RGAP(2)/1,1/                                          13
      REAL RE_MTDD_RGAP                                                   13
C -                                                                       13
      REAL MTDD_MTDMOTHERDX                                               13
      EQUIVALENCE (BANK_MTDD(44),MTDD_MTDMOTHERDX,RE_MTDD_MTDMOTHERDX),   13
     *(NAMES_MTDD(13),NMTDD_MTDMOTHERDX)                                  13
      EQUIVALENCE (COMMEN_MTDD(13),CMTDD_MTDMOTHERDX),(MAP_MTDD(1,13),    13
     *MMTDD_MTDMOTHERDX)                                                  13
      CHARACTER CMTDD_MTDMOTHERDX*40/' '/                                 13
      CHARACTER NMTDD_MTDMOTHERDX*12/'R.MTDMOTHERDX'/                     13
      INTEGER MMTDD_MTDMOTHERDX(2)/1,1/                                   13
      REAL RE_MTDD_MTDMOTHERDX                                            13
C -                                                                       13
      REAL MTDD_MTDMOTHERDY                                               13
      EQUIVALENCE (BANK_MTDD(45),MTDD_MTDMOTHERDY,RE_MTDD_MTDMOTHERDY),   13
     *(NAMES_MTDD(14),NMTDD_MTDMOTHERDY)                                  13
      EQUIVALENCE (COMMEN_MTDD(14),CMTDD_MTDMOTHERDY),(MAP_MTDD(1,14),    13
     *MMTDD_MTDMOTHERDY)                                                  13
      CHARACTER CMTDD_MTDMOTHERDY*40/' '/                                 13
      CHARACTER NMTDD_MTDMOTHERDY*12/'R.MTDMOTHERDY'/                     13
      INTEGER MMTDD_MTDMOTHERDY(2)/1,1/                                   13
      REAL RE_MTDD_MTDMOTHERDY                                            13
C -                                                                       13
      REAL MTDD_MTDMOTHERDZ3                                              13
      EQUIVALENCE (BANK_MTDD(46),MTDD_MTDMOTHERDZ3,                       13
     *RE_MTDD_MTDMOTHERDZ3),(NAMES_MTDD(15),NMTDD_MTDMOTHERDZ3)           13
      EQUIVALENCE (COMMEN_MTDD(15),CMTDD_MTDMOTHERDZ3),(MAP_MTDD(1,15),   13
     *MMTDD_MTDMOTHERDZ3)                                                 13
      CHARACTER CMTDD_MTDMOTHERDZ3*40/' '/                                13
      CHARACTER NMTDD_MTDMOTHERDZ3*12/'R.MTDMOTHERDZ3'/                   13
      INTEGER MMTDD_MTDMOTHERDZ3(2)/1,1/                                  13
      REAL RE_MTDD_MTDMOTHERDZ3                                           13
C -                                                                       13
      REAL MTDD_MTDMOTHERDZ5                                              13
      EQUIVALENCE (BANK_MTDD(47),MTDD_MTDMOTHERDZ5,                       13
     *RE_MTDD_MTDMOTHERDZ5),(NAMES_MTDD(16),NMTDD_MTDMOTHERDZ5)           13
      EQUIVALENCE (COMMEN_MTDD(16),CMTDD_MTDMOTHERDZ5),(MAP_MTDD(1,16),   13
     *MMTDD_MTDMOTHERDZ5)                                                 13
      CHARACTER CMTDD_MTDMOTHERDZ5*40/' '/                                13
      CHARACTER NMTDD_MTDMOTHERDZ5*12/'R.MTDMOTHERDZ5'/                   13
      INTEGER MMTDD_MTDMOTHERDZ5(2)/1,1/                                  13
      REAL RE_MTDD_MTDMOTHERDZ5                                           13
C -                                                                       13
      REAL MTDD_MTDTRAYDX                                                 13
      EQUIVALENCE (BANK_MTDD(48),MTDD_MTDTRAYDX,RE_MTDD_MTDTRAYDX),       13
     *(NAMES_MTDD(17),NMTDD_MTDTRAYDX)                                    13
      EQUIVALENCE (COMMEN_MTDD(17),CMTDD_MTDTRAYDX),(MAP_MTDD(1,17),      13
     *MMTDD_MTDTRAYDX)                                                    13
      CHARACTER CMTDD_MTDTRAYDX*40/' '/                                   13
      CHARACTER NMTDD_MTDTRAYDX*12/'R.MTDTRAYDX'/                         13
      INTEGER MMTDD_MTDTRAYDX(2)/1,1/                                     13
      REAL RE_MTDD_MTDTRAYDX                                              13
C -                                                                       13
      REAL MTDD_MTDTRAYDY                                                 13
      EQUIVALENCE (BANK_MTDD(49),MTDD_MTDTRAYDY,RE_MTDD_MTDTRAYDY),       13
     *(NAMES_MTDD(18),NMTDD_MTDTRAYDY)                                    13
      EQUIVALENCE (COMMEN_MTDD(18),CMTDD_MTDTRAYDY),(MAP_MTDD(1,18),      13
     *MMTDD_MTDTRAYDY)                                                    13
      CHARACTER CMTDD_MTDTRAYDY*40/' '/                                   13
      CHARACTER NMTDD_MTDTRAYDY*12/'R.MTDTRAYDY'/                         13
      INTEGER MMTDD_MTDTRAYDY(2)/1,1/                                     13
      REAL RE_MTDD_MTDTRAYDY                                              13
C -                                                                       13
      REAL MTDD_MTDTRAYDZ                                                 13
      EQUIVALENCE (BANK_MTDD(50),MTDD_MTDTRAYDZ,RE_MTDD_MTDTRAYDZ),       13
     *(NAMES_MTDD(19),NMTDD_MTDTRAYDZ)                                    13
      EQUIVALENCE (COMMEN_MTDD(19),CMTDD_MTDTRAYDZ),(MAP_MTDD(1,19),      13
     *MMTDD_MTDTRAYDZ)                                                    13
      CHARACTER CMTDD_MTDTRAYDZ*40/' '/                                   13
      CHARACTER NMTDD_MTDTRAYDZ*12/'R.MTDTRAYDZ'/                         13
      INTEGER MMTDD_MTDTRAYDZ(2)/1,1/                                     13
      REAL RE_MTDD_MTDTRAYDZ                                              13
C -                                                                       13
      REAL MTDD_MTDBPLATE                                                 13
      EQUIVALENCE (BANK_MTDD(51),MTDD_MTDBPLATE,RE_MTDD_MTDBPLATE),       13
     *(NAMES_MTDD(20),NMTDD_MTDBPLATE)                                    13
      EQUIVALENCE (COMMEN_MTDD(20),CMTDD_MTDBPLATE),(MAP_MTDD(1,20),      13
     *MMTDD_MTDBPLATE)                                                    13
      CHARACTER CMTDD_MTDBPLATE*40/' '/                                   13
      CHARACTER NMTDD_MTDBPLATE*12/'R.MTDBPLATE'/                         13
      INTEGER MMTDD_MTDBPLATE(2)/1,1/                                     13
      REAL RE_MTDD_MTDBPLATE                                              13
C -                                                                       13
      REAL MTDD_MTDTPLATE                                                 13
      EQUIVALENCE (BANK_MTDD(52),MTDD_MTDTPLATE,RE_MTDD_MTDTPLATE),       13
     *(NAMES_MTDD(21),NMTDD_MTDTPLATE)                                    13
      EQUIVALENCE (COMMEN_MTDD(21),CMTDD_MTDTPLATE),(MAP_MTDD(1,21),      13
     *MMTDD_MTDTPLATE)                                                    13
      CHARACTER CMTDD_MTDTPLATE*40/' '/                                   13
      CHARACTER NMTDD_MTDTPLATE*12/'R.MTDTPLATE'/                         13
      INTEGER MMTDD_MTDTPLATE(2)/1,1/                                     13
      REAL RE_MTDD_MTDTPLATE                                              13
C -                                                                       13
      REAL MTDD_MTDIGSTACKDY                                              13
      EQUIVALENCE (BANK_MTDD(53),MTDD_MTDIGSTACKDY,                       13
     *RE_MTDD_MTDIGSTACKDY),(NAMES_MTDD(22),NMTDD_MTDIGSTACKDY)           13
      EQUIVALENCE (COMMEN_MTDD(22),CMTDD_MTDIGSTACKDY),(MAP_MTDD(1,22),   13
     *MMTDD_MTDIGSTACKDY)                                                 13
      CHARACTER CMTDD_MTDIGSTACKDY*40/' '/                                13
      CHARACTER NMTDD_MTDIGSTACKDY*12/'R.MTDIGSTACKDY'/                   13
      INTEGER MMTDD_MTDIGSTACKDY(2)/1,1/                                  13
      REAL RE_MTDD_MTDIGSTACKDY                                           13
C -                                                                       13
      REAL MTDD_MTDIGSTACKDZ                                              13
      EQUIVALENCE (BANK_MTDD(54),MTDD_MTDIGSTACKDZ,                       13
     *RE_MTDD_MTDIGSTACKDZ),(NAMES_MTDD(23),NMTDD_MTDIGSTACKDZ)           13
      EQUIVALENCE (COMMEN_MTDD(23),CMTDD_MTDIGSTACKDZ),(MAP_MTDD(1,23),   13
     *MMTDD_MTDIGSTACKDZ)                                                 13
      CHARACTER CMTDD_MTDIGSTACKDZ*40/' '/                                13
      CHARACTER NMTDD_MTDIGSTACKDZ*12/'R.MTDIGSTACKDZ'/                   13
      INTEGER MMTDD_MTDIGSTACKDZ(2)/1,1/                                  13
      REAL RE_MTDD_MTDIGSTACKDZ                                           13
C -                                                                       13
      REAL MTDD_MTDIGSTACKNGAPS                                           13
      EQUIVALENCE (BANK_MTDD(55),MTDD_MTDIGSTACKNGAPS,                    13
     *RE_MTDD_MTDIGSTACKNGAPS),(NAMES_MTDD(24),NMTDD_MTDIGSTACKNGAPS)     13
      EQUIVALENCE (COMMEN_MTDD(24),CMTDD_MTDIGSTACKNGAPS),(MAP_MTDD(1,    13
     *24),MMTDD_MTDIGSTACKNGAPS)                                          13
      CHARACTER CMTDD_MTDIGSTACKNGAPS*40/' '/                             13
      CHARACTER NMTDD_MTDIGSTACKNGAPS*12/'R.MTDIGSTACKNGAPS'/             13
      INTEGER MMTDD_MTDIGSTACKNGAPS(2)/1,1/                               13
      REAL RE_MTDD_MTDIGSTACKNGAPS                                        13
C -                                                                       13
      REAL MTDD_MTDIGSTACKGAPTHICK                                        13
      EQUIVALENCE (BANK_MTDD(56),MTDD_MTDIGSTACKGAPTHICK,                 13
     *RE_MTDD_MTDIGSTACKGAPTHICK),(NAMES_MTDD(25),                        13
     *NMTDD_MTDIGSTACKGAPTHICK)                                           13
      EQUIVALENCE (COMMEN_MTDD(25),CMTDD_MTDIGSTACKGAPTHICK),             13
     *(MAP_MTDD(1,25),MMTDD_MTDIGSTACKGAPTHICK)                           13
      CHARACTER CMTDD_MTDIGSTACKGAPTHICK*40/' '/                          13
      CHARACTER NMTDD_MTDIGSTACKGAPTHICK*12/'R.MTDIGSTACKGAPTHICK'/       13
      INTEGER MMTDD_MTDIGSTACKGAPTHICK(2)/1,1/                            13
      REAL RE_MTDD_MTDIGSTACKGAPTHICK                                     13
C -                                                                       13
      REAL MTDD_MTDIGSTACKTHINGLASSTHICK                                  13
      EQUIVALENCE (BANK_MTDD(57),MTDD_MTDIGSTACKTHINGLASSTHICK,           13
     *RE_MTDD_MTDIGSTACKTHINGLASSTHICK),(NAMES_MTDD(26),                  13
     *NMTDD_MTDIGSTACKTHINGLASSTHICK)                                     13
      EQUIVALENCE (COMMEN_MTDD(26),CMTDD_MTDIGSTACKTHINGLASSTHICK),       13
     *(MAP_MTDD(1,26),MMTDD_MTDIGSTACKTHINGLASSTHICK)                     13
      CHARACTER CMTDD_MTDIGSTACKTHINGLASSTHICK*40/' '/                    13
      CHARACTER NMTDD_MTDIGSTACKTHINGLASSTHICK*12/'R.MTDIGSTACKTHINGLASS  13
     *THICK'/                                                             13
      INTEGER MMTDD_MTDIGSTACKTHINGLASSTHICK(2)/1,1/                      13
      REAL RE_MTDD_MTDIGSTACKTHINGLASSTHICK                               13
C -                                                                       13
      INTEGER LEN_MTDD,LLEN_MTDD,LINK_MTDD                                13
      PARAMETER (LEN_MTDD=57)                                             13
      PARAMETER (LLEN_MTDD=26)                                            13
      INTEGER FLAG_MTDD/0/,MAP_MTDD(2,LLEN_MTDD),IDNUM_MTDD(10)           13
      CHARACTER NAMES_MTDD(LLEN_MTDD)*12,COMMEN_MTDD(LLEN_MTDD)*40        13
      DATA IDNUM_MTDD/10*0/                                               13
      REAL BANK_MTDD(LEN_MTDD)                                            13
      CHARACTER*60 BPATH_MTDD/' '/                                        13
      SAVE FLAG_MTDD,MAP_MTDD,IDNUM_MTDD,NAMES_MTDD,COMMEN_MTDD,          13
     *BPATH_MTDD                                                          13
      DATA LINK_MTDD/0/,BANK_MTDD/LEN_MTDD*0/                             13
C *                                                                       13
      INTEGER KTRAYCONFIG,IPHI,IBL,NTRAYPLACED,IGAP,NGAPS, NTRAYGROUP     14
      REAL THISPHI,THISX,THISY,CHANDIM,THISH,THISR,DEGRAD,THISPHIB,       17
     *THISXB,THISYB, THISRB                                               17
C *                                                                       19
      CALL AGSBEGM(MODULE,IPRIN)                                          19
      IF (IPRIN.LT.0) RETURN                                              19
C Filling bank MTDG - MUON SYSTEM CONFIGURATION PARAMETERS                19
      BANK_TITLE='MUON SYSTEM CONFIGURATION PARAMETERS'                   19
      BPATH_MTDG='MTDG*'                                                  19
      IDNUM_MTDG(1)=0                                                     20
C VERSION NUMBER                                                          20
      MTDG_VERSION=1                                                      20
      CMTDG_VERSION='VERSION NUMBER'                                      20
C =1 SINGLE BACKLEG (RUN11), =2 FULL SYSTEM (RUN13)                       21
      MTDG_CONFIG=1                                                       21
      CMTDG_CONFIG='=1 SINGLE BACKLEG (RUN11), =2 FULL SYSTEM (RUN13)'    21
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MTDG',    22
     *BPATH_MTDG,IDNUM_MTDG,1, MAP_MTDG,NAMES_MTDG,COMMEN_MTDG,           22
     *BANK_MTDG,LEN_MTDG,LLEN_MTDG,LINK_MTDG,FLAG_MTDG)                   22
C *                                                                       22
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MTDG',    22
     *BPATH_MTDG,IDNUM_MTDG,1, MAP_MTDG,NAMES_MTDG,COMMEN_MTDG,           22
     *BANK_MTDG,LEN_MTDG,LLEN_MTDG,LINK_MTDG,FLAG_MTDG)                   22
C *                                                                       23
      CALL AGSBEGM(MODULE,IPRIN)                                          23
      IF (IPRIN.LT.0) RETURN                                              23
C Filling bank MTDD - MUON SYSTEM GEOMETRY PARAMETERS                     23
      BANK_TITLE='MUON SYSTEM GEOMETRY PARAMETERS'                        23
      BPATH_MTDD='MTDD*'                                                  23
      IDNUM_MTDD(1)=0                                                     24
C INTEGRATION RADIUS LOWER                                                24
      MTDD_RMIN=364.25                                                    24
      CMTDD_RMIN='INTEGRATION RADIUS LOWER'                               24
C INTEGRATION RADIUS UPPER                                                25
      MTDD_RMAX=430.0                                                     25
      CMTDD_RMAX='INTEGRATION RADIUS UPPER'                               25
C RADIUS TO TRAY CENTROID IN INCHES BY BACKLEG ID                         31
      MTDD_RI(1)=154.664                                                  31
      MTDD_RI(2)=154.664                                                  31
      MTDD_RI(3)=154.664                                                  31
      MTDD_RI(4)=154.664                                                  31
      MTDD_RI(5)=154.664                                                  31
      MTDD_RI(6)=154.664                                                  31
      MTDD_RI(7)=154.664                                                  31
      MTDD_RI(8)=160.809                                                  31
      MTDD_RI(9)=-1.0000                                                  31
      MTDD_RI(10)=154.730                                                 31
      MTDD_RI(11)=162.960                                                 31
      MTDD_RI(12)=162.960                                                 31
      MTDD_RI(13)=162.960                                                 31
      MTDD_RI(14)=162.960                                                 31
      MTDD_RI(15)=162.960                                                 31
      MTDD_RI(16)=162.960                                                 31
      MTDD_RI(17)=162.960                                                 31
      MTDD_RI(18)=162.960                                                 31
      MTDD_RI(19)=162.960                                                 31
      MTDD_RI(20)=162.960                                                 31
      MTDD_RI(21)=160.664                                                 31
      MTDD_RI(22)=154.730                                                 31
      MTDD_RI(23)=-1.0000                                                 31
      MTDD_RI(24)=155.664                                                 31
      MTDD_RI(25)=154.664                                                 31
      MTDD_RI(26)=154.664                                                 31
      MTDD_RI(27)=154.664                                                 31
      MTDD_RI(28)=154.664                                                 31
      MTDD_RI(29)=154.664                                                 31
      MTDD_RI(30)=154.664                                                 31
      CMTDD_RI='RADIUS TO TRAY CENTROID IN INCHES BY BACKLEG ID'          31
C BACKLEG 8 SHIFT FOR BUS COVER CLEARANCE                                 32
      MTDD_YSHIFT08=5.222                                                 32
      CMTDD_YSHIFT08='BACKLEG 8 SHIFT FOR BUS COVER CLEARANCE'            32
C BACKLEG 24 SHIFT FOR BUS COVER CLEARANCE                                33
      MTDD_YSHIFT24=-3.394                                                33
      CMTDD_YSHIFT24='BACKLEG 24 SHIFT FOR BUS COVER CLEARANCE'           33
C INTEGRATION HALF LENGTH IN Z                                            34
      MTDD_DZMOTHER=300.0                                                 34
      CMTDD_DZMOTHER='INTEGRATION HALF LENGTH IN Z'                       34
C RADIUS OF A BACKLEG AT PHI-CENTER                                       35
      MTDD_BACKLEGR=364.2                                                 35
      CMTDD_BACKLEGR='RADIUS OF A BACKLEG AT PHI-CENTER'                  35
C BEMC ELECTRONICS BOX HALF-LENGTH X (HEIGHT)                             36
      MTDD_BEMCELECTBOXDX=11.035                                          36
      CMTDD_BEMCELECTBOXDX='BEMC ELECTRONICS BOX HALF-LENGTH X            36
     *(HEIGHT)'                                                           36
C BEMC ELECTRONICS BOX HALF-LENGTH Y (WIDTH)                              37
      MTDD_BEMCELECTBOXDY=30.65                                           37
      CMTDD_BEMCELECTBOXDY='BEMC ELECTRONICS BOX HALF-LENGTH Y (WIDTH)'   37
C BEMC ELECTRONICS BOX HALF-LENGTH Z (LENGTH 5-PACK)                      38
      MTDD_BEMCELECTBOXDZ5=290.0                                          38
      CMTDD_BEMCELECTBOXDZ5='BEMC ELECTRONICS BOX HALF-LENGTH Z (LENGTH   38
     *5-PACK)'                                                            38
C BEMC ELECTRONICS BOX HALF-LENGTH Z (LENGTH 3-PACK)                      39
      MTDD_BEMCELECTBOXDZ3=200.0                                          39
      CMTDD_BEMCELECTBOXDZ3='BEMC ELECTRONICS BOX HALF-LENGTH Z (LENGTH   39
     *3-PACK)'                                                            39
C DISTANCE TOP OF BEMC BOX TO UNDERSIDE OF LOWEST TRAY                    40
      MTDD_RGAP=3.81                                                      40
      CMTDD_RGAP='DISTANCE TOP OF BEMC BOX TO UNDERSIDE OF LOWEST TRAY'   40
C TRAY MOTHER HALF-LENGTH X (HEIGHT) (2*1.965IN)*2.54/2                   41
      MTDD_MTDMOTHERDX=4.9911                                             41
      CMTDD_MTDMOTHERDX='TRAY MOTHER HALF-LENGTH X (HEIGHT) (2*1.965IN)*  41
     *2.54/2'                                                             41
C TRAY MOTHER HALF-LENGTH Y (WIDTH) ***WAS 32***                          42
      MTDD_MTDMOTHERDY=33.8                                               42
      CMTDD_MTDMOTHERDY='TRAY MOTHER HALF-LENGTH Y (WIDTH) ***WAS         42
     *32***'                                                              42
C TRAY MOTHER HALF-LENGTH Z (LENGTH LONG)                                 43
      MTDD_MTDMOTHERDZ5=285.0                                             43
      CMTDD_MTDMOTHERDZ5='TRAY MOTHER HALF-LENGTH Z (LENGTH LONG)'        43
C TRAY MOTHER HALF-LENGTH Z (LENGTH SHORT)                                44
      MTDD_MTDMOTHERDZ3=195.0                                             44
      CMTDD_MTDMOTHERDZ3='TRAY MOTHER HALF-LENGTH Z (LENGTH SHORT)'       44
C TRAY HALF-HEIGHT (1.965IN)*2.54/2                                       45
      MTDD_MTDTRAYDX=2.49555                                              45
      CMTDD_MTDTRAYDX='TRAY HALF-HEIGHT (1.965IN)*2.54/2'                 45
C TRAY HALF-WIDTH                                                         46
      MTDD_MTDTRAYDY=33.765                                               46
      CMTDD_MTDTRAYDY='TRAY HALF-WIDTH'                                   46
C TRAY HALF-LENGTH                                                        47
      MTDD_MTDTRAYDZ=54.005                                               47
      CMTDD_MTDTRAYDZ='TRAY HALF-LENGTH'                                  47
C TRAY BOTTOM PLATE FULL THICKNESS (0.125IN)                              48
      MTDD_MTDBPLATE=0.3175                                               48
      CMTDD_MTDBPLATE='TRAY BOTTOM PLATE FULL THICKNESS (0.125IN)'        48
C TRAY TOP PLATE FULL THICKNESS (0.090IN)                                 49
      MTDD_MTDTPLATE=0.2286                                               49
      CMTDD_MTDTPLATE='TRAY TOP PLATE FULL THICKNESS (0.090IN)'           49
C WMRPC INNER GLASS STACK VOLUME HALF-LENGTH Y                            50
      MTDD_MTDIGSTACKDY=26.1                                              50
      CMTDD_MTDIGSTACKDY='WMRPC INNER GLASS STACK VOLUME HALF-LENGTH Y'   50
C WMRPC INNER GLASS STACK VOLUME HALF-LENGTH Y                            51
      MTDD_MTDIGSTACKDZ=43.5                                              51
      CMTDD_MTDIGSTACKDZ='WMRPC INNER GLASS STACK VOLUME HALF-LENGTH Y'   51
C WMPRC NUMBER OF GAS GAPS                                                52
      MTDD_MTDIGSTACKNGAPS=5.0                                            52
      CMTDD_MTDIGSTACKNGAPS='WMPRC NUMBER OF GAS GAPS'                    52
C WMRPC GAS THICKNESS (0.25MM)                                            53
      MTDD_MTDIGSTACKGAPTHICK=0.025                                       53
      CMTDD_MTDIGSTACKGAPTHICK='WMRPC GAS THICKNESS (0.25MM)'             53
C WMRPC THIN GLASS THICKNESS (0.7MM)                                      54
      MTDD_MTDIGSTACKTHINGLASSTHICK=0.07                                  54
      CMTDD_MTDIGSTACKTHINGLASSTHICK='WMRPC THIN GLASS THICKNESS (0.7MM)  54
     *'                                                                   54
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MTDD',    55
     *BPATH_MTDD,IDNUM_MTDD,1, MAP_MTDD,NAMES_MTDD,COMMEN_MTDD,           55
     *BANK_MTDD,LEN_MTDD,LLEN_MTDD,LINK_MTDD,FLAG_MTDD)                   55
C *                                                                       55
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MTDD',    55
     *BPATH_MTDD,IDNUM_MTDD,1, MAP_MTDD,NAMES_MTDD,COMMEN_MTDD,           55
     *BANK_MTDD,LEN_MTDD,LLEN_MTDD,LINK_MTDD,FLAG_MTDD)                   55
C use default MTDG bank                                                   56
      BPATH_MTDG='MTDG*'                                                  56
      IDNUM_MTDG(1)=0                                                     56
      CALL AGDGETP('MUTDGEO5','*',-999,'MTDG',' ',BANK_MTDG, BPATH_MTDG,  56
     *IDNUM_MTDG,1,MAP_MTDG,NAMES_MTDG,BANK_MTDG,LEN_MTDG,LLEN_MTDG,      56
     *LINK_MTDG,FLAG_MTDG)                                                56
C use default MTDD bank                                                   57
      BPATH_MTDD='MTDD*'                                                  57
      IDNUM_MTDD(1)=0                                                     57
      CALL AGDGETP('MUTDGEO5','*',-999,'MTDD',' ',BANK_MTDD, BPATH_MTDD,  57
     *IDNUM_MTDD,1,MAP_MTDD,NAMES_MTDD,BANK_MTDD,LEN_MTDD,LLEN_MTDD,      57
     *LINK_MTDD,FLAG_MTDD)                                                57
      WRITE(LOUT,5010)MTDG_VERSION                                        57
5010  FORMAT(' MuTD: MTDG_Version = ', F5.3)                              59
      WRITE(LOUT,5020)MTDG_CONFIG                                         59
5020  FORMAT(' MuTD: MTDG_Config = ', F5.3)                               61
      KTRAYCONFIG = NINT(MTDG_CONFIG)                                     62
      NGAPS = MTDD_MTDIGSTACKNGAPS                                        63
      DEGRAD = 57.2957795                                                 63
C Geant COMPONENT : H ===> A=1 Z=1 W=0.90*2*1./102.+0.+0.05*10*1./58. <=  65
C ==                                                                      65
      CALL AGSRESET                                                       65
      AG_A = 1                                                            65
      AG_Z = 1                                                            65
      AG_W = 0.90*2*1./102.+0.+0.05*10*1./58.                             65
      AG_TITLE = 'H'                                                      65
      AG_EXNAME = 'COMPONENT'                                             65
      AG_PARLIST = 'A_Z_W'                                                65
      CALL AXCOMPONENT                                                    65
C *                                                                       65
C Geant COMPONENT : C ===> A=12 Z=6 W=0.90*2*12./102.+0.+0.05*4*12./58.   65
C <===                                                                    65
      CALL AGSRESET                                                       66
      AG_A = 12                                                           66
      AG_Z = 6                                                            66
      AG_W = 0.90*2*12./102.+0.+0.05*4*12./58.                            66
      AG_TITLE = 'C'                                                      66
      AG_EXNAME = 'COMPONENT'                                             66
      AG_PARLIST = 'A_Z_W'                                                66
      CALL AXCOMPONENT                                                    66
C *                                                                       66
C Geant COMPONENT : F ===> A=19 Z=9 W=0.90*4*19./102.+0.05*6*19./146.+0.  66
C  <===                                                                   66
      CALL AGSRESET                                                       67
      AG_A = 19                                                           67
      AG_Z = 9                                                            67
      AG_W = 0.90*4*19./102.+0.05*6*19./146.+0.                           67
      AG_TITLE = 'F'                                                      67
      AG_EXNAME = 'COMPONENT'                                             67
      AG_PARLIST = 'A_Z_W'                                                67
      CALL AXCOMPONENT                                                    67
C *                                                                       67
C Geant COMPONENT : S ===> A=32 Z=16 W=0.+0.05*1*32./146.+0. <===         67
      CALL AGSRESET                                                       68
      AG_A = 32                                                           68
      AG_Z = 16                                                           68
      AG_W = 0.+0.05*1*32./146.+0.                                        68
      AG_TITLE = 'S'                                                      68
      AG_EXNAME = 'COMPONENT'                                             68
      AG_PARLIST = 'A_Z_W'                                                68
      CALL AXCOMPONENT                                                    68
C *                                                                       68
C Geant MIXTURE : RPCGAS ===> DENS=4.55E-3 <===                           68
      CALL AGSRESET                                                       69
      AG_DENS = 4.55E-3                                                   69
      AG_TITLE = 'RPCGAS'                                                 69
      AG_EXNAME = 'MIXTURE'                                               69
      AG_PARLIST = 'DENS'                                                 69
      CALL AXMIXTURE                                                      69
C *                                                                       69
C Geant COMPONENT : SI ===> A=28 Z=14 W=1. <===                           69
      CALL AGSRESET                                                       70
      AG_A = 28                                                           70
      AG_Z = 14                                                           70
      AG_W = 1.                                                           70
      AG_TITLE = 'SI'                                                     70
      AG_EXNAME = 'COMPONENT'                                             70
      AG_PARLIST = 'A_Z_W'                                                70
      CALL AXCOMPONENT                                                    70
C *                                                                       70
C Geant COMPONENT : O ===> A=16 Z=8 W=2. <===                             70
      CALL AGSRESET                                                       71
      AG_A = 16                                                           71
      AG_Z = 8                                                            71
      AG_W = 2.                                                           71
      AG_TITLE = 'O'                                                      71
      AG_EXNAME = 'COMPONENT'                                             71
      AG_PARLIST = 'A_Z_W'                                                71
      CALL AXCOMPONENT                                                    71
C *                                                                       71
C Geant MIXTURE : GLASS ===> DENS=2.5 <===                                71
      CALL AGSRESET                                                       72
      AG_DENS = 2.5                                                       72
      AG_TITLE = 'GLASS'                                                  72
      AG_EXNAME = 'MIXTURE'                                               72
      AG_PARLIST = 'DENS'                                                 72
      CALL AXMIXTURE                                                      72
C *                                                                       72
      CALL AGSBEGM(MODULE,IPRIN)                                          72
C Check IPRIN<0                                                           73
      IF (IPRIN.LT.0) RETURN                                              73
C Create MUTD :                                                           73
C Call To Local Procedure MUTD                                            73
      ASSIGN 5040 TO BLOCK_MUTD                                           73
      GO TO  5030                                                         73
5040  CONTINUE                                                            73
C *                                                                       73
C Geant POSITION : MUTD ===> MOTHER='CAVE' <===                           74
      CALL AGSRESET                                                       74
      AG_MOTHER = 'CAVE'                                                  74
      AG_TITLE = 'MUTD'                                                   74
      AG_EXNAME = 'POSITION'                                              74
      AG_PARLIST = 'MOTHER'                                               74
      CALL AXPOSITION                                                     74
C *                                                                       74
C ----------------------------------------------------------------------  74
C ------------ MUTD                                                       74
      CALL AGSENDM                                                        76
      RETURN                                                              76
C Begin Local Procedure MUTD                                              76
C MUTD IS THE MUON DETECTOR MOTHER                                        76
5030  CONTINUE                                                            76
         AG_TITLE='MUTD'                                                  76
         AG_MODULE=MODULE                                                 76
         AG_MARK='V00'                                                    76
         CALL AGSPUSH                                                     76
         AG_MODULE=MODULE                                                 76
C    *                                                                    76
C    Geant MATERIAL : AIR ===> inherited arguments used <===              77
         CALL AGSRESET                                                    77
         AG_TITLE = 'AIR'                                                 77
         AG_EXNAME = 'MATERIAL'                                           77
         AG_PARLIST = 'NONE'                                              77
         CALL AXMATERIAL                                                  77
C    *                                                                    77
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           77
         CALL AGSRESET                                                    78
         AG_TITLE = 'STANDARD'                                            78
         AG_EXNAME = 'MEDIUM'                                             78
         AG_PARLIST = 'NONE'                                              78
         CALL AXMEDIUM                                                    78
C    *                                                                    78
C    Geant ATTRIBUTE : MUTD ===> SEEN=0 COLO=1 <===                       78
         CALL AGSRESET                                                    79
         AG_SEEN = 0                                                      79
         AG_COLO = 1                                                      79
         AG_TITLE = 'MUTD'                                                79
         AG_EXNAME = 'ATTRIBUTE'                                          79
         AG_PARLIST = 'SEEN_COLO'                                         79
         CALL AXATTRIBUTE                                                 79
C    *                                                                    79
C    Geant SHAPE : TUBE ===> RMIN=MTDD_RMIN RMAX=MTDD_RMAX, DZ=MTDD_DZMO  79
C    THER <===                                                            79
         CALL AGSRESET                                                    81
         AG_RMIN = MTDD_RMIN                                              81
         AG_RMAX = MTDD_RMAX                                              81
         AG_DZ = MTDD_DZMOTHER                                            81
         AG_TITLE = 'TUBE'                                                81
         AG_EXNAME = 'SHAPE'                                              81
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      81
         CALL AXSHAPE                                                     81
C    *                                                                    81
         NTRAYPLACED = 0                                                  81
C    Check KTRAYCONFIG.EQ.4                                               83
         IF (KTRAYCONFIG.EQ.4) THEN                                       83
         THISPHI = 156                                                    84
         NTRAYGROUP=3                                                     84
C    Create MTTG :                                                        86
C    Call To Local Procedure MTTG                                         86
         ASSIGN 5060 TO BLOCK_MTTG                                        86
         GO TO  5050                                                      86
5060     CONTINUE                                                         86
C    *                                                                    86
C    Geant POSITION : MTTG ===> Z=0 ALPHAZ=156 <===                       87
         CALL AGSRESET                                                    87
         AG_Z = 0                                                         87
         AG_ALPHAN=AG_ALPHAN+1                                            87
         AG_ALPHAK(AG_ALPHAN)=3                                           87
         AG_ALPHAL(AG_ALPHAN)=156                                         87
         AG_TITLE = 'MTTG'                                                87
         AG_EXNAME = 'POSITION'                                           87
         AG_PARLIST = 'Z_ALPHAZ'                                          87
         CALL AXPOSITION                                                  87
C    *                                                                    87
         NTRAYPLACED = NTRAYPLACED + 3                                    87
         ELSEIF (KTRAYCONFIG.EQ.5) THEN                                   89
         WRITE(LOUT,5070)KTRAYCONFIG                                      89
5070     FORMAT('MTD configuration is OBSOLETE kTrayConfig=', I3)         89
         ELSEIF (KTRAYCONFIG.EQ.12) THEN                                  91
         WRITE(LOUT,5080)KTRAYCONFIG                                      91
5080     FORMAT('Creating Run-12 MTD geometry with kTrayConfig=', I3)     91
C    *                                                                    93
         IBL=26                                                           93
            GO TO 5093                                                    93
5091        IBL=IBL+1                                                     93
5093        IF(IBL-(28).GT.0) GO TO 5092                                  93
            THISPHI = 90.0 + (IBL-1)*(-12.0)                              94
            THISR = MTDD_RI(IBL)*2.54 + 0.965*2.54                        95
            THISX = THISR*COS(THISPHI/DEGRAD)                             96
            THISY = THISR*SIN(THISPHI/DEGRAD)                             97
C       Check IBL.EQ.26                                                   98
            IF (IBL.EQ.26) THEN                                           98
            NTRAYGROUP=3                                                  98
C       Create MTTG :                                                     100
C       Call To Local Procedure MTTG                                      100
            ASSIGN 5100 TO BLOCK_MTTG                                     100
            GO TO  5050                                                   100
5100        CONTINUE                                                      100
C       *                                                                 100
C       Geant POSITION : MTTG ===> X=THISX Y=THISY Z=0 ALPHAZ=THISPHI <=  101
C       ==                                                                101
            CALL AGSRESET                                                 101
            AG_X = THISX                                                  101
            AG_Y = THISY                                                  101
            AG_Z = 0                                                      101
            AG_ALPHAN=AG_ALPHAN+1                                         101
            AG_ALPHAK(AG_ALPHAN)=3                                        101
            AG_ALPHAL(AG_ALPHAN)=THISPHI                                  101
            AG_TITLE = 'MTTG'                                             101
            AG_EXNAME = 'POSITION'                                        101
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   101
            CALL AXPOSITION                                               101
C       *                                                                 101
            NTRAYPLACED = NTRAYPLACED + 3                                 101
            ELSE                                                          103
            NTRAYGROUP=5                                                  103
C       Create MTTG :                                                     105
C       Call To Local Procedure MTTG                                      105
            ASSIGN 5110 TO BLOCK_MTTG                                     105
            GO TO  5050                                                   105
5110        CONTINUE                                                      105
C       *                                                                 105
C       Geant POSITION : MTTG ===> X=THISX Y=THISY Z=0 ALPHAZ=THISPHI <=  106
C       ==                                                                106
            CALL AGSRESET                                                 106
            AG_X = THISX                                                  106
            AG_Y = THISY                                                  106
            AG_Z = 0                                                      106
            AG_ALPHAN=AG_ALPHAN+1                                         106
            AG_ALPHAK(AG_ALPHAN)=3                                        106
            AG_ALPHAL(AG_ALPHAN)=THISPHI                                  106
            AG_TITLE = 'MTTG'                                             106
            AG_EXNAME = 'POSITION'                                        106
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   106
            CALL AXPOSITION                                               106
C       *                                                                 106
            NTRAYPLACED = NTRAYPLACED + 5                                 106
            ENDIF                                                         108
         GO TO 5091                                                       109
5092     CONTINUE                                                         109
         ELSEIF (KTRAYCONFIG.EQ.13) THEN                                  110
         WRITE(LOUT,5120)KTRAYCONFIG                                      110
5120     FORMAT('Creating Run-13 MTD geometry with kTrayConfig=', I3)     110
C    *                                                                    112
         IBL=1                                                            112
            GO TO 5133                                                    112
5131        IBL=IBL+1                                                     112
5133        IF(IBL-(30).GT.0) GO TO 5132                                  112
            THISPHI = 90.0 + (IBL-1)*(-12.0)                              113
            THISR = MTDD_RI(IBL)*2.54 + 0.965*2.54                        114
            THISX = THISR*COS(THISPHI/DEGRAD)                             115
            THISY = THISR*SIN(THISPHI/DEGRAD)                             116
C       Check (IBL.NE.8.AND.IBL.NE.9.AND.IBL.NE.23.AND.IBL.NE.24)         117
            IF ((IBL.NE.8.AND.IBL.NE.9.AND.IBL.NE.23.AND.IBL.NE.24))      117
     *      THEN                                                          117
C       Check IBL.LE.10.OR.IBL.GE.22                                      118
            IF (IBL.LE.10.OR.IBL.GE.22) THEN                              118
            NTRAYGROUP=5                                                  118
C       Create MTTG :                                                     120
C       Call To Local Procedure MTTG                                      120
            ASSIGN 5140 TO BLOCK_MTTG                                     120
            GO TO  5050                                                   120
5140        CONTINUE                                                      120
C       *                                                                 120
C       Geant POSITION : MTTG ===> X=THISX Y=THISY Z=0 ALPHAZ=THISPHI <=  121
C       ==                                                                121
            CALL AGSRESET                                                 121
            AG_X = THISX                                                  121
            AG_Y = THISY                                                  121
            AG_Z = 0                                                      121
            AG_ALPHAN=AG_ALPHAN+1                                         121
            AG_ALPHAK(AG_ALPHAN)=3                                        121
            AG_ALPHAL(AG_ALPHAN)=THISPHI                                  121
            AG_TITLE = 'MTTG'                                             121
            AG_EXNAME = 'POSITION'                                        121
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   121
            CALL AXPOSITION                                               121
C       *                                                                 121
            NTRAYPLACED = NTRAYPLACED + 5                                 121
            ENDIF                                                         123
            ENDIF                                                         124
         GO TO 5131                                                       125
5132     CONTINUE                                                         125
         ELSEIF (KTRAYCONFIG.EQ.14) THEN                                  126
C    *                                                                    127
         IBL=1                                                            127
            GO TO 5153                                                    127
5151        IBL=IBL+1                                                     127
5153        IF(IBL-(30).GT.0) GO TO 5152                                  127
            THISPHI = 90.0 + (IBL-1)*(-12.0)                              128
            THISR = MTDD_RI(IBL)*2.54 + 0.965*2.54                        129
            THISX = THISR*COS(THISPHI/DEGRAD)                             130
            THISY = THISR*SIN(THISPHI/DEGRAD)                             131
C       Check THISR.GT.0                                                  132
            IF (THISR.GT.0) THEN                                          132
C       Check IBL.GE.12.AND.IBL.LE.20                                     133
            IF (IBL.GE.12.AND.IBL.LE.20) THEN                             133
            NTRAYGROUP=3                                                  133
C       Create MTTG :                                                     135
C       Call To Local Procedure MTTG                                      135
            ASSIGN 5160 TO BLOCK_MTTG                                     135
            GO TO  5050                                                   135
5160        CONTINUE                                                      135
C       *                                                                 135
C       Geant POSITION : MTTG ===> X=THISX Y=THISY Z=0 ALPHAZ=THISPHI <=  136
C       ==                                                                136
            CALL AGSRESET                                                 136
            AG_X = THISX                                                  136
            AG_Y = THISY                                                  136
            AG_Z = 0                                                      136
            AG_ALPHAN=AG_ALPHAN+1                                         136
            AG_ALPHAK(AG_ALPHAN)=3                                        136
            AG_ALPHAL(AG_ALPHAN)=THISPHI                                  136
            AG_TITLE = 'MTTG'                                             136
            AG_EXNAME = 'POSITION'                                        136
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   136
            CALL AXPOSITION                                               136
C       *                                                                 136
            NTRAYPLACED = NTRAYPLACED + 3                                 136
            ENDIF                                                         138
C       Check IBL.LT.12.OR.IBL.GT.20                                      139
            IF (IBL.LT.12.OR.IBL.GT.20) THEN                              139
C       Check IBL.NE.8.AND.IBL.NE.24                                      140
            IF (IBL.NE.8.AND.IBL.NE.24) THEN                              140
            NTRAYGROUP=5                                                  140
C       Create MTTG :                                                     142
C       Call To Local Procedure MTTG                                      142
            ASSIGN 5170 TO BLOCK_MTTG                                     142
            GO TO  5050                                                   142
5170        CONTINUE                                                      142
C       *                                                                 142
C       Geant POSITION : MTTG ===> X=THISX Y=THISY Z=0 ALPHAZ=THISPHI <=  143
C       ==                                                                143
            CALL AGSRESET                                                 143
            AG_X = THISX                                                  143
            AG_Y = THISY                                                  143
            AG_Z = 0                                                      143
            AG_ALPHAN=AG_ALPHAN+1                                         143
            AG_ALPHAK(AG_ALPHAN)=3                                        143
            AG_ALPHAL(AG_ALPHAN)=THISPHI                                  143
            AG_TITLE = 'MTTG'                                             143
            AG_EXNAME = 'POSITION'                                        143
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   143
            CALL AXPOSITION                                               143
C       *                                                                 143
            NTRAYPLACED = NTRAYPLACED + 5                                 143
            ENDIF                                                         145
C       Check IBL.EQ.8                                                    146
            IF (IBL.EQ.8) THEN                                            146
            THISRB = SQRT(THISR*THISR +                                   147
     *      2.54*MTDD_YSHIFT08*2.54*MTDD_YSHIFT08)                        147
            THISPHIB = DEGRAD*ATAN(2.54*MTDD_YSHIFT08/THISR) + THISPHI    148
            THISXB = THISRB*COS(THISPHIB/DEGRAD)                          149
            THISYB = THISRB*SIN(THISPHIB/DEGRAD)                          150
            NTRAYGROUP=5                                                  150
C       Create MTTG :                                                     152
C       Call To Local Procedure MTTG                                      152
            ASSIGN 5180 TO BLOCK_MTTG                                     152
            GO TO  5050                                                   152
5180        CONTINUE                                                      152
C       *                                                                 152
C       Geant POSITION : MTTG ===> X=THISXB Y=THISYB Z=0 ALPHAZ=THISPHI   153
C       <===                                                              153
            CALL AGSRESET                                                 153
            AG_X = THISXB                                                 153
            AG_Y = THISYB                                                 153
            AG_Z = 0                                                      153
            AG_ALPHAN=AG_ALPHAN+1                                         153
            AG_ALPHAK(AG_ALPHAN)=3                                        153
            AG_ALPHAL(AG_ALPHAN)=THISPHI                                  153
            AG_TITLE = 'MTTG'                                             153
            AG_EXNAME = 'POSITION'                                        153
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   153
            CALL AXPOSITION                                               153
C       *                                                                 153
            NTRAYPLACED = NTRAYPLACED + 5                                 153
            ENDIF                                                         155
C       Check IBL.EQ.24                                                   156
            IF (IBL.EQ.24) THEN                                           156
            THISRB = SQRT(THISR*THISR +                                   157
     *      2.54*MTDD_YSHIFT24*2.54*MTDD_YSHIFT24)                        157
            THISPHIB = DEGRAD*ATAN(2.54*MTDD_YSHIFT24/THISR) + THISPHI    158
            THISXB = THISRB*COS(THISPHIB/DEGRAD)                          159
            THISYB = THISRB*SIN(THISPHIB/DEGRAD)                          160
            NTRAYGROUP=5                                                  160
C       Create MTTG :                                                     162
C       Call To Local Procedure MTTG                                      162
            ASSIGN 5190 TO BLOCK_MTTG                                     162
            GO TO  5050                                                   162
5190        CONTINUE                                                      162
C       *                                                                 162
C       Geant POSITION : MTTG ===> X=THISXB Y=THISYB Z=0 ALPHAZ=THISPHI   163
C       <===                                                              163
            CALL AGSRESET                                                 163
            AG_X = THISXB                                                 163
            AG_Y = THISYB                                                 163
            AG_Z = 0                                                      163
            AG_ALPHAN=AG_ALPHAN+1                                         163
            AG_ALPHAK(AG_ALPHAN)=3                                        163
            AG_ALPHAL(AG_ALPHAN)=THISPHI                                  163
            AG_TITLE = 'MTTG'                                             163
            AG_EXNAME = 'POSITION'                                        163
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   163
            CALL AXPOSITION                                               163
C       *                                                                 163
            NTRAYPLACED = NTRAYPLACED + 5                                 163
            ENDIF                                                         165
            ENDIF                                                         166
            ENDIF                                                         167
         GO TO 5151                                                       168
5152     CONTINUE                                                         168
         ENDIF                                                            169
         WRITE(LOUT,5200)NTRAYPLACED                                      169
5200     FORMAT('Number of MTD trays placed=', I3)                        169
5031  CALL AGSPOPB                                                        171
      GO TO BLOCK_MUTD, (5030 ,5040)                                      171
C End of Local Procedure                                                  171
C ----------------------------------------------------------------------  172
C ------------ MTTG                                                       172
C Begin Local Procedure MTTG                                              173
C MTTG IS THE MTD TRAY GROUP                                              173
5050  CONTINUE                                                            173
         AG_TITLE='MTTG'                                                  173
         AG_MODULE=MODULE                                                 173
         AG_MARK='V00'                                                    173
         CALL AGSPUSH                                                     173
         AG_MODULE=MODULE                                                 173
C    *                                                                    173
C    Geant MATERIAL : AIR ===> inherited arguments used <===              174
         CALL AGSRESET                                                    174
         AG_TITLE = 'AIR'                                                 174
         AG_EXNAME = 'MATERIAL'                                           174
         AG_PARLIST = 'NONE'                                              174
         CALL AXMATERIAL                                                  174
C    *                                                                    174
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           174
         CALL AGSRESET                                                    175
         AG_TITLE = 'STANDARD'                                            175
         AG_EXNAME = 'MEDIUM'                                             175
         AG_PARLIST = 'NONE'                                              175
         CALL AXMEDIUM                                                    175
C    *                                                                    175
C    Geant ATTRIBUTE : MTTT ===> SEEN=0 COLO=3 <===                       175
         CALL AGSRESET                                                    176
         AG_SEEN = 0                                                      176
         AG_COLO = 3                                                      176
         AG_TITLE = 'MTTT'                                                176
         AG_EXNAME = 'ATTRIBUTE'                                          176
         AG_PARLIST = 'SEEN_COLO'                                         176
         CALL AXATTRIBUTE                                                 176
C    *                                                                    176
C    Check NTRAYGROUP==3                                                  176
         IF (NTRAYGROUP.EQ.3) THEN                                        177
C    Geant SHAPE : BOX ===> DX=MTDD_MTDMOTHERDX DY=MTDD_MTDMOTHERDY, DZ=  179
C    MTDD_MTDMOTHERDZ3 <===                                               179
         CALL AGSRESET                                                    179
         AG_DX = MTDD_MTDMOTHERDX                                         179
         AG_DY = MTDD_MTDMOTHERDY                                         179
         AG_DZ = MTDD_MTDMOTHERDZ3                                        179
         AG_TITLE = 'BOX'                                                 179
         AG_EXNAME = 'SHAPE'                                              179
         AG_PARLIST = 'DX_DY_DZ'                                          179
         CALL AXSHAPE                                                     179
C    *                                                                    179
C    Create MTRA :                                                        180
C    Call To Local Procedure MTRA                                         180
         ASSIGN 5220 TO BLOCK_MTRA                                        180
         GO TO  5210                                                      180
5220     CONTINUE                                                         180
C    *                                                                    180
C    Geant POSITION : MTRA ===> X=-MTDD_MTDTRAYDX Z=-87 <===              181
         CALL AGSRESET                                                    181
         AG_X = -MTDD_MTDTRAYDX                                           181
         AG_Z = -87                                                       181
         AG_TITLE = 'MTRA'                                                181
         AG_EXNAME = 'POSITION'                                           181
         AG_PARLIST = 'X_Z'                                               181
         CALL AXPOSITION                                                  181
C    *                                                                    181
C    Geant POSITION : MTRA ===> X=MTDD_MTDTRAYDX Z=0 <===                 181
         CALL AGSRESET                                                    182
         AG_X = MTDD_MTDTRAYDX                                            182
         AG_Z = 0                                                         182
         AG_TITLE = 'MTRA'                                                182
         AG_EXNAME = 'POSITION'                                           182
         AG_PARLIST = 'X_Z'                                               182
         CALL AXPOSITION                                                  182
C    *                                                                    182
C    Geant POSITION : MTRA ===> X=-MTDD_MTDTRAYDX Z=87 <===               182
         CALL AGSRESET                                                    183
         AG_X = -MTDD_MTDTRAYDX                                           183
         AG_Z = 87                                                        183
         AG_TITLE = 'MTRA'                                                183
         AG_EXNAME = 'POSITION'                                           183
         AG_PARLIST = 'X_Z'                                               183
         CALL AXPOSITION                                                  183
C    *                                                                    183
         ENDIF                                                            183
C    Check NTRAYGROUP==5                                                  185
         IF (NTRAYGROUP.EQ.5) THEN                                        185
C    Geant SHAPE : BOX ===> DX=MTDD_MTDMOTHERDX DY=MTDD_MTDMOTHERDY, DZ=  187
C    MTDD_MTDMOTHERDZ5 <===                                               187
         CALL AGSRESET                                                    187
         AG_DX = MTDD_MTDMOTHERDX                                         187
         AG_DY = MTDD_MTDMOTHERDY                                         187
         AG_DZ = MTDD_MTDMOTHERDZ5                                        187
         AG_TITLE = 'BOX'                                                 187
         AG_EXNAME = 'SHAPE'                                              187
         AG_PARLIST = 'DX_DY_DZ'                                          187
         CALL AXSHAPE                                                     187
C    *                                                                    187
C    Create MTRA :                                                        188
C    Call To Local Procedure MTRA                                         188
         ASSIGN 5230 TO BLOCK_MTRA                                        188
         GO TO  5210                                                      188
5230     CONTINUE                                                         188
C    *                                                                    188
C    Geant POSITION : MTRA ===> X=MTDD_MTDTRAYDX Z=-174 <===              189
         CALL AGSRESET                                                    189
         AG_X = MTDD_MTDTRAYDX                                            189
         AG_Z = -174                                                      189
         AG_TITLE = 'MTRA'                                                189
         AG_EXNAME = 'POSITION'                                           189
         AG_PARLIST = 'X_Z'                                               189
         CALL AXPOSITION                                                  189
C    *                                                                    189
C    Geant POSITION : MTRA ===> X=-MTDD_MTDTRAYDX Z=-87 <===              189
         CALL AGSRESET                                                    190
         AG_X = -MTDD_MTDTRAYDX                                           190
         AG_Z = -87                                                       190
         AG_TITLE = 'MTRA'                                                190
         AG_EXNAME = 'POSITION'                                           190
         AG_PARLIST = 'X_Z'                                               190
         CALL AXPOSITION                                                  190
C    *                                                                    190
C    Geant POSITION : MTRA ===> X=MTDD_MTDTRAYDX Z=0 <===                 190
         CALL AGSRESET                                                    191
         AG_X = MTDD_MTDTRAYDX                                            191
         AG_Z = 0                                                         191
         AG_TITLE = 'MTRA'                                                191
         AG_EXNAME = 'POSITION'                                           191
         AG_PARLIST = 'X_Z'                                               191
         CALL AXPOSITION                                                  191
C    *                                                                    191
C    Geant POSITION : MTRA ===> X=-MTDD_MTDTRAYDX Z=87 <===               191
         CALL AGSRESET                                                    192
         AG_X = -MTDD_MTDTRAYDX                                           192
         AG_Z = 87                                                        192
         AG_TITLE = 'MTRA'                                                192
         AG_EXNAME = 'POSITION'                                           192
         AG_PARLIST = 'X_Z'                                               192
         CALL AXPOSITION                                                  192
C    *                                                                    192
C    Geant POSITION : MTRA ===> X=MTDD_MTDTRAYDX Z=174 <===               192
         CALL AGSRESET                                                    193
         AG_X = MTDD_MTDTRAYDX                                            193
         AG_Z = 174                                                       193
         AG_TITLE = 'MTRA'                                                193
         AG_EXNAME = 'POSITION'                                           193
         AG_PARLIST = 'X_Z'                                               193
         CALL AXPOSITION                                                  193
C    *                                                                    193
         ENDIF                                                            193
5051  CALL AGSPOPB                                                        195
      GO TO BLOCK_MTTG, (5050 ,5060,5100,5110,5140,5160,5170,5180,5190)   195
C End of Local Procedure                                                  195
C ----------------------------------------------------------------------  196
C ------------ MTRA                                                       196
C Begin Local Procedure MTRA                                              197
C MTRA IS AN MTD11-STYLE TRAY                                             197
5210  CONTINUE                                                            197
         AG_TITLE='MTRA'                                                  197
         AG_MODULE=MODULE                                                 197
         AG_MARK='V00'                                                    197
         CALL AGSPUSH                                                     197
         AG_MODULE=MODULE                                                 197
C    *                                                                    197
C    Geant MATERIAL : RPCGAS ===> inherited arguments used <===           198
         CALL AGSRESET                                                    198
         AG_TITLE = 'RPCGAS'                                              198
         AG_EXNAME = 'MATERIAL'                                           198
         AG_PARLIST = 'NONE'                                              198
         CALL AXMATERIAL                                                  198
C    *                                                                    198
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           198
         CALL AGSRESET                                                    199
         AG_TITLE = 'STANDARD'                                            199
         AG_EXNAME = 'MEDIUM'                                             199
         AG_PARLIST = 'NONE'                                              199
         CALL AXMEDIUM                                                    199
C    *                                                                    199
C    Geant ATTRIBUTE : MTRA ===> SEEN=1 COLO=6 <===                       199
         CALL AGSRESET                                                    200
         AG_SEEN = 1                                                      200
         AG_COLO = 6                                                      200
         AG_TITLE = 'MTRA'                                                200
         AG_EXNAME = 'ATTRIBUTE'                                          200
         AG_PARLIST = 'SEEN_COLO'                                         200
         CALL AXATTRIBUTE                                                 200
C    *                                                                    200
C    Geant SHAPE : BOX ===> DX=MTDD_MTDTRAYDX DY=MTDD_MTDTRAYDY, DZ=MTDD  200
C    _MTDTRAYDZ <===                                                      200
         CALL AGSRESET                                                    202
         AG_DX = MTDD_MTDTRAYDX                                           202
         AG_DY = MTDD_MTDTRAYDY                                           202
         AG_DZ = MTDD_MTDTRAYDZ                                           202
         AG_TITLE = 'BOX'                                                 202
         AG_EXNAME = 'SHAPE'                                              202
         AG_PARLIST = 'DX_DY_DZ'                                          202
         CALL AXSHAPE                                                     202
C    *                                                                    202
         AG_DX = MTDD_MTDBPLATE/2.                                        202
C    Create MTBP :                                                        203
C    Call To Local Procedure MTBP                                         203
         ASSIGN 5250 TO BLOCK_MTBP                                        203
         GO TO  5240                                                      203
5250     CONTINUE                                                         203
C    *                                                                    203
C    Geant POSITION : MTBP ===> X=-MTDD_MTDTRAYDX+(MTDD_MTDBPLATE/2.) <=  204
C    ==                                                                   204
         CALL AGSRESET                                                    204
         AG_X = -MTDD_MTDTRAYDX+(MTDD_MTDBPLATE/2.)                       204
         AG_TITLE = 'MTBP'                                                204
         AG_EXNAME = 'POSITION'                                           204
         AG_PARLIST = 'X'                                                 204
         CALL AXPOSITION                                                  204
C    *                                                                    204
         AG_DX = MTDD_MTDTPLATE/2.                                        204
C    Create MTTP :                                                        205
C    Call To Local Procedure MTTP                                         205
         ASSIGN 5270 TO BLOCK_MTTP                                        205
         GO TO  5260                                                      205
5270     CONTINUE                                                         205
C    *                                                                    205
C    Geant POSITION : MTTP ===> X=MTDD_MTDTRAYDX-(MTDD_MTDTPLATE/2.) <==  206
C    =                                                                    206
         CALL AGSRESET                                                    206
         AG_X = MTDD_MTDTRAYDX-(MTDD_MTDTPLATE/2.)                        206
         AG_TITLE = 'MTTP'                                                206
         AG_EXNAME = 'POSITION'                                           206
         AG_PARLIST = 'X'                                                 206
         CALL AXPOSITION                                                  206
C    *                                                                    206
         CHANDIM = MTDD_MTDTRAYDX - MTDD_MTDBPLATE/2. - MTDD_MTDBPLATE/   206
     *   2.                                                               206
         AG_DZ = MTDD_MTDTRAYDZ-2.*CHANDIM                                208
         AG_DX = CHANDIM                                                  208
         AG_DY = CHANDIM                                                  208
C    Create MCHL :                                                        208
C    Call To Local Procedure MCHL                                         208
         ASSIGN 5290 TO BLOCK_MCHL                                        208
         GO TO  5280                                                      208
5290     CONTINUE                                                         208
C    *                                                                    208
C    Geant POSITION : MCHL ===> X=-MTDD_MTDTRAYDX+MTDD_MTDBPLATE+CHANDIM  210
C     Y=MTDD_MTDTRAYDY-CHANDIM Z=0 <===                                   210
         CALL AGSRESET                                                    210
         AG_X = -MTDD_MTDTRAYDX+MTDD_MTDBPLATE+CHANDIM                    210
         AG_Y = MTDD_MTDTRAYDY-CHANDIM                                    210
         AG_Z = 0                                                         210
         AG_TITLE = 'MCHL'                                                210
         AG_EXNAME = 'POSITION'                                           210
         AG_PARLIST = 'X_Y_Z'                                             210
         CALL AXPOSITION                                                  210
C    *                                                                    210
C    Geant POSITION : MCHL ===> X=-MTDD_MTDTRAYDX+MTDD_MTDBPLATE+CHANDIM  210
C     Y=-MTDD_MTDTRAYDY+CHANDIM Z=0 ALPHAZ=180 <===                       210
         CALL AGSRESET                                                    212
         AG_X = -MTDD_MTDTRAYDX+MTDD_MTDBPLATE+CHANDIM                    212
         AG_Y = -MTDD_MTDTRAYDY+CHANDIM                                   212
         AG_Z = 0                                                         212
         AG_ALPHAN=AG_ALPHAN+1                                            212
         AG_ALPHAK(AG_ALPHAN)=3                                           212
         AG_ALPHAL(AG_ALPHAN)=180                                         212
         AG_TITLE = 'MCHL'                                                212
         AG_EXNAME = 'POSITION'                                           212
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      212
         CALL AXPOSITION                                                  212
C    *                                                                    212
         AG_DZ = CHANDIM                                                  212
         AG_DX = CHANDIM                                                  213
         AG_DY = MTDD_MTDTRAYDY                                           213
C    Create MCHS :                                                        213
C    Call To Local Procedure MCHS                                         213
         ASSIGN 5310 TO BLOCK_MCHS                                        213
         GO TO  5300                                                      213
5310     CONTINUE                                                         213
C    *                                                                    213
C    Geant POSITION : MCHS ===> X=-MTDD_MTDTRAYDX+MTDD_MTDBPLATE+CHANDIM  215
C     Y=0 Z=MTDD_MTDTRAYDZ-CHANDIM <===                                   215
         CALL AGSRESET                                                    215
         AG_X = -MTDD_MTDTRAYDX+MTDD_MTDBPLATE+CHANDIM                    215
         AG_Y = 0                                                         215
         AG_Z = MTDD_MTDTRAYDZ-CHANDIM                                    215
         AG_TITLE = 'MCHS'                                                215
         AG_EXNAME = 'POSITION'                                           215
         AG_PARLIST = 'X_Y_Z'                                             215
         CALL AXPOSITION                                                  215
C    *                                                                    215
C    Geant POSITION : MCHS ===> X=-MTDD_MTDTRAYDX+MTDD_MTDBPLATE+CHANDIM  215
C     Y=0 Z=-MTDD_MTDTRAYDZ+CHANDIM ALPHAY=180 <===                       215
         CALL AGSRESET                                                    217
         AG_X = -MTDD_MTDTRAYDX+MTDD_MTDBPLATE+CHANDIM                    217
         AG_Y = 0                                                         217
         AG_Z = -MTDD_MTDTRAYDZ+CHANDIM                                   217
         AG_ALPHAN=AG_ALPHAN+1                                            217
         AG_ALPHAK(AG_ALPHAN)=2                                           217
         AG_ALPHAL(AG_ALPHAN)=180                                         217
         AG_TITLE = 'MCHS'                                                217
         AG_EXNAME = 'POSITION'                                           217
         AG_PARLIST = 'X_Y_Z_ALPHAY'                                      217
         CALL AXPOSITION                                                  217
C    *                                                                    217
         THISH = MTDD_MTDIGSTACKNGAPS*MTDD_MTDIGSTACKGAPTHICK +           217
     *   (MTDD_MTDIGSTACKNGAPS - 1.0)*MTDD_MTDIGSTACKTHINGLASSTHICK       217
         AG_DZ = MTDD_MTDIGSTACKDZ                                        220
         AG_DX = THISH/2.0                                                220
         AG_DY = MTDD_MTDIGSTACKDY                                        220
C    Create MIGS :                                                        220
C    Call To Local Procedure MIGS                                         220
         ASSIGN 5330 TO BLOCK_MIGS                                        220
         GO TO  5320                                                      220
5330     CONTINUE                                                         220
C    *                                                                    220
C    Geant POSITION : MIGS ===> X=0 Y=0 Z=0 <===                          221
         CALL AGSRESET                                                    221
         AG_X = 0                                                         221
         AG_Y = 0                                                         221
         AG_Z = 0                                                         221
         AG_TITLE = 'MIGS'                                                221
         AG_EXNAME = 'POSITION'                                           221
         AG_PARLIST = 'X_Y_Z'                                             221
         CALL AXPOSITION                                                  221
C    *                                                                    221
5211  CALL AGSPOPB                                                        222
      GO TO BLOCK_MTRA, (5210 ,5220,5230)                                 222
C End of Local Procedure                                                  222
C ----------------------------------------------------------------------  223
C ------------ MIGS                                                       223
C Begin Local Procedure MIGS                                              224
C MIGS IS THE INNER GLASS STACK                                           224
5320  CONTINUE                                                            224
         AG_TITLE='MIGS'                                                  224
         AG_MODULE=MODULE                                                 224
         AG_MARK='V00'                                                    224
         CALL AGSPUSH                                                     224
         AG_MODULE=MODULE                                                 224
C    *                                                                    224
C    Geant ATTRIBUTE : MIGS ===> SEEN=0 COLO=4 <===                       225
         CALL AGSRESET                                                    225
         AG_SEEN = 0                                                      225
         AG_COLO = 4                                                      225
         AG_TITLE = 'MIGS'                                                225
         AG_EXNAME = 'ATTRIBUTE'                                          225
         AG_PARLIST = 'SEEN_COLO'                                         225
         CALL AXATTRIBUTE                                                 225
C    *                                                                    225
C    Geant MATERIAL : GLASS ===> inherited arguments used <===            225
         CALL AGSRESET                                                    226
         AG_TITLE = 'GLASS'                                               226
         AG_EXNAME = 'MATERIAL'                                           226
         AG_PARLIST = 'NONE'                                              226
         CALL AXMATERIAL                                                  226
C    *                                                                    226
C    Geant SHAPE : BOX ===> inherited arguments used <===                 226
         CALL AGSRESET                                                    227
         AG_TITLE = 'BOX'                                                 227
         AG_EXNAME = 'SHAPE'                                              227
         AG_PARLIST = 'NONE'                                              227
         CALL AXSHAPE                                                     227
C    *                                                                    227
         AG_DZ = MTDD_MTDIGSTACKDZ                                        227
         AG_DX = MTDD_MTDIGSTACKGAPTHICK/2.0                              228
         AG_DY = MTDD_MTDIGSTACKDY                                        228
C    Create MIGG :                                                        228
C    Call To Local Procedure MIGG                                         228
         ASSIGN 5350 TO BLOCK_MIGG                                        228
         GO TO  5340                                                      228
5350     CONTINUE                                                         228
C    *                                                                    228
C    Check NGAPS.EQ.6                                                     229
         IF (NGAPS.EQ.6) THEN                                             229
C    Geant POSITION : MIGG ===> X=-2.5*MTDD_MTDIGSTACKTHINGLASSTHICK-2.5  230
C    *MTDD_MTDIGSTACKGAPTHICK <===                                        230
         CALL AGSRESET                                                    230
         AG_X =                                                           230
     *   -2.5*MTDD_MTDIGSTACKTHINGLASSTHICK-2.5*MTDD_MTDIGSTACKGAPTHICK   230
         AG_TITLE = 'MIGG'                                                230
         AG_EXNAME = 'POSITION'                                           230
         AG_PARLIST = 'X'                                                 230
         CALL AXPOSITION                                                  230
C    *                                                                    230
C    Geant POSITION : MIGG ===> X=-1.5*MTDD_MTDIGSTACKTHINGLASSTHICK-1.5  230
C    *MTDD_MTDIGSTACKGAPTHICK <===                                        230
         CALL AGSRESET                                                    231
         AG_X =                                                           231
     *   -1.5*MTDD_MTDIGSTACKTHINGLASSTHICK-1.5*MTDD_MTDIGSTACKGAPTHICK   231
         AG_TITLE = 'MIGG'                                                231
         AG_EXNAME = 'POSITION'                                           231
         AG_PARLIST = 'X'                                                 231
         CALL AXPOSITION                                                  231
C    *                                                                    231
C    Geant POSITION : MIGG ===> X=-0.5*MTDD_MTDIGSTACKTHINGLASSTHICK-0.5  231
C    *MTDD_MTDIGSTACKGAPTHICK <===                                        231
         CALL AGSRESET                                                    232
         AG_X =                                                           232
     *   -0.5*MTDD_MTDIGSTACKTHINGLASSTHICK-0.5*MTDD_MTDIGSTACKGAPTHICK   232
         AG_TITLE = 'MIGG'                                                232
         AG_EXNAME = 'POSITION'                                           232
         AG_PARLIST = 'X'                                                 232
         CALL AXPOSITION                                                  232
C    *                                                                    232
C    Geant POSITION : MIGG ===> X=0.5*MTDD_MTDIGSTACKTHINGLASSTHICK+0.5*  232
C    MTDD_MTDIGSTACKGAPTHICK <===                                         232
         CALL AGSRESET                                                    233
         AG_X =                                                           233
     *   0.5*MTDD_MTDIGSTACKTHINGLASSTHICK+0.5*MTDD_MTDIGSTACKGAPTHICK    233
         AG_TITLE = 'MIGG'                                                233
         AG_EXNAME = 'POSITION'                                           233
         AG_PARLIST = 'X'                                                 233
         CALL AXPOSITION                                                  233
C    *                                                                    233
C    Geant POSITION : MIGG ===> X=1.5*MTDD_MTDIGSTACKTHINGLASSTHICK+1.5*  233
C    MTDD_MTDIGSTACKGAPTHICK <===                                         233
         CALL AGSRESET                                                    234
         AG_X =                                                           234
     *   1.5*MTDD_MTDIGSTACKTHINGLASSTHICK+1.5*MTDD_MTDIGSTACKGAPTHICK    234
         AG_TITLE = 'MIGG'                                                234
         AG_EXNAME = 'POSITION'                                           234
         AG_PARLIST = 'X'                                                 234
         CALL AXPOSITION                                                  234
C    *                                                                    234
C    Geant POSITION : MIGG ===> X=2.5*MTDD_MTDIGSTACKTHINGLASSTHICK+2.5*  234
C    MTDD_MTDIGSTACKGAPTHICK <===                                         234
         CALL AGSRESET                                                    235
         AG_X =                                                           235
     *   2.5*MTDD_MTDIGSTACKTHINGLASSTHICK+2.5*MTDD_MTDIGSTACKGAPTHICK    235
         AG_TITLE = 'MIGG'                                                235
         AG_EXNAME = 'POSITION'                                           235
         AG_PARLIST = 'X'                                                 235
         CALL AXPOSITION                                                  235
C    *                                                                    235
         ELSEIF (NGAPS.EQ.5) THEN                                         235
C    Geant POSITION : MIGG ===> X=-2.0*MTDD_MTDIGSTACKTHINGLASSTHICK-2.0  237
C    *MTDD_MTDIGSTACKGAPTHICK <===                                        237
         CALL AGSRESET                                                    237
         AG_X =                                                           237
     *   -2.0*MTDD_MTDIGSTACKTHINGLASSTHICK-2.0*MTDD_MTDIGSTACKGAPTHICK   237
         AG_TITLE = 'MIGG'                                                237
         AG_EXNAME = 'POSITION'                                           237
         AG_PARLIST = 'X'                                                 237
         CALL AXPOSITION                                                  237
C    *                                                                    237
C    Geant POSITION : MIGG ===> X=-1.0*MTDD_MTDIGSTACKTHINGLASSTHICK-1.0  237
C    *MTDD_MTDIGSTACKGAPTHICK <===                                        237
         CALL AGSRESET                                                    238
         AG_X =                                                           238
     *   -1.0*MTDD_MTDIGSTACKTHINGLASSTHICK-1.0*MTDD_MTDIGSTACKGAPTHICK   238
         AG_TITLE = 'MIGG'                                                238
         AG_EXNAME = 'POSITION'                                           238
         AG_PARLIST = 'X'                                                 238
         CALL AXPOSITION                                                  238
C    *                                                                    238
C    Geant POSITION : MIGG ===> X=0.0*MTDD_MTDIGSTACKTHINGLASSTHICK+0.0*  238
C    MTDD_MTDIGSTACKGAPTHICK <===                                         238
         CALL AGSRESET                                                    239
         AG_X =                                                           239
     *   0.0*MTDD_MTDIGSTACKTHINGLASSTHICK+0.0*MTDD_MTDIGSTACKGAPTHICK    239
         AG_TITLE = 'MIGG'                                                239
         AG_EXNAME = 'POSITION'                                           239
         AG_PARLIST = 'X'                                                 239
         CALL AXPOSITION                                                  239
C    *                                                                    239
C    Geant POSITION : MIGG ===> X=1.0*MTDD_MTDIGSTACKTHINGLASSTHICK+1.0*  239
C    MTDD_MTDIGSTACKGAPTHICK <===                                         239
         CALL AGSRESET                                                    240
         AG_X =                                                           240
     *   1.0*MTDD_MTDIGSTACKTHINGLASSTHICK+1.0*MTDD_MTDIGSTACKGAPTHICK    240
         AG_TITLE = 'MIGG'                                                240
         AG_EXNAME = 'POSITION'                                           240
         AG_PARLIST = 'X'                                                 240
         CALL AXPOSITION                                                  240
C    *                                                                    240
C    Geant POSITION : MIGG ===> X=2.0*MTDD_MTDIGSTACKTHINGLASSTHICK+2.0*  240
C    MTDD_MTDIGSTACKGAPTHICK <===                                         240
         CALL AGSRESET                                                    241
         AG_X =                                                           241
     *   2.0*MTDD_MTDIGSTACKTHINGLASSTHICK+2.0*MTDD_MTDIGSTACKGAPTHICK    241
         AG_TITLE = 'MIGG'                                                241
         AG_EXNAME = 'POSITION'                                           241
         AG_PARLIST = 'X'                                                 241
         CALL AXPOSITION                                                  241
C    *                                                                    241
         ENDIF                                                            241
5321  CALL AGSPOPB                                                        243
      GO TO BLOCK_MIGS, (5320 ,5330)                                      243
C End of Local Procedure                                                  243
C ----------------------------------------------------------------------  244
C ------------ MIGG                                                       244
C Begin Local Procedure MIGG                                              245
C MIGG IS A SINGLE GAS GAP                                                245
5340  CONTINUE                                                            245
         AG_TITLE='MIGG'                                                  245
         AG_MODULE=MODULE                                                 245
         AG_MARK='V00'                                                    245
         CALL AGSPUSH                                                     245
         AG_MODULE=MODULE                                                 245
C    *                                                                    245
C    Geant ATTRIBUTE : MIGG ===> SEEN=0 COLO=7 <===                       246
         CALL AGSRESET                                                    246
         AG_SEEN = 0                                                      246
         AG_COLO = 7                                                      246
         AG_TITLE = 'MIGG'                                                246
         AG_EXNAME = 'ATTRIBUTE'                                          246
         AG_PARLIST = 'SEEN_COLO'                                         246
         CALL AXATTRIBUTE                                                 246
C    *                                                                    246
C    Geant MATERIAL : RPCGAS ===> inherited arguments used <===           246
         CALL AGSRESET                                                    247
         AG_TITLE = 'RPCGAS'                                              247
         AG_EXNAME = 'MATERIAL'                                           247
         AG_PARLIST = 'NONE'                                              247
         CALL AXMATERIAL                                                  247
C    *                                                                    247
C    Geant MEDIUM : RPCGAS_SENSITIVE ===> ISVOL=1 <===                    247
         CALL AGSRESET                                                    248
         AG_ISVOL = 1                                                     248
         AG_TITLE = 'RPCGAS_SENSITIVE'                                    248
         AG_EXNAME = 'MEDIUM'                                             248
         AG_PARLIST = 'ISVOL'                                             248
         CALL AXMEDIUM                                                    248
C    *                                                                    248
C    Geant SHAPE : BOX ===> inherited arguments used <===                 248
         CALL AGSRESET                                                    249
         AG_TITLE = 'BOX'                                                 249
         AG_EXNAME = 'SHAPE'                                              249
         AG_PARLIST = 'NONE'                                              249
         CALL AXSHAPE                                                     249
C    *                                                                    249
C    Defining Hits: X:.01:S Y:.01: Z:.01: PTOT:18:(0,100) CX:10: CY:10:   249
C    CZ:10: SLENG:.1:(0,500), TOF:16:(0,1.0E-7) STEP:.01: ELOSS:16:(0,1.  249
C    0E-6)                                                                249
         AG_TITLE = 'MIGG'                                                251
         AG_EXNAME = 'HITS'                                               251
         AG_TYPE=AG_MAGIC                                                 251
         CALL AGSDETV('MIGG')                                             251
C    *                                                                    251
         AG_PARLIST='X'                                                   251
         AG_TITLE = 'DETH'                                                251
         AG_NBITS = AG_MAGIC                                              251
         AG_BIN=.01                                                       251
         AG_OPTION='S'                                                    251
         AG_FHMIN = AG_MAGIC                                              251
         AG_FHMAX = AG_MAGIC                                              251
         CALL AGSDETH (0)                                                 251
C    *                                                                    251
         AG_PARLIST='Y'                                                   251
         AG_TITLE = 'DETH'                                                251
         AG_NBITS = AG_MAGIC                                              251
         AG_BIN=.01                                                       251
         AG_FHMIN=AG_MAGIC                                                251
         AG_FHMAX=AG_MAGIC                                                251
         AG_OPTION='*'                                                    251
         CALL AGSDETH (0)                                                 251
C    *                                                                    251
         AG_PARLIST='Z'                                                   251
         AG_TITLE = 'DETH'                                                251
         AG_NBITS = AG_MAGIC                                              251
         AG_BIN=.01                                                       251
         AG_FHMIN=AG_MAGIC                                                251
         AG_FHMAX=AG_MAGIC                                                251
         AG_OPTION='*'                                                    251
         CALL AGSDETH (0)                                                 251
C    *                                                                    251
         AG_PARLIST='PTOT'                                                251
         AG_TITLE = 'DETH'                                                251
         AG_NBITS = 18                                                    251
         AG_BIN=AG_MAGIC                                                  251
         AG_OPTION='*'                                                    251
         AG_FHMIN = 0                                                     251
         AG_FHMAX = 100                                                   251
         CALL AGSDETH (0)                                                 251
C    *                                                                    251
         AG_PARLIST='CX'                                                  251
         AG_TITLE = 'DETH'                                                251
         AG_NBITS = 10                                                    251
         AG_BIN=AG_MAGIC                                                  251
         AG_FHMIN=AG_MAGIC                                                251
         AG_FHMAX=AG_MAGIC                                                251
         AG_OPTION='*'                                                    251
         CALL AGSDETH (0)                                                 251
C    *                                                                    251
         AG_PARLIST='CY'                                                  251
         AG_TITLE = 'DETH'                                                251
         AG_NBITS = 10                                                    251
         AG_BIN=AG_MAGIC                                                  251
         AG_FHMIN=AG_MAGIC                                                251
         AG_FHMAX=AG_MAGIC                                                251
         AG_OPTION='*'                                                    251
         CALL AGSDETH (0)                                                 251
C    *                                                                    251
         AG_PARLIST='CZ'                                                  251
         AG_TITLE = 'DETH'                                                251
         AG_NBITS = 10                                                    251
         AG_BIN=AG_MAGIC                                                  251
         AG_FHMIN=AG_MAGIC                                                251
         AG_FHMAX=AG_MAGIC                                                251
         AG_OPTION='*'                                                    251
         CALL AGSDETH (0)                                                 251
C    *                                                                    251
         AG_PARLIST='SLENG'                                               251
         AG_TITLE = 'DETH'                                                251
         AG_NBITS = AG_MAGIC                                              251
         AG_BIN=.1                                                        251
         AG_OPTION='*'                                                    251
         AG_FHMIN = 0                                                     251
         AG_FHMAX = 500                                                   251
         CALL AGSDETH (0)                                                 251
C    *                                                                    251
         AG_PARLIST='TOF'                                                 251
         AG_TITLE = 'DETH'                                                251
         AG_NBITS = 16                                                    251
         AG_BIN=AG_MAGIC                                                  251
         AG_OPTION='*'                                                    251
         AG_FHMIN = 0                                                     251
         AG_FHMAX = 1.0E-7                                                251
         CALL AGSDETH (0)                                                 251
C    *                                                                    251
         AG_PARLIST='STEP'                                                251
         AG_TITLE = 'DETH'                                                251
         AG_NBITS = AG_MAGIC                                              251
         AG_BIN=.01                                                       251
         AG_FHMIN=AG_MAGIC                                                251
         AG_FHMAX=AG_MAGIC                                                251
         AG_OPTION='*'                                                    251
         CALL AGSDETH (0)                                                 251
C    *                                                                    251
         AG_PARLIST='ELOSS'                                               251
         AG_TITLE = 'DETH'                                                251
         AG_NBITS = 16                                                    251
         AG_BIN=AG_MAGIC                                                  251
         AG_OPTION='*'                                                    251
         AG_FHMIN = 0                                                     251
         AG_FHMAX = 1.0E-6                                                251
         CALL AGSDETH (0)                                                 251
C    *                                                                    251
         CALL AGSDETU('MIGG')                                             251
C    *                                                                    251
5341  CALL AGSPOPB                                                        252
      GO TO BLOCK_MIGG, (5340 ,5350)                                      252
C End of Local Procedure                                                  252
C ----------------------------------------------------------------------  253
C ------------ MTBP                                                       253
C Begin Local Procedure MTBP                                              254
C MTBP IS THE MTD11 BOTTOM PLATE                                          254
5240  CONTINUE                                                            254
         AG_TITLE='MTBP'                                                  254
         AG_MODULE=MODULE                                                 254
         AG_MARK='V00'                                                    254
         CALL AGSPUSH                                                     254
         AG_MODULE=MODULE                                                 254
C    *                                                                    254
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        255
         CALL AGSRESET                                                    255
         AG_TITLE = 'ALUMINIUM'                                           255
         AG_EXNAME = 'MATERIAL'                                           255
         AG_PARLIST = 'NONE'                                              255
         CALL AXMATERIAL                                                  255
C    *                                                                    255
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           255
         CALL AGSRESET                                                    256
         AG_TITLE = 'STANDARD'                                            256
         AG_EXNAME = 'MEDIUM'                                             256
         AG_PARLIST = 'NONE'                                              256
         CALL AXMEDIUM                                                    256
C    *                                                                    256
C    Geant ATTRIBUTE : MTBP ===> SEEN=1 COLO=6 <===                       256
         CALL AGSRESET                                                    257
         AG_SEEN = 1                                                      257
         AG_COLO = 6                                                      257
         AG_TITLE = 'MTBP'                                                257
         AG_EXNAME = 'ATTRIBUTE'                                          257
         AG_PARLIST = 'SEEN_COLO'                                         257
         CALL AXATTRIBUTE                                                 257
C    *                                                                    257
C    Geant SHAPE : BOX ===> inherited arguments used <===                 257
         CALL AGSRESET                                                    258
         AG_TITLE = 'BOX'                                                 258
         AG_EXNAME = 'SHAPE'                                              258
         AG_PARLIST = 'NONE'                                              258
         CALL AXSHAPE                                                     258
C    *                                                                    258
5241  CALL AGSPOPB                                                        259
      GO TO BLOCK_MTBP, (5240 ,5250)                                      259
C End of Local Procedure                                                  259
C ----------------------------------------------------------------------  260
C ------------ MTTP                                                       260
C Begin Local Procedure MTTP                                              261
C MTTP IS THE MTD11 TOP PLATE                                             261
5260  CONTINUE                                                            261
         AG_TITLE='MTTP'                                                  261
         AG_MODULE=MODULE                                                 261
         AG_MARK='V00'                                                    261
         CALL AGSPUSH                                                     261
         AG_MODULE=MODULE                                                 261
C    *                                                                    261
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        262
         CALL AGSRESET                                                    262
         AG_TITLE = 'ALUMINIUM'                                           262
         AG_EXNAME = 'MATERIAL'                                           262
         AG_PARLIST = 'NONE'                                              262
         CALL AXMATERIAL                                                  262
C    *                                                                    262
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           262
         CALL AGSRESET                                                    263
         AG_TITLE = 'STANDARD'                                            263
         AG_EXNAME = 'MEDIUM'                                             263
         AG_PARLIST = 'NONE'                                              263
         CALL AXMEDIUM                                                    263
C    *                                                                    263
C    Geant ATTRIBUTE : MTTP ===> SEEN=1 COLO=6 <===                       263
         CALL AGSRESET                                                    264
         AG_SEEN = 1                                                      264
         AG_COLO = 6                                                      264
         AG_TITLE = 'MTTP'                                                264
         AG_EXNAME = 'ATTRIBUTE'                                          264
         AG_PARLIST = 'SEEN_COLO'                                         264
         CALL AXATTRIBUTE                                                 264
C    *                                                                    264
C    Geant SHAPE : BOX ===> inherited arguments used <===                 264
         CALL AGSRESET                                                    265
         AG_TITLE = 'BOX'                                                 265
         AG_EXNAME = 'SHAPE'                                              265
         AG_PARLIST = 'NONE'                                              265
         CALL AXSHAPE                                                     265
C    *                                                                    265
5261  CALL AGSPOPB                                                        266
      GO TO BLOCK_MTTP, (5260 ,5270)                                      266
C End of Local Procedure                                                  266
C ----------------------------------------------------------------------  267
C ------------ MCHL                                                       267
C Begin Local Procedure MCHL                                              268
C MCHL IS THE MTD11 ARCHITECTURAL CHANNEL LONG SIDE                       268
5280  CONTINUE                                                            268
         AG_TITLE='MCHL'                                                  268
         AG_MODULE=MODULE                                                 268
         AG_MARK='V00'                                                    268
         CALL AGSPUSH                                                     268
         AG_MODULE=MODULE                                                 268
C    *                                                                    268
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        269
         CALL AGSRESET                                                    269
         AG_TITLE = 'ALUMINIUM'                                           269
         AG_EXNAME = 'MATERIAL'                                           269
         AG_PARLIST = 'NONE'                                              269
         CALL AXMATERIAL                                                  269
C    *                                                                    269
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           269
         CALL AGSRESET                                                    270
         AG_TITLE = 'STANDARD'                                            270
         AG_EXNAME = 'MEDIUM'                                             270
         AG_PARLIST = 'NONE'                                              270
         CALL AXMEDIUM                                                    270
C    *                                                                    270
C    Geant ATTRIBUTE : MCHL ===> SEEN=1 COLO=6 <===                       270
         CALL AGSRESET                                                    271
         AG_SEEN = 1                                                      271
         AG_COLO = 6                                                      271
         AG_TITLE = 'MCHL'                                                271
         AG_EXNAME = 'ATTRIBUTE'                                          271
         AG_PARLIST = 'SEEN_COLO'                                         271
         CALL AXATTRIBUTE                                                 271
C    *                                                                    271
C    Geant SHAPE : BOX ===> inherited arguments used <===                 271
         CALL AGSRESET                                                    272
         AG_TITLE = 'BOX'                                                 272
         AG_EXNAME = 'SHAPE'                                              272
         AG_PARLIST = 'NONE'                                              272
         CALL AXSHAPE                                                     272
C    *                                                                    272
         AG_DZ = MTDD_MTDTRAYDZ-2.*CHANDIM                                272
         AG_DX = CHANDIM-0.3175                                           273
         AG_DY = CHANDIM-0.3175/2.                                        273
C    Create MCAL :                                                        273
C    Call To Local Procedure MCAL                                         273
         ASSIGN 5370 TO BLOCK_MCAL                                        273
         GO TO  5360                                                      273
5370     CONTINUE                                                         273
C    *                                                                    273
C    Geant POSITION : MCAL ===> X=0 Y=0.3175/2. Z=0 <===                  274
         CALL AGSRESET                                                    274
         AG_X = 0                                                         274
         AG_Y = 0.3175/2.                                                 274
         AG_Z = 0                                                         274
         AG_TITLE = 'MCAL'                                                274
         AG_EXNAME = 'POSITION'                                           274
         AG_PARLIST = 'X_Y_Z'                                             274
         CALL AXPOSITION                                                  274
C    *                                                                    274
5281  CALL AGSPOPB                                                        275
      GO TO BLOCK_MCHL, (5280 ,5290)                                      275
C End of Local Procedure                                                  275
C ----------------------------------------------------------------------  276
C ------------ MCAL                                                       276
C Begin Local Procedure MCAL                                              277
C MCAL IS THE AIR IN THE MTD11 ARCHITECTURAL CHANNEL LONG SIDE            277
5360  CONTINUE                                                            277
         AG_TITLE='MCAL'                                                  277
         AG_MODULE=MODULE                                                 277
         AG_MARK='V00'                                                    277
         CALL AGSPUSH                                                     277
         AG_MODULE=MODULE                                                 277
C    *                                                                    277
C    Geant MATERIAL : AIR ===> inherited arguments used <===              278
         CALL AGSRESET                                                    278
         AG_TITLE = 'AIR'                                                 278
         AG_EXNAME = 'MATERIAL'                                           278
         AG_PARLIST = 'NONE'                                              278
         CALL AXMATERIAL                                                  278
C    *                                                                    278
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           278
         CALL AGSRESET                                                    279
         AG_TITLE = 'STANDARD'                                            279
         AG_EXNAME = 'MEDIUM'                                             279
         AG_PARLIST = 'NONE'                                              279
         CALL AXMEDIUM                                                    279
C    *                                                                    279
C    Geant ATTRIBUTE : MCAL ===> SEEN=1 COLO=7 <===                       279
         CALL AGSRESET                                                    280
         AG_SEEN = 1                                                      280
         AG_COLO = 7                                                      280
         AG_TITLE = 'MCAL'                                                280
         AG_EXNAME = 'ATTRIBUTE'                                          280
         AG_PARLIST = 'SEEN_COLO'                                         280
         CALL AXATTRIBUTE                                                 280
C    *                                                                    280
C    Geant SHAPE : BOX ===> inherited arguments used <===                 280
         CALL AGSRESET                                                    281
         AG_TITLE = 'BOX'                                                 281
         AG_EXNAME = 'SHAPE'                                              281
         AG_PARLIST = 'NONE'                                              281
         CALL AXSHAPE                                                     281
C    *                                                                    281
5361  CALL AGSPOPB                                                        282
      GO TO BLOCK_MCAL, (5360 ,5370)                                      282
C End of Local Procedure                                                  282
C ----------------------------------------------------------------------  283
C ------------ MCHS                                                       283
C Begin Local Procedure MCHS                                              284
C MCHS IS THE MTD11 ARCHITECTURAL CHANNEL SHORT SIDE                      284
5300  CONTINUE                                                            284
         AG_TITLE='MCHS'                                                  284
         AG_MODULE=MODULE                                                 284
         AG_MARK='V00'                                                    284
         CALL AGSPUSH                                                     284
         AG_MODULE=MODULE                                                 284
C    *                                                                    284
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        285
         CALL AGSRESET                                                    285
         AG_TITLE = 'ALUMINIUM'                                           285
         AG_EXNAME = 'MATERIAL'                                           285
         AG_PARLIST = 'NONE'                                              285
         CALL AXMATERIAL                                                  285
C    *                                                                    285
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           285
         CALL AGSRESET                                                    286
         AG_TITLE = 'STANDARD'                                            286
         AG_EXNAME = 'MEDIUM'                                             286
         AG_PARLIST = 'NONE'                                              286
         CALL AXMEDIUM                                                    286
C    *                                                                    286
C    Geant ATTRIBUTE : MCHS ===> SEEN=1 COLO=6 <===                       286
         CALL AGSRESET                                                    287
         AG_SEEN = 1                                                      287
         AG_COLO = 6                                                      287
         AG_TITLE = 'MCHS'                                                287
         AG_EXNAME = 'ATTRIBUTE'                                          287
         AG_PARLIST = 'SEEN_COLO'                                         287
         CALL AXATTRIBUTE                                                 287
C    *                                                                    287
C    Geant SHAPE : BOX ===> inherited arguments used <===                 287
         CALL AGSRESET                                                    288
         AG_TITLE = 'BOX'                                                 288
         AG_EXNAME = 'SHAPE'                                              288
         AG_PARLIST = 'NONE'                                              288
         CALL AXSHAPE                                                     288
C    *                                                                    288
         AG_DZ = CHANDIM-0.3175/2.                                        288
         AG_DX = CHANDIM-0.3175                                           289
         AG_DY = MTDD_MTDTRAYDY                                           289
C    Create MCAS :                                                        289
C    Call To Local Procedure MCAS                                         289
         ASSIGN 5390 TO BLOCK_MCAS                                        289
         GO TO  5380                                                      289
5390     CONTINUE                                                         289
C    *                                                                    289
C    Geant POSITION : MCAS ===> X=0 Y=0 Z=+0.3175/2. <===                 290
         CALL AGSRESET                                                    290
         AG_X = 0                                                         290
         AG_Y = 0                                                         290
         AG_Z = +0.3175/2.                                                290
         AG_TITLE = 'MCAS'                                                290
         AG_EXNAME = 'POSITION'                                           290
         AG_PARLIST = 'X_Y_Z'                                             290
         CALL AXPOSITION                                                  290
C    *                                                                    290
5301  CALL AGSPOPB                                                        291
      GO TO BLOCK_MCHS, (5300 ,5310)                                      291
C End of Local Procedure                                                  291
C ----------------------------------------------------------------------  292
C ------------ MCAS                                                       292
C Begin Local Procedure MCAS                                              293
C MCAS IS THE AIR IN THE MTD11 ARCHITECTURAL CHANNEL SHORT SIDE           293
5380  CONTINUE                                                            293
         AG_TITLE='MCAS'                                                  293
         AG_MODULE=MODULE                                                 293
         AG_MARK='V00'                                                    293
         CALL AGSPUSH                                                     293
         AG_MODULE=MODULE                                                 293
C    *                                                                    293
C    Geant MATERIAL : AIR ===> inherited arguments used <===              294
         CALL AGSRESET                                                    294
         AG_TITLE = 'AIR'                                                 294
         AG_EXNAME = 'MATERIAL'                                           294
         AG_PARLIST = 'NONE'                                              294
         CALL AXMATERIAL                                                  294
C    *                                                                    294
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           294
         CALL AGSRESET                                                    295
         AG_TITLE = 'STANDARD'                                            295
         AG_EXNAME = 'MEDIUM'                                             295
         AG_PARLIST = 'NONE'                                              295
         CALL AXMEDIUM                                                    295
C    *                                                                    295
C    Geant ATTRIBUTE : MCAS ===> SEEN=1 COLO=7 <===                       295
         CALL AGSRESET                                                    296
         AG_SEEN = 1                                                      296
         AG_COLO = 7                                                      296
         AG_TITLE = 'MCAS'                                                296
         AG_EXNAME = 'ATTRIBUTE'                                          296
         AG_PARLIST = 'SEEN_COLO'                                         296
         CALL AXATTRIBUTE                                                 296
C    *                                                                    296
C    Geant SHAPE : BOX ===> inherited arguments used <===                 296
         CALL AGSRESET                                                    297
         AG_TITLE = 'BOX'                                                 297
         AG_EXNAME = 'SHAPE'                                              297
         AG_PARLIST = 'NONE'                                              297
         CALL AXSHAPE                                                     297
C    *                                                                    297
5381  CALL AGSPOPB                                                        298
      GO TO BLOCK_MCAS, (5380 ,5390)                                      298
C End of Local Procedure                                                  298
      WRITE(LOUT,5400)'===>>  [End AgML Module MUTDGEO5]  <<==='          298
5400  FORMAT(A32,/,/)                                                     298
C MODULE MUTDGEO5                                                         300
      END                                                                 300
