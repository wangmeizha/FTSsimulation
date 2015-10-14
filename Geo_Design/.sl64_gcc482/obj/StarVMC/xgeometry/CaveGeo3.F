C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE CAVEGEO3                                                 1
C CAVEGEO3 STAR WIDE ANGLE HALL GEOMETRY.                                 1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'CAVEGEO3'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'STAR WIDE ANGLE HALL GEOMETRY.'/                     1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C AUTHOR JASON WEBB, YURI FISYAK                                          1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'JASON WEBB, YURI FISYAK'/                     2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C CREATED MAY 2013                                                        2
      CHARACTER*40 CREATED /'MAY 2013'/                                   3
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
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            4
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      4
      COMMON/GCMAIL/ CHMAIL                                               4
      CHARACTER*132 CHMAIL                                                4
      INTEGER BLOCK_HALL                                                  4
      INTEGER BLOCK_CAVE                                                  5
      INTEGER BLOCK_WALL                                                  5
      INTEGER BLOCK_HOLE                                                  5
      INTEGER BLOCK_SHLD                                                  5
      INTEGER BLOCK_CRAT                                                  5
      INTEGER BLOCK_MGWP                                                  5
      INTEGER BLOCK_MGWT                                                  5
C -                                                                       6
      REAL CVCF_VERSION                                                   6
      EQUIVALENCE (BANK_CVCF(3),CVCF_VERSION,RE_CVCF_VERSION),            6
     *(NAMES_CVCF(1),NCVCF_VERSION)                                       6
      EQUIVALENCE (COMMEN_CVCF(1),CCVCF_VERSION),(MAP_CVCF(1,1),          6
     *MCVCF_VERSION)                                                      6
      CHARACTER CCVCF_VERSION*40/' '/                                     6
      CHARACTER NCVCF_VERSION*12/'R.VERSION'/                             6
      INTEGER MCVCF_VERSION(2)/1,1/                                       6
      REAL RE_CVCF_VERSION                                                6
C -                                                                       6
      INTEGER CVCF_CONFIG                                                 6
      EQUIVALENCE (BANK_CVCF(4),CVCF_CONFIG,RE_CVCF_CONFIG),              6
     *(NAMES_CVCF(2),NCVCF_CONFIG)                                        6
      EQUIVALENCE (COMMEN_CVCF(2),CCVCF_CONFIG),(MAP_CVCF(1,2),           6
     *MCVCF_CONFIG)                                                       6
      CHARACTER CCVCF_CONFIG*40/' '/                                      6
      CHARACTER NCVCF_CONFIG*12/'I.CONFIG'/                               6
      INTEGER MCVCF_CONFIG(2)/1,1/                                        6
      REAL RE_CVCF_CONFIG                                                 6
C -                                                                       6
      INTEGER CVCF_TPCREFSYS                                              6
      EQUIVALENCE (BANK_CVCF(5),CVCF_TPCREFSYS,RE_CVCF_TPCREFSYS),        6
     *(NAMES_CVCF(3),NCVCF_TPCREFSYS)                                     6
      EQUIVALENCE (COMMEN_CVCF(3),CCVCF_TPCREFSYS),(MAP_CVCF(1,3),        6
     *MCVCF_TPCREFSYS)                                                    6
      CHARACTER CCVCF_TPCREFSYS*40/' '/                                   6
      CHARACTER NCVCF_TPCREFSYS*12/'I.TPCREFSYS'/                         6
      INTEGER MCVCF_TPCREFSYS(2)/1,1/                                     6
      REAL RE_CVCF_TPCREFSYS                                              6
C -                                                                       6
      INTEGER LEN_CVCF,LLEN_CVCF,LINK_CVCF                                6
      PARAMETER (LEN_CVCF=5)                                              6
      PARAMETER (LLEN_CVCF=3)                                             6
      INTEGER FLAG_CVCF/0/,MAP_CVCF(2,LLEN_CVCF),IDNUM_CVCF(10)           6
      CHARACTER NAMES_CVCF(LLEN_CVCF)*12,COMMEN_CVCF(LLEN_CVCF)*40        6
      DATA IDNUM_CVCF/10*0/                                               6
      REAL BANK_CVCF(LEN_CVCF)                                            6
      CHARACTER*60 BPATH_CVCF/' '/                                        6
      SAVE FLAG_CVCF,MAP_CVCF,IDNUM_CVCF,NAMES_CVCF,COMMEN_CVCF,          6
     *BPATH_CVCF                                                          6
      DATA LINK_CVCF/0/,BANK_CVCF/LEN_CVCF*0/                             6
C *                                                                       6
C -                                                                       7
      REAL CAVE_VERSION                                                   7
      EQUIVALENCE (BANK_CAVE(3),CAVE_VERSION,RE_CAVE_VERSION),            7
     *(NAMES_CAVE(1),NCAVE_VERSION)                                       7
      EQUIVALENCE (COMMEN_CAVE(1),CCAVE_VERSION),(MAP_CAVE(1,1),          7
     *MCAVE_VERSION)                                                      7
      CHARACTER CCAVE_VERSION*40/' '/                                     7
      CHARACTER NCAVE_VERSION*12/'R.VERSION'/                             7
      INTEGER MCAVE_VERSION(2)/1,1/                                       7
      REAL RE_CAVE_VERSION                                                7
C -                                                                       7
      REAL CAVE_RMIN                                                      7
      EQUIVALENCE (BANK_CAVE(4),CAVE_RMIN,RE_CAVE_RMIN),(NAMES_CAVE(2),   7
     *NCAVE_RMIN)                                                         7
      EQUIVALENCE (COMMEN_CAVE(2),CCAVE_RMIN),(MAP_CAVE(1,2),             7
     *MCAVE_RMIN)                                                         7
      CHARACTER CCAVE_RMIN*40/' '/                                        7
      CHARACTER NCAVE_RMIN*12/'R.RMIN'/                                   7
      INTEGER MCAVE_RMIN(2)/1,1/                                          7
      REAL RE_CAVE_RMIN                                                   7
C -                                                                       7
      REAL CAVE_RMAX(2)                                                   7
      EQUIVALENCE (BANK_CAVE(5),CAVE_RMAX,RE_CAVE_RMAX),(NAMES_CAVE(3),   7
     *NCAVE_RMAX)                                                         7
      EQUIVALENCE (COMMEN_CAVE(3),CCAVE_RMAX),(MAP_CAVE(1,3),             7
     *MCAVE_RMAX)                                                         7
      CHARACTER CCAVE_RMAX*40/' '/                                        7
      CHARACTER NCAVE_RMAX*12/'R.RMAX'/                                   7
      INTEGER MCAVE_RMAX(2)/2,1/                                          7
      REAL RE_CAVE_RMAX                                                   7
C -                                                                       7
      REAL CAVE_DZ(2)                                                     7
      EQUIVALENCE (BANK_CAVE(7),CAVE_DZ,RE_CAVE_DZ),(NAMES_CAVE(4),       7
     *NCAVE_DZ)                                                           7
      EQUIVALENCE (COMMEN_CAVE(4),CCAVE_DZ),(MAP_CAVE(1,4),MCAVE_DZ)      7
      CHARACTER CCAVE_DZ*40/' '/                                          7
      CHARACTER NCAVE_DZ*12/'R.DZ'/                                       7
      INTEGER MCAVE_DZ(2)/2,1/                                            7
      REAL RE_CAVE_DZ                                                     7
C -                                                                       7
      REAL CAVE_DCONC                                                     7
      EQUIVALENCE (BANK_CAVE(9),CAVE_DCONC,RE_CAVE_DCONC),                7
     *(NAMES_CAVE(5),NCAVE_DCONC)                                         7
      EQUIVALENCE (COMMEN_CAVE(5),CCAVE_DCONC),(MAP_CAVE(1,5),            7
     *MCAVE_DCONC)                                                        7
      CHARACTER CCAVE_DCONC*40/' '/                                       7
      CHARACTER NCAVE_DCONC*12/'R.DCONC'/                                 7
      INTEGER MCAVE_DCONC(2)/1,1/                                         7
      REAL RE_CAVE_DCONC                                                  7
C -                                                                       7
      INTEGER LEN_CAVE,LLEN_CAVE,LINK_CAVE                                7
      PARAMETER (LEN_CAVE=9)                                              7
      PARAMETER (LLEN_CAVE=5)                                             7
      INTEGER FLAG_CAVE/0/,MAP_CAVE(2,LLEN_CAVE),IDNUM_CAVE(10)           7
      CHARACTER NAMES_CAVE(LLEN_CAVE)*12,COMMEN_CAVE(LLEN_CAVE)*40        7
      DATA IDNUM_CAVE/10*0/                                               7
      REAL BANK_CAVE(LEN_CAVE)                                            7
      CHARACTER*60 BPATH_CAVE/' '/                                        7
      SAVE FLAG_CAVE,MAP_CAVE,IDNUM_CAVE,NAMES_CAVE,COMMEN_CAVE,          7
     *BPATH_CAVE                                                          7
      DATA LINK_CAVE/0/,BANK_CAVE/LEN_CAVE*0/                             7
C *                                                                       7
C -                                                                       8
      REAL CDIM_VERSION                                                   8
      EQUIVALENCE (BANK_CDIM(3),CDIM_VERSION,RE_CDIM_VERSION),            8
     *(NAMES_CDIM(1),NCDIM_VERSION)                                       8
      EQUIVALENCE (COMMEN_CDIM(1),CCDIM_VERSION),(MAP_CDIM(1,1),          8
     *MCDIM_VERSION)                                                      8
      CHARACTER CCDIM_VERSION*40/' '/                                     8
      CHARACTER NCDIM_VERSION*12/'R.VERSION'/                             8
      INTEGER MCDIM_VERSION(2)/1,1/                                       8
      REAL RE_CDIM_VERSION                                                8
C -                                                                       8
      REAL CDIM_DCEILING                                                  8
      EQUIVALENCE (BANK_CDIM(4),CDIM_DCEILING,RE_CDIM_DCEILING),          8
     *(NAMES_CDIM(2),NCDIM_DCEILING)                                      8
      EQUIVALENCE (COMMEN_CDIM(2),CCDIM_DCEILING),(MAP_CDIM(1,2),         8
     *MCDIM_DCEILING)                                                     8
      CHARACTER CCDIM_DCEILING*40/' '/                                    8
      CHARACTER NCDIM_DCEILING*12/'R.DCEILING'/                           8
      INTEGER MCDIM_DCEILING(2)/1,1/                                      8
      REAL RE_CDIM_DCEILING                                               8
C -                                                                       8
      REAL CDIM_DFLOOR                                                    8
      EQUIVALENCE (BANK_CDIM(5),CDIM_DFLOOR,RE_CDIM_DFLOOR),              8
     *(NAMES_CDIM(3),NCDIM_DFLOOR)                                        8
      EQUIVALENCE (COMMEN_CDIM(3),CCDIM_DFLOOR),(MAP_CDIM(1,3),           8
     *MCDIM_DFLOOR)                                                       8
      CHARACTER CCDIM_DFLOOR*40/' '/                                      8
      CHARACTER NCDIM_DFLOOR*12/'R.DFLOOR'/                               8
      INTEGER MCDIM_DFLOOR(2)/1,1/                                        8
      REAL RE_CDIM_DFLOOR                                                 8
C -                                                                       8
      REAL CDIM_DNORTH                                                    8
      EQUIVALENCE (BANK_CDIM(6),CDIM_DNORTH,RE_CDIM_DNORTH),              8
     *(NAMES_CDIM(4),NCDIM_DNORTH)                                        8
      EQUIVALENCE (COMMEN_CDIM(4),CCDIM_DNORTH),(MAP_CDIM(1,4),           8
     *MCDIM_DNORTH)                                                       8
      CHARACTER CCDIM_DNORTH*40/' '/                                      8
      CHARACTER NCDIM_DNORTH*12/'R.DNORTH'/                               8
      INTEGER MCDIM_DNORTH(2)/1,1/                                        8
      REAL RE_CDIM_DNORTH                                                 8
C -                                                                       8
      REAL CDIM_DSOUTH                                                    8
      EQUIVALENCE (BANK_CDIM(7),CDIM_DSOUTH,RE_CDIM_DSOUTH),              8
     *(NAMES_CDIM(5),NCDIM_DSOUTH)                                        8
      EQUIVALENCE (COMMEN_CDIM(5),CCDIM_DSOUTH),(MAP_CDIM(1,5),           8
     *MCDIM_DSOUTH)                                                       8
      CHARACTER CCDIM_DSOUTH*40/' '/                                      8
      CHARACTER NCDIM_DSOUTH*12/'R.DSOUTH'/                               8
      INTEGER MCDIM_DSOUTH(2)/1,1/                                        8
      REAL RE_CDIM_DSOUTH                                                 8
C -                                                                       8
      REAL CDIM_THICKCEIL                                                 8
      EQUIVALENCE (BANK_CDIM(8),CDIM_THICKCEIL,RE_CDIM_THICKCEIL),        8
     *(NAMES_CDIM(6),NCDIM_THICKCEIL)                                     8
      EQUIVALENCE (COMMEN_CDIM(6),CCDIM_THICKCEIL),(MAP_CDIM(1,6),        8
     *MCDIM_THICKCEIL)                                                    8
      CHARACTER CCDIM_THICKCEIL*40/' '/                                   8
      CHARACTER NCDIM_THICKCEIL*12/'R.THICKCEIL'/                         8
      INTEGER MCDIM_THICKCEIL(2)/1,1/                                     8
      REAL RE_CDIM_THICKCEIL                                              8
C -                                                                       8
      REAL CDIM_RMAX                                                      8
      EQUIVALENCE (BANK_CDIM(9),CDIM_RMAX,RE_CDIM_RMAX),(NAMES_CDIM(7),   8
     *NCDIM_RMAX)                                                         8
      EQUIVALENCE (COMMEN_CDIM(7),CCDIM_RMAX),(MAP_CDIM(1,7),             8
     *MCDIM_RMAX)                                                         8
      CHARACTER CCDIM_RMAX*40/' '/                                        8
      CHARACTER NCDIM_RMAX*12/'R.RMAX'/                                   8
      INTEGER MCDIM_RMAX(2)/1,1/                                          8
      REAL RE_CDIM_RMAX                                                   8
C -                                                                       8
      REAL CDIM_DZ                                                        8
      EQUIVALENCE (BANK_CDIM(10),CDIM_DZ,RE_CDIM_DZ),(NAMES_CDIM(8),      8
     *NCDIM_DZ)                                                           8
      EQUIVALENCE (COMMEN_CDIM(8),CCDIM_DZ),(MAP_CDIM(1,8),MCDIM_DZ)      8
      CHARACTER CCDIM_DZ*40/' '/                                          8
      CHARACTER NCDIM_DZ*12/'R.DZ'/                                       8
      INTEGER MCDIM_DZ(2)/1,1/                                            8
      REAL RE_CDIM_DZ                                                     8
C -                                                                       8
      INTEGER LEN_CDIM,LLEN_CDIM,LINK_CDIM                                8
      PARAMETER (LEN_CDIM=10)                                             8
      PARAMETER (LLEN_CDIM=8)                                             8
      INTEGER FLAG_CDIM/0/,MAP_CDIM(2,LLEN_CDIM),IDNUM_CDIM(10)           8
      CHARACTER NAMES_CDIM(LLEN_CDIM)*12,COMMEN_CDIM(LLEN_CDIM)*40        8
      DATA IDNUM_CDIM/10*0/                                               8
      REAL BANK_CDIM(LEN_CDIM)                                            8
      CHARACTER*60 BPATH_CDIM/' '/                                        8
      SAVE FLAG_CDIM,MAP_CDIM,IDNUM_CDIM,NAMES_CDIM,COMMEN_CDIM,          8
     *BPATH_CDIM                                                          8
      DATA LINK_CDIM/0/,BANK_CDIM/LEN_CDIM*0/                             8
C *                                                                       8
C -                                                                       9
      REAL TDIM_VERSION                                                   9
      EQUIVALENCE (BANK_TDIM(3),TDIM_VERSION,RE_TDIM_VERSION),            9
     *(NAMES_TDIM(1),NTDIM_VERSION)                                       9
      EQUIVALENCE (COMMEN_TDIM(1),CTDIM_VERSION),(MAP_TDIM(1,1),          9
     *MTDIM_VERSION)                                                      9
      CHARACTER CTDIM_VERSION*40/' '/                                     9
      CHARACTER NTDIM_VERSION*12/'R.VERSION'/                             9
      INTEGER MTDIM_VERSION(2)/1,1/                                       9
      REAL RE_TDIM_VERSION                                                9
C -                                                                       9
      REAL TDIM_DCEILING                                                  9
      EQUIVALENCE (BANK_TDIM(4),TDIM_DCEILING,RE_TDIM_DCEILING),          9
     *(NAMES_TDIM(2),NTDIM_DCEILING)                                      9
      EQUIVALENCE (COMMEN_TDIM(2),CTDIM_DCEILING),(MAP_TDIM(1,2),         9
     *MTDIM_DCEILING)                                                     9
      CHARACTER CTDIM_DCEILING*40/' '/                                    9
      CHARACTER NTDIM_DCEILING*12/'R.DCEILING'/                           9
      INTEGER MTDIM_DCEILING(2)/1,1/                                      9
      REAL RE_TDIM_DCEILING                                               9
C -                                                                       9
      REAL TDIM_DFLOOR                                                    9
      EQUIVALENCE (BANK_TDIM(5),TDIM_DFLOOR,RE_TDIM_DFLOOR),              9
     *(NAMES_TDIM(3),NTDIM_DFLOOR)                                        9
      EQUIVALENCE (COMMEN_TDIM(3),CTDIM_DFLOOR),(MAP_TDIM(1,3),           9
     *MTDIM_DFLOOR)                                                       9
      CHARACTER CTDIM_DFLOOR*40/' '/                                      9
      CHARACTER NTDIM_DFLOOR*12/'R.DFLOOR'/                               9
      INTEGER MTDIM_DFLOOR(2)/1,1/                                        9
      REAL RE_TDIM_DFLOOR                                                 9
C -                                                                       9
      REAL TDIM_DNORTH                                                    9
      EQUIVALENCE (BANK_TDIM(6),TDIM_DNORTH,RE_TDIM_DNORTH),              9
     *(NAMES_TDIM(4),NTDIM_DNORTH)                                        9
      EQUIVALENCE (COMMEN_TDIM(4),CTDIM_DNORTH),(MAP_TDIM(1,4),           9
     *MTDIM_DNORTH)                                                       9
      CHARACTER CTDIM_DNORTH*40/' '/                                      9
      CHARACTER NTDIM_DNORTH*12/'R.DNORTH'/                               9
      INTEGER MTDIM_DNORTH(2)/1,1/                                        9
      REAL RE_TDIM_DNORTH                                                 9
C -                                                                       9
      REAL TDIM_DSOUTH                                                    9
      EQUIVALENCE (BANK_TDIM(7),TDIM_DSOUTH,RE_TDIM_DSOUTH),              9
     *(NAMES_TDIM(5),NTDIM_DSOUTH)                                        9
      EQUIVALENCE (COMMEN_TDIM(5),CTDIM_DSOUTH),(MAP_TDIM(1,5),           9
     *MTDIM_DSOUTH)                                                       9
      CHARACTER CTDIM_DSOUTH*40/' '/                                      9
      CHARACTER NTDIM_DSOUTH*12/'R.DSOUTH'/                               9
      INTEGER MTDIM_DSOUTH(2)/1,1/                                        9
      REAL RE_TDIM_DSOUTH                                                 9
C -                                                                       9
      REAL TDIM_RMAX                                                      9
      EQUIVALENCE (BANK_TDIM(8),TDIM_RMAX,RE_TDIM_RMAX),(NAMES_TDIM(6),   9
     *NTDIM_RMAX)                                                         9
      EQUIVALENCE (COMMEN_TDIM(6),CTDIM_RMAX),(MAP_TDIM(1,6),             9
     *MTDIM_RMAX)                                                         9
      CHARACTER CTDIM_RMAX*40/' '/                                        9
      CHARACTER NTDIM_RMAX*12/'R.RMAX'/                                   9
      INTEGER MTDIM_RMAX(2)/1,1/                                          9
      REAL RE_TDIM_RMAX                                                   9
C -                                                                       9
      REAL TDIM_DZ                                                        9
      EQUIVALENCE (BANK_TDIM(9),TDIM_DZ,RE_TDIM_DZ),(NAMES_TDIM(7),       9
     *NTDIM_DZ)                                                           9
      EQUIVALENCE (COMMEN_TDIM(7),CTDIM_DZ),(MAP_TDIM(1,7),MTDIM_DZ)      9
      CHARACTER CTDIM_DZ*40/' '/                                          9
      CHARACTER NTDIM_DZ*12/'R.DZ'/                                       9
      INTEGER MTDIM_DZ(2)/1,1/                                            9
      REAL RE_TDIM_DZ                                                     9
C -                                                                       9
      INTEGER LEN_TDIM,LLEN_TDIM,LINK_TDIM                                9
      PARAMETER (LEN_TDIM=9)                                              9
      PARAMETER (LLEN_TDIM=7)                                             9
      INTEGER FLAG_TDIM/0/,MAP_TDIM(2,LLEN_TDIM),IDNUM_TDIM(10)           9
      CHARACTER NAMES_TDIM(LLEN_TDIM)*12,COMMEN_TDIM(LLEN_TDIM)*40        9
      DATA IDNUM_TDIM/10*0/                                               9
      REAL BANK_TDIM(LEN_TDIM)                                            9
      CHARACTER*60 BPATH_TDIM/' '/                                        9
      SAVE FLAG_TDIM,MAP_TDIM,IDNUM_TDIM,NAMES_TDIM,COMMEN_TDIM,          9
     *BPATH_TDIM                                                          9
      DATA LINK_TDIM/0/,BANK_TDIM/LEN_TDIM*0/                             9
C *                                                                       9
C -                                                                       10
      REAL SHLD_VERSION                                                   10
      EQUIVALENCE (BANK_SHLD(3),SHLD_VERSION,RE_SHLD_VERSION),            10
     *(NAMES_SHLD(1),NSHLD_VERSION)                                       10
      EQUIVALENCE (COMMEN_SHLD(1),CSHLD_VERSION),(MAP_SHLD(1,1),          10
     *MSHLD_VERSION)                                                      10
      CHARACTER CSHLD_VERSION*40/' '/                                     10
      CHARACTER NSHLD_VERSION*12/'R.VERSION'/                             10
      INTEGER MSHLD_VERSION(2)/1,1/                                       10
      REAL RE_SHLD_VERSION                                                10
C -                                                                       10
      REAL SHLD_XWIDTH(4)                                                 10
      EQUIVALENCE (BANK_SHLD(4),SHLD_XWIDTH,RE_SHLD_XWIDTH),              10
     *(NAMES_SHLD(2),NSHLD_XWIDTH)                                        10
      EQUIVALENCE (COMMEN_SHLD(2),CSHLD_XWIDTH),(MAP_SHLD(1,2),           10
     *MSHLD_XWIDTH)                                                       10
      CHARACTER CSHLD_XWIDTH*40/' '/                                      10
      CHARACTER NSHLD_XWIDTH*12/'R.XWIDTH'/                               10
      INTEGER MSHLD_XWIDTH(2)/4,1/                                        10
      REAL RE_SHLD_XWIDTH                                                 10
C -                                                                       10
      REAL SHLD_YWIDTH(4)                                                 10
      EQUIVALENCE (BANK_SHLD(8),SHLD_YWIDTH,RE_SHLD_YWIDTH),              10
     *(NAMES_SHLD(3),NSHLD_YWIDTH)                                        10
      EQUIVALENCE (COMMEN_SHLD(3),CSHLD_YWIDTH),(MAP_SHLD(1,3),           10
     *MSHLD_YWIDTH)                                                       10
      CHARACTER CSHLD_YWIDTH*40/' '/                                      10
      CHARACTER NSHLD_YWIDTH*12/'R.YWIDTH'/                               10
      INTEGER MSHLD_YWIDTH(2)/4,1/                                        10
      REAL RE_SHLD_YWIDTH                                                 10
C -                                                                       10
      REAL SHLD_ZWIDTH(4)                                                 10
      EQUIVALENCE (BANK_SHLD(12),SHLD_ZWIDTH,RE_SHLD_ZWIDTH),             10
     *(NAMES_SHLD(4),NSHLD_ZWIDTH)                                        10
      EQUIVALENCE (COMMEN_SHLD(4),CSHLD_ZWIDTH),(MAP_SHLD(1,4),           10
     *MSHLD_ZWIDTH)                                                       10
      CHARACTER CSHLD_ZWIDTH*40/' '/                                      10
      CHARACTER NSHLD_ZWIDTH*12/'R.ZWIDTH'/                               10
      INTEGER MSHLD_ZWIDTH(2)/4,1/                                        10
      REAL RE_SHLD_ZWIDTH                                                 10
C -                                                                       10
      INTEGER LEN_SHLD,LLEN_SHLD,LINK_SHLD                                10
      PARAMETER (LEN_SHLD=15)                                             10
      PARAMETER (LLEN_SHLD=4)                                             10
      INTEGER FLAG_SHLD/0/,MAP_SHLD(2,LLEN_SHLD),IDNUM_SHLD(10)           10
      CHARACTER NAMES_SHLD(LLEN_SHLD)*12,COMMEN_SHLD(LLEN_SHLD)*40        10
      DATA IDNUM_SHLD/10*0/                                               10
      REAL BANK_SHLD(LEN_SHLD)                                            10
      CHARACTER*60 BPATH_SHLD/' '/                                        10
      SAVE FLAG_SHLD,MAP_SHLD,IDNUM_SHLD,NAMES_SHLD,COMMEN_SHLD,          10
     *BPATH_SHLD                                                          10
      DATA LINK_SHLD/0/,BANK_SHLD/LEN_SHLD*0/                             10
C *                                                                       10
      REAL *4 D1,D2,D3,Z1, WALL_XX, WALL_YY, WALL_ZZ, WALL_DX, WALL_DY,   11
     *WALL_DZ                                                             11
      INTEGER *4 I, J, K                                                  13
C *                                                                       15
      CALL AGSBEGM(MODULE,IPRIN)                                          15
      IF (IPRIN.LT.0) RETURN                                              15
C Filling bank CVCF - CAVE CONFIGURATION                                  15
      BANK_TITLE='CAVE CONFIGURATION'                                     15
      BPATH_CVCF='CVCF*'                                                  15
      IDNUM_CVCF(1)=0                                                     16
C VERSION                                                                 16
      CVCF_VERSION=5                                                      16
      CCVCF_VERSION='VERSION'                                             16
C DEFAULT CONFIG                                                          17
      CVCF_CONFIG=1                                                       17
      CCVCF_CONFIG='DEFAULT CONFIG'                                       17
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CVCF',    18
     *BPATH_CVCF,IDNUM_CVCF,1, MAP_CVCF,NAMES_CVCF,COMMEN_CVCF,           18
     *BANK_CVCF,LEN_CVCF,LLEN_CVCF,LINK_CVCF,FLAG_CVCF)                   18
C *                                                                       18
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CVCF',    18
     *BPATH_CVCF,IDNUM_CVCF,1, MAP_CVCF,NAMES_CVCF,COMMEN_CVCF,           18
     *BANK_CVCF,LEN_CVCF,LLEN_CVCF,LINK_CVCF,FLAG_CVCF)                   18
C *                                                                       19
      CALL AGSBEGM(MODULE,IPRIN)                                          19
      IF (IPRIN.LT.0) RETURN                                              19
C Filling bank CAVE - STAR CAVE GEOMETRY                                  19
      BANK_TITLE='STAR CAVE GEOMETRY'                                     19
      BPATH_CAVE='CAVE*'                                                  19
      IDNUM_CAVE(1)=0                                                     20
C VERSION                                                                 20
      CAVE_VERSION=5                                                      20
      CCAVE_VERSION='VERSION'                                             20
C INNER RADIUS OF CAVE HAD BETTER BE ZERO                                 21
      CAVE_RMIN=0                                                         21
      CCAVE_RMIN='INNER RADIUS OF CAVE HAD BETTER BE ZERO'                21
C FURTHEST DISTANCE TO A CONCRETE WALL                                    22
      CAVE_RMAX(1)=1575.02                                                22
      CAVE_RMAX(2)=329.95                                                 22
      CCAVE_RMAX='FURTHEST DISTANCE TO A CONCRETE WALL'                   22
C DZ(1) IS DISTANCE TO EAST/WEST WALL FROM CENTER OF STAR, DZ(2) DEPTH I  23
C NTO TUNNELS                                                             23
      CAVE_DZ(1)=807.72                                                   23
      CAVE_DZ(2)=2020.0                                                   23
      CCAVE_DZ='DZ(1) IS DISTANCE TO EAST/WEST WALL FROM CENTER OF STAR,  23
     * DZ(2) DEPTH INTO TUNNELS'                                          23
C CONCRETE THICKNESS 36INCHES                                             24
      CAVE_DCONC=91.44                                                    24
      CCAVE_DCONC='CONCRETE THICKNESS 36INCHES'                           24
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CAVE',    25
     *BPATH_CAVE,IDNUM_CAVE,1, MAP_CAVE,NAMES_CAVE,COMMEN_CAVE,           25
     *BANK_CAVE,LEN_CAVE,LLEN_CAVE,LINK_CAVE,FLAG_CAVE)                   25
C *                                                                       25
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CAVE',    25
     *BPATH_CAVE,IDNUM_CAVE,1, MAP_CAVE,NAMES_CAVE,COMMEN_CAVE,           25
     *BANK_CAVE,LEN_CAVE,LLEN_CAVE,LINK_CAVE,FLAG_CAVE)                   25
C *                                                                       26
      CALL AGSBEGM(MODULE,IPRIN)                                          26
      IF (IPRIN.LT.0) RETURN                                              26
C Filling bank CDIM - CAVE DIMENSIONS                                     26
      BANK_TITLE='CAVE DIMENSIONS'                                        26
      BPATH_CDIM='CDIM*'                                                  26
      IDNUM_CDIM(1)=0                                                     27
C VERSION                                                                 27
      CDIM_VERSION=1.000                                                  27
      CCDIM_VERSION='VERSION'                                             27
C DIST FROM BEAM TO CEILING ... FAKE                                      28
      CDIM_DCEILING=2*170.0 *2.54                                         28
      CCDIM_DCEILING='DIST FROM BEAM TO CEILING ... FAKE'                 28
C DIST FROM BEAM TO FLOOR                                                 29
      CDIM_DFLOOR=169.75 *2.54                                            29
      CCDIM_DFLOOR='DIST FROM BEAM TO FLOOR'                              29
C DIST FROM BEAM TO THE NORTH (SHIELD) WALL                               30
      CDIM_DNORTH=481.9 *2.54                                             30
      CCDIM_DNORTH='DIST FROM BEAM TO THE NORTH (SHIELD) WALL'            30
C DIST FROM BEAM TO SOUTH WALL                                            31
      CDIM_DSOUTH=620.1 *2.54                                             31
      CCDIM_DSOUTH='DIST FROM BEAM TO SOUTH WALL'                         31
C THICKNESS OF CEILING                                                    32
      CDIM_THICKCEIL=20                                                   32
      CCDIM_THICKCEIL='THICKNESS OF CEILING'                              32
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CDIM',    33
     *BPATH_CDIM,IDNUM_CDIM,1, MAP_CDIM,NAMES_CDIM,COMMEN_CDIM,           33
     *BANK_CDIM,LEN_CDIM,LLEN_CDIM,LINK_CDIM,FLAG_CDIM)                   33
C *                                                                       33
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CDIM',    33
     *BPATH_CDIM,IDNUM_CDIM,1, MAP_CDIM,NAMES_CDIM,COMMEN_CDIM,           33
     *BANK_CDIM,LEN_CDIM,LLEN_CDIM,LINK_CDIM,FLAG_CDIM)                   33
C *                                                                       34
      CALL AGSBEGM(MODULE,IPRIN)                                          34
      IF (IPRIN.LT.0) RETURN                                              34
C Filling bank TDIM - TUNNEL DIMENSIONS                                   34
      BANK_TITLE='TUNNEL DIMENSIONS'                                      34
      BPATH_TDIM='TDIM*'                                                  34
      IDNUM_TDIM(1)=0                                                     35
C VERSION                                                                 35
      TDIM_VERSION=1.000                                                  35
      CTDIM_VERSION='VERSION'                                             35
C DIST FROM BEAM TO CEILING                                               36
      TDIM_DCEILING=14.0 *12.0*2.54-50.0 *2.54                            36
      CTDIM_DCEILING='DIST FROM BEAM TO CEILING'                          36
C DIST FROM BEAM TO FLOOR                                                 37
      TDIM_DFLOOR=50.0 *2.54                                              37
      CTDIM_DFLOOR='DIST FROM BEAM TO FLOOR'                              37
C DIST FROM BEAM TO THE NORTH WALL                                        38
      TDIM_DNORTH=329.95                                                  38
      CTDIM_DNORTH='DIST FROM BEAM TO THE NORTH WALL'                     38
C DIST FROM BEAM TO SOUTH WALL                                            39
      TDIM_DSOUTH=279.65                                                  39
      CTDIM_DSOUTH='DIST FROM BEAM TO SOUTH WALL'                         39
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TDIM',    40
     *BPATH_TDIM,IDNUM_TDIM,1, MAP_TDIM,NAMES_TDIM,COMMEN_TDIM,           40
     *BANK_TDIM,LEN_TDIM,LLEN_TDIM,LINK_TDIM,FLAG_TDIM)                   40
C *                                                                       40
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TDIM',    40
     *BPATH_TDIM,IDNUM_TDIM,1, MAP_TDIM,NAMES_TDIM,COMMEN_TDIM,           40
     *BANK_TDIM,LEN_TDIM,LLEN_TDIM,LINK_TDIM,FLAG_TDIM)                   40
C *                                                                       41
      CALL AGSBEGM(MODULE,IPRIN)                                          41
      IF (IPRIN.LT.0) RETURN                                              41
C Filling bank SHLD - SHIELD DIMENSIONS                                   41
      BANK_TITLE='SHIELD DIMENSIONS'                                      41
      BPATH_SHLD='SHLD*'                                                  41
      IDNUM_SHLD(1)=0                                                     42
C VERSION                                                                 42
      SHLD_VERSION=1.000                                                  42
      CSHLD_VERSION='VERSION'                                             42
C X WIDTHS                                                                44
      SHLD_XWIDTH(1)=48.0 *2.54                                           44
      SHLD_XWIDTH(2)=54.5 *2.54                                           44
      SHLD_XWIDTH(3)=45.5 *2.54                                           44
      SHLD_XWIDTH(4)=45.1 *2.54                                           44
      CSHLD_XWIDTH='X WIDTHS'                                             44
C Y WIDTHS                                                                46
      SHLD_YWIDTH(1)=111. *2.54                                           46
      SHLD_YWIDTH(2)=111. *2.54                                           46
      SHLD_YWIDTH(3)=111. *2.54                                           46
      SHLD_YWIDTH(4)=108. *2.54                                           46
      CSHLD_YWIDTH='Y WIDTHS'                                             46
C Z WIDTHS                                                                48
      SHLD_ZWIDTH(1)=48.0 *2.54                                           48
      SHLD_ZWIDTH(2)=48.0 *2.54                                           48
      SHLD_ZWIDTH(3)=48.0 *2.54                                           48
      SHLD_ZWIDTH(4)=48.0 *2.54                                           48
      CSHLD_ZWIDTH='Z WIDTHS'                                             48
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SHLD',    49
     *BPATH_SHLD,IDNUM_SHLD,1, MAP_SHLD,NAMES_SHLD,COMMEN_SHLD,           49
     *BANK_SHLD,LEN_SHLD,LLEN_SHLD,LINK_SHLD,FLAG_SHLD)                   49
C *                                                                       49
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SHLD',    49
     *BPATH_SHLD,IDNUM_SHLD,1, MAP_SHLD,NAMES_SHLD,COMMEN_SHLD,           49
     *BANK_SHLD,LEN_SHLD,LLEN_SHLD,LINK_SHLD,FLAG_SHLD)                   49
C use default CVCF bank                                                   50
      BPATH_CVCF='CVCF*'                                                  50
      IDNUM_CVCF(1)=0                                                     50
      CALL AGDGETP('CAVEGEO3','*',-999,'CVCF',' ',BANK_CVCF, BPATH_CVCF,  50
     *IDNUM_CVCF,1,MAP_CVCF,NAMES_CVCF,BANK_CVCF,LEN_CVCF,LLEN_CVCF,      50
     *LINK_CVCF,FLAG_CVCF)                                                50
C use CAVE bank                                                           51
      BPATH_CAVE='CAVE*'                                                  51
      IDNUM_CAVE(1)=0                                                     51
      CAVE_VERSION=CVCF_CONFIG                                            51
      CALL AGDGETP('CAVEGEO3','*',-999,'CAVE',NCAVE_VERSION,              51
     *RE_CAVE_VERSION,BPATH_CAVE,IDNUM_CAVE,1,MAP_CAVE,NAMES_CAVE,        51
     *BANK_CAVE,LEN_CAVE,LLEN_CAVE,LINK_CAVE,FLAG_CAVE)                   51
C use default CDIM bank                                                   52
      BPATH_CDIM='CDIM*'                                                  52
      IDNUM_CDIM(1)=0                                                     52
      CALL AGDGETP('CAVEGEO3','*',-999,'CDIM',' ',BANK_CDIM, BPATH_CDIM,  52
     *IDNUM_CDIM,1,MAP_CDIM,NAMES_CDIM,BANK_CDIM,LEN_CDIM,LLEN_CDIM,      52
     *LINK_CDIM,FLAG_CDIM)                                                52
C use default TDIM bank                                                   53
      BPATH_TDIM='TDIM*'                                                  53
      IDNUM_TDIM(1)=0                                                     53
      CALL AGDGETP('CAVEGEO3','*',-999,'TDIM',' ',BANK_TDIM, BPATH_TDIM,  53
     *IDNUM_TDIM,1,MAP_TDIM,NAMES_TDIM,BANK_TDIM,LEN_TDIM,LLEN_TDIM,      53
     *LINK_TDIM,FLAG_TDIM)                                                53
      WRITE(LOUT,5010)CVCF_CONFIG                                         53
5010  FORMAT('CaveGeo Module is called CONFIG=', I3)                      53
      CALL AGSBEGM(MODULE,IPRIN)                                          55
C Check IPRIN<0                                                           55
      IF (IPRIN.LT.0) RETURN                                              55
C Create HALL :                                                           55
C Call To Local Procedure HALL                                            55
      ASSIGN 5030 TO BLOCK_HALL                                           55
      GO TO  5020                                                         55
5030  CONTINUE                                                            55
C *                                                                       55
C ----------------------------------------------------------------------  56
C ------------ HALL                                                       56
      CALL AGSENDM                                                        57
      RETURN                                                              57
C Begin Local Procedure HALL                                              57
C HALL THE STAR WIDE ANGLE HALL                                           57
5020  CONTINUE                                                            57
         AG_TITLE='HALL'                                                  57
         AG_MODULE=MODULE                                                 57
         AG_MARK='V00'                                                    57
         CALL AGSPUSH                                                     57
         AG_MODULE=MODULE                                                 57
C    *                                                                    57
C    Geant COMPONENT : SILICON ===> A=28.09 Z=14 W=0.2 <===               58
         CALL AGSRESET                                                    58
         AG_A = 28.09                                                     58
         AG_Z = 14                                                        58
         AG_W = 0.2                                                       58
         AG_TITLE = 'SILICON'                                             58
         AG_EXNAME = 'COMPONENT'                                          58
         AG_PARLIST = 'A_Z_W'                                             58
         CALL AXCOMPONENT                                                 58
C    *                                                                    58
C    Geant COMPONENT : OXYGEN ===> A=15.999 Z=8 W=0.5 <===                58
         CALL AGSRESET                                                    59
         AG_A = 15.999                                                    59
         AG_Z = 8                                                         59
         AG_W = 0.5                                                       59
         AG_TITLE = 'OXYGEN'                                              59
         AG_EXNAME = 'COMPONENT'                                          59
         AG_PARLIST = 'A_Z_W'                                             59
         CALL AXCOMPONENT                                                 59
C    *                                                                    59
C    Geant COMPONENT : HYDROGEN ===> A=1.00794 Z=1 W=0.006 <===           59
         CALL AGSRESET                                                    60
         AG_A = 1.00794                                                   60
         AG_Z = 1                                                         60
         AG_W = 0.006                                                     60
         AG_TITLE = 'HYDROGEN'                                            60
         AG_EXNAME = 'COMPONENT'                                          60
         AG_PARLIST = 'A_Z_W'                                             60
         CALL AXCOMPONENT                                                 60
C    *                                                                    60
C    Geant COMPONENT : CARBON ===> A=12.011 Z=6 W=0.03 <===               60
         CALL AGSRESET                                                    61
         AG_A = 12.011                                                    61
         AG_Z = 6                                                         61
         AG_W = 0.03                                                      61
         AG_TITLE = 'CARBON'                                              61
         AG_EXNAME = 'COMPONENT'                                          61
         AG_PARLIST = 'A_Z_W'                                             61
         CALL AXCOMPONENT                                                 61
C    *                                                                    61
C    Geant COMPONENT : SODIUM ===> A=22.99 Z=11 W=0.01 <===               61
         CALL AGSRESET                                                    62
         AG_A = 22.99                                                     62
         AG_Z = 11                                                        62
         AG_W = 0.01                                                      62
         AG_TITLE = 'SODIUM'                                              62
         AG_EXNAME = 'COMPONENT'                                          62
         AG_PARLIST = 'A_Z_W'                                             62
         CALL AXCOMPONENT                                                 62
C    *                                                                    62
C    Geant COMPONENT : ALUMINIUM ===> A=26.98 Z=13 W=0.03 <===            62
         CALL AGSRESET                                                    63
         AG_A = 26.98                                                     63
         AG_Z = 13                                                        63
         AG_W = 0.03                                                      63
         AG_TITLE = 'ALUMINIUM'                                           63
         AG_EXNAME = 'COMPONENT'                                          63
         AG_PARLIST = 'A_Z_W'                                             63
         CALL AXCOMPONENT                                                 63
C    *                                                                    63
C    Geant COMPONENT : POTASSIUM ===> A=39.098 Z=19 W=0.01 <===           63
         CALL AGSRESET                                                    64
         AG_A = 39.098                                                    64
         AG_Z = 19                                                        64
         AG_W = 0.01                                                      64
         AG_TITLE = 'POTASSIUM'                                           64
         AG_EXNAME = 'COMPONENT'                                          64
         AG_PARLIST = 'A_Z_W'                                             64
         CALL AXCOMPONENT                                                 64
C    *                                                                    64
C    Geant COMPONENT : CALCIUM ===> A=40.078 Z=20 W=0.2 <===              64
         CALL AGSRESET                                                    65
         AG_A = 40.078                                                    65
         AG_Z = 20                                                        65
         AG_W = 0.2                                                       65
         AG_TITLE = 'CALCIUM'                                             65
         AG_EXNAME = 'COMPONENT'                                          65
         AG_PARLIST = 'A_Z_W'                                             65
         CALL AXCOMPONENT                                                 65
C    *                                                                    65
C    Geant COMPONENT : IRON ===> A=55.85 Z=26 W=0.014 <===                65
         CALL AGSRESET                                                    66
         AG_A = 55.85                                                     66
         AG_Z = 26                                                        66
         AG_W = 0.014                                                     66
         AG_TITLE = 'IRON'                                                66
         AG_EXNAME = 'COMPONENT'                                          66
         AG_PARLIST = 'A_Z_W'                                             66
         CALL AXCOMPONENT                                                 66
C    *                                                                    66
C    Geant MIXTURE : CONCRETE ===> DENS=2.35 <===                         66
         CALL AGSRESET                                                    67
         AG_DENS = 2.35                                                   67
         AG_TITLE = 'CONCRETE'                                            67
         AG_EXNAME = 'MIXTURE'                                            67
         AG_PARLIST = 'DENS'                                              67
         CALL AXMIXTURE                                                   67
C    *                                                                    67
C    Geant MEDIUM : CONCRETE ===> inherited arguments used <===           67
         CALL AGSRESET                                                    68
         AG_TITLE = 'CONCRETE'                                            68
         AG_EXNAME = 'MEDIUM'                                             68
         AG_PARLIST = 'NONE'                                              68
         CALL AXMEDIUM                                                    68
C    *                                                                    68
C    Geant ATTRIBUTE : HALL ===> SEEN=1 COLO=3 <===                       68
         CALL AGSRESET                                                    69
         AG_SEEN = 1                                                      69
         AG_COLO = 3                                                      69
         AG_TITLE = 'HALL'                                                69
         AG_EXNAME = 'ATTRIBUTE'                                          69
         AG_PARLIST = 'SEEN_COLO'                                         69
         CALL AXATTRIBUTE                                                 69
C    *                                                                    69
         D1=CAVE_RMAX(1) + CAVE_DCONC                                     69
         D2=CAVE_RMAX(2) + CAVE_DCONC                                     71
         Z1=CAVE_DZ(1) + CAVE_DCONC                                       72
C    Geant SHAPE : PGON ===> PHI1=45 DPHI=360 NPDIV=4 NZ=6, ZI={-CAVE_DZ  76
C    (2),-Z1,-Z1,Z1,Z1,CAVE_DZ(2)} RMN={CAVE_RMIN, CAVE_RMIN,CAVE_RMIN,C  76
C    AVE_RMIN,CAVE_RMIN, CAVE_RMIN} RMX={D2,D2,D1,D1,D2,D2} <===          76
         CALL AGSRESET                                                    76
         AG_PHI1 = 45                                                     76
         AG_DPHI = 360                                                    76
         AG_NPDIV = 4                                                     76
         AG_NZ = 6                                                        76
         AG_ZI(1)=-CAVE_DZ(2)                                             76
         AG_ZI(2)=-Z1                                                     76
         AG_ZI(3)=-Z1                                                     76
         AG_ZI(4)=Z1                                                      76
         AG_ZI(5)=Z1                                                      76
         AG_ZI(6)=CAVE_DZ(2)                                              76
         AG_RMN(1)=CAVE_RMIN                                              76
         AG_RMN(2)=CAVE_RMIN                                              76
         AG_RMN(3)=CAVE_RMIN                                              76
         AG_RMN(4)=CAVE_RMIN                                              76
         AG_RMN(5)=CAVE_RMIN                                              76
         AG_RMN(6)=CAVE_RMIN                                              76
         AG_RMX(1)=D2                                                     76
         AG_RMX(2)=D2                                                     76
         AG_RMX(3)=D1                                                     76
         AG_RMX(4)=D1                                                     76
         AG_RMX(5)=D2                                                     76
         AG_RMX(6)=D2                                                     76
         AG_TITLE = 'PGON'                                                76
         AG_EXNAME = 'SHAPE'                                              76
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     76
         CALL AXSHAPE                                                     76
C    *                                                                    76
         WALL_DX = D1                                                     76
         WALL_DY = CAVE_DCONC/2                                           78
         WALL_DZ = Z1                                                     79
         WALL_YY = D1 - WALL_DY                                           80
C    Create HOLE :                                                        81
C    Call To Local Procedure HOLE                                         81
         ASSIGN 5050 TO BLOCK_HOLE                                        81
         GO TO  5040                                                      81
5050     CONTINUE                                                         81
C    *                                                                    81
C    Geant POSITION : HOLE ===> MOTHER='HALL' Y=WALL_YY <===              82
         CALL AGSRESET                                                    82
         AG_MOTHER = 'HALL'                                               82
         AG_Y = WALL_YY                                                   82
         AG_TITLE = 'HOLE'                                                82
         AG_EXNAME = 'POSITION'                                           82
         AG_PARLIST = 'MOTHER_Y'                                          82
         CALL AXPOSITION                                                  82
C    *                                                                    82
C    Create CAVE :                                                        83
C    Call To Local Procedure CAVE                                         83
         ASSIGN 5070 TO BLOCK_CAVE                                        83
         GO TO  5060                                                      83
5070     CONTINUE                                                         83
C    *                                                                    83
C    Geant POSITION : CAVE ===> MOTHER='HALL' <===                        84
         CALL AGSRESET                                                    84
         AG_MOTHER = 'HALL'                                               84
         AG_TITLE = 'CAVE'                                                84
         AG_EXNAME = 'POSITION'                                           84
         AG_PARLIST = 'MOTHER'                                            84
         CALL AXPOSITION                                                  84
C    *                                                                    84
5021  CALL AGSPOPB                                                        85
      GO TO BLOCK_HALL, (5020 ,5030)                                      85
C End of Local Procedure                                                  85
C ----------------------------------------------------------------------  86
C ------------ CAVE                                                       86
C Begin Local Procedure CAVE                                              87
C CAVE IS GSTAR CAVE WITH SUBSYSTEM ENVELOPES                             87
5060  CONTINUE                                                            87
         AG_TITLE='CAVE'                                                  87
         AG_MODULE=MODULE                                                 87
         AG_MARK='V00'                                                    87
         CALL AGSPUSH                                                     87
         AG_MODULE=MODULE                                                 87
C    *                                                                    87
C    Geant MATERIAL : AIR ===> inherited arguments used <===              88
         CALL AGSRESET                                                    88
         AG_TITLE = 'AIR'                                                 88
         AG_EXNAME = 'MATERIAL'                                           88
         AG_PARLIST = 'NONE'                                              88
         CALL AXMATERIAL                                                  88
C    *                                                                    88
C    Geant MATERIAL : AIR ===> ISVOL=0 <===                               88
         CALL AGSRESET                                                    89
         AG_ISVOL = 0                                                     89
         AG_TITLE = 'AIR'                                                 89
         AG_EXNAME = 'MATERIAL'                                           89
         AG_PARLIST = 'ISVOL'                                             89
         CALL AXMATERIAL                                                  89
C    *                                                                    89
C    Geant MEDIUM : AIR ===> inherited arguments used <===                89
         CALL AGSRESET                                                    90
         AG_TITLE = 'AIR'                                                 90
         AG_EXNAME = 'MEDIUM'                                             90
         AG_PARLIST = 'NONE'                                              90
         CALL AXMEDIUM                                                    90
C    *                                                                    90
C    Geant ATTRIBUTE : CAVE ===> SEEN=1 COLO=2 <===                       90
         CALL AGSRESET                                                    91
         AG_SEEN = 1                                                      91
         AG_COLO = 2                                                      91
         AG_TITLE = 'CAVE'                                                91
         AG_EXNAME = 'ATTRIBUTE'                                          91
         AG_PARLIST = 'SEEN_COLO'                                         91
         CALL AXATTRIBUTE                                                 91
C    *                                                                    91
C    Geant SHAPE : PGON ===> PHI1=45 DPHI=360 NPDIV=4 NZ=6, ZI={-CAVE_DZ  91
C    (2),-CAVE_DZ(1),-CAVE_DZ(1),+CAVE_DZ(1), +CAVE_DZ(1),+CAVE_DZ(2)} R  91
C    MN={CAVE_RMIN, CAVE_RMIN,CAVE_RMIN,CAVE_RMIN,CAVE_RMIN,CAVE_RMIN},   91
C    RMX={+CAVE_RMAX(2),+CAVE_RMAX(2),+CAVE_RMAX(1), +CAVE_RMAX(1),+CAVE  91
C    _RMAX(2),+CAVE_RMAX(2)} <===                                         91
         CALL AGSRESET                                                    97
         AG_PHI1 = 45                                                     97
         AG_DPHI = 360                                                    97
         AG_NPDIV = 4                                                     97
         AG_NZ = 6                                                        97
         AG_ZI(1)=-CAVE_DZ(2)                                             97
         AG_ZI(2)=-CAVE_DZ(1)                                             97
         AG_ZI(3)=-CAVE_DZ(1)                                             97
         AG_ZI(4)=+CAVE_DZ(1)                                             97
         AG_ZI(5)=+CAVE_DZ(1)                                             97
         AG_ZI(6)=+CAVE_DZ(2)                                             97
         AG_RMN(1)=CAVE_RMIN                                              97
         AG_RMN(2)=CAVE_RMIN                                              97
         AG_RMN(3)=CAVE_RMIN                                              97
         AG_RMN(4)=CAVE_RMIN                                              97
         AG_RMN(5)=CAVE_RMIN                                              97
         AG_RMN(6)=CAVE_RMIN                                              97
         AG_RMX(1)=+CAVE_RMAX(2)                                          97
         AG_RMX(2)=+CAVE_RMAX(2)                                          97
         AG_RMX(3)=+CAVE_RMAX(1)                                          97
         AG_RMX(4)=+CAVE_RMAX(1)                                          97
         AG_RMX(5)=+CAVE_RMAX(2)                                          97
         AG_RMX(6)=+CAVE_RMAX(2)                                          97
         AG_TITLE = 'PGON'                                                97
         AG_EXNAME = 'SHAPE'                                              97
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     97
         CALL AXSHAPE                                                     97
C    *                                                                    97
C    REFERENCE SYSTEM TPCREFSYS IGNORED                                   97
C    REFERENCE SYSTEM MAGREFSYS IGNORED                                   99
C    REFERENCE SYSTEM CAVEREFSYS IGNORED                                  100
         CDIM_RMAX = CAVE_RMAX(1)                                         101
         TDIM_RMAX = CAVE_RMAX(2)                                         102
         CDIM_DZ = CAVE_DZ(1)                                             103
         TDIM_DZ = CAVE_DZ(2) - CAVE_DZ(1)                                104
         D1=CAVE_RMAX(1)                                                  105
         D2=CAVE_RMAX(2)                                                  106
         Z1=CAVE_DZ(1)                                                    107
         WALL_DX = D1                                                     108
         WALL_DY = CDIM_THICKCEIL/2                                       109
         WALL_DZ = CAVE_DZ(1)                                             110
         D3 = CDIM_DCEILING - WALL_DY                                     111
         WALL_YY = D3                                                     112
C    Create WALL :                                                        113
C    Call To Local Procedure WALL                                         113
         ASSIGN 5090 TO BLOCK_WALL                                        113
         GO TO  5080                                                      113
5090     CONTINUE                                                         113
C    *                                                                    113
C    Geant POSITION : WALL ===> Y=WALL_YY <===                            114
         CALL AGSRESET                                                    114
         AG_Y = WALL_YY                                                   114
         AG_TITLE = 'WALL'                                                114
         AG_EXNAME = 'POSITION'                                           114
         AG_PARLIST = 'Y'                                                 114
         CALL AXPOSITION                                                  114
C    *                                                                    114
         WALL_DY =(D1 - CDIM_DFLOOR)/2                                    114
C    Create WALL :                                                        116
C    Call To Local Procedure WALL                                         116
         ASSIGN 5100 TO BLOCK_WALL                                        116
         GO TO  5080                                                      116
5100     CONTINUE                                                         116
C    *                                                                    116
C    Geant POSITION : WALL ===> Y=-CAVE_RMAX(1)+WALL_DY <===              117
         CALL AGSRESET                                                    117
         AG_Y = -CAVE_RMAX(1)+WALL_DY                                     117
         AG_TITLE = 'WALL'                                                117
         AG_EXNAME = 'POSITION'                                           117
         AG_PARLIST = 'Y'                                                 117
         CALL AXPOSITION                                                  117
C    *                                                                    117
         D3 = CDIM_DCEILING - CDIM_THICKCEIL                              117
         WALL_DX = 68*2.54/2                                              119
         WALL_DY =(D3 + CDIM_DFLOOR)/2                                    120
         WALL_XX = - CDIM_DNORTH + WALL_DX                                121
         WALL_YY =(D3 - CDIM_DFLOOR)/2                                    122
C    Create WALL :                                                        123
C    Call To Local Procedure WALL                                         123
         ASSIGN 5110 TO BLOCK_WALL                                        123
         GO TO  5080                                                      123
5110     CONTINUE                                                         123
C    *                                                                    123
C    Geant POSITION : WALL ===> X=WALL_XX Y=WALL_YY <===                  124
         CALL AGSRESET                                                    124
         AG_X = WALL_XX                                                   124
         AG_Y = WALL_YY                                                   124
         AG_TITLE = 'WALL'                                                124
         AG_EXNAME = 'POSITION'                                           124
         AG_PARLIST = 'X_Y'                                               124
         CALL AXPOSITION                                                  124
C    *                                                                    124
         WALL_ZZ =(CAVE_DZ(2) + CAVE_DZ(1))/2                             124
         WALL_DZ =(CAVE_DZ(2) - CAVE_DZ(1))/2                             126
         D1 = CAVE_RMAX(2)                                                127
         WALL_DX = D1                                                     128
         WALL_DY =(D1 - TDIM_DCEILING)/2                                  129
         WALL_YY =(D1 + TDIM_DCEILING)/2                                  130
         D2 = WALL_YY - WALL_DY                                           131
C    Create WALL :                                                        132
C    Call To Local Procedure WALL                                         132
         ASSIGN 5120 TO BLOCK_WALL                                        132
         GO TO  5080                                                      132
5120     CONTINUE                                                         132
C    *                                                                    132
C    Geant POSITION : WALL ===> Y=WALL_YY Z=WALL_ZZ <===                  133
         CALL AGSRESET                                                    133
         AG_Y = WALL_YY                                                   133
         AG_Z = WALL_ZZ                                                   133
         AG_TITLE = 'WALL'                                                133
         AG_EXNAME = 'POSITION'                                           133
         AG_PARLIST = 'Y_Z'                                               133
         CALL AXPOSITION                                                  133
C    *                                                                    133
C    Geant POSITION : WALL ===> Y=WALL_YY Z=-WALL_ZZ <===                 133
         CALL AGSRESET                                                    134
         AG_Y = WALL_YY                                                   134
         AG_Z = -WALL_ZZ                                                  134
         AG_TITLE = 'WALL'                                                134
         AG_EXNAME = 'POSITION'                                           134
         AG_PARLIST = 'Y_Z'                                               134
         CALL AXPOSITION                                                  134
C    *                                                                    134
         WALL_DY =(D1 - TDIM_DFLOOR)/2                                    134
         WALL_YY =(D1 + TDIM_DFLOOR)/2                                    136
         D3 = - (WALL_YY - WALL_DY)                                       137
C    Create WALL :                                                        138
C    Call To Local Procedure WALL                                         138
         ASSIGN 5130 TO BLOCK_WALL                                        138
         GO TO  5080                                                      138
5130     CONTINUE                                                         138
C    *                                                                    138
C    Geant POSITION : WALL ===> Y=-WALL_YY Z=WALL_ZZ <===                 139
         CALL AGSRESET                                                    139
         AG_Y = -WALL_YY                                                  139
         AG_Z = WALL_ZZ                                                   139
         AG_TITLE = 'WALL'                                                139
         AG_EXNAME = 'POSITION'                                           139
         AG_PARLIST = 'Y_Z'                                               139
         CALL AXPOSITION                                                  139
C    *                                                                    139
C    Geant POSITION : WALL ===> Y=-WALL_YY Z=-WALL_ZZ <===                139
         CALL AGSRESET                                                    140
         AG_Y = -WALL_YY                                                  140
         AG_Z = -WALL_ZZ                                                  140
         AG_TITLE = 'WALL'                                                140
         AG_EXNAME = 'POSITION'                                           140
         AG_PARLIST = 'Y_Z'                                               140
         CALL AXPOSITION                                                  140
C    *                                                                    140
         WALL_DY =(D2 - D3)/2                                             140
         WALL_YY =(D2 + D3)/2                                             142
         WALL_DX =(TDIM_DNORTH - TDIM_DSOUTH)/2                           143
         WALL_XX =(TDIM_DNORTH + TDIM_DSOUTH)/2                           144
C    Create WALL :                                                        145
C    Call To Local Procedure WALL                                         145
         ASSIGN 5140 TO BLOCK_WALL                                        145
         GO TO  5080                                                      145
5140     CONTINUE                                                         145
C    *                                                                    145
C    Geant POSITION : WALL ===> X=WALL_XX Y=WALL_YY Z=WALL_ZZ <===        146
         CALL AGSRESET                                                    146
         AG_X = WALL_XX                                                   146
         AG_Y = WALL_YY                                                   146
         AG_Z = WALL_ZZ                                                   146
         AG_TITLE = 'WALL'                                                146
         AG_EXNAME = 'POSITION'                                           146
         AG_PARLIST = 'X_Y_Z'                                             146
         CALL AXPOSITION                                                  146
C    *                                                                    146
C    Geant POSITION : WALL ===> X=WALL_XX Y=WALL_YY Z=-WALL_ZZ <===       146
         CALL AGSRESET                                                    147
         AG_X = WALL_XX                                                   147
         AG_Y = WALL_YY                                                   147
         AG_Z = -WALL_ZZ                                                  147
         AG_TITLE = 'WALL'                                                147
         AG_EXNAME = 'POSITION'                                           147
         AG_PARLIST = 'X_Y_Z'                                             147
         CALL AXPOSITION                                                  147
C    *                                                                    147
         WALL_DX = 0.5*SHLD_XWIDTH(1)                                     147
         WALL_DY = 0.5*SHLD_YWIDTH(1)                                     149
         WALL_DZ = 0.5*SHLD_ZWIDTH(1)                                     150
         WALL_ZZ = - 221.0*2.54 - WALL_DZ - CDIM_DZ                       151
         WALL_YY = - TDIM_DFLOOR + WALL_DY                                152
         WALL_XX = + TDIM_DSOUTH - WALL_DX                                153
C    Create WALL :                                                        154
C    Call To Local Procedure WALL                                         154
         ASSIGN 5150 TO BLOCK_WALL                                        154
         GO TO  5080                                                      154
5150     CONTINUE                                                         154
C    *                                                                    154
C    Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL  155
C    _ZZ <===                                                             155
         CALL AGSRESET                                                    155
         AG_MOTHER = 'CAVE'                                               155
         AG_X = WALL_XX                                                   155
         AG_Y = WALL_YY                                                   155
         AG_Z = WALL_ZZ                                                   155
         AG_TITLE = 'WALL'                                                155
         AG_EXNAME = 'POSITION'                                           155
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      155
         CALL AXPOSITION                                                  155
C    *                                                                    155
         WALL_DX = 0.5*SHLD_XWIDTH(2)                                     155
         WALL_DY = 0.5*SHLD_YWIDTH(2)                                     157
         WALL_DZ = 0.5*SHLD_ZWIDTH(2)                                     158
         WALL_ZZ = - 220.0*2.54 - WALL_DZ - CDIM_DZ                       159
         WALL_YY = - TDIM_DFLOOR + WALL_DY                                160
         WALL_XX = - TDIM_DNORTH + WALL_DX                                161
C    Create WALL :                                                        162
C    Call To Local Procedure WALL                                         162
         ASSIGN 5160 TO BLOCK_WALL                                        162
         GO TO  5080                                                      162
5160     CONTINUE                                                         162
C    *                                                                    162
C    Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL  163
C    _ZZ <===                                                             163
         CALL AGSRESET                                                    163
         AG_MOTHER = 'CAVE'                                               163
         AG_X = WALL_XX                                                   163
         AG_Y = WALL_YY                                                   163
         AG_Z = WALL_ZZ                                                   163
         AG_TITLE = 'WALL'                                                163
         AG_EXNAME = 'POSITION'                                           163
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      163
         CALL AXPOSITION                                                  163
C    *                                                                    163
         WALL_DX = 0.5*SHLD_XWIDTH(3)                                     163
         WALL_DY = 0.5*SHLD_YWIDTH(3)                                     165
         WALL_DZ = 0.5*SHLD_ZWIDTH(3)                                     166
         WALL_ZZ = + 512.3*2.54 + WALL_DZ                                 167
         WALL_YY = - TDIM_DFLOOR + WALL_DY                                168
         WALL_XX = + TDIM_DSOUTH - WALL_DX                                169
C    Create WALL :                                                        170
C    Call To Local Procedure WALL                                         170
         ASSIGN 5170 TO BLOCK_WALL                                        170
         GO TO  5080                                                      170
5170     CONTINUE                                                         170
C    *                                                                    170
C    Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL  171
C    _ZZ <===                                                             171
         CALL AGSRESET                                                    171
         AG_MOTHER = 'CAVE'                                               171
         AG_X = WALL_XX                                                   171
         AG_Y = WALL_YY                                                   171
         AG_Z = WALL_ZZ                                                   171
         AG_TITLE = 'WALL'                                                171
         AG_EXNAME = 'POSITION'                                           171
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      171
         CALL AXPOSITION                                                  171
C    *                                                                    171
         WALL_DX = 0.5*SHLD_XWIDTH(4)                                     171
         WALL_DY = 0.5*SHLD_YWIDTH(4)                                     173
         WALL_DZ = 0.5*SHLD_ZWIDTH(4)                                     174
         WALL_ZZ = + 512.3*2.54 + 144.25*2.54 - 236.0*2.54 + WALL_DZ      175
         WALL_YY = - TDIM_DFLOOR + WALL_DY                                176
         WALL_XX = - TDIM_DNORTH + WALL_DX                                177
C    Create WALL :                                                        178
C    Call To Local Procedure WALL                                         178
         ASSIGN 5180 TO BLOCK_WALL                                        178
         GO TO  5080                                                      178
5180     CONTINUE                                                         178
C    *                                                                    178
C    Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL  179
C    _ZZ <===                                                             179
         CALL AGSRESET                                                    179
         AG_MOTHER = 'CAVE'                                               179
         AG_X = WALL_XX                                                   179
         AG_Y = WALL_YY                                                   179
         AG_Z = WALL_ZZ                                                   179
         AG_TITLE = 'WALL'                                                179
         AG_EXNAME = 'POSITION'                                           179
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      179
         CALL AXPOSITION                                                  179
C    *                                                                    179
         WALL_DX = 0.5*(62.0 + 77.0)*2.54                                 179
         WALL_DY = 0.5*25.0*2.54                                          181
         WALL_DZ = 0.5*82.0*2.54                                          182
         WALL_ZZ = - (538.0 + 120.0)*2.54 - WALL_DZ                       183
         WALL_XX = + 0.5*(77.0 - 62.0)                                    184
         WALL_YY = - TDIM_DFLOOR + WALL_DY                                185
C    Create WALL :                                                        186
C    Call To Local Procedure WALL                                         186
         ASSIGN 5190 TO BLOCK_WALL                                        186
         GO TO  5080                                                      186
5190     CONTINUE                                                         186
C    *                                                                    186
C    Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL  187
C    _ZZ <===                                                             187
         CALL AGSRESET                                                    187
         AG_MOTHER = 'CAVE'                                               187
         AG_X = WALL_XX                                                   187
         AG_Y = WALL_YY                                                   187
         AG_Z = WALL_ZZ                                                   187
         AG_TITLE = 'WALL'                                                187
         AG_EXNAME = 'POSITION'                                           187
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      187
         CALL AXPOSITION                                                  187
C    *                                                                    187
         WALL_DZ = 0.5*45.0*2.54                                          187
C    SAME DX                                                              189
         WALL_DX = WALL_DX                                                189
         WALL_DY = 0.5*(146.0 - 65.0)*2.54                                190
         WALL_ZZ = - (538.0 + 120.0)*2.54 - WALL_DZ                       191
C    SAME XX                                                              192
         WALL_XX = WALL_XX                                                192
         WALL_YY = - TDIM_DFLOOR + 65.0*2.54 + WALL_DY                    193
C    Create SHLD :                                                        194
C    Call To Local Procedure SHLD                                         194
         ASSIGN 5210 TO BLOCK_SHLD                                        194
         GO TO  5200                                                      194
5210     CONTINUE                                                         194
C    *                                                                    194
C    Geant POSITION : SHLD ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL  195
C    _ZZ <===                                                             195
         CALL AGSRESET                                                    195
         AG_MOTHER = 'CAVE'                                               195
         AG_X = WALL_XX                                                   195
         AG_Y = WALL_YY                                                   195
         AG_Z = WALL_ZZ                                                   195
         AG_TITLE = 'SHLD'                                                195
         AG_EXNAME = 'POSITION'                                           195
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      195
         CALL AXPOSITION                                                  195
C    *                                                                    195
         WALL_DX = 0.5*(60.5 + 77.5)*2.54                                 195
         WALL_DY = 0.5*25.0*2.54                                          197
         WALL_DZ = 0.5*81.0*2.54                                          198
         WALL_ZZ =(512.3 + 144.5)*2.54 + WALL_DZ                          199
         WALL_XX = + 0.5*(77.5 - 60.5)                                    200
         WALL_YY = - TDIM_DFLOOR + WALL_DY                                201
C    Create WALL :                                                        202
C    Call To Local Procedure WALL                                         202
         ASSIGN 5220 TO BLOCK_WALL                                        202
         GO TO  5080                                                      202
5220     CONTINUE                                                         202
C    *                                                                    202
C    Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL  203
C    _ZZ <===                                                             203
         CALL AGSRESET                                                    203
         AG_MOTHER = 'CAVE'                                               203
         AG_X = WALL_XX                                                   203
         AG_Y = WALL_YY                                                   203
         AG_Z = WALL_ZZ                                                   203
         AG_TITLE = 'WALL'                                                203
         AG_EXNAME = 'POSITION'                                           203
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      203
         CALL AXPOSITION                                                  203
C    *                                                                    203
         WALL_DZ = 0.5*45.0*2.54                                          203
C    SAME DX                                                              205
         WALL_DX = WALL_DX                                                205
         WALL_DY = 0.5*(145.5 - 65.0)*2.54                                206
         WALL_ZZ =(512.3 + 144.5)*2.54 + WALL_DZ                          207
C    SAME XX                                                              208
         WALL_XX = WALL_XX                                                208
         WALL_YY = - TDIM_DFLOOR + 65.0*2.54 + WALL_DY                    209
C    Create SHLD :                                                        210
C    Call To Local Procedure SHLD                                         210
         ASSIGN 5230 TO BLOCK_SHLD                                        210
         GO TO  5200                                                      210
5230     CONTINUE                                                         210
C    *                                                                    210
C    Geant POSITION : SHLD ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL  211
C    _ZZ <===                                                             211
         CALL AGSRESET                                                    211
         AG_MOTHER = 'CAVE'                                               211
         AG_X = WALL_XX                                                   211
         AG_Y = WALL_YY                                                   211
         AG_Z = WALL_ZZ                                                   211
         AG_TITLE = 'SHLD'                                                211
         AG_EXNAME = 'POSITION'                                           211
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      211
         CALL AXPOSITION                                                  211
C    *                                                                    211
C    EXACT                                                                211
         WALL_DZ = 0.5*45.0*2.54                                          212
C    VERY GOOD APPROX                                                     213
         WALL_DY = 0.5*(65.0 - 25.0)*2.54                                 213
C    WAG(WILD ASS GUESS)                                                  214
         WALL_DX = 0.5*(10.0/28.5)*(62. + 77.)*2.54                       214
C    EXACT                                                                216
         WALL_ZZ = - (538.0 + 120.0 + 82.0)*2.54 + WALL_DZ                216
C    WAG                                                                  217
         WALL_XX = + 4.0*4.877*2.54 + WALL_DX                             217
C    VERY GOOD APPROX                                                     218
         WALL_YY = - TDIM_DFLOOR + 25.0*2.54 + WALL_DY                    218
C    Create SHLD :                                                        219
C    Call To Local Procedure SHLD                                         219
         ASSIGN 5240 TO BLOCK_SHLD                                        219
         GO TO  5200                                                      219
5240     CONTINUE                                                         219
C    *                                                                    219
C    Geant POSITION : SHLD ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL  220
C    _ZZ <===                                                             220
         CALL AGSRESET                                                    220
         AG_MOTHER = 'CAVE'                                               220
         AG_X = WALL_XX                                                   220
         AG_Y = WALL_YY                                                   220
         AG_Z = WALL_ZZ                                                   220
         AG_TITLE = 'SHLD'                                                220
         AG_EXNAME = 'POSITION'                                           220
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      220
         CALL AXPOSITION                                                  220
C    *                                                                    220
         WALL_DZ = 0.5*(82.0 - 45.0)*2.54                                 220
         WALL_DX = WALL_DX                                                222
         WALL_DY = WALL_DY                                                223
C    EXACT                                                                225
         WALL_ZZ = - (538.0 + 120.0)*2.54 - WALL_DZ                       225
         WALL_XX = WALL_XX                                                226
         WALL_YY = WALL_YY                                                227
C    Create WALL :                                                        228
C    Call To Local Procedure WALL                                         228
         ASSIGN 5250 TO BLOCK_WALL                                        228
         GO TO  5080                                                      228
5250     CONTINUE                                                         228
C    *                                                                    228
C    Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL  229
C    _ZZ <===                                                             229
         CALL AGSRESET                                                    229
         AG_MOTHER = 'CAVE'                                               229
         AG_X = WALL_XX                                                   229
         AG_Y = WALL_YY                                                   229
         AG_Z = WALL_ZZ                                                   229
         AG_TITLE = 'WALL'                                                229
         AG_EXNAME = 'POSITION'                                           229
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      229
         CALL AXPOSITION                                                  229
C    *                                                                    229
C    EXACT                                                                229
         WALL_DZ = 0.5*45.0*2.54                                          230
C    VERY GOOD APPROX                                                     231
         WALL_DY = 0.5*(65.0 - 25.0)*2.54                                 231
C    WAG(WILD ASS GUESS)                                                  232
         WALL_DX = 0.5*(7.0/28.5)*(62. + 77.)*2.54                        232
C    EXACT                                                                233
         WALL_ZZ = - (538.0 + 120.0 + 82.0)*2.54 + WALL_DZ                233
C    WAG                                                                  234
         WALL_XX = - 4.0*4.877*2.54 - WALL_DX                             234
C    VERY GOOD APPROX                                                     235
         WALL_YY = - TDIM_DFLOOR + 25.0*2.54 + WALL_DY                    235
C    Create SHLD :                                                        236
C    Call To Local Procedure SHLD                                         236
         ASSIGN 5260 TO BLOCK_SHLD                                        236
         GO TO  5200                                                      236
5260     CONTINUE                                                         236
C    *                                                                    236
C    Geant POSITION : SHLD ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL  237
C    _ZZ <===                                                             237
         CALL AGSRESET                                                    237
         AG_MOTHER = 'CAVE'                                               237
         AG_X = WALL_XX                                                   237
         AG_Y = WALL_YY                                                   237
         AG_Z = WALL_ZZ                                                   237
         AG_TITLE = 'SHLD'                                                237
         AG_EXNAME = 'POSITION'                                           237
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      237
         CALL AXPOSITION                                                  237
C    *                                                                    237
         WALL_DZ = 0.5*(82.0 - 45.0)*2.54                                 237
         WALL_DX = WALL_DX                                                239
         WALL_DY = WALL_DY                                                240
C    EXACT                                                                241
         WALL_ZZ = - (538.0 + 120.0)*2.54 - WALL_DZ                       241
         WALL_XX = WALL_XX                                                242
         WALL_YY = WALL_YY                                                243
C    Create WALL :                                                        244
C    Call To Local Procedure WALL                                         244
         ASSIGN 5270 TO BLOCK_WALL                                        244
         GO TO  5080                                                      244
5270     CONTINUE                                                         244
C    *                                                                    244
C    Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL  245
C    _ZZ <===                                                             245
         CALL AGSRESET                                                    245
         AG_MOTHER = 'CAVE'                                               245
         AG_X = WALL_XX                                                   245
         AG_Y = WALL_YY                                                   245
         AG_Z = WALL_ZZ                                                   245
         AG_TITLE = 'WALL'                                                245
         AG_EXNAME = 'POSITION'                                           245
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      245
         CALL AXPOSITION                                                  245
C    *                                                                    245
C    EXACT                                                                245
         WALL_DZ = 0.5*45.0*2.54                                          246
C    VERY GOOD APPROX                                                     247
         WALL_DY = 0.5*(65.0 - 25.0)*2.54                                 247
C    WAG(WILD ASS GUESS)                                                  248
         WALL_DX = 0.5*(10.0/28.5)*(62. + 77.)*2.54                       248
C    EXACT                                                                249
         WALL_ZZ = + (512.3 + 144.25 + 81.0)*2.54 - WALL_DZ               249
C    WAG                                                                  250
         WALL_XX = + 4.0*4.877*2.54 + WALL_DX                             250
C    VERY GOOD APPROX                                                     251
         WALL_YY = - TDIM_DFLOOR + 25.0*2.54 + WALL_DY                    251
C    Create SHLD :                                                        252
C    Call To Local Procedure SHLD                                         252
         ASSIGN 5280 TO BLOCK_SHLD                                        252
         GO TO  5200                                                      252
5280     CONTINUE                                                         252
C    *                                                                    252
C    Geant POSITION : SHLD ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL  253
C    _ZZ <===                                                             253
         CALL AGSRESET                                                    253
         AG_MOTHER = 'CAVE'                                               253
         AG_X = WALL_XX                                                   253
         AG_Y = WALL_YY                                                   253
         AG_Z = WALL_ZZ                                                   253
         AG_TITLE = 'SHLD'                                                253
         AG_EXNAME = 'POSITION'                                           253
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      253
         CALL AXPOSITION                                                  253
C    *                                                                    253
         WALL_DZ = 0.5*(81.0 - 45.0)*2.54                                 253
         WALL_DX = WALL_DX                                                255
         WALL_DY = WALL_DY                                                256
C    EXACT                                                                258
         WALL_ZZ = + (512.3 + 144.25 + (81.0 - 45.0))*2.54 - WALL_DZ      258
         WALL_XX = WALL_XX                                                259
         WALL_YY = WALL_YY                                                260
C    Create WALL :                                                        261
C    Call To Local Procedure WALL                                         261
         ASSIGN 5290 TO BLOCK_WALL                                        261
         GO TO  5080                                                      261
5290     CONTINUE                                                         261
C    *                                                                    261
C    Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL  262
C    _ZZ <===                                                             262
         CALL AGSRESET                                                    262
         AG_MOTHER = 'CAVE'                                               262
         AG_X = WALL_XX                                                   262
         AG_Y = WALL_YY                                                   262
         AG_Z = WALL_ZZ                                                   262
         AG_TITLE = 'WALL'                                                262
         AG_EXNAME = 'POSITION'                                           262
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      262
         CALL AXPOSITION                                                  262
C    *                                                                    262
C    EXACT                                                                262
         WALL_DZ = 0.5*45.0*2.54                                          263
C    VERY GOOD APPROX                                                     264
         WALL_DY = 0.5*(65.0 - 25.0)*2.54                                 264
C    WAG(WILD ASS GUESS)                                                  265
         WALL_DX = 0.5*(7.0/28.5)*(62. + 77.)*2.54                        265
C    EXACT                                                                267
         WALL_ZZ = + (512.3 + 144.25 + 81.0)*2.54 - WALL_DZ               267
C    WAG                                                                  268
         WALL_XX = - 4.0*4.877*2.54 - WALL_DX                             268
C    VERY GOOD APPROX                                                     269
         WALL_YY = - TDIM_DFLOOR + 25.0*2.54 + WALL_DY                    269
C    Create SHLD :                                                        270
C    Call To Local Procedure SHLD                                         270
         ASSIGN 5300 TO BLOCK_SHLD                                        270
         GO TO  5200                                                      270
5300     CONTINUE                                                         270
C    *                                                                    270
C    Geant POSITION : SHLD ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL  271
C    _ZZ <===                                                             271
         CALL AGSRESET                                                    271
         AG_MOTHER = 'CAVE'                                               271
         AG_X = WALL_XX                                                   271
         AG_Y = WALL_YY                                                   271
         AG_Z = WALL_ZZ                                                   271
         AG_TITLE = 'SHLD'                                                271
         AG_EXNAME = 'POSITION'                                           271
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      271
         CALL AXPOSITION                                                  271
C    *                                                                    271
         WALL_DZ = 0.5*(81.0 - 45.0)*2.54                                 271
         WALL_DX = WALL_DX                                                273
         WALL_DY = WALL_DY                                                274
C    EXACT                                                                276
         WALL_ZZ = + (512.3 + 144.25 + (81.0 - 45.0))*2.54 - WALL_DZ      276
         WALL_XX = WALL_XX                                                277
         WALL_YY = WALL_YY                                                278
C    Create WALL :                                                        279
C    Call To Local Procedure WALL                                         279
         ASSIGN 5310 TO BLOCK_WALL                                        279
         GO TO  5080                                                      279
5310     CONTINUE                                                         279
C    *                                                                    279
C    Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL  280
C    _ZZ <===                                                             280
         CALL AGSRESET                                                    280
         AG_MOTHER = 'CAVE'                                               280
         AG_X = WALL_XX                                                   280
         AG_Y = WALL_YY                                                   280
         AG_Z = WALL_ZZ                                                   280
         AG_TITLE = 'WALL'                                                280
         AG_EXNAME = 'POSITION'                                           280
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      280
         CALL AXPOSITION                                                  280
C    *                                                                    280
C    EXTMUDTRADIUS                                                        280
         D1 = 415                                                         281
C    CELQ_ENDOFPLATFORM                                                   282
         D2 = 1000                                                        282
C    CELQ_THICK/2                                                         283
         D3 = 5                                                           283
         WALL_DX =(D2 - D1)/2                                             284
         WALL_XX =(D2 + D1)/2                                             285
         WALL_DY = D3/2                                                   286
C    CELQ_DZ                                                              287
         WALL_DZ = 500                                                    287
C    Create SHLD :                                                        288
C    Call To Local Procedure SHLD                                         288
         ASSIGN 5320 TO BLOCK_SHLD                                        288
         GO TO  5200                                                      288
5320     CONTINUE                                                         288
C    *                                                                    288
C    Geant POSITION : SHLD ===> MOTHER='CAVE' X=WALL_XX Y=-150-D3/2 <===  289
         CALL AGSRESET                                                    289
         AG_MOTHER = 'CAVE'                                               289
         AG_X = WALL_XX                                                   289
         AG_Y = -150-D3/2                                                 289
         AG_TITLE = 'SHLD'                                                289
         AG_EXNAME = 'POSITION'                                           289
         AG_PARLIST = 'MOTHER_X_Y'                                        289
         CALL AXPOSITION                                                  289
C    *                                                                    289
C    Geant POSITION : SHLD ===> MOTHER='CAVE' X=WALL_XX Y=150-D3/2 <===   289
         CALL AGSRESET                                                    290
         AG_MOTHER = 'CAVE'                                               290
         AG_X = WALL_XX                                                   290
         AG_Y = 150-D3/2                                                  290
         AG_TITLE = 'SHLD'                                                290
         AG_EXNAME = 'POSITION'                                           290
         AG_PARLIST = 'MOTHER_X_Y'                                        290
         CALL AXPOSITION                                                  290
C    *                                                                    290
C    EXTMUDTRADIUS                                                        290
         D1 = 415                                                         291
C    CELQ_ENDOFPLATFORM                                                   292
         D2 = 600                                                         292
         WALL_DX =(D2 - D1)/2                                             293
         WALL_XX =(D2 + D1)/2                                             294
C    Create SHLD :                                                        295
C    Call To Local Procedure SHLD                                         295
         ASSIGN 5330 TO BLOCK_SHLD                                        295
         GO TO  5200                                                      295
5330     CONTINUE                                                         295
C    *                                                                    295
C    Geant POSITION : SHLD ===> MOTHER='CAVE' X=-WALL_XX Y=-150-D3/2 <==  296
C    =                                                                    296
         CALL AGSRESET                                                    296
         AG_MOTHER = 'CAVE'                                               296
         AG_X = -WALL_XX                                                  296
         AG_Y = -150-D3/2                                                 296
         AG_TITLE = 'SHLD'                                                296
         AG_EXNAME = 'POSITION'                                           296
         AG_PARLIST = 'MOTHER_X_Y'                                        296
         CALL AXPOSITION                                                  296
C    *                                                                    296
C    Geant POSITION : SHLD ===> MOTHER='CAVE' X=-WALL_XX Y=150-D3/2 <===  296
         CALL AGSRESET                                                    297
         AG_MOTHER = 'CAVE'                                               297
         AG_X = -WALL_XX                                                  297
         AG_Y = 150-D3/2                                                  297
         AG_TITLE = 'SHLD'                                                297
         AG_EXNAME = 'POSITION'                                           297
         AG_PARLIST = 'MOTHER_X_Y'                                        297
         CALL AXPOSITION                                                  297
C    *                                                                    297
         WALL_DX = 40                                                     297
         WALL_DY = 120                                                    299
         WALL_DZ = 60                                                     300
C    Create CRAT :                                                        301
C    Call To Local Procedure CRAT                                         301
         ASSIGN 5350 TO BLOCK_CRAT                                        301
         GO TO  5340                                                      301
5350     CONTINUE                                                         301
C    *                                                                    301
C    *                                                                    302
         I=1                                                              302
            GO TO 5363                                                    302
5361        I=I+1                                                         302
5363        IF(I-(2).GT.0) GO TO 5362                                     302
            WALL_YY = -150 + 300*(I-1) + WALL_DY                          303
C       *                                                                 304
            J=1                                                           304
               GO TO 5373                                                 304
5371           J=J+1                                                      304
5373           IF(J-(2).GT.0) GO TO 5372                                  304
               WALL_XX = 600 + 300*(J-1)                                  305
C          *                                                              306
               K=1                                                        306
                  GO TO 5383                                              306
5381              K=K+1                                                   306
5383              IF(K-(8).GT.0) GO TO 5382                               306
                  WALL_ZZ = -260 + 65*(K-1)                               307
C             Geant POSITION : CRAT ===> MOTHER='CAVE' X=WALL_XX Y=WALL_  308
C             YY Z=WALL_ZZ <===                                           308
                  CALL AGSRESET                                           308
                  AG_MOTHER = 'CAVE'                                      308
                  AG_X = WALL_XX                                          308
                  AG_Y = WALL_YY                                          308
                  AG_Z = WALL_ZZ                                          308
                  AG_TITLE = 'CRAT'                                       308
                  AG_EXNAME = 'POSITION'                                  308
                  AG_PARLIST = 'MOTHER_X_Y_Z'                             308
                  CALL AXPOSITION                                         308
C             *                                                           308
               GO TO 5381                                                 309
5382           CONTINUE                                                   309
            GO TO 5371                                                    310
5372        CONTINUE                                                      310
         GO TO 5361                                                       311
5362     CONTINUE                                                         311
C    Create MGWP :                                                        312
C    Call To Local Procedure MGWP                                         312
         ASSIGN 5400 TO BLOCK_MGWP                                        312
         GO TO  5390                                                      312
5400     CONTINUE                                                         312
C    *                                                                    312
C    *                                                                    313
         I=1                                                              313
            GO TO 5413                                                    313
5411        I=I+1                                                         313
5413        IF(I-(2).GT.0) GO TO 5412                                     313
            WALL_XX = (2*I-3)*450                                         314
C       *                                                                 315
            J=1                                                           315
               GO TO 5423                                                 315
5421           J=J+1                                                      315
5423           IF(J-(2).GT.0) GO TO 5422                                  315
               WALL_YY = (2*J-3)*100                                      316
C          Geant POSITION : MGWP ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY   317
C          <===                                                           317
               CALL AGSRESET                                              317
               AG_MOTHER = 'CAVE'                                         317
               AG_X = WALL_XX                                             317
               AG_Y = WALL_YY                                             317
               AG_TITLE = 'MGWP'                                          317
               AG_EXNAME = 'POSITION'                                     317
               AG_PARLIST = 'MOTHER_X_Y'                                  317
               CALL AXPOSITION                                            317
C          *                                                              317
            GO TO 5421                                                    318
5422        CONTINUE                                                      318
         GO TO 5411                                                       319
5412     CONTINUE                                                         319
5061  CALL AGSPOPB                                                        320
      GO TO BLOCK_CAVE, (5060 ,5070)                                      320
C End of Local Procedure                                                  320
C ----------------------------------------------------------------------  321
C ------------ WALL                                                       321
C Begin Local Procedure WALL                                              322
C WALL IS THE FLOOR (OR BOTTOM) OF THE CAVE                               322
5080  CONTINUE                                                            322
         AG_TITLE='WALL'                                                  322
         AG_MODULE=MODULE                                                 322
         AG_MARK='V00'                                                    322
         CALL AGSPUSH                                                     322
         AG_MODULE=MODULE                                                 322
C    *                                                                    322
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=1 <===                      323
         CALL AGSRESET                                                    323
         AG_A = 28.08                                                     323
         AG_Z = 14                                                        323
         AG_W = 1                                                         323
         AG_TITLE = 'SI'                                                  323
         AG_EXNAME = 'COMPONENT'                                          323
         AG_PARLIST = 'A_Z_W'                                             323
         CALL AXCOMPONENT                                                 323
C    *                                                                    323
C    Geant COMPONENT : O2 ===> A=16 Z=8 W=2 <===                          323
         CALL AGSRESET                                                    324
         AG_A = 16                                                        324
         AG_Z = 8                                                         324
         AG_W = 2                                                         324
         AG_TITLE = 'O2'                                                  324
         AG_EXNAME = 'COMPONENT'                                          324
         AG_PARLIST = 'A_Z_W'                                             324
         CALL AXCOMPONENT                                                 324
C    *                                                                    324
C    Geant MIXTURE : CONCRETE ===> DENS=2.5 <===                          324
         CALL AGSRESET                                                    325
         AG_DENS = 2.5                                                    325
         AG_TITLE = 'CONCRETE'                                            325
         AG_EXNAME = 'MIXTURE'                                            325
         AG_PARLIST = 'DENS'                                              325
         CALL AXMIXTURE                                                   325
C    *                                                                    325
C    Geant MEDIUM : CONCRETE ===> inherited arguments used <===           325
         CALL AGSRESET                                                    326
         AG_TITLE = 'CONCRETE'                                            326
         AG_EXNAME = 'MEDIUM'                                             326
         AG_PARLIST = 'NONE'                                              326
         CALL AXMEDIUM                                                    326
C    *                                                                    326
C    Geant ATTRIBUTE : WALL ===> SEEN=0 COLO=3 <===                       326
         CALL AGSRESET                                                    327
         AG_SEEN = 0                                                      327
         AG_COLO = 3                                                      327
         AG_TITLE = 'WALL'                                                327
         AG_EXNAME = 'ATTRIBUTE'                                          327
         AG_PARLIST = 'SEEN_COLO'                                         327
         CALL AXATTRIBUTE                                                 327
C    *                                                                    327
C    Geant SHAPE : BOX ===> DX=WALL_DX DY=WALL_DY DZ=WALL_DZ <===         327
         CALL AGSRESET                                                    328
         AG_DX = WALL_DX                                                  328
         AG_DY = WALL_DY                                                  328
         AG_DZ = WALL_DZ                                                  328
         AG_TITLE = 'BOX'                                                 328
         AG_EXNAME = 'SHAPE'                                              328
         AG_PARLIST = 'DX_DY_DZ'                                          328
         CALL AXSHAPE                                                     328
C    *                                                                    328
5081  CALL AGSPOPB                                                        329
      GO TO BLOCK_WALL, (5080 ,5090,5100,5110,5120,5130,5140,5150,5160,   329
     *5170,5180,5190,5220,5250,5270,5290,5310)                            329
C End of Local Procedure                                                  329
C ----------------------------------------------------------------------  330
C ------------ SHLD                                                       330
C Begin Local Procedure SHLD                                              331
C SHLD IS THE SHIELDING                                                   331
5200  CONTINUE                                                            331
         AG_TITLE='SHLD'                                                  331
         AG_MODULE=MODULE                                                 331
         AG_MARK='V00'                                                    331
         CALL AGSPUSH                                                     331
         AG_MODULE=MODULE                                                 331
C    *                                                                    331
C    Geant MATERIAL : IRON ===> inherited arguments used <===             332
         CALL AGSRESET                                                    332
         AG_TITLE = 'IRON'                                                332
         AG_EXNAME = 'MATERIAL'                                           332
         AG_PARLIST = 'NONE'                                              332
         CALL AXMATERIAL                                                  332
C    *                                                                    332
C    Geant MATERIAL : IRON ===> ISVOL=0 <===                              332
         CALL AGSRESET                                                    333
         AG_ISVOL = 0                                                     333
         AG_TITLE = 'IRON'                                                333
         AG_EXNAME = 'MATERIAL'                                           333
         AG_PARLIST = 'ISVOL'                                             333
         CALL AXMATERIAL                                                  333
C    *                                                                    333
C    Geant MEDIUM : IRON ===> inherited arguments used <===               333
         CALL AGSRESET                                                    334
         AG_TITLE = 'IRON'                                                334
         AG_EXNAME = 'MEDIUM'                                             334
         AG_PARLIST = 'NONE'                                              334
         CALL AXMEDIUM                                                    334
C    *                                                                    334
C    Geant ATTRIBUTE : SHLD ===> SEEN=0 COLO=2 <===                       334
         CALL AGSRESET                                                    335
         AG_SEEN = 0                                                      335
         AG_COLO = 2                                                      335
         AG_TITLE = 'SHLD'                                                335
         AG_EXNAME = 'ATTRIBUTE'                                          335
         AG_PARLIST = 'SEEN_COLO'                                         335
         CALL AXATTRIBUTE                                                 335
C    *                                                                    335
C    Geant SHAPE : BOX ===> DX=WALL_DX DY=WALL_DY DZ=WALL_DZ <===         335
         CALL AGSRESET                                                    336
         AG_DX = WALL_DX                                                  336
         AG_DY = WALL_DY                                                  336
         AG_DZ = WALL_DZ                                                  336
         AG_TITLE = 'BOX'                                                 336
         AG_EXNAME = 'SHAPE'                                              336
         AG_PARLIST = 'DX_DY_DZ'                                          336
         CALL AXSHAPE                                                     336
C    *                                                                    336
5201  CALL AGSPOPB                                                        337
      GO TO BLOCK_SHLD, (5200 ,5210,5230,5240,5260,5280,5300,5320,5330)   337
C End of Local Procedure                                                  337
C ----------------------------------------------------------------------  338
C ------------ HOLE                                                       338
C Begin Local Procedure HOLE                                              339
C HOLE IS THE FLOOR (OR BOTTOM) OF THE CAVE                               339
5040  CONTINUE                                                            339
         AG_TITLE='HOLE'                                                  339
         AG_MODULE=MODULE                                                 339
         AG_MARK='V00'                                                    339
         CALL AGSPUSH                                                     339
         AG_MODULE=MODULE                                                 339
C    *                                                                    339
C    Geant MATERIAL : AIR ===> inherited arguments used <===              340
         CALL AGSRESET                                                    340
         AG_TITLE = 'AIR'                                                 340
         AG_EXNAME = 'MATERIAL'                                           340
         AG_PARLIST = 'NONE'                                              340
         CALL AXMATERIAL                                                  340
C    *                                                                    340
C    Geant MATERIAL : AIR ===> ISVOL=0 <===                               340
         CALL AGSRESET                                                    341
         AG_ISVOL = 0                                                     341
         AG_TITLE = 'AIR'                                                 341
         AG_EXNAME = 'MATERIAL'                                           341
         AG_PARLIST = 'ISVOL'                                             341
         CALL AXMATERIAL                                                  341
C    *                                                                    341
C    Geant MEDIUM : AIR ===> inherited arguments used <===                341
         CALL AGSRESET                                                    342
         AG_TITLE = 'AIR'                                                 342
         AG_EXNAME = 'MEDIUM'                                             342
         AG_PARLIST = 'NONE'                                              342
         CALL AXMEDIUM                                                    342
C    *                                                                    342
C    Geant ATTRIBUTE : HOLE ===> SEEN=0 COLO=5 <===                       342
         CALL AGSRESET                                                    343
         AG_SEEN = 0                                                      343
         AG_COLO = 5                                                      343
         AG_TITLE = 'HOLE'                                                343
         AG_EXNAME = 'ATTRIBUTE'                                          343
         AG_PARLIST = 'SEEN_COLO'                                         343
         CALL AXATTRIBUTE                                                 343
C    *                                                                    343
C    Geant SHAPE : BOX ===> DX=WALL_DX DY=WALL_DY DZ=WALL_DZ <===         343
         CALL AGSRESET                                                    344
         AG_DX = WALL_DX                                                  344
         AG_DY = WALL_DY                                                  344
         AG_DZ = WALL_DZ                                                  344
         AG_TITLE = 'BOX'                                                 344
         AG_EXNAME = 'SHAPE'                                              344
         AG_PARLIST = 'DX_DY_DZ'                                          344
         CALL AXSHAPE                                                     344
C    *                                                                    344
5041  CALL AGSPOPB                                                        345
      GO TO BLOCK_HOLE, (5040 ,5050)                                      345
C End of Local Procedure                                                  345
C ----------------------------------------------------------------------  346
C ------------ CRAT                                                       346
C Begin Local Procedure CRAT                                              347
C CRAT IS THE ELECTRONICS CRATE                                           347
5340  CONTINUE                                                            347
         AG_TITLE='CRAT'                                                  347
         AG_MODULE=MODULE                                                 347
         AG_MARK='V00'                                                    347
         CALL AGSPUSH                                                     347
         AG_MODULE=MODULE                                                 347
C    *                                                                    347
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===          348
         CALL AGSRESET                                                    348
         AG_A = 28.08                                                     348
         AG_Z = 14                                                        348
         AG_W = 0.6*1*28./60.                                             348
         AG_TITLE = 'SI'                                                  348
         AG_EXNAME = 'COMPONENT'                                          348
         AG_PARLIST = 'A_Z_W'                                             348
         CALL AXCOMPONENT                                                 348
C    *                                                                    348
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. <===               348
         CALL AGSRESET                                                    349
         AG_A = 16                                                        349
         AG_Z = 8                                                         349
         AG_W = 0.6*2*16./60.                                             349
         AG_TITLE = 'O'                                                   349
         AG_EXNAME = 'COMPONENT'                                          349
         AG_PARLIST = 'A_Z_W'                                             349
         CALL AXCOMPONENT                                                 349
C    *                                                                    349
C    Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===              349
         CALL AGSRESET                                                    350
         AG_A = 12                                                        350
         AG_Z = 6                                                         350
         AG_W = 0.4*8*12./174.                                            350
         AG_TITLE = 'C'                                                   350
         AG_EXNAME = 'COMPONENT'                                          350
         AG_PARLIST = 'A_Z_W'                                             350
         CALL AXCOMPONENT                                                 350
C    *                                                                    350
C    Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===               350
         CALL AGSRESET                                                    351
         AG_A = 1                                                         351
         AG_Z = 1                                                         351
         AG_W = 0.4*14*1./174.                                            351
         AG_TITLE = 'H'                                                   351
         AG_EXNAME = 'COMPONENT'                                          351
         AG_PARLIST = 'A_Z_W'                                             351
         CALL AXCOMPONENT                                                 351
C    *                                                                    351
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.4*4*16./174. <===              351
         CALL AGSRESET                                                    352
         AG_A = 16                                                        352
         AG_Z = 8                                                         352
         AG_W = 0.4*4*16./174.                                            352
         AG_TITLE = 'O'                                                   352
         AG_EXNAME = 'COMPONENT'                                          352
         AG_PARLIST = 'A_Z_W'                                             352
         CALL AXCOMPONENT                                                 352
C    *                                                                    352
C    Geant MIXTURE : ELECTRONCS ===> DENS=0.13 <===                       352
         CALL AGSRESET                                                    353
         AG_DENS = 0.13                                                   353
         AG_TITLE = 'ELECTRONCS'                                          353
         AG_EXNAME = 'MIXTURE'                                            353
         AG_PARLIST = 'DENS'                                              353
         CALL AXMIXTURE                                                   353
C    *                                                                    353
C    Geant MEDIUM : ELECTRONICS ===> inherited arguments used <===        353
         CALL AGSRESET                                                    354
         AG_TITLE = 'ELECTRONICS'                                         354
         AG_EXNAME = 'MEDIUM'                                             354
         AG_PARLIST = 'NONE'                                              354
         CALL AXMEDIUM                                                    354
C    *                                                                    354
C    Geant ATTRIBUTE : CRAT ===> SEEN=0 COLO=6 <===                       354
         CALL AGSRESET                                                    355
         AG_SEEN = 0                                                      355
         AG_COLO = 6                                                      355
         AG_TITLE = 'CRAT'                                                355
         AG_EXNAME = 'ATTRIBUTE'                                          355
         AG_PARLIST = 'SEEN_COLO'                                         355
         CALL AXATTRIBUTE                                                 355
C    *                                                                    355
C    Geant SHAPE : BOX ===> DX=WALL_DX DY=WALL_DY DZ=WALL_DZ <===         355
         CALL AGSRESET                                                    356
         AG_DX = WALL_DX                                                  356
         AG_DY = WALL_DY                                                  356
         AG_DZ = WALL_DZ                                                  356
         AG_TITLE = 'BOX'                                                 356
         AG_EXNAME = 'SHAPE'                                              356
         AG_PARLIST = 'DX_DY_DZ'                                          356
         CALL AXSHAPE                                                     356
C    *                                                                    356
5341  CALL AGSPOPB                                                        357
      GO TO BLOCK_CRAT, (5340 ,5350)                                      357
C End of Local Procedure                                                  357
C ----------------------------------------------------------------------  358
C ------------ MGWP                                                       358
C Begin Local Procedure MGWP                                              359
C MGWP IS THE MAGNET WATER PIPE                                           359
5390  CONTINUE                                                            359
         AG_TITLE='MGWP'                                                  359
         AG_MODULE=MODULE                                                 359
         AG_MARK='V00'                                                    359
         CALL AGSPUSH                                                     359
         AG_MODULE=MODULE                                                 359
C    *                                                                    359
C    Geant MATERIAL : IRON ===> inherited arguments used <===             360
         CALL AGSRESET                                                    360
         AG_TITLE = 'IRON'                                                360
         AG_EXNAME = 'MATERIAL'                                           360
         AG_PARLIST = 'NONE'                                              360
         CALL AXMATERIAL                                                  360
C    *                                                                    360
C    Geant MEDIUM : IRON ===> inherited arguments used <===               360
         CALL AGSRESET                                                    361
         AG_TITLE = 'IRON'                                                361
         AG_EXNAME = 'MEDIUM'                                             361
         AG_PARLIST = 'NONE'                                              361
         CALL AXMEDIUM                                                    361
C    *                                                                    361
C    Geant ATTRIBUTE : WGWP ===> SEEN=0 COLO=6 <===                       361
         CALL AGSRESET                                                    362
         AG_SEEN = 0                                                      362
         AG_COLO = 6                                                      362
         AG_TITLE = 'WGWP'                                                362
         AG_EXNAME = 'ATTRIBUTE'                                          362
         AG_PARLIST = 'SEEN_COLO'                                         362
         CALL AXATTRIBUTE                                                 362
C    *                                                                    362
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=12 DZ=312 <===                   362
         CALL AGSRESET                                                    363
         AG_RMIN = 0                                                      363
         AG_RMAX = 12                                                     363
         AG_DZ = 312                                                      363
         AG_TITLE = 'TUBE'                                                363
         AG_EXNAME = 'SHAPE'                                              363
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      363
         CALL AXSHAPE                                                     363
C    *                                                                    363
C    Create MGWT :                                                        364
C    Call To Local Procedure MGWT                                         364
         ASSIGN 5440 TO BLOCK_MGWT                                        364
         GO TO  5430                                                      364
5440     CONTINUE                                                         364
C    *                                                                    364
C    Geant POSITION : MGWT ===> inherited arguments used <===             365
         CALL AGSRESET                                                    365
         AG_TITLE = 'MGWT'                                                365
         AG_EXNAME = 'POSITION'                                           365
         AG_PARLIST = 'NONE'                                              365
         CALL AXPOSITION                                                  365
C    *                                                                    365
5391  CALL AGSPOPB                                                        366
      GO TO BLOCK_MGWP, (5390 ,5400)                                      366
C End of Local Procedure                                                  366
C ----------------------------------------------------------------------  367
C ------------ MGWT                                                       367
C Begin Local Procedure MGWT                                              368
C MGWT IS THE WATER IN MAGNET WATER PIPE                                  368
5430  CONTINUE                                                            368
         AG_TITLE='MGWT'                                                  368
         AG_MODULE=MODULE                                                 368
         AG_MARK='V00'                                                    368
         CALL AGSPUSH                                                     368
         AG_MODULE=MODULE                                                 368
C    *                                                                    368
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           369
         CALL AGSRESET                                                    369
         AG_A = 1                                                         369
         AG_Z = 1                                                         369
         AG_W = 2                                                         369
         AG_TITLE = 'H2'                                                  369
         AG_EXNAME = 'COMPONENT'                                          369
         AG_PARLIST = 'A_Z_W'                                             369
         CALL AXCOMPONENT                                                 369
C    *                                                                    369
C    Geant COMPONENT : O ===> A=16 Z=8 W=1 <===                           369
         CALL AGSRESET                                                    370
         AG_A = 16                                                        370
         AG_Z = 8                                                         370
         AG_W = 1                                                         370
         AG_TITLE = 'O'                                                   370
         AG_EXNAME = 'COMPONENT'                                          370
         AG_PARLIST = 'A_Z_W'                                             370
         CALL AXCOMPONENT                                                 370
C    *                                                                    370
C    Geant MIXTURE : WATER ===> DENS=1.0 <===                             370
         CALL AGSRESET                                                    371
         AG_DENS = 1.0                                                    371
         AG_TITLE = 'WATER'                                               371
         AG_EXNAME = 'MIXTURE'                                            371
         AG_PARLIST = 'DENS'                                              371
         CALL AXMIXTURE                                                   371
C    *                                                                    371
C    Geant MEDIUM : WATER ===> inherited arguments used <===              371
         CALL AGSRESET                                                    372
         AG_TITLE = 'WATER'                                               372
         AG_EXNAME = 'MEDIUM'                                             372
         AG_PARLIST = 'NONE'                                              372
         CALL AXMEDIUM                                                    372
C    *                                                                    372
C    Geant ATTRIBUTE : MGWT ===> SEEN=0 COLO=6 <===                       372
         CALL AGSRESET                                                    373
         AG_SEEN = 0                                                      373
         AG_COLO = 6                                                      373
         AG_TITLE = 'MGWT'                                                373
         AG_EXNAME = 'ATTRIBUTE'                                          373
         AG_PARLIST = 'SEEN_COLO'                                         373
         CALL AXATTRIBUTE                                                 373
C    *                                                                    373
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=10 DZ=310 <===                   373
         CALL AGSRESET                                                    374
         AG_RMIN = 0                                                      374
         AG_RMAX = 10                                                     374
         AG_DZ = 310                                                      374
         AG_TITLE = 'TUBE'                                                374
         AG_EXNAME = 'SHAPE'                                              374
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      374
         CALL AXSHAPE                                                     374
C    *                                                                    374
5431  CALL AGSPOPB                                                        375
      GO TO BLOCK_MGWT, (5430 ,5440)                                      375
C End of Local Procedure                                                  375
      WRITE(LOUT,5450)'===>>  [End AgML Module CaveGeo3]  <<==='          375
5450  FORMAT(A32,/,/)                                                     375
C MODULE CAVEGEO3                                                         377
      END                                                                 377
