C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
* =============================================================================
*
      SUBROUTINE WALLGEO                                                  5
C WALLGEO DEFINES THE WALLS, FLOORS AND CEILINGS OF THE STAR CAVE AND TU  5
C NNELS. IT ALSO CREATES THE SHIELD WALLS PRESENT IN THE TUNNELS.         5
      IMPLICIT NONE                                                       5
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       5
      DATA MODULE/'WALLGEO'/, BANK_TITLE/'*'/                             5
      DATA MCOMMENT/'DEFINES THE WALLS, FLOORS AND CEILINGS OF THE STAR   5
     *CAVE AND TUNNELS. IT ALSO CREATES THE SHIELD WALLS PRESENT IN THE   5
     *TUNNELS.'/                                                          5
      LOGICAL FIRST                                                       5
      DATA FIRST/.TRUE./                                                  5
      SAVE FIRST                                                          5
      INTEGER JUMPAD                                                      5
      EXTERNAL JUMPAD                                                     5
      INTEGER OK                                                          5
      PARAMETER (OK=0)                                                    5
      INTEGER AG_I,AG_J                                                   5
      SAVE AG_I,AG_J                                                      5
      DATA AG_I/0/,AG_J/0/                                                5
*
C AUTHOR JASON C. WEBB                                                    6
      CHARACTER*40 AUTHOR /'JASON C. WEBB'/                               7
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  7
C CREATED JANUARY 29, 2010                                                7
      CHARACTER*40 CREATED /'JANUARY 29, 2010'/                           8
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  8
*
* Drawings are marked out in inches, so we define some units.
*
* Usage:
*     oneCM = 1 unit(cm)
*     oneIN = 1 unit(in)
*
*
* REPLACE [;ASSERT(#);] with [;call assert((#1), __LINE__,'wallgeo.g');]
* REPLACE [;ASSERT(#);] with []
*
* and I really need an increment operator...
*  REPLACE [(#)++] with [#1=#1+1]
*
* ==================================================================== Notes ==
*
* This module should be used with version=5 of the CAVE geometry.  It
* implements the STAR cave in the WAH as built.  The cave creates a
* symmetric 3D polygon shape, into which WALL adds the floor, ceiling
* and walls to best approximate the actual cave and tunnel geometry.
*
*    ------------------------  ASSUMPTION --------------------------------
*
* We assume that the east and west tunnels have the same North-South
* and Top-Bottom dimenstions
*
* Note that the CAVE geometry is responsible for initializing some of the
* variables (cdim_rmax and tdim_rmax) used in this module.
*
*    ---------------------- SIMPLICIFACTION ------------------------------
*
* We implement the tunnel sheilding using concrete blocks.  This is not
* correct, as the shielding is a combination of concrete and stacked iron
* plate.  This is a temporary placeholder to get the geometry in place.
*
* Helpful to keep in mind STAR coordinate system:
*
* +z is WEST / blue beam direction / towards endcap
* -z is EAST / yell beam direction / away from endcap
* +x is SOUTH
* -x is NORTH
* +y is UP
* -y is DOWN
*
* Cave is created with Rmax = distance to south wall.  So we only need to
* create the floor, ceiling and north walls in the cave.
*
* The beam tunnel is opposite: Rmax = distance to north wall.  So we create
* the floor, ceiling and south walls in the cave.
*
* =============================================================================
C - Advanced Geant interface                                              67
      CHARACTER*20 AG_MODULE,AG_TITLE,AG_EXNAME,AG_PARLIST,AG_CHDIR,      67
     *AG_MATERIAL,AG_MIXTURE,AG_COMPONENT,AG_MEDIUM                       67
      CHARACTER*4 AG_VOLUME,AG_MOTHER,AG_SHAPE,AG_CNICK,AG_KONLY,         67
     *AG_OPTION,AG_ORT,AG_MARK                                            67
      INTEGER AG_BEGCOM,AG_IVOLUME,AG_IMOTHER,AG_IGNUM,AG_ISHAPE,         67
     *AG_IMED,AG_IMAT,AG_IFIELD, AG_NLMAT,AG_IERROR,AG_NWBUF,AG_NPAR,     67
     *AG_ISTATUS,AG_IROT,AG_NBITS,AG_SERIAL, AG_ATTRIBUTE(6),AG_WORK,     67
     *AG_SEEN,AG_LSTY,AG_LWID,AG_COLO,AG_FILL,AG_JDU,AG_ORTI, AG_NDIV,    67
     *AG_IAXIS,AG_NDVMAX,AG_IPRIN,AG_RESET1,AG_RESET2,AG_BEGSCR,          67
     *AG_ENDSCR, AG_IDTYPE,AG_ISET,AG_IDET,AG_ISVOL,AG_LEVEL,AG_IRESER,   67
     *AG_IGEOM,AG_STANDALONE, AG_LSTACK,AG_NWUHIT,AG_NWUVOL,AG_MAGIC,     67
     *AG_LDETU,AG_NPDIV,AG_NPDV,AG_NZ,AG_NCOPY, AG_IDEBU,AG_IGRAP,        67
     *AG_IHIST,AG_IMFLD,AG_ISIMU,AG_ENDSAVE,AG_CODE,AG_TRKTYP, AG_MODE,   67
     *AG_ECODE,AG_PDG,IPRIN, AG_ALPHAN,AG_ALPHAK(10),IGOR                 67
      REAL AG_FIELDM,AG_TMAXFD,AG_STEMAX,AG_DEEMAX,AG_EPSIL,AG_STMIN,     67
     *AG_DENS,AG_RADL,AG_ABSL, AG_THETAX,AG_PHIX,AG_THETAY,AG_PHIY,       67
     *AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,AG_ALFAZ, AG_ALPHAL(10),        67
     *AG_ALPHAX,AG_ALPHAY,AG_ALPHAZ,AG_TYPE, AG_DX, AG_DX1, AG_DX2,       67
     *AG_DY, AG_DY1, AG_DY2, AG_DMAXMS, AG_TWIST, AG_THET, AG_THE1,       67
     *AG_THE2, AG_PHI, AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2,       67
     *AG_RMIN, AG_RMAX, AG_RMN, AG_RMX, AG_ZI , AG_RMN1, AG_RMN2,         67
     *AG_RMX1, AG_RMX2, AG_H1, AG_H2, AG_BL1, AG_BL2, AG_TL1, AG_TL2,     67
     *AG_DPHI, AG_DZ, AG_TWIS, AG_X, AG_Y, AG_Z, AG_A, AG_ZA, AG_W,       67
     *AG_STEP, AG_C0, AG_PAR, AG_AA, AG_ZZ, AG_WW, AG_LX, AG_LY, AG_LZ,   67
     *AG_HX, AG_HY, AG_HZ, AG_P1, AG_P2, AG_STACK,AG_UBUF,AG_XHMAX,       67
     *AG_YHMAX,AG_ZHMAX,AG_RHMAX,AG_FHMAX,AG_FHMIN,AG_BIN, AG_CHARGE,     67
     *AG_MASS,AG_TLIFE,AG_BRATIO                                          67
      PARAMETER (AG_LSTACK=130,AG_NWUHIT=10,AG_NWUVOL=3,AG_MAGIC=         67
     *-696969,AG_LDETU=250)                                               67
      COMMON/AGCGLOB/ AG_MODULE,AG_CHDIR,AG_LEVEL,AG_IDTYPE,AG_IERROR,    67
     *AG_STANDALONE,IPRIN, AG_IPRIN,AG_IGEOM,AG_IDEBU,AG_IGRAP,AG_IHIST,  67
     *AG_IMFLD,AG_ISIMU                                                   67
C Inherited variables a: saved during internal calls                      67
C b: reset at the entry in a block and then saved                         67
      COMMON/AGCPARA/ AG_BEGCOM, AG_IVOLUME, AG_IMOTHER, AG_IGNUM,        67
     *AG_ISHAPE, AG_IMED, AG_IMAT, AG_IFIELD, AG_FIELDM, AG_TMAXFD,       67
     *AG_STEMAX, AG_DEEMAX, AG_EPSIL, AG_STMIN, AG_DENS, AG_RADL,         67
     *AG_ABSL, AG_DX, AG_DX1, AG_DX2, AG_DY, AG_DY1, AG_DY2, AG_RMN1,     67
     *AG_RMN2, AG_RMX1, AG_RMX2, AG_THET, AG_THE1, AG_THE2, AG_PHI,       67
     *AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2, AG_H1, AG_BL1,         67
     *AG_TL1, AG_H2, AG_BL2, AG_TL2, AG_RMIN, AG_RMAX, AG_DPHI,           67
     *AG_NPDIV, AG_NZ, AG_DZ, AG_TWIS, AG_LX, AG_LY, AG_LZ, AG_HX,        67
     *AG_HY, AG_HZ, AG_A, AG_ZA, AG_W, AG_NLMAT, AG_WORK, AG_SEEN,        67
     *AG_LSTY, AG_LWID, AG_COLO, AG_FILL, AG_SERIAL, AG_ISVOL,            67
     *AG_ISTATUS, AG_ZI(16), AG_RMN(16), AG_RMX(16), AG_VOLUME,           67
     *AG_MOTHER, AG_SHAPE, AG_CNICK, AG_ENDSAVE, AG_RESET1, AG_THETAX,    67
     *AG_PHIX,AG_THETAY,AG_PHIY,AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,      67
     *AG_ALFAZ, AG_X,AG_Y,AG_Z, AG_STEP,AG_C0,AG_NDIV,AG_IAXIS,           67
     *AG_NDVMAX,AG_ORTI,AG_NCOPY, AG_ALPHAN,AG_ALPHAK,AG_ALPHAL,          67
     *AG_RESET2, AG_KONLY, AG_ORT, AG_MARK                                67
C local variables valid inside same block                                 67
      COMMON/AGCLOCA/ AG_BEGSCR,AG_UBUF(100),AG_PAR(100),AG_AA(20),       67
     *AG_ZZ(20),AG_WW(20),AG_NWBUF, AG_XHMAX,AG_YHMAX,AG_ZHMAX,AG_RHMAX,  67
     *AG_FHMAX,AG_FHMIN,AG_NBITS,AG_BIN,AG_TYPE, AG_IROT,AG_NPAR,         67
     *AG_ISET,AG_IDET,AG_JDU,AG_IRESER, AG_ENDSCR, AG_TITLE,AG_EXNAME,    67
     *AG_PARLIST,AG_MATERIAL,AG_MIXTURE, AG_COMPONENT,AG_MEDIUM,          67
     *AG_OPTION                                                           67
      COMMON/AGCPART/AG_CODE,AG_TRKTYP,AG_MASS,AG_CHARGE,AG_TLIFE,        67
     *AG_BRATIO(6),AG_MODE(6),AG_PDG,AG_ECODE                             67
      COMMON/AGCSTAC/ AG_STACK(AG_LSTACK,15)                              67
C temporarly                                                              67
      EQUIVALENCE (AG_ATTRIBUTE,AG_WORK),(AG_STEMAX,AG_DMAXMS),           67
     *(AG_ALPHAX,AG_ALFAX), (AG_ALPHAY,AG_ALFAY),(AG_ALPHAZ,AG_ALFAZ),    67
     *(AG_TWIST,AG_TWIS),(AG_NPDIV,AG_NPDV),  (AG_P1,AG_HX),(AG_P2,       67
     *AG_HY)                                                              67
      REAL MIL_P,INCH_P                                                   67
      PARAMETER (INCH_P=2.54,MIL_P=0.00254)                               67
C - GEANT description of logical units                                    67
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            68
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      68
      COMMON/GCMAIL/ CHMAIL                                               68
      CHARACTER*132 CHMAIL                                                68
*
      INTEGER BLOCK_WALL                                                  70
      INTEGER BLOCK_TWLL                                                  70
*
* Structures hodling the dimensions of the cave and tunnel.
* =====================================================================
* NOTE: Rmax will be filled when cavegeo is created, using AgDETPadd.
*       This ensures that this module gets the correct dimensions for
*       the cavegeo.
* =====================================================================
*
*                                                              Cave Dimensions
C -                                                                       86
      REAL CDIM_VERSION                                                   86
      EQUIVALENCE (BANK_CDIM(3),CDIM_VERSION,RE_CDIM_VERSION),            86
     *(NAMES_CDIM(1),NCDIM_VERSION)                                       86
      EQUIVALENCE (COMMEN_CDIM(1),CCDIM_VERSION),(MAP_CDIM(1,1),          86
     *MCDIM_VERSION)                                                      86
      CHARACTER CCDIM_VERSION*40/' '/                                     86
      CHARACTER NCDIM_VERSION*12/'R.VERSION'/                             86
      INTEGER MCDIM_VERSION(2)/1,1/                                       86
      REAL RE_CDIM_VERSION                                                86
C -                                                                       86
      REAL CDIM_DCEILING                                                  86
      EQUIVALENCE (BANK_CDIM(4),CDIM_DCEILING,RE_CDIM_DCEILING),          86
     *(NAMES_CDIM(2),NCDIM_DCEILING)                                      86
      EQUIVALENCE (COMMEN_CDIM(2),CCDIM_DCEILING),(MAP_CDIM(1,2),         86
     *MCDIM_DCEILING)                                                     86
      CHARACTER CCDIM_DCEILING*40/' '/                                    86
      CHARACTER NCDIM_DCEILING*12/'R.DCEILING'/                           86
      INTEGER MCDIM_DCEILING(2)/1,1/                                      86
      REAL RE_CDIM_DCEILING                                               86
C -                                                                       86
      REAL CDIM_DFLOOR                                                    86
      EQUIVALENCE (BANK_CDIM(5),CDIM_DFLOOR,RE_CDIM_DFLOOR),              86
     *(NAMES_CDIM(3),NCDIM_DFLOOR)                                        86
      EQUIVALENCE (COMMEN_CDIM(3),CCDIM_DFLOOR),(MAP_CDIM(1,3),           86
     *MCDIM_DFLOOR)                                                       86
      CHARACTER CCDIM_DFLOOR*40/' '/                                      86
      CHARACTER NCDIM_DFLOOR*12/'R.DFLOOR'/                               86
      INTEGER MCDIM_DFLOOR(2)/1,1/                                        86
      REAL RE_CDIM_DFLOOR                                                 86
C -                                                                       86
      REAL CDIM_DNORTH                                                    86
      EQUIVALENCE (BANK_CDIM(6),CDIM_DNORTH,RE_CDIM_DNORTH),              86
     *(NAMES_CDIM(4),NCDIM_DNORTH)                                        86
      EQUIVALENCE (COMMEN_CDIM(4),CCDIM_DNORTH),(MAP_CDIM(1,4),           86
     *MCDIM_DNORTH)                                                       86
      CHARACTER CCDIM_DNORTH*40/' '/                                      86
      CHARACTER NCDIM_DNORTH*12/'R.DNORTH'/                               86
      INTEGER MCDIM_DNORTH(2)/1,1/                                        86
      REAL RE_CDIM_DNORTH                                                 86
C -                                                                       86
      REAL CDIM_DSOUTH                                                    86
      EQUIVALENCE (BANK_CDIM(7),CDIM_DSOUTH,RE_CDIM_DSOUTH),              86
     *(NAMES_CDIM(5),NCDIM_DSOUTH)                                        86
      EQUIVALENCE (COMMEN_CDIM(5),CCDIM_DSOUTH),(MAP_CDIM(1,5),           86
     *MCDIM_DSOUTH)                                                       86
      CHARACTER CCDIM_DSOUTH*40/' '/                                      86
      CHARACTER NCDIM_DSOUTH*12/'R.DSOUTH'/                               86
      INTEGER MCDIM_DSOUTH(2)/1,1/                                        86
      REAL RE_CDIM_DSOUTH                                                 86
C -                                                                       86
      REAL CDIM_RMAX                                                      86
      EQUIVALENCE (BANK_CDIM(8),CDIM_RMAX,RE_CDIM_RMAX),(NAMES_CDIM(6),   86
     *NCDIM_RMAX)                                                         86
      EQUIVALENCE (COMMEN_CDIM(6),CCDIM_RMAX),(MAP_CDIM(1,6),             86
     *MCDIM_RMAX)                                                         86
      CHARACTER CCDIM_RMAX*40/' '/                                        86
      CHARACTER NCDIM_RMAX*12/'R.RMAX'/                                   86
      INTEGER MCDIM_RMAX(2)/1,1/                                          86
      REAL RE_CDIM_RMAX                                                   86
C -                                                                       86
      REAL CDIM_DZ                                                        86
      EQUIVALENCE (BANK_CDIM(9),CDIM_DZ,RE_CDIM_DZ),(NAMES_CDIM(7),       86
     *NCDIM_DZ)                                                           86
      EQUIVALENCE (COMMEN_CDIM(7),CCDIM_DZ),(MAP_CDIM(1,7),MCDIM_DZ)      86
      CHARACTER CCDIM_DZ*40/' '/                                          86
      CHARACTER NCDIM_DZ*12/'R.DZ'/                                       86
      INTEGER MCDIM_DZ(2)/1,1/                                            86
      REAL RE_CDIM_DZ                                                     86
C -                                                                       86
      INTEGER LEN_CDIM,LLEN_CDIM,LINK_CDIM                                86
      PARAMETER (LEN_CDIM=9)                                              86
      PARAMETER (LLEN_CDIM=7)                                             86
      INTEGER FLAG_CDIM/0/,MAP_CDIM(2,LLEN_CDIM),IDNUM_CDIM(10)           86
      CHARACTER NAMES_CDIM(LLEN_CDIM)*12,COMMEN_CDIM(LLEN_CDIM)*40        86
      DATA IDNUM_CDIM/10*0/                                               86
      REAL BANK_CDIM(LEN_CDIM)                                            86
      CHARACTER*60 BPATH_CDIM/' '/                                        86
      SAVE FLAG_CDIM,MAP_CDIM,IDNUM_CDIM,NAMES_CDIM,COMMEN_CDIM,          86
     *BPATH_CDIM                                                          86
      DATA LINK_CDIM/0/,BANK_CDIM/LEN_CDIM*0/                             86
C *                                                                       86
*
*                                                            Tunnel Dimensions
C -                                                                       95
      REAL TDIM_VERSION                                                   95
      EQUIVALENCE (BANK_TDIM(3),TDIM_VERSION,RE_TDIM_VERSION),            95
     *(NAMES_TDIM(1),NTDIM_VERSION)                                       95
      EQUIVALENCE (COMMEN_TDIM(1),CTDIM_VERSION),(MAP_TDIM(1,1),          95
     *MTDIM_VERSION)                                                      95
      CHARACTER CTDIM_VERSION*40/' '/                                     95
      CHARACTER NTDIM_VERSION*12/'R.VERSION'/                             95
      INTEGER MTDIM_VERSION(2)/1,1/                                       95
      REAL RE_TDIM_VERSION                                                95
C -                                                                       95
      REAL TDIM_DCEILING                                                  95
      EQUIVALENCE (BANK_TDIM(4),TDIM_DCEILING,RE_TDIM_DCEILING),          95
     *(NAMES_TDIM(2),NTDIM_DCEILING)                                      95
      EQUIVALENCE (COMMEN_TDIM(2),CTDIM_DCEILING),(MAP_TDIM(1,2),         95
     *MTDIM_DCEILING)                                                     95
      CHARACTER CTDIM_DCEILING*40/' '/                                    95
      CHARACTER NTDIM_DCEILING*12/'R.DCEILING'/                           95
      INTEGER MTDIM_DCEILING(2)/1,1/                                      95
      REAL RE_TDIM_DCEILING                                               95
C -                                                                       95
      REAL TDIM_DFLOOR                                                    95
      EQUIVALENCE (BANK_TDIM(5),TDIM_DFLOOR,RE_TDIM_DFLOOR),              95
     *(NAMES_TDIM(3),NTDIM_DFLOOR)                                        95
      EQUIVALENCE (COMMEN_TDIM(3),CTDIM_DFLOOR),(MAP_TDIM(1,3),           95
     *MTDIM_DFLOOR)                                                       95
      CHARACTER CTDIM_DFLOOR*40/' '/                                      95
      CHARACTER NTDIM_DFLOOR*12/'R.DFLOOR'/                               95
      INTEGER MTDIM_DFLOOR(2)/1,1/                                        95
      REAL RE_TDIM_DFLOOR                                                 95
C -                                                                       95
      REAL TDIM_DNORTH                                                    95
      EQUIVALENCE (BANK_TDIM(6),TDIM_DNORTH,RE_TDIM_DNORTH),              95
     *(NAMES_TDIM(4),NTDIM_DNORTH)                                        95
      EQUIVALENCE (COMMEN_TDIM(4),CTDIM_DNORTH),(MAP_TDIM(1,4),           95
     *MTDIM_DNORTH)                                                       95
      CHARACTER CTDIM_DNORTH*40/' '/                                      95
      CHARACTER NTDIM_DNORTH*12/'R.DNORTH'/                               95
      INTEGER MTDIM_DNORTH(2)/1,1/                                        95
      REAL RE_TDIM_DNORTH                                                 95
C -                                                                       95
      REAL TDIM_DSOUTH                                                    95
      EQUIVALENCE (BANK_TDIM(7),TDIM_DSOUTH,RE_TDIM_DSOUTH),              95
     *(NAMES_TDIM(5),NTDIM_DSOUTH)                                        95
      EQUIVALENCE (COMMEN_TDIM(5),CTDIM_DSOUTH),(MAP_TDIM(1,5),           95
     *MTDIM_DSOUTH)                                                       95
      CHARACTER CTDIM_DSOUTH*40/' '/                                      95
      CHARACTER NTDIM_DSOUTH*12/'R.DSOUTH'/                               95
      INTEGER MTDIM_DSOUTH(2)/1,1/                                        95
      REAL RE_TDIM_DSOUTH                                                 95
C -                                                                       95
      REAL TDIM_RMAX                                                      95
      EQUIVALENCE (BANK_TDIM(8),TDIM_RMAX,RE_TDIM_RMAX),(NAMES_TDIM(6),   95
     *NTDIM_RMAX)                                                         95
      EQUIVALENCE (COMMEN_TDIM(6),CTDIM_RMAX),(MAP_TDIM(1,6),             95
     *MTDIM_RMAX)                                                         95
      CHARACTER CTDIM_RMAX*40/' '/                                        95
      CHARACTER NTDIM_RMAX*12/'R.RMAX'/                                   95
      INTEGER MTDIM_RMAX(2)/1,1/                                          95
      REAL RE_TDIM_RMAX                                                   95
C -                                                                       95
      REAL TDIM_DZ                                                        95
      EQUIVALENCE (BANK_TDIM(9),TDIM_DZ,RE_TDIM_DZ),(NAMES_TDIM(7),       95
     *NTDIM_DZ)                                                           95
      EQUIVALENCE (COMMEN_TDIM(7),CTDIM_DZ),(MAP_TDIM(1,7),MTDIM_DZ)      95
      CHARACTER CTDIM_DZ*40/' '/                                          95
      CHARACTER NTDIM_DZ*12/'R.DZ'/                                       95
      INTEGER MTDIM_DZ(2)/1,1/                                            95
      REAL RE_TDIM_DZ                                                     95
C -                                                                       95
      INTEGER LEN_TDIM,LLEN_TDIM,LINK_TDIM                                95
      PARAMETER (LEN_TDIM=9)                                              95
      PARAMETER (LLEN_TDIM=7)                                             95
      INTEGER FLAG_TDIM/0/,MAP_TDIM(2,LLEN_TDIM),IDNUM_TDIM(10)           95
      CHARACTER NAMES_TDIM(LLEN_TDIM)*12,COMMEN_TDIM(LLEN_TDIM)*40        95
      DATA IDNUM_TDIM/10*0/                                               95
      REAL BANK_TDIM(LEN_TDIM)                                            95
      CHARACTER*60 BPATH_TDIM/' '/                                        95
      SAVE FLAG_TDIM,MAP_TDIM,IDNUM_TDIM,NAMES_TDIM,COMMEN_TDIM,          95
     *BPATH_TDIM                                                          95
      DATA LINK_TDIM/0/,BANK_TDIM/LEN_TDIM*0/                             95
C *                                                                       95
*
C -                                                                       100
      REAL SHLD_VERSION                                                   100
      EQUIVALENCE (BANK_SHLD(3),SHLD_VERSION,RE_SHLD_VERSION),            100
     *(NAMES_SHLD(1),NSHLD_VERSION)                                       100
      EQUIVALENCE (COMMEN_SHLD(1),CSHLD_VERSION),(MAP_SHLD(1,1),          100
     *MSHLD_VERSION)                                                      100
      CHARACTER CSHLD_VERSION*40/' '/                                     100
      CHARACTER NSHLD_VERSION*12/'R.VERSION'/                             100
      INTEGER MSHLD_VERSION(2)/1,1/                                       100
      REAL RE_SHLD_VERSION                                                100
C -                                                                       100
      REAL SHLD_XWIDTH(4)                                                 100
      EQUIVALENCE (BANK_SHLD(4),SHLD_XWIDTH,RE_SHLD_XWIDTH),              100
     *(NAMES_SHLD(2),NSHLD_XWIDTH)                                        100
      EQUIVALENCE (COMMEN_SHLD(2),CSHLD_XWIDTH),(MAP_SHLD(1,2),           100
     *MSHLD_XWIDTH)                                                       100
      CHARACTER CSHLD_XWIDTH*40/' '/                                      100
      CHARACTER NSHLD_XWIDTH*12/'R.XWIDTH'/                               100
      INTEGER MSHLD_XWIDTH(2)/4,1/                                        100
      REAL RE_SHLD_XWIDTH                                                 100
C -                                                                       100
      REAL SHLD_YWIDTH(4)                                                 100
      EQUIVALENCE (BANK_SHLD(8),SHLD_YWIDTH,RE_SHLD_YWIDTH),              100
     *(NAMES_SHLD(3),NSHLD_YWIDTH)                                        100
      EQUIVALENCE (COMMEN_SHLD(3),CSHLD_YWIDTH),(MAP_SHLD(1,3),           100
     *MSHLD_YWIDTH)                                                       100
      CHARACTER CSHLD_YWIDTH*40/' '/                                      100
      CHARACTER NSHLD_YWIDTH*12/'R.YWIDTH'/                               100
      INTEGER MSHLD_YWIDTH(2)/4,1/                                        100
      REAL RE_SHLD_YWIDTH                                                 100
C -                                                                       100
      REAL SHLD_ZWIDTH(4)                                                 100
      EQUIVALENCE (BANK_SHLD(12),SHLD_ZWIDTH,RE_SHLD_ZWIDTH),             100
     *(NAMES_SHLD(4),NSHLD_ZWIDTH)                                        100
      EQUIVALENCE (COMMEN_SHLD(4),CSHLD_ZWIDTH),(MAP_SHLD(1,4),           100
     *MSHLD_ZWIDTH)                                                       100
      CHARACTER CSHLD_ZWIDTH*40/' '/                                      100
      CHARACTER NSHLD_ZWIDTH*12/'R.ZWIDTH'/                               100
      INTEGER MSHLD_ZWIDTH(2)/4,1/                                        100
      REAL RE_SHLD_ZWIDTH                                                 100
C -                                                                       100
      INTEGER LEN_SHLD,LLEN_SHLD,LINK_SHLD                                100
      PARAMETER (LEN_SHLD=15)                                             100
      PARAMETER (LLEN_SHLD=4)                                             100
      INTEGER FLAG_SHLD/0/,MAP_SHLD(2,LLEN_SHLD),IDNUM_SHLD(10)           100
      CHARACTER NAMES_SHLD(LLEN_SHLD)*12,COMMEN_SHLD(LLEN_SHLD)*40        100
      DATA IDNUM_SHLD/10*0/                                               100
      REAL BANK_SHLD(LEN_SHLD)                                            100
      CHARACTER*60 BPATH_SHLD/' '/                                        100
      SAVE FLAG_SHLD,MAP_SHLD,IDNUM_SHLD,NAMES_SHLD,COMMEN_SHLD,          100
     *BPATH_SHLD                                                          100
      DATA LINK_SHLD/0/,BANK_SHLD/LEN_SHLD*0/                             100
C *                                                                       100
*
* =============================================================================
*
      INTEGER ID_CAVE                                                     105
      INTEGER AGEXIST                                                     106
*
      REAL WALL_XX, WALL_YY, WALL_ZZ, WALL_DX, WALL_DY, WALL_DZ           108
*
* Enumerate material types
      INTEGER WALL_STUFF, NWALL                                           113
* =============================================================================
*
C *                                                                       117
      CALL AGSBEGM(MODULE,IPRIN)                                          117
      IF (IPRIN.LT.0) RETURN                                              117
C Filling bank CDIM - CAVE DIMENSIONS                                     117
      BANK_TITLE='CAVE DIMENSIONS'                                        117
      BPATH_CDIM='CDIM*'                                                  117
      IDNUM_CDIM(1)=0                                                     118
C VERSION                                                                 118
      CDIM_VERSION=1.000                                                  118
      CCDIM_VERSION='VERSION'                                             118
C DIST FROM BEAM TO CEILING <<<<<<<<< F                                   119
      CDIM_DCEILING=2*170.0 *2.5400                                       119
      CCDIM_DCEILING='DIST FROM BEAM TO CEILING <<<<<<<<< F'              119
C DIST FROM BEAM TO FLOOR                                                 120
      CDIM_DFLOOR=169.75 *2.5400                                          120
      CCDIM_DFLOOR='DIST FROM BEAM TO FLOOR'                              120
C DIST FROM BEAM TO THE NORTH (SHIELD) WALL                               121
      CDIM_DNORTH=481.9 *2.5400                                           121
      CCDIM_DNORTH='DIST FROM BEAM TO THE NORTH (SHIELD) WALL'            121
C DIST FROM BEAM TO SOUTH WALL                                            122
      CDIM_DSOUTH=620.1 *2.5400                                           122
      CCDIM_DSOUTH='DIST FROM BEAM TO SOUTH WALL'                         122
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CDIM',    123
     *BPATH_CDIM,IDNUM_CDIM,1, MAP_CDIM,NAMES_CDIM,COMMEN_CDIM,           123
     *BANK_CDIM,LEN_CDIM,LLEN_CDIM,LINK_CDIM,FLAG_CDIM)                   123
C *                                                                       123
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CDIM',    123
     *BPATH_CDIM,IDNUM_CDIM,1, MAP_CDIM,NAMES_CDIM,COMMEN_CDIM,           123
     *BANK_CDIM,LEN_CDIM,LLEN_CDIM,LINK_CDIM,FLAG_CDIM)                   123
* Rmax, and Dz filled in cavegeo
* Rmax = distance from beam to edge of cave
* Dz   = distance from (0,0,0) to tunnel entrance
*
* =============================================================================
*
C *                                                                       130
      CALL AGSBEGM(MODULE,IPRIN)                                          130
      IF (IPRIN.LT.0) RETURN                                              130
C Filling bank TDIM - CAVE DIMENSIONS                                     130
      BANK_TITLE='CAVE DIMENSIONS'                                        130
      BPATH_TDIM='TDIM*'                                                  130
      IDNUM_TDIM(1)=0                                                     131
C VERSION                                                                 131
      TDIM_VERSION=1.000                                                  131
      CTDIM_VERSION='VERSION'                                             131
C DIST FROM BEAM TO CEILING                                               132
      TDIM_DCEILING=14.0 *12.0*2.5400-50.0 *2.5400                        132
      CTDIM_DCEILING='DIST FROM BEAM TO CEILING'                          132
C DIST FROM BEAM TO FLOOR                                                 133
      TDIM_DFLOOR=50.0 *2.5400                                            133
      CTDIM_DFLOOR='DIST FROM BEAM TO FLOOR'                              133
C DIST FROM BEAM TO THE NORTH WALL                                        134
      TDIM_DNORTH=329.95 *1.0                                             134
      CTDIM_DNORTH='DIST FROM BEAM TO THE NORTH WALL'                     134
C DIST FROM BEAM TO SOUTH WALL                                            135
      TDIM_DSOUTH=279.65 *1.0                                             135
      CTDIM_DSOUTH='DIST FROM BEAM TO SOUTH WALL'                         135
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TDIM',    136
     *BPATH_TDIM,IDNUM_TDIM,1, MAP_TDIM,NAMES_TDIM,COMMEN_TDIM,           136
     *BANK_TDIM,LEN_TDIM,LLEN_TDIM,LINK_TDIM,FLAG_TDIM)                   136
C *                                                                       136
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TDIM',    136
     *BPATH_TDIM,IDNUM_TDIM,1, MAP_TDIM,NAMES_TDIM,COMMEN_TDIM,           136
     *BANK_TDIM,LEN_TDIM,LLEN_TDIM,LINK_TDIM,FLAG_TDIM)                   136
* Rmax, and Dz filled in cavegeo
* Rmax = distance from beam to edge of tunnel
* Dz   = distance from start of tunnel to end of tunnel                                                           (DOUBL
*
* =============================================================================
*
C *                                                                       143
      CALL AGSBEGM(MODULE,IPRIN)                                          143
      IF (IPRIN.LT.0) RETURN                                              143
C Filling bank SHLD - SHIELD DIMENSIONS                                   143
      BANK_TITLE='SHIELD DIMENSIONS'                                      143
      BPATH_SHLD='SHLD*'                                                  143
      IDNUM_SHLD(1)=0                                                     144
C VERSION                                                                 144
      SHLD_VERSION=1.000                                                  144
      CSHLD_VERSION='VERSION'                                             144
C X WIDTHS                                                                148
      SHLD_XWIDTH(1)=48.0 *2.5400                                         148
      SHLD_XWIDTH(2)=54.5 *2.5400                                         148
      SHLD_XWIDTH(3)=45.5 *2.5400                                         148
      SHLD_XWIDTH(4)=45.1 *2.5400                                         148
      CSHLD_XWIDTH='X WIDTHS'                                             148
C Y WIDTHS                                                                152
      SHLD_YWIDTH(1)=111. *2.5400                                         152
      SHLD_YWIDTH(2)=111. *2.5400                                         152
      SHLD_YWIDTH(3)=111. *2.5400                                         152
      SHLD_YWIDTH(4)=108. *2.5400                                         152
      CSHLD_YWIDTH='Y WIDTHS'                                             152
C Z WIDTHS                                                                156
      SHLD_ZWIDTH(1)=48.0 *2.5400                                         156
      SHLD_ZWIDTH(2)=48.0 *2.5400                                         156
      SHLD_ZWIDTH(3)=48.0 *2.5400                                         156
      SHLD_ZWIDTH(4)=48.0 *2.5400                                         156
      CSHLD_ZWIDTH='Z WIDTHS'                                             156
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SHLD',    157
     *BPATH_SHLD,IDNUM_SHLD,1, MAP_SHLD,NAMES_SHLD,COMMEN_SHLD,           157
     *BANK_SHLD,LEN_SHLD,LLEN_SHLD,LINK_SHLD,FLAG_SHLD)                   157
C *                                                                       157
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SHLD',    157
     *BPATH_SHLD,IDNUM_SHLD,1, MAP_SHLD,NAMES_SHLD,COMMEN_SHLD,           157
     *BANK_SHLD,LEN_SHLD,LLEN_SHLD,LINK_SHLD,FLAG_SHLD)                   157
* =============================================================================
*
C use default CDIM bank                                                   160
      BPATH_CDIM='CDIM*'                                                  160
      IDNUM_CDIM(1)=0                                                     160
      CALL AGDGETP('WALLGEO','*',-999,'CDIM',' ',BANK_CDIM, BPATH_CDIM,   160
     *IDNUM_CDIM,1,MAP_CDIM,NAMES_CDIM,BANK_CDIM,LEN_CDIM,LLEN_CDIM,      160
     *LINK_CDIM,FLAG_CDIM)                                                160
C use default TDIM bank                                                   161
      BPATH_TDIM='TDIM*'                                                  161
      IDNUM_TDIM(1)=0                                                     161
      CALL AGDGETP('WALLGEO','*',-999,'TDIM',' ',BANK_TDIM, BPATH_TDIM,   161
     *IDNUM_TDIM,1,MAP_TDIM,NAMES_TDIM,BANK_TDIM,LEN_TDIM,LLEN_TDIM,      161
     *LINK_TDIM,FLAG_TDIM)                                                161
*                                                    Sanity: ensure cave exists
      ID_CAVE = AGEXIST('CAVE')                                           163
*  assert( id_cave .gt. 0 )
*                                                Sanity: ensure cave dimensions
*  assert( cdim_rmax .gt. tdim_rmax )
*  assert( tdim_rmax .gt. 0. )
*
      NWALL = 1                                                           169
*
C set material proerties                                                  172
      WALL_STUFF = 1                                                      173
*
* =============================================================================
*                                  STAR CAVE
* ==================================================================== Floor ==
*
C HALF WIDTH ALONG N-S                                                    179
      WALL_DX = CDIM_RMAX                                                 179
C HALF WIDTH ALONG T-B                                                    180
      WALL_DY = 0.5*(CDIM_RMAX-CDIM_DFLOOR)                               180
C HALF WIDTH ALONG E-W                                                    181
      WALL_DZ = CDIM_DZ                                                   181
C CENTER ALONG N-S                                                        182
      WALL_XX = 0.0                                                       182
C CENTER AT AVG FLOOR                                                     183
      WALL_YY = -CDIM_DFLOOR - WALL_DY                                    183
C CENTER ALONG E-W                                                        184
      WALL_ZZ = 0.0                                                       184
*
      CALL AGSBEGM(MODULE,IPRIN)                                          186
C Check IPRIN<0                                                           186
      IF (IPRIN.LT.0) RETURN                                              186
C Create WALL :                                                           186
C Call To Local Procedure WALL                                            186
      ASSIGN 5020 TO BLOCK_WALL                                           186
      GO TO  5010                                                         186
5020  CONTINUE                                                            186
C *                                                                       186
C Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL_ZZ  186
C  <===                                                                   186
      CALL AGSRESET                                                       186
      AG_MOTHER = 'CAVE'                                                  186
      AG_X = WALL_XX                                                      186
      AG_Y = WALL_YY                                                      186
      AG_Z = WALL_ZZ                                                      186
      AG_TITLE = 'WALL'                                                   186
      AG_EXNAME = 'POSITION'                                              186
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         186
      CALL AXPOSITION                                                     186
C *                                                                       186
*
* ================================================================== Ceiling ==
*
C HALF WIDTH ALONG N-S                                                    191
      WALL_DX = CDIM_RMAX                                                 191
C HALF WIDTH ALONG T-B                                                    192
      WALL_DY = 0.5*(CDIM_RMAX-CDIM_DCEILING)                             192
C HALF WIDTH ALONG E-W                                                    193
      WALL_DZ = CDIM_DZ                                                   193
C CENTER ALONG N-S                                                        194
      WALL_XX = 0.0                                                       194
C CENTER AT AVG CEILING                                                   195
      WALL_YY = +CDIM_DCEILING + WALL_DY                                  195
C CENTER ALONG E-W                                                        196
      WALL_ZZ = 0.0                                                       196
*
      CALL AGSBEGM(MODULE,IPRIN)                                          198
C Check IPRIN<0                                                           198
      IF (IPRIN.LT.0) RETURN                                              198
C Create WALL :                                                           198
C Call To Local Procedure WALL                                            198
      ASSIGN 5030 TO BLOCK_WALL                                           198
      GO TO  5010                                                         198
5030  CONTINUE                                                            198
C *                                                                       198
C Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL_ZZ  198
C  <===                                                                   198
      CALL AGSRESET                                                       198
      AG_MOTHER = 'CAVE'                                                  198
      AG_X = WALL_XX                                                      198
      AG_Y = WALL_YY                                                      198
      AG_Z = WALL_ZZ                                                      198
      AG_TITLE = 'WALL'                                                   198
      AG_EXNAME = 'POSITION'                                              198
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         198
      CALL AXPOSITION                                                     198
C *                                                                       198
*
* ==================================================================== North ==
*
C HALF WIDTH ALONG N-S                                                    202
      WALL_DX = 0.5*(CDIM_RMAX-CDIM_DNORTH)                               202
C HALF WIDTH ALONG T-B                                                    203
      WALL_DY = CDIM_RMAX                                                 203
C HALF WIDTH ALONG E-W                                                    204
      WALL_DZ = CDIM_DZ                                                   204
C CENTER ALONG N-S                                                        205
      WALL_XX = -CDIM_DNORTH - WALL_DX                                    205
C CENTER AT AVG WALL                                                      206
      WALL_YY = 0.0                                                       206
C CENTER ALONG E-W                                                        207
      WALL_ZZ = 0.0                                                       207
*
      CALL AGSBEGM(MODULE,IPRIN)                                          209
C Check IPRIN<0                                                           209
      IF (IPRIN.LT.0) RETURN                                              209
C Create WALL :                                                           209
C Call To Local Procedure WALL                                            209
      ASSIGN 5040 TO BLOCK_WALL                                           209
      GO TO  5010                                                         209
5040  CONTINUE                                                            209
C *                                                                       209
C Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL_ZZ  209
C  <===                                                                   209
      CALL AGSRESET                                                       209
      AG_MOTHER = 'CAVE'                                                  209
      AG_X = WALL_XX                                                      209
      AG_Y = WALL_YY                                                      209
      AG_Z = WALL_ZZ                                                      209
      AG_TITLE = 'WALL'                                                   209
      AG_EXNAME = 'POSITION'                                              209
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         209
      CALL AXPOSITION                                                     209
C *                                                                       209
*
* =============================================================================
*                                WEST TUNNEL
* ==================================================================== Floor ==
*
C HALF WIDTH ALONG N-S                                                    215
      WALL_DX = TDIM_RMAX                                                 215
C HALF WIDTH ALONG T-B                                                    216
      WALL_DY = 0.5*(TDIM_RMAX-TDIM_DFLOOR)                               216
C HALF WIDTH ALONG E-W                                                    217
      WALL_DZ = TDIM_DZ/2                                                 217
C CENTER ALONG N-S                                                        218
      WALL_XX = 0.0                                                       218
C CENTER AT AVG FLOOR                                                     219
      WALL_YY = -TDIM_DFLOOR - WALL_DY                                    219
C CENTER ALONG E-W                                                        220
      WALL_ZZ = CDIM_DZ+WALL_DZ                                           220
*
      CALL AGSBEGM(MODULE,IPRIN)                                          222
C Check IPRIN<0                                                           222
      IF (IPRIN.LT.0) RETURN                                              222
C Create WALL :                                                           222
C Call To Local Procedure WALL                                            222
      ASSIGN 5050 TO BLOCK_WALL                                           222
      GO TO  5010                                                         222
5050  CONTINUE                                                            222
C *                                                                       222
C Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL_ZZ  222
C  <===                                                                   222
      CALL AGSRESET                                                       222
      AG_MOTHER = 'CAVE'                                                  222
      AG_X = WALL_XX                                                      222
      AG_Y = WALL_YY                                                      222
      AG_Z = WALL_ZZ                                                      222
      AG_TITLE = 'WALL'                                                   222
      AG_EXNAME = 'POSITION'                                              222
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         222
      CALL AXPOSITION                                                     222
C *                                                                       222
*
* ================================================================== Ceiling ==
*
C HALF WIDTH ALONG N-S                                                    226
      WALL_DX = TDIM_RMAX                                                 226
C HALF WIDTH ALONG T-B                                                    227
      WALL_DY = 0.5*(TDIM_RMAX-TDIM_DCEILING)                             227
C HALF WIDTH ALONG E-W                                                    228
      WALL_DZ = TDIM_DZ/2                                                 228
C CENTER ALONG N-S                                                        229
      WALL_XX = 0.0                                                       229
C CENTER AT AVG FLOOR                                                     230
      WALL_YY = +TDIM_DCEILING + WALL_DY                                  230
C CENTER ALONG E-W                                                        231
      WALL_ZZ = CDIM_DZ+WALL_DZ                                           231
*
      CALL AGSBEGM(MODULE,IPRIN)                                          233
C Check IPRIN<0                                                           233
      IF (IPRIN.LT.0) RETURN                                              233
C Create WALL :                                                           233
C Call To Local Procedure WALL                                            233
      ASSIGN 5060 TO BLOCK_WALL                                           233
      GO TO  5010                                                         233
5060  CONTINUE                                                            233
C *                                                                       233
C Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL_ZZ  233
C  <===                                                                   233
      CALL AGSRESET                                                       233
      AG_MOTHER = 'CAVE'                                                  233
      AG_X = WALL_XX                                                      233
      AG_Y = WALL_YY                                                      233
      AG_Z = WALL_ZZ                                                      233
      AG_TITLE = 'WALL'                                                   233
      AG_EXNAME = 'POSITION'                                              233
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         233
      CALL AXPOSITION                                                     233
C *                                                                       233
*
* ==================================================================== South ==
*
C HALF WIDTH ALONG N-S                                                    237
      WALL_DX = 0.5*(TDIM_RMAX-TDIM_DSOUTH)                               237
C HALF WIDTH ALONG T-B                                                    238
      WALL_DY = TDIM_RMAX                                                 238
C HALF WIDTH ALONG E-W                                                    239
      WALL_DZ = TDIM_DZ/2                                                 239
C CENTER ALONG N-S                                                        240
      WALL_XX = + TDIM_DSOUTH + WALL_DX                                   240
C CENTER AT AVG WALL                                                      241
      WALL_YY = 0.0                                                       241
C CENTER ALONG E-W                                                        242
      WALL_ZZ = CDIM_DZ+WALL_DZ                                           242
*
      CALL AGSBEGM(MODULE,IPRIN)                                          244
C Check IPRIN<0                                                           244
      IF (IPRIN.LT.0) RETURN                                              244
C Create WALL :                                                           244
C Call To Local Procedure WALL                                            244
      ASSIGN 5070 TO BLOCK_WALL                                           244
      GO TO  5010                                                         244
5070  CONTINUE                                                            244
C *                                                                       244
C Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL_ZZ  244
C  <===                                                                   244
      CALL AGSRESET                                                       244
      AG_MOTHER = 'CAVE'                                                  244
      AG_X = WALL_XX                                                      244
      AG_Y = WALL_YY                                                      244
      AG_Z = WALL_ZZ                                                      244
      AG_TITLE = 'WALL'                                                   244
      AG_EXNAME = 'POSITION'                                              244
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         244
      CALL AXPOSITION                                                     244
C *                                                                       244
*
* =============================================================================
*                                EAST TUNNEL
* ==================================================================== Floor ==
*
C HALF WIDTH ALONG N-S                                                    250
      WALL_DX = TDIM_RMAX                                                 250
C HALF WIDTH ALONG T-B                                                    251
      WALL_DY = 0.5*(TDIM_RMAX-TDIM_DFLOOR)                               251
C HALF WIDTH ALONG E-W                                                    252
      WALL_DZ = TDIM_DZ/2                                                 252
C CENTER ALONG N-S                                                        253
      WALL_XX = 0.0                                                       253
C CENTER AT AVG FLOOR                                                     254
      WALL_YY = -TDIM_DFLOOR - WALL_DY                                    254
C CENTER ALONG E-W                                                        255
      WALL_ZZ = CDIM_DZ+WALL_DZ                                           255
      WALL_ZZ = -WALL_ZZ                                                  256
*
      CALL AGSBEGM(MODULE,IPRIN)                                          258
C Check IPRIN<0                                                           258
      IF (IPRIN.LT.0) RETURN                                              258
C Create WALL :                                                           258
C Call To Local Procedure WALL                                            258
      ASSIGN 5080 TO BLOCK_WALL                                           258
      GO TO  5010                                                         258
5080  CONTINUE                                                            258
C *                                                                       258
C Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL_ZZ  258
C  <===                                                                   258
      CALL AGSRESET                                                       258
      AG_MOTHER = 'CAVE'                                                  258
      AG_X = WALL_XX                                                      258
      AG_Y = WALL_YY                                                      258
      AG_Z = WALL_ZZ                                                      258
      AG_TITLE = 'WALL'                                                   258
      AG_EXNAME = 'POSITION'                                              258
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         258
      CALL AXPOSITION                                                     258
C *                                                                       258
*
* ================================================================== Ceiling ==
*
C HALF WIDTH ALONG N-S                                                    262
      WALL_DX = TDIM_RMAX                                                 262
C HALF WIDTH ALONG T-B                                                    263
      WALL_DY = 0.5*(TDIM_RMAX-TDIM_DCEILING)                             263
C HALF WIDTH ALONG E-W                                                    264
      WALL_DZ = TDIM_DZ/2                                                 264
C CENTER ALONG N-S                                                        265
      WALL_XX = 0.0                                                       265
C CENTER AT AVG FLOOR                                                     266
      WALL_YY = +TDIM_DCEILING + WALL_DY                                  266
C CENTER ALONG E-W                                                        267
      WALL_ZZ = CDIM_DZ+WALL_DZ                                           267
      WALL_ZZ = -WALL_ZZ                                                  268
*
      CALL AGSBEGM(MODULE,IPRIN)                                          270
C Check IPRIN<0                                                           270
      IF (IPRIN.LT.0) RETURN                                              270
C Create WALL :                                                           270
C Call To Local Procedure WALL                                            270
      ASSIGN 5090 TO BLOCK_WALL                                           270
      GO TO  5010                                                         270
5090  CONTINUE                                                            270
C *                                                                       270
C Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL_ZZ  270
C  <===                                                                   270
      CALL AGSRESET                                                       270
      AG_MOTHER = 'CAVE'                                                  270
      AG_X = WALL_XX                                                      270
      AG_Y = WALL_YY                                                      270
      AG_Z = WALL_ZZ                                                      270
      AG_TITLE = 'WALL'                                                   270
      AG_EXNAME = 'POSITION'                                              270
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         270
      CALL AXPOSITION                                                     270
C *                                                                       270
*
* ==================================================================== South ==
*
C HALF WIDTH ALONG N-S                                                    274
      WALL_DX = 0.5*(TDIM_RMAX-TDIM_DSOUTH)                               274
C HALF WIDTH ALONG T-B                                                    275
      WALL_DY = TDIM_RMAX                                                 275
C HALF WIDTH ALONG E-W                                                    276
      WALL_DZ = TDIM_DZ/2                                                 276
C CENTER ALONG N-S                                                        277
      WALL_XX = + TDIM_DSOUTH + WALL_DX                                   277
C CENTER AT AVG WALL                                                      278
      WALL_YY = 0.0                                                       278
C CENTER ALONG E-W                                                        279
      WALL_ZZ = CDIM_DZ+WALL_DZ                                           279
      WALL_ZZ = -WALL_ZZ                                                  280
*
      CALL AGSBEGM(MODULE,IPRIN)                                          282
C Check IPRIN<0                                                           282
      IF (IPRIN.LT.0) RETURN                                              282
C Create WALL :                                                           282
C Call To Local Procedure WALL                                            282
      ASSIGN 5100 TO BLOCK_WALL                                           282
      GO TO  5010                                                         282
5100  CONTINUE                                                            282
C *                                                                       282
C Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL_ZZ  282
C  <===                                                                   282
      CALL AGSRESET                                                       282
      AG_MOTHER = 'CAVE'                                                  282
      AG_X = WALL_XX                                                      282
      AG_Y = WALL_YY                                                      282
      AG_Z = WALL_ZZ                                                      282
      AG_TITLE = 'WALL'                                                   282
      AG_EXNAME = 'POSITION'                                              282
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         282
      CALL AXPOSITION                                                     282
C *                                                                       282
*
*
* =============================================================================
*                               Tunnel Shielding
*
* The following code implements the two blocks which are located along the N
* and S walls in the two tunnels (E-S, E-N, W-S and W-N).  This is followed
* by the large shielding blocks which are stacked around the beamlines (E-B
* and W-B).
*
* ====================================================================== E-S ==
*
      WALL_DX = 0.5 * SHLD_XWIDTH(1)                                      295
      WALL_DY = 0.5 * SHLD_YWIDTH(1)                                      296
      WALL_DZ = 0.5 * SHLD_ZWIDTH(1)                                      297
*
      WALL_ZZ = -221.0 *2.5400 - WALL_DZ - CDIM_DZ                        299
      WALL_YY = -TDIM_DFLOOR + WALL_DY                                    300
      WALL_XX = +TDIM_DSOUTH - WALL_DX                                    301
*
      CALL AGSBEGM(MODULE,IPRIN)                                          303
C Check IPRIN<0                                                           303
      IF (IPRIN.LT.0) RETURN                                              303
C Create WALL :                                                           303
C Call To Local Procedure WALL                                            303
      ASSIGN 5110 TO BLOCK_WALL                                           303
      GO TO  5010                                                         303
5110  CONTINUE                                                            303
C *                                                                       303
C Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL_ZZ  303
C  <===                                                                   303
      CALL AGSRESET                                                       303
      AG_MOTHER = 'CAVE'                                                  303
      AG_X = WALL_XX                                                      303
      AG_Y = WALL_YY                                                      303
      AG_Z = WALL_ZZ                                                      303
      AG_TITLE = 'WALL'                                                   303
      AG_EXNAME = 'POSITION'                                              303
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         303
      CALL AXPOSITION                                                     303
C *                                                                       303
*
* ====================================================================== E-N ==
      WALL_DX = 0.5 * SHLD_XWIDTH(2)                                      306
      WALL_DY = 0.5 * SHLD_YWIDTH(2)                                      307
      WALL_DZ = 0.5 * SHLD_ZWIDTH(2)                                      308
*
      WALL_ZZ = -220.0 *2.5400 - WALL_DZ - CDIM_DZ                        310
      WALL_YY = -TDIM_DFLOOR + WALL_DY                                    311
      WALL_XX = -TDIM_DNORTH + WALL_DX                                    312
*
      CALL AGSBEGM(MODULE,IPRIN)                                          314
C Check IPRIN<0                                                           314
      IF (IPRIN.LT.0) RETURN                                              314
C Create WALL :                                                           314
C Call To Local Procedure WALL                                            314
      ASSIGN 5120 TO BLOCK_WALL                                           314
      GO TO  5010                                                         314
5120  CONTINUE                                                            314
C *                                                                       314
C Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL_ZZ  314
C  <===                                                                   314
      CALL AGSRESET                                                       314
      AG_MOTHER = 'CAVE'                                                  314
      AG_X = WALL_XX                                                      314
      AG_Y = WALL_YY                                                      314
      AG_Z = WALL_ZZ                                                      314
      AG_TITLE = 'WALL'                                                   314
      AG_EXNAME = 'POSITION'                                              314
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         314
      CALL AXPOSITION                                                     314
C *                                                                       314
*
* ====================================================================== W-S ==
      WALL_DX = 0.5 * SHLD_XWIDTH(3)                                      317
      WALL_DY = 0.5 * SHLD_YWIDTH(3)                                      318
      WALL_DZ = 0.5 * SHLD_ZWIDTH(3)                                      319
*
      WALL_ZZ = +512.3 *2.5400 + WALL_DZ                                  321
      WALL_YY = -TDIM_DFLOOR + WALL_DY                                    322
      WALL_XX = +TDIM_DSOUTH - WALL_DX                                    323
*
      CALL AGSBEGM(MODULE,IPRIN)                                          325
C Check IPRIN<0                                                           325
      IF (IPRIN.LT.0) RETURN                                              325
C Create WALL :                                                           325
C Call To Local Procedure WALL                                            325
      ASSIGN 5130 TO BLOCK_WALL                                           325
      GO TO  5010                                                         325
5130  CONTINUE                                                            325
C *                                                                       325
C Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL_ZZ  325
C  <===                                                                   325
      CALL AGSRESET                                                       325
      AG_MOTHER = 'CAVE'                                                  325
      AG_X = WALL_XX                                                      325
      AG_Y = WALL_YY                                                      325
      AG_Z = WALL_ZZ                                                      325
      AG_TITLE = 'WALL'                                                   325
      AG_EXNAME = 'POSITION'                                              325
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         325
      CALL AXPOSITION                                                     325
C *                                                                       325
*
* ====================================================================== W-N ==
      WALL_DX = 0.5 * SHLD_XWIDTH(4)                                      328
      WALL_DY = 0.5 * SHLD_YWIDTH(4)                                      329
      WALL_DZ = 0.5 * SHLD_ZWIDTH(4)                                      330
*
      WALL_ZZ = +512.3 *2.5400 + 144.25 *2.5400 - 236.0 *2.5400 +         333
     *WALL_DZ                                                             333
      WALL_YY = -TDIM_DFLOOR + WALL_DY                                    334
      WALL_XX = -TDIM_DNORTH + WALL_DX                                    335
*
      CALL AGSBEGM(MODULE,IPRIN)                                          337
C Check IPRIN<0                                                           337
      IF (IPRIN.LT.0) RETURN                                              337
C Create WALL :                                                           337
C Call To Local Procedure WALL                                            337
      ASSIGN 5140 TO BLOCK_WALL                                           337
      GO TO  5010                                                         337
5140  CONTINUE                                                            337
C *                                                                       337
C Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL_ZZ  337
C  <===                                                                   337
      CALL AGSRESET                                                       337
      AG_MOTHER = 'CAVE'                                                  337
      AG_X = WALL_XX                                                      337
      AG_Y = WALL_YY                                                      337
      AG_Z = WALL_ZZ                                                      337
      AG_TITLE = 'WALL'                                                   337
      AG_EXNAME = 'POSITION'                                              337
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         337
      CALL AXPOSITION                                                     337
C *                                                                       337
*
* =============================================================================
*
*
* =============================================================================
*                                  EAST SIDE
* =============================================================================
*
*                                                             82" concrete base
*                                                             beneath beam
C set material proerties                                                  348
      WALL_STUFF = 1                                                      349
      WALL_DX = 0.5 * ( 62.0 + 77.0 ) *2.5400                             350
      WALL_DY = 0.5 * 25.0 *2.5400                                        351
      WALL_DZ = 0.5 * 82.0 *2.5400                                        352
*
      WALL_ZZ = -( 538.0 + 120.0 ) *2.5400 - WALL_DZ                      354
      WALL_XX = +0.5 * ( 77.0 - 62.0 )                                    355
      WALL_YY = -TDIM_DFLOOR + WALL_DY                                    356
*
* East side
      CALL AGSBEGM(MODULE,IPRIN)                                          359
C Check IPRIN<0                                                           359
      IF (IPRIN.LT.0) RETURN                                              359
C Create WALL :                                                           359
C Call To Local Procedure WALL                                            359
      ASSIGN 5150 TO BLOCK_WALL                                           359
      GO TO  5010                                                         359
5150  CONTINUE                                                            359
C *                                                                       359
C Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL_ZZ  359
C  <===                                                                   359
      CALL AGSRESET                                                       359
      AG_MOTHER = 'CAVE'                                                  359
      AG_X = WALL_XX                                                      359
      AG_Y = WALL_YY                                                      359
      AG_Z = WALL_ZZ                                                      359
      AG_TITLE = 'WALL'                                                   359
      AG_EXNAME = 'POSITION'                                              359
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         359
      CALL AXPOSITION                                                     359
C *                                                                       359
*
*                                                             Steel stack above
*                                                             the beam pipe
C set material proerties                                                  363
      WALL_STUFF = 2                                                      364
*
      WALL_DZ = 0.5 * 45.0 *2.5400                                        366
C SAME DX                                                                 367
      WALL_DX = WALL_DX                                                   367
      WALL_DY = 0.5 * (146.0 - 65.0) *2.5400                              368
*
      WALL_ZZ = -( 538.0 + 120.0 ) *2.5400 - WALL_DZ                      370
C SAME XX                                                                 371
      WALL_XX = WALL_XX                                                   371
      WALL_YY = -TDIM_DFLOOR + 65.0 *2.5400 + WALL_DY                     372
*
      CALL AGSBEGM(MODULE,IPRIN)                                          374
C Check IPRIN<0                                                           374
      IF (IPRIN.LT.0) RETURN                                              374
C Create WALL :                                                           374
C Call To Local Procedure WALL                                            374
      ASSIGN 5160 TO BLOCK_WALL                                           374
      GO TO  5010                                                         374
5160  CONTINUE                                                            374
C *                                                                       374
C Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL_ZZ  374
C  <===                                                                   374
      CALL AGSRESET                                                       374
      AG_MOTHER = 'CAVE'                                                  374
      AG_X = WALL_XX                                                      374
      AG_Y = WALL_YY                                                      374
      AG_Z = WALL_ZZ                                                      374
      AG_TITLE = 'WALL'                                                   374
      AG_EXNAME = 'POSITION'                                              374
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         374
      CALL AXPOSITION                                                     374
C *                                                                       374
*
*
* =============================================================================
*                                  WEST SIDE
* =============================================================================
*
*                                                             82" concrete base
*                                                             beneath beam
C set material proerties                                                  383
      WALL_STUFF = 1                                                      384
      WALL_DX = 0.5 * ( 60.5 + 77.5 ) *2.5400                             385
      WALL_DY = 0.5 * 25.0 *2.5400                                        386
      WALL_DZ = 0.5 * 81.0 *2.5400                                        387
*
      WALL_ZZ = (512.3 + 144.5) *2.5400 + WALL_DZ                         389
      WALL_XX = +0.5 * ( 77.5 - 60.5 )                                    390
      WALL_YY = -TDIM_DFLOOR + WALL_DY                                    391
*
* East side
      CALL AGSBEGM(MODULE,IPRIN)                                          394
C Check IPRIN<0                                                           394
      IF (IPRIN.LT.0) RETURN                                              394
C Create WALL :                                                           394
C Call To Local Procedure WALL                                            394
      ASSIGN 5170 TO BLOCK_WALL                                           394
      GO TO  5010                                                         394
5170  CONTINUE                                                            394
C *                                                                       394
C Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL_ZZ  394
C  <===                                                                   394
      CALL AGSRESET                                                       394
      AG_MOTHER = 'CAVE'                                                  394
      AG_X = WALL_XX                                                      394
      AG_Y = WALL_YY                                                      394
      AG_Z = WALL_ZZ                                                      394
      AG_TITLE = 'WALL'                                                   394
      AG_EXNAME = 'POSITION'                                              394
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         394
      CALL AXPOSITION                                                     394
C *                                                                       394
*
*                                                             Steel stack above
*                                                             the beam pipe
C set material proerties                                                  398
      WALL_STUFF = 2                                                      399
*
      WALL_DZ = 0.5 * 45.0 *2.5400                                        401
C SAME DX                                                                 402
      WALL_DX = WALL_DX                                                   402
      WALL_DY = 0.5 * (145.5 - 65.0) *2.5400                              403
*
      WALL_ZZ = ( 512.3 + 144.5 ) *2.5400 + WALL_DZ                       405
C SAME XX                                                                 406
      WALL_XX = WALL_XX                                                   406
      WALL_YY = -TDIM_DFLOOR + 65.0 *2.5400 + WALL_DY                     407
*
      CALL AGSBEGM(MODULE,IPRIN)                                          409
C Check IPRIN<0                                                           409
      IF (IPRIN.LT.0) RETURN                                              409
C Create WALL :                                                           409
C Call To Local Procedure WALL                                            409
      ASSIGN 5180 TO BLOCK_WALL                                           409
      GO TO  5010                                                         409
5180  CONTINUE                                                            409
C *                                                                       409
C Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL_ZZ  409
C  <===                                                                   409
      CALL AGSRESET                                                       409
      AG_MOTHER = 'CAVE'                                                  409
      AG_X = WALL_XX                                                      409
      AG_Y = WALL_YY                                                      409
      AG_Z = WALL_ZZ                                                      409
      AG_TITLE = 'WALL'                                                   409
      AG_EXNAME = 'POSITION'                                              409
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         409
      CALL AXPOSITION                                                     409
C *                                                                       409
*
*
*****************************************************************************
*
* The following are guesstimate assuming that the scale of the drawings in XY
* are reasonable... they are to within 10%, but this is only an approximation
* Need better drawings of the shield walls.
*
*****************************************************************************
*
* =============================================================================
*                                    EAST SIDE
* =============================================================================
*                                                             Steel stack around
*                                                             the beam pipe
C set material proerties                                                  426
      WALL_STUFF = 2                                                      427
* cubs side of beam pipe  (north side)
C EXACT                                                                   429
      WALL_DZ = 0.5 * 45.0 *2.5400                                        429
C VERY GOOD APPROX                                                        430
      WALL_DY = 0.5 * (65.0 - 25.0 ) *2.5400                              430
C WAG (WILD ASS GUESS)                                                    431
      WALL_DX = 0.5 * (10.0 / 28.5) * (62.+77.) *2.5400                   431
*
C EXACT                                                                   434
      WALL_ZZ = -(538.0 + 120.0 + 82.0) *2.5400 + WALL_DZ                 434
C WAG                                                                     436
      WALL_XX = +4.0 * 4.877 *2.5400 + WALL_DX                            436
C VERY GOOD APPROX                                                        437
      WALL_YY = -TDIM_DFLOOR + 25.0 *2.5400 + WALL_DY                     437
*
      CALL AGSBEGM(MODULE,IPRIN)                                          439
C Check IPRIN<0                                                           439
      IF (IPRIN.LT.0) RETURN                                              439
C Create WALL :                                                           439
C Call To Local Procedure WALL                                            439
      ASSIGN 5190 TO BLOCK_WALL                                           439
      GO TO  5010                                                         439
5190  CONTINUE                                                            439
C *                                                                       439
C Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL_ZZ  439
C  <===                                                                   439
      CALL AGSRESET                                                       439
      AG_MOTHER = 'CAVE'                                                  439
      AG_X = WALL_XX                                                      439
      AG_Y = WALL_YY                                                      439
      AG_Z = WALL_ZZ                                                      439
      AG_TITLE = 'WALL'                                                   439
      AG_EXNAME = 'POSITION'                                              439
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         439
      CALL AXPOSITION                                                     439
C *                                                                       439
*
C set material proerties                                                  441
      WALL_STUFF = 1                                                      442
      WALL_DZ = 0.5 * (82.0-45.0) *2.5400                                 443
      WALL_DX = WALL_DX                                                   444
      WALL_DY = WALL_DY                                                   445
*
C EXACT                                                                   448
      WALL_ZZ = -(538.0+120.0) *2.5400 -WALL_DZ                           448
      WALL_XX = WALL_XX                                                   449
      WALL_YY = WALL_YY                                                   450
*
      CALL AGSBEGM(MODULE,IPRIN)                                          452
C Check IPRIN<0                                                           452
      IF (IPRIN.LT.0) RETURN                                              452
C Create WALL :                                                           452
C Call To Local Procedure WALL                                            452
      ASSIGN 5200 TO BLOCK_WALL                                           452
      GO TO  5010                                                         452
5200  CONTINUE                                                            452
C *                                                                       452
C Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL_ZZ  452
C  <===                                                                   452
      CALL AGSRESET                                                       452
      AG_MOTHER = 'CAVE'                                                  452
      AG_X = WALL_XX                                                      452
      AG_Y = WALL_YY                                                      452
      AG_Z = WALL_ZZ                                                      452
      AG_TITLE = 'WALL'                                                   452
      AG_EXNAME = 'POSITION'                                              452
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         452
      CALL AXPOSITION                                                     452
C *                                                                       452
*
C set material proerties                                                  455
      WALL_STUFF = 2                                                      456
* white sox side of beam pipe (south side)
C EXACT                                                                   458
      WALL_DZ = 0.5 * 45.0 *2.5400                                        458
C VERY GOOD APPROX                                                        459
      WALL_DY = 0.5 * (65.0 - 25.0 ) *2.5400                              459
C WAG (WILD ASS GUESS)                                                    460
      WALL_DX = 0.5 * (7.0 / 28.5) * (62.+77.) *2.5400                    460
*
C EXACT                                                                   463
      WALL_ZZ = -(538.0 + 120.0 + 82.0) *2.5400 + WALL_DZ                 463
C WAG                                                                     464
      WALL_XX = -4.0 * 4.877 *2.5400 - WALL_DX                            464
C VERY GOOD APPROX                                                        465
      WALL_YY = -TDIM_DFLOOR + 25.0 *2.5400 + WALL_DY                     465
*
      CALL AGSBEGM(MODULE,IPRIN)                                          467
C Check IPRIN<0                                                           467
      IF (IPRIN.LT.0) RETURN                                              467
C Create WALL :                                                           467
C Call To Local Procedure WALL                                            467
      ASSIGN 5210 TO BLOCK_WALL                                           467
      GO TO  5010                                                         467
5210  CONTINUE                                                            467
C *                                                                       467
C Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL_ZZ  467
C  <===                                                                   467
      CALL AGSRESET                                                       467
      AG_MOTHER = 'CAVE'                                                  467
      AG_X = WALL_XX                                                      467
      AG_Y = WALL_YY                                                      467
      AG_Z = WALL_ZZ                                                      467
      AG_TITLE = 'WALL'                                                   467
      AG_EXNAME = 'POSITION'                                              467
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         467
      CALL AXPOSITION                                                     467
C *                                                                       467
*
C set material proerties                                                  469
      WALL_STUFF = 1                                                      470
      WALL_DZ = 0.5 * (82.0-45.0) *2.5400                                 471
      WALL_DX = WALL_DX                                                   472
      WALL_DY = WALL_DY                                                   473
*
C EXACT                                                                   476
      WALL_ZZ = -(538.0+120.0) *2.5400 -WALL_DZ                           476
      WALL_XX = WALL_XX                                                   477
      WALL_YY = WALL_YY                                                   478
*
      CALL AGSBEGM(MODULE,IPRIN)                                          480
C Check IPRIN<0                                                           480
      IF (IPRIN.LT.0) RETURN                                              480
C Create WALL :                                                           480
C Call To Local Procedure WALL                                            480
      ASSIGN 5220 TO BLOCK_WALL                                           480
      GO TO  5010                                                         480
5220  CONTINUE                                                            480
C *                                                                       480
C Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL_ZZ  480
C  <===                                                                   480
      CALL AGSRESET                                                       480
      AG_MOTHER = 'CAVE'                                                  480
      AG_X = WALL_XX                                                      480
      AG_Y = WALL_YY                                                      480
      AG_Z = WALL_ZZ                                                      480
      AG_TITLE = 'WALL'                                                   480
      AG_EXNAME = 'POSITION'                                              480
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         480
      CALL AXPOSITION                                                     480
C *                                                                       480
*
*
* =============================================================================
*                                    WEST SIDE
* =============================================================================
*                                                             Steel stack around
*                                                             the beam pipe
C set material proerties                                                  491
      WALL_STUFF = 2                                                      492
* cubs side of beam pipe  (north side)
C EXACT                                                                   494
      WALL_DZ = 0.5 * 45.0 *2.5400                                        494
C VERY GOOD APPROX                                                        495
      WALL_DY = 0.5 * (65.0 - 25.0 ) *2.5400                              495
C WAG (WILD ASS GUESS)                                                    496
      WALL_DX = 0.5 * (10.0 / 28.5) * (62.+77.) *2.5400                   496
*
C EXACT                                                                   499
      WALL_ZZ = +(512.3 + 144.25 + 81.0) *2.5400 - WALL_DZ                499
C WAG                                                                     501
      WALL_XX = +4.0 * 4.877 *2.5400 + WALL_DX                            501
C VERY GOOD APPROX                                                        502
      WALL_YY = -TDIM_DFLOOR + 25.0 *2.5400 + WALL_DY                     502
*
      CALL AGSBEGM(MODULE,IPRIN)                                          504
C Check IPRIN<0                                                           504
      IF (IPRIN.LT.0) RETURN                                              504
C Create WALL :                                                           504
C Call To Local Procedure WALL                                            504
      ASSIGN 5230 TO BLOCK_WALL                                           504
      GO TO  5010                                                         504
5230  CONTINUE                                                            504
C *                                                                       504
C Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL_ZZ  504
C  <===                                                                   504
      CALL AGSRESET                                                       504
      AG_MOTHER = 'CAVE'                                                  504
      AG_X = WALL_XX                                                      504
      AG_Y = WALL_YY                                                      504
      AG_Z = WALL_ZZ                                                      504
      AG_TITLE = 'WALL'                                                   504
      AG_EXNAME = 'POSITION'                                              504
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         504
      CALL AXPOSITION                                                     504
C *                                                                       504
*
C set material proerties                                                  506
      WALL_STUFF = 1                                                      507
      WALL_DZ = 0.5 * (81.0-45.0) *2.5400                                 508
      WALL_DX = WALL_DX                                                   509
      WALL_DY = WALL_DY                                                   510
*
C EXACT                                                                   513
      WALL_ZZ = +(512.3+144.25+(81.0-45.0)) *2.5400 -WALL_DZ              513
      WALL_XX = WALL_XX                                                   514
      WALL_YY = WALL_YY                                                   515
*
      CALL AGSBEGM(MODULE,IPRIN)                                          517
C Check IPRIN<0                                                           517
      IF (IPRIN.LT.0) RETURN                                              517
C Create WALL :                                                           517
C Call To Local Procedure WALL                                            517
      ASSIGN 5240 TO BLOCK_WALL                                           517
      GO TO  5010                                                         517
5240  CONTINUE                                                            517
C *                                                                       517
C Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL_ZZ  517
C  <===                                                                   517
      CALL AGSRESET                                                       517
      AG_MOTHER = 'CAVE'                                                  517
      AG_X = WALL_XX                                                      517
      AG_Y = WALL_YY                                                      517
      AG_Z = WALL_ZZ                                                      517
      AG_TITLE = 'WALL'                                                   517
      AG_EXNAME = 'POSITION'                                              517
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         517
      CALL AXPOSITION                                                     517
C *                                                                       517
*
*
C set material proerties                                                  520
      WALL_STUFF = 2                                                      521
* white sox side of beam pipe (south side)
C EXACT                                                                   523
      WALL_DZ = 0.5 * 45.0 *2.5400                                        523
C VERY GOOD APPROX                                                        524
      WALL_DY = 0.5 * (65.0 - 25.0 ) *2.5400                              524
C WAG (WILD ASS GUESS)                                                    525
      WALL_DX = 0.5 * (7.0 / 28.5) * (62.+77.) *2.5400                    525
*
C EXACT                                                                   528
      WALL_ZZ = +(512.3 + 144.25 + 81.0) *2.5400 - WALL_DZ                528
C WAG                                                                     529
      WALL_XX = -4.0 * 4.877 *2.5400 - WALL_DX                            529
C VERY GOOD APPROX                                                        530
      WALL_YY = -TDIM_DFLOOR + 25.0 *2.5400 + WALL_DY                     530
*
      CALL AGSBEGM(MODULE,IPRIN)                                          532
C Check IPRIN<0                                                           532
      IF (IPRIN.LT.0) RETURN                                              532
C Create WALL :                                                           532
C Call To Local Procedure WALL                                            532
      ASSIGN 5250 TO BLOCK_WALL                                           532
      GO TO  5010                                                         532
5250  CONTINUE                                                            532
C *                                                                       532
C Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL_ZZ  532
C  <===                                                                   532
      CALL AGSRESET                                                       532
      AG_MOTHER = 'CAVE'                                                  532
      AG_X = WALL_XX                                                      532
      AG_Y = WALL_YY                                                      532
      AG_Z = WALL_ZZ                                                      532
      AG_TITLE = 'WALL'                                                   532
      AG_EXNAME = 'POSITION'                                              532
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         532
      CALL AXPOSITION                                                     532
C *                                                                       532
*
C set material proerties                                                  534
      WALL_STUFF = 1                                                      535
      WALL_DZ = 0.5 * (81.0-45.0) *2.5400                                 536
      WALL_DX = WALL_DX                                                   537
      WALL_DY = WALL_DY                                                   538
*
C EXACT                                                                   541
      WALL_ZZ = +(512.3+144.25+(81.0-45.0)) *2.5400 -WALL_DZ              541
      WALL_XX = WALL_XX                                                   542
      WALL_YY = WALL_YY                                                   543
*
      CALL AGSBEGM(MODULE,IPRIN)                                          545
C Check IPRIN<0                                                           545
      IF (IPRIN.LT.0) RETURN                                              545
C Create WALL :                                                           545
C Call To Local Procedure WALL                                            545
      ASSIGN 5260 TO BLOCK_WALL                                           545
      GO TO  5010                                                         545
5260  CONTINUE                                                            545
C *                                                                       545
C Geant POSITION : WALL ===> MOTHER='CAVE' X=WALL_XX Y=WALL_YY Z=WALL_ZZ  545
C  <===                                                                   545
      CALL AGSRESET                                                       545
      AG_MOTHER = 'CAVE'                                                  545
      AG_X = WALL_XX                                                      545
      AG_Y = WALL_YY                                                      545
      AG_Z = WALL_ZZ                                                      545
      AG_TITLE = 'WALL'                                                   545
      AG_EXNAME = 'POSITION'                                              545
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         545
      CALL AXPOSITION                                                     545
C *                                                                       545
*
      CALL AGSBEGM(MODULE,IPRIN)                                          555
C Check IPRIN<0                                                           555
      IF (IPRIN.LT.0) RETURN                                              555
C Create TWLL :                                                           555
C Call To Local Procedure TWLL                                            555
      ASSIGN 5280 TO BLOCK_TWLL                                           555
      GO TO  5270                                                         555
5280  CONTINUE                                                            555
C *                                                                       555
* =============================================================================
      CALL AGSENDM                                                        557
      RETURN                                                              557
C Begin Local Procedure WALL                                              557
C WALL IS THE FLOOR (OR BOTTOM) OF THE CAVE                               557
5010  CONTINUE                                                            557
         AG_TITLE='WALL'                                                  557
         AG_MODULE=MODULE                                                 557
         AG_MARK='V00'                                                    557
         CALL AGSPUSH                                                     557
         AG_MODULE=MODULE                                                 557
C    *                                                                    557
*
C    Check WALL_STUFF .EQ. KCONCRETE                                      559
         IF (WALL_STUFF .EQ. 1) THEN                                      559
C    Geant COMPONENT : SI ===> Z=14 A=28.08 W=1 <===                      560
         CALL AGSRESET                                                    560
         AG_Z = 14                                                        560
         AG_A = 28.08                                                     560
         AG_W = 1                                                         560
         AG_TITLE = 'SI'                                                  560
         AG_EXNAME = 'COMPONENT'                                          560
         AG_PARLIST = 'Z_A_W'                                             560
         CALL AXCOMPONENT                                                 560
C    *                                                                    560
C    Geant COMPONENT : O2 ===> Z=8 A=16 W=2 <===                          560
         CALL AGSRESET                                                    561
         AG_Z = 8                                                         561
         AG_A = 16                                                        561
         AG_W = 2                                                         561
         AG_TITLE = 'O2'                                                  561
         AG_EXNAME = 'COMPONENT'                                          561
         AG_PARLIST = 'Z_A_W'                                             561
         CALL AXCOMPONENT                                                 561
C    *                                                                    561
C    Geant MIXTURE : CONCRETE ===> DENS=2.5 <===                          561
         CALL AGSRESET                                                    562
         AG_DENS = 2.5                                                    562
         AG_TITLE = 'CONCRETE'                                            562
         AG_EXNAME = 'MIXTURE'                                            562
         AG_PARLIST = 'DENS'                                              562
         CALL AXMIXTURE                                                   562
C    *                                                                    562
         ENDIF                                                            562
C    Check WALL_STUFF .EQ. KSTEEL                                         564
         IF (WALL_STUFF .EQ. 2) THEN                                      564
C    Geant MATERIAL : IRON ===> inherited arguments used <===             565
         CALL AGSRESET                                                    565
         AG_TITLE = 'IRON'                                                565
         AG_EXNAME = 'MATERIAL'                                           565
         AG_PARLIST = 'NONE'                                              565
         CALL AXMATERIAL                                                  565
C    *                                                                    565
C    Geant MATERIAL : IRON ===> ISVOL=0 <===                              565
         CALL AGSRESET                                                    566
         AG_ISVOL = 0                                                     566
         AG_TITLE = 'IRON'                                                566
         AG_EXNAME = 'MATERIAL'                                           566
         AG_PARLIST = 'ISVOL'                                             566
         CALL AXMATERIAL                                                  566
C    *                                                                    566
         ENDIF                                                            566
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           568
         CALL AGSRESET                                                    568
         AG_TITLE = 'STANDARD'                                            568
         AG_EXNAME = 'MEDIUM'                                             568
         AG_PARLIST = 'NONE'                                              568
         CALL AXMEDIUM                                                    568
C    *                                                                    568
C    Geant ATTRIBUTE : WALL ===> SEEN=0 <===                              568
         CALL AGSRESET                                                    569
         AG_SEEN = 0                                                      569
         AG_TITLE = 'WALL'                                                569
         AG_EXNAME = 'ATTRIBUTE'                                          569
         AG_PARLIST = 'SEEN'                                              569
         CALL AXATTRIBUTE                                                 569
C    *                                                                    569
C    Geant SHAPE : BOX ===> DZ=WALL_DZ DX=WALL_DX DY=WALL_DY <===         569
         CALL AGSRESET                                                    570
         AG_DZ = WALL_DZ                                                  570
         AG_DX = WALL_DX                                                  570
         AG_DY = WALL_DY                                                  570
         AG_TITLE = 'BOX'                                                 570
         AG_EXNAME = 'SHAPE'                                              570
         AG_PARLIST = 'DZ_DX_DY'                                          570
         CALL AXSHAPE                                                     570
C    *                                                                    570
*
5011  CALL AGSPOPB                                                        572
      GO TO BLOCK_WALL, (5010 ,5020,5030,5040,5050,5060,5070,5080,5090,   572
     *5100,5110,5120,5130,5140,5150,5160,5170,5180,5190,5200,5210,5220,   572
     *5230,5240,5250,5260)                                                572
C End of Local Procedure                                                  572
C WALL                                                                    572
* =============================================================================
* =============================================================================
C Begin Local Procedure TWLL                                              576
C TWLL IS THE FLOOR (OR BOTTOM) OF THE CAVE                               576
5270  CONTINUE                                                            576
         AG_TITLE='TWLL'                                                  576
         AG_MODULE=MODULE                                                 576
         AG_MARK='V00'                                                    576
         CALL AGSPUSH                                                     576
         AG_MODULE=MODULE                                                 576
C    *                                                                    576
*
C    Geant COMPONENT : SI ===> Z=14 A=28.08 W=1 <===                      578
         CALL AGSRESET                                                    578
         AG_Z = 14                                                        578
         AG_A = 28.08                                                     578
         AG_W = 1                                                         578
         AG_TITLE = 'SI'                                                  578
         AG_EXNAME = 'COMPONENT'                                          578
         AG_PARLIST = 'Z_A_W'                                             578
         CALL AXCOMPONENT                                                 578
C    *                                                                    578
C    Geant COMPONENT : O2 ===> Z=8 A=16 W=2 <===                          578
         CALL AGSRESET                                                    579
         AG_Z = 8                                                         579
         AG_A = 16                                                        579
         AG_W = 2                                                         579
         AG_TITLE = 'O2'                                                  579
         AG_EXNAME = 'COMPONENT'                                          579
         AG_PARLIST = 'Z_A_W'                                             579
         CALL AXCOMPONENT                                                 579
C    *                                                                    579
C    Geant MIXTURE : CONCRETE ===> DENS=2.5 <===                          579
         CALL AGSRESET                                                    580
         AG_DENS = 2.5                                                    580
         AG_TITLE = 'CONCRETE'                                            580
         AG_EXNAME = 'MIXTURE'                                            580
         AG_PARLIST = 'DENS'                                              580
         CALL AXMIXTURE                                                   580
C    *                                                                    580
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           580
         CALL AGSRESET                                                    581
         AG_TITLE = 'STANDARD'                                            581
         AG_EXNAME = 'MEDIUM'                                             581
         AG_PARLIST = 'NONE'                                              581
         CALL AXMEDIUM                                                    581
C    *                                                                    581
*
C    Geant SHAPE : BOX ===> DZ=WALL_DZ DX=WALL_DX DY=WALL_DY <===         583
         CALL AGSRESET                                                    583
         AG_DZ = WALL_DZ                                                  583
         AG_DX = WALL_DX                                                  583
         AG_DY = WALL_DY                                                  583
         AG_TITLE = 'BOX'                                                 583
         AG_EXNAME = 'SHAPE'                                              583
         AG_PARLIST = 'DZ_DX_DY'                                          583
         CALL AXSHAPE                                                     583
C    *                                                                    583
*
5271  CALL AGSPOPB                                                        585
      GO TO BLOCK_TWLL, (5270 ,5280)                                      585
C End of Local Procedure                                                  585
C WALL                                                                    585
* =============================================================================
      END                                                                 589
