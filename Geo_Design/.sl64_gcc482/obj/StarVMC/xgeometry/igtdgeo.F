C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
* $Id: igtdgeo.g,v 1.6 2007/11/13 21:30:34 perev Exp $
* $Log: igtdgeo.g,v $
* Revision 1.6  2007/11/13 21:30:34  perev
* material ALKAP fixed
*
* Revision 1.5  2006/11/14 00:18:52  potekhin
* Allow for a modified set of parameters as per recent change
* in the IGT geometry (Ross, Bernd)
*
* Revision 1.4  2006/10/21 18:16:35  potekhin
* Add 'MANY' option to ensure correct material
* taken into account when working with the new support structure
*
* Revision 1.3  2006/10/06 23:07:02  potekhin
* An extraneous Denstity=1.7 in one of the G10
* references caused and extr material to be created,
* which is in fact a bug. Fixed that.
*
* Revision 1.2  2006/06/26 17:51:33  potekhin
* Added a comment to clarify the configuration of the detector:
* there are currently a fwe GEM devices described, so we need
* to add some clarity to their denomination
*
* Revision 1.1  2006/05/05 17:22:12  potekhin
* The first cut of the forward GEM disks ("smaller
* aperture design as compared to fgt") by Doug Hasell.
* Redundant code removed, and other cosmetic changes
* by Maxim.
*
*
* IGTDGEO - Inner GEM Tracking Detector Geometry
*
* Describes four disks of triple GEM detectors for a possible STAR upgrade
* Distinct from, and alternative to, the previosuly proposed
* LAGRGE GEM surfaces
*
* Inner and outer radii as well as position along the Z axis are variable
* parameters as well as the geometry, foil thicknesses, and spacing of the
* triple GEM detectors.
*
* First design 2006/05/01 - D.K. Hasell
*
******************************************************************************
      SUBROUTINE IGTDGEO                                                  44
C IGTDGEO IS THE GEOMETRY OF THE INNER GEM TRACKING DETECTOR              44
      IMPLICIT NONE                                                       44
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       44
      DATA MODULE/'IGTDGEO'/, BANK_TITLE/'*'/                             44
      DATA MCOMMENT/'IS THE GEOMETRY OF THE INNER GEM TRACKING            44
     *DETECTOR'/                                                          44
      LOGICAL FIRST                                                       44
      DATA FIRST/.TRUE./                                                  44
      SAVE FIRST                                                          44
      INTEGER JUMPAD                                                      44
      EXTERNAL JUMPAD                                                     44
      INTEGER OK                                                          44
      PARAMETER (OK=0)                                                    44
      INTEGER AG_I,AG_J                                                   44
      SAVE AG_I,AG_J                                                      44
C CREATED 5/1/06                                                          44
      DATA AG_I/0/,AG_J/0/                                                45
      CHARACTER*40 CREATED /'5/1/06'/                                     45
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  45
C AUTHOR DOUGLAS HASELL                                                   45
      CHARACTER*40 AUTHOR /'DOUGLAS HASELL'/                              46
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  46
******************************************************************************
C - Advanced Geant interface                                              48
      CHARACTER*20 AG_MODULE,AG_TITLE,AG_EXNAME,AG_PARLIST,AG_CHDIR,      48
     *AG_MATERIAL,AG_MIXTURE,AG_COMPONENT,AG_MEDIUM                       48
      CHARACTER*4 AG_VOLUME,AG_MOTHER,AG_SHAPE,AG_CNICK,AG_KONLY,         48
     *AG_OPTION,AG_ORT,AG_MARK                                            48
      INTEGER AG_BEGCOM,AG_IVOLUME,AG_IMOTHER,AG_IGNUM,AG_ISHAPE,         48
     *AG_IMED,AG_IMAT,AG_IFIELD, AG_NLMAT,AG_IERROR,AG_NWBUF,AG_NPAR,     48
     *AG_ISTATUS,AG_IROT,AG_NBITS,AG_SERIAL, AG_ATTRIBUTE(6),AG_WORK,     48
     *AG_SEEN,AG_LSTY,AG_LWID,AG_COLO,AG_FILL,AG_JDU,AG_ORTI, AG_NDIV,    48
     *AG_IAXIS,AG_NDVMAX,AG_IPRIN,AG_RESET1,AG_RESET2,AG_BEGSCR,          48
     *AG_ENDSCR, AG_IDTYPE,AG_ISET,AG_IDET,AG_ISVOL,AG_LEVEL,AG_IRESER,   48
     *AG_IGEOM,AG_STANDALONE, AG_LSTACK,AG_NWUHIT,AG_NWUVOL,AG_MAGIC,     48
     *AG_LDETU,AG_NPDIV,AG_NPDV,AG_NZ,AG_NCOPY, AG_IDEBU,AG_IGRAP,        48
     *AG_IHIST,AG_IMFLD,AG_ISIMU,AG_ENDSAVE,AG_CODE,AG_TRKTYP, AG_MODE,   48
     *AG_ECODE,AG_PDG,IPRIN, AG_ALPHAN,AG_ALPHAK(10),IGOR                 48
      REAL AG_FIELDM,AG_TMAXFD,AG_STEMAX,AG_DEEMAX,AG_EPSIL,AG_STMIN,     48
     *AG_DENS,AG_RADL,AG_ABSL, AG_THETAX,AG_PHIX,AG_THETAY,AG_PHIY,       48
     *AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,AG_ALFAZ, AG_ALPHAL(10),        48
     *AG_ALPHAX,AG_ALPHAY,AG_ALPHAZ,AG_TYPE, AG_DX, AG_DX1, AG_DX2,       48
     *AG_DY, AG_DY1, AG_DY2, AG_DMAXMS, AG_TWIST, AG_THET, AG_THE1,       48
     *AG_THE2, AG_PHI, AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2,       48
     *AG_RMIN, AG_RMAX, AG_RMN, AG_RMX, AG_ZI , AG_RMN1, AG_RMN2,         48
     *AG_RMX1, AG_RMX2, AG_H1, AG_H2, AG_BL1, AG_BL2, AG_TL1, AG_TL2,     48
     *AG_DPHI, AG_DZ, AG_TWIS, AG_X, AG_Y, AG_Z, AG_A, AG_ZA, AG_W,       48
     *AG_STEP, AG_C0, AG_PAR, AG_AA, AG_ZZ, AG_WW, AG_LX, AG_LY, AG_LZ,   48
     *AG_HX, AG_HY, AG_HZ, AG_P1, AG_P2, AG_STACK,AG_UBUF,AG_XHMAX,       48
     *AG_YHMAX,AG_ZHMAX,AG_RHMAX,AG_FHMAX,AG_FHMIN,AG_BIN, AG_CHARGE,     48
     *AG_MASS,AG_TLIFE,AG_BRATIO                                          48
      PARAMETER (AG_LSTACK=130,AG_NWUHIT=10,AG_NWUVOL=3,AG_MAGIC=         48
     *-696969,AG_LDETU=250)                                               48
      COMMON/AGCGLOB/ AG_MODULE,AG_CHDIR,AG_LEVEL,AG_IDTYPE,AG_IERROR,    48
     *AG_STANDALONE,IPRIN, AG_IPRIN,AG_IGEOM,AG_IDEBU,AG_IGRAP,AG_IHIST,  48
     *AG_IMFLD,AG_ISIMU                                                   48
C Inherited variables a: saved during internal calls                      48
C b: reset at the entry in a block and then saved                         48
      COMMON/AGCPARA/ AG_BEGCOM, AG_IVOLUME, AG_IMOTHER, AG_IGNUM,        48
     *AG_ISHAPE, AG_IMED, AG_IMAT, AG_IFIELD, AG_FIELDM, AG_TMAXFD,       48
     *AG_STEMAX, AG_DEEMAX, AG_EPSIL, AG_STMIN, AG_DENS, AG_RADL,         48
     *AG_ABSL, AG_DX, AG_DX1, AG_DX2, AG_DY, AG_DY1, AG_DY2, AG_RMN1,     48
     *AG_RMN2, AG_RMX1, AG_RMX2, AG_THET, AG_THE1, AG_THE2, AG_PHI,       48
     *AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2, AG_H1, AG_BL1,         48
     *AG_TL1, AG_H2, AG_BL2, AG_TL2, AG_RMIN, AG_RMAX, AG_DPHI,           48
     *AG_NPDIV, AG_NZ, AG_DZ, AG_TWIS, AG_LX, AG_LY, AG_LZ, AG_HX,        48
     *AG_HY, AG_HZ, AG_A, AG_ZA, AG_W, AG_NLMAT, AG_WORK, AG_SEEN,        48
     *AG_LSTY, AG_LWID, AG_COLO, AG_FILL, AG_SERIAL, AG_ISVOL,            48
     *AG_ISTATUS, AG_ZI(16), AG_RMN(16), AG_RMX(16), AG_VOLUME,           48
     *AG_MOTHER, AG_SHAPE, AG_CNICK, AG_ENDSAVE, AG_RESET1, AG_THETAX,    48
     *AG_PHIX,AG_THETAY,AG_PHIY,AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,      48
     *AG_ALFAZ, AG_X,AG_Y,AG_Z, AG_STEP,AG_C0,AG_NDIV,AG_IAXIS,           48
     *AG_NDVMAX,AG_ORTI,AG_NCOPY, AG_ALPHAN,AG_ALPHAK,AG_ALPHAL,          48
     *AG_RESET2, AG_KONLY, AG_ORT, AG_MARK                                48
C local variables valid inside same block                                 48
      COMMON/AGCLOCA/ AG_BEGSCR,AG_UBUF(100),AG_PAR(100),AG_AA(20),       48
     *AG_ZZ(20),AG_WW(20),AG_NWBUF, AG_XHMAX,AG_YHMAX,AG_ZHMAX,AG_RHMAX,  48
     *AG_FHMAX,AG_FHMIN,AG_NBITS,AG_BIN,AG_TYPE, AG_IROT,AG_NPAR,         48
     *AG_ISET,AG_IDET,AG_JDU,AG_IRESER, AG_ENDSCR, AG_TITLE,AG_EXNAME,    48
     *AG_PARLIST,AG_MATERIAL,AG_MIXTURE, AG_COMPONENT,AG_MEDIUM,          48
     *AG_OPTION                                                           48
      COMMON/AGCPART/AG_CODE,AG_TRKTYP,AG_MASS,AG_CHARGE,AG_TLIFE,        48
     *AG_BRATIO(6),AG_MODE(6),AG_PDG,AG_ECODE                             48
      COMMON/AGCSTAC/ AG_STACK(AG_LSTACK,15)                              48
C temporarly                                                              48
      EQUIVALENCE (AG_ATTRIBUTE,AG_WORK),(AG_STEMAX,AG_DMAXMS),           48
     *(AG_ALPHAX,AG_ALFAX), (AG_ALPHAY,AG_ALFAY),(AG_ALPHAZ,AG_ALFAZ),    48
     *(AG_TWIST,AG_TWIS),(AG_NPDIV,AG_NPDV),  (AG_P1,AG_HX),(AG_P2,       48
     *AG_HY)                                                              48
      REAL MIL_P,INCH_P                                                   48
      PARAMETER (INCH_P=2.54,MIL_P=0.00254)                               48
C - GEANT description of logical units                                    48
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            48
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      48
      COMMON/GCMAIL/ CHMAIL                                               48
      CHARACTER*132 CHMAIL                                                48
* Declare variables used in the code.
C CENTRE OF COMPLETE ASSEMBLY ALONG Z AXIS                                52
      REAL CENTER                                                         52
C LENGTH OF COMPLETE ASSEMBLY ALONG Z AXIS                                53
      REAL LENGTH                                                         53
C THICKNESS IN Z OF A SINGLE GEM DISK                                     54
      REAL THICK                                                          54
C USED TO DETERMINE THE MINIMUM RADIUS                                    55
      REAL RMN                                                            55
C USED TO DETERMINE THE MAXIMUM RADIUS                                    56
      REAL RMX                                                            56
C INNER RADIUS FOR EACH GEM DETECTOR                                      57
      REAL RIN                                                            57
C OUTER RADIUS FOR EACH GEM DETECTOR                                      58
      REAL ROUT                                                           58
C ACCUMULATES Z POSITION FOR LAYERS IN GEM                                59
      REAL ZSUM                                                           59
C HALF THICKNESS OF LAYER IN EACH GEM                                     60
      REAL THK                                                            60
C INDEX FOR COUNTING OVER THE GEM DISKS                                   62
      INTEGER DISK                                                        62
C INDEX FRO COUNTING OVER THE LAYERS IN GEM                               63
      INTEGER LAYER                                                       63
* Declare the blocks to be defined in the code.
      INTEGER BLOCK_IGMO                                                  66
      INTEGER BLOCK_IGDO                                                  66
      INTEGER BLOCK_IGFO                                                  66
      INTEGER BLOCK_IGIS                                                  66
      INTEGER BLOCK_IGOS                                                  66
      INTEGER BLOCK_IGRL                                                  66
      INTEGER BLOCK_IGAL                                                  66
C -                                                                       68
      REAL IGTV_VERSION                                                   68
      EQUIVALENCE (BANK_IGTV(3),IGTV_VERSION,RE_IGTV_VERSION),            68
     *(NAMES_IGTV(1),NIGTV_VERSION)                                       68
      EQUIVALENCE (COMMEN_IGTV(1),CIGTV_VERSION),(MAP_IGTV(1,1),          68
     *MIGTV_VERSION)                                                      68
      CHARACTER CIGTV_VERSION*40/' '/                                     68
      CHARACTER NIGTV_VERSION*12/'R.VERSION'/                             68
      INTEGER MIGTV_VERSION(2)/1,1/                                       68
      REAL RE_IGTV_VERSION                                                68
C -                                                                       68
      INTEGER IGTV_CONFIG                                                 68
      EQUIVALENCE (BANK_IGTV(4),IGTV_CONFIG,RE_IGTV_CONFIG),              68
     *(NAMES_IGTV(2),NIGTV_CONFIG)                                        68
      EQUIVALENCE (COMMEN_IGTV(2),CIGTV_CONFIG),(MAP_IGTV(1,2),           68
     *MIGTV_CONFIG)                                                       68
      CHARACTER CIGTV_CONFIG*40/' '/                                      68
      CHARACTER NIGTV_CONFIG*12/'I.CONFIG'/                               68
      INTEGER MIGTV_CONFIG(2)/1,1/                                        68
      REAL RE_IGTV_CONFIG                                                 68
C -                                                                       68
      INTEGER LEN_IGTV,LLEN_IGTV,LINK_IGTV                                68
      PARAMETER (LEN_IGTV=4)                                              68
      PARAMETER (LLEN_IGTV=2)                                             68
      INTEGER FLAG_IGTV/0/,MAP_IGTV(2,LLEN_IGTV),IDNUM_IGTV(10)           68
      CHARACTER NAMES_IGTV(LLEN_IGTV)*12,COMMEN_IGTV(LLEN_IGTV)*40        68
      DATA IDNUM_IGTV/10*0/                                               68
      REAL BANK_IGTV(LEN_IGTV)                                            68
      CHARACTER*60 BPATH_IGTV/' '/                                        68
      SAVE FLAG_IGTV,MAP_IGTV,IDNUM_IGTV,NAMES_IGTV,COMMEN_IGTV,          68
     *BPATH_IGTV                                                          68
      DATA LINK_IGTV/0/,BANK_IGTV/LEN_IGTV*0/                             68
C *                                                                       68
* Declare the structure IGTG which defines the detector geometry.
C -                                                                       71
      REAL IGTG_CONFIG                                                    71
      EQUIVALENCE (BANK_IGTG(3),IGTG_CONFIG,RE_IGTG_CONFIG),              71
     *(NAMES_IGTG(1),NIGTG_CONFIG)                                        71
      EQUIVALENCE (COMMEN_IGTG(1),CIGTG_CONFIG),(MAP_IGTG(1,1),           71
     *MIGTG_CONFIG)                                                       71
      CHARACTER CIGTG_CONFIG*40/' '/                                      71
      CHARACTER NIGTG_CONFIG*12/'R.CONFIG'/                               71
      INTEGER MIGTG_CONFIG(2)/1,1/                                        71
      REAL RE_IGTG_CONFIG                                                 71
C -                                                                       71
      REAL IGTG_RI(4)                                                     71
      EQUIVALENCE (BANK_IGTG(4),IGTG_RI,RE_IGTG_RI),(NAMES_IGTG(2),       71
     *NIGTG_RI)                                                           71
      EQUIVALENCE (COMMEN_IGTG(2),CIGTG_RI),(MAP_IGTG(1,2),MIGTG_RI)      71
      CHARACTER CIGTG_RI*40/' '/                                          71
      CHARACTER NIGTG_RI*12/'R.RI'/                                       71
      INTEGER MIGTG_RI(2)/4,1/                                            71
      REAL RE_IGTG_RI                                                     71
C -                                                                       71
      REAL IGTG_RO(4)                                                     71
      EQUIVALENCE (BANK_IGTG(8),IGTG_RO,RE_IGTG_RO),(NAMES_IGTG(3),       71
     *NIGTG_RO)                                                           71
      EQUIVALENCE (COMMEN_IGTG(3),CIGTG_RO),(MAP_IGTG(1,3),MIGTG_RO)      71
      CHARACTER CIGTG_RO*40/' '/                                          71
      CHARACTER NIGTG_RO*12/'R.RO'/                                       71
      INTEGER MIGTG_RO(2)/4,1/                                            71
      REAL RE_IGTG_RO                                                     71
C -                                                                       71
      REAL IGTG_ZSTART                                                    71
      EQUIVALENCE (BANK_IGTG(12),IGTG_ZSTART,RE_IGTG_ZSTART),             71
     *(NAMES_IGTG(4),NIGTG_ZSTART)                                        71
      EQUIVALENCE (COMMEN_IGTG(4),CIGTG_ZSTART),(MAP_IGTG(1,4),           71
     *MIGTG_ZSTART)                                                       71
      CHARACTER CIGTG_ZSTART*40/' '/                                      71
      CHARACTER NIGTG_ZSTART*12/'R.ZSTART'/                               71
      INTEGER MIGTG_ZSTART(2)/1,1/                                        71
      REAL RE_IGTG_ZSTART                                                 71
C -                                                                       71
      REAL IGTG_Z(4)                                                      71
      EQUIVALENCE (BANK_IGTG(13),IGTG_Z,RE_IGTG_Z),(NAMES_IGTG(5),        71
     *NIGTG_Z)                                                            71
      EQUIVALENCE (COMMEN_IGTG(5),CIGTG_Z),(MAP_IGTG(1,5),MIGTG_Z)        71
      CHARACTER CIGTG_Z*40/' '/                                           71
      CHARACTER NIGTG_Z*12/'R.Z'/                                         71
      INTEGER MIGTG_Z(2)/4,1/                                             71
      REAL RE_IGTG_Z                                                      71
C -                                                                       71
      REAL IGTG_FTHK(4)                                                   71
      EQUIVALENCE (BANK_IGTG(17),IGTG_FTHK,RE_IGTG_FTHK),(NAMES_IGTG(6),  71
     *NIGTG_FTHK)                                                         71
      EQUIVALENCE (COMMEN_IGTG(6),CIGTG_FTHK),(MAP_IGTG(1,6),             71
     *MIGTG_FTHK)                                                         71
      CHARACTER CIGTG_FTHK*40/' '/                                        71
      CHARACTER NIGTG_FTHK*12/'R.FTHK'/                                   71
      INTEGER MIGTG_FTHK(2)/4,1/                                          71
      REAL RE_IGTG_FTHK                                                   71
C -                                                                       71
      REAL IGTG_STHK(4)                                                   71
      EQUIVALENCE (BANK_IGTG(21),IGTG_STHK,RE_IGTG_STHK),(NAMES_IGTG(7),  71
     *NIGTG_STHK)                                                         71
      EQUIVALENCE (COMMEN_IGTG(7),CIGTG_STHK),(MAP_IGTG(1,7),             71
     *MIGTG_STHK)                                                         71
      CHARACTER CIGTG_STHK*40/' '/                                        71
      CHARACTER NIGTG_STHK*12/'R.STHK'/                                   71
      INTEGER MIGTG_STHK(2)/4,1/                                          71
      REAL RE_IGTG_STHK                                                   71
C -                                                                       71
      REAL IGTG_SR                                                        71
      EQUIVALENCE (BANK_IGTG(25),IGTG_SR,RE_IGTG_SR),(NAMES_IGTG(8),      71
     *NIGTG_SR)                                                           71
      EQUIVALENCE (COMMEN_IGTG(8),CIGTG_SR),(MAP_IGTG(1,8),MIGTG_SR)      71
      CHARACTER CIGTG_SR*40/' '/                                          71
      CHARACTER NIGTG_SR*12/'R.SR'/                                       71
      INTEGER MIGTG_SR(2)/1,1/                                            71
      REAL RE_IGTG_SR                                                     71
C -                                                                       71
      REAL IGTG_RTHK                                                      71
      EQUIVALENCE (BANK_IGTG(26),IGTG_RTHK,RE_IGTG_RTHK),(NAMES_IGTG(9),  71
     *NIGTG_RTHK)                                                         71
      EQUIVALENCE (COMMEN_IGTG(9),CIGTG_RTHK),(MAP_IGTG(1,9),             71
     *MIGTG_RTHK)                                                         71
      CHARACTER CIGTG_RTHK*40/' '/                                        71
      CHARACTER NIGTG_RTHK*12/'R.RTHK'/                                   71
      INTEGER MIGTG_RTHK(2)/1,1/                                          71
      REAL RE_IGTG_RTHK                                                   71
C -                                                                       71
      INTEGER LEN_IGTG,LLEN_IGTG,LINK_IGTG                                71
      PARAMETER (LEN_IGTG=26)                                             71
      PARAMETER (LLEN_IGTG=9)                                             71
      INTEGER FLAG_IGTG/0/,MAP_IGTG(2,LLEN_IGTG),IDNUM_IGTG(10)           71
      CHARACTER NAMES_IGTG(LLEN_IGTG)*12,COMMEN_IGTG(LLEN_IGTG)*40        71
      DATA IDNUM_IGTG/10*0/                                               71
      REAL BANK_IGTG(LEN_IGTG)                                            71
      CHARACTER*60 BPATH_IGTG/' '/                                        71
      SAVE FLAG_IGTG,MAP_IGTG,IDNUM_IGTG,NAMES_IGTG,COMMEN_IGTG,          71
     *BPATH_IGTG                                                          71
      DATA LINK_IGTG/0/,BANK_IGTG/LEN_IGTG*0/                             71
C *                                                                       71
* -----------------------------------------------------------------------------
C *                                                                       75
      CALL AGSBEGM(MODULE,IPRIN)                                          75
      IF (IPRIN.LT.0) RETURN                                              75
C Filling bank IGTV - IGT GEOMETRY VERSION                                75
      BANK_TITLE='IGT GEOMETRY VERSION'                                   75
      BPATH_IGTV='IGTV*'                                                  75
      IDNUM_IGTV(1)=0                                                     76
C GEOMETRY VERSION - DUMMY                                                76
      IGTV_VERSION=1                                                      76
      CIGTV_VERSION='GEOMETRY VERSION - DUMMY'                            76
C CONFIG                                                                  77
      IGTV_CONFIG=1                                                       77
      CIGTV_CONFIG='CONFIG'                                               77
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'IGTV',    78
     *BPATH_IGTV,IDNUM_IGTV,1, MAP_IGTV,NAMES_IGTV,COMMEN_IGTV,           78
     *BANK_IGTV,LEN_IGTV,LLEN_IGTV,LINK_IGTV,FLAG_IGTV)                   78
C *                                                                       78
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'IGTV',    78
     *BPATH_IGTV,IDNUM_IGTV,1, MAP_IGTV,NAMES_IGTV,COMMEN_IGTV,           78
     *BANK_IGTV,LEN_IGTV,LLEN_IGTV,LINK_IGTV,FLAG_IGTV)                   78
C *                                                                       80
      CALL AGSBEGM(MODULE,IPRIN)                                          80
      IF (IPRIN.LT.0) RETURN                                              80
C Filling bank IGTG - INNER GEM TRACKER GEOMETRY DATA                     80
      BANK_TITLE='INNER GEM TRACKER GEOMETRY DATA'                        80
      BPATH_IGTG='IGTG*'                                                  80
      IDNUM_IGTG(1)=0                                                     81
C VERSION                                                                 81
      IGTG_CONFIG=1                                                       81
      CIGTG_CONFIG='VERSION'                                              81
C INNER RADII FOR EACH GEM                                                82
      IGTG_RI(1)=15.0                                                     82
      IGTG_RI(2)=20.0                                                     82
      IGTG_RI(3)=25.0                                                     82
      IGTG_RI(4)=30.0                                                     82
      CIGTG_RI='INNER RADII FOR EACH GEM'                                 82
C OUTER RADII FOR EACH GEM                                                83
      IGTG_RO(1)=45.0                                                     83
      IGTG_RO(2)=45.0                                                     83
      IGTG_RO(3)=45.0                                                     83
      IGTG_RO(4)=45.0                                                     83
      CIGTG_RO='OUTER RADII FOR EACH GEM'                                 83
C STARTING POSITION ALONG Z AXIS                                          84
      IGTG_ZSTART=55.0                                                    84
      CIGTG_ZSTART='STARTING POSITION ALONG Z AXIS'                       84
C Z POSITIONS FOR GEM FRONT FACE                                          85
      IGTG_Z(1)=0.0                                                       85
      IGTG_Z(2)=30.0                                                      85
      IGTG_Z(3)=60.0                                                      85
      IGTG_Z(4)=90.0                                                      85
      CIGTG_Z='Z POSITIONS FOR GEM FRONT FACE'                            85
C FOIL THICKNESSES INSIDE GEM                                             86
      IGTG_FTHK(1)=0.05                                                   86
      IGTG_FTHK(2)=0.05                                                   86
      IGTG_FTHK(3)=0.05                                                   86
      IGTG_FTHK(4)=0.05                                                   86
      CIGTG_FTHK='FOIL THICKNESSES INSIDE GEM'                            86
C SUPPORT/SPACING THICKNESSES                                             87
      IGTG_STHK(1)=0.4                                                    87
      IGTG_STHK(2)=0.3                                                    87
      IGTG_STHK(3)=0.3                                                    87
      IGTG_STHK(4)=0.4                                                    87
      CIGTG_STHK='SUPPORT/SPACING THICKNESSES'                            87
C RADIAL SIZE FOR SUPPORT                                                 88
      IGTG_SR=1.0                                                         88
      CIGTG_SR='RADIAL SIZE FOR SUPPORT'                                  88
C READOUT PLANE THICKNESS                                                 89
      IGTG_RTHK=0.3                                                       89
      CIGTG_RTHK='READOUT PLANE THICKNESS'                                89
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'IGTG',    90
     *BPATH_IGTG,IDNUM_IGTG,1, MAP_IGTG,NAMES_IGTG,COMMEN_IGTG,           90
     *BANK_IGTG,LEN_IGTG,LLEN_IGTG,LINK_IGTG,FLAG_IGTG)                   90
C *                                                                       90
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'IGTG',    90
     *BPATH_IGTG,IDNUM_IGTG,1, MAP_IGTG,NAMES_IGTG,COMMEN_IGTG,           90
     *BANK_IGTG,LEN_IGTG,LLEN_IGTG,LINK_IGTG,FLAG_IGTG)                   90
C *                                                                       92
      CALL AGSBEGM(MODULE,IPRIN)                                          92
      IF (IPRIN.LT.0) RETURN                                              92
C Filling bank IGTG - INNER GEM TRACKER GEOMETRY DATA                     92
      BANK_TITLE='INNER GEM TRACKER GEOMETRY DATA'                        92
      BPATH_IGTG='IGTG*'                                                  92
      IDNUM_IGTG(1)=0                                                     93
C VERSION                                                                 93
      IGTG_CONFIG=2                                                       93
      CIGTG_CONFIG='VERSION'                                              93
C INNER RADII FOR EACH GEM.                                               94
      IGTG_RI(1)=15.0                                                     94
      IGTG_RI(2)=20.0                                                     94
      IGTG_RI(3)=25.0                                                     94
      IGTG_RI(4)=30.0                                                     94
      CIGTG_RI='INNER RADII FOR EACH GEM.'                                94
C OUTER RADII FOR EACH GEM                                                95
      IGTG_RO(1)=43.0                                                     95
      IGTG_RO(2)=43.0                                                     95
      IGTG_RO(3)=43.0                                                     95
      IGTG_RO(4)=43.0                                                     95
      CIGTG_RO='OUTER RADII FOR EACH GEM'                                 95
C STARTING POSITION ALONG Z AXIS. RCC 55.0 ORIGINALLY                     96
      IGTG_ZSTART=58.0                                                    96
      CIGTG_ZSTART='STARTING POSITION ALONG Z AXIS. RCC 55.0              96
     *ORIGINALLY'                                                         96
C Z POSITIONS FOR GEM FRONT FACE                                          97
      IGTG_Z(1)=0.0                                                       97
      IGTG_Z(2)=30.0                                                      97
      IGTG_Z(3)=60.0                                                      97
      IGTG_Z(4)=90.0                                                      97
      CIGTG_Z='Z POSITIONS FOR GEM FRONT FACE'                            97
C FOIL THICKNESSES INSIDE GEM                                             98
      IGTG_FTHK(1)=0.05                                                   98
      IGTG_FTHK(2)=0.05                                                   98
      IGTG_FTHK(3)=0.05                                                   98
      IGTG_FTHK(4)=0.05                                                   98
      CIGTG_FTHK='FOIL THICKNESSES INSIDE GEM'                            98
C SUPPORT/SPACING THICKNESSES                                             99
      IGTG_STHK(1)=0.4                                                    99
      IGTG_STHK(2)=0.3                                                    99
      IGTG_STHK(3)=0.3                                                    99
      IGTG_STHK(4)=0.4                                                    99
      CIGTG_STHK='SUPPORT/SPACING THICKNESSES'                            99
C RADIAL SIZE FOR SUPPORT                                                 100
      IGTG_SR=1.0                                                         100
      CIGTG_SR='RADIAL SIZE FOR SUPPORT'                                  100
C READOUT PLANE THICKNESS                                                 101
      IGTG_RTHK=0.3                                                       101
      CIGTG_RTHK='READOUT PLANE THICKNESS'                                101
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'IGTG',    102
     *BPATH_IGTG,IDNUM_IGTG,1, MAP_IGTG,NAMES_IGTG,COMMEN_IGTG,           102
     *BANK_IGTG,LEN_IGTG,LLEN_IGTG,LINK_IGTG,FLAG_IGTG)                   102
C *                                                                       102
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'IGTG',    102
     *BPATH_IGTG,IDNUM_IGTG,1, MAP_IGTG,NAMES_IGTG,COMMEN_IGTG,           102
     *BANK_IGTG,LEN_IGTG,LLEN_IGTG,LINK_IGTG,FLAG_IGTG)                   102
*******************************************************************************
* Calculate some parameters from the input variables.
C use default IGTV bank                                                   108
      BPATH_IGTV='IGTV*'                                                  108
      IDNUM_IGTV(1)=0                                                     108
      CALL AGDGETP('IGTDGEO','*',-999,'IGTV',' ',BANK_IGTV, BPATH_IGTV,   108
     *IDNUM_IGTV,1,MAP_IGTV,NAMES_IGTV,BANK_IGTV,LEN_IGTV,LLEN_IGTV,      108
     *LINK_IGTV,FLAG_IGTV)                                                108
C use IGTG bank                                                           108
      BPATH_IGTG='IGTG*'                                                  108
      IDNUM_IGTG(1)=0                                                     108
      IGTG_CONFIG=IGTV_CONFIG                                             108
      CALL AGDGETP('IGTDGEO','*',-999,'IGTG',NIGTG_CONFIG,                108
     *RE_IGTG_CONFIG,BPATH_IGTG,IDNUM_IGTG,1,MAP_IGTG,NAMES_IGTG,         108
     *BANK_IGTG,LEN_IGTG,LLEN_IGTG,LINK_IGTG,FLAG_IGTG)                   108
      WRITE(*,*) 'IGTD Version:',IGTV_CONFIG                              110
* use aluminized mylar mixture instead of kapton
C Geant COMPONENT : C5 ===> A=12 Z=6 W=5 <===                             113
      CALL AGSRESET                                                       113
      AG_A = 12                                                           113
      AG_Z = 6                                                            113
      AG_W = 5                                                            113
      AG_TITLE = 'C5'                                                     113
      AG_EXNAME = 'COMPONENT'                                             113
      AG_PARLIST = 'A_Z_W'                                                113
      CALL AXCOMPONENT                                                    113
C *                                                                       113
C Geant COMPONENT : H4 ===> A=1 Z=1 W=4 <===                              113
      CALL AGSRESET                                                       114
      AG_A = 1                                                            114
      AG_Z = 1                                                            114
      AG_W = 4                                                            114
      AG_TITLE = 'H4'                                                     114
      AG_EXNAME = 'COMPONENT'                                             114
      AG_PARLIST = 'A_Z_W'                                                114
      CALL AXCOMPONENT                                                    114
C *                                                                       114
C Geant COMPONENT : O2 ===> A=16 Z=8 W=2 <===                             114
      CALL AGSRESET                                                       115
      AG_A = 16                                                           115
      AG_Z = 8                                                            115
      AG_W = 2                                                            115
      AG_TITLE = 'O2'                                                     115
      AG_EXNAME = 'COMPONENT'                                             115
      AG_PARLIST = 'A_Z_W'                                                115
      CALL AXCOMPONENT                                                    115
C *                                                                       115
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.2302 <===                       115
      CALL AGSRESET                                                       116
      AG_A = 27                                                           116
      AG_Z = 13                                                           116
      AG_W = 0.2302                                                       116
      AG_TITLE = 'AL'                                                     116
      AG_EXNAME = 'COMPONENT'                                             116
      AG_PARLIST = 'A_Z_W'                                                116
      CALL AXCOMPONENT                                                    116
C *                                                                       116
C Geant MIXTURE : ALKAP ===> DENS=1.432 <===                              116
      CALL AGSRESET                                                       117
      AG_DENS = 1.432                                                     117
      AG_TITLE = 'ALKAP'                                                  117
      AG_EXNAME = 'MIXTURE'                                               117
      AG_PARLIST = 'DENS'                                                 117
      CALL AXMIXTURE                                                      117
C *                                                                       117
*     G10 is about 60% SiO2 and 40% epoxy
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             119
      CALL AGSRESET                                                       119
      AG_A = 28.08                                                        119
      AG_Z = 14                                                           119
      AG_W = 0.6*1*28./60.                                                119
      AG_TITLE = 'SI'                                                     119
      AG_EXNAME = 'COMPONENT'                                             119
      AG_PARLIST = 'A_Z_W'                                                119
      CALL AXCOMPONENT                                                    119
C *                                                                       119
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. <===                  119
      CALL AGSRESET                                                       120
      AG_A = 16                                                           120
      AG_Z = 8                                                            120
      AG_W = 0.6*2*16./60.                                                120
      AG_TITLE = 'O'                                                      120
      AG_EXNAME = 'COMPONENT'                                             120
      AG_PARLIST = 'A_Z_W'                                                120
      CALL AXCOMPONENT                                                    120
C *                                                                       120
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 120
      CALL AGSRESET                                                       121
      AG_A = 12                                                           121
      AG_Z = 6                                                            121
      AG_W = 0.4*8*12./174.                                               121
      AG_TITLE = 'C'                                                      121
      AG_EXNAME = 'COMPONENT'                                             121
      AG_PARLIST = 'A_Z_W'                                                121
      CALL AXCOMPONENT                                                    121
C *                                                                       121
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  121
      CALL AGSRESET                                                       122
      AG_A = 1                                                            122
      AG_Z = 1                                                            122
      AG_W = 0.4*14*1./174.                                               122
      AG_TITLE = 'H'                                                      122
      AG_EXNAME = 'COMPONENT'                                             122
      AG_PARLIST = 'A_Z_W'                                                122
      CALL AXCOMPONENT                                                    122
C *                                                                       122
C Geant COMPONENT : O ===> A=16 Z=8 W=0.4*4*16./174. <===                 122
      CALL AGSRESET                                                       123
      AG_A = 16                                                           123
      AG_Z = 8                                                            123
      AG_W = 0.4*4*16./174.                                               123
      AG_TITLE = 'O'                                                      123
      AG_EXNAME = 'COMPONENT'                                             123
      AG_PARLIST = 'A_Z_W'                                                123
      CALL AXCOMPONENT                                                    123
C *                                                                       123
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  123
      CALL AGSRESET                                                       124
      AG_DENS = 1.7                                                       124
      AG_TITLE = 'G10'                                                    124
      AG_EXNAME = 'MIXTURE'                                               124
      AG_PARLIST = 'DENS'                                                 124
      CALL AXMIXTURE                                                      124
C *                                                                       124
* Loop over the layers in a GEM detector and accumulate the total thickness.
      THICK = 0.0                                                         129
C *                                                                       131
      DO 5011 LAYER=1,4                                                   131
         THICK = THICK + IGTG_FTHK(LAYER) + IGTG_STHK(LAYER)              132
5011  CONTINUE                                                            133
5012  CONTINUE                                                            133
      THICK = THICK + IGTG_RTHK                                           135
* Loop over the GEM disks and determine the min and max radius and max Z.
      RMN = 10000.0                                                       139
      RMX = - RMN                                                         140
      LENGTH = 0.0                                                        141
C *                                                                       143
      DO 5021 DISK=1,4                                                    143
         IF (RMN .GT. IGTG_RI(DISK)) THEN                                 145
         RMN = IGTG_RI(DISK)                                              146
         ENDIF                                                            147
         IF (RMX .LT. IGTG_RO(DISK)) THEN                                 149
         RMX = IGTG_RO(DISK)                                              150
         ENDIF                                                            151
         IF (LENGTH .LT. IGTG_Z(DISK)) THEN                               153
         LENGTH = IGTG_Z(DISK)                                            154
         ENDIF                                                            155
5021  CONTINUE                                                            156
5022  CONTINUE                                                            156
      LENGTH = LENGTH + THICK                                             158
* Calculate centre of assembly.
      CENTER = IGTG_ZSTART + LENGTH / 2.0                                 162
      CALL AGSBEGM(MODULE,IPRIN)                                          164
C Check IPRIN<0                                                           164
      IF (IPRIN.LT.0) RETURN                                              164
C Create IGMO :                                                           164
C Call To Local Procedure IGMO                                            164
      ASSIGN 5040 TO BLOCK_IGMO                                           164
      GO TO  5030                                                         164
5040  CONTINUE                                                            164
C *                                                                       164
C Geant POSITION : IGMO ===> MOTHER='CAVE' Z=+CENTER KONLY='MANY' <===    165
      CALL AGSRESET                                                       165
      AG_MOTHER = 'CAVE'                                                  165
      AG_Z = +CENTER                                                      165
      AG_KONLY = 'MANY'                                                   165
      AG_TITLE = 'IGMO'                                                   165
      AG_EXNAME = 'POSITION'                                              165
      AG_PARLIST = 'MOTHER_Z_KONLY'                                       165
      CALL AXPOSITION                                                     165
C *                                                                       165
* -----------------------------------------------------------------------------
      CALL AGSENDM                                                        168
      RETURN                                                              168
C Begin Local Procedure IGMO                                              168
C IGMO IS THE MOTHER VOLUME FOR THE IGTD                                  168
5030  CONTINUE                                                            168
         AG_TITLE='IGMO'                                                  168
         AG_MODULE=MODULE                                                 168
         AG_MARK='V00'                                                    168
         CALL AGSPUSH                                                     168
         AG_MODULE=MODULE                                                 168
C    *                                                                    168
C    Geant MATERIAL : AIR ===> inherited arguments used <===              169
         CALL AGSRESET                                                    169
         AG_TITLE = 'AIR'                                                 169
         AG_EXNAME = 'MATERIAL'                                           169
         AG_PARLIST = 'NONE'                                              169
         CALL AXMATERIAL                                                  169
C    *                                                                    169
C    Geant ATTRIBUTE : IGMO ===> SEEN=1 COLO=6 <===                       169
         CALL AGSRESET                                                    170
         AG_SEEN = 1                                                      170
         AG_COLO = 6                                                      170
         AG_TITLE = 'IGMO'                                                170
         AG_EXNAME = 'ATTRIBUTE'                                          170
         AG_PARLIST = 'SEEN_COLO'                                         170
         CALL AXATTRIBUTE                                                 170
C    *                                                                    170
C    Geant SHAPE : TUBE ===> RMIN=RMN RMAX=RMX DZ=LENGTH/2.0 <===         172
         CALL AGSRESET                                                    172
         AG_RMIN = RMN                                                    172
         AG_RMAX = RMX                                                    172
         AG_DZ = LENGTH/2.0                                               172
         AG_TITLE = 'TUBE'                                                172
         AG_EXNAME = 'SHAPE'                                              172
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      172
         CALL AXSHAPE                                                     172
C    *                                                                    172
* Loop over the four GEM disks.
C    *                                                                    176
         DISK=1                                                           176
            GO TO 5053                                                    176
5051        DISK=DISK+1                                                   176
5053        IF(DISK-(4).GT.0) GO TO 5052                                  176
            RIN = IGTG_RI(DISK)                                           177
            ROUT = IGTG_RO(DISK)                                          178
C       Create IGDO :                                                     179
C       Call To Local Procedure IGDO                                      179
            ASSIGN 5070 TO BLOCK_IGDO                                     179
            GO TO  5060                                                   179
5070        CONTINUE                                                      179
C       *                                                                 179
C       Geant POSITION : IGDO ===> Z=-LENGTH/2.0 + THICK/2.0 + IGTG_Z(DI  179
C       SK) <===                                                          179
            CALL AGSRESET                                                 179
            AG_Z = -LENGTH/2.0 + THICK/2.0 + IGTG_Z(DISK)                 179
            AG_TITLE = 'IGDO'                                             179
            AG_EXNAME = 'POSITION'                                        179
            AG_PARLIST = 'Z'                                              179
            CALL AXPOSITION                                               179
C       *                                                                 179
         GO TO 5051                                                       180
5052     CONTINUE                                                         180
5031  CALL AGSPOPB                                                        182
      GO TO BLOCK_IGMO, (5030 ,5040)                                      182
C End of Local Procedure                                                  182
* -----------------------------------------------------------------------------
C Begin Local Procedure IGDO                                              185
C IGDO IS THE MOTHER VOLUME OF THE INDIVIDUAL GEM DISKS                   185
5060  CONTINUE                                                            185
         AG_TITLE='IGDO'                                                  185
         AG_MODULE=MODULE                                                 185
         AG_MARK='V00'                                                    185
         CALL AGSPUSH                                                     185
         AG_MODULE=MODULE                                                 185
C    *                                                                    185
C    Geant COMPONENT : AR ===> A=39.95 Z=18. W=0.9 <===                   186
         CALL AGSRESET                                                    186
         AG_A = 39.95                                                     186
         AG_Z = 18.                                                       186
         AG_W = 0.9                                                       186
         AG_TITLE = 'AR'                                                  186
         AG_EXNAME = 'COMPONENT'                                          186
         AG_PARLIST = 'A_Z_W'                                             186
         CALL AXCOMPONENT                                                 186
C    *                                                                    186
C    Geant COMPONENT : C ===> A=12.01 Z=6. W=0.1*1*12.01/44.01 <===       186
         CALL AGSRESET                                                    187
         AG_A = 12.01                                                     187
         AG_Z = 6.                                                        187
         AG_W = 0.1*1*12.01/44.01                                         187
         AG_TITLE = 'C'                                                   187
         AG_EXNAME = 'COMPONENT'                                          187
         AG_PARLIST = 'A_Z_W'                                             187
         CALL AXCOMPONENT                                                 187
C    *                                                                    187
C    Geant COMPONENT : O ===> A=16. Z=8. W=0.1*2*16./44.01 <===           187
         CALL AGSRESET                                                    188
         AG_A = 16.                                                       188
         AG_Z = 8.                                                        188
         AG_W = 0.1*2*16./44.01                                           188
         AG_TITLE = 'O'                                                   188
         AG_EXNAME = 'COMPONENT'                                          188
         AG_PARLIST = 'A_Z_W'                                             188
         CALL AXCOMPONENT                                                 188
C    *                                                                    188
C    Geant MIXTURE : AR_MIX ===> DENS=0.0018015 <===                      188
         CALL AGSRESET                                                    189
         AG_DENS = 0.0018015                                              189
         AG_TITLE = 'AR_MIX'                                              189
         AG_EXNAME = 'MIXTURE'                                            189
         AG_PARLIST = 'DENS'                                              189
         CALL AXMIXTURE                                                   189
C    *                                                                    189
C    Geant ATTRIBUTE : IGDO ===> SEEN=0 COLO=6 <===                       191
         CALL AGSRESET                                                    191
         AG_SEEN = 0                                                      191
         AG_COLO = 6                                                      191
         AG_TITLE = 'IGDO'                                                191
         AG_EXNAME = 'ATTRIBUTE'                                          191
         AG_PARLIST = 'SEEN_COLO'                                         191
         CALL AXATTRIBUTE                                                 191
C    *                                                                    191
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=ROUT DZ=THICK/2.0 <===         193
         CALL AGSRESET                                                    193
         AG_RMIN = RIN                                                    193
         AG_RMAX = ROUT                                                   193
         AG_DZ = THICK/2.0                                                193
         AG_TITLE = 'TUBE'                                                193
         AG_EXNAME = 'SHAPE'                                              193
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      193
         CALL AXSHAPE                                                     193
C    *                                                                    193
* Loop over the layers inside the GEM.
C    START AT THE FRONT FACE.                                             197
         ZSUM = -THICK/2.0                                                197
C    *                                                                    199
         LAYER=1                                                          199
            GO TO 5083                                                    199
5081        LAYER=LAYER+1                                                 199
5083        IF(LAYER-(4).GT.0) GO TO 5082                                 199
* Create GEM foil.
C       HALF THICKNESS OF FOIL                                            203
            THK = IGTG_FTHK(LAYER)/2.0                                    203
C       POSITION OF FOIL CENTRE                                           204
            ZSUM = ZSUM + THK                                             204
C       Create IGFO :                                                     206
C       Call To Local Procedure IGFO                                      206
            ASSIGN 5100 TO BLOCK_IGFO                                     206
            GO TO  5090                                                   206
5100        CONTINUE                                                      206
C       *                                                                 206
C       Geant POSITION : IGFO ===> Z=ZSUM <===                            206
            CALL AGSRESET                                                 206
            AG_Z = ZSUM                                                   206
            AG_TITLE = 'IGFO'                                             206
            AG_EXNAME = 'POSITION'                                        206
            AG_PARLIST = 'Z'                                              206
            CALL AXPOSITION                                               206
C       *                                                                 206
C       POSITION OF FOIL REAR FACE                                        208
            ZSUM = ZSUM + THK                                             208
* Create inner and outer spacer/support.
C       HALF THICKNESS OF SPACER                                          212
            THK = IGTG_STHK(LAYER)/2.0                                    212
C       POSITION OF SPACER CENTRE                                         213
            ZSUM = ZSUM + THK                                             213
C       Create IGIS :                                                     215
C       Call To Local Procedure IGIS                                      215
            ASSIGN 5120 TO BLOCK_IGIS                                     215
            GO TO  5110                                                   215
5120        CONTINUE                                                      215
C       *                                                                 215
C       Geant POSITION : IGIS ===> Z=ZSUM <===                            215
            CALL AGSRESET                                                 215
            AG_Z = ZSUM                                                   215
            AG_TITLE = 'IGIS'                                             215
            AG_EXNAME = 'POSITION'                                        215
            AG_PARLIST = 'Z'                                              215
            CALL AXPOSITION                                               215
C       *                                                                 215
C       Create IGOS :                                                     216
C       Call To Local Procedure IGOS                                      216
            ASSIGN 5140 TO BLOCK_IGOS                                     216
            GO TO  5130                                                   216
5140        CONTINUE                                                      216
C       *                                                                 216
C       Geant POSITION : IGOS ===> Z=ZSUM <===                            216
            CALL AGSRESET                                                 216
            AG_Z = ZSUM                                                   216
            AG_TITLE = 'IGOS'                                             216
            AG_EXNAME = 'POSITION'                                        216
            AG_PARLIST = 'Z'                                              216
            CALL AXPOSITION                                               216
C       *                                                                 216
* First gap is active volume -> create it.
            IF (LAYER .EQ. 1) THEN                                        220
C       Create IGAL :                                                     221
C       Call To Local Procedure IGAL                                      221
            ASSIGN 5160 TO BLOCK_IGAL                                     221
            GO TO  5150                                                   221
5160        CONTINUE                                                      221
C       *                                                                 221
C       Geant POSITION : IGAL ===> Z=ZSUM <===                            221
            CALL AGSRESET                                                 221
            AG_Z = ZSUM                                                   221
            AG_TITLE = 'IGAL'                                             221
            AG_EXNAME = 'POSITION'                                        221
            AG_PARLIST = 'Z'                                              221
            CALL AXPOSITION                                               221
C       *                                                                 221
            ENDIF                                                         221
C       POSITION OF SPACER REAR FACE                                      224
            ZSUM = ZSUM + THK                                             224
         GO TO 5081                                                       226
5082     CONTINUE                                                         226
* Create readout layer as last layer.
C    HALF THICKNESS OF READOUT LAYER                                      230
         THK = IGTG_RTHK/2.0                                              230
C    POSITION OF READOUT LAYER CENTRE                                     231
         ZSUM = ZSUM + THK                                                231
C    Create IGRL :                                                        233
C    Call To Local Procedure IGRL                                         233
         ASSIGN 5180 TO BLOCK_IGRL                                        233
         GO TO  5170                                                      233
5180     CONTINUE                                                         233
C    *                                                                    233
C    Geant POSITION : IGRL ===> Z=ZSUM <===                               233
         CALL AGSRESET                                                    233
         AG_Z = ZSUM                                                      233
         AG_TITLE = 'IGRL'                                                233
         AG_EXNAME = 'POSITION'                                           233
         AG_PARLIST = 'Z'                                                 233
         CALL AXPOSITION                                                  233
C    *                                                                    233
5061  CALL AGSPOPB                                                        235
      GO TO BLOCK_IGDO, (5060 ,5070)                                      235
C End of Local Procedure                                                  235
* -----------------------------------------------------------------------------
C Begin Local Procedure IGFO                                              238
C IGFO DESCRIBES THE GEM FOILS                                            238
5090  CONTINUE                                                            238
         AG_TITLE='IGFO'                                                  238
         AG_MODULE=MODULE                                                 238
         AG_MARK='V00'                                                    238
         CALL AGSPUSH                                                     238
         AG_MODULE=MODULE                                                 238
C    *                                                                    238
C    Geant MATERIAL : ALKAP ===> inherited arguments used <===            239
         CALL AGSRESET                                                    239
         AG_TITLE = 'ALKAP'                                               239
         AG_EXNAME = 'MATERIAL'                                           239
         AG_PARLIST = 'NONE'                                              239
         CALL AXMATERIAL                                                  239
C    *                                                                    239
C    Geant ATTRIBUTE : IGFO ===> SEEN=1 COLO=4 <===                       239
         CALL AGSRESET                                                    240
         AG_SEEN = 1                                                      240
         AG_COLO = 4                                                      240
         AG_TITLE = 'IGFO'                                                240
         AG_EXNAME = 'ATTRIBUTE'                                          240
         AG_PARLIST = 'SEEN_COLO'                                         240
         CALL AXATTRIBUTE                                                 240
C    *                                                                    240
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=ROUT DZ=THK <===               242
         CALL AGSRESET                                                    242
         AG_RMIN = RIN                                                    242
         AG_RMAX = ROUT                                                   242
         AG_DZ = THK                                                      242
         AG_TITLE = 'TUBE'                                                242
         AG_EXNAME = 'SHAPE'                                              242
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      242
         CALL AXSHAPE                                                     242
C    *                                                                    242
5091  CALL AGSPOPB                                                        244
      GO TO BLOCK_IGFO, (5090 ,5100)                                      244
C End of Local Procedure                                                  244
* -----------------------------------------------------------------------------
C Begin Local Procedure IGIS                                              247
C IGIS DESCRIBES THE INNER SUPPORT OR SPACER                              247
5110  CONTINUE                                                            247
         AG_TITLE='IGIS'                                                  247
         AG_MODULE=MODULE                                                 247
         AG_MARK='V00'                                                    247
         CALL AGSPUSH                                                     247
         AG_MODULE=MODULE                                                 247
C    *                                                                    247
C    Geant MATERIAL : G10 ===> inherited arguments used <===              248
         CALL AGSRESET                                                    248
         AG_TITLE = 'G10'                                                 248
         AG_EXNAME = 'MATERIAL'                                           248
         AG_PARLIST = 'NONE'                                              248
         CALL AXMATERIAL                                                  248
C    *                                                                    248
C    Geant ATTRIBUTE : IGIS ===> SEEN=1 COLO=3 <===                       248
         CALL AGSRESET                                                    249
         AG_SEEN = 1                                                      249
         AG_COLO = 3                                                      249
         AG_TITLE = 'IGIS'                                                249
         AG_EXNAME = 'ATTRIBUTE'                                          249
         AG_PARLIST = 'SEEN_COLO'                                         249
         CALL AXATTRIBUTE                                                 249
C    *                                                                    249
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=RIN+IGTG_SR DZ=THK <===        251
         CALL AGSRESET                                                    251
         AG_RMIN = RIN                                                    251
         AG_RMAX = RIN+IGTG_SR                                            251
         AG_DZ = THK                                                      251
         AG_TITLE = 'TUBE'                                                251
         AG_EXNAME = 'SHAPE'                                              251
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      251
         CALL AXSHAPE                                                     251
C    *                                                                    251
5111  CALL AGSPOPB                                                        253
      GO TO BLOCK_IGIS, (5110 ,5120)                                      253
C End of Local Procedure                                                  253
* -----------------------------------------------------------------------------
C Begin Local Procedure IGOS                                              256
C IGOS DESCRIBES THE OUTER SUPPORT OR SPACER                              256
5130  CONTINUE                                                            256
         AG_TITLE='IGOS'                                                  256
         AG_MODULE=MODULE                                                 256
         AG_MARK='V00'                                                    256
         CALL AGSPUSH                                                     256
         AG_MODULE=MODULE                                                 256
C    *                                                                    256
C    Geant MATERIAL : G10 ===> inherited arguments used <===              257
         CALL AGSRESET                                                    257
         AG_TITLE = 'G10'                                                 257
         AG_EXNAME = 'MATERIAL'                                           257
         AG_PARLIST = 'NONE'                                              257
         CALL AXMATERIAL                                                  257
C    *                                                                    257
C    Geant ATTRIBUTE : IGOS ===> SEEN=1 COLO=3 <===                       257
         CALL AGSRESET                                                    258
         AG_SEEN = 1                                                      258
         AG_COLO = 3                                                      258
         AG_TITLE = 'IGOS'                                                258
         AG_EXNAME = 'ATTRIBUTE'                                          258
         AG_PARLIST = 'SEEN_COLO'                                         258
         CALL AXATTRIBUTE                                                 258
C    *                                                                    258
C    Geant SHAPE : TUBE ===> RMIN=ROUT-IGTG_SR RMAX=ROUT DZ=THK <===      260
         CALL AGSRESET                                                    260
         AG_RMIN = ROUT-IGTG_SR                                           260
         AG_RMAX = ROUT                                                   260
         AG_DZ = THK                                                      260
         AG_TITLE = 'TUBE'                                                260
         AG_EXNAME = 'SHAPE'                                              260
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      260
         CALL AXSHAPE                                                     260
C    *                                                                    260
5131  CALL AGSPOPB                                                        262
      GO TO BLOCK_IGOS, (5130 ,5140)                                      262
C End of Local Procedure                                                  262
* -----------------------------------------------------------------------------
C Begin Local Procedure IGRL                                              265
C IGRL DESCRIBES THE READOUT LAYER                                        265
5170  CONTINUE                                                            265
         AG_TITLE='IGRL'                                                  265
         AG_MODULE=MODULE                                                 265
         AG_MARK='V00'                                                    265
         CALL AGSPUSH                                                     265
         AG_MODULE=MODULE                                                 265
C    *                                                                    265
C    Geant MATERIAL : G10 ===> inherited arguments used <===              266
         CALL AGSRESET                                                    266
         AG_TITLE = 'G10'                                                 266
         AG_EXNAME = 'MATERIAL'                                           266
         AG_PARLIST = 'NONE'                                              266
         CALL AXMATERIAL                                                  266
C    *                                                                    266
C    Geant ATTRIBUTE : IGRL ===> SEEN=1 COLO=3 <===                       266
         CALL AGSRESET                                                    267
         AG_SEEN = 1                                                      267
         AG_COLO = 3                                                      267
         AG_TITLE = 'IGRL'                                                267
         AG_EXNAME = 'ATTRIBUTE'                                          267
         AG_PARLIST = 'SEEN_COLO'                                         267
         CALL AXATTRIBUTE                                                 267
C    *                                                                    267
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=ROUT DZ=THK <===               269
         CALL AGSRESET                                                    269
         AG_RMIN = RIN                                                    269
         AG_RMAX = ROUT                                                   269
         AG_DZ = THK                                                      269
         AG_TITLE = 'TUBE'                                                269
         AG_EXNAME = 'SHAPE'                                              269
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      269
         CALL AXSHAPE                                                     269
C    *                                                                    269
5171  CALL AGSPOPB                                                        271
      GO TO BLOCK_IGRL, (5170 ,5180)                                      271
C End of Local Procedure                                                  271
* -----------------------------------------------------------------------------
C Begin Local Procedure IGAL                                              274
C IGAL DESCRIBES THE ACTIVE AREA                                          274
5150  CONTINUE                                                            274
         AG_TITLE='IGAL'                                                  274
         AG_MODULE=MODULE                                                 274
         AG_MARK='V00'                                                    274
         CALL AGSPUSH                                                     274
         AG_MODULE=MODULE                                                 274
C    *                                                                    274
C    Geant COMPONENT : AR ===> A=39.95 Z=18. W=0.9 <===                   275
         CALL AGSRESET                                                    275
         AG_A = 39.95                                                     275
         AG_Z = 18.                                                       275
         AG_W = 0.9                                                       275
         AG_TITLE = 'AR'                                                  275
         AG_EXNAME = 'COMPONENT'                                          275
         AG_PARLIST = 'A_Z_W'                                             275
         CALL AXCOMPONENT                                                 275
C    *                                                                    275
C    Geant COMPONENT : C ===> A=12.01 Z=6. W=0.1*1*12.01/44.01 <===       275
         CALL AGSRESET                                                    276
         AG_A = 12.01                                                     276
         AG_Z = 6.                                                        276
         AG_W = 0.1*1*12.01/44.01                                         276
         AG_TITLE = 'C'                                                   276
         AG_EXNAME = 'COMPONENT'                                          276
         AG_PARLIST = 'A_Z_W'                                             276
         CALL AXCOMPONENT                                                 276
C    *                                                                    276
C    Geant COMPONENT : O ===> A=16. Z=8. W=0.1*2*16./44.01 <===           276
         CALL AGSRESET                                                    277
         AG_A = 16.                                                       277
         AG_Z = 8.                                                        277
         AG_W = 0.1*2*16./44.01                                           277
         AG_TITLE = 'O'                                                   277
         AG_EXNAME = 'COMPONENT'                                          277
         AG_PARLIST = 'A_Z_W'                                             277
         CALL AXCOMPONENT                                                 277
C    *                                                                    277
C    Geant MIXTURE : AR_MIX ===> DENS=0.0018015 ISVOL=1 <===              277
         CALL AGSRESET                                                    278
         AG_DENS = 0.0018015                                              278
         AG_ISVOL = 1                                                     278
         AG_TITLE = 'AR_MIX'                                              278
         AG_EXNAME = 'MIXTURE'                                            278
         AG_PARLIST = 'DENS_ISVOL'                                        278
         CALL AXMIXTURE                                                   278
C    *                                                                    278
C    Geant MATERIAL : SENSITIVE ===> ISVOL=1 <===                         280
         CALL AGSRESET                                                    280
         AG_ISVOL = 1                                                     280
         AG_TITLE = 'SENSITIVE'                                           280
         AG_EXNAME = 'MATERIAL'                                           280
         AG_PARLIST = 'ISVOL'                                             280
         CALL AXMATERIAL                                                  280
C    *                                                                    280
C    Geant ATTRIBUTE : IGAL ===> SEEN=1 COLO=6 <===                       282
         CALL AGSRESET                                                    282
         AG_SEEN = 1                                                      282
         AG_COLO = 6                                                      282
         AG_TITLE = 'IGAL'                                                282
         AG_EXNAME = 'ATTRIBUTE'                                          282
         AG_PARLIST = 'SEEN_COLO'                                         282
         CALL AXATTRIBUTE                                                 282
C    *                                                                    282
C    Geant SHAPE : TUBE ===> RMIN=RIN+IGTG_SR RMAX=ROUT-IGTG_SR DZ=IGTG_  284
C    STHK(1)/2.0 <===                                                     284
         CALL AGSRESET                                                    284
         AG_RMIN = RIN+IGTG_SR                                            284
         AG_RMAX = ROUT-IGTG_SR                                           284
         AG_DZ = IGTG_STHK(1)/2.0                                         284
         AG_TITLE = 'TUBE'                                                284
         AG_EXNAME = 'SHAPE'                                              284
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      284
         CALL AXSHAPE                                                     284
C    *                                                                    284
C    Defining Hits: Z:.001:S Y:.001: X:.001: PTOT:16:(0,100), CX:10: CY:  288
C    10: CZ:10: SLENG:16:(0,500), TOF:16:(0,1.E-6) STEP:.01: ELOSS:16:(0  288
C    ,0.001)                                                              288
         AG_TITLE = 'IGAL'                                                288
         AG_EXNAME = 'HITS'                                               288
         AG_TYPE=AG_MAGIC                                                 288
         CALL AGSDETV('IGAL')                                             288
C    *                                                                    288
         AG_PARLIST='Z'                                                   288
         AG_TITLE = 'DETH'                                                288
         AG_NBITS = AG_MAGIC                                              288
         AG_BIN=.001                                                      288
         AG_OPTION='S'                                                    288
         AG_FHMIN = AG_MAGIC                                              288
         AG_FHMAX = AG_MAGIC                                              288
         CALL AGSDETH (0)                                                 288
C    *                                                                    288
         AG_PARLIST='Y'                                                   288
         AG_TITLE = 'DETH'                                                288
         AG_NBITS = AG_MAGIC                                              288
         AG_BIN=.001                                                      288
         AG_FHMIN=AG_MAGIC                                                288
         AG_FHMAX=AG_MAGIC                                                288
         AG_OPTION='*'                                                    288
         CALL AGSDETH (0)                                                 288
C    *                                                                    288
         AG_PARLIST='X'                                                   288
         AG_TITLE = 'DETH'                                                288
         AG_NBITS = AG_MAGIC                                              288
         AG_BIN=.001                                                      288
         AG_FHMIN=AG_MAGIC                                                288
         AG_FHMAX=AG_MAGIC                                                288
         AG_OPTION='*'                                                    288
         CALL AGSDETH (0)                                                 288
C    *                                                                    288
         AG_PARLIST='PTOT'                                                288
         AG_TITLE = 'DETH'                                                288
         AG_NBITS = 16                                                    288
         AG_BIN=AG_MAGIC                                                  288
         AG_OPTION='*'                                                    288
         AG_FHMIN = 0                                                     288
         AG_FHMAX = 100                                                   288
         CALL AGSDETH (0)                                                 288
C    *                                                                    288
         AG_PARLIST='CX'                                                  288
         AG_TITLE = 'DETH'                                                288
         AG_NBITS = 10                                                    288
         AG_BIN=AG_MAGIC                                                  288
         AG_FHMIN=AG_MAGIC                                                288
         AG_FHMAX=AG_MAGIC                                                288
         AG_OPTION='*'                                                    288
         CALL AGSDETH (0)                                                 288
C    *                                                                    288
         AG_PARLIST='CY'                                                  288
         AG_TITLE = 'DETH'                                                288
         AG_NBITS = 10                                                    288
         AG_BIN=AG_MAGIC                                                  288
         AG_FHMIN=AG_MAGIC                                                288
         AG_FHMAX=AG_MAGIC                                                288
         AG_OPTION='*'                                                    288
         CALL AGSDETH (0)                                                 288
C    *                                                                    288
         AG_PARLIST='CZ'                                                  288
         AG_TITLE = 'DETH'                                                288
         AG_NBITS = 10                                                    288
         AG_BIN=AG_MAGIC                                                  288
         AG_FHMIN=AG_MAGIC                                                288
         AG_FHMAX=AG_MAGIC                                                288
         AG_OPTION='*'                                                    288
         CALL AGSDETH (0)                                                 288
C    *                                                                    288
         AG_PARLIST='SLENG'                                               288
         AG_TITLE = 'DETH'                                                288
         AG_NBITS = 16                                                    288
         AG_BIN=AG_MAGIC                                                  288
         AG_OPTION='*'                                                    288
         AG_FHMIN = 0                                                     288
         AG_FHMAX = 500                                                   288
         CALL AGSDETH (0)                                                 288
C    *                                                                    288
         AG_PARLIST='TOF'                                                 288
         AG_TITLE = 'DETH'                                                288
         AG_NBITS = 16                                                    288
         AG_BIN=AG_MAGIC                                                  288
         AG_OPTION='*'                                                    288
         AG_FHMIN = 0                                                     288
         AG_FHMAX = 1.E-6                                                 288
         CALL AGSDETH (0)                                                 288
C    *                                                                    288
         AG_PARLIST='STEP'                                                288
         AG_TITLE = 'DETH'                                                288
         AG_NBITS = AG_MAGIC                                              288
         AG_BIN=.01                                                       288
         AG_FHMIN=AG_MAGIC                                                288
         AG_FHMAX=AG_MAGIC                                                288
         AG_OPTION='*'                                                    288
         CALL AGSDETH (0)                                                 288
C    *                                                                    288
         AG_PARLIST='ELOSS'                                               288
         AG_TITLE = 'DETH'                                                288
         AG_NBITS = 16                                                    288
         AG_BIN=AG_MAGIC                                                  288
         AG_OPTION='*'                                                    288
         AG_FHMIN = 0                                                     288
         AG_FHMAX = 0.001                                                 288
         CALL AGSDETH (0)                                                 288
C    *                                                                    288
         CALL AGSDETU('IGAL')                                             288
C    *                                                                    288
5151  CALL AGSPOPB                                                        290
      GO TO BLOCK_IGAL, (5150 ,5160)                                      290
C End of Local Procedure                                                  290
* -----------------------------------------------------------------------------
      END                                                                 294
