C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
* $Id: fstdgeo.g,v 1.11 2006/10/20 19:24:47 potekhin Exp $
* $Log: fstdgeo.g,v $
* Revision 1.11  2006/10/20 19:24:47  potekhin
* Korrekt a tipo
*
* Revision 1.10  2006/10/20 19:15:18  potekhin
* Added the 'MANY' option to the positioning (necessary due
* to overlaps in the upgrade geometries) and made the mother
* volume visible (to aid in resolution of same)
*
* Revision 1.9  2005/02/11 20:26:48  nieuwhzs
* Moved the silicon sensors to their proper positions, i.e. flush with the
* front faces of the hybrids.
* Adjusted the size of the readout chip 'strip' to be more realistic.
* Added second cooling channel on the backside of the modules and adjusted
* mother volume size to accomodate this second cooling channel. This make the
* modules symmetric.
* Adjusted the size of the FST mother volume so that the 4 disks can be placed
* in their proper position in the 'CAVE' volume. The 'center' of the disks is
* assumed to be exactly halway between the silicon sensors.
* +++Gerrit van Nieuwenhuizen, 01/11/05
*
* Revision 1.6  2005/02/02 22:07:28  potekhin
* Changed the sensor color as per Gerrit's request
*
* Revision 1.5  2005/01/26 01:12:46  potekhin
* Removed active and passive layers, corrected nesting of
* the water duct volume
*
* Revision 1.4  2005/01/25 22:43:36  potekhin
* Added the water manifold (duct). Corrected a small
* error in the size of AlN plate. Added HITS (to be further devloped)
*
* Revision 1.3  2005/01/06 02:01:09  potekhin
* Substantial reorganization, better AlN description
*
* Revision 1.2  2005/01/04 23:47:48  potekhin
* A waypoint check-in:
* implemented a full fledged double wafer in each layer,
* and the AlN thermal plate.
*
* Revision 1.1  2004/12/07 00:41:00  potekhin
* We need a directory subtree for the Forward Tracker.
* It has been planned for a long time.
*
*
******************************************************************************
      SUBROUTINE FSTDGEO                                                  48
C FSTDGEO IS THE GEOMETRY OF THE FORWARD SILICON TRACKER PIXEL DETECTOR   48
      IMPLICIT NONE                                                       48
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       48
      DATA MODULE/'FSTDGEO'/, BANK_TITLE/'*'/                             48
      DATA MCOMMENT/'IS THE GEOMETRY OF THE FORWARD SILICON TRACKER PIXE  48
     *L DETECTOR'/                                                        48
      LOGICAL FIRST                                                       48
      DATA FIRST/.TRUE./                                                  48
      SAVE FIRST                                                          48
      INTEGER JUMPAD                                                      48
      EXTERNAL JUMPAD                                                     48
      INTEGER OK                                                          48
      PARAMETER (OK=0)                                                    48
      INTEGER AG_I,AG_J                                                   48
      SAVE AG_I,AG_J                                                      48
C CREATED 12/06/04                                                        48
      DATA AG_I/0/,AG_J/0/                                                49
      CHARACTER*40 CREATED /'12/06/04'/                                   49
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  49
C AUTHOR MAXIM POTEKHIN                                                   49
      CHARACTER*40 AUTHOR /'MAXIM POTEKHIN'/                              50
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  50
******************************************************************************
C - Advanced Geant interface                                              52
      CHARACTER*20 AG_MODULE,AG_TITLE,AG_EXNAME,AG_PARLIST,AG_CHDIR,      52
     *AG_MATERIAL,AG_MIXTURE,AG_COMPONENT,AG_MEDIUM                       52
      CHARACTER*4 AG_VOLUME,AG_MOTHER,AG_SHAPE,AG_CNICK,AG_KONLY,         52
     *AG_OPTION,AG_ORT,AG_MARK                                            52
      INTEGER AG_BEGCOM,AG_IVOLUME,AG_IMOTHER,AG_IGNUM,AG_ISHAPE,         52
     *AG_IMED,AG_IMAT,AG_IFIELD, AG_NLMAT,AG_IERROR,AG_NWBUF,AG_NPAR,     52
     *AG_ISTATUS,AG_IROT,AG_NBITS,AG_SERIAL, AG_ATTRIBUTE(6),AG_WORK,     52
     *AG_SEEN,AG_LSTY,AG_LWID,AG_COLO,AG_FILL,AG_JDU,AG_ORTI, AG_NDIV,    52
     *AG_IAXIS,AG_NDVMAX,AG_IPRIN,AG_RESET1,AG_RESET2,AG_BEGSCR,          52
     *AG_ENDSCR, AG_IDTYPE,AG_ISET,AG_IDET,AG_ISVOL,AG_LEVEL,AG_IRESER,   52
     *AG_IGEOM,AG_STANDALONE, AG_LSTACK,AG_NWUHIT,AG_NWUVOL,AG_MAGIC,     52
     *AG_LDETU,AG_NPDIV,AG_NPDV,AG_NZ,AG_NCOPY, AG_IDEBU,AG_IGRAP,        52
     *AG_IHIST,AG_IMFLD,AG_ISIMU,AG_ENDSAVE,AG_CODE,AG_TRKTYP, AG_MODE,   52
     *AG_ECODE,AG_PDG,IPRIN, AG_ALPHAN,AG_ALPHAK(10),IGOR                 52
      REAL AG_FIELDM,AG_TMAXFD,AG_STEMAX,AG_DEEMAX,AG_EPSIL,AG_STMIN,     52
     *AG_DENS,AG_RADL,AG_ABSL, AG_THETAX,AG_PHIX,AG_THETAY,AG_PHIY,       52
     *AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,AG_ALFAZ, AG_ALPHAL(10),        52
     *AG_ALPHAX,AG_ALPHAY,AG_ALPHAZ,AG_TYPE, AG_DX, AG_DX1, AG_DX2,       52
     *AG_DY, AG_DY1, AG_DY2, AG_DMAXMS, AG_TWIST, AG_THET, AG_THE1,       52
     *AG_THE2, AG_PHI, AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2,       52
     *AG_RMIN, AG_RMAX, AG_RMN, AG_RMX, AG_ZI , AG_RMN1, AG_RMN2,         52
     *AG_RMX1, AG_RMX2, AG_H1, AG_H2, AG_BL1, AG_BL2, AG_TL1, AG_TL2,     52
     *AG_DPHI, AG_DZ, AG_TWIS, AG_X, AG_Y, AG_Z, AG_A, AG_ZA, AG_W,       52
     *AG_STEP, AG_C0, AG_PAR, AG_AA, AG_ZZ, AG_WW, AG_LX, AG_LY, AG_LZ,   52
     *AG_HX, AG_HY, AG_HZ, AG_P1, AG_P2, AG_STACK,AG_UBUF,AG_XHMAX,       52
     *AG_YHMAX,AG_ZHMAX,AG_RHMAX,AG_FHMAX,AG_FHMIN,AG_BIN, AG_CHARGE,     52
     *AG_MASS,AG_TLIFE,AG_BRATIO                                          52
      PARAMETER (AG_LSTACK=130,AG_NWUHIT=10,AG_NWUVOL=3,AG_MAGIC=         52
     *-696969,AG_LDETU=250)                                               52
      COMMON/AGCGLOB/ AG_MODULE,AG_CHDIR,AG_LEVEL,AG_IDTYPE,AG_IERROR,    52
     *AG_STANDALONE,IPRIN, AG_IPRIN,AG_IGEOM,AG_IDEBU,AG_IGRAP,AG_IHIST,  52
     *AG_IMFLD,AG_ISIMU                                                   52
C Inherited variables a: saved during internal calls                      52
C b: reset at the entry in a block and then saved                         52
      COMMON/AGCPARA/ AG_BEGCOM, AG_IVOLUME, AG_IMOTHER, AG_IGNUM,        52
     *AG_ISHAPE, AG_IMED, AG_IMAT, AG_IFIELD, AG_FIELDM, AG_TMAXFD,       52
     *AG_STEMAX, AG_DEEMAX, AG_EPSIL, AG_STMIN, AG_DENS, AG_RADL,         52
     *AG_ABSL, AG_DX, AG_DX1, AG_DX2, AG_DY, AG_DY1, AG_DY2, AG_RMN1,     52
     *AG_RMN2, AG_RMX1, AG_RMX2, AG_THET, AG_THE1, AG_THE2, AG_PHI,       52
     *AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2, AG_H1, AG_BL1,         52
     *AG_TL1, AG_H2, AG_BL2, AG_TL2, AG_RMIN, AG_RMAX, AG_DPHI,           52
     *AG_NPDIV, AG_NZ, AG_DZ, AG_TWIS, AG_LX, AG_LY, AG_LZ, AG_HX,        52
     *AG_HY, AG_HZ, AG_A, AG_ZA, AG_W, AG_NLMAT, AG_WORK, AG_SEEN,        52
     *AG_LSTY, AG_LWID, AG_COLO, AG_FILL, AG_SERIAL, AG_ISVOL,            52
     *AG_ISTATUS, AG_ZI(16), AG_RMN(16), AG_RMX(16), AG_VOLUME,           52
     *AG_MOTHER, AG_SHAPE, AG_CNICK, AG_ENDSAVE, AG_RESET1, AG_THETAX,    52
     *AG_PHIX,AG_THETAY,AG_PHIY,AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,      52
     *AG_ALFAZ, AG_X,AG_Y,AG_Z, AG_STEP,AG_C0,AG_NDIV,AG_IAXIS,           52
     *AG_NDVMAX,AG_ORTI,AG_NCOPY, AG_ALPHAN,AG_ALPHAK,AG_ALPHAL,          52
     *AG_RESET2, AG_KONLY, AG_ORT, AG_MARK                                52
C local variables valid inside same block                                 52
      COMMON/AGCLOCA/ AG_BEGSCR,AG_UBUF(100),AG_PAR(100),AG_AA(20),       52
     *AG_ZZ(20),AG_WW(20),AG_NWBUF, AG_XHMAX,AG_YHMAX,AG_ZHMAX,AG_RHMAX,  52
     *AG_FHMAX,AG_FHMIN,AG_NBITS,AG_BIN,AG_TYPE, AG_IROT,AG_NPAR,         52
     *AG_ISET,AG_IDET,AG_JDU,AG_IRESER, AG_ENDSCR, AG_TITLE,AG_EXNAME,    52
     *AG_PARLIST,AG_MATERIAL,AG_MIXTURE, AG_COMPONENT,AG_MEDIUM,          52
     *AG_OPTION                                                           52
      COMMON/AGCPART/AG_CODE,AG_TRKTYP,AG_MASS,AG_CHARGE,AG_TLIFE,        52
     *AG_BRATIO(6),AG_MODE(6),AG_PDG,AG_ECODE                             52
      COMMON/AGCSTAC/ AG_STACK(AG_LSTACK,15)                              52
C temporarly                                                              52
      EQUIVALENCE (AG_ATTRIBUTE,AG_WORK),(AG_STEMAX,AG_DMAXMS),           52
     *(AG_ALPHAX,AG_ALFAX), (AG_ALPHAY,AG_ALFAY),(AG_ALPHAZ,AG_ALFAZ),    52
     *(AG_TWIST,AG_TWIS),(AG_NPDIV,AG_NPDV),  (AG_P1,AG_HX),(AG_P2,       52
     *AG_HY)                                                              52
      REAL MIL_P,INCH_P                                                   52
      PARAMETER (INCH_P=2.54,MIL_P=0.00254)                               52
C - GEANT description of logical units                                    52
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            52
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      52
      COMMON/GCMAIL/ CHMAIL                                               52
      CHARACTER*132 CHMAIL                                                52
      REAL CENTER, DEPTH                                                  54
      REAL WEDGEOFFSET, WEDGEDX1, WEDGEDX2, WEDGELENGTH                   55
      REAL PLATEOFFSET, PLATEDX1, PLATEDX2, OFFHACK                       56
      REAL SENSOROFFSET,SENSORDX1,SENSORDX2,SENSORLENGTH                  57
      REAL RADDEG,START,ANGLE, TANHALFANGLE                               58
      INTEGER NL, LY                                                      60
      INTEGER BLOCK_FSMO                                                  62
      INTEGER BLOCK_FDMO                                                  62
      INTEGER BLOCK_FDMS                                                  62
      INTEGER BLOCK_FDMW                                                  62
      INTEGER BLOCK_FDSW                                                  62
      INTEGER BLOCK_FDTP                                                  62
      INTEGER BLOCK_FDSC                                                  62
      INTEGER BLOCK_FDWD                                                  62
      INTEGER BLOCK_FDWW                                                  62
C -                                                                       66
      REAL FSTG_LAYER                                                     66
      EQUIVALENCE (BANK_FSTG(3),FSTG_LAYER,RE_FSTG_LAYER),                66
     *(NAMES_FSTG(1),NFSTG_LAYER)                                         66
      EQUIVALENCE (COMMEN_FSTG(1),CFSTG_LAYER),(MAP_FSTG(1,1),            66
     *MFSTG_LAYER)                                                        66
      CHARACTER CFSTG_LAYER*40/' '/                                       66
      CHARACTER NFSTG_LAYER*12/'R.LAYER'/                                 66
      INTEGER MFSTG_LAYER(2)/1,1/                                         66
      REAL RE_FSTG_LAYER                                                  66
C -                                                                       66
      REAL FSTG_NSEC                                                      66
      EQUIVALENCE (BANK_FSTG(4),FSTG_NSEC,RE_FSTG_NSEC),(NAMES_FSTG(2),   66
     *NFSTG_NSEC)                                                         66
      EQUIVALENCE (COMMEN_FSTG(2),CFSTG_NSEC),(MAP_FSTG(1,2),             66
     *MFSTG_NSEC)                                                         66
      CHARACTER CFSTG_NSEC*40/' '/                                        66
      CHARACTER NFSTG_NSEC*12/'R.NSEC'/                                   66
      INTEGER MFSTG_NSEC(2)/1,1/                                          66
      REAL RE_FSTG_NSEC                                                   66
C -                                                                       66
      REAL FSTG_SPACING                                                   66
      EQUIVALENCE (BANK_FSTG(5),FSTG_SPACING,RE_FSTG_SPACING),            66
     *(NAMES_FSTG(3),NFSTG_SPACING)                                       66
      EQUIVALENCE (COMMEN_FSTG(3),CFSTG_SPACING),(MAP_FSTG(1,3),          66
     *MFSTG_SPACING)                                                      66
      CHARACTER CFSTG_SPACING*40/' '/                                     66
      CHARACTER NFSTG_SPACING*12/'R.SPACING'/                             66
      INTEGER MFSTG_SPACING(2)/1,1/                                       66
      REAL RE_FSTG_SPACING                                                66
C -                                                                       66
      REAL FSTG_RMINS                                                     66
      EQUIVALENCE (BANK_FSTG(6),FSTG_RMINS,RE_FSTG_RMINS),                66
     *(NAMES_FSTG(4),NFSTG_RMINS)                                         66
      EQUIVALENCE (COMMEN_FSTG(4),CFSTG_RMINS),(MAP_FSTG(1,4),            66
     *MFSTG_RMINS)                                                        66
      CHARACTER CFSTG_RMINS*40/' '/                                       66
      CHARACTER NFSTG_RMINS*12/'R.RMINS'/                                 66
      INTEGER MFSTG_RMINS(2)/1,1/                                         66
      REAL RE_FSTG_RMINS                                                  66
C -                                                                       66
      REAL FSTG_RMAXS                                                     66
      EQUIVALENCE (BANK_FSTG(7),FSTG_RMAXS,RE_FSTG_RMAXS),                66
     *(NAMES_FSTG(5),NFSTG_RMAXS)                                         66
      EQUIVALENCE (COMMEN_FSTG(5),CFSTG_RMAXS),(MAP_FSTG(1,5),            66
     *MFSTG_RMAXS)                                                        66
      CHARACTER CFSTG_RMAXS*40/' '/                                       66
      CHARACTER NFSTG_RMAXS*12/'R.RMAXS'/                                 66
      INTEGER MFSTG_RMAXS(2)/1,1/                                         66
      REAL RE_FSTG_RMAXS                                                  66
C -                                                                       66
      REAL FSTG_RMIN                                                      66
      EQUIVALENCE (BANK_FSTG(8),FSTG_RMIN,RE_FSTG_RMIN),(NAMES_FSTG(6),   66
     *NFSTG_RMIN)                                                         66
      EQUIVALENCE (COMMEN_FSTG(6),CFSTG_RMIN),(MAP_FSTG(1,6),             66
     *MFSTG_RMIN)                                                         66
      CHARACTER CFSTG_RMIN*40/' '/                                        66
      CHARACTER NFSTG_RMIN*12/'R.RMIN'/                                   66
      INTEGER MFSTG_RMIN(2)/1,1/                                          66
      REAL RE_FSTG_RMIN                                                   66
C -                                                                       66
      REAL FSTG_RMAX                                                      66
      EQUIVALENCE (BANK_FSTG(9),FSTG_RMAX,RE_FSTG_RMAX),(NAMES_FSTG(7),   66
     *NFSTG_RMAX)                                                         66
      EQUIVALENCE (COMMEN_FSTG(7),CFSTG_RMAX),(MAP_FSTG(1,7),             66
     *MFSTG_RMAX)                                                         66
      CHARACTER CFSTG_RMAX*40/' '/                                        66
      CHARACTER NFSTG_RMAX*12/'R.RMAX'/                                   66
      INTEGER MFSTG_RMAX(2)/1,1/                                          66
      REAL RE_FSTG_RMAX                                                   66
C -                                                                       66
      REAL FSTG_ZMIN                                                      66
      EQUIVALENCE (BANK_FSTG(10),FSTG_ZMIN,RE_FSTG_ZMIN),(NAMES_FSTG(8),  66
     *NFSTG_ZMIN)                                                         66
      EQUIVALENCE (COMMEN_FSTG(8),CFSTG_ZMIN),(MAP_FSTG(1,8),             66
     *MFSTG_ZMIN)                                                         66
      CHARACTER CFSTG_ZMIN*40/' '/                                        66
      CHARACTER NFSTG_ZMIN*12/'R.ZMIN'/                                   66
      INTEGER MFSTG_ZMIN(2)/1,1/                                          66
      REAL RE_FSTG_ZMIN                                                   66
C -                                                                       66
      REAL FSTG_ZMAX                                                      66
      EQUIVALENCE (BANK_FSTG(11),FSTG_ZMAX,RE_FSTG_ZMAX),(NAMES_FSTG(9),  66
     *NFSTG_ZMAX)                                                         66
      EQUIVALENCE (COMMEN_FSTG(9),CFSTG_ZMAX),(MAP_FSTG(1,9),             66
     *MFSTG_ZMAX)                                                         66
      CHARACTER CFSTG_ZMAX*40/' '/                                        66
      CHARACTER NFSTG_ZMAX*12/'R.ZMAX'/                                   66
      INTEGER MFSTG_ZMAX(2)/1,1/                                          66
      REAL RE_FSTG_ZMAX                                                   66
C -                                                                       66
      REAL FSTG_WEDGETHK                                                  66
      EQUIVALENCE (BANK_FSTG(12),FSTG_WEDGETHK,RE_FSTG_WEDGETHK),         66
     *(NAMES_FSTG(10),NFSTG_WEDGETHK)                                     66
      EQUIVALENCE (COMMEN_FSTG(10),CFSTG_WEDGETHK),(MAP_FSTG(1,10),       66
     *MFSTG_WEDGETHK)                                                     66
      CHARACTER CFSTG_WEDGETHK*40/' '/                                    66
      CHARACTER NFSTG_WEDGETHK*12/'R.WEDGETHK'/                           66
      INTEGER MFSTG_WEDGETHK(2)/1,1/                                      66
      REAL RE_FSTG_WEDGETHK                                               66
C -                                                                       66
      REAL FSTG_SENSATHK                                                  66
      EQUIVALENCE (BANK_FSTG(13),FSTG_SENSATHK,RE_FSTG_SENSATHK),         66
     *(NAMES_FSTG(11),NFSTG_SENSATHK)                                     66
      EQUIVALENCE (COMMEN_FSTG(11),CFSTG_SENSATHK),(MAP_FSTG(1,11),       66
     *MFSTG_SENSATHK)                                                     66
      CHARACTER CFSTG_SENSATHK*40/' '/                                    66
      CHARACTER NFSTG_SENSATHK*12/'R.SENSATHK'/                           66
      INTEGER MFSTG_SENSATHK(2)/1,1/                                      66
      REAL RE_FSTG_SENSATHK                                               66
C -                                                                       66
      REAL FSTG_SENSORTHK                                                 66
      EQUIVALENCE (BANK_FSTG(14),FSTG_SENSORTHK,RE_FSTG_SENSORTHK),       66
     *(NAMES_FSTG(12),NFSTG_SENSORTHK)                                    66
      EQUIVALENCE (COMMEN_FSTG(12),CFSTG_SENSORTHK),(MAP_FSTG(1,12),      66
     *MFSTG_SENSORTHK)                                                    66
      CHARACTER CFSTG_SENSORTHK*40/' '/                                   66
      CHARACTER NFSTG_SENSORTHK*12/'R.SENSORTHK'/                         66
      INTEGER MFSTG_SENSORTHK(2)/1,1/                                     66
      REAL RE_FSTG_SENSORTHK                                              66
C -                                                                       66
      REAL FSTG_Z(4)                                                      66
      EQUIVALENCE (BANK_FSTG(15),FSTG_Z,RE_FSTG_Z),(NAMES_FSTG(13),       66
     *NFSTG_Z)                                                            66
      EQUIVALENCE (COMMEN_FSTG(13),CFSTG_Z),(MAP_FSTG(1,13),MFSTG_Z)      66
      CHARACTER CFSTG_Z*40/' '/                                           66
      CHARACTER NFSTG_Z*12/'R.Z'/                                         66
      INTEGER MFSTG_Z(2)/4,1/                                             66
      REAL RE_FSTG_Z                                                      66
C -                                                                       66
      INTEGER LEN_FSTG,LLEN_FSTG,LINK_FSTG                                66
      PARAMETER (LEN_FSTG=18)                                             66
      PARAMETER (LLEN_FSTG=13)                                            66
      INTEGER FLAG_FSTG/0/,MAP_FSTG(2,LLEN_FSTG),IDNUM_FSTG(10)           66
      CHARACTER NAMES_FSTG(LLEN_FSTG)*12,COMMEN_FSTG(LLEN_FSTG)*40        66
      DATA IDNUM_FSTG/10*0/                                               66
      REAL BANK_FSTG(LEN_FSTG)                                            66
      CHARACTER*60 BPATH_FSTG/' '/                                        66
      SAVE FLAG_FSTG,MAP_FSTG,IDNUM_FSTG,NAMES_FSTG,COMMEN_FSTG,          66
     *BPATH_FSTG                                                          66
      DATA LINK_FSTG/0/,BANK_FSTG/LEN_FSTG*0/                             66
C *                                                                       66
C -                                                                       68
      REAL FSAN_VERSION                                                   68
      EQUIVALENCE (BANK_FSAN(3),FSAN_VERSION,RE_FSAN_VERSION),            68
     *(NAMES_FSAN(1),NFSAN_VERSION)                                       68
      EQUIVALENCE (COMMEN_FSAN(1),CFSAN_VERSION),(MAP_FSAN(1,1),          68
     *MFSAN_VERSION)                                                      68
      CHARACTER CFSAN_VERSION*40/' '/                                     68
      CHARACTER NFSAN_VERSION*12/'R.VERSION'/                             68
      INTEGER MFSAN_VERSION(2)/1,1/                                       68
      REAL RE_FSAN_VERSION                                                68
C -                                                                       68
      REAL FSAN_THK                                                       68
      EQUIVALENCE (BANK_FSAN(4),FSAN_THK,RE_FSAN_THK),(NAMES_FSAN(2),     68
     *NFSAN_THK)                                                          68
      EQUIVALENCE (COMMEN_FSAN(2),CFSAN_THK),(MAP_FSAN(1,2),MFSAN_THK)    68
      CHARACTER CFSAN_THK*40/' '/                                         68
      CHARACTER NFSAN_THK*12/'R.THK'/                                     68
      INTEGER MFSAN_THK(2)/1,1/                                           68
      REAL RE_FSAN_THK                                                    68
C -                                                                       68
      REAL FSAN_LENGTH                                                    68
      EQUIVALENCE (BANK_FSAN(5),FSAN_LENGTH,RE_FSAN_LENGTH),              68
     *(NAMES_FSAN(3),NFSAN_LENGTH)                                        68
      EQUIVALENCE (COMMEN_FSAN(3),CFSAN_LENGTH),(MAP_FSAN(1,3),           68
     *MFSAN_LENGTH)                                                       68
      CHARACTER CFSAN_LENGTH*40/' '/                                      68
      CHARACTER NFSAN_LENGTH*12/'R.LENGTH'/                               68
      INTEGER MFSAN_LENGTH(2)/1,1/                                        68
      REAL RE_FSAN_LENGTH                                                 68
C -                                                                       68
      INTEGER LEN_FSAN,LLEN_FSAN,LINK_FSAN                                68
      PARAMETER (LEN_FSAN=5)                                              68
      PARAMETER (LLEN_FSAN=3)                                             68
      INTEGER FLAG_FSAN/0/,MAP_FSAN(2,LLEN_FSAN),IDNUM_FSAN(10)           68
      CHARACTER NAMES_FSAN(LLEN_FSAN)*12,COMMEN_FSAN(LLEN_FSAN)*40        68
      DATA IDNUM_FSAN/10*0/                                               68
      REAL BANK_FSAN(LEN_FSAN)                                            68
      CHARACTER*60 BPATH_FSAN/' '/                                        68
      SAVE FLAG_FSAN,MAP_FSAN,IDNUM_FSAN,NAMES_FSAN,COMMEN_FSAN,          68
     *BPATH_FSAN                                                          68
      DATA LINK_FSAN/0/,BANK_FSAN/LEN_FSAN*0/                             68
C *                                                                       68
C -                                                                       70
      REAL FSCG_NUMMER                                                    70
      EQUIVALENCE (BANK_FSCG(3),FSCG_NUMMER,RE_FSCG_NUMMER),              70
     *(NAMES_FSCG(1),NFSCG_NUMMER)                                        70
      EQUIVALENCE (COMMEN_FSCG(1),CFSCG_NUMMER),(MAP_FSCG(1,1),           70
     *MFSCG_NUMMER)                                                       70
      CHARACTER CFSCG_NUMMER*40/' '/                                      70
      CHARACTER NFSCG_NUMMER*12/'R.NUMMER'/                               70
      INTEGER MFSCG_NUMMER(2)/1,1/                                        70
      REAL RE_FSCG_NUMMER                                                 70
C -                                                                       70
      REAL FSCG_W                                                         70
      EQUIVALENCE (BANK_FSCG(4),FSCG_W,RE_FSCG_W),(NAMES_FSCG(2),         70
     *NFSCG_W)                                                            70
      EQUIVALENCE (COMMEN_FSCG(2),CFSCG_W),(MAP_FSCG(1,2),MFSCG_W)        70
      CHARACTER CFSCG_W*40/' '/                                           70
      CHARACTER NFSCG_W*12/'R.W'/                                         70
      INTEGER MFSCG_W(2)/1,1/                                             70
      REAL RE_FSCG_W                                                      70
C -                                                                       70
      REAL FSCG_H                                                         70
      EQUIVALENCE (BANK_FSCG(5),FSCG_H,RE_FSCG_H),(NAMES_FSCG(3),         70
     *NFSCG_H)                                                            70
      EQUIVALENCE (COMMEN_FSCG(3),CFSCG_H),(MAP_FSCG(1,3),MFSCG_H)        70
      CHARACTER CFSCG_H*40/' '/                                           70
      CHARACTER NFSCG_H*12/'R.H'/                                         70
      INTEGER MFSCG_H(2)/1,1/                                             70
      REAL RE_FSCG_H                                                      70
C -                                                                       70
      REAL FSCG_THK                                                       70
      EQUIVALENCE (BANK_FSCG(6),FSCG_THK,RE_FSCG_THK),(NAMES_FSCG(4),     70
     *NFSCG_THK)                                                          70
      EQUIVALENCE (COMMEN_FSCG(4),CFSCG_THK),(MAP_FSCG(1,4),MFSCG_THK)    70
      CHARACTER CFSCG_THK*40/' '/                                         70
      CHARACTER NFSCG_THK*12/'R.THK'/                                     70
      INTEGER MFSCG_THK(2)/1,1/                                           70
      REAL RE_FSCG_THK                                                    70
C -                                                                       70
      INTEGER LEN_FSCG,LLEN_FSCG,LINK_FSCG                                70
      PARAMETER (LEN_FSCG=6)                                              70
      PARAMETER (LLEN_FSCG=4)                                             70
      INTEGER FLAG_FSCG/0/,MAP_FSCG(2,LLEN_FSCG),IDNUM_FSCG(10)           70
      CHARACTER NAMES_FSCG(LLEN_FSCG)*12,COMMEN_FSCG(LLEN_FSCG)*40        70
      DATA IDNUM_FSCG/10*0/                                               70
      REAL BANK_FSCG(LEN_FSCG)                                            70
      CHARACTER*60 BPATH_FSCG/' '/                                        70
      SAVE FLAG_FSCG,MAP_FSCG,IDNUM_FSCG,NAMES_FSCG,COMMEN_FSCG,          70
     *BPATH_FSCG                                                          70
      DATA LINK_FSCG/0/,BANK_FSCG/LEN_FSCG*0/                             70
C *                                                                       70
C -                                                                       72
      REAL FDWG_VERSION                                                   72
      EQUIVALENCE (BANK_FDWG(3),FDWG_VERSION,RE_FDWG_VERSION),            72
     *(NAMES_FDWG(1),NFDWG_VERSION)                                       72
      EQUIVALENCE (COMMEN_FDWG(1),CFDWG_VERSION),(MAP_FDWG(1,1),          72
     *MFDWG_VERSION)                                                      72
      CHARACTER CFDWG_VERSION*40/' '/                                     72
      CHARACTER NFDWG_VERSION*12/'R.VERSION'/                             72
      INTEGER MFDWG_VERSION(2)/1,1/                                       72
      REAL RE_FDWG_VERSION                                                72
C -                                                                       72
      REAL FDWG_RMIN                                                      72
      EQUIVALENCE (BANK_FDWG(4),FDWG_RMIN,RE_FDWG_RMIN),(NAMES_FDWG(2),   72
     *NFDWG_RMIN)                                                         72
      EQUIVALENCE (COMMEN_FDWG(2),CFDWG_RMIN),(MAP_FDWG(1,2),             72
     *MFDWG_RMIN)                                                         72
      CHARACTER CFDWG_RMIN*40/' '/                                        72
      CHARACTER NFDWG_RMIN*12/'R.RMIN'/                                   72
      INTEGER MFDWG_RMIN(2)/1,1/                                          72
      REAL RE_FDWG_RMIN                                                   72
C -                                                                       72
      REAL FDWG_RMAX                                                      72
      EQUIVALENCE (BANK_FDWG(5),FDWG_RMAX,RE_FDWG_RMAX),(NAMES_FDWG(3),   72
     *NFDWG_RMAX)                                                         72
      EQUIVALENCE (COMMEN_FDWG(3),CFDWG_RMAX),(MAP_FDWG(1,3),             72
     *MFDWG_RMAX)                                                         72
      CHARACTER CFDWG_RMAX*40/' '/                                        72
      CHARACTER NFDWG_RMAX*12/'R.RMAX'/                                   72
      INTEGER MFDWG_RMAX(2)/1,1/                                          72
      REAL RE_FDWG_RMAX                                                   72
C -                                                                       72
      REAL FDWG_THK                                                       72
      EQUIVALENCE (BANK_FDWG(6),FDWG_THK,RE_FDWG_THK),(NAMES_FDWG(4),     72
     *NFDWG_THK)                                                          72
      EQUIVALENCE (COMMEN_FDWG(4),CFDWG_THK),(MAP_FDWG(1,4),MFDWG_THK)    72
      CHARACTER CFDWG_THK*40/' '/                                         72
      CHARACTER NFDWG_THK*12/'R.THK'/                                     72
      INTEGER MFDWG_THK(2)/1,1/                                           72
      REAL RE_FDWG_THK                                                    72
C -                                                                       72
      REAL FDWG_LEN                                                       72
      EQUIVALENCE (BANK_FDWG(7),FDWG_LEN,RE_FDWG_LEN),(NAMES_FDWG(5),     72
     *NFDWG_LEN)                                                          72
      EQUIVALENCE (COMMEN_FDWG(5),CFDWG_LEN),(MAP_FDWG(1,5),MFDWG_LEN)    72
      CHARACTER CFDWG_LEN*40/' '/                                         72
      CHARACTER NFDWG_LEN*12/'R.LEN'/                                     72
      INTEGER MFDWG_LEN(2)/1,1/                                           72
      REAL RE_FDWG_LEN                                                    72
C -                                                                       72
      REAL FDWG_WALLTHK                                                   72
      EQUIVALENCE (BANK_FDWG(8),FDWG_WALLTHK,RE_FDWG_WALLTHK),            72
     *(NAMES_FDWG(6),NFDWG_WALLTHK)                                       72
      EQUIVALENCE (COMMEN_FDWG(6),CFDWG_WALLTHK),(MAP_FDWG(1,6),          72
     *MFDWG_WALLTHK)                                                      72
      CHARACTER CFDWG_WALLTHK*40/' '/                                     72
      CHARACTER NFDWG_WALLTHK*12/'R.WALLTHK'/                             72
      INTEGER MFDWG_WALLTHK(2)/1,1/                                       72
      REAL RE_FDWG_WALLTHK                                                72
C -                                                                       72
      INTEGER LEN_FDWG,LLEN_FDWG,LINK_FDWG                                72
      PARAMETER (LEN_FDWG=8)                                              72
      PARAMETER (LLEN_FDWG=6)                                             72
      INTEGER FLAG_FDWG/0/,MAP_FDWG(2,LLEN_FDWG),IDNUM_FDWG(10)           72
      CHARACTER NAMES_FDWG(LLEN_FDWG)*12,COMMEN_FDWG(LLEN_FDWG)*40        72
      DATA IDNUM_FDWG/10*0/                                               72
      REAL BANK_FDWG(LEN_FDWG)                                            72
      CHARACTER*60 BPATH_FDWG/' '/                                        72
      SAVE FLAG_FDWG,MAP_FDWG,IDNUM_FDWG,NAMES_FDWG,COMMEN_FDWG,          72
     *BPATH_FDWG                                                          72
      DATA LINK_FDWG/0/,BANK_FDWG/LEN_FDWG*0/                             72
C *                                                                       72
*
* -----------------------------------------------------------------------------
*
C *                                                                       76
      CALL AGSBEGM(MODULE,IPRIN)                                          76
      IF (IPRIN.LT.0) RETURN                                              76
C Filling bank FSTG - FORWARD SILICON TRACKER GEOMETRY DATA               76
      BANK_TITLE='FORWARD SILICON TRACKER GEOMETRY DATA'                  76
      BPATH_FSTG='FSTG*'                                                  76
      IDNUM_FSTG(1)=0                                                     77
C LAYER INDEX (RESERVED)                                                  77
      FSTG_LAYER=1                                                        77
      CFSTG_LAYER='LAYER INDEX (RESERVED)'                                77
C NUMBER OF SECTORS IN A WHEEL                                            78
      FSTG_NSEC=21                                                        78
      CFSTG_NSEC='NUMBER OF SECTORS IN A WHEEL'                           78
C SPACING BETWEEN THE WAFERS                                              79
      FSTG_SPACING=0.1686                                                 79
      CFSTG_SPACING='SPACING BETWEEN THE WAFERS'                          79
C INNER RADIUS OF SENSITIVE WEDGE                                         81
      FSTG_RMINS=7.0                                                      81
      CFSTG_RMINS='INNER RADIUS OF SENSITIVE WEDGE'                       81
C OUTER RADIUS OF SENSITIVE WEDGE                                         82
      FSTG_RMAXS=19.0                                                     82
      CFSTG_RMAXS='OUTER RADIUS OF SENSITIVE WEDGE'                       82
C INNER RADIUS OF ALL OF THE DETECTOR                                     84
      FSTG_RMIN=7.0                                                       84
      CFSTG_RMIN='INNER RADIUS OF ALL OF THE DETECTOR'                    84
C OUTER RADIUS OF ALL OF THE DETECTOR                                     85
      FSTG_RMAX=22.5                                                      85
      CFSTG_RMAX='OUTER RADIUS OF ALL OF THE DETECTOR'                    85
*GvN First disk is sitting at 280mm, last disk is sitting at 370mm
C Z-START OF THE BARREL COMPRISING THE THREE PANCAKES                     88
      FSTG_ZMIN=27.4619                                                   88
      CFSTG_ZMIN='Z-START OF THE BARREL COMPRISING THE THREE PANCAKES'    88
C Z-FINISH OF THE BARREL COMPRISING THE THREE PANCAKES                    89
      FSTG_ZMAX=37.5381                                                   89
      CFSTG_ZMAX='Z-FINISH OF THE BARREL COMPRISING THE THREE PANCAKES'   89
C INCLUDES SENSOR ASSEMBLY AND WATER PIPES                                92
      FSTG_WEDGETHK=1.0762                                                92
      CFSTG_WEDGETHK='INCLUDES SENSOR ASSEMBLY AND WATER PIPES'           92
C SENSOR ASSEMBLY THK: INCLUDES TWO LAYERS OF SI, ALN PLATES AND CHIPS    93
      FSTG_SENSATHK=0.3686                                                93
      CFSTG_SENSATHK='SENSOR ASSEMBLY THK: INCLUDES TWO LAYERS OF SI, AL  93
     *N PLATES AND CHIPS'                                                 93
C TOTAL SILICON THICKNESS, INCLUDES PASSIVE AND ACTIVE                    94
      FSTG_SENSORTHK=0.030                                                94
      CFSTG_SENSORTHK='TOTAL SILICON THICKNESS, INCLUDES PASSIVE AND ACT  94
     *IVE'                                                                94
C NOMINAL ZS OF THE FACES                                                 96
      FSTG_Z(1)=0.0                                                       96
      FSTG_Z(2)=3.0                                                       96
      FSTG_Z(3)=6.0                                                       96
      FSTG_Z(4)=9.0                                                       96
      CFSTG_Z='NOMINAL ZS OF THE FACES'                                   96
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FSTG',    98
     *BPATH_FSTG,IDNUM_FSTG,1, MAP_FSTG,NAMES_FSTG,COMMEN_FSTG,           98
     *BANK_FSTG,LEN_FSTG,LLEN_FSTG,LINK_FSTG,FLAG_FSTG)                   98
C *                                                                       98
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FSTG',    98
     *BPATH_FSTG,IDNUM_FSTG,1, MAP_FSTG,NAMES_FSTG,COMMEN_FSTG,           98
     *BANK_FSTG,LEN_FSTG,LLEN_FSTG,LINK_FSTG,FLAG_FSTG)                   98
C *                                                                       100
      CALL AGSBEGM(MODULE,IPRIN)                                          100
      IF (IPRIN.LT.0) RETURN                                              100
C Filling bank FSAN - ALUMINUM NITRIDE THERMAL PLATE                      100
      BANK_TITLE='ALUMINUM NITRIDE THERMAL PLATE'                         100
      BPATH_FSAN='FSAN*'                                                  100
      IDNUM_FSAN(1)=0                                                     101
C MAY HAVE A FEW                                                          101
      FSAN_VERSION=1                                                      101
      CFSAN_VERSION='MAY HAVE A FEW'                                      101
C ALN THICKNESS                                                           102
      FSAN_THK=0.0762                                                     102
      CFSAN_THK='ALN THICKNESS'                                           102
C ALN LENGTH ALONG RADIUS                                                 103
      FSAN_LENGTH=3.0                                                     103
      CFSAN_LENGTH='ALN LENGTH ALONG RADIUS'                              103
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FSAN',    104
     *BPATH_FSAN,IDNUM_FSAN,1, MAP_FSAN,NAMES_FSAN,COMMEN_FSAN,           104
     *BANK_FSAN,LEN_FSAN,LLEN_FSAN,LINK_FSAN,FLAG_FSAN)                   104
C *                                                                       104
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FSAN',    104
     *BPATH_FSAN,IDNUM_FSAN,1, MAP_FSAN,NAMES_FSAN,COMMEN_FSAN,           104
     *BANK_FSAN,LEN_FSAN,LLEN_FSAN,LINK_FSAN,FLAG_FSAN)                   104
C *                                                                       106
      CALL AGSBEGM(MODULE,IPRIN)                                          106
      IF (IPRIN.LT.0) RETURN                                              106
C Filling bank FSAN - ALUMINUM NITRIDE THERMAL PLATE                      106
      BANK_TITLE='ALUMINUM NITRIDE THERMAL PLATE'                         106
      BPATH_FSAN='FSAN*'                                                  106
      IDNUM_FSAN(1)=0                                                     107
C MAY HAVE A FEW                                                          107
      FSAN_VERSION=2                                                      107
      CFSAN_VERSION='MAY HAVE A FEW'                                      107
C ALN THICKNESS                                                           108
      FSAN_THK=0.0762                                                     108
      CFSAN_THK='ALN THICKNESS'                                           108
C ALN LENGTH ALONG RADIUS                                                 109
      FSAN_LENGTH=2.0                                                     109
      CFSAN_LENGTH='ALN LENGTH ALONG RADIUS'                              109
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FSAN',    110
     *BPATH_FSAN,IDNUM_FSAN,1, MAP_FSAN,NAMES_FSAN,COMMEN_FSAN,           110
     *BANK_FSAN,LEN_FSAN,LLEN_FSAN,LINK_FSAN,FLAG_FSAN)                   110
C *                                                                       110
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FSAN',    110
     *BPATH_FSAN,IDNUM_FSAN,1, MAP_FSAN,NAMES_FSAN,COMMEN_FSAN,           110
     *BANK_FSAN,LEN_FSAN,LLEN_FSAN,LINK_FSAN,FLAG_FSAN)                   110
C *                                                                       113
      CALL AGSBEGM(MODULE,IPRIN)                                          113
      IF (IPRIN.LT.0) RETURN                                              113
C Filling bank FSCG - FORWARD SILICON READOUT CHIP GEOMETRY, FIRST APPRO  113
C X.                                                                      113
      BANK_TITLE='FORWARD SILICON READOUT CHIP GEOMETRY, FIRST APPROX.'   113
      BPATH_FSCG='FSCG*'                                                  113
      IDNUM_FSCG(1)=0                                                     114
C WE CAN HAVE A FEW DIFFERENT CHIPS                                       114
      FSCG_NUMMER=1                                                       114
      CFSCG_NUMMER='WE CAN HAVE A FEW DIFFERENT CHIPS'                    114
C WIDTH                                                                   115
      FSCG_W=5.4                                                          115
      CFSCG_W='WIDTH'                                                     115
C HEIGHT, APV25 SIZE KEEPS CHANGING!                                      116
      FSCG_H=0.82                                                         116
      CFSCG_H='HEIGHT, APV25 SIZE KEEPS CHANGING!'                        116
C THICKNESS                                                               117
      FSCG_THK=0.07                                                       117
      CFSCG_THK='THICKNESS'                                               117
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FSCG',    118
     *BPATH_FSCG,IDNUM_FSCG,1, MAP_FSCG,NAMES_FSCG,COMMEN_FSCG,           118
     *BANK_FSCG,LEN_FSCG,LLEN_FSCG,LINK_FSCG,FLAG_FSCG)                   118
C *                                                                       118
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FSCG',    118
     *BPATH_FSCG,IDNUM_FSCG,1, MAP_FSCG,NAMES_FSCG,COMMEN_FSCG,           118
     *BANK_FSCG,LEN_FSCG,LLEN_FSCG,LINK_FSCG,FLAG_FSCG)                   118
C *                                                                       120
      CALL AGSBEGM(MODULE,IPRIN)                                          120
      IF (IPRIN.LT.0) RETURN                                              120
C Filling bank FDWG - WATER DUCT GEOMETRY                                 120
      BANK_TITLE='WATER DUCT GEOMETRY'                                    120
      BPATH_FDWG='FDWG*'                                                  120
      IDNUM_FDWG(1)=0                                                     121
C VERSION                                                                 121
      FDWG_VERSION=1                                                      121
      CFDWG_VERSION='VERSION'                                             121
C INNER RADIUS OF THE DUCT                                                122
      FDWG_RMIN=21.0                                                      122
      CFDWG_RMIN='INNER RADIUS OF THE DUCT'                               122
C OUTER RADIUS OF THE DUCT                                                123
      FDWG_RMAX=22.0                                                      123
      CFDWG_RMAX='OUTER RADIUS OF THE DUCT'                               123
C THICKNESS IN Z                                                          124
      FDWG_THK=0.5                                                        124
      CFDWG_THK='THICKNESS IN Z'                                          124
C LENGTH                                                                  125
      FDWG_LEN=6.3                                                        125
      CFDWG_LEN='LENGTH'                                                  125
C WALL THICKNESS                                                          126
      FDWG_WALLTHK=0.1                                                    126
      CFDWG_WALLTHK='WALL THICKNESS'                                      126
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FDWG',    127
     *BPATH_FDWG,IDNUM_FDWG,1, MAP_FDWG,NAMES_FDWG,COMMEN_FDWG,           127
     *BANK_FDWG,LEN_FDWG,LLEN_FDWG,LINK_FDWG,FLAG_FDWG)                   127
C *                                                                       127
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FDWG',    127
     *BPATH_FDWG,IDNUM_FDWG,1, MAP_FDWG,NAMES_FDWG,COMMEN_FDWG,           127
     *BANK_FDWG,LEN_FDWG,LLEN_FDWG,LINK_FDWG,FLAG_FDWG)                   127
*
******************************************************
C use default FSTG bank                                                   131
      BPATH_FSTG='FSTG*'                                                  131
      IDNUM_FSTG(1)=0                                                     131
      CALL AGDGETP('FSTDGEO','*',-999,'FSTG',' ',BANK_FSTG, BPATH_FSTG,   131
     *IDNUM_FSTG,1,MAP_FSTG,NAMES_FSTG,BANK_FSTG,LEN_FSTG,LLEN_FSTG,      131
     *LINK_FSTG,FLAG_FSTG)                                                131
C use FSAN bank                                                           132
      BPATH_FSAN='FSAN*'                                                  132
      IDNUM_FSAN(1)=0                                                     132
      FSAN_VERSION=1                                                      132
      CALL AGDGETP('FSTDGEO','*',-999,'FSAN',NFSAN_VERSION,               132
     *RE_FSAN_VERSION,BPATH_FSAN,IDNUM_FSAN,1,MAP_FSAN,NAMES_FSAN,        132
     *BANK_FSAN,LEN_FSAN,LLEN_FSAN,LINK_FSAN,FLAG_FSAN)                   132
C use default FSCG bank                                                   133
      BPATH_FSCG='FSCG*'                                                  133
      IDNUM_FSCG(1)=0                                                     133
      CALL AGDGETP('FSTDGEO','*',-999,'FSCG',' ',BANK_FSCG, BPATH_FSCG,   133
     *IDNUM_FSCG,1,MAP_FSCG,NAMES_FSCG,BANK_FSCG,LEN_FSCG,LLEN_FSCG,      133
     *LINK_FSCG,FLAG_FSCG)                                                133
*
      RADDEG=3.14159265/180.0                                             135
C OPENING ANGLE OF THE SECTOR                                             137
      ANGLE=360.0/FSTG_NSEC                                               137
      TANHALFANGLE=TAN(ANGLE*RADDEG/2.0)                                  138
      SENSORLENGTH= FSTG_RMAXS - FSTG_RMINS                               140
      SENSORDX1 = FSTG_RMINS*TANHALFANGLE                                 141
      SENSORDX2 = FSTG_RMAXS*TANHALFANGLE                                 142
      WEDGELENGTH = SENSORLENGTH+FSAN_LENGTH                              142
      SENSOROFFSET = -0.5*(WEDGELENGTH-SENSORLENGTH)                      143
      WEDGEDX1 = FSTG_RMIN*TANHALFANGLE                                   144
      WEDGEDX2 = (FSTG_RMAXS+FSAN_LENGTH)*TANHALFANGLE                    145
      WEDGEOFFSET = FSTG_RMIN + 0.5*WEDGELENGTH                           146
* Top sector has to be at 12 o'clock: May need Angular Offset --
* Keep this blank for now
      START=0.0                                                           152
      CENTER=(FSTG_ZMAX+FSTG_ZMIN)/2.0                                    154
      DEPTH =(FSTG_ZMAX-FSTG_ZMIN)                                        155
      CALL AGSBEGM(MODULE,IPRIN)                                          157
C Check IPRIN<0                                                           157
      IF (IPRIN.LT.0) RETURN                                              157
C Create FSMO :                                                           157
C Call To Local Procedure FSMO                                            157
      ASSIGN 5020 TO BLOCK_FSMO                                           157
      GO TO  5010                                                         157
5020  CONTINUE                                                            157
C *                                                                       157
C Geant POSITION : FSMO ===> MOTHER='CAVE' Z=+CENTER KONLY='MANY' <===    158
      CALL AGSRESET                                                       158
      AG_MOTHER = 'CAVE'                                                  158
      AG_Z = +CENTER                                                      158
      AG_KONLY = 'MANY'                                                   158
      AG_TITLE = 'FSMO'                                                   158
      AG_EXNAME = 'POSITION'                                              158
      AG_PARLIST = 'MOTHER_Z_KONLY'                                       158
      CALL AXPOSITION                                                     158
C *                                                                       158
* -----------------------------------------------------------------------------
      CALL AGSENDM                                                        160
      RETURN                                                              160
C Begin Local Procedure FSMO                                              160
C FSMO IS THE MOTHER OF ONE ENDCAP OF FSTD                                160
5010  CONTINUE                                                            160
         AG_TITLE='FSMO'                                                  160
         AG_MODULE=MODULE                                                 160
         AG_MARK='V00'                                                    160
         CALL AGSPUSH                                                     160
         AG_MODULE=MODULE                                                 160
C    *                                                                    160
C    Geant MATERIAL : AIR ===> inherited arguments used <===              161
         CALL AGSRESET                                                    161
         AG_TITLE = 'AIR'                                                 161
         AG_EXNAME = 'MATERIAL'                                           161
         AG_PARLIST = 'NONE'                                              161
         CALL AXMATERIAL                                                  161
C    *                                                                    161
C    Geant ATTRIBUTE : FSMO ===> SEEN=1 COLO=6 <===                       161
         CALL AGSRESET                                                    162
         AG_SEEN = 1                                                      162
         AG_COLO = 6                                                      162
         AG_TITLE = 'FSMO'                                                162
         AG_EXNAME = 'ATTRIBUTE'                                          162
         AG_PARLIST = 'SEEN_COLO'                                         162
         CALL AXATTRIBUTE                                                 162
C    *                                                                    162
C    Geant SHAPE : TUBE ===> RMIN=FSTG_RMIN RMAX=FSTG_RMAX DZ=DEPTH/2.0   166
C    <===                                                                 166
         CALL AGSRESET                                                    166
         AG_RMIN = FSTG_RMIN                                              166
         AG_RMAX = FSTG_RMAX                                              166
         AG_DZ = DEPTH/2.0                                                166
         AG_TITLE = 'TUBE'                                                166
         AG_EXNAME = 'SHAPE'                                              166
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      166
         CALL AXSHAPE                                                     166
C    *                                                                    166
*     Place the 4 disks in the mother volume
C    Create FDMO :                                                        169
C    Call To Local Procedure FDMO                                         169
         ASSIGN 5040 TO BLOCK_FDMO                                        169
         GO TO  5030                                                      169
5040     CONTINUE                                                         169
C    *                                                                    169
C    *                                                                    170
         NL=1                                                             170
            GO TO 5053                                                    170
5051        NL=NL+1                                                       170
5053        IF(NL-(4).GT.0) GO TO 5052                                    170
C       Geant POSITION : FDMO ===> Z=-DEPTH/2.0+FSTG_WEDGETHK/2.0+FSTG_Z  171
C       (NL) <===                                                         171
            CALL AGSRESET                                                 171
            AG_Z = -DEPTH/2.0+FSTG_WEDGETHK/2.0+FSTG_Z(NL)                171
            AG_TITLE = 'FDMO'                                             171
            AG_EXNAME = 'POSITION'                                        171
            AG_PARLIST = 'Z'                                              171
            CALL AXPOSITION                                               171
C       *                                                                 171
         GO TO 5051                                                       172
5052     CONTINUE                                                         172
5011  CALL AGSPOPB                                                        174
      GO TO BLOCK_FSMO, (5010 ,5020)                                      174
C End of Local Procedure                                                  174
* -----------------------------------------------------------------------------
C Begin Local Procedure FDMO                                              177
C FDMO IS THE MOTHER OF AN INDIVIDUAL TWO-LAYER DISK ASSEMBLY (WAFERS AN  177
C D COOLING)                                                              177
5030  CONTINUE                                                            177
         AG_TITLE='FDMO'                                                  177
         AG_MODULE=MODULE                                                 177
         AG_MARK='V00'                                                    177
         CALL AGSPUSH                                                     177
         AG_MODULE=MODULE                                                 177
C    *                                                                    177
C    Geant MATERIAL : AIR ===> inherited arguments used <===              178
         CALL AGSRESET                                                    178
         AG_TITLE = 'AIR'                                                 178
         AG_EXNAME = 'MATERIAL'                                           178
         AG_PARLIST = 'NONE'                                              178
         CALL AXMATERIAL                                                  178
C    *                                                                    178
C    Geant ATTRIBUTE : FDMO ===> SEEN=0 COLO=6 <===                       178
         CALL AGSRESET                                                    179
         AG_SEEN = 0                                                      179
         AG_COLO = 6                                                      179
         AG_TITLE = 'FDMO'                                                179
         AG_EXNAME = 'ATTRIBUTE'                                          179
         AG_PARLIST = 'SEEN_COLO'                                         179
         CALL AXATTRIBUTE                                                 179
C    *                                                                    179
C    Geant SHAPE : TUBE ===> RMIN=FSTG_RMIN RMAX=FSTG_RMAX DZ=FSTG_WEDGE  183
C    THK/2.0 <===                                                         183
         CALL AGSRESET                                                    183
         AG_RMIN = FSTG_RMIN                                              183
         AG_RMAX = FSTG_RMAX                                              183
         AG_DZ = FSTG_WEDGETHK/2.0                                        183
         AG_TITLE = 'TUBE'                                                183
         AG_EXNAME = 'SHAPE'                                              183
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      183
         CALL AXSHAPE                                                     183
C    *                                                                    183
C    Create FDMS :                                                        185
C    Call To Local Procedure FDMS                                         185
         ASSIGN 5070 TO BLOCK_FDMS                                        185
         GO TO  5060                                                      185
5070     CONTINUE                                                         185
C    *                                                                    185
5031  CALL AGSPOPB                                                        186
      GO TO BLOCK_FDMO, (5030 ,5040)                                      186
C End of Local Procedure                                                  186
*
* -----------------------------------------------------------------------------
C Begin Local Procedure FDMS                                              190
C FDMS IS A DIVISION WITHIN AN INDIVIDUAL DISK                            190
5060  CONTINUE                                                            190
         AG_TITLE='FDMS'                                                  190
         AG_MODULE=MODULE                                                 190
         AG_MARK='V00'                                                    190
         CALL AGSPUSH                                                     190
         AG_MODULE=MODULE                                                 190
C    *                                                                    190
C    Geant SHAPE : DIVISION ===> IAXIS=2 NDIV=21 C0=START <===            192
         CALL AGSRESET                                                    192
         AG_IAXIS = 2                                                     192
         AG_NDIV = 21                                                     192
         AG_C0 = START                                                    192
         AG_TITLE = 'DIVISION'                                            192
         AG_EXNAME = 'SHAPE'                                              192
         AG_PARLIST = 'IAXIS_NDIV_C0'                                     192
         CALL AXSHAPE                                                     192
C    *                                                                    192
* -------------------------------------
* sensor assembly mother
C    Create FDMW :                                                        196
C    Call To Local Procedure FDMW                                         196
         ASSIGN 5090 TO BLOCK_FDMW                                        196
         GO TO  5080                                                      196
5090     CONTINUE                                                         196
C    *                                                                    196
C    Geant POSITION : FDMW ===> X=WEDGEOFFSET Y=0.0 Z=0.0 ORT=YZX <===    200
         CALL AGSRESET                                                    200
         AG_X = WEDGEOFFSET                                               200
         AG_Y = 0.0                                                       200
         AG_Z = 0.0                                                       200
         AG_ALPHAN=AG_ALPHAN+1                                            200
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 200
         AG_TITLE = 'FDMW'                                                200
         AG_EXNAME = 'POSITION'                                           200
         AG_PARLIST = 'X_Y_Z_ORT'                                         200
         CALL AXPOSITION                                                  200
C    *                                                                    200
* -------------------------------------
* Water manifold (duct)
C    Create FDWD :                                                        204
C    Call To Local Procedure FDWD                                         204
         ASSIGN 5110 TO BLOCK_FDWD                                        204
         GO TO  5100                                                      204
5110     CONTINUE                                                         204
C    *                                                                    204
C    Geant POSITION : FDWD ===> X=FDWG_RMIN+0.5*(FDWG_RMAX-FDWG_RMIN) Y=  209
C    0 Z=-0.5*(FDWG_THK+FSAN_THK) ORT=YZX <===                            209
         CALL AGSRESET                                                    209
         AG_X = FDWG_RMIN+0.5*(FDWG_RMAX-FDWG_RMIN)                       209
         AG_Y = 0                                                         209
         AG_Z = -0.5*(FDWG_THK+FSAN_THK)                                  209
         AG_ALPHAN=AG_ALPHAN+1                                            209
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 209
         AG_TITLE = 'FDWD'                                                209
         AG_EXNAME = 'POSITION'                                           209
         AG_PARLIST = 'X_Y_Z_ORT'                                         209
         CALL AXPOSITION                                                  209
C    *                                                                    209
C    Geant POSITION : FDWD ===> X=FDWG_RMIN+0.5*(FDWG_RMAX-FDWG_RMIN) Y=  209
C    0 Z=+0.5*(FDWG_THK+FSAN_THK) ORT=YZX <===                            209
         CALL AGSRESET                                                    213
         AG_X = FDWG_RMIN+0.5*(FDWG_RMAX-FDWG_RMIN)                       213
         AG_Y = 0                                                         213
         AG_Z = +0.5*(FDWG_THK+FSAN_THK)                                  213
         AG_ALPHAN=AG_ALPHAN+1                                            213
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 213
         AG_TITLE = 'FDWD'                                                213
         AG_EXNAME = 'POSITION'                                           213
         AG_PARLIST = 'X_Y_Z_ORT'                                         213
         CALL AXPOSITION                                                  213
C    *                                                                    213
5061  CALL AGSPOPB                                                        215
      GO TO BLOCK_FDMS, (5060 ,5070)                                      215
C End of Local Procedure                                                  215
* -----------------------------------------------------------------------------
C Begin Local Procedure FDMW                                              217
C FDMW IS THE MOTHER WEDGE, HOUSING PLATE, SENSOR AND CHIPS               217
5080  CONTINUE                                                            217
         AG_TITLE='FDMW'                                                  217
         AG_MODULE=MODULE                                                 217
         AG_MARK='V00'                                                    217
         CALL AGSPUSH                                                     217
         AG_MODULE=MODULE                                                 217
C    *                                                                    217
C    Geant ATTRIBUTE : FDMW ===> SEEN=0 COLO=4 <===                       218
         CALL AGSRESET                                                    218
         AG_SEEN = 0                                                      218
         AG_COLO = 4                                                      218
         AG_TITLE = 'FDMW'                                                218
         AG_EXNAME = 'ATTRIBUTE'                                          218
         AG_PARLIST = 'SEEN_COLO'                                         218
         CALL AXATTRIBUTE                                                 218
C    *                                                                    218
C    Geant SHAPE : TRD1 ===> DX1=WEDGEDX1 DX2=WEDGEDX2 DY=FSTG_SENSATHK/  223
C    2.0 DZ=WEDGELENGTH/2.0 <===                                          223
         CALL AGSRESET                                                    223
         AG_DX1 = WEDGEDX1                                                223
         AG_DX2 = WEDGEDX2                                                223
         AG_DY = FSTG_SENSATHK/2.0                                        223
         AG_DZ = WEDGELENGTH/2.0                                          223
         AG_TITLE = 'TRD1'                                                223
         AG_EXNAME = 'SHAPE'                                              223
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     223
         CALL AXSHAPE                                                     223
C    *                                                                    223
* -------------------------------------
* The Silicon Wafer
C    Create FDSW :                                                        227
C    Call To Local Procedure FDSW                                         227
         ASSIGN 5130 TO BLOCK_FDSW                                        227
         GO TO  5120                                                      227
5130     CONTINUE                                                         227
C    *                                                                    227
C    Geant POSITION : FDSW ===> X=0.0 Y=-FSTG_SPACING/2.0-FSTG_SENSORTHK  230
C    /2.0 Z=SENSOROFFSET <===                                             230
         CALL AGSRESET                                                    230
         AG_X = 0.0                                                       230
         AG_Y = -FSTG_SPACING/2.0-FSTG_SENSORTHK/2.0                      230
         AG_Z = SENSOROFFSET                                              230
         AG_TITLE = 'FDSW'                                                230
         AG_EXNAME = 'POSITION'                                           230
         AG_PARLIST = 'X_Y_Z'                                             230
         CALL AXPOSITION                                                  230
C    *                                                                    230
C    Geant POSITION : FDSW ===> X=0.0 Y=+FSTG_SPACING/2.0+FSTG_SENSORTHK  230
C    /2.0 Z=SENSOROFFSET ALPHAZ=180 <===                                  230
         CALL AGSRESET                                                    233
         AG_X = 0.0                                                       233
         AG_Y = +FSTG_SPACING/2.0+FSTG_SENSORTHK/2.0                      233
         AG_Z = SENSOROFFSET                                              233
         AG_ALPHAN=AG_ALPHAN+1                                            233
         AG_ALPHAK(AG_ALPHAN)=3                                           233
         AG_ALPHAL(AG_ALPHAN)=180                                         233
         AG_TITLE = 'FDSW'                                                233
         AG_EXNAME = 'POSITION'                                           233
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      233
         CALL AXPOSITION                                                  233
C    *                                                                    233
* -------------------------------------
* ALN thermal plate
C    use FSAN bank                                                        237
         BPATH_FSAN='FSAN*'                                               237
         IDNUM_FSAN(1)=0                                                  237
         FSAN_VERSION=1                                                   237
C    THE CENTER PLATE                                                     237
         CALL AGDGETP('FSTDGEO','*',-999,'FSAN',NFSAN_VERSION,            237
     *   RE_FSAN_VERSION,BPATH_FSAN,IDNUM_FSAN,1,MAP_FSAN,NAMES_FSAN,     237
     *   BANK_FSAN,LEN_FSAN,LLEN_FSAN,LINK_FSAN,FLAG_FSAN)                237
         PLATEDX1 = FSTG_RMAXS*TANHALFANGLE                               238
         PLATEDX2 = (FSTG_RMAXS+FSAN_LENGTH)*TANHALFANGLE                 239
C    Create FDTP :                                                        241
C    Call To Local Procedure FDTP                                         241
         ASSIGN 5150 TO BLOCK_FDTP                                        241
         GO TO  5140                                                      241
5150     CONTINUE                                                         241
C    *                                                                    241
C    WEDGELENGTH = SENSORLENGTH+FSAN_LENGTH                               242
         PLATEOFFSET = 0.5*(SENSORLENGTH)                                 242
C    Geant POSITION : FDTP ===> Z=PLATEOFFSET <===                        243
         CALL AGSRESET                                                    243
         AG_Z = PLATEOFFSET                                               243
         AG_TITLE = 'FDTP'                                                243
         AG_EXNAME = 'POSITION'                                           243
         AG_PARLIST = 'Z'                                                 243
         CALL AXPOSITION                                                  243
C    *                                                                    243
C    Create FDSC :                                                        245
C    Call To Local Procedure FDSC                                         245
         ASSIGN 5170 TO BLOCK_FDSC                                        245
         GO TO  5160                                                      245
5170     CONTINUE                                                         245
C    *                                                                    245
C    Geant POSITION : FDSC ===> X=0.0 Y=+0.5*FSCG_THK+1.5*FSAN_THK Z=(0.  248
C    5*WEDGELENGTH - FSAN_LENGTH+FSCG_H/2.0) <===                         248
         CALL AGSRESET                                                    248
         AG_X = 0.0                                                       248
         AG_Y = +0.5*FSCG_THK+1.5*FSAN_THK                                248
         AG_Z = (0.5*WEDGELENGTH - FSAN_LENGTH+FSCG_H/2.0)                248
         AG_TITLE = 'FDSC'                                                248
         AG_EXNAME = 'POSITION'                                           248
         AG_PARLIST = 'X_Y_Z'                                             248
         CALL AXPOSITION                                                  248
C    *                                                                    248
C    Geant POSITION : FDSC ===> X=0.0 Y=-0.5*FSCG_THK-1.5*FSAN_THK Z=(0.  248
C    5*WEDGELENGTH - FSAN_LENGTH+FSCG_H/2.0) <===                         248
         CALL AGSRESET                                                    251
         AG_X = 0.0                                                       251
         AG_Y = -0.5*FSCG_THK-1.5*FSAN_THK                                251
         AG_Z = (0.5*WEDGELENGTH - FSAN_LENGTH+FSCG_H/2.0)                251
         AG_TITLE = 'FDSC'                                                251
         AG_EXNAME = 'POSITION'                                           251
         AG_PARLIST = 'X_Y_Z'                                             251
         CALL AXPOSITION                                                  251
C    *                                                                    251
* -------------------------------------
* ALN substrate
C    use FSAN bank                                                        255
         BPATH_FSAN='FSAN*'                                               255
         IDNUM_FSAN(1)=0                                                  255
         FSAN_VERSION=2                                                   255
C    THE SUBSTRATE PLATE                                                  255
         CALL AGDGETP('FSTDGEO','*',-999,'FSAN',NFSAN_VERSION,            255
     *   RE_FSAN_VERSION,BPATH_FSAN,IDNUM_FSAN,1,MAP_FSAN,NAMES_FSAN,     255
     *   BANK_FSAN,LEN_FSAN,LLEN_FSAN,LINK_FSAN,FLAG_FSAN)                255
         PLATEDX1 = FSTG_RMAXS*TANHALFANGLE                               256
         PLATEDX2 = (FSTG_RMAXS+FSAN_LENGTH)*TANHALFANGLE                 257
C    Create FDTP :                                                        259
C    Call To Local Procedure FDTP                                         259
         ASSIGN 5180 TO BLOCK_FDTP                                        259
         GO TO  5140                                                      259
5180     CONTINUE                                                         259
C    *                                                                    259
         OFFHACK=0.5                                                      260
C    ENCAPSULATED HACK                                                    261
         PLATEOFFSET = PLATEOFFSET-OFFHACK                                261
C    Geant POSITION : FDTP ===> Y=+FSAN_THK Z=PLATEOFFSET <===            262
         CALL AGSRESET                                                    262
         AG_Y = +FSAN_THK                                                 262
         AG_Z = PLATEOFFSET                                               262
         AG_TITLE = 'FDTP'                                                262
         AG_EXNAME = 'POSITION'                                           262
         AG_PARLIST = 'Y_Z'                                               262
         CALL AXPOSITION                                                  262
C    *                                                                    262
C    Geant POSITION : FDTP ===> Y=-FSAN_THK Z=PLATEOFFSET <===            262
         CALL AGSRESET                                                    263
         AG_Y = -FSAN_THK                                                 263
         AG_Z = PLATEOFFSET                                               263
         AG_TITLE = 'FDTP'                                                263
         AG_EXNAME = 'POSITION'                                           263
         AG_PARLIST = 'Y_Z'                                               263
         CALL AXPOSITION                                                  263
C    *                                                                    263
5081  CALL AGSPOPB                                                        265
      GO TO BLOCK_FDMW, (5080 ,5090)                                      265
C End of Local Procedure                                                  265
* -----------------------------------------------------------------------------
C Begin Local Procedure FDSW                                              267
C FDSW IS THE SILICON WAFER (ALL ACTIVE)                                  267
5120  CONTINUE                                                            267
         AG_TITLE='FDSW'                                                  267
         AG_MODULE=MODULE                                                 267
         AG_MARK='V00'                                                    267
         CALL AGSPUSH                                                     267
         AG_MODULE=MODULE                                                 267
C    *                                                                    267
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          268
         CALL AGSRESET                                                    268
         AG_TITLE = 'SILICON'                                             268
         AG_EXNAME = 'MATERIAL'                                           268
         AG_PARLIST = 'NONE'                                              268
         CALL AXMATERIAL                                                  268
C    *                                                                    268
C    Geant MATERIAL : SENSITIVE ===> ISVOL=1 <===                         268
         CALL AGSRESET                                                    269
         AG_ISVOL = 1                                                     269
         AG_TITLE = 'SENSITIVE'                                           269
         AG_EXNAME = 'MATERIAL'                                           269
         AG_PARLIST = 'ISVOL'                                             269
         CALL AXMATERIAL                                                  269
C    *                                                                    269
C    Geant ATTRIBUTE : FDSW ===> SEEN=1 COLO=4 <===                       271
         CALL AGSRESET                                                    271
         AG_SEEN = 1                                                      271
         AG_COLO = 4                                                      271
         AG_TITLE = 'FDSW'                                                271
         AG_EXNAME = 'ATTRIBUTE'                                          271
         AG_PARLIST = 'SEEN_COLO'                                         271
         CALL AXATTRIBUTE                                                 271
C    *                                                                    271
C    Geant SHAPE : TRD1 ===> DX1=SENSORDX1 DX2=SENSORDX2 DY=FSTG_SENSORT  273
C    HK/2.0 DZ=SENSORLENGTH/2.0 <===                                      273
         CALL AGSRESET                                                    273
         AG_DX1 = SENSORDX1                                               273
         AG_DX2 = SENSORDX2                                               273
         AG_DY = FSTG_SENSORTHK/2.0                                       273
         AG_DZ = SENSORLENGTH/2.0                                         273
         AG_TITLE = 'TRD1'                                                273
         AG_EXNAME = 'SHAPE'                                              273
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     273
         CALL AXSHAPE                                                     273
C    *                                                                    273
C    Defining Hits: Z:.001:S Y:.001: X:.001: PTOT:16:(0,100), CX:10: CY:  277
C    10: CZ:10: SLENG:16:(0,500), TOF:16:(0,1.E-6) STEP:.01: ELOSS:16:(0  277
C    ,0.001)                                                              277
         AG_TITLE = 'FDSW'                                                277
         AG_EXNAME = 'HITS'                                               277
         AG_TYPE=AG_MAGIC                                                 277
         CALL AGSDETV('FDSW')                                             277
C    *                                                                    277
         AG_PARLIST='Z'                                                   277
         AG_TITLE = 'DETH'                                                277
         AG_NBITS = AG_MAGIC                                              277
         AG_BIN=.001                                                      277
         AG_OPTION='S'                                                    277
         AG_FHMIN = AG_MAGIC                                              277
         AG_FHMAX = AG_MAGIC                                              277
         CALL AGSDETH (0)                                                 277
C    *                                                                    277
         AG_PARLIST='Y'                                                   277
         AG_TITLE = 'DETH'                                                277
         AG_NBITS = AG_MAGIC                                              277
         AG_BIN=.001                                                      277
         AG_FHMIN=AG_MAGIC                                                277
         AG_FHMAX=AG_MAGIC                                                277
         AG_OPTION='*'                                                    277
         CALL AGSDETH (0)                                                 277
C    *                                                                    277
         AG_PARLIST='X'                                                   277
         AG_TITLE = 'DETH'                                                277
         AG_NBITS = AG_MAGIC                                              277
         AG_BIN=.001                                                      277
         AG_FHMIN=AG_MAGIC                                                277
         AG_FHMAX=AG_MAGIC                                                277
         AG_OPTION='*'                                                    277
         CALL AGSDETH (0)                                                 277
C    *                                                                    277
         AG_PARLIST='PTOT'                                                277
         AG_TITLE = 'DETH'                                                277
         AG_NBITS = 16                                                    277
         AG_BIN=AG_MAGIC                                                  277
         AG_OPTION='*'                                                    277
         AG_FHMIN = 0                                                     277
         AG_FHMAX = 100                                                   277
         CALL AGSDETH (0)                                                 277
C    *                                                                    277
         AG_PARLIST='CX'                                                  277
         AG_TITLE = 'DETH'                                                277
         AG_NBITS = 10                                                    277
         AG_BIN=AG_MAGIC                                                  277
         AG_FHMIN=AG_MAGIC                                                277
         AG_FHMAX=AG_MAGIC                                                277
         AG_OPTION='*'                                                    277
         CALL AGSDETH (0)                                                 277
C    *                                                                    277
         AG_PARLIST='CY'                                                  277
         AG_TITLE = 'DETH'                                                277
         AG_NBITS = 10                                                    277
         AG_BIN=AG_MAGIC                                                  277
         AG_FHMIN=AG_MAGIC                                                277
         AG_FHMAX=AG_MAGIC                                                277
         AG_OPTION='*'                                                    277
         CALL AGSDETH (0)                                                 277
C    *                                                                    277
         AG_PARLIST='CZ'                                                  277
         AG_TITLE = 'DETH'                                                277
         AG_NBITS = 10                                                    277
         AG_BIN=AG_MAGIC                                                  277
         AG_FHMIN=AG_MAGIC                                                277
         AG_FHMAX=AG_MAGIC                                                277
         AG_OPTION='*'                                                    277
         CALL AGSDETH (0)                                                 277
C    *                                                                    277
         AG_PARLIST='SLENG'                                               277
         AG_TITLE = 'DETH'                                                277
         AG_NBITS = 16                                                    277
         AG_BIN=AG_MAGIC                                                  277
         AG_OPTION='*'                                                    277
         AG_FHMIN = 0                                                     277
         AG_FHMAX = 500                                                   277
         CALL AGSDETH (0)                                                 277
C    *                                                                    277
         AG_PARLIST='TOF'                                                 277
         AG_TITLE = 'DETH'                                                277
         AG_NBITS = 16                                                    277
         AG_BIN=AG_MAGIC                                                  277
         AG_OPTION='*'                                                    277
         AG_FHMIN = 0                                                     277
         AG_FHMAX = 1.E-6                                                 277
         CALL AGSDETH (0)                                                 277
C    *                                                                    277
         AG_PARLIST='STEP'                                                277
         AG_TITLE = 'DETH'                                                277
         AG_NBITS = AG_MAGIC                                              277
         AG_BIN=.01                                                       277
         AG_FHMIN=AG_MAGIC                                                277
         AG_FHMAX=AG_MAGIC                                                277
         AG_OPTION='*'                                                    277
         CALL AGSDETH (0)                                                 277
C    *                                                                    277
         AG_PARLIST='ELOSS'                                               277
         AG_TITLE = 'DETH'                                                277
         AG_NBITS = 16                                                    277
         AG_BIN=AG_MAGIC                                                  277
         AG_OPTION='*'                                                    277
         AG_FHMIN = 0                                                     277
         AG_FHMAX = 0.001                                                 277
         CALL AGSDETH (0)                                                 277
C    *                                                                    277
         CALL AGSDETU('FDSW')                                             277
C    *                                                                    277
* I'll leave this for reference, but we no longer have
* passive and active layers in this sensor. It's all active
*      Create and Position FDSA y=-(FSTG_SensorThk-FSTG_ActiveThk)/2.0
*      Create and Position FDSP y=+(FSTG_SensorThk-FSTG_PassiveThk)/2.0
5121  CALL AGSPOPB                                                        285
      GO TO BLOCK_FDSW, (5120 ,5130)                                      285
C End of Local Procedure                                                  285
* -----------------------------------------------------------------------------
C Begin Local Procedure FDTP                                              287
C FDTP IS THE ALN THERMAL PLATE                                           287
5140  CONTINUE                                                            287
         AG_TITLE='FDTP'                                                  287
         AG_MODULE=MODULE                                                 287
         AG_MARK='V00'                                                    287
         CALL AGSPUSH                                                     287
         AG_MODULE=MODULE                                                 287
C    *                                                                    287
C    Geant ATTRIBUTE : FDTP ===> SEEN=1 COLO=6 <===                       288
         CALL AGSRESET                                                    288
         AG_SEEN = 1                                                      288
         AG_COLO = 6                                                      288
         AG_TITLE = 'FDTP'                                                288
         AG_EXNAME = 'ATTRIBUTE'                                          288
         AG_PARLIST = 'SEEN_COLO'                                         288
         CALL AXATTRIBUTE                                                 288
C    *                                                                    288
C    Geant COMPONENT : AL ===> A=27 Z=13 W=1 <===                         290
         CALL AGSRESET                                                    290
         AG_A = 27                                                        290
         AG_Z = 13                                                        290
         AG_W = 1                                                         290
         AG_TITLE = 'AL'                                                  290
         AG_EXNAME = 'COMPONENT'                                          290
         AG_PARLIST = 'A_Z_W'                                             290
         CALL AXCOMPONENT                                                 290
C    *                                                                    290
C    Geant COMPONENT : N ===> A=14 Z=7 W=1 <===                           290
         CALL AGSRESET                                                    291
         AG_A = 14                                                        291
         AG_Z = 7                                                         291
         AG_W = 1                                                         291
         AG_TITLE = 'N'                                                   291
         AG_EXNAME = 'COMPONENT'                                          291
         AG_PARLIST = 'A_Z_W'                                             291
         CALL AXCOMPONENT                                                 291
C    *                                                                    291
C    Geant MIXTURE : ALN ===> DENS=3.30 <===                              291
         CALL AGSRESET                                                    292
         AG_DENS = 3.30                                                   292
         AG_TITLE = 'ALN'                                                 292
         AG_EXNAME = 'MIXTURE'                                            292
         AG_PARLIST = 'DENS'                                              292
         CALL AXMIXTURE                                                   292
C    *                                                                    292
C    Geant SHAPE : TRD1 ===> DX1=PLATEDX1 DX2=PLATEDX2 DY=FSAN_THK/2.0 D  297
C    Z=FSAN_LENGTH/2.0 <===                                               297
         CALL AGSRESET                                                    297
         AG_DX1 = PLATEDX1                                                297
         AG_DX2 = PLATEDX2                                                297
         AG_DY = FSAN_THK/2.0                                             297
         AG_DZ = FSAN_LENGTH/2.0                                          297
         AG_TITLE = 'TRD1'                                                297
         AG_EXNAME = 'SHAPE'                                              297
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     297
         CALL AXSHAPE                                                     297
C    *                                                                    297
5141  CALL AGSPOPB                                                        299
      GO TO BLOCK_FDTP, (5140 ,5150,5180)                                 299
C End of Local Procedure                                                  299
* -----------------------------------------------------------------------------
C Begin Local Procedure FDSC                                              301
C FDSC IS THE READOUT CHIP                                                301
5160  CONTINUE                                                            301
         AG_TITLE='FDSC'                                                  301
         AG_MODULE=MODULE                                                 301
         AG_MARK='V00'                                                    301
         CALL AGSPUSH                                                     301
         AG_MODULE=MODULE                                                 301
C    *                                                                    301
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          302
         CALL AGSRESET                                                    302
         AG_TITLE = 'SILICON'                                             302
         AG_EXNAME = 'MATERIAL'                                           302
         AG_PARLIST = 'NONE'                                              302
         CALL AXMATERIAL                                                  302
C    *                                                                    302
C    Geant ATTRIBUTE : FDSC ===> SEEN=1 COLO=1 <===                       302
         CALL AGSRESET                                                    303
         AG_SEEN = 1                                                      303
         AG_COLO = 1                                                      303
         AG_TITLE = 'FDSC'                                                303
         AG_EXNAME = 'ATTRIBUTE'                                          303
         AG_PARLIST = 'SEEN_COLO'                                         303
         CALL AXATTRIBUTE                                                 303
C    *                                                                    303
C    Geant SHAPE : BOX ===> DX=FSCG_W/2.0 DY=FSCG_THK/2.0 DZ=FSCG_H/2.0   307
C    <===                                                                 307
         CALL AGSRESET                                                    307
         AG_DX = FSCG_W/2.0                                               307
         AG_DY = FSCG_THK/2.0                                             307
         AG_DZ = FSCG_H/2.0                                               307
         AG_TITLE = 'BOX'                                                 307
         AG_EXNAME = 'SHAPE'                                              307
         AG_PARLIST = 'DX_DY_DZ'                                          307
         CALL AXSHAPE                                                     307
C    *                                                                    307
5161  CALL AGSPOPB                                                        309
      GO TO BLOCK_FDSC, (5160 ,5170)                                      309
C End of Local Procedure                                                  309
* -----------------------------------------------------------------------------
C Begin Local Procedure FDWD                                              311
C FDWD IS THE WATER DUCT MADE OF CARBON COMPOSITE                         311
5100  CONTINUE                                                            311
         AG_TITLE='FDWD'                                                  311
         AG_MODULE=MODULE                                                 311
         AG_MARK='V00'                                                    311
         CALL AGSPUSH                                                     311
         AG_MODULE=MODULE                                                 311
C    *                                                                    311
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           312
         CALL AGSRESET                                                    312
         AG_TITLE = 'CARBON'                                              312
         AG_EXNAME = 'MATERIAL'                                           312
         AG_PARLIST = 'NONE'                                              312
         CALL AXMATERIAL                                                  312
C    *                                                                    312
C    Geant ATTRIBUTE : FDWD ===> SEEN=1 COLO=1 <===                       312
         CALL AGSRESET                                                    313
         AG_SEEN = 1                                                      313
         AG_COLO = 1                                                      313
         AG_TITLE = 'FDWD'                                                313
         AG_EXNAME = 'ATTRIBUTE'                                          313
         AG_PARLIST = 'SEEN_COLO'                                         313
         CALL AXATTRIBUTE                                                 313
C    *                                                                    313
C    Geant SHAPE : BOX ===> DX=FDWG_LEN/2.0 DY=FDWG_THK/2.0 DZ=(FDWG_RMA  317
C    X-FDWG_RMIN)/2.0 <===                                                317
         CALL AGSRESET                                                    317
         AG_DX = FDWG_LEN/2.0                                             317
         AG_DY = FDWG_THK/2.0                                             317
         AG_DZ = (FDWG_RMAX-FDWG_RMIN)/2.0                                317
         AG_TITLE = 'BOX'                                                 317
         AG_EXNAME = 'SHAPE'                                              317
         AG_PARLIST = 'DX_DY_DZ'                                          317
         CALL AXSHAPE                                                     317
C    *                                                                    317
C    Create FDWW :                                                        318
C    Call To Local Procedure FDWW                                         318
         ASSIGN 5200 TO BLOCK_FDWW                                        318
         GO TO  5190                                                      318
5200     CONTINUE                                                         318
C    *                                                                    318
C    Geant POSITION : FDWW ===> inherited arguments used <===             318
         CALL AGSRESET                                                    318
         AG_TITLE = 'FDWW'                                                318
         AG_EXNAME = 'POSITION'                                           318
         AG_PARLIST = 'NONE'                                              318
         CALL AXPOSITION                                                  318
C    *                                                                    318
5101  CALL AGSPOPB                                                        320
      GO TO BLOCK_FDWD, (5100 ,5110)                                      320
C End of Local Procedure                                                  320
* -----------------------------------------------------------------------------
C Begin Local Procedure FDWW                                              322
C FDWW IS THE WATER INSIDE THE CARBON DUCT                                322
5190  CONTINUE                                                            322
         AG_TITLE='FDWW'                                                  322
         AG_MODULE=MODULE                                                 322
         AG_MARK='V00'                                                    322
         CALL AGSPUSH                                                     322
         AG_MODULE=MODULE                                                 322
C    *                                                                    322
*     Pellegrino:
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           324
         CALL AGSRESET                                                    324
         AG_A = 1                                                         324
         AG_Z = 1                                                         324
         AG_W = 2                                                         324
         AG_TITLE = 'H2'                                                  324
         AG_EXNAME = 'COMPONENT'                                          324
         AG_PARLIST = 'A_Z_W'                                             324
         CALL AXCOMPONENT                                                 324
C    *                                                                    324
C    Geant COMPONENT : O ===> A=16 Z=8 W=1 <===                           324
         CALL AGSRESET                                                    325
         AG_A = 16                                                        325
         AG_Z = 8                                                         325
         AG_W = 1                                                         325
         AG_TITLE = 'O'                                                   325
         AG_EXNAME = 'COMPONENT'                                          325
         AG_PARLIST = 'A_Z_W'                                             325
         CALL AXCOMPONENT                                                 325
C    *                                                                    325
C    Geant MIXTURE : WATER ===> DENS=1.0 <===                             325
         CALL AGSRESET                                                    326
         AG_DENS = 1.0                                                    326
         AG_TITLE = 'WATER'                                               326
         AG_EXNAME = 'MIXTURE'                                            326
         AG_PARLIST = 'DENS'                                              326
         CALL AXMIXTURE                                                   326
C    *                                                                    326
C    Geant ATTRIBUTE : FDWD ===> SEEN=1 COLO=3 <===                       328
         CALL AGSRESET                                                    328
         AG_SEEN = 1                                                      328
         AG_COLO = 3                                                      328
         AG_TITLE = 'FDWD'                                                328
         AG_EXNAME = 'ATTRIBUTE'                                          328
         AG_PARLIST = 'SEEN_COLO'                                         328
         CALL AXATTRIBUTE                                                 328
C    *                                                                    328
C    Geant SHAPE : BOX ===> DX=FDWG_LEN/2.0 DY=(FDWG_THK-2.0*FDWG_WALLTH  332
C    K)/2.0 DZ=(FDWG_RMAX-FDWG_RMIN-2.0*FDWG_WALLTHK)/2.0 <===            332
         CALL AGSRESET                                                    332
         AG_DX = FDWG_LEN/2.0                                             332
         AG_DY = (FDWG_THK-2.0*FDWG_WALLTHK)/2.0                          332
         AG_DZ = (FDWG_RMAX-FDWG_RMIN-2.0*FDWG_WALLTHK)/2.0               332
         AG_TITLE = 'BOX'                                                 332
         AG_EXNAME = 'SHAPE'                                              332
         AG_PARLIST = 'DX_DY_DZ'                                          332
         CALL AXSHAPE                                                     332
C    *                                                                    332
5191  CALL AGSPOPB                                                        334
      GO TO BLOCK_FDWW, (5190 ,5200)                                      334
C End of Local Procedure                                                  334
* -----------------------------------------------------------------------------
      END                                                                 338
