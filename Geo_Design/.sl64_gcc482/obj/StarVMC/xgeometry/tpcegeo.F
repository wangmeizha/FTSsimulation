C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
****************************************************************************
* $Id: tpcegeo.g,v 1.31 2011/02/28 16:47:18 jwebb Exp $
*
* $Log: tpcegeo.g,v $
* Revision 1.31  2011/02/28 16:47:18  jwebb
* Added "EndFILL" statements where required.
*
* Revision 1.30  2009/11/10 02:14:31  perev
* Where GSTPAR, set local material avoid bug in gphysi
*
* Revision 1.29  2005/07/14 22:06:43  potekhin
* An informative diagnostics about the TPC gas density added,
* prompted by a recent debugging session with Yuri
*
* Revision 1.28  2003/11/13 01:37:46  potekhin
* Added four missing lines of code
* for the gas density correction,
* a comment from Yuri and a diagnostic
* print.
*
* Revision 1.27  2003/11/13 00:56:40  potekhin
* Added the Log CVS tag
*
*
      SUBROUTINE TPCEGEO                                                  25
C TPCEGEO IS THE TPC SYSTEM IN GSTAR                                      25
      IMPLICIT NONE                                                       25
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       25
      DATA MODULE/'TPCEGEO'/, BANK_TITLE/'*'/                             25
      DATA MCOMMENT/'IS THE TPC SYSTEM IN GSTAR'/                         25
      LOGICAL FIRST                                                       25
      DATA FIRST/.TRUE./                                                  25
      SAVE FIRST                                                          25
      INTEGER JUMPAD                                                      25
      EXTERNAL JUMPAD                                                     25
      INTEGER OK                                                          25
      PARAMETER (OK=0)                                                    25
      INTEGER AG_I,AG_J                                                   25
      SAVE AG_I,AG_J                                                      25
C AUTHOR PAVEL NEVSKI (MEPHI)                                             25
      DATA AG_I/0/,AG_J/0/                                                26
      CHARACTER*40 AUTHOR /'PAVEL NEVSKI (MEPHI)'/                        26
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  26
C CREATED MARCH 16, 1996                                                  26
      CHARACTER*40 CREATED /'MARCH 16, 1996'/                             27
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  27
*  Original Version - Peter Jacobs (LBL), March 16, 1995.                   *
*  dimensions checked with the latest design with D.Fritz and H.Weiman      *
*  modification history                                                     *
*  PN  04/04/96: MWC sensetive volumes added into TPC sectors               *
*  WJL 04/15/97: changed no of pads in rows 10, 13, and 38                  *
*  WJL 05/07/97: changed divisioning of TMSE to work with new /sim/mws      *
*  PN  05/29/97: interpolation inside padrow corrected                      *
*                Laser hits generated in a special subroutine TPCELASER     *
*  PN  06/28/97: gas in pseudo-padrows and MWC volumes corrected            *
*  PN  08/15/97: CSADDR,vect_inp/out not used anymore                       *
*  PN  08/21/97: anything denser than TPC gas absorbs laserino              *
*  PN  10/28/98: two more pseudo-padrows in the outer sector for Iwona      *
*  PN  11/10/99: full gap between gating grid and the padplane is sensitive *
*  PN  03/10/01: TMSE hit x-size determined automatically with MWCread>=2   *
*  PN  06/04/02: TPSM membrane material changed to mylar, copper was a bug  *
*****************************************************************************
C - Advanced Geant interface                                              44
      CHARACTER*20 AG_MODULE,AG_TITLE,AG_EXNAME,AG_PARLIST,AG_CHDIR,      44
     *AG_MATERIAL,AG_MIXTURE,AG_COMPONENT,AG_MEDIUM                       44
      CHARACTER*4 AG_VOLUME,AG_MOTHER,AG_SHAPE,AG_CNICK,AG_KONLY,         44
     *AG_OPTION,AG_ORT,AG_MARK                                            44
      INTEGER AG_BEGCOM,AG_IVOLUME,AG_IMOTHER,AG_IGNUM,AG_ISHAPE,         44
     *AG_IMED,AG_IMAT,AG_IFIELD, AG_NLMAT,AG_IERROR,AG_NWBUF,AG_NPAR,     44
     *AG_ISTATUS,AG_IROT,AG_NBITS,AG_SERIAL, AG_ATTRIBUTE(6),AG_WORK,     44
     *AG_SEEN,AG_LSTY,AG_LWID,AG_COLO,AG_FILL,AG_JDU,AG_ORTI, AG_NDIV,    44
     *AG_IAXIS,AG_NDVMAX,AG_IPRIN,AG_RESET1,AG_RESET2,AG_BEGSCR,          44
     *AG_ENDSCR, AG_IDTYPE,AG_ISET,AG_IDET,AG_ISVOL,AG_LEVEL,AG_IRESER,   44
     *AG_IGEOM,AG_STANDALONE, AG_LSTACK,AG_NWUHIT,AG_NWUVOL,AG_MAGIC,     44
     *AG_LDETU,AG_NPDIV,AG_NPDV,AG_NZ,AG_NCOPY, AG_IDEBU,AG_IGRAP,        44
     *AG_IHIST,AG_IMFLD,AG_ISIMU,AG_ENDSAVE,AG_CODE,AG_TRKTYP, AG_MODE,   44
     *AG_ECODE,AG_PDG,IPRIN, AG_ALPHAN,AG_ALPHAK(10),IGOR                 44
      REAL AG_FIELDM,AG_TMAXFD,AG_STEMAX,AG_DEEMAX,AG_EPSIL,AG_STMIN,     44
     *AG_DENS,AG_RADL,AG_ABSL, AG_THETAX,AG_PHIX,AG_THETAY,AG_PHIY,       44
     *AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,AG_ALFAZ, AG_ALPHAL(10),        44
     *AG_ALPHAX,AG_ALPHAY,AG_ALPHAZ,AG_TYPE, AG_DX, AG_DX1, AG_DX2,       44
     *AG_DY, AG_DY1, AG_DY2, AG_DMAXMS, AG_TWIST, AG_THET, AG_THE1,       44
     *AG_THE2, AG_PHI, AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2,       44
     *AG_RMIN, AG_RMAX, AG_RMN, AG_RMX, AG_ZI , AG_RMN1, AG_RMN2,         44
     *AG_RMX1, AG_RMX2, AG_H1, AG_H2, AG_BL1, AG_BL2, AG_TL1, AG_TL2,     44
     *AG_DPHI, AG_DZ, AG_TWIS, AG_X, AG_Y, AG_Z, AG_A, AG_ZA, AG_W,       44
     *AG_STEP, AG_C0, AG_PAR, AG_AA, AG_ZZ, AG_WW, AG_LX, AG_LY, AG_LZ,   44
     *AG_HX, AG_HY, AG_HZ, AG_P1, AG_P2, AG_STACK,AG_UBUF,AG_XHMAX,       44
     *AG_YHMAX,AG_ZHMAX,AG_RHMAX,AG_FHMAX,AG_FHMIN,AG_BIN, AG_CHARGE,     44
     *AG_MASS,AG_TLIFE,AG_BRATIO                                          44
      PARAMETER (AG_LSTACK=130,AG_NWUHIT=10,AG_NWUVOL=3,AG_MAGIC=         44
     *-696969,AG_LDETU=250)                                               44
      COMMON/AGCGLOB/ AG_MODULE,AG_CHDIR,AG_LEVEL,AG_IDTYPE,AG_IERROR,    44
     *AG_STANDALONE,IPRIN, AG_IPRIN,AG_IGEOM,AG_IDEBU,AG_IGRAP,AG_IHIST,  44
     *AG_IMFLD,AG_ISIMU                                                   44
C Inherited variables a: saved during internal calls                      44
C b: reset at the entry in a block and then saved                         44
      COMMON/AGCPARA/ AG_BEGCOM, AG_IVOLUME, AG_IMOTHER, AG_IGNUM,        44
     *AG_ISHAPE, AG_IMED, AG_IMAT, AG_IFIELD, AG_FIELDM, AG_TMAXFD,       44
     *AG_STEMAX, AG_DEEMAX, AG_EPSIL, AG_STMIN, AG_DENS, AG_RADL,         44
     *AG_ABSL, AG_DX, AG_DX1, AG_DX2, AG_DY, AG_DY1, AG_DY2, AG_RMN1,     44
     *AG_RMN2, AG_RMX1, AG_RMX2, AG_THET, AG_THE1, AG_THE2, AG_PHI,       44
     *AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2, AG_H1, AG_BL1,         44
     *AG_TL1, AG_H2, AG_BL2, AG_TL2, AG_RMIN, AG_RMAX, AG_DPHI,           44
     *AG_NPDIV, AG_NZ, AG_DZ, AG_TWIS, AG_LX, AG_LY, AG_LZ, AG_HX,        44
     *AG_HY, AG_HZ, AG_A, AG_ZA, AG_W, AG_NLMAT, AG_WORK, AG_SEEN,        44
     *AG_LSTY, AG_LWID, AG_COLO, AG_FILL, AG_SERIAL, AG_ISVOL,            44
     *AG_ISTATUS, AG_ZI(16), AG_RMN(16), AG_RMX(16), AG_VOLUME,           44
     *AG_MOTHER, AG_SHAPE, AG_CNICK, AG_ENDSAVE, AG_RESET1, AG_THETAX,    44
     *AG_PHIX,AG_THETAY,AG_PHIY,AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,      44
     *AG_ALFAZ, AG_X,AG_Y,AG_Z, AG_STEP,AG_C0,AG_NDIV,AG_IAXIS,           44
     *AG_NDVMAX,AG_ORTI,AG_NCOPY, AG_ALPHAN,AG_ALPHAK,AG_ALPHAL,          44
     *AG_RESET2, AG_KONLY, AG_ORT, AG_MARK                                44
C local variables valid inside same block                                 44
      COMMON/AGCLOCA/ AG_BEGSCR,AG_UBUF(100),AG_PAR(100),AG_AA(20),       44
     *AG_ZZ(20),AG_WW(20),AG_NWBUF, AG_XHMAX,AG_YHMAX,AG_ZHMAX,AG_RHMAX,  44
     *AG_FHMAX,AG_FHMIN,AG_NBITS,AG_BIN,AG_TYPE, AG_IROT,AG_NPAR,         44
     *AG_ISET,AG_IDET,AG_JDU,AG_IRESER, AG_ENDSCR, AG_TITLE,AG_EXNAME,    44
     *AG_PARLIST,AG_MATERIAL,AG_MIXTURE, AG_COMPONENT,AG_MEDIUM,          44
     *AG_OPTION                                                           44
      COMMON/AGCPART/AG_CODE,AG_TRKTYP,AG_MASS,AG_CHARGE,AG_TLIFE,        44
     *AG_BRATIO(6),AG_MODE(6),AG_PDG,AG_ECODE                             44
      COMMON/AGCSTAC/ AG_STACK(AG_LSTACK,15)                              44
C temporarly                                                              44
      EQUIVALENCE (AG_ATTRIBUTE,AG_WORK),(AG_STEMAX,AG_DMAXMS),           44
     *(AG_ALPHAX,AG_ALFAX), (AG_ALPHAY,AG_ALFAY),(AG_ALPHAZ,AG_ALFAZ),    44
     *(AG_TWIST,AG_TWIS),(AG_NPDIV,AG_NPDV),  (AG_P1,AG_HX),(AG_P2,       44
     *AG_HY)                                                              44
      REAL MIL_P,INCH_P                                                   44
      PARAMETER (INCH_P=2.54,MIL_P=0.00254)                               44
C - GEANT description of logical units                                    44
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            44
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      44
      COMMON/GCMAIL/ CHMAIL                                               44
      CHARACTER*132 CHMAIL                                                44
C - Natural constants (or considered as such)                             44
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   44
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        44
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       44
      INTEGER BLOCK_TPCE                                                  44
      INTEGER BLOCK_TOFC                                                  47
      INTEGER BLOCK_TOFS                                                  47
      INTEGER BLOCK_TOST                                                  47
      INTEGER BLOCK_TOKA                                                  47
      INTEGER BLOCK_TONX                                                  47
      INTEGER BLOCK_TOAD                                                  47
      INTEGER BLOCK_TOHA                                                  47
      INTEGER BLOCK_TPGV                                                  47
      INTEGER BLOCK_TPSS                                                  47
      INTEGER BLOCK_TIFC                                                  47
      INTEGER BLOCK_TIAL                                                  47
      INTEGER BLOCK_TIKA                                                  47
      INTEGER BLOCK_TINX                                                  47
      INTEGER BLOCK_TPCW                                                  47
      INTEGER BLOCK_TWSS                                                  47
      INTEGER BLOCK_TWGI                                                  47
      INTEGER BLOCK_TPCM                                                  47
      INTEGER BLOCK_TPEA                                                  47
      INTEGER BLOCK_TESS                                                  47
      INTEGER BLOCK_TSEC                                                  47
      INTEGER BLOCK_TMWC                                                  47
      INTEGER BLOCK_TMEA                                                  47
      INTEGER BLOCK_TMSE                                                  47
      INTEGER BLOCK_TIAG                                                  47
      INTEGER BLOCK_TOAE                                                  47
      INTEGER BLOCK_TPAD                                                  47
      INTEGER BLOCK_TPAI                                                  47
      INTEGER BLOCK_TPAO                                                  47
*
C -                                                                       52
      REAL TPCG_VERSION                                                   52
      EQUIVALENCE (BANK_TPCG(3),TPCG_VERSION,RE_TPCG_VERSION),            52
     *(NAMES_TPCG(1),NTPCG_VERSION)                                       52
      EQUIVALENCE (COMMEN_TPCG(1),CTPCG_VERSION),(MAP_TPCG(1,1),          52
     *MTPCG_VERSION)                                                      52
      CHARACTER CTPCG_VERSION*40/' '/                                     52
      CHARACTER NTPCG_VERSION*12/'R.VERSION'/                             52
      INTEGER MTPCG_VERSION(2)/1,1/                                       52
      REAL RE_TPCG_VERSION                                                52
C -                                                                       52
      REAL TPCG_RMIN                                                      52
      EQUIVALENCE (BANK_TPCG(4),TPCG_RMIN,RE_TPCG_RMIN),(NAMES_TPCG(2),   52
     *NTPCG_RMIN)                                                         52
      EQUIVALENCE (COMMEN_TPCG(2),CTPCG_RMIN),(MAP_TPCG(1,2),             52
     *MTPCG_RMIN)                                                         52
      CHARACTER CTPCG_RMIN*40/' '/                                        52
      CHARACTER NTPCG_RMIN*12/'R.RMIN'/                                   52
      INTEGER MTPCG_RMIN(2)/1,1/                                          52
      REAL RE_TPCG_RMIN                                                   52
C -                                                                       52
      REAL TPCG_RMAX                                                      52
      EQUIVALENCE (BANK_TPCG(5),TPCG_RMAX,RE_TPCG_RMAX),(NAMES_TPCG(3),   52
     *NTPCG_RMAX)                                                         52
      EQUIVALENCE (COMMEN_TPCG(3),CTPCG_RMAX),(MAP_TPCG(1,3),             52
     *MTPCG_RMAX)                                                         52
      CHARACTER CTPCG_RMAX*40/' '/                                        52
      CHARACTER NTPCG_RMAX*12/'R.RMAX'/                                   52
      INTEGER MTPCG_RMAX(2)/1,1/                                          52
      REAL RE_TPCG_RMAX                                                   52
C -                                                                       52
      REAL TPCG_LENGTH                                                    52
      EQUIVALENCE (BANK_TPCG(6),TPCG_LENGTH,RE_TPCG_LENGTH),              52
     *(NAMES_TPCG(4),NTPCG_LENGTH)                                        52
      EQUIVALENCE (COMMEN_TPCG(4),CTPCG_LENGTH),(MAP_TPCG(1,4),           52
     *MTPCG_LENGTH)                                                       52
      CHARACTER CTPCG_LENGTH*40/' '/                                      52
      CHARACTER NTPCG_LENGTH*12/'R.LENGTH'/                               52
      INTEGER MTPCG_LENGTH(2)/1,1/                                        52
      REAL RE_TPCG_LENGTH                                                 52
C -                                                                       52
      REAL TPCG_WHEELIR                                                   52
      EQUIVALENCE (BANK_TPCG(7),TPCG_WHEELIR,RE_TPCG_WHEELIR),            52
     *(NAMES_TPCG(5),NTPCG_WHEELIR)                                       52
      EQUIVALENCE (COMMEN_TPCG(5),CTPCG_WHEELIR),(MAP_TPCG(1,5),          52
     *MTPCG_WHEELIR)                                                      52
      CHARACTER CTPCG_WHEELIR*40/' '/                                     52
      CHARACTER NTPCG_WHEELIR*12/'R.WHEELIR'/                             52
      INTEGER MTPCG_WHEELIR(2)/1,1/                                       52
      REAL RE_TPCG_WHEELIR                                                52
C -                                                                       52
      REAL TPCG_WHEELOR                                                   52
      EQUIVALENCE (BANK_TPCG(8),TPCG_WHEELOR,RE_TPCG_WHEELOR),            52
     *(NAMES_TPCG(6),NTPCG_WHEELOR)                                       52
      EQUIVALENCE (COMMEN_TPCG(6),CTPCG_WHEELOR),(MAP_TPCG(1,6),          52
     *MTPCG_WHEELOR)                                                      52
      CHARACTER CTPCG_WHEELOR*40/' '/                                     52
      CHARACTER NTPCG_WHEELOR*12/'R.WHEELOR'/                             52
      INTEGER MTPCG_WHEELOR(2)/1,1/                                       52
      REAL RE_TPCG_WHEELOR                                                52
C -                                                                       52
      REAL TPCG_WHEELTHK                                                  52
      EQUIVALENCE (BANK_TPCG(9),TPCG_WHEELTHK,RE_TPCG_WHEELTHK),          52
     *(NAMES_TPCG(7),NTPCG_WHEELTHK)                                      52
      EQUIVALENCE (COMMEN_TPCG(7),CTPCG_WHEELTHK),(MAP_TPCG(1,7),         52
     *MTPCG_WHEELTHK)                                                     52
      CHARACTER CTPCG_WHEELTHK*40/' '/                                    52
      CHARACTER NTPCG_WHEELTHK*12/'R.WHEELTHK'/                           52
      INTEGER MTPCG_WHEELTHK(2)/1,1/                                      52
      REAL RE_TPCG_WHEELTHK                                               52
C -                                                                       52
      REAL TPCG_SENGASOR                                                  52
      EQUIVALENCE (BANK_TPCG(10),TPCG_SENGASOR,RE_TPCG_SENGASOR),         52
     *(NAMES_TPCG(8),NTPCG_SENGASOR)                                      52
      EQUIVALENCE (COMMEN_TPCG(8),CTPCG_SENGASOR),(MAP_TPCG(1,8),         52
     *MTPCG_SENGASOR)                                                     52
      CHARACTER CTPCG_SENGASOR*40/' '/                                    52
      CHARACTER NTPCG_SENGASOR*12/'R.SENGASOR'/                           52
      INTEGER MTPCG_SENGASOR(2)/1,1/                                      52
      REAL RE_TPCG_SENGASOR                                               52
C -                                                                       52
      REAL TPCG_TPEATHK                                                   52
      EQUIVALENCE (BANK_TPCG(11),TPCG_TPEATHK,RE_TPCG_TPEATHK),           52
     *(NAMES_TPCG(9),NTPCG_TPEATHK)                                       52
      EQUIVALENCE (COMMEN_TPCG(9),CTPCG_TPEATHK),(MAP_TPCG(1,9),          52
     *MTPCG_TPEATHK)                                                      52
      CHARACTER CTPCG_TPEATHK*40/' '/                                     52
      CHARACTER NTPCG_TPEATHK*12/'R.TPEATHK'/                             52
      INTEGER MTPCG_TPEATHK(2)/1,1/                                       52
      REAL RE_TPCG_TPEATHK                                                52
C -                                                                       52
      REAL TPCG_MEMBTHK                                                   52
      EQUIVALENCE (BANK_TPCG(12),TPCG_MEMBTHK,RE_TPCG_MEMBTHK),           52
     *(NAMES_TPCG(10),NTPCG_MEMBTHK)                                      52
      EQUIVALENCE (COMMEN_TPCG(10),CTPCG_MEMBTHK),(MAP_TPCG(1,10),        52
     *MTPCG_MEMBTHK)                                                      52
      CHARACTER CTPCG_MEMBTHK*40/' '/                                     52
      CHARACTER NTPCG_MEMBTHK*12/'R.MEMBTHK'/                             52
      INTEGER MTPCG_MEMBTHK(2)/1,1/                                       52
      REAL RE_TPCG_MEMBTHK                                                52
C -                                                                       52
      REAL TPCG_TIADDR                                                    52
      EQUIVALENCE (BANK_TPCG(13),TPCG_TIADDR,RE_TPCG_TIADDR),             52
     *(NAMES_TPCG(11),NTPCG_TIADDR)                                       52
      EQUIVALENCE (COMMEN_TPCG(11),CTPCG_TIADDR),(MAP_TPCG(1,11),         52
     *MTPCG_TIADDR)                                                       52
      CHARACTER CTPCG_TIADDR*40/' '/                                      52
      CHARACTER NTPCG_TIADDR*12/'R.TIADDR'/                               52
      INTEGER MTPCG_TIADDR(2)/1,1/                                        52
      REAL RE_TPCG_TIADDR                                                 52
C -                                                                       52
      REAL TPCG_TINXDR                                                    52
      EQUIVALENCE (BANK_TPCG(14),TPCG_TINXDR,RE_TPCG_TINXDR),             52
     *(NAMES_TPCG(12),NTPCG_TINXDR)                                       52
      EQUIVALENCE (COMMEN_TPCG(12),CTPCG_TINXDR),(MAP_TPCG(1,12),         52
     *MTPCG_TINXDR)                                                       52
      CHARACTER CTPCG_TINXDR*40/' '/                                      52
      CHARACTER NTPCG_TINXDR*12/'R.TINXDR'/                               52
      INTEGER MTPCG_TINXDR(2)/1,1/                                        52
      REAL RE_TPCG_TINXDR                                                 52
C -                                                                       52
      REAL TPCG_TIKADR                                                    52
      EQUIVALENCE (BANK_TPCG(15),TPCG_TIKADR,RE_TPCG_TIKADR),             52
     *(NAMES_TPCG(13),NTPCG_TIKADR)                                       52
      EQUIVALENCE (COMMEN_TPCG(13),CTPCG_TIKADR),(MAP_TPCG(1,13),         52
     *MTPCG_TIKADR)                                                       52
      CHARACTER CTPCG_TIKADR*40/' '/                                      52
      CHARACTER NTPCG_TIKADR*12/'R.TIKADR'/                               52
      INTEGER MTPCG_TIKADR(2)/1,1/                                        52
      REAL RE_TPCG_TIKADR                                                 52
C -                                                                       52
      REAL TPCG_TIALDR                                                    52
      EQUIVALENCE (BANK_TPCG(16),TPCG_TIALDR,RE_TPCG_TIALDR),             52
     *(NAMES_TPCG(14),NTPCG_TIALDR)                                       52
      EQUIVALENCE (COMMEN_TPCG(14),CTPCG_TIALDR),(MAP_TPCG(1,14),         52
     *MTPCG_TIALDR)                                                       52
      CHARACTER CTPCG_TIALDR*40/' '/                                      52
      CHARACTER NTPCG_TIALDR*12/'R.TIALDR'/                               52
      INTEGER MTPCG_TIALDR(2)/1,1/                                        52
      REAL RE_TPCG_TIALDR                                                 52
C -                                                                       52
      REAL TPCG_TOCSDR                                                    52
      EQUIVALENCE (BANK_TPCG(17),TPCG_TOCSDR,RE_TPCG_TOCSDR),             52
     *(NAMES_TPCG(15),NTPCG_TOCSDR)                                       52
      EQUIVALENCE (COMMEN_TPCG(15),CTPCG_TOCSDR),(MAP_TPCG(1,15),         52
     *MTPCG_TOCSDR)                                                       52
      CHARACTER CTPCG_TOCSDR*40/' '/                                      52
      CHARACTER NTPCG_TOCSDR*12/'R.TOCSDR'/                               52
      INTEGER MTPCG_TOCSDR(2)/1,1/                                        52
      REAL RE_TPCG_TOCSDR                                                 52
C -                                                                       52
      REAL TPCG_TOKADR                                                    52
      EQUIVALENCE (BANK_TPCG(18),TPCG_TOKADR,RE_TPCG_TOKADR),             52
     *(NAMES_TPCG(16),NTPCG_TOKADR)                                       52
      EQUIVALENCE (COMMEN_TPCG(16),CTPCG_TOKADR),(MAP_TPCG(1,16),         52
     *MTPCG_TOKADR)                                                       52
      CHARACTER CTPCG_TOKADR*40/' '/                                      52
      CHARACTER NTPCG_TOKADR*12/'R.TOKADR'/                               52
      INTEGER MTPCG_TOKADR(2)/1,1/                                        52
      REAL RE_TPCG_TOKADR                                                 52
C -                                                                       52
      REAL TPCG_TONXDR                                                    52
      EQUIVALENCE (BANK_TPCG(19),TPCG_TONXDR,RE_TPCG_TONXDR),             52
     *(NAMES_TPCG(17),NTPCG_TONXDR)                                       52
      EQUIVALENCE (COMMEN_TPCG(17),CTPCG_TONXDR),(MAP_TPCG(1,17),         52
     *MTPCG_TONXDR)                                                       52
      CHARACTER CTPCG_TONXDR*40/' '/                                      52
      CHARACTER NTPCG_TONXDR*12/'R.TONXDR'/                               52
      INTEGER MTPCG_TONXDR(2)/1,1/                                        52
      REAL RE_TPCG_TONXDR                                                 52
C -                                                                       52
      REAL TPCG_TOADDR                                                    52
      EQUIVALENCE (BANK_TPCG(20),TPCG_TOADDR,RE_TPCG_TOADDR),             52
     *(NAMES_TPCG(18),NTPCG_TOADDR)                                       52
      EQUIVALENCE (COMMEN_TPCG(18),CTPCG_TOADDR),(MAP_TPCG(1,18),         52
     *MTPCG_TOADDR)                                                       52
      CHARACTER CTPCG_TOADDR*40/' '/                                      52
      CHARACTER NTPCG_TOADDR*12/'R.TOADDR'/                               52
      INTEGER MTPCG_TOADDR(2)/1,1/                                        52
      REAL RE_TPCG_TOADDR                                                 52
C -                                                                       52
      REAL TPCG_TOIGDR                                                    52
      EQUIVALENCE (BANK_TPCG(21),TPCG_TOIGDR,RE_TPCG_TOIGDR),             52
     *(NAMES_TPCG(19),NTPCG_TOIGDR)                                       52
      EQUIVALENCE (COMMEN_TPCG(19),CTPCG_TOIGDR),(MAP_TPCG(1,19),         52
     *MTPCG_TOIGDR)                                                       52
      CHARACTER CTPCG_TOIGDR*40/' '/                                      52
      CHARACTER NTPCG_TOIGDR*12/'R.TOIGDR'/                               52
      INTEGER MTPCG_TOIGDR(2)/1,1/                                        52
      REAL RE_TPCG_TOIGDR                                                 52
C -                                                                       52
      REAL TPCG_TOALDR                                                    52
      EQUIVALENCE (BANK_TPCG(22),TPCG_TOALDR,RE_TPCG_TOALDR),             52
     *(NAMES_TPCG(20),NTPCG_TOALDR)                                       52
      EQUIVALENCE (COMMEN_TPCG(20),CTPCG_TOALDR),(MAP_TPCG(1,20),         52
     *MTPCG_TOALDR)                                                       52
      CHARACTER CTPCG_TOALDR*40/' '/                                      52
      CHARACTER NTPCG_TOALDR*12/'R.TOALDR'/                               52
      INTEGER MTPCG_TOALDR(2)/1,1/                                        52
      REAL RE_TPCG_TOALDR                                                 52
C -                                                                       52
      REAL TPCG_TOHADR                                                    52
      EQUIVALENCE (BANK_TPCG(23),TPCG_TOHADR,RE_TPCG_TOHADR),             52
     *(NAMES_TPCG(21),NTPCG_TOHADR)                                       52
      EQUIVALENCE (COMMEN_TPCG(21),CTPCG_TOHADR),(MAP_TPCG(1,21),         52
     *MTPCG_TOHADR)                                                       52
      CHARACTER CTPCG_TOHADR*40/' '/                                      52
      CHARACTER NTPCG_TOHADR*12/'R.TOHADR'/                               52
      INTEGER MTPCG_TOHADR(2)/1,1/                                        52
      REAL RE_TPCG_TOHADR                                                 52
C -                                                                       52
      REAL TPCG_MWCREAD                                                   52
      EQUIVALENCE (BANK_TPCG(24),TPCG_MWCREAD,RE_TPCG_MWCREAD),           52
     *(NAMES_TPCG(22),NTPCG_MWCREAD)                                      52
      EQUIVALENCE (COMMEN_TPCG(22),CTPCG_MWCREAD),(MAP_TPCG(1,22),        52
     *MTPCG_MWCREAD)                                                      52
      CHARACTER CTPCG_MWCREAD*40/' '/                                     52
      CHARACTER NTPCG_MWCREAD*12/'R.MWCREAD'/                             52
      INTEGER MTPCG_MWCREAD(2)/1,1/                                       52
      REAL RE_TPCG_MWCREAD                                                52
C -                                                                       52
      REAL TPCG_GASCORR                                                   52
      EQUIVALENCE (BANK_TPCG(25),TPCG_GASCORR,RE_TPCG_GASCORR),           52
     *(NAMES_TPCG(23),NTPCG_GASCORR)                                      52
      EQUIVALENCE (COMMEN_TPCG(23),CTPCG_GASCORR),(MAP_TPCG(1,23),        52
     *MTPCG_GASCORR)                                                      52
      CHARACTER CTPCG_GASCORR*40/' '/                                     52
      CHARACTER NTPCG_GASCORR*12/'R.GASCORR'/                             52
      INTEGER MTPCG_GASCORR(2)/1,1/                                       52
      REAL RE_TPCG_GASCORR                                                52
C -                                                                       52
      INTEGER LEN_TPCG,LLEN_TPCG,LINK_TPCG                                52
      PARAMETER (LEN_TPCG=25)                                             52
      PARAMETER (LLEN_TPCG=23)                                            52
      INTEGER FLAG_TPCG/0/,MAP_TPCG(2,LLEN_TPCG),IDNUM_TPCG(10)           52
      CHARACTER NAMES_TPCG(LLEN_TPCG)*12,COMMEN_TPCG(LLEN_TPCG)*40        52
      DATA IDNUM_TPCG/10*0/                                               52
      REAL BANK_TPCG(LEN_TPCG)                                            52
      CHARACTER*60 BPATH_TPCG/' '/                                        52
      SAVE FLAG_TPCG,MAP_TPCG,IDNUM_TPCG,NAMES_TPCG,COMMEN_TPCG,          52
     *BPATH_TPCG                                                          52
      DATA LINK_TPCG/0/,BANK_TPCG/LEN_TPCG*0/                             52
C *                                                                       52
*
C -                                                                       57
      REAL TECW_SEC                                                       57
      EQUIVALENCE (BANK_TECW(3),TECW_SEC,RE_TECW_SEC),(NAMES_TECW(1),     57
     *NTECW_SEC)                                                          57
      EQUIVALENCE (COMMEN_TECW(1),CTECW_SEC),(MAP_TECW(1,1),MTECW_SEC)    57
      CHARACTER CTECW_SEC*40/' '/                                         57
      CHARACTER NTECW_SEC*12/'R.SEC'/                                     57
      INTEGER MTECW_SEC(2)/1,1/                                           57
      REAL RE_TECW_SEC                                                    57
C -                                                                       57
      REAL TECW_GAPRAD                                                    57
      EQUIVALENCE (BANK_TECW(4),TECW_GAPRAD,RE_TECW_GAPRAD),              57
     *(NAMES_TECW(2),NTECW_GAPRAD)                                        57
      EQUIVALENCE (COMMEN_TECW(2),CTECW_GAPRAD),(MAP_TECW(1,2),           57
     *MTECW_GAPRAD)                                                       57
      CHARACTER CTECW_GAPRAD*40/' '/                                      57
      CHARACTER NTECW_GAPRAD*12/'R.GAPRAD'/                               57
      INTEGER MTECW_GAPRAD(2)/1,1/                                        57
      REAL RE_TECW_GAPRAD                                                 57
C -                                                                       57
      REAL TECW_GAPHEIT                                                   57
      EQUIVALENCE (BANK_TECW(5),TECW_GAPHEIT,RE_TECW_GAPHEIT),            57
     *(NAMES_TECW(3),NTECW_GAPHEIT)                                       57
      EQUIVALENCE (COMMEN_TECW(3),CTECW_GAPHEIT),(MAP_TECW(1,3),          57
     *MTECW_GAPHEIT)                                                      57
      CHARACTER CTECW_GAPHEIT*40/' '/                                     57
      CHARACTER NTECW_GAPHEIT*12/'R.GAPHEIT'/                             57
      INTEGER MTECW_GAPHEIT(2)/1,1/                                       57
      REAL RE_TECW_GAPHEIT                                                57
C -                                                                       57
      REAL TECW_GAPWIDI                                                   57
      EQUIVALENCE (BANK_TECW(6),TECW_GAPWIDI,RE_TECW_GAPWIDI),            57
     *(NAMES_TECW(4),NTECW_GAPWIDI)                                       57
      EQUIVALENCE (COMMEN_TECW(4),CTECW_GAPWIDI),(MAP_TECW(1,4),          57
     *MTECW_GAPWIDI)                                                      57
      CHARACTER CTECW_GAPWIDI*40/' '/                                     57
      CHARACTER NTECW_GAPWIDI*12/'R.GAPWIDI'/                             57
      INTEGER MTECW_GAPWIDI(2)/1,1/                                       57
      REAL RE_TECW_GAPWIDI                                                57
C -                                                                       57
      REAL TECW_GAPWIDO                                                   57
      EQUIVALENCE (BANK_TECW(7),TECW_GAPWIDO,RE_TECW_GAPWIDO),            57
     *(NAMES_TECW(5),NTECW_GAPWIDO)                                       57
      EQUIVALENCE (COMMEN_TECW(5),CTECW_GAPWIDO),(MAP_TECW(1,5),          57
     *MTECW_GAPWIDO)                                                      57
      CHARACTER CTECW_GAPWIDO*40/' '/                                     57
      CHARACTER NTECW_GAPWIDO*12/'R.GAPWIDO'/                             57
      INTEGER MTECW_GAPWIDO(2)/1,1/                                       57
      REAL RE_TECW_GAPWIDO                                                57
C -                                                                       57
      REAL TECW_INWIDTH                                                   57
      EQUIVALENCE (BANK_TECW(8),TECW_INWIDTH,RE_TECW_INWIDTH),            57
     *(NAMES_TECW(6),NTECW_INWIDTH)                                       57
      EQUIVALENCE (COMMEN_TECW(6),CTECW_INWIDTH),(MAP_TECW(1,6),          57
     *MTECW_INWIDTH)                                                      57
      CHARACTER CTECW_INWIDTH*40/' '/                                     57
      CHARACTER NTECW_INWIDTH*12/'R.INWIDTH'/                             57
      INTEGER MTECW_INWIDTH(2)/1,1/                                       57
      REAL RE_TECW_INWIDTH                                                57
C -                                                                       57
      REAL TECW_OUWIDTH                                                   57
      EQUIVALENCE (BANK_TECW(9),TECW_OUWIDTH,RE_TECW_OUWIDTH),            57
     *(NAMES_TECW(7),NTECW_OUWIDTH)                                       57
      EQUIVALENCE (COMMEN_TECW(7),CTECW_OUWIDTH),(MAP_TECW(1,7),          57
     *MTECW_OUWIDTH)                                                      57
      CHARACTER CTECW_OUWIDTH*40/' '/                                     57
      CHARACTER NTECW_OUWIDTH*12/'R.OUWIDTH'/                             57
      INTEGER MTECW_OUWIDTH(2)/1,1/                                       57
      REAL RE_TECW_OUWIDTH                                                57
C -                                                                       57
      REAL TECW_HEIGHT                                                    57
      EQUIVALENCE (BANK_TECW(10),TECW_HEIGHT,RE_TECW_HEIGHT),             57
     *(NAMES_TECW(8),NTECW_HEIGHT)                                        57
      EQUIVALENCE (COMMEN_TECW(8),CTECW_HEIGHT),(MAP_TECW(1,8),           57
     *MTECW_HEIGHT)                                                       57
      CHARACTER CTECW_HEIGHT*40/' '/                                      57
      CHARACTER NTECW_HEIGHT*12/'R.HEIGHT'/                               57
      INTEGER MTECW_HEIGHT(2)/1,1/                                        57
      REAL RE_TECW_HEIGHT                                                 57
C -                                                                       57
      REAL TECW_PPDEPTH                                                   57
      EQUIVALENCE (BANK_TECW(11),TECW_PPDEPTH,RE_TECW_PPDEPTH),           57
     *(NAMES_TECW(9),NTECW_PPDEPTH)                                       57
      EQUIVALENCE (COMMEN_TECW(9),CTECW_PPDEPTH),(MAP_TECW(1,9),          57
     *MTECW_PPDEPTH)                                                      57
      CHARACTER CTECW_PPDEPTH*40/' '/                                     57
      CHARACTER NTECW_PPDEPTH*12/'R.PPDEPTH'/                             57
      INTEGER MTECW_PPDEPTH(2)/1,1/                                       57
      REAL RE_TECW_PPDEPTH                                                57
C -                                                                       57
      REAL TECW_ASDEPTH                                                   57
      EQUIVALENCE (BANK_TECW(12),TECW_ASDEPTH,RE_TECW_ASDEPTH),           57
     *(NAMES_TECW(10),NTECW_ASDEPTH)                                      57
      EQUIVALENCE (COMMEN_TECW(10),CTECW_ASDEPTH),(MAP_TECW(1,10),        57
     *MTECW_ASDEPTH)                                                      57
      CHARACTER CTECW_ASDEPTH*40/' '/                                     57
      CHARACTER NTECW_ASDEPTH*12/'R.ASDEPTH'/                             57
      INTEGER MTECW_ASDEPTH(2)/1,1/                                       57
      REAL RE_TECW_ASDEPTH                                                57
C -                                                                       57
      REAL TECW_GGDEPTH                                                   57
      EQUIVALENCE (BANK_TECW(13),TECW_GGDEPTH,RE_TECW_GGDEPTH),           57
     *(NAMES_TECW(11),NTECW_GGDEPTH)                                      57
      EQUIVALENCE (COMMEN_TECW(11),CTECW_GGDEPTH),(MAP_TECW(1,11),        57
     *MTECW_GGDEPTH)                                                      57
      CHARACTER CTECW_GGDEPTH*40/' '/                                     57
      CHARACTER NTECW_GGDEPTH*12/'R.GGDEPTH'/                             57
      INTEGER MTECW_GGDEPTH(2)/1,1/                                       57
      REAL RE_TECW_GGDEPTH                                                57
C -                                                                       57
      REAL TECW_MWCDEPTH                                                  57
      EQUIVALENCE (BANK_TECW(14),TECW_MWCDEPTH,RE_TECW_MWCDEPTH),         57
     *(NAMES_TECW(12),NTECW_MWCDEPTH)                                     57
      EQUIVALENCE (COMMEN_TECW(12),CTECW_MWCDEPTH),(MAP_TECW(1,12),       57
     *MTECW_MWCDEPTH)                                                     57
      CHARACTER CTECW_MWCDEPTH*40/' '/                                    57
      CHARACTER NTECW_MWCDEPTH*12/'R.MWCDEPTH'/                           57
      INTEGER MTECW_MWCDEPTH(2)/1,1/                                      57
      REAL RE_TECW_MWCDEPTH                                               57
C -                                                                       57
      REAL TECW_BOUNDARY                                                  57
      EQUIVALENCE (BANK_TECW(15),TECW_BOUNDARY,RE_TECW_BOUNDARY),         57
     *(NAMES_TECW(13),NTECW_BOUNDARY)                                     57
      EQUIVALENCE (COMMEN_TECW(13),CTECW_BOUNDARY),(MAP_TECW(1,13),       57
     *MTECW_BOUNDARY)                                                     57
      CHARACTER CTECW_BOUNDARY*40/' '/                                    57
      CHARACTER NTECW_BOUNDARY*12/'R.BOUNDARY'/                           57
      INTEGER MTECW_BOUNDARY(2)/1,1/                                      57
      REAL RE_TECW_BOUNDARY                                               57
C -                                                                       57
      REAL TECW_RCENTER                                                   57
      EQUIVALENCE (BANK_TECW(16),TECW_RCENTER,RE_TECW_RCENTER),           57
     *(NAMES_TECW(14),NTECW_RCENTER)                                      57
      EQUIVALENCE (COMMEN_TECW(14),CTECW_RCENTER),(MAP_TECW(1,14),        57
     *MTECW_RCENTER)                                                      57
      CHARACTER CTECW_RCENTER*40/' '/                                     57
      CHARACTER NTECW_RCENTER*12/'R.RCENTER'/                             57
      INTEGER MTECW_RCENTER(2)/1,1/                                       57
      REAL RE_TECW_RCENTER                                                57
C -                                                                       57
      REAL TECW_MWCINN                                                    57
      EQUIVALENCE (BANK_TECW(17),TECW_MWCINN,RE_TECW_MWCINN),             57
     *(NAMES_TECW(15),NTECW_MWCINN)                                       57
      EQUIVALENCE (COMMEN_TECW(15),CTECW_MWCINN),(MAP_TECW(1,15),         57
     *MTECW_MWCINN)                                                       57
      CHARACTER CTECW_MWCINN*40/' '/                                      57
      CHARACTER NTECW_MWCINN*12/'R.MWCINN'/                               57
      INTEGER MTECW_MWCINN(2)/1,1/                                        57
      REAL RE_TECW_MWCINN                                                 57
C -                                                                       57
      REAL TECW_MWCOUT                                                    57
      EQUIVALENCE (BANK_TECW(18),TECW_MWCOUT,RE_TECW_MWCOUT),             57
     *(NAMES_TECW(16),NTECW_MWCOUT)                                       57
      EQUIVALENCE (COMMEN_TECW(16),CTECW_MWCOUT),(MAP_TECW(1,16),         57
     *MTECW_MWCOUT)                                                       57
      CHARACTER CTECW_MWCOUT*40/' '/                                      57
      CHARACTER NTECW_MWCOUT*12/'R.MWCOUT'/                               57
      INTEGER MTECW_MWCOUT(2)/1,1/                                        57
      REAL RE_TECW_MWCOUT                                                 57
C -                                                                       57
      REAL TECW_MWCHEI                                                    57
      EQUIVALENCE (BANK_TECW(19),TECW_MWCHEI,RE_TECW_MWCHEI),             57
     *(NAMES_TECW(17),NTECW_MWCHEI)                                       57
      EQUIVALENCE (COMMEN_TECW(17),CTECW_MWCHEI),(MAP_TECW(1,17),         57
     *MTECW_MWCHEI)                                                       57
      CHARACTER CTECW_MWCHEI*40/' '/                                      57
      CHARACTER NTECW_MWCHEI*12/'R.MWCHEI'/                               57
      INTEGER MTECW_MWCHEI(2)/1,1/                                        57
      REAL RE_TECW_MWCHEI                                                 57
C -                                                                       57
      REAL TECW_MWCCENT                                                   57
      EQUIVALENCE (BANK_TECW(20),TECW_MWCCENT,RE_TECW_MWCCENT),           57
     *(NAMES_TECW(18),NTECW_MWCCENT)                                      57
      EQUIVALENCE (COMMEN_TECW(18),CTECW_MWCCENT),(MAP_TECW(1,18),        57
     *MTECW_MWCCENT)                                                      57
      CHARACTER CTECW_MWCCENT*40/' '/                                     57
      CHARACTER NTECW_MWCCENT*12/'R.MWCCENT'/                             57
      INTEGER MTECW_MWCCENT(2)/1,1/                                       57
      REAL RE_TECW_MWCCENT                                                57
C -                                                                       57
      REAL TECW_MWCNWIR                                                   57
      EQUIVALENCE (BANK_TECW(21),TECW_MWCNWIR,RE_TECW_MWCNWIR),           57
     *(NAMES_TECW(19),NTECW_MWCNWIR)                                      57
      EQUIVALENCE (COMMEN_TECW(19),CTECW_MWCNWIR),(MAP_TECW(1,19),        57
     *MTECW_MWCNWIR)                                                      57
      CHARACTER CTECW_MWCNWIR*40/' '/                                     57
      CHARACTER NTECW_MWCNWIR*12/'R.MWCNWIR'/                             57
      INTEGER MTECW_MWCNWIR(2)/1,1/                                       57
      REAL RE_TECW_MWCNWIR                                                57
C -                                                                       57
      REAL TECW_N                                                         57
      EQUIVALENCE (BANK_TECW(22),TECW_N,RE_TECW_N),(NAMES_TECW(20),       57
     *NTECW_N)                                                            57
      EQUIVALENCE (COMMEN_TECW(20),CTECW_N),(MAP_TECW(1,20),MTECW_N)      57
      CHARACTER CTECW_N*40/' '/                                           57
      CHARACTER NTECW_N*12/'R.N'/                                         57
      INTEGER MTECW_N(2)/1,1/                                             57
      REAL RE_TECW_N                                                      57
C -                                                                       57
      REAL TECW_NEX                                                       57
      EQUIVALENCE (BANK_TECW(23),TECW_NEX,RE_TECW_NEX),(NAMES_TECW(21),   57
     *NTECW_NEX)                                                          57
      EQUIVALENCE (COMMEN_TECW(21),CTECW_NEX),(MAP_TECW(1,21),            57
     *MTECW_NEX)                                                          57
      CHARACTER CTECW_NEX*40/' '/                                         57
      CHARACTER NTECW_NEX*12/'R.NEX'/                                     57
      INTEGER MTECW_NEX(2)/1,1/                                           57
      REAL RE_TECW_NEX                                                    57
C -                                                                       57
      REAL TECW_Z(8)                                                      57
      EQUIVALENCE (BANK_TECW(24),TECW_Z,RE_TECW_Z),(NAMES_TECW(22),       57
     *NTECW_Z)                                                            57
      EQUIVALENCE (COMMEN_TECW(22),CTECW_Z),(MAP_TECW(1,22),MTECW_Z)      57
      CHARACTER CTECW_Z*40/' '/                                           57
      CHARACTER NTECW_Z*12/'R.Z'/                                         57
      INTEGER MTECW_Z(2)/8,1/                                             57
      REAL RE_TECW_Z                                                      57
C -                                                                       57
      REAL TECW_DZ(8)                                                     57
      EQUIVALENCE (BANK_TECW(32),TECW_DZ,RE_TECW_DZ),(NAMES_TECW(23),     57
     *NTECW_DZ)                                                           57
      EQUIVALENCE (COMMEN_TECW(23),CTECW_DZ),(MAP_TECW(1,23),MTECW_DZ)    57
      CHARACTER CTECW_DZ*40/' '/                                          57
      CHARACTER NTECW_DZ*12/'R.DZ'/                                       57
      INTEGER MTECW_DZ(2)/8,1/                                            57
      REAL RE_TECW_DZ                                                     57
C -                                                                       57
      REAL TECW_XEX(5)                                                    57
      EQUIVALENCE (BANK_TECW(40),TECW_XEX,RE_TECW_XEX),(NAMES_TECW(24),   57
     *NTECW_XEX)                                                          57
      EQUIVALENCE (COMMEN_TECW(24),CTECW_XEX),(MAP_TECW(1,24),            57
     *MTECW_XEX)                                                          57
      CHARACTER CTECW_XEX*40/' '/                                         57
      CHARACTER NTECW_XEX*12/'R.XEX'/                                     57
      INTEGER MTECW_XEX(2)/5,1/                                           57
      REAL RE_TECW_XEX                                                    57
C -                                                                       57
      REAL TECW_ZEX(5)                                                    57
      EQUIVALENCE (BANK_TECW(45),TECW_ZEX,RE_TECW_ZEX),(NAMES_TECW(25),   57
     *NTECW_ZEX)                                                          57
      EQUIVALENCE (COMMEN_TECW(25),CTECW_ZEX),(MAP_TECW(1,25),            57
     *MTECW_ZEX)                                                          57
      CHARACTER CTECW_ZEX*40/' '/                                         57
      CHARACTER NTECW_ZEX*12/'R.ZEX'/                                     57
      INTEGER MTECW_ZEX(2)/5,1/                                           57
      REAL RE_TECW_ZEX                                                    57
C -                                                                       57
      REAL TECW_DXEX(5)                                                   57
      EQUIVALENCE (BANK_TECW(50),TECW_DXEX,RE_TECW_DXEX),                 57
     *(NAMES_TECW(26),NTECW_DXEX)                                         57
      EQUIVALENCE (COMMEN_TECW(26),CTECW_DXEX),(MAP_TECW(1,26),           57
     *MTECW_DXEX)                                                         57
      CHARACTER CTECW_DXEX*40/' '/                                        57
      CHARACTER NTECW_DXEX*12/'R.DXEX'/                                   57
      INTEGER MTECW_DXEX(2)/5,1/                                          57
      REAL RE_TECW_DXEX                                                   57
C -                                                                       57
      REAL TECW_DZEX(5)                                                   57
      EQUIVALENCE (BANK_TECW(55),TECW_DZEX,RE_TECW_DZEX),                 57
     *(NAMES_TECW(27),NTECW_DZEX)                                         57
      EQUIVALENCE (COMMEN_TECW(27),CTECW_DZEX),(MAP_TECW(1,27),           57
     *MTECW_DZEX)                                                         57
      CHARACTER CTECW_DZEX*40/' '/                                        57
      CHARACTER NTECW_DZEX*12/'R.DZEX'/                                   57
      INTEGER MTECW_DZEX(2)/5,1/                                          57
      REAL RE_TECW_DZEX                                                   57
C -                                                                       57
      INTEGER LEN_TECW,LLEN_TECW,LINK_TECW                                57
      PARAMETER (LEN_TECW=59)                                             57
      PARAMETER (LLEN_TECW=27)                                            57
      INTEGER FLAG_TECW/0/,MAP_TECW(2,LLEN_TECW),IDNUM_TECW(10)           57
      CHARACTER NAMES_TECW(LLEN_TECW)*12,COMMEN_TECW(LLEN_TECW)*40        57
      DATA IDNUM_TECW/10*0/                                               57
      REAL BANK_TECW(LEN_TECW)                                            57
      CHARACTER*60 BPATH_TECW/' '/                                        57
      SAVE FLAG_TECW,MAP_TECW,IDNUM_TECW,NAMES_TECW,COMMEN_TECW,          57
     *BPATH_TECW                                                          57
      DATA LINK_TECW/0/,BANK_TECW/LEN_TECW*0/                             57
C *                                                                       57
*
C -                                                                       59
      REAL TPRS_SEC                                                       59
      EQUIVALENCE (BANK_TPRS(3),TPRS_SEC,RE_TPRS_SEC),(NAMES_TPRS(1),     59
     *NTPRS_SEC)                                                          59
      EQUIVALENCE (COMMEN_TPRS(1),CTPRS_SEC),(MAP_TPRS(1,1),MTPRS_SEC)    59
      CHARACTER CTPRS_SEC*40/' '/                                         59
      CHARACTER NTPRS_SEC*12/'R.SEC'/                                     59
      INTEGER MTPRS_SEC(2)/1,1/                                           59
      REAL RE_TPRS_SEC                                                    59
C -                                                                       59
      REAL TPRS_NROW                                                      59
      EQUIVALENCE (BANK_TPRS(4),TPRS_NROW,RE_TPRS_NROW),(NAMES_TPRS(2),   59
     *NTPRS_NROW)                                                         59
      EQUIVALENCE (COMMEN_TPRS(2),CTPRS_NROW),(MAP_TPRS(1,2),             59
     *MTPRS_NROW)                                                         59
      CHARACTER CTPRS_NROW*40/' '/                                        59
      CHARACTER NTPRS_NROW*12/'R.NROW'/                                   59
      INTEGER MTPRS_NROW(2)/1,1/                                          59
      REAL RE_TPRS_NROW                                                   59
C -                                                                       59
      REAL TPRS_PITCH                                                     59
      EQUIVALENCE (BANK_TPRS(5),TPRS_PITCH,RE_TPRS_PITCH),                59
     *(NAMES_TPRS(3),NTPRS_PITCH)                                         59
      EQUIVALENCE (COMMEN_TPRS(3),CTPRS_PITCH),(MAP_TPRS(1,3),            59
     *MTPRS_PITCH)                                                        59
      CHARACTER CTPRS_PITCH*40/' '/                                       59
      CHARACTER NTPRS_PITCH*12/'R.PITCH'/                                 59
      INTEGER MTPRS_PITCH(2)/1,1/                                         59
      REAL RE_TPRS_PITCH                                                  59
C -                                                                       59
      REAL TPRS_WIDTH                                                     59
      EQUIVALENCE (BANK_TPRS(6),TPRS_WIDTH,RE_TPRS_WIDTH),                59
     *(NAMES_TPRS(4),NTPRS_WIDTH)                                         59
      EQUIVALENCE (COMMEN_TPRS(4),CTPRS_WIDTH),(MAP_TPRS(1,4),            59
     *MTPRS_WIDTH)                                                        59
      CHARACTER CTPRS_WIDTH*40/' '/                                       59
      CHARACTER NTPRS_WIDTH*12/'R.WIDTH'/                                 59
      INTEGER MTPRS_WIDTH(2)/1,1/                                         59
      REAL RE_TPRS_WIDTH                                                  59
C -                                                                       59
      REAL TPRS_SUPER                                                     59
      EQUIVALENCE (BANK_TPRS(7),TPRS_SUPER,RE_TPRS_SUPER),                59
     *(NAMES_TPRS(5),NTPRS_SUPER)                                         59
      EQUIVALENCE (COMMEN_TPRS(5),CTPRS_SUPER),(MAP_TPRS(1,5),            59
     *MTPRS_SUPER)                                                        59
      CHARACTER CTPRS_SUPER*40/' '/                                       59
      CHARACTER NTPRS_SUPER*12/'R.SUPER'/                                 59
      INTEGER MTPRS_SUPER(2)/1,1/                                         59
      REAL RE_TPRS_SUPER                                                  59
C -                                                                       59
      REAL TPRS_RPADS(40)                                                 59
      EQUIVALENCE (BANK_TPRS(8),TPRS_RPADS,RE_TPRS_RPADS),                59
     *(NAMES_TPRS(6),NTPRS_RPADS)                                         59
      EQUIVALENCE (COMMEN_TPRS(6),CTPRS_RPADS),(MAP_TPRS(1,6),            59
     *MTPRS_RPADS)                                                        59
      CHARACTER CTPRS_RPADS*40/' '/                                       59
      CHARACTER NTPRS_RPADS*12/'R.RPADS'/                                 59
      INTEGER MTPRS_RPADS(2)/40,1/                                        59
      REAL RE_TPRS_RPADS                                                  59
C -                                                                       59
      REAL TPRS_NPADS(40)                                                 59
      EQUIVALENCE (BANK_TPRS(48),TPRS_NPADS,RE_TPRS_NPADS),               59
     *(NAMES_TPRS(7),NTPRS_NPADS)                                         59
      EQUIVALENCE (COMMEN_TPRS(7),CTPRS_NPADS),(MAP_TPRS(1,7),            59
     *MTPRS_NPADS)                                                        59
      CHARACTER CTPRS_NPADS*40/' '/                                       59
      CHARACTER NTPRS_NPADS*12/'R.NPADS'/                                 59
      INTEGER MTPRS_NPADS(2)/40,1/                                        59
      REAL RE_TPRS_NPADS                                                  59
C -                                                                       59
      INTEGER LEN_TPRS,LLEN_TPRS,LINK_TPRS                                59
      PARAMETER (LEN_TPRS=87)                                             59
      PARAMETER (LLEN_TPRS=7)                                             59
      INTEGER FLAG_TPRS/0/,MAP_TPRS(2,LLEN_TPRS),IDNUM_TPRS(10)           59
      CHARACTER NAMES_TPRS(LLEN_TPRS)*12,COMMEN_TPRS(LLEN_TPRS)*40        59
      DATA IDNUM_TPRS/10*0/                                               59
      REAL BANK_TPRS(LEN_TPRS)                                            59
      CHARACTER*60 BPATH_TPRS/' '/                                        59
      SAVE FLAG_TPRS,MAP_TPRS,IDNUM_TPRS,NAMES_TPRS,COMMEN_TPRS,          59
     *BPATH_TPRS                                                          59
      DATA LINK_TPRS/0/,BANK_TPRS/LEN_TPRS*0/                             59
C *                                                                       59
*
      REAL TOCSIR,TOCSOR,TOKAIR,TOKAOR,TONXIR,TONXOR,TOADIR,TOADOR,       65
     *TOIGIR,TOIGOR, TOALIR,TOALOR,TOHAIR,TOHAOR,TOFSIR,TOFSOR,TOFCIR,    65
     *TOFCOR, TIADIR,TIADOR,TINXIR,TINXOR,TIKAIR,TIKAOR,TIALIR,TIALOR,    65
     *TIFCIR,TIFCOR,TPGVIR,TPGVLENG,TOFCLENG, TPCWZ,TPGVZ,TPEAZ,DX_DZ,    65
     *DXB,DEL,A,B                                                         65
      REAL T, P, PATM, RHO, DENSITY                                       67
      INTEGER I_ROW,I_SEC,I                                               69
      EXTERNAL TPADSTEP,TPAISTEP,TPAOSTEP,TPCELASER                       70
*******************************************************************************
*
C *                                                                       73
      CALL AGSBEGM(MODULE,IPRIN)                                          73
      IF (IPRIN.LT.0) RETURN                                              73
C Filling bank TPCG - TPC BASIC DIMENSIONS                                73
      BANK_TITLE='TPC BASIC DIMENSIONS'                                   73
      BPATH_TPCG='TPCG*'                                                  73
      IDNUM_TPCG(1)=0                                                     74
C CURRENT VERSION                                                         74
      TPCG_VERSION=2                                                      74
      CTPCG_VERSION='CURRENT VERSION'                                     74
C TPC INNER RADIUS                                                        75
      TPCG_RMIN=46.107                                                    75
      CTPCG_RMIN='TPC INNER RADIUS'                                       75
C TPC OUTER RADIUS                                                        76
      TPCG_RMAX=207.750                                                   76
      CTPCG_RMAX='TPC OUTER RADIUS'                                       76
C TPC FULL LENGTH                                                         77
      TPCG_LENGTH=459.37                                                  77
      CTPCG_LENGTH='TPC FULL LENGTH'                                      77
C SUPPORT WHEEL INNER RADIUS                                              78
      TPCG_WHEELIR=49.60                                                  78
      CTPCG_WHEELIR='SUPPORT WHEEL INNER RADIUS'                          78
C SUPPORT WHEEL OUTER RADIUS                                              79
      TPCG_WHEELOR=206.75                                                 79
      CTPCG_WHEELOR='SUPPORT WHEEL OUTER RADIUS'                          79
C SUPPORT WHEEL LENGTH                                                    80
      TPCG_WHEELTHK=11.43                                                 80
      CTPCG_WHEELTHK='SUPPORT WHEEL LENGTH'                               80
C TPC SENSITIVE GAS OUTER RADIUS                                          81
      TPCG_SENGASOR=200                                                   81
      CTPCG_SENGASOR='TPC SENSITIVE GAS OUTER RADIUS'                     81
C ENDCAP MWC + SECTOR THICKNESS                                           82
      TPCG_TPEATHK=9.7                                                    82
      CTPCG_TPEATHK='ENDCAP MWC + SECTOR THICKNESS'                       82
C CENTRAL MEMBRANE THICKNESS                                              83
      TPCG_MEMBTHK=.00762                                                 83
      CTPCG_MEMBTHK='CENTRAL MEMBRANE THICKNESS'                          83
C OUTER COPPER THICKNESS                                                  84
      TPCG_TOCSDR=.013                                                    84
      CTPCG_TOCSDR='OUTER COPPER THICKNESS'                               84
C OUTER KAPTON THICKNESS                                                  85
      TPCG_TOKADR=.015                                                    85
      CTPCG_TOKADR='OUTER KAPTON THICKNESS'                               85
C OUTER NOMEX THICKNESS                                                   86
      TPCG_TONXDR=1.00                                                    86
      CTPCG_TONXDR='OUTER NOMEX THICKNESS'                                86
C OUTER ADHESIVE THICKNESS                                                87
      TPCG_TOADDR=0.02                                                    87
      CTPCG_TOADDR='OUTER ADHESIVE THICKNESS'                             87
C OUTER ISOLATING GAS THICKNESS                                           88
      TPCG_TOIGDR=5.70                                                    88
      CTPCG_TOIGDR='OUTER ISOLATING GAS THICKNESS'                        88
C OUTER ALUMINUM THICKNESS                                                89
      TPCG_TOALDR=0.40                                                    89
      CTPCG_TOALDR='OUTER ALUMINUM THICKNESS'                             89
C OUTER HONEYUCOMB AL THICKNESS                                           90
      TPCG_TOHADR=0.60                                                    90
      CTPCG_TOHADR='OUTER HONEYUCOMB AL THICKNESS'                        90
C INNER ADHESIVE LAYER THICKNESS                                          91
      TPCG_TIADDR=0.080                                                   91
      CTPCG_TIADDR='INNER ADHESIVE LAYER THICKNESS'                       91
C INNER NOMEX STRUCTURE THICKNESS                                         92
      TPCG_TINXDR=1.270                                                   92
      CTPCG_TINXDR='INNER NOMEX STRUCTURE THICKNESS'                      92
C INNER KAPTON LAYER THICKNESS                                            93
      TPCG_TIKADR=0.015                                                   93
      CTPCG_TIKADR='INNER KAPTON LAYER THICKNESS'                         93
C INNER ALUMINUM LAYER THICKNESS                                          94
      TPCG_TIALDR=0.004                                                   94
      CTPCG_TIALDR='INNER ALUMINUM LAYER THICKNESS'                       94
C MWC READOUT FLAG                                                        95
      TPCG_MWCREAD=2                                                      95
      CTPCG_MWCREAD='MWC READOUT FLAG'                                    95
C GAS DENSITY CORRECTION VERSION                                          96
      TPCG_GASCORR=1                                                      96
      CTPCG_GASCORR='GAS DENSITY CORRECTION VERSION'                      96
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCG',    97
     *BPATH_TPCG,IDNUM_TPCG,1, MAP_TPCG,NAMES_TPCG,COMMEN_TPCG,           97
     *BANK_TPCG,LEN_TPCG,LLEN_TPCG,LINK_TPCG,FLAG_TPCG)                   97
C *                                                                       97
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCG',    97
     *BPATH_TPCG,IDNUM_TPCG,1, MAP_TPCG,NAMES_TPCG,COMMEN_TPCG,           97
     *BANK_TPCG,LEN_TPCG,LLEN_TPCG,LINK_TPCG,FLAG_TPCG)                   97
*
C *                                                                       99
      CALL AGSBEGM(MODULE,IPRIN)                                          99
      IF (IPRIN.LT.0) RETURN                                              99
C Filling bank TPRS - SECTOR OF PADROWS                                   99
      BANK_TITLE='SECTOR OF PADROWS'                                      99
      BPATH_TPRS='TPRS*'                                                  99
      IDNUM_TPRS(1)=0                                                     100
C SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER                                 100
      TPRS_SEC=1                                                          100
      CTPRS_SEC='SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER'                 100
C NUMBER OF PADROWS IN THE SECTOR                                         101
      TPRS_NROW=13                                                        101
      CTPRS_NROW='NUMBER OF PADROWS IN THE SECTOR'                        101
C TPC PADROW PITCH WIDTH                                                  102
      TPRS_PITCH=0.335                                                    102
      CTPRS_PITCH='TPC PADROW PITCH WIDTH'                                102
C TPC PADROW THICKNESS                                                    103
      TPRS_WIDTH=1.15                                                     103
      CTPRS_WIDTH='TPC PADROW THICKNESS'                                  103
C NUMBER OF PADRAWS IN A SUPERPADROW                                      104
      TPRS_SUPER=3                                                        104
      CTPRS_SUPER='NUMBER OF PADRAWS IN A SUPERPADROW'                    104
C NUMBER OF PADS IN ROW                                                   106
      TPRS_NPADS(1)=88                                                    106
      TPRS_NPADS(2)=96                                                    106
      TPRS_NPADS(3)=104                                                   106
      TPRS_NPADS(4)=112                                                   106
      TPRS_NPADS(5)=118                                                   106
      TPRS_NPADS(6)=126                                                   106
      TPRS_NPADS(7)=134                                                   106
      TPRS_NPADS(8)=142                                                   106
      TPRS_NPADS(9)=150                                                   106
      TPRS_NPADS(10)=158                                                  106
      TPRS_NPADS(11)=166                                                  106
      TPRS_NPADS(12)=174                                                  106
      TPRS_NPADS(13)=182                                                  106
      CTPRS_NPADS='NUMBER OF PADS IN ROW'                                 106
C TPC PADROW RADII                                                        108
      TPRS_RPADS(1)=60.0                                                  108
      TPRS_RPADS(2)=64.8                                                  108
      TPRS_RPADS(3)=69.6                                                  108
      TPRS_RPADS(4)=74.4                                                  108
      TPRS_RPADS(5)=79.2                                                  108
      TPRS_RPADS(6)=84.0                                                  108
      TPRS_RPADS(7)=88.8                                                  108
      TPRS_RPADS(8)=93.6                                                  108
      TPRS_RPADS(9)=98.8                                                  108
      TPRS_RPADS(10)=104.0                                                108
      TPRS_RPADS(11)=109.2                                                108
      TPRS_RPADS(12)=114.4                                                108
      TPRS_RPADS(13)=119.6                                                108
      CTPRS_RPADS='TPC PADROW RADII'                                      108
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPRS',    109
     *BPATH_TPRS,IDNUM_TPRS,1, MAP_TPRS,NAMES_TPRS,COMMEN_TPRS,           109
     *BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)                   109
C *                                                                       109
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPRS',    109
     *BPATH_TPRS,IDNUM_TPRS,1, MAP_TPRS,NAMES_TPRS,COMMEN_TPRS,           109
     *BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)                   109
*
C *                                                                       111
      CALL AGSBEGM(MODULE,IPRIN)                                          111
      IF (IPRIN.LT.0) RETURN                                              111
C Filling bank TPRS - SECTOR OF PADROWS                                   111
      BANK_TITLE='SECTOR OF PADROWS'                                      111
      BPATH_TPRS='TPRS*'                                                  111
      IDNUM_TPRS(1)=0                                                     112
C SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER                                 112
      TPRS_SEC=2                                                          112
      CTPRS_SEC='SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER'                 112
C NUMBER OF PADROWS IN OUTER SECTOR                                       113
      TPRS_NROW=32                                                        113
      CTPRS_NROW='NUMBER OF PADROWS IN OUTER SECTOR'                      113
C OUTER TPC PADROW PITCH WIDTH                                            114
      TPRS_PITCH=0.67                                                     114
      CTPRS_PITCH='OUTER TPC PADROW PITCH WIDTH'                          114
C OUTER TPC PADROW THICKNESS                                              115
      TPRS_WIDTH=1.95                                                     115
      CTPRS_WIDTH='OUTER TPC PADROW THICKNESS'                            115
C NUMBER OF PADROWS IN A SUPERPADROW                                      116
      TPRS_SUPER=1                                                        116
      CTPRS_SUPER='NUMBER OF PADROWS IN A SUPERPADROW'                    116
C NUMBER OF PADS IN ROW                                                   120
      TPRS_NPADS(1)=98                                                    120
      TPRS_NPADS(2)=100                                                   120
      TPRS_NPADS(3)=102                                                   120
      TPRS_NPADS(4)=104                                                   120
      TPRS_NPADS(5)=106                                                   120
      TPRS_NPADS(6)=106                                                   120
      TPRS_NPADS(7)=108                                                   120
      TPRS_NPADS(8)=110                                                   120
      TPRS_NPADS(9)=112                                                   120
      TPRS_NPADS(10)=112                                                  120
      TPRS_NPADS(11)=114                                                  120
      TPRS_NPADS(12)=116                                                  120
      TPRS_NPADS(13)=118                                                  120
      TPRS_NPADS(14)=120                                                  120
      TPRS_NPADS(15)=122                                                  120
      TPRS_NPADS(16)=122                                                  120
      TPRS_NPADS(17)=124                                                  120
      TPRS_NPADS(18)=126                                                  120
      TPRS_NPADS(19)=128                                                  120
      TPRS_NPADS(20)=128                                                  120
      TPRS_NPADS(21)=130                                                  120
      TPRS_NPADS(22)=132                                                  120
      TPRS_NPADS(23)=134                                                  120
      TPRS_NPADS(24)=136                                                  120
      TPRS_NPADS(25)=138                                                  120
      TPRS_NPADS(26)=138                                                  120
      TPRS_NPADS(27)=140                                                  120
      TPRS_NPADS(28)=142                                                  120
      TPRS_NPADS(29)=144                                                  120
      TPRS_NPADS(30)=144                                                  120
      TPRS_NPADS(31)=144                                                  120
      TPRS_NPADS(32)=144                                                  120
      CTPRS_NPADS='NUMBER OF PADS IN ROW'                                 120
C TPC PADROW RADII                                                        127
      TPRS_RPADS(1)=127.195                                               127
      TPRS_RPADS(2)=129.195                                               127
      TPRS_RPADS(3)=131.195                                               127
      TPRS_RPADS(4)=133.195                                               127
      TPRS_RPADS(5)=135.195                                               127
      TPRS_RPADS(6)=137.195                                               127
      TPRS_RPADS(7)=139.195                                               127
      TPRS_RPADS(8)=141.195                                               127
      TPRS_RPADS(9)=143.195                                               127
      TPRS_RPADS(10)=145.195                                              127
      TPRS_RPADS(11)=147.195                                              127
      TPRS_RPADS(12)=149.195                                              127
      TPRS_RPADS(13)=151.195                                              127
      TPRS_RPADS(14)=153.195                                              127
      TPRS_RPADS(15)=155.195                                              127
      TPRS_RPADS(16)=157.195                                              127
      TPRS_RPADS(17)=159.195                                              127
      TPRS_RPADS(18)=161.195                                              127
      TPRS_RPADS(19)=163.195                                              127
      TPRS_RPADS(20)=165.195                                              127
      TPRS_RPADS(21)=167.195                                              127
      TPRS_RPADS(22)=169.195                                              127
      TPRS_RPADS(23)=171.195                                              127
      TPRS_RPADS(24)=173.195                                              127
      TPRS_RPADS(25)=175.195                                              127
      TPRS_RPADS(26)=177.195                                              127
      TPRS_RPADS(27)=179.195                                              127
      TPRS_RPADS(28)=181.195                                              127
      TPRS_RPADS(29)=183.195                                              127
      TPRS_RPADS(30)=185.195                                              127
      TPRS_RPADS(31)=187.195                                              127
      TPRS_RPADS(32)=189.195                                              127
      CTPRS_RPADS='TPC PADROW RADII'                                      127
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPRS',    128
     *BPATH_TPRS,IDNUM_TPRS,1, MAP_TPRS,NAMES_TPRS,COMMEN_TPRS,           128
     *BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)                   128
C *                                                                       128
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPRS',    128
     *BPATH_TPRS,IDNUM_TPRS,1, MAP_TPRS,NAMES_TPRS,COMMEN_TPRS,           128
     *BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)                   128
*
C *                                                                       130
      CALL AGSBEGM(MODULE,IPRIN)                                          130
      IF (IPRIN.LT.0) RETURN                                              130
C Filling bank TECW - EC TRAPEZOID AND SUPPORT WHEEL                      130
      BANK_TITLE='EC TRAPEZOID AND SUPPORT WHEEL'                         130
      BPATH_TECW='TECW*'                                                  130
      IDNUM_TECW(1)=0                                                     131
C SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER                                 131
      TECW_SEC=1                                                          131
      CTECW_SEC='SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER'                 131
C AIR IN SUPPORT WHEEL - INNER WIDTH                                      132
      TECW_GAPWIDI=2.* 10.91                                              132
      CTECW_GAPWIDI='AIR IN SUPPORT WHEEL - INNER WIDTH'                  132
C AIR IN SUPPORT WHEEL - OUTER WIDTH                                      133
      TECW_GAPWIDO=2.* 27.56                                              133
      CTECW_GAPWIDO='AIR IN SUPPORT WHEEL - OUTER WIDTH'                  133
C AIR IN SUPPORT WHEEL - HEIGHT (DR)                                      134
      TECW_GAPHEIT=62.15                                                  134
      CTECW_GAPHEIT='AIR IN SUPPORT WHEEL - HEIGHT (DR)'                  134
C AIR IN SUPPORT WHEEL - CENTER RADIUS                                    135
      TECW_GAPRAD=87.6                                                    135
      CTECW_GAPRAD='AIR IN SUPPORT WHEEL - CENTER RADIUS'                 135
C SECTOR WIDTH AT INNER RADIUS                                            136
      TECW_INWIDTH=2.* 13.75                                              136
      CTECW_INWIDTH='SECTOR WIDTH AT INNER RADIUS'                        136
C SECTOR WIDTH AT OUTER RADIUS                                            137
      TECW_OUWIDTH=2.* 32.38                                              137
      CTECW_OUWIDTH='SECTOR WIDTH AT OUTER RADIUS'                        137
C SECTOR RADIAL HEIGHT                                                    138
      TECW_HEIGHT=69.52                                                   138
      CTECW_HEIGHT='SECTOR RADIAL HEIGHT'                                 138
C PADPLANE THICKNESS (BOTH AL AND PCB)                                    139
      TECW_PPDEPTH=1.35+0.32                                              139
      CTECW_PPDEPTH='PADPLANE THICKNESS (BOTH AL AND PCB)'                139
C DEPTH OF OPENINGS IN ALUMINUM STRUCTURE                                 140
      TECW_ASDEPTH=6.99                                                   140
      CTECW_ASDEPTH='DEPTH OF OPENINGS IN ALUMINUM STRUCTURE'             140
C MWC GAP FROM GATED GRID TO PAD PLANE                                    141
      TECW_GGDEPTH=1.0                                                    141
      CTECW_GGDEPTH='MWC GAP FROM GATED GRID TO PAD PLANE'                141
C SENSITIVE MWC GAS GAP FULL THICKNESS                                    142
      TECW_MWCDEPTH=1.0                                                   142
      CTECW_MWCDEPTH='SENSITIVE MWC GAS GAP FULL THICKNESS'               142
C AL FRAME - BOUNDARY WIDTH                                               143
      TECW_BOUNDARY=3.74                                                  143
      CTECW_BOUNDARY='AL FRAME - BOUNDARY WIDTH'                          143
C SECTOR CENTER RADIUS (SET BY PRECISION HOLES)                           144
      TECW_RCENTER=86.669                                                 144
      CTECW_RCENTER='SECTOR CENTER RADIUS (SET BY PRECISION HOLES)'       144
C MWC SENSITIVE REGION INNER SIZE                                         145
      TECW_MWCINN=2*12.804                                                145
      CTECW_MWCINN='MWC SENSITIVE REGION INNER SIZE'                      145
C MWC SENSITIVE REGION OUTER SIZE                                         146
      TECW_MWCOUT=2*29.953                                                146
      CTECW_MWCOUT='MWC SENSITIVE REGION OUTER SIZE'                      146
C MWC SENSITIVE REGION HEIGHT (RADIAL)                                    147
      TECW_MWCHEI=64                                                      147
      CTECW_MWCHEI='MWC SENSITIVE REGION HEIGHT (RADIAL)'                 147
C SENSITIVE REGION CENTER POSITION                                        148
      TECW_MWCCENT=-1.669                                                 148
      CTECW_MWCCENT='SENSITIVE REGION CENTER POSITION'                    148
C NUMBER OF MWC SENSITIVE WIRES                                           149
      TECW_MWCNWIR=160                                                    149
      CTECW_MWCNWIR='NUMBER OF MWC SENSITIVE WIRES'                       149
C NUMBER OF AIR GAPS IN AL                                                150
      TECW_N=5                                                            150
      CTECW_N='NUMBER OF AIR GAPS IN AL'                                  150
C NUMBER OF EXTRA ALUMINUM SUPPORT PIECES                                 151
      TECW_NEX=0                                                          151
      CTECW_NEX='NUMBER OF EXTRA ALUMINUM SUPPORT PIECES'                 151
C POSITIONS OF AIR GAPS                                                   152
      TECW_Z(1)=6.67                                                      152
      TECW_Z(2)=18.22                                                     152
      TECW_Z(3)=34.22                                                     152
      TECW_Z(4)=50.22                                                     152
      TECW_Z(5)=62.65                                                     152
      CTECW_Z='POSITIONS OF AIR GAPS'                                     152
C SIZE OF AIR GAPS                                                        153
      TECW_DZ(1)=6.13                                                     153
      TECW_DZ(2)=15.05                                                    153
      TECW_DZ(3)=15.05                                                    153
      TECW_DZ(4)=15.05                                                    153
      TECW_DZ(5)=7.09                                                     153
      CTECW_DZ='SIZE OF AIR GAPS'                                         153
C X POSITIONS OF EXTRA ALUMINUM                                           154
      TECW_XEX(1)=0                                                       154
      TECW_XEX(2)=0                                                       154
      TECW_XEX(3)=0                                                       154
      TECW_XEX(4)=0                                                       154
      TECW_XEX(5)=0                                                       154
      CTECW_XEX='X POSITIONS OF EXTRA ALUMINUM'                           154
C Z POSITIONS OF EXTRA ALUMINUM                                           155
      TECW_ZEX(1)=0                                                       155
      TECW_ZEX(2)=0                                                       155
      TECW_ZEX(3)=0                                                       155
      TECW_ZEX(4)=0                                                       155
      TECW_ZEX(5)=0                                                       155
      CTECW_ZEX='Z POSITIONS OF EXTRA ALUMINUM'                           155
C X-THICKNESS OF EXTRA ALUMINUM                                           156
      TECW_DXEX(1)=0                                                      156
      TECW_DXEX(2)=0                                                      156
      TECW_DXEX(3)=0                                                      156
      TECW_DXEX(4)=0                                                      156
      TECW_DXEX(5)=0                                                      156
      CTECW_DXEX='X-THICKNESS OF EXTRA ALUMINUM'                          156
C Z-THICKNESS OF EXTRA ALUMINUM                                           157
      TECW_DZEX(1)=0                                                      157
      TECW_DZEX(2)=0                                                      157
      TECW_DZEX(3)=0                                                      157
      TECW_DZEX(4)=0                                                      157
      TECW_DZEX(5)=0                                                      157
      CTECW_DZEX='Z-THICKNESS OF EXTRA ALUMINUM'                          157
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TECW',    158
     *BPATH_TECW,IDNUM_TECW,1, MAP_TECW,NAMES_TECW,COMMEN_TECW,           158
     *BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)                   158
C *                                                                       158
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TECW',    158
     *BPATH_TECW,IDNUM_TECW,1, MAP_TECW,NAMES_TECW,COMMEN_TECW,           158
     *BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)                   158
*
C *                                                                       160
      CALL AGSBEGM(MODULE,IPRIN)                                          160
      IF (IPRIN.LT.0) RETURN                                              160
C Filling bank TECW - ENDCAP TRAPEZOID AND SUPPORT WHEEL                  160
      BANK_TITLE='ENDCAP TRAPEZOID AND SUPPORT WHEEL'                     160
      BPATH_TECW='TECW*'                                                  160
      IDNUM_TECW(1)=0                                                     161
C SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER                                 161
      TECW_SEC=2                                                          161
      CTECW_SEC='SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER'                 161
C AIR IN SUPPORT WHEEL - INNER WIDTH                                      162
      TECW_GAPWIDI=2.* 28.92                                              162
      CTECW_GAPWIDI='AIR IN SUPPORT WHEEL - INNER WIDTH'                  162
C AIR IN SUPPORT WHEEL - OUTER WIDTH                                      163
      TECW_GAPWIDO=2.* 46.74                                              163
      CTECW_GAPWIDO='AIR IN SUPPORT WHEEL - OUTER WIDTH'                  163
C AIR IN SUPPORT WHEEL - HEIGHT (DR)                                      164
      TECW_GAPHEIT=66.47                                                  164
      CTECW_GAPHEIT='AIR IN SUPPORT WHEEL - HEIGHT (DR)'                  164
C AIR IN SUPPORT WHEEL - RADIUS                                           165
      TECW_GAPRAD=157.34                                                  165
      CTECW_GAPRAD='AIR IN SUPPORT WHEEL - RADIUS'                        165
C SECTOR WIDTH AT INNER RADIUS                                            166
      TECW_INWIDTH=64.92                                                  166
      CTECW_INWIDTH='SECTOR WIDTH AT INNER RADIUS'                        166
C SECTOR WIDTH AT OUTER RADIUS                                            167
      TECW_OUWIDTH=103.25                                                 167
      CTECW_OUWIDTH='SECTOR WIDTH AT OUTER RADIUS'                        167
C SECTOR RADIAL HEIGHT                                                    168
      TECW_HEIGHT=71.51                                                   168
      CTECW_HEIGHT='SECTOR RADIAL HEIGHT'                                 168
C PADPLANE THICKNESS (BOTH AL AND PCB)                                    169
      TECW_PPDEPTH=0.95+0.32                                              169
      CTECW_PPDEPTH='PADPLANE THICKNESS (BOTH AL AND PCB)'                169
C DEPTH OF OPENINGS IN ALUMINUM STRUCTURE                                 170
      TECW_ASDEPTH=6.99                                                   170
      CTECW_ASDEPTH='DEPTH OF OPENINGS IN ALUMINUM STRUCTURE'             170
C MWC GAP FROM GATED GRID TO PAD PLANE                                    171
      TECW_GGDEPTH=1.4                                                    171
      CTECW_GGDEPTH='MWC GAP FROM GATED GRID TO PAD PLANE'                171
C SENSITIVE MWC GAS GAP FULL THICKNESS                                    172
      TECW_MWCDEPTH=1.4                                                   172
      CTECW_MWCDEPTH='SENSITIVE MWC GAS GAP FULL THICKNESS'               172
C AL FRAME - BOUNDARY WIDTH                                               173
      TECW_BOUNDARY=3.74                                                  173
      CTECW_BOUNDARY='AL FRAME - BOUNDARY WIDTH'                          173
C SECTOR CENTER RADIUS (SET BY PRECISION HOLES)                           174
      TECW_RCENTER=157.488                                                174
      CTECW_RCENTER='SECTOR CENTER RADIUS (SET BY PRECISION HOLES)'       174
C MWC SENSITIVE REGION INNER SIZE                                         175
      TECW_MWCINN=2*32.202                                                175
      CTECW_MWCINN='MWC SENSITIVE REGION INNER SIZE'                      175
C MWC SENSITIVE REGION OUTER SIZE                                         176
      TECW_MWCOUT=2*49.351                                                176
      CTECW_MWCOUT='MWC SENSITIVE REGION OUTER SIZE'                      176
C MWC SENSITIVE REGION HEIGHT (RADIAL)                                    177
      TECW_MWCHEI=64                                                      177
      CTECW_MWCHEI='MWC SENSITIVE REGION HEIGHT (RADIAL)'                 177
C SENSITIVE REGION CENTER POSITION                                        178
      TECW_MWCCENT=-0.093                                                 178
      CTECW_MWCCENT='SENSITIVE REGION CENTER POSITION'                    178
C NUMBER OF MWC SENSITIVE WIRES                                           179
      TECW_MWCNWIR=160                                                    179
      CTECW_MWCNWIR='NUMBER OF MWC SENSITIVE WIRES'                       179
C NUMBER OF AIR GAPS IN AL                                                180
      TECW_N=8                                                            180
      CTECW_N='NUMBER OF AIR GAPS IN AL'                                  180
C NUMBER OF EXTRA ALUMINUM SUPPORT PIECES                                 181
      TECW_NEX=5                                                          181
      CTECW_NEX='NUMBER OF EXTRA ALUMINUM SUPPORT PIECES'                 181
C OPENING POSITIONS                                                       183
      TECW_Z(1)=8.216                                                     183
      TECW_Z(2)=16.217                                                    183
      TECW_Z(3)=24.218                                                    183
      TECW_Z(4)=32.219                                                    183
      TECW_Z(5)=40.220                                                    183
      TECW_Z(6)=48.221                                                    183
      TECW_Z(7)=56.222                                                    183
      TECW_Z(8)=64.143                                                    183
      CTECW_Z='OPENING POSITIONS'                                         183
C OPENING SIZE                                                            185
      TECW_DZ(1)=7.05                                                     185
      TECW_DZ(2)=7.05                                                     185
      TECW_DZ(3)=7.05                                                     185
      TECW_DZ(4)=7.05                                                     185
      TECW_DZ(5)=7.05                                                     185
      TECW_DZ(6)=7.05                                                     185
      TECW_DZ(7)=7.05                                                     185
      TECW_DZ(8)=6.89                                                     185
      CTECW_DZ='OPENING SIZE'                                             185
C X POSITIONS                                                             186
      TECW_XEX(1)=0.                                                      186
      TECW_XEX(2)=0.                                                      186
      TECW_XEX(3)=0.                                                      186
      TECW_XEX(4)=-21.907                                                 186
      TECW_XEX(5)=21.907                                                  186
      CTECW_XEX='X POSITIONS'                                             186
C Z POSITIONS                                                             187
      TECW_ZEX(1)=6.69                                                    187
      TECW_ZEX(2)=32.22                                                   187
      TECW_ZEX(3)=40.22                                                   187
      TECW_ZEX(4)=56.22                                                   187
      TECW_ZEX(5)=56.22                                                   187
      CTECW_ZEX='Z POSITIONS'                                             187
C X-THICKNESS                                                             188
      TECW_DXEX(1)=2.54                                                   188
      TECW_DXEX(2)=0.476                                                  188
      TECW_DXEX(3)=0.476                                                  188
      TECW_DXEX(4)=3.175                                                  188
      TECW_DXEX(5)=3.175                                                  188
      CTECW_DXEX='X-THICKNESS'                                            188
C Z-THICKNESS                                                             189
      TECW_DZEX(1)=4.00                                                   189
      TECW_DZEX(2)=7.05                                                   189
      TECW_DZEX(3)=7.05                                                   189
      TECW_DZEX(4)=7.05                                                   189
      TECW_DZEX(5)=7.05                                                   189
      CTECW_DZEX='Z-THICKNESS'                                            189
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TECW',    190
     *BPATH_TECW,IDNUM_TECW,1, MAP_TECW,NAMES_TECW,COMMEN_TECW,           190
     *BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)                   190
C *                                                                       190
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TECW',    190
     *BPATH_TECW,IDNUM_TECW,1, MAP_TECW,NAMES_TECW,COMMEN_TECW,           190
     *BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)                   190
*     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*
C use default TPCG bank                                                   193
      BPATH_TPCG='TPCG*'                                                  193
      IDNUM_TPCG(1)=0                                                     193
      CALL AGDGETP('TPCEGEO','*',-999,'TPCG',' ',BANK_TPCG, BPATH_TPCG,   193
     *IDNUM_TPCG,1,MAP_TPCG,NAMES_TPCG,BANK_TPCG,LEN_TPCG,LLEN_TPCG,      193
     *LINK_TPCG,FLAG_TPCG)                                                193
*
*  TPC basic dimensions are the full system size, the gas volume length
*  and inner radius are derived from them and from material thicknesses.
*  The outer gas radius is also an input ( as this is used for winding ),
*  but the remaining clearance is checked to be positive (PN, 16 Mar 96).
*
C GAS PLUS ENDCAPS                                                        200
      TOFCLENG = TPCG_LENGTH-2*TPCG_WHEELTHK                              200
C ACTIVE GAS                                                              201
      TPGVLENG = (TOFCLENG-TPCG_MEMBTHK-2*TPCG_TPEATHK)/2                 201
*
* layer names mnemonic:
*-----------------------------------------------------------------------
* letter 1-6 :   t  :     i/o     :  cs/ka/nx/al/fc/fs  :     IR/OR    :
*-----------------------------------------------------------------------
*   meaning  :  TPC : inner/outer :    materials as     :  inner/outer :
*            :      :     cage    : Copper shild,Kapton :    radius    :
*            :      :             : Nomex,Aluminum  OR  :    of the    :
*            :      :             : field cage and      :    layer     :
*            :      :             : field cage support  :              :
*-----------------------------------------------------------------------
*
*** calculate radii of outer finest structures
      TOCSIR = TPCG_SENGASOR                                              214
      TOCSOR = TOCSIR + TPCG_TOCSDR                                       215
      TOKAIR = TOCSOR                                                     215
      TOKAOR = TOKAIR + TPCG_TOKADR                                       216
      TONXIR = TOKAOR                                                     216
      TONXOR = TONXIR + TPCG_TONXDR                                       217
      TOADIR = TONXOR                                                     217
      TOADOR = TOADIR + TPCG_TOADDR                                       218
      TOIGIR = TOADOR                                                     218
      TOIGOR = TOIGIR + TPCG_TOIGDR                                       219
      TOALIR = TOIGOR                                                     219
      TOALOR = TOALIR + TPCG_TOALDR                                       220
      TOHAIR = TOALOR                                                     220
      TOHAOR = TOHAIR + TPCG_TOHADR                                       221
*
*** calculate radii of inner finest structures
C INNER FIELD CAGE INNER RADIUS                                           224
      TIFCIR = TPCG_RMIN                                                  224
      TIADIR = TIFCIR                                                     224
      TIADOR = TIADIR + TPCG_TIADDR                                       225
      TINXIR = TIADOR                                                     225
      TINXOR = TINXIR + TPCG_TINXDR                                       226
      TIKAIR = TINXOR                                                     226
      TIKAOR = TIKAIR + TPCG_TIKADR                                       227
      TIALIR = TIKAOR                                                     227
      TIALOR = TIALIR + TPCG_TIALDR                                       228
*
***  derive radii of larger structures
      TOFCIR = TOCSIR                                                     230
      TOFCOR = TOHAOR                                                     231
      TOFSIR = TOCSIR                                                     231
      TOFSOR = TOADOR                                                     232
      TIFCIR = TIADIR                                                     232
      TIFCOR = TIALOR                                                     233
C TPC GAS INNER RADIUS                                                    234
      TPGVIR = TIFCOR                                                     234
*
***  make primitive control and basic printout
      DEL = TPCG_RMAX-TOFCOR                                              237
C Check IPRIN>=1                                                          238
      IF (IPRIN.GE.1) WRITE(LOUT,5010)TOFCOR,DEL                          238
5010  FORMAT(' TPCEgeo: maximum  TPC  Radius is ',F10.4,'  clearance is   239
     *',F8.4)                                                             239
C Check DEL<0                                                             240
      IF (DEL.LT.0) PRINT *,' *** TPCEgeo ERROR : outer clearance negati  240
     *ve ',DEL                                                            240
C Check IPRIN>=1                                                          240
      IF (IPRIN.GE.1) WRITE(LOUT,5020)TPGVIR                              240
5020  FORMAT(' TPCEgeo: senset. gas inner radius',F10.4)                  241
C Check IPRIN>=1                                                          241
      IF (IPRIN.GE.1) WRITE(LOUT,5030)TPGVLENG                            241
5030  FORMAT(' TPCEgeo: sensitive gas length is ',F10.4)                  242
*
*
      CALL AGSSTEP(TPCELASER)                                             245
      CALL AGSBEGM(MODULE,IPRIN)                                          246
C Check IPRIN<0                                                           246
      IF (IPRIN.LT.0) RETURN                                              246
C Create TPCE :                                                           246
C Call To Local Procedure TPCE                                            246
      ASSIGN 5050 TO BLOCK_TPCE                                           246
      GO TO  5040                                                         246
5050  CONTINUE                                                            246
C *                                                                       246
C Geant POSITION : TPCE ===> MOTHER='CAVE' <===                           246
      CALL AGSRESET                                                       246
      AG_MOTHER = 'CAVE'                                                  246
      AG_TITLE = 'TPCE'                                                   246
      AG_EXNAME = 'POSITION'                                              246
      AG_PARLIST = 'MOTHER'                                               246
      CALL AXPOSITION                                                     246
C *                                                                       246
*
*------------------------------------------------------------------------------
*
      CALL AGSENDM                                                        250
      RETURN                                                              250
C Begin Local Procedure TPCE                                              250
C TPCE IS THE TPC ENVELOPE                                                250
5040  CONTINUE                                                            250
         AG_TITLE='TPCE'                                                  250
         AG_MODULE=MODULE                                                 250
         AG_MARK='V00'                                                    250
         CALL AGSPUSH                                                     250
         AG_MODULE=MODULE                                                 250
C    *                                                                    250
*
C    Geant MATERIAL : AIR ===> inherited arguments used <===              252
         CALL AGSRESET                                                    252
         AG_TITLE = 'AIR'                                                 252
         AG_EXNAME = 'MATERIAL'                                           252
         AG_PARLIST = 'NONE'                                              252
         CALL AXMATERIAL                                                  252
C    *                                                                    252
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           252
         CALL AGSRESET                                                    253
         AG_TITLE = 'STANDARD'                                            253
         AG_EXNAME = 'MEDIUM'                                             253
         AG_PARLIST = 'NONE'                                              253
         CALL AXMEDIUM                                                    253
C    *                                                                    253
C    Geant ATTRIBUTE : TPCE ===> SEEN=0 COLO=2 <===                       253
         CALL AGSRESET                                                    254
         AG_SEEN = 0                                                      254
         AG_COLO = 2                                                      254
         AG_TITLE = 'TPCE'                                                254
         AG_EXNAME = 'ATTRIBUTE'                                          254
         AG_PARLIST = 'SEEN_COLO'                                         254
         CALL AXATTRIBUTE                                                 254
C    *                                                                    254
C    Geant SHAPE : TUBE ===> RMIN=TPCG_RMIN RMAX=TPCG_RMAX DZ=TPCG_LENGT  254
C    H/2 <===                                                             254
         CALL AGSRESET                                                    255
         AG_RMIN = TPCG_RMIN                                              255
         AG_RMAX = TPCG_RMAX                                              255
         AG_DZ = TPCG_LENGTH/2                                            255
         AG_TITLE = 'TUBE'                                                255
         AG_EXNAME = 'SHAPE'                                              255
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      255
         CALL AXSHAPE                                                     255
C    *                                                                    255
*                                                                1
C       gas volume                                                        257
         TPGVZ = (TPCG_MEMBTHK + TPGVLENG)/2                              257
C    Create TPGV :                                                        261
C    Call To Local Procedure TPGV                                         261
         ASSIGN 5070 TO BLOCK_TPGV                                        261
         GO TO  5060                                                      261
5070     CONTINUE                                                         261
C    *                                                                    261
C    Geant POSITION : TPGV ===> Z=+TPGVZ, THETAX=90 THETAY=90 THETAZ=0,   261
C    PHIX = 75 PHIY =-15 PHIZ=0 <===                                      261
         CALL AGSRESET                                                    261
         AG_Z = +TPGVZ                                                    261
         AG_THETAX = 90                                                   261
         AG_THETAY = 90                                                   261
         AG_THETAZ = 0                                                    261
         AG_PHIX = 75                                                     261
         AG_PHIY = -15                                                    261
         AG_PHIZ = 0                                                      261
         AG_TITLE = 'TPGV'                                                261
         AG_EXNAME = 'POSITION'                                           261
         AG_PARLIST = 'Z_THETAX_THETAY_THETAZ_PHIX_PHIY_PHIZ'             261
         CALL AXPOSITION                                                  261
C    *                                                                    261
C    Geant POSITION : TPGV ===> Z=-TPGVZ, THETAX=90 THETAY=90 THETAZ=180  261
C    , PHIX =105 PHIY =195 PHIZ=0 <===                                    261
         CALL AGSRESET                                                    264
         AG_Z = -TPGVZ                                                    264
         AG_THETAX = 90                                                   264
         AG_THETAY = 90                                                   264
         AG_THETAZ = 180                                                  264
         AG_PHIX = 105                                                    264
         AG_PHIY = 195                                                    264
         AG_PHIZ = 0                                                      264
         AG_TITLE = 'TPGV'                                                264
         AG_EXNAME = 'POSITION'                                           264
         AG_PARLIST = 'Z_THETAX_THETAY_THETAZ_PHIX_PHIY_PHIZ'             264
         CALL AXPOSITION                                                  264
C    *                                                                    264
*                                                                2
C    Create TIFC :                                                        266
C    Call To Local Procedure TIFC                                         266
         ASSIGN 5090 TO BLOCK_TIFC                                        266
         GO TO  5080                                                      266
5090     CONTINUE                                                         266
C    *                                                                    266
C    Geant POSITION : TIFC ===> inherited arguments used <===             266
         CALL AGSRESET                                                    266
         AG_TITLE = 'TIFC'                                                266
         AG_EXNAME = 'POSITION'                                           266
         AG_PARLIST = 'NONE'                                              266
         CALL AXPOSITION                                                  266
C    *                                                                    266
C    Create TOFC :                                                        267
C    Call To Local Procedure TOFC                                         267
         ASSIGN 5110 TO BLOCK_TOFC                                        267
         GO TO  5100                                                      267
5110     CONTINUE                                                         267
C    *                                                                    267
C    Geant POSITION : TOFC ===> inherited arguments used <===             267
         CALL AGSRESET                                                    267
         AG_TITLE = 'TOFC'                                                267
         AG_EXNAME = 'POSITION'                                           267
         AG_PARLIST = 'NONE'                                              267
         CALL AXPOSITION                                                  267
C    *                                                                    267
C    Create TPCM :                                                        268
C    Call To Local Procedure TPCM                                         268
         ASSIGN 5130 TO BLOCK_TPCM                                        268
         GO TO  5120                                                      268
5130     CONTINUE                                                         268
C    *                                                                    268
C    Geant POSITION : TPCM ===> inherited arguments used <===             268
         CALL AGSRESET                                                    268
         AG_TITLE = 'TPCM'                                                268
         AG_EXNAME = 'POSITION'                                           268
         AG_PARLIST = 'NONE'                                              268
         CALL AXPOSITION                                                  268
C    *                                                                    268
*
*                                                                3
C       TPC endcaps                                                       271
         TPEAZ = (TOFCLENG - TPCG_TPEATHK)/2                              271
C    Create TPEA :                                                        274
C    Call To Local Procedure TPEA                                         274
         ASSIGN 5150 TO BLOCK_TPEA                                        274
         GO TO  5140                                                      274
5150     CONTINUE                                                         274
C    *                                                                    274
C    Geant POSITION : TPEA ===> Z=+TPEAZ, THETAX=90 THETAY=90 THETAZ=0,   274
C    PHIX = 75 PHIY =-15 PHIZ=0 <===                                      274
         CALL AGSRESET                                                    274
         AG_Z = +TPEAZ                                                    274
         AG_THETAX = 90                                                   274
         AG_THETAY = 90                                                   274
         AG_THETAZ = 0                                                    274
         AG_PHIX = 75                                                     274
         AG_PHIY = -15                                                    274
         AG_PHIZ = 0                                                      274
         AG_TITLE = 'TPEA'                                                274
         AG_EXNAME = 'POSITION'                                           274
         AG_PARLIST = 'Z_THETAX_THETAY_THETAZ_PHIX_PHIY_PHIZ'             274
         CALL AXPOSITION                                                  274
C    *                                                                    274
C    Geant POSITION : TPEA ===> Z=-TPEAZ, THETAX=90 THETAY=90 THETAZ=180  274
C    , PHIX =105 PHIY =195 PHIZ=0 <===                                    274
         CALL AGSRESET                                                    277
         AG_Z = -TPEAZ                                                    277
         AG_THETAX = 90                                                   277
         AG_THETAY = 90                                                   277
         AG_THETAZ = 180                                                  277
         AG_PHIX = 105                                                    277
         AG_PHIY = 195                                                    277
         AG_PHIZ = 0                                                      277
         AG_TITLE = 'TPEA'                                                277
         AG_EXNAME = 'POSITION'                                           277
         AG_PARLIST = 'Z_THETAX_THETAY_THETAZ_PHIX_PHIY_PHIZ'             277
         CALL AXPOSITION                                                  277
C    *                                                                    277
*                                                                4
C     support wheels                                                      279
         TPCWZ = (TPCG_LENGTH-TPCG_WHEELTHK)/2                            279
C    Create TPCW :                                                        282
C    Call To Local Procedure TPCW                                         282
         ASSIGN 5170 TO BLOCK_TPCW                                        282
         GO TO  5160                                                      282
5170     CONTINUE                                                         282
C    *                                                                    282
C    Geant POSITION : TPCW ===> Z=+TPCWZ, THETAX=90 THETAY=90 THETAZ=0,   282
C    PHIX = 75 PHIY =-15 PHIZ=0 <===                                      282
         CALL AGSRESET                                                    282
         AG_Z = +TPCWZ                                                    282
         AG_THETAX = 90                                                   282
         AG_THETAY = 90                                                   282
         AG_THETAZ = 0                                                    282
         AG_PHIX = 75                                                     282
         AG_PHIY = -15                                                    282
         AG_PHIZ = 0                                                      282
         AG_TITLE = 'TPCW'                                                282
         AG_EXNAME = 'POSITION'                                           282
         AG_PARLIST = 'Z_THETAX_THETAY_THETAZ_PHIX_PHIY_PHIZ'             282
         CALL AXPOSITION                                                  282
C    *                                                                    282
C    Geant POSITION : TPCW ===> Z=-TPCWZ, THETAX=90 THETAY=90 THETAZ=180  282
C    , PHIX =105 PHIY =195 PHIZ=0 <===                                    282
         CALL AGSRESET                                                    285
         AG_Z = -TPCWZ                                                    285
         AG_THETAX = 90                                                   285
         AG_THETAY = 90                                                   285
         AG_THETAZ = 180                                                  285
         AG_PHIX = 105                                                    285
         AG_PHIY = 195                                                    285
         AG_PHIZ = 0                                                      285
         AG_TITLE = 'TPCW'                                                285
         AG_EXNAME = 'POSITION'                                           285
         AG_PARLIST = 'Z_THETAX_THETAY_THETAZ_PHIX_PHIY_PHIZ'             285
         CALL AXPOSITION                                                  285
C    *                                                                    285
*
5041  CALL AGSPOPB                                                        287
      GO TO BLOCK_TPCE, (5040 ,5050)                                      287
C End of Local Procedure                                                  287
*
******************************************************************************
*                       section one - sensitive gas                          *
******************************************************************************
*
C Begin Local Procedure TPGV                                              293
C TPGV IS THE GAS VOLUME PLACED IN TPC                                    293
5060  CONTINUE                                                            293
         AG_TITLE='TPGV'                                                  293
         AG_MODULE=MODULE                                                 293
         AG_MARK='V00'                                                    293
         CALL AGSPUSH                                                     293
         AG_MODULE=MODULE                                                 293
C    *                                                                    293
*
*     TPC default gas P10: Ar/methane 9:1 by weight
*     Component Ar    A=40  Z=18 W=.9
*     Component C     A=12  Z=6  W=.1*12./16.
*     Component H     A=1   Z=1  W=.1* 4./16.
*     TPC default gas P10: Ar/methane 9:1 by volume
C    Geant COMPONENT : AR ===> A=40 Z=18 W=9 <===                         300
         CALL AGSRESET                                                    300
         AG_A = 40                                                        300
         AG_Z = 18                                                        300
         AG_W = 9                                                         300
         AG_TITLE = 'AR'                                                  300
         AG_EXNAME = 'COMPONENT'                                          300
         AG_PARLIST = 'A_Z_W'                                             300
         CALL AXCOMPONENT                                                 300
C    *                                                                    300
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           300
         CALL AGSRESET                                                    301
         AG_A = 12                                                        301
         AG_Z = 6                                                         301
         AG_W = 1                                                         301
         AG_TITLE = 'C'                                                   301
         AG_EXNAME = 'COMPONENT'                                          301
         AG_PARLIST = 'A_Z_W'                                             301
         CALL AXCOMPONENT                                                 301
C    *                                                                    301
C    Geant COMPONENT : H ===> A=1 Z=1 W=4 <===                            301
         CALL AGSRESET                                                    302
         AG_A = 1                                                         302
         AG_Z = 1                                                         302
         AG_W = 4                                                         302
         AG_TITLE = 'H'                                                   302
         AG_EXNAME = 'COMPONENT'                                          302
         AG_PARLIST = 'A_Z_W'                                             302
         CALL AXCOMPONENT                                                 302
C    *                                                                    302
         IF (TPCG_GASCORR.EQ.1) THEN                                      304
         DENSITY=0.9*0.001782+0.1*0.000667                                305
         WRITE(*,*) 'Older (buggy) TPC gas density = ',DENSITY            306
         ELSE                                                             307
* Comment by Y.Fisyak:
* The STAR TPC has been operating with P10 (90% Ar, 10% methane by volume)
* at ~2 mbar above the ambient atmospheric pressure.
* Temperature is regulated close to 75F, both for the hall [supposedly regulated at (75 +/-5)F]
* dAu run (2003) average
C    Kelvin degree                                                        313
         T = 298.06                                                       313
C    mbar                                                                 314
         P = 1016.59                                                      314
C    mbar => torr => atm                                                  315
         PATM = 0.001*P*750.062/760.                                      315
C     g/cm**3                                                             316
         RHO =(0.9*0.001782+0.1*0.000717)                                 316
* at STP (Standard Temperature and Pressure, 0°C and one atmosphere pressure).
         DENSITY=RHO*(273.15/T)*PATM                                      319
         WRITE(*,*) 'Corrected TPC GAS DENSITY = ',DENSITY                320
         ENDIF                                                            321
C    Geant MIXTURE : P10 ===> DENS=DENSITY <===                           323
         CALL AGSRESET                                                    323
         AG_DENS = DENSITY                                                323
         AG_TITLE = 'P10'                                                 323
         AG_EXNAME = 'MIXTURE'                                            323
         AG_PARLIST = 'DENS'                                              323
         CALL AXMIXTURE                                                   323
C    *                                                                    323
C    Geant SHAPE : TUBE ===> RMIN=TPGVIR RMAX=TPCG_SENGASOR DZ=TPGVLENG/  323
C    2 <===                                                               323
         CALL AGSRESET                                                    324
         AG_RMIN = TPGVIR                                                 324
         AG_RMAX = TPCG_SENGASOR                                          324
         AG_DZ = TPGVLENG/2                                               324
         AG_TITLE = 'TUBE'                                                324
         AG_EXNAME = 'SHAPE'                                              324
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      324
         CALL AXSHAPE                                                     324
C    *                                                                    324
C    Create TPSS :                                                        325
C    Call To Local Procedure TPSS                                         325
         ASSIGN 5190 TO BLOCK_TPSS                                        325
         GO TO  5180                                                      325
5190     CONTINUE                                                         325
C    *                                                                    325
5061  CALL AGSPOPB                                                        326
      GO TO BLOCK_TPGV, (5060 ,5070)                                      326
C End of Local Procedure                                                  326
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Begin Local Procedure TPSS                                              328
C TPSS IS A DIVISION OF GAS VOLUME CORRESPONDING TO A SUPERSECTORS        328
5180  CONTINUE                                                            328
         AG_TITLE='TPSS'                                                  328
         AG_MODULE=MODULE                                                 328
         AG_MARK='V00'                                                    328
         CALL AGSPUSH                                                     328
         AG_MODULE=MODULE                                                 328
C    *                                                                    328
C    Geant ATTRIBUTE : TPSS ===> SEEN=0 <===                              329
         CALL AGSRESET                                                    329
         AG_SEEN = 0                                                      329
         AG_TITLE = 'TPSS'                                                329
         AG_EXNAME = 'ATTRIBUTE'                                          329
         AG_PARLIST = 'SEEN'                                              329
         CALL AXATTRIBUTE                                                 329
C    *                                                                    329
C    Geant SHAPE : DIVISION ===> NDIV=12 IAXIS=2 <===                     329
         CALL AGSRESET                                                    330
         AG_NDIV = 12                                                     330
         AG_IAXIS = 2                                                     330
         AG_TITLE = 'DIVISION'                                            330
         AG_EXNAME = 'SHAPE'                                              330
         AG_PARLIST = 'NDIV_IAXIS'                                        330
         CALL AXSHAPE                                                     330
C    *                                                                    330
*
C    *                                                                    332
         I_SEC=1                                                          332
            GO TO 5203                                                    332
5201        I_SEC=I_SEC+1                                                 332
5203        IF(I_SEC-(2).GT.0) GO TO 5202                                 332
C       use TPRS bank                                                     333
            BPATH_TPRS='TPRS*'                                            333
            IDNUM_TPRS(1)=0                                               333
            TPRS_SEC=I_SEC                                                333
            CALL AGDGETP('TPCEGEO','*',-999,'TPRS',NTPRS_SEC,             333
     *      RE_TPRS_SEC,BPATH_TPRS,IDNUM_TPRS,1,MAP_TPRS,NAMES_TPRS,      333
     *      BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)             333
*        position within supersector (this assumes rectangular padrows)
C       *                                                                 336
            I_ROW=1                                                       336
               GO TO 5213                                                 336
5211           I_ROW=I_ROW+1                                              336
5213           IF(I_ROW-(NINT(TPRS_NROW)).GT.0) GO TO 5212                336
C          Check NINT(TPRS_SUPER)==3 | I_ROW==1                           337
               IF (NINT(TPRS_SUPER).EQ.3 .OR. I_ROW.EQ.1) THEN            337
C          Create TPAD :                                                  339
C          Call To Local Procedure TPAD                                   339
               ASSIGN 5230 TO BLOCK_TPAD                                  339
               GO TO  5220                                                339
5230           CONTINUE                                                   339
C          *                                                              339
C          Geant POSITION : TPAD ===> X=TPRS_RPADS(I_ROW)-TPRS_WIDTH, DX  339
C          =TPRS_WIDTH/2 DY=TPRS_NPADS(I_ROW)*TPRS_PITCH/2 <===           339
               CALL AGSRESET                                              339
               AG_X = TPRS_RPADS(I_ROW)-TPRS_WIDTH                        339
               AG_DX = TPRS_WIDTH/2                                       339
               AG_DY = TPRS_NPADS(I_ROW)*TPRS_PITCH/2                     339
               AG_TITLE = 'TPAD'                                          339
               AG_EXNAME = 'POSITION'                                     339
               AG_PARLIST = 'X_DX_DY'                                     339
               CALL AXPOSITION                                            339
C          *                                                              339
               ENDIF                                                      339
C          Check NINT(TPRS_SUPER)>0                                       341
               IF (NINT(TPRS_SUPER).GT.0) THEN                            341
C          Create TPAD :                                                  343
C          Call To Local Procedure TPAD                                   343
               ASSIGN 5240 TO BLOCK_TPAD                                  343
               GO TO  5220                                                343
5240           CONTINUE                                                   343
C          *                                                              343
C          Geant POSITION : TPAD ===> X=TPRS_RPADS(I_ROW), DX=TPRS_WIDTH  343
C          /2 DY=TPRS_NPADS(I_ROW)*TPRS_PITCH/2 <===                      343
               CALL AGSRESET                                              343
               AG_X = TPRS_RPADS(I_ROW)                                   343
               AG_DX = TPRS_WIDTH/2                                       343
               AG_DY = TPRS_NPADS(I_ROW)*TPRS_PITCH/2                     343
               AG_TITLE = 'TPAD'                                          343
               AG_EXNAME = 'POSITION'                                     343
               AG_PARLIST = 'X_DX_DY'                                     343
               CALL AXPOSITION                                            343
C          *                                                              343
               ENDIF                                                      343
C          Check NINT(TPRS_SUPER)==3 | I_ROW==NINT(TPRS_NROW)             345
               IF (NINT(TPRS_SUPER).EQ.3 .OR. I_ROW.EQ.NINT(TPRS_NROW))   345
     *         THEN                                                       345
C          Create TPAD :                                                  347
C          Call To Local Procedure TPAD                                   347
               ASSIGN 5250 TO BLOCK_TPAD                                  347
               GO TO  5220                                                347
5250           CONTINUE                                                   347
C          *                                                              347
C          Geant POSITION : TPAD ===> X=TPRS_RPADS(I_ROW)+TPRS_WIDTH, DX  347
C          =TPRS_WIDTH/2 DY=TPRS_NPADS(I_ROW)*TPRS_PITCH/2 <===           347
               CALL AGSRESET                                              347
               AG_X = TPRS_RPADS(I_ROW)+TPRS_WIDTH                        347
               AG_DX = TPRS_WIDTH/2                                       347
               AG_DY = TPRS_NPADS(I_ROW)*TPRS_PITCH/2                     347
               AG_TITLE = 'TPAD'                                          347
               AG_EXNAME = 'POSITION'                                     347
               AG_PARLIST = 'X_DX_DY'                                     347
               CALL AXPOSITION                                            347
C          *                                                              347
               ENDIF                                                      347
            GO TO 5211                                                    349
5212        CONTINUE                                                      349
         GO TO 5201                                                       350
5202     CONTINUE                                                         350
5181  CALL AGSPOPB                                                        351
      GO TO BLOCK_TPSS, (5180 ,5190)                                      351
C End of Local Procedure                                                  351
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Begin Local Procedure TPAD                                              353
C TPAD IS A REAL PADROW WITH DIMENSIONS DEFINED AT POSITIONING TIME       353
5220  CONTINUE                                                            353
         AG_TITLE='TPAD'                                                  353
         AG_MODULE=MODULE                                                 353
         AG_MARK='V00'                                                    353
         CALL AGSPUSH                                                     353
         AG_MODULE=MODULE                                                 353
C    *                                                                    353
* We want to use PAI model in padrows to simulate energy loss more precisely.
* (see GEANT PHYS334, Allisson and Cobb, Ann.Rev.Nucl.Part.Sci.30(1980),253).
* (    best formalism in Grishin,Ermilova,Kotelnikov, NIM A307(1991),273).
* To switch it ON only in this volume we introduce P10 as a different MATERIAL,
* since ISTRA flag is kept in medium, but all tables are stored in the material
* According to AGI rules for this we need A parameter. Lets use ISVOL=1 (!)
* (- this eliminates a need for a separate medium definition ?)
*
C    Geant ATTRIBUTE : TPAD ===> SEEN=0 COLO=2 <===                       362
         CALL AGSRESET                                                    362
         AG_SEEN = 0                                                      362
         AG_COLO = 2                                                      362
         AG_TITLE = 'TPAD'                                                362
         AG_EXNAME = 'ATTRIBUTE'                                          362
         AG_PARLIST = 'SEEN_COLO'                                         362
         CALL AXATTRIBUTE                                                 362
C    *                                                                    362
C    Geant MATERIAL : P10 ===> inherited arguments used <===              362
         CALL AGSRESET                                                    363
         AG_TITLE = 'P10'                                                 363
         AG_EXNAME = 'MATERIAL'                                           363
         AG_PARLIST = 'NONE'                                              363
         CALL AXMATERIAL                                                  363
C    *                                                                    363
C    Geant MATERIAL : SENSITIVE_GAS ===> ISVOL=1 STEMAX=2.5*TPRS_WIDTH <  363
C    ===                                                                  363
         CALL AGSRESET                                                    364
         AG_ISVOL = 1                                                     364
         AG_STEMAX = 2.5*TPRS_WIDTH                                       364
         AG_TITLE = 'SENSITIVE_GAS'                                       364
         AG_EXNAME = 'MATERIAL'                                           364
         AG_PARLIST = 'ISVOL_STEMAX'                                      364
         CALL AXMATERIAL                                                  364
C    *                                                                    364
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           364
         CALL AGSRESET                                                    365
         AG_DX = 0                                                        365
         AG_DY = 0                                                        365
         AG_DZ = 0                                                        365
         AG_TITLE = 'BOX'                                                 365
         AG_EXNAME = 'SHAPE'                                              365
         AG_PARLIST = 'DX_DY_DZ'                                          365
         CALL AXSHAPE                                                     365
C    *                                                                    365
         CALL GSTPAR(AG_IMED,'STRA',1.)                                   365
*
*     The following is the corrected hits definition: 25-dec-98 (PN)
C    Defining Hits: Z:.0005:S Y:.0005: X:.0005: CX:10: CY:10: CZ:10:, LP  372
C    TOT:18:(-3,2) SLENG:.1:(0,800), TOF:16:(0,1.E-6) LGAM:16:(-2,2), ST  372
C    EP:11:(0,10) USER:21:(-.01,.01)                                      372
         AG_TITLE = 'TPAD'                                                372
         AG_EXNAME = 'HITS'                                               372
         AG_TYPE=AG_MAGIC                                                 372
         CALL AGSDETV('TPAD')                                             372
C    *                                                                    372
         AG_PARLIST='Z'                                                   372
         AG_TITLE = 'DETH'                                                372
         AG_NBITS = AG_MAGIC                                              372
         AG_BIN=.0005                                                     372
         AG_OPTION='S'                                                    372
         AG_FHMIN = AG_MAGIC                                              372
         AG_FHMAX = AG_MAGIC                                              372
         CALL AGSDETH (0)                                                 372
C    *                                                                    372
         AG_PARLIST='Y'                                                   372
         AG_TITLE = 'DETH'                                                372
         AG_NBITS = AG_MAGIC                                              372
         AG_BIN=.0005                                                     372
         AG_FHMIN=AG_MAGIC                                                372
         AG_FHMAX=AG_MAGIC                                                372
         AG_OPTION='*'                                                    372
         CALL AGSDETH (0)                                                 372
C    *                                                                    372
         AG_PARLIST='X'                                                   372
         AG_TITLE = 'DETH'                                                372
         AG_NBITS = AG_MAGIC                                              372
         AG_BIN=.0005                                                     372
         AG_FHMIN=AG_MAGIC                                                372
         AG_FHMAX=AG_MAGIC                                                372
         AG_OPTION='*'                                                    372
         CALL AGSDETH (0)                                                 372
C    *                                                                    372
         AG_PARLIST='CX'                                                  372
         AG_TITLE = 'DETH'                                                372
         AG_NBITS = 10                                                    372
         AG_BIN=AG_MAGIC                                                  372
         AG_FHMIN=AG_MAGIC                                                372
         AG_FHMAX=AG_MAGIC                                                372
         AG_OPTION='*'                                                    372
         CALL AGSDETH (0)                                                 372
C    *                                                                    372
         AG_PARLIST='CY'                                                  372
         AG_TITLE = 'DETH'                                                372
         AG_NBITS = 10                                                    372
         AG_BIN=AG_MAGIC                                                  372
         AG_FHMIN=AG_MAGIC                                                372
         AG_FHMAX=AG_MAGIC                                                372
         AG_OPTION='*'                                                    372
         CALL AGSDETH (0)                                                 372
C    *                                                                    372
         AG_PARLIST='CZ'                                                  372
         AG_TITLE = 'DETH'                                                372
         AG_NBITS = 10                                                    372
         AG_BIN=AG_MAGIC                                                  372
         AG_OPTION='*'                                                    372
         AG_FHMIN = AG_MAGIC                                              372
         AG_FHMAX = AG_MAGIC                                              372
         CALL AGSDETH (0)                                                 372
C    *                                                                    372
         AG_PARLIST='LPTOT'                                               372
         AG_TITLE = 'DETH'                                                372
         AG_NBITS = 18                                                    372
         AG_BIN=AG_MAGIC                                                  372
         AG_OPTION='*'                                                    372
         AG_FHMIN = -3                                                    372
         AG_FHMAX = 2                                                     372
         CALL AGSDETH (0)                                                 372
C    *                                                                    372
         AG_PARLIST='SLENG'                                               372
         AG_TITLE = 'DETH'                                                372
         AG_NBITS = AG_MAGIC                                              372
         AG_BIN=.1                                                        372
         AG_OPTION='*'                                                    372
         AG_FHMIN = 0                                                     372
         AG_FHMAX = 800                                                   372
         CALL AGSDETH (0)                                                 372
C    *                                                                    372
         AG_PARLIST='TOF'                                                 372
         AG_TITLE = 'DETH'                                                372
         AG_NBITS = 16                                                    372
         AG_BIN=AG_MAGIC                                                  372
         AG_OPTION='*'                                                    372
         AG_FHMIN = 0                                                     372
         AG_FHMAX = 1.E-6                                                 372
         CALL AGSDETH (0)                                                 372
C    *                                                                    372
         AG_PARLIST='LGAM'                                                372
         AG_TITLE = 'DETH'                                                372
         AG_NBITS = 16                                                    372
         AG_BIN=AG_MAGIC                                                  372
         AG_OPTION='*'                                                    372
         AG_FHMIN = -2                                                    372
         AG_FHMAX = 2                                                     372
         CALL AGSDETH (0)                                                 372
C    *                                                                    372
         AG_PARLIST='STEP'                                                372
         AG_TITLE = 'DETH'                                                372
         AG_NBITS = 11                                                    372
         AG_BIN=AG_MAGIC                                                  372
         AG_OPTION='*'                                                    372
         AG_FHMIN = 0                                                     372
         AG_FHMAX = 10                                                    372
         CALL AGSDETH (0)                                                 372
C    *                                                                    372
         AG_PARLIST='USER'                                                372
         AG_TITLE = 'DETH'                                                372
         AG_NBITS = 21                                                    372
         AG_BIN=AG_MAGIC                                                  372
         AG_OPTION='*'                                                    372
         AG_FHMIN = -.01                                                  372
         AG_FHMAX = .01                                                   372
         CALL AGSDETH (JUMPAD(TPADSTEP))                                  372
C    *                                                                    372
         CALL AGSDETU('TPAD')                                             372
C    *                                                                    372
5221  CALL AGSPOPB                                                        374
      GO TO BLOCK_TPAD, (5220 ,5230,5240,5250)                            374
C End of Local Procedure                                                  374
*
********************************************************************************
*        section two:   TPC inner and outer envelopes, membrane                *
* Groups of thin layers are positioned sequentially one inside another keeping *
* the outer radius constant. This is more efficient for GEANT and they can be  *
* easily replaced by single layer with a combined mixture (if needed later)    *
********************************************************************************
*
C Begin Local Procedure TIFC                                              383
C TIFC DEFINES THE INNER FIELD CAGE PLACED IN TPC                         383
5080  CONTINUE                                                            383
         AG_TITLE='TIFC'                                                  383
         AG_MODULE=MODULE                                                 383
         AG_MARK='V00'                                                    383
         CALL AGSPUSH                                                     383
         AG_MODULE=MODULE                                                 383
C    *                                                                    383
*** Contents of Inner Field Cage Structure TIFC: Adhesive,Nomex,Kapton,Aluminum
*   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            386
         CALL AGSRESET                                                    386
         AG_TITLE = 'MYLAR'                                               386
         AG_EXNAME = 'MATERIAL'                                           386
         AG_PARLIST = 'NONE'                                              386
         CALL AXMATERIAL                                                  386
C    *                                                                    386
C    Geant ATTRIBUTE : TIFC ===> SEEN=1 COLO=4 <===                       386
         CALL AGSRESET                                                    387
         AG_SEEN = 1                                                      387
         AG_COLO = 4                                                      387
         AG_TITLE = 'TIFC'                                                387
         AG_EXNAME = 'ATTRIBUTE'                                          387
         AG_PARLIST = 'SEEN_COLO'                                         387
         CALL AXATTRIBUTE                                                 387
C    *                                                                    387
C    Geant SHAPE : TUBE ===> RMIN=TIADIR RMAX=TIFCOR DZ=TOFCLENG/2 <===   387
         CALL AGSRESET                                                    388
         AG_RMIN = TIADIR                                                 388
         AG_RMAX = TIFCOR                                                 388
         AG_DZ = TOFCLENG/2                                               388
         AG_TITLE = 'TUBE'                                                388
         AG_EXNAME = 'SHAPE'                                              388
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      388
         CALL AXSHAPE                                                     388
C    *                                                                    388
C    Create TINX :                                                        389
C    Call To Local Procedure TINX                                         389
         ASSIGN 5270 TO BLOCK_TINX                                        389
         GO TO  5260                                                      389
5270     CONTINUE                                                         389
C    *                                                                    389
C    Geant POSITION : TINX ===> inherited arguments used <===             389
         CALL AGSRESET                                                    389
         AG_TITLE = 'TINX'                                                389
         AG_EXNAME = 'POSITION'                                           389
         AG_PARLIST = 'NONE'                                              389
         CALL AXPOSITION                                                  389
C    *                                                                    389
5081  CALL AGSPOPB                                                        389
      GO TO BLOCK_TIFC, (5080 ,5090)                                      390
C End of Local Procedure                                                  390
*
C Begin Local Procedure TINX                                              392
C TINX IS THE INNER NOMEX STRUCTURE                                       392
5260  CONTINUE                                                            392
         AG_TITLE='TINX'                                                  392
         AG_MODULE=MODULE                                                 392
         AG_MARK='V00'                                                    392
         CALL AGSPUSH                                                     392
         AG_MODULE=MODULE                                                 392
C    *                                                                    392
*
*     nomex - PN  8/3/96 following I.Sacreda note
C    Geant COMPONENT : C ===> A=12 Z=6 W=5 <===                           395
         CALL AGSRESET                                                    395
         AG_A = 12                                                        395
         AG_Z = 6                                                         395
         AG_W = 5                                                         395
         AG_TITLE = 'C'                                                   395
         AG_EXNAME = 'COMPONENT'                                          395
         AG_PARLIST = 'A_Z_W'                                             395
         CALL AXCOMPONENT                                                 395
C    *                                                                    395
C    Geant COMPONENT : H ===> A=1 Z=1 W=8 <===                            395
         CALL AGSRESET                                                    396
         AG_A = 1                                                         396
         AG_Z = 1                                                         396
         AG_W = 8                                                         396
         AG_TITLE = 'H'                                                   396
         AG_EXNAME = 'COMPONENT'                                          396
         AG_PARLIST = 'A_Z_W'                                             396
         CALL AXCOMPONENT                                                 396
C    *                                                                    396
C    Geant COMPONENT : O ===> A=16 Z=8 W=2 <===                           396
         CALL AGSRESET                                                    397
         AG_A = 16                                                        397
         AG_Z = 8                                                         397
         AG_W = 2                                                         397
         AG_TITLE = 'O'                                                   397
         AG_EXNAME = 'COMPONENT'                                          397
         AG_PARLIST = 'A_Z_W'                                             397
         CALL AXCOMPONENT                                                 397
C    *                                                                    397
C    Geant MIXTURE : NOMEX ===> DENS=0.048 <===                           397
         CALL AGSRESET                                                    398
         AG_DENS = 0.048                                                  398
         AG_TITLE = 'NOMEX'                                               398
         AG_EXNAME = 'MIXTURE'                                            398
         AG_PARLIST = 'DENS'                                              398
         CALL AXMIXTURE                                                   398
C    *                                                                    398
C    Geant ATTRIBUTE : TINX ===> SEEN=1 COLO=5 <===                       398
         CALL AGSRESET                                                    399
         AG_SEEN = 1                                                      399
         AG_COLO = 5                                                      399
         AG_TITLE = 'TINX'                                                399
         AG_EXNAME = 'ATTRIBUTE'                                          399
         AG_PARLIST = 'SEEN_COLO'                                         399
         CALL AXATTRIBUTE                                                 399
C    *                                                                    399
C    Geant SHAPE : TUBE ===> RMIN=TINXIR <===                             399
         CALL AGSRESET                                                    400
         AG_RMIN = TINXIR                                                 400
         AG_TITLE = 'TUBE'                                                400
         AG_EXNAME = 'SHAPE'                                              400
         AG_PARLIST = 'RMIN'                                              400
         CALL AXSHAPE                                                     400
C    *                                                                    400
C    Create TIKA :                                                        401
C    Call To Local Procedure TIKA                                         401
         ASSIGN 5290 TO BLOCK_TIKA                                        401
         GO TO  5280                                                      401
5290     CONTINUE                                                         401
C    *                                                                    401
C    Geant POSITION : TIKA ===> inherited arguments used <===             401
         CALL AGSRESET                                                    401
         AG_TITLE = 'TIKA'                                                401
         AG_EXNAME = 'POSITION'                                           401
         AG_PARLIST = 'NONE'                                              401
         CALL AXPOSITION                                                  401
C    *                                                                    401
5261  CALL AGSPOPB                                                        401
      GO TO BLOCK_TINX, (5260 ,5270)                                      402
C End of Local Procedure                                                  402
*
C Begin Local Procedure TIKA                                              404
C TIKA IS THE KAPTON FILM OF THE INNER FIELD CAGE                         404
5280  CONTINUE                                                            404
         AG_TITLE='TIKA'                                                  404
         AG_MODULE=MODULE                                                 404
         AG_MARK='V00'                                                    404
         CALL AGSPUSH                                                     404
         AG_MODULE=MODULE                                                 404
C    *                                                                    404
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            405
         CALL AGSRESET                                                    405
         AG_TITLE = 'MYLAR'                                               405
         AG_EXNAME = 'MATERIAL'                                           405
         AG_PARLIST = 'NONE'                                              405
         CALL AXMATERIAL                                                  405
C    *                                                                    405
C    Geant ATTRIBUTE : TIKA ===> SEEN=1 COLO=6 <===                       405
         CALL AGSRESET                                                    406
         AG_SEEN = 1                                                      406
         AG_COLO = 6                                                      406
         AG_TITLE = 'TIKA'                                                406
         AG_EXNAME = 'ATTRIBUTE'                                          406
         AG_PARLIST = 'SEEN_COLO'                                         406
         CALL AXATTRIBUTE                                                 406
C    *                                                                    406
C    Geant SHAPE : TUBE ===> RMIN=TIKAIR <===                             406
         CALL AGSRESET                                                    407
         AG_RMIN = TIKAIR                                                 407
         AG_TITLE = 'TUBE'                                                407
         AG_EXNAME = 'SHAPE'                                              407
         AG_PARLIST = 'RMIN'                                              407
         CALL AXSHAPE                                                     407
C    *                                                                    407
C    Create TIAL :                                                        408
C    Call To Local Procedure TIAL                                         408
         ASSIGN 5310 TO BLOCK_TIAL                                        408
         GO TO  5300                                                      408
5310     CONTINUE                                                         408
C    *                                                                    408
C    Geant POSITION : TIAL ===> inherited arguments used <===             408
         CALL AGSRESET                                                    408
         AG_TITLE = 'TIAL'                                                408
         AG_EXNAME = 'POSITION'                                           408
         AG_PARLIST = 'NONE'                                              408
         CALL AXPOSITION                                                  408
C    *                                                                    408
5281  CALL AGSPOPB                                                        408
      GO TO BLOCK_TIKA, (5280 ,5290)                                      409
C End of Local Procedure                                                  409
*
C Begin Local Procedure TIAL                                              411
C TIAL IS THE INNER ALUMINUM CYLINDER                                     411
5300  CONTINUE                                                            411
         AG_TITLE='TIAL'                                                  411
         AG_MODULE=MODULE                                                 411
         AG_MARK='V00'                                                    411
         CALL AGSPUSH                                                     411
         AG_MODULE=MODULE                                                 411
C    *                                                                    411
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        412
         CALL AGSRESET                                                    412
         AG_TITLE = 'ALUMINIUM'                                           412
         AG_EXNAME = 'MATERIAL'                                           412
         AG_PARLIST = 'NONE'                                              412
         CALL AXMATERIAL                                                  412
C    *                                                                    412
C    Geant ATTRIBUTE : TIAL ===> SEEN=1 COLO=7 <===                       412
         CALL AGSRESET                                                    413
         AG_SEEN = 1                                                      413
         AG_COLO = 7                                                      413
         AG_TITLE = 'TIAL'                                                413
         AG_EXNAME = 'ATTRIBUTE'                                          413
         AG_PARLIST = 'SEEN_COLO'                                         413
         CALL AXATTRIBUTE                                                 413
C    *                                                                    413
C    Geant SHAPE : TUBE ===> RMIN=TIALIR <===                             413
         CALL AGSRESET                                                    414
         AG_RMIN = TIALIR                                                 414
         AG_TITLE = 'TUBE'                                                414
         AG_EXNAME = 'SHAPE'                                              414
         AG_PARLIST = 'RMIN'                                              414
         CALL AXSHAPE                                                     414
C    *                                                                    414
5301  CALL AGSPOPB                                                        414
      GO TO BLOCK_TIAL, (5300 ,5310)                                      415
C End of Local Procedure                                                  415
*
*******************************************************************************
*
C Begin Local Procedure TOFC                                              419
C TOFC DEFINES OUTER FIELD CAGE - FILL IT WITH INSULATING GAS ALREADY     419
5100  CONTINUE                                                            419
         AG_TITLE='TOFC'                                                  419
         AG_MODULE=MODULE                                                 419
         AG_MARK='V00'                                                    419
         CALL AGSPUSH                                                     419
         AG_MODULE=MODULE                                                 419
C    *                                                                    419
C    Geant MATERIAL : NITROGEN_GAS ===> inherited arguments used <===     420
         CALL AGSRESET                                                    420
         AG_TITLE = 'NITROGEN_GAS'                                        420
         AG_EXNAME = 'MATERIAL'                                           420
         AG_PARLIST = 'NONE'                                              420
         CALL AXMATERIAL                                                  420
C    *                                                                    420
C    Geant ATTRIBUTE : TOFC ===> SEEN=1 COLO=3 <===                       420
         CALL AGSRESET                                                    421
         AG_SEEN = 1                                                      421
         AG_COLO = 3                                                      421
         AG_TITLE = 'TOFC'                                                421
         AG_EXNAME = 'ATTRIBUTE'                                          421
         AG_PARLIST = 'SEEN_COLO'                                         421
         CALL AXATTRIBUTE                                                 421
C    *                                                                    421
C    Geant SHAPE : TUBE ===> RMIN=TOFCIR RMAX=TOFCOR DZ=TOFCLENG/2 <===   421
         CALL AGSRESET                                                    422
         AG_RMIN = TOFCIR                                                 422
         AG_RMAX = TOFCOR                                                 422
         AG_DZ = TOFCLENG/2                                               422
         AG_TITLE = 'TUBE'                                                422
         AG_EXNAME = 'SHAPE'                                              422
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      422
         CALL AXSHAPE                                                     422
C    *                                                                    422
C    Create TOFS :                                                        423
C    Call To Local Procedure TOFS                                         423
         ASSIGN 5330 TO BLOCK_TOFS                                        423
         GO TO  5320                                                      423
5330     CONTINUE                                                         423
C    *                                                                    423
C    Geant POSITION : TOFS ===> inherited arguments used <===             423
         CALL AGSRESET                                                    423
         AG_TITLE = 'TOFS'                                                423
         AG_EXNAME = 'POSITION'                                           423
         AG_PARLIST = 'NONE'                                              423
         CALL AXPOSITION                                                  423
C    *                                                                    423
C    Create TOST :                                                        424
C    Call To Local Procedure TOST                                         424
         ASSIGN 5350 TO BLOCK_TOST                                        424
         GO TO  5340                                                      424
5350     CONTINUE                                                         424
C    *                                                                    424
C    Geant POSITION : TOST ===> inherited arguments used <===             424
         CALL AGSRESET                                                    424
         AG_TITLE = 'TOST'                                                424
         AG_EXNAME = 'POSITION'                                           424
         AG_PARLIST = 'NONE'                                              424
         CALL AXPOSITION                                                  424
C    *                                                                    424
5101  CALL AGSPOPB                                                        424
      GO TO BLOCK_TOFC, (5100 ,5110)                                      425
C End of Local Procedure                                                  425
*------------------------------------------------------------------------------
*
*** Contents of Outer Field Cage Structure TOFS:  Copper/Kapton/Nomex/Adhesive
*   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Begin Local Procedure TOFS                                              430
C TOFS IS THE OUTER FIELD CAGE STRUCTURE                                  430
5320  CONTINUE                                                            430
         AG_TITLE='TOFS'                                                  430
         AG_MODULE=MODULE                                                 430
         AG_MARK='V00'                                                    430
         CALL AGSPUSH                                                     430
         AG_MODULE=MODULE                                                 430
C    *                                                                    430
* Actually only Copper strips will remain as material,
* the rest will be taken out by other layers.
* This allows to avoid thin cylinder and to reduce number of volums
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           434
         CALL AGSRESET                                                    434
         AG_TITLE = 'COPPER'                                              434
         AG_EXNAME = 'MATERIAL'                                           434
         AG_PARLIST = 'NONE'                                              434
         CALL AXMATERIAL                                                  434
C    *                                                                    434
C    Geant ATTRIBUTE : TOFS ===> SEEN=1 COLO=4 <===                       434
         CALL AGSRESET                                                    435
         AG_SEEN = 1                                                      435
         AG_COLO = 4                                                      435
         AG_TITLE = 'TOFS'                                                435
         AG_EXNAME = 'ATTRIBUTE'                                          435
         AG_PARLIST = 'SEEN_COLO'                                         435
         CALL AXATTRIBUTE                                                 435
C    *                                                                    435
C    Geant SHAPE : TUBE ===> RMAX=TOFSOR <===                             435
         CALL AGSRESET                                                    436
         AG_RMAX = TOFSOR                                                 436
         AG_TITLE = 'TUBE'                                                436
         AG_EXNAME = 'SHAPE'                                              436
         AG_PARLIST = 'RMAX'                                              436
         CALL AXSHAPE                                                     436
C    *                                                                    436
*
C    Create TOKA :                                                        438
C    Call To Local Procedure TOKA                                         438
         ASSIGN 5370 TO BLOCK_TOKA                                        438
         GO TO  5360                                                      438
5370     CONTINUE                                                         438
C    *                                                                    438
C    Geant POSITION : TOKA ===> inherited arguments used <===             438
         CALL AGSRESET                                                    438
         AG_TITLE = 'TOKA'                                                438
         AG_EXNAME = 'POSITION'                                           438
         AG_PARLIST = 'NONE'                                              438
         CALL AXPOSITION                                                  438
C    *                                                                    438
5321  CALL AGSPOPB                                                        438
      GO TO BLOCK_TOFS, (5320 ,5330)                                      439
C End of Local Procedure                                                  439
*
C Begin Local Procedure TOKA                                              441
C TOKA IS KAPTON LAYER                                                    441
5360  CONTINUE                                                            441
         AG_TITLE='TOKA'                                                  441
         AG_MODULE=MODULE                                                 441
         AG_MARK='V00'                                                    441
         CALL AGSPUSH                                                     441
         AG_MODULE=MODULE                                                 441
C    *                                                                    441
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            442
         CALL AGSRESET                                                    442
         AG_TITLE = 'MYLAR'                                               442
         AG_EXNAME = 'MATERIAL'                                           442
         AG_PARLIST = 'NONE'                                              442
         CALL AXMATERIAL                                                  442
C    *                                                                    442
C    Geant ATTRIBUTE : TOKA ===> SEEN=1 COLO=3 <===                       442
         CALL AGSRESET                                                    443
         AG_SEEN = 1                                                      443
         AG_COLO = 3                                                      443
         AG_TITLE = 'TOKA'                                                443
         AG_EXNAME = 'ATTRIBUTE'                                          443
         AG_PARLIST = 'SEEN_COLO'                                         443
         CALL AXATTRIBUTE                                                 443
C    *                                                                    443
C    Geant SHAPE : TUBE ===> RMIN=TOKAIR <===                             443
         CALL AGSRESET                                                    444
         AG_RMIN = TOKAIR                                                 444
         AG_TITLE = 'TUBE'                                                444
         AG_EXNAME = 'SHAPE'                                              444
         AG_PARLIST = 'RMIN'                                              444
         CALL AXSHAPE                                                     444
C    *                                                                    444
C    Create TONX :                                                        445
C    Call To Local Procedure TONX                                         445
         ASSIGN 5390 TO BLOCK_TONX                                        445
         GO TO  5380                                                      445
5390     CONTINUE                                                         445
C    *                                                                    445
C    Geant POSITION : TONX ===> inherited arguments used <===             445
         CALL AGSRESET                                                    445
         AG_TITLE = 'TONX'                                                445
         AG_EXNAME = 'POSITION'                                           445
         AG_PARLIST = 'NONE'                                              445
         CALL AXPOSITION                                                  445
C    *                                                                    445
5361  CALL AGSPOPB                                                        445
      GO TO BLOCK_TOKA, (5360 ,5370)                                      446
C End of Local Procedure                                                  446
*
C Begin Local Procedure TONX                                              448
C TONX IS NOMEX SUPPORT                                                   448
5380  CONTINUE                                                            448
         AG_TITLE='TONX'                                                  448
         AG_MODULE=MODULE                                                 448
         AG_MARK='V00'                                                    448
         CALL AGSPUSH                                                     448
         AG_MODULE=MODULE                                                 448
C    *                                                                    448
C    Geant MATERIAL : NOMEX ===> inherited arguments used <===            449
         CALL AGSRESET                                                    449
         AG_TITLE = 'NOMEX'                                               449
         AG_EXNAME = 'MATERIAL'                                           449
         AG_PARLIST = 'NONE'                                              449
         CALL AXMATERIAL                                                  449
C    *                                                                    449
C    Geant ATTRIBUTE : TONX ===> SEEN=1 COLO=2 <===                       449
         CALL AGSRESET                                                    450
         AG_SEEN = 1                                                      450
         AG_COLO = 2                                                      450
         AG_TITLE = 'TONX'                                                450
         AG_EXNAME = 'ATTRIBUTE'                                          450
         AG_PARLIST = 'SEEN_COLO'                                         450
         CALL AXATTRIBUTE                                                 450
C    *                                                                    450
C    Geant SHAPE : TUBE ===> RMIN=TONXIR <===                             450
         CALL AGSRESET                                                    451
         AG_RMIN = TONXIR                                                 451
         AG_TITLE = 'TUBE'                                                451
         AG_EXNAME = 'SHAPE'                                              451
         AG_PARLIST = 'RMIN'                                              451
         CALL AXSHAPE                                                     451
C    *                                                                    451
C    Create TOAD :                                                        452
C    Call To Local Procedure TOAD                                         452
         ASSIGN 5410 TO BLOCK_TOAD                                        452
         GO TO  5400                                                      452
5410     CONTINUE                                                         452
C    *                                                                    452
C    Geant POSITION : TOAD ===> inherited arguments used <===             452
         CALL AGSRESET                                                    452
         AG_TITLE = 'TOAD'                                                452
         AG_EXNAME = 'POSITION'                                           452
         AG_PARLIST = 'NONE'                                              452
         CALL AXPOSITION                                                  452
C    *                                                                    452
5381  CALL AGSPOPB                                                        452
      GO TO BLOCK_TONX, (5380 ,5390)                                      453
C End of Local Procedure                                                  453
*
C Begin Local Procedure TOAD                                              455
C TOAD IS ADHESIVE LAYER                                                  455
5400  CONTINUE                                                            455
         AG_TITLE='TOAD'                                                  455
         AG_MODULE=MODULE                                                 455
         AG_MARK='V00'                                                    455
         CALL AGSPUSH                                                     455
         AG_MODULE=MODULE                                                 455
C    *                                                                    455
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            456
         CALL AGSRESET                                                    456
         AG_TITLE = 'MYLAR'                                               456
         AG_EXNAME = 'MATERIAL'                                           456
         AG_PARLIST = 'NONE'                                              456
         CALL AXMATERIAL                                                  456
C    *                                                                    456
C    Geant ATTRIBUTE : TOAD ===> SEEN=1 COLO=7 <===                       456
         CALL AGSRESET                                                    457
         AG_SEEN = 1                                                      457
         AG_COLO = 7                                                      457
         AG_TITLE = 'TOAD'                                                457
         AG_EXNAME = 'ATTRIBUTE'                                          457
         AG_PARLIST = 'SEEN_COLO'                                         457
         CALL AXATTRIBUTE                                                 457
C    *                                                                    457
C    Geant SHAPE : TUBE ===> RMIN=TOADIR <===                             457
         CALL AGSRESET                                                    458
         AG_RMIN = TOADIR                                                 458
         AG_TITLE = 'TUBE'                                                458
         AG_EXNAME = 'SHAPE'                                              458
         AG_PARLIST = 'RMIN'                                              458
         CALL AXSHAPE                                                     458
C    *                                                                    458
5401  CALL AGSPOPB                                                        458
      GO TO BLOCK_TOAD, (5400 ,5410)                                      459
C End of Local Procedure                                                  459
*
*------------------------------------------------------------------------------
*** Contents of Outer Field Cage Support TOST:  Aluminum/Honecomb
*   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Begin Local Procedure TOST                                              464
C TOST IS THE OUTER FIELD CAGE SUPPORT                                    464
5340  CONTINUE                                                            464
         AG_TITLE='TOST'                                                  464
         AG_MODULE=MODULE                                                 464
         AG_MARK='V00'                                                    464
         CALL AGSPUSH                                                     464
         AG_MODULE=MODULE                                                 464
C    *                                                                    464
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        465
         CALL AGSRESET                                                    465
         AG_TITLE = 'ALUMINIUM'                                           465
         AG_EXNAME = 'MATERIAL'                                           465
         AG_PARLIST = 'NONE'                                              465
         CALL AXMATERIAL                                                  465
C    *                                                                    465
C    Geant ATTRIBUTE : TOST ===> SEEN=1 COLO=2 <===                       465
         CALL AGSRESET                                                    466
         AG_SEEN = 1                                                      466
         AG_COLO = 2                                                      466
         AG_TITLE = 'TOST'                                                466
         AG_EXNAME = 'ATTRIBUTE'                                          466
         AG_PARLIST = 'SEEN_COLO'                                         466
         CALL AXATTRIBUTE                                                 466
C    *                                                                    466
C    Geant SHAPE : TUBE ===> RMIN=TOALIR <===                             466
         CALL AGSRESET                                                    467
         AG_RMIN = TOALIR                                                 467
         AG_TITLE = 'TUBE'                                                467
         AG_EXNAME = 'SHAPE'                                              467
         AG_PARLIST = 'RMIN'                                              467
         CALL AXSHAPE                                                     467
C    *                                                                    467
C    Create TOHA :                                                        468
C    Call To Local Procedure TOHA                                         468
         ASSIGN 5430 TO BLOCK_TOHA                                        468
         GO TO  5420                                                      468
5430     CONTINUE                                                         468
C    *                                                                    468
C    Geant POSITION : TOHA ===> inherited arguments used <===             468
         CALL AGSRESET                                                    468
         AG_TITLE = 'TOHA'                                                468
         AG_EXNAME = 'POSITION'                                           468
         AG_PARLIST = 'NONE'                                              468
         CALL AXPOSITION                                                  468
C    *                                                                    468
5341  CALL AGSPOPB                                                        468
      GO TO BLOCK_TOST, (5340 ,5350)                                      469
C End of Local Procedure                                                  469
*
C Begin Local Procedure TOHA                                              471
C TOHA IS HONEYCOMB/ADHESIVE MIXTURE                                      471
5420  CONTINUE                                                            471
         AG_TITLE='TOHA'                                                  471
         AG_MODULE=MODULE                                                 471
         AG_MARK='V00'                                                    471
         CALL AGSPUSH                                                     471
         AG_MODULE=MODULE                                                 471
C    *                                                                    471
C    Geant COMPONENT : AL ===> A=27 Z=13 W=0.0105 <===                    472
         CALL AGSRESET                                                    472
         AG_A = 27                                                        472
         AG_Z = 13                                                        472
         AG_W = 0.0105                                                    472
         AG_TITLE = 'AL'                                                  472
         AG_EXNAME = 'COMPONENT'                                          472
         AG_PARLIST = 'A_Z_W'                                             472
         CALL AXCOMPONENT                                                 472
C    *                                                                    472
C    Geant COMPONENT : N ===> A=14 Z=7 W=0.7395 <===                      472
         CALL AGSRESET                                                    473
         AG_A = 14                                                        473
         AG_Z = 7                                                         473
         AG_W = 0.7395                                                    473
         AG_TITLE = 'N'                                                   473
         AG_EXNAME = 'COMPONENT'                                          473
         AG_PARLIST = 'A_Z_W'                                             473
         CALL AXCOMPONENT                                                 473
C    *                                                                    473
C    Geant COMPONENT : ADHESIVE ===> A=9 Z=4.5 W=0.2500 <===              473
         CALL AGSRESET                                                    474
         AG_A = 9                                                         474
         AG_Z = 4.5                                                       474
         AG_W = 0.2500                                                    474
         AG_TITLE = 'ADHESIVE'                                            474
         AG_EXNAME = 'COMPONENT'                                          474
         AG_PARLIST = 'A_Z_W'                                             474
         CALL AXCOMPONENT                                                 474
C    *                                                                    474
C    Geant MIXTURE : AL_HONEYCOMB ===> DENS=0.282 <===                    474
         CALL AGSRESET                                                    475
         AG_DENS = 0.282                                                  475
         AG_TITLE = 'AL_HONEYCOMB'                                        475
         AG_EXNAME = 'MIXTURE'                                            475
         AG_PARLIST = 'DENS'                                              475
         CALL AXMIXTURE                                                   475
C    *                                                                    475
C    Geant ATTRIBUTE : TOHA ===> SEEN=1 COLO=7 <===                       475
         CALL AGSRESET                                                    476
         AG_SEEN = 1                                                      476
         AG_COLO = 7                                                      476
         AG_TITLE = 'TOHA'                                                476
         AG_EXNAME = 'ATTRIBUTE'                                          476
         AG_PARLIST = 'SEEN_COLO'                                         476
         CALL AXATTRIBUTE                                                 476
C    *                                                                    476
C    Geant SHAPE : TUBE ===> RMIN=TOHAIR <===                             476
         CALL AGSRESET                                                    477
         AG_RMIN = TOHAIR                                                 477
         AG_TITLE = 'TUBE'                                                477
         AG_EXNAME = 'SHAPE'                                              477
         AG_PARLIST = 'RMIN'                                              477
         CALL AXSHAPE                                                     477
C    *                                                                    477
5421  CALL AGSPOPB                                                        477
      GO TO BLOCK_TOHA, (5420 ,5430)                                      478
C End of Local Procedure                                                  478
*------------------------------------------------------------------------------
C Begin Local Procedure TPCM                                              481
C TPCM IS THE CENTRAL MEMBRANE PLACED IN TPC                              481
5120  CONTINUE                                                            481
         AG_TITLE='TPCM'                                                  481
         AG_MODULE=MODULE                                                 481
         AG_MARK='V00'                                                    481
         CALL AGSPUSH                                                     481
         AG_MODULE=MODULE                                                 481
C    *                                                                    481
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            482
         CALL AGSRESET                                                    482
         AG_TITLE = 'MYLAR'                                               482
         AG_EXNAME = 'MATERIAL'                                           482
         AG_PARLIST = 'NONE'                                              482
         CALL AXMATERIAL                                                  482
C    *                                                                    482
C    Geant ATTRIBUTE : TPCM ===> SEEN=1 COLO=4 <===                       482
         CALL AGSRESET                                                    483
         AG_SEEN = 1                                                      483
         AG_COLO = 4                                                      483
         AG_TITLE = 'TPCM'                                                483
         AG_EXNAME = 'ATTRIBUTE'                                          483
         AG_PARLIST = 'SEEN_COLO'                                         483
         CALL AXATTRIBUTE                                                 483
C    *                                                                    483
*     Medium    dense_membrane  SteMax=1  " just to show that we define it "
C    Geant SHAPE : TUBE ===> RMIN=TPGVIR RMAX=TPCG_SENGASOR DZ=TPCG_MEMB  485
C    THK/2 <===                                                           485
         CALL AGSRESET                                                    485
         AG_RMIN = TPGVIR                                                 485
         AG_RMAX = TPCG_SENGASOR                                          485
         AG_DZ = TPCG_MEMBTHK/2                                           485
         AG_TITLE = 'TUBE'                                                485
         AG_EXNAME = 'SHAPE'                                              485
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      485
         CALL AXSHAPE                                                     485
C    *                                                                    485
5121  CALL AGSPOPB                                                        485
      GO TO BLOCK_TPCM, (5120 ,5130)                                      486
C End of Local Procedure                                                  486
*
******************************************************************************
*                           section three - endcaps                          *
******************************************************************************
C Begin Local Procedure TPEA                                              491
C TPEA IS ONE ENDCAP PLACED IN TPC                                        491
5140  CONTINUE                                                            491
         AG_TITLE='TPEA'                                                  491
         AG_MODULE=MODULE                                                 491
         AG_MARK='V00'                                                    491
         CALL AGSPUSH                                                     491
         AG_MODULE=MODULE                                                 491
C    *                                                                    491
C    Geant MATERIAL : P10 ===> inherited arguments used <===              492
         CALL AGSRESET                                                    492
         AG_TITLE = 'P10'                                                 492
         AG_EXNAME = 'MATERIAL'                                           492
         AG_PARLIST = 'NONE'                                              492
         CALL AXMATERIAL                                                  492
C    *                                                                    492
C    Geant ATTRIBUTE : TPEA ===> SEEN=0 <===                              492
         CALL AGSRESET                                                    493
         AG_SEEN = 0                                                      493
         AG_TITLE = 'TPEA'                                                493
         AG_EXNAME = 'ATTRIBUTE'                                          493
         AG_PARLIST = 'SEEN'                                              493
         CALL AXATTRIBUTE                                                 493
C    *                                                                    493
C    Geant SHAPE : TUBE ===> RMIN=TPGVIR RMAX=TPCG_SENGASOR DZ=TPCG_TPEA  493
C    THK/2 <===                                                           493
         CALL AGSRESET                                                    494
         AG_RMIN = TPGVIR                                                 494
         AG_RMAX = TPCG_SENGASOR                                          494
         AG_DZ = TPCG_TPEATHK/2                                           494
         AG_TITLE = 'TUBE'                                                494
         AG_EXNAME = 'SHAPE'                                              494
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      494
         CALL AXSHAPE                                                     494
C    *                                                                    494
C    Create TESS :                                                        495
C    Call To Local Procedure TESS                                         495
         ASSIGN 5450 TO BLOCK_TESS                                        495
         GO TO  5440                                                      495
5450     CONTINUE                                                         495
C    *                                                                    495
5141  CALL AGSPOPB                                                        496
      GO TO BLOCK_TPEA, (5140 ,5150)                                      496
C End of Local Procedure                                                  496
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Begin Local Procedure TESS                                              498
C TESS IS A DIVISION OF ENDCAP VOLUME CORRESPONDING TO ONE SUPERSECTOR    498
5440  CONTINUE                                                            498
         AG_TITLE='TESS'                                                  498
         AG_MODULE=MODULE                                                 498
         AG_MARK='V00'                                                    498
         CALL AGSPUSH                                                     498
         AG_MODULE=MODULE                                                 498
C    *                                                                    498
C    Geant ATTRIBUTE : TESS ===> SEEN=1 <===                              499
         CALL AGSRESET                                                    499
         AG_SEEN = 1                                                      499
         AG_TITLE = 'TESS'                                                499
         AG_EXNAME = 'ATTRIBUTE'                                          499
         AG_PARLIST = 'SEEN'                                              499
         CALL AXATTRIBUTE                                                 499
C    *                                                                    499
C    Geant SHAPE : DIVISION ===> NDIV=12 IAXIS=2 <===                     499
         CALL AGSRESET                                                    500
         AG_NDIV = 12                                                     500
         AG_IAXIS = 2                                                     500
         AG_TITLE = 'DIVISION'                                            500
         AG_EXNAME = 'SHAPE'                                              500
         AG_PARLIST = 'NDIV_IAXIS'                                        500
         CALL AXSHAPE                                                     500
C    *                                                                    500
C    *                                                                    502
         I_SEC=1                                                          502
            GO TO 5463                                                    502
5461        I_SEC=I_SEC+1                                                 502
5463        IF(I_SEC-(2).GT.0) GO TO 5462                                 502
C       use TECW bank                                                     503
            BPATH_TECW='TECW*'                                            503
            IDNUM_TECW(1)=0                                               503
            TECW_SEC=I_SEC                                                503
            CALL AGDGETP('TPCEGEO','*',-999,'TECW',NTECW_SEC,             503
     *      RE_TECW_SEC,BPATH_TECW,IDNUM_TECW,1,MAP_TECW,NAMES_TECW,      503
     *      BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)             503
*PN:     sectors are positioned by precision holes (drawing 24A7266)
C       Create TSEC :                                                     507
C       Call To Local Procedure TSEC                                      507
            ASSIGN 5480 TO BLOCK_TSEC                                     507
            GO TO  5470                                                   507
5480        CONTINUE                                                      507
C       *                                                                 507
C       Geant POSITION : TSEC ===> X=TECW_RCENTER, Z=(TPCG_TPEATHK-TECW_  507
C       ASDEPTH-TECW_PPDEPTH-TECW_GGDEPTH)/2, ORT=YZX <===                507
            CALL AGSRESET                                                 507
            AG_X = TECW_RCENTER                                           507
            AG_Z =                                                        507
     *      (TPCG_TPEATHK-TECW_ASDEPTH-TECW_PPDEPTH-TECW_GGDEPTH)/2       507
            AG_ALPHAN=AG_ALPHAN+1                                         507
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              507
            AG_TITLE = 'TSEC'                                             507
            AG_EXNAME = 'POSITION'                                        507
            AG_PARLIST = 'X_Z_ORT'                                        507
            CALL AXPOSITION                                               507
C       *                                                                 507
         GO TO 5461                                                       508
5462     CONTINUE                                                         508
5441  CALL AGSPOPB                                                        510
      GO TO BLOCK_TESS, (5440 ,5450)                                      510
C End of Local Procedure                                                  510
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Begin Local Procedure TSEC                                              512
C TSEC IS A SUPERSECTOR CONTAINING AL SECTOR, PCB AND MWC VOLUME          512
5470  CONTINUE                                                            512
         AG_TITLE='TSEC'                                                  512
         AG_MODULE=MODULE                                                 512
         AG_MARK='V00'                                                    512
         CALL AGSPUSH                                                     512
         AG_MODULE=MODULE                                                 512
C    *                                                                    512
*
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        514
         CALL AGSRESET                                                    514
         AG_TITLE = 'ALUMINIUM'                                           514
         AG_EXNAME = 'MATERIAL'                                           514
         AG_PARLIST = 'NONE'                                              514
         CALL AXMATERIAL                                                  514
C    *                                                                    514
C    Geant SHAPE : TRD1 ===> DX1=TECW_INWIDTH/2 DX2=TECW_OUWIDTH/2, DY=(  514
C    TECW_PPDEPTH+TECW_ASDEPTH+TECW_GGDEPTH)/2, DZ=TECW_HEIGHT/2 <===     514
         CALL AGSRESET                                                    517
         AG_DX1 = TECW_INWIDTH/2                                          517
         AG_DX2 = TECW_OUWIDTH/2                                          517
         AG_DY = (TECW_PPDEPTH+TECW_ASDEPTH+TECW_GGDEPTH)/2               517
         AG_DZ = TECW_HEIGHT/2                                            517
         AG_TITLE = 'TRD1'                                                517
         AG_EXNAME = 'SHAPE'                                              517
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     517
         CALL AXSHAPE                                                     517
C    *                                                                    517
*    First, produce the MWC as a whole, all internal structure put inside
C    Create TMWC :                                                        520
C    Call To Local Procedure TMWC                                         520
         ASSIGN 5500 TO BLOCK_TMWC                                        520
         GO TO  5490                                                      520
5500     CONTINUE                                                         520
C    *                                                                    520
C    Geant POSITION : TMWC ===> Y=-(TECW_PPDEPTH+TECW_ASDEPTH)/2 <===     520
         CALL AGSRESET                                                    520
         AG_Y = -(TECW_PPDEPTH+TECW_ASDEPTH)/2                            520
         AG_TITLE = 'TMWC'                                                520
         AG_EXNAME = 'POSITION'                                           520
         AG_PARLIST = 'Y'                                                 520
         CALL AXPOSITION                                                  520
C    *                                                                    520
*
*    PCB is not created separetely, so it is now included in Al sector volume,
*    it can be created in this block to replace an additional 0.32 layer of Al
*    like:  create and position TPCB  y=(tecw_GgDepth+tecw_PcbDepth)/2
*
C    Create TIAG :                                                        526
C    Call To Local Procedure TIAG                                         526
         ASSIGN 5520 TO BLOCK_TIAG                                        526
         GO TO  5510                                                      526
5520     CONTINUE                                                         526
C    *                                                                    526
*    trapezoids defining gaps have same angles as sector boundary
         DX_DZ = (TECW_OUWIDTH - TECW_INWIDTH)/(2*TECW_HEIGHT )           529
*    boundary width is given parallel to sector edge, get it parallel to x-axis
         DXB = TECW_BOUNDARY / SQRT( 1. + DX_DZ**2 )                      532
C    *                                                                    534
         I=1                                                              534
            GO TO 5533                                                    534
5531        I=I+1                                                         534
5533        IF(I-(NINT(TECW_N)).GT.0) GO TO 5532                          534
*        opening in pad back plane are at ouside and they contain extra pieces:
C       Geant POSITION : TIAG ===> KONLY='MANY', Z=-TECW_HEIGHT/2+TECW_Z  541
C       (I) Y=+TECW_PPDEPTH/2+TECW_GGDEPTH/2, DX1=TECW_INWIDTH/2-DXB+(TE  541
C       CW_Z(I)-TECW_DZ(I)/2)*DX_DZ, DX2=TECW_INWIDTH/2-DXB+(TECW_Z(I)+T  541
C       ECW_DZ(I)/2)*DX_DZ, DY=TECW_ASDEPTH/2, DZ=TECW_DZ(I)/2 <===       541
            CALL AGSRESET                                                 541
            AG_KONLY = 'MANY'                                             541
            AG_Z = -TECW_HEIGHT/2+TECW_Z(I)                               541
            AG_Y = +TECW_PPDEPTH/2+TECW_GGDEPTH/2                         541
            AG_DX1 = TECW_INWIDTH/2-DXB+(TECW_Z(I)-TECW_DZ(I)/2)*DX_DZ    541
            AG_DX2 = TECW_INWIDTH/2-DXB+(TECW_Z(I)+TECW_DZ(I)/2)*DX_DZ    541
            AG_DY = TECW_ASDEPTH/2                                        541
            AG_DZ = TECW_DZ(I)/2                                          541
            AG_TITLE = 'TIAG'                                             541
            AG_EXNAME = 'POSITION'                                        541
            AG_PARLIST = 'KONLY_Z_Y_DX1_DX2_DY_DZ'                        541
            CALL AXPOSITION                                               541
C       *                                                                 541
         GO TO 5531                                                       542
5532     CONTINUE                                                         542
*
C    Create TOAE :                                                        544
C    Call To Local Procedure TOAE                                         544
         ASSIGN 5550 TO BLOCK_TOAE                                        544
         GO TO  5540                                                      544
5550     CONTINUE                                                         544
C    *                                                                    544
C    *                                                                    545
         I=1                                                              545
            GO TO 5563                                                    545
5561        I=I+1                                                         545
5563        IF(I-(NINT(TECW_NEX)).GT.0) GO TO 5562                        545
C       Geant POSITION : TOAE ===> X=TECW_XEX(I) Y=+TECW_PPDEPTH/2+TECW_  548
C       GGDEPTH/2, Z=TECW_ZEX(I)-TECW_HEIGHT/2, DX=TECW_DXEX(I)/2 DY=TEC  548
C       W_ASDEPTH/2 DZ=TECW_DZEX(I)/2 <===                                548
            CALL AGSRESET                                                 548
            AG_X = TECW_XEX(I)                                            548
            AG_Y = +TECW_PPDEPTH/2+TECW_GGDEPTH/2                         548
            AG_Z = TECW_ZEX(I)-TECW_HEIGHT/2                              548
            AG_DX = TECW_DXEX(I)/2                                        548
            AG_DY = TECW_ASDEPTH/2                                        548
            AG_DZ = TECW_DZEX(I)/2                                        548
            AG_TITLE = 'TOAE'                                             548
            AG_EXNAME = 'POSITION'                                        548
            AG_PARLIST = 'X_Y_Z_DX_DY_DZ'                                 548
            CALL AXPOSITION                                               548
C       *                                                                 548
         GO TO 5561                                                       549
5562     CONTINUE                                                         549
5471  CALL AGSPOPB                                                        550
      GO TO BLOCK_TSEC, (5470 ,5480)                                      550
C End of Local Procedure                                                  550
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Begin Local Procedure TMWC                                              552
C TMWC IS A WIRE CHAMBER VOLUME WITH BOTH GATED AND SENSITIVE VOLUMES     552
5490  CONTINUE                                                            552
         AG_TITLE='TMWC'                                                  552
         AG_MODULE=MODULE                                                 552
         AG_MARK='V00'                                                    552
         CALL AGSPUSH                                                     552
         AG_MODULE=MODULE                                                 552
C    *                                                                    552
* Wire grids are not created now. If needed, they should be included as thin
* planes at the leftmost position of TMWC and TMSE
C    Geant MATERIAL : P10 ===> inherited arguments used <===              555
         CALL AGSRESET                                                    555
         AG_TITLE = 'P10'                                                 555
         AG_EXNAME = 'MATERIAL'                                           555
         AG_PARLIST = 'NONE'                                              555
         CALL AXMATERIAL                                                  555
C    *                                                                    555
C    Geant ATTRIBUTE : TMWC ===> SEEN=1 COLO=3 <===                       555
         CALL AGSRESET                                                    556
         AG_SEEN = 1                                                      556
         AG_COLO = 3                                                      556
         AG_TITLE = 'TMWC'                                                556
         AG_EXNAME = 'ATTRIBUTE'                                          556
         AG_PARLIST = 'SEEN_COLO'                                         556
         CALL AXATTRIBUTE                                                 556
C    *                                                                    556
C    Geant SHAPE : TRD1 ===> DX1=TECW_INWIDTH/2 DX2=TECW_OUWIDTH/2, DY=T  556
C    ECW_GGDEPTH/2, DZ=TECW_HEIGHT/2 <===                                 556
         CALL AGSRESET                                                    558
         AG_DX1 = TECW_INWIDTH/2                                          558
         AG_DX2 = TECW_OUWIDTH/2                                          558
         AG_DY = TECW_GGDEPTH/2                                           558
         AG_DZ = TECW_HEIGHT/2                                            558
         AG_TITLE = 'TRD1'                                                558
         AG_EXNAME = 'SHAPE'                                              558
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     558
         CALL AXSHAPE                                                     558
C    *                                                                    558
C    Skip Unless TPCG_MWCREAD>0                                           558
         IF (TPCG_MWCREAD.LE.0)GO TO 5491                                 559
C    Create TMEA :                                                        561
C    Call To Local Procedure TMEA                                         561
         ASSIGN 5580 TO BLOCK_TMEA                                        561
         GO TO  5570                                                      561
5580     CONTINUE                                                         561
C    *                                                                    561
C    Geant POSITION : TMEA ===> Y=+(TECW_GGDEPTH-TECW_MWCDEPTH)/2, Z=TEC  561
C    W_MWCCENT <===                                                       561
         CALL AGSRESET                                                    561
         AG_Y = +(TECW_GGDEPTH-TECW_MWCDEPTH)/2                           561
         AG_Z = TECW_MWCCENT                                              561
         AG_TITLE = 'TMEA'                                                561
         AG_EXNAME = 'POSITION'                                           561
         AG_PARLIST = 'Y_Z'                                               561
         CALL AXPOSITION                                                  561
C    *                                                                    561
5491  CALL AGSPOPB                                                        563
      GO TO BLOCK_TMWC, (5490 ,5500)                                      563
C End of Local Procedure                                                  563
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Begin Local Procedure TMEA                                              565
C TMEA IS A DOUBLE SENSITIVE LAYER AROUND GATING GRID                     565
5570  CONTINUE                                                            565
         AG_TITLE='TMEA'                                                  565
         AG_MODULE=MODULE                                                 565
         AG_MARK='V00'                                                    565
         CALL AGSPUSH                                                     565
         AG_MODULE=MODULE                                                 565
C    *                                                                    565
C    Geant MATERIAL : P10 ===> inherited arguments used <===              566
         CALL AGSRESET                                                    566
         AG_TITLE = 'P10'                                                 566
         AG_EXNAME = 'MATERIAL'                                           566
         AG_PARLIST = 'NONE'                                              566
         CALL AXMATERIAL                                                  566
C    *                                                                    566
C    Geant MATERIAL : SENSITIVE_GAS ===> ISVOL=1 STEMAX=5 <===            566
         CALL AGSRESET                                                    567
         AG_ISVOL = 1                                                     567
         AG_STEMAX = 5                                                    567
         AG_TITLE = 'SENSITIVE_GAS'                                       567
         AG_EXNAME = 'MATERIAL'                                           567
         AG_PARLIST = 'ISVOL_STEMAX'                                      567
         CALL AXMATERIAL                                                  567
C    *                                                                    567
C    Geant ATTRIBUTE : TMEA ===> SEEN=1 COLO=4 <===                       567
         CALL AGSRESET                                                    568
         AG_SEEN = 1                                                      568
         AG_COLO = 4                                                      568
         AG_TITLE = 'TMEA'                                                568
         AG_EXNAME = 'ATTRIBUTE'                                          568
         AG_PARLIST = 'SEEN_COLO'                                         568
         CALL AXATTRIBUTE                                                 568
C    *                                                                    568
C    Geant SHAPE : TRD1 ===> DX1=TECW_MWCINN/2 DX2=TECW_MWCOUT/2, DY=TEC  568
C    W_MWCDEPTH/2 DZ=TECW_MWCHEI/2 <===                                   568
         CALL AGSRESET                                                    570
         AG_DX1 = TECW_MWCINN/2                                           570
         AG_DX2 = TECW_MWCOUT/2                                           570
         AG_DY = TECW_MWCDEPTH/2                                          570
         AG_DZ = TECW_MWCHEI/2                                            570
         AG_TITLE = 'TRD1'                                                570
         AG_EXNAME = 'SHAPE'                                              570
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     570
         CALL AXSHAPE                                                     570
C    *                                                                    570
C    Create TMSE :                                                        571
C    Call To Local Procedure TMSE                                         571
         ASSIGN 5600 TO BLOCK_TMSE                                        571
         GO TO  5590                                                      571
5600     CONTINUE                                                         571
C    *                                                                    571
         CALL GSTPAR(AG_IMED,'STRA',1.)                                   572
5571  CALL AGSPOPB                                                        573
      GO TO BLOCK_TMEA, (5570 ,5580)                                      573
C End of Local Procedure                                                  573
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Begin Local Procedure TMSE                                              575
C TMSE IS A SINGLE SENSITIVE VOLUME                                       575
5590  CONTINUE                                                            575
         AG_TITLE='TMSE'                                                  575
         AG_MODULE=MODULE                                                 575
         AG_MARK='V00'                                                    575
         CALL AGSPUSH                                                     575
         AG_MODULE=MODULE                                                 575
C    *                                                                    575
C    Geant SHAPE : DIVISION ===> IAXIS=3 NDIV=TECW_MWCNWIR <===           577
         CALL AGSRESET                                                    577
         AG_IAXIS = 3                                                     577
         AG_NDIV = TECW_MWCNWIR                                           577
         AG_TITLE = 'DIVISION'                                            577
         AG_EXNAME = 'SHAPE'                                              577
         AG_PARLIST = 'IAXIS_NDIV'                                        577
         CALL AXSHAPE                                                     577
C    *                                                                    577
C    Check TPCG_MWCREAD==1                                                577
         IF (TPCG_MWCREAD.EQ.1) THEN                                      578
*     until 10-03-01 AGI was not seting proper dimensions here, force to max:
C    Defining Hits: Z:.01:S Y:.01: X:.01:(-TECW_MWCOUT/2,TECW_MWCOUT/2),  584
C     CX:10: CY:10: CZ:10:, SLENG:0.1:(0,800) STEP:.01:, TOF:16:(0,1.E-6  584
C    ) PTOT:16:(0,100), LGAM:16:(-2,2) ELOS:16:(0,0.01)                   584
         AG_TITLE = 'TMSE'                                                584
         AG_EXNAME = 'HITS'                                               584
         AG_TYPE=AG_MAGIC                                                 584
         CALL AGSDETV('TMSE')                                             584
C    *                                                                    584
         AG_PARLIST='Z'                                                   584
         AG_TITLE = 'DETH'                                                584
         AG_NBITS = AG_MAGIC                                              584
         AG_BIN=.01                                                       584
         AG_OPTION='S'                                                    584
         AG_FHMIN = AG_MAGIC                                              584
         AG_FHMAX = AG_MAGIC                                              584
         CALL AGSDETH (0)                                                 584
C    *                                                                    584
         AG_PARLIST='Y'                                                   584
         AG_TITLE = 'DETH'                                                584
         AG_NBITS = AG_MAGIC                                              584
         AG_BIN=.01                                                       584
         AG_FHMIN=AG_MAGIC                                                584
         AG_FHMAX=AG_MAGIC                                                584
         AG_OPTION='*'                                                    584
         CALL AGSDETH (0)                                                 584
C    *                                                                    584
         AG_PARLIST='X'                                                   584
         AG_TITLE = 'DETH'                                                584
         AG_NBITS = AG_MAGIC                                              584
         AG_BIN=.01                                                       584
         AG_OPTION='*'                                                    584
         AG_FHMIN = -TECW_MWCOUT/2                                        584
         AG_FHMAX = TECW_MWCOUT/2                                         584
         CALL AGSDETH (0)                                                 584
C    *                                                                    584
         AG_PARLIST='CX'                                                  584
         AG_TITLE = 'DETH'                                                584
         AG_NBITS = 10                                                    584
         AG_BIN=AG_MAGIC                                                  584
         AG_FHMIN=AG_MAGIC                                                584
         AG_FHMAX=AG_MAGIC                                                584
         AG_OPTION='*'                                                    584
         CALL AGSDETH (0)                                                 584
C    *                                                                    584
         AG_PARLIST='CY'                                                  584
         AG_TITLE = 'DETH'                                                584
         AG_NBITS = 10                                                    584
         AG_BIN=AG_MAGIC                                                  584
         AG_FHMIN=AG_MAGIC                                                584
         AG_FHMAX=AG_MAGIC                                                584
         AG_OPTION='*'                                                    584
         CALL AGSDETH (0)                                                 584
C    *                                                                    584
         AG_PARLIST='CZ'                                                  584
         AG_TITLE = 'DETH'                                                584
         AG_NBITS = 10                                                    584
         AG_BIN=AG_MAGIC                                                  584
         AG_OPTION='*'                                                    584
         AG_FHMIN = AG_MAGIC                                              584
         AG_FHMAX = AG_MAGIC                                              584
         CALL AGSDETH (0)                                                 584
C    *                                                                    584
         AG_PARLIST='SLENG'                                               584
         AG_TITLE = 'DETH'                                                584
         AG_NBITS = AG_MAGIC                                              584
         AG_BIN=0.1                                                       584
         AG_OPTION='*'                                                    584
         AG_FHMIN = 0                                                     584
         AG_FHMAX = 800                                                   584
         CALL AGSDETH (0)                                                 584
C    *                                                                    584
         AG_PARLIST='STEP'                                                584
         AG_TITLE = 'DETH'                                                584
         AG_NBITS = AG_MAGIC                                              584
         AG_BIN=.01                                                       584
         AG_OPTION='*'                                                    584
         AG_FHMIN = AG_MAGIC                                              584
         AG_FHMAX = AG_MAGIC                                              584
         CALL AGSDETH (0)                                                 584
C    *                                                                    584
         AG_PARLIST='TOF'                                                 584
         AG_TITLE = 'DETH'                                                584
         AG_NBITS = 16                                                    584
         AG_BIN=AG_MAGIC                                                  584
         AG_OPTION='*'                                                    584
         AG_FHMIN = 0                                                     584
         AG_FHMAX = 1.E-6                                                 584
         CALL AGSDETH (0)                                                 584
C    *                                                                    584
         AG_PARLIST='PTOT'                                                584
         AG_TITLE = 'DETH'                                                584
         AG_NBITS = 16                                                    584
         AG_BIN=AG_MAGIC                                                  584
         AG_OPTION='*'                                                    584
         AG_FHMIN = 0                                                     584
         AG_FHMAX = 100                                                   584
         CALL AGSDETH (0)                                                 584
C    *                                                                    584
         AG_PARLIST='LGAM'                                                584
         AG_TITLE = 'DETH'                                                584
         AG_NBITS = 16                                                    584
         AG_BIN=AG_MAGIC                                                  584
         AG_OPTION='*'                                                    584
         AG_FHMIN = -2                                                    584
         AG_FHMAX = 2                                                     584
         CALL AGSDETH (0)                                                 584
C    *                                                                    584
         AG_PARLIST='ELOS'                                                584
         AG_TITLE = 'DETH'                                                584
         AG_NBITS = 16                                                    584
         AG_BIN=AG_MAGIC                                                  584
         AG_OPTION='*'                                                    584
         AG_FHMIN = 0                                                     584
         AG_FHMAX = 0.01                                                  584
         CALL AGSDETH (0)                                                 584
C    *                                                                    584
         CALL AGSDETU('TMSE')                                             584
C    *                                                                    584
         ELSE                                                             585
C    Defining Hits: Z:.01:S Y:.01: X:.01:, CX:10: CY:10: CZ:10:, SLENG:0  590
C    .1:(0,800) STEP:.01:, TOF:16:(0,1.E-6) PTOT:16:(0,100), LGAM:16:(-2  590
C    ,2) ELOS:16:(0,0.01)                                                 590
         AG_TITLE = 'TMSE'                                                590
         AG_EXNAME = 'HITS'                                               590
         AG_TYPE=AG_MAGIC                                                 590
         CALL AGSDETV('TMSE')                                             590
C    *                                                                    590
         AG_PARLIST='Z'                                                   590
         AG_TITLE = 'DETH'                                                590
         AG_NBITS = AG_MAGIC                                              590
         AG_BIN=.01                                                       590
         AG_OPTION='S'                                                    590
         AG_FHMIN = AG_MAGIC                                              590
         AG_FHMAX = AG_MAGIC                                              590
         CALL AGSDETH (0)                                                 590
C    *                                                                    590
         AG_PARLIST='Y'                                                   590
         AG_TITLE = 'DETH'                                                590
         AG_NBITS = AG_MAGIC                                              590
         AG_BIN=.01                                                       590
         AG_FHMIN=AG_MAGIC                                                590
         AG_FHMAX=AG_MAGIC                                                590
         AG_OPTION='*'                                                    590
         CALL AGSDETH (0)                                                 590
C    *                                                                    590
         AG_PARLIST='X'                                                   590
         AG_TITLE = 'DETH'                                                590
         AG_NBITS = AG_MAGIC                                              590
         AG_BIN=.01                                                       590
         AG_OPTION='*'                                                    590
         AG_FHMIN = AG_MAGIC                                              590
         AG_FHMAX = AG_MAGIC                                              590
         CALL AGSDETH (0)                                                 590
C    *                                                                    590
         AG_PARLIST='CX'                                                  590
         AG_TITLE = 'DETH'                                                590
         AG_NBITS = 10                                                    590
         AG_BIN=AG_MAGIC                                                  590
         AG_FHMIN=AG_MAGIC                                                590
         AG_FHMAX=AG_MAGIC                                                590
         AG_OPTION='*'                                                    590
         CALL AGSDETH (0)                                                 590
C    *                                                                    590
         AG_PARLIST='CY'                                                  590
         AG_TITLE = 'DETH'                                                590
         AG_NBITS = 10                                                    590
         AG_BIN=AG_MAGIC                                                  590
         AG_FHMIN=AG_MAGIC                                                590
         AG_FHMAX=AG_MAGIC                                                590
         AG_OPTION='*'                                                    590
         CALL AGSDETH (0)                                                 590
C    *                                                                    590
         AG_PARLIST='CZ'                                                  590
         AG_TITLE = 'DETH'                                                590
         AG_NBITS = 10                                                    590
         AG_BIN=AG_MAGIC                                                  590
         AG_OPTION='*'                                                    590
         AG_FHMIN = AG_MAGIC                                              590
         AG_FHMAX = AG_MAGIC                                              590
         CALL AGSDETH (0)                                                 590
C    *                                                                    590
         AG_PARLIST='SLENG'                                               590
         AG_TITLE = 'DETH'                                                590
         AG_NBITS = AG_MAGIC                                              590
         AG_BIN=0.1                                                       590
         AG_OPTION='*'                                                    590
         AG_FHMIN = 0                                                     590
         AG_FHMAX = 800                                                   590
         CALL AGSDETH (0)                                                 590
C    *                                                                    590
         AG_PARLIST='STEP'                                                590
         AG_TITLE = 'DETH'                                                590
         AG_NBITS = AG_MAGIC                                              590
         AG_BIN=.01                                                       590
         AG_OPTION='*'                                                    590
         AG_FHMIN = AG_MAGIC                                              590
         AG_FHMAX = AG_MAGIC                                              590
         CALL AGSDETH (0)                                                 590
C    *                                                                    590
         AG_PARLIST='TOF'                                                 590
         AG_TITLE = 'DETH'                                                590
         AG_NBITS = 16                                                    590
         AG_BIN=AG_MAGIC                                                  590
         AG_OPTION='*'                                                    590
         AG_FHMIN = 0                                                     590
         AG_FHMAX = 1.E-6                                                 590
         CALL AGSDETH (0)                                                 590
C    *                                                                    590
         AG_PARLIST='PTOT'                                                590
         AG_TITLE = 'DETH'                                                590
         AG_NBITS = 16                                                    590
         AG_BIN=AG_MAGIC                                                  590
         AG_OPTION='*'                                                    590
         AG_FHMIN = 0                                                     590
         AG_FHMAX = 100                                                   590
         CALL AGSDETH (0)                                                 590
C    *                                                                    590
         AG_PARLIST='LGAM'                                                590
         AG_TITLE = 'DETH'                                                590
         AG_NBITS = 16                                                    590
         AG_BIN=AG_MAGIC                                                  590
         AG_OPTION='*'                                                    590
         AG_FHMIN = -2                                                    590
         AG_FHMAX = 2                                                     590
         CALL AGSDETH (0)                                                 590
C    *                                                                    590
         AG_PARLIST='ELOS'                                                590
         AG_TITLE = 'DETH'                                                590
         AG_NBITS = 16                                                    590
         AG_BIN=AG_MAGIC                                                  590
         AG_OPTION='*'                                                    590
         AG_FHMIN = 0                                                     590
         AG_FHMAX = 0.01                                                  590
         CALL AGSDETH (0)                                                 590
C    *                                                                    590
         CALL AGSDETU('TMSE')                                             590
C    *                                                                    590
         ENDIF                                                            591
5591  CALL AGSPOPB                                                        592
      GO TO BLOCK_TMSE, (5590 ,5600)                                      592
C End of Local Procedure                                                  592
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Begin Local Procedure TIAG                                              594
C TIAG IS AN AIR GAP IN INNER SECTOR ALUMINUM STRUCTURE                   594
5510  CONTINUE                                                            594
         AG_TITLE='TIAG'                                                  594
         AG_MODULE=MODULE                                                 594
         AG_MARK='V00'                                                    594
         CALL AGSPUSH                                                     594
         AG_MODULE=MODULE                                                 594
C    *                                                                    594
*     in reality they are filled with electronics PCB (PN!)
C    Geant MATERIAL : AIR ===> inherited arguments used <===              596
         CALL AGSRESET                                                    596
         AG_TITLE = 'AIR'                                                 596
         AG_EXNAME = 'MATERIAL'                                           596
         AG_PARLIST = 'NONE'                                              596
         CALL AXMATERIAL                                                  596
C    *                                                                    596
C    Geant SHAPE : TRD1 ===> DX1=0 DX2=0 DY=0 DZ=0 <===                   596
         CALL AGSRESET                                                    597
         AG_DX1 = 0                                                       597
         AG_DX2 = 0                                                       597
         AG_DY = 0                                                        597
         AG_DZ = 0                                                        597
         AG_TITLE = 'TRD1'                                                597
         AG_EXNAME = 'SHAPE'                                              597
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     597
         CALL AXSHAPE                                                     597
C    *                                                                    597
5511  CALL AGSPOPB                                                        597
      GO TO BLOCK_TIAG, (5510 ,5520)                                      598
C End of Local Procedure                                                  598
*
C Begin Local Procedure TOAE                                              600
C TOAE IS EXTRA ALUMINUM SUPPORTS IN THE AIR OPENNINGS                    600
5540  CONTINUE                                                            600
         AG_TITLE='TOAE'                                                  600
         AG_MODULE=MODULE                                                 600
         AG_MARK='V00'                                                    600
         CALL AGSPUSH                                                     600
         AG_MODULE=MODULE                                                 600
C    *                                                                    600
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        601
         CALL AGSRESET                                                    601
         AG_TITLE = 'ALUMINIUM'                                           601
         AG_EXNAME = 'MATERIAL'                                           601
         AG_PARLIST = 'NONE'                                              601
         CALL AXMATERIAL                                                  601
C    *                                                                    601
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           601
         CALL AGSRESET                                                    602
         AG_DX = 0                                                        602
         AG_DY = 0                                                        602
         AG_DZ = 0                                                        602
         AG_TITLE = 'BOX'                                                 602
         AG_EXNAME = 'SHAPE'                                              602
         AG_PARLIST = 'DX_DY_DZ'                                          602
         CALL AXSHAPE                                                     602
C    *                                                                    602
5541  CALL AGSPOPB                                                        602
      GO TO BLOCK_TOAE, (5540 ,5550)                                      603
C End of Local Procedure                                                  603
*
*******************************************************************************
*                   section four - support wheels
*******************************************************************************
*
C Begin Local Procedure TPCW                                              609
C TPCW IN THE TPC SUPPORTING ENDCAP WHEEL                                 609
5160  CONTINUE                                                            609
         AG_TITLE='TPCW'                                                  609
         AG_MODULE=MODULE                                                 609
         AG_MARK='V00'                                                    609
         CALL AGSPUSH                                                     609
         AG_MODULE=MODULE                                                 609
C    *                                                                    609
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        610
         CALL AGSRESET                                                    610
         AG_TITLE = 'ALUMINIUM'                                           610
         AG_EXNAME = 'MATERIAL'                                           610
         AG_PARLIST = 'NONE'                                              610
         CALL AXMATERIAL                                                  610
C    *                                                                    610
C    Geant ATTRIBUTE : TPEA ===> SEEN=1 <===                              610
         CALL AGSRESET                                                    611
         AG_SEEN = 1                                                      611
         AG_TITLE = 'TPEA'                                                611
         AG_EXNAME = 'ATTRIBUTE'                                          611
         AG_PARLIST = 'SEEN'                                              611
         CALL AXATTRIBUTE                                                 611
C    *                                                                    611
C    Geant SHAPE : TUBE ===> RMIN=TPCG_WHEELIR RMAX=TPCG_WHEELOR DZ=TPCG  611
C    _WHEELTHK/2 <===                                                     611
         CALL AGSRESET                                                    612
         AG_RMIN = TPCG_WHEELIR                                           612
         AG_RMAX = TPCG_WHEELOR                                           612
         AG_DZ = TPCG_WHEELTHK/2                                          612
         AG_TITLE = 'TUBE'                                                612
         AG_EXNAME = 'SHAPE'                                              612
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      612
         CALL AXSHAPE                                                     612
C    *                                                                    612
C    Create TWSS :                                                        613
C    Call To Local Procedure TWSS                                         613
         ASSIGN 5620 TO BLOCK_TWSS                                        613
         GO TO  5610                                                      613
5620     CONTINUE                                                         613
C    *                                                                    613
5161  CALL AGSPOPB                                                        614
      GO TO BLOCK_TPCW, (5160 ,5170)                                      614
C End of Local Procedure                                                  614
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Begin Local Procedure TWSS                                              616
C TWSS IS A DIVISION OF WHEEL CORRESPONDING TO SUPERSECTORS               616
5610  CONTINUE                                                            616
         AG_TITLE='TWSS'                                                  616
         AG_MODULE=MODULE                                                 616
         AG_MARK='V00'                                                    616
         CALL AGSPUSH                                                     616
         AG_MODULE=MODULE                                                 616
C    *                                                                    616
C    Geant ATTRIBUTE : TWSS ===> SEEN=0 <===                              617
         CALL AGSRESET                                                    617
         AG_SEEN = 0                                                      617
         AG_TITLE = 'TWSS'                                                617
         AG_EXNAME = 'ATTRIBUTE'                                          617
         AG_PARLIST = 'SEEN'                                              617
         CALL AXATTRIBUTE                                                 617
C    *                                                                    617
C    Geant SHAPE : DIVISION ===> NDIV=12 IAXIS=2 <===                     617
         CALL AGSRESET                                                    618
         AG_NDIV = 12                                                     618
         AG_IAXIS = 2                                                     618
         AG_TITLE = 'DIVISION'                                            618
         AG_EXNAME = 'SHAPE'                                              618
         AG_PARLIST = 'NDIV_IAXIS'                                        618
         CALL AXSHAPE                                                     618
C    *                                                                    618
C    *                                                                    618
         I_SEC=1                                                          619
            GO TO 5633                                                    619
5631        I_SEC=I_SEC+1                                                 619
5633        IF(I_SEC-(2).GT.0) GO TO 5632                                 619
C       use TECW bank                                                     620
            BPATH_TECW='TECW*'                                            620
            IDNUM_TECW(1)=0                                               620
            TECW_SEC=I_SEC                                                620
            CALL AGDGETP('TPCEGEO','*',-999,'TECW',NTECW_SEC,             620
     *      RE_TECW_SEC,BPATH_TECW,IDNUM_TECW,1,MAP_TECW,NAMES_TECW,      620
     *      BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)             620
C       Create TWGI :                                                     621
C       Call To Local Procedure TWGI                                      621
            ASSIGN 5650 TO BLOCK_TWGI                                     621
            GO TO  5640                                                   621
5650        CONTINUE                                                      621
C       *                                                                 621
C       Geant POSITION : TWGI ===> X=TECW_GAPRAD ORT=YZX <===             621
            CALL AGSRESET                                                 621
            AG_X = TECW_GAPRAD                                            621
            AG_ALPHAN=AG_ALPHAN+1                                         621
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              621
            AG_TITLE = 'TWGI'                                             621
            AG_EXNAME = 'POSITION'                                        621
            AG_PARLIST = 'X_ORT'                                          621
            CALL AXPOSITION                                               621
C       *                                                                 621
         GO TO 5631                                                       622
5632     CONTINUE                                                         622
5611  CALL AGSPOPB                                                        623
      GO TO BLOCK_TWSS, (5610 ,5620)                                      623
C End of Local Procedure                                                  623
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Begin Local Procedure TWGI                                              625
C TWGI IS THE INNER AIR GAP IN WHEEL                                      625
5640  CONTINUE                                                            625
         AG_TITLE='TWGI'                                                  625
         AG_MODULE=MODULE                                                 625
         AG_MARK='V00'                                                    625
         CALL AGSPUSH                                                     625
         AG_MODULE=MODULE                                                 625
C    *                                                                    625
C    Geant MATERIAL : AIR ===> inherited arguments used <===              626
         CALL AGSRESET                                                    626
         AG_TITLE = 'AIR'                                                 626
         AG_EXNAME = 'MATERIAL'                                           626
         AG_PARLIST = 'NONE'                                              626
         CALL AXMATERIAL                                                  626
C    *                                                                    626
C    Geant ATTRIBUTE : TWGI ===> SEEN=1 COLO=7 <===                       626
         CALL AGSRESET                                                    627
         AG_SEEN = 1                                                      627
         AG_COLO = 7                                                      627
         AG_TITLE = 'TWGI'                                                627
         AG_EXNAME = 'ATTRIBUTE'                                          627
         AG_PARLIST = 'SEEN_COLO'                                         627
         CALL AXATTRIBUTE                                                 627
C    *                                                                    627
C    Geant SHAPE : TRD1 ===> DX1=TECW_GAPWIDI/2 DX2=TECW_GAPWIDO/2, DY=T  627
C    PCG_WHEELTHK/2 DZ=TECW_GAPHEIT/2 <===                                627
         CALL AGSRESET                                                    629
         AG_DX1 = TECW_GAPWIDI/2                                          629
         AG_DX2 = TECW_GAPWIDO/2                                          629
         AG_DY = TPCG_WHEELTHK/2                                          629
         AG_DZ = TECW_GAPHEIT/2                                           629
         AG_TITLE = 'TRD1'                                                629
         AG_EXNAME = 'SHAPE'                                              629
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     629
         CALL AXSHAPE                                                     629
C    *                                                                    629
5641  CALL AGSPOPB                                                        629
      GO TO BLOCK_TWGI, (5640 ,5650)                                      630
C End of Local Procedure                                                  630
*
1     CONTINUE                                                            632
      END                                                                 632
*******************************************************************************
      SUBROUTINE TPCELASER                                                636
*                                                                             *
* Description: generate correct energy deposition produced by the laser beam  *
*   Regarding the beam intensity and ionization, A. Lebedev wrote in his      *
*   note that the ionization density decreases along the beam according to    *
*   I(l) = I(0)*exp(-l/300.),  l in cm I(0) = 100e-/cm., dE of 1 e- is 26 eV  *
*   PN  21.08.97: anything denser than TPC gas absorbs laserino               *
*   PN  30.03.99: introducing dE dependence on beam intensity (Q measured)    *
*       Normalisation is done on a measured Q peak value for cosmic muons     *
*       Although the beam intensity is sqrt of Q, the ionization is I**2.     *
*******************************************************************************
      IMPLICIT NONE                                                       647
C - Parameters of current material                                        648
      INTEGER NMAT,NAMATE                                                 648
      REAL A,Z,DENS,RADL,ABSL                                             648
      COMMON/GCMATE/ NMAT,NAMATE(5),A,Z,DENS,RADL,ABSL                    648
C - parameters of current tracking medium                                 648
      INTEGER NUMED,NATMED,ISVOL,IFIELD,IUPD,ISTPAR,NUMOLD                648
      REAL FIELDM,TMAXFD,STEMAX,DEEMAX,EPSIL,STMIN,CFIELD,PREC            648
      COMMON/GCTMED/ NUMED,NATMED(5),ISVOL,IFIELD,FIELDM,TMAXFD,STEMAX,   648
     *DEEMAX,EPSIL,STMIN,CFIELD,PREC,IUPD,ISTPAR,NUMOLD                   648
      REAL THRIND,PMIN,DP,DNDL                                            648
      INTEGER JMIN,ITCKOV,IMCKOV,NPCKOV                                   648
      COMMON/GCTLIT/ THRIND,PMIN,DP,DNDL,JMIN,ITCKOV,IMCKOV,NPCKOV        648
C - Kinematics of current track                                           648
      INTEGER IKINE,ITRA,ISTAK,IVERT,IPART,ITRTYP,NAPART,IPAOLD           648
      REAL PKINE,AMASS,CHARGE,TLIFE,VERT,PVERT                            648
      COMMON/GCKINE/ IKINE,PKINE(10),ITRA,ISTAK,IVERT,IPART,ITRTYP,       648
     *NAPART(5), AMASS,CHARGE,TLIFE,VERT(3),PVERT(4),IPAOLD               648
C - track parameters at current point                                     648
      INTEGER NMEC,LMEC,NAMEC,NSTEP,MAXNST,IGNEXT,INWVOL,ISTOP,IGAUTO,    648
     *IEKBIN,ILOSL,IMULL,INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY               648
      REAL VECT,GETOT,GEKIN,VOUT,DESTEP,DESTEL,SAFETY, SLENG,STEP,SNEXT,  648
     *SFIELD,TOFG,GEKRAT,UPWGHT                                           648
      COMMON/GCTRAK/ VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(30),           648
     *NAMEC(30),NSTEP, MAXNST,DESTEP,DESTEL,SAFETY,SLENG,STEP,SNEXT,      648
     *SFIELD,TOFG, GEKRAT,UPWGHT,IGNEXT,INWVOL,ISTOP,IGAUTO,IEKBIN,       648
     *ILOSL,IMULL, INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY                     648
C Connection between AgGSTEP, AgGHIT and a user step routine              648
      REAL VECT0, VLOC0, VLOC, XLOC ,ASTEP,ADESTEP,HITS                   648
      INTEGER IHAVEMORE                                                   648
      COMMON /AGCSTEP/ VECT0(7),VLOC0(7),VLOC(7),XLOC(7),ASTEP,ADESTEP    648
      COMMON /AGCSTEPH/ HITS(15)                                          648
      COMMON /AGCSTEPI/ IHAVEMORE                                         648
      CHARACTER*8 CPART,LASERINO/'LASERINO'/                              648
      EQUIVALENCE (CPART,NAPART)                                          650
      REAL AVERAGE                                                        651
C  MUON <Q> IN 87 MEASURMENTS                                             652
      REAL ANORM/4.2/                                                     652
C  Npairs/cm for relativistic muons                                       653
      REAL PNORM/100./                                                    653
      INTEGER NPAIR,IER                                                   654
*                                    this happens only with primary laserino
C Skip Unless CPART==LASERINO                                             656
      IF (CPART.NE.LASERINO)GO TO 1                                       656
C Check DENS>0.005                                                        657
      IF (DENS.GT.0.005) ISTOP=1                                          657
C Skip Unless ISVOL > 0 & ISTAK==0                                        658
      IF (ISVOL.LE.0.OR.ISTAK.NE.0)GO TO 1                                658
*                                    number of produced pairs follows Poisson
      AVERAGE = PNORM*ASTEP * GETOT/ANORM * EXP(-SLENG/300.)              660
      CALL POISSN(AVERAGE,NPAIR,IER)                                      661
      ADESTEP = 26.E-9*NPAIR                                              662
*
1     CONTINUE                                                            664
      END                                                                 664
*******************************************************************************
      SUBROUTINE T P A D S T E P (J,HIT)                                  668
* Description:  Step routine for TPCs, called at the end of the HITS operator *
*    A correction for the track curvature is introduced here. It assumes that
*    hits are (z,y,x)- packed and negative Z are in left-handed coordinates.
*
*******************************************************************************
*
      IMPLICIT NONE                                                       675
C - Dynamic core for the GEANT data (v3.15)                               675
      INTEGER KWBANK,KWWORK,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JDIGI,   675
     *JDRAW, JHEAD,JHITS,JKINE,JMATE,JPART,JROTM,JRUNG,JSET,JSTAK,        675
     *JGSTAT,JTMED, JTRACK,JVERTX,JVOLUM,JXYZ,JGPAR,JGPAR2,JSKLT,IQ,IWS,  675
     *LQ,JCG                                                              675
      REAL ZVERSN,GVERSN,FENDQ,WS,Q                                       675
      PARAMETER (KWBANK=69000)                                            675
      PARAMETER (KWWORK=5200)                                             675
      COMMON /GCBANK/ NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,           675
     *FENDQ(16),LMAIN,LR1,WS(KWBANK)                                      675
      COMMON /GCLINK/ JDIGI,JDRAW, JHEAD,JHITS ,JKINE, JMATE ,JPART,      675
     *JROTM,JRUNG, JSET, JSTAK,JGSTAT,JTMED,JTRACK,JVERTX,JVOLUM,JXYZ,    675
     *JGPAR,JGPAR2,JSKLT                                                  675
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)                                675
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1)),(JCG,   675
     *JGSTAT)                                                             675
C - Multi-level current volume description                                675
      INTEGER NLEVEL,NAMES,NUMBER,LVOLUM,LINDEX,INFROM,NLEVMX,NLDEV,      675
     *LINMX                                                               675
      REAL GTRAN,GRMAT,GONLY,GLX                                          675
      COMMON /GCVOLU/ NLEVEL,NAMES(15),NUMBER(15),LVOLUM(15),LINDEX(15),  675
     *INFROM, NLEVMX,NLDEV(15),LINMX(15),GTRAN(3,15),GRMAT(10,15),        675
     *GONLY(15),GLX(3)                                                    675
C - Kinematics of current track                                           675
      INTEGER IKINE,ITRA,ISTAK,IVERT,IPART,ITRTYP,NAPART,IPAOLD           675
      REAL PKINE,AMASS,CHARGE,TLIFE,VERT,PVERT                            675
      COMMON/GCKINE/ IKINE,PKINE(10),ITRA,ISTAK,IVERT,IPART,ITRTYP,       675
     *NAPART(5), AMASS,CHARGE,TLIFE,VERT(3),PVERT(4),IPAOLD               675
C - track parameters at current point                                     675
      INTEGER NMEC,LMEC,NAMEC,NSTEP,MAXNST,IGNEXT,INWVOL,ISTOP,IGAUTO,    675
     *IEKBIN,ILOSL,IMULL,INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY               675
      REAL VECT,GETOT,GEKIN,VOUT,DESTEP,DESTEL,SAFETY, SLENG,STEP,SNEXT,  675
     *SFIELD,TOFG,GEKRAT,UPWGHT                                           675
      COMMON/GCTRAK/ VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(30),           675
     *NAMEC(30),NSTEP, MAXNST,DESTEP,DESTEL,SAFETY,SLENG,STEP,SNEXT,      675
     *SFIELD,TOFG, GEKRAT,UPWGHT,IGNEXT,INWVOL,ISTOP,IGAUTO,IEKBIN,       675
     *ILOSL,IMULL, INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY                     675
C Connection between AgGSTEP, AgGHIT and a user step routine              675
      REAL VECT0, VLOC0, VLOC, XLOC ,ASTEP,ADESTEP,HITS                   675
      INTEGER IHAVEMORE                                                   675
      COMMON /AGCSTEP/ VECT0(7),VLOC0(7),VLOC(7),XLOC(7),ASTEP,ADESTEP    675
      COMMON /AGCSTEPH/ HITS(15)                                          675
      COMMON /AGCSTEPI/ IHAVEMORE                                         675
C - Flags and variables to control the run                                675
      INTEGER IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN,IEOTRI,       675
     *IEVENT,ISWIT,IFINIT,NEVENT,NRNDM                                    675
      COMMON/GCFLAG/ IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN,       675
     *IEOTRI, IEVENT,ISWIT(10),IFINIT(20),NEVENT,NRNDM(2)                 675
*/AGCSTEP/: r*7 vect0,vloc0,vloc,xloc, r: Astep,Adestep
*
      INTEGER J,MYPAD,JMYPAD,ISHAPE,JMYPAR,I_FLAG,JCENTER                 678
      REAL CENTER(3)/0,0,0/,FIELD(3)/0,0,0/,HIT,DR,DT,VECT_MIDDLE(6),     680
     *XI,YI,XO,YO,DX,DY,DPHI,DTR,VR,PT,SIGN,SMAX/6.0/                     680
      DATA I_FLAG/0/,JCENTER/-1/,VECT_MIDDLE/6*0./                        681
      LOGICAL VALID_HIT,FIRST/.TRUE./                                     682
      CHARACTER CNAME*4                                                   683
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ENTRY TPAISTEP (J,HIT)                                              685
      ENTRY TPAOSTEP (J,HIT)                                              686
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Check FIRST                                                             688
      IF (FIRST) THEN                                                     688
      FIRST = .FALSE.                                                     689
      CALL GUFLD (CENTER,FIELD)                                           690
C Check IDEBUG>1                                                          691
      IF (IDEBUG.GT.1) PRINT *,' In TPADSTEP field=',FIELD                691
      ENDIF                                                               692
*
* Extract parameters of the current padraw from GEANT
*
*    This we need to get the actual padraw width from GEANT:
C POINTER TO THE VOLUME PARAMETERS                                        697
      JMYPAR = LQ(JGPAR-NLEVEL)                                           697
C RADIAL HALF-SIZE                                                        698
      DR = Q(JMYPAR+1)                                                    698
C TRANSVERSE HALF-SIZE                                                    699
      DT = Q(JMYPAR+2)                                                    699
*
C Check IDEBUG>0                                                          701
      IF (IDEBUG.GT.0) THEN                                               701
*    this is for control purpouse only - not really needed:
C PADROW VOLUME POINTER (NUMBER)                                          703
      MYPAD = LVOLUM(NLEVEL)                                              703
C POINTER TO THE VOLUME BANK                                              704
      JMYPAD = LQ(JVOLUM-MYPAD)                                           704
C GEANT SHAPE CODE OF THE PADRAW                                          705
      ISHAPE = Q(JMYPAD+2)                                                705
C Check ISHAPE != 1                                                       706
      IF (ISHAPE .NE. 1) THEN                                             706
C SHOULD BE TPAD,TPAI OR TPAO                                             707
      CALL UHTOC(IQ(JMYPAD-4),4,CNAME,4)                                  707
      PRINT *,' Awful error in TPADSTEP, valid hit may not be valid !'    708
      PRINT *,' volume ',CNAME,' at level ',NLEVEL,' shape=',ISHAPE,' dr  709
     *=',DR                                                               709
      PRINT *,' we are in point ',VECT0                                   710
      ENDIF                                                               711
      ENDIF                                                               712
*
*    A hit is called Valid when trajectory cross a padrow (almost) completely:
      VALID_HIT = ABS(VLOC0(1)-VLOC(1)) .GT. 2*DR-0.1                     715
*
      XI = VLOC0(1)                                                       716
      YI = VLOC0(2)                                                       717
      XO = VLOC(1)                                                        717
      YO = VLOC(2)                                                        718
      PT = VLOC0(7) * SQRT(VLOC0(4)**2+VLOC0(5)**2)                       720
      VR = 1.E9                                                           720
C Check PT>0                                                              721
      IF (PT.GT.0) VR = 0.0003*FIELD(3)/PT                                721
*     more precisely 2*asin(S/2R), but we use a limited linear approximation:
      DPHI = MIN (ASTEP*VR,1.0)                                           724
      DTR = DPHI/8*CHARGE*SIGN(1.,VECT(3))                                725
*     correction to   HITS TPAD  Z  Y  X  cx cy cz
*     again precise coefficient is S/R*(1-cos(phi/2), take linear term only:
      DY = DTR*(XI-XO)                                                    728
      HITS(2) = MIN(MAX(-DT,HITS(2)+DY),DT)                               729
      DX = DTR*(YO-YI)                                                    729
      HITS(3) = MIN(MAX(-DR,HITS(3)+DX),DR)                               730
      HIT = ADESTEP                                                       732
      END                                                                 733
