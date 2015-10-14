C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
* $Id: itspgeo.g,v 1.2 2007/11/13 21:30:51 perev Exp $
*
* $Log: itspgeo.g,v $
* Revision 1.2  2007/11/13 21:30:51  perev
* material ALKAP fixed
*
* Revision 1.1  2006/10/21 17:58:05  potekhin
* Adding a new crucial subsystem, which is the support structure
* for the tracking upgrade configurations.
* ased on the SVT conde design with a few simplifications. On the west, we
* have a mockup cone which is a slimmed-down version of the East one (which will
* stay). Copper cables have been removed for now.
*
*
*
*******************************************************************************
      SUBROUTINE ITSPGEO                                                  17
C ITSPGEO IS THE INNER TRACKER SUPPORT                                    17
      IMPLICIT NONE                                                       17
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       17
      DATA MODULE/'ITSPGEO'/, BANK_TITLE/'*'/                             17
      DATA MCOMMENT/'IS THE INNER TRACKER SUPPORT'/                       17
      LOGICAL FIRST                                                       17
      DATA FIRST/.TRUE./                                                  17
      SAVE FIRST                                                          17
      INTEGER JUMPAD                                                      17
      EXTERNAL JUMPAD                                                     17
      INTEGER OK                                                          17
      PARAMETER (OK=0)                                                    17
      INTEGER AG_I,AG_J                                                   17
      SAVE AG_I,AG_J                                                      17
      DATA AG_I/0/,AG_J/0/                                                17
*-->Based on the SVTTGEO6 code
C AUTHOR MAXIM POTEKHIN                                                   19
      CHARACTER*40 AUTHOR /'MAXIM POTEKHIN'/                              20
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  20
C CREATED 18 OCT 2006                                                     20
      CHARACTER*40 CREATED /'18 OCT 2006'/                                21
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  21
C - Advanced Geant interface                                              23
      CHARACTER*20 AG_MODULE,AG_TITLE,AG_EXNAME,AG_PARLIST,AG_CHDIR,      23
     *AG_MATERIAL,AG_MIXTURE,AG_COMPONENT,AG_MEDIUM                       23
      CHARACTER*4 AG_VOLUME,AG_MOTHER,AG_SHAPE,AG_CNICK,AG_KONLY,         23
     *AG_OPTION,AG_ORT,AG_MARK                                            23
      INTEGER AG_BEGCOM,AG_IVOLUME,AG_IMOTHER,AG_IGNUM,AG_ISHAPE,         23
     *AG_IMED,AG_IMAT,AG_IFIELD, AG_NLMAT,AG_IERROR,AG_NWBUF,AG_NPAR,     23
     *AG_ISTATUS,AG_IROT,AG_NBITS,AG_SERIAL, AG_ATTRIBUTE(6),AG_WORK,     23
     *AG_SEEN,AG_LSTY,AG_LWID,AG_COLO,AG_FILL,AG_JDU,AG_ORTI, AG_NDIV,    23
     *AG_IAXIS,AG_NDVMAX,AG_IPRIN,AG_RESET1,AG_RESET2,AG_BEGSCR,          23
     *AG_ENDSCR, AG_IDTYPE,AG_ISET,AG_IDET,AG_ISVOL,AG_LEVEL,AG_IRESER,   23
     *AG_IGEOM,AG_STANDALONE, AG_LSTACK,AG_NWUHIT,AG_NWUVOL,AG_MAGIC,     23
     *AG_LDETU,AG_NPDIV,AG_NPDV,AG_NZ,AG_NCOPY, AG_IDEBU,AG_IGRAP,        23
     *AG_IHIST,AG_IMFLD,AG_ISIMU,AG_ENDSAVE,AG_CODE,AG_TRKTYP, AG_MODE,   23
     *AG_ECODE,AG_PDG,IPRIN, AG_ALPHAN,AG_ALPHAK(10),IGOR                 23
      REAL AG_FIELDM,AG_TMAXFD,AG_STEMAX,AG_DEEMAX,AG_EPSIL,AG_STMIN,     23
     *AG_DENS,AG_RADL,AG_ABSL, AG_THETAX,AG_PHIX,AG_THETAY,AG_PHIY,       23
     *AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,AG_ALFAZ, AG_ALPHAL(10),        23
     *AG_ALPHAX,AG_ALPHAY,AG_ALPHAZ,AG_TYPE, AG_DX, AG_DX1, AG_DX2,       23
     *AG_DY, AG_DY1, AG_DY2, AG_DMAXMS, AG_TWIST, AG_THET, AG_THE1,       23
     *AG_THE2, AG_PHI, AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2,       23
     *AG_RMIN, AG_RMAX, AG_RMN, AG_RMX, AG_ZI , AG_RMN1, AG_RMN2,         23
     *AG_RMX1, AG_RMX2, AG_H1, AG_H2, AG_BL1, AG_BL2, AG_TL1, AG_TL2,     23
     *AG_DPHI, AG_DZ, AG_TWIS, AG_X, AG_Y, AG_Z, AG_A, AG_ZA, AG_W,       23
     *AG_STEP, AG_C0, AG_PAR, AG_AA, AG_ZZ, AG_WW, AG_LX, AG_LY, AG_LZ,   23
     *AG_HX, AG_HY, AG_HZ, AG_P1, AG_P2, AG_STACK,AG_UBUF,AG_XHMAX,       23
     *AG_YHMAX,AG_ZHMAX,AG_RHMAX,AG_FHMAX,AG_FHMIN,AG_BIN, AG_CHARGE,     23
     *AG_MASS,AG_TLIFE,AG_BRATIO                                          23
      PARAMETER (AG_LSTACK=130,AG_NWUHIT=10,AG_NWUVOL=3,AG_MAGIC=         23
     *-696969,AG_LDETU=250)                                               23
      COMMON/AGCGLOB/ AG_MODULE,AG_CHDIR,AG_LEVEL,AG_IDTYPE,AG_IERROR,    23
     *AG_STANDALONE,IPRIN, AG_IPRIN,AG_IGEOM,AG_IDEBU,AG_IGRAP,AG_IHIST,  23
     *AG_IMFLD,AG_ISIMU                                                   23
C Inherited variables a: saved during internal calls                      23
C b: reset at the entry in a block and then saved                         23
      COMMON/AGCPARA/ AG_BEGCOM, AG_IVOLUME, AG_IMOTHER, AG_IGNUM,        23
     *AG_ISHAPE, AG_IMED, AG_IMAT, AG_IFIELD, AG_FIELDM, AG_TMAXFD,       23
     *AG_STEMAX, AG_DEEMAX, AG_EPSIL, AG_STMIN, AG_DENS, AG_RADL,         23
     *AG_ABSL, AG_DX, AG_DX1, AG_DX2, AG_DY, AG_DY1, AG_DY2, AG_RMN1,     23
     *AG_RMN2, AG_RMX1, AG_RMX2, AG_THET, AG_THE1, AG_THE2, AG_PHI,       23
     *AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2, AG_H1, AG_BL1,         23
     *AG_TL1, AG_H2, AG_BL2, AG_TL2, AG_RMIN, AG_RMAX, AG_DPHI,           23
     *AG_NPDIV, AG_NZ, AG_DZ, AG_TWIS, AG_LX, AG_LY, AG_LZ, AG_HX,        23
     *AG_HY, AG_HZ, AG_A, AG_ZA, AG_W, AG_NLMAT, AG_WORK, AG_SEEN,        23
     *AG_LSTY, AG_LWID, AG_COLO, AG_FILL, AG_SERIAL, AG_ISVOL,            23
     *AG_ISTATUS, AG_ZI(16), AG_RMN(16), AG_RMX(16), AG_VOLUME,           23
     *AG_MOTHER, AG_SHAPE, AG_CNICK, AG_ENDSAVE, AG_RESET1, AG_THETAX,    23
     *AG_PHIX,AG_THETAY,AG_PHIY,AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,      23
     *AG_ALFAZ, AG_X,AG_Y,AG_Z, AG_STEP,AG_C0,AG_NDIV,AG_IAXIS,           23
     *AG_NDVMAX,AG_ORTI,AG_NCOPY, AG_ALPHAN,AG_ALPHAK,AG_ALPHAL,          23
     *AG_RESET2, AG_KONLY, AG_ORT, AG_MARK                                23
C local variables valid inside same block                                 23
      COMMON/AGCLOCA/ AG_BEGSCR,AG_UBUF(100),AG_PAR(100),AG_AA(20),       23
     *AG_ZZ(20),AG_WW(20),AG_NWBUF, AG_XHMAX,AG_YHMAX,AG_ZHMAX,AG_RHMAX,  23
     *AG_FHMAX,AG_FHMIN,AG_NBITS,AG_BIN,AG_TYPE, AG_IROT,AG_NPAR,         23
     *AG_ISET,AG_IDET,AG_JDU,AG_IRESER, AG_ENDSCR, AG_TITLE,AG_EXNAME,    23
     *AG_PARLIST,AG_MATERIAL,AG_MIXTURE, AG_COMPONENT,AG_MEDIUM,          23
     *AG_OPTION                                                           23
      COMMON/AGCPART/AG_CODE,AG_TRKTYP,AG_MASS,AG_CHARGE,AG_TLIFE,        23
     *AG_BRATIO(6),AG_MODE(6),AG_PDG,AG_ECODE                             23
      COMMON/AGCSTAC/ AG_STACK(AG_LSTACK,15)                              23
C temporarly                                                              23
      EQUIVALENCE (AG_ATTRIBUTE,AG_WORK),(AG_STEMAX,AG_DMAXMS),           23
     *(AG_ALPHAX,AG_ALFAX), (AG_ALPHAY,AG_ALFAY),(AG_ALPHAZ,AG_ALFAZ),    23
     *(AG_TWIST,AG_TWIS),(AG_NPDIV,AG_NPDV),  (AG_P1,AG_HX),(AG_P2,       23
     *AG_HY)                                                              23
      REAL MIL_P,INCH_P                                                   23
      PARAMETER (INCH_P=2.54,MIL_P=0.00254)                               23
C - Natural constants (or considered as such)                             23
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   23
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        23
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       23
C - GEANT description of logical units                                    23
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            23
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      23
      COMMON/GCMAIL/ CHMAIL                                               23
      CHARACTER*132 CHMAIL                                                23
*
      INTEGER BLOCK_ITSP                                                  25
      INTEGER BLOCK_SGRA                                                  25
      INTEGER BLOCK_STAP                                                  25
      INTEGER BLOCK_SHMB                                                  25
      INTEGER BLOCK_SCMY                                                  25
      INTEGER BLOCK_SCAL                                                  25
*
C -                                                                       31
      REAL SSUP_VERSION                                                   31
      EQUIVALENCE (BANK_SSUP(3),SSUP_VERSION,RE_SSUP_VERSION),            31
     *(NAMES_SSUP(1),NSSUP_VERSION)                                       31
      EQUIVALENCE (COMMEN_SSUP(1),CSSUP_VERSION),(MAP_SSUP(1,1),          31
     *MSSUP_VERSION)                                                      31
      CHARACTER CSSUP_VERSION*40/' '/                                     31
      CHARACTER NSSUP_VERSION*12/'R.VERSION'/                             31
      INTEGER MSSUP_VERSION(2)/1,1/                                       31
      REAL RE_SSUP_VERSION                                                31
C -                                                                       31
      REAL SSUP_CABTHK                                                    31
      EQUIVALENCE (BANK_SSUP(4),SSUP_CABTHK,RE_SSUP_CABTHK),              31
     *(NAMES_SSUP(2),NSSUP_CABTHK)                                        31
      EQUIVALENCE (COMMEN_SSUP(2),CSSUP_CABTHK),(MAP_SSUP(1,2),           31
     *MSSUP_CABTHK)                                                       31
      CHARACTER CSSUP_CABTHK*40/' '/                                      31
      CHARACTER NSSUP_CABTHK*12/'R.CABTHK'/                               31
      INTEGER MSSUP_CABTHK(2)/1,1/                                        31
      REAL RE_SSUP_CABTHK                                                 31
C -                                                                       31
      REAL SSUP_HOSRMX                                                    31
      EQUIVALENCE (BANK_SSUP(5),SSUP_HOSRMX,RE_SSUP_HOSRMX),              31
     *(NAMES_SSUP(3),NSSUP_HOSRMX)                                        31
      EQUIVALENCE (COMMEN_SSUP(3),CSSUP_HOSRMX),(MAP_SSUP(1,3),           31
     *MSSUP_HOSRMX)                                                       31
      CHARACTER CSSUP_HOSRMX*40/' '/                                      31
      CHARACTER NSSUP_HOSRMX*12/'R.HOSRMX'/                               31
      INTEGER MSSUP_HOSRMX(2)/1,1/                                        31
      REAL RE_SSUP_HOSRMX                                                 31
C -                                                                       31
      REAL SSUP_WRPMYTHK                                                  31
      EQUIVALENCE (BANK_SSUP(6),SSUP_WRPMYTHK,RE_SSUP_WRPMYTHK),          31
     *(NAMES_SSUP(4),NSSUP_WRPMYTHK)                                      31
      EQUIVALENCE (COMMEN_SSUP(4),CSSUP_WRPMYTHK),(MAP_SSUP(1,4),         31
     *MSSUP_WRPMYTHK)                                                     31
      CHARACTER CSSUP_WRPMYTHK*40/' '/                                    31
      CHARACTER NSSUP_WRPMYTHK*12/'R.WRPMYTHK'/                           31
      INTEGER MSSUP_WRPMYTHK(2)/1,1/                                      31
      REAL RE_SSUP_WRPMYTHK                                               31
C -                                                                       31
      REAL SSUP_WRPALTHK                                                  31
      EQUIVALENCE (BANK_SSUP(7),SSUP_WRPALTHK,RE_SSUP_WRPALTHK),          31
     *(NAMES_SSUP(5),NSSUP_WRPALTHK)                                      31
      EQUIVALENCE (COMMEN_SSUP(5),CSSUP_WRPALTHK),(MAP_SSUP(1,5),         31
     *MSSUP_WRPALTHK)                                                     31
      CHARACTER CSSUP_WRPALTHK*40/' '/                                    31
      CHARACTER NSSUP_WRPALTHK*12/'R.WRPALTHK'/                           31
      INTEGER MSSUP_WRPALTHK(2)/1,1/                                      31
      REAL RE_SSUP_WRPALTHK                                               31
C -                                                                       31
      REAL SSUP_RODLEN                                                    31
      EQUIVALENCE (BANK_SSUP(8),SSUP_RODLEN,RE_SSUP_RODLEN),              31
     *(NAMES_SSUP(6),NSSUP_RODLEN)                                        31
      EQUIVALENCE (COMMEN_SSUP(6),CSSUP_RODLEN),(MAP_SSUP(1,6),           31
     *MSSUP_RODLEN)                                                       31
      CHARACTER CSSUP_RODLEN*40/' '/                                      31
      CHARACTER NSSUP_RODLEN*12/'R.RODLEN'/                               31
      INTEGER MSSUP_RODLEN(2)/1,1/                                        31
      REAL RE_SSUP_RODLEN                                                 31
C -                                                                       31
      REAL SSUP_GRPHTHK                                                   31
      EQUIVALENCE (BANK_SSUP(9),SSUP_GRPHTHK,RE_SSUP_GRPHTHK),            31
     *(NAMES_SSUP(7),NSSUP_GRPHTHK)                                       31
      EQUIVALENCE (COMMEN_SSUP(7),CSSUP_GRPHTHK),(MAP_SSUP(1,7),          31
     *MSSUP_GRPHTHK)                                                      31
      CHARACTER CSSUP_GRPHTHK*40/' '/                                     31
      CHARACTER NSSUP_GRPHTHK*12/'R.GRPHTHK'/                             31
      INTEGER MSSUP_GRPHTHK(2)/1,1/                                       31
      REAL RE_SSUP_GRPHTHK                                                31
C -                                                                       31
      REAL SSUP_CONE1ZMN                                                  31
      EQUIVALENCE (BANK_SSUP(10),SSUP_CONE1ZMN,RE_SSUP_CONE1ZMN),         31
     *(NAMES_SSUP(8),NSSUP_CONE1ZMN)                                      31
      EQUIVALENCE (COMMEN_SSUP(8),CSSUP_CONE1ZMN),(MAP_SSUP(1,8),         31
     *MSSUP_CONE1ZMN)                                                     31
      CHARACTER CSSUP_CONE1ZMN*40/' '/                                    31
      CHARACTER NSSUP_CONE1ZMN*12/'R.CONE1ZMN'/                           31
      INTEGER MSSUP_CONE1ZMN(2)/1,1/                                      31
      REAL RE_SSUP_CONE1ZMN                                               31
C -                                                                       31
      REAL SSUP_CON1IDMN                                                  31
      EQUIVALENCE (BANK_SSUP(11),SSUP_CON1IDMN,RE_SSUP_CON1IDMN),         31
     *(NAMES_SSUP(9),NSSUP_CON1IDMN)                                      31
      EQUIVALENCE (COMMEN_SSUP(9),CSSUP_CON1IDMN),(MAP_SSUP(1,9),         31
     *MSSUP_CON1IDMN)                                                     31
      CHARACTER CSSUP_CON1IDMN*40/' '/                                    31
      CHARACTER NSSUP_CON1IDMN*12/'R.CON1IDMN'/                           31
      INTEGER MSSUP_CON1IDMN(2)/1,1/                                      31
      REAL RE_SSUP_CON1IDMN                                               31
C -                                                                       31
      REAL SSUP_CON3IDMN                                                  31
      EQUIVALENCE (BANK_SSUP(12),SSUP_CON3IDMN,RE_SSUP_CON3IDMN),         31
     *(NAMES_SSUP(10),NSSUP_CON3IDMN)                                     31
      EQUIVALENCE (COMMEN_SSUP(10),CSSUP_CON3IDMN),(MAP_SSUP(1,10),       31
     *MSSUP_CON3IDMN)                                                     31
      CHARACTER CSSUP_CON3IDMN*40/' '/                                    31
      CHARACTER NSSUP_CON3IDMN*12/'R.CON3IDMN'/                           31
      INTEGER MSSUP_CON3IDMN(2)/1,1/                                      31
      REAL RE_SSUP_CON3IDMN                                               31
C -                                                                       31
      REAL SSUP_CON4IDMN                                                  31
      EQUIVALENCE (BANK_SSUP(13),SSUP_CON4IDMN,RE_SSUP_CON4IDMN),         31
     *(NAMES_SSUP(11),NSSUP_CON4IDMN)                                     31
      EQUIVALENCE (COMMEN_SSUP(11),CSSUP_CON4IDMN),(MAP_SSUP(1,11),       31
     *MSSUP_CON4IDMN)                                                     31
      CHARACTER CSSUP_CON4IDMN*40/' '/                                    31
      CHARACTER NSSUP_CON4IDMN*12/'R.CON4IDMN'/                           31
      INTEGER MSSUP_CON4IDMN(2)/1,1/                                      31
      REAL RE_SSUP_CON4IDMN                                               31
C -                                                                       31
      REAL SSUP_CON4IDMX                                                  31
      EQUIVALENCE (BANK_SSUP(14),SSUP_CON4IDMX,RE_SSUP_CON4IDMX),         31
     *(NAMES_SSUP(12),NSSUP_CON4IDMX)                                     31
      EQUIVALENCE (COMMEN_SSUP(12),CSSUP_CON4IDMX),(MAP_SSUP(1,12),       31
     *MSSUP_CON4IDMX)                                                     31
      CHARACTER CSSUP_CON4IDMX*40/' '/                                    31
      CHARACTER NSSUP_CON4IDMX*12/'R.CON4IDMX'/                           31
      INTEGER MSSUP_CON4IDMX(2)/1,1/                                      31
      REAL RE_SSUP_CON4IDMX                                               31
C -                                                                       31
      REAL SSUP_CONE3ZMX                                                  31
      EQUIVALENCE (BANK_SSUP(15),SSUP_CONE3ZMX,RE_SSUP_CONE3ZMX),         31
     *(NAMES_SSUP(13),NSSUP_CONE3ZMX)                                     31
      EQUIVALENCE (COMMEN_SSUP(13),CSSUP_CONE3ZMX),(MAP_SSUP(1,13),       31
     *MSSUP_CONE3ZMX)                                                     31
      CHARACTER CSSUP_CONE3ZMX*40/' '/                                    31
      CHARACTER NSSUP_CONE3ZMX*12/'R.CONE3ZMX'/                           31
      INTEGER MSSUP_CONE3ZMX(2)/1,1/                                      31
      REAL RE_SSUP_CONE3ZMX                                               31
C -                                                                       31
      REAL SSUP_CONE4ZMX                                                  31
      EQUIVALENCE (BANK_SSUP(16),SSUP_CONE4ZMX,RE_SSUP_CONE4ZMX),         31
     *(NAMES_SSUP(14),NSSUP_CONE4ZMX)                                     31
      EQUIVALENCE (COMMEN_SSUP(14),CSSUP_CONE4ZMX),(MAP_SSUP(1,14),       31
     *MSSUP_CONE4ZMX)                                                     31
      CHARACTER CSSUP_CONE4ZMX*40/' '/                                    31
      CHARACTER NSSUP_CONE4ZMX*12/'R.CONE4ZMX'/                           31
      INTEGER MSSUP_CONE4ZMX(2)/1,1/                                      31
      REAL RE_SSUP_CONE4ZMX                                               31
C -                                                                       31
      INTEGER LEN_SSUP,LLEN_SSUP,LINK_SSUP                                31
      PARAMETER (LEN_SSUP=16)                                             31
      PARAMETER (LLEN_SSUP=14)                                            31
      INTEGER FLAG_SSUP/0/,MAP_SSUP(2,LLEN_SSUP),IDNUM_SSUP(10)           31
      CHARACTER NAMES_SSUP(LLEN_SSUP)*12,COMMEN_SSUP(LLEN_SSUP)*40        31
      DATA IDNUM_SSUP/10*0/                                               31
      REAL BANK_SSUP(LEN_SSUP)                                            31
      CHARACTER*60 BPATH_SSUP/' '/                                        31
      SAVE FLAG_SSUP,MAP_SSUP,IDNUM_SSUP,NAMES_SSUP,COMMEN_SSUP,          31
     *BPATH_SSUP                                                          31
      DATA LINK_SSUP/0/,BANK_SSUP/LEN_SSUP*0/                             31
C *                                                                       31
*
      INTEGER I,J                                                         33
      REAL CONE_THK1,CONE_THK2,ROFFSET,RSIZEMAX,DEG,RAD,C0                34
      REAL CONE_LEN,CONE_SIN,CONE_COS,RMIN,RMAX,ZMIN,ZMAX,ANGLE           35
      REAL XPOS,YPOS,ZPOS,CLEARANCE,RIN,ROU,ELETHK,TABLEN,RADMAX          36
      REAL ANG,WAFPCKLEN,DTHK,RADTILT                                     37
*******************************************************************************
* Historical note from the recent version of the SVTT code (2006).
* which is useful to keep in mind. We have removed the extra version
* of this structure (obsoileted anyway) --maxim--
*------------------------------------------------------------
* This is a copy of the above (base) version with one difference which is
* the thickness of the copper layer which models the cables attached to
* the cones. Based on the new measured data from Dave Lynn, we normalize the
* effective thickness of this approximation, such that it matches the actual
* cable weight.It's a factor of 4.35 compared to the previous number. That
* previous number's origin is now impossible to determine.
* NOTE: historically in the code this number, CabThk, is divided by 2.
* I will keep it the same way to more easily compare the old with the new.
*
C *                                                                       52
      CALL AGSBEGM(MODULE,IPRIN)                                          52
      IF (IPRIN.LT.0) RETURN                                              52
C Filling bank SSUP - SUPPORT STRUCTURES                                  52
      BANK_TITLE='SUPPORT STRUCTURES'                                     52
      BPATH_SSUP='SSUP*'                                                  52
      IDNUM_SSUP(1)=0                                                     53
C GEOMETRY VERSION                                                        53
      SSUP_VERSION=1                                                      53
      CSSUP_VERSION='GEOMETRY VERSION'                                    53
C THICKNESS OF LAYER OF CABLES ON SUPPORT CONE                            54
      SSUP_CABTHK=0.21                                                    54
      CSSUP_CABTHK='THICKNESS OF LAYER OF CABLES ON SUPPORT CONE'         54
C OUTER RADIUS OF WATER HOSES ON SUPPORT CONE                             55
      SSUP_HOSRMX=0.95                                                    55
      CSSUP_HOSRMX='OUTER RADIUS OF WATER HOSES ON SUPPORT CONE'          55
C THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)                             56
      SSUP_WRPMYTHK=0.10                                                  56
      CSSUP_WRPMYTHK='THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)'        56
C THICKNESS OF AL ON MYLAR WRAP (GUESS)                                   57
      SSUP_WRPALTHK=0.01                                                  57
      CSSUP_WRPALTHK='THICKNESS OF AL ON MYLAR WRAP (GUESS)'              57
C LENGTH OF SUPPORT RODS                                                  58
      SSUP_RODLEN=110.8                                                   58
      CSSUP_RODLEN='LENGTH OF SUPPORT RODS'                               58
C SUPPORT CONE THICKNESS                                                  59
      SSUP_GRPHTHK=0.16                                                   59
      CSSUP_GRPHTHK='SUPPORT CONE THICKNESS'                              59
C CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)                              60
      SSUP_CONE1ZMN=52.23                                                 60
      CSSUP_CONE1ZMN='CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)'         60
C MINIMUM ID OF CONE 1                                                    61
      SSUP_CON1IDMN=15.67                                                 61
      CSSUP_CON1IDMN='MINIMUM ID OF CONE 1'                               61
C MINIMUM ID OF CONE 3 (TBD)                                              62
      SSUP_CON3IDMN=21.67                                                 62
      CSSUP_CON3IDMN='MINIMUM ID OF CONE 3 (TBD)'                         62
C MINIMUM ID OF CONE 4 (TBD)                                              63
      SSUP_CON4IDMN=37.4                                                  63
      CSSUP_CON4IDMN='MINIMUM ID OF CONE 4 (TBD)'                         63
C MAXIMUM ID OF CONE 4 (TBD)                                              64
      SSUP_CON4IDMX=37.4                                                  64
      CSSUP_CON4IDMX='MAXIMUM ID OF CONE 4 (TBD)'                         64
C MAXIMUM Z OF CONE 3 (TBD)                                               65
      SSUP_CONE3ZMX=150.0                                                 65
      CSSUP_CONE3ZMX='MAXIMUM Z OF CONE 3 (TBD)'                          65
C MAXIMUM Z OF CONE 4 (TBD)                                               66
      SSUP_CONE4ZMX=229.36                                                66
      CSSUP_CONE4ZMX='MAXIMUM Z OF CONE 4 (TBD)'                          66
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    67
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           67
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   67
C *                                                                       67
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    67
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           67
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   67
C *                                                                       68
      CALL AGSBEGM(MODULE,IPRIN)                                          68
      IF (IPRIN.LT.0) RETURN                                              68
C Filling bank SSUP - SUPPORT STRUCTURES                                  68
      BANK_TITLE='SUPPORT STRUCTURES'                                     68
      BPATH_SSUP='SSUP*'                                                  68
      IDNUM_SSUP(1)=0                                                     69
C GEOMETRY VERSION                                                        69
      SSUP_VERSION=2                                                      69
      CSSUP_VERSION='GEOMETRY VERSION'                                    69
C THICKNESS OF LAYER OF CABLES ON SUPPORT CONE                            70
      SSUP_CABTHK=0.21                                                    70
      CSSUP_CABTHK='THICKNESS OF LAYER OF CABLES ON SUPPORT CONE'         70
C OUTER RADIUS OF WATER HOSES ON SUPPORT CONE                             71
      SSUP_HOSRMX=0.95                                                    71
      CSSUP_HOSRMX='OUTER RADIUS OF WATER HOSES ON SUPPORT CONE'          71
C THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)                             72
      SSUP_WRPMYTHK=0.10                                                  72
      CSSUP_WRPMYTHK='THICKNESS OF MYLAR WRAP AROUND CONE (GUESS)'        72
C THICKNESS OF AL ON MYLAR WRAP (GUESS)                                   73
      SSUP_WRPALTHK=0.01                                                  73
      CSSUP_WRPALTHK='THICKNESS OF AL ON MYLAR WRAP (GUESS)'              73
C SUPPORT CONE THICKNESS                                                  74
      SSUP_GRPHTHK=0.16                                                   74
      CSSUP_GRPHTHK='SUPPORT CONE THICKNESS'                              74
C CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)                              75
      SSUP_CONE1ZMN=52.23                                                 75
      CSSUP_CONE1ZMN='CONE Z MIN (PARTS 1,2,3,4 IN INCREASING Z)'         75
C MINIMUM ID OF CONE 1                                                    76
      SSUP_CON1IDMN=7.835                                                 76
      CSSUP_CON1IDMN='MINIMUM ID OF CONE 1'                               76
C MINIMUM ID OF CONE 3 (TBD)                                              77
      SSUP_CON3IDMN=10.835                                                77
      CSSUP_CON3IDMN='MINIMUM ID OF CONE 3 (TBD)'                         77
C MINIMUM ID OF CONE 4 (TBD)                                              78
      SSUP_CON4IDMN=18.7                                                  78
      CSSUP_CON4IDMN='MINIMUM ID OF CONE 4 (TBD)'                         78
C MAXIMUM ID OF CONE 4 (TBD)                                              79
      SSUP_CON4IDMX=18.7                                                  79
      CSSUP_CON4IDMX='MAXIMUM ID OF CONE 4 (TBD)'                         79
C MAXIMUM Z OF CONE 3 (TBD)                                               80
      SSUP_CONE3ZMX=150.0                                                 80
      CSSUP_CONE3ZMX='MAXIMUM Z OF CONE 3 (TBD)'                          80
C MAXIMUM Z OF CONE 4 (TBD)                                               81
      SSUP_CONE4ZMX=229.36                                                81
      CSSUP_CONE4ZMX='MAXIMUM Z OF CONE 4 (TBD)'                          81
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    82
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           82
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   82
C *                                                                       82
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSUP',    82
     *BPATH_SSUP,IDNUM_SSUP,1, MAP_SSUP,NAMES_SSUP,COMMEN_SSUP,           82
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   82
*------------------------------  SHALL WE BEGIN? -------------------------------------------
* introduce common materials here
*
*     G10 is about 60% SiO2 and 40% epoxy (stolen from ftpcgeo.g)
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             88
      CALL AGSRESET                                                       88
      AG_A = 28.08                                                        88
      AG_Z = 14                                                           88
      AG_W = 0.6*1*28./60.                                                88
      AG_TITLE = 'SI'                                                     88
      AG_EXNAME = 'COMPONENT'                                             88
      AG_PARLIST = 'A_Z_W'                                                88
      CALL AXCOMPONENT                                                    88
C *                                                                       88
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. <===                  88
      CALL AGSRESET                                                       89
      AG_A = 16                                                           89
      AG_Z = 8                                                            89
      AG_W = 0.6*2*16./60.                                                89
      AG_TITLE = 'O'                                                      89
      AG_EXNAME = 'COMPONENT'                                             89
      AG_PARLIST = 'A_Z_W'                                                89
      CALL AXCOMPONENT                                                    89
C *                                                                       89
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 89
      CALL AGSRESET                                                       90
      AG_A = 12                                                           90
      AG_Z = 6                                                            90
      AG_W = 0.4*8*12./174.                                               90
      AG_TITLE = 'C'                                                      90
      AG_EXNAME = 'COMPONENT'                                             90
      AG_PARLIST = 'A_Z_W'                                                90
      CALL AXCOMPONENT                                                    90
C *                                                                       90
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  90
      CALL AGSRESET                                                       91
      AG_A = 1                                                            91
      AG_Z = 1                                                            91
      AG_W = 0.4*14*1./174.                                               91
      AG_TITLE = 'H'                                                      91
      AG_EXNAME = 'COMPONENT'                                             91
      AG_PARLIST = 'A_Z_W'                                                91
      CALL AXCOMPONENT                                                    91
C *                                                                       91
C Geant COMPONENT : O ===> A=16 Z=8 W=0.4*4*16./174. <===                 91
      CALL AGSRESET                                                       92
      AG_A = 16                                                           92
      AG_Z = 8                                                            92
      AG_W = 0.4*4*16./174.                                               92
      AG_TITLE = 'O'                                                      92
      AG_EXNAME = 'COMPONENT'                                             92
      AG_PARLIST = 'A_Z_W'                                                92
      CALL AXCOMPONENT                                                    92
C *                                                                       92
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  92
      CALL AGSRESET                                                       93
      AG_DENS = 1.7                                                       93
      AG_TITLE = 'G10'                                                    93
      AG_EXNAME = 'MIXTURE'                                               93
      AG_PARLIST = 'DENS'                                                 93
      CALL AXMIXTURE                                                      93
C *                                                                       93
* use aluminized mylar mixture instead of kapton
C Geant COMPONENT : C5 ===> A=12 Z=6 W=5 <===                             96
      CALL AGSRESET                                                       96
      AG_A = 12                                                           96
      AG_Z = 6                                                            96
      AG_W = 5                                                            96
      AG_TITLE = 'C5'                                                     96
      AG_EXNAME = 'COMPONENT'                                             96
      AG_PARLIST = 'A_Z_W'                                                96
      CALL AXCOMPONENT                                                    96
C *                                                                       96
C Geant COMPONENT : H4 ===> A=1 Z=1 W=4 <===                              96
      CALL AGSRESET                                                       97
      AG_A = 1                                                            97
      AG_Z = 1                                                            97
      AG_W = 4                                                            97
      AG_TITLE = 'H4'                                                     97
      AG_EXNAME = 'COMPONENT'                                             97
      AG_PARLIST = 'A_Z_W'                                                97
      CALL AXCOMPONENT                                                    97
C *                                                                       97
C Geant COMPONENT : O2 ===> A=16 Z=8 W=2 <===                             97
      CALL AGSRESET                                                       98
      AG_A = 16                                                           98
      AG_Z = 8                                                            98
      AG_W = 2                                                            98
      AG_TITLE = 'O2'                                                     98
      AG_EXNAME = 'COMPONENT'                                             98
      AG_PARLIST = 'A_Z_W'                                                98
      CALL AXCOMPONENT                                                    98
C *                                                                       98
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.2302 <===                       98
      CALL AGSRESET                                                       99
      AG_A = 27                                                           99
      AG_Z = 13                                                           99
      AG_W = 0.2302                                                       99
      AG_TITLE = 'AL'                                                     99
      AG_EXNAME = 'COMPONENT'                                             99
      AG_PARLIST = 'A_Z_W'                                                99
      CALL AXCOMPONENT                                                    99
C *                                                                       99
C Geant MIXTURE : ALKAP ===> DENS=1.432 <===                              99
      CALL AGSRESET                                                       100
      AG_DENS = 1.432                                                     100
      AG_TITLE = 'ALKAP'                                                  100
      AG_EXNAME = 'MIXTURE'                                               100
      AG_PARLIST = 'DENS'                                                 100
      CALL AXMIXTURE                                                      100
C *                                                                       100
*
C use SSUP bank                                                           102
      BPATH_SSUP='SSUP*'                                                  102
      IDNUM_SSUP(1)=0                                                     102
      SSUP_VERSION=1                                                      102
      CALL AGDGETP('ITSPGEO','*',-999,'SSUP',NSSUP_VERSION,               102
     *RE_SSUP_VERSION,BPATH_SSUP,IDNUM_SSUP,1,MAP_SSUP,NAMES_SSUP,        102
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   102
      WRITE(*,*) '********* ITSP Support Cone Cable layer thickness: ',   103
     *SSUP_CABTHK                                                         103
*     EAST
      CALL AGSBEGM(MODULE,IPRIN)                                          105
C Check IPRIN<0                                                           105
      IF (IPRIN.LT.0) RETURN                                              105
C Create ITSP :                                                           105
C Call To Local Procedure ITSP                                            105
      ASSIGN 5020 TO BLOCK_ITSP                                           105
      GO TO  5010                                                         105
5020  CONTINUE                                                            105
C *                                                                       105
C Geant POSITION : ITSP ===> MOTHER='CAVE' THETAZ=180 <===                105
      CALL AGSRESET                                                       105
      AG_MOTHER = 'CAVE'                                                  105
      AG_THETAZ = 180                                                     105
      AG_TITLE = 'ITSP'                                                   105
      AG_EXNAME = 'POSITION'                                              105
      AG_PARLIST = 'MOTHER_THETAZ'                                        105
      CALL AXPOSITION                                                     105
C *                                                                       105
C use SSUP bank                                                           107
      BPATH_SSUP='SSUP*'                                                  107
      IDNUM_SSUP(1)=0                                                     107
      SSUP_VERSION=2                                                      107
      CALL AGDGETP('ITSPGEO','*',-999,'SSUP',NSSUP_VERSION,               107
     *RE_SSUP_VERSION,BPATH_SSUP,IDNUM_SSUP,1,MAP_SSUP,NAMES_SSUP,        107
     *BANK_SSUP,LEN_SSUP,LLEN_SSUP,LINK_SSUP,FLAG_SSUP)                   107
      WRITE(*,*) '********* ITSP Support Cone Cable layer thickness: ',   108
     *SSUP_CABTHK                                                         108
*     WEST
      CALL AGSBEGM(MODULE,IPRIN)                                          110
C Check IPRIN<0                                                           110
      IF (IPRIN.LT.0) RETURN                                              110
C Create ITSP :                                                           110
C Call To Local Procedure ITSP                                            110
      ASSIGN 5030 TO BLOCK_ITSP                                           110
      GO TO  5010                                                         110
5030  CONTINUE                                                            110
C *                                                                       110
C Geant POSITION : ITSP ===> MOTHER='CAVE' KONLY='ONLY' <===              110
      CALL AGSRESET                                                       110
      AG_MOTHER = 'CAVE'                                                  110
      AG_KONLY = 'ONLY'                                                   110
      AG_TITLE = 'ITSP'                                                   110
      AG_EXNAME = 'POSITION'                                              110
      AG_PARLIST = 'MOTHER_KONLY'                                         110
      CALL AXPOSITION                                                     110
C *                                                                       110
*
*******************************************************************************
*
      CALL AGSENDM                                                        115
      RETURN                                                              115
C Begin Local Procedure ITSP                                              115
C ITSP IS THE INNER TRACKER SUPPORT CONE MOTHER VOLUME                    115
5010  CONTINUE                                                            115
         AG_TITLE='ITSP'                                                  115
         AG_MODULE=MODULE                                                 115
         AG_MARK='V00'                                                    115
         CALL AGSPUSH                                                     115
         AG_MODULE=MODULE                                                 115
C    *                                                                    115
* There is a graphite-epoxy cone SGRA (guess = carbon)
* On top of that there are cones of plastic and copper for Twin-Ax STAP
* On top of that there are 10 water hoses SHOS, with water inside SHSW
* On top of that there is a layer of mylar SCMY
* On top of that there is a layer of aluminum SCAL
*
* The parts of the cones are labeled 1 through 4 as you go away from the
* interaction point
*
         CONE_THK1=SSUP_GRPHTHK                                           125
         CONE_THK2=CONE_THK1+SSUP_CABTHK+2*SSUP_HOSRMX                    126
         CONE_THK2=CONE_THK2+SSUP_WRPMYTHK+SSUP_WRPALTHK                  127
C    Geant MATERIAL : AIR ===> inherited arguments used <===              129
         CALL AGSRESET                                                    129
         AG_TITLE = 'AIR'                                                 129
         AG_EXNAME = 'MATERIAL'                                           129
         AG_PARLIST = 'NONE'                                              129
         CALL AXMATERIAL                                                  129
C    *                                                                    129
C    Geant ATTRIBUTE : ITSP ===> SEEN=1 COLO=1 <===                       129
         CALL AGSRESET                                                    130
         AG_SEEN = 1                                                      130
         AG_COLO = 1                                                      130
         AG_TITLE = 'ITSP'                                                130
         AG_EXNAME = 'ATTRIBUTE'                                          130
         AG_PARLIST = 'SEEN_COLO'                                         130
         CALL AXATTRIBUTE                                                 130
C    *                                                                    130
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=7, ZI ={SSUP_CONE1ZMN, S  130
C    SUP_RODLEN/2, SSUP_RODLEN/2, SSUP_RODLEN/2+SSUP_GRPHTHK, SSUP_RODLE  130
C    N/2+SSUP_GRPHTHK, SSUP_CONE3ZMX, SSUP_CONE4ZMX}, RMX={SSUP_CON1IDMN  130
C    +CONE_THK1, SSUP_CON1IDMN+CONE_THK1, SSUP_CON3IDMN+CONE_THK1, SSUP_  130
C    CON3IDMN+CONE_THK1, SSUP_CON3IDMN+CONE_THK2, SSUP_CON4IDMN+CONE_THK  130
C    2, SSUP_CON4IDMX+CONE_THK2}, RMN={SSUP_CON1IDMN, SSUP_CON1IDMN, SSU  130
C    P_CON1IDMN, SSUP_CON1IDMN, SSUP_CON3IDMN, SSUP_CON4IDMN, SSUP_CON4I  130
C    DMX} <===                                                            130
         CALL AGSRESET                                                    152
         AG_PHI1 = 0                                                      152
         AG_DPHI = 360                                                    152
         AG_NZ = 7                                                        152
         AG_ZI(1)=SSUP_CONE1ZMN                                           152
         AG_ZI(2)=SSUP_RODLEN/2                                           152
         AG_ZI(3)=SSUP_RODLEN/2                                           152
         AG_ZI(4)=SSUP_RODLEN/2+SSUP_GRPHTHK                              152
         AG_ZI(5)=SSUP_RODLEN/2+SSUP_GRPHTHK                              152
         AG_ZI(6)=SSUP_CONE3ZMX                                           152
         AG_ZI(7)=SSUP_CONE4ZMX                                           152
         AG_RMX(1)=SSUP_CON1IDMN+CONE_THK1                                152
         AG_RMX(2)=SSUP_CON1IDMN+CONE_THK1                                152
         AG_RMX(3)=SSUP_CON3IDMN+CONE_THK1                                152
         AG_RMX(4)=SSUP_CON3IDMN+CONE_THK1                                152
         AG_RMX(5)=SSUP_CON3IDMN+CONE_THK2                                152
         AG_RMX(6)=SSUP_CON4IDMN+CONE_THK2                                152
         AG_RMX(7)=SSUP_CON4IDMX+CONE_THK2                                152
         AG_RMN(1)=SSUP_CON1IDMN                                          152
         AG_RMN(2)=SSUP_CON1IDMN                                          152
         AG_RMN(3)=SSUP_CON1IDMN                                          152
         AG_RMN(4)=SSUP_CON1IDMN                                          152
         AG_RMN(5)=SSUP_CON3IDMN                                          152
         AG_RMN(6)=SSUP_CON4IDMN                                          152
         AG_RMN(7)=SSUP_CON4IDMX                                          152
         AG_TITLE = 'PCON'                                                152
         AG_EXNAME = 'SHAPE'                                              152
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMX_RMN'                           152
         CALL AXSHAPE                                                     152
C    *                                                                    152
*
C    Create SGRA :                                                        154
C    Call To Local Procedure SGRA                                         154
         ASSIGN 5050 TO BLOCK_SGRA                                        154
         GO TO  5040                                                      154
5050     CONTINUE                                                         154
C    *                                                                    154
C    Geant POSITION : SGRA ===> inherited arguments used <===             155
         CALL AGSRESET                                                    155
         AG_TITLE = 'SGRA'                                                155
         AG_EXNAME = 'POSITION'                                           155
         AG_PARLIST = 'NONE'                                              155
         CALL AXPOSITION                                                  155
C    *                                                                    155
C    Create STAP :                                                        156
C    Call To Local Procedure STAP                                         156
         ASSIGN 5070 TO BLOCK_STAP                                        156
         GO TO  5060                                                      156
5070     CONTINUE                                                         156
C    *                                                                    156
C    Geant POSITION : STAP ===> inherited arguments used <===             157
         CALL AGSRESET                                                    157
         AG_TITLE = 'STAP'                                                157
         AG_EXNAME = 'POSITION'                                           157
         AG_PARLIST = 'NONE'                                              157
         CALL AXPOSITION                                                  157
C    *                                                                    157
C    Create SCMY :                                                        158
C    Call To Local Procedure SCMY                                         158
         ASSIGN 5090 TO BLOCK_SCMY                                        158
         GO TO  5080                                                      158
5090     CONTINUE                                                         158
C    *                                                                    158
C    Geant POSITION : SCMY ===> inherited arguments used <===             159
         CALL AGSRESET                                                    159
         AG_TITLE = 'SCMY'                                                159
         AG_EXNAME = 'POSITION'                                           159
         AG_PARLIST = 'NONE'                                              159
         CALL AXPOSITION                                                  159
C    *                                                                    159
C    Create SCAL :                                                        160
C    Call To Local Procedure SCAL                                         160
         ASSIGN 5110 TO BLOCK_SCAL                                        160
         GO TO  5100                                                      160
5110     CONTINUE                                                         160
C    *                                                                    160
C    Geant POSITION : SCAL ===> inherited arguments used <===             161
         CALL AGSRESET                                                    161
         AG_TITLE = 'SCAL'                                                161
         AG_EXNAME = 'POSITION'                                           161
         AG_PARLIST = 'NONE'                                              161
         CALL AXPOSITION                                                  161
C    *                                                                    161
5011  CALL AGSPOPB                                                        161
      GO TO BLOCK_ITSP, (5010 ,5020,5030)                                 162
C End of Local Procedure                                                  162
*
*------------------------------------------------------------------------------
* -keep
C Begin Local Procedure SGRA                                              166
C SGRA IS THE GRAPHITE/EPOXY SUPPORT CONE                                 166
5040  CONTINUE                                                            166
         AG_TITLE='SGRA'                                                  166
         AG_MODULE=MODULE                                                 166
         AG_MARK='V00'                                                    166
         CALL AGSPUSH                                                     166
         AG_MODULE=MODULE                                                 166
C    *                                                                    166
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           167
         CALL AGSRESET                                                    167
         AG_TITLE = 'CARBON'                                              167
         AG_EXNAME = 'MATERIAL'                                           167
         AG_PARLIST = 'NONE'                                              167
         CALL AXMATERIAL                                                  167
C    *                                                                    167
C    Geant ATTRIBUTE : SGRA ===> SEEN=1 COLO=6 <===                       167
         CALL AGSRESET                                                    168
         AG_SEEN = 1                                                      168
         AG_COLO = 6                                                      168
         AG_TITLE = 'SGRA'                                                168
         AG_EXNAME = 'ATTRIBUTE'                                          168
         AG_PARLIST = 'SEEN_COLO'                                         168
         CALL AXATTRIBUTE                                                 168
C    *                                                                    168
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=7, ZI ={SSUP_RODLEN/2, S  168
C    SUP_RODLEN/2, SSUP_RODLEN/2+SSUP_GRPHTHK, SSUP_RODLEN/2+SSUP_GRPHTH  168
C    K, SSUP_CONE3ZMX, SSUP_CONE4ZMX}, RMX={SSUP_CON1IDMN+SSUP_GRPHTHK,   168
C    SSUP_CON3IDMN+SSUP_GRPHTHK, SSUP_CON3IDMN+SSUP_GRPHTHK, SSUP_CON3ID  168
C    MN+SSUP_GRPHTHK, SSUP_CON4IDMN+SSUP_GRPHTHK, SSUP_CON4IDMX+SSUP_GRP  168
C    HTHK}, RMN={SSUP_CON1IDMN, SSUP_CON1IDMN, SSUP_CON1IDMN, SSUP_CON3I  168
C    DMN, SSUP_CON4IDMN, SSUP_CON4IDMX} <===                              168
         CALL AGSRESET                                                    187
         AG_PHI1 = 0                                                      187
         AG_DPHI = 360                                                    187
         AG_NZ = 7                                                        187
         AG_ZI(1)=SSUP_RODLEN/2                                           187
         AG_ZI(2)=SSUP_RODLEN/2                                           187
         AG_ZI(3)=SSUP_RODLEN/2+SSUP_GRPHTHK                              187
         AG_ZI(4)=SSUP_RODLEN/2+SSUP_GRPHTHK                              187
         AG_ZI(5)=SSUP_CONE3ZMX                                           187
         AG_ZI(6)=SSUP_CONE4ZMX                                           187
         AG_RMX(1)=SSUP_CON1IDMN+SSUP_GRPHTHK                             187
         AG_RMX(2)=SSUP_CON3IDMN+SSUP_GRPHTHK                             187
         AG_RMX(3)=SSUP_CON3IDMN+SSUP_GRPHTHK                             187
         AG_RMX(4)=SSUP_CON3IDMN+SSUP_GRPHTHK                             187
         AG_RMX(5)=SSUP_CON4IDMN+SSUP_GRPHTHK                             187
         AG_RMX(6)=SSUP_CON4IDMX+SSUP_GRPHTHK                             187
         AG_RMN(1)=SSUP_CON1IDMN                                          187
         AG_RMN(2)=SSUP_CON1IDMN                                          187
         AG_RMN(3)=SSUP_CON1IDMN                                          187
         AG_RMN(4)=SSUP_CON3IDMN                                          187
         AG_RMN(5)=SSUP_CON4IDMN                                          187
         AG_RMN(6)=SSUP_CON4IDMX                                          187
         AG_TITLE = 'PCON'                                                187
         AG_EXNAME = 'SHAPE'                                              187
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMX_RMN'                           187
         CALL AXSHAPE                                                     187
C    *                                                                    187
5041  CALL AGSPOPB                                                        187
      GO TO BLOCK_SGRA, (5040 ,5050)                                      188
C End of Local Procedure                                                  188
*
*------------------------------------------------------------------------------
* -keep
C Begin Local Procedure STAP                                              192
C STAP IS THE PLASTIC PART OF THE TWIN-AX CABLE LAYER (GUESS POLYETHYLEN  192
C E)                                                                      192
5060  CONTINUE                                                            192
         AG_TITLE='STAP'                                                  192
         AG_MODULE=MODULE                                                 192
         AG_MARK='V00'                                                    192
         CALL AGSPUSH                                                     192
         AG_MODULE=MODULE                                                 192
C    *                                                                    192
         ROFFSET=SSUP_GRPHTHK                                             193
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           194
         CALL AGSRESET                                                    194
         AG_A = 12                                                        194
         AG_Z = 6                                                         194
         AG_W = 1                                                         194
         AG_TITLE = 'C'                                                   194
         AG_EXNAME = 'COMPONENT'                                          194
         AG_PARLIST = 'A_Z_W'                                             194
         CALL AXCOMPONENT                                                 194
C    *                                                                    194
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           194
         CALL AGSRESET                                                    195
         AG_A = 1                                                         195
         AG_Z = 1                                                         195
         AG_W = 2                                                         195
         AG_TITLE = 'H2'                                                  195
         AG_EXNAME = 'COMPONENT'                                          195
         AG_PARLIST = 'A_Z_W'                                             195
         CALL AXCOMPONENT                                                 195
C    *                                                                    195
C    Geant MIXTURE : CH2 ===> DENS=0.935 <===                             195
         CALL AGSRESET                                                    196
         AG_DENS = 0.935                                                  196
         AG_TITLE = 'CH2'                                                 196
         AG_EXNAME = 'MIXTURE'                                            196
         AG_PARLIST = 'DENS'                                              196
         CALL AXMIXTURE                                                   196
C    *                                                                    196
C    Geant ATTRIBUTE : STAP ===> SEEN=1 COLO=3 <===                       196
         CALL AGSRESET                                                    197
         AG_SEEN = 1                                                      197
         AG_COLO = 3                                                      197
         AG_TITLE = 'STAP'                                                197
         AG_EXNAME = 'ATTRIBUTE'                                          197
         AG_PARLIST = 'SEEN_COLO'                                         197
         CALL AXATTRIBUTE                                                 197
C    *                                                                    197
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3, ZI ={SSUP_RODLEN/2+SS  197
C    UP_GRPHTHK, SSUP_CONE3ZMX, SSUP_CONE4ZMX}, RMX={SSUP_CON3IDMN+ROFFS  197
C    ET+SSUP_CABTHK/2, SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2, SSUP_CON4IDM  197
C    X+ROFFSET+SSUP_CABTHK/2}, RMN={SSUP_CON3IDMN+ROFFSET, SSUP_CON4IDMN  197
C    +ROFFSET, SSUP_CON4IDMX+ROFFSET} <===                                197
         CALL AGSRESET                                                    207
         AG_PHI1 = 0                                                      207
         AG_DPHI = 360                                                    207
         AG_NZ = 3                                                        207
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              207
         AG_ZI(2)=SSUP_CONE3ZMX                                           207
         AG_ZI(3)=SSUP_CONE4ZMX                                           207
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_CABTHK/2                    207
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_CABTHK/2                    207
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_CABTHK/2                    207
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  207
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  207
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  207
         AG_TITLE = 'PCON'                                                207
         AG_EXNAME = 'SHAPE'                                              207
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMX_RMN'                           207
         CALL AXSHAPE                                                     207
C    *                                                                    207
5061  CALL AGSPOPB                                                        207
      GO TO BLOCK_STAP, (5060 ,5070)                                      208
C End of Local Procedure                                                  208
*
*------------------------------------------------------------------------------
* -keep
C Begin Local Procedure SCMY                                              212
C SCMY IS A MYLAR WRAP AROUND THE SUPPORT CONE                            212
5080  CONTINUE                                                            212
         AG_TITLE='SCMY'                                                  212
         AG_MODULE=MODULE                                                 212
         AG_MARK='V00'                                                    212
         CALL AGSPUSH                                                     212
         AG_MODULE=MODULE                                                 212
C    *                                                                    212
         ROFFSET=SSUP_GRPHTHK+SSUP_CABTHK+2.0*SSUP_HOSRMX                 213
C    Geant COMPONENT : C5 ===> A=12 Z=6 W=5 <===                          214
         CALL AGSRESET                                                    214
         AG_A = 12                                                        214
         AG_Z = 6                                                         214
         AG_W = 5                                                         214
         AG_TITLE = 'C5'                                                  214
         AG_EXNAME = 'COMPONENT'                                          214
         AG_PARLIST = 'A_Z_W'                                             214
         CALL AXCOMPONENT                                                 214
C    *                                                                    214
C    Geant COMPONENT : H4 ===> A=1 Z=1 W=4 <===                           214
         CALL AGSRESET                                                    215
         AG_A = 1                                                         215
         AG_Z = 1                                                         215
         AG_W = 4                                                         215
         AG_TITLE = 'H4'                                                  215
         AG_EXNAME = 'COMPONENT'                                          215
         AG_PARLIST = 'A_Z_W'                                             215
         CALL AXCOMPONENT                                                 215
C    *                                                                    215
C    Geant COMPONENT : O2 ===> A=16 Z=8 W=2 <===                          215
         CALL AGSRESET                                                    216
         AG_A = 16                                                        216
         AG_Z = 8                                                         216
         AG_W = 2                                                         216
         AG_TITLE = 'O2'                                                  216
         AG_EXNAME = 'COMPONENT'                                          216
         AG_PARLIST = 'A_Z_W'                                             216
         CALL AXCOMPONENT                                                 216
C    *                                                                    216
C    Geant MIXTURE : MYLAR ===> DENS=1.39 <===                            216
         CALL AGSRESET                                                    217
         AG_DENS = 1.39                                                   217
         AG_TITLE = 'MYLAR'                                               217
         AG_EXNAME = 'MIXTURE'                                            217
         AG_PARLIST = 'DENS'                                              217
         CALL AXMIXTURE                                                   217
C    *                                                                    217
C    Geant ATTRIBUTE : SCMY ===> SEEN=1 COLO=3 <===                       217
         CALL AGSRESET                                                    218
         AG_SEEN = 1                                                      218
         AG_COLO = 3                                                      218
         AG_TITLE = 'SCMY'                                                218
         AG_EXNAME = 'ATTRIBUTE'                                          218
         AG_PARLIST = 'SEEN_COLO'                                         218
         CALL AXATTRIBUTE                                                 218
C    *                                                                    218
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3, ZI ={SSUP_RODLEN/2+SS  218
C    UP_GRPHTHK, SSUP_CONE3ZMX, SSUP_CONE4ZMX}, RMX={SSUP_CON3IDMN+ROFFS  218
C    ET+SSUP_WRPMYTHK, SSUP_CON4IDMN+ROFFSET+SSUP_WRPMYTHK, SSUP_CON4IDM  218
C    X+ROFFSET+SSUP_WRPMYTHK}, RMN={SSUP_CON3IDMN+ROFFSET, SSUP_CON4IDMN  218
C    +ROFFSET, SSUP_CON4IDMX+ROFFSET} <===                                218
         CALL AGSRESET                                                    228
         AG_PHI1 = 0                                                      228
         AG_DPHI = 360                                                    228
         AG_NZ = 3                                                        228
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              228
         AG_ZI(2)=SSUP_CONE3ZMX                                           228
         AG_ZI(3)=SSUP_CONE4ZMX                                           228
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_WRPMYTHK                    228
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_WRPMYTHK                    228
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_WRPMYTHK                    228
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  228
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  228
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  228
         AG_TITLE = 'PCON'                                                228
         AG_EXNAME = 'SHAPE'                                              228
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMX_RMN'                           228
         CALL AXSHAPE                                                     228
C    *                                                                    228
5081  CALL AGSPOPB                                                        228
      GO TO BLOCK_SCMY, (5080 ,5090)                                      229
C End of Local Procedure                                                  229
*
*------------------------------------------------------------------------------
* -keep
C Begin Local Procedure SCAL                                              233
C SCAL IS THE ALUMINIZATION ON THE MYLAR WRAP AROUND THE SUPPORT CONE     233
5100  CONTINUE                                                            233
         AG_TITLE='SCAL'                                                  233
         AG_MODULE=MODULE                                                 233
         AG_MARK='V00'                                                    233
         CALL AGSPUSH                                                     233
         AG_MODULE=MODULE                                                 233
C    *                                                                    233
*     note combine with mylar a la Pavel's example
         ROFFSET=SSUP_GRPHTHK+SSUP_CABTHK+2.0*SSUP_HOSRMX+SSUP_WRPMYTHK   235
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        236
         CALL AGSRESET                                                    236
         AG_TITLE = 'ALUMINIUM'                                           236
         AG_EXNAME = 'MATERIAL'                                           236
         AG_PARLIST = 'NONE'                                              236
         CALL AXMATERIAL                                                  236
C    *                                                                    236
C    Geant ATTRIBUTE : SCAL ===> SEEN=1 COLO=2 <===                       236
         CALL AGSRESET                                                    237
         AG_SEEN = 1                                                      237
         AG_COLO = 2                                                      237
         AG_TITLE = 'SCAL'                                                237
         AG_EXNAME = 'ATTRIBUTE'                                          237
         AG_PARLIST = 'SEEN_COLO'                                         237
         CALL AXATTRIBUTE                                                 237
C    *                                                                    237
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3, ZI ={SSUP_RODLEN/2+SS  237
C    UP_GRPHTHK, SSUP_CONE3ZMX, SSUP_CONE4ZMX}, RMX={SSUP_CON3IDMN+ROFFS  237
C    ET+SSUP_WRPALTHK, SSUP_CON4IDMN+ROFFSET+SSUP_WRPALTHK, SSUP_CON4IDM  237
C    X+ROFFSET+SSUP_WRPALTHK}, RMN={SSUP_CON3IDMN+ROFFSET, SSUP_CON4IDMN  237
C    +ROFFSET, SSUP_CON4IDMX+ROFFSET} <===                                237
         CALL AGSRESET                                                    247
         AG_PHI1 = 0                                                      247
         AG_DPHI = 360                                                    247
         AG_NZ = 3                                                        247
         AG_ZI(1)=SSUP_RODLEN/2+SSUP_GRPHTHK                              247
         AG_ZI(2)=SSUP_CONE3ZMX                                           247
         AG_ZI(3)=SSUP_CONE4ZMX                                           247
         AG_RMX(1)=SSUP_CON3IDMN+ROFFSET+SSUP_WRPALTHK                    247
         AG_RMX(2)=SSUP_CON4IDMN+ROFFSET+SSUP_WRPALTHK                    247
         AG_RMX(3)=SSUP_CON4IDMX+ROFFSET+SSUP_WRPALTHK                    247
         AG_RMN(1)=SSUP_CON3IDMN+ROFFSET                                  247
         AG_RMN(2)=SSUP_CON4IDMN+ROFFSET                                  247
         AG_RMN(3)=SSUP_CON4IDMX+ROFFSET                                  247
         AG_TITLE = 'PCON'                                                247
         AG_EXNAME = 'SHAPE'                                              247
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMX_RMN'                           247
         CALL AXSHAPE                                                     247
C    *                                                                    247
5101  CALL AGSPOPB                                                        247
      GO TO BLOCK_SCAL, (5100 ,5110)                                      248
C End of Local Procedure                                                  248
*
*------------------------------------------------------------------------------
*
      END                                                                 252
