C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
***************************************************************************
* $Id: hpdtgeo.g,v 1.3 2006/10/21 18:38:03 potekhin Exp $
* $Log: hpdtgeo.g,v $
* Revision 1.3  2006/10/21 18:38:03  potekhin
* Added a small safety margin on Rin, used 'ONLY' option to keep hits
*
* Revision 1.2  2006/10/11 17:52:05  potekhin
* Corrected the angular offset, 60 instead of 70
*
* Revision 1.1  2006/09/15 19:55:10  potekhin
* New pixle detector requires new code.
*
*
* Modified 09/08/06
* the cell size and the support structure with the request of N.Smirnov
******************************************************************************
      SUBROUTINE HPDTGEO                                                  17
C HPDTGEO IS THE GEOMETRY OF THE STAR PIXEL DETECTOR                      17
      IMPLICIT NONE                                                       17
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       17
      DATA MODULE/'HPDTGEO'/, BANK_TITLE/'*'/                             17
      DATA MCOMMENT/'IS THE GEOMETRY OF THE STAR PIXEL DETECTOR'/         17
      LOGICAL FIRST                                                       17
      DATA FIRST/.TRUE./                                                  17
      SAVE FIRST                                                          17
      INTEGER JUMPAD                                                      17
      EXTERNAL JUMPAD                                                     17
      INTEGER OK                                                          17
      PARAMETER (OK=0)                                                    17
      INTEGER AG_I,AG_J                                                   17
      SAVE AG_I,AG_J                                                      17
C CREATED 09/01/06                                                        17
      DATA AG_I/0/,AG_J/0/                                                18
      CHARACTER*40 CREATED /'09/01/06'/                                   18
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  18
C AUTHOR SEVIL SALUR                                                      18
      CHARACTER*40 AUTHOR /'SEVIL SALUR'/                                 19
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  19
******************************************************************************
C - Advanced Geant interface                                              21
      CHARACTER*20 AG_MODULE,AG_TITLE,AG_EXNAME,AG_PARLIST,AG_CHDIR,      21
     *AG_MATERIAL,AG_MIXTURE,AG_COMPONENT,AG_MEDIUM                       21
      CHARACTER*4 AG_VOLUME,AG_MOTHER,AG_SHAPE,AG_CNICK,AG_KONLY,         21
     *AG_OPTION,AG_ORT,AG_MARK                                            21
      INTEGER AG_BEGCOM,AG_IVOLUME,AG_IMOTHER,AG_IGNUM,AG_ISHAPE,         21
     *AG_IMED,AG_IMAT,AG_IFIELD, AG_NLMAT,AG_IERROR,AG_NWBUF,AG_NPAR,     21
     *AG_ISTATUS,AG_IROT,AG_NBITS,AG_SERIAL, AG_ATTRIBUTE(6),AG_WORK,     21
     *AG_SEEN,AG_LSTY,AG_LWID,AG_COLO,AG_FILL,AG_JDU,AG_ORTI, AG_NDIV,    21
     *AG_IAXIS,AG_NDVMAX,AG_IPRIN,AG_RESET1,AG_RESET2,AG_BEGSCR,          21
     *AG_ENDSCR, AG_IDTYPE,AG_ISET,AG_IDET,AG_ISVOL,AG_LEVEL,AG_IRESER,   21
     *AG_IGEOM,AG_STANDALONE, AG_LSTACK,AG_NWUHIT,AG_NWUVOL,AG_MAGIC,     21
     *AG_LDETU,AG_NPDIV,AG_NPDV,AG_NZ,AG_NCOPY, AG_IDEBU,AG_IGRAP,        21
     *AG_IHIST,AG_IMFLD,AG_ISIMU,AG_ENDSAVE,AG_CODE,AG_TRKTYP, AG_MODE,   21
     *AG_ECODE,AG_PDG,IPRIN, AG_ALPHAN,AG_ALPHAK(10),IGOR                 21
      REAL AG_FIELDM,AG_TMAXFD,AG_STEMAX,AG_DEEMAX,AG_EPSIL,AG_STMIN,     21
     *AG_DENS,AG_RADL,AG_ABSL, AG_THETAX,AG_PHIX,AG_THETAY,AG_PHIY,       21
     *AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,AG_ALFAZ, AG_ALPHAL(10),        21
     *AG_ALPHAX,AG_ALPHAY,AG_ALPHAZ,AG_TYPE, AG_DX, AG_DX1, AG_DX2,       21
     *AG_DY, AG_DY1, AG_DY2, AG_DMAXMS, AG_TWIST, AG_THET, AG_THE1,       21
     *AG_THE2, AG_PHI, AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2,       21
     *AG_RMIN, AG_RMAX, AG_RMN, AG_RMX, AG_ZI , AG_RMN1, AG_RMN2,         21
     *AG_RMX1, AG_RMX2, AG_H1, AG_H2, AG_BL1, AG_BL2, AG_TL1, AG_TL2,     21
     *AG_DPHI, AG_DZ, AG_TWIS, AG_X, AG_Y, AG_Z, AG_A, AG_ZA, AG_W,       21
     *AG_STEP, AG_C0, AG_PAR, AG_AA, AG_ZZ, AG_WW, AG_LX, AG_LY, AG_LZ,   21
     *AG_HX, AG_HY, AG_HZ, AG_P1, AG_P2, AG_STACK,AG_UBUF,AG_XHMAX,       21
     *AG_YHMAX,AG_ZHMAX,AG_RHMAX,AG_FHMAX,AG_FHMIN,AG_BIN, AG_CHARGE,     21
     *AG_MASS,AG_TLIFE,AG_BRATIO                                          21
      PARAMETER (AG_LSTACK=130,AG_NWUHIT=10,AG_NWUVOL=3,AG_MAGIC=         21
     *-696969,AG_LDETU=250)                                               21
      COMMON/AGCGLOB/ AG_MODULE,AG_CHDIR,AG_LEVEL,AG_IDTYPE,AG_IERROR,    21
     *AG_STANDALONE,IPRIN, AG_IPRIN,AG_IGEOM,AG_IDEBU,AG_IGRAP,AG_IHIST,  21
     *AG_IMFLD,AG_ISIMU                                                   21
C Inherited variables a: saved during internal calls                      21
C b: reset at the entry in a block and then saved                         21
      COMMON/AGCPARA/ AG_BEGCOM, AG_IVOLUME, AG_IMOTHER, AG_IGNUM,        21
     *AG_ISHAPE, AG_IMED, AG_IMAT, AG_IFIELD, AG_FIELDM, AG_TMAXFD,       21
     *AG_STEMAX, AG_DEEMAX, AG_EPSIL, AG_STMIN, AG_DENS, AG_RADL,         21
     *AG_ABSL, AG_DX, AG_DX1, AG_DX2, AG_DY, AG_DY1, AG_DY2, AG_RMN1,     21
     *AG_RMN2, AG_RMX1, AG_RMX2, AG_THET, AG_THE1, AG_THE2, AG_PHI,       21
     *AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2, AG_H1, AG_BL1,         21
     *AG_TL1, AG_H2, AG_BL2, AG_TL2, AG_RMIN, AG_RMAX, AG_DPHI,           21
     *AG_NPDIV, AG_NZ, AG_DZ, AG_TWIS, AG_LX, AG_LY, AG_LZ, AG_HX,        21
     *AG_HY, AG_HZ, AG_A, AG_ZA, AG_W, AG_NLMAT, AG_WORK, AG_SEEN,        21
     *AG_LSTY, AG_LWID, AG_COLO, AG_FILL, AG_SERIAL, AG_ISVOL,            21
     *AG_ISTATUS, AG_ZI(16), AG_RMN(16), AG_RMX(16), AG_VOLUME,           21
     *AG_MOTHER, AG_SHAPE, AG_CNICK, AG_ENDSAVE, AG_RESET1, AG_THETAX,    21
     *AG_PHIX,AG_THETAY,AG_PHIY,AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,      21
     *AG_ALFAZ, AG_X,AG_Y,AG_Z, AG_STEP,AG_C0,AG_NDIV,AG_IAXIS,           21
     *AG_NDVMAX,AG_ORTI,AG_NCOPY, AG_ALPHAN,AG_ALPHAK,AG_ALPHAL,          21
     *AG_RESET2, AG_KONLY, AG_ORT, AG_MARK                                21
C local variables valid inside same block                                 21
      COMMON/AGCLOCA/ AG_BEGSCR,AG_UBUF(100),AG_PAR(100),AG_AA(20),       21
     *AG_ZZ(20),AG_WW(20),AG_NWBUF, AG_XHMAX,AG_YHMAX,AG_ZHMAX,AG_RHMAX,  21
     *AG_FHMAX,AG_FHMIN,AG_NBITS,AG_BIN,AG_TYPE, AG_IROT,AG_NPAR,         21
     *AG_ISET,AG_IDET,AG_JDU,AG_IRESER, AG_ENDSCR, AG_TITLE,AG_EXNAME,    21
     *AG_PARLIST,AG_MATERIAL,AG_MIXTURE, AG_COMPONENT,AG_MEDIUM,          21
     *AG_OPTION                                                           21
      COMMON/AGCPART/AG_CODE,AG_TRKTYP,AG_MASS,AG_CHARGE,AG_TLIFE,        21
     *AG_BRATIO(6),AG_MODE(6),AG_PDG,AG_ECODE                             21
      COMMON/AGCSTAC/ AG_STACK(AG_LSTACK,15)                              21
C temporarly                                                              21
      EQUIVALENCE (AG_ATTRIBUTE,AG_WORK),(AG_STEMAX,AG_DMAXMS),           21
     *(AG_ALPHAX,AG_ALFAX), (AG_ALPHAY,AG_ALFAY),(AG_ALPHAZ,AG_ALFAZ),    21
     *(AG_TWIST,AG_TWIS),(AG_NPDIV,AG_NPDV),  (AG_P1,AG_HX),(AG_P2,       21
     *AG_HY)                                                              21
      REAL MIL_P,INCH_P                                                   21
      PARAMETER (INCH_P=2.54,MIL_P=0.00254)                               21
C - GEANT description of logical units                                    21
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            21
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      21
      COMMON/GCMAIL/ CHMAIL                                               21
      CHARACTER*132 CHMAIL                                                21
*
      REAL ANGLE,ANGLEPOS,ANGLECORR,RADDEG, ANGLESEP                      23
      INTEGER NLADDER,NSECTOR, NEXTRALADDER, NWAFER, NCELL                24
      INTEGER BLOCK_YPXM                                                  26
      INTEGER BLOCK_YPSC                                                  26
      INTEGER BLOCK_YPLM                                                  26
      INTEGER BLOCK_YPLP                                                  26
      INTEGER BLOCK_YPLA                                                  26
      INTEGER BLOCK_YPWM                                                  26
      INTEGER BLOCK_YPWP                                                  26
      INTEGER BLOCK_YPWR                                                  26
*
*                      WaferLen, PWaferLen,GuardLen,
C -                                                                       33
      REAL YPXG_RIN                                                       33
      EQUIVALENCE (BANK_YPXG(3),YPXG_RIN,RE_YPXG_RIN),(NAMES_YPXG(1),     33
     *NYPXG_RIN)                                                          33
      EQUIVALENCE (COMMEN_YPXG(1),CYPXG_RIN),(MAP_YPXG(1,1),MYPXG_RIN)    33
      CHARACTER CYPXG_RIN*40/' '/                                         33
      CHARACTER NYPXG_RIN*12/'R.RIN'/                                     33
      INTEGER MYPXG_RIN(2)/1,1/                                           33
      REAL RE_YPXG_RIN                                                    33
C -                                                                       33
      REAL YPXG_ROUT                                                      33
      EQUIVALENCE (BANK_YPXG(4),YPXG_ROUT,RE_YPXG_ROUT),(NAMES_YPXG(2),   33
     *NYPXG_ROUT)                                                         33
      EQUIVALENCE (COMMEN_YPXG(2),CYPXG_ROUT),(MAP_YPXG(1,2),             33
     *MYPXG_ROUT)                                                         33
      CHARACTER CYPXG_ROUT*40/' '/                                        33
      CHARACTER NYPXG_ROUT*12/'R.ROUT'/                                   33
      INTEGER MYPXG_ROUT(2)/1,1/                                          33
      REAL RE_YPXG_ROUT                                                   33
C -                                                                       33
      REAL YPXG_TOTALLENGTH                                               33
      EQUIVALENCE (BANK_YPXG(5),YPXG_TOTALLENGTH,RE_YPXG_TOTALLENGTH),    33
     *(NAMES_YPXG(3),NYPXG_TOTALLENGTH)                                   33
      EQUIVALENCE (COMMEN_YPXG(3),CYPXG_TOTALLENGTH),(MAP_YPXG(1,3),      33
     *MYPXG_TOTALLENGTH)                                                  33
      CHARACTER CYPXG_TOTALLENGTH*40/' '/                                 33
      CHARACTER NYPXG_TOTALLENGTH*12/'R.TOTALLENGTH'/                     33
      INTEGER MYPXG_TOTALLENGTH(2)/1,1/                                   33
      REAL RE_YPXG_TOTALLENGTH                                            33
C -                                                                       33
      REAL YPXG_ACLENGTH                                                  33
      EQUIVALENCE (BANK_YPXG(6),YPXG_ACLENGTH,RE_YPXG_ACLENGTH),          33
     *(NAMES_YPXG(4),NYPXG_ACLENGTH)                                      33
      EQUIVALENCE (COMMEN_YPXG(4),CYPXG_ACLENGTH),(MAP_YPXG(1,4),         33
     *MYPXG_ACLENGTH)                                                     33
      CHARACTER CYPXG_ACLENGTH*40/' '/                                    33
      CHARACTER NYPXG_ACLENGTH*12/'R.ACLENGTH'/                           33
      INTEGER MYPXG_ACLENGTH(2)/1,1/                                      33
      REAL RE_YPXG_ACLENGTH                                               33
C -                                                                       33
      REAL YPXG_CELLLEN                                                   33
      EQUIVALENCE (BANK_YPXG(7),YPXG_CELLLEN,RE_YPXG_CELLLEN),            33
     *(NAMES_YPXG(5),NYPXG_CELLLEN)                                       33
      EQUIVALENCE (COMMEN_YPXG(5),CYPXG_CELLLEN),(MAP_YPXG(1,5),          33
     *MYPXG_CELLLEN)                                                      33
      CHARACTER CYPXG_CELLLEN*40/' '/                                     33
      CHARACTER NYPXG_CELLLEN*12/'R.CELLLEN'/                             33
      INTEGER MYPXG_CELLLEN(2)/1,1/                                       33
      REAL RE_YPXG_CELLLEN                                                33
C -                                                                       33
      REAL YPXG_PCELLLEN                                                  33
      EQUIVALENCE (BANK_YPXG(8),YPXG_PCELLLEN,RE_YPXG_PCELLLEN),          33
     *(NAMES_YPXG(6),NYPXG_PCELLLEN)                                      33
      EQUIVALENCE (COMMEN_YPXG(6),CYPXG_PCELLLEN),(MAP_YPXG(1,6),         33
     *MYPXG_PCELLLEN)                                                     33
      CHARACTER CYPXG_PCELLLEN*40/' '/                                    33
      CHARACTER NYPXG_PCELLLEN*12/'R.PCELLLEN'/                           33
      INTEGER MYPXG_PCELLLEN(2)/1,1/                                      33
      REAL RE_YPXG_PCELLLEN                                               33
C -                                                                       33
      REAL YPXG_CELLGUARDLEN                                              33
      EQUIVALENCE (BANK_YPXG(9),YPXG_CELLGUARDLEN,RE_YPXG_CELLGUARDLEN),  33
     *(NAMES_YPXG(7),NYPXG_CELLGUARDLEN)                                  33
      EQUIVALENCE (COMMEN_YPXG(7),CYPXG_CELLGUARDLEN),(MAP_YPXG(1,7),     33
     *MYPXG_CELLGUARDLEN)                                                 33
      CHARACTER CYPXG_CELLGUARDLEN*40/' '/                                33
      CHARACTER NYPXG_CELLGUARDLEN*12/'R.CELLGUARDLEN'/                   33
      INTEGER MYPXG_CELLGUARDLEN(2)/1,1/                                  33
      REAL RE_YPXG_CELLGUARDLEN                                           33
C -                                                                       33
      REAL YPXG_LADDERWIDTH                                               33
      EQUIVALENCE (BANK_YPXG(10),YPXG_LADDERWIDTH,RE_YPXG_LADDERWIDTH),   33
     *(NAMES_YPXG(8),NYPXG_LADDERWIDTH)                                   33
      EQUIVALENCE (COMMEN_YPXG(8),CYPXG_LADDERWIDTH),(MAP_YPXG(1,8),      33
     *MYPXG_LADDERWIDTH)                                                  33
      CHARACTER CYPXG_LADDERWIDTH*40/' '/                                 33
      CHARACTER NYPXG_LADDERWIDTH*12/'R.LADDERWIDTH'/                     33
      INTEGER MYPXG_LADDERWIDTH(2)/1,1/                                   33
      REAL RE_YPXG_LADDERWIDTH                                            33
C -                                                                       33
      REAL YPXG_PASSLADDERW                                               33
      EQUIVALENCE (BANK_YPXG(11),YPXG_PASSLADDERW,RE_YPXG_PASSLADDERW),   33
     *(NAMES_YPXG(9),NYPXG_PASSLADDERW)                                   33
      EQUIVALENCE (COMMEN_YPXG(9),CYPXG_PASSLADDERW),(MAP_YPXG(1,9),      33
     *MYPXG_PASSLADDERW)                                                  33
      CHARACTER CYPXG_PASSLADDERW*40/' '/                                 33
      CHARACTER NYPXG_PASSLADDERW*12/'R.PASSLADDERW'/                     33
      INTEGER MYPXG_PASSLADDERW(2)/1,1/                                   33
      REAL RE_YPXG_PASSLADDERW                                            33
C -                                                                       33
      REAL YPXG_LADDERTHK                                                 33
      EQUIVALENCE (BANK_YPXG(12),YPXG_LADDERTHK,RE_YPXG_LADDERTHK),       33
     *(NAMES_YPXG(10),NYPXG_LADDERTHK)                                    33
      EQUIVALENCE (COMMEN_YPXG(10),CYPXG_LADDERTHK),(MAP_YPXG(1,10),      33
     *MYPXG_LADDERTHK)                                                    33
      CHARACTER CYPXG_LADDERTHK*40/' '/                                   33
      CHARACTER NYPXG_LADDERTHK*12/'R.LADDERTHK'/                         33
      INTEGER MYPXG_LADDERTHK(2)/1,1/                                     33
      REAL RE_YPXG_LADDERTHK                                              33
C -                                                                       33
      REAL YPXG_CARBONTHK                                                 33
      EQUIVALENCE (BANK_YPXG(13),YPXG_CARBONTHK,RE_YPXG_CARBONTHK),       33
     *(NAMES_YPXG(11),NYPXG_CARBONTHK)                                    33
      EQUIVALENCE (COMMEN_YPXG(11),CYPXG_CARBONTHK),(MAP_YPXG(1,11),      33
     *MYPXG_CARBONTHK)                                                    33
      CHARACTER CYPXG_CARBONTHK*40/' '/                                   33
      CHARACTER NYPXG_CARBONTHK*12/'R.CARBONTHK'/                         33
      INTEGER MYPXG_CARBONTHK(2)/1,1/                                     33
      REAL RE_YPXG_CARBONTHK                                              33
C -                                                                       33
      REAL YPXG_PASSIVETHK                                                33
      EQUIVALENCE (BANK_YPXG(14),YPXG_PASSIVETHK,RE_YPXG_PASSIVETHK),     33
     *(NAMES_YPXG(12),NYPXG_PASSIVETHK)                                   33
      EQUIVALENCE (COMMEN_YPXG(12),CYPXG_PASSIVETHK),(MAP_YPXG(1,12),     33
     *MYPXG_PASSIVETHK)                                                   33
      CHARACTER CYPXG_PASSIVETHK*40/' '/                                  33
      CHARACTER NYPXG_PASSIVETHK*12/'R.PASSIVETHK'/                       33
      INTEGER MYPXG_PASSIVETHK(2)/1,1/                                    33
      REAL RE_YPXG_PASSIVETHK                                             33
C -                                                                       33
      REAL YPXG_ACTIVETHK                                                 33
      EQUIVALENCE (BANK_YPXG(15),YPXG_ACTIVETHK,RE_YPXG_ACTIVETHK),       33
     *(NAMES_YPXG(13),NYPXG_ACTIVETHK)                                    33
      EQUIVALENCE (COMMEN_YPXG(13),CYPXG_ACTIVETHK),(MAP_YPXG(1,13),      33
     *MYPXG_ACTIVETHK)                                                    33
      CHARACTER CYPXG_ACTIVETHK*40/' '/                                   33
      CHARACTER NYPXG_ACTIVETHK*12/'R.ACTIVETHK'/                         33
      INTEGER MYPXG_ACTIVETHK(2)/1,1/                                     33
      REAL RE_YPXG_ACTIVETHK                                              33
C -                                                                       33
      REAL YPXG_READOUTTHK                                                33
      EQUIVALENCE (BANK_YPXG(16),YPXG_READOUTTHK,RE_YPXG_READOUTTHK),     33
     *(NAMES_YPXG(14),NYPXG_READOUTTHK)                                   33
      EQUIVALENCE (COMMEN_YPXG(14),CYPXG_READOUTTHK),(MAP_YPXG(1,14),     33
     *MYPXG_READOUTTHK)                                                   33
      CHARACTER CYPXG_READOUTTHK*40/' '/                                  33
      CHARACTER NYPXG_READOUTTHK*12/'R.READOUTTHK'/                       33
      INTEGER MYPXG_READOUTTHK(2)/1,1/                                    33
      REAL RE_YPXG_READOUTTHK                                             33
C -                                                                       33
      REAL YPXG_R                                                         33
      EQUIVALENCE (BANK_YPXG(17),YPXG_R,RE_YPXG_R),(NAMES_YPXG(15),       33
     *NYPXG_R)                                                            33
      EQUIVALENCE (COMMEN_YPXG(15),CYPXG_R),(MAP_YPXG(1,15),MYPXG_R)      33
      CHARACTER CYPXG_R*40/' '/                                           33
      CHARACTER NYPXG_R*12/'R.R'/                                         33
      INTEGER MYPXG_R(2)/1,1/                                             33
      REAL RE_YPXG_R                                                      33
C -                                                                       33
      REAL YPXG_A                                                         33
      EQUIVALENCE (BANK_YPXG(18),YPXG_A,RE_YPXG_A),(NAMES_YPXG(16),       33
     *NYPXG_A)                                                            33
      EQUIVALENCE (COMMEN_YPXG(16),CYPXG_A),(MAP_YPXG(1,16),MYPXG_A)      33
      CHARACTER CYPXG_A*40/' '/                                           33
      CHARACTER NYPXG_A*12/'R.A'/                                         33
      INTEGER MYPXG_A(2)/1,1/                                             33
      REAL RE_YPXG_A                                                      33
C -                                                                       33
      REAL YPXG_POFFSET                                                   33
      EQUIVALENCE (BANK_YPXG(19),YPXG_POFFSET,RE_YPXG_POFFSET),           33
     *(NAMES_YPXG(17),NYPXG_POFFSET)                                      33
      EQUIVALENCE (COMMEN_YPXG(17),CYPXG_POFFSET),(MAP_YPXG(1,17),        33
     *MYPXG_POFFSET)                                                      33
      CHARACTER CYPXG_POFFSET*40/' '/                                     33
      CHARACTER NYPXG_POFFSET*12/'R.POFFSET'/                             33
      INTEGER MYPXG_POFFSET(2)/1,1/                                       33
      REAL RE_YPXG_POFFSET                                                33
C -                                                                       33
      REAL YPXG_AOFFSET                                                   33
      EQUIVALENCE (BANK_YPXG(20),YPXG_AOFFSET,RE_YPXG_AOFFSET),           33
     *(NAMES_YPXG(18),NYPXG_AOFFSET)                                      33
      EQUIVALENCE (COMMEN_YPXG(18),CYPXG_AOFFSET),(MAP_YPXG(1,18),        33
     *MYPXG_AOFFSET)                                                      33
      CHARACTER CYPXG_AOFFSET*40/' '/                                     33
      CHARACTER NYPXG_AOFFSET*12/'R.AOFFSET'/                             33
      INTEGER MYPXG_AOFFSET(2)/1,1/                                       33
      REAL RE_YPXG_AOFFSET                                                33
C -                                                                       33
      REAL YPXG_NUMBERLADDER                                              33
      EQUIVALENCE (BANK_YPXG(21),YPXG_NUMBERLADDER,                       33
     *RE_YPXG_NUMBERLADDER),(NAMES_YPXG(19),NYPXG_NUMBERLADDER)           33
      EQUIVALENCE (COMMEN_YPXG(19),CYPXG_NUMBERLADDER),(MAP_YPXG(1,19),   33
     *MYPXG_NUMBERLADDER)                                                 33
      CHARACTER CYPXG_NUMBERLADDER*40/' '/                                33
      CHARACTER NYPXG_NUMBERLADDER*12/'R.NUMBERLADDER'/                   33
      INTEGER MYPXG_NUMBERLADDER(2)/1,1/                                  33
      REAL RE_YPXG_NUMBERLADDER                                           33
C -                                                                       33
      REAL YPXG_NUMBERCELL                                                33
      EQUIVALENCE (BANK_YPXG(22),YPXG_NUMBERCELL,RE_YPXG_NUMBERCELL),     33
     *(NAMES_YPXG(20),NYPXG_NUMBERCELL)                                   33
      EQUIVALENCE (COMMEN_YPXG(20),CYPXG_NUMBERCELL),(MAP_YPXG(1,20),     33
     *MYPXG_NUMBERCELL)                                                   33
      CHARACTER CYPXG_NUMBERCELL*40/' '/                                  33
      CHARACTER NYPXG_NUMBERCELL*12/'R.NUMBERCELL'/                       33
      INTEGER MYPXG_NUMBERCELL(2)/1,1/                                    33
      REAL RE_YPXG_NUMBERCELL                                             33
C -                                                                       33
      INTEGER LEN_YPXG,LLEN_YPXG,LINK_YPXG                                33
      PARAMETER (LEN_YPXG=22)                                             33
      PARAMETER (LLEN_YPXG=20)                                            33
      INTEGER FLAG_YPXG/0/,MAP_YPXG(2,LLEN_YPXG),IDNUM_YPXG(10)           33
      CHARACTER NAMES_YPXG(LLEN_YPXG)*12,COMMEN_YPXG(LLEN_YPXG)*40        33
      DATA IDNUM_YPXG/10*0/                                               33
      REAL BANK_YPXG(LEN_YPXG)                                            33
      CHARACTER*60 BPATH_YPXG/' '/                                        33
      SAVE FLAG_YPXG,MAP_YPXG,IDNUM_YPXG,NAMES_YPXG,COMMEN_YPXG,          33
     *BPATH_YPXG                                                          33
      DATA LINK_YPXG/0/,BANK_YPXG/LEN_YPXG*0/                             33
C *                                                                       33
*
* -----------------------------------------------------------------------------
*
C *                                                                       37
      CALL AGSBEGM(MODULE,IPRIN)                                          37
      IF (IPRIN.LT.0) RETURN                                              37
C Filling bank YPXG - PIXEL DETECTOR DATA                                 37
      BANK_TITLE='PIXEL DETECTOR DATA'                                    37
      BPATH_YPXG='YPXG*'                                                  37
      IDNUM_YPXG(1)=0                                                     38
C INNER RADIUS IN X OR Y                                                  38
      YPXG_RIN=8.61                                                       38
      CYPXG_RIN='INNER RADIUS IN X OR Y'                                  38
C OUTER RADIUS IN X OR Y                                                  39
      YPXG_ROUT=11.                                                       39
      CYPXG_ROUT='OUTER RADIUS IN X OR Y'                                 39
C OVERAL LENGTH OF THE DETECTOR Z DIRECTION                               40
      YPXG_TOTALLENGTH=49.5                                               40
      CYPXG_TOTALLENGTH='OVERAL LENGTH OF THE DETECTOR Z DIRECTION'       40
C ACTIVE LENGTH OF THE DETECTOR Z DIRECTION                               41
      YPXG_ACLENGTH=28.288                                                41
      CYPXG_ACLENGTH='ACTIVE LENGTH OF THE DETECTOR Z DIRECTION'          41
*      WaferLen   =  7.072      ! Active silicon Wafer Length
*      PWaferLen  =  0.001      ! Spacing between two Wafers
*      GuardLen =    0.112      ! The space around the Wafer (Frame)
C ACTIVE SILICON CELL LENGTH                                              45
      YPXG_CELLLEN=1.36                                                   45
      CYPXG_CELLLEN='ACTIVE SILICON CELL LENGTH'                          45
C SPACING BETWEEN TWO CELLS                                               46
      YPXG_PCELLLEN=0.001                                                 46
      CYPXG_PCELLLEN='SPACING BETWEEN TWO CELLS'                          46
C THE SPACE AROUND THE CELLS (FRAME)                                      47
      YPXG_CELLGUARDLEN=0.112                                             47
      CYPXG_CELLGUARDLEN='THE SPACE AROUND THE CELLS (FRAME)'             47
*
C LADDER WIDTH                                                            51
      YPXG_LADDERWIDTH=1.28                                               51
      CYPXG_LADDERWIDTH='LADDER WIDTH'                                    51
C PASSIVE LADDER WIDTH                                                    52
      YPXG_PASSLADDERW=1.68                                               52
      CYPXG_PASSLADDERW='PASSIVE LADDER WIDTH'                            52
*
C TOTAL LADDER THICKNESS                                                  54
      YPXG_LADDERTHK=0.1780                                               54
      CYPXG_LADDERTHK='TOTAL LADDER THICKNESS'                            54
C PASSIVE MYLAR THICKNESS                                                 55
      YPXG_PASSIVETHK=0.1380                                              55
      CYPXG_PASSIVETHK='PASSIVE MYLAR THICKNESS'                          55
C ACTIVE CARBON THICKNESS                                                 56
      YPXG_CARBONTHK=0.056                                                56
      CYPXG_CARBONTHK='ACTIVE CARBON THICKNESS'                           56
C ACTIVE SILICON THICKNESS                                                57
      YPXG_ACTIVETHK=0.0200                                               57
      CYPXG_ACTIVETHK='ACTIVE SILICON THICKNESS'                          57
C INACTIVE SILICON FOR READOUT                                            58
      YPXG_READOUTTHK=0.0200                                              58
      CYPXG_READOUTTHK='INACTIVE SILICON FOR READOUT'                     58
*
C 1ST LADDER NOMINAL RADIUS                                               61
      YPXG_R=9.1                                                          61
      CYPXG_R='1ST LADDER NOMINAL RADIUS'                                 61
C 1ST LADDER NOMINAL POSITION ANGLE                                       62
      YPXG_A=3.75                                                         62
      CYPXG_A='1ST LADDER NOMINAL POSITION ANGLE'                         62
C ANGULAR OFFSET                                                          63
      YPXG_AOFFSET=60.0                                                   63
      CYPXG_AOFFSET='ANGULAR OFFSET'                                      63
C NUMBER OF LADDERS                                                       64
      YPXG_NUMBERLADDER=48                                                64
      CYPXG_NUMBERLADDER='NUMBER OF LADDERS'                              64
C NUMBER OF CELLS                                                         65
      YPXG_NUMBERCELL=20                                                  65
      CYPXG_NUMBERCELL='NUMBER OF CELLS'                                  65
*
C POSITION OFFSET (SHIFT)                                                 67
      YPXG_POFFSET=0.0                                                    67
      CYPXG_POFFSET='POSITION OFFSET (SHIFT)'                             67
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'YPXG',    68
     *BPATH_YPXG,IDNUM_YPXG,1, MAP_YPXG,NAMES_YPXG,COMMEN_YPXG,           68
     *BANK_YPXG,LEN_YPXG,LLEN_YPXG,LINK_YPXG,FLAG_YPXG)                   68
C *                                                                       68
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'YPXG',    68
     *BPATH_YPXG,IDNUM_YPXG,1, MAP_YPXG,NAMES_YPXG,COMMEN_YPXG,           68
     *BANK_YPXG,LEN_YPXG,LLEN_YPXG,LINK_YPXG,FLAG_YPXG)                   68
******************************************************
C use default YPXG bank                                                   72
      BPATH_YPXG='YPXG*'                                                  72
      IDNUM_YPXG(1)=0                                                     72
      CALL AGDGETP('HPDTGEO','*',-999,'YPXG',' ',BANK_YPXG, BPATH_YPXG,   72
     *IDNUM_YPXG,1,MAP_YPXG,NAMES_YPXG,BANK_YPXG,LEN_YPXG,LLEN_YPXG,      72
     *LINK_YPXG,FLAG_YPXG)                                                72
*
      RADDEG=3.14159265/180.0                                             74
      ANGLE = YPXG_A                                                      75
      ANGLESEP= 360/YPXG_NUMBERLADDER                                     76
      CALL AGSBEGM(MODULE,IPRIN)                                          78
C Check IPRIN<0                                                           78
      IF (IPRIN.LT.0) RETURN                                              78
C Create YPXM :! CREATE THE OBJECT FOR THE POSITION IN THE CAVE FOR THE   78
C DETECTOR                                                                78
C Call To Local Procedure YPXM                                            78
      ASSIGN 5020 TO BLOCK_YPXM                                           78
      GO TO  5010                                                         78
5020  CONTINUE                                                            78
C *                                                                       78
C Geant POSITION : YPXM ===> MOTHER='CAVE' KONLY='ONLY' <===              79
      CALL AGSRESET                                                       79
      AG_MOTHER = 'CAVE'                                                  79
      AG_KONLY = 'ONLY'                                                   79
      AG_TITLE = 'YPXM'                                                   79
      AG_EXNAME = 'POSITION'                                              79
      AG_PARLIST = 'MOTHER_KONLY'                                         79
      CALL AXPOSITION                                                     79
C *                                                                       79
* -----------------------------------------------------------------------------
      CALL AGSENDM                                                        82
      RETURN                                                              82
C Begin Local Procedure YPXM                                              82
C YPXM IS THE MOTHER OF THE PIXEL DETECTOR VOLUMES !!DEFINE EACH OBJECT   82
5010  CONTINUE                                                            82
         AG_TITLE='YPXM'                                                  82
         AG_MODULE=MODULE                                                 82
         AG_MARK='V00'                                                    82
         CALL AGSPUSH                                                     82
         AG_MODULE=MODULE                                                 82
C    *                                                                    82
C    Geant MATERIAL : AIR ===> inherited arguments used <===              83
         CALL AGSRESET                                                    83
         AG_TITLE = 'AIR'                                                 83
         AG_EXNAME = 'MATERIAL'                                           83
         AG_PARLIST = 'NONE'                                              83
         CALL AXMATERIAL                                                  83
C    *                                                                    83
C    Geant ATTRIBUTE : YPXM ===> SEEN=1 COLO=2 <===                       83
         CALL AGSRESET                                                    84
         AG_SEEN = 1                                                      84
         AG_COLO = 2                                                      84
         AG_TITLE = 'YPXM'                                                84
         AG_EXNAME = 'ATTRIBUTE'                                          84
         AG_PARLIST = 'SEEN_COLO'                                         84
         CALL AXATTRIBUTE                                                 84
C    *                                                                    84
C    Geant SHAPE : TUBE ===> RMIN=YPXG_RIN RMAX=YPXG_ROUT DZ=YPXG_TOTALL  86
C    ENGTH/2.0 <===                                                       86
         CALL AGSRESET                                                    86
         AG_RMIN = YPXG_RIN                                               86
         AG_RMAX = YPXG_ROUT                                              86
         AG_DZ = YPXG_TOTALLENGTH/2.0                                     86
         AG_TITLE = 'TUBE'                                                86
         AG_EXNAME = 'SHAPE'                                              86
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      86
         CALL AXSHAPE                                                     86
C    *                                                                    86
C    *                                                                    88
         NLADDER=1                                                        88
            GO TO 5033                                                    88
5031        NLADDER=NLADDER+1                                             88
5033        IF(NLADDER-(YPXG_NUMBERLADDER).GT.0) GO TO 5032               88
C       INNER LOOP, CREATE LADDERS INSIDE THE SECTOR                      88
C       +ANGLECORR SEE ABOVE COMMENT                                      89
            ANGLEPOS = ANGLE*RADDEG + ANGLESEP*(NLADDER-1)*RADDEG         89
C       Create YPLM :                                                     92
C       Call To Local Procedure YPLM                                      92
            ASSIGN 5050 TO BLOCK_YPLM                                     92
            GO TO  5040                                                   92
5050        CONTINUE                                                      92
C       *                                                                 92
C       Geant POSITION : YPLM ===> X=YPXG_R*COS(ANGLEPOS) Y=YPXG_R*SIN(A  92
C       NGLEPOS), Z=0.0 ALPHAZ=YPXG_AOFFSET+ANGLE+ANGLESEP*(NLADDER-1) <  92
C       ===                                                               92
            CALL AGSRESET                                                 92
            AG_X = YPXG_R*COS(ANGLEPOS)                                   92
            AG_Y = YPXG_R*SIN(ANGLEPOS)                                   92
            AG_Z = 0.0                                                    92
            AG_ALPHAN=AG_ALPHAN+1                                         92
            AG_ALPHAK(AG_ALPHAN)=3                                        92
            AG_ALPHAL(AG_ALPHAN)=                                         92
     *      YPXG_AOFFSET+ANGLE+ANGLESEP*(NLADDER-1)                       92
            AG_TITLE = 'YPLM'                                             92
            AG_EXNAME = 'POSITION'                                        92
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   92
            CALL AXPOSITION                                               92
C       *                                                                 92
         GO TO 5031                                                       94
5032     CONTINUE                                                         94
C    !LOOP ENDED FOR THE NLADDER                                          94
5011  CALL AGSPOPB                                                        96
      GO TO BLOCK_YPXM, (5010 ,5020)                                      96
C End of Local Procedure                                                  96
* -----------------------------------------------------------------------------
C Begin Local Procedure YPLM                                              98
C YPLM IS THE MOTHER OF THE PIXEL LADDER                                  98
5040  CONTINUE                                                            98
         AG_TITLE='YPLM'                                                  98
         AG_MODULE=MODULE                                                 98
         AG_MARK='V00'                                                    98
         CALL AGSPUSH                                                     98
         AG_MODULE=MODULE                                                 98
C    *                                                                    98
C    Geant MATERIAL : AIR ===> inherited arguments used <===              99
         CALL AGSRESET                                                    99
         AG_TITLE = 'AIR'                                                 99
         AG_EXNAME = 'MATERIAL'                                           99
         AG_PARLIST = 'NONE'                                              99
         CALL AXMATERIAL                                                  99
C    *                                                                    99
C    Geant ATTRIBUTE : YPLM ===> SEEN=1 COLO=4 <===                       99
         CALL AGSRESET                                                    100
         AG_SEEN = 1                                                      100
         AG_COLO = 4                                                      100
         AG_TITLE = 'YPLM'                                                100
         AG_EXNAME = 'ATTRIBUTE'                                          100
         AG_PARLIST = 'SEEN_COLO'                                         100
         CALL AXATTRIBUTE                                                 100
C    *                                                                    100
C    Geant SHAPE : BOX ===> DX=YPXG_PASSLADDERW/2.0 DY=YPXG_LADDERTHK/2.  100
C    0 DZ=YPXG_TOTALLENGTH/2.0 <===                                       100
         CALL AGSRESET                                                    101
         AG_DX = YPXG_PASSLADDERW/2.0                                     101
         AG_DY = YPXG_LADDERTHK/2.0                                       101
         AG_DZ = YPXG_TOTALLENGTH/2.0                                     101
         AG_TITLE = 'BOX'                                                 101
         AG_EXNAME = 'SHAPE'                                              101
         AG_PARLIST = 'DX_DY_DZ'                                          101
         CALL AXSHAPE                                                     101
C    *                                                                    101
C    Create YPLP :                                                        102
C    Call To Local Procedure YPLP                                         102
         ASSIGN 5070 TO BLOCK_YPLP                                        102
         GO TO  5060                                                      102
5070     CONTINUE                                                         102
C    *                                                                    102
C    Geant POSITION : YPLP ===> Y=-YPXG_LADDERTHK/2.0+YPXG_PASSIVETHK/2   102
C    <===                                                                 102
         CALL AGSRESET                                                    102
         AG_Y = -YPXG_LADDERTHK/2.0+YPXG_PASSIVETHK/2                     102
         AG_TITLE = 'YPLP'                                                102
         AG_EXNAME = 'POSITION'                                           102
         AG_PARLIST = 'Y'                                                 102
         CALL AXPOSITION                                                  102
C    *                                                                    102
C    Create YPWP :                                                        103
C    Call To Local Procedure YPWP                                         103
         ASSIGN 5090 TO BLOCK_YPWP                                        103
         GO TO  5080                                                      103
5090     CONTINUE                                                         103
C    *                                                                    103
C    Geant POSITION : YPWP ===> Y=YPXG_LADDERTHK/2.0-YPXG_ACTIVETHK/2-YP  103
C    XG_READOUTTHK/2 <===                                                 103
         CALL AGSRESET                                                    103
         AG_Y = YPXG_LADDERTHK/2.0-YPXG_ACTIVETHK/2-YPXG_READOUTTHK/2     103
         AG_TITLE = 'YPWP'                                                103
         AG_EXNAME = 'POSITION'                                           103
         AG_PARLIST = 'Y'                                                 103
         CALL AXPOSITION                                                  103
C    *                                                                    103
5041  CALL AGSPOPB                                                        103
      GO TO BLOCK_YPLM, (5040 ,5050)                                      104
C End of Local Procedure                                                  104
*-----------------------------------------------------------------------------
C Begin Local Procedure YPLP                                              106
C YPLP IS THE MOTHER OF THE SUPPORT                                       106
5060  CONTINUE                                                            106
         AG_TITLE='YPLP'                                                  106
         AG_MODULE=MODULE                                                 106
         AG_MARK='V00'                                                    106
         CALL AGSPUSH                                                     106
         AG_MODULE=MODULE                                                 106
C    *                                                                    106
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            107
         CALL AGSRESET                                                    107
         AG_TITLE = 'MYLAR'                                               107
         AG_EXNAME = 'MATERIAL'                                           107
         AG_PARLIST = 'NONE'                                              107
         CALL AXMATERIAL                                                  107
C    *                                                                    107
C    Geant ATTRIBUTE : YPLP ===> SEEN=1 COLO=4 <===                       107
         CALL AGSRESET                                                    108
         AG_SEEN = 1                                                      108
         AG_COLO = 4                                                      108
         AG_TITLE = 'YPLP'                                                108
         AG_EXNAME = 'ATTRIBUTE'                                          108
         AG_PARLIST = 'SEEN_COLO'                                         108
         CALL AXATTRIBUTE                                                 108
C    *                                                                    108
C    Geant SHAPE : BOX ===> DX=YPXG_PASSLADDERW/2.0 DY=YPXG_PASSIVETHK/2  108
C    .0 DZ=YPXG_TOTALLENGTH/2.0 <===                                      108
         CALL AGSRESET                                                    109
         AG_DX = YPXG_PASSLADDERW/2.0                                     109
         AG_DY = YPXG_PASSIVETHK/2.0                                      109
         AG_DZ = YPXG_TOTALLENGTH/2.0                                     109
         AG_TITLE = 'BOX'                                                 109
         AG_EXNAME = 'SHAPE'                                              109
         AG_PARLIST = 'DX_DY_DZ'                                          109
         CALL AXSHAPE                                                     109
C    *                                                                    109
C    Create YPWR :                                                        110
C    Call To Local Procedure YPWR                                         110
         ASSIGN 5110 TO BLOCK_YPWR                                        110
         GO TO  5100                                                      110
5110     CONTINUE                                                         110
C    *                                                                    110
C    Geant POSITION : YPWR ===> Y=YPXG_CARBONTHK/2.0-YPXG_PASSIVETHK/2 <  110
C    ===                                                                  110
         CALL AGSRESET                                                    110
         AG_Y = YPXG_CARBONTHK/2.0-YPXG_PASSIVETHK/2                      110
         AG_TITLE = 'YPWR'                                                110
         AG_EXNAME = 'POSITION'                                           110
         AG_PARLIST = 'Y'                                                 110
         CALL AXPOSITION                                                  110
C    *                                                                    110
5061  CALL AGSPOPB                                                        110
      GO TO BLOCK_YPLP, (5060 ,5070)                                      111
C End of Local Procedure                                                  111
*---------------------------------------------------------------------------------
C Begin Local Procedure YPWR                                              114
C YPWR IS THE OTHER SUPPORT                                               114
5100  CONTINUE                                                            114
         AG_TITLE='YPWR'                                                  114
         AG_MODULE=MODULE                                                 114
         AG_MARK='V00'                                                    114
         CALL AGSPUSH                                                     114
         AG_MODULE=MODULE                                                 114
C    *                                                                    114
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           115
         CALL AGSRESET                                                    115
         AG_TITLE = 'CARBON'                                              115
         AG_EXNAME = 'MATERIAL'                                           115
         AG_PARLIST = 'NONE'                                              115
         CALL AXMATERIAL                                                  115
C    *                                                                    115
C    Geant ATTRIBUTE : YPWR ===> SEEN=1 COLO=4 <===                       115
         CALL AGSRESET                                                    116
         AG_SEEN = 1                                                      116
         AG_COLO = 4                                                      116
         AG_TITLE = 'YPWR'                                                116
         AG_EXNAME = 'ATTRIBUTE'                                          116
         AG_PARLIST = 'SEEN_COLO'                                         116
         CALL AXATTRIBUTE                                                 116
C    *                                                                    116
C    Geant SHAPE : BOX ===> DX=YPXG_PASSLADDERW/2.0 DY=YPXG_CARBONTHK/2.  116
C    0 DZ=YPXG_TOTALLENGTH/2.0 <===                                       116
         CALL AGSRESET                                                    117
         AG_DX = YPXG_PASSLADDERW/2.0                                     117
         AG_DY = YPXG_CARBONTHK/2.0                                       117
         AG_DZ = YPXG_TOTALLENGTH/2.0                                     117
         AG_TITLE = 'BOX'                                                 117
         AG_EXNAME = 'SHAPE'                                              117
         AG_PARLIST = 'DX_DY_DZ'                                          117
         CALL AXSHAPE                                                     117
C    *                                                                    117
5101  CALL AGSPOPB                                                        119
      GO TO BLOCK_YPWR, (5100 ,5110)                                      119
C End of Local Procedure                                                  119
*---------------------------------------------------------------------------------
C Begin Local Procedure YPWP                                              122
C YPWP IS THE ALL SILICON LAYER ACTIVE AND READOUT                        122
5080  CONTINUE                                                            122
         AG_TITLE='YPWP'                                                  122
         AG_MODULE=MODULE                                                 122
         AG_MARK='V00'                                                    122
         CALL AGSPUSH                                                     122
         AG_MODULE=MODULE                                                 122
C    *                                                                    122
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          123
         CALL AGSRESET                                                    123
         AG_TITLE = 'SILICON'                                             123
         AG_EXNAME = 'MATERIAL'                                           123
         AG_PARLIST = 'NONE'                                              123
         CALL AXMATERIAL                                                  123
C    *                                                                    123
C    Geant ATTRIBUTE : YPWP ===> SEEN=1 COLO=2 <===                       123
         CALL AGSRESET                                                    124
         AG_SEEN = 1                                                      124
         AG_COLO = 2                                                      124
         AG_TITLE = 'YPWP'                                                124
         AG_EXNAME = 'ATTRIBUTE'                                          124
         AG_PARLIST = 'SEEN_COLO'                                         124
         CALL AXATTRIBUTE                                                 124
C    *                                                                    124
C    Geant SHAPE : BOX ===> DX=YPXG_LADDERWIDTH/2.0+YPXG_CELLGUARDLEN/2   124
C    DY=YPXG_ACTIVETHK/2.0+YPXG_READOUTTHK/2.0 DZ=YPXG_TOTALLENGTH/2.0 <  124
C    ===                                                                  124
         CALL AGSRESET                                                    126
         AG_DX = YPXG_LADDERWIDTH/2.0+YPXG_CELLGUARDLEN/2                 126
         AG_DY = YPXG_ACTIVETHK/2.0+YPXG_READOUTTHK/2.0                   126
         AG_DZ = YPXG_TOTALLENGTH/2.0                                     126
         AG_TITLE = 'BOX'                                                 126
         AG_EXNAME = 'SHAPE'                                              126
         AG_PARLIST = 'DX_DY_DZ'                                          126
         CALL AXSHAPE                                                     126
C    *                                                                    126
C    *                                                                    128
         NCELL=1                                                          128
            GO TO 5123                                                    128
5121        NCELL=NCELL+1                                                 128
5123        IF(NCELL-(YPXG_NUMBERCELL).GT.0) GO TO 5122                   128
C       WAFER LOOP, CREAT WAFERS INSIDE THE LADDERS                       128
C       Create YPLA :                                                     130
C       Call To Local Procedure YPLA                                      130
            ASSIGN 5140 TO BLOCK_YPLA                                     130
            GO TO  5130                                                   130
5140        CONTINUE                                                      130
C       *                                                                 130
C       Geant POSITION : YPLA ===> X=0 Y=YPXG_ACTIVETHK/2.0, Z=(YPXG_CEL  130
C       LLEN+YPXG_CELLGUARDLEN+YPXG_PCELLLEN)*((NCELL-1)-(YPXG_NUMBERCEL  130
C       L-1)*0.5) <===                                                    130
            CALL AGSRESET                                                 130
            AG_X = 0                                                      130
            AG_Y = YPXG_ACTIVETHK/2.0                                     130
            AG_Z =                                                        130
     *      (YPXG_CELLLEN+YPXG_CELLGUARDLEN+YPXG_PCELLLEN)*((NCELL-1)-(   130
     *      YPXG_NUMBERCELL-1)*0.5)                                       130
            AG_TITLE = 'YPLA'                                             130
            AG_EXNAME = 'POSITION'                                        130
            AG_PARLIST = 'X_Y_Z'                                          130
            CALL AXPOSITION                                               130
C       *                                                                 130
         GO TO 5121                                                       132
5122     CONTINUE                                                         132
C    !LOOP ENDED FOR THE NCELL                                            132
5081  CALL AGSPOPB                                                        134
      GO TO BLOCK_YPWP, (5080 ,5090)                                      134
C End of Local Procedure                                                  134
* -----------------------------------------------------------------------------
C Begin Local Procedure YPLA                                              136
C YPLA IS THE ACTIVE LAYER OF THE PIXEL                                   136
5130  CONTINUE                                                            136
         AG_TITLE='YPLA'                                                  136
         AG_MODULE=MODULE                                                 136
         AG_MARK='V00'                                                    136
         CALL AGSPUSH                                                     136
         AG_MODULE=MODULE                                                 136
C    *                                                                    136
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          137
         CALL AGSRESET                                                    137
         AG_TITLE = 'SILICON'                                             137
         AG_EXNAME = 'MATERIAL'                                           137
         AG_PARLIST = 'NONE'                                              137
         CALL AXMATERIAL                                                  137
C    *                                                                    137
C    Geant MATERIAL : SENSITIVE ===> ISVOL=1 <===                         137
         CALL AGSRESET                                                    138
         AG_ISVOL = 1                                                     138
         AG_TITLE = 'SENSITIVE'                                           138
         AG_EXNAME = 'MATERIAL'                                           138
         AG_PARLIST = 'ISVOL'                                             138
         CALL AXMATERIAL                                                  138
C    *                                                                    138
C    Geant ATTRIBUTE : YPLA ===> SEEN=1 COLO=4 <===                       138
         CALL AGSRESET                                                    139
         AG_SEEN = 1                                                      139
         AG_COLO = 4                                                      139
         AG_TITLE = 'YPLA'                                                139
         AG_EXNAME = 'ATTRIBUTE'                                          139
         AG_PARLIST = 'SEEN_COLO'                                         139
         CALL AXATTRIBUTE                                                 139
C    *                                                                    139
C    Geant SHAPE : BOX ===> DX=YPXG_LADDERWIDTH/2.0 DY=YPXG_ACTIVETHK/2.  141
C    0 DZ=YPXG_CELLLEN/2.0 <===                                           141
         CALL AGSRESET                                                    141
         AG_DX = YPXG_LADDERWIDTH/2.0                                     141
         AG_DY = YPXG_ACTIVETHK/2.0                                       141
         AG_DZ = YPXG_CELLLEN/2.0                                         141
         AG_TITLE = 'BOX'                                                 141
         AG_EXNAME = 'SHAPE'                                              141
         AG_PARLIST = 'DX_DY_DZ'                                          141
         CALL AXSHAPE                                                     141
C    *                                                                    141
         CALL GSTPAR (AG_IMED,'STRA',1.)                                  143
C    Defining Hits: Z:.0001:S Y:.0001: X:.0001: PTOT:16:(0,100), CX:10:   148
C    CY:10: CZ:10: SLENG:16:(0,500), TOF:16:(0,1.E-6) STEP:.01: ELOSS:32  148
C    :(0,0.001), ZZ:.0001: YY:.0001: XX:.0001:                            148
         AG_TITLE = 'YPLA'                                                148
         AG_EXNAME = 'HITS'                                               148
         AG_TYPE=AG_MAGIC                                                 148
         CALL AGSDETV('YPLA')                                             148
C    *                                                                    148
         AG_PARLIST='Z'                                                   148
         AG_TITLE = 'DETH'                                                148
         AG_NBITS = AG_MAGIC                                              148
         AG_BIN=.0001                                                     148
         AG_OPTION='S'                                                    148
         AG_FHMIN = AG_MAGIC                                              148
         AG_FHMAX = AG_MAGIC                                              148
         CALL AGSDETH (0)                                                 148
C    *                                                                    148
         AG_PARLIST='Y'                                                   148
         AG_TITLE = 'DETH'                                                148
         AG_NBITS = AG_MAGIC                                              148
         AG_BIN=.0001                                                     148
         AG_FHMIN=AG_MAGIC                                                148
         AG_FHMAX=AG_MAGIC                                                148
         AG_OPTION='*'                                                    148
         CALL AGSDETH (0)                                                 148
C    *                                                                    148
         AG_PARLIST='X'                                                   148
         AG_TITLE = 'DETH'                                                148
         AG_NBITS = AG_MAGIC                                              148
         AG_BIN=.0001                                                     148
         AG_FHMIN=AG_MAGIC                                                148
         AG_FHMAX=AG_MAGIC                                                148
         AG_OPTION='*'                                                    148
         CALL AGSDETH (0)                                                 148
C    *                                                                    148
         AG_PARLIST='PTOT'                                                148
         AG_TITLE = 'DETH'                                                148
         AG_NBITS = 16                                                    148
         AG_BIN=AG_MAGIC                                                  148
         AG_OPTION='*'                                                    148
         AG_FHMIN = 0                                                     148
         AG_FHMAX = 100                                                   148
         CALL AGSDETH (0)                                                 148
C    *                                                                    148
         AG_PARLIST='CX'                                                  148
         AG_TITLE = 'DETH'                                                148
         AG_NBITS = 10                                                    148
         AG_BIN=AG_MAGIC                                                  148
         AG_FHMIN=AG_MAGIC                                                148
         AG_FHMAX=AG_MAGIC                                                148
         AG_OPTION='*'                                                    148
         CALL AGSDETH (0)                                                 148
C    *                                                                    148
         AG_PARLIST='CY'                                                  148
         AG_TITLE = 'DETH'                                                148
         AG_NBITS = 10                                                    148
         AG_BIN=AG_MAGIC                                                  148
         AG_FHMIN=AG_MAGIC                                                148
         AG_FHMAX=AG_MAGIC                                                148
         AG_OPTION='*'                                                    148
         CALL AGSDETH (0)                                                 148
C    *                                                                    148
         AG_PARLIST='CZ'                                                  148
         AG_TITLE = 'DETH'                                                148
         AG_NBITS = 10                                                    148
         AG_BIN=AG_MAGIC                                                  148
         AG_FHMIN=AG_MAGIC                                                148
         AG_FHMAX=AG_MAGIC                                                148
         AG_OPTION='*'                                                    148
         CALL AGSDETH (0)                                                 148
C    *                                                                    148
         AG_PARLIST='SLENG'                                               148
         AG_TITLE = 'DETH'                                                148
         AG_NBITS = 16                                                    148
         AG_BIN=AG_MAGIC                                                  148
         AG_OPTION='*'                                                    148
         AG_FHMIN = 0                                                     148
         AG_FHMAX = 500                                                   148
         CALL AGSDETH (0)                                                 148
C    *                                                                    148
         AG_PARLIST='TOF'                                                 148
         AG_TITLE = 'DETH'                                                148
         AG_NBITS = 16                                                    148
         AG_BIN=AG_MAGIC                                                  148
         AG_OPTION='*'                                                    148
         AG_FHMIN = 0                                                     148
         AG_FHMAX = 1.E-6                                                 148
         CALL AGSDETH (0)                                                 148
C    *                                                                    148
         AG_PARLIST='STEP'                                                148
         AG_TITLE = 'DETH'                                                148
         AG_NBITS = AG_MAGIC                                              148
         AG_BIN=.01                                                       148
         AG_FHMIN=AG_MAGIC                                                148
         AG_FHMAX=AG_MAGIC                                                148
         AG_OPTION='*'                                                    148
         CALL AGSDETH (0)                                                 148
C    *                                                                    148
         AG_PARLIST='ELOSS'                                               148
         AG_TITLE = 'DETH'                                                148
         AG_NBITS = 32                                                    148
         AG_BIN=AG_MAGIC                                                  148
         AG_OPTION='*'                                                    148
         AG_FHMIN = 0                                                     148
         AG_FHMAX = 0.001                                                 148
         CALL AGSDETH (0)                                                 148
C    *                                                                    148
         AG_PARLIST='ZZ'                                                  148
         AG_TITLE = 'DETH'                                                148
         AG_NBITS = AG_MAGIC                                              148
         AG_BIN=.0001                                                     148
         AG_FHMIN=AG_MAGIC                                                148
         AG_FHMAX=AG_MAGIC                                                148
         AG_OPTION='*'                                                    148
         CALL AGSDETH (0)                                                 148
C    *                                                                    148
         AG_PARLIST='YY'                                                  148
         AG_TITLE = 'DETH'                                                148
         AG_NBITS = AG_MAGIC                                              148
         AG_BIN=.0001                                                     148
         AG_FHMIN=AG_MAGIC                                                148
         AG_FHMAX=AG_MAGIC                                                148
         AG_OPTION='*'                                                    148
         CALL AGSDETH (0)                                                 148
C    *                                                                    148
         AG_PARLIST='XX'                                                  148
         AG_TITLE = 'DETH'                                                148
         AG_NBITS = AG_MAGIC                                              148
         AG_BIN=.0001                                                     148
         AG_FHMIN=AG_MAGIC                                                148
         AG_FHMAX=AG_MAGIC                                                148
         AG_OPTION='*'                                                    148
         CALL AGSDETH (0)                                                 148
C    *                                                                    148
         CALL AGSDETU('YPLA')                                             148
C    *                                                                    148
5131  CALL AGSPOPB                                                        149
      GO TO BLOCK_YPLA, (5130 ,5140)                                      149
C End of Local Procedure                                                  149
* -----------------------------------------------------------------------------
      END                                                                 152
