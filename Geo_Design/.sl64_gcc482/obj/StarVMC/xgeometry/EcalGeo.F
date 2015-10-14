C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE ECALGEO                                                  1
C ECALGEO IS THE EM ENDCAP CALORIMETER GEOMETRY                           1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'ECALGEO'/, BANK_TITLE/'*'/                             1
      DATA MCOMMENT/'IS THE EM ENDCAP CALORIMETER GEOMETRY'/              1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C CREATED 26 JAN 1996                                                     1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 CREATED /'26 JAN 1996'/                                2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C AUTHOR RASHID MEHDIYEV                                                  2
      CHARACTER*40 AUTHOR /'RASHID MEHDIYEV'/                             3
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
C - Natural constants (or considered as such)                             4
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   5
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        5
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       5
C - GEANT description of logical units                                    5
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            6
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      6
      COMMON/GCMAIL/ CHMAIL                                               6
      CHARACTER*132 CHMAIL                                                6
      INTEGER BLOCK_EAGA                                                  6
      INTEGER BLOCK_EALP                                                  8
      INTEGER BLOCK_ECAL                                                  8
      INTEGER BLOCK_ECHC                                                  8
      INTEGER BLOCK_ECVO                                                  8
      INTEGER BLOCK_ECGH                                                  8
      INTEGER BLOCK_EFLP                                                  8
      INTEGER BLOCK_EHMS                                                  8
      INTEGER BLOCK_ELED                                                  8
      INTEGER BLOCK_EMGT                                                  8
      INTEGER BLOCK_EMOD                                                  8
      INTEGER BLOCK_EPER                                                  8
      INTEGER BLOCK_EPSB                                                  8
      INTEGER BLOCK_ERAD                                                  8
      INTEGER BLOCK_ERCM                                                  8
      INTEGER BLOCK_ERSM                                                  8
      INTEGER BLOCK_ESHM                                                  8
      INTEGER BLOCK_ESEC                                                  8
      INTEGER BLOCK_ESCI                                                  8
      INTEGER BLOCK_ESPL                                                  8
      INTEGER BLOCK_ESSP                                                  8
      INTEGER BLOCK_EMSS                                                  8
      INTEGER BLOCK_ETAR                                                  8
      INTEGER BLOCK_EXGT                                                  8
      INTEGER BLOCK_EXSG                                                  8
C -                                                                       9
      REAL EMCG_VERSION                                                   9
      EQUIVALENCE (BANK_EMCG(3),EMCG_VERSION,RE_EMCG_VERSION),            9
     *(NAMES_EMCG(1),NEMCG_VERSION)                                       9
      EQUIVALENCE (COMMEN_EMCG(1),CEMCG_VERSION),(MAP_EMCG(1,1),          9
     *MEMCG_VERSION)                                                      9
      CHARACTER CEMCG_VERSION*40/' '/                                     9
      CHARACTER NEMCG_VERSION*12/'R.VERSION'/                             9
      INTEGER MEMCG_VERSION(2)/1,1/                                       9
      REAL RE_EMCG_VERSION                                                9
C -                                                                       9
      INTEGER EMCG_ONOFF                                                  9
      EQUIVALENCE (BANK_EMCG(4),EMCG_ONOFF,RE_EMCG_ONOFF),                9
     *(NAMES_EMCG(2),NEMCG_ONOFF)                                         9
      EQUIVALENCE (COMMEN_EMCG(2),CEMCG_ONOFF),(MAP_EMCG(1,2),            9
     *MEMCG_ONOFF)                                                        9
      CHARACTER CEMCG_ONOFF*40/' '/                                       9
      CHARACTER NEMCG_ONOFF*12/'I.ONOFF'/                                 9
      INTEGER MEMCG_ONOFF(2)/1,1/                                         9
      REAL RE_EMCG_ONOFF                                                  9
C -                                                                       9
      INTEGER EMCG_FILLMODE                                               9
      EQUIVALENCE (BANK_EMCG(5),EMCG_FILLMODE,RE_EMCG_FILLMODE),          9
     *(NAMES_EMCG(3),NEMCG_FILLMODE)                                      9
      EQUIVALENCE (COMMEN_EMCG(3),CEMCG_FILLMODE),(MAP_EMCG(1,3),         9
     *MEMCG_FILLMODE)                                                     9
      CHARACTER CEMCG_FILLMODE*40/' '/                                    9
      CHARACTER NEMCG_FILLMODE*12/'I.FILLMODE'/                           9
      INTEGER MEMCG_FILLMODE(2)/1,1/                                      9
      REAL RE_EMCG_FILLMODE                                               9
C -                                                                       9
      INTEGER LEN_EMCG,LLEN_EMCG,LINK_EMCG                                9
      PARAMETER (LEN_EMCG=5)                                              9
      PARAMETER (LLEN_EMCG=3)                                             9
      INTEGER FLAG_EMCG/0/,MAP_EMCG(2,LLEN_EMCG),IDNUM_EMCG(10)           9
      CHARACTER NAMES_EMCG(LLEN_EMCG)*12,COMMEN_EMCG(LLEN_EMCG)*40        9
      DATA IDNUM_EMCG/10*0/                                               9
      REAL BANK_EMCG(LEN_EMCG)                                            9
      CHARACTER*60 BPATH_EMCG/' '/                                        9
      SAVE FLAG_EMCG,MAP_EMCG,IDNUM_EMCG,NAMES_EMCG,COMMEN_EMCG,          9
     *BPATH_EMCG                                                          9
      DATA LINK_EMCG/0/,BANK_EMCG/LEN_EMCG*0/                             9
C *                                                                       9
C -                                                                       12
      REAL EMCS_TYPE                                                      12
      EQUIVALENCE (BANK_EMCS(3),EMCS_TYPE,RE_EMCS_TYPE),(NAMES_EMCS(1),   12
     *NEMCS_TYPE)                                                         12
      EQUIVALENCE (COMMEN_EMCS(1),CEMCS_TYPE),(MAP_EMCS(1,1),             12
     *MEMCS_TYPE)                                                         12
      CHARACTER CEMCS_TYPE*40/' '/                                        12
      CHARACTER NEMCS_TYPE*12/'R.TYPE'/                                   12
      INTEGER MEMCS_TYPE(2)/1,1/                                          12
      REAL RE_EMCS_TYPE                                                   12
C -                                                                       12
      REAL EMCS_ZORIG                                                     12
      EQUIVALENCE (BANK_EMCS(4),EMCS_ZORIG,RE_EMCS_ZORIG),                12
     *(NAMES_EMCS(2),NEMCS_ZORIG)                                         12
      EQUIVALENCE (COMMEN_EMCS(2),CEMCS_ZORIG),(MAP_EMCS(1,2),            12
     *MEMCS_ZORIG)                                                        12
      CHARACTER CEMCS_ZORIG*40/' '/                                       12
      CHARACTER NEMCS_ZORIG*12/'R.ZORIG'/                                 12
      INTEGER MEMCS_ZORIG(2)/1,1/                                         12
      REAL RE_EMCS_ZORIG                                                  12
C -                                                                       12
      REAL EMCS_ZEND                                                      12
      EQUIVALENCE (BANK_EMCS(5),EMCS_ZEND,RE_EMCS_ZEND),(NAMES_EMCS(3),   12
     *NEMCS_ZEND)                                                         12
      EQUIVALENCE (COMMEN_EMCS(3),CEMCS_ZEND),(MAP_EMCS(1,3),             12
     *MEMCS_ZEND)                                                         12
      CHARACTER CEMCS_ZEND*40/' '/                                        12
      CHARACTER NEMCS_ZEND*12/'R.ZEND'/                                   12
      INTEGER MEMCS_ZEND(2)/1,1/                                          12
      REAL RE_EMCS_ZEND                                                   12
C -                                                                       12
      REAL EMCS_ETAMIN                                                    12
      EQUIVALENCE (BANK_EMCS(6),EMCS_ETAMIN,RE_EMCS_ETAMIN),              12
     *(NAMES_EMCS(4),NEMCS_ETAMIN)                                        12
      EQUIVALENCE (COMMEN_EMCS(4),CEMCS_ETAMIN),(MAP_EMCS(1,4),           12
     *MEMCS_ETAMIN)                                                       12
      CHARACTER CEMCS_ETAMIN*40/' '/                                      12
      CHARACTER NEMCS_ETAMIN*12/'R.ETAMIN'/                               12
      INTEGER MEMCS_ETAMIN(2)/1,1/                                        12
      REAL RE_EMCS_ETAMIN                                                 12
C -                                                                       12
      REAL EMCS_ETAMAX                                                    12
      EQUIVALENCE (BANK_EMCS(7),EMCS_ETAMAX,RE_EMCS_ETAMAX),              12
     *(NAMES_EMCS(5),NEMCS_ETAMAX)                                        12
      EQUIVALENCE (COMMEN_EMCS(5),CEMCS_ETAMAX),(MAP_EMCS(1,5),           12
     *MEMCS_ETAMAX)                                                       12
      CHARACTER CEMCS_ETAMAX*40/' '/                                      12
      CHARACTER NEMCS_ETAMAX*12/'R.ETAMAX'/                               12
      INTEGER MEMCS_ETAMAX(2)/1,1/                                        12
      REAL RE_EMCS_ETAMAX                                                 12
C -                                                                       12
      REAL EMCS_PHIMIN                                                    12
      EQUIVALENCE (BANK_EMCS(8),EMCS_PHIMIN,RE_EMCS_PHIMIN),              12
     *(NAMES_EMCS(6),NEMCS_PHIMIN)                                        12
      EQUIVALENCE (COMMEN_EMCS(6),CEMCS_PHIMIN),(MAP_EMCS(1,6),           12
     *MEMCS_PHIMIN)                                                       12
      CHARACTER CEMCS_PHIMIN*40/' '/                                      12
      CHARACTER NEMCS_PHIMIN*12/'R.PHIMIN'/                               12
      INTEGER MEMCS_PHIMIN(2)/1,1/                                        12
      REAL RE_EMCS_PHIMIN                                                 12
C -                                                                       12
      REAL EMCS_PHIMAX                                                    12
      EQUIVALENCE (BANK_EMCS(9),EMCS_PHIMAX,RE_EMCS_PHIMAX),              12
     *(NAMES_EMCS(7),NEMCS_PHIMAX)                                        12
      EQUIVALENCE (COMMEN_EMCS(7),CEMCS_PHIMAX),(MAP_EMCS(1,7),           12
     *MEMCS_PHIMAX)                                                       12
      CHARACTER CEMCS_PHIMAX*40/' '/                                      12
      CHARACTER NEMCS_PHIMAX*12/'R.PHIMAX'/                               12
      INTEGER MEMCS_PHIMAX(2)/1,1/                                        12
      REAL RE_EMCS_PHIMAX                                                 12
C -                                                                       12
      REAL EMCS_OFFSET                                                    12
      EQUIVALENCE (BANK_EMCS(10),EMCS_OFFSET,RE_EMCS_OFFSET),             12
     *(NAMES_EMCS(8),NEMCS_OFFSET)                                        12
      EQUIVALENCE (COMMEN_EMCS(8),CEMCS_OFFSET),(MAP_EMCS(1,8),           12
     *MEMCS_OFFSET)                                                       12
      CHARACTER CEMCS_OFFSET*40/' '/                                      12
      CHARACTER NEMCS_OFFSET*12/'R.OFFSET'/                               12
      INTEGER MEMCS_OFFSET(2)/1,1/                                        12
      REAL RE_EMCS_OFFSET                                                 12
C -                                                                       12
      REAL EMCS_NSUPSEC                                                   12
      EQUIVALENCE (BANK_EMCS(11),EMCS_NSUPSEC,RE_EMCS_NSUPSEC),           12
     *(NAMES_EMCS(9),NEMCS_NSUPSEC)                                       12
      EQUIVALENCE (COMMEN_EMCS(9),CEMCS_NSUPSEC),(MAP_EMCS(1,9),          12
     *MEMCS_NSUPSEC)                                                      12
      CHARACTER CEMCS_NSUPSEC*40/' '/                                     12
      CHARACTER NEMCS_NSUPSEC*12/'R.NSUPSEC'/                             12
      INTEGER MEMCS_NSUPSEC(2)/1,1/                                       12
      REAL RE_EMCS_NSUPSEC                                                12
C -                                                                       12
      REAL EMCS_NSECTOR                                                   12
      EQUIVALENCE (BANK_EMCS(12),EMCS_NSECTOR,RE_EMCS_NSECTOR),           12
     *(NAMES_EMCS(10),NEMCS_NSECTOR)                                      12
      EQUIVALENCE (COMMEN_EMCS(10),CEMCS_NSECTOR),(MAP_EMCS(1,10),        12
     *MEMCS_NSECTOR)                                                      12
      CHARACTER CEMCS_NSECTOR*40/' '/                                     12
      CHARACTER NEMCS_NSECTOR*12/'R.NSECTOR'/                             12
      INTEGER MEMCS_NSECTOR(2)/1,1/                                       12
      REAL RE_EMCS_NSECTOR                                                12
C -                                                                       12
      REAL EMCS_NSECTION                                                  12
      EQUIVALENCE (BANK_EMCS(13),EMCS_NSECTION,RE_EMCS_NSECTION),         12
     *(NAMES_EMCS(11),NEMCS_NSECTION)                                     12
      EQUIVALENCE (COMMEN_EMCS(11),CEMCS_NSECTION),(MAP_EMCS(1,11),       12
     *MEMCS_NSECTION)                                                     12
      CHARACTER CEMCS_NSECTION*40/' '/                                    12
      CHARACTER NEMCS_NSECTION*12/'R.NSECTION'/                           12
      INTEGER MEMCS_NSECTION(2)/1,1/                                      12
      REAL RE_EMCS_NSECTION                                               12
C -                                                                       12
      REAL EMCS_NSLICES                                                   12
      EQUIVALENCE (BANK_EMCS(14),EMCS_NSLICES,RE_EMCS_NSLICES),           12
     *(NAMES_EMCS(12),NEMCS_NSLICES)                                      12
      EQUIVALENCE (COMMEN_EMCS(12),CEMCS_NSLICES),(MAP_EMCS(1,12),        12
     *MEMCS_NSLICES)                                                      12
      CHARACTER CEMCS_NSLICES*40/' '/                                     12
      CHARACTER NEMCS_NSLICES*12/'R.NSLICES'/                             12
      INTEGER MEMCS_NSLICES(2)/1,1/                                       12
      REAL RE_EMCS_NSLICES                                                12
C -                                                                       12
      REAL EMCS_FRONT                                                     12
      EQUIVALENCE (BANK_EMCS(15),EMCS_FRONT,RE_EMCS_FRONT),               12
     *(NAMES_EMCS(13),NEMCS_FRONT)                                        12
      EQUIVALENCE (COMMEN_EMCS(13),CEMCS_FRONT),(MAP_EMCS(1,13),          12
     *MEMCS_FRONT)                                                        12
      CHARACTER CEMCS_FRONT*40/' '/                                       12
      CHARACTER NEMCS_FRONT*12/'R.FRONT'/                                 12
      INTEGER MEMCS_FRONT(2)/1,1/                                         12
      REAL RE_EMCS_FRONT                                                  12
C -                                                                       12
      REAL EMCS_ALINCELL                                                  12
      EQUIVALENCE (BANK_EMCS(16),EMCS_ALINCELL,RE_EMCS_ALINCELL),         12
     *(NAMES_EMCS(14),NEMCS_ALINCELL)                                     12
      EQUIVALENCE (COMMEN_EMCS(14),CEMCS_ALINCELL),(MAP_EMCS(1,14),       12
     *MEMCS_ALINCELL)                                                     12
      CHARACTER CEMCS_ALINCELL*40/' '/                                    12
      CHARACTER NEMCS_ALINCELL*12/'R.ALINCELL'/                           12
      INTEGER MEMCS_ALINCELL(2)/1,1/                                      12
      REAL RE_EMCS_ALINCELL                                               12
C -                                                                       12
      REAL EMCS_FRPLAST                                                   12
      EQUIVALENCE (BANK_EMCS(17),EMCS_FRPLAST,RE_EMCS_FRPLAST),           12
     *(NAMES_EMCS(15),NEMCS_FRPLAST)                                      12
      EQUIVALENCE (COMMEN_EMCS(15),CEMCS_FRPLAST),(MAP_EMCS(1,15),        12
     *MEMCS_FRPLAST)                                                      12
      CHARACTER CEMCS_FRPLAST*40/' '/                                     12
      CHARACTER NEMCS_FRPLAST*12/'R.FRPLAST'/                             12
      INTEGER MEMCS_FRPLAST(2)/1,1/                                       12
      REAL RE_EMCS_FRPLAST                                                12
C -                                                                       12
      REAL EMCS_BKPLAST                                                   12
      EQUIVALENCE (BANK_EMCS(18),EMCS_BKPLAST,RE_EMCS_BKPLAST),           12
     *(NAMES_EMCS(16),NEMCS_BKPLAST)                                      12
      EQUIVALENCE (COMMEN_EMCS(16),CEMCS_BKPLAST),(MAP_EMCS(1,16),        12
     *MEMCS_BKPLAST)                                                      12
      CHARACTER CEMCS_BKPLAST*40/' '/                                     12
      CHARACTER NEMCS_BKPLAST*12/'R.BKPLAST'/                             12
      INTEGER MEMCS_BKPLAST(2)/1,1/                                       12
      REAL RE_EMCS_BKPLAST                                                12
C -                                                                       12
      REAL EMCS_PBPLATE                                                   12
      EQUIVALENCE (BANK_EMCS(19),EMCS_PBPLATE,RE_EMCS_PBPLATE),           12
     *(NAMES_EMCS(17),NEMCS_PBPLATE)                                      12
      EQUIVALENCE (COMMEN_EMCS(17),CEMCS_PBPLATE),(MAP_EMCS(1,17),        12
     *MEMCS_PBPLATE)                                                      12
      CHARACTER CEMCS_PBPLATE*40/' '/                                     12
      CHARACTER NEMCS_PBPLATE*12/'R.PBPLATE'/                             12
      INTEGER MEMCS_PBPLATE(2)/1,1/                                       12
      REAL RE_EMCS_PBPLATE                                                12
C -                                                                       12
      REAL EMCS_LAMPLATE                                                  12
      EQUIVALENCE (BANK_EMCS(20),EMCS_LAMPLATE,RE_EMCS_LAMPLATE),         12
     *(NAMES_EMCS(18),NEMCS_LAMPLATE)                                     12
      EQUIVALENCE (COMMEN_EMCS(18),CEMCS_LAMPLATE),(MAP_EMCS(1,18),       12
     *MEMCS_LAMPLATE)                                                     12
      CHARACTER CEMCS_LAMPLATE*40/' '/                                    12
      CHARACTER NEMCS_LAMPLATE*12/'R.LAMPLATE'/                           12
      INTEGER MEMCS_LAMPLATE(2)/1,1/                                      12
      REAL RE_EMCS_LAMPLATE                                               12
C -                                                                       12
      REAL EMCS_BCKPLATE                                                  12
      EQUIVALENCE (BANK_EMCS(21),EMCS_BCKPLATE,RE_EMCS_BCKPLATE),         12
     *(NAMES_EMCS(19),NEMCS_BCKPLATE)                                     12
      EQUIVALENCE (COMMEN_EMCS(19),CEMCS_BCKPLATE),(MAP_EMCS(1,19),       12
     *MEMCS_BCKPLATE)                                                     12
      CHARACTER CEMCS_BCKPLATE*40/' '/                                    12
      CHARACTER NEMCS_BCKPLATE*12/'R.BCKPLATE'/                           12
      INTEGER MEMCS_BCKPLATE(2)/1,1/                                      12
      REAL RE_EMCS_BCKPLATE                                               12
C -                                                                       12
      REAL EMCS_HUB                                                       12
      EQUIVALENCE (BANK_EMCS(22),EMCS_HUB,RE_EMCS_HUB),(NAMES_EMCS(20),   12
     *NEMCS_HUB)                                                          12
      EQUIVALENCE (COMMEN_EMCS(20),CEMCS_HUB),(MAP_EMCS(1,20),            12
     *MEMCS_HUB)                                                          12
      CHARACTER CEMCS_HUB*40/' '/                                         12
      CHARACTER NEMCS_HUB*12/'R.HUB'/                                     12
      INTEGER MEMCS_HUB(2)/1,1/                                           12
      REAL RE_EMCS_HUB                                                    12
C -                                                                       12
      REAL EMCS_RMSHIFT                                                   12
      EQUIVALENCE (BANK_EMCS(23),EMCS_RMSHIFT,RE_EMCS_RMSHIFT),           12
     *(NAMES_EMCS(21),NEMCS_RMSHIFT)                                      12
      EQUIVALENCE (COMMEN_EMCS(21),CEMCS_RMSHIFT),(MAP_EMCS(1,21),        12
     *MEMCS_RMSHIFT)                                                      12
      CHARACTER CEMCS_RMSHIFT*40/' '/                                     12
      CHARACTER NEMCS_RMSHIFT*12/'R.RMSHIFT'/                             12
      INTEGER MEMCS_RMSHIFT(2)/1,1/                                       12
      REAL RE_EMCS_RMSHIFT                                                12
C -                                                                       12
      REAL EMCS_SMSHIFT                                                   12
      EQUIVALENCE (BANK_EMCS(24),EMCS_SMSHIFT,RE_EMCS_SMSHIFT),           12
     *(NAMES_EMCS(22),NEMCS_SMSHIFT)                                      12
      EQUIVALENCE (COMMEN_EMCS(22),CEMCS_SMSHIFT),(MAP_EMCS(1,22),        12
     *MEMCS_SMSHIFT)                                                      12
      CHARACTER CEMCS_SMSHIFT*40/' '/                                     12
      CHARACTER NEMCS_SMSHIFT*12/'R.SMSHIFT'/                             12
      INTEGER MEMCS_SMSHIFT(2)/1,1/                                       12
      REAL RE_EMCS_SMSHIFT                                                12
C -                                                                       12
      REAL EMCS_GAPPLT                                                    12
      EQUIVALENCE (BANK_EMCS(25),EMCS_GAPPLT,RE_EMCS_GAPPLT),             12
     *(NAMES_EMCS(23),NEMCS_GAPPLT)                                       12
      EQUIVALENCE (COMMEN_EMCS(23),CEMCS_GAPPLT),(MAP_EMCS(1,23),         12
     *MEMCS_GAPPLT)                                                       12
      CHARACTER CEMCS_GAPPLT*40/' '/                                      12
      CHARACTER NEMCS_GAPPLT*12/'R.GAPPLT'/                               12
      INTEGER MEMCS_GAPPLT(2)/1,1/                                        12
      REAL RE_EMCS_GAPPLT                                                 12
C -                                                                       12
      REAL EMCS_GAPCEL                                                    12
      EQUIVALENCE (BANK_EMCS(26),EMCS_GAPCEL,RE_EMCS_GAPCEL),             12
     *(NAMES_EMCS(24),NEMCS_GAPCEL)                                       12
      EQUIVALENCE (COMMEN_EMCS(24),CEMCS_GAPCEL),(MAP_EMCS(1,24),         12
     *MEMCS_GAPCEL)                                                       12
      CHARACTER CEMCS_GAPCEL*40/' '/                                      12
      CHARACTER NEMCS_GAPCEL*12/'R.GAPCEL'/                               12
      INTEGER MEMCS_GAPCEL(2)/1,1/                                        12
      REAL RE_EMCS_GAPCEL                                                 12
C -                                                                       12
      REAL EMCS_GAPSMD                                                    12
      EQUIVALENCE (BANK_EMCS(27),EMCS_GAPSMD,RE_EMCS_GAPSMD),             12
     *(NAMES_EMCS(25),NEMCS_GAPSMD)                                       12
      EQUIVALENCE (COMMEN_EMCS(25),CEMCS_GAPSMD),(MAP_EMCS(1,25),         12
     *MEMCS_GAPSMD)                                                       12
      CHARACTER CEMCS_GAPSMD*40/' '/                                      12
      CHARACTER NEMCS_GAPSMD*12/'R.GAPSMD'/                               12
      INTEGER MEMCS_GAPSMD(2)/1,1/                                        12
      REAL RE_EMCS_GAPSMD                                                 12
C -                                                                       12
      REAL EMCS_SMDCENTR                                                  12
      EQUIVALENCE (BANK_EMCS(28),EMCS_SMDCENTR,RE_EMCS_SMDCENTR),         12
     *(NAMES_EMCS(26),NEMCS_SMDCENTR)                                     12
      EQUIVALENCE (COMMEN_EMCS(26),CEMCS_SMDCENTR),(MAP_EMCS(1,26),       12
     *MEMCS_SMDCENTR)                                                     12
      CHARACTER CEMCS_SMDCENTR*40/' '/                                    12
      CHARACTER NEMCS_SMDCENTR*12/'R.SMDCENTR'/                           12
      INTEGER MEMCS_SMDCENTR(2)/1,1/                                      12
      REAL RE_EMCS_SMDCENTR                                               12
C -                                                                       12
      REAL EMCS_TIEROD(2)                                                 12
      EQUIVALENCE (BANK_EMCS(29),EMCS_TIEROD,RE_EMCS_TIEROD),             12
     *(NAMES_EMCS(27),NEMCS_TIEROD)                                       12
      EQUIVALENCE (COMMEN_EMCS(27),CEMCS_TIEROD),(MAP_EMCS(1,27),         12
     *MEMCS_TIEROD)                                                       12
      CHARACTER CEMCS_TIEROD*40/' '/                                      12
      CHARACTER NEMCS_TIEROD*12/'R.TIEROD'/                               12
      INTEGER MEMCS_TIEROD(2)/2,1/                                        12
      REAL RE_EMCS_TIEROD                                                 12
C -                                                                       12
      REAL EMCS_BCKFRNT                                                   12
      EQUIVALENCE (BANK_EMCS(31),EMCS_BCKFRNT,RE_EMCS_BCKFRNT),           12
     *(NAMES_EMCS(28),NEMCS_BCKFRNT)                                      12
      EQUIVALENCE (COMMEN_EMCS(28),CEMCS_BCKFRNT),(MAP_EMCS(1,28),        12
     *MEMCS_BCKFRNT)                                                      12
      CHARACTER CEMCS_BCKFRNT*40/' '/                                     12
      CHARACTER NEMCS_BCKFRNT*12/'R.BCKFRNT'/                             12
      INTEGER MEMCS_BCKFRNT(2)/1,1/                                       12
      REAL RE_EMCS_BCKFRNT                                                12
C -                                                                       12
      REAL EMCS_GAPHALF                                                   12
      EQUIVALENCE (BANK_EMCS(32),EMCS_GAPHALF,RE_EMCS_GAPHALF),           12
     *(NAMES_EMCS(29),NEMCS_GAPHALF)                                      12
      EQUIVALENCE (COMMEN_EMCS(29),CEMCS_GAPHALF),(MAP_EMCS(1,29),        12
     *MEMCS_GAPHALF)                                                      12
      CHARACTER CEMCS_GAPHALF*40/' '/                                     12
      CHARACTER NEMCS_GAPHALF*12/'R.GAPHALF'/                             12
      INTEGER MEMCS_GAPHALF(2)/1,1/                                       12
      REAL RE_EMCS_GAPHALF                                                12
C -                                                                       12
      REAL EMCS_COVER                                                     12
      EQUIVALENCE (BANK_EMCS(33),EMCS_COVER,RE_EMCS_COVER),               12
     *(NAMES_EMCS(30),NEMCS_COVER)                                        12
      EQUIVALENCE (COMMEN_EMCS(30),CEMCS_COVER),(MAP_EMCS(1,30),          12
     *MEMCS_COVER)                                                        12
      CHARACTER CEMCS_COVER*40/' '/                                       12
      CHARACTER NEMCS_COVER*12/'R.COVER'/                                 12
      INTEGER MEMCS_COVER(2)/1,1/                                         12
      REAL RE_EMCS_COVER                                                  12
C -                                                                       12
      INTEGER LEN_EMCS,LLEN_EMCS,LINK_EMCS                                12
      PARAMETER (LEN_EMCS=33)                                             12
      PARAMETER (LLEN_EMCS=30)                                            12
      INTEGER FLAG_EMCS/0/,MAP_EMCS(2,LLEN_EMCS),IDNUM_EMCS(10)           12
      CHARACTER NAMES_EMCS(LLEN_EMCS)*12,COMMEN_EMCS(LLEN_EMCS)*40        12
      DATA IDNUM_EMCS/10*0/                                               12
      REAL BANK_EMCS(LEN_EMCS)                                            12
      CHARACTER*60 BPATH_EMCS/' '/                                        12
      SAVE FLAG_EMCS,MAP_EMCS,IDNUM_EMCS,NAMES_EMCS,COMMEN_EMCS,          12
     *BPATH_EMCS                                                          12
      DATA LINK_EMCS/0/,BANK_EMCS/LEN_EMCS*0/                             12
C *                                                                       12
C -                                                                       13
      REAL EETR_TYPE                                                      13
      EQUIVALENCE (BANK_EETR(3),EETR_TYPE,RE_EETR_TYPE),(NAMES_EETR(1),   13
     *NEETR_TYPE)                                                         13
      EQUIVALENCE (COMMEN_EETR(1),CEETR_TYPE),(MAP_EETR(1,1),             13
     *MEETR_TYPE)                                                         13
      CHARACTER CEETR_TYPE*40/' '/                                        13
      CHARACTER NEETR_TYPE*12/'R.TYPE'/                                   13
      INTEGER MEETR_TYPE(2)/1,1/                                          13
      REAL RE_EETR_TYPE                                                   13
C -                                                                       13
      REAL EETR_ETAGR                                                     13
      EQUIVALENCE (BANK_EETR(4),EETR_ETAGR,RE_EETR_ETAGR),                13
     *(NAMES_EETR(2),NEETR_ETAGR)                                         13
      EQUIVALENCE (COMMEN_EETR(2),CEETR_ETAGR),(MAP_EETR(1,2),            13
     *MEETR_ETAGR)                                                        13
      CHARACTER CEETR_ETAGR*40/' '/                                       13
      CHARACTER NEETR_ETAGR*12/'R.ETAGR'/                                 13
      INTEGER MEETR_ETAGR(2)/1,1/                                         13
      REAL RE_EETR_ETAGR                                                  13
C -                                                                       13
      REAL EETR_PHIGR                                                     13
      EQUIVALENCE (BANK_EETR(5),EETR_PHIGR,RE_EETR_PHIGR),                13
     *(NAMES_EETR(3),NEETR_PHIGR)                                         13
      EQUIVALENCE (COMMEN_EETR(3),CEETR_PHIGR),(MAP_EETR(1,3),            13
     *MEETR_PHIGR)                                                        13
      CHARACTER CEETR_PHIGR*40/' '/                                       13
      CHARACTER NEETR_PHIGR*12/'R.PHIGR'/                                 13
      INTEGER MEETR_PHIGR(2)/1,1/                                         13
      REAL RE_EETR_PHIGR                                                  13
C -                                                                       13
      REAL EETR_NETA                                                      13
      EQUIVALENCE (BANK_EETR(6),EETR_NETA,RE_EETR_NETA),(NAMES_EETR(4),   13
     *NEETR_NETA)                                                         13
      EQUIVALENCE (COMMEN_EETR(4),CEETR_NETA),(MAP_EETR(1,4),             13
     *MEETR_NETA)                                                         13
      CHARACTER CEETR_NETA*40/' '/                                        13
      CHARACTER NEETR_NETA*12/'R.NETA'/                                   13
      INTEGER MEETR_NETA(2)/1,1/                                          13
      REAL RE_EETR_NETA                                                   13
C -                                                                       13
      REAL EETR_ETABIN(13)                                                13
      EQUIVALENCE (BANK_EETR(7),EETR_ETABIN,RE_EETR_ETABIN),              13
     *(NAMES_EETR(5),NEETR_ETABIN)                                        13
      EQUIVALENCE (COMMEN_EETR(5),CEETR_ETABIN),(MAP_EETR(1,5),           13
     *MEETR_ETABIN)                                                       13
      CHARACTER CEETR_ETABIN*40/' '/                                      13
      CHARACTER NEETR_ETABIN*12/'R.ETABIN'/                               13
      INTEGER MEETR_ETABIN(2)/13,1/                                       13
      REAL RE_EETR_ETABIN                                                 13
C -                                                                       13
      INTEGER LEN_EETR,LLEN_EETR,LINK_EETR                                13
      PARAMETER (LEN_EETR=19)                                             13
      PARAMETER (LLEN_EETR=5)                                             13
      INTEGER FLAG_EETR/0/,MAP_EETR(2,LLEN_EETR),IDNUM_EETR(10)           13
      CHARACTER NAMES_EETR(LLEN_EETR)*12,COMMEN_EETR(LLEN_EETR)*40        13
      DATA IDNUM_EETR/10*0/                                               13
      REAL BANK_EETR(LEN_EETR)                                            13
      CHARACTER*60 BPATH_EETR/' '/                                        13
      SAVE FLAG_EETR,MAP_EETR,IDNUM_EETR,NAMES_EETR,COMMEN_EETR,          13
     *BPATH_EETR                                                          13
      DATA LINK_EETR/0/,BANK_EETR/LEN_EETR*0/                             13
C *                                                                       13
C -                                                                       14
      REAL ESEC_ISECT                                                     14
      EQUIVALENCE (BANK_ESEC(3),ESEC_ISECT,RE_ESEC_ISECT),                14
     *(NAMES_ESEC(1),NESEC_ISECT)                                         14
      EQUIVALENCE (COMMEN_ESEC(1),CESEC_ISECT),(MAP_ESEC(1,1),            14
     *MESEC_ISECT)                                                        14
      CHARACTER CESEC_ISECT*40/' '/                                       14
      CHARACTER NESEC_ISECT*12/'R.ISECT'/                                 14
      INTEGER MESEC_ISECT(2)/1,1/                                         14
      REAL RE_ESEC_ISECT                                                  14
C -                                                                       14
      REAL ESEC_FPLMAT                                                    14
      EQUIVALENCE (BANK_ESEC(4),ESEC_FPLMAT,RE_ESEC_FPLMAT),              14
     *(NAMES_ESEC(2),NESEC_FPLMAT)                                        14
      EQUIVALENCE (COMMEN_ESEC(2),CESEC_FPLMAT),(MAP_ESEC(1,2),           14
     *MESEC_FPLMAT)                                                       14
      CHARACTER CESEC_FPLMAT*40/' '/                                      14
      CHARACTER NESEC_FPLMAT*12/'R.FPLMAT'/                               14
      INTEGER MESEC_FPLMAT(2)/1,1/                                        14
      REAL RE_ESEC_FPLMAT                                                 14
C -                                                                       14
      REAL ESEC_CELL                                                      14
      EQUIVALENCE (BANK_ESEC(5),ESEC_CELL,RE_ESEC_CELL),(NAMES_ESEC(3),   14
     *NESEC_CELL)                                                         14
      EQUIVALENCE (COMMEN_ESEC(3),CESEC_CELL),(MAP_ESEC(1,3),             14
     *MESEC_CELL)                                                         14
      CHARACTER CESEC_CELL*40/' '/                                        14
      CHARACTER NESEC_CELL*12/'R.CELL'/                                   14
      INTEGER MESEC_CELL(2)/1,1/                                          14
      REAL RE_ESEC_CELL                                                   14
C -                                                                       14
      REAL ESEC_SCINT                                                     14
      EQUIVALENCE (BANK_ESEC(6),ESEC_SCINT,RE_ESEC_SCINT),                14
     *(NAMES_ESEC(4),NESEC_SCINT)                                         14
      EQUIVALENCE (COMMEN_ESEC(4),CESEC_SCINT),(MAP_ESEC(1,4),            14
     *MESEC_SCINT)                                                        14
      CHARACTER CESEC_SCINT*40/' '/                                       14
      CHARACTER NESEC_SCINT*12/'R.SCINT'/                                 14
      INTEGER MESEC_SCINT(2)/1,1/                                         14
      REAL RE_ESEC_SCINT                                                  14
C -                                                                       14
      REAL ESEC_NLAYER                                                    14
      EQUIVALENCE (BANK_ESEC(7),ESEC_NLAYER,RE_ESEC_NLAYER),              14
     *(NAMES_ESEC(5),NESEC_NLAYER)                                        14
      EQUIVALENCE (COMMEN_ESEC(5),CESEC_NLAYER),(MAP_ESEC(1,5),           14
     *MESEC_NLAYER)                                                       14
      CHARACTER CESEC_NLAYER*40/' '/                                      14
      CHARACTER NESEC_NLAYER*12/'R.NLAYER'/                               14
      INTEGER MESEC_NLAYER(2)/1,1/                                        14
      REAL RE_ESEC_NLAYER                                                 14
C -                                                                       14
      INTEGER LEN_ESEC,LLEN_ESEC,LINK_ESEC                                14
      PARAMETER (LEN_ESEC=7)                                              14
      PARAMETER (LLEN_ESEC=5)                                             14
      INTEGER FLAG_ESEC/0/,MAP_ESEC(2,LLEN_ESEC),IDNUM_ESEC(10)           14
      CHARACTER NAMES_ESEC(LLEN_ESEC)*12,COMMEN_ESEC(LLEN_ESEC)*40        14
      DATA IDNUM_ESEC/10*0/                                               14
      REAL BANK_ESEC(LEN_ESEC)                                            14
      CHARACTER*60 BPATH_ESEC/' '/                                        14
      SAVE FLAG_ESEC,MAP_ESEC,IDNUM_ESEC,NAMES_ESEC,COMMEN_ESEC,          14
     *BPATH_ESEC                                                          14
      DATA LINK_ESEC/0/,BANK_ESEC/LEN_ESEC*0/                             14
C *                                                                       14
C -                                                                       15
      REAL EMXG_VERSION                                                   15
      EQUIVALENCE (BANK_EMXG(3),EMXG_VERSION,RE_EMXG_VERSION),            15
     *(NAMES_EMXG(1),NEMXG_VERSION)                                       15
      EQUIVALENCE (COMMEN_EMXG(1),CEMXG_VERSION),(MAP_EMXG(1,1),          15
     *MEMXG_VERSION)                                                      15
      CHARACTER CEMXG_VERSION*40/' '/                                     15
      CHARACTER NEMXG_VERSION*12/'R.VERSION'/                             15
      INTEGER MEMXG_VERSION(2)/1,1/                                       15
      REAL RE_EMXG_VERSION                                                15
C -                                                                       15
      REAL EMXG_SAPEX                                                     15
      EQUIVALENCE (BANK_EMXG(4),EMXG_SAPEX,RE_EMXG_SAPEX),                15
     *(NAMES_EMXG(2),NEMXG_SAPEX)                                         15
      EQUIVALENCE (COMMEN_EMXG(2),CEMXG_SAPEX),(MAP_EMXG(1,2),            15
     *MEMXG_SAPEX)                                                        15
      CHARACTER CEMXG_SAPEX*40/' '/                                       15
      CHARACTER NEMXG_SAPEX*12/'R.SAPEX'/                                 15
      INTEGER MEMXG_SAPEX(2)/1,1/                                         15
      REAL RE_EMXG_SAPEX                                                  15
C -                                                                       15
      REAL EMXG_SBASE                                                     15
      EQUIVALENCE (BANK_EMXG(5),EMXG_SBASE,RE_EMXG_SBASE),                15
     *(NAMES_EMXG(3),NEMXG_SBASE)                                         15
      EQUIVALENCE (COMMEN_EMXG(3),CEMXG_SBASE),(MAP_EMXG(1,3),            15
     *MEMXG_SBASE)                                                        15
      CHARACTER CEMXG_SBASE*40/' '/                                       15
      CHARACTER NEMXG_SBASE*12/'R.SBASE'/                                 15
      INTEGER MEMXG_SBASE(2)/1,1/                                         15
      REAL RE_EMXG_SBASE                                                  15
C -                                                                       15
      REAL EMXG_RIN                                                       15
      EQUIVALENCE (BANK_EMXG(6),EMXG_RIN,RE_EMXG_RIN),(NAMES_EMXG(4),     15
     *NEMXG_RIN)                                                          15
      EQUIVALENCE (COMMEN_EMXG(4),CEMXG_RIN),(MAP_EMXG(1,4),MEMXG_RIN)    15
      CHARACTER CEMXG_RIN*40/' '/                                         15
      CHARACTER NEMXG_RIN*12/'R.RIN'/                                     15
      INTEGER MEMXG_RIN(2)/1,1/                                           15
      REAL RE_EMXG_RIN                                                    15
C -                                                                       15
      REAL EMXG_ROUT                                                      15
      EQUIVALENCE (BANK_EMXG(7),EMXG_ROUT,RE_EMXG_ROUT),(NAMES_EMXG(5),   15
     *NEMXG_ROUT)                                                         15
      EQUIVALENCE (COMMEN_EMXG(5),CEMXG_ROUT),(MAP_EMXG(1,5),             15
     *MEMXG_ROUT)                                                         15
      CHARACTER CEMXG_ROUT*40/' '/                                        15
      CHARACTER NEMXG_ROUT*12/'R.ROUT'/                                   15
      INTEGER MEMXG_ROUT(2)/1,1/                                          15
      REAL RE_EMXG_ROUT                                                   15
C -                                                                       15
      REAL EMXG_F4                                                        15
      EQUIVALENCE (BANK_EMXG(8),EMXG_F4,RE_EMXG_F4),(NAMES_EMXG(6),       15
     *NEMXG_F4)                                                           15
      EQUIVALENCE (COMMEN_EMXG(6),CEMXG_F4),(MAP_EMXG(1,6),MEMXG_F4)      15
      CHARACTER CEMXG_F4*40/' '/                                          15
      CHARACTER NEMXG_F4*12/'R.F4'/                                       15
      INTEGER MEMXG_F4(2)/1,1/                                            15
      REAL RE_EMXG_F4                                                     15
C -                                                                       15
      INTEGER LEN_EMXG,LLEN_EMXG,LINK_EMXG                                15
      PARAMETER (LEN_EMXG=8)                                              15
      PARAMETER (LLEN_EMXG=6)                                             15
      INTEGER FLAG_EMXG/0/,MAP_EMXG(2,LLEN_EMXG),IDNUM_EMXG(10)           15
      CHARACTER NAMES_EMXG(LLEN_EMXG)*12,COMMEN_EMXG(LLEN_EMXG)*40        15
      DATA IDNUM_EMXG/10*0/                                               15
      REAL BANK_EMXG(LEN_EMXG)                                            15
      CHARACTER*60 BPATH_EMXG/' '/                                        15
      SAVE FLAG_EMXG,MAP_EMXG,IDNUM_EMXG,NAMES_EMXG,COMMEN_EMXG,          15
     *BPATH_EMXG                                                          15
      DATA LINK_EMXG/0/,BANK_EMXG/LEN_EMXG*0/                             15
C *                                                                       15
C -                                                                       16
      REAL EXSE_JSECT                                                     16
      EQUIVALENCE (BANK_EXSE(3),EXSE_JSECT,RE_EXSE_JSECT),                16
     *(NAMES_EXSE(1),NEXSE_JSECT)                                         16
      EQUIVALENCE (COMMEN_EXSE(1),CEXSE_JSECT),(MAP_EXSE(1,1),            16
     *MEXSE_JSECT)                                                        16
      CHARACTER CEXSE_JSECT*40/' '/                                       16
      CHARACTER NEXSE_JSECT*12/'R.JSECT'/                                 16
      INTEGER MEXSE_JSECT(2)/1,1/                                         16
      REAL RE_EXSE_JSECT                                                  16
C -                                                                       16
      REAL EXSE_ZSHIFT                                                    16
      EQUIVALENCE (BANK_EXSE(4),EXSE_ZSHIFT,RE_EXSE_ZSHIFT),              16
     *(NAMES_EXSE(2),NEXSE_ZSHIFT)                                        16
      EQUIVALENCE (COMMEN_EXSE(2),CEXSE_ZSHIFT),(MAP_EXSE(1,2),           16
     *MEXSE_ZSHIFT)                                                       16
      CHARACTER CEXSE_ZSHIFT*40/' '/                                      16
      CHARACTER NEXSE_ZSHIFT*12/'R.ZSHIFT'/                               16
      INTEGER MEXSE_ZSHIFT(2)/1,1/                                        16
      REAL RE_EXSE_ZSHIFT                                                 16
C -                                                                       16
      REAL EXSE_SECTYPE(6)                                                16
      EQUIVALENCE (BANK_EXSE(5),EXSE_SECTYPE,RE_EXSE_SECTYPE),            16
     *(NAMES_EXSE(3),NEXSE_SECTYPE)                                       16
      EQUIVALENCE (COMMEN_EXSE(3),CEXSE_SECTYPE),(MAP_EXSE(1,3),          16
     *MEXSE_SECTYPE)                                                      16
      CHARACTER CEXSE_SECTYPE*40/' '/                                     16
      CHARACTER NEXSE_SECTYPE*12/'R.SECTYPE'/                             16
      INTEGER MEXSE_SECTYPE(2)/6,1/                                       16
      REAL RE_EXSE_SECTYPE                                                16
C -                                                                       16
      INTEGER LEN_EXSE,LLEN_EXSE,LINK_EXSE                                16
      PARAMETER (LEN_EXSE=10)                                             16
      PARAMETER (LLEN_EXSE=3)                                             16
      INTEGER FLAG_EXSE/0/,MAP_EXSE(2,LLEN_EXSE),IDNUM_EXSE(10)           16
      CHARACTER NAMES_EXSE(LLEN_EXSE)*12,COMMEN_EXSE(LLEN_EXSE)*40        16
      DATA IDNUM_EXSE/10*0/                                               16
      REAL BANK_EXSE(LEN_EXSE)                                            16
      CHARACTER*60 BPATH_EXSE/' '/                                        16
      SAVE FLAG_EXSE,MAP_EXSE,IDNUM_EXSE,NAMES_EXSE,COMMEN_EXSE,          16
     *BPATH_EXSE                                                          16
      DATA LINK_EXSE/0/,BANK_EXSE/LEN_EXSE*0/                             16
C *                                                                       16
      INTEGER I_SECTION,J_SECTION,IE,IS,ISEC,I_STR,NSTR,TYPE,II,JJ,CUT,   18
     *FSECT,LSECT, IHALF,FILLED,MYKASE                                    18
      REAL CENTER,PLATE,CELL,G10,DIFF,HALFI,TAN_LOW,TAN_UPP,RBOT,RTOP,    21
     *DETA,ETAX,SQ2, SQ3,DUP,DD,D2,D3,RSHIFT,DPHI,RADIATOR,ORGKEEP,       21
     *ENDKEEP                                                             21
      REAL MAXCNT,MSECWD,MXGTEN,CURR,SECWID,SECTION,CURCL,ETATOP,ETABOT,  25
     *SLCWID,ZSLICE, GAP,MGT,XLEFT,XRIGHT,YLEFT,YRIGHT,CURRENT,RTH,LEN,   25
     *P,XC,YC,XX,YY,RBOTRAD,RDEL, DXY,DDN,DDUP                            25
      INTEGER N                                                           27
      PARAMETER (N=12)                                                    29
C * LAMBDA FUNCTION TANF                                                  30
      REAL TANF                                                           31
      TANF(ETAX) = TAN(2*ATAN(EXP(-ETAX)))                                32
C *                                                                       33
      CALL AGSBEGM(MODULE,IPRIN)                                          33
      IF (IPRIN.LT.0) RETURN                                              33
C Filling bank EMCG - EM ENDCAP CALORIMETER BASIC DATA                    33
      BANK_TITLE='EM ENDCAP CALORIMETER BASIC DATA'                       33
      BPATH_EMCG='EMCG*'                                                  33
      IDNUM_EMCG(1)=0                                                     34
C GEOMETRY VERSION                                                        34
      EMCG_VERSION=5.0                                                    34
      CEMCG_VERSION='GEOMETRY VERSION'                                    34
C CONFIGURATIONS 0-NO, 1-WEST 2-EAST 3-BOTH                               35
      EMCG_ONOFF=3                                                        35
      CEMCG_ONOFF='CONFIGURATIONS 0-NO, 1-WEST 2-EAST 3-BOTH'             35
C SECTORS FILL MODE                                                       36
      EMCG_FILLMODE=3                                                     36
      CEMCG_FILLMODE='SECTORS FILL MODE'                                  36
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EMCG',    37
     *BPATH_EMCG,IDNUM_EMCG,1, MAP_EMCG,NAMES_EMCG,COMMEN_EMCG,           37
     *BANK_EMCG,LEN_EMCG,LLEN_EMCG,LINK_EMCG,FLAG_EMCG)                   37
C *                                                                       37
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EMCG',    37
     *BPATH_EMCG,IDNUM_EMCG,1, MAP_EMCG,NAMES_EMCG,COMMEN_EMCG,           37
     *BANK_EMCG,LEN_EMCG,LLEN_EMCG,LINK_EMCG,FLAG_EMCG)                   37
C *                                                                       38
      CALL AGSBEGM(MODULE,IPRIN)                                          38
      IF (IPRIN.LT.0) RETURN                                              38
C Filling bank EMCS - EM ENDCAP CALORIMETER GEOMETRY                      38
      BANK_TITLE='EM ENDCAP CALORIMETER GEOMETRY'                         38
      BPATH_EMCS='EMCS*'                                                  38
      IDNUM_EMCS(1)=0                                                     39
C =1 ENDCAP, =2 FPD EDCAP PROTOTYPE                                       39
      EMCS_TYPE=1                                                         39
      CEMCS_TYPE='=1 ENDCAP, =2 FPD EDCAP PROTOTYPE'                      39
C CALORIMETER ORIGIN IN Z                                                 40
      EMCS_ZORIG=268.763                                                  40
      CEMCS_ZORIG='CALORIMETER ORIGIN IN Z'                               40
C CALORIMETER END IN Z                                                    41
      EMCS_ZEND=310.007                                                   41
      CEMCS_ZEND='CALORIMETER END IN Z'                                   41
C UPPER FEDUCIAL ETA CUT                                                  42
      EMCS_ETAMIN=1.086                                                   42
      CEMCS_ETAMIN='UPPER FEDUCIAL ETA CUT'                               42
C LOWER FIDUCIAL ETA CUT                                                  43
      EMCS_ETAMAX=2.000                                                   43
      CEMCS_ETAMAX='LOWER FIDUCIAL ETA CUT'                               43
C MIN PHI                                                                 44
      EMCS_PHIMIN=-90                                                     44
      CEMCS_PHIMIN='MIN PHI'                                              44
C MAX PHI                                                                 45
      EMCS_PHIMAX=90                                                      45
      CEMCS_PHIMAX='MAX PHI'                                              45
C OFFSET IN X                                                             46
      EMCS_OFFSET=0.0                                                     46
      CEMCS_OFFSET='OFFSET IN X'                                          46
C NUMBER OF AZIMUTHAL SUPERSECTORS                                        47
      EMCS_NSUPSEC=6                                                      47
      CEMCS_NSUPSEC='NUMBER OF AZIMUTHAL SUPERSECTORS'                    47
C NUMBER OF AZIMUTAL SECTORS (PHI GRANULARITY)                            48
      EMCS_NSECTOR=30                                                     48
      CEMCS_NSECTOR='NUMBER OF AZIMUTAL SECTORS (PHI GRANULARITY)'        48
C NUMBER OF PHI SLICES IN SUPERSECTOR                                     49
      EMCS_NSLICES=5                                                      49
      CEMCS_NSLICES='NUMBER OF PHI SLICES IN SUPERSECTOR'                 49
C NUMBER OF READOUT SECTIONS                                              50
      EMCS_NSECTION=4                                                     50
      CEMCS_NSECTION='NUMBER OF READOUT SECTIONS'                         50
C THICKNESS OF THE FRONT AL PLATES                                        51
      EMCS_FRONT=0.953                                                    51
      CEMCS_FRONT='THICKNESS OF THE FRONT AL PLATES'                      51
C ALUMINIM PLATE IN CELL                                                  52
      EMCS_ALINCELL=0.02                                                  52
      CEMCS_ALINCELL='ALUMINIM PLATE IN CELL'                             52
C FRONT PLASTIC IN MEGATILE                                               53
      EMCS_FRPLAST=0.015                                                  53
      CEMCS_FRPLAST='FRONT PLASTIC IN MEGATILE'                           53
C FIBER ROUTING GUIDES AND BACK PLASTIC                                   54
      EMCS_BKPLAST=0.155                                                  54
      CEMCS_BKPLAST='FIBER ROUTING GUIDES AND BACK PLASTIC'               54
C LEAD RADIATOR THICKNESS                                                 55
      EMCS_PBPLATE=0.457                                                  55
      CEMCS_PBPLATE='LEAD RADIATOR THICKNESS'                             55
C LAMINATED SS PLATE THICKNESS                                            56
      EMCS_LAMPLATE=0.05                                                  56
      CEMCS_LAMPLATE='LAMINATED SS PLATE THICKNESS'                       56
C BACK SS PLATE THICKNESS                                                 57
      EMCS_BCKPLATE=3.175                                                 57
      CEMCS_BCKPLATE='BACK SS PLATE THICKNESS'                            57
C THICKNESS OF ENDCAP HUB                                                 58
      EMCS_HUB=3.81                                                       58
      CEMCS_HUB='THICKNESS OF ENDCAP HUB'                                 58
C RADIAL SHIFT OF MODULE                                                  59
      EMCS_RMSHIFT=2.121                                                  59
      CEMCS_RMSHIFT='RADIAL SHIFT OF MODULE'                              59
C RADIAL SHIFT OF STEEL SUPPORT WALLS                                     60
      EMCS_SMSHIFT=0.12                                                   60
      CEMCS_SMSHIFT='RADIAL SHIFT OF STEEL SUPPORT WALLS'                 60
C HALF OF THE INTER-PLATE GAP IN PHI                                      61
      EMCS_GAPPLT=0.3/2                                                   61
      CEMCS_GAPPLT='HALF OF THE INTER-PLATE GAP IN PHI'                   61
C HALF OF THE RADIAL INTER-CELL GAP                                       62
      EMCS_GAPCEL=0.03/2                                                  62
      CEMCS_GAPCEL='HALF OF THE RADIAL INTER-CELL GAP'                    62
C SPACE FOR SMD DETECTOR                                                  63
      EMCS_GAPSMD=3.400                                                   63
      CEMCS_GAPSMD='SPACE FOR SMD DETECTOR'                               63
C SMD POSITION                                                            64
      EMCS_SMDCENTR=279.542                                               64
      CEMCS_SMDCENTR='SMD POSITION'                                       64
C RADIAL POSITION OF TIE RODS                                             65
      EMCS_TIEROD(1)=160.                                                 65
      EMCS_TIEROD(2)=195                                                  65
      CEMCS_TIEROD='RADIAL POSITION OF TIE RODS'                          65
C BACKPLATE FRONT Z                                                       66
      EMCS_BCKFRNT=306.832                                                66
      CEMCS_BCKFRNT='BACKPLATE FRONT Z'                                   66
C 1/2 GAP BETWEEN HALVES OF ENDCAP WHEEL                                  67
      EMCS_GAPHALF=0.4                                                    67
      CEMCS_GAPHALF='1/2 GAP BETWEEN HALVES OF ENDCAP WHEEL'              67
C COVER OF WHEEL HALF                                                     68
      EMCS_COVER=0.075                                                    68
      CEMCS_COVER='COVER OF WHEEL HALF'                                   68
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EMCS',    69
     *BPATH_EMCS,IDNUM_EMCS,1, MAP_EMCS,NAMES_EMCS,COMMEN_EMCS,           69
     *BANK_EMCS,LEN_EMCS,LLEN_EMCS,LINK_EMCS,FLAG_EMCS)                   69
C *                                                                       69
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EMCS',    69
     *BPATH_EMCS,IDNUM_EMCS,1, MAP_EMCS,NAMES_EMCS,COMMEN_EMCS,           69
     *BANK_EMCS,LEN_EMCS,LLEN_EMCS,LINK_EMCS,FLAG_EMCS)                   69
C *                                                                       70
      CALL AGSBEGM(MODULE,IPRIN)                                          70
      IF (IPRIN.LT.0) RETURN                                              70
C Filling bank EETR - ETA AND PHI GRID VALUES                             70
      BANK_TITLE='ETA AND PHI GRID VALUES'                                70
      BPATH_EETR='EETR*'                                                  70
      IDNUM_EETR(1)=0                                                     71
C =1 ENDCAP, =2 FPD                                                       71
      EETR_TYPE=1                                                         71
      CEETR_TYPE='=1 ENDCAP, =2 FPD'                                      71
C ETA_TOP/ETA_BOT TOWER GRANULARITY                                       72
      EETR_ETAGR=1.0536                                                   72
      CEETR_ETAGR='ETA_TOP/ETA_BOT TOWER GRANULARITY'                     72
C PHI GRANULARITY (RADIANS)                                               73
      EETR_PHIGR=0.0981747                                                73
      CEETR_PHIGR='PHI GRANULARITY (RADIANS)'                             73
C ETA GRANULARITY                                                         74
      EETR_NETA=12                                                        74
      CEETR_NETA='ETA GRANULARITY'                                        74
C ETA RAPIDITIES                                                          77
      EETR_ETABIN(1)=2.0                                                  77
      EETR_ETABIN(2)=1.9008                                               77
      EETR_ETABIN(3)=1.8065                                               77
      EETR_ETABIN(4)=1.7168                                               77
      EETR_ETABIN(5)=1.6317                                               77
      EETR_ETABIN(6)=1.5507                                               77
      EETR_ETABIN(7)=1.4738                                               77
      EETR_ETABIN(8)=1.4007                                               77
      EETR_ETABIN(9)=1.3312                                               77
      EETR_ETABIN(10)=1.2651                                              77
      EETR_ETABIN(11)=1.2023                                              77
      EETR_ETABIN(12)=1.1427                                              77
      EETR_ETABIN(13)=1.086                                               77
      CEETR_ETABIN='ETA RAPIDITIES'                                       77
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EETR',    78
     *BPATH_EETR,IDNUM_EETR,1, MAP_EETR,NAMES_EETR,COMMEN_EETR,           78
     *BANK_EETR,LEN_EETR,LLEN_EETR,LINK_EETR,FLAG_EETR)                   78
C *                                                                       78
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EETR',    78
     *BPATH_EETR,IDNUM_EETR,1, MAP_EETR,NAMES_EETR,COMMEN_EETR,           78
     *BANK_EETR,LEN_EETR,LLEN_EETR,LINK_EETR,FLAG_EETR)                   78
C *                                                                       79
      CALL AGSBEGM(MODULE,IPRIN)                                          79
      IF (IPRIN.LT.0) RETURN                                              79
C Filling bank ESEC - FIRST EM SECTION                                    79
      BANK_TITLE='FIRST EM SECTION'                                       79
      BPATH_ESEC='ESEC*'                                                  79
      IDNUM_ESEC(1)=0                                                     80
C SECTION NUMBER                                                          80
      ESEC_ISECT=1                                                        80
      CESEC_ISECT='SECTION NUMBER'                                        80
C NUMBER OF SCI LAYERS ALONG Z                                            81
      ESEC_NLAYER=1                                                       81
      CESEC_NLAYER='NUMBER OF SCI LAYERS ALONG Z'                         81
C CELL FULL WIDTH IN Z                                                    82
      ESEC_CELL=1.505                                                     82
      CESEC_CELL='CELL FULL WIDTH IN Z'                                   82
C SCI LAYER THICKNESS                                                     83
      ESEC_SCINT=0.5                                                      83
      CESEC_SCINT='SCI LAYER THICKNESS'                                   83
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESEC',    84
     *BPATH_ESEC,IDNUM_ESEC,1, MAP_ESEC,NAMES_ESEC,COMMEN_ESEC,           84
     *BANK_ESEC,LEN_ESEC,LLEN_ESEC,LINK_ESEC,FLAG_ESEC)                   84
C *                                                                       84
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESEC',    84
     *BPATH_ESEC,IDNUM_ESEC,1, MAP_ESEC,NAMES_ESEC,COMMEN_ESEC,           84
     *BANK_ESEC,LEN_ESEC,LLEN_ESEC,LINK_ESEC,FLAG_ESEC)                   84
C *                                                                       85
      CALL AGSBEGM(MODULE,IPRIN)                                          85
      IF (IPRIN.LT.0) RETURN                                              85
C Filling bank ESEC - FIRST EM SECTION                                    85
      BANK_TITLE='FIRST EM SECTION'                                       85
      BPATH_ESEC='ESEC*'                                                  85
      IDNUM_ESEC(1)=0                                                     86
C SECTION NUMBER                                                          86
      ESEC_ISECT=2                                                        86
      CESEC_ISECT='SECTION NUMBER'                                        86
C NUMBER OF SCI LAYERS ALONG Z                                            87
      ESEC_NLAYER=1                                                       87
      CESEC_NLAYER='NUMBER OF SCI LAYERS ALONG Z'                         87
C CELL FULL WIDTH IN Z                                                    88
      ESEC_CELL=1.505                                                     88
      CESEC_CELL='CELL FULL WIDTH IN Z'                                   88
C SCI LAYER THICKNESS                                                     89
      ESEC_SCINT=0.5                                                      89
      CESEC_SCINT='SCI LAYER THICKNESS'                                   89
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESEC',    90
     *BPATH_ESEC,IDNUM_ESEC,1, MAP_ESEC,NAMES_ESEC,COMMEN_ESEC,           90
     *BANK_ESEC,LEN_ESEC,LLEN_ESEC,LINK_ESEC,FLAG_ESEC)                   90
C *                                                                       90
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESEC',    90
     *BPATH_ESEC,IDNUM_ESEC,1, MAP_ESEC,NAMES_ESEC,COMMEN_ESEC,           90
     *BANK_ESEC,LEN_ESEC,LLEN_ESEC,LINK_ESEC,FLAG_ESEC)                   90
C *                                                                       91
      CALL AGSBEGM(MODULE,IPRIN)                                          91
      IF (IPRIN.LT.0) RETURN                                              91
C Filling bank ESEC - SECOND EM SECTION                                   91
      BANK_TITLE='SECOND EM SECTION'                                      91
      BPATH_ESEC='ESEC*'                                                  91
      IDNUM_ESEC(1)=0                                                     92
C SECTION NUMBER                                                          92
      ESEC_ISECT=3                                                        92
      CESEC_ISECT='SECTION NUMBER'                                        92
C NUMBER OF SCI LAYERS ALONG Z                                            93
      ESEC_NLAYER=4                                                       93
      CESEC_NLAYER='NUMBER OF SCI LAYERS ALONG Z'                         93
C CELL FULL WIDTH IN Z                                                    94
      ESEC_CELL=1.405                                                     94
      CESEC_CELL='CELL FULL WIDTH IN Z'                                   94
C SCI LAYER THICKNESS                                                     95
      ESEC_SCINT=0.4                                                      95
      CESEC_SCINT='SCI LAYER THICKNESS'                                   95
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESEC',    96
     *BPATH_ESEC,IDNUM_ESEC,1, MAP_ESEC,NAMES_ESEC,COMMEN_ESEC,           96
     *BANK_ESEC,LEN_ESEC,LLEN_ESEC,LINK_ESEC,FLAG_ESEC)                   96
C *                                                                       96
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESEC',    96
     *BPATH_ESEC,IDNUM_ESEC,1, MAP_ESEC,NAMES_ESEC,COMMEN_ESEC,           96
     *BANK_ESEC,LEN_ESEC,LLEN_ESEC,LINK_ESEC,FLAG_ESEC)                   96
C *                                                                       97
      CALL AGSBEGM(MODULE,IPRIN)                                          97
      IF (IPRIN.LT.0) RETURN                                              97
C Filling bank ESEC - THIRD EM SECTION                                    97
      BANK_TITLE='THIRD EM SECTION'                                       97
      BPATH_ESEC='ESEC*'                                                  97
      IDNUM_ESEC(1)=0                                                     98
C SECTION                                                                 98
      ESEC_ISECT=4                                                        98
      CESEC_ISECT='SECTION'                                               98
C NUMBER OF LAYERS ALONG Z                                                99
      ESEC_NLAYER=18                                                      99
      CESEC_NLAYER='NUMBER OF LAYERS ALONG Z'                             99
C CELL FULL WIDTH IN Z                                                    100
      ESEC_CELL=1.405                                                     100
      CESEC_CELL='CELL FULL WIDTH IN Z'                                   100
C SCI LAYER THICKNESS                                                     101
      ESEC_SCINT=0.4                                                      101
      CESEC_SCINT='SCI LAYER THICKNESS'                                   101
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESEC',    102
     *BPATH_ESEC,IDNUM_ESEC,1, MAP_ESEC,NAMES_ESEC,COMMEN_ESEC,           102
     *BANK_ESEC,LEN_ESEC,LLEN_ESEC,LINK_ESEC,FLAG_ESEC)                   102
C *                                                                       102
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESEC',    102
     *BPATH_ESEC,IDNUM_ESEC,1, MAP_ESEC,NAMES_ESEC,COMMEN_ESEC,           102
     *BANK_ESEC,LEN_ESEC,LLEN_ESEC,LINK_ESEC,FLAG_ESEC)                   102
C *                                                                       103
      CALL AGSBEGM(MODULE,IPRIN)                                          103
      IF (IPRIN.LT.0) RETURN                                              103
C Filling bank ESEC - 4TH EM SECTION                                      103
      BANK_TITLE='4TH EM SECTION'                                         103
      BPATH_ESEC='ESEC*'                                                  103
      IDNUM_ESEC(1)=0                                                     104
C SECTION                                                                 104
      ESEC_ISECT=5                                                        104
      CESEC_ISECT='SECTION'                                               104
C NUMBER OF LAYERS ALONG Z                                                105
      ESEC_NLAYER=1                                                       105
      CESEC_NLAYER='NUMBER OF LAYERS ALONG Z'                             105
C CELL FULL WIDTH IN Z                                                    106
      ESEC_CELL=1.505                                                     106
      CESEC_CELL='CELL FULL WIDTH IN Z'                                   106
C SCI LAYER THICKNESS                                                     107
      ESEC_SCINT=0.5                                                      107
      CESEC_SCINT='SCI LAYER THICKNESS'                                   107
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESEC',    108
     *BPATH_ESEC,IDNUM_ESEC,1, MAP_ESEC,NAMES_ESEC,COMMEN_ESEC,           108
     *BANK_ESEC,LEN_ESEC,LLEN_ESEC,LINK_ESEC,FLAG_ESEC)                   108
C *                                                                       108
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESEC',    108
     *BPATH_ESEC,IDNUM_ESEC,1, MAP_ESEC,NAMES_ESEC,COMMEN_ESEC,           108
     *BANK_ESEC,LEN_ESEC,LLEN_ESEC,LINK_ESEC,FLAG_ESEC)                   108
C *                                                                       109
      CALL AGSBEGM(MODULE,IPRIN)                                          109
      IF (IPRIN.LT.0) RETURN                                              109
C Filling bank EMXG - EM ENDCAP SMD BASIC DATA                            109
      BANK_TITLE='EM ENDCAP SMD BASIC DATA'                               109
      BPATH_EMXG='EMXG*'                                                  109
      IDNUM_EMXG(1)=0                                                     110
C GEOMETRY VERSION                                                        110
      EMXG_VERSION=1                                                      110
      CEMXG_VERSION='GEOMETRY VERSION'                                    110
C SCINTILLATOR STRIP APEX                                                 111
      EMXG_SAPEX=0.7                                                      111
      CEMXG_SAPEX='SCINTILLATOR STRIP APEX'                               111
C SCINTILLATOR STRIP BASE                                                 112
      EMXG_SBASE=1.0                                                      112
      CEMXG_SBASE='SCINTILLATOR STRIP BASE'                               112
C INNER RADIUS OF SMD PLANE                                               113
      EMXG_RIN=77.41                                                      113
      CEMXG_RIN='INNER RADIUS OF SMD PLANE'                               113
C OUTER RADIUS OF SMD PLANE                                               114
      EMXG_ROUT=213.922                                                   114
      CEMXG_ROUT='OUTER RADIUS OF SMD PLANE'                              114
C F4 THICKNESS                                                            115
      EMXG_F4=.15                                                         115
      CEMXG_F4='F4 THICKNESS'                                             115
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EMXG',    116
     *BPATH_EMXG,IDNUM_EMXG,1, MAP_EMXG,NAMES_EMXG,COMMEN_EMXG,           116
     *BANK_EMXG,LEN_EMXG,LLEN_EMXG,LINK_EMXG,FLAG_EMXG)                   116
C *                                                                       116
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EMXG',    116
     *BPATH_EMXG,IDNUM_EMXG,1, MAP_EMXG,NAMES_EMXG,COMMEN_EMXG,           116
     *BANK_EMXG,LEN_EMXG,LLEN_EMXG,LINK_EMXG,FLAG_EMXG)                   116
C *                                                                       117
      CALL AGSBEGM(MODULE,IPRIN)                                          117
      IF (IPRIN.LT.0) RETURN                                              117
C Filling bank EXSE - FIRST SMD SECTION                                   117
      BANK_TITLE='FIRST SMD SECTION'                                      117
      BPATH_EXSE='EXSE*'                                                  117
      IDNUM_EXSE(1)=0                                                     118
C SECTION NUMBER                                                          118
      EXSE_JSECT=1                                                        118
      CEXSE_JSECT='SECTION NUMBER'                                        118
C SECTION WIDTH                                                           119
      EXSE_ZSHIFT=-1.215                                                  119
      CEXSE_ZSHIFT='SECTION WIDTH'                                        119
C 1-V,2-U,3-CUTV,4-CUTU                                                   120
      EXSE_SECTYPE(1)=4                                                   120
      EXSE_SECTYPE(2)=1                                                   120
      EXSE_SECTYPE(3)=0                                                   120
      EXSE_SECTYPE(4)=2                                                   120
      EXSE_SECTYPE(5)=1                                                   120
      EXSE_SECTYPE(6)=0                                                   120
      CEXSE_SECTYPE='1-V,2-U,3-CUTV,4-CUTU'                               120
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EXSE',    121
     *BPATH_EXSE,IDNUM_EXSE,1, MAP_EXSE,NAMES_EXSE,COMMEN_EXSE,           121
     *BANK_EXSE,LEN_EXSE,LLEN_EXSE,LINK_EXSE,FLAG_EXSE)                   121
C *                                                                       121
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EXSE',    121
     *BPATH_EXSE,IDNUM_EXSE,1, MAP_EXSE,NAMES_EXSE,COMMEN_EXSE,           121
     *BANK_EXSE,LEN_EXSE,LLEN_EXSE,LINK_EXSE,FLAG_EXSE)                   121
C *                                                                       122
      CALL AGSBEGM(MODULE,IPRIN)                                          122
      IF (IPRIN.LT.0) RETURN                                              122
C Filling bank EXSE - SECOND SMD SECTION                                  122
      BANK_TITLE='SECOND SMD SECTION'                                     122
      BPATH_EXSE='EXSE*'                                                  122
      IDNUM_EXSE(1)=0                                                     123
C SECTION NUMBER                                                          123
      EXSE_JSECT=2                                                        123
      CEXSE_JSECT='SECTION NUMBER'                                        123
C SECTION WIDTH                                                           124
      EXSE_ZSHIFT=0.                                                      124
      CEXSE_ZSHIFT='SECTION WIDTH'                                        124
C 1-V,2-U,3-CUTV,4-CUTU                                                   125
      EXSE_SECTYPE(1)=0                                                   125
      EXSE_SECTYPE(2)=2                                                   125
      EXSE_SECTYPE(3)=1                                                   125
      EXSE_SECTYPE(4)=0                                                   125
      EXSE_SECTYPE(5)=2                                                   125
      EXSE_SECTYPE(6)=3                                                   125
      CEXSE_SECTYPE='1-V,2-U,3-CUTV,4-CUTU'                               125
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EXSE',    126
     *BPATH_EXSE,IDNUM_EXSE,1, MAP_EXSE,NAMES_EXSE,COMMEN_EXSE,           126
     *BANK_EXSE,LEN_EXSE,LLEN_EXSE,LINK_EXSE,FLAG_EXSE)                   126
C *                                                                       126
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EXSE',    126
     *BPATH_EXSE,IDNUM_EXSE,1, MAP_EXSE,NAMES_EXSE,COMMEN_EXSE,           126
     *BANK_EXSE,LEN_EXSE,LLEN_EXSE,LINK_EXSE,FLAG_EXSE)                   126
C *                                                                       127
      CALL AGSBEGM(MODULE,IPRIN)                                          127
      IF (IPRIN.LT.0) RETURN                                              127
C Filling bank EXSE - THIRD SMD SECTION                                   127
      BANK_TITLE='THIRD SMD SECTION'                                      127
      BPATH_EXSE='EXSE*'                                                  127
      IDNUM_EXSE(1)=0                                                     128
C SECTION NUMBER                                                          128
      EXSE_JSECT=3                                                        128
      CEXSE_JSECT='SECTION NUMBER'                                        128
C SECTION WIDTH                                                           129
      EXSE_ZSHIFT=1.215                                                   129
      CEXSE_ZSHIFT='SECTION WIDTH'                                        129
C 1-V,2-U,3-CUTV,4-CUTU                                                   130
      EXSE_SECTYPE(1)=1                                                   130
      EXSE_SECTYPE(2)=0                                                   130
      EXSE_SECTYPE(3)=2                                                   130
      EXSE_SECTYPE(4)=1                                                   130
      EXSE_SECTYPE(5)=0                                                   130
      EXSE_SECTYPE(6)=2                                                   130
      CEXSE_SECTYPE='1-V,2-U,3-CUTV,4-CUTU'                               130
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EXSE',    131
     *BPATH_EXSE,IDNUM_EXSE,1, MAP_EXSE,NAMES_EXSE,COMMEN_EXSE,           131
     *BANK_EXSE,LEN_EXSE,LLEN_EXSE,LINK_EXSE,FLAG_EXSE)                   131
C *                                                                       131
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EXSE',    131
     *BPATH_EXSE,IDNUM_EXSE,1, MAP_EXSE,NAMES_EXSE,COMMEN_EXSE,           131
     *BANK_EXSE,LEN_EXSE,LLEN_EXSE,LINK_EXSE,FLAG_EXSE)                   131
C use default EMCG bank                                                   132
      BPATH_EMCG='EMCG*'                                                  132
      IDNUM_EMCG(1)=0                                                     132
      CALL AGDGETP('ECALGEO','*',-999,'EMCG',' ',BANK_EMCG, BPATH_EMCG,   132
     *IDNUM_EMCG,1,MAP_EMCG,NAMES_EMCG,BANK_EMCG,LEN_EMCG,LLEN_EMCG,      132
     *LINK_EMCG,FLAG_EMCG)                                                132
      SQ3 = SQRT(3.)                                                      133
      SQ2 = SQRT(2.)                                                      134
C Check IPRIN>=1                                                          134
      IF (IPRIN.GE.1) WRITE(LOUT,5010)EMCG_VERSION                        134
5010  FORMAT('ECALGEO version ', F4.2)                                    136
C use EMCS bank                                                           137
      BPATH_EMCS='EMCS*'                                                  137
      IDNUM_EMCS(1)=0                                                     137
      EMCS_TYPE=1                                                         137
      CALL AGDGETP('ECALGEO','*',-999,'EMCS',NEMCS_TYPE,RE_EMCS_TYPE,     137
     *BPATH_EMCS,IDNUM_EMCS,1,MAP_EMCS,NAMES_EMCS,BANK_EMCS,LEN_EMCS,     137
     *LLEN_EMCS,LINK_EMCS,FLAG_EMCS)                                      137
C use EETR bank                                                           138
      BPATH_EETR='EETR*'                                                  138
      IDNUM_EETR(1)=0                                                     138
      EETR_TYPE=1                                                         138
      CALL AGDGETP('ECALGEO','*',-999,'EETR',NEETR_TYPE,RE_EETR_TYPE,     138
     *BPATH_EETR,IDNUM_EETR,1,MAP_EETR,NAMES_EETR,BANK_EETR,LEN_EETR,     138
     *LLEN_EETR,LINK_EETR,FLAG_EETR)                                      138
      ORGKEEP = EMCS_ZORIG                                                139
      ENDKEEP = EMCS_ZEND                                                 140
C Check EMCG_ONOFF>0                                                      141
      IF (EMCG_ONOFF.GT.0) THEN                                           141
      DIFF = 0.0                                                          142
      CENTER = (EMCS_ZORIG+EMCS_ZEND)/2                                   143
      TAN_UPP = TANF(EMCS_ETAMIN)                                         144
      TAN_LOW = TANF(EMCS_ETAMAX)                                         145
      RTH = SQRT(1. + TAN_LOW*TAN_LOW)                                    146
      RSHIFT = EMCS_HUB * RTH                                             147
      DUP=EMCS_RMSHIFT*TAN_UPP                                            148
      DD=EMCS_RMSHIFT*RTH                                                 149
      D2=RSHIFT + DD                                                      150
      RADIATOR = EMCS_PBPLATE + 2*EMCS_LAMPLATE                           151
      DPHI = (EMCS_PHIMAX-EMCS_PHIMIN)/EMCS_NSECTOR                       152
      CALL AGSBEGM(MODULE,IPRIN)                                          153
C Check IPRIN<0                                                           153
      IF (IPRIN.LT.0) RETURN                                              153
C Create ECAL :                                                           153
C Call To Local Procedure ECAL                                            153
      ASSIGN 5030 TO BLOCK_ECAL                                           153
      GO TO  5020                                                         153
5030  CONTINUE                                                            153
C *                                                                       153
C Check EMCG_ONOFF==1|EMCG_ONOFF==3                                       154
      IF (EMCG_ONOFF.EQ.1.OR.EMCG_ONOFF.EQ.3) THEN                        154
C Geant POSITION : ECAL ===> MOTHER='CAVE' Z=+CENTER <===                 155
      CALL AGSRESET                                                       155
      AG_MOTHER = 'CAVE'                                                  155
      AG_Z = +CENTER                                                      155
      AG_TITLE = 'ECAL'                                                   155
      AG_EXNAME = 'POSITION'                                              155
      AG_PARLIST = 'MOTHER_Z'                                             155
      CALL AXPOSITION                                                     155
C *                                                                       155
      ENDIF                                                               155
C Check EMCG_ONOFF==2|EMCG_ONOFF==3                                       157
      IF (EMCG_ONOFF.EQ.2.OR.EMCG_ONOFF.EQ.3) THEN                        157
C Geant POSITION : ECAL ===> MOTHER='CAVE' Z=-CENTER THETAZ=180 <===      158
      CALL AGSRESET                                                       158
      AG_MOTHER = 'CAVE'                                                  158
      AG_Z = -CENTER                                                      158
      AG_THETAZ = 180                                                     158
      AG_TITLE = 'ECAL'                                                   158
      AG_EXNAME = 'POSITION'                                              158
      AG_PARLIST = 'MOTHER_Z_THETAZ'                                      158
      CALL AXPOSITION                                                     158
C *                                                                       158
      ENDIF                                                               158
C Check SECTION>EMCS_ZEND                                                 160
      IF (SECTION.GT.EMCS_ZEND) THEN                                      160
      WRITE(LOUT,5040)SECTION,EMCS_ZEND                                   160
5040  FORMAT(' ECALGEO error: sum of sections exceeds maximum ',2F12.4)   162
      ENDIF                                                               163
C Check IPRIN>=1                                                          163
      IF (IPRIN.GE.1) WRITE(LOUT,5050)SECTION                             163
5050  FORMAT(' EndCap calorimeter total depth ',F12.4)                    165
      ENDIF                                                               166
C Check IPRIN>=1                                                          166
      IF (IPRIN.GE.1) WRITE(LOUT,5060)                                    166
5060  FORMAT('ECALGEO finished')                                          168
C ----------------------------------------------------------------------  169
C ------------ ECAL                                                       169
      CALL AGSENDM                                                        170
      RETURN                                                              170
C Begin Local Procedure ECAL                                              170
C ECAL IS ONE EMC ENDCAP WHEEL                                            170
5020  CONTINUE                                                            170
         AG_TITLE='ECAL'                                                  170
         AG_MODULE=MODULE                                                 170
         AG_MARK='V00'                                                    170
         CALL AGSPUSH                                                     170
         AG_MODULE=MODULE                                                 170
C    *                                                                    170
C    Geant MATERIAL : AIR ===> inherited arguments used <===              171
         CALL AGSRESET                                                    171
         AG_TITLE = 'AIR'                                                 171
         AG_EXNAME = 'MATERIAL'                                           171
         AG_PARLIST = 'NONE'                                              171
         CALL AXMATERIAL                                                  171
C    *                                                                    171
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           171
         CALL AGSRESET                                                    172
         AG_TITLE = 'STANDARD'                                            172
         AG_EXNAME = 'MEDIUM'                                             172
         AG_PARLIST = 'NONE'                                              172
         CALL AXMEDIUM                                                    172
C    *                                                                    172
C    Geant ATTRIBUTE : ECAL ===> SEEN=1 COLO=7 <===                       172
         CALL AGSRESET                                                    173
         AG_SEEN = 1                                                      173
         AG_COLO = 7                                                      173
         AG_TITLE = 'ECAL'                                                173
         AG_EXNAME = 'ATTRIBUTE'                                          173
         AG_PARLIST = 'SEEN_COLO'                                         173
         CALL AXATTRIBUTE                                                 173
C    *                                                                    173
C    Geant SHAPE : CONE ===> DZ=(EMCS_ZEND-EMCS_ZORIG)/2 RMN1=ORGKEEP*TA  173
C    N_LOW-D2, RMX1=ORGKEEP*TAN_UPP+DUP, RMN2=ENDKEEP*TAN_LOW-D2 RMX2=EN  173
C    DKEEP*TAN_UPP+DUP <===                                               173
         CALL AGSRESET                                                    176
         AG_DZ = (EMCS_ZEND-EMCS_ZORIG)/2                                 176
         AG_RMN1 = ORGKEEP*TAN_LOW-D2                                     176
         AG_RMX1 = ORGKEEP*TAN_UPP+DUP                                    176
         AG_RMN2 = ENDKEEP*TAN_LOW-D2                                     176
         AG_RMX2 = ENDKEEP*TAN_UPP+DUP                                    176
         AG_TITLE = 'CONE'                                                176
         AG_EXNAME = 'SHAPE'                                              176
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            176
         CALL AXSHAPE                                                     176
C    *                                                                    176
C    *                                                                    176
         IHALF=1                                                          177
            GO TO 5073                                                    177
5071        IHALF=IHALF+1                                                 177
5073        IF(IHALF-(2).GT.0) GO TO 5072                                 177
            FILLED=1                                                      178
            HALFI = -105 + (IHALF-1)*180                                  179
C       Check (IHALF==2&EMCG_FILLMODE<3)                                  180
            IF ((IHALF.EQ.2.AND.EMCG_FILLMODE.LT.3)) THEN                 180
            FILLED = 0                                                    181
            ENDIF                                                         182
C       Create EAGA :                                                     183
C       Call To Local Procedure EAGA                                      183
            ASSIGN 5090 TO BLOCK_EAGA                                     183
            GO TO  5080                                                   183
5090        CONTINUE                                                      183
C       *                                                                 183
C       Geant POSITION : EAGA ===> ALPHAZ=HALFI <===                      184
            CALL AGSRESET                                                 184
            AG_ALPHAN=AG_ALPHAN+1                                         184
            AG_ALPHAK(AG_ALPHAN)=3                                        184
            AG_ALPHAL(AG_ALPHAN)=HALFI                                    184
            AG_TITLE = 'EAGA'                                             184
            AG_EXNAME = 'POSITION'                                        184
            AG_PARLIST = 'ALPHAZ'                                         184
            CALL AXPOSITION                                               184
C       *                                                                 184
         GO TO 5071                                                       185
5072     CONTINUE                                                         185
5021  CALL AGSPOPB                                                        186
      GO TO BLOCK_ECAL, (5020 ,5030)                                      186
C End of Local Procedure                                                  186
C ----------------------------------------------------------------------  187
C ------------ EAGA                                                       187
C Begin Local Procedure EAGA                                              188
C EAGA IS HALF OF WHEEL AIR VOLUME FOR THE ENDCAP MODULE                  188
5080  CONTINUE                                                            188
         AG_TITLE='EAGA'                                                  188
         AG_MODULE=MODULE                                                 188
         AG_MARK='V00'                                                    188
         CALL AGSPUSH                                                     188
         AG_MODULE=MODULE                                                 188
C    *                                                                    188
C    Geant ATTRIBUTE : EAGA ===> SEEN=1 COLO=1 SERIAL=FILLED <===         189
         CALL AGSRESET                                                    189
         AG_SEEN = 1                                                      189
         AG_COLO = 1                                                      189
         AG_SERIAL = FILLED                                               189
         AG_TITLE = 'EAGA'                                                189
         AG_EXNAME = 'ATTRIBUTE'                                          189
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  189
         CALL AXATTRIBUTE                                                 189
C    *                                                                    189
C    Geant MATERIAL : AIR ===> inherited arguments used <===              189
         CALL AGSRESET                                                    190
         AG_TITLE = 'AIR'                                                 190
         AG_EXNAME = 'MATERIAL'                                           190
         AG_PARLIST = 'NONE'                                              190
         CALL AXMATERIAL                                                  190
C    *                                                                    190
C    Geant SHAPE : CONS ===> DZ=(EMCS_ZEND-EMCS_ZORIG)/2 RMN1=ORGKEEP*TA  190
C    N_LOW-D2, RMX1=ORGKEEP*TAN_UPP+DUP, RMN2=ENDKEEP*TAN_LOW-D2 RMX2=EN  190
C    DKEEP*TAN_UPP+DUP, PHI1=EMCS_PHIMIN PHI2=EMCS_PHIMAX <===            190
         CALL AGSRESET                                                    194
         AG_DZ = (EMCS_ZEND-EMCS_ZORIG)/2                                 194
         AG_RMN1 = ORGKEEP*TAN_LOW-D2                                     194
         AG_RMX1 = ORGKEEP*TAN_UPP+DUP                                    194
         AG_RMN2 = ENDKEEP*TAN_LOW-D2                                     194
         AG_RMX2 = ENDKEEP*TAN_UPP+DUP                                    194
         AG_PHI1 = EMCS_PHIMIN                                            194
         AG_PHI2 = EMCS_PHIMAX                                            194
         AG_TITLE = 'CONS'                                                194
         AG_EXNAME = 'SHAPE'                                              194
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2_PHI1_PHI2'                  194
         CALL AXSHAPE                                                     194
C    *                                                                    194
C    Check FILLED==1                                                      194
         IF (FILLED.EQ.1) THEN                                            195
C    Create EMSS :                                                        196
C    Call To Local Procedure EMSS                                         196
         ASSIGN 5110 TO BLOCK_EMSS                                        196
         GO TO  5100                                                      196
5110     CONTINUE                                                         196
C    *                                                                    196
C    Geant POSITION : EMSS ===> KONLY='MANY' <===                         197
         CALL AGSRESET                                                    197
         AG_KONLY = 'MANY'                                                197
         AG_TITLE = 'EMSS'                                                197
         AG_EXNAME = 'POSITION'                                           197
         AG_PARLIST = 'KONLY'                                             197
         CALL AXPOSITION                                                  197
C    *                                                                    197
         CURR = ORGKEEP                                                   197
         CURCL = ENDKEEP                                                  198
C    Create ECGH :                                                        199
C    Call To Local Procedure ECGH                                         199
         ASSIGN 5130 TO BLOCK_ECGH                                        199
         GO TO  5120                                                      199
5130     CONTINUE                                                         199
C    *                                                                    199
C    Geant POSITION : ECGH ===> ALPHAZ=90 KONLY='ONLY' <===               200
         CALL AGSRESET                                                    200
         AG_ALPHAN=AG_ALPHAN+1                                            200
         AG_ALPHAK(AG_ALPHAN)=3                                           200
         AG_ALPHAL(AG_ALPHAN)=90                                          200
         AG_KONLY = 'ONLY'                                                200
         AG_TITLE = 'ECGH'                                                200
         AG_EXNAME = 'POSITION'                                           200
         AG_PARLIST = 'ALPHAZ_KONLY'                                      200
         CALL AXPOSITION                                                  200
C    *                                                                    200
         ENDIF                                                            200
5081  CALL AGSPOPB                                                        202
      GO TO BLOCK_EAGA, (5080 ,5090)                                      202
C End of Local Procedure                                                  202
C ----------------------------------------------------------------------  203
C ------------ EMSS                                                       203
C Begin Local Procedure EMSS                                              204
C EMSS IS STEEL SUPPORT OF THE ENDCAP MODULE                              204
5100  CONTINUE                                                            204
         AG_TITLE='EMSS'                                                  204
         AG_MODULE=MODULE                                                 204
         AG_MARK='V00'                                                    204
         CALL AGSPUSH                                                     204
         AG_MODULE=MODULE                                                 204
C    *                                                                    204
C    Geant ATTRIBUTE : EMSS ===> SEEN=1 COLO=1 <===                       205
         CALL AGSRESET                                                    205
         AG_SEEN = 1                                                      205
         AG_COLO = 1                                                      205
         AG_TITLE = 'EMSS'                                                205
         AG_EXNAME = 'ATTRIBUTE'                                          205
         AG_PARLIST = 'SEEN_COLO'                                         205
         CALL AXATTRIBUTE                                                 205
C    *                                                                    205
C    Geant MATERIAL : IRON ===> inherited arguments used <===             205
         CALL AGSRESET                                                    206
         AG_TITLE = 'IRON'                                                206
         AG_EXNAME = 'MATERIAL'                                           206
         AG_PARLIST = 'NONE'                                              206
         CALL AXMATERIAL                                                  206
C    *                                                                    206
C    Geant SHAPE : CONS ===> DZ=(EMCS_ZEND-EMCS_ZORIG)/2 RMN1=ORGKEEP*TA  206
C    N_LOW-D2, RMX1=ORGKEEP*TAN_UPP+DUP, RMN2=ENDKEEP*TAN_LOW-D2 RMX2=EN  206
C    DKEEP*TAN_UPP+DUP, PHI1=EMCS_PHIMIN PHI2=EMCS_PHIMAX <===            206
         CALL AGSRESET                                                    210
         AG_DZ = (EMCS_ZEND-EMCS_ZORIG)/2                                 210
         AG_RMN1 = ORGKEEP*TAN_LOW-D2                                     210
         AG_RMX1 = ORGKEEP*TAN_UPP+DUP                                    210
         AG_RMN2 = ENDKEEP*TAN_LOW-D2                                     210
         AG_RMX2 = ENDKEEP*TAN_UPP+DUP                                    210
         AG_PHI1 = EMCS_PHIMIN                                            210
         AG_PHI2 = EMCS_PHIMAX                                            210
         AG_TITLE = 'CONS'                                                210
         AG_EXNAME = 'SHAPE'                                              210
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2_PHI1_PHI2'                  210
         CALL AXSHAPE                                                     210
C    *                                                                    210
         ZSLICE = EMCS_ZORIG                                              210
C    Check IPRIN>=1                                                       211
         IF (IPRIN.GE.1) WRITE(LOUT,5140)ZSLICE                           211
5140     FORMAT(' Front Al plane starts at: ',F12.4)                      213
         SLCWID = EMCS_FRONT                                              214
C    Create EFLP :                                                        215
C    Call To Local Procedure EFLP                                         215
         ASSIGN 5160 TO BLOCK_EFLP                                        215
         GO TO  5150                                                      215
5160     CONTINUE                                                         215
C    *                                                                    215
C    Geant POSITION : EFLP ===> Z=ZSLICE-CENTER+SLCWID/2 <===             216
         CALL AGSRESET                                                    216
         AG_Z = ZSLICE-CENTER+SLCWID/2                                    216
         AG_TITLE = 'EFLP'                                                216
         AG_EXNAME = 'POSITION'                                           216
         AG_PARLIST = 'Z'                                                 216
         CALL AXPOSITION                                                  216
C    *                                                                    216
         ZSLICE = ZSLICE + SLCWID                                         216
C    Check IPRIN>=1                                                       217
         IF (IPRIN.GE.1) WRITE(LOUT,5170)ZSLICE                           217
5170     FORMAT(' First calorimeter starts at: ',F12.4)                   219
         FSECT = 1                                                        219
         LSECT = 3                                                        220
         SLCWID = EMCS_SMDCENTR - EMCS_GAPSMD/2 - ZSLICE                  221
C    Create ECVO :                                                        222
C    Call To Local Procedure ECVO                                         222
         ASSIGN 5190 TO BLOCK_ECVO                                        222
         GO TO  5180                                                      222
5190     CONTINUE                                                         222
C    *                                                                    222
C    Geant POSITION : ECVO ===> Z=ZSLICE-CENTER+SLCWID/2 <===             223
         CALL AGSRESET                                                    223
         AG_Z = ZSLICE-CENTER+SLCWID/2                                    223
         AG_TITLE = 'ECVO'                                                223
         AG_EXNAME = 'POSITION'                                           223
         AG_PARLIST = 'Z'                                                 223
         CALL AXPOSITION                                                  223
C    *                                                                    223
         SLCWID = EMCS_GAPSMD                                             223
         ZSLICE = EMCS_SMDCENTR - EMCS_GAPSMD/2                           225
C    Check IPRIN>=1                                                       225
         IF (IPRIN.GE.1) WRITE(LOUT,5200)SECTION,ZSLICE                   225
5200     FORMAT(' 1st calorimeter ends, SMD starts at: ',2F10.5)          227
C    Create ESHM :                                                        228
C    Call To Local Procedure ESHM                                         228
         ASSIGN 5220 TO BLOCK_ESHM                                        228
         GO TO  5210                                                      228
5220     CONTINUE                                                         228
C    *                                                                    228
C    Geant POSITION : ESHM ===> Z=ZSLICE-CENTER+SLCWID/2 <===             229
         CALL AGSRESET                                                    229
         AG_Z = ZSLICE-CENTER+SLCWID/2                                    229
         AG_TITLE = 'ESHM'                                                229
         AG_EXNAME = 'POSITION'                                           229
         AG_PARLIST = 'Z'                                                 229
         CALL AXPOSITION                                                  229
C    *                                                                    229
         ZSLICE = ZSLICE + SLCWID                                         229
C    Check IPRIN>=1                                                       230
         IF (IPRIN.GE.1) WRITE(LOUT,5230)ZSLICE                           230
5230     FORMAT(' SMD ends at: ',F10.5)                                   232
         SLCWID = 0                                                       233
         FSECT = 4                                                        233
         LSECT = 5                                                        234
C    *                                                                    235
         I_SECTION=FSECT                                                  235
            GO TO 5243                                                    235
5241        I_SECTION=I_SECTION+1                                         235
5243        IF(I_SECTION-(LSECT).GT.0) GO TO 5242                         235
C       use ESEC bank                                                     236
            BPATH_ESEC='ESEC*'                                            236
            IDNUM_ESEC(1)=0                                               236
            ESEC_ISECT=I_SECTION                                          236
            CALL AGDGETP('ECALGEO','*',-999,'ESEC',NESEC_ISECT,           236
     *      RE_ESEC_ISECT,BPATH_ESEC,IDNUM_ESEC,1,MAP_ESEC,NAMES_ESEC,    236
     *      BANK_ESEC,LEN_ESEC,LLEN_ESEC,LINK_ESEC,FLAG_ESEC)             236
            SLCWID = SLCWID + ESEC_CELL*ESEC_NLAYER                       237
         GO TO 5241                                                       238
5242     CONTINUE                                                         238
         SLCWID = EMCS_BCKFRNT - ZSLICE                                   239
C    Create ECVO :                                                        240
C    Call To Local Procedure ECVO                                         240
         ASSIGN 5250 TO BLOCK_ECVO                                        240
         GO TO  5180                                                      240
5250     CONTINUE                                                         240
C    *                                                                    240
C    Geant POSITION : ECVO ===> Z=ZSLICE-CENTER+SLCWID/2 <===             241
         CALL AGSRESET                                                    241
         AG_Z = ZSLICE-CENTER+SLCWID/2                                    241
         AG_TITLE = 'ECVO'                                                241
         AG_EXNAME = 'POSITION'                                           241
         AG_PARLIST = 'Z'                                                 241
         CALL AXPOSITION                                                  241
C    *                                                                    241
         ZSLICE = EMCS_BCKFRNT                                            241
C    Check IPRIN>=1                                                       242
         IF (IPRIN.GE.1) WRITE(LOUT,5260)SECTION,ZSLICE                   242
5260     FORMAT(' 2nd calorimeter ends, Back plate starts at: ',2F10.5)   244
         SLCWID = EMCS_BCKPLATE                                           245
C    Create ESSP :                                                        246
C    Call To Local Procedure ESSP                                         246
         ASSIGN 5280 TO BLOCK_ESSP                                        246
         GO TO  5270                                                      246
5280     CONTINUE                                                         246
C    *                                                                    246
C    Geant POSITION : ESSP ===> Z=ZSLICE-CENTER+SLCWID/2 <===             247
         CALL AGSRESET                                                    247
         AG_Z = ZSLICE-CENTER+SLCWID/2                                    247
         AG_TITLE = 'ESSP'                                                247
         AG_EXNAME = 'POSITION'                                           247
         AG_PARLIST = 'Z'                                                 247
         CALL AXPOSITION                                                  247
C    *                                                                    247
         ZSLICE = ZSLICE + SLCWID                                         247
C    Check IPRIN>=1                                                       248
         IF (IPRIN.GE.1) WRITE(LOUT,5290)ZSLICE                           248
5290     FORMAT(' BackPlate ends at: ',F10.5)                             250
         SLCWID = EMCS_ZEND - EMCS_ZORIG                                  251
C    Create ERCM :                                                        252
C    Call To Local Procedure ERCM                                         252
         ASSIGN 5310 TO BLOCK_ERCM                                        252
         GO TO  5300                                                      252
5310     CONTINUE                                                         252
C    *                                                                    252
C    *                                                                    253
         I_STR=1                                                          253
            GO TO 5323                                                    253
5321        I_STR=I_STR+1                                                 253
5323        IF(I_STR-(2).GT.0) GO TO 5322                                 253
C       *                                                                 254
            IS=1                                                          254
               GO TO 5333                                                 254
5331           IS=IS+1                                                    254
5333           IF(IS-(5).GT.0) GO TO 5332                                 254
               XX = EMCS_PHIMIN + IS*30                                   255
               YY = XX*DEGRAD                                             256
               XC = COS(YY)*EMCS_TIEROD(I_STR)                            257
               YC = SIN(YY)*EMCS_TIEROD(I_STR)                            258
C          Geant POSITION : ERCM ===> X=XC Y=YC Z=0 <===                  259
               CALL AGSRESET                                              259
               AG_X = XC                                                  259
               AG_Y = YC                                                  259
               AG_Z = 0                                                   259
               AG_TITLE = 'ERCM'                                          259
               AG_EXNAME = 'POSITION'                                     259
               AG_PARLIST = 'X_Y_Z'                                       259
               CALL AXPOSITION                                            259
C          *                                                              259
            GO TO 5331                                                    260
5332        CONTINUE                                                      260
         GO TO 5321                                                       261
5322     CONTINUE                                                         261
         RTH = ORGKEEP*TAN_UPP + DUP + 2.5/2                              262
         XC =(ENDKEEP - ORGKEEP)*TAN_UPP                                  263
         LEN = .5*(ENDKEEP + ORGKEEP)*TAN_UPP + DUP + 2.5/2               264
         YC = EMCS_ZEND - EMCS_ZORIG                                      265
         P = ATAN(XC/YC)/DEGRAD                                           266
C    Create EPSB :                                                        267
C    Call To Local Procedure EPSB                                         267
         ASSIGN 5350 TO BLOCK_EPSB                                        267
         GO TO  5340                                                      267
5350     CONTINUE                                                         267
C    *                                                                    267
C    *                                                                    268
         IS=1                                                             268
            GO TO 5363                                                    268
5361        IS=IS+1                                                       268
5363        IF(IS-(6).GT.0) GO TO 5362                                    268
            XX = -75 + (IS-1)*30                                          269
            YY = XX*DEGRAD                                                270
            XC = COS(YY)*LEN                                              271
            YC = SIN(YY)*LEN                                              272
C       Geant POSITION : EPSB ===> X=XC Y=YC ALPHAZ=XX <===               273
            CALL AGSRESET                                                 273
            AG_X = XC                                                     273
            AG_Y = YC                                                     273
            AG_ALPHAN=AG_ALPHAN+1                                         273
            AG_ALPHAK(AG_ALPHAN)=3                                        273
            AG_ALPHAL(AG_ALPHAN)=XX                                       273
            AG_TITLE = 'EPSB'                                             273
            AG_EXNAME = 'POSITION'                                        273
            AG_PARLIST = 'X_Y_ALPHAZ'                                     273
            CALL AXPOSITION                                               273
C       *                                                                 273
         GO TO 5361                                                       274
5362     CONTINUE                                                         274
5101  CALL AGSPOPB                                                        275
      GO TO BLOCK_EMSS, (5100 ,5110)                                      275
C End of Local Procedure                                                  275
C ----------------------------------------------------------------------  276
C ------------ ECVO                                                       276
C Begin Local Procedure ECVO                                              277
C ECVO IS ONE OF ENDCAP VOLUME WITH MEGATILES AND RADIATORS               277
5180  CONTINUE                                                            277
         AG_TITLE='ECVO'                                                  277
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
C    Geant ATTRIBUTE : ECVO ===> SEEN=1 COLO=3 <===                       278
         CALL AGSRESET                                                    279
         AG_SEEN = 1                                                      279
         AG_COLO = 3                                                      279
         AG_TITLE = 'ECVO'                                                279
         AG_EXNAME = 'ATTRIBUTE'                                          279
         AG_PARLIST = 'SEEN_COLO'                                         279
         CALL AXATTRIBUTE                                                 279
C    *                                                                    279
C    Geant SHAPE : CONS ===> DZ=SLCWID/2 RMN1=ZSLICE*TAN_LOW-DD, RMX1=ZS  279
C    LICE*TAN_UPP+DUP RMN2=(ZSLICE+SLCWID)*TAN_LOW-DD, RMX2=(ZSLICE+SLCW  279
C    ID)*TAN_UPP+DUP <===                                                 279
         CALL AGSRESET                                                    282
         AG_DZ = SLCWID/2                                                 282
         AG_RMN1 = ZSLICE*TAN_LOW-DD                                      282
         AG_RMX1 = ZSLICE*TAN_UPP+DUP                                     282
         AG_RMN2 = (ZSLICE+SLCWID)*TAN_LOW-DD                             282
         AG_RMX2 = (ZSLICE+SLCWID)*TAN_UPP+DUP                            282
         AG_TITLE = 'CONS'                                                282
         AG_EXNAME = 'SHAPE'                                              282
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            282
         CALL AXSHAPE                                                     282
C    *                                                                    282
C    *                                                                    282
         J_SECTION=1                                                      283
            GO TO 5373                                                    283
5371        J_SECTION=J_SECTION+1                                         283
5373        IF(J_SECTION-(6).GT.0) GO TO 5372                             283
C       Check EMCG_FILLMODE>1                                             284
            IF (EMCG_FILLMODE.GT.1) THEN                                  284
            FILLED=1                                                      285
            ELSEIF (J_SECTION.GT.1.AND.J_SECTION.LT.6) THEN               286
            FILLED=1                                                      287
            ELSE                                                          288
            FILLED=0                                                      289
            ENDIF                                                         290
            D3 = 75 - (J_SECTION-1)*30                                    291
C       Create EMOD :                                                     292
C       Call To Local Procedure EMOD                                      292
            ASSIGN 5390 TO BLOCK_EMOD                                     292
            GO TO  5380                                                   292
5390        CONTINUE                                                      292
C       *                                                                 292
C       Geant POSITION : EMOD ===> ALPHAZ=D3 NCOPY=J_SECTION <===         293
            CALL AGSRESET                                                 293
            AG_ALPHAN=AG_ALPHAN+1                                         293
            AG_ALPHAK(AG_ALPHAN)=3                                        293
            AG_ALPHAL(AG_ALPHAN)=D3                                       293
            AG_NCOPY = J_SECTION                                          293
            AG_TITLE = 'EMOD'                                             293
            AG_EXNAME = 'POSITION'                                        293
            AG_PARLIST = 'ALPHAZ_NCOPY'                                   293
            CALL AXPOSITION                                               293
C       *                                                                 293
         GO TO 5371                                                       294
5372     CONTINUE                                                         294
5181  CALL AGSPOPB                                                        295
      GO TO BLOCK_ECVO, (5180 ,5190,5250)                                 295
C End of Local Procedure                                                  295
C ----------------------------------------------------------------------  296
C ------------ ESHM                                                       296
C Begin Local Procedure ESHM                                              297
C ESHM IS THE SHOWER MAX SECTION                                          297
5210  CONTINUE                                                            297
         AG_TITLE='ESHM'                                                  297
         AG_MODULE=MODULE                                                 297
         AG_MARK='V00'                                                    297
         CALL AGSPUSH                                                     297
         AG_MODULE=MODULE                                                 297
C    *                                                                    297
C    Geant MATERIAL : AIR ===> inherited arguments used <===              298
         CALL AGSRESET                                                    298
         AG_TITLE = 'AIR'                                                 298
         AG_EXNAME = 'MATERIAL'                                           298
         AG_PARLIST = 'NONE'                                              298
         CALL AXMATERIAL                                                  298
C    *                                                                    298
C    Geant ATTRIBUTE : ESHM ===> SEEN=1 COLO=4 <===                       298
         CALL AGSRESET                                                    299
         AG_SEEN = 1                                                      299
         AG_COLO = 4                                                      299
         AG_TITLE = 'ESHM'                                                299
         AG_EXNAME = 'ATTRIBUTE'                                          299
         AG_PARLIST = 'SEEN_COLO'                                         299
         CALL AXATTRIBUTE                                                 299
C    *                                                                    299
C    Geant SHAPE : CONS ===> DZ=SLCWID/2 RMN1=ZSLICE*TAN_LOW-DD, RMX1=(Z  299
C    SLICE)*TAN_UPP+DUP RMN2=(ZSLICE+SLCWID)*TAN_LOW-DD, RMX2=(ZSLICE+SL  299
C    CWID)*TAN_UPP+DUP, PHI1=EMCS_PHIMIN PHI2=EMCS_PHIMAX <===            299
         CALL AGSRESET                                                    303
         AG_DZ = SLCWID/2                                                 303
         AG_RMN1 = ZSLICE*TAN_LOW-DD                                      303
         AG_RMX1 = (ZSLICE)*TAN_UPP+DUP                                   303
         AG_RMN2 = (ZSLICE+SLCWID)*TAN_LOW-DD                             303
         AG_RMX2 = (ZSLICE+SLCWID)*TAN_UPP+DUP                            303
         AG_PHI1 = EMCS_PHIMIN                                            303
         AG_PHI2 = EMCS_PHIMAX                                            303
         AG_TITLE = 'CONS'                                                303
         AG_EXNAME = 'SHAPE'                                              303
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2_PHI1_PHI2'                  303
         CALL AXSHAPE                                                     303
C    *                                                                    303
C    use EMXG bank                                                        303
         BPATH_EMXG='EMXG*'                                               304
         IDNUM_EMXG(1)=0                                                  304
         EMXG_VERSION=1                                                   304
         CALL AGDGETP('ECALGEO','*',-999,'EMXG',NEMXG_VERSION,            304
     *   RE_EMXG_VERSION,BPATH_EMXG,IDNUM_EMXG,1,MAP_EMXG,NAMES_EMXG,     304
     *   BANK_EMXG,LEN_EMXG,LLEN_EMXG,LINK_EMXG,FLAG_EMXG)                304
         MAXCNT = EMCS_SMDCENTR                                           305
C    Check IPRIN>=1                                                       305
         IF (IPRIN.GE.1) WRITE(LOUT,5400)ZSLICE,SECTION,CENTER            305
5400     FORMAT(' Z start for SMD,section: ',3F12.4)                      307
C    *                                                                    308
         J_SECTION=1                                                      308
            GO TO 5413                                                    308
5411        J_SECTION=J_SECTION+1                                         308
5413        IF(J_SECTION-(3).GT.0) GO TO 5412                             308
C       use EXSE bank                                                     309
            BPATH_EXSE='EXSE*'                                            309
            IDNUM_EXSE(1)=0                                               309
            EXSE_JSECT=J_SECTION                                          309
            CALL AGDGETP('ECALGEO','*',-999,'EXSE',NEXSE_JSECT,           309
     *      RE_EXSE_JSECT,BPATH_EXSE,IDNUM_EXSE,1,MAP_EXSE,NAMES_EXSE,    309
     *      BANK_EXSE,LEN_EXSE,LLEN_EXSE,LINK_EXSE,FLAG_EXSE)             309
            CURRENT = EXSE_ZSHIFT                                         310
            SECWID = EMXG_SAPEX + 2.*EMXG_F4                              311
            SECTION = MAXCNT + EXSE_ZSHIFT                                312
C       Check IPRIN>=1                                                    312
            IF (IPRIN.GE.1) WRITE(LOUT,5420)J_SECTION,CURRENT,SECTION,    312
     *      SECWID                                                        312
5420        FORMAT(' layer, Z, width : ',I3,3F12.4)                       314
            RBOT=SECTION*TAN_LOW                                          315
            RTOP=SECTION*TAN_UPP                                          316
C       Check IPRIN>=1                                                    316
            IF (IPRIN.GE.1) WRITE(LOUT,5430)J_SECTION,RBOT,RTOP           316
5430        FORMAT(' layer, rbot,rtop : ',I3,2F12.4)                      318
C       Create ESPL :                                                     319
C       Call To Local Procedure ESPL                                      319
            ASSIGN 5450 TO BLOCK_ESPL                                     319
            GO TO  5440                                                   319
5450        CONTINUE                                                      319
C       *                                                                 319
C       Geant POSITION : ESPL ===> Z=CURRENT <===                         320
            CALL AGSRESET                                                 320
            AG_Z = CURRENT                                                320
            AG_TITLE = 'ESPL'                                             320
            AG_EXNAME = 'POSITION'                                        320
            AG_PARLIST = 'Z'                                              320
            CALL AXPOSITION                                               320
C       *                                                                 320
         GO TO 5411                                                       321
5412     CONTINUE                                                         321
C    Create ERSM :                                                        322
C    Call To Local Procedure ERSM                                         322
         ASSIGN 5470 TO BLOCK_ERSM                                        322
         GO TO  5460                                                      322
5470     CONTINUE                                                         322
C    *                                                                    322
C    *                                                                    323
         I_STR=1                                                          323
            GO TO 5483                                                    323
5481        I_STR=I_STR+1                                                 323
5483        IF(I_STR-(2).GT.0) GO TO 5482                                 323
C       *                                                                 324
            IS=1                                                          324
               GO TO 5493                                                 324
5491           IS=IS+1                                                    324
5493           IF(IS-(5).GT.0) GO TO 5492                                 324
               XX = EMCS_PHIMIN + (IS)*30                                 325
               YY = XX*DEGRAD                                             326
               XC = COS(YY)*EMCS_TIEROD(I_STR)                            327
               YC = SIN(YY)*EMCS_TIEROD(I_STR)                            328
C          Geant POSITION : ERSM ===> X=XC Y=YC Z=0 <===                  329
               CALL AGSRESET                                              329
               AG_X = XC                                                  329
               AG_Y = YC                                                  329
               AG_Z = 0                                                   329
               AG_TITLE = 'ERSM'                                          329
               AG_EXNAME = 'POSITION'                                     329
               AG_PARLIST = 'X_Y_Z'                                       329
               CALL AXPOSITION                                            329
C          *                                                              329
            GO TO 5491                                                    330
5492        CONTINUE                                                      330
         GO TO 5481                                                       331
5482     CONTINUE                                                         331
5211  CALL AGSPOPB                                                        332
      GO TO BLOCK_ESHM, (5210 ,5220)                                      332
C End of Local Procedure                                                  332
C ----------------------------------------------------------------------  333
C ------------ ECGH                                                       333
C Begin Local Procedure ECGH                                              334
C ECGH IS AIR GAP BETWEEN ENDCAP HALF WHEELS                              334
5120  CONTINUE                                                            334
         AG_TITLE='ECGH'                                                  334
         AG_MODULE=MODULE                                                 334
         AG_MARK='V00'                                                    334
         CALL AGSPUSH                                                     334
         AG_MODULE=MODULE                                                 334
C    *                                                                    334
C    Geant MATERIAL : AIR ===> inherited arguments used <===              335
         CALL AGSRESET                                                    335
         AG_TITLE = 'AIR'                                                 335
         AG_EXNAME = 'MATERIAL'                                           335
         AG_PARLIST = 'NONE'                                              335
         CALL AXMATERIAL                                                  335
C    *                                                                    335
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           335
         CALL AGSRESET                                                    336
         AG_TITLE = 'STANDARD'                                            336
         AG_EXNAME = 'MEDIUM'                                             336
         AG_PARLIST = 'NONE'                                              336
         CALL AXMEDIUM                                                    336
C    *                                                                    336
C    Geant ATTRIBUTE : ECGH ===> SEEN=0 COLO=7 <===                       336
         CALL AGSRESET                                                    337
         AG_SEEN = 0                                                      337
         AG_COLO = 7                                                      337
         AG_TITLE = 'ECGH'                                                337
         AG_EXNAME = 'ATTRIBUTE'                                          337
         AG_PARLIST = 'SEEN_COLO'                                         337
         CALL AXATTRIBUTE                                                 337
C    *                                                                    337
C    Geant SHAPE : TRD1 ===> DX1=ORGKEEP*TAN_UPP+DUP DX2=ENDKEEP*TAN_UPP  337
C    +DUP, DY=(EMCS_GAPHALF+EMCS_COVER)/2, DZ=(EMCS_ZEND-EMCS_ZORIG)/2 <  337
C    ===                                                                  337
         CALL AGSRESET                                                    340
         AG_DX1 = ORGKEEP*TAN_UPP+DUP                                     340
         AG_DX2 = ENDKEEP*TAN_UPP+DUP                                     340
         AG_DY = (EMCS_GAPHALF+EMCS_COVER)/2                              340
         AG_DZ = (EMCS_ZEND-EMCS_ZORIG)/2                                 340
         AG_TITLE = 'TRD1'                                                340
         AG_EXNAME = 'SHAPE'                                              340
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     340
         CALL AXSHAPE                                                     340
C    *                                                                    340
         RTH = EMCS_GAPHALF + EMCS_COVER                                  340
         XX=CURR*TAN_LOW - D2                                             342
         XLEFT = SQRT(XX*XX - RTH*RTH)                                    343
         YY=CURR*TAN_UPP + DUP                                            344
         XRIGHT = SQRT(YY*YY - RTH*RTH)                                   345
         SECWID = YY - XX                                                 346
         XX=CURCL*TAN_LOW - D2                                            347
         YLEFT = SQRT(XX*XX - RTH*RTH)                                    348
         YY=CURCL*TAN_UPP + DUP                                           349
         YRIGHT = SQRT(YY*YY - RTH*RTH)                                   350
         SLCWID = YY - XX                                                 351
         XX=(XLEFT + XRIGHT)/2                                            352
         YY=(YLEFT + YRIGHT)/2                                            353
         XC = YY - XX                                                     354
         LEN =(XX + YY)/2                                                 355
         YC = CURCL - CURR                                                356
         P = ATAN(XC/YC)/DEGRAD                                           357
         RTH = - (EMCS_GAPHALF + EMCS_COVER)/2                            358
C    Create ECHC :                                                        359
C    Call To Local Procedure ECHC                                         359
         ASSIGN 5510 TO BLOCK_ECHC                                        359
         GO TO  5500                                                      359
5510     CONTINUE                                                         359
C    *                                                                    359
C    Geant POSITION : ECHC ===> X=LEN Y=RTH <===                          360
         CALL AGSRESET                                                    360
         AG_X = LEN                                                       360
         AG_Y = RTH                                                       360
         AG_TITLE = 'ECHC'                                                360
         AG_EXNAME = 'POSITION'                                           360
         AG_PARLIST = 'X_Y'                                               360
         CALL AXPOSITION                                                  360
C    *                                                                    360
C    Geant POSITION : ECHC ===> X=-LEN Y=RTH ALPHAZ=180 <===              360
         CALL AGSRESET                                                    361
         AG_X = -LEN                                                      361
         AG_Y = RTH                                                       361
         AG_ALPHAN=AG_ALPHAN+1                                            361
         AG_ALPHAK(AG_ALPHAN)=3                                           361
         AG_ALPHAL(AG_ALPHAN)=180                                         361
         AG_TITLE = 'ECHC'                                                361
         AG_EXNAME = 'POSITION'                                           361
         AG_PARLIST = 'X_Y_ALPHAZ'                                        361
         CALL AXPOSITION                                                  361
C    *                                                                    361
5121  CALL AGSPOPB                                                        362
      GO TO BLOCK_ECGH, (5120 ,5130)                                      362
C End of Local Procedure                                                  362
C ----------------------------------------------------------------------  363
C ------------ ECHC                                                       363
C Begin Local Procedure ECHC                                              364
C ECHC IS STEEL ENDCAP HALF COVER                                         364
5500  CONTINUE                                                            364
         AG_TITLE='ECHC'                                                  364
         AG_MODULE=MODULE                                                 364
         AG_MARK='V00'                                                    364
         CALL AGSPUSH                                                     364
         AG_MODULE=MODULE                                                 364
C    *                                                                    364
C    Geant ATTRIBUTE : ECHC ===> SEEN=1 COLO=1 <===                       365
         CALL AGSRESET                                                    365
         AG_SEEN = 1                                                      365
         AG_COLO = 1                                                      365
         AG_TITLE = 'ECHC'                                                365
         AG_EXNAME = 'ATTRIBUTE'                                          365
         AG_PARLIST = 'SEEN_COLO'                                         365
         CALL AXATTRIBUTE                                                 365
C    *                                                                    365
C    Geant MATERIAL : IRON ===> inherited arguments used <===             365
         CALL AGSRESET                                                    366
         AG_TITLE = 'IRON'                                                366
         AG_EXNAME = 'MATERIAL'                                           366
         AG_PARLIST = 'NONE'                                              366
         CALL AXMATERIAL                                                  366
C    *                                                                    366
C    Geant SHAPE : TRAP ===> DZ=(CURCL-CURR)/2 THET=P PHI=0, H1=EMCS_COV  366
C    ER/2 BL1=SECWID/2 TL1=SECWID/2 ALP1=0, H2=EMCS_COVER/2 BL2=SLCWID/2  366
C     TL2=SLCWID/2, ALP2=0 <===                                           366
         CALL AGSRESET                                                    370
         AG_DZ = (CURCL-CURR)/2                                           370
         AG_THET = P                                                      370
         AG_PHI = 0                                                       370
         AG_H1 = EMCS_COVER/2                                             370
         AG_BL1 = SECWID/2                                                370
         AG_TL1 = SECWID/2                                                370
         AG_ALP1 = 0                                                      370
         AG_H2 = EMCS_COVER/2                                             370
         AG_BL2 = SLCWID/2                                                370
         AG_TL2 = SLCWID/2                                                370
         AG_ALP2 = 0                                                      370
         AG_TITLE = 'TRAP'                                                370
         AG_EXNAME = 'SHAPE'                                              370
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       370
         CALL AXSHAPE                                                     370
C    *                                                                    370
5501  CALL AGSPOPB                                                        371
      GO TO BLOCK_ECHC, (5500 ,5510)                                      371
C End of Local Procedure                                                  371
C ----------------------------------------------------------------------  372
C ------------ ESSP                                                       372
C Begin Local Procedure ESSP                                              373
C ESSP IS STAINLESS STEEL BACK PLATE                                      373
5270  CONTINUE                                                            373
         AG_TITLE='ESSP'                                                  373
         AG_MODULE=MODULE                                                 373
         AG_MARK='V00'                                                    373
         CALL AGSPUSH                                                     373
         AG_MODULE=MODULE                                                 373
C    *                                                                    373
C    Geant MATERIAL : IRON ===> inherited arguments used <===             374
         CALL AGSRESET                                                    374
         AG_TITLE = 'IRON'                                                374
         AG_EXNAME = 'MATERIAL'                                           374
         AG_PARLIST = 'NONE'                                              374
         CALL AXMATERIAL                                                  374
C    *                                                                    374
C    Geant ATTRIBUTE : ESSP ===> SEEN=1 COLO=6 FILL=1 <===                374
         CALL AGSRESET                                                    375
         AG_SEEN = 1                                                      375
         AG_COLO = 6                                                      375
         AG_FILL = 1                                                      375
         AG_TITLE = 'ESSP'                                                375
         AG_EXNAME = 'ATTRIBUTE'                                          375
         AG_PARLIST = 'SEEN_COLO_FILL'                                    375
         CALL AXATTRIBUTE                                                 375
C    *                                                                    375
C    Geant SHAPE : CONS ===> DZ=EMCS_BCKPLATE/2 RMN1=ZSLICE*TAN_LOW-DD,   375
C    RMX1=ZSLICE*TAN_UPP+DUP RMN2=(ZSLICE+SLCWID)*TAN_LOW-DD, RMX2=(ZSLI  375
C    CE+SLCWID)*TAN_UPP+DUP, PHI1=EMCS_PHIMIN PHI2=EMCS_PHIMAX <===       375
         CALL AGSRESET                                                    379
         AG_DZ = EMCS_BCKPLATE/2                                          379
         AG_RMN1 = ZSLICE*TAN_LOW-DD                                      379
         AG_RMX1 = ZSLICE*TAN_UPP+DUP                                     379
         AG_RMN2 = (ZSLICE+SLCWID)*TAN_LOW-DD                             379
         AG_RMX2 = (ZSLICE+SLCWID)*TAN_UPP+DUP                            379
         AG_PHI1 = EMCS_PHIMIN                                            379
         AG_PHI2 = EMCS_PHIMAX                                            379
         AG_TITLE = 'CONS'                                                379
         AG_EXNAME = 'SHAPE'                                              379
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2_PHI1_PHI2'                  379
         CALL AXSHAPE                                                     379
C    *                                                                    379
5271  CALL AGSPOPB                                                        380
      GO TO BLOCK_ESSP, (5270 ,5280)                                      380
C End of Local Procedure                                                  380
C ----------------------------------------------------------------------  381
C ------------ EPSB                                                       381
C Begin Local Procedure EPSB                                              382
C EPSB IS PROJECTILE STAINLESS STEEL BAR                                  382
5340  CONTINUE                                                            382
         AG_TITLE='EPSB'                                                  382
         AG_MODULE=MODULE                                                 382
         AG_MARK='V00'                                                    382
         CALL AGSPUSH                                                     382
         AG_MODULE=MODULE                                                 382
C    *                                                                    382
C    Geant MATERIAL : IRON ===> inherited arguments used <===             383
         CALL AGSRESET                                                    383
         AG_TITLE = 'IRON'                                                383
         AG_EXNAME = 'MATERIAL'                                           383
         AG_PARLIST = 'NONE'                                              383
         CALL AXMATERIAL                                                  383
C    *                                                                    383
C    Geant ATTRIBUTE : EPSB ===> SEEN=1 COLO=6 FILL=1 <===                383
         CALL AGSRESET                                                    384
         AG_SEEN = 1                                                      384
         AG_COLO = 6                                                      384
         AG_FILL = 1                                                      384
         AG_TITLE = 'EPSB'                                                384
         AG_EXNAME = 'ATTRIBUTE'                                          384
         AG_PARLIST = 'SEEN_COLO_FILL'                                    384
         CALL AXATTRIBUTE                                                 384
C    *                                                                    384
C    Geant SHAPE : TRAP ===> DZ=(EMCS_ZEND-EMCS_ZORIG)/2 THET=P, PHI=0 H  384
C    1=2.0/2 BL1=2.5/2 TL1=2.5/2 ALP1=0, H2=2.0/2 BL2=2.5/2 TL2=2.5/2 AL  384
C    P2=0 <===                                                            384
         CALL AGSRESET                                                    387
         AG_DZ = (EMCS_ZEND-EMCS_ZORIG)/2                                 387
         AG_THET = P                                                      387
         AG_PHI = 0                                                       387
         AG_H1 = 2.0/2                                                    387
         AG_BL1 = 2.5/2                                                   387
         AG_TL1 = 2.5/2                                                   387
         AG_ALP1 = 0                                                      387
         AG_H2 = 2.0/2                                                    387
         AG_BL2 = 2.5/2                                                   387
         AG_TL2 = 2.5/2                                                   387
         AG_ALP2 = 0                                                      387
         AG_TITLE = 'TRAP'                                                387
         AG_EXNAME = 'SHAPE'                                              387
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       387
         CALL AXSHAPE                                                     387
C    *                                                                    387
5341  CALL AGSPOPB                                                        388
      GO TO BLOCK_EPSB, (5340 ,5350)                                      388
C End of Local Procedure                                                  388
C ----------------------------------------------------------------------  389
C ------------ ERCM                                                       389
C Begin Local Procedure ERCM                                              390
C ERCM IS STAINLESS STEEL TIE ROD IN CALORIMETER SECTIONS                 390
5300  CONTINUE                                                            390
         AG_TITLE='ERCM'                                                  390
         AG_MODULE=MODULE                                                 390
         AG_MARK='V00'                                                    390
         CALL AGSPUSH                                                     390
         AG_MODULE=MODULE                                                 390
C    *                                                                    390
C    Geant MATERIAL : IRON ===> inherited arguments used <===             391
         CALL AGSRESET                                                    391
         AG_TITLE = 'IRON'                                                391
         AG_EXNAME = 'MATERIAL'                                           391
         AG_PARLIST = 'NONE'                                              391
         CALL AXMATERIAL                                                  391
C    *                                                                    391
C    Geant ATTRIBUTE : ERSM ===> SEEN=1 COLO=6 FILL=1 <===                391
         CALL AGSRESET                                                    392
         AG_SEEN = 1                                                      392
         AG_COLO = 6                                                      392
         AG_FILL = 1                                                      392
         AG_TITLE = 'ERSM'                                                392
         AG_EXNAME = 'ATTRIBUTE'                                          392
         AG_PARLIST = 'SEEN_COLO_FILL'                                    392
         CALL AXATTRIBUTE                                                 392
C    *                                                                    392
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=1.0425 DZ=SLCWID/2 <===          392
         CALL AGSRESET                                                    393
         AG_RMIN = 0                                                      393
         AG_RMAX = 1.0425                                                 393
         AG_DZ = SLCWID/2                                                 393
         AG_TITLE = 'TUBE'                                                393
         AG_EXNAME = 'SHAPE'                                              393
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      393
         CALL AXSHAPE                                                     393
C    *                                                                    393
5301  CALL AGSPOPB                                                        394
      GO TO BLOCK_ERCM, (5300 ,5310)                                      394
C End of Local Procedure                                                  394
C ----------------------------------------------------------------------  395
C ------------ ERSM                                                       395
C Begin Local Procedure ERSM                                              396
C ERSM IS STAINLESS STEEL TIE ROD IN SHOWER MAX                           396
5460  CONTINUE                                                            396
         AG_TITLE='ERSM'                                                  396
         AG_MODULE=MODULE                                                 396
         AG_MARK='V00'                                                    396
         CALL AGSPUSH                                                     396
         AG_MODULE=MODULE                                                 396
C    *                                                                    396
C    Geant MATERIAL : IRON ===> inherited arguments used <===             397
         CALL AGSRESET                                                    397
         AG_TITLE = 'IRON'                                                397
         AG_EXNAME = 'MATERIAL'                                           397
         AG_PARLIST = 'NONE'                                              397
         CALL AXMATERIAL                                                  397
C    *                                                                    397
C    Geant ATTRIBUTE : ERSM ===> SEEN=1 COLO=6 FILL=1 <===                397
         CALL AGSRESET                                                    398
         AG_SEEN = 1                                                      398
         AG_COLO = 6                                                      398
         AG_FILL = 1                                                      398
         AG_TITLE = 'ERSM'                                                398
         AG_EXNAME = 'ATTRIBUTE'                                          398
         AG_PARLIST = 'SEEN_COLO_FILL'                                    398
         CALL AXATTRIBUTE                                                 398
C    *                                                                    398
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=1.0425 DZ=SLCWID/2 <===          398
         CALL AGSRESET                                                    399
         AG_RMIN = 0                                                      399
         AG_RMAX = 1.0425                                                 399
         AG_DZ = SLCWID/2                                                 399
         AG_TITLE = 'TUBE'                                                399
         AG_EXNAME = 'SHAPE'                                              399
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      399
         CALL AXSHAPE                                                     399
C    *                                                                    399
5461  CALL AGSPOPB                                                        400
      GO TO BLOCK_ERSM, (5460 ,5470)                                      400
C End of Local Procedure                                                  400
C ----------------------------------------------------------------------  401
C ------------ EMOD                                                       401
C Begin Local Procedure EMOD                                              402
C EMOD IS ONE MODULE OF THE EM ENDCAP                                     402
5380  CONTINUE                                                            402
         AG_TITLE='EMOD'                                                  402
         AG_MODULE=MODULE                                                 402
         AG_MARK='V00'                                                    402
         CALL AGSPUSH                                                     402
         AG_MODULE=MODULE                                                 402
C    *                                                                    402
C    Geant ATTRIBUTE : EMOD ===> SEEN=1 COLO=3 SERIAL=FILLED <===         403
         CALL AGSRESET                                                    403
         AG_SEEN = 1                                                      403
         AG_COLO = 3                                                      403
         AG_SERIAL = FILLED                                               403
         AG_TITLE = 'EMOD'                                                403
         AG_EXNAME = 'ATTRIBUTE'                                          403
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  403
         CALL AXATTRIBUTE                                                 403
C    *                                                                    403
C    Geant MATERIAL : AIR ===> inherited arguments used <===              403
         CALL AGSRESET                                                    404
         AG_TITLE = 'AIR'                                                 404
         AG_EXNAME = 'MATERIAL'                                           404
         AG_PARLIST = 'NONE'                                              404
         CALL AXMATERIAL                                                  404
C    *                                                                    404
C    Geant SHAPE : CONS ===> DZ=SLCWID/2 RMN1=ZSLICE*TAN_LOW-DD, RMX1=ZS  404
C    LICE*TAN_UPP+DUP RMN2=(ZSLICE+SLCWID)*TAN_LOW-DD, RMX2=(ZSLICE+SLCW  404
C    ID)*TAN_UPP+DUP, PHI1=EMCS_PHIMIN/EMCS_NSUPSEC PHI2=EMCS_PHIMAX/EMC  404
C    S_NSUPSEC <===                                                       404
         CALL AGSRESET                                                    408
         AG_DZ = SLCWID/2                                                 408
         AG_RMN1 = ZSLICE*TAN_LOW-DD                                      408
         AG_RMX1 = ZSLICE*TAN_UPP+DUP                                     408
         AG_RMN2 = (ZSLICE+SLCWID)*TAN_LOW-DD                             408
         AG_RMX2 = (ZSLICE+SLCWID)*TAN_UPP+DUP                            408
         AG_PHI1 = EMCS_PHIMIN/EMCS_NSUPSEC                               408
         AG_PHI2 = EMCS_PHIMAX/EMCS_NSUPSEC                               408
         AG_TITLE = 'CONS'                                                408
         AG_EXNAME = 'SHAPE'                                              408
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2_PHI1_PHI2'                  408
         CALL AXSHAPE                                                     408
C    *                                                                    408
         SECTION = ZSLICE                                                 408
         CURR = ZSLICE + SLCWID/2                                         410
C    *                                                                    411
         I_SECTION=FSECT                                                  411
            GO TO 5523                                                    411
5521        I_SECTION=I_SECTION+1                                         411
5523        IF(I_SECTION-(LSECT).GT.0) GO TO 5522                         411
C       use ESEC bank                                                     412
            BPATH_ESEC='ESEC*'                                            412
            IDNUM_ESEC(1)=0                                               412
            ESEC_ISECT=I_SECTION                                          412
            CALL AGDGETP('ECALGEO','*',-999,'ESEC',NESEC_ISECT,           412
     *      RE_ESEC_ISECT,BPATH_ESEC,IDNUM_ESEC,1,MAP_ESEC,NAMES_ESEC,    412
     *      BANK_ESEC,LEN_ESEC,LLEN_ESEC,LINK_ESEC,FLAG_ESEC)             412
            SECWID = ESEC_CELL*ESEC_NLAYER                                413
C       Check I_SECTION==3|I_SECTION==5                                   414
            IF (I_SECTION.EQ.3.OR.I_SECTION.EQ.5) THEN                    414
            SECWID = SECWID - RADIATOR                                    415
            ELSEIF (I_SECTION.EQ.4) THEN                                  416
            SECWID = SECWID - ESEC_CELL + RADIATOR                        417
            ENDIF                                                         418
C       Create ESEC :                                                     419
C       Call To Local Procedure ESEC                                      419
            ASSIGN 5540 TO BLOCK_ESEC                                     419
            GO TO  5530                                                   419
5540        CONTINUE                                                      419
C       *                                                                 419
C       Geant POSITION : ESEC ===> Z=SECTION-CURR+SECWID/2 <===           420
            CALL AGSRESET                                                 420
            AG_Z = SECTION-CURR+SECWID/2                                  420
            AG_TITLE = 'ESEC'                                             420
            AG_EXNAME = 'POSITION'                                        420
            AG_PARLIST = 'Z'                                              420
            CALL AXPOSITION                                               420
C       *                                                                 420
            SECTION = SECTION + SECWID                                    420
         GO TO 5521                                                       422
5522     CONTINUE                                                         422
5381  CALL AGSPOPB                                                        423
      GO TO BLOCK_EMOD, (5380 ,5390)                                      423
C End of Local Procedure                                                  423
C ----------------------------------------------------------------------  424
C ------------ ESEC                                                       424
C Begin Local Procedure ESEC                                              425
C ESEC IS A SINGLE EM SECTION                                             425
5530  CONTINUE                                                            425
         AG_TITLE='ESEC'                                                  425
         AG_MODULE=MODULE                                                 425
         AG_MARK='V00'                                                    425
         CALL AGSPUSH                                                     425
         AG_MODULE=MODULE                                                 425
C    *                                                                    425
C    Geant ATTRIBUTE : ESEC ===> SEEN=1 COLO=1 SERIAL=FILLED <===         426
         CALL AGSRESET                                                    426
         AG_SEEN = 1                                                      426
         AG_COLO = 1                                                      426
         AG_SERIAL = FILLED                                               426
         AG_TITLE = 'ESEC'                                                426
         AG_EXNAME = 'ATTRIBUTE'                                          426
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  426
         CALL AXATTRIBUTE                                                 426
C    *                                                                    426
C    Geant MATERIAL : AIR ===> inherited arguments used <===              426
         CALL AGSRESET                                                    427
         AG_TITLE = 'AIR'                                                 427
         AG_EXNAME = 'MATERIAL'                                           427
         AG_PARLIST = 'NONE'                                              427
         CALL AXMATERIAL                                                  427
C    *                                                                    427
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           427
         CALL AGSRESET                                                    428
         AG_TITLE = 'STANDARD'                                            428
         AG_EXNAME = 'MEDIUM'                                             428
         AG_PARLIST = 'NONE'                                              428
         CALL AXMEDIUM                                                    428
C    *                                                                    428
C    Geant SHAPE : CONS ===> DZ=SECWID/2 RMN1=(SECTION-DIFF)*TAN_LOW-DD,  428
C     RMX1=(SECTION-DIFF)*TAN_UPP+DUP, RMN2=(SECTION+SECWID-DIFF)*TAN_LO  428
C    W-DD RMX2=(SECTION+SECWID-DIFF)*TAN_UPP+DUP <===                     428
         CALL AGSRESET                                                    431
         AG_DZ = SECWID/2                                                 431
         AG_RMN1 = (SECTION-DIFF)*TAN_LOW-DD                              431
         AG_RMX1 = (SECTION-DIFF)*TAN_UPP+DUP                             431
         AG_RMN2 = (SECTION+SECWID-DIFF)*TAN_LOW-DD                       431
         AG_RMX2 = (SECTION+SECWID-DIFF)*TAN_UPP+DUP                      431
         AG_TITLE = 'CONS'                                                431
         AG_EXNAME = 'SHAPE'                                              431
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            431
         CALL AXSHAPE                                                     431
C    *                                                                    431
         LEN = - SECWID/2                                                 431
         CURRENT = SECTION                                                433
         MGT = ESEC_SCINT + EMCS_ALINCELL + EMCS_FRPLAST + EMCS_BKPLAST   434
         GAP = ESEC_CELL - RADIATOR - MGT                                 435
C    Check IPRIN>=2                                                       435
         IF (IPRIN.GE.2) WRITE(LOUT,5550)I_SECTION,SECTION                435
5550     FORMAT(' ESEC:I_section,section',I3,F12.4)                       437
C    *                                                                    438
         IS=1                                                             438
            GO TO 5563                                                    438
5561        IS=IS+1                                                       438
5563        IF(IS-(ESEC_NLAYER).GT.0) GO TO 5562                          438
            CELL = ESEC_CELL                                              439
            PLATE = RADIATOR                                              440
C       Check IS==NINT(ESEC_NLAYER)&(I_SECTION==3|I_SECTION==5)           441
            IF                                                            441
     *      (IS.EQ.NINT(ESEC_NLAYER).AND.(I_SECTION.EQ.3.OR.I_SECTION.    441
     *      EQ.5)) THEN                                                   441
            CELL = MGT + GAP                                              442
            PLATE=0                                                       443
            ELSEIF (I_SECTION.EQ.4.AND.IS.EQ.1) THEN                      444
            CELL = RADIATOR                                               445
            ENDIF                                                         446
C       Check IPRIN>=2                                                    446
            IF (IPRIN.GE.2) WRITE(LOUT,5570)I_SECTION,IS,LEN,CELL,        446
     *      CURRENT                                                       446
5570        FORMAT(' ESEC:I_section,is,len,cell,current ',2I3,3F12.4)     448
C       Check I_SECTION==4&IS==1                                          449
            IF (I_SECTION.EQ.4.AND.IS.EQ.1) THEN                          449
            CELL = RADIATOR + .14                                         450
C       Create ERAD :                                                     451
C       Call To Local Procedure ERAD                                      451
            ASSIGN 5590 TO BLOCK_ERAD                                     451
            GO TO  5580                                                   451
5590        CONTINUE                                                      451
C       *                                                                 451
C       Geant POSITION : ERAD ===> Z=LEN+(CELL)/2 <===                    452
            CALL AGSRESET                                                 452
            AG_Z = LEN+(CELL)/2                                           452
            AG_TITLE = 'ERAD'                                             452
            AG_EXNAME = 'POSITION'                                        452
            AG_PARLIST = 'Z'                                              452
            CALL AXPOSITION                                               452
C       *                                                                 452
            LEN = LEN + CELL                                              452
            CURRENT = CURRENT + CELL                                      454
            ELSE                                                          455
            CELL = MGT                                                    456
C       Check FILLED==1                                                   457
            IF (FILLED.EQ.1) THEN                                         457
C       Create EMGT :                                                     458
C       Call To Local Procedure EMGT                                      458
            ASSIGN 5610 TO BLOCK_EMGT                                     458
            GO TO  5600                                                   458
5610        CONTINUE                                                      458
C       *                                                                 458
C       Geant POSITION : EMGT ===> Z=LEN+(GAP+CELL)/2 <===                459
            CALL AGSRESET                                                 459
            AG_Z = LEN+(GAP+CELL)/2                                       459
            AG_TITLE = 'EMGT'                                             459
            AG_EXNAME = 'POSITION'                                        459
            AG_PARLIST = 'Z'                                              459
            CALL AXPOSITION                                               459
C       *                                                                 459
            XX = CURRENT + (GAP+CELL)/2                                   459
C       Check IPRIN>=2                                                    460
            IF (IPRIN.GE.2) WRITE(LOUT,5620)I_SECTION,IS,XX               460
5620        FORMAT(' MEGA I_section,is ',2I3,F10.4)                       462
            ENDIF                                                         463
            LEN = LEN + CELL + GAP                                        464
            CURRENT = CURRENT + CELL + GAP                                465
C       Check PLATE>0                                                     466
            IF (PLATE.GT.0) THEN                                          466
            CELL = RADIATOR                                               467
C       Create ERAD :                                                     468
C       Call To Local Procedure ERAD                                      468
            ASSIGN 5630 TO BLOCK_ERAD                                     468
            GO TO  5580                                                   468
5630        CONTINUE                                                      468
C       *                                                                 468
C       Geant POSITION : ERAD ===> Z=LEN+CELL/2 <===                      469
            CALL AGSRESET                                                 469
            AG_Z = LEN+CELL/2                                             469
            AG_TITLE = 'ERAD'                                             469
            AG_EXNAME = 'POSITION'                                        469
            AG_PARLIST = 'Z'                                              469
            CALL AXPOSITION                                               469
C       *                                                                 469
            LEN = LEN + CELL                                              469
            CURRENT = CURRENT + CELL                                      471
            ENDIF                                                         472
            ENDIF                                                         473
         GO TO 5561                                                       474
5562     CONTINUE                                                         474
5531  CALL AGSPOPB                                                        475
      GO TO BLOCK_ESEC, (5530 ,5540)                                      475
C End of Local Procedure                                                  475
C ----------------------------------------------------------------------  476
C ------------ EMGT                                                       476
C Begin Local Procedure EMGT                                              477
C EMGT IS A MEGATILE EM SECTION                                           477
5600  CONTINUE                                                            477
         AG_TITLE='EMGT'                                                  477
         AG_MODULE=MODULE                                                 477
         AG_MARK='V00'                                                    477
         CALL AGSPUSH                                                     477
         AG_MODULE=MODULE                                                 477
C    *                                                                    477
C    Geant ATTRIBUTE : EMGT ===> SEEN=1 COLO=1 <===                       478
         CALL AGSRESET                                                    478
         AG_SEEN = 1                                                      478
         AG_COLO = 1                                                      478
         AG_TITLE = 'EMGT'                                                478
         AG_EXNAME = 'ATTRIBUTE'                                          478
         AG_PARLIST = 'SEEN_COLO'                                         478
         CALL AXATTRIBUTE                                                 478
C    *                                                                    478
C    Geant MATERIAL : AIR ===> inherited arguments used <===              478
         CALL AGSRESET                                                    479
         AG_TITLE = 'AIR'                                                 479
         AG_EXNAME = 'MATERIAL'                                           479
         AG_PARLIST = 'NONE'                                              479
         CALL AXMATERIAL                                                  479
C    *                                                                    479
         MYKASE=2                                                         479
C    Check (I_SECTION==1|I_SECTION==2|I_SECTION==5)                       481
         IF ((I_SECTION.EQ.1.OR.I_SECTION.EQ.2.OR.I_SECTION.EQ.5)) THEN   481
         MYKASE=1                                                         482
         ENDIF                                                            483
C    Check MYKASE==1                                                      484
         IF (MYKASE.EQ.1) THEN                                            484
C    Geant MATERIAL : AIR_EMGT1 ===> ISVOL=0 <===                         485
         CALL AGSRESET                                                    485
         AG_ISVOL = 0                                                     485
         AG_TITLE = 'AIR_EMGT1'                                           485
         AG_EXNAME = 'MATERIAL'                                           485
         AG_PARLIST = 'ISVOL'                                             485
         CALL AXMATERIAL                                                  485
C    *                                                                    485
         ELSE                                                             485
C    Geant MATERIAL : AIR_EMGT2 ===> ISVOL=0 <===                         487
         CALL AGSRESET                                                    487
         AG_ISVOL = 0                                                     487
         AG_TITLE = 'AIR_EMGT2'                                           487
         AG_EXNAME = 'MATERIAL'                                           487
         AG_PARLIST = 'ISVOL'                                             487
         CALL AXMATERIAL                                                  487
C    *                                                                    487
         ENDIF                                                            487
C    Geant SHAPE : CONS ===> DZ=MGT/2 RMN1=(CURRENT-DIFF)*TAN_LOW-DD, RM  491
C    X1=(CURRENT-DIFF)*TAN_UPP+DUP RMN2=(CURRENT+MGT-DIFF)*TAN_LOW-DD, R  491
C    MX2=(CURRENT+MGT-DIFF)*TAN_UPP+DUP <===                              491
         CALL AGSRESET                                                    491
         AG_DZ = MGT/2                                                    491
         AG_RMN1 = (CURRENT-DIFF)*TAN_LOW-DD                              491
         AG_RMX1 = (CURRENT-DIFF)*TAN_UPP+DUP                             491
         AG_RMN2 = (CURRENT+MGT-DIFF)*TAN_LOW-DD                          491
         AG_RMX2 = (CURRENT+MGT-DIFF)*TAN_UPP+DUP                         491
         AG_TITLE = 'CONS'                                                491
         AG_EXNAME = 'SHAPE'                                              491
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            491
         CALL AXSHAPE                                                     491
C    *                                                                    491
C    Check MYKASE==1                                                      491
         IF (MYKASE.EQ.1) THEN                                            492
         CALL GSTPAR(AG_IMED, 'CUTGAM',0.00001)                           493
         CALL GSTPAR(AG_IMED, 'CUTELE',0.00001)                           494
         ELSE                                                             495
         CALL GSTPAR(AG_IMED, 'CUTGAM',0.00008)                           496
         CALL GSTPAR(AG_IMED, 'CUTELE',0.001)                             497
         CALL GSTPAR(AG_IMED, 'BCUTE',0.0001)                             498
         ENDIF                                                            499
C    *                                                                    500
         ISEC=1                                                           500
            GO TO 5643                                                    500
5641        ISEC=ISEC+1                                                   500
5643        IF(ISEC-(NINT(EMCS_NSLICES)).GT.0) GO TO 5642                 500
C       Create EPER :                                                     501
C       Call To Local Procedure EPER                                      501
            ASSIGN 5660 TO BLOCK_EPER                                     501
            GO TO  5650                                                   501
5660        CONTINUE                                                      501
C       *                                                                 501
C       Geant POSITION : EPER ===> ALPHAZ=(EMCS_NSLICES/2-ISEC+0.5)*DPHI  502
C        <===                                                             502
            CALL AGSRESET                                                 502
            AG_ALPHAN=AG_ALPHAN+1                                         502
            AG_ALPHAK(AG_ALPHAN)=3                                        502
            AG_ALPHAL(AG_ALPHAN)=(EMCS_NSLICES/2-ISEC+0.5)*DPHI           502
            AG_TITLE = 'EPER'                                             502
            AG_EXNAME = 'POSITION'                                        502
            AG_PARLIST = 'ALPHAZ'                                         502
            CALL AXPOSITION                                               502
C       *                                                                 502
         GO TO 5641                                                       503
5642     CONTINUE                                                         503
5601  CALL AGSPOPB                                                        504
      GO TO BLOCK_EMGT, (5600 ,5610)                                      504
C End of Local Procedure                                                  504
C ----------------------------------------------------------------------  505
C ------------ EPER                                                       505
C Begin Local Procedure EPER                                              506
C EPER IS A EM SUBSECTION PERIOD (SUPER LAYER)                            506
5650  CONTINUE                                                            506
         AG_TITLE='EPER'                                                  506
         AG_MODULE=MODULE                                                 506
         AG_MARK='V00'                                                    506
         CALL AGSPUSH                                                     506
         AG_MODULE=MODULE                                                 506
C    *                                                                    506
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       507
         CALL AGSRESET                                                    507
         AG_TITLE = 'POLYSTYREN'                                          507
         AG_EXNAME = 'MATERIAL'                                           507
         AG_PARLIST = 'NONE'                                              507
         CALL AXMATERIAL                                                  507
C    *                                                                    507
C    Geant ATTRIBUTE : EPER ===> SEEN=1 COLO=1 <===                       507
         CALL AGSRESET                                                    508
         AG_SEEN = 1                                                      508
         AG_COLO = 1                                                      508
         AG_TITLE = 'EPER'                                                508
         AG_EXNAME = 'ATTRIBUTE'                                          508
         AG_PARLIST = 'SEEN_COLO'                                         508
         CALL AXATTRIBUTE                                                 508
C    *                                                                    508
C    Geant SHAPE : CONS ===> DZ=MGT/2 RMN1=(CURRENT-DIFF)*TAN_LOW-DD, RM  508
C    X1=(CURRENT-DIFF)*TAN_UPP+DUP RMN2=(CURRENT+MGT-DIFF)*TAN_LOW-DD, R  508
C    MX2=(CURRENT+MGT-DIFF)*TAN_UPP+DUP, PHI1=EMCS_PHIMIN/EMCS_NSECTOR,   508
C    PHI2=+EMCS_PHIMAX/EMCS_NSECTOR <===                                  508
         CALL AGSRESET                                                    513
         AG_DZ = MGT/2                                                    513
         AG_RMN1 = (CURRENT-DIFF)*TAN_LOW-DD                              513
         AG_RMX1 = (CURRENT-DIFF)*TAN_UPP+DUP                             513
         AG_RMN2 = (CURRENT+MGT-DIFF)*TAN_LOW-DD                          513
         AG_RMX2 = (CURRENT+MGT-DIFF)*TAN_UPP+DUP                         513
         AG_PHI1 = EMCS_PHIMIN/EMCS_NSECTOR                               513
         AG_PHI2 = +EMCS_PHIMAX/EMCS_NSECTOR                              513
         AG_TITLE = 'CONS'                                                513
         AG_EXNAME = 'SHAPE'                                              513
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2_PHI1_PHI2'                  513
         CALL AXSHAPE                                                     513
C    *                                                                    513
         CURCL = CURRENT + MGT/2                                          513
C    *                                                                    515
         IE=1                                                             515
            GO TO 5673                                                    515
5671        IE=IE+1                                                       515
5673        IF(IE-(NINT(EETR_NETA)).GT.0) GO TO 5672                      515
            ETABOT = EETR_ETABIN(IE)                                      516
            ETATOP = EETR_ETABIN(IE+1)                                    517
            RBOT=(CURCL-DIFF)*TANF(ETABOT)                                518
C       Check PLATE>0                                                     519
            IF (PLATE.GT.0) THEN                                          519
            RTOP=MIN((CURCL-DIFF)*TANF(ETATOP),                           520
     *      ((CURRENT-DIFF)*TAN_UPP+DUP))                                 520
            ELSE                                                          521
            RTOP=MIN((CURCL-DIFF)*TANF(ETATOP),                           522
     *      ((CURRENT-DIFF)*TAN_UPP+DUP))                                 522
            ENDIF                                                         523
C       Skip Unless RBOT<RTOP                                             524
            IF (RBOT.GE.RTOP)GO TO 5671                                   524
            XX=TAN(PI*EMCS_PHIMAX/180.0/EMCS_NSECTOR)                     525
            YY=COS(PI*EMCS_PHIMAX/180.0/EMCS_NSECTOR)                     526
C       Create ETAR :                                                     527
C       Call To Local Procedure ETAR                                      527
            ASSIGN 5690 TO BLOCK_ETAR                                     527
            GO TO  5680                                                   527
5690        CONTINUE                                                      527
C       *                                                                 527
C       Geant POSITION : ETAR ===> X=(RBOT+RTOP)/2 ORT=YZX <===           528
            CALL AGSRESET                                                 528
            AG_X = (RBOT+RTOP)/2                                          528
            AG_ALPHAN=AG_ALPHAN+1                                         528
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              528
            AG_TITLE = 'ETAR'                                             528
            AG_EXNAME = 'POSITION'                                        528
            AG_PARLIST = 'X_ORT'                                          528
            CALL AXPOSITION                                               528
C       *                                                                 528
C       Check IPRIN>=2                                                    528
            IF (IPRIN.GE.2) WRITE(LOUT,5700)IE,ETATOP,ETABOT,RBOT,RTOP    528
5700        FORMAT(' EPER : ie,EtaTop,EtaBot,rbot,rtop ',I3,4F12.4)       530
         GO TO 5671                                                       531
5672     CONTINUE                                                         531
5651  CALL AGSPOPB                                                        532
      GO TO BLOCK_EPER, (5650 ,5660)                                      532
C End of Local Procedure                                                  532
C ----------------------------------------------------------------------  533
C ------------ ETAR                                                       533
C Begin Local Procedure ETAR                                              534
C ETAR IS ONE CELL OF SCINTILLATOR, FIBER AND PLASTIC                     534
5680  CONTINUE                                                            534
         AG_TITLE='ETAR'                                                  534
         AG_MODULE=MODULE                                                 534
         AG_MARK='V00'                                                    534
         CALL AGSPUSH                                                     534
         AG_MODULE=MODULE                                                 534
C    *                                                                    534
C    Geant ATTRIBUTE : ETAR ===> SEEN=1 COLO=4 <===                       535
         CALL AGSRESET                                                    535
         AG_SEEN = 1                                                      535
         AG_COLO = 4                                                      535
         AG_TITLE = 'ETAR'                                                535
         AG_EXNAME = 'ATTRIBUTE'                                          535
         AG_PARLIST = 'SEEN_COLO'                                         535
         CALL AXATTRIBUTE                                                 535
C    *                                                                    535
C    Geant SHAPE : TRD1 ===> DX1=RBOT*XX-EMCS_GAPCEL/YY DX2=RTOP*XX-EMCS  535
C    _GAPCEL/YY, DY=MGT/2 DZ=(RTOP-RBOT)/2 <===                           535
         CALL AGSRESET                                                    537
         AG_DX1 = RBOT*XX-EMCS_GAPCEL/YY                                  537
         AG_DX2 = RTOP*XX-EMCS_GAPCEL/YY                                  537
         AG_DY = MGT/2                                                    537
         AG_DZ = (RTOP-RBOT)/2                                            537
         AG_TITLE = 'TRD1'                                                537
         AG_EXNAME = 'SHAPE'                                              537
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     537
         CALL AXSHAPE                                                     537
C    *                                                                    537
C    Create EALP :                                                        538
C    Call To Local Procedure EALP                                         538
         ASSIGN 5720 TO BLOCK_EALP                                        538
         GO TO  5710                                                      538
5720     CONTINUE                                                         538
C    *                                                                    538
C    Geant POSITION : EALP ===> Y=(-MGT+EMCS_ALINCELL)/2 <===             539
         CALL AGSRESET                                                    539
         AG_Y = (-MGT+EMCS_ALINCELL)/2                                    539
         AG_TITLE = 'EALP'                                                539
         AG_EXNAME = 'POSITION'                                           539
         AG_PARLIST = 'Y'                                                 539
         CALL AXPOSITION                                                  539
C    *                                                                    539
         G10 = ESEC_SCINT                                                 539
C    Create ESCI :                                                        541
C    Call To Local Procedure ESCI                                         541
         ASSIGN 5740 TO BLOCK_ESCI                                        541
         GO TO  5730                                                      541
5740     CONTINUE                                                         541
C    *                                                                    541
C    Geant POSITION : ESCI ===> Y=(-MGT+G10)/2+EMCS_ALINCELL+EMCS_FRPLAS  542
C    T <===                                                               542
         CALL AGSRESET                                                    542
         AG_Y = (-MGT+G10)/2+EMCS_ALINCELL+EMCS_FRPLAST                   542
         AG_TITLE = 'ESCI'                                                542
         AG_EXNAME = 'POSITION'                                           542
         AG_PARLIST = 'Y'                                                 542
         CALL AXPOSITION                                                  542
C    *                                                                    542
5681  CALL AGSPOPB                                                        543
      GO TO BLOCK_ETAR, (5680 ,5690)                                      543
C End of Local Procedure                                                  543
C ----------------------------------------------------------------------  544
C ------------ ESCI                                                       544
C Begin Local Procedure ESCI                                              545
C ESCI IS THE ACTIVE SCINTILLATOR (POLYSTYREN) LAYER                      545
5730  CONTINUE                                                            545
         AG_TITLE='ESCI'                                                  545
         AG_MODULE=MODULE                                                 545
         AG_MARK='V00'                                                    545
         CALL AGSPUSH                                                     545
         AG_MODULE=MODULE                                                 545
C    *                                                                    545
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       546
         CALL AGSRESET                                                    546
         AG_TITLE = 'POLYSTYREN'                                          546
         AG_EXNAME = 'MATERIAL'                                           546
         AG_PARLIST = 'NONE'                                              546
         CALL AXMATERIAL                                                  546
C    *                                                                    546
C    Geant MATERIAL : CPOLYSTYREN ===> ISVOL=1 <===                       546
         CALL AGSRESET                                                    547
         AG_ISVOL = 1                                                     547
         AG_TITLE = 'CPOLYSTYREN'                                         547
         AG_EXNAME = 'MATERIAL'                                           547
         AG_PARLIST = 'ISVOL'                                             547
         CALL AXMATERIAL                                                  547
C    *                                                                    547
C    Geant ATTRIBUTE : ESCI ===> SEEN=1 COLO=7 FILL=0 <===                547
         CALL AGSRESET                                                    548
         AG_SEEN = 1                                                      548
         AG_COLO = 7                                                      548
         AG_FILL = 0                                                      548
         AG_TITLE = 'ESCI'                                                548
         AG_EXNAME = 'ATTRIBUTE'                                          548
         AG_PARLIST = 'SEEN_COLO_FILL'                                    548
         CALL AXATTRIBUTE                                                 548
C    *                                                                    548
C    Geant SHAPE : TRD1 ===> DY=ESEC_SCINT/2 DZ=(RTOP-RBOT)/2-EMCS_GAPCE  548
C    L <===                                                               548
         CALL AGSRESET                                                    549
         AG_DY = ESEC_SCINT/2                                             549
         AG_DZ = (RTOP-RBOT)/2-EMCS_GAPCEL                                549
         AG_TITLE = 'TRD1'                                                549
         AG_EXNAME = 'SHAPE'                                              549
         AG_PARLIST = 'DY_DZ'                                             549
         CALL AXSHAPE                                                     549
C    *                                                                    549
         CALL GSTPAR(AG_IMED, 'CUTGAM',0.00008)                           549
         CALL GSTPAR(AG_IMED, 'CUTELE',0.001)                             551
         CALL GSTPAR(AG_IMED, 'BCUTE',0.0001)                             552
         CALL GSTPAR(AG_IMED, 'CUTNEU',0.001)                             553
         CALL GSTPAR(AG_IMED, 'CUTHAD',0.001)                             554
         CALL GSTPAR(AG_IMED, 'CUTMUO',0.001)                             555
         CALL GSTPAR(AG_IMED, 'BIRK1', 1.)                                556
         CALL GSTPAR(AG_IMED, 'BIRK2', 0.013)                             557
         CALL GSTPAR(AG_IMED, 'BIRK3', 9.6E-6)                            558
C    Defining Hits: BIRK:0:(0,10)                                         559
         AG_TITLE = 'ESCI'                                                559
         AG_EXNAME = 'HITS'                                               559
         AG_TYPE=AG_MAGIC                                                 559
         CALL AGSDETV('ESCI')                                             559
C    *                                                                    559
         AG_PARLIST='BIRK'                                                559
         AG_TITLE = 'DETH'                                                559
         AG_NBITS = 0                                                     559
         AG_BIN=AG_MAGIC                                                  559
         AG_OPTION='*'                                                    559
         AG_FHMIN = 0                                                     559
         AG_FHMAX = 10                                                    559
         CALL AGSDETH (0)                                                 559
C    *                                                                    559
         CALL AGSDETU('ESCI')                                             559
C    *                                                                    559
5731  CALL AGSPOPB                                                        560
      GO TO BLOCK_ESCI, (5730 ,5740)                                      560
C End of Local Procedure                                                  560
C ----------------------------------------------------------------------  561
C ------------ ERAD                                                       561
C Begin Local Procedure ERAD                                              562
C ERAD IS RADIATOR                                                        562
5580  CONTINUE                                                            562
         AG_TITLE='ERAD'                                                  562
         AG_MODULE=MODULE                                                 562
         AG_MARK='V00'                                                    562
         CALL AGSPUSH                                                     562
         AG_MODULE=MODULE                                                 562
C    *                                                                    562
C    Geant MATERIAL : IRON ===> inherited arguments used <===             563
         CALL AGSRESET                                                    563
         AG_TITLE = 'IRON'                                                563
         AG_EXNAME = 'MATERIAL'                                           563
         AG_PARLIST = 'NONE'                                              563
         CALL AXMATERIAL                                                  563
C    *                                                                    563
C    Geant ATTRIBUTE : ERAD ===> SEEN=1 COLO=6 FILL=1 <===                563
         CALL AGSRESET                                                    564
         AG_SEEN = 1                                                      564
         AG_COLO = 6                                                      564
         AG_FILL = 1                                                      564
         AG_TITLE = 'ERAD'                                                564
         AG_EXNAME = 'ATTRIBUTE'                                          564
         AG_PARLIST = 'SEEN_COLO_FILL'                                    564
         CALL AXATTRIBUTE                                                 564
C    *                                                                    564
C    Geant SHAPE : CONS ===> DZ=RADIATOR/2 RMN1=(CURRENT)*TAN_LOW-DD, RM  564
C    X1=(CURRENT)*TAN_UPP+DUP RMN2=(CURRENT+CELL)*TAN_LOW-DD, RMX2=(CURR  564
C    ENT+RADIATOR)*TAN_UPP+DUP <===                                       564
         CALL AGSRESET                                                    567
         AG_DZ = RADIATOR/2                                               567
         AG_RMN1 = (CURRENT)*TAN_LOW-DD                                   567
         AG_RMX1 = (CURRENT)*TAN_UPP+DUP                                  567
         AG_RMN2 = (CURRENT+CELL)*TAN_LOW-DD                              567
         AG_RMX2 = (CURRENT+RADIATOR)*TAN_UPP+DUP                         567
         AG_TITLE = 'CONS'                                                567
         AG_EXNAME = 'SHAPE'                                              567
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            567
         CALL AXSHAPE                                                     567
C    *                                                                    567
C    Create ELED :                                                        568
C    Call To Local Procedure ELED                                         568
         ASSIGN 5760 TO BLOCK_ELED                                        568
         GO TO  5750                                                      568
5760     CONTINUE                                                         568
C    *                                                                    568
C    Geant POSITION : ELED ===> inherited arguments used <===             569
         CALL AGSRESET                                                    569
         AG_TITLE = 'ELED'                                                569
         AG_EXNAME = 'POSITION'                                           569
         AG_PARLIST = 'NONE'                                              569
         CALL AXPOSITION                                                  569
C    *                                                                    569
5581  CALL AGSPOPB                                                        570
      GO TO BLOCK_ERAD, (5580 ,5590,5630)                                 570
C End of Local Procedure                                                  570
C ----------------------------------------------------------------------  571
C ------------ ELED                                                       571
C Begin Local Procedure ELED                                              572
C ELED IS LEAD ABSORBER PLATE                                             572
5750  CONTINUE                                                            572
         AG_TITLE='ELED'                                                  572
         AG_MODULE=MODULE                                                 572
         AG_MARK='V00'                                                    572
         CALL AGSPUSH                                                     572
         AG_MODULE=MODULE                                                 572
C    *                                                                    572
C    Geant MATERIAL : LEAD ===> inherited arguments used <===             573
         CALL AGSRESET                                                    573
         AG_TITLE = 'LEAD'                                                573
         AG_EXNAME = 'MATERIAL'                                           573
         AG_PARLIST = 'NONE'                                              573
         CALL AXMATERIAL                                                  573
C    *                                                                    573
C    Geant MATERIAL : LEAD_ELED ===> ISVOL=0 <===                         573
         CALL AGSRESET                                                    574
         AG_ISVOL = 0                                                     574
         AG_TITLE = 'LEAD_ELED'                                           574
         AG_EXNAME = 'MATERIAL'                                           574
         AG_PARLIST = 'ISVOL'                                             574
         CALL AXMATERIAL                                                  574
C    *                                                                    574
C    Geant ATTRIBUTE : ELED ===> SEEN=1 COLO=4 FILL=1 <===                574
         CALL AGSRESET                                                    575
         AG_SEEN = 1                                                      575
         AG_COLO = 4                                                      575
         AG_FILL = 1                                                      575
         AG_TITLE = 'ELED'                                                575
         AG_EXNAME = 'ATTRIBUTE'                                          575
         AG_PARLIST = 'SEEN_COLO_FILL'                                    575
         CALL AXATTRIBUTE                                                 575
C    *                                                                    575
C    Geant SHAPE : TUBS ===> RMIN=(CURRENT)*TAN_LOW RMAX=(CURRENT+EMCS_P  575
C    BPLATE)*TAN_UPP, DZ=EMCS_PBPLATE/2 <===                              575
         CALL AGSRESET                                                    577
         AG_RMIN = (CURRENT)*TAN_LOW                                      577
         AG_RMAX = (CURRENT+EMCS_PBPLATE)*TAN_UPP                         577
         AG_DZ = EMCS_PBPLATE/2                                           577
         AG_TITLE = 'TUBS'                                                577
         AG_EXNAME = 'SHAPE'                                              577
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      577
         CALL AXSHAPE                                                     577
C    *                                                                    577
         CALL GSTPAR(AG_IMED, 'CUTGAM',0.00008)                           577
         CALL GSTPAR(AG_IMED, 'CUTELE',0.001)                             579
         CALL GSTPAR(AG_IMED, 'BCUTE',0.0001)                             580
         CALL GSTPAR(AG_IMED, 'CUTNEU',0.001)                             581
         CALL GSTPAR(AG_IMED, 'CUTHAD',0.001)                             582
         CALL GSTPAR(AG_IMED, 'CUTMUO',0.001)                             583
5751  CALL AGSPOPB                                                        584
      GO TO BLOCK_ELED, (5750 ,5760)                                      584
C End of Local Procedure                                                  584
C ----------------------------------------------------------------------  585
C ------------ EFLP                                                       585
C Begin Local Procedure EFLP                                              586
C EFLP IS FIRST ALUMINIUM PLATE                                           586
5150  CONTINUE                                                            586
         AG_TITLE='EFLP'                                                  586
         AG_MODULE=MODULE                                                 586
         AG_MARK='V00'                                                    586
         CALL AGSPUSH                                                     586
         AG_MODULE=MODULE                                                 586
C    *                                                                    586
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        587
         CALL AGSRESET                                                    587
         AG_TITLE = 'ALUMINIUM'                                           587
         AG_EXNAME = 'MATERIAL'                                           587
         AG_PARLIST = 'NONE'                                              587
         CALL AXMATERIAL                                                  587
C    *                                                                    587
C    Geant ATTRIBUTE : EFLP ===> SEEN=1 COLO=3 FILL=1 <===                587
         CALL AGSRESET                                                    588
         AG_SEEN = 1                                                      588
         AG_COLO = 3                                                      588
         AG_FILL = 1                                                      588
         AG_TITLE = 'EFLP'                                                588
         AG_EXNAME = 'ATTRIBUTE'                                          588
         AG_PARLIST = 'SEEN_COLO_FILL'                                    588
         CALL AXATTRIBUTE                                                 588
C    *                                                                    588
C    Geant SHAPE : CONS ===> DZ=EMCS_FRONT/2 RMN1=68.813 RMX1=(ZSLICE-DI  588
C    FF)*TAN_UPP+DUP, RMN2=68.813 RMX2=(ZSLICE+SLCWID-DIFF)*TAN_UPP+DUP,  588
C     PHI1=EMCS_PHIMIN, PHI2=EMCS_PHIMAX <===                             588
         CALL AGSRESET                                                    592
         AG_DZ = EMCS_FRONT/2                                             592
         AG_RMN1 = 68.813                                                 592
         AG_RMX1 = (ZSLICE-DIFF)*TAN_UPP+DUP                              592
         AG_RMN2 = 68.813                                                 592
         AG_RMX2 = (ZSLICE+SLCWID-DIFF)*TAN_UPP+DUP                       592
         AG_PHI1 = EMCS_PHIMIN                                            592
         AG_PHI2 = EMCS_PHIMAX                                            592
         AG_TITLE = 'CONS'                                                592
         AG_EXNAME = 'SHAPE'                                              592
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2_PHI1_PHI2'                  592
         CALL AXSHAPE                                                     592
C    *                                                                    592
5151  CALL AGSPOPB                                                        593
      GO TO BLOCK_EFLP, (5150 ,5160)                                      593
C End of Local Procedure                                                  593
C ----------------------------------------------------------------------  594
C ------------ EALP                                                       594
C Begin Local Procedure EALP                                              595
C EALP IS ALUMINIUM PLATE IN CALORIMETER CELL                             595
5710  CONTINUE                                                            595
         AG_TITLE='EALP'                                                  595
         AG_MODULE=MODULE                                                 595
         AG_MARK='V00'                                                    595
         CALL AGSPUSH                                                     595
         AG_MODULE=MODULE                                                 595
C    *                                                                    595
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        596
         CALL AGSRESET                                                    596
         AG_TITLE = 'ALUMINIUM'                                           596
         AG_EXNAME = 'MATERIAL'                                           596
         AG_PARLIST = 'NONE'                                              596
         CALL AXMATERIAL                                                  596
C    *                                                                    596
C    Geant MATERIAL : STRALUMINIUM ===> ISVOL=0 <===                      596
         CALL AGSRESET                                                    597
         AG_ISVOL = 0                                                     597
         AG_TITLE = 'STRALUMINIUM'                                        597
         AG_EXNAME = 'MATERIAL'                                           597
         AG_PARLIST = 'ISVOL'                                             597
         CALL AXMATERIAL                                                  597
C    *                                                                    597
C    Geant ATTRIBUTE : EALP ===> SEEN=1 COLO=1 <===                       597
         CALL AGSRESET                                                    598
         AG_SEEN = 1                                                      598
         AG_COLO = 1                                                      598
         AG_TITLE = 'EALP'                                                598
         AG_EXNAME = 'ATTRIBUTE'                                          598
         AG_PARLIST = 'SEEN_COLO'                                         598
         CALL AXATTRIBUTE                                                 598
C    *                                                                    598
C    Geant SHAPE : TRD1 ===> DY=EMCS_ALINCELL/2 DZ=(RTOP-RBOT)/2 <===     598
         CALL AGSRESET                                                    599
         AG_DY = EMCS_ALINCELL/2                                          599
         AG_DZ = (RTOP-RBOT)/2                                            599
         AG_TITLE = 'TRD1'                                                599
         AG_EXNAME = 'SHAPE'                                              599
         AG_PARLIST = 'DY_DZ'                                             599
         CALL AXSHAPE                                                     599
C    *                                                                    599
         CALL GSTPAR(AG_IMED, 'CUTGAM',0.00001)                           599
         CALL GSTPAR(AG_IMED, 'CUTELE',0.00001)                           601
         CALL GSTPAR(AG_IMED, 'LOSS', 1.)                                 602
         CALL GSTPAR(AG_IMED, 'STRA', 1.)                                 603
5711  CALL AGSPOPB                                                        604
      GO TO BLOCK_EALP, (5710 ,5720)                                      604
C End of Local Procedure                                                  604
C ----------------------------------------------------------------------  605
C ------------ ESPL                                                       605
C Begin Local Procedure ESPL                                              606
C ESPL IS ONE OF THE SHOWER MAX PLANES                                    606
5440  CONTINUE                                                            606
         AG_TITLE='ESPL'                                                  606
         AG_MODULE=MODULE                                                 606
         AG_MARK='V00'                                                    606
         CALL AGSPUSH                                                     606
         AG_MODULE=MODULE                                                 606
C    *                                                                    606
C    Geant MATERIAL : AIR ===> inherited arguments used <===              607
         CALL AGSRESET                                                    607
         AG_TITLE = 'AIR'                                                 607
         AG_EXNAME = 'MATERIAL'                                           607
         AG_PARLIST = 'NONE'                                              607
         CALL AXMATERIAL                                                  607
C    *                                                                    607
C    Geant ATTRIBUTE : ESPL ===> SEEN=1 COLO=3 <===                       607
         CALL AGSRESET                                                    608
         AG_SEEN = 1                                                      608
         AG_COLO = 3                                                      608
         AG_TITLE = 'ESPL'                                                608
         AG_EXNAME = 'ATTRIBUTE'                                          608
         AG_PARLIST = 'SEEN_COLO'                                         608
         CALL AXATTRIBUTE                                                 608
C    *                                                                    608
C    Geant SHAPE : TUBS ===> RMIN=SECTION*TAN_LOW-1.526 RMAX=(SECTION-SE  608
C    CWID/2)*TAN_UPP+DUP, PHI1=EMCS_PHIMIN, PHI2=EMCS_PHIMAX DZ=SECWID/2  608
C     <===                                                                608
         CALL AGSRESET                                                    611
         AG_RMIN = SECTION*TAN_LOW-1.526                                  611
         AG_RMAX = (SECTION-SECWID/2)*TAN_UPP+DUP                         611
         AG_PHI1 = EMCS_PHIMIN                                            611
         AG_PHI2 = EMCS_PHIMAX                                            611
         AG_DZ = SECWID/2                                                 611
         AG_TITLE = 'TUBS'                                                611
         AG_EXNAME = 'SHAPE'                                              611
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            611
         CALL AXSHAPE                                                     611
C    *                                                                    611
C    use EMXG bank                                                        611
         BPATH_EMXG='EMXG*'                                               612
         IDNUM_EMXG(1)=0                                                  612
         EMXG_VERSION=1                                                   612
         CALL AGDGETP('ECALGEO','*',-999,'EMXG',NEMXG_VERSION,            612
     *   RE_EMXG_VERSION,BPATH_EMXG,IDNUM_EMXG,1,MAP_EMXG,NAMES_EMXG,     612
     *   BANK_EMXG,LEN_EMXG,LLEN_EMXG,LINK_EMXG,FLAG_EMXG)                612
         MSECWD =(EMXG_SAPEX + EMXG_F4)/2                                 613
C    *                                                                    614
         ISEC=1                                                           614
            GO TO 5773                                                    614
5771        ISEC=ISEC+1                                                   614
5773        IF(ISEC-(6).GT.0) GO TO 5772                                  614
            CUT=1                                                         615
            D3 = 75 - (ISEC-1)*30                                         616
C       Check EXSE_SECTYPE(ISEC)==0|(EMCG_FILLMODE==1&(ISEC==6|ISEC==1))  617
            IF                                                            617
     *      (EXSE_SECTYPE(ISEC).EQ.0.OR.(EMCG_FILLMODE.EQ.1.AND.(ISEC.    617
     *      EQ.6.OR.ISEC.EQ.1))) THEN                                     617
            CUT = 0                                                       618
C       Create EXSG :                                                     619
C       Call To Local Procedure EXSG                                      619
            ASSIGN 5790 TO BLOCK_EXSG                                     619
            GO TO  5780                                                   619
5790        CONTINUE                                                      619
C       *                                                                 619
C       Geant POSITION : EXSG ===> ALPHAZ=D3 NCOPY=ISEC <===              620
            CALL AGSRESET                                                 620
            AG_ALPHAN=AG_ALPHAN+1                                         620
            AG_ALPHAK(AG_ALPHAN)=3                                        620
            AG_ALPHAL(AG_ALPHAN)=D3                                       620
            AG_NCOPY = ISEC                                               620
            AG_TITLE = 'EXSG'                                             620
            AG_EXNAME = 'POSITION'                                        620
            AG_PARLIST = 'ALPHAZ_NCOPY'                                   620
            CALL AXPOSITION                                               620
C       *                                                                 620
            ELSEIF (EXSE_SECTYPE(ISEC).EQ.1) THEN                         620
C       Create EXSG :                                                     622
C       Call To Local Procedure EXSG                                      622
            ASSIGN 5800 TO BLOCK_EXSG                                     622
            GO TO  5780                                                   622
5800        CONTINUE                                                      622
C       *                                                                 622
C       Geant POSITION : EXSG ===> ALPHAZ=D3 NCOPY=ISEC <===              623
            CALL AGSRESET                                                 623
            AG_ALPHAN=AG_ALPHAN+1                                         623
            AG_ALPHAK(AG_ALPHAN)=3                                        623
            AG_ALPHAL(AG_ALPHAN)=D3                                       623
            AG_NCOPY = ISEC                                               623
            AG_TITLE = 'EXSG'                                             623
            AG_EXNAME = 'POSITION'                                        623
            AG_PARLIST = 'ALPHAZ_NCOPY'                                   623
            CALL AXPOSITION                                               623
C       *                                                                 623
C       Create EXGT :                                                     624
C       Call To Local Procedure EXGT                                      624
            ASSIGN 5820 TO BLOCK_EXGT                                     624
            GO TO  5810                                                   624
5820        CONTINUE                                                      624
C       *                                                                 624
C       Geant POSITION : EXGT ===> Z=MSECWD ALPHAZ=D3 <===                625
            CALL AGSRESET                                                 625
            AG_Z = MSECWD                                                 625
            AG_ALPHAN=AG_ALPHAN+1                                         625
            AG_ALPHAK(AG_ALPHAN)=3                                        625
            AG_ALPHAL(AG_ALPHAN)=D3                                       625
            AG_TITLE = 'EXGT'                                             625
            AG_EXNAME = 'POSITION'                                        625
            AG_PARLIST = 'Z_ALPHAZ'                                       625
            CALL AXPOSITION                                               625
C       *                                                                 625
            ELSEIF (EXSE_SECTYPE(ISEC).EQ.2) THEN                         625
C       Create EXSG :                                                     627
C       Call To Local Procedure EXSG                                      627
            ASSIGN 5830 TO BLOCK_EXSG                                     627
            GO TO  5780                                                   627
5830        CONTINUE                                                      627
C       *                                                                 627
C       Geant POSITION : EXSG ===> ALPHAZ=D3 ORT=X-Y-Z NCOPY=ISEC <===    628
            CALL AGSRESET                                                 628
            AG_ALPHAN=AG_ALPHAN+1                                         628
            AG_ALPHAK(AG_ALPHAN)=3                                        628
            AG_ALPHAL(AG_ALPHAN)=D3                                       628
            AG_ALPHAN=AG_ALPHAN+1                                         628
            AG_ALPHAK(AG_ALPHAN)=IGOR('X-Y-Z')                            628
            AG_NCOPY = ISEC                                               628
            AG_TITLE = 'EXSG'                                             628
            AG_EXNAME = 'POSITION'                                        628
            AG_PARLIST = 'ALPHAZ_ORT_NCOPY'                               628
            CALL AXPOSITION                                               628
C       *                                                                 628
C       Create EXGT :                                                     629
C       Call To Local Procedure EXGT                                      629
            ASSIGN 5840 TO BLOCK_EXGT                                     629
            GO TO  5810                                                   629
5840        CONTINUE                                                      629
C       *                                                                 629
C       Geant POSITION : EXGT ===> Z=-MSECWD ALPHAZ=D3 <===               630
            CALL AGSRESET                                                 630
            AG_Z = -MSECWD                                                630
            AG_ALPHAN=AG_ALPHAN+1                                         630
            AG_ALPHAK(AG_ALPHAN)=3                                        630
            AG_ALPHAL(AG_ALPHAN)=D3                                       630
            AG_TITLE = 'EXGT'                                             630
            AG_EXNAME = 'POSITION'                                        630
            AG_PARLIST = 'Z_ALPHAZ'                                       630
            CALL AXPOSITION                                               630
C       *                                                                 630
            ELSEIF (EXSE_SECTYPE(ISEC).EQ.3) THEN                         630
            CUT=2                                                         632
C       Create EXSG :                                                     633
C       Call To Local Procedure EXSG                                      633
            ASSIGN 5850 TO BLOCK_EXSG                                     633
            GO TO  5780                                                   633
5850        CONTINUE                                                      633
C       *                                                                 633
C       Geant POSITION : EXSG ===> ALPHAZ=D3 NCOPY=ISEC <===              634
            CALL AGSRESET                                                 634
            AG_ALPHAN=AG_ALPHAN+1                                         634
            AG_ALPHAK(AG_ALPHAN)=3                                        634
            AG_ALPHAL(AG_ALPHAN)=D3                                       634
            AG_NCOPY = ISEC                                               634
            AG_TITLE = 'EXSG'                                             634
            AG_EXNAME = 'POSITION'                                        634
            AG_PARLIST = 'ALPHAZ_NCOPY'                                   634
            CALL AXPOSITION                                               634
C       *                                                                 634
C       Create EXGT :                                                     635
C       Call To Local Procedure EXGT                                      635
            ASSIGN 5860 TO BLOCK_EXGT                                     635
            GO TO  5810                                                   635
5860        CONTINUE                                                      635
C       *                                                                 635
C       Geant POSITION : EXGT ===> Z=MSECWD ALPHAZ=D3 <===                636
            CALL AGSRESET                                                 636
            AG_Z = MSECWD                                                 636
            AG_ALPHAN=AG_ALPHAN+1                                         636
            AG_ALPHAK(AG_ALPHAN)=3                                        636
            AG_ALPHAL(AG_ALPHAN)=D3                                       636
            AG_TITLE = 'EXGT'                                             636
            AG_EXNAME = 'POSITION'                                        636
            AG_PARLIST = 'Z_ALPHAZ'                                       636
            CALL AXPOSITION                                               636
C       *                                                                 636
            ELSEIF (EXSE_SECTYPE(ISEC).EQ.4) THEN                         636
            CUT=2                                                         638
C       Create EXSG :                                                     639
C       Call To Local Procedure EXSG                                      639
            ASSIGN 5870 TO BLOCK_EXSG                                     639
            GO TO  5780                                                   639
5870        CONTINUE                                                      639
C       *                                                                 639
C       Geant POSITION : EXSG ===> ALPHAZ=D3 ORT=X-Y-Z NCOPY=ISEC <===    640
            CALL AGSRESET                                                 640
            AG_ALPHAN=AG_ALPHAN+1                                         640
            AG_ALPHAK(AG_ALPHAN)=3                                        640
            AG_ALPHAL(AG_ALPHAN)=D3                                       640
            AG_ALPHAN=AG_ALPHAN+1                                         640
            AG_ALPHAK(AG_ALPHAN)=IGOR('X-Y-Z')                            640
            AG_NCOPY = ISEC                                               640
            AG_TITLE = 'EXSG'                                             640
            AG_EXNAME = 'POSITION'                                        640
            AG_PARLIST = 'ALPHAZ_ORT_NCOPY'                               640
            CALL AXPOSITION                                               640
C       *                                                                 640
C       Create EXGT :                                                     641
C       Call To Local Procedure EXGT                                      641
            ASSIGN 5880 TO BLOCK_EXGT                                     641
            GO TO  5810                                                   641
5880        CONTINUE                                                      641
C       *                                                                 641
C       Geant POSITION : EXGT ===> Z=-MSECWD ALPHAZ=D3 <===               642
            CALL AGSRESET                                                 642
            AG_Z = -MSECWD                                                642
            AG_ALPHAN=AG_ALPHAN+1                                         642
            AG_ALPHAK(AG_ALPHAN)=3                                        642
            AG_ALPHAL(AG_ALPHAN)=D3                                       642
            AG_TITLE = 'EXGT'                                             642
            AG_EXNAME = 'POSITION'                                        642
            AG_PARLIST = 'Z_ALPHAZ'                                       642
            CALL AXPOSITION                                               642
C       *                                                                 642
            ENDIF                                                         642
         GO TO 5771                                                       644
5772     CONTINUE                                                         644
5441  CALL AGSPOPB                                                        645
      GO TO BLOCK_ESPL, (5440 ,5450)                                      645
C End of Local Procedure                                                  645
C ----------------------------------------------------------------------  646
C ------------ EXSG                                                       646
C Begin Local Procedure EXSG                                              647
C EXSG IS THE SHOWER MAX GAP FOR SCINTILLATOR STRIPS                      647
5780  CONTINUE                                                            647
         AG_TITLE='EXSG'                                                  647
         AG_MODULE=MODULE                                                 647
         AG_MARK='V00'                                                    647
         CALL AGSPUSH                                                     647
         AG_MODULE=MODULE                                                 647
C    *                                                                    647
C    Geant ATTRIBUTE : EXSG ===> SEEN=1 COLO=7 SERIAL=CUT <===            648
         CALL AGSRESET                                                    648
         AG_SEEN = 1                                                      648
         AG_COLO = 7                                                      648
         AG_SERIAL = CUT                                                  648
         AG_TITLE = 'EXSG'                                                648
         AG_EXNAME = 'ATTRIBUTE'                                          648
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  648
         CALL AXATTRIBUTE                                                 648
C    *                                                                    648
C    Geant MATERIAL : AIR ===> inherited arguments used <===              648
         CALL AGSRESET                                                    649
         AG_TITLE = 'AIR'                                                 649
         AG_EXNAME = 'MATERIAL'                                           649
         AG_PARLIST = 'NONE'                                              649
         CALL AXMATERIAL                                                  649
C    *                                                                    649
C    Geant SHAPE : TUBS ===> RMIN=SECTION*TAN_LOW-1.526 RMAX=(SECTION-SE  649
C    CWID/2)*TAN_UPP+DUP, PHI1=EMCS_PHIMIN/EMCS_NSUPSEC, PHI2=EMCS_PHIMA  649
C    X/EMCS_NSUPSEC, DZ=SECWID/2 <===                                     649
         CALL AGSRESET                                                    653
         AG_RMIN = SECTION*TAN_LOW-1.526                                  653
         AG_RMAX = (SECTION-SECWID/2)*TAN_UPP+DUP                         653
         AG_PHI1 = EMCS_PHIMIN/EMCS_NSUPSEC                               653
         AG_PHI2 = EMCS_PHIMAX/EMCS_NSUPSEC                               653
         AG_DZ = SECWID/2                                                 653
         AG_TITLE = 'TUBS'                                                653
         AG_EXNAME = 'SHAPE'                                              653
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            653
         CALL AXSHAPE                                                     653
C    *                                                                    653
         RBOT = EMXG_RIN                                                  653
         RTOP = EMXG_ROUT                                                 655
C    Check CUT>0                                                          656
         IF (CUT.GT.0) THEN                                               656
C    Check CUT==1                                                         657
         IF (CUT.EQ.1) THEN                                               657
         RDEL = 3.938                                                     658
         NSTR = 288                                                       659
         ELSE                                                             660
         RDEL = -.475                                                     661
         NSTR = 285                                                       662
         ENDIF                                                            663
C    .53 --- TENTATAVILY                                                  664
         RTH = .53*RDEL                                                   664
         DDN = SQ3*1.713 + RDEL                                           665
         DDUP = .5*1.846 + 1.713                                          666
C    Check IPRIN>=2                                                       666
         IF (IPRIN.GE.2) WRITE(LOUT,5890)RBOT,RTOP,NSTR                   666
5890     FORMAT(' EXSG: Rbot,Rtop,Nstr',2F12.4,I5)                        668
         MGT = EMXG_SBASE + .01                                           669
C    *                                                                    670
         I_STR=1                                                          670
            GO TO 5903                                                    670
5901        I_STR=I_STR+1                                                 670
5903        IF(I_STR-(NSTR).GT.0) GO TO 5902                              670
            P = .5*(I_STR-1)*MGT + 41.3655                                671
C       Check P<=(.5*RBOT*SQ3+RTH)                                        672
            IF (P.LE.(.5*RBOT*SQ3+RTH)) THEN                              672
            DXY = 1.9375*SQ2                                              673
            XLEFT = .5*SQ2*P*(SQ3 + 1.) - DXY                             674
            YLEFT = .5*SQ2*P*(SQ3 - 1.) - DXY                             675
            YRIGHT = .5*SQ2*(SQRT( RBOT*RBOT - P*P) - P)                  676
            XRIGHT = SQ2*P + YRIGHT                                       677
            ELSEIF ((.5*RBOT*SQ3+RTH).LT.P.AND.P.LE.(.5*RTOP+1.5)) THEN   678
C       Check IPRIN>=2                                                    678
            IF (IPRIN.GE.2) WRITE(LOUT,5910)I_STR,P                       678
5910        FORMAT(' EXSG: 2 - -i_str,p:',I3,F12.4)                       680
            DXY = 1.9375*SQ2                                              681
            XLEFT = .5*SQ2*P*(SQ3 + 1.) - DXY                             682
            YLEFT = .5*SQ2*P*(SQ3 - 1.) - DXY                             683
            DXY = RDEL*SQ2/SQ3                                            684
            YRIGHT = .5*SQ2*P*(1.- 1./SQ3)                                685
            XRIGHT = SQ2*P - YRIGHT - DXY                                 686
            YRIGHT = -YRIGHT - DXY                                        687
            ELSEIF (P.GT.(.5*RTOP+1.5)) THEN                              688
C       Check IPRIN>=2                                                    688
            IF (IPRIN.GE.2) WRITE(LOUT,5920)I_STR,P                       688
5920        FORMAT(' EXSG: 3 - - i_str,p:',I3,F12.4)                      690
            YLEFT = (SQRT(RTOP*RTOP - P*P) - P)/SQ2                       691
            XLEFT = SQ2*P + YLEFT                                         692
            DXY = RDEL*SQ2/SQ3                                            693
            YRIGHT = .5*SQ2*P*(1.- 1./SQ3)                                694
            XRIGHT = SQ2*P - YRIGHT - DXY                                 695
            YRIGHT = -YRIGHT - DXY                                        696
            DXY = 0.                                                      697
C       Check (.5*SQ3*160.-DDN)<P&P<=(.5*SQ3*160.+DDUP)                   698
            IF ((.5*SQ3*160.-DDN).LT.P.AND.P.LE.(.5*SQ3*160.+DDUP))       698
     *      THEN                                                          698
C       Check IPRIN>=2                                                    698
            IF (IPRIN.GE.2) WRITE(LOUT,5930)I_STR,P                       698
5930        FORMAT(' EXSG: 4 - - i_str,p:',I3,F12.4)                      700
            XC = .5*(SQ3*160.+1.846)                                      701
            YC = XC - .5*SQ3*1.713                                        702
C       Check P>YC                                                        703
            IF (P.GT.YC) THEN                                             703
            DXY = .5*SQ2*(2/SQ3*RDEL + .5*SQ3*1.846 + SQRT(1.713*1.713    705
     *      - (P-XC)*(P-XC)))                                             705
            ELSE                                                          706
            DXY = SQ2/SQ3*(P - .5*SQ3* 160. + DDN)                        707
            ENDIF                                                         708
            ELSEIF ((.5*SQ3*195.-DDN).LT.P.AND.P.LE.(.5*SQ3*195.+DDUP))   709
     *      THEN                                                          709
C       Check IPRIN>=2                                                    709
            IF (IPRIN.GE.2) WRITE(LOUT,5940)I_STR,P                       709
5940        FORMAT(' EXSG: 5 - - i_str,p:',I3,F12.4)                      711
            XC = .5*(SQ3*195.+1.846)                                      712
            YC = XC - .5*SQ3*1.713                                        713
C       Check P>YC                                                        714
            IF (P.GT.YC) THEN                                             714
            DXY = .5*SQ2*(2/SQ3*RDEL + .5*SQ3*1.846 + SQRT(1.713*1.713    716
     *      - (P-XC)*(P-XC)))                                             716
            ELSE                                                          717
            DXY = SQ2/SQ3*(P - .5*SQ3*195. + DDN)                         718
            ENDIF                                                         719
            ENDIF                                                         720
            XRIGHT = XRIGHT + DXY                                         721
            YRIGHT = YRIGHT + DXY                                         722
            ENDIF                                                         723
            DXY = SECTION*TAN_UPP - RTOP                                  724
            XC = .5*(XRIGHT+XLEFT) + DXY                                  725
            YC = .5*(YRIGHT+YLEFT)                                        726
            XX = .5*SQ2*(XLEFT+YLEFT)                                     727
            YY = .5*SQ2*(XRIGHT+YRIGHT)                                   728
            LEN = XX-YY                                                   729
C       Check IPRIN>=2                                                    729
            IF (IPRIN.GE.2) WRITE(LOUT,5950)I_STR,P,YY,XX,LEN,XC,YC       729
5950        FORMAT(' EXSG: i_str,x,y1,y2,len,xc,yc:',I3,6F12.4)           731
C       Create EHMS :                                                     732
C       Call To Local Procedure EHMS                                      732
            ASSIGN 5970 TO BLOCK_EHMS                                     732
            GO TO  5960                                                   732
5970        CONTINUE                                                      732
C       *                                                                 732
C       Check MOD(I_STR,2)!=0                                             733
            IF (MOD(I_STR,2).NE.0) THEN                                   733
C       Geant POSITION : EHMS ===> X=XC Y=YC ALPHAZ=-45 <===              734
            CALL AGSRESET                                                 734
            AG_X = XC                                                     734
            AG_Y = YC                                                     734
            AG_ALPHAN=AG_ALPHAN+1                                         734
            AG_ALPHAK(AG_ALPHAN)=3                                        734
            AG_ALPHAL(AG_ALPHAN)=-45                                      734
            AG_TITLE = 'EHMS'                                             734
            AG_EXNAME = 'POSITION'                                        734
            AG_PARLIST = 'X_Y_ALPHAZ'                                     734
            CALL AXPOSITION                                               734
C       *                                                                 734
            ELSE                                                          734
C       Geant POSITION : EHMS ===> X=XC Y=YC ALPHAZ=-45 ORT=X-Y-Z <===    736
            CALL AGSRESET                                                 736
            AG_X = XC                                                     736
            AG_Y = YC                                                     736
            AG_ALPHAN=AG_ALPHAN+1                                         736
            AG_ALPHAK(AG_ALPHAN)=3                                        736
            AG_ALPHAL(AG_ALPHAN)=-45                                      736
            AG_ALPHAN=AG_ALPHAN+1                                         736
            AG_ALPHAK(AG_ALPHAN)=IGOR('X-Y-Z')                            736
            AG_TITLE = 'EHMS'                                             736
            AG_EXNAME = 'POSITION'                                        736
            AG_PARLIST = 'X_Y_ALPHAZ_ORT'                                 736
            CALL AXPOSITION                                               736
C       *                                                                 736
            ENDIF                                                         736
         GO TO 5901                                                       738
5902     CONTINUE                                                         738
         ENDIF                                                            739
5781  CALL AGSPOPB                                                        740
      GO TO BLOCK_EXSG, (5780 ,5790,5800,5830,5850,5870)                  740
C End of Local Procedure                                                  740
C ----------------------------------------------------------------------  741
C ------------ EHMS                                                       741
C Begin Local Procedure EHMS                                              742
C EHMS IS SHOWER MAX STRIP                                                742
5960  CONTINUE                                                            742
         AG_TITLE='EHMS'                                                  742
         AG_MODULE=MODULE                                                 742
         AG_MARK='V00'                                                    742
         CALL AGSPUSH                                                     742
         AG_MODULE=MODULE                                                 742
C    *                                                                    742
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       743
         CALL AGSRESET                                                    743
         AG_TITLE = 'POLYSTYREN'                                          743
         AG_EXNAME = 'MATERIAL'                                           743
         AG_PARLIST = 'NONE'                                              743
         CALL AXMATERIAL                                                  743
C    *                                                                    743
C    Geant MATERIAL : CPOLYSTYREN ===> ISVOL=1 <===                       743
         CALL AGSRESET                                                    744
         AG_ISVOL = 1                                                     744
         AG_TITLE = 'CPOLYSTYREN'                                         744
         AG_EXNAME = 'MATERIAL'                                           744
         AG_PARLIST = 'ISVOL'                                             744
         CALL AXMATERIAL                                                  744
C    *                                                                    744
C    Geant ATTRIBUTE : EHMS ===> SEEN=1 COLO=2 SERIAL=CUT <===            744
         CALL AGSRESET                                                    745
         AG_SEEN = 1                                                      745
         AG_COLO = 2                                                      745
         AG_SERIAL = CUT                                                  745
         AG_TITLE = 'EHMS'                                                745
         AG_EXNAME = 'ATTRIBUTE'                                          745
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  745
         CALL AXATTRIBUTE                                                 745
C    *                                                                    745
C    Geant SHAPE : TRD1 ===> DX1=0 DX2=EMXG_SBASE/2 DY=LEN/2, DZ=EMXG_SA  745
C    PEX/2 <===                                                           745
         CALL AGSRESET                                                    747
         AG_DX1 = 0                                                       747
         AG_DX2 = EMXG_SBASE/2                                            747
         AG_DY = LEN/2                                                    747
         AG_DZ = EMXG_SAPEX/2                                             747
         AG_TITLE = 'TRD1'                                                747
         AG_EXNAME = 'SHAPE'                                              747
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     747
         CALL AXSHAPE                                                     747
C    *                                                                    747
         CALL GSTPAR(AG_IMED, 'CUTGAM',0.00008)                           747
         CALL GSTPAR(AG_IMED, 'CUTELE',0.001)                             749
         CALL GSTPAR(AG_IMED, 'BCUTE',0.0001)                             750
         CALL GSTPAR(AG_IMED, 'BIRK1', 1.)                                751
         CALL GSTPAR(AG_IMED, 'BIRK2', 0.0130)                            752
         CALL GSTPAR(AG_IMED, 'BIRK3', 9.6E-6)                            753
C    Defining Hits: BIRK:0:(0,10)                                         754
         AG_TITLE = 'EHMS'                                                754
         AG_EXNAME = 'HITS'                                               754
         AG_TYPE=AG_MAGIC                                                 754
         CALL AGSDETV('EHMS')                                             754
C    *                                                                    754
         AG_PARLIST='BIRK'                                                754
         AG_TITLE = 'DETH'                                                754
         AG_NBITS = 0                                                     754
         AG_BIN=AG_MAGIC                                                  754
         AG_OPTION='*'                                                    754
         AG_FHMIN = 0                                                     754
         AG_FHMAX = 10                                                    754
         CALL AGSDETH (0)                                                 754
C    *                                                                    754
         CALL AGSDETU('EHMS')                                             754
C    *                                                                    754
5961  CALL AGSPOPB                                                        755
      GO TO BLOCK_EHMS, (5960 ,5970)                                      755
C End of Local Procedure                                                  755
C ----------------------------------------------------------------------  756
C ------------ EXGT                                                       756
C Begin Local Procedure EXGT                                              757
C EXGT IS THE G10 LAYER IN THE SHOWER MAX                                 757
5810  CONTINUE                                                            757
         AG_TITLE='EXGT'                                                  757
         AG_MODULE=MODULE                                                 757
         AG_MARK='V00'                                                    757
         CALL AGSPUSH                                                     757
         AG_MODULE=MODULE                                                 757
C    *                                                                    757
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===          758
         CALL AGSRESET                                                    758
         AG_A = 28.08                                                     758
         AG_Z = 14                                                        758
         AG_W = 0.6*1*28./60.                                             758
         AG_TITLE = 'SI'                                                  758
         AG_EXNAME = 'COMPONENT'                                          758
         AG_PARLIST = 'A_Z_W'                                             758
         CALL AXCOMPONENT                                                 758
C    *                                                                    758
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. <===               758
         CALL AGSRESET                                                    759
         AG_A = 16                                                        759
         AG_Z = 8                                                         759
         AG_W = 0.6*2*16./60.                                             759
         AG_TITLE = 'O'                                                   759
         AG_EXNAME = 'COMPONENT'                                          759
         AG_PARLIST = 'A_Z_W'                                             759
         CALL AXCOMPONENT                                                 759
C    *                                                                    759
C    Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===              759
         CALL AGSRESET                                                    760
         AG_A = 12                                                        760
         AG_Z = 6                                                         760
         AG_W = 0.4*8*12./174.                                            760
         AG_TITLE = 'C'                                                   760
         AG_EXNAME = 'COMPONENT'                                          760
         AG_PARLIST = 'A_Z_W'                                             760
         CALL AXCOMPONENT                                                 760
C    *                                                                    760
C    Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===               760
         CALL AGSRESET                                                    761
         AG_A = 1                                                         761
         AG_Z = 1                                                         761
         AG_W = 0.4*14*1./174.                                            761
         AG_TITLE = 'H'                                                   761
         AG_EXNAME = 'COMPONENT'                                          761
         AG_PARLIST = 'A_Z_W'                                             761
         CALL AXCOMPONENT                                                 761
C    *                                                                    761
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.4*4*16./174. <===              761
         CALL AGSRESET                                                    762
         AG_A = 16                                                        762
         AG_Z = 8                                                         762
         AG_W = 0.4*4*16./174.                                            762
         AG_TITLE = 'O'                                                   762
         AG_EXNAME = 'COMPONENT'                                          762
         AG_PARLIST = 'A_Z_W'                                             762
         CALL AXCOMPONENT                                                 762
C    *                                                                    762
C    Geant MIXTURE : G10 ===> DENS=1.7 <===                               762
         CALL AGSRESET                                                    763
         AG_DENS = 1.7                                                    763
         AG_TITLE = 'G10'                                                 763
         AG_EXNAME = 'MIXTURE'                                            763
         AG_PARLIST = 'DENS'                                              763
         CALL AXMIXTURE                                                   763
C    *                                                                    763
C    Geant ATTRIBUTE : EXGT ===> SEEN=1 COLO=7 <===                       763
         CALL AGSRESET                                                    764
         AG_SEEN = 1                                                      764
         AG_COLO = 7                                                      764
         AG_TITLE = 'EXGT'                                                764
         AG_EXNAME = 'ATTRIBUTE'                                          764
         AG_PARLIST = 'SEEN_COLO'                                         764
         CALL AXATTRIBUTE                                                 764
C    *                                                                    764
C    Geant SHAPE : TUBS ===> RMIN=(SECTION-DIFF)*TAN_LOW-1.526, RMAX=(SE  764
C    CTION+MSECWD-DIFF)*TAN_UPP PHI1=EMCS_PHIMIN/EMCS_NSUPSEC, PHI2=EMCS  764
C    _PHIMAX/EMCS_NSUPSEC, DZ=EMXG_F4/2 <===                              764
         CALL AGSRESET                                                    768
         AG_RMIN = (SECTION-DIFF)*TAN_LOW-1.526                           768
         AG_RMAX = (SECTION+MSECWD-DIFF)*TAN_UPP                          768
         AG_PHI1 = EMCS_PHIMIN/EMCS_NSUPSEC                               768
         AG_PHI2 = EMCS_PHIMAX/EMCS_NSUPSEC                               768
         AG_DZ = EMXG_F4/2                                                768
         AG_TITLE = 'TUBS'                                                768
         AG_EXNAME = 'SHAPE'                                              768
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            768
         CALL AXSHAPE                                                     768
C    *                                                                    768
         CALL GSTPAR(AG_IMED, 'CUTGAM',0.00001)                           768
         CALL GSTPAR(AG_IMED, 'CUTELE',0.00001)                           770
5811  CALL AGSPOPB                                                        771
      GO TO BLOCK_EXGT, (5810 ,5820,5840,5860,5880)                       771
C End of Local Procedure                                                  771
      WRITE(LOUT,5980)'===>>  [End AgML Module ECALGEO]  <<==='           771
5980  FORMAT(A32,/,/)                                                     771
C MODULE ECALGEO                                                          773
1     CONTINUE                                                            773
      END                                                                 773
