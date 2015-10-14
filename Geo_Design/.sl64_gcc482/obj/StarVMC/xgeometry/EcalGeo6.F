C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE ECALGEO6                                                 19
C ECALGEO6 IS THE EM ENDCAP CALORIMETER GEOMETRY                          19
      IMPLICIT NONE                                                       19
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       19
      DATA MODULE/'ECALGEO6'/, BANK_TITLE/'*'/                            19
      DATA MCOMMENT/'IS THE EM ENDCAP CALORIMETER GEOMETRY'/              19
      LOGICAL FIRST                                                       19
      DATA FIRST/.TRUE./                                                  19
      SAVE FIRST                                                          19
      INTEGER JUMPAD                                                      19
      EXTERNAL JUMPAD                                                     19
      INTEGER OK                                                          19
      PARAMETER (OK=0)                                                    19
      INTEGER AG_I,AG_J                                                   19
      SAVE AG_I,AG_J                                                      19
C CREATED 11/13/2009                                                      19
      DATA AG_I/0/,AG_J/0/                                                20
      CHARACTER*40 CREATED /'11/13/2009'/                                 20
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  20
C AUTHOR JASON WEBB, HAL SPINKA, ILYA SELYUZHENKOV, ALICE BRIDGEMAN, KEI  20
C TH KRUEGER, MICHAEL BETANCOURT                                          20
      CHARACTER*40 AUTHOR /'JASON WEBB, HAL SPINKA, ILYA SELYUZHENKOV, A  22
     *LICE BRIDGEMAN, KEITH KRUEGER, MICHAEL BETANCOURT'/                 22
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  22
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
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   24
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        24
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       24
C - GEANT description of logical units                                    24
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            25
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      25
      COMMON/GCMAIL/ CHMAIL                                               25
      CHARACTER*132 CHMAIL                                                25
      INTEGER BLOCK_EAGA                                                  25
      INTEGER BLOCK_EALP                                                  27
      INTEGER BLOCK_ECAL                                                  27
      INTEGER BLOCK_ECHC                                                  27
      INTEGER BLOCK_ECVO                                                  27
      INTEGER BLOCK_ECGH                                                  27
      INTEGER BLOCK_EFLP                                                  27
      INTEGER BLOCK_EHMS                                                  27
      INTEGER BLOCK_ELED                                                  27
      INTEGER BLOCK_EMGT                                                  27
      INTEGER BLOCK_EMOD                                                  27
      INTEGER BLOCK_EPER                                                  27
      INTEGER BLOCK_EPSB                                                  27
      INTEGER BLOCK_ERAD                                                  27
      INTEGER BLOCK_ERCM                                                  27
      INTEGER BLOCK_ERSM                                                  27
      INTEGER BLOCK_ESHM                                                  27
      INTEGER BLOCK_ESEC                                                  27
      INTEGER BLOCK_ESCI                                                  27
      INTEGER BLOCK_ESPL                                                  27
      INTEGER BLOCK_ESSP                                                  27
      INTEGER BLOCK_EMSS                                                  27
      INTEGER BLOCK_ETAR                                                  27
      INTEGER BLOCK_EXSG                                                  27
      INTEGER BLOCK_EXPS                                                  27
      INTEGER BLOCK_EFLS                                                  27
      INTEGER BLOCK_EBLS                                                  27
C -                                                                       28
      REAL EMCG_VERSION                                                   28
      EQUIVALENCE (BANK_EMCG(3),EMCG_VERSION,RE_EMCG_VERSION),            28
     *(NAMES_EMCG(1),NEMCG_VERSION)                                       28
      EQUIVALENCE (COMMEN_EMCG(1),CEMCG_VERSION),(MAP_EMCG(1,1),          28
     *MEMCG_VERSION)                                                      28
      CHARACTER CEMCG_VERSION*40/' '/                                     28
      CHARACTER NEMCG_VERSION*12/'R.VERSION'/                             28
      INTEGER MEMCG_VERSION(2)/1,1/                                       28
      REAL RE_EMCG_VERSION                                                28
C -                                                                       28
      INTEGER EMCG_ONOFF                                                  28
      EQUIVALENCE (BANK_EMCG(4),EMCG_ONOFF,RE_EMCG_ONOFF),                28
     *(NAMES_EMCG(2),NEMCG_ONOFF)                                         28
      EQUIVALENCE (COMMEN_EMCG(2),CEMCG_ONOFF),(MAP_EMCG(1,2),            28
     *MEMCG_ONOFF)                                                        28
      CHARACTER CEMCG_ONOFF*40/' '/                                       28
      CHARACTER NEMCG_ONOFF*12/'I.ONOFF'/                                 28
      INTEGER MEMCG_ONOFF(2)/1,1/                                         28
      REAL RE_EMCG_ONOFF                                                  28
C -                                                                       28
      INTEGER EMCG_FILLMODE                                               28
      EQUIVALENCE (BANK_EMCG(5),EMCG_FILLMODE,RE_EMCG_FILLMODE),          28
     *(NAMES_EMCG(3),NEMCG_FILLMODE)                                      28
      EQUIVALENCE (COMMEN_EMCG(3),CEMCG_FILLMODE),(MAP_EMCG(1,3),         28
     *MEMCG_FILLMODE)                                                     28
      CHARACTER CEMCG_FILLMODE*40/' '/                                    28
      CHARACTER NEMCG_FILLMODE*12/'I.FILLMODE'/                           28
      INTEGER MEMCG_FILLMODE(2)/1,1/                                      28
      REAL RE_EMCG_FILLMODE                                               28
C -                                                                       28
      INTEGER LEN_EMCG,LLEN_EMCG,LINK_EMCG                                28
      PARAMETER (LEN_EMCG=5)                                              28
      PARAMETER (LLEN_EMCG=3)                                             28
      INTEGER FLAG_EMCG/0/,MAP_EMCG(2,LLEN_EMCG),IDNUM_EMCG(10)           28
      CHARACTER NAMES_EMCG(LLEN_EMCG)*12,COMMEN_EMCG(LLEN_EMCG)*40        28
      DATA IDNUM_EMCG/10*0/                                               28
      REAL BANK_EMCG(LEN_EMCG)                                            28
      CHARACTER*60 BPATH_EMCG/' '/                                        28
      SAVE FLAG_EMCG,MAP_EMCG,IDNUM_EMCG,NAMES_EMCG,COMMEN_EMCG,          28
     *BPATH_EMCG                                                          28
      DATA LINK_EMCG/0/,BANK_EMCG/LEN_EMCG*0/                             28
C *                                                                       28
C -                                                                       32
      REAL EMCS_VERSION                                                   32
      EQUIVALENCE (BANK_EMCS(3),EMCS_VERSION,RE_EMCS_VERSION),            32
     *(NAMES_EMCS(1),NEMCS_VERSION)                                       32
      EQUIVALENCE (COMMEN_EMCS(1),CEMCS_VERSION),(MAP_EMCS(1,1),          32
     *MEMCS_VERSION)                                                      32
      CHARACTER CEMCS_VERSION*40/' '/                                     32
      CHARACTER NEMCS_VERSION*12/'R.VERSION'/                             32
      INTEGER MEMCS_VERSION(2)/1,1/                                       32
      REAL RE_EMCS_VERSION                                                32
C -                                                                       32
      REAL EMCS_TYPE                                                      32
      EQUIVALENCE (BANK_EMCS(4),EMCS_TYPE,RE_EMCS_TYPE),(NAMES_EMCS(2),   32
     *NEMCS_TYPE)                                                         32
      EQUIVALENCE (COMMEN_EMCS(2),CEMCS_TYPE),(MAP_EMCS(1,2),             32
     *MEMCS_TYPE)                                                         32
      CHARACTER CEMCS_TYPE*40/' '/                                        32
      CHARACTER NEMCS_TYPE*12/'R.TYPE'/                                   32
      INTEGER MEMCS_TYPE(2)/1,1/                                          32
      REAL RE_EMCS_TYPE                                                   32
C -                                                                       32
      REAL EMCS_ZORG                                                      32
      EQUIVALENCE (BANK_EMCS(5),EMCS_ZORG,RE_EMCS_ZORG),(NAMES_EMCS(3),   32
     *NEMCS_ZORG)                                                         32
      EQUIVALENCE (COMMEN_EMCS(3),CEMCS_ZORG),(MAP_EMCS(1,3),             32
     *MEMCS_ZORG)                                                         32
      CHARACTER CEMCS_ZORG*40/' '/                                        32
      CHARACTER NEMCS_ZORG*12/'R.ZORG'/                                   32
      INTEGER MEMCS_ZORG(2)/1,1/                                          32
      REAL RE_EMCS_ZORG                                                   32
C -                                                                       32
      REAL EMCS_ZEND                                                      32
      EQUIVALENCE (BANK_EMCS(6),EMCS_ZEND,RE_EMCS_ZEND),(NAMES_EMCS(4),   32
     *NEMCS_ZEND)                                                         32
      EQUIVALENCE (COMMEN_EMCS(4),CEMCS_ZEND),(MAP_EMCS(1,4),             32
     *MEMCS_ZEND)                                                         32
      CHARACTER CEMCS_ZEND*40/' '/                                        32
      CHARACTER NEMCS_ZEND*12/'R.ZEND'/                                   32
      INTEGER MEMCS_ZEND(2)/1,1/                                          32
      REAL RE_EMCS_ZEND                                                   32
C -                                                                       32
      REAL EMCS_ETAMIN                                                    32
      EQUIVALENCE (BANK_EMCS(7),EMCS_ETAMIN,RE_EMCS_ETAMIN),              32
     *(NAMES_EMCS(5),NEMCS_ETAMIN)                                        32
      EQUIVALENCE (COMMEN_EMCS(5),CEMCS_ETAMIN),(MAP_EMCS(1,5),           32
     *MEMCS_ETAMIN)                                                       32
      CHARACTER CEMCS_ETAMIN*40/' '/                                      32
      CHARACTER NEMCS_ETAMIN*12/'R.ETAMIN'/                               32
      INTEGER MEMCS_ETAMIN(2)/1,1/                                        32
      REAL RE_EMCS_ETAMIN                                                 32
C -                                                                       32
      REAL EMCS_ETAMAX                                                    32
      EQUIVALENCE (BANK_EMCS(8),EMCS_ETAMAX,RE_EMCS_ETAMAX),              32
     *(NAMES_EMCS(6),NEMCS_ETAMAX)                                        32
      EQUIVALENCE (COMMEN_EMCS(6),CEMCS_ETAMAX),(MAP_EMCS(1,6),           32
     *MEMCS_ETAMAX)                                                       32
      CHARACTER CEMCS_ETAMAX*40/' '/                                      32
      CHARACTER NEMCS_ETAMAX*12/'R.ETAMAX'/                               32
      INTEGER MEMCS_ETAMAX(2)/1,1/                                        32
      REAL RE_EMCS_ETAMAX                                                 32
C -                                                                       32
      REAL EMCS_PHIMIN                                                    32
      EQUIVALENCE (BANK_EMCS(9),EMCS_PHIMIN,RE_EMCS_PHIMIN),              32
     *(NAMES_EMCS(7),NEMCS_PHIMIN)                                        32
      EQUIVALENCE (COMMEN_EMCS(7),CEMCS_PHIMIN),(MAP_EMCS(1,7),           32
     *MEMCS_PHIMIN)                                                       32
      CHARACTER CEMCS_PHIMIN*40/' '/                                      32
      CHARACTER NEMCS_PHIMIN*12/'R.PHIMIN'/                               32
      INTEGER MEMCS_PHIMIN(2)/1,1/                                        32
      REAL RE_EMCS_PHIMIN                                                 32
C -                                                                       32
      REAL EMCS_PHIMAX                                                    32
      EQUIVALENCE (BANK_EMCS(10),EMCS_PHIMAX,RE_EMCS_PHIMAX),             32
     *(NAMES_EMCS(8),NEMCS_PHIMAX)                                        32
      EQUIVALENCE (COMMEN_EMCS(8),CEMCS_PHIMAX),(MAP_EMCS(1,8),           32
     *MEMCS_PHIMAX)                                                       32
      CHARACTER CEMCS_PHIMAX*40/' '/                                      32
      CHARACTER NEMCS_PHIMAX*12/'R.PHIMAX'/                               32
      INTEGER MEMCS_PHIMAX(2)/1,1/                                        32
      REAL RE_EMCS_PHIMAX                                                 32
C -                                                                       32
      REAL EMCS_OFFSET                                                    32
      EQUIVALENCE (BANK_EMCS(11),EMCS_OFFSET,RE_EMCS_OFFSET),             32
     *(NAMES_EMCS(9),NEMCS_OFFSET)                                        32
      EQUIVALENCE (COMMEN_EMCS(9),CEMCS_OFFSET),(MAP_EMCS(1,9),           32
     *MEMCS_OFFSET)                                                       32
      CHARACTER CEMCS_OFFSET*40/' '/                                      32
      CHARACTER NEMCS_OFFSET*12/'R.OFFSET'/                               32
      INTEGER MEMCS_OFFSET(2)/1,1/                                        32
      REAL RE_EMCS_OFFSET                                                 32
C -                                                                       32
      REAL EMCS_NSUPSEC                                                   32
      EQUIVALENCE (BANK_EMCS(12),EMCS_NSUPSEC,RE_EMCS_NSUPSEC),           32
     *(NAMES_EMCS(10),NEMCS_NSUPSEC)                                      32
      EQUIVALENCE (COMMEN_EMCS(10),CEMCS_NSUPSEC),(MAP_EMCS(1,10),        32
     *MEMCS_NSUPSEC)                                                      32
      CHARACTER CEMCS_NSUPSEC*40/' '/                                     32
      CHARACTER NEMCS_NSUPSEC*12/'R.NSUPSEC'/                             32
      INTEGER MEMCS_NSUPSEC(2)/1,1/                                       32
      REAL RE_EMCS_NSUPSEC                                                32
C -                                                                       32
      REAL EMCS_NSECTOR                                                   32
      EQUIVALENCE (BANK_EMCS(13),EMCS_NSECTOR,RE_EMCS_NSECTOR),           32
     *(NAMES_EMCS(11),NEMCS_NSECTOR)                                      32
      EQUIVALENCE (COMMEN_EMCS(11),CEMCS_NSECTOR),(MAP_EMCS(1,11),        32
     *MEMCS_NSECTOR)                                                      32
      CHARACTER CEMCS_NSECTOR*40/' '/                                     32
      CHARACTER NEMCS_NSECTOR*12/'R.NSECTOR'/                             32
      INTEGER MEMCS_NSECTOR(2)/1,1/                                       32
      REAL RE_EMCS_NSECTOR                                                32
C -                                                                       32
      REAL EMCS_NSECTION                                                  32
      EQUIVALENCE (BANK_EMCS(14),EMCS_NSECTION,RE_EMCS_NSECTION),         32
     *(NAMES_EMCS(12),NEMCS_NSECTION)                                     32
      EQUIVALENCE (COMMEN_EMCS(12),CEMCS_NSECTION),(MAP_EMCS(1,12),       32
     *MEMCS_NSECTION)                                                     32
      CHARACTER CEMCS_NSECTION*40/' '/                                    32
      CHARACTER NEMCS_NSECTION*12/'R.NSECTION'/                           32
      INTEGER MEMCS_NSECTION(2)/1,1/                                      32
      REAL RE_EMCS_NSECTION                                               32
C -                                                                       32
      REAL EMCS_NSLICES                                                   32
      EQUIVALENCE (BANK_EMCS(15),EMCS_NSLICES,RE_EMCS_NSLICES),           32
     *(NAMES_EMCS(13),NEMCS_NSLICES)                                      32
      EQUIVALENCE (COMMEN_EMCS(13),CEMCS_NSLICES),(MAP_EMCS(1,13),        32
     *MEMCS_NSLICES)                                                      32
      CHARACTER CEMCS_NSLICES*40/' '/                                     32
      CHARACTER NEMCS_NSLICES*12/'R.NSLICES'/                             32
      INTEGER MEMCS_NSLICES(2)/1,1/                                       32
      REAL RE_EMCS_NSLICES                                                32
C -                                                                       32
      REAL EMCS_FRONT                                                     32
      EQUIVALENCE (BANK_EMCS(16),EMCS_FRONT,RE_EMCS_FRONT),               32
     *(NAMES_EMCS(14),NEMCS_FRONT)                                        32
      EQUIVALENCE (COMMEN_EMCS(14),CEMCS_FRONT),(MAP_EMCS(1,14),          32
     *MEMCS_FRONT)                                                        32
      CHARACTER CEMCS_FRONT*40/' '/                                       32
      CHARACTER NEMCS_FRONT*12/'R.FRONT'/                                 32
      INTEGER MEMCS_FRONT(2)/1,1/                                         32
      REAL RE_EMCS_FRONT                                                  32
C -                                                                       32
      REAL EMCS_ALINCELL                                                  32
      EQUIVALENCE (BANK_EMCS(17),EMCS_ALINCELL,RE_EMCS_ALINCELL),         32
     *(NAMES_EMCS(15),NEMCS_ALINCELL)                                     32
      EQUIVALENCE (COMMEN_EMCS(15),CEMCS_ALINCELL),(MAP_EMCS(1,15),       32
     *MEMCS_ALINCELL)                                                     32
      CHARACTER CEMCS_ALINCELL*40/' '/                                    32
      CHARACTER NEMCS_ALINCELL*12/'R.ALINCELL'/                           32
      INTEGER MEMCS_ALINCELL(2)/1,1/                                      32
      REAL RE_EMCS_ALINCELL                                               32
C -                                                                       32
      REAL EMCS_FRPLAST                                                   32
      EQUIVALENCE (BANK_EMCS(18),EMCS_FRPLAST,RE_EMCS_FRPLAST),           32
     *(NAMES_EMCS(16),NEMCS_FRPLAST)                                      32
      EQUIVALENCE (COMMEN_EMCS(16),CEMCS_FRPLAST),(MAP_EMCS(1,16),        32
     *MEMCS_FRPLAST)                                                      32
      CHARACTER CEMCS_FRPLAST*40/' '/                                     32
      CHARACTER NEMCS_FRPLAST*12/'R.FRPLAST'/                             32
      INTEGER MEMCS_FRPLAST(2)/1,1/                                       32
      REAL RE_EMCS_FRPLAST                                                32
C -                                                                       32
      REAL EMCS_BKPLAST                                                   32
      EQUIVALENCE (BANK_EMCS(19),EMCS_BKPLAST,RE_EMCS_BKPLAST),           32
     *(NAMES_EMCS(17),NEMCS_BKPLAST)                                      32
      EQUIVALENCE (COMMEN_EMCS(17),CEMCS_BKPLAST),(MAP_EMCS(1,17),        32
     *MEMCS_BKPLAST)                                                      32
      CHARACTER CEMCS_BKPLAST*40/' '/                                     32
      CHARACTER NEMCS_BKPLAST*12/'R.BKPLAST'/                             32
      INTEGER MEMCS_BKPLAST(2)/1,1/                                       32
      REAL RE_EMCS_BKPLAST                                                32
C -                                                                       32
      REAL EMCS_PBPLATE                                                   32
      EQUIVALENCE (BANK_EMCS(20),EMCS_PBPLATE,RE_EMCS_PBPLATE),           32
     *(NAMES_EMCS(18),NEMCS_PBPLATE)                                      32
      EQUIVALENCE (COMMEN_EMCS(18),CEMCS_PBPLATE),(MAP_EMCS(1,18),        32
     *MEMCS_PBPLATE)                                                      32
      CHARACTER CEMCS_PBPLATE*40/' '/                                     32
      CHARACTER NEMCS_PBPLATE*12/'R.PBPLATE'/                             32
      INTEGER MEMCS_PBPLATE(2)/1,1/                                       32
      REAL RE_EMCS_PBPLATE                                                32
C -                                                                       32
      REAL EMCS_LAMPLATE                                                  32
      EQUIVALENCE (BANK_EMCS(21),EMCS_LAMPLATE,RE_EMCS_LAMPLATE),         32
     *(NAMES_EMCS(19),NEMCS_LAMPLATE)                                     32
      EQUIVALENCE (COMMEN_EMCS(19),CEMCS_LAMPLATE),(MAP_EMCS(1,19),       32
     *MEMCS_LAMPLATE)                                                     32
      CHARACTER CEMCS_LAMPLATE*40/' '/                                    32
      CHARACTER NEMCS_LAMPLATE*12/'R.LAMPLATE'/                           32
      INTEGER MEMCS_LAMPLATE(2)/1,1/                                      32
      REAL RE_EMCS_LAMPLATE                                               32
C -                                                                       32
      REAL EMCS_BCKPLATE                                                  32
      EQUIVALENCE (BANK_EMCS(22),EMCS_BCKPLATE,RE_EMCS_BCKPLATE),         32
     *(NAMES_EMCS(20),NEMCS_BCKPLATE)                                     32
      EQUIVALENCE (COMMEN_EMCS(20),CEMCS_BCKPLATE),(MAP_EMCS(1,20),       32
     *MEMCS_BCKPLATE)                                                     32
      CHARACTER CEMCS_BCKPLATE*40/' '/                                    32
      CHARACTER NEMCS_BCKPLATE*12/'R.BCKPLATE'/                           32
      INTEGER MEMCS_BCKPLATE(2)/1,1/                                      32
      REAL RE_EMCS_BCKPLATE                                               32
C -                                                                       32
      REAL EMCS_HUB                                                       32
      EQUIVALENCE (BANK_EMCS(23),EMCS_HUB,RE_EMCS_HUB),(NAMES_EMCS(21),   32
     *NEMCS_HUB)                                                          32
      EQUIVALENCE (COMMEN_EMCS(21),CEMCS_HUB),(MAP_EMCS(1,21),            32
     *MEMCS_HUB)                                                          32
      CHARACTER CEMCS_HUB*40/' '/                                         32
      CHARACTER NEMCS_HUB*12/'R.HUB'/                                     32
      INTEGER MEMCS_HUB(2)/1,1/                                           32
      REAL RE_EMCS_HUB                                                    32
C -                                                                       32
      REAL EMCS_RMSHIFT                                                   32
      EQUIVALENCE (BANK_EMCS(24),EMCS_RMSHIFT,RE_EMCS_RMSHIFT),           32
     *(NAMES_EMCS(22),NEMCS_RMSHIFT)                                      32
      EQUIVALENCE (COMMEN_EMCS(22),CEMCS_RMSHIFT),(MAP_EMCS(1,22),        32
     *MEMCS_RMSHIFT)                                                      32
      CHARACTER CEMCS_RMSHIFT*40/' '/                                     32
      CHARACTER NEMCS_RMSHIFT*12/'R.RMSHIFT'/                             32
      INTEGER MEMCS_RMSHIFT(2)/1,1/                                       32
      REAL RE_EMCS_RMSHIFT                                                32
C -                                                                       32
      REAL EMCS_SMSHIFT                                                   32
      EQUIVALENCE (BANK_EMCS(25),EMCS_SMSHIFT,RE_EMCS_SMSHIFT),           32
     *(NAMES_EMCS(23),NEMCS_SMSHIFT)                                      32
      EQUIVALENCE (COMMEN_EMCS(23),CEMCS_SMSHIFT),(MAP_EMCS(1,23),        32
     *MEMCS_SMSHIFT)                                                      32
      CHARACTER CEMCS_SMSHIFT*40/' '/                                     32
      CHARACTER NEMCS_SMSHIFT*12/'R.SMSHIFT'/                             32
      INTEGER MEMCS_SMSHIFT(2)/1,1/                                       32
      REAL RE_EMCS_SMSHIFT                                                32
C -                                                                       32
      REAL EMCS_GAPPLT                                                    32
      EQUIVALENCE (BANK_EMCS(26),EMCS_GAPPLT,RE_EMCS_GAPPLT),             32
     *(NAMES_EMCS(24),NEMCS_GAPPLT)                                       32
      EQUIVALENCE (COMMEN_EMCS(24),CEMCS_GAPPLT),(MAP_EMCS(1,24),         32
     *MEMCS_GAPPLT)                                                       32
      CHARACTER CEMCS_GAPPLT*40/' '/                                      32
      CHARACTER NEMCS_GAPPLT*12/'R.GAPPLT'/                               32
      INTEGER MEMCS_GAPPLT(2)/1,1/                                        32
      REAL RE_EMCS_GAPPLT                                                 32
C -                                                                       32
      REAL EMCS_GAPCEL                                                    32
      EQUIVALENCE (BANK_EMCS(27),EMCS_GAPCEL,RE_EMCS_GAPCEL),             32
     *(NAMES_EMCS(25),NEMCS_GAPCEL)                                       32
      EQUIVALENCE (COMMEN_EMCS(25),CEMCS_GAPCEL),(MAP_EMCS(1,25),         32
     *MEMCS_GAPCEL)                                                       32
      CHARACTER CEMCS_GAPCEL*40/' '/                                      32
      CHARACTER NEMCS_GAPCEL*12/'R.GAPCEL'/                               32
      INTEGER MEMCS_GAPCEL(2)/1,1/                                        32
      REAL RE_EMCS_GAPCEL                                                 32
C -                                                                       32
      REAL EMCS_GAPSMD                                                    32
      EQUIVALENCE (BANK_EMCS(28),EMCS_GAPSMD,RE_EMCS_GAPSMD),             32
     *(NAMES_EMCS(26),NEMCS_GAPSMD)                                       32
      EQUIVALENCE (COMMEN_EMCS(26),CEMCS_GAPSMD),(MAP_EMCS(1,26),         32
     *MEMCS_GAPSMD)                                                       32
      CHARACTER CEMCS_GAPSMD*40/' '/                                      32
      CHARACTER NEMCS_GAPSMD*12/'R.GAPSMD'/                               32
      INTEGER MEMCS_GAPSMD(2)/1,1/                                        32
      REAL RE_EMCS_GAPSMD                                                 32
C -                                                                       32
      REAL EMCS_SMDCENTR                                                  32
      EQUIVALENCE (BANK_EMCS(29),EMCS_SMDCENTR,RE_EMCS_SMDCENTR),         32
     *(NAMES_EMCS(27),NEMCS_SMDCENTR)                                     32
      EQUIVALENCE (COMMEN_EMCS(27),CEMCS_SMDCENTR),(MAP_EMCS(1,27),       32
     *MEMCS_SMDCENTR)                                                     32
      CHARACTER CEMCS_SMDCENTR*40/' '/                                    32
      CHARACTER NEMCS_SMDCENTR*12/'R.SMDCENTR'/                           32
      INTEGER MEMCS_SMDCENTR(2)/1,1/                                      32
      REAL RE_EMCS_SMDCENTR                                               32
C -                                                                       32
      REAL EMCS_TIEROD(2)                                                 32
      EQUIVALENCE (BANK_EMCS(30),EMCS_TIEROD,RE_EMCS_TIEROD),             32
     *(NAMES_EMCS(28),NEMCS_TIEROD)                                       32
      EQUIVALENCE (COMMEN_EMCS(28),CEMCS_TIEROD),(MAP_EMCS(1,28),         32
     *MEMCS_TIEROD)                                                       32
      CHARACTER CEMCS_TIEROD*40/' '/                                      32
      CHARACTER NEMCS_TIEROD*12/'R.TIEROD'/                               32
      INTEGER MEMCS_TIEROD(2)/2,1/                                        32
      REAL RE_EMCS_TIEROD                                                 32
C -                                                                       32
      REAL EMCS_BCKFRNT                                                   32
      EQUIVALENCE (BANK_EMCS(32),EMCS_BCKFRNT,RE_EMCS_BCKFRNT),           32
     *(NAMES_EMCS(29),NEMCS_BCKFRNT)                                      32
      EQUIVALENCE (COMMEN_EMCS(29),CEMCS_BCKFRNT),(MAP_EMCS(1,29),        32
     *MEMCS_BCKFRNT)                                                      32
      CHARACTER CEMCS_BCKFRNT*40/' '/                                     32
      CHARACTER NEMCS_BCKFRNT*12/'R.BCKFRNT'/                             32
      INTEGER MEMCS_BCKFRNT(2)/1,1/                                       32
      REAL RE_EMCS_BCKFRNT                                                32
C -                                                                       32
      REAL EMCS_GAPHALF                                                   32
      EQUIVALENCE (BANK_EMCS(33),EMCS_GAPHALF,RE_EMCS_GAPHALF),           32
     *(NAMES_EMCS(30),NEMCS_GAPHALF)                                      32
      EQUIVALENCE (COMMEN_EMCS(30),CEMCS_GAPHALF),(MAP_EMCS(1,30),        32
     *MEMCS_GAPHALF)                                                      32
      CHARACTER CEMCS_GAPHALF*40/' '/                                     32
      CHARACTER NEMCS_GAPHALF*12/'R.GAPHALF'/                             32
      INTEGER MEMCS_GAPHALF(2)/1,1/                                       32
      REAL RE_EMCS_GAPHALF                                                32
C -                                                                       32
      REAL EMCS_COVER                                                     32
      EQUIVALENCE (BANK_EMCS(34),EMCS_COVER,RE_EMCS_COVER),               32
     *(NAMES_EMCS(31),NEMCS_COVER)                                        32
      EQUIVALENCE (COMMEN_EMCS(31),CEMCS_COVER),(MAP_EMCS(1,31),          32
     *MEMCS_COVER)                                                        32
      CHARACTER CEMCS_COVER*40/' '/                                       32
      CHARACTER NEMCS_COVER*12/'R.COVER'/                                 32
      INTEGER MEMCS_COVER(2)/1,1/                                         32
      REAL RE_EMCS_COVER                                                  32
C -                                                                       32
      REAL EMCS_RTIE                                                      32
      EQUIVALENCE (BANK_EMCS(35),EMCS_RTIE,RE_EMCS_RTIE),                 32
     *(NAMES_EMCS(32),NEMCS_RTIE)                                         32
      EQUIVALENCE (COMMEN_EMCS(32),CEMCS_RTIE),(MAP_EMCS(1,32),           32
     *MEMCS_RTIE)                                                         32
      CHARACTER CEMCS_RTIE*40/' '/                                        32
      CHARACTER NEMCS_RTIE*12/'R.RTIE'/                                   32
      INTEGER MEMCS_RTIE(2)/1,1/                                          32
      REAL RE_EMCS_RTIE                                                   32
C -                                                                       32
      REAL EMCS_SLOP                                                      32
      EQUIVALENCE (BANK_EMCS(36),EMCS_SLOP,RE_EMCS_SLOP),                 32
     *(NAMES_EMCS(33),NEMCS_SLOP)                                         32
      EQUIVALENCE (COMMEN_EMCS(33),CEMCS_SLOP),(MAP_EMCS(1,33),           32
     *MEMCS_SLOP)                                                         32
      CHARACTER CEMCS_SLOP*40/' '/                                        32
      CHARACTER NEMCS_SLOP*12/'R.SLOP'/                                   32
      INTEGER MEMCS_SLOP(2)/1,1/                                          32
      REAL RE_EMCS_SLOP                                                   32
C -                                                                       32
      INTEGER LEN_EMCS,LLEN_EMCS,LINK_EMCS                                32
      PARAMETER (LEN_EMCS=36)                                             32
      PARAMETER (LLEN_EMCS=33)                                            32
      INTEGER FLAG_EMCS/0/,MAP_EMCS(2,LLEN_EMCS),IDNUM_EMCS(10)           32
      CHARACTER NAMES_EMCS(LLEN_EMCS)*12,COMMEN_EMCS(LLEN_EMCS)*40        32
      DATA IDNUM_EMCS/10*0/                                               32
      REAL BANK_EMCS(LEN_EMCS)                                            32
      CHARACTER*60 BPATH_EMCS/' '/                                        32
      SAVE FLAG_EMCS,MAP_EMCS,IDNUM_EMCS,NAMES_EMCS,COMMEN_EMCS,          32
     *BPATH_EMCS                                                          32
      DATA LINK_EMCS/0/,BANK_EMCS/LEN_EMCS*0/                             32
C *                                                                       32
C -                                                                       33
      REAL EETR_TYPE                                                      33
      EQUIVALENCE (BANK_EETR(3),EETR_TYPE,RE_EETR_TYPE),(NAMES_EETR(1),   33
     *NEETR_TYPE)                                                         33
      EQUIVALENCE (COMMEN_EETR(1),CEETR_TYPE),(MAP_EETR(1,1),             33
     *MEETR_TYPE)                                                         33
      CHARACTER CEETR_TYPE*40/' '/                                        33
      CHARACTER NEETR_TYPE*12/'R.TYPE'/                                   33
      INTEGER MEETR_TYPE(2)/1,1/                                          33
      REAL RE_EETR_TYPE                                                   33
C -                                                                       33
      REAL EETR_ETAGR                                                     33
      EQUIVALENCE (BANK_EETR(4),EETR_ETAGR,RE_EETR_ETAGR),                33
     *(NAMES_EETR(2),NEETR_ETAGR)                                         33
      EQUIVALENCE (COMMEN_EETR(2),CEETR_ETAGR),(MAP_EETR(1,2),            33
     *MEETR_ETAGR)                                                        33
      CHARACTER CEETR_ETAGR*40/' '/                                       33
      CHARACTER NEETR_ETAGR*12/'R.ETAGR'/                                 33
      INTEGER MEETR_ETAGR(2)/1,1/                                         33
      REAL RE_EETR_ETAGR                                                  33
C -                                                                       33
      REAL EETR_PHIGR                                                     33
      EQUIVALENCE (BANK_EETR(5),EETR_PHIGR,RE_EETR_PHIGR),                33
     *(NAMES_EETR(3),NEETR_PHIGR)                                         33
      EQUIVALENCE (COMMEN_EETR(3),CEETR_PHIGR),(MAP_EETR(1,3),            33
     *MEETR_PHIGR)                                                        33
      CHARACTER CEETR_PHIGR*40/' '/                                       33
      CHARACTER NEETR_PHIGR*12/'R.PHIGR'/                                 33
      INTEGER MEETR_PHIGR(2)/1,1/                                         33
      REAL RE_EETR_PHIGR                                                  33
C -                                                                       33
      REAL EETR_NETA                                                      33
      EQUIVALENCE (BANK_EETR(6),EETR_NETA,RE_EETR_NETA),(NAMES_EETR(4),   33
     *NEETR_NETA)                                                         33
      EQUIVALENCE (COMMEN_EETR(4),CEETR_NETA),(MAP_EETR(1,4),             33
     *MEETR_NETA)                                                         33
      CHARACTER CEETR_NETA*40/' '/                                        33
      CHARACTER NEETR_NETA*12/'R.NETA'/                                   33
      INTEGER MEETR_NETA(2)/1,1/                                          33
      REAL RE_EETR_NETA                                                   33
C -                                                                       33
      REAL EETR_ETABIN(13)                                                33
      EQUIVALENCE (BANK_EETR(7),EETR_ETABIN,RE_EETR_ETABIN),              33
     *(NAMES_EETR(5),NEETR_ETABIN)                                        33
      EQUIVALENCE (COMMEN_EETR(5),CEETR_ETABIN),(MAP_EETR(1,5),           33
     *MEETR_ETABIN)                                                       33
      CHARACTER CEETR_ETABIN*40/' '/                                      33
      CHARACTER NEETR_ETABIN*12/'R.ETABIN'/                               33
      INTEGER MEETR_ETABIN(2)/13,1/                                       33
      REAL RE_EETR_ETABIN                                                 33
C -                                                                       33
      INTEGER LEN_EETR,LLEN_EETR,LINK_EETR                                33
      PARAMETER (LEN_EETR=19)                                             33
      PARAMETER (LLEN_EETR=5)                                             33
      INTEGER FLAG_EETR/0/,MAP_EETR(2,LLEN_EETR),IDNUM_EETR(10)           33
      CHARACTER NAMES_EETR(LLEN_EETR)*12,COMMEN_EETR(LLEN_EETR)*40        33
      DATA IDNUM_EETR/10*0/                                               33
      REAL BANK_EETR(LEN_EETR)                                            33
      CHARACTER*60 BPATH_EETR/' '/                                        33
      SAVE FLAG_EETR,MAP_EETR,IDNUM_EETR,NAMES_EETR,COMMEN_EETR,          33
     *BPATH_EETR                                                          33
      DATA LINK_EETR/0/,BANK_EETR/LEN_EETR*0/                             33
C *                                                                       33
C -                                                                       34
      REAL ESEC_ISECT                                                     34
      EQUIVALENCE (BANK_ESEC(3),ESEC_ISECT,RE_ESEC_ISECT),                34
     *(NAMES_ESEC(1),NESEC_ISECT)                                         34
      EQUIVALENCE (COMMEN_ESEC(1),CESEC_ISECT),(MAP_ESEC(1,1),            34
     *MESEC_ISECT)                                                        34
      CHARACTER CESEC_ISECT*40/' '/                                       34
      CHARACTER NESEC_ISECT*12/'R.ISECT'/                                 34
      INTEGER MESEC_ISECT(2)/1,1/                                         34
      REAL RE_ESEC_ISECT                                                  34
C -                                                                       34
      REAL ESEC_FPLMAT                                                    34
      EQUIVALENCE (BANK_ESEC(4),ESEC_FPLMAT,RE_ESEC_FPLMAT),              34
     *(NAMES_ESEC(2),NESEC_FPLMAT)                                        34
      EQUIVALENCE (COMMEN_ESEC(2),CESEC_FPLMAT),(MAP_ESEC(1,2),           34
     *MESEC_FPLMAT)                                                       34
      CHARACTER CESEC_FPLMAT*40/' '/                                      34
      CHARACTER NESEC_FPLMAT*12/'R.FPLMAT'/                               34
      INTEGER MESEC_FPLMAT(2)/1,1/                                        34
      REAL RE_ESEC_FPLMAT                                                 34
C -                                                                       34
      REAL ESEC_CELL                                                      34
      EQUIVALENCE (BANK_ESEC(5),ESEC_CELL,RE_ESEC_CELL),(NAMES_ESEC(3),   34
     *NESEC_CELL)                                                         34
      EQUIVALENCE (COMMEN_ESEC(3),CESEC_CELL),(MAP_ESEC(1,3),             34
     *MESEC_CELL)                                                         34
      CHARACTER CESEC_CELL*40/' '/                                        34
      CHARACTER NESEC_CELL*12/'R.CELL'/                                   34
      INTEGER MESEC_CELL(2)/1,1/                                          34
      REAL RE_ESEC_CELL                                                   34
C -                                                                       34
      REAL ESEC_SCINT                                                     34
      EQUIVALENCE (BANK_ESEC(6),ESEC_SCINT,RE_ESEC_SCINT),                34
     *(NAMES_ESEC(4),NESEC_SCINT)                                         34
      EQUIVALENCE (COMMEN_ESEC(4),CESEC_SCINT),(MAP_ESEC(1,4),            34
     *MESEC_SCINT)                                                        34
      CHARACTER CESEC_SCINT*40/' '/                                       34
      CHARACTER NESEC_SCINT*12/'R.SCINT'/                                 34
      INTEGER MESEC_SCINT(2)/1,1/                                         34
      REAL RE_ESEC_SCINT                                                  34
C -                                                                       34
      REAL ESEC_NLAYER                                                    34
      EQUIVALENCE (BANK_ESEC(7),ESEC_NLAYER,RE_ESEC_NLAYER),              34
     *(NAMES_ESEC(5),NESEC_NLAYER)                                        34
      EQUIVALENCE (COMMEN_ESEC(5),CESEC_NLAYER),(MAP_ESEC(1,5),           34
     *MESEC_NLAYER)                                                       34
      CHARACTER CESEC_NLAYER*40/' '/                                      34
      CHARACTER NESEC_NLAYER*12/'R.NLAYER'/                               34
      INTEGER MESEC_NLAYER(2)/1,1/                                        34
      REAL RE_ESEC_NLAYER                                                 34
C -                                                                       34
      REAL ESEC_DELTAZ                                                    34
      EQUIVALENCE (BANK_ESEC(8),ESEC_DELTAZ,RE_ESEC_DELTAZ),              34
     *(NAMES_ESEC(6),NESEC_DELTAZ)                                        34
      EQUIVALENCE (COMMEN_ESEC(6),CESEC_DELTAZ),(MAP_ESEC(1,6),           34
     *MESEC_DELTAZ)                                                       34
      CHARACTER CESEC_DELTAZ*40/' '/                                      34
      CHARACTER NESEC_DELTAZ*12/'R.DELTAZ'/                               34
      INTEGER MESEC_DELTAZ(2)/1,1/                                        34
      REAL RE_ESEC_DELTAZ                                                 34
C -                                                                       34
      REAL ESEC_JIGGLE(18)                                                34
      EQUIVALENCE (BANK_ESEC(9),ESEC_JIGGLE,RE_ESEC_JIGGLE),              34
     *(NAMES_ESEC(7),NESEC_JIGGLE)                                        34
      EQUIVALENCE (COMMEN_ESEC(7),CESEC_JIGGLE),(MAP_ESEC(1,7),           34
     *MESEC_JIGGLE)                                                       34
      CHARACTER CESEC_JIGGLE*40/' '/                                      34
      CHARACTER NESEC_JIGGLE*12/'R.JIGGLE'/                               34
      INTEGER MESEC_JIGGLE(2)/18,1/                                       34
      REAL RE_ESEC_JIGGLE                                                 34
C -                                                                       34
      INTEGER LEN_ESEC,LLEN_ESEC,LINK_ESEC                                34
      PARAMETER (LEN_ESEC=26)                                             34
      PARAMETER (LLEN_ESEC=7)                                             34
      INTEGER FLAG_ESEC/0/,MAP_ESEC(2,LLEN_ESEC),IDNUM_ESEC(10)           34
      CHARACTER NAMES_ESEC(LLEN_ESEC)*12,COMMEN_ESEC(LLEN_ESEC)*40        34
      DATA IDNUM_ESEC/10*0/                                               34
      REAL BANK_ESEC(LEN_ESEC)                                            34
      CHARACTER*60 BPATH_ESEC/' '/                                        34
      SAVE FLAG_ESEC,MAP_ESEC,IDNUM_ESEC,NAMES_ESEC,COMMEN_ESEC,          34
     *BPATH_ESEC                                                          34
      DATA LINK_ESEC/0/,BANK_ESEC/LEN_ESEC*0/                             34
C *                                                                       34
C -                                                                       35
      REAL EMXG_VERSION                                                   35
      EQUIVALENCE (BANK_EMXG(3),EMXG_VERSION,RE_EMXG_VERSION),            35
     *(NAMES_EMXG(1),NEMXG_VERSION)                                       35
      EQUIVALENCE (COMMEN_EMXG(1),CEMXG_VERSION),(MAP_EMXG(1,1),          35
     *MEMXG_VERSION)                                                      35
      CHARACTER CEMXG_VERSION*40/' '/                                     35
      CHARACTER NEMXG_VERSION*12/'R.VERSION'/                             35
      INTEGER MEMXG_VERSION(2)/1,1/                                       35
      REAL RE_EMXG_VERSION                                                35
C -                                                                       35
      REAL EMXG_SAPEX                                                     35
      EQUIVALENCE (BANK_EMXG(4),EMXG_SAPEX,RE_EMXG_SAPEX),                35
     *(NAMES_EMXG(2),NEMXG_SAPEX)                                         35
      EQUIVALENCE (COMMEN_EMXG(2),CEMXG_SAPEX),(MAP_EMXG(1,2),            35
     *MEMXG_SAPEX)                                                        35
      CHARACTER CEMXG_SAPEX*40/' '/                                       35
      CHARACTER NEMXG_SAPEX*12/'R.SAPEX'/                                 35
      INTEGER MEMXG_SAPEX(2)/1,1/                                         35
      REAL RE_EMXG_SAPEX                                                  35
C -                                                                       35
      REAL EMXG_SBASE                                                     35
      EQUIVALENCE (BANK_EMXG(5),EMXG_SBASE,RE_EMXG_SBASE),                35
     *(NAMES_EMXG(3),NEMXG_SBASE)                                         35
      EQUIVALENCE (COMMEN_EMXG(3),CEMXG_SBASE),(MAP_EMXG(1,3),            35
     *MEMXG_SBASE)                                                        35
      CHARACTER CEMXG_SBASE*40/' '/                                       35
      CHARACTER NEMXG_SBASE*12/'R.SBASE'/                                 35
      INTEGER MEMXG_SBASE(2)/1,1/                                         35
      REAL RE_EMXG_SBASE                                                  35
C -                                                                       35
      REAL EMXG_RIN                                                       35
      EQUIVALENCE (BANK_EMXG(6),EMXG_RIN,RE_EMXG_RIN),(NAMES_EMXG(4),     35
     *NEMXG_RIN)                                                          35
      EQUIVALENCE (COMMEN_EMXG(4),CEMXG_RIN),(MAP_EMXG(1,4),MEMXG_RIN)    35
      CHARACTER CEMXG_RIN*40/' '/                                         35
      CHARACTER NEMXG_RIN*12/'R.RIN'/                                     35
      INTEGER MEMXG_RIN(2)/1,1/                                           35
      REAL RE_EMXG_RIN                                                    35
C -                                                                       35
      REAL EMXG_ROUT                                                      35
      EQUIVALENCE (BANK_EMXG(7),EMXG_ROUT,RE_EMXG_ROUT),(NAMES_EMXG(5),   35
     *NEMXG_ROUT)                                                         35
      EQUIVALENCE (COMMEN_EMXG(5),CEMXG_ROUT),(MAP_EMXG(1,5),             35
     *MEMXG_ROUT)                                                         35
      CHARACTER CEMXG_ROUT*40/' '/                                        35
      CHARACTER NEMXG_ROUT*12/'R.ROUT'/                                   35
      INTEGER MEMXG_ROUT(2)/1,1/                                          35
      REAL RE_EMXG_ROUT                                                   35
C -                                                                       35
      REAL EMXG_F4                                                        35
      EQUIVALENCE (BANK_EMXG(8),EMXG_F4,RE_EMXG_F4),(NAMES_EMXG(6),       35
     *NEMXG_F4)                                                           35
      EQUIVALENCE (COMMEN_EMXG(6),CEMXG_F4),(MAP_EMXG(1,6),MEMXG_F4)      35
      CHARACTER CEMXG_F4*40/' '/                                          35
      CHARACTER NEMXG_F4*12/'R.F4'/                                       35
      INTEGER MEMXG_F4(2)/1,1/                                            35
      REAL RE_EMXG_F4                                                     35
C -                                                                       35
      INTEGER LEN_EMXG,LLEN_EMXG,LINK_EMXG                                35
      PARAMETER (LEN_EMXG=8)                                              35
      PARAMETER (LLEN_EMXG=6)                                             35
      INTEGER FLAG_EMXG/0/,MAP_EMXG(2,LLEN_EMXG),IDNUM_EMXG(10)           35
      CHARACTER NAMES_EMXG(LLEN_EMXG)*12,COMMEN_EMXG(LLEN_EMXG)*40        35
      DATA IDNUM_EMXG/10*0/                                               35
      REAL BANK_EMXG(LEN_EMXG)                                            35
      CHARACTER*60 BPATH_EMXG/' '/                                        35
      SAVE FLAG_EMXG,MAP_EMXG,IDNUM_EMXG,NAMES_EMXG,COMMEN_EMXG,          35
     *BPATH_EMXG                                                          35
      DATA LINK_EMXG/0/,BANK_EMXG/LEN_EMXG*0/                             35
C *                                                                       35
C -                                                                       36
      REAL EXSE_JSECT                                                     36
      EQUIVALENCE (BANK_EXSE(3),EXSE_JSECT,RE_EXSE_JSECT),                36
     *(NAMES_EXSE(1),NEXSE_JSECT)                                         36
      EQUIVALENCE (COMMEN_EXSE(1),CEXSE_JSECT),(MAP_EXSE(1,1),            36
     *MEXSE_JSECT)                                                        36
      CHARACTER CEXSE_JSECT*40/' '/                                       36
      CHARACTER NEXSE_JSECT*12/'R.JSECT'/                                 36
      INTEGER MEXSE_JSECT(2)/1,1/                                         36
      REAL RE_EXSE_JSECT                                                  36
C -                                                                       36
      REAL EXSE_ZSHIFT                                                    36
      EQUIVALENCE (BANK_EXSE(4),EXSE_ZSHIFT,RE_EXSE_ZSHIFT),              36
     *(NAMES_EXSE(2),NEXSE_ZSHIFT)                                        36
      EQUIVALENCE (COMMEN_EXSE(2),CEXSE_ZSHIFT),(MAP_EXSE(1,2),           36
     *MEXSE_ZSHIFT)                                                       36
      CHARACTER CEXSE_ZSHIFT*40/' '/                                      36
      CHARACTER NEXSE_ZSHIFT*12/'R.ZSHIFT'/                               36
      INTEGER MEXSE_ZSHIFT(2)/1,1/                                        36
      REAL RE_EXSE_ZSHIFT                                                 36
C -                                                                       36
      REAL EXSE_SECTYPE(6)                                                36
      EQUIVALENCE (BANK_EXSE(5),EXSE_SECTYPE,RE_EXSE_SECTYPE),            36
     *(NAMES_EXSE(3),NEXSE_SECTYPE)                                       36
      EQUIVALENCE (COMMEN_EXSE(3),CEXSE_SECTYPE),(MAP_EXSE(1,3),          36
     *MEXSE_SECTYPE)                                                      36
      CHARACTER CEXSE_SECTYPE*40/' '/                                     36
      CHARACTER NEXSE_SECTYPE*12/'R.SECTYPE'/                             36
      INTEGER MEXSE_SECTYPE(2)/6,1/                                       36
      REAL RE_EXSE_SECTYPE                                                36
C -                                                                       36
      INTEGER LEN_EXSE,LLEN_EXSE,LINK_EXSE                                36
      PARAMETER (LEN_EXSE=10)                                             36
      PARAMETER (LLEN_EXSE=3)                                             36
      INTEGER FLAG_EXSE/0/,MAP_EXSE(2,LLEN_EXSE),IDNUM_EXSE(10)           36
      CHARACTER NAMES_EXSE(LLEN_EXSE)*12,COMMEN_EXSE(LLEN_EXSE)*40        36
      DATA IDNUM_EXSE/10*0/                                               36
      REAL BANK_EXSE(LEN_EXSE)                                            36
      CHARACTER*60 BPATH_EXSE/' '/                                        36
      SAVE FLAG_EXSE,MAP_EXSE,IDNUM_EXSE,NAMES_EXSE,COMMEN_EXSE,          36
     *BPATH_EXSE                                                          36
      DATA LINK_EXSE/0/,BANK_EXSE/LEN_EXSE*0/                             36
C *                                                                       36
C -                                                                       37
      REAL ESMD_VERSION                                                   37
      EQUIVALENCE (BANK_ESMD(3),ESMD_VERSION,RE_ESMD_VERSION),            37
     *(NAMES_ESMD(1),NESMD_VERSION)                                       37
      EQUIVALENCE (COMMEN_ESMD(1),CESMD_VERSION),(MAP_ESMD(1,1),          37
     *MESMD_VERSION)                                                      37
      CHARACTER CESMD_VERSION*40/' '/                                     37
      CHARACTER NESMD_VERSION*12/'R.VERSION'/                             37
      INTEGER MESMD_VERSION(2)/1,1/                                       37
      REAL RE_ESMD_VERSION                                                37
C -                                                                       37
      REAL ESMD_FRONT_LAYER                                               37
      EQUIVALENCE (BANK_ESMD(4),ESMD_FRONT_LAYER,RE_ESMD_FRONT_LAYER),    37
     *(NAMES_ESMD(2),NESMD_FRONT_LAYER)                                   37
      EQUIVALENCE (COMMEN_ESMD(2),CESMD_FRONT_LAYER),(MAP_ESMD(1,2),      37
     *MESMD_FRONT_LAYER)                                                  37
      CHARACTER CESMD_FRONT_LAYER*40/' '/                                 37
      CHARACTER NESMD_FRONT_LAYER*12/'R.FRONT_LAYER'/                     37
      INTEGER MESMD_FRONT_LAYER(2)/1,1/                                   37
      REAL RE_ESMD_FRONT_LAYER                                            37
C -                                                                       37
      REAL ESMD_BACK_LAYER                                                37
      EQUIVALENCE (BANK_ESMD(5),ESMD_BACK_LAYER,RE_ESMD_BACK_LAYER),      37
     *(NAMES_ESMD(3),NESMD_BACK_LAYER)                                    37
      EQUIVALENCE (COMMEN_ESMD(3),CESMD_BACK_LAYER),(MAP_ESMD(1,3),       37
     *MESMD_BACK_LAYER)                                                   37
      CHARACTER CESMD_BACK_LAYER*40/' '/                                  37
      CHARACTER NESMD_BACK_LAYER*12/'R.BACK_LAYER'/                       37
      INTEGER MESMD_BACK_LAYER(2)/1,1/                                    37
      REAL RE_ESMD_BACK_LAYER                                             37
C -                                                                       37
      REAL ESMD_SPACER_LAYER                                              37
      EQUIVALENCE (BANK_ESMD(6),ESMD_SPACER_LAYER,RE_ESMD_SPACER_LAYER),  37
     *(NAMES_ESMD(4),NESMD_SPACER_LAYER)                                  37
      EQUIVALENCE (COMMEN_ESMD(4),CESMD_SPACER_LAYER),(MAP_ESMD(1,4),     37
     *MESMD_SPACER_LAYER)                                                 37
      CHARACTER CESMD_SPACER_LAYER*40/' '/                                37
      CHARACTER NESMD_SPACER_LAYER*12/'R.SPACER_LAYER'/                   37
      INTEGER MESMD_SPACER_LAYER(2)/1,1/                                  37
      REAL RE_ESMD_SPACER_LAYER                                           37
C -                                                                       37
      REAL ESMD_BASE                                                      37
      EQUIVALENCE (BANK_ESMD(7),ESMD_BASE,RE_ESMD_BASE),(NAMES_ESMD(5),   37
     *NESMD_BASE)                                                         37
      EQUIVALENCE (COMMEN_ESMD(5),CESMD_BASE),(MAP_ESMD(1,5),             37
     *MESMD_BASE)                                                         37
      CHARACTER CESMD_BASE*40/' '/                                        37
      CHARACTER NESMD_BASE*12/'R.BASE'/                                   37
      INTEGER MESMD_BASE(2)/1,1/                                          37
      REAL RE_ESMD_BASE                                                   37
C -                                                                       37
      REAL ESMD_APEX                                                      37
      EQUIVALENCE (BANK_ESMD(8),ESMD_APEX,RE_ESMD_APEX),(NAMES_ESMD(6),   37
     *NESMD_APEX)                                                         37
      EQUIVALENCE (COMMEN_ESMD(6),CESMD_APEX),(MAP_ESMD(1,6),             37
     *MESMD_APEX)                                                         37
      CHARACTER CESMD_APEX*40/' '/                                        37
      CHARACTER NESMD_APEX*12/'R.APEX'/                                   37
      INTEGER MESMD_APEX(2)/1,1/                                          37
      REAL RE_ESMD_APEX                                                   37
C -                                                                       37
      INTEGER LEN_ESMD,LLEN_ESMD,LINK_ESMD                                37
      PARAMETER (LEN_ESMD=8)                                              37
      PARAMETER (LLEN_ESMD=6)                                             37
      INTEGER FLAG_ESMD/0/,MAP_ESMD(2,LLEN_ESMD),IDNUM_ESMD(10)           37
      CHARACTER NAMES_ESMD(LLEN_ESMD)*12,COMMEN_ESMD(LLEN_ESMD)*40        37
      DATA IDNUM_ESMD/10*0/                                               37
      REAL BANK_ESMD(LEN_ESMD)                                            37
      CHARACTER*60 BPATH_ESMD/' '/                                        37
      SAVE FLAG_ESMD,MAP_ESMD,IDNUM_ESMD,NAMES_ESMD,COMMEN_ESMD,          37
     *BPATH_ESMD                                                          37
      DATA LINK_ESMD/0/,BANK_ESMD/LEN_ESMD*0/                             37
C *                                                                       37
C -                                                                       38
      REAL ECUT_VERSION                                                   38
      EQUIVALENCE (BANK_ECUT(3),ECUT_VERSION,RE_ECUT_VERSION),            38
     *(NAMES_ECUT(1),NECUT_VERSION)                                       38
      EQUIVALENCE (COMMEN_ECUT(1),CECUT_VERSION),(MAP_ECUT(1,1),          38
     *MECUT_VERSION)                                                      38
      CHARACTER CECUT_VERSION*40/' '/                                     38
      CHARACTER NECUT_VERSION*12/'R.VERSION'/                             38
      INTEGER MECUT_VERSION(2)/1,1/                                       38
      REAL RE_ECUT_VERSION                                                38
C -                                                                       38
      REAL ECUT_ABSORBER                                                  38
      EQUIVALENCE (BANK_ECUT(4),ECUT_ABSORBER,RE_ECUT_ABSORBER),          38
     *(NAMES_ECUT(2),NECUT_ABSORBER)                                      38
      EQUIVALENCE (COMMEN_ECUT(2),CECUT_ABSORBER),(MAP_ECUT(1,2),         38
     *MECUT_ABSORBER)                                                     38
      CHARACTER CECUT_ABSORBER*40/' '/                                    38
      CHARACTER NECUT_ABSORBER*12/'R.ABSORBER'/                           38
      INTEGER MECUT_ABSORBER(2)/1,1/                                      38
      REAL RE_ECUT_ABSORBER                                               38
C -                                                                       38
      REAL ECUT_SENSITIVE                                                 38
      EQUIVALENCE (BANK_ECUT(5),ECUT_SENSITIVE,RE_ECUT_SENSITIVE),        38
     *(NAMES_ECUT(3),NECUT_SENSITIVE)                                     38
      EQUIVALENCE (COMMEN_ECUT(3),CECUT_SENSITIVE),(MAP_ECUT(1,3),        38
     *MECUT_SENSITIVE)                                                    38
      CHARACTER CECUT_SENSITIVE*40/' '/                                   38
      CHARACTER NECUT_SENSITIVE*12/'R.SENSITIVE'/                         38
      INTEGER MECUT_SENSITIVE(2)/1,1/                                     38
      REAL RE_ECUT_SENSITIVE                                              38
C -                                                                       38
      REAL ECUT_BLAH                                                      38
      EQUIVALENCE (BANK_ECUT(6),ECUT_BLAH,RE_ECUT_BLAH),(NAMES_ECUT(4),   38
     *NECUT_BLAH)                                                         38
      EQUIVALENCE (COMMEN_ECUT(4),CECUT_BLAH),(MAP_ECUT(1,4),             38
     *MECUT_BLAH)                                                         38
      CHARACTER CECUT_BLAH*40/' '/                                        38
      CHARACTER NECUT_BLAH*12/'R.BLAH'/                                   38
      INTEGER MECUT_BLAH(2)/1,1/                                          38
      REAL RE_ECUT_BLAH                                                   38
C -                                                                       38
      INTEGER LEN_ECUT,LLEN_ECUT,LINK_ECUT                                38
      PARAMETER (LEN_ECUT=6)                                              38
      PARAMETER (LLEN_ECUT=4)                                             38
      INTEGER FLAG_ECUT/0/,MAP_ECUT(2,LLEN_ECUT),IDNUM_ECUT(10)           38
      CHARACTER NAMES_ECUT(LLEN_ECUT)*12,COMMEN_ECUT(LLEN_ECUT)*40        38
      DATA IDNUM_ECUT/10*0/                                               38
      REAL BANK_ECUT(LEN_ECUT)                                            38
      CHARACTER*60 BPATH_ECUT/' '/                                        38
      SAVE FLAG_ECUT,MAP_ECUT,IDNUM_ECUT,NAMES_ECUT,COMMEN_ECUT,          38
     *BPATH_ECUT                                                          38
      DATA LINK_ECUT/0/,BANK_ECUT/LEN_ECUT*0/                             38
C *                                                                       38
C -                                                                       40
      REAL EABS_VERSION                                                   40
      EQUIVALENCE (BANK_EABS(3),EABS_VERSION,RE_EABS_VERSION),            40
     *(NAMES_EABS(1),NEABS_VERSION)                                       40
      EQUIVALENCE (COMMEN_EABS(1),CEABS_VERSION),(MAP_EABS(1,1),          40
     *MEABS_VERSION)                                                      40
      CHARACTER CEABS_VERSION*40/' '/                                     40
      CHARACTER NEABS_VERSION*12/'R.VERSION'/                             40
      INTEGER MEABS_VERSION(2)/1,1/                                       40
      REAL RE_EABS_VERSION                                                40
C -                                                                       40
      REAL EABS_CUTGAM                                                    40
      EQUIVALENCE (BANK_EABS(4),EABS_CUTGAM,RE_EABS_CUTGAM),              40
     *(NAMES_EABS(2),NEABS_CUTGAM)                                        40
      EQUIVALENCE (COMMEN_EABS(2),CEABS_CUTGAM),(MAP_EABS(1,2),           40
     *MEABS_CUTGAM)                                                       40
      CHARACTER CEABS_CUTGAM*40/' '/                                      40
      CHARACTER NEABS_CUTGAM*12/'R.CUTGAM'/                               40
      INTEGER MEABS_CUTGAM(2)/1,1/                                        40
      REAL RE_EABS_CUTGAM                                                 40
C -                                                                       40
      REAL EABS_CUTELE                                                    40
      EQUIVALENCE (BANK_EABS(5),EABS_CUTELE,RE_EABS_CUTELE),              40
     *(NAMES_EABS(3),NEABS_CUTELE)                                        40
      EQUIVALENCE (COMMEN_EABS(3),CEABS_CUTELE),(MAP_EABS(1,3),           40
     *MEABS_CUTELE)                                                       40
      CHARACTER CEABS_CUTELE*40/' '/                                      40
      CHARACTER NEABS_CUTELE*12/'R.CUTELE'/                               40
      INTEGER MEABS_CUTELE(2)/1,1/                                        40
      REAL RE_EABS_CUTELE                                                 40
C -                                                                       40
      REAL EABS_CUTNEU                                                    40
      EQUIVALENCE (BANK_EABS(6),EABS_CUTNEU,RE_EABS_CUTNEU),              40
     *(NAMES_EABS(4),NEABS_CUTNEU)                                        40
      EQUIVALENCE (COMMEN_EABS(4),CEABS_CUTNEU),(MAP_EABS(1,4),           40
     *MEABS_CUTNEU)                                                       40
      CHARACTER CEABS_CUTNEU*40/' '/                                      40
      CHARACTER NEABS_CUTNEU*12/'R.CUTNEU'/                               40
      INTEGER MEABS_CUTNEU(2)/1,1/                                        40
      REAL RE_EABS_CUTNEU                                                 40
C -                                                                       40
      REAL EABS_CUTHAD                                                    40
      EQUIVALENCE (BANK_EABS(7),EABS_CUTHAD,RE_EABS_CUTHAD),              40
     *(NAMES_EABS(5),NEABS_CUTHAD)                                        40
      EQUIVALENCE (COMMEN_EABS(5),CEABS_CUTHAD),(MAP_EABS(1,5),           40
     *MEABS_CUTHAD)                                                       40
      CHARACTER CEABS_CUTHAD*40/' '/                                      40
      CHARACTER NEABS_CUTHAD*12/'R.CUTHAD'/                               40
      INTEGER MEABS_CUTHAD(2)/1,1/                                        40
      REAL RE_EABS_CUTHAD                                                 40
C -                                                                       40
      REAL EABS_CUTMUO                                                    40
      EQUIVALENCE (BANK_EABS(8),EABS_CUTMUO,RE_EABS_CUTMUO),              40
     *(NAMES_EABS(6),NEABS_CUTMUO)                                        40
      EQUIVALENCE (COMMEN_EABS(6),CEABS_CUTMUO),(MAP_EABS(1,6),           40
     *MEABS_CUTMUO)                                                       40
      CHARACTER CEABS_CUTMUO*40/' '/                                      40
      CHARACTER NEABS_CUTMUO*12/'R.CUTMUO'/                               40
      INTEGER MEABS_CUTMUO(2)/1,1/                                        40
      REAL RE_EABS_CUTMUO                                                 40
C -                                                                       40
      REAL EABS_DCUTE                                                     40
      EQUIVALENCE (BANK_EABS(9),EABS_DCUTE,RE_EABS_DCUTE),                40
     *(NAMES_EABS(7),NEABS_DCUTE)                                         40
      EQUIVALENCE (COMMEN_EABS(7),CEABS_DCUTE),(MAP_EABS(1,7),            40
     *MEABS_DCUTE)                                                        40
      CHARACTER CEABS_DCUTE*40/' '/                                       40
      CHARACTER NEABS_DCUTE*12/'R.DCUTE'/                                 40
      INTEGER MEABS_DCUTE(2)/1,1/                                         40
      REAL RE_EABS_DCUTE                                                  40
C -                                                                       40
      REAL EABS_DCUTM                                                     40
      EQUIVALENCE (BANK_EABS(10),EABS_DCUTM,RE_EABS_DCUTM),               40
     *(NAMES_EABS(8),NEABS_DCUTM)                                         40
      EQUIVALENCE (COMMEN_EABS(8),CEABS_DCUTM),(MAP_EABS(1,8),            40
     *MEABS_DCUTM)                                                        40
      CHARACTER CEABS_DCUTM*40/' '/                                       40
      CHARACTER NEABS_DCUTM*12/'R.DCUTM'/                                 40
      INTEGER MEABS_DCUTM(2)/1,1/                                         40
      REAL RE_EABS_DCUTM                                                  40
C -                                                                       40
      REAL EABS_BCUTE                                                     40
      EQUIVALENCE (BANK_EABS(11),EABS_BCUTE,RE_EABS_BCUTE),               40
     *(NAMES_EABS(9),NEABS_BCUTE)                                         40
      EQUIVALENCE (COMMEN_EABS(9),CEABS_BCUTE),(MAP_EABS(1,9),            40
     *MEABS_BCUTE)                                                        40
      CHARACTER CEABS_BCUTE*40/' '/                                       40
      CHARACTER NEABS_BCUTE*12/'R.BCUTE'/                                 40
      INTEGER MEABS_BCUTE(2)/1,1/                                         40
      REAL RE_EABS_BCUTE                                                  40
C -                                                                       40
      REAL EABS_BCUTM                                                     40
      EQUIVALENCE (BANK_EABS(12),EABS_BCUTM,RE_EABS_BCUTM),               40
     *(NAMES_EABS(10),NEABS_BCUTM)                                        40
      EQUIVALENCE (COMMEN_EABS(10),CEABS_BCUTM),(MAP_EABS(1,10),          40
     *MEABS_BCUTM)                                                        40
      CHARACTER CEABS_BCUTM*40/' '/                                       40
      CHARACTER NEABS_BCUTM*12/'R.BCUTM'/                                 40
      INTEGER MEABS_BCUTM(2)/1,1/                                         40
      REAL RE_EABS_BCUTM                                                  40
C -                                                                       40
      INTEGER LEN_EABS,LLEN_EABS,LINK_EABS                                40
      PARAMETER (LEN_EABS=12)                                             40
      PARAMETER (LLEN_EABS=10)                                            40
      INTEGER FLAG_EABS/0/,MAP_EABS(2,LLEN_EABS),IDNUM_EABS(10)           40
      CHARACTER NAMES_EABS(LLEN_EABS)*12,COMMEN_EABS(LLEN_EABS)*40        40
      DATA IDNUM_EABS/10*0/                                               40
      REAL BANK_EABS(LEN_EABS)                                            40
      CHARACTER*60 BPATH_EABS/' '/                                        40
      SAVE FLAG_EABS,MAP_EABS,IDNUM_EABS,NAMES_EABS,COMMEN_EABS,          40
     *BPATH_EABS                                                          40
      DATA LINK_EABS/0/,BANK_EABS/LEN_EABS*0/                             40
C *                                                                       40
C -                                                                       42
      REAL ESEN_VERSION                                                   42
      EQUIVALENCE (BANK_ESEN(3),ESEN_VERSION,RE_ESEN_VERSION),            42
     *(NAMES_ESEN(1),NESEN_VERSION)                                       42
      EQUIVALENCE (COMMEN_ESEN(1),CESEN_VERSION),(MAP_ESEN(1,1),          42
     *MESEN_VERSION)                                                      42
      CHARACTER CESEN_VERSION*40/' '/                                     42
      CHARACTER NESEN_VERSION*12/'R.VERSION'/                             42
      INTEGER MESEN_VERSION(2)/1,1/                                       42
      REAL RE_ESEN_VERSION                                                42
C -                                                                       42
      REAL ESEN_CUTGAM                                                    42
      EQUIVALENCE (BANK_ESEN(4),ESEN_CUTGAM,RE_ESEN_CUTGAM),              42
     *(NAMES_ESEN(2),NESEN_CUTGAM)                                        42
      EQUIVALENCE (COMMEN_ESEN(2),CESEN_CUTGAM),(MAP_ESEN(1,2),           42
     *MESEN_CUTGAM)                                                       42
      CHARACTER CESEN_CUTGAM*40/' '/                                      42
      CHARACTER NESEN_CUTGAM*12/'R.CUTGAM'/                               42
      INTEGER MESEN_CUTGAM(2)/1,1/                                        42
      REAL RE_ESEN_CUTGAM                                                 42
C -                                                                       42
      REAL ESEN_CUTELE                                                    42
      EQUIVALENCE (BANK_ESEN(5),ESEN_CUTELE,RE_ESEN_CUTELE),              42
     *(NAMES_ESEN(3),NESEN_CUTELE)                                        42
      EQUIVALENCE (COMMEN_ESEN(3),CESEN_CUTELE),(MAP_ESEN(1,3),           42
     *MESEN_CUTELE)                                                       42
      CHARACTER CESEN_CUTELE*40/' '/                                      42
      CHARACTER NESEN_CUTELE*12/'R.CUTELE'/                               42
      INTEGER MESEN_CUTELE(2)/1,1/                                        42
      REAL RE_ESEN_CUTELE                                                 42
C -                                                                       42
      REAL ESEN_CUTNEU                                                    42
      EQUIVALENCE (BANK_ESEN(6),ESEN_CUTNEU,RE_ESEN_CUTNEU),              42
     *(NAMES_ESEN(4),NESEN_CUTNEU)                                        42
      EQUIVALENCE (COMMEN_ESEN(4),CESEN_CUTNEU),(MAP_ESEN(1,4),           42
     *MESEN_CUTNEU)                                                       42
      CHARACTER CESEN_CUTNEU*40/' '/                                      42
      CHARACTER NESEN_CUTNEU*12/'R.CUTNEU'/                               42
      INTEGER MESEN_CUTNEU(2)/1,1/                                        42
      REAL RE_ESEN_CUTNEU                                                 42
C -                                                                       42
      REAL ESEN_CUTHAD                                                    42
      EQUIVALENCE (BANK_ESEN(7),ESEN_CUTHAD,RE_ESEN_CUTHAD),              42
     *(NAMES_ESEN(5),NESEN_CUTHAD)                                        42
      EQUIVALENCE (COMMEN_ESEN(5),CESEN_CUTHAD),(MAP_ESEN(1,5),           42
     *MESEN_CUTHAD)                                                       42
      CHARACTER CESEN_CUTHAD*40/' '/                                      42
      CHARACTER NESEN_CUTHAD*12/'R.CUTHAD'/                               42
      INTEGER MESEN_CUTHAD(2)/1,1/                                        42
      REAL RE_ESEN_CUTHAD                                                 42
C -                                                                       42
      REAL ESEN_CUTMUO                                                    42
      EQUIVALENCE (BANK_ESEN(8),ESEN_CUTMUO,RE_ESEN_CUTMUO),              42
     *(NAMES_ESEN(6),NESEN_CUTMUO)                                        42
      EQUIVALENCE (COMMEN_ESEN(6),CESEN_CUTMUO),(MAP_ESEN(1,6),           42
     *MESEN_CUTMUO)                                                       42
      CHARACTER CESEN_CUTMUO*40/' '/                                      42
      CHARACTER NESEN_CUTMUO*12/'R.CUTMUO'/                               42
      INTEGER MESEN_CUTMUO(2)/1,1/                                        42
      REAL RE_ESEN_CUTMUO                                                 42
C -                                                                       42
      REAL ESEN_DCUTE                                                     42
      EQUIVALENCE (BANK_ESEN(9),ESEN_DCUTE,RE_ESEN_DCUTE),                42
     *(NAMES_ESEN(7),NESEN_DCUTE)                                         42
      EQUIVALENCE (COMMEN_ESEN(7),CESEN_DCUTE),(MAP_ESEN(1,7),            42
     *MESEN_DCUTE)                                                        42
      CHARACTER CESEN_DCUTE*40/' '/                                       42
      CHARACTER NESEN_DCUTE*12/'R.DCUTE'/                                 42
      INTEGER MESEN_DCUTE(2)/1,1/                                         42
      REAL RE_ESEN_DCUTE                                                  42
C -                                                                       42
      REAL ESEN_DCUTM                                                     42
      EQUIVALENCE (BANK_ESEN(10),ESEN_DCUTM,RE_ESEN_DCUTM),               42
     *(NAMES_ESEN(8),NESEN_DCUTM)                                         42
      EQUIVALENCE (COMMEN_ESEN(8),CESEN_DCUTM),(MAP_ESEN(1,8),            42
     *MESEN_DCUTM)                                                        42
      CHARACTER CESEN_DCUTM*40/' '/                                       42
      CHARACTER NESEN_DCUTM*12/'R.DCUTM'/                                 42
      INTEGER MESEN_DCUTM(2)/1,1/                                         42
      REAL RE_ESEN_DCUTM                                                  42
C -                                                                       42
      REAL ESEN_BCUTE                                                     42
      EQUIVALENCE (BANK_ESEN(11),ESEN_BCUTE,RE_ESEN_BCUTE),               42
     *(NAMES_ESEN(9),NESEN_BCUTE)                                         42
      EQUIVALENCE (COMMEN_ESEN(9),CESEN_BCUTE),(MAP_ESEN(1,9),            42
     *MESEN_BCUTE)                                                        42
      CHARACTER CESEN_BCUTE*40/' '/                                       42
      CHARACTER NESEN_BCUTE*12/'R.BCUTE'/                                 42
      INTEGER MESEN_BCUTE(2)/1,1/                                         42
      REAL RE_ESEN_BCUTE                                                  42
C -                                                                       42
      REAL ESEN_BCUTM                                                     42
      EQUIVALENCE (BANK_ESEN(12),ESEN_BCUTM,RE_ESEN_BCUTM),               42
     *(NAMES_ESEN(10),NESEN_BCUTM)                                        42
      EQUIVALENCE (COMMEN_ESEN(10),CESEN_BCUTM),(MAP_ESEN(1,10),          42
     *MESEN_BCUTM)                                                        42
      CHARACTER CESEN_BCUTM*40/' '/                                       42
      CHARACTER NESEN_BCUTM*12/'R.BCUTM'/                                 42
      INTEGER MESEN_BCUTM(2)/1,1/                                         42
      REAL RE_ESEN_BCUTM                                                  42
C -                                                                       42
      INTEGER LEN_ESEN,LLEN_ESEN,LINK_ESEN                                42
      PARAMETER (LEN_ESEN=12)                                             42
      PARAMETER (LLEN_ESEN=10)                                            42
      INTEGER FLAG_ESEN/0/,MAP_ESEN(2,LLEN_ESEN),IDNUM_ESEN(10)           42
      CHARACTER NAMES_ESEN(LLEN_ESEN)*12,COMMEN_ESEN(LLEN_ESEN)*40        42
      DATA IDNUM_ESEN/10*0/                                               42
      REAL BANK_ESEN(LEN_ESEN)                                            42
      CHARACTER*60 BPATH_ESEN/' '/                                        42
      SAVE FLAG_ESEN,MAP_ESEN,IDNUM_ESEN,NAMES_ESEN,COMMEN_ESEN,          42
     *BPATH_ESEN                                                          42
      DATA LINK_ESEN/0/,BANK_ESEN/LEN_ESEN*0/                             42
C *                                                                       42
      INTEGER I_SECTION,J_SECTION,IE,IS,ISEC,ISTRIP,NSTR,TYPE,II,JJ,CUT,  44
     *FSECT,LSECT, IHALF,FILLED,I,J,K,I_SECTOR                            44
      REAL CENTER,PLATE,CELL,G10,HALFI,TAN_LOW,TAN_UPP,RBOT,RTOP,DETA,    47
     *SQ2,SQ3,DUP, DD,D2,D3,RSHIFT,DPHI,RADIATOR                          47
      REAL MAXCNT,MSECWD,MXGTEN,CURR,SECWID,SECTION,CURCL,ETATOP,ETABOT,  51
     *ZWIDTH,ZSLICE, GAP,MEGATILE,XLEFT,XRIGHT,YLEFT,YRIGHT,CURRENT,RTH,  51
     *LENGTH,P,XC,YC,XX,YY,RDEL, DXY,DDN,DDUP                             51
      REAL MYPHI                                                          53
      INTEGER N                                                           55
      PARAMETER (N=12)                                                    57
C * LAMBDA FUNCTION TANF                                                  58
C ARGUEMENT FOR ETAX                                                      59
      REAL ETAX                                                           59
      REAL TANF                                                           60
      TANF(ETAX) = TAN(2*ATAN(EXP(-ETAX)))                                61
C *                                                                       62
      CALL AGSBEGM(MODULE,IPRIN)                                          62
      IF (IPRIN.LT.0) RETURN                                              62
C Filling bank EMCG - EM ENDCAP CALORIMETER BASIC DATA                    62
      BANK_TITLE='EM ENDCAP CALORIMETER BASIC DATA'                       62
      BPATH_EMCG='EMCG*'                                                  62
      IDNUM_EMCG(1)=0                                                     63
C GEOMETRY VERSION                                                        63
      EMCG_VERSION=6.1                                                    63
      CEMCG_VERSION='GEOMETRY VERSION'                                    63
C CONFIGURATIONS 0-NO, 1-WEST 2-EAST 3-BOTH                               64
      EMCG_ONOFF=3                                                        64
      CEMCG_ONOFF='CONFIGURATIONS 0-NO, 1-WEST 2-EAST 3-BOTH'             64
C SECTORS FILL MODE                                                       65
      EMCG_FILLMODE=3                                                     65
      CEMCG_FILLMODE='SECTORS FILL MODE'                                  65
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EMCG',    66
     *BPATH_EMCG,IDNUM_EMCG,1, MAP_EMCG,NAMES_EMCG,COMMEN_EMCG,           66
     *BANK_EMCG,LEN_EMCG,LLEN_EMCG,LINK_EMCG,FLAG_EMCG)                   66
C *                                                                       66
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EMCG',    66
     *BPATH_EMCG,IDNUM_EMCG,1, MAP_EMCG,NAMES_EMCG,COMMEN_EMCG,           66
     *BANK_EMCG,LEN_EMCG,LLEN_EMCG,LINK_EMCG,FLAG_EMCG)                   66
C *                                                                       67
      CALL AGSBEGM(MODULE,IPRIN)                                          67
      IF (IPRIN.LT.0) RETURN                                              67
C Filling bank EMCS - EM ENDCAP CALORIMETER GEOMETRY                      67
      BANK_TITLE='EM ENDCAP CALORIMETER GEOMETRY'                         67
      BPATH_EMCS='EMCS*'                                                  67
      IDNUM_EMCS(1)=0                                                     68
C VERSIONING                                                              68
      EMCS_VERSION=1                                                      68
      CEMCS_VERSION='VERSIONING'                                          68
C =1 ENDCAP, =2 FPD EDCAP PROTOTYPE                                       69
      EMCS_TYPE=1                                                         69
      CEMCS_TYPE='=1 ENDCAP, =2 FPD EDCAP PROTOTYPE'                      69
C CALORIMETER ORIGIN IN Z                                                 70
      EMCS_ZORG=268.763                                                   70
      CEMCS_ZORG='CALORIMETER ORIGIN IN Z'                                70
C CALORIMETER END IN Z                                                    71
      EMCS_ZEND=310.007                                                   71
      CEMCS_ZEND='CALORIMETER END IN Z'                                   71
C UPPER FEDUCIAL ETA CUT                                                  72
      EMCS_ETAMIN=1.086                                                   72
      CEMCS_ETAMIN='UPPER FEDUCIAL ETA CUT'                               72
C LOWER FIDUCIAL ETA CUT                                                  73
      EMCS_ETAMAX=2.000                                                   73
      CEMCS_ETAMAX='LOWER FIDUCIAL ETA CUT'                               73
C MIN PHI                                                                 74
      EMCS_PHIMIN=-90                                                     74
      CEMCS_PHIMIN='MIN PHI'                                              74
C MAX PHI                                                                 75
      EMCS_PHIMAX=90                                                      75
      CEMCS_PHIMAX='MAX PHI'                                              75
C OFFSET IN X                                                             76
      EMCS_OFFSET=0.0                                                     76
      CEMCS_OFFSET='OFFSET IN X'                                          76
C NUMBER OF AZIMUTHAL SUPERSECTORS                                        77
      EMCS_NSUPSEC=6                                                      77
      CEMCS_NSUPSEC='NUMBER OF AZIMUTHAL SUPERSECTORS'                    77
C NUMBER OF AZIMUTAL SECTORS (PHI GRANULARITY)                            78
      EMCS_NSECTOR=30                                                     78
      CEMCS_NSECTOR='NUMBER OF AZIMUTAL SECTORS (PHI GRANULARITY)'        78
C NUMBER OF PHI SLICES IN SUPERSECTOR                                     79
      EMCS_NSLICES=5                                                      79
      CEMCS_NSLICES='NUMBER OF PHI SLICES IN SUPERSECTOR'                 79
C NUMBER OF READOUT SECTIONS                                              80
      EMCS_NSECTION=4                                                     80
      CEMCS_NSECTION='NUMBER OF READOUT SECTIONS'                         80
C THICKNESS OF THE FRONT AL PLATES                                        81
      EMCS_FRONT=0.953                                                    81
      CEMCS_FRONT='THICKNESS OF THE FRONT AL PLATES'                      81
C ALUMINIM PLATE IN CELL                                                  82
      EMCS_ALINCELL=0.02                                                  82
      CEMCS_ALINCELL='ALUMINIM PLATE IN CELL'                             82
C FRONT PLASTIC IN MEGATILE                                               83
      EMCS_FRPLAST=0.015                                                  83
      CEMCS_FRPLAST='FRONT PLASTIC IN MEGATILE'                           83
C FIBER ROUTING GUIDES AND BACK PLASTIC                                   84
      EMCS_BKPLAST=0.155                                                  84
      CEMCS_BKPLAST='FIBER ROUTING GUIDES AND BACK PLASTIC'               84
C LEAD RADIATOR THICKNESS                                                 85
      EMCS_PBPLATE=0.457                                                  85
      CEMCS_PBPLATE='LEAD RADIATOR THICKNESS'                             85
C LAMINATED SS PLATE THICKNESS                                            86
      EMCS_LAMPLATE=0.05                                                  86
      CEMCS_LAMPLATE='LAMINATED SS PLATE THICKNESS'                       86
C BACK SS PLATE THICKNESS                                                 87
      EMCS_BCKPLATE=3.175                                                 87
      CEMCS_BCKPLATE='BACK SS PLATE THICKNESS'                            87
C THICKNESS OF ENDCAP HUB                                                 88
      EMCS_HUB=3.81                                                       88
      CEMCS_HUB='THICKNESS OF ENDCAP HUB'                                 88
C RADIAL SHIFT OF MODULE                                                  89
      EMCS_RMSHIFT=2.121                                                  89
      CEMCS_RMSHIFT='RADIAL SHIFT OF MODULE'                              89
C RADIAL SHIFT OF STEEL SUPPORT WALLS                                     90
      EMCS_SMSHIFT=0.12                                                   90
      CEMCS_SMSHIFT='RADIAL SHIFT OF STEEL SUPPORT WALLS'                 90
C HALF OF THE INTER-PLATE GAP IN PHI                                      91
      EMCS_GAPPLT=0.3/2                                                   91
      CEMCS_GAPPLT='HALF OF THE INTER-PLATE GAP IN PHI'                   91
C HALF OF THE RADIAL INTER-CELL GAP                                       92
      EMCS_GAPCEL=0.03/2                                                  92
      CEMCS_GAPCEL='HALF OF THE RADIAL INTER-CELL GAP'                    92
C SPACE FOR SMD DETECTOR << VERSION 2 -- 3.600 >>                         93
      EMCS_GAPSMD=3.400                                                   93
      CEMCS_GAPSMD='SPACE FOR SMD DETECTOR << VERSION 2 -- 3.600 >>'      93
C SMD POSITION                                                            94
      EMCS_SMDCENTR=279.542                                               94
      CEMCS_SMDCENTR='SMD POSITION'                                       94
C RADIAL POSITION OF TIE RODS                                             95
      EMCS_TIEROD(1)=160.                                                 95
      EMCS_TIEROD(2)=195                                                  95
      CEMCS_TIEROD='RADIAL POSITION OF TIE RODS'                          95
C BACKPLATE FRONT Z                                                       96
      EMCS_BCKFRNT=306.832                                                96
      CEMCS_BCKFRNT='BACKPLATE FRONT Z'                                   96
C 1/2 GAP BETWEEN HALVES OF ENDCAP WHEEL                                  97
      EMCS_GAPHALF=0.4                                                    97
      CEMCS_GAPHALF='1/2 GAP BETWEEN HALVES OF ENDCAP WHEEL'              97
C COVER OF WHEEL HALF                                                     98
      EMCS_COVER=0.075                                                    98
      CEMCS_COVER='COVER OF WHEEL HALF'                                   98
C RADIUS OF TIE ROD                                                       99
      EMCS_RTIE=1.0425                                                    99
      CEMCS_RTIE='RADIUS OF TIE ROD'                                      99
C ADDED TO CELL CONTAINING RADIATOR 6 (FORMERLY HARDCODED IN GEOM)        100
      EMCS_SLOP=0.1400                                                    100
      CEMCS_SLOP='ADDED TO CELL CONTAINING RADIATOR 6 (FORMERLY HARDCODE  100
     *D IN GEOM)'                                                         100
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EMCS',    101
     *BPATH_EMCS,IDNUM_EMCS,1, MAP_EMCS,NAMES_EMCS,COMMEN_EMCS,           101
     *BANK_EMCS,LEN_EMCS,LLEN_EMCS,LINK_EMCS,FLAG_EMCS)                   101
C *                                                                       101
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EMCS',    101
     *BPATH_EMCS,IDNUM_EMCS,1, MAP_EMCS,NAMES_EMCS,COMMEN_EMCS,           101
     *BANK_EMCS,LEN_EMCS,LLEN_EMCS,LINK_EMCS,FLAG_EMCS)                   101
C *                                                                       102
      CALL AGSBEGM(MODULE,IPRIN)                                          102
      IF (IPRIN.LT.0) RETURN                                              102
C Filling bank EMCS - EM ENDCAP CALORIMETER GEOMETRY                      102
      BANK_TITLE='EM ENDCAP CALORIMETER GEOMETRY'                         102
      BPATH_EMCS='EMCS*'                                                  102
      IDNUM_EMCS(1)=0                                                     103
C VERSIONING                                                              103
      EMCS_VERSION=2                                                      103
      CEMCS_VERSION='VERSIONING'                                          103
C =1 ENDCAP, =2 FPD EDCAP PROTOTYPE                                       104
      EMCS_TYPE=1                                                         104
      CEMCS_TYPE='=1 ENDCAP, =2 FPD EDCAP PROTOTYPE'                      104
C CALORIMETER ORIGIN IN Z                                                 105
      EMCS_ZORG=268.763                                                   105
      CEMCS_ZORG='CALORIMETER ORIGIN IN Z'                                105
C CALORIMETER END IN Z                                                    106
      EMCS_ZEND=310.007                                                   106
      CEMCS_ZEND='CALORIMETER END IN Z'                                   106
C UPPER FEDUCIAL ETA CUT                                                  107
      EMCS_ETAMIN=1.086                                                   107
      CEMCS_ETAMIN='UPPER FEDUCIAL ETA CUT'                               107
C LOWER FIDUCIAL ETA CUT                                                  108
      EMCS_ETAMAX=2.000                                                   108
      CEMCS_ETAMAX='LOWER FIDUCIAL ETA CUT'                               108
C MIN PHI                                                                 109
      EMCS_PHIMIN=-90                                                     109
      CEMCS_PHIMIN='MIN PHI'                                              109
C MAX PHI                                                                 110
      EMCS_PHIMAX=90                                                      110
      CEMCS_PHIMAX='MAX PHI'                                              110
C OFFSET IN X                                                             111
      EMCS_OFFSET=0.0                                                     111
      CEMCS_OFFSET='OFFSET IN X'                                          111
C NUMBER OF AZIMUTHAL SUPERSECTORS                                        112
      EMCS_NSUPSEC=6                                                      112
      CEMCS_NSUPSEC='NUMBER OF AZIMUTHAL SUPERSECTORS'                    112
C NUMBER OF AZIMUTAL SECTORS (PHI GRANULARITY)                            113
      EMCS_NSECTOR=30                                                     113
      CEMCS_NSECTOR='NUMBER OF AZIMUTAL SECTORS (PHI GRANULARITY)'        113
C NUMBER OF PHI SLICES IN SUPERSECTOR                                     114
      EMCS_NSLICES=5                                                      114
      CEMCS_NSLICES='NUMBER OF PHI SLICES IN SUPERSECTOR'                 114
C NUMBER OF READOUT SECTIONS                                              115
      EMCS_NSECTION=4                                                     115
      CEMCS_NSECTION='NUMBER OF READOUT SECTIONS'                         115
C THICKNESS OF THE FRONT AL PLATES                                        116
      EMCS_FRONT=0.953                                                    116
      CEMCS_FRONT='THICKNESS OF THE FRONT AL PLATES'                      116
C ALUMINIM PLATE IN CELL                                                  117
      EMCS_ALINCELL=0.02                                                  117
      CEMCS_ALINCELL='ALUMINIM PLATE IN CELL'                             117
C FRONT PLASTIC IN MEGATILE                                               118
      EMCS_FRPLAST=0.015                                                  118
      CEMCS_FRPLAST='FRONT PLASTIC IN MEGATILE'                           118
C FIBER ROUTING GUIDES AND BACK PLASTIC                                   119
      EMCS_BKPLAST=0.155                                                  119
      CEMCS_BKPLAST='FIBER ROUTING GUIDES AND BACK PLASTIC'               119
C LEAD RADIATOR THICKNESS                                                 120
      EMCS_PBPLATE=0.457                                                  120
      CEMCS_PBPLATE='LEAD RADIATOR THICKNESS'                             120
C LAMINATED SS PLATE THICKNESS                                            121
      EMCS_LAMPLATE=0.05                                                  121
      CEMCS_LAMPLATE='LAMINATED SS PLATE THICKNESS'                       121
C BACK SS PLATE THICKNESS                                                 122
      EMCS_BCKPLATE=3.175                                                 122
      CEMCS_BCKPLATE='BACK SS PLATE THICKNESS'                            122
C THICKNESS OF ENDCAP HUB                                                 123
      EMCS_HUB=3.81                                                       123
      CEMCS_HUB='THICKNESS OF ENDCAP HUB'                                 123
C RADIAL SHIFT OF MODULE                                                  124
      EMCS_RMSHIFT=2.121                                                  124
      CEMCS_RMSHIFT='RADIAL SHIFT OF MODULE'                              124
C RADIAL SHIFT OF STEEL SUPPORT WALLS                                     125
      EMCS_SMSHIFT=0.12                                                   125
      CEMCS_SMSHIFT='RADIAL SHIFT OF STEEL SUPPORT WALLS'                 125
C HALF OF THE INTER-PLATE GAP IN PHI                                      126
      EMCS_GAPPLT=0.3/2                                                   126
      CEMCS_GAPPLT='HALF OF THE INTER-PLATE GAP IN PHI'                   126
C HALF OF THE RADIAL INTER-CELL GAP                                       127
      EMCS_GAPCEL=0.03/2                                                  127
      CEMCS_GAPCEL='HALF OF THE RADIAL INTER-CELL GAP'                    127
C SPACE FOR SMD DETECTOR (* FROM MASTER_GEOM_BMP.XLS *)                   128
      EMCS_GAPSMD=3.600                                                   128
      CEMCS_GAPSMD='SPACE FOR SMD DETECTOR (* FROM MASTER_GEOM_BMP.XLS *  128
     *)'                                                                  128
C SMD POSITION                                                            129
      EMCS_SMDCENTR=279.542                                               129
      CEMCS_SMDCENTR='SMD POSITION'                                       129
C RADIAL POSITION OF TIE RODS                                             130
      EMCS_TIEROD(1)=160.                                                 130
      EMCS_TIEROD(2)=195                                                  130
      CEMCS_TIEROD='RADIAL POSITION OF TIE RODS'                          130
C BACKPLATE FRONT Z                                                       131
      EMCS_BCKFRNT=306.832                                                131
      CEMCS_BCKFRNT='BACKPLATE FRONT Z'                                   131
C 1/2 GAP BETWEEN HALVES OF ENDCAP WHEEL                                  132
      EMCS_GAPHALF=0.4                                                    132
      CEMCS_GAPHALF='1/2 GAP BETWEEN HALVES OF ENDCAP WHEEL'              132
C COVER OF WHEEL HALF                                                     133
      EMCS_COVER=0.075                                                    133
      CEMCS_COVER='COVER OF WHEEL HALF'                                   133
C RADIUS OF TIE ROD                                                       134
      EMCS_RTIE=0.75                                                      134
      CEMCS_RTIE='RADIUS OF TIE ROD'                                      134
C ADDED TO CELL CONTAINING RADIATOR 6 (FORMERLY HARDCODED IN GEOM)        135
      EMCS_SLOP=0.0000                                                    135
      CEMCS_SLOP='ADDED TO CELL CONTAINING RADIATOR 6 (FORMERLY HARDCODE  135
     *D IN GEOM)'                                                         135
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EMCS',    136
     *BPATH_EMCS,IDNUM_EMCS,1, MAP_EMCS,NAMES_EMCS,COMMEN_EMCS,           136
     *BANK_EMCS,LEN_EMCS,LLEN_EMCS,LINK_EMCS,FLAG_EMCS)                   136
C *                                                                       136
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EMCS',    136
     *BPATH_EMCS,IDNUM_EMCS,1, MAP_EMCS,NAMES_EMCS,COMMEN_EMCS,           136
     *BANK_EMCS,LEN_EMCS,LLEN_EMCS,LINK_EMCS,FLAG_EMCS)                   136
C *                                                                       137
      CALL AGSBEGM(MODULE,IPRIN)                                          137
      IF (IPRIN.LT.0) RETURN                                              137
C Filling bank ESMD - SHOWER MAXIMUM DETECTOR INFORMATION                 137
      BANK_TITLE='SHOWER MAXIMUM DETECTOR INFORMATION'                    137
      BPATH_ESMD='ESMD*'                                                  137
      IDNUM_ESMD(1)=0                                                     138
C VERSIONING INFORMATION                                                  138
      ESMD_VERSION=1                                                      138
      CESMD_VERSION='VERSIONING INFORMATION'                              138
C THICKNESS OF FRONT LAYER                                                139
      ESMD_FRONT_LAYER=0.161                                              139
      CESMD_FRONT_LAYER='THICKNESS OF FRONT LAYER'                        139
C THICKNESS OF BACK LAYER                                                 140
      ESMD_BACK_LAYER=0.210                                               140
      CESMD_BACK_LAYER='THICKNESS OF BACK LAYER'                          140
C BASE OF THE SMD STRIP                                                   141
      ESMD_BASE=1.0                                                       141
      CESMD_BASE='BASE OF THE SMD STRIP'                                  141
C APEX OF THE SMD STRIP                                                   142
      ESMD_APEX=0.7                                                       142
      CESMD_APEX='APEX OF THE SMD STRIP'                                  142
C SPACER LAYER                                                            143
      ESMD_SPACER_LAYER=1.2                                               143
      CESMD_SPACER_LAYER='SPACER LAYER'                                   143
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESMD',    144
     *BPATH_ESMD,IDNUM_ESMD,1, MAP_ESMD,NAMES_ESMD,COMMEN_ESMD,           144
     *BANK_ESMD,LEN_ESMD,LLEN_ESMD,LINK_ESMD,FLAG_ESMD)                   144
C *                                                                       144
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESMD',    144
     *BPATH_ESMD,IDNUM_ESMD,1, MAP_ESMD,NAMES_ESMD,COMMEN_ESMD,           144
     *BANK_ESMD,LEN_ESMD,LLEN_ESMD,LINK_ESMD,FLAG_ESMD)                   144
C *                                                                       145
      CALL AGSBEGM(MODULE,IPRIN)                                          145
      IF (IPRIN.LT.0) RETURN                                              145
C Filling bank EETR - ETA AND PHI GRID VALUES                             145
      BANK_TITLE='ETA AND PHI GRID VALUES'                                145
      BPATH_EETR='EETR*'                                                  145
      IDNUM_EETR(1)=0                                                     146
C =1 ENDCAP, =2 FPD                                                       146
      EETR_TYPE=1                                                         146
      CEETR_TYPE='=1 ENDCAP, =2 FPD'                                      146
C ETA_TOP/ETA_BOT TOWER GRANULARITY                                       147
      EETR_ETAGR=1.0536                                                   147
      CEETR_ETAGR='ETA_TOP/ETA_BOT TOWER GRANULARITY'                     147
C PHI GRANULARITY (RADIANS)                                               148
      EETR_PHIGR=0.0981747                                                148
      CEETR_PHIGR='PHI GRANULARITY (RADIANS)'                             148
C ETA GRANULARITY                                                         149
      EETR_NETA=12                                                        149
      CEETR_NETA='ETA GRANULARITY'                                        149
C ETA RAPIDITIES                                                          152
      EETR_ETABIN(1)=2.0                                                  152
      EETR_ETABIN(2)=1.9008                                               152
      EETR_ETABIN(3)=1.8065                                               152
      EETR_ETABIN(4)=1.7168                                               152
      EETR_ETABIN(5)=1.6317                                               152
      EETR_ETABIN(6)=1.5507                                               152
      EETR_ETABIN(7)=1.4738                                               152
      EETR_ETABIN(8)=1.4007                                               152
      EETR_ETABIN(9)=1.3312                                               152
      EETR_ETABIN(10)=1.2651                                              152
      EETR_ETABIN(11)=1.2023                                              152
      EETR_ETABIN(12)=1.1427                                              152
      EETR_ETABIN(13)=1.086                                               152
      CEETR_ETABIN='ETA RAPIDITIES'                                       152
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EETR',    153
     *BPATH_EETR,IDNUM_EETR,1, MAP_EETR,NAMES_EETR,COMMEN_EETR,           153
     *BANK_EETR,LEN_EETR,LLEN_EETR,LINK_EETR,FLAG_EETR)                   153
C *                                                                       153
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EETR',    153
     *BPATH_EETR,IDNUM_EETR,1, MAP_EETR,NAMES_EETR,COMMEN_EETR,           153
     *BANK_EETR,LEN_EETR,LLEN_EETR,LINK_EETR,FLAG_EETR)                   153
C *                                                                       154
      CALL AGSBEGM(MODULE,IPRIN)                                          154
      IF (IPRIN.LT.0) RETURN                                              154
C Filling bank ESEC - PRESHOWER 1 / RADIATOR 1                            154
      BANK_TITLE='PRESHOWER 1 / RADIATOR 1'                               154
      BPATH_ESEC='ESEC*'                                                  154
      IDNUM_ESEC(1)=0                                                     155
C SECTION NUMBER                                                          155
      ESEC_ISECT=1                                                        155
      CESEC_ISECT='SECTION NUMBER'                                        155
C NUMBER OF SCI LAYERS ALONG Z                                            156
      ESEC_NLAYER=1                                                       156
      CESEC_NLAYER='NUMBER OF SCI LAYERS ALONG Z'                         156
C CELL FULL WIDTH IN Z                                                    157
      ESEC_CELL=1.505                                                     157
      CESEC_CELL='CELL FULL WIDTH IN Z'                                   157
C SCI LAYER THICKNESS (4.75MM BICRON)                                     158
      ESEC_SCINT=0.475                                                    158
      CESEC_SCINT='SCI LAYER THICKNESS (4.75MM BICRON)'                   158
C AMOUNT TO SHIFT SECTION IN Z TO ALIGN WITH AS-BUILT NUMBERS             159
      ESEC_DELTAZ=-0.014                                                  159
      CESEC_DELTAZ='AMOUNT TO SHIFT SECTION IN Z TO ALIGN WITH AS-BUILT   159
     *NUMBERS'                                                            159
C DEGREES TO SHIFT EPER IN EACH LAYER                                     161
      ESEC_JIGGLE(1)=0                                                    161
      ESEC_JIGGLE(2)=0                                                    161
      ESEC_JIGGLE(3)=0                                                    161
      ESEC_JIGGLE(4)=0                                                    161
      ESEC_JIGGLE(5)=0                                                    161
      ESEC_JIGGLE(6)=0                                                    161
      ESEC_JIGGLE(7)=0                                                    161
      ESEC_JIGGLE(8)=0                                                    161
      ESEC_JIGGLE(9)=0                                                    161
      ESEC_JIGGLE(10)=0                                                   161
      ESEC_JIGGLE(11)=0                                                   161
      ESEC_JIGGLE(12)=0                                                   161
      ESEC_JIGGLE(13)=0                                                   161
      ESEC_JIGGLE(14)=0                                                   161
      ESEC_JIGGLE(15)=0                                                   161
      ESEC_JIGGLE(16)=0                                                   161
      ESEC_JIGGLE(17)=0                                                   161
      ESEC_JIGGLE(18)=0                                                   161
      CESEC_JIGGLE='DEGREES TO SHIFT EPER IN EACH LAYER'                  161
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESEC',    162
     *BPATH_ESEC,IDNUM_ESEC,1, MAP_ESEC,NAMES_ESEC,COMMEN_ESEC,           162
     *BANK_ESEC,LEN_ESEC,LLEN_ESEC,LINK_ESEC,FLAG_ESEC)                   162
C *                                                                       162
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESEC',    162
     *BPATH_ESEC,IDNUM_ESEC,1, MAP_ESEC,NAMES_ESEC,COMMEN_ESEC,           162
     *BANK_ESEC,LEN_ESEC,LLEN_ESEC,LINK_ESEC,FLAG_ESEC)                   162
C *                                                                       163
      CALL AGSBEGM(MODULE,IPRIN)                                          163
      IF (IPRIN.LT.0) RETURN                                              163
C Filling bank ESEC - PRESHOWER 2 / RADIATOR 2                            163
      BANK_TITLE='PRESHOWER 2 / RADIATOR 2'                               163
      BPATH_ESEC='ESEC*'                                                  163
      IDNUM_ESEC(1)=0                                                     164
C SECTION NUMBER                                                          164
      ESEC_ISECT=2                                                        164
      CESEC_ISECT='SECTION NUMBER'                                        164
C NUMBER OF SCI LAYERS ALONG Z                                            165
      ESEC_NLAYER=1                                                       165
      CESEC_NLAYER='NUMBER OF SCI LAYERS ALONG Z'                         165
C CELL FULL WIDTH IN Z                                                    166
      ESEC_CELL=1.505                                                     166
      CESEC_CELL='CELL FULL WIDTH IN Z'                                   166
C SCI LAYER THICKNESS (4.75MM BICRON)                                     167
      ESEC_SCINT=0.475                                                    167
      CESEC_SCINT='SCI LAYER THICKNESS (4.75MM BICRON)'                   167
C AMOUNT TO SHIFT SECTION IN Z TO ALIGN WITH AS-BUILT NUMBERS             168
      ESEC_DELTAZ=-0.0182                                                 168
      CESEC_DELTAZ='AMOUNT TO SHIFT SECTION IN Z TO ALIGN WITH AS-BUILT   168
     *NUMBERS'                                                            168
C DEGREES TO SHIFT EPER IN EACH LAYER                                     170
      ESEC_JIGGLE(1)=0                                                    170
      ESEC_JIGGLE(2)=0                                                    170
      ESEC_JIGGLE(3)=0                                                    170
      ESEC_JIGGLE(4)=0                                                    170
      ESEC_JIGGLE(5)=0                                                    170
      ESEC_JIGGLE(6)=0                                                    170
      ESEC_JIGGLE(7)=0                                                    170
      ESEC_JIGGLE(8)=0                                                    170
      ESEC_JIGGLE(9)=0                                                    170
      ESEC_JIGGLE(10)=0                                                   170
      ESEC_JIGGLE(11)=0                                                   170
      ESEC_JIGGLE(12)=0                                                   170
      ESEC_JIGGLE(13)=0                                                   170
      ESEC_JIGGLE(14)=0                                                   170
      ESEC_JIGGLE(15)=0                                                   170
      ESEC_JIGGLE(16)=0                                                   170
      ESEC_JIGGLE(17)=0                                                   170
      ESEC_JIGGLE(18)=0                                                   170
      CESEC_JIGGLE='DEGREES TO SHIFT EPER IN EACH LAYER'                  170
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESEC',    171
     *BPATH_ESEC,IDNUM_ESEC,1, MAP_ESEC,NAMES_ESEC,COMMEN_ESEC,           171
     *BANK_ESEC,LEN_ESEC,LLEN_ESEC,LINK_ESEC,FLAG_ESEC)                   171
C *                                                                       171
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESEC',    171
     *BPATH_ESEC,IDNUM_ESEC,1, MAP_ESEC,NAMES_ESEC,COMMEN_ESEC,           171
     *BANK_ESEC,LEN_ESEC,LLEN_ESEC,LINK_ESEC,FLAG_ESEC)                   171
C *                                                                       172
      CALL AGSBEGM(MODULE,IPRIN)                                          172
      IF (IPRIN.LT.0) RETURN                                              172
C Filling bank ESEC - MEGATILES 3-6 / RADIATORS 3-5                       172
      BANK_TITLE='MEGATILES 3-6 / RADIATORS 3-5'                          172
      BPATH_ESEC='ESEC*'                                                  172
      IDNUM_ESEC(1)=0                                                     173
C SECTION NUMBER                                                          173
      ESEC_ISECT=3                                                        173
      CESEC_ISECT='SECTION NUMBER'                                        173
C NUMBER OF SCI LAYERS ALONG Z                                            174
      ESEC_NLAYER=4                                                       174
      CESEC_NLAYER='NUMBER OF SCI LAYERS ALONG Z'                         174
C CELL FULL WIDTH IN Z                                                    175
      ESEC_CELL=1.405                                                     175
      CESEC_CELL='CELL FULL WIDTH IN Z'                                   175
C SCI LAYER THICKNESS                                                     176
      ESEC_SCINT=0.4                                                      176
      CESEC_SCINT='SCI LAYER THICKNESS'                                   176
C AMOUNT TO SHIFT SECTION IN Z TO ALIGN WITH AS-BUILT NUMBERS             177
      ESEC_DELTAZ=-0.0145                                                 177
      CESEC_DELTAZ='AMOUNT TO SHIFT SECTION IN Z TO ALIGN WITH AS-BUILT   177
     *NUMBERS'                                                            177
C DEGREES TO SHIFT EPER IN EACH LAYER                                     179
      ESEC_JIGGLE(1)=0                                                    179
      ESEC_JIGGLE(2)=0                                                    179
      ESEC_JIGGLE(3)=0                                                    179
      ESEC_JIGGLE(4)=0                                                    179
      ESEC_JIGGLE(5)=0                                                    179
      ESEC_JIGGLE(6)=0                                                    179
      ESEC_JIGGLE(7)=0                                                    179
      ESEC_JIGGLE(8)=0                                                    179
      ESEC_JIGGLE(9)=0                                                    179
      ESEC_JIGGLE(10)=0                                                   179
      ESEC_JIGGLE(11)=0                                                   179
      ESEC_JIGGLE(12)=0                                                   179
      ESEC_JIGGLE(13)=0                                                   179
      ESEC_JIGGLE(14)=0                                                   179
      ESEC_JIGGLE(15)=0                                                   179
      ESEC_JIGGLE(16)=0                                                   179
      ESEC_JIGGLE(17)=0                                                   179
      ESEC_JIGGLE(18)=0                                                   179
      CESEC_JIGGLE='DEGREES TO SHIFT EPER IN EACH LAYER'                  179
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESEC',    180
     *BPATH_ESEC,IDNUM_ESEC,1, MAP_ESEC,NAMES_ESEC,COMMEN_ESEC,           180
     *BANK_ESEC,LEN_ESEC,LLEN_ESEC,LINK_ESEC,FLAG_ESEC)                   180
C *                                                                       180
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESEC',    180
     *BPATH_ESEC,IDNUM_ESEC,1, MAP_ESEC,NAMES_ESEC,COMMEN_ESEC,           180
     *BANK_ESEC,LEN_ESEC,LLEN_ESEC,LINK_ESEC,FLAG_ESEC)                   180
C *                                                                       181
      CALL AGSBEGM(MODULE,IPRIN)                                          181
      IF (IPRIN.LT.0) RETURN                                              181
C Filling bank ESEC - MEGATILES 7-23 / RADIATORS 6-23                     181
      BANK_TITLE='MEGATILES 7-23 / RADIATORS 6-23'                        181
      BPATH_ESEC='ESEC*'                                                  181
      IDNUM_ESEC(1)=0                                                     182
C SECTION                                                                 182
      ESEC_ISECT=4                                                        182
      CESEC_ISECT='SECTION'                                               182
C NUMBER OF LAYERS ALONG Z                                                183
      ESEC_NLAYER=18                                                      183
      CESEC_NLAYER='NUMBER OF LAYERS ALONG Z'                             183
C CELL FULL WIDTH IN Z                                                    184
      ESEC_CELL=1.405                                                     184
      CESEC_CELL='CELL FULL WIDTH IN Z'                                   184
C SCI LAYER THICKNESS                                                     185
      ESEC_SCINT=0.4                                                      185
      CESEC_SCINT='SCI LAYER THICKNESS'                                   185
C AMOUNT TO SHIFT SECTION IN Z TO ALIGN WITH AS-BUILT NUMBERS             186
      ESEC_DELTAZ=+0.0336                                                 186
      CESEC_DELTAZ='AMOUNT TO SHIFT SECTION IN Z TO ALIGN WITH AS-BUILT   186
     *NUMBERS'                                                            186
C DEGREES TO SHIFT EPER IN EACH LAYER                                     188
      ESEC_JIGGLE(1)=0                                                    188
      ESEC_JIGGLE(2)=0                                                    188
      ESEC_JIGGLE(3)=0                                                    188
      ESEC_JIGGLE(4)=0                                                    188
      ESEC_JIGGLE(5)=0                                                    188
      ESEC_JIGGLE(6)=0                                                    188
      ESEC_JIGGLE(7)=0                                                    188
      ESEC_JIGGLE(8)=0                                                    188
      ESEC_JIGGLE(9)=0                                                    188
      ESEC_JIGGLE(10)=0                                                   188
      ESEC_JIGGLE(11)=0                                                   188
      ESEC_JIGGLE(12)=0                                                   188
      ESEC_JIGGLE(13)=0                                                   188
      ESEC_JIGGLE(14)=0                                                   188
      ESEC_JIGGLE(15)=0                                                   188
      ESEC_JIGGLE(16)=0                                                   188
      ESEC_JIGGLE(17)=0                                                   188
      ESEC_JIGGLE(18)=0                                                   188
      CESEC_JIGGLE='DEGREES TO SHIFT EPER IN EACH LAYER'                  188
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESEC',    189
     *BPATH_ESEC,IDNUM_ESEC,1, MAP_ESEC,NAMES_ESEC,COMMEN_ESEC,           189
     *BANK_ESEC,LEN_ESEC,LLEN_ESEC,LINK_ESEC,FLAG_ESEC)                   189
C *                                                                       189
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESEC',    189
     *BPATH_ESEC,IDNUM_ESEC,1, MAP_ESEC,NAMES_ESEC,COMMEN_ESEC,           189
     *BANK_ESEC,LEN_ESEC,LLEN_ESEC,LINK_ESEC,FLAG_ESEC)                   189
C *                                                                       190
      CALL AGSBEGM(MODULE,IPRIN)                                          190
      IF (IPRIN.LT.0) RETURN                                              190
C Filling bank ESEC - POSTSHOWER                                          190
      BANK_TITLE='POSTSHOWER'                                             190
      BPATH_ESEC='ESEC*'                                                  190
      IDNUM_ESEC(1)=0                                                     191
C SECTION                                                                 191
      ESEC_ISECT=5                                                        191
      CESEC_ISECT='SECTION'                                               191
C NUMBER OF LAYERS ALONG Z                                                192
      ESEC_NLAYER=1                                                       192
      CESEC_NLAYER='NUMBER OF LAYERS ALONG Z'                             192
C CELL FULL WIDTH IN Z                                                    193
      ESEC_CELL=1.505                                                     193
      CESEC_CELL='CELL FULL WIDTH IN Z'                                   193
C SCI LAYER THICKNESS (5.0MM KURARARY)                                    194
      ESEC_SCINT=0.5                                                      194
      CESEC_SCINT='SCI LAYER THICKNESS (5.0MM KURARARY)'                  194
C AMOUNT TO SHIFT SECTION IN Z TO ALIGN WITH AS-BUILT NUMBERS             195
      ESEC_DELTAZ=+0.036                                                  195
      CESEC_DELTAZ='AMOUNT TO SHIFT SECTION IN Z TO ALIGN WITH AS-BUILT   195
     *NUMBERS'                                                            195
C DEGREES TO SHIFT EPER IN EACH LAYER                                     197
      ESEC_JIGGLE(1)=0                                                    197
      ESEC_JIGGLE(2)=0                                                    197
      ESEC_JIGGLE(3)=0                                                    197
      ESEC_JIGGLE(4)=0                                                    197
      ESEC_JIGGLE(5)=0                                                    197
      ESEC_JIGGLE(6)=0                                                    197
      ESEC_JIGGLE(7)=0                                                    197
      ESEC_JIGGLE(8)=0                                                    197
      ESEC_JIGGLE(9)=0                                                    197
      ESEC_JIGGLE(10)=0                                                   197
      ESEC_JIGGLE(11)=0                                                   197
      ESEC_JIGGLE(12)=0                                                   197
      ESEC_JIGGLE(13)=0                                                   197
      ESEC_JIGGLE(14)=0                                                   197
      ESEC_JIGGLE(15)=0                                                   197
      ESEC_JIGGLE(16)=0                                                   197
      ESEC_JIGGLE(17)=0                                                   197
      ESEC_JIGGLE(18)=0                                                   197
      CESEC_JIGGLE='DEGREES TO SHIFT EPER IN EACH LAYER'                  197
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESEC',    198
     *BPATH_ESEC,IDNUM_ESEC,1, MAP_ESEC,NAMES_ESEC,COMMEN_ESEC,           198
     *BANK_ESEC,LEN_ESEC,LLEN_ESEC,LINK_ESEC,FLAG_ESEC)                   198
C *                                                                       198
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESEC',    198
     *BPATH_ESEC,IDNUM_ESEC,1, MAP_ESEC,NAMES_ESEC,COMMEN_ESEC,           198
     *BANK_ESEC,LEN_ESEC,LLEN_ESEC,LINK_ESEC,FLAG_ESEC)                   198
C *                                                                       199
      CALL AGSBEGM(MODULE,IPRIN)                                          199
      IF (IPRIN.LT.0) RETURN                                              199
C Filling bank EMXG - EM ENDCAP SMD BASIC DATA                            199
      BANK_TITLE='EM ENDCAP SMD BASIC DATA'                               199
      BPATH_EMXG='EMXG*'                                                  199
      IDNUM_EMXG(1)=0                                                     200
C GEOMETRY VERSION                                                        200
      EMXG_VERSION=1                                                      200
      CEMXG_VERSION='GEOMETRY VERSION'                                    200
C SCINTILLATOR STRIP APEX                                                 201
      EMXG_SAPEX=0.7                                                      201
      CEMXG_SAPEX='SCINTILLATOR STRIP APEX'                               201
C SCINTILLATOR STRIP BASE                                                 202
      EMXG_SBASE=1.0                                                      202
      CEMXG_SBASE='SCINTILLATOR STRIP BASE'                               202
C INNER RADIUS OF SMD PLANE                                               203
      EMXG_RIN=77.41                                                      203
      CEMXG_RIN='INNER RADIUS OF SMD PLANE'                               203
C OUTER RADIUS OF SMD PLANE                                               204
      EMXG_ROUT=213.922                                                   204
      CEMXG_ROUT='OUTER RADIUS OF SMD PLANE'                              204
C F4 THICKNESS                                                            205
      EMXG_F4=.15                                                         205
      CEMXG_F4='F4 THICKNESS'                                             205
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EMXG',    206
     *BPATH_EMXG,IDNUM_EMXG,1, MAP_EMXG,NAMES_EMXG,COMMEN_EMXG,           206
     *BANK_EMXG,LEN_EMXG,LLEN_EMXG,LINK_EMXG,FLAG_EMXG)                   206
C *                                                                       206
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EMXG',    206
     *BPATH_EMXG,IDNUM_EMXG,1, MAP_EMXG,NAMES_EMXG,COMMEN_EMXG,           206
     *BANK_EMXG,LEN_EMXG,LLEN_EMXG,LINK_EMXG,FLAG_EMXG)                   206
C *                                                                       207
      CALL AGSBEGM(MODULE,IPRIN)                                          207
      IF (IPRIN.LT.0) RETURN                                              207
C Filling bank EXSE - FIRST SMD SECTION                                   207
      BANK_TITLE='FIRST SMD SECTION'                                      207
      BPATH_EXSE='EXSE*'                                                  207
      IDNUM_EXSE(1)=0                                                     208
C SECTION NUMBER                                                          208
      EXSE_JSECT=1                                                        208
      CEXSE_JSECT='SECTION NUMBER'                                        208
C SECTION WIDTH                                                           209
      EXSE_ZSHIFT=-1.215                                                  209
      CEXSE_ZSHIFT='SECTION WIDTH'                                        209
C 1-V,2-U,3-CUTV,4-CUTU                                                   210
      EXSE_SECTYPE(1)=4                                                   210
      EXSE_SECTYPE(2)=1                                                   210
      EXSE_SECTYPE(3)=0                                                   210
      EXSE_SECTYPE(4)=2                                                   210
      EXSE_SECTYPE(5)=1                                                   210
      EXSE_SECTYPE(6)=0                                                   210
      CEXSE_SECTYPE='1-V,2-U,3-CUTV,4-CUTU'                               210
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EXSE',    211
     *BPATH_EXSE,IDNUM_EXSE,1, MAP_EXSE,NAMES_EXSE,COMMEN_EXSE,           211
     *BANK_EXSE,LEN_EXSE,LLEN_EXSE,LINK_EXSE,FLAG_EXSE)                   211
C *                                                                       211
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EXSE',    211
     *BPATH_EXSE,IDNUM_EXSE,1, MAP_EXSE,NAMES_EXSE,COMMEN_EXSE,           211
     *BANK_EXSE,LEN_EXSE,LLEN_EXSE,LINK_EXSE,FLAG_EXSE)                   211
C *                                                                       212
      CALL AGSBEGM(MODULE,IPRIN)                                          212
      IF (IPRIN.LT.0) RETURN                                              212
C Filling bank EXSE - SECOND SMD SECTION                                  212
      BANK_TITLE='SECOND SMD SECTION'                                     212
      BPATH_EXSE='EXSE*'                                                  212
      IDNUM_EXSE(1)=0                                                     213
C SECTION NUMBER                                                          213
      EXSE_JSECT=2                                                        213
      CEXSE_JSECT='SECTION NUMBER'                                        213
C SECTION WIDTH                                                           214
      EXSE_ZSHIFT=0.                                                      214
      CEXSE_ZSHIFT='SECTION WIDTH'                                        214
C 1-V,2-U,3-CUTV,4-CUTU                                                   215
      EXSE_SECTYPE(1)=0                                                   215
      EXSE_SECTYPE(2)=2                                                   215
      EXSE_SECTYPE(3)=1                                                   215
      EXSE_SECTYPE(4)=0                                                   215
      EXSE_SECTYPE(5)=2                                                   215
      EXSE_SECTYPE(6)=3                                                   215
      CEXSE_SECTYPE='1-V,2-U,3-CUTV,4-CUTU'                               215
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EXSE',    216
     *BPATH_EXSE,IDNUM_EXSE,1, MAP_EXSE,NAMES_EXSE,COMMEN_EXSE,           216
     *BANK_EXSE,LEN_EXSE,LLEN_EXSE,LINK_EXSE,FLAG_EXSE)                   216
C *                                                                       216
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EXSE',    216
     *BPATH_EXSE,IDNUM_EXSE,1, MAP_EXSE,NAMES_EXSE,COMMEN_EXSE,           216
     *BANK_EXSE,LEN_EXSE,LLEN_EXSE,LINK_EXSE,FLAG_EXSE)                   216
C *                                                                       217
      CALL AGSBEGM(MODULE,IPRIN)                                          217
      IF (IPRIN.LT.0) RETURN                                              217
C Filling bank EXSE - THIRD SMD SECTION                                   217
      BANK_TITLE='THIRD SMD SECTION'                                      217
      BPATH_EXSE='EXSE*'                                                  217
      IDNUM_EXSE(1)=0                                                     218
C SECTION NUMBER                                                          218
      EXSE_JSECT=3                                                        218
      CEXSE_JSECT='SECTION NUMBER'                                        218
C SECTION WIDTH                                                           219
      EXSE_ZSHIFT=1.215                                                   219
      CEXSE_ZSHIFT='SECTION WIDTH'                                        219
C 1-V,2-U,3-CUTV,4-CUTU                                                   220
      EXSE_SECTYPE(1)=1                                                   220
      EXSE_SECTYPE(2)=0                                                   220
      EXSE_SECTYPE(3)=2                                                   220
      EXSE_SECTYPE(4)=1                                                   220
      EXSE_SECTYPE(5)=0                                                   220
      EXSE_SECTYPE(6)=2                                                   220
      CEXSE_SECTYPE='1-V,2-U,3-CUTV,4-CUTU'                               220
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EXSE',    221
     *BPATH_EXSE,IDNUM_EXSE,1, MAP_EXSE,NAMES_EXSE,COMMEN_EXSE,           221
     *BANK_EXSE,LEN_EXSE,LLEN_EXSE,LINK_EXSE,FLAG_EXSE)                   221
C *                                                                       221
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EXSE',    221
     *BPATH_EXSE,IDNUM_EXSE,1, MAP_EXSE,NAMES_EXSE,COMMEN_EXSE,           221
     *BANK_EXSE,LEN_EXSE,LLEN_EXSE,LINK_EXSE,FLAG_EXSE)                   221
C *                                                                       222
      CALL AGSBEGM(MODULE,IPRIN)                                          222
      IF (IPRIN.LT.0) RETURN                                              222
C Filling bank ECUT - CUT SELECTION                                       222
      BANK_TITLE='CUT SELECTION'                                          222
      BPATH_ECUT='ECUT*'                                                  222
      IDNUM_ECUT(1)=0                                                     223
C SELECTOR                                                                223
      ECUT_VERSION=1                                                      223
      CECUT_VERSION='SELECTOR'                                            223
C ABSORBER CUTS                                                           224
      ECUT_ABSORBER=0                                                     224
      CECUT_ABSORBER='ABSORBER CUTS'                                      224
C SENSITIVE CUTS                                                          225
      ECUT_SENSITIVE=0                                                    225
      CECUT_SENSITIVE='SENSITIVE CUTS'                                    225
C MEH                                                                     226
      ECUT_BLAH=1                                                         226
      CECUT_BLAH='MEH'                                                    226
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ECUT',    227
     *BPATH_ECUT,IDNUM_ECUT,1, MAP_ECUT,NAMES_ECUT,COMMEN_ECUT,           227
     *BANK_ECUT,LEN_ECUT,LLEN_ECUT,LINK_ECUT,FLAG_ECUT)                   227
C *                                                                       227
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ECUT',    227
     *BPATH_ECUT,IDNUM_ECUT,1, MAP_ECUT,NAMES_ECUT,COMMEN_ECUT,           227
     *BANK_ECUT,LEN_ECUT,LLEN_ECUT,LINK_ECUT,FLAG_ECUT)                   227
C *                                                                       228
      CALL AGSBEGM(MODULE,IPRIN)                                          228
      IF (IPRIN.LT.0) RETURN                                              228
C Filling bank EABS - THE VALUES BELOW ARE THE UNTUNED DEFAULTS IN THE O  228
C RIGINAL GEOMETRY                                                        228
      BANK_TITLE='THE VALUES BELOW ARE THE UNTUNED DEFAULTS IN THE ORIGI  228
     *NAL GEOMETRY'                                                       228
      BPATH_EABS='EABS*'                                                  228
      IDNUM_EABS(1)=0                                                     229
C VERSIONING                                                              229
      EABS_VERSION=0                                                      229
      CEABS_VERSION='VERSIONING'                                          229
C GAMMA TRANSPORT CUT                                                     230
      EABS_CUTGAM=80 *1.0E-6                                              230
      CEABS_CUTGAM='GAMMA TRANSPORT CUT'                                  230
C ELECTRON TRANSPORT CUT                                                  231
      EABS_CUTELE=1 *1.0E-3                                               231
      CEABS_CUTELE='ELECTRON TRANSPORT CUT'                               231
C HADRON TRANSPORT CUT                                                    232
      EABS_CUTHAD=1 *1.0E-3                                               232
      CEABS_CUTHAD='HADRON TRANSPORT CUT'                                 232
C NEUTRON TRANSPORT CUT                                                   233
      EABS_CUTNEU=1 *1.0E-3                                               233
      CEABS_CUTNEU='NEUTRON TRANSPORT CUT'                                233
C MUON TRANSPORT CUT                                                      234
      EABS_CUTMUO=1 *1.0E-3                                               234
      CEABS_CUTMUO='MUON TRANSPORT CUT'                                   234
C ELECTRON DELTA RAY CUT                                                  235
      EABS_DCUTE=1 *1.0E-3                                                235
      CEABS_DCUTE='ELECTRON DELTA RAY CUT'                                235
C MUON DELTA RAY CUT                                                      236
      EABS_DCUTM=1 *1.0E-3                                                236
      CEABS_DCUTM='MUON DELTA RAY CUT'                                    236
C ELECTRON BREM CUT                                                       237
      EABS_BCUTE=100 *1.0E-6                                              237
      CEABS_BCUTE='ELECTRON BREM CUT'                                     237
C MUON BREM CUT                                                           238
      EABS_BCUTM=1 *1.0E-3                                                238
      CEABS_BCUTM='MUON BREM CUT'                                         238
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EABS',    239
     *BPATH_EABS,IDNUM_EABS,1, MAP_EABS,NAMES_EABS,COMMEN_EABS,           239
     *BANK_EABS,LEN_EABS,LLEN_EABS,LINK_EABS,FLAG_EABS)                   239
C *                                                                       239
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EABS',    239
     *BPATH_EABS,IDNUM_EABS,1, MAP_EABS,NAMES_EABS,COMMEN_EABS,           239
     *BANK_EABS,LEN_EABS,LLEN_EABS,LINK_EABS,FLAG_EABS)                   239
C *                                                                       240
      CALL AGSBEGM(MODULE,IPRIN)                                          240
      IF (IPRIN.LT.0) RETURN                                              240
C Filling bank EABS - EM CUTS IN ABSORBING MATERIAL                       240
      BANK_TITLE='EM CUTS IN ABSORBING MATERIAL'                          240
      BPATH_EABS='EABS*'                                                  240
      IDNUM_EABS(1)=0                                                     241
C VERSIONING                                                              241
      EABS_VERSION=1                                                      241
      CEABS_VERSION='VERSIONING'                                          241
C GAMMA TRANSPORT CUT                                                     242
      EABS_CUTGAM=10 *1.0E-6                                              242
      CEABS_CUTGAM='GAMMA TRANSPORT CUT'                                  242
C ELECTRON TRANSPORT CUT                                                  243
      EABS_CUTELE=10 *1.0E-6                                              243
      CEABS_CUTELE='ELECTRON TRANSPORT CUT'                               243
C HADRON TRANSPORT CUT                                                    244
      EABS_CUTHAD=1 *1.0E-3                                               244
      CEABS_CUTHAD='HADRON TRANSPORT CUT'                                 244
C NEUTRON TRANSPORT CUT                                                   245
      EABS_CUTNEU=1 *1.0E-3                                               245
      CEABS_CUTNEU='NEUTRON TRANSPORT CUT'                                245
C MUON TRANSPORT CUT                                                      246
      EABS_CUTMUO=1 *1.0E-3                                               246
      CEABS_CUTMUO='MUON TRANSPORT CUT'                                   246
C ELECTRON DELTA RAY CUT                                                  247
      EABS_DCUTE=10 *1.0E-6                                               247
      CEABS_DCUTE='ELECTRON DELTA RAY CUT'                                247
C MUON DELTA RAY CUT                                                      248
      EABS_DCUTM=10 *1.0E-6                                               248
      CEABS_DCUTM='MUON DELTA RAY CUT'                                    248
C ELECTRON BREM CUT                                                       249
      EABS_BCUTE=10 *1.0E-6                                               249
      CEABS_BCUTE='ELECTRON BREM CUT'                                     249
C MUON BREM CUT                                                           250
      EABS_BCUTM=10 *1.0E-6                                               250
      CEABS_BCUTM='MUON BREM CUT'                                         250
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EABS',    251
     *BPATH_EABS,IDNUM_EABS,1, MAP_EABS,NAMES_EABS,COMMEN_EABS,           251
     *BANK_EABS,LEN_EABS,LLEN_EABS,LINK_EABS,FLAG_EABS)                   251
C *                                                                       251
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EABS',    251
     *BPATH_EABS,IDNUM_EABS,1, MAP_EABS,NAMES_EABS,COMMEN_EABS,           251
     *BANK_EABS,LEN_EABS,LLEN_EABS,LINK_EABS,FLAG_EABS)                   251
C *                                                                       252
      CALL AGSBEGM(MODULE,IPRIN)                                          252
      IF (IPRIN.LT.0) RETURN                                              252
C Filling bank EABS - EM CUTS IN ABSORBING MATERIAL                       252
      BANK_TITLE='EM CUTS IN ABSORBING MATERIAL'                          252
      BPATH_EABS='EABS*'                                                  252
      IDNUM_EABS(1)=0                                                     253
C VERSIONING                                                              253
      EABS_VERSION=2                                                      253
      CEABS_VERSION='VERSIONING'                                          253
C GAMMA TRANSPORT CUT                                                     254
      EABS_CUTGAM=30 *1.0E-6                                              254
      CEABS_CUTGAM='GAMMA TRANSPORT CUT'                                  254
C ELECTRON TRANSPORT CUT                                                  255
      EABS_CUTELE=30 *1.0E-6                                              255
      CEABS_CUTELE='ELECTRON TRANSPORT CUT'                               255
C HADRON TRANSPORT CUT                                                    256
      EABS_CUTHAD=1 *1.0E-3                                               256
      CEABS_CUTHAD='HADRON TRANSPORT CUT'                                 256
C NEUTRON TRANSPORT CUT                                                   257
      EABS_CUTNEU=1 *1.0E-3                                               257
      CEABS_CUTNEU='NEUTRON TRANSPORT CUT'                                257
C MUON TRANSPORT CUT                                                      258
      EABS_CUTMUO=1 *1.0E-3                                               258
      CEABS_CUTMUO='MUON TRANSPORT CUT'                                   258
C ELECTRON DELTA RAY CUT                                                  259
      EABS_DCUTE=30 *1.0E-6                                               259
      CEABS_DCUTE='ELECTRON DELTA RAY CUT'                                259
C MUON DELTA RAY CUT                                                      260
      EABS_DCUTM=30 *1.0E-6                                               260
      CEABS_DCUTM='MUON DELTA RAY CUT'                                    260
C ELECTRON BREM CUT                                                       261
      EABS_BCUTE=30 *1.0E-6                                               261
      CEABS_BCUTE='ELECTRON BREM CUT'                                     261
C MUON BREM CUT                                                           262
      EABS_BCUTM=30 *1.0E-6                                               262
      CEABS_BCUTM='MUON BREM CUT'                                         262
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EABS',    263
     *BPATH_EABS,IDNUM_EABS,1, MAP_EABS,NAMES_EABS,COMMEN_EABS,           263
     *BANK_EABS,LEN_EABS,LLEN_EABS,LINK_EABS,FLAG_EABS)                   263
C *                                                                       263
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EABS',    263
     *BPATH_EABS,IDNUM_EABS,1, MAP_EABS,NAMES_EABS,COMMEN_EABS,           263
     *BANK_EABS,LEN_EABS,LLEN_EABS,LINK_EABS,FLAG_EABS)                   263
C *                                                                       264
      CALL AGSBEGM(MODULE,IPRIN)                                          264
      IF (IPRIN.LT.0) RETURN                                              264
C Filling bank EABS - EM CUTS IN ABSORBING MATERIAL                       264
      BANK_TITLE='EM CUTS IN ABSORBING MATERIAL'                          264
      BPATH_EABS='EABS*'                                                  264
      IDNUM_EABS(1)=0                                                     265
C VERSIONING                                                              265
      EABS_VERSION=3                                                      265
      CEABS_VERSION='VERSIONING'                                          265
C GAMMA TRANSPORT CUT                                                     266
      EABS_CUTGAM=100 *1.0E-6                                             266
      CEABS_CUTGAM='GAMMA TRANSPORT CUT'                                  266
C ELECTRON TRANSPORT CUT                                                  267
      EABS_CUTELE=100 *1.0E-6                                             267
      CEABS_CUTELE='ELECTRON TRANSPORT CUT'                               267
C HADRON TRANSPORT CUT                                                    268
      EABS_CUTHAD=1 *1.0E-3                                               268
      CEABS_CUTHAD='HADRON TRANSPORT CUT'                                 268
C NEUTRON TRANSPORT CUT                                                   269
      EABS_CUTNEU=1 *1.0E-3                                               269
      CEABS_CUTNEU='NEUTRON TRANSPORT CUT'                                269
C MUON TRANSPORT CUT                                                      270
      EABS_CUTMUO=1 *1.0E-3                                               270
      CEABS_CUTMUO='MUON TRANSPORT CUT'                                   270
C ELECTRON DELTA RAY CUT                                                  271
      EABS_DCUTE=100 *1.0E-6                                              271
      CEABS_DCUTE='ELECTRON DELTA RAY CUT'                                271
C MUON DELTA RAY CUT                                                      272
      EABS_DCUTM=100 *1.0E-6                                              272
      CEABS_DCUTM='MUON DELTA RAY CUT'                                    272
C ELECTRON BREM CUT                                                       273
      EABS_BCUTE=100 *1.0E-6                                              273
      CEABS_BCUTE='ELECTRON BREM CUT'                                     273
C MUON BREM CUT                                                           274
      EABS_BCUTM=100 *1.0E-6                                              274
      CEABS_BCUTM='MUON BREM CUT'                                         274
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EABS',    275
     *BPATH_EABS,IDNUM_EABS,1, MAP_EABS,NAMES_EABS,COMMEN_EABS,           275
     *BANK_EABS,LEN_EABS,LLEN_EABS,LINK_EABS,FLAG_EABS)                   275
C *                                                                       275
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EABS',    275
     *BPATH_EABS,IDNUM_EABS,1, MAP_EABS,NAMES_EABS,COMMEN_EABS,           275
     *BANK_EABS,LEN_EABS,LLEN_EABS,LINK_EABS,FLAG_EABS)                   275
C *                                                                       276
      CALL AGSBEGM(MODULE,IPRIN)                                          276
      IF (IPRIN.LT.0) RETURN                                              276
C Filling bank EABS - EM CUTS IN ABSORBING MATERIAL                       276
      BANK_TITLE='EM CUTS IN ABSORBING MATERIAL'                          276
      BPATH_EABS='EABS*'                                                  276
      IDNUM_EABS(1)=0                                                     277
C VERSIONING                                                              277
      EABS_VERSION=4                                                      277
      CEABS_VERSION='VERSIONING'                                          277
C GAMMA TRANSPORT CUT                                                     278
      EABS_CUTGAM=1 *1.0E-3                                               278
      CEABS_CUTGAM='GAMMA TRANSPORT CUT'                                  278
C ELECTRON TRANSPORT CUT                                                  279
      EABS_CUTELE=1 *1.0E-3                                               279
      CEABS_CUTELE='ELECTRON TRANSPORT CUT'                               279
C HADRON TRANSPORT CUT                                                    280
      EABS_CUTHAD=1 *1.0E-3                                               280
      CEABS_CUTHAD='HADRON TRANSPORT CUT'                                 280
C NEUTRON TRANSPORT CUT                                                   281
      EABS_CUTNEU=1 *1.0E-3                                               281
      CEABS_CUTNEU='NEUTRON TRANSPORT CUT'                                281
C MUON TRANSPORT CUT                                                      282
      EABS_CUTMUO=1 *1.0E-3                                               282
      CEABS_CUTMUO='MUON TRANSPORT CUT'                                   282
C ELECTRON DELTA RAY CUT                                                  283
      EABS_DCUTE=1 *1.0E-3                                                283
      CEABS_DCUTE='ELECTRON DELTA RAY CUT'                                283
C MUON DELTA RAY CUT                                                      284
      EABS_DCUTM=1 *1.0E-3                                                284
      CEABS_DCUTM='MUON DELTA RAY CUT'                                    284
C ELECTRON BREM CUT                                                       285
      EABS_BCUTE=1 *1.0E-3                                                285
      CEABS_BCUTE='ELECTRON BREM CUT'                                     285
C MUON BREM CUT                                                           286
      EABS_BCUTM=1 *1.0E-3                                                286
      CEABS_BCUTM='MUON BREM CUT'                                         286
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EABS',    287
     *BPATH_EABS,IDNUM_EABS,1, MAP_EABS,NAMES_EABS,COMMEN_EABS,           287
     *BANK_EABS,LEN_EABS,LLEN_EABS,LINK_EABS,FLAG_EABS)                   287
C *                                                                       287
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EABS',    287
     *BPATH_EABS,IDNUM_EABS,1, MAP_EABS,NAMES_EABS,COMMEN_EABS,           287
     *BANK_EABS,LEN_EABS,LLEN_EABS,LINK_EABS,FLAG_EABS)                   287
C *                                                                       288
      CALL AGSBEGM(MODULE,IPRIN)                                          288
      IF (IPRIN.LT.0) RETURN                                              288
C Filling bank ESEN - THE VALUES BELOW ARE THE UNTUNED DEFAULTS IN THE O  288
C RIGINAL GEOMETRY                                                        288
      BANK_TITLE='THE VALUES BELOW ARE THE UNTUNED DEFAULTS IN THE ORIGI  288
     *NAL GEOMETRY'                                                       288
      BPATH_ESEN='ESEN*'                                                  288
      IDNUM_ESEN(1)=0                                                     289
C VERSIONING                                                              289
      ESEN_VERSION=0                                                      289
      CESEN_VERSION='VERSIONING'                                          289
C GAMMA TRANSPORT CUT                                                     290
      ESEN_CUTGAM=80 *1.0E-6                                              290
      CESEN_CUTGAM='GAMMA TRANSPORT CUT'                                  290
C ELECTRON TRANSPORT CUT                                                  291
      ESEN_CUTELE=1 *1.0E-3                                               291
      CESEN_CUTELE='ELECTRON TRANSPORT CUT'                               291
C HADRON TRANSPORT CUT                                                    292
      ESEN_CUTHAD=1 *1.0E-3                                               292
      CESEN_CUTHAD='HADRON TRANSPORT CUT'                                 292
C NEUTRON TRANSPORT CUT                                                   293
      ESEN_CUTNEU=1 *1.0E-3                                               293
      CESEN_CUTNEU='NEUTRON TRANSPORT CUT'                                293
C MUON TRANSPORT CUT                                                      294
      ESEN_CUTMUO=1 *1.0E-3                                               294
      CESEN_CUTMUO='MUON TRANSPORT CUT'                                   294
C ELECTRON DELTA RAY CUT                                                  295
      ESEN_DCUTE=1 *1.0E-3                                                295
      CESEN_DCUTE='ELECTRON DELTA RAY CUT'                                295
C MUON DELTA RAY CUT                                                      296
      ESEN_DCUTM=1 *1.0E-3                                                296
      CESEN_DCUTM='MUON DELTA RAY CUT'                                    296
C ELECTRON BREM CUT                                                       297
      ESEN_BCUTE=100 *1.0E-6                                              297
      CESEN_BCUTE='ELECTRON BREM CUT'                                     297
C MUON BREM CUT                                                           298
      ESEN_BCUTM=1 *1.0E-3                                                298
      CESEN_BCUTM='MUON BREM CUT'                                         298
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESEN',    299
     *BPATH_ESEN,IDNUM_ESEN,1, MAP_ESEN,NAMES_ESEN,COMMEN_ESEN,           299
     *BANK_ESEN,LEN_ESEN,LLEN_ESEN,LINK_ESEN,FLAG_ESEN)                   299
C *                                                                       299
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESEN',    299
     *BPATH_ESEN,IDNUM_ESEN,1, MAP_ESEN,NAMES_ESEN,COMMEN_ESEN,           299
     *BANK_ESEN,LEN_ESEN,LLEN_ESEN,LINK_ESEN,FLAG_ESEN)                   299
C *                                                                       300
      CALL AGSBEGM(MODULE,IPRIN)                                          300
      IF (IPRIN.LT.0) RETURN                                              300
C Filling bank ESEN - EM CUTS IN ABSORBING MATERIAL                       300
      BANK_TITLE='EM CUTS IN ABSORBING MATERIAL'                          300
      BPATH_ESEN='ESEN*'                                                  300
      IDNUM_ESEN(1)=0                                                     301
C VERSIONING                                                              301
      ESEN_VERSION=1                                                      301
      CESEN_VERSION='VERSIONING'                                          301
C GAMMA TRANSPORT CUT                                                     302
      ESEN_CUTGAM=10 *1.0E-6                                              302
      CESEN_CUTGAM='GAMMA TRANSPORT CUT'                                  302
C ELECTRON TRANSPORT CUT                                                  303
      ESEN_CUTELE=10 *1.0E-6                                              303
      CESEN_CUTELE='ELECTRON TRANSPORT CUT'                               303
C HADRON TRANSPORT CUT                                                    304
      ESEN_CUTHAD=1 *1.0E-3                                               304
      CESEN_CUTHAD='HADRON TRANSPORT CUT'                                 304
C NEUTRON TRANSPORT CUT                                                   305
      ESEN_CUTNEU=1 *1.0E-3                                               305
      CESEN_CUTNEU='NEUTRON TRANSPORT CUT'                                305
C MUON TRANSPORT CUT                                                      306
      ESEN_CUTMUO=1 *1.0E-3                                               306
      CESEN_CUTMUO='MUON TRANSPORT CUT'                                   306
C ELECTRON DELTA RAY CUT                                                  307
      ESEN_DCUTE=10 *1.0E-6                                               307
      CESEN_DCUTE='ELECTRON DELTA RAY CUT'                                307
C MUON DELTA RAY CUT                                                      308
      ESEN_DCUTM=10 *1.0E-6                                               308
      CESEN_DCUTM='MUON DELTA RAY CUT'                                    308
C ELECTRON BREM CUT                                                       309
      ESEN_BCUTE=10 *1.0E-6                                               309
      CESEN_BCUTE='ELECTRON BREM CUT'                                     309
C MUON BREM CUT                                                           310
      ESEN_BCUTM=10 *1.0E-6                                               310
      CESEN_BCUTM='MUON BREM CUT'                                         310
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESEN',    311
     *BPATH_ESEN,IDNUM_ESEN,1, MAP_ESEN,NAMES_ESEN,COMMEN_ESEN,           311
     *BANK_ESEN,LEN_ESEN,LLEN_ESEN,LINK_ESEN,FLAG_ESEN)                   311
C *                                                                       311
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESEN',    311
     *BPATH_ESEN,IDNUM_ESEN,1, MAP_ESEN,NAMES_ESEN,COMMEN_ESEN,           311
     *BANK_ESEN,LEN_ESEN,LLEN_ESEN,LINK_ESEN,FLAG_ESEN)                   311
C *                                                                       312
      CALL AGSBEGM(MODULE,IPRIN)                                          312
      IF (IPRIN.LT.0) RETURN                                              312
C Filling bank ESEN - EM CUTS IN ABSORBING MATERIAL                       312
      BANK_TITLE='EM CUTS IN ABSORBING MATERIAL'                          312
      BPATH_ESEN='ESEN*'                                                  312
      IDNUM_ESEN(1)=0                                                     313
C VERSIONING                                                              313
      ESEN_VERSION=2                                                      313
      CESEN_VERSION='VERSIONING'                                          313
C GAMMA TRANSPORT CUT                                                     314
      ESEN_CUTGAM=30 *1.0E-6                                              314
      CESEN_CUTGAM='GAMMA TRANSPORT CUT'                                  314
C ELECTRON TRANSPORT CUT                                                  315
      ESEN_CUTELE=30 *1.0E-6                                              315
      CESEN_CUTELE='ELECTRON TRANSPORT CUT'                               315
C HADRON TRANSPORT CUT                                                    316
      ESEN_CUTHAD=1 *1.0E-3                                               316
      CESEN_CUTHAD='HADRON TRANSPORT CUT'                                 316
C NEUTRON TRANSPORT CUT                                                   317
      ESEN_CUTNEU=1 *1.0E-3                                               317
      CESEN_CUTNEU='NEUTRON TRANSPORT CUT'                                317
C MUON TRANSPORT CUT                                                      318
      ESEN_CUTMUO=1 *1.0E-3                                               318
      CESEN_CUTMUO='MUON TRANSPORT CUT'                                   318
C ELECTRON DELTA RAY CUT                                                  319
      ESEN_DCUTE=30 *1.0E-6                                               319
      CESEN_DCUTE='ELECTRON DELTA RAY CUT'                                319
C MUON DELTA RAY CUT                                                      320
      ESEN_DCUTM=30 *1.0E-6                                               320
      CESEN_DCUTM='MUON DELTA RAY CUT'                                    320
C ELECTRON BREM CUT                                                       321
      ESEN_BCUTE=30 *1.0E-6                                               321
      CESEN_BCUTE='ELECTRON BREM CUT'                                     321
C MUON BREM CUT                                                           322
      ESEN_BCUTM=30 *1.0E-6                                               322
      CESEN_BCUTM='MUON BREM CUT'                                         322
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESEN',    323
     *BPATH_ESEN,IDNUM_ESEN,1, MAP_ESEN,NAMES_ESEN,COMMEN_ESEN,           323
     *BANK_ESEN,LEN_ESEN,LLEN_ESEN,LINK_ESEN,FLAG_ESEN)                   323
C *                                                                       323
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESEN',    323
     *BPATH_ESEN,IDNUM_ESEN,1, MAP_ESEN,NAMES_ESEN,COMMEN_ESEN,           323
     *BANK_ESEN,LEN_ESEN,LLEN_ESEN,LINK_ESEN,FLAG_ESEN)                   323
C *                                                                       324
      CALL AGSBEGM(MODULE,IPRIN)                                          324
      IF (IPRIN.LT.0) RETURN                                              324
C Filling bank ESEN - EM CUTS IN ABSORBING MATERIAL                       324
      BANK_TITLE='EM CUTS IN ABSORBING MATERIAL'                          324
      BPATH_ESEN='ESEN*'                                                  324
      IDNUM_ESEN(1)=0                                                     325
C VERSIONING                                                              325
      ESEN_VERSION=3                                                      325
      CESEN_VERSION='VERSIONING'                                          325
C GAMMA TRANSPORT CUT                                                     326
      ESEN_CUTGAM=100 *1.0E-6                                             326
      CESEN_CUTGAM='GAMMA TRANSPORT CUT'                                  326
C ELECTRON TRANSPORT CUT                                                  327
      ESEN_CUTELE=100 *1.0E-6                                             327
      CESEN_CUTELE='ELECTRON TRANSPORT CUT'                               327
C HADRON TRANSPORT CUT                                                    328
      ESEN_CUTHAD=1 *1.0E-3                                               328
      CESEN_CUTHAD='HADRON TRANSPORT CUT'                                 328
C NEUTRON TRANSPORT CUT                                                   329
      ESEN_CUTNEU=1 *1.0E-3                                               329
      CESEN_CUTNEU='NEUTRON TRANSPORT CUT'                                329
C MUON TRANSPORT CUT                                                      330
      ESEN_CUTMUO=1 *1.0E-3                                               330
      CESEN_CUTMUO='MUON TRANSPORT CUT'                                   330
C ELECTRON DELTA RAY CUT                                                  331
      ESEN_DCUTE=100 *1.0E-6                                              331
      CESEN_DCUTE='ELECTRON DELTA RAY CUT'                                331
C MUON DELTA RAY CUT                                                      332
      ESEN_DCUTM=100 *1.0E-6                                              332
      CESEN_DCUTM='MUON DELTA RAY CUT'                                    332
C ELECTRON BREM CUT                                                       333
      ESEN_BCUTE=100 *1.0E-6                                              333
      CESEN_BCUTE='ELECTRON BREM CUT'                                     333
C MUON BREM CUT                                                           334
      ESEN_BCUTM=100 *1.0E-6                                              334
      CESEN_BCUTM='MUON BREM CUT'                                         334
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESEN',    335
     *BPATH_ESEN,IDNUM_ESEN,1, MAP_ESEN,NAMES_ESEN,COMMEN_ESEN,           335
     *BANK_ESEN,LEN_ESEN,LLEN_ESEN,LINK_ESEN,FLAG_ESEN)                   335
C *                                                                       335
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESEN',    335
     *BPATH_ESEN,IDNUM_ESEN,1, MAP_ESEN,NAMES_ESEN,COMMEN_ESEN,           335
     *BANK_ESEN,LEN_ESEN,LLEN_ESEN,LINK_ESEN,FLAG_ESEN)                   335
C *                                                                       336
      CALL AGSBEGM(MODULE,IPRIN)                                          336
      IF (IPRIN.LT.0) RETURN                                              336
C Filling bank ESEN - EM CUTS IN ABSORBING MATERIAL                       336
      BANK_TITLE='EM CUTS IN ABSORBING MATERIAL'                          336
      BPATH_ESEN='ESEN*'                                                  336
      IDNUM_ESEN(1)=0                                                     337
C VERSIONING                                                              337
      ESEN_VERSION=4                                                      337
      CESEN_VERSION='VERSIONING'                                          337
C GAMMA TRANSPORT CUT                                                     338
      ESEN_CUTGAM=1 *1.0E-3                                               338
      CESEN_CUTGAM='GAMMA TRANSPORT CUT'                                  338
C ELECTRON TRANSPORT CUT                                                  339
      ESEN_CUTELE=1 *1.0E-3                                               339
      CESEN_CUTELE='ELECTRON TRANSPORT CUT'                               339
C HADRON TRANSPORT CUT                                                    340
      ESEN_CUTHAD=1 *1.0E-3                                               340
      CESEN_CUTHAD='HADRON TRANSPORT CUT'                                 340
C NEUTRON TRANSPORT CUT                                                   341
      ESEN_CUTNEU=1 *1.0E-3                                               341
      CESEN_CUTNEU='NEUTRON TRANSPORT CUT'                                341
C MUON TRANSPORT CUT                                                      342
      ESEN_CUTMUO=1 *1.0E-3                                               342
      CESEN_CUTMUO='MUON TRANSPORT CUT'                                   342
C ELECTRON DELTA RAY CUT                                                  343
      ESEN_DCUTE=1 *1.0E-3                                                343
      CESEN_DCUTE='ELECTRON DELTA RAY CUT'                                343
C MUON DELTA RAY CUT                                                      344
      ESEN_DCUTM=1 *1.0E-3                                                344
      CESEN_DCUTM='MUON DELTA RAY CUT'                                    344
C ELECTRON BREM CUT                                                       345
      ESEN_BCUTE=1 *1.0E-3                                                345
      CESEN_BCUTE='ELECTRON BREM CUT'                                     345
C MUON BREM CUT                                                           346
      ESEN_BCUTM=1 *1.0E-3                                                346
      CESEN_BCUTM='MUON BREM CUT'                                         346
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESEN',    347
     *BPATH_ESEN,IDNUM_ESEN,1, MAP_ESEN,NAMES_ESEN,COMMEN_ESEN,           347
     *BANK_ESEN,LEN_ESEN,LLEN_ESEN,LINK_ESEN,FLAG_ESEN)                   347
C *                                                                       347
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'ESEN',    347
     *BPATH_ESEN,IDNUM_ESEN,1, MAP_ESEN,NAMES_ESEN,COMMEN_ESEN,           347
     *BANK_ESEN,LEN_ESEN,LLEN_ESEN,LINK_ESEN,FLAG_ESEN)                   347
C use default EMCG bank                                                   348
      BPATH_EMCG='EMCG*'                                                  348
      IDNUM_EMCG(1)=0                                                     348
      CALL AGDGETP('ECALGEO6','*',-999,'EMCG',' ',BANK_EMCG, BPATH_EMCG,  348
     *IDNUM_EMCG,1,MAP_EMCG,NAMES_EMCG,BANK_EMCG,LEN_EMCG,LLEN_EMCG,      348
     *LINK_EMCG,FLAG_EMCG)                                                348
C use EMCS bank                                                           349
      BPATH_EMCS='EMCS*'                                                  349
      IDNUM_EMCS(1)=0                                                     349
      EMCS_VERSION=2                                                      349
      CALL AGDGETP('ECALGEO6','*',-999,'EMCS',NEMCS_VERSION,              349
     *RE_EMCS_VERSION,BPATH_EMCS,IDNUM_EMCS,1,MAP_EMCS,NAMES_EMCS,        349
     *BANK_EMCS,LEN_EMCS,LLEN_EMCS,LINK_EMCS,FLAG_EMCS)                   349
C use default EETR bank                                                   350
      BPATH_EETR='EETR*'                                                  350
      IDNUM_EETR(1)=0                                                     350
      CALL AGDGETP('ECALGEO6','*',-999,'EETR',' ',BANK_EETR, BPATH_EETR,  350
     *IDNUM_EETR,1,MAP_EETR,NAMES_EETR,BANK_EETR,LEN_EETR,LLEN_EETR,      350
     *LINK_EETR,FLAG_EETR)                                                350
C use ECUT bank                                                           351
      BPATH_ECUT='ECUT*'                                                  351
      IDNUM_ECUT(1)=0                                                     351
      ECUT_VERSION=1                                                      351
      CALL AGDGETP('ECALGEO6','*',-999,'ECUT',NECUT_VERSION,              351
     *RE_ECUT_VERSION,BPATH_ECUT,IDNUM_ECUT,1,MAP_ECUT,NAMES_ECUT,        351
     *BANK_ECUT,LEN_ECUT,LLEN_ECUT,LINK_ECUT,FLAG_ECUT)                   351
C use EABS bank                                                           352
      BPATH_EABS='EABS*'                                                  352
      IDNUM_EABS(1)=0                                                     352
      EABS_VERSION=ECUT_ABSORBER                                          352
      CALL AGDGETP('ECALGEO6','*',-999,'EABS',NEABS_VERSION,              352
     *RE_EABS_VERSION,BPATH_EABS,IDNUM_EABS,1,MAP_EABS,NAMES_EABS,        352
     *BANK_EABS,LEN_EABS,LLEN_EABS,LINK_EABS,FLAG_EABS)                   352
C use ESEN bank                                                           353
      BPATH_ESEN='ESEN*'                                                  353
      IDNUM_ESEN(1)=0                                                     353
      ESEN_VERSION=ECUT_SENSITIVE                                         353
      CALL AGDGETP('ECALGEO6','*',-999,'ESEN',NESEN_VERSION,              353
     *RE_ESEN_VERSION,BPATH_ESEN,IDNUM_ESEN,1,MAP_ESEN,NAMES_ESEN,        353
     *BANK_ESEN,LEN_ESEN,LLEN_ESEN,LINK_ESEN,FLAG_ESEN)                   353
C 1/TAN(30DEG) = SQ3                                                      354
      SQ3 = SQRT(3.)                                                      354
      SQ2 = SQRT(2.)                                                      355
C CENTER OF THE CALORIMETER                                               356
      CENTER = (EMCS_ZORG+EMCS_ZEND)/2                                    356
C THINK THIS IS ANGLE POINTING TO TOP OF CALO                             357
      TAN_UPP = TANF(EMCS_ETAMIN)                                         357
C THINK THIS IS ANGLE POINTING TO BOT OF CALO                             358
      TAN_LOW = TANF(EMCS_ETAMAX)                                         358
C ??                                                                      359
      RTH = SQRT(1. + TAN_LOW*TAN_LOW)                                    359
C ??                                                                      360
      RSHIFT = EMCS_HUB * RTH                                             360
      DUP = EMCS_RMSHIFT*TAN_UPP                                          361
      DD = EMCS_RMSHIFT*RTH                                               362
      D2 = RSHIFT + DD                                                    363
C THICKNESS OF RADIATOR ASSEMBLY                                          364
      RADIATOR = EMCS_PBPLATE + 2*EMCS_LAMPLATE                           364
C SINGLE ENDCAP SECTOR                                                    365
      DPHI = (EMCS_PHIMAX-EMCS_PHIMIN)/EMCS_NSECTOR                       365
      WRITE(LOUT,5010)EMCG_VERSION                                        365
5010  FORMAT('ecalgeo version: ', F4.2)                                   367
      WRITE(LOUT,5020)ECUT_ABSORBER                                       367
5020  FORMAT(' absorber cuts=', F4.2)                                     369
      WRITE(LOUT,5030)ECUT_SENSITIVE                                      369
5030  FORMAT(' sensitive cuts=', F4.2)                                    371
C Check EMCG_ONOFF>0                                                      372
      IF (EMCG_ONOFF.GT.0) THEN                                           372
      CALL AGSBEGM(MODULE,IPRIN)                                          373
C Check IPRIN<0                                                           373
      IF (IPRIN.LT.0) RETURN                                              373
C Create ECAL :                                                           373
C Call To Local Procedure ECAL                                            373
      ASSIGN 5050 TO BLOCK_ECAL                                           373
      GO TO  5040                                                         373
5050  CONTINUE                                                            373
C *                                                                       373
C Check EMCG_ONOFF==1|EMCG_ONOFF==3                                       374
      IF (EMCG_ONOFF.EQ.1.OR.EMCG_ONOFF.EQ.3) THEN                        374
C Geant POSITION : ECAL ===> MOTHER='CAVE' Z=+CENTER <===                 375
      CALL AGSRESET                                                       375
      AG_MOTHER = 'CAVE'                                                  375
      AG_Z = +CENTER                                                      375
      AG_TITLE = 'ECAL'                                                   375
      AG_EXNAME = 'POSITION'                                              375
      AG_PARLIST = 'MOTHER_Z'                                             375
      CALL AXPOSITION                                                     375
C *                                                                       375
      ENDIF                                                               375
C Check SECTION>EMCS_ZEND                                                 377
      IF (SECTION.GT.EMCS_ZEND) THEN                                      377
C Check IPRIN>=1                                                          377
      IF (IPRIN.GE.1) WRITE(LOUT,5060)SECTION,EMCS_ZEND                   377
5060  FORMAT(' ECALGEO error: sum of sections exceeds maximum ',2F12.4)   379
      ENDIF                                                               380
C Check EMCG_ONOFF==2|EMCG_ONOFF==3                                       381
      IF (EMCG_ONOFF.EQ.2.OR.EMCG_ONOFF.EQ.3) THEN                        381
C Geant POSITION : ECAL ===> MOTHER='CAVE' Z=-CENTER THETAZ=180 <===      382
      CALL AGSRESET                                                       382
      AG_MOTHER = 'CAVE'                                                  382
      AG_Z = -CENTER                                                      382
      AG_THETAZ = 180                                                     382
      AG_TITLE = 'ECAL'                                                   382
      AG_EXNAME = 'POSITION'                                              382
      AG_PARLIST = 'MOTHER_Z_THETAZ'                                      382
      CALL AXPOSITION                                                     382
C *                                                                       382
      ENDIF                                                               382
      ENDIF                                                               384
C Check IPRIN>=1                                                          384
      IF (IPRIN.GE.1) WRITE(LOUT,5070)                                    384
5070  FORMAT('ECALGEO finished')                                          386
C ----------------------------------------------------------------------  387
C ------------ ECAL                                                       387
      CALL AGSENDM                                                        388
      RETURN                                                              388
C Begin Local Procedure ECAL                                              388
C ECAL IS ONE EMC ENDCAP WHEEL                                            388
5040  CONTINUE                                                            388
         AG_TITLE='ECAL'                                                  388
         AG_MODULE=MODULE                                                 388
         AG_MARK='V00'                                                    388
         CALL AGSPUSH                                                     388
         AG_MODULE=MODULE                                                 388
C    *                                                                    388
C    Geant MATERIAL : AIR ===> inherited arguments used <===              389
         CALL AGSRESET                                                    389
         AG_TITLE = 'AIR'                                                 389
         AG_EXNAME = 'MATERIAL'                                           389
         AG_PARLIST = 'NONE'                                              389
         CALL AXMATERIAL                                                  389
C    *                                                                    389
C    Geant MATERIAL : ECAL_AIR ===> ISVOL=0 <===                          389
         CALL AGSRESET                                                    390
         AG_ISVOL = 0                                                     390
         AG_TITLE = 'ECAL_AIR'                                            390
         AG_EXNAME = 'MATERIAL'                                           390
         AG_PARLIST = 'ISVOL'                                             390
         CALL AXMATERIAL                                                  390
C    *                                                                    390
C    Geant ATTRIBUTE : ECAL ===> SEEN=0 COLO=7 <===                       390
         CALL AGSRESET                                                    391
         AG_SEEN = 0                                                      391
         AG_COLO = 7                                                      391
         AG_TITLE = 'ECAL'                                                391
         AG_EXNAME = 'ATTRIBUTE'                                          391
         AG_PARLIST = 'SEEN_COLO'                                         391
         CALL AXATTRIBUTE                                                 391
C    *                                                                    391
C    Geant SHAPE : CONE ===> DZ=(EMCS_ZEND-EMCS_ZORG)/2 RMN1=EMCS_ZORG*T  391
C    AN_LOW-D2, RMX1=EMCS_ZORG*TAN_UPP+DUP, RMN2=EMCS_ZEND*TAN_LOW-D2 RM  391
C    X2=EMCS_ZEND*TAN_UPP+DUP <===                                        391
         CALL AGSRESET                                                    394
         AG_DZ = (EMCS_ZEND-EMCS_ZORG)/2                                  394
         AG_RMN1 = EMCS_ZORG*TAN_LOW-D2                                   394
         AG_RMX1 = EMCS_ZORG*TAN_UPP+DUP                                  394
         AG_RMN2 = EMCS_ZEND*TAN_LOW-D2                                   394
         AG_RMX2 = EMCS_ZEND*TAN_UPP+DUP                                  394
         AG_TITLE = 'CONE'                                                394
         AG_EXNAME = 'SHAPE'                                              394
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            394
         CALL AXSHAPE                                                     394
C    *                                                                    394
C    Check EABS_VERSION .GT. 0                                            395
         IF (EABS_VERSION .GT. 0) THEN                                    395
         CALL GSTPAR( AG_IMED, 'CUTGAM', EABS_CUTGAM )                    395
         CALL GSTPAR( AG_IMED, 'CUTELE', EABS_CUTELE )                    395
         CALL GSTPAR( AG_IMED, 'CUTHAD', EABS_CUTHAD )                    395
         CALL GSTPAR( AG_IMED, 'CUTNEU', EABS_CUTNEU )                    395
         CALL GSTPAR( AG_IMED, 'CUTMUO', EABS_CUTMUO )                    395
         CALL GSTPAR( AG_IMED, 'DCUTE', EABS_DCUTE )                      395
         CALL GSTPAR( AG_IMED, 'DCUTM', EABS_DCUTM )                      395
         CALL GSTPAR( AG_IMED, 'BCUTE', EABS_BCUTE )                      395
         CALL GSTPAR( AG_IMED, 'BCUTM', EABS_BCUTM )                      395
         ENDIF                                                            395
C    *                                                                    396
         IHALF=1                                                          396
            GO TO 5083                                                    396
5081        IHALF=IHALF+1                                                 396
5083        IF(IHALF-(2).GT.0) GO TO 5082                                 396
            FILLED = 1                                                    397
            HALFI = -105 + (IHALF-1)*180                                  398
C       Check (IHALF==2&EMCG_FILLMODE<3)                                  399
            IF ((IHALF.EQ.2.AND.EMCG_FILLMODE.LT.3)) THEN                 399
            FILLED = 0                                                    400
            ENDIF                                                         401
C       Create EAGA :                                                     402
C       Call To Local Procedure EAGA                                      402
            ASSIGN 5100 TO BLOCK_EAGA                                     402
            GO TO  5090                                                   402
5100        CONTINUE                                                      402
C       *                                                                 402
C       Geant POSITION : EAGA ===> ALPHAZ=HALFI <===                      403
            CALL AGSRESET                                                 403
            AG_ALPHAN=AG_ALPHAN+1                                         403
            AG_ALPHAK(AG_ALPHAN)=3                                        403
            AG_ALPHAL(AG_ALPHAN)=HALFI                                    403
            AG_TITLE = 'EAGA'                                             403
            AG_EXNAME = 'POSITION'                                        403
            AG_PARLIST = 'ALPHAZ'                                         403
            CALL AXPOSITION                                               403
C       *                                                                 403
         GO TO 5081                                                       404
5082     CONTINUE                                                         404
5041  CALL AGSPOPB                                                        405
      GO TO BLOCK_ECAL, (5040 ,5050)                                      405
C End of Local Procedure                                                  405
C ----------------------------------------------------------------------  406
C ------------ EAGA                                                       406
C Begin Local Procedure EAGA                                              407
C EAGA IS HALF OF WHEEL AIR VOLUME FOR THE ENDCAP MODULE                  407
5090  CONTINUE                                                            407
         AG_TITLE='EAGA'                                                  407
         AG_MODULE=MODULE                                                 407
         AG_MARK='V00'                                                    407
         CALL AGSPUSH                                                     407
         AG_MODULE=MODULE                                                 407
C    *                                                                    407
C    Geant MATERIAL : ECAL_AIR ===> inherited arguments used <===         408
         CALL AGSRESET                                                    408
         AG_TITLE = 'ECAL_AIR'                                            408
         AG_EXNAME = 'MATERIAL'                                           408
         AG_PARLIST = 'NONE'                                              408
         CALL AXMATERIAL                                                  408
C    *                                                                    408
C    Geant ATTRIBUTE : EAGA ===> SEEN=0 COLO=1 SERIAL=FILLED <===         408
         CALL AGSRESET                                                    409
         AG_SEEN = 0                                                      409
         AG_COLO = 1                                                      409
         AG_SERIAL = FILLED                                               409
         AG_TITLE = 'EAGA'                                                409
         AG_EXNAME = 'ATTRIBUTE'                                          409
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  409
         CALL AXATTRIBUTE                                                 409
C    *                                                                    409
C    Geant SHAPE : CONS ===> DZ=(EMCS_ZEND-EMCS_ZORG)/2 RMN1=EMCS_ZORG*T  409
C    AN_LOW-D2, RMX1=EMCS_ZORG*TAN_UPP+DUP, RMN2=EMCS_ZEND*TAN_LOW-D2 RM  409
C    X2=EMCS_ZEND*TAN_UPP+DUP, PHI1=EMCS_PHIMIN PHI2=EMCS_PHIMAX <===     409
         CALL AGSRESET                                                    413
         AG_DZ = (EMCS_ZEND-EMCS_ZORG)/2                                  413
         AG_RMN1 = EMCS_ZORG*TAN_LOW-D2                                   413
         AG_RMX1 = EMCS_ZORG*TAN_UPP+DUP                                  413
         AG_RMN2 = EMCS_ZEND*TAN_LOW-D2                                   413
         AG_RMX2 = EMCS_ZEND*TAN_UPP+DUP                                  413
         AG_PHI1 = EMCS_PHIMIN                                            413
         AG_PHI2 = EMCS_PHIMAX                                            413
         AG_TITLE = 'CONS'                                                413
         AG_EXNAME = 'SHAPE'                                              413
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2_PHI1_PHI2'                  413
         CALL AXSHAPE                                                     413
C    *                                                                    413
C    Check FILLED.EQ.1                                                    413
         IF (FILLED.EQ.1) THEN                                            414
C    Create EMSS :                                                        415
C    Call To Local Procedure EMSS                                         415
         ASSIGN 5120 TO BLOCK_EMSS                                        415
         GO TO  5110                                                      415
5120     CONTINUE                                                         415
C    *                                                                    415
C    Geant POSITION : EMSS ===> KONLY='MANY' <===                         416
         CALL AGSRESET                                                    416
         AG_KONLY = 'MANY'                                                416
         AG_TITLE = 'EMSS'                                                416
         AG_EXNAME = 'POSITION'                                           416
         AG_PARLIST = 'KONLY'                                             416
         CALL AXPOSITION                                                  416
C    *                                                                    416
         CURR = EMCS_ZORG                                                 416
         CURCL = EMCS_ZEND                                                418
C    Create ECGH :                                                        419
C    Call To Local Procedure ECGH                                         419
         ASSIGN 5140 TO BLOCK_ECGH                                        419
         GO TO  5130                                                      419
5140     CONTINUE                                                         419
C    *                                                                    419
C    Geant POSITION : ECGH ===> ALPHAZ=90 KONLY='ONLY' <===               420
         CALL AGSRESET                                                    420
         AG_ALPHAN=AG_ALPHAN+1                                            420
         AG_ALPHAK(AG_ALPHAN)=3                                           420
         AG_ALPHAL(AG_ALPHAN)=90                                          420
         AG_KONLY = 'ONLY'                                                420
         AG_TITLE = 'ECGH'                                                420
         AG_EXNAME = 'POSITION'                                           420
         AG_PARLIST = 'ALPHAZ_KONLY'                                      420
         CALL AXPOSITION                                                  420
C    *                                                                    420
         ENDIF                                                            420
5091  CALL AGSPOPB                                                        422
      GO TO BLOCK_EAGA, (5090 ,5100)                                      422
C End of Local Procedure                                                  422
C ----------------------------------------------------------------------  423
C ------------ EMSS                                                       423
C Begin Local Procedure EMSS                                              424
C EMSS IS THE STEEL SUPPORT OF THE ENDCAP MODULE                          424
5110  CONTINUE                                                            424
         AG_TITLE='EMSS'                                                  424
         AG_MODULE=MODULE                                                 424
         AG_MARK='V00'                                                    424
         CALL AGSPUSH                                                     424
         AG_MODULE=MODULE                                                 424
C    *                                                                    424
C    Geant COMPONENT : CR ===> A=51.9960 Z=24 W=0.19 <===                 425
         CALL AGSRESET                                                    425
         AG_A = 51.9960                                                   425
         AG_Z = 24                                                        425
         AG_W = 0.19                                                      425
         AG_TITLE = 'CR'                                                  425
         AG_EXNAME = 'COMPONENT'                                          425
         AG_PARLIST = 'A_Z_W'                                             425
         CALL AXCOMPONENT                                                 425
C    *                                                                    425
C    Geant COMPONENT : NI ===> A=58.6934 Z=28 W=0.09 <===                 425
         CALL AGSRESET                                                    426
         AG_A = 58.6934                                                   426
         AG_Z = 28                                                        426
         AG_W = 0.09                                                      426
         AG_TITLE = 'NI'                                                  426
         AG_EXNAME = 'COMPONENT'                                          426
         AG_PARLIST = 'A_Z_W'                                             426
         CALL AXCOMPONENT                                                 426
C    *                                                                    426
C    Geant COMPONENT : FE ===> A=55.8450 Z=26 W=0.72 <===                 426
         CALL AGSRESET                                                    427
         AG_A = 55.8450                                                   427
         AG_Z = 26                                                        427
         AG_W = 0.72                                                      427
         AG_TITLE = 'FE'                                                  427
         AG_EXNAME = 'COMPONENT'                                          427
         AG_PARLIST = 'A_Z_W'                                             427
         CALL AXCOMPONENT                                                 427
C    *                                                                    427
C    Geant MIXTURE : ECAL_STEEL ===> DENS=8.03 <===                       427
         CALL AGSRESET                                                    428
         AG_DENS = 8.03                                                   428
         AG_TITLE = 'ECAL_STEEL'                                          428
         AG_EXNAME = 'MIXTURE'                                            428
         AG_PARLIST = 'DENS'                                              428
         CALL AXMIXTURE                                                   428
C    *                                                                    428
C    Geant ATTRIBUTE : EMSS ===> SEEN=1 COLO=1 <===                       428
         CALL AGSRESET                                                    429
         AG_SEEN = 1                                                      429
         AG_COLO = 1                                                      429
         AG_TITLE = 'EMSS'                                                429
         AG_EXNAME = 'ATTRIBUTE'                                          429
         AG_PARLIST = 'SEEN_COLO'                                         429
         CALL AXATTRIBUTE                                                 429
C    *                                                                    429
C    Geant SHAPE : CONS ===> DZ=(EMCS_ZEND-EMCS_ZORG)/2 RMN1=EMCS_ZORG*T  429
C    AN_LOW-D2, RMX1=EMCS_ZORG*TAN_UPP+DUP, RMN2=EMCS_ZEND*TAN_LOW-D2 RM  429
C    X2=EMCS_ZEND*TAN_UPP+DUP, PHI1=EMCS_PHIMIN PHI2=EMCS_PHIMAX <===     429
         CALL AGSRESET                                                    433
         AG_DZ = (EMCS_ZEND-EMCS_ZORG)/2                                  433
         AG_RMN1 = EMCS_ZORG*TAN_LOW-D2                                   433
         AG_RMX1 = EMCS_ZORG*TAN_UPP+DUP                                  433
         AG_RMN2 = EMCS_ZEND*TAN_LOW-D2                                   433
         AG_RMX2 = EMCS_ZEND*TAN_UPP+DUP                                  433
         AG_PHI1 = EMCS_PHIMIN                                            433
         AG_PHI2 = EMCS_PHIMAX                                            433
         AG_TITLE = 'CONS'                                                433
         AG_EXNAME = 'SHAPE'                                              433
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2_PHI1_PHI2'                  433
         CALL AXSHAPE                                                     433
C    *                                                                    433
C    Check EABS_VERSION .GT. 0                                            434
         IF (EABS_VERSION .GT. 0) THEN                                    434
         CALL GSTPAR( AG_IMED, 'CUTGAM', EABS_CUTGAM )                    434
         CALL GSTPAR( AG_IMED, 'CUTELE', EABS_CUTELE )                    434
         CALL GSTPAR( AG_IMED, 'CUTHAD', EABS_CUTHAD )                    434
         CALL GSTPAR( AG_IMED, 'CUTNEU', EABS_CUTNEU )                    434
         CALL GSTPAR( AG_IMED, 'CUTMUO', EABS_CUTMUO )                    434
         CALL GSTPAR( AG_IMED, 'DCUTE', EABS_DCUTE )                      434
         CALL GSTPAR( AG_IMED, 'DCUTM', EABS_DCUTM )                      434
         CALL GSTPAR( AG_IMED, 'BCUTE', EABS_BCUTE )                      434
         CALL GSTPAR( AG_IMED, 'BCUTM', EABS_BCUTM )                      434
         ENDIF                                                            434
         ZSLICE = EMCS_ZORG                                               435
         ZWIDTH = EMCS_FRONT                                              436
C    Check IPRIN>=1                                                       436
         IF (IPRIN.GE.1) WRITE(LOUT,5150)ZSLICE+ZWIDTH/2                  436
5150     FORMAT(' Front Al plate centered at: ', F12.4)                   438
C    Create EFLP :                                                        439
C    Call To Local Procedure EFLP                                         439
         ASSIGN 5170 TO BLOCK_EFLP                                        439
         GO TO  5160                                                      439
5170     CONTINUE                                                         439
C    *                                                                    439
C    Geant POSITION : EFLP ===> Z=ZSLICE-CENTER+ZWIDTH/2 <===             440
         CALL AGSRESET                                                    440
         AG_Z = ZSLICE-CENTER+ZWIDTH/2                                    440
         AG_TITLE = 'EFLP'                                                440
         AG_EXNAME = 'POSITION'                                           440
         AG_PARLIST = 'Z'                                                 440
         CALL AXPOSITION                                                  440
C    *                                                                    440
         ZSLICE = ZSLICE + ZWIDTH                                         440
C    Check IPRIN>=1                                                       441
         IF (IPRIN.GE.1) WRITE(LOUT,5180)ZSLICE                           441
5180     FORMAT(' FIRST CALORIMETER STARTS AT: ',F12.4)                   443
C    FIRST SECTION                                                        444
         FSECT = 1                                                        444
C    LAST SECTION                                                         445
         LSECT = 3                                                        445
C    WIDTH OF CURRENT SLICE                                               446
         ZWIDTH = EMCS_SMDCENTR - EMCS_GAPSMD/2 - ZSLICE                  446
C    Check IPRIN>=1                                                       446
         IF (IPRIN.GE.1) WRITE(LOUT,5190)ZSLICE+ZWIDTH/2                  446
5190     FORMAT('Sections 1-3 positioned at: ', F12.4)                    448
C    Create ECVO :                                                        449
C    Call To Local Procedure ECVO                                         449
         ASSIGN 5210 TO BLOCK_ECVO                                        449
         GO TO  5200                                                      449
5210     CONTINUE                                                         449
C    *                                                                    449
C    Geant POSITION : ECVO ===> Z=ZSLICE-CENTER+ZWIDTH/2 <===             450
         CALL AGSRESET                                                    450
         AG_Z = ZSLICE-CENTER+ZWIDTH/2                                    450
         AG_TITLE = 'ECVO'                                                450
         AG_EXNAME = 'POSITION'                                           450
         AG_PARLIST = 'Z'                                                 450
         CALL AXPOSITION                                                  450
C    *                                                                    450
         ZWIDTH = EMCS_GAPSMD                                             450
         ZSLICE = EMCS_SMDCENTR - EMCS_GAPSMD/2                           452
C    Check IPRIN>=1                                                       452
         IF (IPRIN.GE.1) WRITE(LOUT,5220)SECTION,ZSLICE                   452
5220     FORMAT(' 1st calorimeter ends, smd starts at: ',2F10.5)          454
C    Check IPRIN>=1                                                       454
         IF (IPRIN.GE.1) WRITE(LOUT,5230)ZWIDTH                           454
5230     FORMAT(' smd width = ',F10.5)                                    456
C    Check IPRIN>=1                                                       456
         IF (IPRIN.GE.1) WRITE(LOUT,5240)ZSLICE+ZWIDTH/2                  456
5240     FORMAT('SMD section centered at: ', F12.4)                       458
C    Create ESHM :                                                        459
C    Call To Local Procedure ESHM                                         459
         ASSIGN 5260 TO BLOCK_ESHM                                        459
         GO TO  5250                                                      459
5260     CONTINUE                                                         459
C    *                                                                    459
C    Geant POSITION : ESHM ===> Z=ZSLICE-CENTER+ZWIDTH/2 KONLY='MANY' <=  460
C    ==                                                                   460
         CALL AGSRESET                                                    460
         AG_Z = ZSLICE-CENTER+ZWIDTH/2                                    460
         AG_KONLY = 'MANY'                                                460
         AG_TITLE = 'ESHM'                                                460
         AG_EXNAME = 'POSITION'                                           460
         AG_PARLIST = 'Z_KONLY'                                           460
         CALL AXPOSITION                                                  460
C    *                                                                    460
         ZSLICE = ZSLICE + ZWIDTH                                         460
C    Check IPRIN>=1                                                       461
         IF (IPRIN.GE.1) WRITE(LOUT,5270)ZSLICE                           461
5270     FORMAT(' SMD ends at: ',F10.5)                                   463
C    FIRST SECTION                                                        464
         FSECT = 4                                                        464
C    LAST SECTION                                                         465
         LSECT = 5                                                        465
         ZWIDTH = 0                                                       466
C    *                                                                    467
         I_SECTION=FSECT                                                  467
            GO TO 5283                                                    467
5281        I_SECTION=I_SECTION+1                                         467
5283        IF(I_SECTION-(LSECT).GT.0) GO TO 5282                         467
C       use ESEC bank                                                     468
            BPATH_ESEC='ESEC*'                                            468
            IDNUM_ESEC(1)=0                                               468
            ESEC_ISECT=I_SECTION                                          468
            CALL AGDGETP('ECALGEO6','*',-999,'ESEC',NESEC_ISECT,          468
     *      RE_ESEC_ISECT,BPATH_ESEC,IDNUM_ESEC,1,MAP_ESEC,NAMES_ESEC,    468
     *      BANK_ESEC,LEN_ESEC,LLEN_ESEC,LINK_ESEC,FLAG_ESEC)             468
            ZWIDTH = ZWIDTH + ESEC_CELL*ESEC_NLAYER                       469
         GO TO 5281                                                       470
5282     CONTINUE                                                         470
         ZWIDTH = EMCS_BCKFRNT - ZSLICE                                   471
C    Check IPRIN>=1                                                       471
         IF (IPRIN.GE.1) WRITE(LOUT,5290)ZSLICE+ZWIDTH/2                  471
5290     FORMAT('Sections 4-5 positioned at: ', F12.4)                    473
C    Create ECVO :                                                        474
C    Call To Local Procedure ECVO                                         474
         ASSIGN 5300 TO BLOCK_ECVO                                        474
         GO TO  5200                                                      474
5300     CONTINUE                                                         474
C    *                                                                    474
C    Geant POSITION : ECVO ===> Z=ZSLICE-CENTER+ZWIDTH/2 <===             475
         CALL AGSRESET                                                    475
         AG_Z = ZSLICE-CENTER+ZWIDTH/2                                    475
         AG_TITLE = 'ECVO'                                                475
         AG_EXNAME = 'POSITION'                                           475
         AG_PARLIST = 'Z'                                                 475
         CALL AXPOSITION                                                  475
C    *                                                                    475
         ZSLICE = EMCS_BCKFRNT                                            475
C    Check IPRIN>=1                                                       476
         IF (IPRIN.GE.1) WRITE(LOUT,5310)SECTION,ZSLICE                   476
5310     FORMAT(' 2nd calorimeter ends, back plate starts at: ',2F10.5)   478
         ZWIDTH = EMCS_BCKPLATE                                           479
C    Create ESSP :                                                        480
C    Call To Local Procedure ESSP                                         480
         ASSIGN 5330 TO BLOCK_ESSP                                        480
         GO TO  5320                                                      480
5330     CONTINUE                                                         480
C    *                                                                    480
C    Geant POSITION : ESSP ===> Z=ZSLICE-CENTER+ZWIDTH/2 <===             481
         CALL AGSRESET                                                    481
         AG_Z = ZSLICE-CENTER+ZWIDTH/2                                    481
         AG_TITLE = 'ESSP'                                                481
         AG_EXNAME = 'POSITION'                                           481
         AG_PARLIST = 'Z'                                                 481
         CALL AXPOSITION                                                  481
C    *                                                                    481
         ZSLICE = ZSLICE + ZWIDTH                                         481
C    Check IPRIN>=1                                                       482
         IF (IPRIN.GE.1) WRITE(LOUT,5340)ZSLICE                           482
5340     FORMAT('EEMC Al backplate ends at: ',F12.4)                      484
         ZWIDTH = EMCS_ZEND - EMCS_ZORG                                   485
C    Create ERCM :                                                        486
C    Call To Local Procedure ERCM                                         486
         ASSIGN 5360 TO BLOCK_ERCM                                        486
         GO TO  5350                                                      486
5360     CONTINUE                                                         486
C    *                                                                    486
C    *                                                                    487
         I=1                                                              487
            GO TO 5373                                                    487
5371        I=I+1                                                         487
5373        IF(I-(2).GT.0) GO TO 5372                                     487
C       *                                                                 488
            J=1                                                           488
               GO TO 5383                                                 488
5381           J=J+1                                                      488
5383           IF(J-(5).GT.0) GO TO 5382                                  488
               XX = EMCS_PHIMIN + J*30                                    489
               YY = XX*DEGRAD                                             490
               XC = COS(YY)*EMCS_TIEROD(I)                                491
               YC = SIN(YY)*EMCS_TIEROD(I)                                492
C          Geant POSITION : ERCM ===> X=XC Y=YC Z=0 <===                  493
               CALL AGSRESET                                              493
               AG_X = XC                                                  493
               AG_Y = YC                                                  493
               AG_Z = 0                                                   493
               AG_TITLE = 'ERCM'                                          493
               AG_EXNAME = 'POSITION'                                     493
               AG_PARLIST = 'X_Y_Z'                                       493
               CALL AXPOSITION                                            493
C          *                                                              493
            GO TO 5381                                                    494
5382        CONTINUE                                                      494
         GO TO 5371                                                       495
5372     CONTINUE                                                         495
         RTH = EMCS_ZORG*TAN_UPP + DUP + 2.5/2                            496
         XC =(EMCS_ZEND - EMCS_ZORG)*TAN_UPP                              497
         LENGTH = .5*(EMCS_ZEND + EMCS_ZORG)*TAN_UPP + DUP + 2.5/2        498
         YC = EMCS_ZEND - EMCS_ZORG                                       499
         P = ATAN(XC/YC)/DEGRAD                                           500
C    Create EPSB :                                                        501
C    Call To Local Procedure EPSB                                         501
         ASSIGN 5400 TO BLOCK_EPSB                                        501
         GO TO  5390                                                      501
5400     CONTINUE                                                         501
C    *                                                                    501
C    *                                                                    502
         I=1                                                              502
            GO TO 5413                                                    502
5411        I=I+1                                                         502
5413        IF(I-(6).GT.0) GO TO 5412                                     502
            XX = -75 + (I-1)*30                                           503
            YY = XX*DEGRAD                                                504
            XC = COS(YY)*LENGTH                                           505
            YC = SIN(YY)*LENGTH                                           506
C       Geant POSITION : EPSB ===> X=XC Y=YC ALPHAZ=XX <===               507
            CALL AGSRESET                                                 507
            AG_X = XC                                                     507
            AG_Y = YC                                                     507
            AG_ALPHAN=AG_ALPHAN+1                                         507
            AG_ALPHAK(AG_ALPHAN)=3                                        507
            AG_ALPHAL(AG_ALPHAN)=XX                                       507
            AG_TITLE = 'EPSB'                                             507
            AG_EXNAME = 'POSITION'                                        507
            AG_PARLIST = 'X_Y_ALPHAZ'                                     507
            CALL AXPOSITION                                               507
C       *                                                                 507
         GO TO 5411                                                       508
5412     CONTINUE                                                         508
5111  CALL AGSPOPB                                                        509
      GO TO BLOCK_EMSS, (5110 ,5120)                                      509
C End of Local Procedure                                                  509
C ----------------------------------------------------------------------  510
C ------------ ECVO                                                       510
C Begin Local Procedure ECVO                                              511
C ECVO IS ONE OF ENDCAP VOLUME WITH MEGATILES AND RADIATORS               511
5200  CONTINUE                                                            511
         AG_TITLE='ECVO'                                                  511
         AG_MODULE=MODULE                                                 511
         AG_MARK='V00'                                                    511
         CALL AGSPUSH                                                     511
         AG_MODULE=MODULE                                                 511
C    *                                                                    511
C    Geant MATERIAL : ECAL_AIR ===> inherited arguments used <===         512
         CALL AGSRESET                                                    512
         AG_TITLE = 'ECAL_AIR'                                            512
         AG_EXNAME = 'MATERIAL'                                           512
         AG_PARLIST = 'NONE'                                              512
         CALL AXMATERIAL                                                  512
C    *                                                                    512
C    Geant ATTRIBUTE : ECVO ===> SEEN=1 COLO=3 <===                       512
         CALL AGSRESET                                                    513
         AG_SEEN = 1                                                      513
         AG_COLO = 3                                                      513
         AG_TITLE = 'ECVO'                                                513
         AG_EXNAME = 'ATTRIBUTE'                                          513
         AG_PARLIST = 'SEEN_COLO'                                         513
         CALL AXATTRIBUTE                                                 513
C    *                                                                    513
C    Geant SHAPE : CONS ===> DZ=ZWIDTH/2 RMN1=ZSLICE*TAN_LOW-DD, RMX1=ZS  513
C    LICE*TAN_UPP+DUP RMN2=(ZSLICE+ZWIDTH)*TAN_LOW-DD, RMX2=(ZSLICE+ZWID  513
C    TH)*TAN_UPP+DUP <===                                                 513
         CALL AGSRESET                                                    516
         AG_DZ = ZWIDTH/2                                                 516
         AG_RMN1 = ZSLICE*TAN_LOW-DD                                      516
         AG_RMX1 = ZSLICE*TAN_UPP+DUP                                     516
         AG_RMN2 = (ZSLICE+ZWIDTH)*TAN_LOW-DD                             516
         AG_RMX2 = (ZSLICE+ZWIDTH)*TAN_UPP+DUP                            516
         AG_TITLE = 'CONS'                                                516
         AG_EXNAME = 'SHAPE'                                              516
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            516
         CALL AXSHAPE                                                     516
C    *                                                                    516
C    *                                                                    516
         I_SECTOR=1                                                       517
            GO TO 5423                                                    517
5421        I_SECTOR=I_SECTOR+1                                           517
5423        IF(I_SECTOR-(6).GT.0) GO TO 5422                              517
C       Check (1<I_SECTOR .AND. I_SECTOR<6)|EMCG_FILLMODE>1               518
            IF ((1.LT.I_SECTOR .AND.                                      518
     *      I_SECTOR.LT.6).OR.EMCG_FILLMODE.GT.1) THEN                    518
            FILLED = 1                                                    519
            ELSE                                                          520
            FILLED = 0                                                    521
            ENDIF                                                         522
            D3 = 75 - (I_SECTOR-1)*30                                     523
C       Create EMOD :                                                     524
C       Call To Local Procedure EMOD                                      524
            ASSIGN 5440 TO BLOCK_EMOD                                     524
            GO TO  5430                                                   524
5440        CONTINUE                                                      524
C       *                                                                 524
C       Geant POSITION : EMOD ===> ALPHAZ=D3 NCOPY=I_SECTOR <===          525
            CALL AGSRESET                                                 525
            AG_ALPHAN=AG_ALPHAN+1                                         525
            AG_ALPHAK(AG_ALPHAN)=3                                        525
            AG_ALPHAL(AG_ALPHAN)=D3                                       525
            AG_NCOPY = I_SECTOR                                           525
            AG_TITLE = 'EMOD'                                             525
            AG_EXNAME = 'POSITION'                                        525
            AG_PARLIST = 'ALPHAZ_NCOPY'                                   525
            CALL AXPOSITION                                               525
C       *                                                                 525
         GO TO 5421                                                       526
5422     CONTINUE                                                         526
5201  CALL AGSPOPB                                                        527
      GO TO BLOCK_ECVO, (5200 ,5210,5300)                                 527
C End of Local Procedure                                                  527
C ----------------------------------------------------------------------  528
C ------------ ESHM                                                       528
C Begin Local Procedure ESHM                                              529
C ESHM IS THE SHOWER MAX SECTION                                          529
5250  CONTINUE                                                            529
         AG_TITLE='ESHM'                                                  529
         AG_MODULE=MODULE                                                 529
         AG_MARK='V00'                                                    529
         CALL AGSPUSH                                                     529
         AG_MODULE=MODULE                                                 529
C    *                                                                    529
C    Geant MATERIAL : ECAL_AIR ===> inherited arguments used <===         530
         CALL AGSRESET                                                    530
         AG_TITLE = 'ECAL_AIR'                                            530
         AG_EXNAME = 'MATERIAL'                                           530
         AG_PARLIST = 'NONE'                                              530
         CALL AXMATERIAL                                                  530
C    *                                                                    530
C    Geant ATTRIBUTE : ESHM ===> SEEN=1 COLO=4 <===                       530
         CALL AGSRESET                                                    531
         AG_SEEN = 1                                                      531
         AG_COLO = 4                                                      531
         AG_TITLE = 'ESHM'                                                531
         AG_EXNAME = 'ATTRIBUTE'                                          531
         AG_PARLIST = 'SEEN_COLO'                                         531
         CALL AXATTRIBUTE                                                 531
C    *                                                                    531
C    Geant SHAPE : CONS ===> DZ=ZWIDTH/2 RMN1=(ZSLICE*TAN_LOW)-DD, RMX1=  531
C    (ZSLICE)*TAN_UPP+DUP RMN2=(ZSLICE+ZWIDTH)*TAN_LOW-DD, RMX2=(ZSLICE+  531
C    ZWIDTH)*TAN_UPP+DUP, PHI1=EMCS_PHIMIN PHI2=EMCS_PHIMAX <===          531
         CALL AGSRESET                                                    535
         AG_DZ = ZWIDTH/2                                                 535
         AG_RMN1 = (ZSLICE*TAN_LOW)-DD                                    535
         AG_RMX1 = (ZSLICE)*TAN_UPP+DUP                                   535
         AG_RMN2 = (ZSLICE+ZWIDTH)*TAN_LOW-DD                             535
         AG_RMX2 = (ZSLICE+ZWIDTH)*TAN_UPP+DUP                            535
         AG_PHI1 = EMCS_PHIMIN                                            535
         AG_PHI2 = EMCS_PHIMAX                                            535
         AG_TITLE = 'CONS'                                                535
         AG_EXNAME = 'SHAPE'                                              535
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2_PHI1_PHI2'                  535
         CALL AXSHAPE                                                     535
C    *                                                                    535
C    use default EMXG bank                                                535
         BPATH_EMXG='EMXG*'                                               536
         IDNUM_EMXG(1)=0                                                  536
         CALL AGDGETP('ECALGEO6','*',-999,'EMXG',' ',BANK_EMXG,           536
     *   BPATH_EMXG,IDNUM_EMXG,1,MAP_EMXG,NAMES_EMXG,BANK_EMXG,LEN_EMXG,  536
     *   LLEN_EMXG,LINK_EMXG,FLAG_EMXG)                                   536
         MAXCNT = EMCS_SMDCENTR                                           537
C    Check IPRIN>=1                                                       537
         IF (IPRIN.GE.1) WRITE(LOUT,5450)ZSLICE,SECTION,CENTER            537
5450     FORMAT(' === z start for smd,section: ',3F12.4, ' === ')         539
C    *                                                                    540
         J_SECTION=1                                                      540
            GO TO 5463                                                    540
5461        J_SECTION=J_SECTION+1                                         540
5463        IF(J_SECTION-(3).GT.0) GO TO 5462                             540
C       use EXSE bank                                                     541
            BPATH_EXSE='EXSE*'                                            541
            IDNUM_EXSE(1)=0                                               541
            EXSE_JSECT=J_SECTION                                          541
            CALL AGDGETP('ECALGEO6','*',-999,'EXSE',NEXSE_JSECT,          541
     *      RE_EXSE_JSECT,BPATH_EXSE,IDNUM_EXSE,1,MAP_EXSE,NAMES_EXSE,    541
     *      BANK_EXSE,LEN_EXSE,LLEN_EXSE,LINK_EXSE,FLAG_EXSE)             541
            CURRENT = EXSE_ZSHIFT                                         542
            SECWID = EMXG_SAPEX + 2.*EMXG_F4                              543
            SECTION = MAXCNT + EXSE_ZSHIFT                                544
C       Check IPRIN>=1                                                    544
            IF (IPRIN.GE.1) WRITE(LOUT,5470)J_SECTION,CURRENT,SECTION,    544
     *      SECWID                                                        544
5470        FORMAT(' layer, z, width : ',I3,3F12.4)                       546
            RBOT=SECTION*TAN_LOW                                          547
            RTOP=SECTION*TAN_UPP                                          548
C       Check IPRIN>=1                                                    548
            IF (IPRIN.GE.1) WRITE(LOUT,5480)J_SECTION,RBOT,RTOP           548
5480        FORMAT(' layer, rbot,rtop : ',I3,2F12.4)                      550
C       Check IPRIN>=1                                                    550
            IF (IPRIN.GE.1) WRITE(LOUT,5490)J_SECTION,CENTER+CURRENT      550
5490        FORMAT(' smd layer=',I1,' z=',F12.4)                          552
C       Create ESPL :                                                     553
C       Call To Local Procedure ESPL                                      553
            ASSIGN 5510 TO BLOCK_ESPL                                     553
            GO TO  5500                                                   553
5510        CONTINUE                                                      553
C       *                                                                 553
C       Geant POSITION : ESPL ===> Z=CURRENT KONLY='MANY' <===            554
            CALL AGSRESET                                                 554
            AG_Z = CURRENT                                                554
            AG_KONLY = 'MANY'                                             554
            AG_TITLE = 'ESPL'                                             554
            AG_EXNAME = 'POSITION'                                        554
            AG_PARLIST = 'Z_KONLY'                                        554
            CALL AXPOSITION                                               554
C       *                                                                 554
         GO TO 5461                                                       555
5462     CONTINUE                                                         555
C    Create ERSM :                                                        556
C    Call To Local Procedure ERSM                                         556
         ASSIGN 5530 TO BLOCK_ERSM                                        556
         GO TO  5520                                                      556
5530     CONTINUE                                                         556
C    *                                                                    556
C    *                                                                    557
         I=1                                                              557
            GO TO 5543                                                    557
5541        I=I+1                                                         557
5543        IF(I-(2).GT.0) GO TO 5542                                     557
C       *                                                                 558
            J=1                                                           558
               GO TO 5553                                                 558
5551           J=J+1                                                      558
5553           IF(J-(5).GT.0) GO TO 5552                                  558
               XX = EMCS_PHIMIN + J*30                                    559
               YY = XX*DEGRAD                                             560
               XC = COS(YY)*EMCS_TIEROD(I)                                561
               YC = SIN(YY)*EMCS_TIEROD(I)                                562
C          Geant POSITION : ERSM ===> X=XC Y=YC Z=0 <===                  563
               CALL AGSRESET                                              563
               AG_X = XC                                                  563
               AG_Y = YC                                                  563
               AG_Z = 0                                                   563
               AG_TITLE = 'ERSM'                                          563
               AG_EXNAME = 'POSITION'                                     563
               AG_PARLIST = 'X_Y_Z'                                       563
               CALL AXPOSITION                                            563
C          *                                                              563
            GO TO 5551                                                    564
5552        CONTINUE                                                      564
         GO TO 5541                                                       565
5542     CONTINUE                                                         565
5251  CALL AGSPOPB                                                        566
      GO TO BLOCK_ESHM, (5250 ,5260)                                      566
C End of Local Procedure                                                  566
C ----------------------------------------------------------------------  567
C ------------ ECGH                                                       567
C Begin Local Procedure ECGH                                              568
C ECGH IS AIR GAP BETWEEN ENDCAP HALF WHEELS                              568
5130  CONTINUE                                                            568
         AG_TITLE='ECGH'                                                  568
         AG_MODULE=MODULE                                                 568
         AG_MARK='V00'                                                    568
         CALL AGSPUSH                                                     568
         AG_MODULE=MODULE                                                 568
C    *                                                                    568
C    Geant MATERIAL : ECAL_AIR ===> inherited arguments used <===         569
         CALL AGSRESET                                                    569
         AG_TITLE = 'ECAL_AIR'                                            569
         AG_EXNAME = 'MATERIAL'                                           569
         AG_PARLIST = 'NONE'                                              569
         CALL AXMATERIAL                                                  569
C    *                                                                    569
C    Geant ATTRIBUTE : ECGH ===> SEEN=0 COLO=7 <===                       569
         CALL AGSRESET                                                    570
         AG_SEEN = 0                                                      570
         AG_COLO = 7                                                      570
         AG_TITLE = 'ECGH'                                                570
         AG_EXNAME = 'ATTRIBUTE'                                          570
         AG_PARLIST = 'SEEN_COLO'                                         570
         CALL AXATTRIBUTE                                                 570
C    *                                                                    570
C    Geant SHAPE : TRD1 ===> DX1=EMCS_ZORG*TAN_UPP+DUP DX2=EMCS_ZEND*TAN  570
C    _UPP+DUP, DY=(EMCS_GAPHALF+EMCS_COVER)/2, DZ=(EMCS_ZEND-EMCS_ZORG)/  570
C    2 <===                                                               570
         CALL AGSRESET                                                    573
         AG_DX1 = EMCS_ZORG*TAN_UPP+DUP                                   573
         AG_DX2 = EMCS_ZEND*TAN_UPP+DUP                                   573
         AG_DY = (EMCS_GAPHALF+EMCS_COVER)/2                              573
         AG_DZ = (EMCS_ZEND-EMCS_ZORG)/2                                  573
         AG_TITLE = 'TRD1'                                                573
         AG_EXNAME = 'SHAPE'                                              573
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     573
         CALL AXSHAPE                                                     573
C    *                                                                    573
         RTH = EMCS_GAPHALF + EMCS_COVER                                  573
         XX=CURR*TAN_LOW - D2                                             575
         XLEFT = SQRT(XX*XX - RTH*RTH)                                    576
         YY=CURR*TAN_UPP + DUP                                            577
         XRIGHT = SQRT(YY*YY - RTH*RTH)                                   578
         SECWID = YY - XX                                                 579
         XX=CURCL*TAN_LOW - D2                                            580
         YLEFT = SQRT(XX*XX - RTH*RTH)                                    581
         YY=CURCL*TAN_UPP + DUP                                           582
         YRIGHT = SQRT(YY*YY - RTH*RTH)                                   583
         ZWIDTH = YY - XX                                                 584
         XX=(XLEFT + XRIGHT)/2                                            585
         YY=(YLEFT + YRIGHT)/2                                            586
         XC = YY - XX                                                     587
         LENGTH =(XX + YY)/2                                              588
         YC = CURCL - CURR                                                589
         P = ATAN(XC/YC)/DEGRAD                                           590
         RTH = - (EMCS_GAPHALF + EMCS_COVER)/2                            591
C    Create ECHC :                                                        592
C    Call To Local Procedure ECHC                                         592
         ASSIGN 5570 TO BLOCK_ECHC                                        592
         GO TO  5560                                                      592
5570     CONTINUE                                                         592
C    *                                                                    592
C    Geant POSITION : ECHC ===> X=+LENGTH Y=RTH <===                      593
         CALL AGSRESET                                                    593
         AG_X = +LENGTH                                                   593
         AG_Y = RTH                                                       593
         AG_TITLE = 'ECHC'                                                593
         AG_EXNAME = 'POSITION'                                           593
         AG_PARLIST = 'X_Y'                                               593
         CALL AXPOSITION                                                  593
C    *                                                                    593
C    Geant POSITION : ECHC ===> X=-LENGTH Y=RTH ALPHAZ=180 <===           593
         CALL AGSRESET                                                    594
         AG_X = -LENGTH                                                   594
         AG_Y = RTH                                                       594
         AG_ALPHAN=AG_ALPHAN+1                                            594
         AG_ALPHAK(AG_ALPHAN)=3                                           594
         AG_ALPHAL(AG_ALPHAN)=180                                         594
         AG_TITLE = 'ECHC'                                                594
         AG_EXNAME = 'POSITION'                                           594
         AG_PARLIST = 'X_Y_ALPHAZ'                                        594
         CALL AXPOSITION                                                  594
C    *                                                                    594
5131  CALL AGSPOPB                                                        595
      GO TO BLOCK_ECGH, (5130 ,5140)                                      595
C End of Local Procedure                                                  595
C ----------------------------------------------------------------------  596
C ------------ ECHC                                                       596
C Begin Local Procedure ECHC                                              597
C ECHC IS STEEL ENDCAP HALF COVER                                         597
5560  CONTINUE                                                            597
         AG_TITLE='ECHC'                                                  597
         AG_MODULE=MODULE                                                 597
         AG_MARK='V00'                                                    597
         CALL AGSPUSH                                                     597
         AG_MODULE=MODULE                                                 597
C    *                                                                    597
C    Geant MATERIAL : ECAL_STEEL ===> inherited arguments used <===       598
         CALL AGSRESET                                                    598
         AG_TITLE = 'ECAL_STEEL'                                          598
         AG_EXNAME = 'MATERIAL'                                           598
         AG_PARLIST = 'NONE'                                              598
         CALL AXMATERIAL                                                  598
C    *                                                                    598
C    Geant ATTRIBUTE : ECHC ===> SEEN=1 COLO=1 <===                       598
         CALL AGSRESET                                                    599
         AG_SEEN = 1                                                      599
         AG_COLO = 1                                                      599
         AG_TITLE = 'ECHC'                                                599
         AG_EXNAME = 'ATTRIBUTE'                                          599
         AG_PARLIST = 'SEEN_COLO'                                         599
         CALL AXATTRIBUTE                                                 599
C    *                                                                    599
C    Geant SHAPE : TRAP ===> DZ=(CURCL-CURR)/2 THET=P PHI=0, H1=EMCS_COV  599
C    ER/2 BL1=SECWID/2 TL1=SECWID/2 ALP1=0, H2=EMCS_COVER/2 BL2=ZWIDTH/2  599
C     TL2=ZWIDTH/2, ALP2=0 <===                                           599
         CALL AGSRESET                                                    603
         AG_DZ = (CURCL-CURR)/2                                           603
         AG_THET = P                                                      603
         AG_PHI = 0                                                       603
         AG_H1 = EMCS_COVER/2                                             603
         AG_BL1 = SECWID/2                                                603
         AG_TL1 = SECWID/2                                                603
         AG_ALP1 = 0                                                      603
         AG_H2 = EMCS_COVER/2                                             603
         AG_BL2 = ZWIDTH/2                                                603
         AG_TL2 = ZWIDTH/2                                                603
         AG_ALP2 = 0                                                      603
         AG_TITLE = 'TRAP'                                                603
         AG_EXNAME = 'SHAPE'                                              603
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       603
         CALL AXSHAPE                                                     603
C    *                                                                    603
5561  CALL AGSPOPB                                                        604
      GO TO BLOCK_ECHC, (5560 ,5570)                                      604
C End of Local Procedure                                                  604
C ----------------------------------------------------------------------  605
C ------------ ESSP                                                       605
C Begin Local Procedure ESSP                                              606
C ESSP IS STAINLESS STEEL BACK PLATE                                      606
5320  CONTINUE                                                            606
         AG_TITLE='ESSP'                                                  606
         AG_MODULE=MODULE                                                 606
         AG_MARK='V00'                                                    606
         CALL AGSPUSH                                                     606
         AG_MODULE=MODULE                                                 606
C    *                                                                    606
C    Geant MATERIAL : ECAL_STEEL ===> inherited arguments used <===       607
         CALL AGSRESET                                                    607
         AG_TITLE = 'ECAL_STEEL'                                          607
         AG_EXNAME = 'MATERIAL'                                           607
         AG_PARLIST = 'NONE'                                              607
         CALL AXMATERIAL                                                  607
C    *                                                                    607
C    Geant ATTRIBUTE : ESSP ===> SEEN=1 COLO=6 FILL=1 <===                607
         CALL AGSRESET                                                    608
         AG_SEEN = 1                                                      608
         AG_COLO = 6                                                      608
         AG_FILL = 1                                                      608
         AG_TITLE = 'ESSP'                                                608
         AG_EXNAME = 'ATTRIBUTE'                                          608
         AG_PARLIST = 'SEEN_COLO_FILL'                                    608
         CALL AXATTRIBUTE                                                 608
C    *                                                                    608
C    Geant SHAPE : CONS ===> DZ=EMCS_BCKPLATE/2 RMN1=ZSLICE*TAN_LOW-DD,   608
C    RMX1=ZSLICE*TAN_UPP+DUP RMN2=(ZSLICE+ZWIDTH)*TAN_LOW-DD, RMX2=(ZSLI  608
C    CE+ZWIDTH)*TAN_UPP+DUP, PHI1=EMCS_PHIMIN PHI2=EMCS_PHIMAX <===       608
         CALL AGSRESET                                                    612
         AG_DZ = EMCS_BCKPLATE/2                                          612
         AG_RMN1 = ZSLICE*TAN_LOW-DD                                      612
         AG_RMX1 = ZSLICE*TAN_UPP+DUP                                     612
         AG_RMN2 = (ZSLICE+ZWIDTH)*TAN_LOW-DD                             612
         AG_RMX2 = (ZSLICE+ZWIDTH)*TAN_UPP+DUP                            612
         AG_PHI1 = EMCS_PHIMIN                                            612
         AG_PHI2 = EMCS_PHIMAX                                            612
         AG_TITLE = 'CONS'                                                612
         AG_EXNAME = 'SHAPE'                                              612
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2_PHI1_PHI2'                  612
         CALL AXSHAPE                                                     612
C    *                                                                    612
5321  CALL AGSPOPB                                                        613
      GO TO BLOCK_ESSP, (5320 ,5330)                                      613
C End of Local Procedure                                                  613
C ----------------------------------------------------------------------  614
C ------------ EPSB                                                       614
C Begin Local Procedure EPSB                                              615
C EPSB IS A PROJECTILE STAINLESS STEEL BAR                                615
5390  CONTINUE                                                            615
         AG_TITLE='EPSB'                                                  615
         AG_MODULE=MODULE                                                 615
         AG_MARK='V00'                                                    615
         CALL AGSPUSH                                                     615
         AG_MODULE=MODULE                                                 615
C    *                                                                    615
C    Geant MATERIAL : ECAL_STEEL ===> inherited arguments used <===       616
         CALL AGSRESET                                                    616
         AG_TITLE = 'ECAL_STEEL'                                          616
         AG_EXNAME = 'MATERIAL'                                           616
         AG_PARLIST = 'NONE'                                              616
         CALL AXMATERIAL                                                  616
C    *                                                                    616
C    Geant ATTRIBUTE : EPSB ===> SEEN=1 COLO=6 FILL=1 <===                616
         CALL AGSRESET                                                    617
         AG_SEEN = 1                                                      617
         AG_COLO = 6                                                      617
         AG_FILL = 1                                                      617
         AG_TITLE = 'EPSB'                                                617
         AG_EXNAME = 'ATTRIBUTE'                                          617
         AG_PARLIST = 'SEEN_COLO_FILL'                                    617
         CALL AXATTRIBUTE                                                 617
C    *                                                                    617
C    Geant SHAPE : TRAP ===> DZ=(EMCS_ZEND-EMCS_ZORG)/2 THET=P, PHI=0 H1  617
C    =2.0/2 BL1=2.5/2 TL1=2.5/2 ALP1=0, H2=2.0/2 BL2=2.5/2 TL2=2.5/2 ALP  617
C    2=0 <===                                                             617
         CALL AGSRESET                                                    620
         AG_DZ = (EMCS_ZEND-EMCS_ZORG)/2                                  620
         AG_THET = P                                                      620
         AG_PHI = 0                                                       620
         AG_H1 = 2.0/2                                                    620
         AG_BL1 = 2.5/2                                                   620
         AG_TL1 = 2.5/2                                                   620
         AG_ALP1 = 0                                                      620
         AG_H2 = 2.0/2                                                    620
         AG_BL2 = 2.5/2                                                   620
         AG_TL2 = 2.5/2                                                   620
         AG_ALP2 = 0                                                      620
         AG_TITLE = 'TRAP'                                                620
         AG_EXNAME = 'SHAPE'                                              620
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       620
         CALL AXSHAPE                                                     620
C    *                                                                    620
5391  CALL AGSPOPB                                                        621
      GO TO BLOCK_EPSB, (5390 ,5400)                                      621
C End of Local Procedure                                                  621
C ----------------------------------------------------------------------  622
C ------------ ERCM                                                       622
C Begin Local Procedure ERCM                                              623
C ERCM IS STAINLESS STEEL TIE ROD IN CALORIMETER SECTIONS                 623
5350  CONTINUE                                                            623
         AG_TITLE='ERCM'                                                  623
         AG_MODULE=MODULE                                                 623
         AG_MARK='V00'                                                    623
         CALL AGSPUSH                                                     623
         AG_MODULE=MODULE                                                 623
C    *                                                                    623
C    Geant MATERIAL : ECAL_STEEL ===> inherited arguments used <===       624
         CALL AGSRESET                                                    624
         AG_TITLE = 'ECAL_STEEL'                                          624
         AG_EXNAME = 'MATERIAL'                                           624
         AG_PARLIST = 'NONE'                                              624
         CALL AXMATERIAL                                                  624
C    *                                                                    624
C    Geant ATTRIBUTE : ERSM ===> SEEN=1 COLO=6 FILL=1 <===                624
         CALL AGSRESET                                                    625
         AG_SEEN = 1                                                      625
         AG_COLO = 6                                                      625
         AG_FILL = 1                                                      625
         AG_TITLE = 'ERSM'                                                625
         AG_EXNAME = 'ATTRIBUTE'                                          625
         AG_PARLIST = 'SEEN_COLO_FILL'                                    625
         CALL AXATTRIBUTE                                                 625
C    *                                                                    625
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=EMCS_RTIE DZ=ZWIDTH/2 <===       625
         CALL AGSRESET                                                    626
         AG_RMIN = 0                                                      626
         AG_RMAX = EMCS_RTIE                                              626
         AG_DZ = ZWIDTH/2                                                 626
         AG_TITLE = 'TUBE'                                                626
         AG_EXNAME = 'SHAPE'                                              626
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      626
         CALL AXSHAPE                                                     626
C    *                                                                    626
5351  CALL AGSPOPB                                                        627
      GO TO BLOCK_ERCM, (5350 ,5360)                                      627
C End of Local Procedure                                                  627
C ----------------------------------------------------------------------  628
C ------------ ERSM                                                       628
C Begin Local Procedure ERSM                                              629
C ERSM IS STAINLESS STEEL TIE ROD IN SHOWER MAX                           629
5520  CONTINUE                                                            629
         AG_TITLE='ERSM'                                                  629
         AG_MODULE=MODULE                                                 629
         AG_MARK='V00'                                                    629
         CALL AGSPUSH                                                     629
         AG_MODULE=MODULE                                                 629
C    *                                                                    629
C    Geant MATERIAL : ECAL_STEEL ===> inherited arguments used <===       630
         CALL AGSRESET                                                    630
         AG_TITLE = 'ECAL_STEEL'                                          630
         AG_EXNAME = 'MATERIAL'                                           630
         AG_PARLIST = 'NONE'                                              630
         CALL AXMATERIAL                                                  630
C    *                                                                    630
C    Geant ATTRIBUTE : ERSM ===> SEEN=1 COLO=6 FILL=1 <===                630
         CALL AGSRESET                                                    631
         AG_SEEN = 1                                                      631
         AG_COLO = 6                                                      631
         AG_FILL = 1                                                      631
         AG_TITLE = 'ERSM'                                                631
         AG_EXNAME = 'ATTRIBUTE'                                          631
         AG_PARLIST = 'SEEN_COLO_FILL'                                    631
         CALL AXATTRIBUTE                                                 631
C    *                                                                    631
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=EMCS_RTIE DZ=ZWIDTH/2 <===       631
         CALL AGSRESET                                                    632
         AG_RMIN = 0                                                      632
         AG_RMAX = EMCS_RTIE                                              632
         AG_DZ = ZWIDTH/2                                                 632
         AG_TITLE = 'TUBE'                                                632
         AG_EXNAME = 'SHAPE'                                              632
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      632
         CALL AXSHAPE                                                     632
C    *                                                                    632
5521  CALL AGSPOPB                                                        633
      GO TO BLOCK_ERSM, (5520 ,5530)                                      633
C End of Local Procedure                                                  633
C ----------------------------------------------------------------------  634
C ------------ EMOD                                                       634
C Begin Local Procedure EMOD                                              635
C EMOD (FSECT,LSECT) IS ONE MODULE OF THE EM ENDCAP                       635
5430  CONTINUE                                                            635
         AG_TITLE='EMOD'                                                  635
         AG_MODULE=MODULE                                                 635
         AG_MARK='V00'                                                    635
         CALL AGSPUSH                                                     635
         AG_MODULE=MODULE                                                 635
C    *                                                                    635
C    Geant ATTRIBUTE : EMOD ===> SEEN=1 COLO=3 SERIAL=FILLED <===         636
         CALL AGSRESET                                                    636
         AG_SEEN = 1                                                      636
         AG_COLO = 3                                                      636
         AG_SERIAL = FILLED                                               636
         AG_TITLE = 'EMOD'                                                636
         AG_EXNAME = 'ATTRIBUTE'                                          636
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  636
         CALL AXATTRIBUTE                                                 636
C    *                                                                    636
C    Geant MATERIAL : ECAL_AIR ===> inherited arguments used <===         636
         CALL AGSRESET                                                    637
         AG_TITLE = 'ECAL_AIR'                                            637
         AG_EXNAME = 'MATERIAL'                                           637
         AG_PARLIST = 'NONE'                                              637
         CALL AXMATERIAL                                                  637
C    *                                                                    637
C    Geant SHAPE : CONS ===> DZ=ZWIDTH/2 RMN1=ZSLICE*TAN_LOW-DD, RMX1=ZS  637
C    LICE*TAN_UPP+DUP RMN2=(ZSLICE+ZWIDTH)*TAN_LOW-DD, RMX2=(ZSLICE+ZWID  637
C    TH)*TAN_UPP+DUP, PHI1=EMCS_PHIMIN/EMCS_NSUPSEC PHI2=EMCS_PHIMAX/EMC  637
C    S_NSUPSEC <===                                                       637
         CALL AGSRESET                                                    641
         AG_DZ = ZWIDTH/2                                                 641
         AG_RMN1 = ZSLICE*TAN_LOW-DD                                      641
         AG_RMX1 = ZSLICE*TAN_UPP+DUP                                     641
         AG_RMN2 = (ZSLICE+ZWIDTH)*TAN_LOW-DD                             641
         AG_RMX2 = (ZSLICE+ZWIDTH)*TAN_UPP+DUP                            641
         AG_PHI1 = EMCS_PHIMIN/EMCS_NSUPSEC                               641
         AG_PHI2 = EMCS_PHIMAX/EMCS_NSUPSEC                               641
         AG_TITLE = 'CONS'                                                641
         AG_EXNAME = 'SHAPE'                                              641
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2_PHI1_PHI2'                  641
         CALL AXSHAPE                                                     641
C    *                                                                    641
         SECTION = ZSLICE                                                 641
         CURR = ZSLICE + ZWIDTH/2                                         643
C    *                                                                    644
         I_SECTION=FSECT                                                  644
            GO TO 5583                                                    644
5581        I_SECTION=I_SECTION+1                                         644
5583        IF(I_SECTION-(LSECT).GT.0) GO TO 5582                         644
C       use ESEC bank                                                     645
            BPATH_ESEC='ESEC*'                                            645
            IDNUM_ESEC(1)=0                                               645
            ESEC_ISECT=I_SECTION                                          645
            CALL AGDGETP('ECALGEO6','*',-999,'ESEC',NESEC_ISECT,          645
     *      RE_ESEC_ISECT,BPATH_ESEC,IDNUM_ESEC,1,MAP_ESEC,NAMES_ESEC,    645
     *      BANK_ESEC,LEN_ESEC,LLEN_ESEC,LINK_ESEC,FLAG_ESEC)             645
            SECWID = ESEC_CELL*ESEC_NLAYER                                646
C       Check I_SECTION==3|I_SECTION==5                                   647
            IF (I_SECTION.EQ.3.OR.I_SECTION.EQ.5) THEN                    647
            SECWID = SECWID - RADIATOR                                    648
            ELSEIF (I_SECTION.EQ.4) THEN                                  649
            SECWID = SECWID - ESEC_CELL + RADIATOR                        650
            ENDIF                                                         651
C       Check IPRIN>=1                                                    651
            IF (IPRIN.GE.1) WRITE(LOUT,5590)I_SECTION,                    651
     *      SECTION-CURR+SECWID/2                                         651
5590        FORMAT('+ ECVO isection=',I1,' zcenter=', F12.4)              653
C       Create ESEC :                                                     654
C       Call To Local Procedure ESEC                                      654
            ASSIGN 5610 TO BLOCK_ESEC                                     654
            GO TO  5600                                                   654
5610        CONTINUE                                                      654
C       *                                                                 654
C       Geant POSITION : ESEC ===> Z=SECTION-CURR+SECWID/2 <===           655
            CALL AGSRESET                                                 655
            AG_Z = SECTION-CURR+SECWID/2                                  655
            AG_TITLE = 'ESEC'                                             655
            AG_EXNAME = 'POSITION'                                        655
            AG_PARLIST = 'Z'                                              655
            CALL AXPOSITION                                               655
C       *                                                                 655
            SECTION = SECTION + SECWID                                    655
         GO TO 5581                                                       657
5582     CONTINUE                                                         657
5431  CALL AGSPOPB                                                        658
      GO TO BLOCK_EMOD, (5430 ,5440)                                      658
C End of Local Procedure                                                  658
C ----------------------------------------------------------------------  659
C ------------ ESEC                                                       659
C Begin Local Procedure ESEC                                              660
C ESEC IS A SINGLE EM SECTION                                             660
5600  CONTINUE                                                            660
         AG_TITLE='ESEC'                                                  660
         AG_MODULE=MODULE                                                 660
         AG_MARK='V00'                                                    660
         CALL AGSPUSH                                                     660
         AG_MODULE=MODULE                                                 660
C    *                                                                    660
C    Geant MATERIAL : ECAL_AIR ===> inherited arguments used <===         661
         CALL AGSRESET                                                    661
         AG_TITLE = 'ECAL_AIR'                                            661
         AG_EXNAME = 'MATERIAL'                                           661
         AG_PARLIST = 'NONE'                                              661
         CALL AXMATERIAL                                                  661
C    *                                                                    661
C    Geant ATTRIBUTE : ESEC ===> SEEN=1 COLO=1 SERIAL=FILLED LSTY=2 <===  661
         CALL AGSRESET                                                    662
         AG_SEEN = 1                                                      662
         AG_COLO = 1                                                      662
         AG_SERIAL = FILLED                                               662
         AG_LSTY = 2                                                      662
         AG_TITLE = 'ESEC'                                                662
         AG_EXNAME = 'ATTRIBUTE'                                          662
         AG_PARLIST = 'SEEN_COLO_SERIAL_LSTY'                             662
         CALL AXATTRIBUTE                                                 662
C    *                                                                    662
C    Geant SHAPE : CONS ===> DZ=SECWID/2 RMN1=(SECTION)*TAN_LOW-DD, RMX1  662
C    =(SECTION)*TAN_UPP+DUP RMN2=(SECTION+SECWID)*TAN_LOW-DD, RMX2=(SECT  662
C    ION+SECWID)*TAN_UPP+DUP <===                                         662
         CALL AGSRESET                                                    665
         AG_DZ = SECWID/2                                                 665
         AG_RMN1 = (SECTION)*TAN_LOW-DD                                   665
         AG_RMX1 = (SECTION)*TAN_UPP+DUP                                  665
         AG_RMN2 = (SECTION+SECWID)*TAN_LOW-DD                            665
         AG_RMX2 = (SECTION+SECWID)*TAN_UPP+DUP                           665
         AG_TITLE = 'CONS'                                                665
         AG_EXNAME = 'SHAPE'                                              665
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            665
         CALL AXSHAPE                                                     665
C    *                                                                    665
         LENGTH = - SECWID/2                                              665
         CURRENT = SECTION                                                667
         MEGATILE = ESEC_SCINT + EMCS_ALINCELL + EMCS_FRPLAST +           668
     *   EMCS_BKPLAST                                                     668
         GAP = ESEC_CELL - RADIATOR - MEGATILE                            669
C    Check IPRIN>=2                                                       669
         IF (IPRIN.GE.2) WRITE(LOUT,5620)I_SECTION,SECTION                669
5620     FORMAT(' ESEC:i_section,section',I3,F12.4)                       671
C    *                                                                    672
         IS=1                                                             672
            GO TO 5633                                                    672
5631        IS=IS+1                                                       672
5633        IF(IS-(ESEC_NLAYER).GT.0) GO TO 5632                          672
            CELL = ESEC_CELL                                              673
            PLATE = RADIATOR                                              674
C       Check IS==NINT(ESEC_NLAYER) & (I_SECTION==3|I_SECTION==5)         675
            IF (IS.EQ.NINT(ESEC_NLAYER) .AND.                             675
     *      (I_SECTION.EQ.3.OR.I_SECTION.EQ.5)) THEN                      675
            CELL = MEGATILE + GAP                                         676
            PLATE=0                                                       677
            ELSEIF (I_SECTION.EQ.4.AND.IS.EQ.1) THEN                      678
            CELL = RADIATOR                                               679
            ENDIF                                                         680
C       Check IPRIN>=2                                                    680
            IF (IPRIN.GE.2) WRITE(LOUT,5640)I_SECTION,IS,LENGTH,CELL,     680
     *      CURRENT                                                       680
5640        FORMAT(' ESEC_i_section,is,length,cell,current ',2I3,         682
     *      3F12.4)                                                       682
C       Check I_SECTION==4&IS==1                                          683
            IF (I_SECTION.EQ.4.AND.IS.EQ.1) THEN                          683
            CELL = RADIATOR + EMCS_SLOP                                   684
C       Check IPRIN>=1                                                    684
            IF (IPRIN.GE.1) WRITE(LOUT,5650)IS,CURRENT + CELL/            684
     *      2+ESEC_DELTAZ                                                 684
5650        FORMAT(' + ESEC radiator ilayer=',I2,' z=',F12.4)             686
C       Create ERAD :                                                     687
C       Call To Local Procedure ERAD                                      687
            ASSIGN 5670 TO BLOCK_ERAD                                     687
            GO TO  5660                                                   687
5670        CONTINUE                                                      687
C       *                                                                 687
C       Geant POSITION : ERAD ===> Z=LENGTH+(CELL)/2+ESEC_DELTAZ <===     688
            CALL AGSRESET                                                 688
            AG_Z = LENGTH+(CELL)/2+ESEC_DELTAZ                            688
            AG_TITLE = 'ERAD'                                             688
            AG_EXNAME = 'POSITION'                                        688
            AG_PARLIST = 'Z'                                              688
            CALL AXPOSITION                                               688
C       *                                                                 688
            LENGTH = LENGTH + CELL                                        688
            CURRENT = CURRENT + CELL                                      690
            ELSE                                                          691
            CELL = MEGATILE                                               692
C       Check FILLED==1                                                   693
            IF (FILLED.EQ.1) THEN                                         693
C       Create EMGT :                                                     694
C       Call To Local Procedure EMGT                                      694
            ASSIGN 5690 TO BLOCK_EMGT                                     694
            GO TO  5680                                                   694
5690        CONTINUE                                                      694
C       *                                                                 694
C       Geant POSITION : EMGT ===> Z=LENGTH+(GAP+CELL)/2+ESEC_DELTAZ <==  695
C       =                                                                 695
            CALL AGSRESET                                                 695
            AG_Z = LENGTH+(GAP+CELL)/2+ESEC_DELTAZ                        695
            AG_TITLE = 'EMGT'                                             695
            AG_EXNAME = 'POSITION'                                        695
            AG_PARLIST = 'Z'                                              695
            CALL AXPOSITION                                               695
C       *                                                                 695
            XX = CURRENT + (GAP+CELL)/2+ESEC_DELTAZ                       695
C       Check IPRIN>=2                                                    696
            IF (IPRIN.GE.2) WRITE(LOUT,5700)I_SECTION,IS,XX               696
5700        FORMAT(' mega i_section,is ',2I3,F10.4)                       698
C       Check IPRIN>=1                                                    698
            IF (IPRIN.GE.1) WRITE(LOUT,5710)IS,XX                         698
5710        FORMAT(' + ESEC megatile ilayer=',I2,' z=',F12.4)             700
            ENDIF                                                         701
            LENGTH = LENGTH + CELL + GAP                                  702
            CURRENT = CURRENT + CELL + GAP                                703
C       Check PLATE>0                                                     704
            IF (PLATE.GT.0) THEN                                          704
            CELL = RADIATOR                                               705
C       Check IPRIN>=1                                                    705
            IF (IPRIN.GE.1) WRITE(LOUT,5720)IS,CURRENT + CELL/            705
     *      2+ESEC_DELTAZ                                                 705
5720        FORMAT(' + ESEC radiator ilayer=',I2,' z=',F12.4)             707
C       Create ERAD :                                                     708
C       Call To Local Procedure ERAD                                      708
            ASSIGN 5730 TO BLOCK_ERAD                                     708
            GO TO  5660                                                   708
5730        CONTINUE                                                      708
C       *                                                                 708
C       Geant POSITION : ERAD ===> Z=LENGTH+CELL/2+ESEC_DELTAZ <===       709
            CALL AGSRESET                                                 709
            AG_Z = LENGTH+CELL/2+ESEC_DELTAZ                              709
            AG_TITLE = 'ERAD'                                             709
            AG_EXNAME = 'POSITION'                                        709
            AG_PARLIST = 'Z'                                              709
            CALL AXPOSITION                                               709
C       *                                                                 709
            LENGTH = LENGTH + CELL                                        709
            CURRENT = CURRENT + CELL                                      711
            ENDIF                                                         712
            ENDIF                                                         713
         GO TO 5631                                                       714
5632     CONTINUE                                                         714
5601  CALL AGSPOPB                                                        715
      GO TO BLOCK_ESEC, (5600 ,5610)                                      715
C End of Local Procedure                                                  715
C ----------------------------------------------------------------------  716
C ------------ EMGT                                                       716
C Begin Local Procedure EMGT                                              717
C EMGT IS A 30 DEGREE MEGATILE                                            717
5680  CONTINUE                                                            717
         AG_TITLE='EMGT'                                                  717
         AG_MODULE=MODULE                                                 717
         AG_MARK='V00'                                                    717
         CALL AGSPUSH                                                     717
         AG_MODULE=MODULE                                                 717
C    *                                                                    717
C    Geant MATERIAL : ECAL_AIR ===> inherited arguments used <===         718
         CALL AGSRESET                                                    718
         AG_TITLE = 'ECAL_AIR'                                            718
         AG_EXNAME = 'MATERIAL'                                           718
         AG_PARLIST = 'NONE'                                              718
         CALL AXMATERIAL                                                  718
C    *                                                                    718
C    Geant ATTRIBUTE : EMGT ===> SEEN=1 COLO=1 LSTY=2 <===                718
         CALL AGSRESET                                                    719
         AG_SEEN = 1                                                      719
         AG_COLO = 1                                                      719
         AG_LSTY = 2                                                      719
         AG_TITLE = 'EMGT'                                                719
         AG_EXNAME = 'ATTRIBUTE'                                          719
         AG_PARLIST = 'SEEN_COLO_LSTY'                                    719
         CALL AXATTRIBUTE                                                 719
C    *                                                                    719
C    Geant SHAPE : CONS ===> DZ=MEGATILE/2 RMN1=(CURRENT)*TAN_LOW-DD, RM  719
C    X1=(CURRENT)*TAN_UPP+DUP RMN2=(CURRENT+MEGATILE)*TAN_LOW-DD, RMX2=(  719
C    CURRENT+MEGATILE)*TAN_UPP+DUP <===                                   719
         CALL AGSRESET                                                    722
         AG_DZ = MEGATILE/2                                               722
         AG_RMN1 = (CURRENT)*TAN_LOW-DD                                   722
         AG_RMX1 = (CURRENT)*TAN_UPP+DUP                                  722
         AG_RMN2 = (CURRENT+MEGATILE)*TAN_LOW-DD                          722
         AG_RMX2 = (CURRENT+MEGATILE)*TAN_UPP+DUP                         722
         AG_TITLE = 'CONS'                                                722
         AG_EXNAME = 'SHAPE'                                              722
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            722
         CALL AXSHAPE                                                     722
C    *                                                                    722
         CALL GSTPAR(AG_IMED, 'CUTGAM',0.00001)                           722
         CALL GSTPAR(AG_IMED, 'CUTELE',0.00001)                           724
C    Check EABS_VERSION .GT. 0                                            725
         IF (EABS_VERSION .GT. 0) THEN                                    725
         CALL GSTPAR( AG_IMED, 'CUTGAM', EABS_CUTGAM )                    725
         CALL GSTPAR( AG_IMED, 'CUTELE', EABS_CUTELE )                    725
         CALL GSTPAR( AG_IMED, 'CUTHAD', EABS_CUTHAD )                    725
         CALL GSTPAR( AG_IMED, 'CUTNEU', EABS_CUTNEU )                    725
         CALL GSTPAR( AG_IMED, 'CUTMUO', EABS_CUTMUO )                    725
         CALL GSTPAR( AG_IMED, 'DCUTE', EABS_DCUTE )                      725
         CALL GSTPAR( AG_IMED, 'DCUTM', EABS_DCUTM )                      725
         CALL GSTPAR( AG_IMED, 'BCUTE', EABS_BCUTE )                      725
         CALL GSTPAR( AG_IMED, 'BCUTM', EABS_BCUTM )                      725
         ENDIF                                                            725
C    *                                                                    726
         ISEC=1                                                           726
            GO TO 5743                                                    726
5741        ISEC=ISEC+1                                                   726
5743        IF(ISEC-(NINT(EMCS_NSLICES)).GT.0) GO TO 5742                 726
            MYPHI = (EMCS_NSLICES/2-ISEC+0.5)*DPHI + ESEC_JIGGLE(IS)      727
C       Create EPER :                                                     728
C       Call To Local Procedure EPER                                      728
            ASSIGN 5760 TO BLOCK_EPER                                     728
            GO TO  5750                                                   728
5760        CONTINUE                                                      728
C       *                                                                 728
C       Geant POSITION : EPER ===> ALPHAZ=MYPHI <===                      729
            CALL AGSRESET                                                 729
            AG_ALPHAN=AG_ALPHAN+1                                         729
            AG_ALPHAK(AG_ALPHAN)=3                                        729
            AG_ALPHAL(AG_ALPHAN)=MYPHI                                    729
            AG_TITLE = 'EPER'                                             729
            AG_EXNAME = 'POSITION'                                        729
            AG_PARLIST = 'ALPHAZ'                                         729
            CALL AXPOSITION                                               729
C       *                                                                 729
         GO TO 5741                                                       730
5742     CONTINUE                                                         730
5681  CALL AGSPOPB                                                        731
      GO TO BLOCK_EMGT, (5680 ,5690)                                      731
C End of Local Procedure                                                  731
C ----------------------------------------------------------------------  732
C ------------ EPER                                                       732
C Begin Local Procedure EPER                                              733
C EPER IS A 5 DEGREE SLICE OF A 30 DEGREE MEGATILE (SUBSECTOR)            733
5750  CONTINUE                                                            733
         AG_TITLE='EPER'                                                  733
         AG_MODULE=MODULE                                                 733
         AG_MARK='V00'                                                    733
         CALL AGSPUSH                                                     733
         AG_MODULE=MODULE                                                 733
C    *                                                                    733
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       734
         CALL AGSRESET                                                    734
         AG_TITLE = 'POLYSTYREN'                                          734
         AG_EXNAME = 'MATERIAL'                                           734
         AG_PARLIST = 'NONE'                                              734
         CALL AXMATERIAL                                                  734
C    *                                                                    734
C    Geant MATERIAL : ECAL_POLYSTYREN ===> ISVOL=0 <===                   734
         CALL AGSRESET                                                    735
         AG_ISVOL = 0                                                     735
         AG_TITLE = 'ECAL_POLYSTYREN'                                     735
         AG_EXNAME = 'MATERIAL'                                           735
         AG_PARLIST = 'ISVOL'                                             735
         CALL AXMATERIAL                                                  735
C    *                                                                    735
C    Geant ATTRIBUTE : EPER ===> SEEN=1 COLO=1 LSTY=1 <===                735
         CALL AGSRESET                                                    736
         AG_SEEN = 1                                                      736
         AG_COLO = 1                                                      736
         AG_LSTY = 1                                                      736
         AG_TITLE = 'EPER'                                                736
         AG_EXNAME = 'ATTRIBUTE'                                          736
         AG_PARLIST = 'SEEN_COLO_LSTY'                                    736
         CALL AXATTRIBUTE                                                 736
C    *                                                                    736
C    Geant SHAPE : CONS ===> DZ=MEGATILE/2 RMN1=(CURRENT)*TAN_LOW-DD, RM  736
C    X1=(CURRENT)*TAN_UPP+DUP RMN2=(CURRENT+MEGATILE)*TAN_LOW-DD, RMX2=(  736
C    CURRENT+MEGATILE)*TAN_UPP+DUP, PHI1=EMCS_PHIMIN/EMCS_NSECTOR, PHI2=  736
C    EMCS_PHIMAX/EMCS_NSECTOR <===                                        736
         CALL AGSRESET                                                    741
         AG_DZ = MEGATILE/2                                               741
         AG_RMN1 = (CURRENT)*TAN_LOW-DD                                   741
         AG_RMX1 = (CURRENT)*TAN_UPP+DUP                                  741
         AG_RMN2 = (CURRENT+MEGATILE)*TAN_LOW-DD                          741
         AG_RMX2 = (CURRENT+MEGATILE)*TAN_UPP+DUP                         741
         AG_PHI1 = EMCS_PHIMIN/EMCS_NSECTOR                               741
         AG_PHI2 = EMCS_PHIMAX/EMCS_NSECTOR                               741
         AG_TITLE = 'CONS'                                                741
         AG_EXNAME = 'SHAPE'                                              741
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2_PHI1_PHI2'                  741
         CALL AXSHAPE                                                     741
C    *                                                                    741
C    Check EABS_VERSION .GT. 0                                            742
         IF (EABS_VERSION .GT. 0) THEN                                    742
         CALL GSTPAR( AG_IMED, 'CUTGAM', EABS_CUTGAM )                    742
         CALL GSTPAR( AG_IMED, 'CUTELE', EABS_CUTELE )                    742
         CALL GSTPAR( AG_IMED, 'CUTHAD', EABS_CUTHAD )                    742
         CALL GSTPAR( AG_IMED, 'CUTNEU', EABS_CUTNEU )                    742
         CALL GSTPAR( AG_IMED, 'CUTMUO', EABS_CUTMUO )                    742
         CALL GSTPAR( AG_IMED, 'DCUTE', EABS_DCUTE )                      742
         CALL GSTPAR( AG_IMED, 'DCUTM', EABS_DCUTM )                      742
         CALL GSTPAR( AG_IMED, 'BCUTE', EABS_BCUTE )                      742
         CALL GSTPAR( AG_IMED, 'BCUTM', EABS_BCUTM )                      742
         ENDIF                                                            742
         CURCL = CURRENT + MEGATILE/2                                     743
C    *                                                                    744
         IE=1                                                             744
            GO TO 5773                                                    744
5771        IE=IE+1                                                       744
5773        IF(IE-(NINT(EETR_NETA)).GT.0) GO TO 5772                      744
            ETABOT = EETR_ETABIN(IE)                                      745
            ETATOP = EETR_ETABIN(IE+1)                                    746
            RBOT=(CURCL)*TANF(ETABOT)                                     747
            RTOP=MIN((CURCL)*TANF(ETATOP), ((CURRENT)*TAN_UPP+DUP))       748
C       Skip Unless RBOT<RTOP                                             749
            IF (RBOT.GE.RTOP)GO TO 5771                                   749
            XX=TAN(PI*EMCS_PHIMAX/180.0/EMCS_NSECTOR)                     750
            YY=COS(PI*EMCS_PHIMAX/180.0/EMCS_NSECTOR)                     751
C       Create ETAR :                                                     752
C       Call To Local Procedure ETAR                                      752
            ASSIGN 5790 TO BLOCK_ETAR                                     752
            GO TO  5780                                                   752
5790        CONTINUE                                                      752
C       *                                                                 752
C       Geant POSITION : ETAR ===> X=(RBOT+RTOP)/2 ORT=YZX <===           753
            CALL AGSRESET                                                 753
            AG_X = (RBOT+RTOP)/2                                          753
            AG_ALPHAN=AG_ALPHAN+1                                         753
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              753
            AG_TITLE = 'ETAR'                                             753
            AG_EXNAME = 'POSITION'                                        753
            AG_PARLIST = 'X_ORT'                                          753
            CALL AXPOSITION                                               753
C       *                                                                 753
C       Check IPRIN>=2                                                    753
            IF (IPRIN.GE.2) WRITE(LOUT,5800)IE,ETATOP,ETABOT,RBOT,RTOP    753
5800        FORMAT(' EPER : ie,etatop,etabot,rbot,rtop ',I3,4F12.4)       755
         GO TO 5771                                                       756
5772     CONTINUE                                                         756
5751  CALL AGSPOPB                                                        757
      GO TO BLOCK_EPER, (5750 ,5760)                                      757
C End of Local Procedure                                                  757
C ----------------------------------------------------------------------  758
C ------------ ETAR                                                       758
C Begin Local Procedure ETAR                                              760
C ETAR IS A SINGLE CALORIMETER CELL, CONTAINING SCINTILLATOR, FIBER ROUT  760
C ER, ETC...                                                              760
5780  CONTINUE                                                            760
         AG_TITLE='ETAR'                                                  760
         AG_MODULE=MODULE                                                 760
         AG_MARK='V00'                                                    760
         CALL AGSPUSH                                                     760
         AG_MODULE=MODULE                                                 760
C    *                                                                    760
C    Geant MATERIAL : ECAL_POLYSTYREN ===> inherited arguments used <===  761
         CALL AGSRESET                                                    761
         AG_TITLE = 'ECAL_POLYSTYREN'                                     761
         AG_EXNAME = 'MATERIAL'                                           761
         AG_PARLIST = 'NONE'                                              761
         CALL AXMATERIAL                                                  761
C    *                                                                    761
C    Geant ATTRIBUTE : ETAR ===> SEEN=1 COLO=4 LSTY=1 <===                761
         CALL AGSRESET                                                    762
         AG_SEEN = 1                                                      762
         AG_COLO = 4                                                      762
         AG_LSTY = 1                                                      762
         AG_TITLE = 'ETAR'                                                762
         AG_EXNAME = 'ATTRIBUTE'                                          762
         AG_PARLIST = 'SEEN_COLO_LSTY'                                    762
         CALL AXATTRIBUTE                                                 762
C    *                                                                    762
C    Geant SHAPE : TRD1 ===> DX1=RBOT*XX-EMCS_GAPCEL/YY DX2=RTOP*XX-EMCS  762
C    _GAPCEL/YY, DY=MEGATILE/2 DZ=(RTOP-RBOT)/2 <===                      762
         CALL AGSRESET                                                    764
         AG_DX1 = RBOT*XX-EMCS_GAPCEL/YY                                  764
         AG_DX2 = RTOP*XX-EMCS_GAPCEL/YY                                  764
         AG_DY = MEGATILE/2                                               764
         AG_DZ = (RTOP-RBOT)/2                                            764
         AG_TITLE = 'TRD1'                                                764
         AG_EXNAME = 'SHAPE'                                              764
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     764
         CALL AXSHAPE                                                     764
C    *                                                                    764
C    Create EALP :                                                        765
C    Call To Local Procedure EALP                                         765
         ASSIGN 5820 TO BLOCK_EALP                                        765
         GO TO  5810                                                      765
5820     CONTINUE                                                         765
C    *                                                                    765
C    Geant POSITION : EALP ===> Y=(-MEGATILE+EMCS_ALINCELL)/2 <===        766
         CALL AGSRESET                                                    766
         AG_Y = (-MEGATILE+EMCS_ALINCELL)/2                               766
         AG_TITLE = 'EALP'                                                766
         AG_EXNAME = 'POSITION'                                           766
         AG_PARLIST = 'Y'                                                 766
         CALL AXPOSITION                                                  766
C    *                                                                    766
         G10 = ESEC_SCINT                                                 766
C    Create ESCI :                                                        768
C    Call To Local Procedure ESCI                                         768
         ASSIGN 5840 TO BLOCK_ESCI                                        768
         GO TO  5830                                                      768
5840     CONTINUE                                                         768
C    *                                                                    768
C    Geant POSITION : ESCI ===> Y=(-MEGATILE+G10)/2+EMCS_ALINCELL+EMCS_F  769
C    RPLAST <===                                                          769
         CALL AGSRESET                                                    769
         AG_Y = (-MEGATILE+G10)/2+EMCS_ALINCELL+EMCS_FRPLAST              769
         AG_TITLE = 'ESCI'                                                769
         AG_EXNAME = 'POSITION'                                           769
         AG_PARLIST = 'Y'                                                 769
         CALL AXPOSITION                                                  769
C    *                                                                    769
5781  CALL AGSPOPB                                                        770
      GO TO BLOCK_ETAR, (5780 ,5790)                                      770
C End of Local Procedure                                                  770
C ----------------------------------------------------------------------  771
C ------------ ESCI                                                       771
C Begin Local Procedure ESCI                                              772
C ESCI IS THE ACTIVE SCINTILLATOR (POLYSTYRENE) LAYER                     772
5830  CONTINUE                                                            772
         AG_TITLE='ESCI'                                                  772
         AG_MODULE=MODULE                                                 772
         AG_MARK='V00'                                                    772
         CALL AGSPUSH                                                     772
         AG_MODULE=MODULE                                                 772
C    *                                                                    772
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       773
         CALL AGSRESET                                                    773
         AG_TITLE = 'POLYSTYREN'                                          773
         AG_EXNAME = 'MATERIAL'                                           773
         AG_PARLIST = 'NONE'                                              773
         CALL AXMATERIAL                                                  773
C    *                                                                    773
C    Geant MATERIAL : ECAL_SCINT ===> ISVOL=1 <===                        773
         CALL AGSRESET                                                    774
         AG_ISVOL = 1                                                     774
         AG_TITLE = 'ECAL_SCINT'                                          774
         AG_EXNAME = 'MATERIAL'                                           774
         AG_PARLIST = 'ISVOL'                                             774
         CALL AXMATERIAL                                                  774
C    *                                                                    774
C    Geant ATTRIBUTE : ESCI ===> SEEN=1 COLO=7 FILL=0 LSTY=1 <===         774
         CALL AGSRESET                                                    775
         AG_SEEN = 1                                                      775
         AG_COLO = 7                                                      775
         AG_FILL = 0                                                      775
         AG_LSTY = 1                                                      775
         AG_TITLE = 'ESCI'                                                775
         AG_EXNAME = 'ATTRIBUTE'                                          775
         AG_PARLIST = 'SEEN_COLO_FILL_LSTY'                               775
         CALL AXATTRIBUTE                                                 775
C    *                                                                    775
C    Geant SHAPE : TRD1 ===> DY=ESEC_SCINT/2 DZ=(RTOP-RBOT)/2-EMCS_GAPCE  775
C    L <===                                                               775
         CALL AGSRESET                                                    776
         AG_DY = ESEC_SCINT/2                                             776
         AG_DZ = (RTOP-RBOT)/2-EMCS_GAPCEL                                776
         AG_TITLE = 'TRD1'                                                776
         AG_EXNAME = 'SHAPE'                                              776
         AG_PARLIST = 'DY_DZ'                                             776
         CALL AXSHAPE                                                     776
C    *                                                                    776
         CALL GSTPAR(AG_IMED, 'CUTGAM',0.00008)                           776
         CALL GSTPAR(AG_IMED, 'CUTELE',0.001)                             778
         CALL GSTPAR(AG_IMED, 'BCUTE',0.0001)                             779
         CALL GSTPAR(AG_IMED, 'CUTNEU',0.001)                             780
         CALL GSTPAR(AG_IMED, 'CUTHAD',0.001)                             781
         CALL GSTPAR(AG_IMED, 'CUTMUO',0.001)                             782
C    Check ESEN_VERSION .GT. 0                                            783
         IF (ESEN_VERSION .GT. 0) THEN                                    783
         CALL GSTPAR( AG_IMED, 'CUTGAM', ESEN_CUTGAM )                    783
         CALL GSTPAR( AG_IMED, 'CUTELE', ESEN_CUTELE )                    783
         CALL GSTPAR( AG_IMED, 'CUTHAD', ESEN_CUTHAD )                    783
         CALL GSTPAR( AG_IMED, 'CUTNEU', ESEN_CUTNEU )                    783
         CALL GSTPAR( AG_IMED, 'CUTMUO', ESEN_CUTMUO )                    783
         CALL GSTPAR( AG_IMED, 'DCUTE', ESEN_DCUTE )                      783
         CALL GSTPAR( AG_IMED, 'DCUTM', ESEN_DCUTM )                      783
         CALL GSTPAR( AG_IMED, 'BCUTE', ESEN_BCUTE )                      783
         CALL GSTPAR( AG_IMED, 'BCUTM', ESEN_BCUTM )                      783
         ENDIF                                                            783
         CALL GSTPAR(AG_IMED, 'BIRK1', 1.)                                784
         CALL GSTPAR(AG_IMED, 'BIRK2', 0.013)                             785
         CALL GSTPAR(AG_IMED, 'BIRK3', 9.6E-6)                            786
C    Defining Hits: BIRK:0:(0,10)                                         787
         AG_TITLE = 'ESCI'                                                787
         AG_EXNAME = 'HITS'                                               787
         AG_TYPE=AG_MAGIC                                                 787
         CALL AGSDETV('ESCI')                                             787
C    *                                                                    787
         AG_PARLIST='BIRK'                                                787
         AG_TITLE = 'DETH'                                                787
         AG_NBITS = 0                                                     787
         AG_BIN=AG_MAGIC                                                  787
         AG_OPTION='*'                                                    787
         AG_FHMIN = 0                                                     787
         AG_FHMAX = 10                                                    787
         CALL AGSDETH (0)                                                 787
C    *                                                                    787
         CALL AGSDETU('ESCI')                                             787
C    *                                                                    787
5831  CALL AGSPOPB                                                        788
      GO TO BLOCK_ESCI, (5830 ,5840)                                      788
C End of Local Procedure                                                  788
C ----------------------------------------------------------------------  789
C ------------ ERAD                                                       789
C Begin Local Procedure ERAD                                              790
C ERAD IS THE LEAD RADIATOR WITH STAINLESS STEEL CLADDING                 790
5660  CONTINUE                                                            790
         AG_TITLE='ERAD'                                                  790
         AG_MODULE=MODULE                                                 790
         AG_MARK='V00'                                                    790
         CALL AGSPUSH                                                     790
         AG_MODULE=MODULE                                                 790
C    *                                                                    790
C    Geant MATERIAL : ECAL_STEEL ===> inherited arguments used <===       791
         CALL AGSRESET                                                    791
         AG_TITLE = 'ECAL_STEEL'                                          791
         AG_EXNAME = 'MATERIAL'                                           791
         AG_PARLIST = 'NONE'                                              791
         CALL AXMATERIAL                                                  791
C    *                                                                    791
C    Geant ATTRIBUTE : ERAD ===> SEEN=1 COLO=6 FILL=1 LSTY=1 <===         791
         CALL AGSRESET                                                    792
         AG_SEEN = 1                                                      792
         AG_COLO = 6                                                      792
         AG_FILL = 1                                                      792
         AG_LSTY = 1                                                      792
         AG_TITLE = 'ERAD'                                                792
         AG_EXNAME = 'ATTRIBUTE'                                          792
         AG_PARLIST = 'SEEN_COLO_FILL_LSTY'                               792
         CALL AXATTRIBUTE                                                 792
C    *                                                                    792
C    Geant SHAPE : CONS ===> DZ=RADIATOR/2 RMN1=(CURRENT)*TAN_LOW-DD, RM  792
C    X1=(CURRENT)*TAN_UPP+DUP RMN2=(CURRENT+CELL)*TAN_LOW-DD, RMX2=(CURR  792
C    ENT+RADIATOR)*TAN_UPP+DUP <===                                       792
         CALL AGSRESET                                                    795
         AG_DZ = RADIATOR/2                                               795
         AG_RMN1 = (CURRENT)*TAN_LOW-DD                                   795
         AG_RMX1 = (CURRENT)*TAN_UPP+DUP                                  795
         AG_RMN2 = (CURRENT+CELL)*TAN_LOW-DD                              795
         AG_RMX2 = (CURRENT+RADIATOR)*TAN_UPP+DUP                         795
         AG_TITLE = 'CONS'                                                795
         AG_EXNAME = 'SHAPE'                                              795
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            795
         CALL AXSHAPE                                                     795
C    *                                                                    795
C    Create ELED :                                                        796
C    Call To Local Procedure ELED                                         796
         ASSIGN 5860 TO BLOCK_ELED                                        796
         GO TO  5850                                                      796
5860     CONTINUE                                                         796
C    *                                                                    796
C    Geant POSITION : ELED ===> inherited arguments used <===             797
         CALL AGSRESET                                                    797
         AG_TITLE = 'ELED'                                                797
         AG_EXNAME = 'POSITION'                                           797
         AG_PARLIST = 'NONE'                                              797
         CALL AXPOSITION                                                  797
C    *                                                                    797
5661  CALL AGSPOPB                                                        798
      GO TO BLOCK_ERAD, (5660 ,5670,5730)                                 798
C End of Local Procedure                                                  798
C ----------------------------------------------------------------------  799
C ------------ ELED                                                       799
C Begin Local Procedure ELED                                              800
C ELED IS A LEAD ABSORBER PLATE                                           800
5850  CONTINUE                                                            800
         AG_TITLE='ELED'                                                  800
         AG_MODULE=MODULE                                                 800
         AG_MARK='V00'                                                    800
         CALL AGSPUSH                                                     800
         AG_MODULE=MODULE                                                 800
C    *                                                                    800
C    Geant COMPONENT : SN ===> A=118.710 Z=50 W=0.014 <===                801
         CALL AGSRESET                                                    801
         AG_A = 118.710                                                   801
         AG_Z = 50                                                        801
         AG_W = 0.014                                                     801
         AG_TITLE = 'SN'                                                  801
         AG_EXNAME = 'COMPONENT'                                          801
         AG_PARLIST = 'A_Z_W'                                             801
         CALL AXCOMPONENT                                                 801
C    *                                                                    801
C    Geant COMPONENT : CA ===> A=40.0780 Z=20 W=0.00075 <===              801
         CALL AGSRESET                                                    802
         AG_A = 40.0780                                                   802
         AG_Z = 20                                                        802
         AG_W = 0.00075                                                   802
         AG_TITLE = 'CA'                                                  802
         AG_EXNAME = 'COMPONENT'                                          802
         AG_PARLIST = 'A_Z_W'                                             802
         CALL AXCOMPONENT                                                 802
C    *                                                                    802
C    Geant COMPONENT : AL ===> A=26.9815 Z=13 W=0.0003 <===               802
         CALL AGSRESET                                                    803
         AG_A = 26.9815                                                   803
         AG_Z = 13                                                        803
         AG_W = 0.0003                                                    803
         AG_TITLE = 'AL'                                                  803
         AG_EXNAME = 'COMPONENT'                                          803
         AG_PARLIST = 'A_Z_W'                                             803
         CALL AXCOMPONENT                                                 803
C    *                                                                    803
C    Geant COMPONENT : PB ===> A=207.190 Z=82 W=0.98495 <===              803
         CALL AGSRESET                                                    804
         AG_A = 207.190                                                   804
         AG_Z = 82                                                        804
         AG_W = 0.98495                                                   804
         AG_TITLE = 'PB'                                                  804
         AG_EXNAME = 'COMPONENT'                                          804
         AG_PARLIST = 'A_Z_W'                                             804
         CALL AXCOMPONENT                                                 804
C    *                                                                    804
C    Geant MIXTURE : ECAL_PBALLOY ===> DENS=11.35 <===                    804
         CALL AGSRESET                                                    805
         AG_DENS = 11.35                                                  805
         AG_TITLE = 'ECAL_PBALLOY'                                        805
         AG_EXNAME = 'MIXTURE'                                            805
         AG_PARLIST = 'DENS'                                              805
         CALL AXMIXTURE                                                   805
C    *                                                                    805
C    Geant ATTRIBUTE : ELED ===> SEEN=1 COLO=4 FILL=1 LSTY=1 <===         805
         CALL AGSRESET                                                    806
         AG_SEEN = 1                                                      806
         AG_COLO = 4                                                      806
         AG_FILL = 1                                                      806
         AG_LSTY = 1                                                      806
         AG_TITLE = 'ELED'                                                806
         AG_EXNAME = 'ATTRIBUTE'                                          806
         AG_PARLIST = 'SEEN_COLO_FILL_LSTY'                               806
         CALL AXATTRIBUTE                                                 806
C    *                                                                    806
C    Geant SHAPE : TUBS ===> RMIN=(CURRENT)*TAN_LOW RMAX=(CURRENT+EMCS_P  806
C    BPLATE)*TAN_UPP, DZ=EMCS_PBPLATE/2 <===                              806
         CALL AGSRESET                                                    808
         AG_RMIN = (CURRENT)*TAN_LOW                                      808
         AG_RMAX = (CURRENT+EMCS_PBPLATE)*TAN_UPP                         808
         AG_DZ = EMCS_PBPLATE/2                                           808
         AG_TITLE = 'TUBS'                                                808
         AG_EXNAME = 'SHAPE'                                              808
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      808
         CALL AXSHAPE                                                     808
C    *                                                                    808
         CALL GSTPAR(AG_IMED, 'CUTGAM',0.00008)                           808
         CALL GSTPAR(AG_IMED, 'CUTELE',0.001)                             810
         CALL GSTPAR(AG_IMED, 'BCUTE',0.0001)                             811
         CALL GSTPAR(AG_IMED, 'CUTNEU',0.001)                             812
         CALL GSTPAR(AG_IMED, 'CUTHAD',0.001)                             813
         CALL GSTPAR(AG_IMED, 'CUTMUO',0.001)                             814
C    Check EABS_VERSION .GT. 0                                            815
         IF (EABS_VERSION .GT. 0) THEN                                    815
         CALL GSTPAR( AG_IMED, 'CUTGAM', EABS_CUTGAM )                    815
         CALL GSTPAR( AG_IMED, 'CUTELE', EABS_CUTELE )                    815
         CALL GSTPAR( AG_IMED, 'CUTHAD', EABS_CUTHAD )                    815
         CALL GSTPAR( AG_IMED, 'CUTNEU', EABS_CUTNEU )                    815
         CALL GSTPAR( AG_IMED, 'CUTMUO', EABS_CUTMUO )                    815
         CALL GSTPAR( AG_IMED, 'DCUTE', EABS_DCUTE )                      815
         CALL GSTPAR( AG_IMED, 'DCUTM', EABS_DCUTM )                      815
         CALL GSTPAR( AG_IMED, 'BCUTE', EABS_BCUTE )                      815
         CALL GSTPAR( AG_IMED, 'BCUTM', EABS_BCUTM )                      815
         ENDIF                                                            815
5851  CALL AGSPOPB                                                        816
      GO TO BLOCK_ELED, (5850 ,5860)                                      816
C End of Local Procedure                                                  816
C ----------------------------------------------------------------------  817
C ------------ EFLP                                                       817
C Begin Local Procedure EFLP                                              818
C EFLP IS THE ALUMINUM (ALUMINIUM) FRONT PLATE OF THE ENDCAP              818
5160  CONTINUE                                                            818
         AG_TITLE='EFLP'                                                  818
         AG_MODULE=MODULE                                                 818
         AG_MARK='V00'                                                    818
         CALL AGSPUSH                                                     818
         AG_MODULE=MODULE                                                 818
C    *                                                                    818
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        819
         CALL AGSRESET                                                    819
         AG_TITLE = 'ALUMINIUM'                                           819
         AG_EXNAME = 'MATERIAL'                                           819
         AG_PARLIST = 'NONE'                                              819
         CALL AXMATERIAL                                                  819
C    *                                                                    819
C    Geant MATERIAL : ECAL_ALUMINIUM ===> ISVOL=0 <===                    819
         CALL AGSRESET                                                    820
         AG_ISVOL = 0                                                     820
         AG_TITLE = 'ECAL_ALUMINIUM'                                      820
         AG_EXNAME = 'MATERIAL'                                           820
         AG_PARLIST = 'ISVOL'                                             820
         CALL AXMATERIAL                                                  820
C    *                                                                    820
C    Geant ATTRIBUTE : EFLP ===> SEEN=1 COLO=3 FILL=1 LSTY=1 <===         820
         CALL AGSRESET                                                    821
         AG_SEEN = 1                                                      821
         AG_COLO = 3                                                      821
         AG_FILL = 1                                                      821
         AG_LSTY = 1                                                      821
         AG_TITLE = 'EFLP'                                                821
         AG_EXNAME = 'ATTRIBUTE'                                          821
         AG_PARLIST = 'SEEN_COLO_FILL_LSTY'                               821
         CALL AXATTRIBUTE                                                 821
C    *                                                                    821
C    Geant SHAPE : CONS ===> DZ=EMCS_FRONT/2 RMN1=68.813 RMX1=(ZSLICE)*T  821
C    AN_UPP+DUP, RMN2=68.813 RMX2=(ZSLICE+ZWIDTH)*TAN_UPP+DUP, PHI1=EMCS  821
C    _PHIMIN PHI2=EMCS_PHIMAX <===                                        821
         CALL AGSRESET                                                    824
         AG_DZ = EMCS_FRONT/2                                             824
         AG_RMN1 = 68.813                                                 824
         AG_RMX1 = (ZSLICE)*TAN_UPP+DUP                                   824
         AG_RMN2 = 68.813                                                 824
         AG_RMX2 = (ZSLICE+ZWIDTH)*TAN_UPP+DUP                            824
         AG_PHI1 = EMCS_PHIMIN                                            824
         AG_PHI2 = EMCS_PHIMAX                                            824
         AG_TITLE = 'CONS'                                                824
         AG_EXNAME = 'SHAPE'                                              824
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2_PHI1_PHI2'                  824
         CALL AXSHAPE                                                     824
C    *                                                                    824
C    Check EABS_VERSION .GT. 0                                            825
         IF (EABS_VERSION .GT. 0) THEN                                    825
         CALL GSTPAR( AG_IMED, 'CUTGAM', EABS_CUTGAM )                    825
         CALL GSTPAR( AG_IMED, 'CUTELE', EABS_CUTELE )                    825
         CALL GSTPAR( AG_IMED, 'CUTHAD', EABS_CUTHAD )                    825
         CALL GSTPAR( AG_IMED, 'CUTNEU', EABS_CUTNEU )                    825
         CALL GSTPAR( AG_IMED, 'CUTMUO', EABS_CUTMUO )                    825
         CALL GSTPAR( AG_IMED, 'DCUTE', EABS_DCUTE )                      825
         CALL GSTPAR( AG_IMED, 'DCUTM', EABS_DCUTM )                      825
         CALL GSTPAR( AG_IMED, 'BCUTE', EABS_BCUTE )                      825
         CALL GSTPAR( AG_IMED, 'BCUTM', EABS_BCUTM )                      825
         ENDIF                                                            825
5161  CALL AGSPOPB                                                        826
      GO TO BLOCK_EFLP, (5160 ,5170)                                      826
C End of Local Procedure                                                  826
C ----------------------------------------------------------------------  827
C ------------ EALP                                                       827
C Begin Local Procedure EALP                                              828
C EALP IS THE THIN ALUMINIUM PLATE IN CALORIMETER CELL                    828
5810  CONTINUE                                                            828
         AG_TITLE='EALP'                                                  828
         AG_MODULE=MODULE                                                 828
         AG_MARK='V00'                                                    828
         CALL AGSPUSH                                                     828
         AG_MODULE=MODULE                                                 828
C    *                                                                    828
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        829
         CALL AGSRESET                                                    829
         AG_TITLE = 'ALUMINIUM'                                           829
         AG_EXNAME = 'MATERIAL'                                           829
         AG_PARLIST = 'NONE'                                              829
         CALL AXMATERIAL                                                  829
C    *                                                                    829
C    Geant MATERIAL : ECAL_ALUPLATE ===> ISVOL=0 <===                     829
         CALL AGSRESET                                                    830
         AG_ISVOL = 0                                                     830
         AG_TITLE = 'ECAL_ALUPLATE'                                       830
         AG_EXNAME = 'MATERIAL'                                           830
         AG_PARLIST = 'ISVOL'                                             830
         CALL AXMATERIAL                                                  830
C    *                                                                    830
C    Geant ATTRIBUTE : EALP ===> SEEN=1 COLO=1 LSTY=1 <===                830
         CALL AGSRESET                                                    831
         AG_SEEN = 1                                                      831
         AG_COLO = 1                                                      831
         AG_LSTY = 1                                                      831
         AG_TITLE = 'EALP'                                                831
         AG_EXNAME = 'ATTRIBUTE'                                          831
         AG_PARLIST = 'SEEN_COLO_LSTY'                                    831
         CALL AXATTRIBUTE                                                 831
C    *                                                                    831
C    Geant SHAPE : TRD1 ===> DY=EMCS_ALINCELL/2 DZ=(RTOP-RBOT)/2 <===     831
         CALL AGSRESET                                                    832
         AG_DY = EMCS_ALINCELL/2                                          832
         AG_DZ = (RTOP-RBOT)/2                                            832
         AG_TITLE = 'TRD1'                                                832
         AG_EXNAME = 'SHAPE'                                              832
         AG_PARLIST = 'DY_DZ'                                             832
         CALL AXSHAPE                                                     832
C    *                                                                    832
         CALL GSTPAR(AG_IMED, 'CUTGAM',0.00001)                           832
         CALL GSTPAR(AG_IMED, 'CUTELE',0.00001)                           834
         CALL GSTPAR(AG_IMED, 'LOSS', 1.)                                 835
         CALL GSTPAR(AG_IMED, 'STRA', 1.)                                 836
C    Check EABS_VERSION .GT. 0                                            837
         IF (EABS_VERSION .GT. 0) THEN                                    837
         CALL GSTPAR( AG_IMED, 'CUTGAM', EABS_CUTGAM )                    837
         CALL GSTPAR( AG_IMED, 'CUTELE', EABS_CUTELE )                    837
         CALL GSTPAR( AG_IMED, 'CUTHAD', EABS_CUTHAD )                    837
         CALL GSTPAR( AG_IMED, 'CUTNEU', EABS_CUTNEU )                    837
         CALL GSTPAR( AG_IMED, 'CUTMUO', EABS_CUTMUO )                    837
         CALL GSTPAR( AG_IMED, 'DCUTE', EABS_DCUTE )                      837
         CALL GSTPAR( AG_IMED, 'DCUTM', EABS_DCUTM )                      837
         CALL GSTPAR( AG_IMED, 'BCUTE', EABS_BCUTE )                      837
         CALL GSTPAR( AG_IMED, 'BCUTM', EABS_BCUTM )                      837
         ENDIF                                                            837
5811  CALL AGSPOPB                                                        838
      GO TO BLOCK_EALP, (5810 ,5820)                                      838
C End of Local Procedure                                                  838
C ----------------------------------------------------------------------  839
C ------------ ESPL                                                       839
C Begin Local Procedure ESPL                                              840
C ESPL IS THE LOGICAL VOLUME CONTAINING AN SMD PLANE                      840
5500  CONTINUE                                                            840
         AG_TITLE='ESPL'                                                  840
         AG_MODULE=MODULE                                                 840
         AG_MARK='V00'                                                    840
         CALL AGSPUSH                                                     840
         AG_MODULE=MODULE                                                 840
C    *                                                                    840
C    Geant MATERIAL : ECAL_AIR ===> inherited arguments used <===         841
         CALL AGSRESET                                                    841
         AG_TITLE = 'ECAL_AIR'                                            841
         AG_EXNAME = 'MATERIAL'                                           841
         AG_PARLIST = 'NONE'                                              841
         CALL AXMATERIAL                                                  841
C    *                                                                    841
C    Geant ATTRIBUTE : ESPL ===> SEEN=1 COLO=4 LSTY=4 <===                841
         CALL AGSRESET                                                    842
         AG_SEEN = 1                                                      842
         AG_COLO = 4                                                      842
         AG_LSTY = 4                                                      842
         AG_TITLE = 'ESPL'                                                842
         AG_EXNAME = 'ATTRIBUTE'                                          842
         AG_PARLIST = 'SEEN_COLO_LSTY'                                    842
         CALL AXATTRIBUTE                                                 842
C    *                                                                    842
C    Geant SHAPE : TUBS ===> RMIN=SECTION*TAN_LOW-1.526 RMAX=(SECTION-SE  842
C    CWID/2)*TAN_UPP+DUP, PHI1=EMCS_PHIMIN, PHI2=EMCS_PHIMAX DZ=EMCS_GAP  842
C    SMD/3/2 <===                                                         842
         CALL AGSRESET                                                    845
         AG_RMIN = SECTION*TAN_LOW-1.526                                  845
         AG_RMAX = (SECTION-SECWID/2)*TAN_UPP+DUP                         845
         AG_PHI1 = EMCS_PHIMIN                                            845
         AG_PHI2 = EMCS_PHIMAX                                            845
         AG_DZ = EMCS_GAPSMD/3/2                                          845
         AG_TITLE = 'TUBS'                                                845
         AG_EXNAME = 'SHAPE'                                              845
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            845
         CALL AXSHAPE                                                     845
C    *                                                                    845
C    use EMXG bank                                                        845
         BPATH_EMXG='EMXG*'                                               846
         IDNUM_EMXG(1)=0                                                  846
         EMXG_VERSION=1                                                   846
         CALL AGDGETP('ECALGEO6','*',-999,'EMXG',NEMXG_VERSION,           846
     *   RE_EMXG_VERSION,BPATH_EMXG,IDNUM_EMXG,1,MAP_EMXG,NAMES_EMXG,     846
     *   BANK_EMXG,LEN_EMXG,LLEN_EMXG,LINK_EMXG,FLAG_EMXG)                846
         MSECWD =(EMXG_SAPEX + EMXG_F4)/2                                 847
C    *                                                                    848
         ISEC=1                                                           848
            GO TO 5873                                                    848
5871        ISEC=ISEC+1                                                   848
5873        IF(ISEC-(6).GT.0) GO TO 5872                                  848
            CUT=1                                                         849
            D3 = 75 - (ISEC-1)*30                                         850
C       Check EXSE_SECTYPE(ISEC)==0|(EMCG_FILLMODE==1&(ISEC==6|ISEC==1))  851
            IF                                                            851
     *      (EXSE_SECTYPE(ISEC).EQ.0.OR.(EMCG_FILLMODE.EQ.1.AND.(ISEC.    851
     *      EQ.6.OR.ISEC.EQ.1))) THEN                                     851
            CUT = 0                                                       852
            ELSEIF (EXSE_SECTYPE(ISEC).EQ.1) THEN                         853
C       Create EXSG :                                                     854
C       Call To Local Procedure EXSG                                      854
            ASSIGN 5890 TO BLOCK_EXSG                                     854
            GO TO  5880                                                   854
5890        CONTINUE                                                      854
C       *                                                                 854
C       Geant POSITION : EXSG ===> ALPHAZ=D3 NCOPY=ISEC KONLY='MANY' <==  855
C       =                                                                 855
            CALL AGSRESET                                                 855
            AG_ALPHAN=AG_ALPHAN+1                                         855
            AG_ALPHAK(AG_ALPHAN)=3                                        855
            AG_ALPHAL(AG_ALPHAN)=D3                                       855
            AG_NCOPY = ISEC                                               855
            AG_KONLY = 'MANY'                                             855
            AG_TITLE = 'EXSG'                                             855
            AG_EXNAME = 'POSITION'                                        855
            AG_PARLIST = 'ALPHAZ_NCOPY_KONLY'                             855
            CALL AXPOSITION                                               855
C       *                                                                 855
            ELSEIF (EXSE_SECTYPE(ISEC).EQ.2) THEN                         855
C       Create EXSG :                                                     857
C       Call To Local Procedure EXSG                                      857
            ASSIGN 5900 TO BLOCK_EXSG                                     857
            GO TO  5880                                                   857
5900        CONTINUE                                                      857
C       *                                                                 857
C       Geant POSITION : EXSG ===> ALPHAZ=D3 ORT=X-Y-Z NCOPY=ISEC KONLY=  858
C       'MANY' <===                                                       858
            CALL AGSRESET                                                 858
            AG_ALPHAN=AG_ALPHAN+1                                         858
            AG_ALPHAK(AG_ALPHAN)=3                                        858
            AG_ALPHAL(AG_ALPHAN)=D3                                       858
            AG_ALPHAN=AG_ALPHAN+1                                         858
            AG_ALPHAK(AG_ALPHAN)=IGOR('X-Y-Z')                            858
            AG_NCOPY = ISEC                                               858
            AG_KONLY = 'MANY'                                             858
            AG_TITLE = 'EXSG'                                             858
            AG_EXNAME = 'POSITION'                                        858
            AG_PARLIST = 'ALPHAZ_ORT_NCOPY_KONLY'                         858
            CALL AXPOSITION                                               858
C       *                                                                 858
            ELSEIF (EXSE_SECTYPE(ISEC).EQ.3) THEN                         858
            CUT=2                                                         860
C       Create EXSG :                                                     861
C       Call To Local Procedure EXSG                                      861
            ASSIGN 5910 TO BLOCK_EXSG                                     861
            GO TO  5880                                                   861
5910        CONTINUE                                                      861
C       *                                                                 861
C       Geant POSITION : EXSG ===> ALPHAZ=D3 NCOPY=ISEC KONLY='MANY' <==  862
C       =                                                                 862
            CALL AGSRESET                                                 862
            AG_ALPHAN=AG_ALPHAN+1                                         862
            AG_ALPHAK(AG_ALPHAN)=3                                        862
            AG_ALPHAL(AG_ALPHAN)=D3                                       862
            AG_NCOPY = ISEC                                               862
            AG_KONLY = 'MANY'                                             862
            AG_TITLE = 'EXSG'                                             862
            AG_EXNAME = 'POSITION'                                        862
            AG_PARLIST = 'ALPHAZ_NCOPY_KONLY'                             862
            CALL AXPOSITION                                               862
C       *                                                                 862
            ELSEIF (EXSE_SECTYPE(ISEC).EQ.4) THEN                         862
            CUT=2                                                         864
C       Create EXSG :                                                     865
C       Call To Local Procedure EXSG                                      865
            ASSIGN 5920 TO BLOCK_EXSG                                     865
            GO TO  5880                                                   865
5920        CONTINUE                                                      865
C       *                                                                 865
C       Geant POSITION : EXSG ===> ALPHAZ=D3 ORT=X-Y-Z NCOPY=ISEC KONLY=  866
C       'MANY' <===                                                       866
            CALL AGSRESET                                                 866
            AG_ALPHAN=AG_ALPHAN+1                                         866
            AG_ALPHAK(AG_ALPHAN)=3                                        866
            AG_ALPHAL(AG_ALPHAN)=D3                                       866
            AG_ALPHAN=AG_ALPHAN+1                                         866
            AG_ALPHAK(AG_ALPHAN)=IGOR('X-Y-Z')                            866
            AG_NCOPY = ISEC                                               866
            AG_KONLY = 'MANY'                                             866
            AG_TITLE = 'EXSG'                                             866
            AG_EXNAME = 'POSITION'                                        866
            AG_PARLIST = 'ALPHAZ_ORT_NCOPY_KONLY'                         866
            CALL AXPOSITION                                               866
C       *                                                                 866
            ENDIF                                                         866
         GO TO 5871                                                       868
5872     CONTINUE                                                         868
C    *                                                                    869
         ISEC=1                                                           869
            GO TO 5933                                                    869
5931        ISEC=ISEC+1                                                   869
5933        IF(ISEC-(6).GT.0) GO TO 5932                                  869
            D3=75 - (ISEC-1)*30                                           870
C       Check EXSE_SECTYPE(ISEC)==0|(EMCG_FILLMODE==1&(ISEC==6|ISEC==1))  871
            IF                                                            871
     *      (EXSE_SECTYPE(ISEC).EQ.0.OR.(EMCG_FILLMODE.EQ.1.AND.(ISEC.    871
     *      EQ.6.OR.ISEC.EQ.1))) THEN                                     871
            CUT = 0                                                       872
C       Create EXSG :                                                     873
C       Call To Local Procedure EXSG                                      873
            ASSIGN 5940 TO BLOCK_EXSG                                     873
            GO TO  5880                                                   873
5940        CONTINUE                                                      873
C       *                                                                 873
C       Geant POSITION : EXSG ===> ALPHAZ=D3 NCOPY=ISEC KONLY='MANY' <==  874
C       =                                                                 874
            CALL AGSRESET                                                 874
            AG_ALPHAN=AG_ALPHAN+1                                         874
            AG_ALPHAK(AG_ALPHAN)=3                                        874
            AG_ALPHAL(AG_ALPHAN)=D3                                       874
            AG_NCOPY = ISEC                                               874
            AG_KONLY = 'MANY'                                             874
            AG_TITLE = 'EXSG'                                             874
            AG_EXNAME = 'POSITION'                                        874
            AG_PARLIST = 'ALPHAZ_NCOPY_KONLY'                             874
            CALL AXPOSITION                                               874
C       *                                                                 874
            ENDIF                                                         874
         GO TO 5931                                                       876
5932     CONTINUE                                                         876
5501  CALL AGSPOPB                                                        877
      GO TO BLOCK_ESPL, (5500 ,5510)                                      877
C End of Local Procedure                                                  877
C ----------------------------------------------------------------------  878
C ------------ EXSG                                                       878
C Begin Local Procedure EXSG                                              879
C EXSG IS ANOTHER LOGICAL VOLUME... THIS ONE ACUTALLY CREATES THE PLANES  879
5880  CONTINUE                                                            879
         AG_TITLE='EXSG'                                                  879
         AG_MODULE=MODULE                                                 879
         AG_MARK='V00'                                                    879
         CALL AGSPUSH                                                     879
         AG_MODULE=MODULE                                                 879
C    *                                                                    879
C    Geant ATTRIBUTE : EXSG ===> SEEN=1 COLO=7 SERIAL=CUT LSTY=3 <===     880
         CALL AGSRESET                                                    880
         AG_SEEN = 1                                                      880
         AG_COLO = 7                                                      880
         AG_SERIAL = CUT                                                  880
         AG_LSTY = 3                                                      880
         AG_TITLE = 'EXSG'                                                880
         AG_EXNAME = 'ATTRIBUTE'                                          880
         AG_PARLIST = 'SEEN_COLO_SERIAL_LSTY'                             880
         CALL AXATTRIBUTE                                                 880
C    *                                                                    880
C    Geant MATERIAL : ECAL_AIR ===> inherited arguments used <===         880
         CALL AGSRESET                                                    881
         AG_TITLE = 'ECAL_AIR'                                            881
         AG_EXNAME = 'MATERIAL'                                           881
         AG_PARLIST = 'NONE'                                              881
         CALL AXMATERIAL                                                  881
C    *                                                                    881
C    Geant SHAPE : TUBS ===> RMIN=SECTION*TAN_LOW-1.526 RMAX=(SECTION-SE  881
C    CWID/2)*TAN_UPP+DUP, PHI1=EMCS_PHIMIN/EMCS_NSUPSEC-5, PHI2=EMCS_PHI  881
C    MAX/EMCS_NSUPSEC+5, DZ=EMCS_GAPSMD/3/2 <===                          881
         CALL AGSRESET                                                    885
         AG_RMIN = SECTION*TAN_LOW-1.526                                  885
         AG_RMAX = (SECTION-SECWID/2)*TAN_UPP+DUP                         885
         AG_PHI1 = EMCS_PHIMIN/EMCS_NSUPSEC-5                             885
         AG_PHI2 = EMCS_PHIMAX/EMCS_NSUPSEC+5                             885
         AG_DZ = EMCS_GAPSMD/3/2                                          885
         AG_TITLE = 'TUBS'                                                885
         AG_EXNAME = 'SHAPE'                                              885
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            885
         CALL AXSHAPE                                                     885
C    *                                                                    885
         RBOT = EMXG_RIN                                                  885
         RTOP = EMXG_ROUT                                                 887
C    Check CUT.EQ.0                                                       888
         IF (CUT.EQ.0) THEN                                               888
C    Create EXPS :                                                        889
C    Call To Local Procedure EXPS                                         889
         ASSIGN 5960 TO BLOCK_EXPS                                        889
         GO TO  5950                                                      889
5960     CONTINUE                                                         889
C    *                                                                    889
C    Geant POSITION : EXPS ===> KONLY='MANY' <===                         890
         CALL AGSRESET                                                    890
         AG_KONLY = 'MANY'                                                890
         AG_TITLE = 'EXPS'                                                890
         AG_EXNAME = 'POSITION'                                           890
         AG_PARLIST = 'KONLY'                                             890
         CALL AXPOSITION                                                  890
C    *                                                                    890
         ENDIF                                                            890
C    Check CUT>0                                                          892
         IF (CUT.GT.0) THEN                                               892
C    Check CUT==1                                                         893
         IF (CUT.EQ.1) THEN                                               893
         NSTR = 288                                                       894
         ELSE                                                             895
         NSTR = 285                                                       896
         ENDIF                                                            897
C    *                                                                    898
         ISTRIP=1                                                         898
            GO TO 5973                                                    898
5971        ISTRIP=ISTRIP+1                                               898
5973        IF(ISTRIP-(NSTR).GT.0) GO TO 5972                             898
            CALL ECAL_GET_STRIP( SECTION,CUT,ISTRIP,XC,YC,LENGTH )        899
C       Check MOD(ISTRIP,2)!=0                                            900
            IF (MOD(ISTRIP,2).NE.0) THEN                                  900
C       Create EHMS :                                                     901
C       Call To Local Procedure EHMS                                      901
            ASSIGN 5990 TO BLOCK_EHMS                                     901
            GO TO  5980                                                   901
5990        CONTINUE                                                      901
C       *                                                                 901
C       Geant POSITION : EHMS ===> X=XC Y=YC ALPHAZ=-45 <===              902
            CALL AGSRESET                                                 902
            AG_X = XC                                                     902
            AG_Y = YC                                                     902
            AG_ALPHAN=AG_ALPHAN+1                                         902
            AG_ALPHAK(AG_ALPHAN)=3                                        902
            AG_ALPHAL(AG_ALPHAN)=-45                                      902
            AG_TITLE = 'EHMS'                                             902
            AG_EXNAME = 'POSITION'                                        902
            AG_PARLIST = 'X_Y_ALPHAZ'                                     902
            CALL AXPOSITION                                               902
C       *                                                                 902
C       Create EBLS :                                                     903
C       Call To Local Procedure EBLS                                      903
            ASSIGN 6010 TO BLOCK_EBLS                                     903
            GO TO  6000                                                   903
6010        CONTINUE                                                      903
C       *                                                                 903
C       Geant POSITION : EBLS ===> X=XC Y=YC Z=(+ESMD_APEX/2+ESMD_BACK_L  904
C       AYER/2) ALPHAZ=-45 <===                                           904
            CALL AGSRESET                                                 904
            AG_X = XC                                                     904
            AG_Y = YC                                                     904
            AG_Z = (+ESMD_APEX/2+ESMD_BACK_LAYER/2)                       904
            AG_ALPHAN=AG_ALPHAN+1                                         904
            AG_ALPHAK(AG_ALPHAN)=3                                        904
            AG_ALPHAL(AG_ALPHAN)=-45                                      904
            AG_TITLE = 'EBLS'                                             904
            AG_EXNAME = 'POSITION'                                        904
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   904
            CALL AXPOSITION                                               904
C       *                                                                 904
            ELSE                                                          904
C       Create EHMS :                                                     906
C       Call To Local Procedure EHMS                                      906
            ASSIGN 6020 TO BLOCK_EHMS                                     906
            GO TO  5980                                                   906
6020        CONTINUE                                                      906
C       *                                                                 906
C       Geant POSITION : EHMS ===> X=XC Y=YC ALPHAZ=-45 ORT=X-Y-Z <===    907
            CALL AGSRESET                                                 907
            AG_X = XC                                                     907
            AG_Y = YC                                                     907
            AG_ALPHAN=AG_ALPHAN+1                                         907
            AG_ALPHAK(AG_ALPHAN)=3                                        907
            AG_ALPHAL(AG_ALPHAN)=-45                                      907
            AG_ALPHAN=AG_ALPHAN+1                                         907
            AG_ALPHAK(AG_ALPHAN)=IGOR('X-Y-Z')                            907
            AG_TITLE = 'EHMS'                                             907
            AG_EXNAME = 'POSITION'                                        907
            AG_PARLIST = 'X_Y_ALPHAZ_ORT'                                 907
            CALL AXPOSITION                                               907
C       *                                                                 907
C       Create EFLS :                                                     908
C       Call To Local Procedure EFLS                                      908
            ASSIGN 6040 TO BLOCK_EFLS                                     908
            GO TO  6030                                                   908
6040        CONTINUE                                                      908
C       *                                                                 908
C       Geant POSITION : EFLS ===> X=XC Y=YC Z=(-ESMD_APEX/2-ESMD_FRONT_  910
C       LAYER/2) ALPHAZ=-45 ORT=X-Y-Z <===                                910
            CALL AGSRESET                                                 910
            AG_X = XC                                                     910
            AG_Y = YC                                                     910
            AG_Z = (-ESMD_APEX/2-ESMD_FRONT_LAYER/2)                      910
            AG_ALPHAN=AG_ALPHAN+1                                         910
            AG_ALPHAK(AG_ALPHAN)=3                                        910
            AG_ALPHAL(AG_ALPHAN)=-45                                      910
            AG_ALPHAN=AG_ALPHAN+1                                         910
            AG_ALPHAK(AG_ALPHAN)=IGOR('X-Y-Z')                            910
            AG_TITLE = 'EFLS'                                             910
            AG_EXNAME = 'POSITION'                                        910
            AG_PARLIST = 'X_Y_Z_ALPHAZ_ORT'                               910
            CALL AXPOSITION                                               910
C       *                                                                 910
            ENDIF                                                         910
         GO TO 5971                                                       912
5972     CONTINUE                                                         912
         ENDIF                                                            913
5881  CALL AGSPOPB                                                        914
      GO TO BLOCK_EXSG, (5880 ,5890,5900,5910,5920,5940)                  914
C End of Local Procedure                                                  914
C ----------------------------------------------------------------------  915
C ------------ EHMS                                                       915
C Begin Local Procedure EHMS                                              916
C EHMS DEFINES THE TRIANGULAR SMD STRIPS                                  916
5980  CONTINUE                                                            916
         AG_TITLE='EHMS'                                                  916
         AG_MODULE=MODULE                                                 916
         AG_MARK='V00'                                                    916
         CALL AGSPUSH                                                     916
         AG_MODULE=MODULE                                                 916
C    *                                                                    916
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       917
         CALL AGSRESET                                                    917
         AG_TITLE = 'POLYSTYREN'                                          917
         AG_EXNAME = 'MATERIAL'                                           917
         AG_PARLIST = 'NONE'                                              917
         CALL AXMATERIAL                                                  917
C    *                                                                    917
C    Geant MATERIAL : ECAL_SMDSTRIP ===> ISVOL=1 <===                     917
         CALL AGSRESET                                                    918
         AG_ISVOL = 1                                                     918
         AG_TITLE = 'ECAL_SMDSTRIP'                                       918
         AG_EXNAME = 'MATERIAL'                                           918
         AG_PARLIST = 'ISVOL'                                             918
         CALL AXMATERIAL                                                  918
C    *                                                                    918
C    Geant ATTRIBUTE : EHMS ===> SEEN=1 COLO=2 SERIAL=CUT LSTY=1 <===     918
         CALL AGSRESET                                                    919
         AG_SEEN = 1                                                      919
         AG_COLO = 2                                                      919
         AG_SERIAL = CUT                                                  919
         AG_LSTY = 1                                                      919
         AG_TITLE = 'EHMS'                                                919
         AG_EXNAME = 'ATTRIBUTE'                                          919
         AG_PARLIST = 'SEEN_COLO_SERIAL_LSTY'                             919
         CALL AXATTRIBUTE                                                 919
C    *                                                                    919
C    Geant SHAPE : TRD1 ===> DX1=0 DX2=EMXG_SBASE/2 DY=LENGTH/2, DZ=EMXG  919
C    _SAPEX/2 <===                                                        919
         CALL AGSRESET                                                    921
         AG_DX1 = 0                                                       921
         AG_DX2 = EMXG_SBASE/2                                            921
         AG_DY = LENGTH/2                                                 921
         AG_DZ = EMXG_SAPEX/2                                             921
         AG_TITLE = 'TRD1'                                                921
         AG_EXNAME = 'SHAPE'                                              921
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     921
         CALL AXSHAPE                                                     921
C    *                                                                    921
         CALL GSTPAR(AG_IMED, 'CUTGAM',0.00008)                           921
         CALL GSTPAR(AG_IMED, 'CUTELE',0.001)                             923
         CALL GSTPAR(AG_IMED, 'BCUTE',0.0001)                             924
         CALL GSTPAR(AG_IMED, 'BIRK1', 1.)                                925
         CALL GSTPAR(AG_IMED, 'BIRK2', 0.0130)                            926
         CALL GSTPAR(AG_IMED, 'BIRK3', 9.6E-6)                            927
C    Check ESEN_VERSION .GT. 0                                            928
         IF (ESEN_VERSION .GT. 0) THEN                                    928
         CALL GSTPAR( AG_IMED, 'CUTGAM', ESEN_CUTGAM )                    928
         CALL GSTPAR( AG_IMED, 'CUTELE', ESEN_CUTELE )                    928
         CALL GSTPAR( AG_IMED, 'CUTHAD', ESEN_CUTHAD )                    928
         CALL GSTPAR( AG_IMED, 'CUTNEU', ESEN_CUTNEU )                    928
         CALL GSTPAR( AG_IMED, 'CUTMUO', ESEN_CUTMUO )                    928
         CALL GSTPAR( AG_IMED, 'DCUTE', ESEN_DCUTE )                      928
         CALL GSTPAR( AG_IMED, 'DCUTM', ESEN_DCUTM )                      928
         CALL GSTPAR( AG_IMED, 'BCUTE', ESEN_BCUTE )                      928
         CALL GSTPAR( AG_IMED, 'BCUTM', ESEN_BCUTM )                      928
         ENDIF                                                            928
C    Defining Hits: BIRK:0:(0,10)                                         929
         AG_TITLE = 'EHMS'                                                929
         AG_EXNAME = 'HITS'                                               929
         AG_TYPE=AG_MAGIC                                                 929
         CALL AGSDETV('EHMS')                                             929
C    *                                                                    929
         AG_PARLIST='BIRK'                                                929
         AG_TITLE = 'DETH'                                                929
         AG_NBITS = 0                                                     929
         AG_BIN=AG_MAGIC                                                  929
         AG_OPTION='*'                                                    929
         AG_FHMIN = 0                                                     929
         AG_FHMAX = 10                                                    929
         CALL AGSDETH (0)                                                 929
C    *                                                                    929
         CALL AGSDETU('EHMS')                                             929
C    *                                                                    929
5981  CALL AGSPOPB                                                        930
      GO TO BLOCK_EHMS, (5980 ,5990,6020)                                 930
C End of Local Procedure                                                  930
C ----------------------------------------------------------------------  931
C ------------ EFLS                                                       931
C Begin Local Procedure EFLS                                              932
C EFLS IS THE LAYER OF MATERIAL ON THE FRONT OF THE SMD PLANES            932
6030  CONTINUE                                                            932
         AG_TITLE='EFLS'                                                  932
         AG_MODULE=MODULE                                                 932
         AG_MARK='V00'                                                    932
         CALL AGSPUSH                                                     932
         AG_MODULE=MODULE                                                 932
C    *                                                                    932
C    Geant COMPONENT : G10 ===> A=18.017 Z=9.013 W=1.19*1.700/(1.19*1.70  933
C    0+0.25*1.530+0.17*1.390) <===                                        933
         CALL AGSRESET                                                    933
         AG_A = 18.017                                                    933
         AG_Z = 9.013                                                     933
         AG_W = 1.19*1.700/(1.19*1.700+0.25*1.530+0.17*1.390)             933
         AG_TITLE = 'G10'                                                 933
         AG_EXNAME = 'COMPONENT'                                          933
         AG_PARLIST = 'A_Z_W'                                             933
         CALL AXCOMPONENT                                                 933
C    *                                                                    933
C    Geant COMPONENT : FIBERGLASS ===> A=19.103 Z=9.549 W=0.25*1.530/(1.  933
C    19*1.700+0.25*1.530+0.17*1.390) <===                                 933
         CALL AGSRESET                                                    934
         AG_A = 19.103                                                    934
         AG_Z = 9.549                                                     934
         AG_W = 0.25*1.530/(1.19*1.700+0.25*1.530+0.17*1.390)             934
         AG_TITLE = 'FIBERGLASS'                                          934
         AG_EXNAME = 'COMPONENT'                                          934
         AG_PARLIST = 'A_Z_W'                                             934
         CALL AXCOMPONENT                                                 934
C    *                                                                    934
C    Geant COMPONENT : ALMYLAR ===> A=12.889 Z=6.465 W=0.17*1.390/(1.19*  934
C    1.700+0.25*1.530+0.17*1.390) <===                                    934
         CALL AGSRESET                                                    935
         AG_A = 12.889                                                    935
         AG_Z = 6.465                                                     935
         AG_W = 0.17*1.390/(1.19*1.700+0.25*1.530+0.17*1.390)             935
         AG_TITLE = 'ALMYLAR'                                             935
         AG_EXNAME = 'COMPONENT'                                          935
         AG_PARLIST = 'A_Z_W'                                             935
         CALL AXCOMPONENT                                                 935
C    *                                                                    935
C    Geant MIXTURE : EFLS ===> DENS=(1.19*1.7+0.25*1.53+0.17*1.39)/(1.19  935
C    +0.25+0.17) <===                                                     935
         CALL AGSRESET                                                    936
         AG_DENS = (1.19*1.7+0.25*1.53+0.17*1.39)/(1.19+0.25+0.17)        936
         AG_TITLE = 'EFLS'                                                936
         AG_EXNAME = 'MIXTURE'                                            936
         AG_PARLIST = 'DENS'                                              936
         CALL AXMIXTURE                                                   936
C    *                                                                    936
C    Geant ATTRIBUTE : EFLS ===> SEEN=0 COLO=22 LSTY=1 <===               936
         CALL AGSRESET                                                    937
         AG_SEEN = 0                                                      937
         AG_COLO = 22                                                     937
         AG_LSTY = 1                                                      937
         AG_TITLE = 'EFLS'                                                937
         AG_EXNAME = 'ATTRIBUTE'                                          937
         AG_PARLIST = 'SEEN_COLO_LSTY'                                    937
         CALL AXATTRIBUTE                                                 937
C    *                                                                    937
C    Geant SHAPE : TRD1 ===> DX1=ESMD_BASE/2 DX2=ESMD_BASE/2, DY=LENGTH/  937
C    2 DZ=ESMD_FRONT_LAYER/2 <===                                         937
         CALL AGSRESET                                                    939
         AG_DX1 = ESMD_BASE/2                                             939
         AG_DX2 = ESMD_BASE/2                                             939
         AG_DY = LENGTH/2                                                 939
         AG_DZ = ESMD_FRONT_LAYER/2                                       939
         AG_TITLE = 'TRD1'                                                939
         AG_EXNAME = 'SHAPE'                                              939
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     939
         CALL AXSHAPE                                                     939
C    *                                                                    939
C    Check EABS_VERSION .GT. 0                                            940
         IF (EABS_VERSION .GT. 0) THEN                                    940
         CALL GSTPAR( AG_IMED, 'CUTGAM', EABS_CUTGAM )                    940
         CALL GSTPAR( AG_IMED, 'CUTELE', EABS_CUTELE )                    940
         CALL GSTPAR( AG_IMED, 'CUTHAD', EABS_CUTHAD )                    940
         CALL GSTPAR( AG_IMED, 'CUTNEU', EABS_CUTNEU )                    940
         CALL GSTPAR( AG_IMED, 'CUTMUO', EABS_CUTMUO )                    940
         CALL GSTPAR( AG_IMED, 'DCUTE', EABS_DCUTE )                      940
         CALL GSTPAR( AG_IMED, 'DCUTM', EABS_DCUTM )                      940
         CALL GSTPAR( AG_IMED, 'BCUTE', EABS_BCUTE )                      940
         CALL GSTPAR( AG_IMED, 'BCUTM', EABS_BCUTM )                      940
         ENDIF                                                            940
6031  CALL AGSPOPB                                                        941
      GO TO BLOCK_EFLS, (6030 ,6040)                                      941
C End of Local Procedure                                                  941
C ----------------------------------------------------------------------  942
C ------------ EBLS                                                       942
C Begin Local Procedure EBLS                                              943
C EBLS IS THE LAYER OF MATERIAL ON THE BACK OF THE SMD PLANES             943
6000  CONTINUE                                                            943
         AG_TITLE='EBLS'                                                  943
         AG_MODULE=MODULE                                                 943
         AG_MARK='V00'                                                    943
         CALL AGSPUSH                                                     943
         AG_MODULE=MODULE                                                 943
C    *                                                                    943
C    Geant COMPONENT : ALMYLAR ===> A=12.889 Z=6.465 W=0.10*1.390/(0.10*  944
C    1.390+0.25*1.530+1.50*1.032+0.25*2.699) <===                         944
         CALL AGSRESET                                                    944
         AG_A = 12.889                                                    944
         AG_Z = 6.465                                                     944
         AG_W = 0.10*1.390/                                               944
     *   (0.10*1.390+0.25*1.530+1.50*1.032+0.25*2.699)                    944
         AG_TITLE = 'ALMYLAR'                                             944
         AG_EXNAME = 'COMPONENT'                                          944
         AG_PARLIST = 'A_Z_W'                                             944
         CALL AXCOMPONENT                                                 944
C    *                                                                    944
C    Geant COMPONENT : FIBERGLASS ===> A=19.103 Z=9.549 W=0.25*1.530/(0.  944
C    10*1.390+0.25*1.530+1.50*1.032+0.25*2.699) <===                      944
         CALL AGSRESET                                                    945
         AG_A = 19.103                                                    945
         AG_Z = 9.549                                                     945
         AG_W = 0.25*1.530/                                               945
     *   (0.10*1.390+0.25*1.530+1.50*1.032+0.25*2.699)                    945
         AG_TITLE = 'FIBERGLASS'                                          945
         AG_EXNAME = 'COMPONENT'                                          945
         AG_PARLIST = 'A_Z_W'                                             945
         CALL AXCOMPONENT                                                 945
C    *                                                                    945
C    Geant COMPONENT : POLYSTYREN ===> A=11.154 Z=5.615 W=1.50*1.032/(0.  945
C    10*1.390+0.25*1.530+1.50*1.032+0.25*2.699) <===                      945
         CALL AGSRESET                                                    946
         AG_A = 11.154                                                    946
         AG_Z = 5.615                                                     946
         AG_W = 1.50*1.032/                                               946
     *   (0.10*1.390+0.25*1.530+1.50*1.032+0.25*2.699)                    946
         AG_TITLE = 'POLYSTYREN'                                          946
         AG_EXNAME = 'COMPONENT'                                          946
         AG_PARLIST = 'A_Z_W'                                             946
         CALL AXCOMPONENT                                                 946
C    *                                                                    946
C    Geant COMPONENT : AL ===> A=28.08 Z=14.00 W=0.25*2.699/(0.10*1.390+  946
C    0.25*1.530+1.50*1.032+0.25*2.699) <===                               946
         CALL AGSRESET                                                    947
         AG_A = 28.08                                                     947
         AG_Z = 14.00                                                     947
         AG_W = 0.25*2.699/                                               947
     *   (0.10*1.390+0.25*1.530+1.50*1.032+0.25*2.699)                    947
         AG_TITLE = 'AL'                                                  947
         AG_EXNAME = 'COMPONENT'                                          947
         AG_PARLIST = 'A_Z_W'                                             947
         CALL AXCOMPONENT                                                 947
C    *                                                                    947
C    Geant MIXTURE : EBLS ===> DENS=(0.10*1.390+0.25*1.530+1.50*1.032+0.  947
C    25*2.699)/(0.10+0.25+1.50+0.25) <===                                 947
         CALL AGSRESET                                                    948
         AG_DENS = (0.10*1.390+0.25*1.530+1.50*1.032+0.25*2.699)/         948
     *   (0.10+0.25+1.50+0.25)                                            948
         AG_TITLE = 'EBLS'                                                948
         AG_EXNAME = 'MIXTURE'                                            948
         AG_PARLIST = 'DENS'                                              948
         CALL AXMIXTURE                                                   948
C    *                                                                    948
C    Geant ATTRIBUTE : EFLS ===> SEEN=0 COLO=22 LSTY=1 <===               948
         CALL AGSRESET                                                    949
         AG_SEEN = 0                                                      949
         AG_COLO = 22                                                     949
         AG_LSTY = 1                                                      949
         AG_TITLE = 'EFLS'                                                949
         AG_EXNAME = 'ATTRIBUTE'                                          949
         AG_PARLIST = 'SEEN_COLO_LSTY'                                    949
         CALL AXATTRIBUTE                                                 949
C    *                                                                    949
C    Geant SHAPE : TRD1 ===> DX1=ESMD_BASE/2 DX2=ESMD_BASE/2, DY=LENGTH/  949
C    2 DZ=ESMD_BACK_LAYER/2 <===                                          949
         CALL AGSRESET                                                    951
         AG_DX1 = ESMD_BASE/2                                             951
         AG_DX2 = ESMD_BASE/2                                             951
         AG_DY = LENGTH/2                                                 951
         AG_DZ = ESMD_BACK_LAYER/2                                        951
         AG_TITLE = 'TRD1'                                                951
         AG_EXNAME = 'SHAPE'                                              951
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     951
         CALL AXSHAPE                                                     951
C    *                                                                    951
C    Check EABS_VERSION .GT. 0                                            952
         IF (EABS_VERSION .GT. 0) THEN                                    952
         CALL GSTPAR( AG_IMED, 'CUTGAM', EABS_CUTGAM )                    952
         CALL GSTPAR( AG_IMED, 'CUTELE', EABS_CUTELE )                    952
         CALL GSTPAR( AG_IMED, 'CUTHAD', EABS_CUTHAD )                    952
         CALL GSTPAR( AG_IMED, 'CUTNEU', EABS_CUTNEU )                    952
         CALL GSTPAR( AG_IMED, 'CUTMUO', EABS_CUTMUO )                    952
         CALL GSTPAR( AG_IMED, 'DCUTE', EABS_DCUTE )                      952
         CALL GSTPAR( AG_IMED, 'DCUTM', EABS_DCUTM )                      952
         CALL GSTPAR( AG_IMED, 'BCUTE', EABS_BCUTE )                      952
         CALL GSTPAR( AG_IMED, 'BCUTM', EABS_BCUTM )                      952
         ENDIF                                                            952
6001  CALL AGSPOPB                                                        953
      GO TO BLOCK_EBLS, (6000 ,6010)                                      953
C End of Local Procedure                                                  953
C ----------------------------------------------------------------------  954
C ------------ EXPS                                                       954
C Begin Local Procedure EXPS                                              955
C EXPS IS THE PLASTIC SPACER IN THE SHOWER MAXIMUM SECTION                955
5950  CONTINUE                                                            955
         AG_TITLE='EXPS'                                                  955
         AG_MODULE=MODULE                                                 955
         AG_MARK='V00'                                                    955
         CALL AGSPUSH                                                     955
         AG_MODULE=MODULE                                                 955
C    *                                                                    955
C    Geant COMPONENT : H ===> A=1 Z=1 W=3.0*1.0/62.453 <===               956
         CALL AGSRESET                                                    956
         AG_A = 1                                                         956
         AG_Z = 1                                                         956
         AG_W = 3.0*1.0/62.453                                            956
         AG_TITLE = 'H'                                                   956
         AG_EXNAME = 'COMPONENT'                                          956
         AG_PARLIST = 'A_Z_W'                                             956
         CALL AXCOMPONENT                                                 956
C    *                                                                    956
C    Geant COMPONENT : C ===> A=12 Z=6 W=2.0*12.0/62.453 <===             956
         CALL AGSRESET                                                    957
         AG_A = 12                                                        957
         AG_Z = 6                                                         957
         AG_W = 2.0*12.0/62.453                                           957
         AG_TITLE = 'C'                                                   957
         AG_EXNAME = 'COMPONENT'                                          957
         AG_PARLIST = 'A_Z_W'                                             957
         CALL AXCOMPONENT                                                 957
C    *                                                                    957
C    Geant COMPONENT : CL ===> A=35.453 Z=17 W=1.0*35.453/62.453 <===     957
         CALL AGSRESET                                                    958
         AG_A = 35.453                                                    958
         AG_Z = 17                                                        958
         AG_W = 1.0*35.453/62.453                                         958
         AG_TITLE = 'CL'                                                  958
         AG_EXNAME = 'COMPONENT'                                          958
         AG_PARLIST = 'A_Z_W'                                             958
         CALL AXCOMPONENT                                                 958
C    *                                                                    958
C    Geant MIXTURE : PVC_SPACER ===> DENS=1.390*(1.20/1.00) <===          958
         CALL AGSRESET                                                    959
         AG_DENS = 1.390*(1.20/1.00)                                      959
         AG_TITLE = 'PVC_SPACER'                                          959
         AG_EXNAME = 'MIXTURE'                                            959
         AG_PARLIST = 'DENS'                                              959
         CALL AXMIXTURE                                                   959
C    *                                                                    959
C    Geant ATTRIBUTE : EXPS ===> SEEN=1 COLO=6 LSTY=1 LWID=2 <===         959
         CALL AGSRESET                                                    960
         AG_SEEN = 1                                                      960
         AG_COLO = 6                                                      960
         AG_LSTY = 1                                                      960
         AG_LWID = 2                                                      960
         AG_TITLE = 'EXPS'                                                960
         AG_EXNAME = 'ATTRIBUTE'                                          960
         AG_PARLIST = 'SEEN_COLO_LSTY_LWID'                               960
         CALL AXATTRIBUTE                                                 960
C    *                                                                    960
C    Geant SHAPE : TUBS ===> RMIN=(SECTION)*TAN_LOW-1.526, RMAX=(SECTION  960
C    +MSECWD)*TAN_UPP PHI1=EMCS_PHIMIN/EMCS_NSUPSEC, PHI2=EMCS_PHIMAX/EM  960
C    CS_NSUPSEC, DZ=1.0/2 <===                                            960
         CALL AGSRESET                                                    964
         AG_RMIN = (SECTION)*TAN_LOW-1.526                                964
         AG_RMAX = (SECTION+MSECWD)*TAN_UPP                               964
         AG_PHI1 = EMCS_PHIMIN/EMCS_NSUPSEC                               964
         AG_PHI2 = EMCS_PHIMAX/EMCS_NSUPSEC                               964
         AG_DZ = 1.0/2                                                    964
         AG_TITLE = 'TUBS'                                                964
         AG_EXNAME = 'SHAPE'                                              964
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            964
         CALL AXSHAPE                                                     964
C    *                                                                    964
C    Check EABS_VERSION .GT. 0                                            965
         IF (EABS_VERSION .GT. 0) THEN                                    965
         CALL GSTPAR( AG_IMED, 'CUTGAM', EABS_CUTGAM )                    965
         CALL GSTPAR( AG_IMED, 'CUTELE', EABS_CUTELE )                    965
         CALL GSTPAR( AG_IMED, 'CUTHAD', EABS_CUTHAD )                    965
         CALL GSTPAR( AG_IMED, 'CUTNEU', EABS_CUTNEU )                    965
         CALL GSTPAR( AG_IMED, 'CUTMUO', EABS_CUTMUO )                    965
         CALL GSTPAR( AG_IMED, 'DCUTE', EABS_DCUTE )                      965
         CALL GSTPAR( AG_IMED, 'DCUTM', EABS_DCUTM )                      965
         CALL GSTPAR( AG_IMED, 'BCUTE', EABS_BCUTE )                      965
         CALL GSTPAR( AG_IMED, 'BCUTM', EABS_BCUTM )                      965
         ENDIF                                                            965
5951  CALL AGSPOPB                                                        966
      GO TO BLOCK_EXPS, (5950 ,5960)                                      966
C End of Local Procedure                                                  966
      WRITE(LOUT,6050)'===>>  [End AgML Module ECALGEO6]  <<==='          966
6050  FORMAT(A32,/,/)                                                     966
C MODULE ECALGEO6                                                         968
1     CONTINUE                                                            968
      END                                                                 968
C ----------------------------------------------------------------------  969
C ------------ ECAL_GET_STRIP                                             969
      SUBROUTINE ECAL_GET_STRIP(SECTION, CUT, ISTRIP, XCENTER, YCENTER,   970
     *LENGTH)                                                             970
C INTENT=INOUT                                                            971
      REAL SECTION                                                        971
C INTENT=INOUT                                                            972
      INTEGER CUT                                                         972
C INTENT=INOUT                                                            973
      INTEGER ISTRIP                                                      973
C INTENT=INOUT                                                            974
      REAL XCENTER                                                        974
C INTENT=INOUT                                                            975
      REAL YCENTER                                                        975
C INTENT=INOUT                                                            976
      REAL LENGTH                                                         976
C - GEANT description of logical units                                    977
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            977
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      977
      COMMON/GCMAIL/ CHMAIL                                               977
      CHARACTER*132 CHMAIL                                                977
C - Advanced Geant interface                                              977
      CHARACTER*20 AG_MODULE,AG_TITLE,AG_EXNAME,AG_PARLIST,AG_CHDIR,      977
     *AG_MATERIAL,AG_MIXTURE,AG_COMPONENT,AG_MEDIUM                       977
      CHARACTER*4 AG_VOLUME,AG_MOTHER,AG_SHAPE,AG_CNICK,AG_KONLY,         977
     *AG_OPTION,AG_ORT,AG_MARK                                            977
      INTEGER AG_BEGCOM,AG_IVOLUME,AG_IMOTHER,AG_IGNUM,AG_ISHAPE,         977
     *AG_IMED,AG_IMAT,AG_IFIELD, AG_NLMAT,AG_IERROR,AG_NWBUF,AG_NPAR,     977
     *AG_ISTATUS,AG_IROT,AG_NBITS,AG_SERIAL, AG_ATTRIBUTE(6),AG_WORK,     977
     *AG_SEEN,AG_LSTY,AG_LWID,AG_COLO,AG_FILL,AG_JDU,AG_ORTI, AG_NDIV,    977
     *AG_IAXIS,AG_NDVMAX,AG_IPRIN,AG_RESET1,AG_RESET2,AG_BEGSCR,          977
     *AG_ENDSCR, AG_IDTYPE,AG_ISET,AG_IDET,AG_ISVOL,AG_LEVEL,AG_IRESER,   977
     *AG_IGEOM,AG_STANDALONE, AG_LSTACK,AG_NWUHIT,AG_NWUVOL,AG_MAGIC,     977
     *AG_LDETU,AG_NPDIV,AG_NPDV,AG_NZ,AG_NCOPY, AG_IDEBU,AG_IGRAP,        977
     *AG_IHIST,AG_IMFLD,AG_ISIMU,AG_ENDSAVE,AG_CODE,AG_TRKTYP, AG_MODE,   977
     *AG_ECODE,AG_PDG,IPRIN, AG_ALPHAN,AG_ALPHAK(10),IGOR                 977
      REAL AG_FIELDM,AG_TMAXFD,AG_STEMAX,AG_DEEMAX,AG_EPSIL,AG_STMIN,     977
     *AG_DENS,AG_RADL,AG_ABSL, AG_THETAX,AG_PHIX,AG_THETAY,AG_PHIY,       977
     *AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,AG_ALFAZ, AG_ALPHAL(10),        977
     *AG_ALPHAX,AG_ALPHAY,AG_ALPHAZ,AG_TYPE, AG_DX, AG_DX1, AG_DX2,       977
     *AG_DY, AG_DY1, AG_DY2, AG_DMAXMS, AG_TWIST, AG_THET, AG_THE1,       977
     *AG_THE2, AG_PHI, AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2,       977
     *AG_RMIN, AG_RMAX, AG_RMN, AG_RMX, AG_ZI , AG_RMN1, AG_RMN2,         977
     *AG_RMX1, AG_RMX2, AG_H1, AG_H2, AG_BL1, AG_BL2, AG_TL1, AG_TL2,     977
     *AG_DPHI, AG_DZ, AG_TWIS, AG_X, AG_Y, AG_Z, AG_A, AG_ZA, AG_W,       977
     *AG_STEP, AG_C0, AG_PAR, AG_AA, AG_ZZ, AG_WW, AG_LX, AG_LY, AG_LZ,   977
     *AG_HX, AG_HY, AG_HZ, AG_P1, AG_P2, AG_STACK,AG_UBUF,AG_XHMAX,       977
     *AG_YHMAX,AG_ZHMAX,AG_RHMAX,AG_FHMAX,AG_FHMIN,AG_BIN, AG_CHARGE,     977
     *AG_MASS,AG_TLIFE,AG_BRATIO                                          977
      PARAMETER (AG_LSTACK=130,AG_NWUHIT=10,AG_NWUVOL=3,AG_MAGIC=         977
     *-696969,AG_LDETU=250)                                               977
      COMMON/AGCGLOB/ AG_MODULE,AG_CHDIR,AG_LEVEL,AG_IDTYPE,AG_IERROR,    977
     *AG_STANDALONE,IPRIN, AG_IPRIN,AG_IGEOM,AG_IDEBU,AG_IGRAP,AG_IHIST,  977
     *AG_IMFLD,AG_ISIMU                                                   977
C Inherited variables a: saved during internal calls                      977
C b: reset at the entry in a block and then saved                         977
      COMMON/AGCPARA/ AG_BEGCOM, AG_IVOLUME, AG_IMOTHER, AG_IGNUM,        977
     *AG_ISHAPE, AG_IMED, AG_IMAT, AG_IFIELD, AG_FIELDM, AG_TMAXFD,       977
     *AG_STEMAX, AG_DEEMAX, AG_EPSIL, AG_STMIN, AG_DENS, AG_RADL,         977
     *AG_ABSL, AG_DX, AG_DX1, AG_DX2, AG_DY, AG_DY1, AG_DY2, AG_RMN1,     977
     *AG_RMN2, AG_RMX1, AG_RMX2, AG_THET, AG_THE1, AG_THE2, AG_PHI,       977
     *AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2, AG_H1, AG_BL1,         977
     *AG_TL1, AG_H2, AG_BL2, AG_TL2, AG_RMIN, AG_RMAX, AG_DPHI,           977
     *AG_NPDIV, AG_NZ, AG_DZ, AG_TWIS, AG_LX, AG_LY, AG_LZ, AG_HX,        977
     *AG_HY, AG_HZ, AG_A, AG_ZA, AG_W, AG_NLMAT, AG_WORK, AG_SEEN,        977
     *AG_LSTY, AG_LWID, AG_COLO, AG_FILL, AG_SERIAL, AG_ISVOL,            977
     *AG_ISTATUS, AG_ZI(16), AG_RMN(16), AG_RMX(16), AG_VOLUME,           977
     *AG_MOTHER, AG_SHAPE, AG_CNICK, AG_ENDSAVE, AG_RESET1, AG_THETAX,    977
     *AG_PHIX,AG_THETAY,AG_PHIY,AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,      977
     *AG_ALFAZ, AG_X,AG_Y,AG_Z, AG_STEP,AG_C0,AG_NDIV,AG_IAXIS,           977
     *AG_NDVMAX,AG_ORTI,AG_NCOPY, AG_ALPHAN,AG_ALPHAK,AG_ALPHAL,          977
     *AG_RESET2, AG_KONLY, AG_ORT, AG_MARK                                977
C local variables valid inside same block                                 977
      COMMON/AGCLOCA/ AG_BEGSCR,AG_UBUF(100),AG_PAR(100),AG_AA(20),       977
     *AG_ZZ(20),AG_WW(20),AG_NWBUF, AG_XHMAX,AG_YHMAX,AG_ZHMAX,AG_RHMAX,  977
     *AG_FHMAX,AG_FHMIN,AG_NBITS,AG_BIN,AG_TYPE, AG_IROT,AG_NPAR,         977
     *AG_ISET,AG_IDET,AG_JDU,AG_IRESER, AG_ENDSCR, AG_TITLE,AG_EXNAME,    977
     *AG_PARLIST,AG_MATERIAL,AG_MIXTURE, AG_COMPONENT,AG_MEDIUM,          977
     *AG_OPTION                                                           977
      COMMON/AGCPART/AG_CODE,AG_TRKTYP,AG_MASS,AG_CHARGE,AG_TLIFE,        977
     *AG_BRATIO(6),AG_MODE(6),AG_PDG,AG_ECODE                             977
      COMMON/AGCSTAC/ AG_STACK(AG_LSTACK,15)                              977
C temporarly                                                              977
      EQUIVALENCE (AG_ATTRIBUTE,AG_WORK),(AG_STEMAX,AG_DMAXMS),           977
     *(AG_ALPHAX,AG_ALFAX), (AG_ALPHAY,AG_ALFAY),(AG_ALPHAZ,AG_ALFAZ),    977
     *(AG_TWIST,AG_TWIS),(AG_NPDIV,AG_NPDV),  (AG_P1,AG_HX),(AG_P2,       977
     *AG_HY)                                                              977
      REAL MIL_P,INCH_P                                                   977
      PARAMETER (INCH_P=2.54,MIL_P=0.00254)                               977
      INTEGER NSTRIPS                                                     977
      REAL *8 RDEL,RTH,DDN,DDUP,MEGATILE,P,XLEFT,YLEFT,XRIGHT,YRIGHT,     982
     *DXY,XX,YY,SQRT2, SQRT3, BASE/1.0/, RBOT/77.41/, RTOP/213.922/,      982
     *TAN_THETA_MAX/0.761952162/                                          982
C * LAMBDA FUNCTION TANF                                                  984
C ARGUEMENT FOR ETAX                                                      985
      REAL ETAX                                                           985
      REAL TANF                                                           986
      TANF(ETAX) = TAN(2*ATAN(EXP(-ETAX)))                                987
C Check CUT .EQ. 1                                                        988
      IF (CUT .EQ. 1) THEN                                                988
      RDEL = 3.938                                                        989
      NSTRIPS = 288                                                       990
      ELSE                                                                991
      RDEL = -.475                                                        992
      NSTRIPS = 285                                                       993
      ENDIF                                                               994
      XCENTER=0.                                                          995
      YCENTER=0.                                                          996
      LENGTH=0.                                                           997
C Check CUT==0                                                            998
      IF (CUT.EQ.0) THEN                                                  998
      RETURN                                                              999
      ENDIF                                                               1000
      SQRT2 = SQRT(2.0)                                                   1001
      SQRT3 = SQRT(3.0)                                                   1002
C .53 --- TENTATAVILY JCW-- WTF?                                          1003
      RTH = .53*RDEL                                                      1003
      DDN = SQRT(3.0)*1.713 + RDEL                                        1004
      DDUP = .5*1.846 + 1.713                                             1005
      MEGATILE = BASE + .01                                               1006
      P = .5*(ISTRIP-1)*MEGATILE + 41.3655                                1007
C Check P<=(.5*RBOT*SQRT3+RTH)                                            1008
      IF (P.LE.(.5*RBOT*SQRT3+RTH)) THEN                                  1008
      DXY = 1.9375*SQRT2                                                  1009
      XLEFT = .5*SQRT2*P*(SQRT3 + 1.) - DXY                               1010
      YLEFT = .5*SQRT2*P*(SQRT3 - 1.) - DXY                               1011
      YRIGHT = .5*SQRT2*(SQRT( RBOT*RBOT - P*P) - P)                      1012
      XRIGHT = SQRT2*P + YRIGHT                                           1013
      ELSEIF ((.5*RBOT*SQRT3+RTH).LT.P .AND. P .LE.(.5*RTOP+1.5)) THEN    1014
      DXY = 1.9375*SQRT2                                                  1015
      XLEFT = .5*SQRT2*P*(SQRT3 + 1.) - DXY                               1016
      YLEFT = .5*SQRT2*P*(SQRT3 - 1.) - DXY                               1017
      DXY = RDEL*SQRT2/SQRT3                                              1018
      YRIGHT = .5*SQRT2*P*(1.- 1./SQRT3)                                  1019
      XRIGHT = SQRT2*P - YRIGHT - DXY                                     1020
      YRIGHT = -YRIGHT - DXY                                              1021
      ELSEIF (P.GT.(.5*RTOP+1.5)) THEN                                    1022
      YLEFT = (SQRT(RTOP*RTOP - P*P) - P)/SQRT2                           1023
      XLEFT = SQRT2*P + YLEFT                                             1024
      DXY = RDEL*SQRT2/SQRT3                                              1025
      YRIGHT = .5*SQRT2*P*(1.- 1./SQRT3)                                  1026
      XRIGHT = SQRT2*P - YRIGHT - DXY                                     1027
      YRIGHT = -YRIGHT - DXY                                              1028
      DXY = 0.                                                            1029
C Check (.5*SQRT3*160.-DDN)<P .AND. P <=(.5*SQRT3*160.+DDUP)              1030
      IF ((.5*SQRT3*160.-DDN).LT.P .AND. P .LE.(.5*SQRT3*160.+DDUP))      1030
     *THEN                                                                1030
      XCENTER = .5*(SQRT3*160.+1.846)                                     1031
      YCENTER = XCENTER - .5*SQRT3*1.713                                  1032
C Check P>YCENTER                                                         1033
      IF (P.GT.YCENTER) THEN                                              1033
      DXY = .5*SQRT2*(2/SQRT3*RDEL + .5*SQRT3*1.846 + SQRT(1.713*1.713    1034
     *- (P-XCENTER)*(P-XCENTER)))                                         1034
      ELSE                                                                1035
      DXY = SQRT2/SQRT3*(P - .5*SQRT3* 160. + DDN)                        1036
      ENDIF                                                               1037
      ELSEIF ((.5*SQRT3*195.-DDN).LT.P .AND. P                            1038
     *.LE.(.5*SQRT3*195.+DDUP)) THEN                                      1038
      XCENTER = .5*(SQRT3*195.+1.846)                                     1039
      YCENTER = XCENTER - .5*SQRT3*1.713                                  1040
C Check P>YCENTER                                                         1041
      IF (P.GT.YCENTER) THEN                                              1041
      DXY = .5*SQRT2*(2/SQRT3*RDEL + .5*SQRT3*1.846 + SQRT(1.713*1.713    1042
     *- (P-XCENTER)*(P-XCENTER)))                                         1042
      ELSE                                                                1043
      DXY = SQRT2/SQRT3*(P - .5*SQRT3*195. + DDN)                         1044
      ENDIF                                                               1045
      ENDIF                                                               1046
      XRIGHT = XRIGHT + DXY                                               1047
      YRIGHT = YRIGHT + DXY                                               1048
      ENDIF                                                               1049
      DXY = SECTION*TAN_THETA_MAX - RTOP                                  1050
      XCENTER = .5*(XRIGHT+XLEFT) + DXY                                   1051
      YCENTER = .5*(YRIGHT+YLEFT)                                         1052
      XX = .5*SQRT2*(XLEFT+YLEFT)                                         1053
      YY = .5*SQRT2*(XRIGHT+YRIGHT)                                       1054
      LENGTH = XX-YY                                                      1055
      RETURN                                                              1056
C SUBROUTINE ECAL_GET_STRIP                                               1057
      END                                                                 1057
