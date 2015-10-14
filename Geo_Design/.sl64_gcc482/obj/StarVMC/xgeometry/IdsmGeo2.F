C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE IDSMGEO2                                                 1
C IDSMGEO2 SIMPLIFIED BEAM SUPPORT CONE FOR 2013                          1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'IDSMGEO2'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'SIMPLIFIED BEAM SUPPORT CONE FOR 2013'/              1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C CREATED 10/04/2011                                                      1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 CREATED /'10/04/2011'/                                 2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C AUTHOR JAN BALEWSKI MIT, WILLIE LEIGHT MIT (MATERIAL MIXES),FLEMMING V  2
C IDEBAEK                                                                 2
      CHARACTER*40 AUTHOR /'JAN BALEWSKI MIT, WILLIE LEIGHT MIT (MATERIA  3
     *L MIXES),FLEMMING VIDEBAEK'/                                        3
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
C - Natural constants (or considered as such)                             4
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   4
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        4
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       4
      INTEGER BLOCK_IDSM                                                  4
      INTEGER BLOCK_TPRR                                                  16
      INTEGER BLOCK_TPRT                                                  16
      INTEGER BLOCK_OSCA                                                  16
      INTEGER BLOCK_SUCB                                                  16
      INTEGER BLOCK_SUCC                                                  16
      INTEGER BLOCK_SUCD                                                  16
      INTEGER BLOCK_SUCE                                                  16
      INTEGER BLOCK_SUCF                                                  16
      INTEGER BLOCK_SUCG                                                  16
      INTEGER BLOCK_FGRL                                                  16
      INTEGER BLOCK_FGHV                                                  16
      INTEGER BLOCK_EFSA                                                  16
      REAL INCH/2.54/                                                     16
      REAL INR, OUTR,LENGTHZ, EASTZ, WESTZ, EASTB, WESTB, K,SINA, COSA,   19
     *RESR, ANGRES,M,PM, J,ANGRAIL, RRAIL, DPHIHV, ANGFGTCBL, THETA,      19
     *DZC                                                                 19
C -                                                                       21
      REAL IDSC_VERSION                                                   21
      EQUIVALENCE (BANK_IDSC(3),IDSC_VERSION,RE_IDSC_VERSION),            21
     *(NAMES_IDSC(1),NIDSC_VERSION)                                       21
      EQUIVALENCE (COMMEN_IDSC(1),CIDSC_VERSION),(MAP_IDSC(1,1),          21
     *MIDSC_VERSION)                                                      21
      CHARACTER CIDSC_VERSION*40/' '/                                     21
      CHARACTER NIDSC_VERSION*12/'R.VERSION'/                             21
      INTEGER MIDSC_VERSION(2)/1,1/                                       21
      REAL RE_IDSC_VERSION                                                21
C -                                                                       21
      INTEGER LEN_IDSC,LLEN_IDSC,LINK_IDSC                                21
      PARAMETER (LEN_IDSC=3)                                              21
      PARAMETER (LLEN_IDSC=1)                                             21
      INTEGER FLAG_IDSC/0/,MAP_IDSC(2,LLEN_IDSC),IDNUM_IDSC(10)           21
      CHARACTER NAMES_IDSC(LLEN_IDSC)*12,COMMEN_IDSC(LLEN_IDSC)*40        21
      DATA IDNUM_IDSC/10*0/                                               21
      REAL BANK_IDSC(LEN_IDSC)                                            21
      CHARACTER*60 BPATH_IDSC/' '/                                        21
      SAVE FLAG_IDSC,MAP_IDSC,IDNUM_IDSC,NAMES_IDSC,COMMEN_IDSC,          21
     *BPATH_IDSC                                                          21
      DATA LINK_IDSC/0/,BANK_IDSC/LEN_IDSC*0/                             21
C *                                                                       21
C -                                                                       23
      REAL IDSG_VERSION                                                   23
      EQUIVALENCE (BANK_IDSG(3),IDSG_VERSION,RE_IDSG_VERSION),            23
     *(NAMES_IDSG(1),NIDSG_VERSION)                                       23
      EQUIVALENCE (COMMEN_IDSG(1),CIDSG_VERSION),(MAP_IDSG(1,1),          23
     *MIDSG_VERSION)                                                      23
      CHARACTER CIDSG_VERSION*40/' '/                                     23
      CHARACTER NIDSG_VERSION*12/'R.VERSION'/                             23
      INTEGER MIDSG_VERSION(2)/1,1/                                       23
      REAL RE_IDSG_VERSION                                                23
C -                                                                       23
      REAL IDSG_RF                                                        23
      EQUIVALENCE (BANK_IDSG(4),IDSG_RF,RE_IDSG_RF),(NAMES_IDSG(2),       23
     *NIDSG_RF)                                                           23
      EQUIVALENCE (COMMEN_IDSG(2),CIDSG_RF),(MAP_IDSG(1,2),MIDSG_RF)      23
      CHARACTER CIDSG_RF*40/' '/                                          23
      CHARACTER NIDSG_RF*12/'R.RF'/                                       23
      INTEGER MIDSG_RF(2)/1,1/                                            23
      REAL RE_IDSG_RF                                                     23
C -                                                                       23
      REAL IDSG_ANGFLAT                                                   23
      EQUIVALENCE (BANK_IDSG(5),IDSG_ANGFLAT,RE_IDSG_ANGFLAT),            23
     *(NAMES_IDSG(3),NIDSG_ANGFLAT)                                       23
      EQUIVALENCE (COMMEN_IDSG(3),CIDSG_ANGFLAT),(MAP_IDSG(1,3),          23
     *MIDSG_ANGFLAT)                                                      23
      CHARACTER CIDSG_ANGFLAT*40/' '/                                     23
      CHARACTER NIDSG_ANGFLAT*12/'R.ANGFLAT'/                             23
      INTEGER MIDSG_ANGFLAT(2)/1,1/                                       23
      REAL RE_IDSG_ANGFLAT                                                23
C -                                                                       23
      REAL IDSG_R1RES                                                     23
      EQUIVALENCE (BANK_IDSG(6),IDSG_R1RES,RE_IDSG_R1RES),                23
     *(NAMES_IDSG(4),NIDSG_R1RES)                                         23
      EQUIVALENCE (COMMEN_IDSG(4),CIDSG_R1RES),(MAP_IDSG(1,4),            23
     *MIDSG_R1RES)                                                        23
      CHARACTER CIDSG_R1RES*40/' '/                                       23
      CHARACTER NIDSG_R1RES*12/'R.R1RES'/                                 23
      INTEGER MIDSG_R1RES(2)/1,1/                                         23
      REAL RE_IDSG_R1RES                                                  23
C -                                                                       23
      REAL IDSG_R2RES                                                     23
      EQUIVALENCE (BANK_IDSG(7),IDSG_R2RES,RE_IDSG_R2RES),                23
     *(NAMES_IDSG(5),NIDSG_R2RES)                                         23
      EQUIVALENCE (COMMEN_IDSG(5),CIDSG_R2RES),(MAP_IDSG(1,5),            23
     *MIDSG_R2RES)                                                        23
      CHARACTER CIDSG_R2RES*40/' '/                                       23
      CHARACTER NIDSG_R2RES*12/'R.R2RES'/                                 23
      INTEGER MIDSG_R2RES(2)/1,1/                                         23
      REAL RE_IDSG_R2RES                                                  23
C -                                                                       23
      REAL IDSG_RRRES                                                     23
      EQUIVALENCE (BANK_IDSG(8),IDSG_RRRES,RE_IDSG_RRRES),                23
     *(NAMES_IDSG(6),NIDSG_RRRES)                                         23
      EQUIVALENCE (COMMEN_IDSG(6),CIDSG_RRRES),(MAP_IDSG(1,6),            23
     *MIDSG_RRRES)                                                        23
      CHARACTER CIDSG_RRRES*40/' '/                                       23
      CHARACTER NIDSG_RRRES*12/'R.RRRES'/                                 23
      INTEGER MIDSG_RRRES(2)/1,1/                                         23
      REAL RE_IDSG_RRRES                                                  23
C -                                                                       23
      REAL IDSG_DANGRES                                                   23
      EQUIVALENCE (BANK_IDSG(9),IDSG_DANGRES,RE_IDSG_DANGRES),            23
     *(NAMES_IDSG(7),NIDSG_DANGRES)                                       23
      EQUIVALENCE (COMMEN_IDSG(7),CIDSG_DANGRES),(MAP_IDSG(1,7),          23
     *MIDSG_DANGRES)                                                      23
      CHARACTER CIDSG_DANGRES*40/' '/                                     23
      CHARACTER NIDSG_DANGRES*12/'R.DANGRES'/                             23
      INTEGER MIDSG_DANGRES(2)/1,1/                                       23
      REAL RE_IDSG_DANGRES                                                23
C -                                                                       23
      REAL IDSG_DXRES                                                     23
      EQUIVALENCE (BANK_IDSG(10),IDSG_DXRES,RE_IDSG_DXRES),               23
     *(NAMES_IDSG(8),NIDSG_DXRES)                                         23
      EQUIVALENCE (COMMEN_IDSG(8),CIDSG_DXRES),(MAP_IDSG(1,8),            23
     *MIDSG_DXRES)                                                        23
      CHARACTER CIDSG_DXRES*40/' '/                                       23
      CHARACTER NIDSG_DXRES*12/'R.DXRES'/                                 23
      INTEGER MIDSG_DXRES(2)/1,1/                                         23
      REAL RE_IDSG_DXRES                                                  23
C -                                                                       23
      REAL IDSG_DYRES                                                     23
      EQUIVALENCE (BANK_IDSG(11),IDSG_DYRES,RE_IDSG_DYRES),               23
     *(NAMES_IDSG(9),NIDSG_DYRES)                                         23
      EQUIVALENCE (COMMEN_IDSG(9),CIDSG_DYRES),(MAP_IDSG(1,9),            23
     *MIDSG_DYRES)                                                        23
      CHARACTER CIDSG_DYRES*40/' '/                                       23
      CHARACTER NIDSG_DYRES*12/'R.DYRES'/                                 23
      INTEGER MIDSG_DYRES(2)/1,1/                                         23
      REAL RE_IDSG_DYRES                                                  23
C -                                                                       23
      REAL IDSG_FGTSTARTZ                                                 23
      EQUIVALENCE (BANK_IDSG(12),IDSG_FGTSTARTZ,RE_IDSG_FGTSTARTZ),       23
     *(NAMES_IDSG(10),NIDSG_FGTSTARTZ)                                    23
      EQUIVALENCE (COMMEN_IDSG(10),CIDSG_FGTSTARTZ),(MAP_IDSG(1,10),      23
     *MIDSG_FGTSTARTZ)                                                    23
      CHARACTER CIDSG_FGTSTARTZ*40/' '/                                   23
      CHARACTER NIDSG_FGTSTARTZ*12/'R.FGTSTARTZ'/                         23
      INTEGER MIDSG_FGTSTARTZ(2)/1,1/                                     23
      REAL RE_IDSG_FGTSTARTZ                                              23
C -                                                                       23
      REAL IDSG_FGTDISKSTEPZ                                              23
      EQUIVALENCE (BANK_IDSG(13),IDSG_FGTDISKSTEPZ,                       23
     *RE_IDSG_FGTDISKSTEPZ),(NAMES_IDSG(11),NIDSG_FGTDISKSTEPZ)           23
      EQUIVALENCE (COMMEN_IDSG(11),CIDSG_FGTDISKSTEPZ),(MAP_IDSG(1,11),   23
     *MIDSG_FGTDISKSTEPZ)                                                 23
      CHARACTER CIDSG_FGTDISKSTEPZ*40/' '/                                23
      CHARACTER NIDSG_FGTDISKSTEPZ*12/'R.FGTDISKSTEPZ'/                   23
      INTEGER MIDSG_FGTDISKSTEPZ(2)/1,1/                                  23
      REAL RE_IDSG_FGTDISKSTEPZ                                           23
C -                                                                       23
      INTEGER IDSG_FGTNDISK                                               23
      EQUIVALENCE (BANK_IDSG(14),IDSG_FGTNDISK,RE_IDSG_FGTNDISK),         23
     *(NAMES_IDSG(12),NIDSG_FGTNDISK)                                     23
      EQUIVALENCE (COMMEN_IDSG(12),CIDSG_FGTNDISK),(MAP_IDSG(1,12),       23
     *MIDSG_FGTNDISK)                                                     23
      CHARACTER CIDSG_FGTNDISK*40/' '/                                    23
      CHARACTER NIDSG_FGTNDISK*12/'I.FGTNDISK'/                           23
      INTEGER MIDSG_FGTNDISK(2)/1,1/                                      23
      REAL RE_IDSG_FGTNDISK                                               23
C -                                                                       23
      REAL IDSG_EWCSINNERRAD                                              23
      EQUIVALENCE (BANK_IDSG(15),IDSG_EWCSINNERRAD,                       23
     *RE_IDSG_EWCSINNERRAD),(NAMES_IDSG(13),NIDSG_EWCSINNERRAD)           23
      EQUIVALENCE (COMMEN_IDSG(13),CIDSG_EWCSINNERRAD),(MAP_IDSG(1,13),   23
     *MIDSG_EWCSINNERRAD)                                                 23
      CHARACTER CIDSG_EWCSINNERRAD*40/' '/                                23
      CHARACTER NIDSG_EWCSINNERRAD*12/'R.EWCSINNERRAD'/                   23
      INTEGER MIDSG_EWCSINNERRAD(2)/1,1/                                  23
      REAL RE_IDSG_EWCSINNERRAD                                           23
C -                                                                       23
      REAL IDSG_EWCSOUTERRAD                                              23
      EQUIVALENCE (BANK_IDSG(16),IDSG_EWCSOUTERRAD,                       23
     *RE_IDSG_EWCSOUTERRAD),(NAMES_IDSG(14),NIDSG_EWCSOUTERRAD)           23
      EQUIVALENCE (COMMEN_IDSG(14),CIDSG_EWCSOUTERRAD),(MAP_IDSG(1,14),   23
     *MIDSG_EWCSOUTERRAD)                                                 23
      CHARACTER CIDSG_EWCSOUTERRAD*40/' '/                                23
      CHARACTER NIDSG_EWCSOUTERRAD*12/'R.EWCSOUTERRAD'/                   23
      INTEGER MIDSG_EWCSOUTERRAD(2)/1,1/                                  23
      REAL RE_IDSG_EWCSOUTERRAD                                           23
C -                                                                       23
      INTEGER LEN_IDSG,LLEN_IDSG,LINK_IDSG                                23
      PARAMETER (LEN_IDSG=16)                                             23
      PARAMETER (LLEN_IDSG=14)                                            23
      INTEGER FLAG_IDSG/0/,MAP_IDSG(2,LLEN_IDSG),IDNUM_IDSG(10)           23
      CHARACTER NAMES_IDSG(LLEN_IDSG)*12,COMMEN_IDSG(LLEN_IDSG)*40        23
      DATA IDNUM_IDSG/10*0/                                               23
      REAL BANK_IDSG(LEN_IDSG)                                            23
      CHARACTER*60 BPATH_IDSG/' '/                                        23
      SAVE FLAG_IDSG,MAP_IDSG,IDNUM_IDSG,NAMES_IDSG,COMMEN_IDSG,          23
     *BPATH_IDSG                                                          23
      DATA LINK_IDSG/0/,BANK_IDSG/LEN_IDSG*0/                             23
C *                                                                       23
C -                                                                       24
      REAL IDSA_VERSION                                                   24
      EQUIVALENCE (BANK_IDSA(3),IDSA_VERSION,RE_IDSA_VERSION),            24
     *(NAMES_IDSA(1),NIDSA_VERSION)                                       24
      EQUIVALENCE (COMMEN_IDSA(1),CIDSA_VERSION),(MAP_IDSA(1,1),          24
     *MIDSA_VERSION)                                                      24
      CHARACTER CIDSA_VERSION*40/' '/                                     24
      CHARACTER NIDSA_VERSION*12/'R.VERSION'/                             24
      INTEGER MIDSA_VERSION(2)/1,1/                                       24
      REAL RE_IDSA_VERSION                                                24
C -                                                                       24
      REAL IDSA_X                                                         24
      EQUIVALENCE (BANK_IDSA(4),IDSA_X,RE_IDSA_X),(NAMES_IDSA(2),         24
     *NIDSA_X)                                                            24
      EQUIVALENCE (COMMEN_IDSA(2),CIDSA_X),(MAP_IDSA(1,2),MIDSA_X)        24
      CHARACTER CIDSA_X*40/' '/                                           24
      CHARACTER NIDSA_X*12/'R.X'/                                         24
      INTEGER MIDSA_X(2)/1,1/                                             24
      REAL RE_IDSA_X                                                      24
C -                                                                       24
      REAL IDSA_Y                                                         24
      EQUIVALENCE (BANK_IDSA(5),IDSA_Y,RE_IDSA_Y),(NAMES_IDSA(3),         24
     *NIDSA_Y)                                                            24
      EQUIVALENCE (COMMEN_IDSA(3),CIDSA_Y),(MAP_IDSA(1,3),MIDSA_Y)        24
      CHARACTER CIDSA_Y*40/' '/                                           24
      CHARACTER NIDSA_Y*12/'R.Y'/                                         24
      INTEGER MIDSA_Y(2)/1,1/                                             24
      REAL RE_IDSA_Y                                                      24
C -                                                                       24
      REAL IDSA_Z                                                         24
      EQUIVALENCE (BANK_IDSA(6),IDSA_Z,RE_IDSA_Z),(NAMES_IDSA(4),         24
     *NIDSA_Z)                                                            24
      EQUIVALENCE (COMMEN_IDSA(4),CIDSA_Z),(MAP_IDSA(1,4),MIDSA_Z)        24
      CHARACTER CIDSA_Z*40/' '/                                           24
      CHARACTER NIDSA_Z*12/'R.Z'/                                         24
      INTEGER MIDSA_Z(2)/1,1/                                             24
      REAL RE_IDSA_Z                                                      24
C -                                                                       24
      REAL IDSA_THETAX                                                    24
      EQUIVALENCE (BANK_IDSA(7),IDSA_THETAX,RE_IDSA_THETAX),              24
     *(NAMES_IDSA(5),NIDSA_THETAX)                                        24
      EQUIVALENCE (COMMEN_IDSA(5),CIDSA_THETAX),(MAP_IDSA(1,5),           24
     *MIDSA_THETAX)                                                       24
      CHARACTER CIDSA_THETAX*40/' '/                                      24
      CHARACTER NIDSA_THETAX*12/'R.THETAX'/                               24
      INTEGER MIDSA_THETAX(2)/1,1/                                        24
      REAL RE_IDSA_THETAX                                                 24
C -                                                                       24
      REAL IDSA_THETAY                                                    24
      EQUIVALENCE (BANK_IDSA(8),IDSA_THETAY,RE_IDSA_THETAY),              24
     *(NAMES_IDSA(6),NIDSA_THETAY)                                        24
      EQUIVALENCE (COMMEN_IDSA(6),CIDSA_THETAY),(MAP_IDSA(1,6),           24
     *MIDSA_THETAY)                                                       24
      CHARACTER CIDSA_THETAY*40/' '/                                      24
      CHARACTER NIDSA_THETAY*12/'R.THETAY'/                               24
      INTEGER MIDSA_THETAY(2)/1,1/                                        24
      REAL RE_IDSA_THETAY                                                 24
C -                                                                       24
      REAL IDSA_THETAZ                                                    24
      EQUIVALENCE (BANK_IDSA(9),IDSA_THETAZ,RE_IDSA_THETAZ),              24
     *(NAMES_IDSA(7),NIDSA_THETAZ)                                        24
      EQUIVALENCE (COMMEN_IDSA(7),CIDSA_THETAZ),(MAP_IDSA(1,7),           24
     *MIDSA_THETAZ)                                                       24
      CHARACTER CIDSA_THETAZ*40/' '/                                      24
      CHARACTER NIDSA_THETAZ*12/'R.THETAZ'/                               24
      INTEGER MIDSA_THETAZ(2)/1,1/                                        24
      REAL RE_IDSA_THETAZ                                                 24
C -                                                                       24
      REAL IDSA_PHIX                                                      24
      EQUIVALENCE (BANK_IDSA(10),IDSA_PHIX,RE_IDSA_PHIX),(NAMES_IDSA(8),  24
     *NIDSA_PHIX)                                                         24
      EQUIVALENCE (COMMEN_IDSA(8),CIDSA_PHIX),(MAP_IDSA(1,8),             24
     *MIDSA_PHIX)                                                         24
      CHARACTER CIDSA_PHIX*40/' '/                                        24
      CHARACTER NIDSA_PHIX*12/'R.PHIX'/                                   24
      INTEGER MIDSA_PHIX(2)/1,1/                                          24
      REAL RE_IDSA_PHIX                                                   24
C -                                                                       24
      REAL IDSA_PHIY                                                      24
      EQUIVALENCE (BANK_IDSA(11),IDSA_PHIY,RE_IDSA_PHIY),(NAMES_IDSA(9),  24
     *NIDSA_PHIY)                                                         24
      EQUIVALENCE (COMMEN_IDSA(9),CIDSA_PHIY),(MAP_IDSA(1,9),             24
     *MIDSA_PHIY)                                                         24
      CHARACTER CIDSA_PHIY*40/' '/                                        24
      CHARACTER NIDSA_PHIY*12/'R.PHIY'/                                   24
      INTEGER MIDSA_PHIY(2)/1,1/                                          24
      REAL RE_IDSA_PHIY                                                   24
C -                                                                       24
      REAL IDSA_PHIZ                                                      24
      EQUIVALENCE (BANK_IDSA(12),IDSA_PHIZ,RE_IDSA_PHIZ),                 24
     *(NAMES_IDSA(10),NIDSA_PHIZ)                                         24
      EQUIVALENCE (COMMEN_IDSA(10),CIDSA_PHIZ),(MAP_IDSA(1,10),           24
     *MIDSA_PHIZ)                                                         24
      CHARACTER CIDSA_PHIZ*40/' '/                                        24
      CHARACTER NIDSA_PHIZ*12/'R.PHIZ'/                                   24
      INTEGER MIDSA_PHIZ(2)/1,1/                                          24
      REAL RE_IDSA_PHIZ                                                   24
C -                                                                       24
      INTEGER LEN_IDSA,LLEN_IDSA,LINK_IDSA                                24
      PARAMETER (LEN_IDSA=12)                                             24
      PARAMETER (LLEN_IDSA=10)                                            24
      INTEGER FLAG_IDSA/0/,MAP_IDSA(2,LLEN_IDSA),IDNUM_IDSA(10)           24
      CHARACTER NAMES_IDSA(LLEN_IDSA)*12,COMMEN_IDSA(LLEN_IDSA)*40        24
      DATA IDNUM_IDSA/10*0/                                               24
      REAL BANK_IDSA(LEN_IDSA)                                            24
      CHARACTER*60 BPATH_IDSA/' '/                                        24
      SAVE FLAG_IDSA,MAP_IDSA,IDNUM_IDSA,NAMES_IDSA,COMMEN_IDSA,          24
     *BPATH_IDSA                                                          24
      DATA LINK_IDSA/0/,BANK_IDSA/LEN_IDSA*0/                             24
C *                                                                       24
C *                                                                       25
      CALL AGSBEGM(MODULE,IPRIN)                                          25
      IF (IPRIN.LT.0) RETURN                                              25
C Filling bank IDSC - IDS CONTROL STRUCTURE                               25
      BANK_TITLE='IDS CONTROL STRUCTURE'                                  25
      BPATH_IDSC='IDSC*'                                                  25
      IDNUM_IDSC(1)=0                                                     26
C VERSIONING OF THE IDSM GEOMETRY DEFAULT Y2013 VERSION.                  26
      IDSC_VERSION=2.0                                                    26
      CIDSC_VERSION='VERSIONING OF THE IDSM GEOMETRY DEFAULT Y2013 VERSI  26
     *ON.'                                                                26
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'IDSC',    27
     *BPATH_IDSC,IDNUM_IDSC,1, MAP_IDSC,NAMES_IDSC,COMMEN_IDSC,           27
     *BANK_IDSC,LEN_IDSC,LLEN_IDSC,LINK_IDSC,FLAG_IDSC)                   27
C *                                                                       27
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'IDSC',    27
     *BPATH_IDSC,IDNUM_IDSC,1, MAP_IDSC,NAMES_IDSC,COMMEN_IDSC,           27
     *BANK_IDSC,LEN_IDSC,LLEN_IDSC,LINK_IDSC,FLAG_IDSC)                   27
C *                                                                       28
      CALL AGSBEGM(MODULE,IPRIN)                                          28
      IF (IPRIN.LT.0) RETURN                                              28
C Filling bank IDSG - IDS GEOMETRY DATA,46.1CM=INNER RADII OF IFC         28
      BANK_TITLE='IDS GEOMETRY DATA,46.1CM=INNER RADII OF IFC'            28
      BPATH_IDSG='IDSG*'                                                  28
      IDNUM_IDSG(1)=0                                                     29
C 2012 VERSION OF THE IDS GEOMETRY                                        29
      IDSG_VERSION=1.0                                                    29
      CIDSG_VERSION='2012 VERSION OF THE IDS GEOMETRY'                    29
C RADII OF INNER VOLUME BOUNDARY                                          30
      IDSG_RF=2.25                                                        30
      CIDSG_RF='RADII OF INNER VOLUME BOUNDARY'                           30
C ANGLE (DEG) FOR CENTER OF FLAT                                          31
      IDSG_ANGFLAT=106.                                                   31
      CIDSG_ANGFLAT='ANGLE (DEG) FOR CENTER OF FLAT'                      31
C RADIAL DISTANCE OF FOR TPC RESISTOR TUBES                               32
      IDSG_RRRES=43.                                                      32
      CIDSG_RRRES='RADIAL DISTANCE OF FOR TPC RESISTOR TUBES'             32
C INNER RADII FOR TPC RESISTOR TUBES                                      33
      IDSG_R1RES=1.17                                                     33
      CIDSG_R1RES='INNER RADII FOR TPC RESISTOR TUBES'                    33
C OUTER RADII FOR TPC RESISTOR TUBES                                      34
      IDSG_R2RES=1.27                                                     34
      CIDSG_R2RES='OUTER RADII FOR TPC RESISTOR TUBES'                    34
C OPENING ANGLE (DEG) FOR TPC RESISTOR TUBES                              35
      IDSG_DANGRES=11.3                                                   35
      CIDSG_DANGRES='OPENING ANGLE (DEG) FOR TPC RESISTOR TUBES'          35
C THICKNESSFOR TPC RESISTOR                                               36
      IDSG_DXRES=0.13                                                     36
      CIDSG_DXRES='THICKNESSFOR TPC RESISTOR'                             36
C DY FOR TPC RESISTOR                                                     37
      IDSG_DYRES=2.                                                       37
      CIDSG_DYRES='DY FOR TPC RESISTOR'                                   37
C POSITION OF SENSITIVE VOLUME OF THE 1ST DISK                            38
      IDSG_FGTSTARTZ=70.                                                  38
      CIDSG_FGTSTARTZ='POSITION OF SENSITIVE VOLUME OF THE 1ST DISK'      38
C DISK SEPARATION ALONG Z                                                 39
      IDSG_FGTDISKSTEPZ=10.                                               39
      CIDSG_FGTDISKSTEPZ='DISK SEPARATION ALONG Z'                        39
C NUMBER OF DISKS                                                         40
      IDSG_FGTNDISK=6                                                     40
      CIDSG_FGTNDISK='NUMBER OF DISKS'                                    40
C INNER RADIUS OF ESC OSC                                                 41
      IDSG_EWCSINNERRAD=39.91                                             41
      CIDSG_EWCSINNERRAD='INNER RADIUS OF ESC OSC'                        41
C INNER RADIUS OF ESC OSC                                                 42
      IDSG_EWCSOUTERRAD=40.005                                            42
      CIDSG_EWCSOUTERRAD='INNER RADIUS OF ESC OSC'                        42
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'IDSG',    43
     *BPATH_IDSG,IDNUM_IDSG,1, MAP_IDSG,NAMES_IDSG,COMMEN_IDSG,           43
     *BANK_IDSG,LEN_IDSG,LLEN_IDSG,LINK_IDSG,FLAG_IDSG)                   43
C *                                                                       43
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'IDSG',    43
     *BPATH_IDSG,IDNUM_IDSG,1, MAP_IDSG,NAMES_IDSG,COMMEN_IDSG,           43
     *BANK_IDSG,LEN_IDSG,LLEN_IDSG,LINK_IDSG,FLAG_IDSG)                   43
C *                                                                       44
      CALL AGSBEGM(MODULE,IPRIN)                                          44
      IF (IPRIN.LT.0) RETURN                                              44
C Filling bank IDSG - IDS GEOMETRY DATA,46.1CM=INNER RADII OF IFC         44
      BANK_TITLE='IDS GEOMETRY DATA,46.1CM=INNER RADII OF IFC'            44
      BPATH_IDSG='IDSG*'                                                  44
      IDNUM_IDSG(1)=0                                                     45
C 2013 VERSIONING OF THE IDS GEOMETRY                                     45
      IDSG_VERSION=2.0                                                    45
      CIDSG_VERSION='2013 VERSIONING OF THE IDS GEOMETRY'                 45
C RADII OF INNER VOLUME BOUNDARY                                          46
      IDSG_RF=0.0                                                         46
      CIDSG_RF='RADII OF INNER VOLUME BOUNDARY'                           46
C ANGLE (DEG) FOR CENTER OF FLAT                                          47
      IDSG_ANGFLAT=106.                                                   47
      CIDSG_ANGFLAT='ANGLE (DEG) FOR CENTER OF FLAT'                      47
C RADIAL DISTANCE OF FOR TPC RESISTOR TUBES                               48
      IDSG_RRRES=43.                                                      48
      CIDSG_RRRES='RADIAL DISTANCE OF FOR TPC RESISTOR TUBES'             48
C INNER RADII FOR TPC RESISTOR TUBES                                      49
      IDSG_R1RES=1.17                                                     49
      CIDSG_R1RES='INNER RADII FOR TPC RESISTOR TUBES'                    49
C OUTER RADII FOR TPC RESISTOR TUBES                                      50
      IDSG_R2RES=1.27                                                     50
      CIDSG_R2RES='OUTER RADII FOR TPC RESISTOR TUBES'                    50
C OPENING ANGLE (DEG) FOR TPC RESISTOR TUBES                              51
      IDSG_DANGRES=11.3                                                   51
      CIDSG_DANGRES='OPENING ANGLE (DEG) FOR TPC RESISTOR TUBES'          51
C THICKNESSFOR TPC RESISTOR                                               52
      IDSG_DXRES=0.13                                                     52
      CIDSG_DXRES='THICKNESSFOR TPC RESISTOR'                             52
C DY FOR TPC RESISTOR                                                     53
      IDSG_DYRES=2.                                                       53
      CIDSG_DYRES='DY FOR TPC RESISTOR'                                   53
C POSITION OF SENSITIVE VOLUME OF THE 1ST DISK                            54
      IDSG_FGTSTARTZ=70.                                                  54
      CIDSG_FGTSTARTZ='POSITION OF SENSITIVE VOLUME OF THE 1ST DISK'      54
C DISK SEPARATION ALONG Z                                                 55
      IDSG_FGTDISKSTEPZ=10.                                               55
      CIDSG_FGTDISKSTEPZ='DISK SEPARATION ALONG Z'                        55
C NUMBER OF DISKS                                                         56
      IDSG_FGTNDISK=6                                                     56
      CIDSG_FGTNDISK='NUMBER OF DISKS'                                    56
C INNER RADIUS OF ESC OSC                                                 57
      IDSG_EWCSINNERRAD=39.91                                             57
      CIDSG_EWCSINNERRAD='INNER RADIUS OF ESC OSC'                        57
C INNER RADIUS OF ESC OSC                                                 58
      IDSG_EWCSOUTERRAD=40.005                                            58
      CIDSG_EWCSOUTERRAD='INNER RADIUS OF ESC OSC'                        58
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'IDSG',    59
     *BPATH_IDSG,IDNUM_IDSG,1, MAP_IDSG,NAMES_IDSG,COMMEN_IDSG,           59
     *BANK_IDSG,LEN_IDSG,LLEN_IDSG,LINK_IDSG,FLAG_IDSG)                   59
C *                                                                       59
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'IDSG',    59
     *BPATH_IDSG,IDNUM_IDSG,1, MAP_IDSG,NAMES_IDSG,COMMEN_IDSG,           59
     *BANK_IDSG,LEN_IDSG,LLEN_IDSG,LINK_IDSG,FLAG_IDSG)                   59
C *                                                                       60
      CALL AGSBEGM(MODULE,IPRIN)                                          60
      IF (IPRIN.LT.0) RETURN                                              60
C Filling bank IDSG - IDS GEOMETRY DATA, 46.1CM INNER RADII OF IFC        60
      BANK_TITLE='IDS GEOMETRY DATA, 46.1CM INNER RADII OF IFC'           60
      BPATH_IDSG='IDSG*'                                                  60
      IDNUM_IDSG(1)=0                                                     61
C YEAR 14, STRIP OUT THE FGT CABLES                                       61
      IDSG_VERSION=14                                                     61
      CIDSG_VERSION='YEAR 14, STRIP OUT THE FGT CABLES'                   61
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'IDSG',    62
     *BPATH_IDSG,IDNUM_IDSG,1, MAP_IDSG,NAMES_IDSG,COMMEN_IDSG,           62
     *BANK_IDSG,LEN_IDSG,LLEN_IDSG,LINK_IDSG,FLAG_IDSG)                   62
C *                                                                       62
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'IDSG',    62
     *BPATH_IDSG,IDNUM_IDSG,1, MAP_IDSG,NAMES_IDSG,COMMEN_IDSG,           62
     *BANK_IDSG,LEN_IDSG,LLEN_IDSG,LINK_IDSG,FLAG_IDSG)                   62
C *                                                                       63
      CALL AGSBEGM(MODULE,IPRIN)                                          63
      IF (IPRIN.LT.0) RETURN                                              63
C Filling bank IDSA - ALIGNMENT OF THE IDSM GEOMETRY WITHIN THE CAVE NO   63
C FGT CABLES                                                              63
      BANK_TITLE='ALIGNMENT OF THE IDSM GEOMETRY WITHIN THE CAVE NO FGT   63
     *CABLES'                                                             63
      BPATH_IDSA='IDSA*'                                                  63
      IDNUM_IDSA(1)=0                                                     64
C DEFAULT ALIGNMENT OF IDSM AT (0,0,0) WITH NO ROTATION                   64
      IDSA_VERSION=1.0                                                    64
      CIDSA_VERSION='DEFAULT ALIGNMENT OF IDSM AT (0,0,0) WITH NO ROTATI  64
     *ON'                                                                 64
C X-ALIGNMENT                                                             65
      IDSA_X=0.0                                                          65
      CIDSA_X='X-ALIGNMENT'                                               65
C Y-ALIGNMENT                                                             66
      IDSA_Y=0.0                                                          66
      CIDSA_Y='Y-ALIGNMENT'                                               66
C Z-ALIGNMENT                                                             67
      IDSA_Z=0.0                                                          67
      CIDSA_Z='Z-ALIGNMENT'                                               67
C ALIGN X`-AXIS 90 DEGREES IN THETA WRT CAVE                              68
      IDSA_THETAX=90.0                                                    68
      CIDSA_THETAX='ALIGN X`-AXIS 90 DEGREES IN THETA WRT CAVE'           68
C ALIGN X`-AXIS 0 DEGREES IN PHI WRT CAVE                                 69
      IDSA_PHIX=0.0                                                       69
      CIDSA_PHIX='ALIGN X`-AXIS 0 DEGREES IN PHI WRT CAVE'                69
C ALIGN Y`-AXIS 90 DEGREES IN THETA WRT CAVE                              70
      IDSA_THETAY=90.0                                                    70
      CIDSA_THETAY='ALIGN Y`-AXIS 90 DEGREES IN THETA WRT CAVE'           70
C ALIGN Y`-AXIS 0 DEGREES IN PHI WRT CAVE                                 71
      IDSA_PHIY=90.0                                                      71
      CIDSA_PHIY='ALIGN Y`-AXIS 0 DEGREES IN PHI WRT CAVE'                71
C ALIGN Z`-AXIS 0 DEGREES IN THETA WRT CAVE                               72
      IDSA_THETAZ=0.0                                                     72
      CIDSA_THETAZ='ALIGN Z`-AXIS 0 DEGREES IN THETA WRT CAVE'            72
C ALIGN Z`-AXIS 0 DEGREES IN PHI WRT CAVE                                 73
      IDSA_PHIZ=0.0                                                       73
      CIDSA_PHIZ='ALIGN Z`-AXIS 0 DEGREES IN PHI WRT CAVE'                73
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'IDSA',    74
     *BPATH_IDSA,IDNUM_IDSA,1, MAP_IDSA,NAMES_IDSA,COMMEN_IDSA,           74
     *BANK_IDSA,LEN_IDSA,LLEN_IDSA,LINK_IDSA,FLAG_IDSA)                   74
C *                                                                       74
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'IDSA',    74
     *BPATH_IDSA,IDNUM_IDSA,1, MAP_IDSA,NAMES_IDSA,COMMEN_IDSA,           74
     *BANK_IDSA,LEN_IDSA,LLEN_IDSA,LINK_IDSA,FLAG_IDSA)                   74
C Geant COMPONENT : O ===> A=16 Z=8 W=0.062 <===                          75
      CALL AGSRESET                                                       75
      AG_A = 16                                                           75
      AG_Z = 8                                                            75
      AG_W = 0.062                                                        75
      AG_TITLE = 'O'                                                      75
      AG_EXNAME = 'COMPONENT'                                             75
      AG_PARLIST = 'A_Z_W'                                                75
      CALL AXCOMPONENT                                                    75
C *                                                                       75
C Geant COMPONENT : C ===> A=12 Z=6 W=0.892 <===                          75
      CALL AGSRESET                                                       76
      AG_A = 12                                                           76
      AG_Z = 6                                                            76
      AG_W = 0.892                                                        76
      AG_TITLE = 'C'                                                      76
      AG_EXNAME = 'COMPONENT'                                             76
      AG_PARLIST = 'A_Z_W'                                                76
      CALL AXCOMPONENT                                                    76
C *                                                                       76
C Geant COMPONENT : H ===> A=1 Z=1 W=0.019 <===                           76
      CALL AGSRESET                                                       77
      AG_A = 1                                                            77
      AG_Z = 1                                                            77
      AG_W = 0.019                                                        77
      AG_TITLE = 'H'                                                      77
      AG_EXNAME = 'COMPONENT'                                             77
      AG_PARLIST = 'A_Z_W'                                                77
      CALL AXCOMPONENT                                                    77
C *                                                                       77
C Geant COMPONENT : CL ===> A=35.5 Z=17 W=0.027 <===                      77
      CALL AGSRESET                                                       78
      AG_A = 35.5                                                         78
      AG_Z = 17                                                           78
      AG_W = 0.027                                                        78
      AG_TITLE = 'CL'                                                     78
      AG_EXNAME = 'COMPONENT'                                             78
      AG_PARLIST = 'A_Z_W'                                                78
      CALL AXCOMPONENT                                                    78
C *                                                                       78
C Geant MIXTURE : CFRPMIX ===> DENS=1.78 <===                             78
      CALL AGSRESET                                                       79
      AG_DENS = 1.78                                                      79
      AG_TITLE = 'CFRPMIX'                                                79
      AG_EXNAME = 'MIXTURE'                                               79
      AG_PARLIST = 'DENS'                                                 79
      CALL AXMIXTURE                                                      79
C *                                                                       79
C Geant COMPONENT : AL ===> A=27 Z=13 W=1. <===                           79
      CALL AGSRESET                                                       80
      AG_A = 27                                                           80
      AG_Z = 13                                                           80
      AG_W = 1.                                                           80
      AG_TITLE = 'AL'                                                     80
      AG_EXNAME = 'COMPONENT'                                             80
      AG_PARLIST = 'A_Z_W'                                                80
      CALL AXCOMPONENT                                                    80
C *                                                                       80
C Geant MIXTURE : ALPURE ===> DENS=2.80 <===                              80
      CALL AGSRESET                                                       81
      AG_DENS = 2.80                                                      81
      AG_TITLE = 'ALPURE'                                                 81
      AG_EXNAME = 'MIXTURE'                                               81
      AG_PARLIST = 'DENS'                                                 81
      CALL AXMIXTURE                                                      81
C *                                                                       81
C Geant COMPONENT : O ===> A=16 Z=8 W=0.043 <===                          81
      CALL AGSRESET                                                       82
      AG_A = 16                                                           82
      AG_Z = 8                                                            82
      AG_W = 0.043                                                        82
      AG_TITLE = 'O'                                                      82
      AG_EXNAME = 'COMPONENT'                                             82
      AG_PARLIST = 'A_Z_W'                                                82
      CALL AXCOMPONENT                                                    82
C *                                                                       82
C Geant COMPONENT : C ===> A=12 Z=6 W=0.635 <===                          82
      CALL AGSRESET                                                       83
      AG_A = 12                                                           83
      AG_Z = 6                                                            83
      AG_W = 0.635                                                        83
      AG_TITLE = 'C'                                                      83
      AG_EXNAME = 'COMPONENT'                                             83
      AG_PARLIST = 'A_Z_W'                                                83
      CALL AXCOMPONENT                                                    83
C *                                                                       83
C Geant COMPONENT : H ===> A=1 Z=1 W=0.014 <===                           83
      CALL AGSRESET                                                       84
      AG_A = 1                                                            84
      AG_Z = 1                                                            84
      AG_W = 0.014                                                        84
      AG_TITLE = 'H'                                                      84
      AG_EXNAME = 'COMPONENT'                                             84
      AG_PARLIST = 'A_Z_W'                                                84
      CALL AXCOMPONENT                                                    84
C *                                                                       84
C Geant COMPONENT : CL ===> A=35.5 Z=17 W=0.019 <===                      84
      CALL AGSRESET                                                       85
      AG_A = 35.5                                                         85
      AG_Z = 17                                                           85
      AG_W = 0.019                                                        85
      AG_TITLE = 'CL'                                                     85
      AG_EXNAME = 'COMPONENT'                                             85
      AG_PARLIST = 'A_Z_W'                                                85
      CALL AXCOMPONENT                                                    85
C *                                                                       85
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.017 <===                        85
      CALL AGSRESET                                                       86
      AG_A = 27                                                           86
      AG_Z = 13                                                           86
      AG_W = 0.017                                                        86
      AG_TITLE = 'AL'                                                     86
      AG_EXNAME = 'COMPONENT'                                             86
      AG_PARLIST = 'A_Z_W'                                                86
      CALL AXCOMPONENT                                                    86
C *                                                                       86
C Geant COMPONENT : TI ===> A=47.9 Z=22 W=0.260 <===                      86
      CALL AGSRESET                                                       87
      AG_A = 47.9                                                         87
      AG_Z = 22                                                           87
      AG_W = 0.260                                                        87
      AG_TITLE = 'TI'                                                     87
      AG_EXNAME = 'COMPONENT'                                             87
      AG_PARLIST = 'A_Z_W'                                                87
      CALL AXCOMPONENT                                                    87
C *                                                                       87
C Geant COMPONENT : V ===> A=50.9 Z=23 W=0.012 <===                       87
      CALL AGSRESET                                                       88
      AG_A = 50.9                                                         88
      AG_Z = 23                                                           88
      AG_W = 0.012                                                        88
      AG_TITLE = 'V'                                                      88
      AG_EXNAME = 'COMPONENT'                                             88
      AG_PARLIST = 'A_Z_W'                                                88
      CALL AXCOMPONENT                                                    88
C *                                                                       88
C Geant MIXTURE : SUCBMIX ===> DENS=2.46 <===                             88
      CALL AGSRESET                                                       89
      AG_DENS = 2.46                                                      89
      AG_TITLE = 'SUCBMIX'                                                89
      AG_EXNAME = 'MIXTURE'                                               89
      AG_PARLIST = 'DENS'                                                 89
      CALL AXMIXTURE                                                      89
C *                                                                       89
C Geant COMPONENT : O ===> A=16 Z=8 W=0.048 <===                          89
      CALL AGSRESET                                                       90
      AG_A = 16                                                           90
      AG_Z = 8                                                            90
      AG_W = 0.048                                                        90
      AG_TITLE = 'O'                                                      90
      AG_EXNAME = 'COMPONENT'                                             90
      AG_PARLIST = 'A_Z_W'                                                90
      CALL AXCOMPONENT                                                    90
C *                                                                       90
C Geant COMPONENT : C ===> A=12 Z=6 W=0.694 <===                          90
      CALL AGSRESET                                                       91
      AG_A = 12                                                           91
      AG_Z = 6                                                            91
      AG_W = 0.694                                                        91
      AG_TITLE = 'C'                                                      91
      AG_EXNAME = 'COMPONENT'                                             91
      AG_PARLIST = 'A_Z_W'                                                91
      CALL AXCOMPONENT                                                    91
C *                                                                       91
C Geant COMPONENT : H ===> A=1 Z=1 W=0.015 <===                           91
      CALL AGSRESET                                                       92
      AG_A = 1                                                            92
      AG_Z = 1                                                            92
      AG_W = 0.015                                                        92
      AG_TITLE = 'H'                                                      92
      AG_EXNAME = 'COMPONENT'                                             92
      AG_PARLIST = 'A_Z_W'                                                92
      CALL AXCOMPONENT                                                    92
C *                                                                       92
C Geant COMPONENT : CL ===> A=35.5 Z=17 W=0.021 <===                      92
      CALL AGSRESET                                                       93
      AG_A = 35.5                                                         93
      AG_Z = 17                                                           93
      AG_W = 0.021                                                        93
      AG_TITLE = 'CL'                                                     93
      AG_EXNAME = 'COMPONENT'                                             93
      AG_PARLIST = 'A_Z_W'                                                93
      CALL AXCOMPONENT                                                    93
C *                                                                       93
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.013 <===                        93
      CALL AGSRESET                                                       94
      AG_A = 27                                                           94
      AG_Z = 13                                                           94
      AG_W = 0.013                                                        94
      AG_TITLE = 'AL'                                                     94
      AG_EXNAME = 'COMPONENT'                                             94
      AG_PARLIST = 'A_Z_W'                                                94
      CALL AXCOMPONENT                                                    94
C *                                                                       94
C Geant COMPONENT : TI ===> A=47.9 Z=22 W=0.200 <===                      94
      CALL AGSRESET                                                       95
      AG_A = 47.9                                                         95
      AG_Z = 22                                                           95
      AG_W = 0.200                                                        95
      AG_TITLE = 'TI'                                                     95
      AG_EXNAME = 'COMPONENT'                                             95
      AG_PARLIST = 'A_Z_W'                                                95
      CALL AXCOMPONENT                                                    95
C *                                                                       95
C Geant COMPONENT : V ===> A=50.9 Z=23 W=0.009 <===                       95
      CALL AGSRESET                                                       96
      AG_A = 50.9                                                         96
      AG_Z = 23                                                           96
      AG_W = 0.009                                                        96
      AG_TITLE = 'V'                                                      96
      AG_EXNAME = 'COMPONENT'                                             96
      AG_PARLIST = 'A_Z_W'                                                96
      CALL AXCOMPONENT                                                    96
C *                                                                       96
C Geant MIXTURE : SUCDMIX ===> DENS=2.37 <===                             96
      CALL AGSRESET                                                       97
      AG_DENS = 2.37                                                      97
      AG_TITLE = 'SUCDMIX'                                                97
      AG_EXNAME = 'MIXTURE'                                               97
      AG_PARLIST = 'DENS'                                                 97
      CALL AXMIXTURE                                                      97
C *                                                                       97
C Geant COMPONENT : O ===> A=16 Z=8 W=0.032 <===                          97
      CALL AGSRESET                                                       98
      AG_A = 16                                                           98
      AG_Z = 8                                                            98
      AG_W = 0.032                                                        98
      AG_TITLE = 'O'                                                      98
      AG_EXNAME = 'COMPONENT'                                             98
      AG_PARLIST = 'A_Z_W'                                                98
      CALL AXCOMPONENT                                                    98
C *                                                                       98
C Geant COMPONENT : C ===> A=12 Z=6 W=0.471 <===                          98
      CALL AGSRESET                                                       99
      AG_A = 12                                                           99
      AG_Z = 6                                                            99
      AG_W = 0.471                                                        99
      AG_TITLE = 'C'                                                      99
      AG_EXNAME = 'COMPONENT'                                             99
      AG_PARLIST = 'A_Z_W'                                                99
      CALL AXCOMPONENT                                                    99
C *                                                                       99
C Geant COMPONENT : H ===> A=1 Z=1 W=0.011 <===                           99
      CALL AGSRESET                                                       100
      AG_A = 1                                                            100
      AG_Z = 1                                                            100
      AG_W = 0.011                                                        100
      AG_TITLE = 'H'                                                      100
      AG_EXNAME = 'COMPONENT'                                             100
      AG_PARLIST = 'A_Z_W'                                                100
      CALL AXCOMPONENT                                                    100
C *                                                                       100
C Geant COMPONENT : CL ===> A=35.5 Z=17 W=0.014 <===                      100
      CALL AGSRESET                                                       101
      AG_A = 35.5                                                         101
      AG_Z = 17                                                           101
      AG_W = 0.014                                                        101
      AG_TITLE = 'CL'                                                     101
      AG_EXNAME = 'COMPONENT'                                             101
      AG_PARLIST = 'A_Z_W'                                                101
      CALL AXCOMPONENT                                                    101
C *                                                                       101
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.472 <===                        101
      CALL AGSRESET                                                       102
      AG_A = 27                                                           102
      AG_Z = 13                                                           102
      AG_W = 0.472                                                        102
      AG_TITLE = 'AL'                                                     102
      AG_EXNAME = 'COMPONENT'                                             102
      AG_PARLIST = 'A_Z_W'                                                102
      CALL AXCOMPONENT                                                    102
C *                                                                       102
C Geant MIXTURE : RAILMIX ===> DENS=3.384 <===                            102
      CALL AGSRESET                                                       103
      AG_DENS = 3.384                                                     103
      AG_TITLE = 'RAILMIX'                                                103
      AG_EXNAME = 'MIXTURE'                                               103
      AG_PARLIST = 'DENS'                                                 103
      CALL AXMIXTURE                                                      103
C *                                                                       103
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.131 <===                     103
      CALL AGSRESET                                                       104
      AG_A = 28.08                                                        104
      AG_Z = 14                                                           104
      AG_W = 0.131                                                        104
      AG_TITLE = 'SI'                                                     104
      AG_EXNAME = 'COMPONENT'                                             104
      AG_PARLIST = 'A_Z_W'                                                104
      CALL AXCOMPONENT                                                    104
C *                                                                       104
C Geant COMPONENT : O ===> A=16 Z=8 W=0.117 <===                          104
      CALL AGSRESET                                                       105
      AG_A = 16                                                           105
      AG_Z = 8                                                            105
      AG_W = 0.117                                                        105
      AG_TITLE = 'O'                                                      105
      AG_EXNAME = 'COMPONENT'                                             105
      AG_PARLIST = 'A_Z_W'                                                105
      CALL AXCOMPONENT                                                    105
C *                                                                       105
C Geant COMPONENT : C ===> A=12 Z=6 W=0.193 <===                          105
      CALL AGSRESET                                                       106
      AG_A = 12                                                           106
      AG_Z = 6                                                            106
      AG_W = 0.193                                                        106
      AG_TITLE = 'C'                                                      106
      AG_EXNAME = 'COMPONENT'                                             106
      AG_PARLIST = 'A_Z_W'                                                106
      CALL AXCOMPONENT                                                    106
C *                                                                       106
C Geant COMPONENT : H ===> A=1 Z=1 W=0.024 <===                           106
      CALL AGSRESET                                                       107
      AG_A = 1                                                            107
      AG_Z = 1                                                            107
      AG_W = 0.024                                                        107
      AG_TITLE = 'H'                                                      107
      AG_EXNAME = 'COMPONENT'                                             107
      AG_PARLIST = 'A_Z_W'                                                107
      CALL AXCOMPONENT                                                    107
C *                                                                       107
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.143 <===                        107
      CALL AGSRESET                                                       108
      AG_A = 27                                                           108
      AG_Z = 13                                                           108
      AG_W = 0.143                                                        108
      AG_TITLE = 'AL'                                                     108
      AG_EXNAME = 'COMPONENT'                                             108
      AG_PARLIST = 'A_Z_W'                                                108
      CALL AXCOMPONENT                                                    108
C *                                                                       108
C Geant COMPONENT : CU ===> A=63.5 Z=29 W=0.106 <===                      108
      CALL AGSRESET                                                       109
      AG_A = 63.5                                                         109
      AG_Z = 29                                                           109
      AG_W = 0.106                                                        109
      AG_TITLE = 'CU'                                                     109
      AG_EXNAME = 'COMPONENT'                                             109
      AG_PARLIST = 'A_Z_W'                                                109
      CALL AXCOMPONENT                                                    109
C *                                                                       109
C Geant COMPONENT : F ===> A=19. Z=9 W=0.254 <===                         109
      CALL AGSRESET                                                       110
      AG_A = 19.                                                          110
      AG_Z = 9                                                            110
      AG_W = 0.254                                                        110
      AG_TITLE = 'F'                                                      110
      AG_EXNAME = 'COMPONENT'                                             110
      AG_PARLIST = 'A_Z_W'                                                110
      CALL AXCOMPONENT                                                    110
C *                                                                       110
C Geant COMPONENT : NA ===> A=23. Z=11 W=0.015 <===                       110
      CALL AGSRESET                                                       111
      AG_A = 23.                                                          111
      AG_Z = 11                                                           111
      AG_W = 0.015                                                        111
      AG_TITLE = 'NA'                                                     111
      AG_EXNAME = 'COMPONENT'                                             111
      AG_PARLIST = 'A_Z_W'                                                111
      CALL AXCOMPONENT                                                    111
C *                                                                       111
C Geant COMPONENT : CA ===> A=40.1 Z=20 W=0.017 <===                      111
      CALL AGSRESET                                                       112
      AG_A = 40.1                                                         112
      AG_Z = 20                                                           112
      AG_W = 0.017                                                        112
      AG_TITLE = 'CA'                                                     112
      AG_EXNAME = 'COMPONENT'                                             112
      AG_PARLIST = 'A_Z_W'                                                112
      CALL AXCOMPONENT                                                    112
C *                                                                       112
C Geant MIXTURE : CABLEMIX ===> DENS=1.755 <===                           112
      CALL AGSRESET                                                       113
      AG_DENS = 1.755                                                     113
      AG_TITLE = 'CABLEMIX'                                               113
      AG_EXNAME = 'MIXTURE'                                               113
      AG_PARLIST = 'DENS'                                                 113
      CALL AXMIXTURE                                                      113
C *                                                                       113
C Geant COMPONENT : AL ===> A=27 Z=13 W=2. <===                           113
      CALL AGSRESET                                                       114
      AG_A = 27                                                           114
      AG_Z = 13                                                           114
      AG_W = 2.                                                           114
      AG_TITLE = 'AL'                                                     114
      AG_EXNAME = 'COMPONENT'                                             114
      AG_PARLIST = 'A_Z_W'                                                114
      CALL AXCOMPONENT                                                    114
C *                                                                       114
C Geant COMPONENT : O ===> A=16 Z=8 W=3. <===                             114
      CALL AGSRESET                                                       115
      AG_A = 16                                                           115
      AG_Z = 8                                                            115
      AG_W = 3.                                                           115
      AG_TITLE = 'O'                                                      115
      AG_EXNAME = 'COMPONENT'                                             115
      AG_PARLIST = 'A_Z_W'                                                115
      CALL AXCOMPONENT                                                    115
C *                                                                       115
C Geant MIXTURE : ALUMINA ===> DENS=3.90 <===                             115
      CALL AGSRESET                                                       116
      AG_DENS = 3.90                                                      116
      AG_TITLE = 'ALUMINA'                                                116
      AG_EXNAME = 'MIXTURE'                                               116
      AG_PARLIST = 'DENS'                                                 116
      CALL AXMIXTURE                                                      116
C *                                                                       116
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.281 <===                     116
      CALL AGSRESET                                                       117
      AG_A = 28.08                                                        117
      AG_Z = 14                                                           117
      AG_W = 0.281                                                        117
      AG_TITLE = 'SI'                                                     117
      AG_EXNAME = 'COMPONENT'                                             117
      AG_PARLIST = 'A_Z_W'                                                117
      CALL AXCOMPONENT                                                    117
C *                                                                       117
C Geant COMPONENT : O ===> A=16 Z=8 W=0.467 <===                          117
      CALL AGSRESET                                                       118
      AG_A = 16                                                           118
      AG_Z = 8                                                            118
      AG_W = 0.467                                                        118
      AG_TITLE = 'O'                                                      118
      AG_EXNAME = 'COMPONENT'                                             118
      AG_PARLIST = 'A_Z_W'                                                118
      CALL AXCOMPONENT                                                    118
C *                                                                       118
C Geant COMPONENT : C ===> A=12 Z=6 W=0.220 <===                          118
      CALL AGSRESET                                                       119
      AG_A = 12                                                           119
      AG_Z = 6                                                            119
      AG_W = 0.220                                                        119
      AG_TITLE = 'C'                                                      119
      AG_EXNAME = 'COMPONENT'                                             119
      AG_PARLIST = 'A_Z_W'                                                119
      CALL AXCOMPONENT                                                    119
C *                                                                       119
C Geant COMPONENT : H ===> A=1 Z=1 W=0.032 <===                           119
      CALL AGSRESET                                                       120
      AG_A = 1                                                            120
      AG_Z = 1                                                            120
      AG_W = 0.032                                                        120
      AG_TITLE = 'H'                                                      120
      AG_EXNAME = 'COMPONENT'                                             120
      AG_PARLIST = 'A_Z_W'                                                120
      CALL AXCOMPONENT                                                    120
C *                                                                       120
C Geant MIXTURE : FR4 ===> DENS=1.80 <===                                 120
      CALL AGSRESET                                                       121
      AG_DENS = 1.80                                                      121
      AG_TITLE = 'FR4'                                                    121
      AG_EXNAME = 'MIXTURE'                                               121
      AG_PARLIST = 'DENS'                                                 121
      CALL AXMIXTURE                                                      121
C *                                                                       121
C use default IDSC bank                                                   121
      BPATH_IDSC='IDSC*'                                                  122
      IDNUM_IDSC(1)=0                                                     122
      CALL AGDGETP('IDSMGEO2','*',-999,'IDSC',' ',BANK_IDSC, BPATH_IDSC,  122
     *IDNUM_IDSC,1,MAP_IDSC,NAMES_IDSC,BANK_IDSC,LEN_IDSC,LLEN_IDSC,      122
     *LINK_IDSC,FLAG_IDSC)                                                122
C use IDSG bank                                                           123
      BPATH_IDSG='IDSG*'                                                  123
      IDNUM_IDSG(1)=0                                                     123
      IDSG_VERSION=IDSC_VERSION                                           123
      CALL AGDGETP('IDSMGEO2','*',-999,'IDSG',NIDSG_VERSION,              123
     *RE_IDSG_VERSION,BPATH_IDSG,IDNUM_IDSG,1,MAP_IDSG,NAMES_IDSG,        123
     *BANK_IDSG,LEN_IDSG,LLEN_IDSG,LINK_IDSG,FLAG_IDSG)                   123
C use IDSA bank                                                           124
      BPATH_IDSA='IDSA*'                                                  124
      IDNUM_IDSA(1)=0                                                     124
      IDSA_VERSION=1.0                                                    124
      CALL AGDGETP('IDSMGEO2','*',-999,'IDSA',NIDSA_VERSION,              124
     *RE_IDSA_VERSION,BPATH_IDSA,IDNUM_IDSA,1,MAP_IDSA,NAMES_IDSA,        124
     *BANK_IDSA,LEN_IDSA,LLEN_IDSA,LINK_IDSA,FLAG_IDSA)                   124
      WRITE(LOUT,5010)IDSC_VERSION                                        124
5010  FORMAT('Inner Detector Support Module with IDSC_version = ',        124
     *F5.2)                                                               124
      INR = IDSG_RF                                                       126
      OUTR = IDSG_RRRES + IDSG_R2RES                                      127
      LENGTHZ = 470.                                                      128
      EASTZ = -189*INCH                                                   129
      WESTZ = 403.8                                                       130
      EASTB = -360.0                                                      131
      WESTB = 360.0                                                       132
      SINA = SIN( IDSG_ANGFLAT * DEGRAD )                                 133
      COSA = COS( IDSG_ANGFLAT * DEGRAD )                                 134
      RRAIL=41.5                                                          135
      DPHIHV=0.03                                                         136
      DZC=7.4/2.0                                                         137
      CALL AGSBEGM(MODULE,IPRIN)                                          138
C Check IPRIN<0                                                           138
      IF (IPRIN.LT.0) RETURN                                              138
C Create IDSM :                                                           138
C Call To Local Procedure IDSM                                            138
      ASSIGN 5030 TO BLOCK_IDSM                                           138
      GO TO  5020                                                         138
5030  CONTINUE                                                            138
C *                                                                       138
C Geant POSITION : IDSM ===> MOTHER='CAVE' X=IDSA_X Y=IDSA_Y Z=IDSA_Z TH  141
C ETAX=IDSA_THETAX PHIX=IDSA_PHIX THETAY=IDSA_THETAY PHIY=IDSA_PHIY THET  141
C AZ=IDSA_THETAZ PHIZ=IDSA_PHIZ KONLY='MANY' <===                         141
      CALL AGSRESET                                                       141
      AG_MOTHER = 'CAVE'                                                  141
      AG_X = IDSA_X                                                       141
      AG_Y = IDSA_Y                                                       141
      AG_Z = IDSA_Z                                                       141
      AG_THETAX = IDSA_THETAX                                             141
      AG_PHIX = IDSA_PHIX                                                 141
      AG_THETAY = IDSA_THETAY                                             141
      AG_PHIY = IDSA_PHIY                                                 141
      AG_THETAZ = IDSA_THETAZ                                             141
      AG_PHIZ = IDSA_PHIZ                                                 141
      AG_KONLY = 'MANY'                                                   141
      AG_TITLE = 'IDSM'                                                   141
      AG_EXNAME = 'POSITION'                                              141
      AG_PARLIST = 'MOTHER_X_Y_Z_THETAX_PHIX_THETAY_PHIY_THETAZ_PHIZ_KON  141
     *LY'                                                                 141
      CALL AXPOSITION                                                     141
C *                                                                       141
C ----------------------------------------------------------------------  141
C ------------ IDSM                                                       141
      CALL AGSENDM                                                        143
      RETURN                                                              143
C Begin Local Procedure IDSM                                              143
C IDSM MOTHER VOLUME FOR BEAM SUPPORT CONE                                143
5020  CONTINUE                                                            143
         AG_TITLE='IDSM'                                                  143
         AG_MODULE=MODULE                                                 143
         AG_MARK='V00'                                                    143
         CALL AGSPUSH                                                     143
         AG_MODULE=MODULE                                                 143
C    *                                                                    143
C    Geant ATTRIBUTE : IDSM ===> SEEN=0 COLO=4 <===                       144
         CALL AGSRESET                                                    144
         AG_SEEN = 0                                                      144
         AG_COLO = 4                                                      144
         AG_TITLE = 'IDSM'                                                144
         AG_EXNAME = 'ATTRIBUTE'                                          144
         AG_PARLIST = 'SEEN_COLO'                                         144
         CALL AXATTRIBUTE                                                 144
C    *                                                                    144
C    Geant MATERIAL : AIR ===> inherited arguments used <===              144
         CALL AGSRESET                                                    145
         AG_TITLE = 'AIR'                                                 145
         AG_EXNAME = 'MATERIAL'                                           145
         AG_PARLIST = 'NONE'                                              145
         CALL AXMATERIAL                                                  145
C    *                                                                    145
C    Check IDSC_VERSION.EQ.1                                              145
         IF (IDSC_VERSION.EQ.1) THEN                                      146
C    Geant SHAPE : TUBE ===> RMIN=INR RMAX=OUTR DZ=LENGTHZ/2. <===        147
         CALL AGSRESET                                                    147
         AG_RMIN = INR                                                    147
         AG_RMAX = OUTR                                                   147
         AG_DZ = LENGTHZ/2.                                               147
         AG_TITLE = 'TUBE'                                                147
         AG_EXNAME = 'SHAPE'                                              147
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      147
         CALL AXSHAPE                                                     147
C    *                                                                    147
         ENDIF                                                            147
C    Check IDSC_VERSION>1                                                 149
         IF (IDSC_VERSION.GT.1) THEN                                      149
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=10 ZI={EASTZ, -398,-398,  154
C    EASTB,EASTB,WESTB,WESTB,398,398, WESTZ} RMN={0,0,0,0,0,0,0,0,0,0} R  154
C    MX={1.5*INCH+0.8125*INCH, 1.5*INCH+0.8125*INCH,5.36,5.36, OUTR,OUTR  154
C    ,5.36,5.36,1.5*INCH+0.8125*INCH,1.5*INCH+0.8125*INCH} <===           154
         CALL AGSRESET                                                    154
         AG_PHI1 = 0                                                      154
         AG_DPHI = 360                                                    154
         AG_NZ = 10                                                       154
         AG_ZI(1)=EASTZ                                                   154
         AG_ZI(2)=-398                                                    154
         AG_ZI(3)=-398                                                    154
         AG_ZI(4)=EASTB                                                   154
         AG_ZI(5)=EASTB                                                   154
         AG_ZI(6)=WESTB                                                   154
         AG_ZI(7)=WESTB                                                   154
         AG_ZI(8)=398                                                     154
         AG_ZI(9)=398                                                     154
         AG_ZI(10)=WESTZ                                                  154
         AG_RMN(1)=0                                                      154
         AG_RMN(2)=0                                                      154
         AG_RMN(3)=0                                                      154
         AG_RMN(4)=0                                                      154
         AG_RMN(5)=0                                                      154
         AG_RMN(6)=0                                                      154
         AG_RMN(7)=0                                                      154
         AG_RMN(8)=0                                                      154
         AG_RMN(9)=0                                                      154
         AG_RMN(10)=0                                                     154
         AG_RMX(1)=1.5*INCH+0.8125*INCH                                   154
         AG_RMX(2)=1.5*INCH+0.8125*INCH                                   154
         AG_RMX(3)=5.36                                                   154
         AG_RMX(4)=5.36                                                   154
         AG_RMX(5)=OUTR                                                   154
         AG_RMX(6)=OUTR                                                   154
         AG_RMX(7)=5.36                                                   154
         AG_RMX(8)=5.36                                                   154
         AG_RMX(9)=1.5*INCH+0.8125*INCH                                   154
         AG_RMX(10)=1.5*INCH+0.8125*INCH                                  154
         AG_TITLE = 'PCON'                                                154
         AG_EXNAME = 'SHAPE'                                              154
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           154
         CALL AXSHAPE                                                     154
C    *                                                                    154
         ENDIF                                                            154
C    Create OSCA :                                                        156
C    Call To Local Procedure OSCA                                         156
         ASSIGN 5050 TO BLOCK_OSCA                                        156
         GO TO  5040                                                      156
5050     CONTINUE                                                         156
C    *                                                                    156
C    Geant POSITION : OSCA ===> Z=0 <===                                  157
         CALL AGSRESET                                                    157
         AG_Z = 0                                                         157
         AG_TITLE = 'OSCA'                                                157
         AG_EXNAME = 'POSITION'                                           157
         AG_PARLIST = 'Z'                                                 157
         CALL AXPOSITION                                                  157
C    *                                                                    157
C    *                                                                    157
         K=0                                                              158
            GO TO 5063                                                    158
5061        K=K+1                                                         158
5063        IF(K-(1).GT.0) GO TO 5062                                     158
            PM=1.-2*K                                                     159
C       Create SUCB :                                                     160
C       Call To Local Procedure SUCB                                      160
            ASSIGN 5080 TO BLOCK_SUCB                                     160
            GO TO  5070                                                   160
5080        CONTINUE                                                      160
C       *                                                                 160
C       Geant POSITION : SUCB ===> Z=PM*55.75 <===                        161
            CALL AGSRESET                                                 161
            AG_Z = PM*55.75                                               161
            AG_TITLE = 'SUCB'                                             161
            AG_EXNAME = 'POSITION'                                        161
            AG_PARLIST = 'Z'                                              161
            CALL AXPOSITION                                               161
C       *                                                                 161
C       Create SUCC :                                                     162
C       Call To Local Procedure SUCC                                      162
            ASSIGN 5100 TO BLOCK_SUCC                                     162
            GO TO  5090                                                   162
5100        CONTINUE                                                      162
C       *                                                                 162
C       *                                                                 163
C          Check K==0                                                     163
               IF (K.EQ.0) THEN                                           163
C          Geant POSITION : SUCC ===> Z=-59.75 <===                       164
               CALL AGSRESET                                              164
               AG_Z = -59.75                                              164
               AG_TITLE = 'SUCC'                                          164
               AG_EXNAME = 'POSITION'                                     164
               AG_PARLIST = 'Z'                                           164
               CALL AXPOSITION                                            164
C          *                                                              164
            END IF                                                        165
C       *                                                                 166
C          Check K==1                                                     166
               IF (K.EQ.1) THEN                                           166
C          Geant POSITION : SUCC ===> Z=+59.75 ALPHAX=180. <===           167
               CALL AGSRESET                                              167
               AG_Z = +59.75                                              167
               AG_ALPHAN=AG_ALPHAN+1                                      167
               AG_ALPHAK(AG_ALPHAN)=1                                     167
               AG_ALPHAL(AG_ALPHAN)=180.                                  167
               AG_TITLE = 'SUCC'                                          167
               AG_EXNAME = 'POSITION'                                     167
               AG_PARLIST = 'Z_ALPHAX'                                    167
               CALL AXPOSITION                                            167
C          *                                                              167
            END IF                                                        168
C       Create SUCD :                                                     169
C       Call To Local Procedure SUCD                                      169
            ASSIGN 5120 TO BLOCK_SUCD                                     169
            GO TO  5110                                                   169
5120        CONTINUE                                                      169
C       *                                                                 169
C       Geant POSITION : SUCD ===> Z=PM*63.81 <===                        170
            CALL AGSRESET                                                 170
            AG_Z = PM*63.81                                               170
            AG_TITLE = 'SUCD'                                             170
            AG_EXNAME = 'POSITION'                                        170
            AG_PARLIST = 'Z'                                              170
            CALL AXPOSITION                                               170
C       *                                                                 170
C       Create SUCE :                                                     171
C       Call To Local Procedure SUCE                                      171
            ASSIGN 5140 TO BLOCK_SUCE                                     171
            GO TO  5130                                                   171
5140        CONTINUE                                                      171
C       *                                                                 171
C       Geant POSITION : SUCE ===> Z=PM*144.52 <===                       172
            CALL AGSRESET                                                 172
            AG_Z = PM*144.52                                              172
            AG_TITLE = 'SUCE'                                             172
            AG_EXNAME = 'POSITION'                                        172
            AG_PARLIST = 'Z'                                              172
            CALL AXPOSITION                                               172
C       *                                                                 172
C       Create SUCF :                                                     173
C       Call To Local Procedure SUCF                                      173
            ASSIGN 5160 TO BLOCK_SUCF                                     173
            GO TO  5150                                                   173
5160        CONTINUE                                                      173
C       *                                                                 173
C       Geant POSITION : SUCF ===> Z=PM*224.52 <===                       174
            CALL AGSRESET                                                 174
            AG_Z = PM*224.52                                              174
            AG_TITLE = 'SUCF'                                             174
            AG_EXNAME = 'POSITION'                                        174
            AG_PARLIST = 'Z'                                              174
            CALL AXPOSITION                                               174
C       *                                                                 174
C       Create SUCG :                                                     175
C       Call To Local Procedure SUCG                                      175
            ASSIGN 5180 TO BLOCK_SUCG                                     175
            GO TO  5170                                                   175
5180        CONTINUE                                                      175
C       *                                                                 175
C       Geant POSITION : SUCG ===> Z=PM*225.4 <===                        176
            CALL AGSRESET                                                 176
            AG_Z = PM*225.4                                               176
            AG_TITLE = 'SUCG'                                             176
            AG_EXNAME = 'POSITION'                                        176
            AG_PARLIST = 'Z'                                              176
            CALL AXPOSITION                                               176
C       *                                                                 176
C       Create EFSA :                                                     177
C       Call To Local Procedure EFSA                                      177
            ASSIGN 5200 TO BLOCK_EFSA                                     177
            GO TO  5190                                                   177
5200        CONTINUE                                                      177
C       *                                                                 177
C       *                                                                 178
C          Check K==0                                                     178
               IF (K.EQ.0) THEN                                           178
C          Geant POSITION : EFSA ===> Z=-63.41-0.8 <===                   179
               CALL AGSRESET                                              179
               AG_Z = -63.41-0.8                                          179
               AG_TITLE = 'EFSA'                                          179
               AG_EXNAME = 'POSITION'                                     179
               AG_PARLIST = 'Z'                                           179
               CALL AXPOSITION                                            179
C          *                                                              179
            END IF                                                        180
C       *                                                                 181
C          Check K==1                                                     181
               IF (K.EQ.1) THEN                                           181
C          Geant POSITION : EFSA ===> Z=63.41+0.8 ALPHAX=180. <===        182
               CALL AGSRESET                                              182
               AG_Z = 63.41+0.8                                           182
               AG_ALPHAN=AG_ALPHAN+1                                      182
               AG_ALPHAK(AG_ALPHAN)=1                                     182
               AG_ALPHAL(AG_ALPHAN)=180.                                  182
               AG_TITLE = 'EFSA'                                          182
               AG_EXNAME = 'POSITION'                                     182
               AG_PARLIST = 'Z_ALPHAX'                                    182
               CALL AXPOSITION                                            182
C          *                                                              182
            END IF                                                        183
         GO TO 5061                                                       184
5062     CONTINUE                                                         184
C    *                                                                    185
         M=0                                                              185
            GO TO 5213                                                    185
5211        M=M+1                                                         185
5213        IF(M-(1).GT.0) GO TO 5212                                     185
            ANGRES = IDSG_ANGFLAT - IDSG_DANGRES/2.                       186
C       Check M.EQ.1                                                      187
            IF (M.EQ.1) THEN                                              187
            ANGRES = IDSG_ANGFLAT+IDSG_DANGRES/2.                         188
            ENDIF                                                         189
C       Create TPRT :                                                     190
C       Call To Local Procedure TPRT                                      190
            ASSIGN 5230 TO BLOCK_TPRT                                     190
            GO TO  5220                                                   190
5230        CONTINUE                                                      190
C       *                                                                 190
C       Geant POSITION : TPRT ===> X=IDSG_RRRES*COS(ANGRES/180.*PI) Y=ID  192
C       SG_RRRES*SIN(ANGRES/180.*PI) Z=0 <===                             192
            CALL AGSRESET                                                 192
            AG_X = IDSG_RRRES*COS(ANGRES/180.*PI)                         192
            AG_Y = IDSG_RRRES*SIN(ANGRES/180.*PI)                         192
            AG_Z = 0                                                      192
            AG_TITLE = 'TPRT'                                             192
            AG_EXNAME = 'POSITION'                                        192
            AG_PARLIST = 'X_Y_Z'                                          192
            CALL AXPOSITION                                               192
C       *                                                                 192
         GO TO 5211                                                       193
5212     CONTINUE                                                         193
C    Create TPRR :                                                        194
C    Call To Local Procedure TPRR                                         194
         ASSIGN 5250 TO BLOCK_TPRR                                        194
         GO TO  5240                                                      194
5250     CONTINUE                                                         194
C    *                                                                    194
C    Geant POSITION : TPRR ===> X=IDSG_RRRES*COSA Y=IDSG_RRRES*SINA Z=0   195
C    ALPHAZ=IDSG_ANGFLAT <===                                             195
         CALL AGSRESET                                                    195
         AG_X = IDSG_RRRES*COSA                                           195
         AG_Y = IDSG_RRRES*SINA                                           195
         AG_Z = 0                                                         195
         AG_ALPHAN=AG_ALPHAN+1                                            195
         AG_ALPHAK(AG_ALPHAN)=3                                           195
         AG_ALPHAL(AG_ALPHAN)=IDSG_ANGFLAT                                195
         AG_TITLE = 'TPRR'                                                195
         AG_EXNAME = 'POSITION'                                           195
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      195
         CALL AXPOSITION                                                  195
C    *                                                                    195
C    *                                                                    195
         M=0                                                              196
            GO TO 5263                                                    196
5261        M=M+1                                                         196
5263        IF(M-(1).GT.0) GO TO 5262                                     196
            ANGRAIL= 16./180. * PI + M * PI                               197
C       Create FGRL :                                                     198
C       Call To Local Procedure FGRL                                      198
            ASSIGN 5280 TO BLOCK_FGRL                                     198
            GO TO  5270                                                   198
5280        CONTINUE                                                      198
C       *                                                                 198
C       Geant POSITION : FGRL ===> X=RRAIL*COS(ANGRAIL) Y=RRAIL*SIN(ANGR  199
C       AIL) Z=146.57 <===                                                199
            CALL AGSRESET                                                 199
            AG_X = RRAIL*COS(ANGRAIL)                                     199
            AG_Y = RRAIL*SIN(ANGRAIL)                                     199
            AG_Z = 146.57                                                 199
            AG_TITLE = 'FGRL'                                             199
            AG_EXNAME = 'POSITION'                                        199
            AG_PARLIST = 'X_Y_Z'                                          199
            CALL AXPOSITION                                               199
C       *                                                                 199
         GO TO 5261                                                       200
5262     CONTINUE                                                         200
C    Skip Unless (IDSG_VERSION <= 2)                                      201
         IF (IDSG_VERSION.GT.2)GO TO 5021                                 201
C    *                                                                    202
         M=1                                                              202
            GO TO 5293                                                    202
5291        M=M+1                                                         202
5293        IF(M-(4).GT.0) GO TO 5292                                     202
            ANGFGTCBL=-90./180. * PI                                      203
C       *                                                                 204
            K=1                                                           204
               GO TO 5303                                                 204
5301           K=K+1                                                      204
5303           IF(K-(IDSG_FGTNDISK).GT.0) GO TO 5302                      204
C          Skip Unless (K.EQ.1).OR.(M.LE.3)                               205
               IF (.NOT.((K.EQ.1).OR.(M.LE.3)))GO TO 5301                 205
               PM=1                                                       205
C          Check (K.EQ.1).OR.(K.EQ.3).OR.(K.EQ.5)                         207
               IF ((K.EQ.1).OR.(K.EQ.3).OR.(K.EQ.5)) THEN                 207
               PM=-1                                                      208
               ENDIF                                                      209
C          Create FGHV :                                                  210
C          Call To Local Procedure FGHV                                   210
               ASSIGN 5320 TO BLOCK_FGHV                                  210
               GO TO  5310                                                210
5320           CONTINUE                                                   210
C          *                                                              210
C          *                                                              211
               J=K                                                        211
                  GO TO 5333                                              211
5331              J=J+1                                                   211
5333              IF(J-(16).GT.0) GO TO 5332                              211
C             Geant POSITION : FGHV ===> MOTHER='IDSM' X=(RRAIL+0.5*PM)*  214
C             COS(ANGFGTCBL+(DPHIHV*(M+2.2*K-PM*.8))) Y=(RRAIL+0.5*PM)*S  214
C             IN(ANGFGTCBL+(DPHIHV*(M+2.2*K-PM*.8))) Z=IDSG_FGTSTARTZ+ID  214
C             SG_FGTDISKSTEPZ*(J-0.5) <===                                214
                  CALL AGSRESET                                           214
                  AG_MOTHER = 'IDSM'                                      214
                  AG_X =                                                  214
     *            (RRAIL+0.5*PM)*COS(ANGFGTCBL+(DPHIHV*(M+2.2*K-PM*.8)))  214
                  AG_Y =                                                  214
     *            (RRAIL+0.5*PM)*SIN(ANGFGTCBL+(DPHIHV*(M+2.2*K-PM*.8)))  214
                  AG_Z = IDSG_FGTSTARTZ+IDSG_FGTDISKSTEPZ*(J-0.5)         214
                  AG_TITLE = 'FGHV'                                       214
                  AG_EXNAME = 'POSITION'                                  214
                  AG_PARLIST = 'MOTHER_X_Y_Z'                             214
                  CALL AXPOSITION                                         214
C             *                                                           214
               GO TO 5331                                                 215
5332           CONTINUE                                                   215
            GO TO 5301                                                    216
5302        CONTINUE                                                      216
         GO TO 5291                                                       217
5292     CONTINUE                                                         217
5021  CALL AGSPOPB                                                        218
      GO TO BLOCK_IDSM, (5020 ,5030)                                      218
C End of Local Procedure                                                  218
C ----------------------------------------------------------------------  219
C ------------ OSCA                                                       219
C Begin Local Procedure OSCA                                              220
C OSCA CENTRAL CFIBER TUBE (OSC)                                          220
5040  CONTINUE                                                            220
         AG_TITLE='OSCA'                                                  220
         AG_MODULE=MODULE                                                 220
         AG_MARK='V00'                                                    220
         CALL AGSPUSH                                                     220
         AG_MODULE=MODULE                                                 220
C    *                                                                    220
C    Geant ATTRIBUTE : OSCA ===> SEEN=1 COLO=6 <===                       221
         CALL AGSRESET                                                    221
         AG_SEEN = 1                                                      221
         AG_COLO = 6                                                      221
         AG_TITLE = 'OSCA'                                                221
         AG_EXNAME = 'ATTRIBUTE'                                          221
         AG_PARLIST = 'SEEN_COLO'                                         221
         CALL AXATTRIBUTE                                                 221
C    *                                                                    221
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          221
         CALL AGSRESET                                                    222
         AG_TITLE = 'CFRPMIX'                                             222
         AG_EXNAME = 'MATERIAL'                                           222
         AG_PARLIST = 'NONE'                                              222
         CALL AXMATERIAL                                                  222
C    *                                                                    222
C    Geant SHAPE : TUBE ===> RMIN=21.5 RMAX=21.6 DZ=112./2. <===          222
         CALL AGSRESET                                                    223
         AG_RMIN = 21.5                                                   223
         AG_RMAX = 21.6                                                   223
         AG_DZ = 112./2.                                                  223
         AG_TITLE = 'TUBE'                                                223
         AG_EXNAME = 'SHAPE'                                              223
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      223
         CALL AXSHAPE                                                     223
C    *                                                                    223
5041  CALL AGSPOPB                                                        224
      GO TO BLOCK_OSCA, (5040 ,5050)                                      224
C End of Local Procedure                                                  224
C ----------------------------------------------------------------------  225
C ------------ SUCB                                                       225
C Begin Local Procedure SUCB                                              226
C SUCB SMALL ALU RING AT CENTRAL TUBE                                     226
5070  CONTINUE                                                            226
         AG_TITLE='SUCB'                                                  226
         AG_MODULE=MODULE                                                 226
         AG_MARK='V00'                                                    226
         CALL AGSPUSH                                                     226
         AG_MODULE=MODULE                                                 226
C    *                                                                    226
C    Geant ATTRIBUTE : SUCB ===> SEEN=1 COLO=1 <===                       227
         CALL AGSRESET                                                    227
         AG_SEEN = 1                                                      227
         AG_COLO = 1                                                      227
         AG_TITLE = 'SUCB'                                                227
         AG_EXNAME = 'ATTRIBUTE'                                          227
         AG_PARLIST = 'SEEN_COLO'                                         227
         CALL AXATTRIBUTE                                                 227
C    *                                                                    227
C    Geant MATERIAL : SUCBMIX ===> inherited arguments used <===          227
         CALL AGSRESET                                                    228
         AG_TITLE = 'SUCBMIX'                                             228
         AG_EXNAME = 'MATERIAL'                                           228
         AG_PARLIST = 'NONE'                                              228
         CALL AXMATERIAL                                                  228
C    *                                                                    228
C    Geant SHAPE : TUBE ===> RMIN=21.6 RMAX=22.4 DZ=0.47/2.0 <===         228
         CALL AGSRESET                                                    229
         AG_RMIN = 21.6                                                   229
         AG_RMAX = 22.4                                                   229
         AG_DZ = 0.47/2.0                                                 229
         AG_TITLE = 'TUBE'                                                229
         AG_EXNAME = 'SHAPE'                                              229
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      229
         CALL AXSHAPE                                                     229
C    *                                                                    229
5071  CALL AGSPOPB                                                        230
      GO TO BLOCK_SUCB, (5070 ,5080)                                      230
C End of Local Procedure                                                  230
C ----------------------------------------------------------------------  231
C ------------ SUCC                                                       231
C Begin Local Procedure SUCC                                              232
C SUCC CFIBER CONE SECTION                                                232
5090  CONTINUE                                                            232
         AG_TITLE='SUCC'                                                  232
         AG_MODULE=MODULE                                                 232
         AG_MARK='V00'                                                    232
         CALL AGSPUSH                                                     232
         AG_MODULE=MODULE                                                 232
C    *                                                                    232
C    Geant ATTRIBUTE : SUCC ===> SEEN=1 COLO=6 <===                       233
         CALL AGSRESET                                                    233
         AG_SEEN = 1                                                      233
         AG_COLO = 6                                                      233
         AG_TITLE = 'SUCC'                                                233
         AG_EXNAME = 'ATTRIBUTE'                                          233
         AG_PARLIST = 'SEEN_COLO'                                         233
         CALL AXATTRIBUTE                                                 233
C    *                                                                    233
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          233
         CALL AGSRESET                                                    234
         AG_TITLE = 'CFRPMIX'                                             234
         AG_EXNAME = 'MATERIAL'                                           234
         AG_PARLIST = 'NONE'                                              234
         CALL AXMATERIAL                                                  234
C    *                                                                    234
C    Geant SHAPE : PCON ===> PHI1=0.0 DPHI=360 NZ=6 ZI={-DZC, -DZC+0.6,-  234
C    DZC+0.6,DZC-0.6,DZC-0.6,DZC} RMN={37.61, 36.30,36.30,22.76,21.6,21.  234
C    6} RMX={39.9, 39.9,37.6,24.06,24.06,22.75} <===                      234
         CALL AGSRESET                                                    238
         AG_PHI1 = 0.0                                                    238
         AG_DPHI = 360                                                    238
         AG_NZ = 6                                                        238
         AG_ZI(1)=-DZC                                                    238
         AG_ZI(2)=-DZC+0.6                                                238
         AG_ZI(3)=-DZC+0.6                                                238
         AG_ZI(4)=DZC-0.6                                                 238
         AG_ZI(5)=DZC-0.6                                                 238
         AG_ZI(6)=DZC                                                     238
         AG_RMN(1)=37.61                                                  238
         AG_RMN(2)=36.30                                                  238
         AG_RMN(3)=36.30                                                  238
         AG_RMN(4)=22.76                                                  238
         AG_RMN(5)=21.6                                                   238
         AG_RMN(6)=21.6                                                   238
         AG_RMX(1)=39.9                                                   238
         AG_RMX(2)=39.9                                                   238
         AG_RMX(3)=37.6                                                   238
         AG_RMX(4)=24.06                                                  238
         AG_RMX(5)=24.06                                                  238
         AG_RMX(6)=22.75                                                  238
         AG_TITLE = 'PCON'                                                238
         AG_EXNAME = 'SHAPE'                                              238
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           238
         CALL AXSHAPE                                                     238
C    *                                                                    238
5091  CALL AGSPOPB                                                        239
      GO TO BLOCK_SUCC, (5090 ,5100)                                      239
C End of Local Procedure                                                  239
C ----------------------------------------------------------------------  240
C ------------ SUCD                                                       240
C Begin Local Procedure SUCD                                              241
C SUCD LARGE AL RING AT CONE                                              241
5110  CONTINUE                                                            241
         AG_TITLE='SUCD'                                                  241
         AG_MODULE=MODULE                                                 241
         AG_MARK='V00'                                                    241
         CALL AGSPUSH                                                     241
         AG_MODULE=MODULE                                                 241
C    *                                                                    241
C    Geant ATTRIBUTE : SUCD ===> SEEN=1 COLO=1 <===                       242
         CALL AGSRESET                                                    242
         AG_SEEN = 1                                                      242
         AG_COLO = 1                                                      242
         AG_TITLE = 'SUCD'                                                242
         AG_EXNAME = 'ATTRIBUTE'                                          242
         AG_PARLIST = 'SEEN_COLO'                                         242
         CALL AXATTRIBUTE                                                 242
C    *                                                                    242
C    Geant MATERIAL : SUCDMIX ===> inherited arguments used <===          242
         CALL AGSRESET                                                    243
         AG_TITLE = 'SUCDMIX'                                             243
         AG_EXNAME = 'MATERIAL'                                           243
         AG_PARLIST = 'NONE'                                              243
         CALL AXMATERIAL                                                  243
C    *                                                                    243
C    Geant SHAPE : TUBE ===> RMIN=38.6 RMAX=IDSG_EWCSINNERRAD, DZ=0.62/2  243
C    . <===                                                               243
         CALL AGSRESET                                                    245
         AG_RMIN = 38.6                                                   245
         AG_RMAX = IDSG_EWCSINNERRAD                                      245
         AG_DZ = 0.62/2.                                                  245
         AG_TITLE = 'TUBE'                                                245
         AG_EXNAME = 'SHAPE'                                              245
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      245
         CALL AXSHAPE                                                     245
C    *                                                                    245
5111  CALL AGSPOPB                                                        246
      GO TO BLOCK_SUCD, (5110 ,5120)                                      246
C End of Local Procedure                                                  246
C ----------------------------------------------------------------------  247
C ------------ SUCE                                                       247
C Begin Local Procedure SUCE                                              248
C SUCE EAST OR WEST CFIBER TUBE                                           248
5130  CONTINUE                                                            248
         AG_TITLE='SUCE'                                                  248
         AG_MODULE=MODULE                                                 248
         AG_MARK='V00'                                                    248
         CALL AGSPUSH                                                     248
         AG_MODULE=MODULE                                                 248
C    *                                                                    248
C    Geant ATTRIBUTE : SUCE ===> SEEN=1 COLO=6 <===                       249
         CALL AGSRESET                                                    249
         AG_SEEN = 1                                                      249
         AG_COLO = 6                                                      249
         AG_TITLE = 'SUCE'                                                249
         AG_EXNAME = 'ATTRIBUTE'                                          249
         AG_PARLIST = 'SEEN_COLO'                                         249
         CALL AXATTRIBUTE                                                 249
C    *                                                                    249
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          249
         CALL AGSRESET                                                    250
         AG_TITLE = 'CFRPMIX'                                             250
         AG_EXNAME = 'MATERIAL'                                           250
         AG_PARLIST = 'NONE'                                              250
         CALL AXMATERIAL                                                  250
C    *                                                                    250
C    Geant SHAPE : TUBE ===> RMIN=IDSG_EWCSINNERRAD RMAX=IDSG_EWCSOUTERR  250
C    AD, DZ=161.6/2. <===                                                 250
         CALL AGSRESET                                                    252
         AG_RMIN = IDSG_EWCSINNERRAD                                      252
         AG_RMAX = IDSG_EWCSOUTERRAD                                      252
         AG_DZ = 161.6/2.                                                 252
         AG_TITLE = 'TUBE'                                                252
         AG_EXNAME = 'SHAPE'                                              252
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      252
         CALL AXSHAPE                                                     252
C    *                                                                    252
5131  CALL AGSPOPB                                                        253
      GO TO BLOCK_SUCE, (5130 ,5140)                                      253
C End of Local Procedure                                                  253
C ----------------------------------------------------------------------  254
C ------------ SUCF                                                       254
C Begin Local Procedure SUCF                                              255
C SUCF LARGE ALU RING AT THE END OF WEST CYLINDER                         255
5150  CONTINUE                                                            255
         AG_TITLE='SUCF'                                                  255
         AG_MODULE=MODULE                                                 255
         AG_MARK='V00'                                                    255
         CALL AGSPUSH                                                     255
         AG_MODULE=MODULE                                                 255
C    *                                                                    255
C    Geant ATTRIBUTE : SUCF ===> SEEN=1 COLO=1 <===                       256
         CALL AGSRESET                                                    256
         AG_SEEN = 1                                                      256
         AG_COLO = 1                                                      256
         AG_TITLE = 'SUCF'                                                256
         AG_EXNAME = 'ATTRIBUTE'                                          256
         AG_PARLIST = 'SEEN_COLO'                                         256
         CALL AXATTRIBUTE                                                 256
C    *                                                                    256
C    Geant MATERIAL : ALPURE ===> inherited arguments used <===           256
         CALL AGSRESET                                                    257
         AG_TITLE = 'ALPURE'                                              257
         AG_EXNAME = 'MATERIAL'                                           257
         AG_PARLIST = 'NONE'                                              257
         CALL AXMATERIAL                                                  257
C    *                                                                    257
C    Geant SHAPE : TUBE ===> RMIN=IDSG_EWCSOUTERRAD RMAX=40.5, DZ=1.6/2.  257
C     <===                                                                257
         CALL AGSRESET                                                    259
         AG_RMIN = IDSG_EWCSOUTERRAD                                      259
         AG_RMAX = 40.5                                                   259
         AG_DZ = 1.6/2.                                                   259
         AG_TITLE = 'TUBE'                                                259
         AG_EXNAME = 'SHAPE'                                              259
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      259
         CALL AXSHAPE                                                     259
C    *                                                                    259
5151  CALL AGSPOPB                                                        260
      GO TO BLOCK_SUCF, (5150 ,5160)                                      260
C End of Local Procedure                                                  260
C ----------------------------------------------------------------------  261
C ------------ SUCG                                                       261
C Begin Local Procedure SUCG                                              262
C SUCG LARGE ALU END DISK                                                 262
5170  CONTINUE                                                            262
         AG_TITLE='SUCG'                                                  262
         AG_MODULE=MODULE                                                 262
         AG_MARK='V00'                                                    262
         CALL AGSPUSH                                                     262
         AG_MODULE=MODULE                                                 262
C    *                                                                    262
C    Geant ATTRIBUTE : SUCG ===> SEEN=1 COLO=1 <===                       263
         CALL AGSRESET                                                    263
         AG_SEEN = 1                                                      263
         AG_COLO = 1                                                      263
         AG_TITLE = 'SUCG'                                                263
         AG_EXNAME = 'ATTRIBUTE'                                          263
         AG_PARLIST = 'SEEN_COLO'                                         263
         CALL AXATTRIBUTE                                                 263
C    *                                                                    263
C    Geant MATERIAL : ALPURE ===> inherited arguments used <===           263
         CALL AGSRESET                                                    264
         AG_TITLE = 'ALPURE'                                              264
         AG_EXNAME = 'MATERIAL'                                           264
         AG_PARLIST = 'NONE'                                              264
         CALL AXMATERIAL                                                  264
C    *                                                                    264
C    Check IDSC_VERSION .EQ. 1                                            264
         IF (IDSC_VERSION .EQ. 1) THEN                                    265
C    Geant SHAPE : TUBE ===> RMIN=10.3 RMAX=40.5 DZ=0.16/2. <===          266
         CALL AGSRESET                                                    266
         AG_RMIN = 10.3                                                   266
         AG_RMAX = 40.5                                                   266
         AG_DZ = 0.16/2.                                                  266
         AG_TITLE = 'TUBE'                                                266
         AG_EXNAME = 'SHAPE'                                              266
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      266
         CALL AXSHAPE                                                     266
C    *                                                                    266
         ELSE                                                             266
C    Geant SHAPE : TUBE ===> RMIN=21.62 RMAX=40.5 DZ=0.16/2. <===         268
         CALL AGSRESET                                                    268
         AG_RMIN = 21.62                                                  268
         AG_RMAX = 40.5                                                   268
         AG_DZ = 0.16/2.                                                  268
         AG_TITLE = 'TUBE'                                                268
         AG_EXNAME = 'SHAPE'                                              268
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      268
         CALL AXSHAPE                                                     268
C    *                                                                    268
         ENDIF                                                            268
5171  CALL AGSPOPB                                                        270
      GO TO BLOCK_SUCG, (5170 ,5180)                                      270
C End of Local Procedure                                                  270
C ----------------------------------------------------------------------  271
C ------------ EFSA                                                       271
C Begin Local Procedure EFSA                                              272
C EFSA ELECTROSTATIC SHROUD AT NEAR END OF ESC AND WSC                    272
5190  CONTINUE                                                            272
         AG_TITLE='EFSA'                                                  272
         AG_MODULE=MODULE                                                 272
         AG_MARK='V00'                                                    272
         CALL AGSPUSH                                                     272
         AG_MODULE=MODULE                                                 272
C    *                                                                    272
C    Geant ATTRIBUTE : EFSA ===> SEEN=1 COLO=1 <===                       273
         CALL AGSRESET                                                    273
         AG_SEEN = 1                                                      273
         AG_COLO = 1                                                      273
         AG_TITLE = 'EFSA'                                                273
         AG_EXNAME = 'ATTRIBUTE'                                          273
         AG_PARLIST = 'SEEN_COLO'                                         273
         CALL AXATTRIBUTE                                                 273
C    *                                                                    273
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          273
         CALL AGSRESET                                                    274
         AG_TITLE = 'CFRPMIX'                                             274
         AG_EXNAME = 'MATERIAL'                                           274
         AG_PARLIST = 'NONE'                                              274
         CALL AXMATERIAL                                                  274
C    *                                                                    274
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=10 ZI={0.6, 0.7,0.7,2.04  274
C    ,4,6,8,8.6,15.75,16.55} RMN={39.9, 39.9,39.9,39.9,39.6,38.8,37.6,37  274
C    .2,29.3,29.3}, RMX={40.0,40.0,40.0,40.0,39.7,38.9,37.7,37.3, 29.4,2  274
C    9.4} <===                                                            274
         CALL AGSRESET                                                    279
         AG_PHI1 = 0                                                      279
         AG_DPHI = 360                                                    279
         AG_NZ = 10                                                       279
         AG_ZI(1)=0.6                                                     279
         AG_ZI(2)=0.7                                                     279
         AG_ZI(3)=0.7                                                     279
         AG_ZI(4)=2.04                                                    279
         AG_ZI(5)=4                                                       279
         AG_ZI(6)=6                                                       279
         AG_ZI(7)=8                                                       279
         AG_ZI(8)=8.6                                                     279
         AG_ZI(9)=15.75                                                   279
         AG_ZI(10)=16.55                                                  279
         AG_RMN(1)=39.9                                                   279
         AG_RMN(2)=39.9                                                   279
         AG_RMN(3)=39.9                                                   279
         AG_RMN(4)=39.9                                                   279
         AG_RMN(5)=39.6                                                   279
         AG_RMN(6)=38.8                                                   279
         AG_RMN(7)=37.6                                                   279
         AG_RMN(8)=37.2                                                   279
         AG_RMN(9)=29.3                                                   279
         AG_RMN(10)=29.3                                                  279
         AG_RMX(1)=40.0                                                   279
         AG_RMX(2)=40.0                                                   279
         AG_RMX(3)=40.0                                                   279
         AG_RMX(4)=40.0                                                   279
         AG_RMX(5)=39.7                                                   279
         AG_RMX(6)=38.9                                                   279
         AG_RMX(7)=37.7                                                   279
         AG_RMX(8)=37.3                                                   279
         AG_RMX(9)=29.4                                                   279
         AG_RMX(10)=29.4                                                  279
         AG_TITLE = 'PCON'                                                279
         AG_EXNAME = 'SHAPE'                                              279
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           279
         CALL AXSHAPE                                                     279
C    *                                                                    279
5191  CALL AGSPOPB                                                        280
      GO TO BLOCK_EFSA, (5190 ,5200)                                      280
C End of Local Procedure                                                  280
C ----------------------------------------------------------------------  281
C ------------ TPRR                                                       281
C Begin Local Procedure TPRR                                              282
C TPRR TPC RESISTOR                                                       282
5240  CONTINUE                                                            282
         AG_TITLE='TPRR'                                                  282
         AG_MODULE=MODULE                                                 282
         AG_MARK='V00'                                                    282
         CALL AGSPUSH                                                     282
         AG_MODULE=MODULE                                                 282
C    *                                                                    282
C    Geant ATTRIBUTE : TPRR ===> SEEN=1 COLO=2 <===                       283
         CALL AGSRESET                                                    283
         AG_SEEN = 1                                                      283
         AG_COLO = 2                                                      283
         AG_TITLE = 'TPRR'                                                283
         AG_EXNAME = 'ATTRIBUTE'                                          283
         AG_PARLIST = 'SEEN_COLO'                                         283
         CALL AXATTRIBUTE                                                 283
C    *                                                                    283
C    Geant MATERIAL : ALUMINA ===> inherited arguments used <===          283
         CALL AGSRESET                                                    284
         AG_TITLE = 'ALUMINA'                                             284
         AG_EXNAME = 'MATERIAL'                                           284
         AG_PARLIST = 'NONE'                                              284
         CALL AXMATERIAL                                                  284
C    *                                                                    284
C    Geant SHAPE : BOX ===> DX=IDSG_DXRES/2 DY=IDSG_DYRES/2., DZ=LENGTHZ  284
C    /2. <===                                                             284
         CALL AGSRESET                                                    286
         AG_DX = IDSG_DXRES/2                                             286
         AG_DY = IDSG_DYRES/2.                                            286
         AG_DZ = LENGTHZ/2.                                               286
         AG_TITLE = 'BOX'                                                 286
         AG_EXNAME = 'SHAPE'                                              286
         AG_PARLIST = 'DX_DY_DZ'                                          286
         CALL AXSHAPE                                                     286
C    *                                                                    286
5241  CALL AGSPOPB                                                        287
      GO TO BLOCK_TPRR, (5240 ,5250)                                      287
C End of Local Procedure                                                  287
C ----------------------------------------------------------------------  288
C ------------ TPRT                                                       288
C Begin Local Procedure TPRT                                              289
C TPRT RESISTOR PROTECTION, CARBON FIBER                                  289
5220  CONTINUE                                                            289
         AG_TITLE='TPRT'                                                  289
         AG_MODULE=MODULE                                                 289
         AG_MARK='V00'                                                    289
         CALL AGSPUSH                                                     289
         AG_MODULE=MODULE                                                 289
C    *                                                                    289
C    Geant ATTRIBUTE : TPRT ===> SEEN=1 COLO=3 <===                       290
         CALL AGSRESET                                                    290
         AG_SEEN = 1                                                      290
         AG_COLO = 3                                                      290
         AG_TITLE = 'TPRT'                                                290
         AG_EXNAME = 'ATTRIBUTE'                                          290
         AG_PARLIST = 'SEEN_COLO'                                         290
         CALL AXATTRIBUTE                                                 290
C    *                                                                    290
C    Geant MATERIAL : FR4 ===> inherited arguments used <===              290
         CALL AGSRESET                                                    291
         AG_TITLE = 'FR4'                                                 291
         AG_EXNAME = 'MATERIAL'                                           291
         AG_PARLIST = 'NONE'                                              291
         CALL AXMATERIAL                                                  291
C    *                                                                    291
C    Geant SHAPE : TUBE ===> RMIN=IDSG_R1RES RMAX=IDSG_R2RES, DZ=LENGTHZ  291
C    /2. <===                                                             291
         CALL AGSRESET                                                    293
         AG_RMIN = IDSG_R1RES                                             293
         AG_RMAX = IDSG_R2RES                                             293
         AG_DZ = LENGTHZ/2.                                               293
         AG_TITLE = 'TUBE'                                                293
         AG_EXNAME = 'SHAPE'                                              293
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      293
         CALL AXSHAPE                                                     293
C    *                                                                    293
5221  CALL AGSPOPB                                                        294
      GO TO BLOCK_TPRT, (5220 ,5230)                                      294
C End of Local Procedure                                                  294
C ----------------------------------------------------------------------  295
C ------------ FGRL                                                       295
C Begin Local Procedure FGRL                                              296
C FGRL FGT RAIL                                                           296
5270  CONTINUE                                                            296
         AG_TITLE='FGRL'                                                  296
         AG_MODULE=MODULE                                                 296
         AG_MARK='V00'                                                    296
         CALL AGSPUSH                                                     296
         AG_MODULE=MODULE                                                 296
C    *                                                                    296
C    Geant ATTRIBUTE : FGRL ===> SEEN=1 COLO=2 <===                       297
         CALL AGSRESET                                                    297
         AG_SEEN = 1                                                      297
         AG_COLO = 2                                                      297
         AG_TITLE = 'FGRL'                                                297
         AG_EXNAME = 'ATTRIBUTE'                                          297
         AG_PARLIST = 'SEEN_COLO'                                         297
         CALL AXATTRIBUTE                                                 297
C    *                                                                    297
C    Geant MATERIAL : RAILMIX ===> inherited arguments used <===          297
         CALL AGSRESET                                                    298
         AG_TITLE = 'RAILMIX'                                             298
         AG_EXNAME = 'MATERIAL'                                           298
         AG_PARLIST = 'NONE'                                              298
         CALL AXMATERIAL                                                  298
C    *                                                                    298
C    Geant SHAPE : TUBE ===> RMIN=0.9 RMAX=1.0 DZ=165.6/2. <===           298
         CALL AGSRESET                                                    299
         AG_RMIN = 0.9                                                    299
         AG_RMAX = 1.0                                                    299
         AG_DZ = 165.6/2.                                                 299
         AG_TITLE = 'TUBE'                                                299
         AG_EXNAME = 'SHAPE'                                              299
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      299
         CALL AXSHAPE                                                     299
C    *                                                                    299
5271  CALL AGSPOPB                                                        300
      GO TO BLOCK_FGRL, (5270 ,5280)                                      300
C End of Local Procedure                                                  300
C ----------------------------------------------------------------------  301
C ------------ FGHV                                                       301
C Begin Local Procedure FGHV                                              302
C FGHV FGT CABLES MIXTURE                                                 302
5310  CONTINUE                                                            302
         AG_TITLE='FGHV'                                                  302
         AG_MODULE=MODULE                                                 302
         AG_MARK='V00'                                                    302
         CALL AGSPUSH                                                     302
         AG_MODULE=MODULE                                                 302
C    *                                                                    302
C    Geant ATTRIBUTE : FGHV ===> SEEN=1 COLO=1 <===                       303
         CALL AGSRESET                                                    303
         AG_SEEN = 1                                                      303
         AG_COLO = 1                                                      303
         AG_TITLE = 'FGHV'                                                303
         AG_EXNAME = 'ATTRIBUTE'                                          303
         AG_PARLIST = 'SEEN_COLO'                                         303
         CALL AXATTRIBUTE                                                 303
C    *                                                                    303
C    Geant MATERIAL : CABLEMIX ===> inherited arguments used <===         303
         CALL AGSRESET                                                    304
         AG_TITLE = 'CABLEMIX'                                            304
         AG_EXNAME = 'MATERIAL'                                           304
         AG_PARLIST = 'NONE'                                              304
         CALL AXMATERIAL                                                  304
C    *                                                                    304
C    Geant SHAPE : TUBE ===> RMIN=0. RMAX=0.43 DZ=IDSG_FGTDISKSTEPZ/2. <  304
C    ===                                                                  304
         CALL AGSRESET                                                    305
         AG_RMIN = 0.                                                     305
         AG_RMAX = 0.43                                                   305
         AG_DZ = IDSG_FGTDISKSTEPZ/2.                                     305
         AG_TITLE = 'TUBE'                                                305
         AG_EXNAME = 'SHAPE'                                              305
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      305
         CALL AXSHAPE                                                     305
C    *                                                                    305
5311  CALL AGSPOPB                                                        306
      GO TO BLOCK_FGHV, (5310 ,5320)                                      306
C End of Local Procedure                                                  306
      WRITE(LOUT,5340)'===>>  [End AgML Module IDSMGEO2]  <<==='          306
5340  FORMAT(A32,/,/)                                                     306
C MODULE IDSMGEO2                                                         308
1     CONTINUE                                                            308
      END                                                                 308
