C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE IDSMGEO1                                                 1
C IDSMGEO1 SIMPLIFIED BEAM SUPPORT CONE FOR 2012                          1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'IDSMGEO1'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'SIMPLIFIED BEAM SUPPORT CONE FOR 2012'/              1
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
C AUTHOR JAN BALEWSKI MIT, WILLIE LEIGHT MIT (MATERIAL MIXES)             2
      CHARACTER*40 AUTHOR /'JAN BALEWSKI MIT, WILLIE LEIGHT MIT (MATERIA  3
     *L MIXES)'/                                                          3
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
      INTEGER BLOCK_TPRR                                                  7
      INTEGER BLOCK_TPRT                                                  7
      INTEGER BLOCK_SUCA                                                  7
      INTEGER BLOCK_SUCB                                                  7
      INTEGER BLOCK_SUCC                                                  7
      INTEGER BLOCK_SUCD                                                  7
      INTEGER BLOCK_SUCE                                                  7
      INTEGER BLOCK_SUCF                                                  7
      INTEGER BLOCK_SUCG                                                  7
      INTEGER BLOCK_FGRL                                                  7
      INTEGER BLOCK_FGHV                                                  7
      REAL INR, OUTR,LENGTHZ, K,SINA, COSA, RESR, ANGRES,M,PM, J,         9
     *ANGRAIL, RRAIL, DPHIHV, ANGFGTCBL                                   9
C -                                                                       11
      REAL IDSC_VERSION                                                   11
      EQUIVALENCE (BANK_IDSC(3),IDSC_VERSION,RE_IDSC_VERSION),            11
     *(NAMES_IDSC(1),NIDSC_VERSION)                                       11
      EQUIVALENCE (COMMEN_IDSC(1),CIDSC_VERSION),(MAP_IDSC(1,1),          11
     *MIDSC_VERSION)                                                      11
      CHARACTER CIDSC_VERSION*40/' '/                                     11
      CHARACTER NIDSC_VERSION*12/'R.VERSION'/                             11
      INTEGER MIDSC_VERSION(2)/1,1/                                       11
      REAL RE_IDSC_VERSION                                                11
C -                                                                       11
      INTEGER LEN_IDSC,LLEN_IDSC,LINK_IDSC                                11
      PARAMETER (LEN_IDSC=3)                                              11
      PARAMETER (LLEN_IDSC=1)                                             11
      INTEGER FLAG_IDSC/0/,MAP_IDSC(2,LLEN_IDSC),IDNUM_IDSC(10)           11
      CHARACTER NAMES_IDSC(LLEN_IDSC)*12,COMMEN_IDSC(LLEN_IDSC)*40        11
      DATA IDNUM_IDSC/10*0/                                               11
      REAL BANK_IDSC(LEN_IDSC)                                            11
      CHARACTER*60 BPATH_IDSC/' '/                                        11
      SAVE FLAG_IDSC,MAP_IDSC,IDNUM_IDSC,NAMES_IDSC,COMMEN_IDSC,          11
     *BPATH_IDSC                                                          11
      DATA LINK_IDSC/0/,BANK_IDSC/LEN_IDSC*0/                             11
C *                                                                       11
C -                                                                       13
      REAL IDSG_VERSION                                                   13
      EQUIVALENCE (BANK_IDSG(3),IDSG_VERSION,RE_IDSG_VERSION),            13
     *(NAMES_IDSG(1),NIDSG_VERSION)                                       13
      EQUIVALENCE (COMMEN_IDSG(1),CIDSG_VERSION),(MAP_IDSG(1,1),          13
     *MIDSG_VERSION)                                                      13
      CHARACTER CIDSG_VERSION*40/' '/                                     13
      CHARACTER NIDSG_VERSION*12/'R.VERSION'/                             13
      INTEGER MIDSG_VERSION(2)/1,1/                                       13
      REAL RE_IDSG_VERSION                                                13
C -                                                                       13
      REAL IDSG_RF                                                        13
      EQUIVALENCE (BANK_IDSG(4),IDSG_RF,RE_IDSG_RF),(NAMES_IDSG(2),       13
     *NIDSG_RF)                                                           13
      EQUIVALENCE (COMMEN_IDSG(2),CIDSG_RF),(MAP_IDSG(1,2),MIDSG_RF)      13
      CHARACTER CIDSG_RF*40/' '/                                          13
      CHARACTER NIDSG_RF*12/'R.RF'/                                       13
      INTEGER MIDSG_RF(2)/1,1/                                            13
      REAL RE_IDSG_RF                                                     13
C -                                                                       13
      REAL IDSG_ANGFLAT                                                   13
      EQUIVALENCE (BANK_IDSG(5),IDSG_ANGFLAT,RE_IDSG_ANGFLAT),            13
     *(NAMES_IDSG(3),NIDSG_ANGFLAT)                                       13
      EQUIVALENCE (COMMEN_IDSG(3),CIDSG_ANGFLAT),(MAP_IDSG(1,3),          13
     *MIDSG_ANGFLAT)                                                      13
      CHARACTER CIDSG_ANGFLAT*40/' '/                                     13
      CHARACTER NIDSG_ANGFLAT*12/'R.ANGFLAT'/                             13
      INTEGER MIDSG_ANGFLAT(2)/1,1/                                       13
      REAL RE_IDSG_ANGFLAT                                                13
C -                                                                       13
      REAL IDSG_R1RES                                                     13
      EQUIVALENCE (BANK_IDSG(6),IDSG_R1RES,RE_IDSG_R1RES),                13
     *(NAMES_IDSG(4),NIDSG_R1RES)                                         13
      EQUIVALENCE (COMMEN_IDSG(4),CIDSG_R1RES),(MAP_IDSG(1,4),            13
     *MIDSG_R1RES)                                                        13
      CHARACTER CIDSG_R1RES*40/' '/                                       13
      CHARACTER NIDSG_R1RES*12/'R.R1RES'/                                 13
      INTEGER MIDSG_R1RES(2)/1,1/                                         13
      REAL RE_IDSG_R1RES                                                  13
C -                                                                       13
      REAL IDSG_R2RES                                                     13
      EQUIVALENCE (BANK_IDSG(7),IDSG_R2RES,RE_IDSG_R2RES),                13
     *(NAMES_IDSG(5),NIDSG_R2RES)                                         13
      EQUIVALENCE (COMMEN_IDSG(5),CIDSG_R2RES),(MAP_IDSG(1,5),            13
     *MIDSG_R2RES)                                                        13
      CHARACTER CIDSG_R2RES*40/' '/                                       13
      CHARACTER NIDSG_R2RES*12/'R.R2RES'/                                 13
      INTEGER MIDSG_R2RES(2)/1,1/                                         13
      REAL RE_IDSG_R2RES                                                  13
C -                                                                       13
      REAL IDSG_RRRES                                                     13
      EQUIVALENCE (BANK_IDSG(8),IDSG_RRRES,RE_IDSG_RRRES),                13
     *(NAMES_IDSG(6),NIDSG_RRRES)                                         13
      EQUIVALENCE (COMMEN_IDSG(6),CIDSG_RRRES),(MAP_IDSG(1,6),            13
     *MIDSG_RRRES)                                                        13
      CHARACTER CIDSG_RRRES*40/' '/                                       13
      CHARACTER NIDSG_RRRES*12/'R.RRRES'/                                 13
      INTEGER MIDSG_RRRES(2)/1,1/                                         13
      REAL RE_IDSG_RRRES                                                  13
C -                                                                       13
      REAL IDSG_DANGRES                                                   13
      EQUIVALENCE (BANK_IDSG(9),IDSG_DANGRES,RE_IDSG_DANGRES),            13
     *(NAMES_IDSG(7),NIDSG_DANGRES)                                       13
      EQUIVALENCE (COMMEN_IDSG(7),CIDSG_DANGRES),(MAP_IDSG(1,7),          13
     *MIDSG_DANGRES)                                                      13
      CHARACTER CIDSG_DANGRES*40/' '/                                     13
      CHARACTER NIDSG_DANGRES*12/'R.DANGRES'/                             13
      INTEGER MIDSG_DANGRES(2)/1,1/                                       13
      REAL RE_IDSG_DANGRES                                                13
C -                                                                       13
      REAL IDSG_DXRES                                                     13
      EQUIVALENCE (BANK_IDSG(10),IDSG_DXRES,RE_IDSG_DXRES),               13
     *(NAMES_IDSG(8),NIDSG_DXRES)                                         13
      EQUIVALENCE (COMMEN_IDSG(8),CIDSG_DXRES),(MAP_IDSG(1,8),            13
     *MIDSG_DXRES)                                                        13
      CHARACTER CIDSG_DXRES*40/' '/                                       13
      CHARACTER NIDSG_DXRES*12/'R.DXRES'/                                 13
      INTEGER MIDSG_DXRES(2)/1,1/                                         13
      REAL RE_IDSG_DXRES                                                  13
C -                                                                       13
      REAL IDSG_DYRES                                                     13
      EQUIVALENCE (BANK_IDSG(11),IDSG_DYRES,RE_IDSG_DYRES),               13
     *(NAMES_IDSG(9),NIDSG_DYRES)                                         13
      EQUIVALENCE (COMMEN_IDSG(9),CIDSG_DYRES),(MAP_IDSG(1,9),            13
     *MIDSG_DYRES)                                                        13
      CHARACTER CIDSG_DYRES*40/' '/                                       13
      CHARACTER NIDSG_DYRES*12/'R.DYRES'/                                 13
      INTEGER MIDSG_DYRES(2)/1,1/                                         13
      REAL RE_IDSG_DYRES                                                  13
C -                                                                       13
      REAL IDSG_FGTSTARTZ                                                 13
      EQUIVALENCE (BANK_IDSG(12),IDSG_FGTSTARTZ,RE_IDSG_FGTSTARTZ),       13
     *(NAMES_IDSG(10),NIDSG_FGTSTARTZ)                                    13
      EQUIVALENCE (COMMEN_IDSG(10),CIDSG_FGTSTARTZ),(MAP_IDSG(1,10),      13
     *MIDSG_FGTSTARTZ)                                                    13
      CHARACTER CIDSG_FGTSTARTZ*40/' '/                                   13
      CHARACTER NIDSG_FGTSTARTZ*12/'R.FGTSTARTZ'/                         13
      INTEGER MIDSG_FGTSTARTZ(2)/1,1/                                     13
      REAL RE_IDSG_FGTSTARTZ                                              13
C -                                                                       13
      REAL IDSG_FGTDISKSTEPZ                                              13
      EQUIVALENCE (BANK_IDSG(13),IDSG_FGTDISKSTEPZ,                       13
     *RE_IDSG_FGTDISKSTEPZ),(NAMES_IDSG(11),NIDSG_FGTDISKSTEPZ)           13
      EQUIVALENCE (COMMEN_IDSG(11),CIDSG_FGTDISKSTEPZ),(MAP_IDSG(1,11),   13
     *MIDSG_FGTDISKSTEPZ)                                                 13
      CHARACTER CIDSG_FGTDISKSTEPZ*40/' '/                                13
      CHARACTER NIDSG_FGTDISKSTEPZ*12/'R.FGTDISKSTEPZ'/                   13
      INTEGER MIDSG_FGTDISKSTEPZ(2)/1,1/                                  13
      REAL RE_IDSG_FGTDISKSTEPZ                                           13
C -                                                                       13
      INTEGER IDSG_FGTNDISK                                               13
      EQUIVALENCE (BANK_IDSG(14),IDSG_FGTNDISK,RE_IDSG_FGTNDISK),         13
     *(NAMES_IDSG(12),NIDSG_FGTNDISK)                                     13
      EQUIVALENCE (COMMEN_IDSG(12),CIDSG_FGTNDISK),(MAP_IDSG(1,12),       13
     *MIDSG_FGTNDISK)                                                     13
      CHARACTER CIDSG_FGTNDISK*40/' '/                                    13
      CHARACTER NIDSG_FGTNDISK*12/'I.FGTNDISK'/                           13
      INTEGER MIDSG_FGTNDISK(2)/1,1/                                      13
      REAL RE_IDSG_FGTNDISK                                               13
C -                                                                       13
      INTEGER LEN_IDSG,LLEN_IDSG,LINK_IDSG                                13
      PARAMETER (LEN_IDSG=14)                                             13
      PARAMETER (LLEN_IDSG=12)                                            13
      INTEGER FLAG_IDSG/0/,MAP_IDSG(2,LLEN_IDSG),IDNUM_IDSG(10)           13
      CHARACTER NAMES_IDSG(LLEN_IDSG)*12,COMMEN_IDSG(LLEN_IDSG)*40        13
      DATA IDNUM_IDSG/10*0/                                               13
      REAL BANK_IDSG(LEN_IDSG)                                            13
      CHARACTER*60 BPATH_IDSG/' '/                                        13
      SAVE FLAG_IDSG,MAP_IDSG,IDNUM_IDSG,NAMES_IDSG,COMMEN_IDSG,          13
     *BPATH_IDSG                                                          13
      DATA LINK_IDSG/0/,BANK_IDSG/LEN_IDSG*0/                             13
C *                                                                       13
C -                                                                       14
      REAL IDSA_VERSION                                                   14
      EQUIVALENCE (BANK_IDSA(3),IDSA_VERSION,RE_IDSA_VERSION),            14
     *(NAMES_IDSA(1),NIDSA_VERSION)                                       14
      EQUIVALENCE (COMMEN_IDSA(1),CIDSA_VERSION),(MAP_IDSA(1,1),          14
     *MIDSA_VERSION)                                                      14
      CHARACTER CIDSA_VERSION*40/' '/                                     14
      CHARACTER NIDSA_VERSION*12/'R.VERSION'/                             14
      INTEGER MIDSA_VERSION(2)/1,1/                                       14
      REAL RE_IDSA_VERSION                                                14
C -                                                                       14
      REAL IDSA_X                                                         14
      EQUIVALENCE (BANK_IDSA(4),IDSA_X,RE_IDSA_X),(NAMES_IDSA(2),         14
     *NIDSA_X)                                                            14
      EQUIVALENCE (COMMEN_IDSA(2),CIDSA_X),(MAP_IDSA(1,2),MIDSA_X)        14
      CHARACTER CIDSA_X*40/' '/                                           14
      CHARACTER NIDSA_X*12/'R.X'/                                         14
      INTEGER MIDSA_X(2)/1,1/                                             14
      REAL RE_IDSA_X                                                      14
C -                                                                       14
      REAL IDSA_Y                                                         14
      EQUIVALENCE (BANK_IDSA(5),IDSA_Y,RE_IDSA_Y),(NAMES_IDSA(3),         14
     *NIDSA_Y)                                                            14
      EQUIVALENCE (COMMEN_IDSA(3),CIDSA_Y),(MAP_IDSA(1,3),MIDSA_Y)        14
      CHARACTER CIDSA_Y*40/' '/                                           14
      CHARACTER NIDSA_Y*12/'R.Y'/                                         14
      INTEGER MIDSA_Y(2)/1,1/                                             14
      REAL RE_IDSA_Y                                                      14
C -                                                                       14
      REAL IDSA_Z                                                         14
      EQUIVALENCE (BANK_IDSA(6),IDSA_Z,RE_IDSA_Z),(NAMES_IDSA(4),         14
     *NIDSA_Z)                                                            14
      EQUIVALENCE (COMMEN_IDSA(4),CIDSA_Z),(MAP_IDSA(1,4),MIDSA_Z)        14
      CHARACTER CIDSA_Z*40/' '/                                           14
      CHARACTER NIDSA_Z*12/'R.Z'/                                         14
      INTEGER MIDSA_Z(2)/1,1/                                             14
      REAL RE_IDSA_Z                                                      14
C -                                                                       14
      REAL IDSA_THETAX                                                    14
      EQUIVALENCE (BANK_IDSA(7),IDSA_THETAX,RE_IDSA_THETAX),              14
     *(NAMES_IDSA(5),NIDSA_THETAX)                                        14
      EQUIVALENCE (COMMEN_IDSA(5),CIDSA_THETAX),(MAP_IDSA(1,5),           14
     *MIDSA_THETAX)                                                       14
      CHARACTER CIDSA_THETAX*40/' '/                                      14
      CHARACTER NIDSA_THETAX*12/'R.THETAX'/                               14
      INTEGER MIDSA_THETAX(2)/1,1/                                        14
      REAL RE_IDSA_THETAX                                                 14
C -                                                                       14
      REAL IDSA_THETAY                                                    14
      EQUIVALENCE (BANK_IDSA(8),IDSA_THETAY,RE_IDSA_THETAY),              14
     *(NAMES_IDSA(6),NIDSA_THETAY)                                        14
      EQUIVALENCE (COMMEN_IDSA(6),CIDSA_THETAY),(MAP_IDSA(1,6),           14
     *MIDSA_THETAY)                                                       14
      CHARACTER CIDSA_THETAY*40/' '/                                      14
      CHARACTER NIDSA_THETAY*12/'R.THETAY'/                               14
      INTEGER MIDSA_THETAY(2)/1,1/                                        14
      REAL RE_IDSA_THETAY                                                 14
C -                                                                       14
      REAL IDSA_THETAZ                                                    14
      EQUIVALENCE (BANK_IDSA(9),IDSA_THETAZ,RE_IDSA_THETAZ),              14
     *(NAMES_IDSA(7),NIDSA_THETAZ)                                        14
      EQUIVALENCE (COMMEN_IDSA(7),CIDSA_THETAZ),(MAP_IDSA(1,7),           14
     *MIDSA_THETAZ)                                                       14
      CHARACTER CIDSA_THETAZ*40/' '/                                      14
      CHARACTER NIDSA_THETAZ*12/'R.THETAZ'/                               14
      INTEGER MIDSA_THETAZ(2)/1,1/                                        14
      REAL RE_IDSA_THETAZ                                                 14
C -                                                                       14
      REAL IDSA_PHIX                                                      14
      EQUIVALENCE (BANK_IDSA(10),IDSA_PHIX,RE_IDSA_PHIX),(NAMES_IDSA(8),  14
     *NIDSA_PHIX)                                                         14
      EQUIVALENCE (COMMEN_IDSA(8),CIDSA_PHIX),(MAP_IDSA(1,8),             14
     *MIDSA_PHIX)                                                         14
      CHARACTER CIDSA_PHIX*40/' '/                                        14
      CHARACTER NIDSA_PHIX*12/'R.PHIX'/                                   14
      INTEGER MIDSA_PHIX(2)/1,1/                                          14
      REAL RE_IDSA_PHIX                                                   14
C -                                                                       14
      REAL IDSA_PHIY                                                      14
      EQUIVALENCE (BANK_IDSA(11),IDSA_PHIY,RE_IDSA_PHIY),(NAMES_IDSA(9),  14
     *NIDSA_PHIY)                                                         14
      EQUIVALENCE (COMMEN_IDSA(9),CIDSA_PHIY),(MAP_IDSA(1,9),             14
     *MIDSA_PHIY)                                                         14
      CHARACTER CIDSA_PHIY*40/' '/                                        14
      CHARACTER NIDSA_PHIY*12/'R.PHIY'/                                   14
      INTEGER MIDSA_PHIY(2)/1,1/                                          14
      REAL RE_IDSA_PHIY                                                   14
C -                                                                       14
      REAL IDSA_PHIZ                                                      14
      EQUIVALENCE (BANK_IDSA(12),IDSA_PHIZ,RE_IDSA_PHIZ),                 14
     *(NAMES_IDSA(10),NIDSA_PHIZ)                                         14
      EQUIVALENCE (COMMEN_IDSA(10),CIDSA_PHIZ),(MAP_IDSA(1,10),           14
     *MIDSA_PHIZ)                                                         14
      CHARACTER CIDSA_PHIZ*40/' '/                                        14
      CHARACTER NIDSA_PHIZ*12/'R.PHIZ'/                                   14
      INTEGER MIDSA_PHIZ(2)/1,1/                                          14
      REAL RE_IDSA_PHIZ                                                   14
C -                                                                       14
      INTEGER LEN_IDSA,LLEN_IDSA,LINK_IDSA                                14
      PARAMETER (LEN_IDSA=12)                                             14
      PARAMETER (LLEN_IDSA=10)                                            14
      INTEGER FLAG_IDSA/0/,MAP_IDSA(2,LLEN_IDSA),IDNUM_IDSA(10)           14
      CHARACTER NAMES_IDSA(LLEN_IDSA)*12,COMMEN_IDSA(LLEN_IDSA)*40        14
      DATA IDNUM_IDSA/10*0/                                               14
      REAL BANK_IDSA(LEN_IDSA)                                            14
      CHARACTER*60 BPATH_IDSA/' '/                                        14
      SAVE FLAG_IDSA,MAP_IDSA,IDNUM_IDSA,NAMES_IDSA,COMMEN_IDSA,          14
     *BPATH_IDSA                                                          14
      DATA LINK_IDSA/0/,BANK_IDSA/LEN_IDSA*0/                             14
C *                                                                       14
C *                                                                       15
      CALL AGSBEGM(MODULE,IPRIN)                                          15
      IF (IPRIN.LT.0) RETURN                                              15
C Filling bank IDSC - IDS CONTROL STRUCTURE                               15
      BANK_TITLE='IDS CONTROL STRUCTURE'                                  15
      BPATH_IDSC='IDSC*'                                                  15
      IDNUM_IDSC(1)=0                                                     16
C VERSIONING OF THE IDSM GEOMETRY                                         16
      IDSC_VERSION=1.0                                                    16
      CIDSC_VERSION='VERSIONING OF THE IDSM GEOMETRY'                     16
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'IDSC',    17
     *BPATH_IDSC,IDNUM_IDSC,1, MAP_IDSC,NAMES_IDSC,COMMEN_IDSC,           17
     *BANK_IDSC,LEN_IDSC,LLEN_IDSC,LINK_IDSC,FLAG_IDSC)                   17
C *                                                                       17
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'IDSC',    17
     *BPATH_IDSC,IDNUM_IDSC,1, MAP_IDSC,NAMES_IDSC,COMMEN_IDSC,           17
     *BANK_IDSC,LEN_IDSC,LLEN_IDSC,LINK_IDSC,FLAG_IDSC)                   17
C *                                                                       18
      CALL AGSBEGM(MODULE,IPRIN)                                          18
      IF (IPRIN.LT.0) RETURN                                              18
C Filling bank IDSG - IDS GEOMETRY DATA,46.1CM=INNER RADII OF IFC         18
      BANK_TITLE='IDS GEOMETRY DATA,46.1CM=INNER RADII OF IFC'            18
      BPATH_IDSG='IDSG*'                                                  18
      IDNUM_IDSG(1)=0                                                     19
C 2012 VERSIONIN OF THE IDS GEOMETRY                                      19
      IDSG_VERSION=1.0                                                    19
      CIDSG_VERSION='2012 VERSIONIN OF THE IDS GEOMETRY'                  19
C RADII OF INNER VOLUME BOUNDARY                                          20
      IDSG_RF=2.25                                                        20
      CIDSG_RF='RADII OF INNER VOLUME BOUNDARY'                           20
C ANGLE (DEG) FOR CENTER OF FLAT                                          21
      IDSG_ANGFLAT=106.                                                   21
      CIDSG_ANGFLAT='ANGLE (DEG) FOR CENTER OF FLAT'                      21
C RADIAL DISTANCE OF FOR TPC RESISTOR TUBES                               22
      IDSG_RRRES=43.                                                      22
      CIDSG_RRRES='RADIAL DISTANCE OF FOR TPC RESISTOR TUBES'             22
C INNER RADII FOR TPC RESISTOR TUBES                                      23
      IDSG_R1RES=1.17                                                     23
      CIDSG_R1RES='INNER RADII FOR TPC RESISTOR TUBES'                    23
C OUTER RADII FOR TPC RESISTOR TUBES                                      24
      IDSG_R2RES=1.27                                                     24
      CIDSG_R2RES='OUTER RADII FOR TPC RESISTOR TUBES'                    24
C OPENING ANGLE (DEG) FOR TPC RESISTOR TUBES                              25
      IDSG_DANGRES=11.3                                                   25
      CIDSG_DANGRES='OPENING ANGLE (DEG) FOR TPC RESISTOR TUBES'          25
C THICKNESSFOR TPC RESISTOR                                               26
      IDSG_DXRES=0.13                                                     26
      CIDSG_DXRES='THICKNESSFOR TPC RESISTOR'                             26
C DY FOR TPC RESISTOR                                                     27
      IDSG_DYRES=2.                                                       27
      CIDSG_DYRES='DY FOR TPC RESISTOR'                                   27
C POSITION OF SENSITIVE VOLUME OF THE 1ST DISK                            28
      IDSG_FGTSTARTZ=70.                                                  28
      CIDSG_FGTSTARTZ='POSITION OF SENSITIVE VOLUME OF THE 1ST DISK'      28
C DISK SEPARATION ALONG Z                                                 29
      IDSG_FGTDISKSTEPZ=10.                                               29
      CIDSG_FGTDISKSTEPZ='DISK SEPARATION ALONG Z'                        29
C NUMBER OF DISKS                                                         30
      IDSG_FGTNDISK=6                                                     30
      CIDSG_FGTNDISK='NUMBER OF DISKS'                                    30
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'IDSG',    31
     *BPATH_IDSG,IDNUM_IDSG,1, MAP_IDSG,NAMES_IDSG,COMMEN_IDSG,           31
     *BANK_IDSG,LEN_IDSG,LLEN_IDSG,LINK_IDSG,FLAG_IDSG)                   31
C *                                                                       31
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'IDSG',    31
     *BPATH_IDSG,IDNUM_IDSG,1, MAP_IDSG,NAMES_IDSG,COMMEN_IDSG,           31
     *BANK_IDSG,LEN_IDSG,LLEN_IDSG,LINK_IDSG,FLAG_IDSG)                   31
C *                                                                       32
      CALL AGSBEGM(MODULE,IPRIN)                                          32
      IF (IPRIN.LT.0) RETURN                                              32
C Filling bank IDSG - IDS GEOMETRY DATA,46.1CM=INNER RADII OF IFC         32
      BANK_TITLE='IDS GEOMETRY DATA,46.1CM=INNER RADII OF IFC'            32
      BPATH_IDSG='IDSG*'                                                  32
      IDNUM_IDSG(1)=0                                                     33
C 2013 VERSIONIN OF THE IDS GEOMETRY                                      33
      IDSG_VERSION=2.0                                                    33
      CIDSG_VERSION='2013 VERSIONIN OF THE IDS GEOMETRY'                  33
C RADII OF INNER VOLUME BOUNDARY                                          34
      IDSG_RF=2.25                                                        34
      CIDSG_RF='RADII OF INNER VOLUME BOUNDARY'                           34
C ANGLE (DEG) FOR CENTER OF FLAT                                          35
      IDSG_ANGFLAT=106.                                                   35
      CIDSG_ANGFLAT='ANGLE (DEG) FOR CENTER OF FLAT'                      35
C RADIAL DISTANCE OF FOR TPC RESISTOR TUBES                               36
      IDSG_RRRES=43.                                                      36
      CIDSG_RRRES='RADIAL DISTANCE OF FOR TPC RESISTOR TUBES'             36
C INNER RADII FOR TPC RESISTOR TUBES                                      37
      IDSG_R1RES=1.17                                                     37
      CIDSG_R1RES='INNER RADII FOR TPC RESISTOR TUBES'                    37
C OUTER RADII FOR TPC RESISTOR TUBES                                      38
      IDSG_R2RES=1.27                                                     38
      CIDSG_R2RES='OUTER RADII FOR TPC RESISTOR TUBES'                    38
C OPENING ANGLE (DEG) FOR TPC RESISTOR TUBES                              39
      IDSG_DANGRES=11.3                                                   39
      CIDSG_DANGRES='OPENING ANGLE (DEG) FOR TPC RESISTOR TUBES'          39
C THICKNESSFOR TPC RESISTOR                                               40
      IDSG_DXRES=0.13                                                     40
      CIDSG_DXRES='THICKNESSFOR TPC RESISTOR'                             40
C DY FOR TPC RESISTOR                                                     41
      IDSG_DYRES=2.                                                       41
      CIDSG_DYRES='DY FOR TPC RESISTOR'                                   41
C POSITION OF SENSITIVE VOLUME OF THE 1ST DISK                            42
      IDSG_FGTSTARTZ=70.                                                  42
      CIDSG_FGTSTARTZ='POSITION OF SENSITIVE VOLUME OF THE 1ST DISK'      42
C DISK SEPARATION ALONG Z                                                 43
      IDSG_FGTDISKSTEPZ=10.                                               43
      CIDSG_FGTDISKSTEPZ='DISK SEPARATION ALONG Z'                        43
C NUMBER OF DISKS                                                         44
      IDSG_FGTNDISK=6                                                     44
      CIDSG_FGTNDISK='NUMBER OF DISKS'                                    44
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'IDSG',    45
     *BPATH_IDSG,IDNUM_IDSG,1, MAP_IDSG,NAMES_IDSG,COMMEN_IDSG,           45
     *BANK_IDSG,LEN_IDSG,LLEN_IDSG,LINK_IDSG,FLAG_IDSG)                   45
C *                                                                       45
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'IDSG',    45
     *BPATH_IDSG,IDNUM_IDSG,1, MAP_IDSG,NAMES_IDSG,COMMEN_IDSG,           45
     *BANK_IDSG,LEN_IDSG,LLEN_IDSG,LINK_IDSG,FLAG_IDSG)                   45
C *                                                                       46
      CALL AGSBEGM(MODULE,IPRIN)                                          46
      IF (IPRIN.LT.0) RETURN                                              46
C Filling bank IDSA - ALIGNMENT OF THE IDSM GEOMETRY WITHIN THE CAVE NO   46
C FGT CABLES                                                              46
      BANK_TITLE='ALIGNMENT OF THE IDSM GEOMETRY WITHIN THE CAVE NO FGT   46
     *CABLES'                                                             46
      BPATH_IDSA='IDSA*'                                                  46
      IDNUM_IDSA(1)=0                                                     47
C DEFAULT ALIGNMENT OF IDSM AT (0,0,0) WITH NO ROTATION                   47
      IDSA_VERSION=1.0                                                    47
      CIDSA_VERSION='DEFAULT ALIGNMENT OF IDSM AT (0,0,0) WITH NO ROTATI  47
     *ON'                                                                 47
C X-ALIGNMENT                                                             48
      IDSA_X=0.0                                                          48
      CIDSA_X='X-ALIGNMENT'                                               48
C Y-ALIGNMENT                                                             49
      IDSA_Y=0.0                                                          49
      CIDSA_Y='Y-ALIGNMENT'                                               49
C Z-ALIGNMENT                                                             50
      IDSA_Z=0.0                                                          50
      CIDSA_Z='Z-ALIGNMENT'                                               50
C ALIGN X`-AXIS 90 DEGREES IN THETA WRT CAVE                              51
      IDSA_THETAX=90.0                                                    51
      CIDSA_THETAX='ALIGN X`-AXIS 90 DEGREES IN THETA WRT CAVE'           51
C ALIGN X`-AXIS 0 DEGREES IN PHI WRT CAVE                                 52
      IDSA_PHIX=0.0                                                       52
      CIDSA_PHIX='ALIGN X`-AXIS 0 DEGREES IN PHI WRT CAVE'                52
C ALIGN Y`-AXIS 90 DEGREES IN THETA WRT CAVE                              53
      IDSA_THETAY=90.0                                                    53
      CIDSA_THETAY='ALIGN Y`-AXIS 90 DEGREES IN THETA WRT CAVE'           53
C ALIGN Y`-AXIS 0 DEGREES IN PHI WRT CAVE                                 54
      IDSA_PHIY=90.0                                                      54
      CIDSA_PHIY='ALIGN Y`-AXIS 0 DEGREES IN PHI WRT CAVE'                54
C ALIGN Z`-AXIS 0 DEGREES IN THETA WRT CAVE                               55
      IDSA_THETAZ=0.0                                                     55
      CIDSA_THETAZ='ALIGN Z`-AXIS 0 DEGREES IN THETA WRT CAVE'            55
C ALIGN Z`-AXIS 0 DEGREES IN PHI WRT CAVE                                 56
      IDSA_PHIZ=0.0                                                       56
      CIDSA_PHIZ='ALIGN Z`-AXIS 0 DEGREES IN PHI WRT CAVE'                56
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'IDSA',    57
     *BPATH_IDSA,IDNUM_IDSA,1, MAP_IDSA,NAMES_IDSA,COMMEN_IDSA,           57
     *BANK_IDSA,LEN_IDSA,LLEN_IDSA,LINK_IDSA,FLAG_IDSA)                   57
C *                                                                       57
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'IDSA',    57
     *BPATH_IDSA,IDNUM_IDSA,1, MAP_IDSA,NAMES_IDSA,COMMEN_IDSA,           57
     *BANK_IDSA,LEN_IDSA,LLEN_IDSA,LINK_IDSA,FLAG_IDSA)                   57
C Geant COMPONENT : O ===> A=16 Z=8 W=0.062 <===                          58
      CALL AGSRESET                                                       58
      AG_A = 16                                                           58
      AG_Z = 8                                                            58
      AG_W = 0.062                                                        58
      AG_TITLE = 'O'                                                      58
      AG_EXNAME = 'COMPONENT'                                             58
      AG_PARLIST = 'A_Z_W'                                                58
      CALL AXCOMPONENT                                                    58
C *                                                                       58
C Geant COMPONENT : C ===> A=12 Z=6 W=0.892 <===                          58
      CALL AGSRESET                                                       59
      AG_A = 12                                                           59
      AG_Z = 6                                                            59
      AG_W = 0.892                                                        59
      AG_TITLE = 'C'                                                      59
      AG_EXNAME = 'COMPONENT'                                             59
      AG_PARLIST = 'A_Z_W'                                                59
      CALL AXCOMPONENT                                                    59
C *                                                                       59
C Geant COMPONENT : H ===> A=1 Z=1 W=0.019 <===                           59
      CALL AGSRESET                                                       60
      AG_A = 1                                                            60
      AG_Z = 1                                                            60
      AG_W = 0.019                                                        60
      AG_TITLE = 'H'                                                      60
      AG_EXNAME = 'COMPONENT'                                             60
      AG_PARLIST = 'A_Z_W'                                                60
      CALL AXCOMPONENT                                                    60
C *                                                                       60
C Geant COMPONENT : CL ===> A=35.5 Z=17 W=0.027 <===                      60
      CALL AGSRESET                                                       61
      AG_A = 35.5                                                         61
      AG_Z = 17                                                           61
      AG_W = 0.027                                                        61
      AG_TITLE = 'CL'                                                     61
      AG_EXNAME = 'COMPONENT'                                             61
      AG_PARLIST = 'A_Z_W'                                                61
      CALL AXCOMPONENT                                                    61
C *                                                                       61
C Geant MIXTURE : CFRPMIX ===> DENS=1.78 <===                             61
      CALL AGSRESET                                                       62
      AG_DENS = 1.78                                                      62
      AG_TITLE = 'CFRPMIX'                                                62
      AG_EXNAME = 'MIXTURE'                                               62
      AG_PARLIST = 'DENS'                                                 62
      CALL AXMIXTURE                                                      62
C *                                                                       62
C Geant COMPONENT : AL ===> A=27 Z=13 W=1. <===                           62
      CALL AGSRESET                                                       63
      AG_A = 27                                                           63
      AG_Z = 13                                                           63
      AG_W = 1.                                                           63
      AG_TITLE = 'AL'                                                     63
      AG_EXNAME = 'COMPONENT'                                             63
      AG_PARLIST = 'A_Z_W'                                                63
      CALL AXCOMPONENT                                                    63
C *                                                                       63
C Geant MIXTURE : ALPURE ===> DENS=2.80 <===                              63
      CALL AGSRESET                                                       64
      AG_DENS = 2.80                                                      64
      AG_TITLE = 'ALPURE'                                                 64
      AG_EXNAME = 'MIXTURE'                                               64
      AG_PARLIST = 'DENS'                                                 64
      CALL AXMIXTURE                                                      64
C *                                                                       64
C Geant COMPONENT : O ===> A=16 Z=8 W=0.043 <===                          64
      CALL AGSRESET                                                       65
      AG_A = 16                                                           65
      AG_Z = 8                                                            65
      AG_W = 0.043                                                        65
      AG_TITLE = 'O'                                                      65
      AG_EXNAME = 'COMPONENT'                                             65
      AG_PARLIST = 'A_Z_W'                                                65
      CALL AXCOMPONENT                                                    65
C *                                                                       65
C Geant COMPONENT : C ===> A=12 Z=6 W=0.635 <===                          65
      CALL AGSRESET                                                       66
      AG_A = 12                                                           66
      AG_Z = 6                                                            66
      AG_W = 0.635                                                        66
      AG_TITLE = 'C'                                                      66
      AG_EXNAME = 'COMPONENT'                                             66
      AG_PARLIST = 'A_Z_W'                                                66
      CALL AXCOMPONENT                                                    66
C *                                                                       66
C Geant COMPONENT : H ===> A=1 Z=1 W=0.014 <===                           66
      CALL AGSRESET                                                       67
      AG_A = 1                                                            67
      AG_Z = 1                                                            67
      AG_W = 0.014                                                        67
      AG_TITLE = 'H'                                                      67
      AG_EXNAME = 'COMPONENT'                                             67
      AG_PARLIST = 'A_Z_W'                                                67
      CALL AXCOMPONENT                                                    67
C *                                                                       67
C Geant COMPONENT : CL ===> A=35.5 Z=17 W=0.019 <===                      67
      CALL AGSRESET                                                       68
      AG_A = 35.5                                                         68
      AG_Z = 17                                                           68
      AG_W = 0.019                                                        68
      AG_TITLE = 'CL'                                                     68
      AG_EXNAME = 'COMPONENT'                                             68
      AG_PARLIST = 'A_Z_W'                                                68
      CALL AXCOMPONENT                                                    68
C *                                                                       68
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.017 <===                        68
      CALL AGSRESET                                                       69
      AG_A = 27                                                           69
      AG_Z = 13                                                           69
      AG_W = 0.017                                                        69
      AG_TITLE = 'AL'                                                     69
      AG_EXNAME = 'COMPONENT'                                             69
      AG_PARLIST = 'A_Z_W'                                                69
      CALL AXCOMPONENT                                                    69
C *                                                                       69
C Geant COMPONENT : TI ===> A=47.9 Z=22 W=0.260 <===                      69
      CALL AGSRESET                                                       70
      AG_A = 47.9                                                         70
      AG_Z = 22                                                           70
      AG_W = 0.260                                                        70
      AG_TITLE = 'TI'                                                     70
      AG_EXNAME = 'COMPONENT'                                             70
      AG_PARLIST = 'A_Z_W'                                                70
      CALL AXCOMPONENT                                                    70
C *                                                                       70
C Geant COMPONENT : V ===> A=50.9 Z=23 W=0.012 <===                       70
      CALL AGSRESET                                                       71
      AG_A = 50.9                                                         71
      AG_Z = 23                                                           71
      AG_W = 0.012                                                        71
      AG_TITLE = 'V'                                                      71
      AG_EXNAME = 'COMPONENT'                                             71
      AG_PARLIST = 'A_Z_W'                                                71
      CALL AXCOMPONENT                                                    71
C *                                                                       71
C Geant MIXTURE : SUCBMIX ===> DENS=2.46 <===                             71
      CALL AGSRESET                                                       72
      AG_DENS = 2.46                                                      72
      AG_TITLE = 'SUCBMIX'                                                72
      AG_EXNAME = 'MIXTURE'                                               72
      AG_PARLIST = 'DENS'                                                 72
      CALL AXMIXTURE                                                      72
C *                                                                       72
C Geant COMPONENT : O ===> A=16 Z=8 W=0.048 <===                          72
      CALL AGSRESET                                                       73
      AG_A = 16                                                           73
      AG_Z = 8                                                            73
      AG_W = 0.048                                                        73
      AG_TITLE = 'O'                                                      73
      AG_EXNAME = 'COMPONENT'                                             73
      AG_PARLIST = 'A_Z_W'                                                73
      CALL AXCOMPONENT                                                    73
C *                                                                       73
C Geant COMPONENT : C ===> A=12 Z=6 W=0.694 <===                          73
      CALL AGSRESET                                                       74
      AG_A = 12                                                           74
      AG_Z = 6                                                            74
      AG_W = 0.694                                                        74
      AG_TITLE = 'C'                                                      74
      AG_EXNAME = 'COMPONENT'                                             74
      AG_PARLIST = 'A_Z_W'                                                74
      CALL AXCOMPONENT                                                    74
C *                                                                       74
C Geant COMPONENT : H ===> A=1 Z=1 W=0.015 <===                           74
      CALL AGSRESET                                                       75
      AG_A = 1                                                            75
      AG_Z = 1                                                            75
      AG_W = 0.015                                                        75
      AG_TITLE = 'H'                                                      75
      AG_EXNAME = 'COMPONENT'                                             75
      AG_PARLIST = 'A_Z_W'                                                75
      CALL AXCOMPONENT                                                    75
C *                                                                       75
C Geant COMPONENT : CL ===> A=35.5 Z=17 W=0.021 <===                      75
      CALL AGSRESET                                                       76
      AG_A = 35.5                                                         76
      AG_Z = 17                                                           76
      AG_W = 0.021                                                        76
      AG_TITLE = 'CL'                                                     76
      AG_EXNAME = 'COMPONENT'                                             76
      AG_PARLIST = 'A_Z_W'                                                76
      CALL AXCOMPONENT                                                    76
C *                                                                       76
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.013 <===                        76
      CALL AGSRESET                                                       77
      AG_A = 27                                                           77
      AG_Z = 13                                                           77
      AG_W = 0.013                                                        77
      AG_TITLE = 'AL'                                                     77
      AG_EXNAME = 'COMPONENT'                                             77
      AG_PARLIST = 'A_Z_W'                                                77
      CALL AXCOMPONENT                                                    77
C *                                                                       77
C Geant COMPONENT : TI ===> A=47.9 Z=22 W=0.200 <===                      77
      CALL AGSRESET                                                       78
      AG_A = 47.9                                                         78
      AG_Z = 22                                                           78
      AG_W = 0.200                                                        78
      AG_TITLE = 'TI'                                                     78
      AG_EXNAME = 'COMPONENT'                                             78
      AG_PARLIST = 'A_Z_W'                                                78
      CALL AXCOMPONENT                                                    78
C *                                                                       78
C Geant COMPONENT : V ===> A=50.9 Z=23 W=0.009 <===                       78
      CALL AGSRESET                                                       79
      AG_A = 50.9                                                         79
      AG_Z = 23                                                           79
      AG_W = 0.009                                                        79
      AG_TITLE = 'V'                                                      79
      AG_EXNAME = 'COMPONENT'                                             79
      AG_PARLIST = 'A_Z_W'                                                79
      CALL AXCOMPONENT                                                    79
C *                                                                       79
C Geant MIXTURE : SUCDMIX ===> DENS=2.37 <===                             79
      CALL AGSRESET                                                       80
      AG_DENS = 2.37                                                      80
      AG_TITLE = 'SUCDMIX'                                                80
      AG_EXNAME = 'MIXTURE'                                               80
      AG_PARLIST = 'DENS'                                                 80
      CALL AXMIXTURE                                                      80
C *                                                                       80
C Geant COMPONENT : O ===> A=16 Z=8 W=0.032 <===                          80
      CALL AGSRESET                                                       81
      AG_A = 16                                                           81
      AG_Z = 8                                                            81
      AG_W = 0.032                                                        81
      AG_TITLE = 'O'                                                      81
      AG_EXNAME = 'COMPONENT'                                             81
      AG_PARLIST = 'A_Z_W'                                                81
      CALL AXCOMPONENT                                                    81
C *                                                                       81
C Geant COMPONENT : C ===> A=12 Z=6 W=0.471 <===                          81
      CALL AGSRESET                                                       82
      AG_A = 12                                                           82
      AG_Z = 6                                                            82
      AG_W = 0.471                                                        82
      AG_TITLE = 'C'                                                      82
      AG_EXNAME = 'COMPONENT'                                             82
      AG_PARLIST = 'A_Z_W'                                                82
      CALL AXCOMPONENT                                                    82
C *                                                                       82
C Geant COMPONENT : H ===> A=1 Z=1 W=0.011 <===                           82
      CALL AGSRESET                                                       83
      AG_A = 1                                                            83
      AG_Z = 1                                                            83
      AG_W = 0.011                                                        83
      AG_TITLE = 'H'                                                      83
      AG_EXNAME = 'COMPONENT'                                             83
      AG_PARLIST = 'A_Z_W'                                                83
      CALL AXCOMPONENT                                                    83
C *                                                                       83
C Geant COMPONENT : CL ===> A=35.5 Z=17 W=0.014 <===                      83
      CALL AGSRESET                                                       84
      AG_A = 35.5                                                         84
      AG_Z = 17                                                           84
      AG_W = 0.014                                                        84
      AG_TITLE = 'CL'                                                     84
      AG_EXNAME = 'COMPONENT'                                             84
      AG_PARLIST = 'A_Z_W'                                                84
      CALL AXCOMPONENT                                                    84
C *                                                                       84
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.472 <===                        84
      CALL AGSRESET                                                       85
      AG_A = 27                                                           85
      AG_Z = 13                                                           85
      AG_W = 0.472                                                        85
      AG_TITLE = 'AL'                                                     85
      AG_EXNAME = 'COMPONENT'                                             85
      AG_PARLIST = 'A_Z_W'                                                85
      CALL AXCOMPONENT                                                    85
C *                                                                       85
C Geant MIXTURE : RAILMIX ===> DENS=3.384 <===                            85
      CALL AGSRESET                                                       86
      AG_DENS = 3.384                                                     86
      AG_TITLE = 'RAILMIX'                                                86
      AG_EXNAME = 'MIXTURE'                                               86
      AG_PARLIST = 'DENS'                                                 86
      CALL AXMIXTURE                                                      86
C *                                                                       86
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.131 <===                     86
      CALL AGSRESET                                                       87
      AG_A = 28.08                                                        87
      AG_Z = 14                                                           87
      AG_W = 0.131                                                        87
      AG_TITLE = 'SI'                                                     87
      AG_EXNAME = 'COMPONENT'                                             87
      AG_PARLIST = 'A_Z_W'                                                87
      CALL AXCOMPONENT                                                    87
C *                                                                       87
C Geant COMPONENT : O ===> A=16 Z=8 W=0.117 <===                          87
      CALL AGSRESET                                                       88
      AG_A = 16                                                           88
      AG_Z = 8                                                            88
      AG_W = 0.117                                                        88
      AG_TITLE = 'O'                                                      88
      AG_EXNAME = 'COMPONENT'                                             88
      AG_PARLIST = 'A_Z_W'                                                88
      CALL AXCOMPONENT                                                    88
C *                                                                       88
C Geant COMPONENT : C ===> A=12 Z=6 W=0.193 <===                          88
      CALL AGSRESET                                                       89
      AG_A = 12                                                           89
      AG_Z = 6                                                            89
      AG_W = 0.193                                                        89
      AG_TITLE = 'C'                                                      89
      AG_EXNAME = 'COMPONENT'                                             89
      AG_PARLIST = 'A_Z_W'                                                89
      CALL AXCOMPONENT                                                    89
C *                                                                       89
C Geant COMPONENT : H ===> A=1 Z=1 W=0.024 <===                           89
      CALL AGSRESET                                                       90
      AG_A = 1                                                            90
      AG_Z = 1                                                            90
      AG_W = 0.024                                                        90
      AG_TITLE = 'H'                                                      90
      AG_EXNAME = 'COMPONENT'                                             90
      AG_PARLIST = 'A_Z_W'                                                90
      CALL AXCOMPONENT                                                    90
C *                                                                       90
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.143 <===                        90
      CALL AGSRESET                                                       91
      AG_A = 27                                                           91
      AG_Z = 13                                                           91
      AG_W = 0.143                                                        91
      AG_TITLE = 'AL'                                                     91
      AG_EXNAME = 'COMPONENT'                                             91
      AG_PARLIST = 'A_Z_W'                                                91
      CALL AXCOMPONENT                                                    91
C *                                                                       91
C Geant COMPONENT : CU ===> A=63.5 Z=29 W=0.106 <===                      91
      CALL AGSRESET                                                       92
      AG_A = 63.5                                                         92
      AG_Z = 29                                                           92
      AG_W = 0.106                                                        92
      AG_TITLE = 'CU'                                                     92
      AG_EXNAME = 'COMPONENT'                                             92
      AG_PARLIST = 'A_Z_W'                                                92
      CALL AXCOMPONENT                                                    92
C *                                                                       92
C Geant COMPONENT : F ===> A=19. Z=9 W=0.254 <===                         92
      CALL AGSRESET                                                       93
      AG_A = 19.                                                          93
      AG_Z = 9                                                            93
      AG_W = 0.254                                                        93
      AG_TITLE = 'F'                                                      93
      AG_EXNAME = 'COMPONENT'                                             93
      AG_PARLIST = 'A_Z_W'                                                93
      CALL AXCOMPONENT                                                    93
C *                                                                       93
C Geant COMPONENT : NA ===> A=23. Z=11 W=0.015 <===                       93
      CALL AGSRESET                                                       94
      AG_A = 23.                                                          94
      AG_Z = 11                                                           94
      AG_W = 0.015                                                        94
      AG_TITLE = 'NA'                                                     94
      AG_EXNAME = 'COMPONENT'                                             94
      AG_PARLIST = 'A_Z_W'                                                94
      CALL AXCOMPONENT                                                    94
C *                                                                       94
C Geant COMPONENT : CA ===> A=40.1 Z=20 W=0.017 <===                      94
      CALL AGSRESET                                                       95
      AG_A = 40.1                                                         95
      AG_Z = 20                                                           95
      AG_W = 0.017                                                        95
      AG_TITLE = 'CA'                                                     95
      AG_EXNAME = 'COMPONENT'                                             95
      AG_PARLIST = 'A_Z_W'                                                95
      CALL AXCOMPONENT                                                    95
C *                                                                       95
C Geant MIXTURE : CABLEMIX ===> DENS=1.755 <===                           95
      CALL AGSRESET                                                       96
      AG_DENS = 1.755                                                     96
      AG_TITLE = 'CABLEMIX'                                               96
      AG_EXNAME = 'MIXTURE'                                               96
      AG_PARLIST = 'DENS'                                                 96
      CALL AXMIXTURE                                                      96
C *                                                                       96
C Geant COMPONENT : AL ===> A=27 Z=13 W=2. <===                           96
      CALL AGSRESET                                                       97
      AG_A = 27                                                           97
      AG_Z = 13                                                           97
      AG_W = 2.                                                           97
      AG_TITLE = 'AL'                                                     97
      AG_EXNAME = 'COMPONENT'                                             97
      AG_PARLIST = 'A_Z_W'                                                97
      CALL AXCOMPONENT                                                    97
C *                                                                       97
C Geant COMPONENT : O ===> A=16 Z=8 W=3. <===                             97
      CALL AGSRESET                                                       98
      AG_A = 16                                                           98
      AG_Z = 8                                                            98
      AG_W = 3.                                                           98
      AG_TITLE = 'O'                                                      98
      AG_EXNAME = 'COMPONENT'                                             98
      AG_PARLIST = 'A_Z_W'                                                98
      CALL AXCOMPONENT                                                    98
C *                                                                       98
C Geant MIXTURE : ALUMINA ===> DENS=3.90 <===                             98
      CALL AGSRESET                                                       99
      AG_DENS = 3.90                                                      99
      AG_TITLE = 'ALUMINA'                                                99
      AG_EXNAME = 'MIXTURE'                                               99
      AG_PARLIST = 'DENS'                                                 99
      CALL AXMIXTURE                                                      99
C *                                                                       99
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.281 <===                     99
      CALL AGSRESET                                                       100
      AG_A = 28.08                                                        100
      AG_Z = 14                                                           100
      AG_W = 0.281                                                        100
      AG_TITLE = 'SI'                                                     100
      AG_EXNAME = 'COMPONENT'                                             100
      AG_PARLIST = 'A_Z_W'                                                100
      CALL AXCOMPONENT                                                    100
C *                                                                       100
C Geant COMPONENT : O ===> A=16 Z=8 W=0.467 <===                          100
      CALL AGSRESET                                                       101
      AG_A = 16                                                           101
      AG_Z = 8                                                            101
      AG_W = 0.467                                                        101
      AG_TITLE = 'O'                                                      101
      AG_EXNAME = 'COMPONENT'                                             101
      AG_PARLIST = 'A_Z_W'                                                101
      CALL AXCOMPONENT                                                    101
C *                                                                       101
C Geant COMPONENT : C ===> A=12 Z=6 W=0.220 <===                          101
      CALL AGSRESET                                                       102
      AG_A = 12                                                           102
      AG_Z = 6                                                            102
      AG_W = 0.220                                                        102
      AG_TITLE = 'C'                                                      102
      AG_EXNAME = 'COMPONENT'                                             102
      AG_PARLIST = 'A_Z_W'                                                102
      CALL AXCOMPONENT                                                    102
C *                                                                       102
C Geant COMPONENT : H ===> A=1 Z=1 W=0.032 <===                           102
      CALL AGSRESET                                                       103
      AG_A = 1                                                            103
      AG_Z = 1                                                            103
      AG_W = 0.032                                                        103
      AG_TITLE = 'H'                                                      103
      AG_EXNAME = 'COMPONENT'                                             103
      AG_PARLIST = 'A_Z_W'                                                103
      CALL AXCOMPONENT                                                    103
C *                                                                       103
C Geant MIXTURE : FR4 ===> DENS=1.80 <===                                 103
      CALL AGSRESET                                                       104
      AG_DENS = 1.80                                                      104
      AG_TITLE = 'FR4'                                                    104
      AG_EXNAME = 'MIXTURE'                                               104
      AG_PARLIST = 'DENS'                                                 104
      CALL AXMIXTURE                                                      104
C *                                                                       104
C use default IDSC bank                                                   104
      BPATH_IDSC='IDSC*'                                                  105
      IDNUM_IDSC(1)=0                                                     105
      CALL AGDGETP('IDSMGEO1','*',-999,'IDSC',' ',BANK_IDSC, BPATH_IDSC,  105
     *IDNUM_IDSC,1,MAP_IDSC,NAMES_IDSC,BANK_IDSC,LEN_IDSC,LLEN_IDSC,      105
     *LINK_IDSC,FLAG_IDSC)                                                105
C use IDSG bank                                                           106
      BPATH_IDSG='IDSG*'                                                  106
      IDNUM_IDSG(1)=0                                                     106
      IDSG_VERSION=IDSC_VERSION                                           106
      CALL AGDGETP('IDSMGEO1','*',-999,'IDSG',NIDSG_VERSION,              106
     *RE_IDSG_VERSION,BPATH_IDSG,IDNUM_IDSG,1,MAP_IDSG,NAMES_IDSG,        106
     *BANK_IDSG,LEN_IDSG,LLEN_IDSG,LINK_IDSG,FLAG_IDSG)                   106
C use IDSA bank                                                           107
      BPATH_IDSA='IDSA*'                                                  107
      IDNUM_IDSA(1)=0                                                     107
      IDSA_VERSION=1.0                                                    107
      CALL AGDGETP('IDSMGEO1','*',-999,'IDSA',NIDSA_VERSION,              107
     *RE_IDSA_VERSION,BPATH_IDSA,IDNUM_IDSA,1,MAP_IDSA,NAMES_IDSA,        107
     *BANK_IDSA,LEN_IDSA,LLEN_IDSA,LINK_IDSA,FLAG_IDSA)                   107
      WRITE(LOUT,5010)IDSC_VERSION                                        107
5010  FORMAT('Inner Detector Support Module with IDSC_version = ',        107
     *F5.2)                                                               107
      INR = IDSG_RF                                                       109
      OUTR = IDSG_RRRES + IDSG_R2RES                                      110
      LENGTHZ = 470.                                                      111
      SINA = SIN( IDSG_ANGFLAT * DEGRAD )                                 112
      COSA = COS( IDSG_ANGFLAT * DEGRAD )                                 113
      RRAIL=41.5                                                          114
      DPHIHV=0.03                                                         115
      CALL AGSBEGM(MODULE,IPRIN)                                          116
C Check IPRIN<0                                                           116
      IF (IPRIN.LT.0) RETURN                                              116
C Create IDSM :                                                           116
C Call To Local Procedure IDSM                                            116
      ASSIGN 5030 TO BLOCK_IDSM                                           116
      GO TO  5020                                                         116
5030  CONTINUE                                                            116
C *                                                                       116
C Geant POSITION : IDSM ===> MOTHER='CAVE' X=IDSA_X Y=IDSA_Y Z=IDSA_Z TH  119
C ETAX=IDSA_THETAX PHIX=IDSA_PHIX THETAY=IDSA_THETAY PHIY=IDSA_PHIY THET  119
C AZ=IDSA_THETAZ PHIZ=IDSA_PHIZ <===                                      119
      CALL AGSRESET                                                       119
      AG_MOTHER = 'CAVE'                                                  119
      AG_X = IDSA_X                                                       119
      AG_Y = IDSA_Y                                                       119
      AG_Z = IDSA_Z                                                       119
      AG_THETAX = IDSA_THETAX                                             119
      AG_PHIX = IDSA_PHIX                                                 119
      AG_THETAY = IDSA_THETAY                                             119
      AG_PHIY = IDSA_PHIY                                                 119
      AG_THETAZ = IDSA_THETAZ                                             119
      AG_PHIZ = IDSA_PHIZ                                                 119
      AG_TITLE = 'IDSM'                                                   119
      AG_EXNAME = 'POSITION'                                              119
      AG_PARLIST = 'MOTHER_X_Y_Z_THETAX_PHIX_THETAY_PHIY_THETAZ_PHIZ'     119
      CALL AXPOSITION                                                     119
C *                                                                       119
C ----------------------------------------------------------------------  119
C ------------ IDSM                                                       119
      CALL AGSENDM                                                        121
      RETURN                                                              121
C Begin Local Procedure IDSM                                              121
C IDSM MOTHER VOLUME FOR BEAM SUPPORT CONE                                121
5020  CONTINUE                                                            121
         AG_TITLE='IDSM'                                                  121
         AG_MODULE=MODULE                                                 121
         AG_MARK='V00'                                                    121
         CALL AGSPUSH                                                     121
         AG_MODULE=MODULE                                                 121
C    *                                                                    121
C    Geant ATTRIBUTE : IDSM ===> SEEN=0 COLO=4 <===                       122
         CALL AGSRESET                                                    122
         AG_SEEN = 0                                                      122
         AG_COLO = 4                                                      122
         AG_TITLE = 'IDSM'                                                122
         AG_EXNAME = 'ATTRIBUTE'                                          122
         AG_PARLIST = 'SEEN_COLO'                                         122
         CALL AXATTRIBUTE                                                 122
C    *                                                                    122
C    Geant MATERIAL : AIR ===> inherited arguments used <===              122
         CALL AGSRESET                                                    123
         AG_TITLE = 'AIR'                                                 123
         AG_EXNAME = 'MATERIAL'                                           123
         AG_PARLIST = 'NONE'                                              123
         CALL AXMATERIAL                                                  123
C    *                                                                    123
C    Geant SHAPE : TUBE ===> RMIN=INR RMAX=OUTR DZ=LENGTHZ/2. <===        123
         CALL AGSRESET                                                    124
         AG_RMIN = INR                                                    124
         AG_RMAX = OUTR                                                   124
         AG_DZ = LENGTHZ/2.                                               124
         AG_TITLE = 'TUBE'                                                124
         AG_EXNAME = 'SHAPE'                                              124
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      124
         CALL AXSHAPE                                                     124
C    *                                                                    124
C    Create SUCA :                                                        125
C    Call To Local Procedure SUCA                                         125
         ASSIGN 5050 TO BLOCK_SUCA                                        125
         GO TO  5040                                                      125
5050     CONTINUE                                                         125
C    *                                                                    125
C    Geant POSITION : SUCA ===> Z=0 <===                                  126
         CALL AGSRESET                                                    126
         AG_Z = 0                                                         126
         AG_TITLE = 'SUCA'                                                126
         AG_EXNAME = 'POSITION'                                           126
         AG_PARLIST = 'Z'                                                 126
         CALL AXPOSITION                                                  126
C    *                                                                    126
C    *                                                                    126
         K=0                                                              127
            GO TO 5063                                                    127
5061        K=K+1                                                         127
5063        IF(K-(1).GT.0) GO TO 5062                                     127
            PM=1.-2*K                                                     128
C       Create SUCB :                                                     129
C       Call To Local Procedure SUCB                                      129
            ASSIGN 5080 TO BLOCK_SUCB                                     129
            GO TO  5070                                                   129
5080        CONTINUE                                                      129
C       *                                                                 129
C       Geant POSITION : SUCB ===> Z=PM*55.35 <===                        130
            CALL AGSRESET                                                 130
            AG_Z = PM*55.35                                               130
            AG_TITLE = 'SUCB'                                             130
            AG_EXNAME = 'POSITION'                                        130
            AG_PARLIST = 'Z'                                              130
            CALL AXPOSITION                                               130
C       *                                                                 130
C       Check K.EQ.0                                                      130
            IF (K.EQ.0) THEN                                              131
C       Create SUCC :                                                     132
C       Call To Local Procedure SUCC                                      132
            ASSIGN 5100 TO BLOCK_SUCC                                     132
            GO TO  5090                                                   132
5100        CONTINUE                                                      132
C       *                                                                 132
C       Geant POSITION : SUCC ===> Z=59.55 <===                           133
            CALL AGSRESET                                                 133
            AG_Z = 59.55                                                  133
            AG_TITLE = 'SUCC'                                             133
            AG_EXNAME = 'POSITION'                                        133
            AG_PARLIST = 'Z'                                              133
            CALL AXPOSITION                                               133
C       *                                                                 133
            ELSE                                                          133
C       Geant POSITION : SUCC ===> Z=-59.55 ALPHAX=180. <===              135
            CALL AGSRESET                                                 135
            AG_Z = -59.55                                                 135
            AG_ALPHAN=AG_ALPHAN+1                                         135
            AG_ALPHAK(AG_ALPHAN)=1                                        135
            AG_ALPHAL(AG_ALPHAN)=180.                                     135
            AG_TITLE = 'SUCC'                                             135
            AG_EXNAME = 'POSITION'                                        135
            AG_PARLIST = 'Z_ALPHAX'                                       135
            CALL AXPOSITION                                               135
C       *                                                                 135
            ENDIF                                                         135
C       Create SUCD :                                                     137
C       Call To Local Procedure SUCD                                      137
            ASSIGN 5120 TO BLOCK_SUCD                                     137
            GO TO  5110                                                   137
5120        CONTINUE                                                      137
C       *                                                                 137
C       Geant POSITION : SUCD ===> Z=PM*63.41 <===                        138
            CALL AGSRESET                                                 138
            AG_Z = PM*63.41                                               138
            AG_TITLE = 'SUCD'                                             138
            AG_EXNAME = 'POSITION'                                        138
            AG_PARLIST = 'Z'                                              138
            CALL AXPOSITION                                               138
C       *                                                                 138
C       Create SUCE :                                                     139
C       Call To Local Procedure SUCE                                      139
            ASSIGN 5140 TO BLOCK_SUCE                                     139
            GO TO  5130                                                   139
5140        CONTINUE                                                      139
C       *                                                                 139
C       Geant POSITION : SUCE ===> Z=PM*144.52 <===                       140
            CALL AGSRESET                                                 140
            AG_Z = PM*144.52                                              140
            AG_TITLE = 'SUCE'                                             140
            AG_EXNAME = 'POSITION'                                        140
            AG_PARLIST = 'Z'                                              140
            CALL AXPOSITION                                               140
C       *                                                                 140
C       Create SUCF :                                                     141
C       Call To Local Procedure SUCF                                      141
            ASSIGN 5160 TO BLOCK_SUCF                                     141
            GO TO  5150                                                   141
5160        CONTINUE                                                      141
C       *                                                                 141
C       Geant POSITION : SUCF ===> Z=PM*224.52 <===                       142
            CALL AGSRESET                                                 142
            AG_Z = PM*224.52                                              142
            AG_TITLE = 'SUCF'                                             142
            AG_EXNAME = 'POSITION'                                        142
            AG_PARLIST = 'Z'                                              142
            CALL AXPOSITION                                               142
C       *                                                                 142
C       Create SUCG :                                                     143
C       Call To Local Procedure SUCG                                      143
            ASSIGN 5180 TO BLOCK_SUCG                                     143
            GO TO  5170                                                   143
5180        CONTINUE                                                      143
C       *                                                                 143
C       Geant POSITION : SUCG ===> Z=PM*225.4 <===                        144
            CALL AGSRESET                                                 144
            AG_Z = PM*225.4                                               144
            AG_TITLE = 'SUCG'                                             144
            AG_EXNAME = 'POSITION'                                        144
            AG_PARLIST = 'Z'                                              144
            CALL AXPOSITION                                               144
C       *                                                                 144
         GO TO 5061                                                       145
5062     CONTINUE                                                         145
C    *                                                                    146
         M=0                                                              146
            GO TO 5193                                                    146
5191        M=M+1                                                         146
5193        IF(M-(1).GT.0) GO TO 5192                                     146
            ANGRES = IDSG_ANGFLAT - IDSG_DANGRES/2.                       147
C       Check M.EQ.1                                                      148
            IF (M.EQ.1) THEN                                              148
            ANGRES = IDSG_ANGFLAT+IDSG_DANGRES/2.                         149
            ENDIF                                                         150
C       Create TPRT :                                                     151
C       Call To Local Procedure TPRT                                      151
            ASSIGN 5210 TO BLOCK_TPRT                                     151
            GO TO  5200                                                   151
5210        CONTINUE                                                      151
C       *                                                                 151
C       Geant POSITION : TPRT ===> X=IDSG_RRRES*COS(ANGRES/180.*3.1416)   153
C       Y=IDSG_RRRES*SIN(ANGRES/180.*3.1416) Z=0 <===                     153
            CALL AGSRESET                                                 153
            AG_X = IDSG_RRRES*COS(ANGRES/180.*3.1416)                     153
            AG_Y = IDSG_RRRES*SIN(ANGRES/180.*3.1416)                     153
            AG_Z = 0                                                      153
            AG_TITLE = 'TPRT'                                             153
            AG_EXNAME = 'POSITION'                                        153
            AG_PARLIST = 'X_Y_Z'                                          153
            CALL AXPOSITION                                               153
C       *                                                                 153
         GO TO 5191                                                       154
5192     CONTINUE                                                         154
C    Create TPRR :                                                        155
C    Call To Local Procedure TPRR                                         155
         ASSIGN 5230 TO BLOCK_TPRR                                        155
         GO TO  5220                                                      155
5230     CONTINUE                                                         155
C    *                                                                    155
C    Geant POSITION : TPRR ===> X=IDSG_RRRES*COSA Y=IDSG_RRRES*SINA Z=0   156
C    ALPHAZ=IDSG_ANGFLAT <===                                             156
         CALL AGSRESET                                                    156
         AG_X = IDSG_RRRES*COSA                                           156
         AG_Y = IDSG_RRRES*SINA                                           156
         AG_Z = 0                                                         156
         AG_ALPHAN=AG_ALPHAN+1                                            156
         AG_ALPHAK(AG_ALPHAN)=3                                           156
         AG_ALPHAL(AG_ALPHAN)=IDSG_ANGFLAT                                156
         AG_TITLE = 'TPRR'                                                156
         AG_EXNAME = 'POSITION'                                           156
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      156
         CALL AXPOSITION                                                  156
C    *                                                                    156
C    *                                                                    156
         M=0                                                              157
            GO TO 5243                                                    157
5241        M=M+1                                                         157
5243        IF(M-(1).GT.0) GO TO 5242                                     157
            ANGRAIL=16./180.*3.1416                                       158
C       Check M.EQ.1                                                      159
            IF (M.EQ.1) THEN                                              159
            ANGRAIL = ANGRAIL+3.1416                                      160
            ENDIF                                                         161
C       Create FGRL :                                                     162
C       Call To Local Procedure FGRL                                      162
            ASSIGN 5260 TO BLOCK_FGRL                                     162
            GO TO  5250                                                   162
5260        CONTINUE                                                      162
C       *                                                                 162
C       Geant POSITION : FGRL ===> X=RRAIL*COS(ANGRAIL) Y=RRAIL*SIN(ANGR  163
C       AIL) Z=146.57 <===                                                163
            CALL AGSRESET                                                 163
            AG_X = RRAIL*COS(ANGRAIL)                                     163
            AG_Y = RRAIL*SIN(ANGRAIL)                                     163
            AG_Z = 146.57                                                 163
            AG_TITLE = 'FGRL'                                             163
            AG_EXNAME = 'POSITION'                                        163
            AG_PARLIST = 'X_Y_Z'                                          163
            CALL AXPOSITION                                               163
C       *                                                                 163
         GO TO 5241                                                       164
5242     CONTINUE                                                         164
C    *                                                                    165
         M=1                                                              165
            GO TO 5273                                                    165
5271        M=M+1                                                         165
5273        IF(M-(4).GT.0) GO TO 5272                                     165
            ANGFGTCBL=-90./180.*3.1416                                    166
C       *                                                                 167
            K=1                                                           167
               GO TO 5283                                                 167
5281           K=K+1                                                      167
5283           IF(K-(IDSG_FGTNDISK).GT.0) GO TO 5282                      167
C          Skip Unless (IDSG_VERSION.EQ.2.0).OR.(K.EQ.1).OR.(M.LE.3)      168
               IF                                                         168
     *         (.NOT.((IDSG_VERSION.EQ.2.0).OR.(K.EQ.1).OR.(M.LE.3)))GO   168
     *         TO 5281                                                    168
               PM=1                                                       169
C          Check (K.EQ.1).OR.(K.EQ.3).OR.(K.EQ.5)                         170
               IF ((K.EQ.1).OR.(K.EQ.3).OR.(K.EQ.5)) THEN                 170
               PM=-1                                                      171
               ENDIF                                                      172
C          *                                                              173
               J=K                                                        173
                  GO TO 5293                                              173
5291              J=J+1                                                   173
5293              IF(J-(16).GT.0) GO TO 5292                              173
C             Create FGHV :                                               174
C             Call To Local Procedure FGHV                                174
                  ASSIGN 5310 TO BLOCK_FGHV                               174
                  GO TO  5300                                             174
5310              CONTINUE                                                174
C             *                                                           174
C             Geant POSITION : FGHV ===> X=(RRAIL+0.5*PM)*COS(ANGFGTCBL+  177
C             (DPHIHV*(M+2.2*K-PM*.8))) Y=(RRAIL+0.5*PM)*SIN(ANGFGTCBL+(  177
C             DPHIHV*(M+2.2*K - PM*.8))) Z=IDSG_FGTSTARTZ+IDSG_FGTDISKST  177
C             EPZ*(J-0.5) <===                                            177
                  CALL AGSRESET                                           177
                  AG_X =                                                  177
     *            (RRAIL+0.5*PM)*COS(ANGFGTCBL+(DPHIHV*(M+2.2*K-PM*.8)))  177
                  AG_Y = (RRAIL+0.5*PM)*SIN(ANGFGTCBL+(DPHIHV*(M+2.2*K    177
     *            - PM*.8)))                                              177
                  AG_Z = IDSG_FGTSTARTZ+IDSG_FGTDISKSTEPZ*(J-0.5)         177
                  AG_TITLE = 'FGHV'                                       177
                  AG_EXNAME = 'POSITION'                                  177
                  AG_PARLIST = 'X_Y_Z'                                    177
                  CALL AXPOSITION                                         177
C             *                                                           177
               GO TO 5291                                                 178
5292           CONTINUE                                                   178
            GO TO 5281                                                    179
5282        CONTINUE                                                      179
         GO TO 5271                                                       180
5272     CONTINUE                                                         180
5021  CALL AGSPOPB                                                        181
      GO TO BLOCK_IDSM, (5020 ,5030)                                      181
C End of Local Procedure                                                  181
C ----------------------------------------------------------------------  182
C ------------ SUCA                                                       182
C Begin Local Procedure SUCA                                              183
C SUCA CENTRAL CFIBER TUBE                                                183
5040  CONTINUE                                                            183
         AG_TITLE='SUCA'                                                  183
         AG_MODULE=MODULE                                                 183
         AG_MARK='V00'                                                    183
         CALL AGSPUSH                                                     183
         AG_MODULE=MODULE                                                 183
C    *                                                                    183
C    Geant ATTRIBUTE : SUCA ===> SEEN=1 COLO=6 <===                       184
         CALL AGSRESET                                                    184
         AG_SEEN = 1                                                      184
         AG_COLO = 6                                                      184
         AG_TITLE = 'SUCA'                                                184
         AG_EXNAME = 'ATTRIBUTE'                                          184
         AG_PARLIST = 'SEEN_COLO'                                         184
         CALL AXATTRIBUTE                                                 184
C    *                                                                    184
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          184
         CALL AGSRESET                                                    185
         AG_TITLE = 'CFRPMIX'                                             185
         AG_EXNAME = 'MATERIAL'                                           185
         AG_PARLIST = 'NONE'                                              185
         CALL AXMATERIAL                                                  185
C    *                                                                    185
C    Geant SHAPE : TUBE ===> RMIN=21.5 RMAX=21.6 DZ=112./2. <===          185
         CALL AGSRESET                                                    186
         AG_RMIN = 21.5                                                   186
         AG_RMAX = 21.6                                                   186
         AG_DZ = 112./2.                                                  186
         AG_TITLE = 'TUBE'                                                186
         AG_EXNAME = 'SHAPE'                                              186
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      186
         CALL AXSHAPE                                                     186
C    *                                                                    186
5041  CALL AGSPOPB                                                        187
      GO TO BLOCK_SUCA, (5040 ,5050)                                      187
C End of Local Procedure                                                  187
C ----------------------------------------------------------------------  188
C ------------ SUCB                                                       188
C Begin Local Procedure SUCB                                              189
C SUCB SMALL ALU RING AT CENTRAL TUBE                                     189
5070  CONTINUE                                                            189
         AG_TITLE='SUCB'                                                  189
         AG_MODULE=MODULE                                                 189
         AG_MARK='V00'                                                    189
         CALL AGSPUSH                                                     189
         AG_MODULE=MODULE                                                 189
C    *                                                                    189
C    Geant ATTRIBUTE : SUCB ===> SEEN=1 COLO=1 <===                       190
         CALL AGSRESET                                                    190
         AG_SEEN = 1                                                      190
         AG_COLO = 1                                                      190
         AG_TITLE = 'SUCB'                                                190
         AG_EXNAME = 'ATTRIBUTE'                                          190
         AG_PARLIST = 'SEEN_COLO'                                         190
         CALL AXATTRIBUTE                                                 190
C    *                                                                    190
C    Geant MATERIAL : SUCBMIX ===> inherited arguments used <===          190
         CALL AGSRESET                                                    191
         AG_TITLE = 'SUCBMIX'                                             191
         AG_EXNAME = 'MATERIAL'                                           191
         AG_PARLIST = 'NONE'                                              191
         CALL AXMATERIAL                                                  191
C    *                                                                    191
C    Geant SHAPE : TUBE ===> RMIN=21.6 RMAX=22.4 DZ=1.3/2. <===           191
         CALL AGSRESET                                                    192
         AG_RMIN = 21.6                                                   192
         AG_RMAX = 22.4                                                   192
         AG_DZ = 1.3/2.                                                   192
         AG_TITLE = 'TUBE'                                                192
         AG_EXNAME = 'SHAPE'                                              192
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      192
         CALL AXSHAPE                                                     192
C    *                                                                    192
5071  CALL AGSPOPB                                                        193
      GO TO BLOCK_SUCB, (5070 ,5080)                                      193
C End of Local Procedure                                                  193
C ----------------------------------------------------------------------  194
C ------------ SUCC                                                       194
C Begin Local Procedure SUCC                                              195
C SUCC CFIBER CONE SECTION                                                195
5090  CONTINUE                                                            195
         AG_TITLE='SUCC'                                                  195
         AG_MODULE=MODULE                                                 195
         AG_MARK='V00'                                                    195
         CALL AGSPUSH                                                     195
         AG_MODULE=MODULE                                                 195
C    *                                                                    195
C    Geant ATTRIBUTE : SUCC ===> SEEN=1 COLO=6 <===                       196
         CALL AGSRESET                                                    196
         AG_SEEN = 1                                                      196
         AG_COLO = 6                                                      196
         AG_TITLE = 'SUCC'                                                196
         AG_EXNAME = 'ATTRIBUTE'                                          196
         AG_PARLIST = 'SEEN_COLO'                                         196
         CALL AXATTRIBUTE                                                 196
C    *                                                                    196
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          196
         CALL AGSRESET                                                    197
         AG_TITLE = 'CFRPMIX'                                             197
         AG_EXNAME = 'MATERIAL'                                           197
         AG_PARLIST = 'NONE'                                              197
         CALL AXMATERIAL                                                  197
C    *                                                                    197
C    Geant SHAPE : CONE ===> DZ=7.1/2. RMN1=21.6 RMX1=22.75, RMN2=38.75   197
C    RMX2=39.9 <===                                                       197
         CALL AGSRESET                                                    199
         AG_DZ = 7.1/2.                                                   199
         AG_RMN1 = 21.6                                                   199
         AG_RMX1 = 22.75                                                  199
         AG_RMN2 = 38.75                                                  199
         AG_RMX2 = 39.9                                                   199
         AG_TITLE = 'CONE'                                                199
         AG_EXNAME = 'SHAPE'                                              199
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            199
         CALL AXSHAPE                                                     199
C    *                                                                    199
5091  CALL AGSPOPB                                                        200
      GO TO BLOCK_SUCC, (5090 ,5100)                                      200
C End of Local Procedure                                                  200
C ----------------------------------------------------------------------  201
C ------------ SUCD                                                       201
C Begin Local Procedure SUCD                                              202
C SUCD LARGE ALU RING AT CONE                                             202
5110  CONTINUE                                                            202
         AG_TITLE='SUCD'                                                  202
         AG_MODULE=MODULE                                                 202
         AG_MARK='V00'                                                    202
         CALL AGSPUSH                                                     202
         AG_MODULE=MODULE                                                 202
C    *                                                                    202
C    Geant ATTRIBUTE : SUCD ===> SEEN=1 COLO=1 <===                       203
         CALL AGSRESET                                                    203
         AG_SEEN = 1                                                      203
         AG_COLO = 1                                                      203
         AG_TITLE = 'SUCD'                                                203
         AG_EXNAME = 'ATTRIBUTE'                                          203
         AG_PARLIST = 'SEEN_COLO'                                         203
         CALL AXATTRIBUTE                                                 203
C    *                                                                    203
C    Geant MATERIAL : SUCDMIX ===> inherited arguments used <===          203
         CALL AGSRESET                                                    204
         AG_TITLE = 'SUCDMIX'                                             204
         AG_EXNAME = 'MATERIAL'                                           204
         AG_PARLIST = 'NONE'                                              204
         CALL AXMATERIAL                                                  204
C    *                                                                    204
C    Geant SHAPE : TUBE ===> RMIN=38.6 RMAX=39.9 DZ=0.62/2. <===          204
         CALL AGSRESET                                                    205
         AG_RMIN = 38.6                                                   205
         AG_RMAX = 39.9                                                   205
         AG_DZ = 0.62/2.                                                  205
         AG_TITLE = 'TUBE'                                                205
         AG_EXNAME = 'SHAPE'                                              205
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      205
         CALL AXSHAPE                                                     205
C    *                                                                    205
5111  CALL AGSPOPB                                                        206
      GO TO BLOCK_SUCD, (5110 ,5120)                                      206
C End of Local Procedure                                                  206
C ----------------------------------------------------------------------  207
C ------------ SUCE                                                       207
C Begin Local Procedure SUCE                                              208
C SUCE EAST OR WEST CFIBER TUBE                                           208
5130  CONTINUE                                                            208
         AG_TITLE='SUCE'                                                  208
         AG_MODULE=MODULE                                                 208
         AG_MARK='V00'                                                    208
         CALL AGSPUSH                                                     208
         AG_MODULE=MODULE                                                 208
C    *                                                                    208
C    Geant ATTRIBUTE : SUCE ===> SEEN=1 COLO=6 <===                       209
         CALL AGSRESET                                                    209
         AG_SEEN = 1                                                      209
         AG_COLO = 6                                                      209
         AG_TITLE = 'SUCE'                                                209
         AG_EXNAME = 'ATTRIBUTE'                                          209
         AG_PARLIST = 'SEEN_COLO'                                         209
         CALL AXATTRIBUTE                                                 209
C    *                                                                    209
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          209
         CALL AGSRESET                                                    210
         AG_TITLE = 'CFRPMIX'                                             210
         AG_EXNAME = 'MATERIAL'                                           210
         AG_PARLIST = 'NONE'                                              210
         CALL AXMATERIAL                                                  210
C    *                                                                    210
C    Geant SHAPE : TUBE ===> RMIN=39.8 RMAX=39.9 DZ=161.6/2. <===         210
         CALL AGSRESET                                                    211
         AG_RMIN = 39.8                                                   211
         AG_RMAX = 39.9                                                   211
         AG_DZ = 161.6/2.                                                 211
         AG_TITLE = 'TUBE'                                                211
         AG_EXNAME = 'SHAPE'                                              211
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      211
         CALL AXSHAPE                                                     211
C    *                                                                    211
5131  CALL AGSPOPB                                                        212
      GO TO BLOCK_SUCE, (5130 ,5140)                                      212
C End of Local Procedure                                                  212
C ----------------------------------------------------------------------  213
C ------------ SUCF                                                       213
C Begin Local Procedure SUCF                                              214
C SUCF LARGE ALU RING AT THE END OF WEST CYLINDER                         214
5150  CONTINUE                                                            214
         AG_TITLE='SUCF'                                                  214
         AG_MODULE=MODULE                                                 214
         AG_MARK='V00'                                                    214
         CALL AGSPUSH                                                     214
         AG_MODULE=MODULE                                                 214
C    *                                                                    214
C    Geant ATTRIBUTE : SUCF ===> SEEN=1 COLO=1 <===                       215
         CALL AGSRESET                                                    215
         AG_SEEN = 1                                                      215
         AG_COLO = 1                                                      215
         AG_TITLE = 'SUCF'                                                215
         AG_EXNAME = 'ATTRIBUTE'                                          215
         AG_PARLIST = 'SEEN_COLO'                                         215
         CALL AXATTRIBUTE                                                 215
C    *                                                                    215
C    Geant MATERIAL : ALPURE ===> inherited arguments used <===           215
         CALL AGSRESET                                                    216
         AG_TITLE = 'ALPURE'                                              216
         AG_EXNAME = 'MATERIAL'                                           216
         AG_PARLIST = 'NONE'                                              216
         CALL AXMATERIAL                                                  216
C    *                                                                    216
C    Geant SHAPE : TUBE ===> RMIN=40. RMAX=40.5 DZ=1.6/2. <===            216
         CALL AGSRESET                                                    217
         AG_RMIN = 40.                                                    217
         AG_RMAX = 40.5                                                   217
         AG_DZ = 1.6/2.                                                   217
         AG_TITLE = 'TUBE'                                                217
         AG_EXNAME = 'SHAPE'                                              217
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      217
         CALL AXSHAPE                                                     217
C    *                                                                    217
5151  CALL AGSPOPB                                                        218
      GO TO BLOCK_SUCF, (5150 ,5160)                                      218
C End of Local Procedure                                                  218
C ----------------------------------------------------------------------  219
C ------------ SUCG                                                       219
C Begin Local Procedure SUCG                                              220
C SUCG LARGE ALU END DISK                                                 220
5170  CONTINUE                                                            220
         AG_TITLE='SUCG'                                                  220
         AG_MODULE=MODULE                                                 220
         AG_MARK='V00'                                                    220
         CALL AGSPUSH                                                     220
         AG_MODULE=MODULE                                                 220
C    *                                                                    220
C    Geant ATTRIBUTE : SUCG ===> SEEN=1 COLO=1 <===                       221
         CALL AGSRESET                                                    221
         AG_SEEN = 1                                                      221
         AG_COLO = 1                                                      221
         AG_TITLE = 'SUCG'                                                221
         AG_EXNAME = 'ATTRIBUTE'                                          221
         AG_PARLIST = 'SEEN_COLO'                                         221
         CALL AXATTRIBUTE                                                 221
C    *                                                                    221
C    Geant MATERIAL : ALPURE ===> inherited arguments used <===           221
         CALL AGSRESET                                                    222
         AG_TITLE = 'ALPURE'                                              222
         AG_EXNAME = 'MATERIAL'                                           222
         AG_PARLIST = 'NONE'                                              222
         CALL AXMATERIAL                                                  222
C    *                                                                    222
C    Geant SHAPE : TUBE ===> RMIN=10.3 RMAX=40.5 DZ=0.16/2. <===          222
         CALL AGSRESET                                                    223
         AG_RMIN = 10.3                                                   223
         AG_RMAX = 40.5                                                   223
         AG_DZ = 0.16/2.                                                  223
         AG_TITLE = 'TUBE'                                                223
         AG_EXNAME = 'SHAPE'                                              223
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      223
         CALL AXSHAPE                                                     223
C    *                                                                    223
5171  CALL AGSPOPB                                                        224
      GO TO BLOCK_SUCG, (5170 ,5180)                                      224
C End of Local Procedure                                                  224
C ----------------------------------------------------------------------  225
C ------------ TPRR                                                       225
C Begin Local Procedure TPRR                                              226
C TPRR TPC RESISTOR                                                       226
5220  CONTINUE                                                            226
         AG_TITLE='TPRR'                                                  226
         AG_MODULE=MODULE                                                 226
         AG_MARK='V00'                                                    226
         CALL AGSPUSH                                                     226
         AG_MODULE=MODULE                                                 226
C    *                                                                    226
C    Geant ATTRIBUTE : TPRR ===> SEEN=1 COLO=2 <===                       227
         CALL AGSRESET                                                    227
         AG_SEEN = 1                                                      227
         AG_COLO = 2                                                      227
         AG_TITLE = 'TPRR'                                                227
         AG_EXNAME = 'ATTRIBUTE'                                          227
         AG_PARLIST = 'SEEN_COLO'                                         227
         CALL AXATTRIBUTE                                                 227
C    *                                                                    227
C    Geant MATERIAL : ALUMINA ===> inherited arguments used <===          227
         CALL AGSRESET                                                    228
         AG_TITLE = 'ALUMINA'                                             228
         AG_EXNAME = 'MATERIAL'                                           228
         AG_PARLIST = 'NONE'                                              228
         CALL AXMATERIAL                                                  228
C    *                                                                    228
C    Geant SHAPE : BOX ===> DX=IDSG_DXRES/2 DY=IDSG_DYRES/2., DZ=LENGTHZ  228
C    /2. <===                                                             228
         CALL AGSRESET                                                    230
         AG_DX = IDSG_DXRES/2                                             230
         AG_DY = IDSG_DYRES/2.                                            230
         AG_DZ = LENGTHZ/2.                                               230
         AG_TITLE = 'BOX'                                                 230
         AG_EXNAME = 'SHAPE'                                              230
         AG_PARLIST = 'DX_DY_DZ'                                          230
         CALL AXSHAPE                                                     230
C    *                                                                    230
5221  CALL AGSPOPB                                                        231
      GO TO BLOCK_TPRR, (5220 ,5230)                                      231
C End of Local Procedure                                                  231
C ----------------------------------------------------------------------  232
C ------------ TPRT                                                       232
C Begin Local Procedure TPRT                                              233
C TPRT RESISTOR PROTECTION, CARBON FIBER                                  233
5200  CONTINUE                                                            233
         AG_TITLE='TPRT'                                                  233
         AG_MODULE=MODULE                                                 233
         AG_MARK='V00'                                                    233
         CALL AGSPUSH                                                     233
         AG_MODULE=MODULE                                                 233
C    *                                                                    233
C    Geant ATTRIBUTE : TPRT ===> SEEN=1 COLO=3 <===                       234
         CALL AGSRESET                                                    234
         AG_SEEN = 1                                                      234
         AG_COLO = 3                                                      234
         AG_TITLE = 'TPRT'                                                234
         AG_EXNAME = 'ATTRIBUTE'                                          234
         AG_PARLIST = 'SEEN_COLO'                                         234
         CALL AXATTRIBUTE                                                 234
C    *                                                                    234
C    Geant MATERIAL : FR4 ===> inherited arguments used <===              234
         CALL AGSRESET                                                    235
         AG_TITLE = 'FR4'                                                 235
         AG_EXNAME = 'MATERIAL'                                           235
         AG_PARLIST = 'NONE'                                              235
         CALL AXMATERIAL                                                  235
C    *                                                                    235
C    Geant SHAPE : TUBE ===> RMIN=IDSG_R1RES RMAX=IDSG_R2RES, DZ=LENGTHZ  235
C    /2. <===                                                             235
         CALL AGSRESET                                                    237
         AG_RMIN = IDSG_R1RES                                             237
         AG_RMAX = IDSG_R2RES                                             237
         AG_DZ = LENGTHZ/2.                                               237
         AG_TITLE = 'TUBE'                                                237
         AG_EXNAME = 'SHAPE'                                              237
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      237
         CALL AXSHAPE                                                     237
C    *                                                                    237
5201  CALL AGSPOPB                                                        238
      GO TO BLOCK_TPRT, (5200 ,5210)                                      238
C End of Local Procedure                                                  238
C ----------------------------------------------------------------------  239
C ------------ FGRL                                                       239
C Begin Local Procedure FGRL                                              240
C FGRL FGT RAIL                                                           240
5250  CONTINUE                                                            240
         AG_TITLE='FGRL'                                                  240
         AG_MODULE=MODULE                                                 240
         AG_MARK='V00'                                                    240
         CALL AGSPUSH                                                     240
         AG_MODULE=MODULE                                                 240
C    *                                                                    240
C    Geant ATTRIBUTE : FGRL ===> SEEN=1 COLO=2 <===                       241
         CALL AGSRESET                                                    241
         AG_SEEN = 1                                                      241
         AG_COLO = 2                                                      241
         AG_TITLE = 'FGRL'                                                241
         AG_EXNAME = 'ATTRIBUTE'                                          241
         AG_PARLIST = 'SEEN_COLO'                                         241
         CALL AXATTRIBUTE                                                 241
C    *                                                                    241
C    Geant MATERIAL : RAILMIX ===> inherited arguments used <===          241
         CALL AGSRESET                                                    242
         AG_TITLE = 'RAILMIX'                                             242
         AG_EXNAME = 'MATERIAL'                                           242
         AG_PARLIST = 'NONE'                                              242
         CALL AXMATERIAL                                                  242
C    *                                                                    242
C    Geant SHAPE : TUBE ===> RMIN=0.9 RMAX=1.0 DZ=165.6/2. <===           242
         CALL AGSRESET                                                    243
         AG_RMIN = 0.9                                                    243
         AG_RMAX = 1.0                                                    243
         AG_DZ = 165.6/2.                                                 243
         AG_TITLE = 'TUBE'                                                243
         AG_EXNAME = 'SHAPE'                                              243
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      243
         CALL AXSHAPE                                                     243
C    *                                                                    243
5251  CALL AGSPOPB                                                        244
      GO TO BLOCK_FGRL, (5250 ,5260)                                      244
C End of Local Procedure                                                  244
C ----------------------------------------------------------------------  245
C ------------ FGHV                                                       245
C Begin Local Procedure FGHV                                              246
C FGHV FGT CABLES MIXTURE                                                 246
5300  CONTINUE                                                            246
         AG_TITLE='FGHV'                                                  246
         AG_MODULE=MODULE                                                 246
         AG_MARK='V00'                                                    246
         CALL AGSPUSH                                                     246
         AG_MODULE=MODULE                                                 246
C    *                                                                    246
C    Geant ATTRIBUTE : FGHV ===> SEEN=1 COLO=1 <===                       247
         CALL AGSRESET                                                    247
         AG_SEEN = 1                                                      247
         AG_COLO = 1                                                      247
         AG_TITLE = 'FGHV'                                                247
         AG_EXNAME = 'ATTRIBUTE'                                          247
         AG_PARLIST = 'SEEN_COLO'                                         247
         CALL AXATTRIBUTE                                                 247
C    *                                                                    247
C    Geant MATERIAL : CABLEMIX ===> inherited arguments used <===         247
         CALL AGSRESET                                                    248
         AG_TITLE = 'CABLEMIX'                                            248
         AG_EXNAME = 'MATERIAL'                                           248
         AG_PARLIST = 'NONE'                                              248
         CALL AXMATERIAL                                                  248
C    *                                                                    248
C    Geant SHAPE : TUBE ===> RMIN=0. RMAX=0.43 DZ=IDSG_FGTDISKSTEPZ/2. <  248
C    ===                                                                  248
         CALL AGSRESET                                                    249
         AG_RMIN = 0.                                                     249
         AG_RMAX = 0.43                                                   249
         AG_DZ = IDSG_FGTDISKSTEPZ/2.                                     249
         AG_TITLE = 'TUBE'                                                249
         AG_EXNAME = 'SHAPE'                                              249
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      249
         CALL AXSHAPE                                                     249
C    *                                                                    249
5301  CALL AGSPOPB                                                        250
      GO TO BLOCK_FGHV, (5300 ,5310)                                      250
C End of Local Procedure                                                  250
      WRITE(LOUT,5320)'===>>  [End AgML Module IDSMGEO1]  <<==='          250
5320  FORMAT(A32,/,/)                                                     250
C MODULE IDSMGEO1                                                         252
1     CONTINUE                                                            252
      END                                                                 252
