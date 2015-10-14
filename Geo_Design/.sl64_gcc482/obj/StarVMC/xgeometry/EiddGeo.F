C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE EIDDGEO                                                  2
C EIDDGEO IS THE ETTIE (ENDCAP TRD-TOF FOR IDING ELECTRON) DETECTOR, OF   2
C STAR                                                                    2
      IMPLICIT NONE                                                       2
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       2
      DATA MODULE/'EIDDGEO'/, BANK_TITLE/'*'/                             2
      DATA MCOMMENT/'IS THE ETTIE (ENDCAP TRD-TOF FOR IDING ELECTRON) DE  2
     *TECTOR, OF STAR'/                                                   2
      LOGICAL FIRST                                                       2
      DATA FIRST/.TRUE./                                                  2
      SAVE FIRST                                                          2
      INTEGER JUMPAD                                                      2
      EXTERNAL JUMPAD                                                     2
      INTEGER OK                                                          2
      PARAMETER (OK=0)                                                    2
      INTEGER AG_I,AG_J                                                   2
      SAVE AG_I,AG_J                                                      2
C AUTHOR MING SHAO                                                        2
      DATA AG_I/0/,AG_J/0/                                                3
      CHARACTER*40 AUTHOR /'MING SHAO'/                                   3
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  3
C CREATED 28 SEPTEMBER 2011                                               3
      CHARACTER*40 CREATED /'28 SEPTEMBER 2011'/                          4
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  4
C - Advanced Geant interface                                              5
      CHARACTER*20 AG_MODULE,AG_TITLE,AG_EXNAME,AG_PARLIST,AG_CHDIR,      5
     *AG_MATERIAL,AG_MIXTURE,AG_COMPONENT,AG_MEDIUM                       5
      CHARACTER*4 AG_VOLUME,AG_MOTHER,AG_SHAPE,AG_CNICK,AG_KONLY,         5
     *AG_OPTION,AG_ORT,AG_MARK                                            5
      INTEGER AG_BEGCOM,AG_IVOLUME,AG_IMOTHER,AG_IGNUM,AG_ISHAPE,         5
     *AG_IMED,AG_IMAT,AG_IFIELD, AG_NLMAT,AG_IERROR,AG_NWBUF,AG_NPAR,     5
     *AG_ISTATUS,AG_IROT,AG_NBITS,AG_SERIAL, AG_ATTRIBUTE(6),AG_WORK,     5
     *AG_SEEN,AG_LSTY,AG_LWID,AG_COLO,AG_FILL,AG_JDU,AG_ORTI, AG_NDIV,    5
     *AG_IAXIS,AG_NDVMAX,AG_IPRIN,AG_RESET1,AG_RESET2,AG_BEGSCR,          5
     *AG_ENDSCR, AG_IDTYPE,AG_ISET,AG_IDET,AG_ISVOL,AG_LEVEL,AG_IRESER,   5
     *AG_IGEOM,AG_STANDALONE, AG_LSTACK,AG_NWUHIT,AG_NWUVOL,AG_MAGIC,     5
     *AG_LDETU,AG_NPDIV,AG_NPDV,AG_NZ,AG_NCOPY, AG_IDEBU,AG_IGRAP,        5
     *AG_IHIST,AG_IMFLD,AG_ISIMU,AG_ENDSAVE,AG_CODE,AG_TRKTYP, AG_MODE,   5
     *AG_ECODE,AG_PDG,IPRIN, AG_ALPHAN,AG_ALPHAK(10),IGOR                 5
      REAL AG_FIELDM,AG_TMAXFD,AG_STEMAX,AG_DEEMAX,AG_EPSIL,AG_STMIN,     5
     *AG_DENS,AG_RADL,AG_ABSL, AG_THETAX,AG_PHIX,AG_THETAY,AG_PHIY,       5
     *AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,AG_ALFAZ, AG_ALPHAL(10),        5
     *AG_ALPHAX,AG_ALPHAY,AG_ALPHAZ,AG_TYPE, AG_DX, AG_DX1, AG_DX2,       5
     *AG_DY, AG_DY1, AG_DY2, AG_DMAXMS, AG_TWIST, AG_THET, AG_THE1,       5
     *AG_THE2, AG_PHI, AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2,       5
     *AG_RMIN, AG_RMAX, AG_RMN, AG_RMX, AG_ZI , AG_RMN1, AG_RMN2,         5
     *AG_RMX1, AG_RMX2, AG_H1, AG_H2, AG_BL1, AG_BL2, AG_TL1, AG_TL2,     5
     *AG_DPHI, AG_DZ, AG_TWIS, AG_X, AG_Y, AG_Z, AG_A, AG_ZA, AG_W,       5
     *AG_STEP, AG_C0, AG_PAR, AG_AA, AG_ZZ, AG_WW, AG_LX, AG_LY, AG_LZ,   5
     *AG_HX, AG_HY, AG_HZ, AG_P1, AG_P2, AG_STACK,AG_UBUF,AG_XHMAX,       5
     *AG_YHMAX,AG_ZHMAX,AG_RHMAX,AG_FHMAX,AG_FHMIN,AG_BIN, AG_CHARGE,     5
     *AG_MASS,AG_TLIFE,AG_BRATIO                                          5
      PARAMETER (AG_LSTACK=130,AG_NWUHIT=10,AG_NWUVOL=3,AG_MAGIC=         5
     *-696969,AG_LDETU=250)                                               5
      COMMON/AGCGLOB/ AG_MODULE,AG_CHDIR,AG_LEVEL,AG_IDTYPE,AG_IERROR,    5
     *AG_STANDALONE,IPRIN, AG_IPRIN,AG_IGEOM,AG_IDEBU,AG_IGRAP,AG_IHIST,  5
     *AG_IMFLD,AG_ISIMU                                                   5
C Inherited variables a: saved during internal calls                      5
C b: reset at the entry in a block and then saved                         5
      COMMON/AGCPARA/ AG_BEGCOM, AG_IVOLUME, AG_IMOTHER, AG_IGNUM,        5
     *AG_ISHAPE, AG_IMED, AG_IMAT, AG_IFIELD, AG_FIELDM, AG_TMAXFD,       5
     *AG_STEMAX, AG_DEEMAX, AG_EPSIL, AG_STMIN, AG_DENS, AG_RADL,         5
     *AG_ABSL, AG_DX, AG_DX1, AG_DX2, AG_DY, AG_DY1, AG_DY2, AG_RMN1,     5
     *AG_RMN2, AG_RMX1, AG_RMX2, AG_THET, AG_THE1, AG_THE2, AG_PHI,       5
     *AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2, AG_H1, AG_BL1,         5
     *AG_TL1, AG_H2, AG_BL2, AG_TL2, AG_RMIN, AG_RMAX, AG_DPHI,           5
     *AG_NPDIV, AG_NZ, AG_DZ, AG_TWIS, AG_LX, AG_LY, AG_LZ, AG_HX,        5
     *AG_HY, AG_HZ, AG_A, AG_ZA, AG_W, AG_NLMAT, AG_WORK, AG_SEEN,        5
     *AG_LSTY, AG_LWID, AG_COLO, AG_FILL, AG_SERIAL, AG_ISVOL,            5
     *AG_ISTATUS, AG_ZI(16), AG_RMN(16), AG_RMX(16), AG_VOLUME,           5
     *AG_MOTHER, AG_SHAPE, AG_CNICK, AG_ENDSAVE, AG_RESET1, AG_THETAX,    5
     *AG_PHIX,AG_THETAY,AG_PHIY,AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,      5
     *AG_ALFAZ, AG_X,AG_Y,AG_Z, AG_STEP,AG_C0,AG_NDIV,AG_IAXIS,           5
     *AG_NDVMAX,AG_ORTI,AG_NCOPY, AG_ALPHAN,AG_ALPHAK,AG_ALPHAL,          5
     *AG_RESET2, AG_KONLY, AG_ORT, AG_MARK                                5
C local variables valid inside same block                                 5
      COMMON/AGCLOCA/ AG_BEGSCR,AG_UBUF(100),AG_PAR(100),AG_AA(20),       5
     *AG_ZZ(20),AG_WW(20),AG_NWBUF, AG_XHMAX,AG_YHMAX,AG_ZHMAX,AG_RHMAX,  5
     *AG_FHMAX,AG_FHMIN,AG_NBITS,AG_BIN,AG_TYPE, AG_IROT,AG_NPAR,         5
     *AG_ISET,AG_IDET,AG_JDU,AG_IRESER, AG_ENDSCR, AG_TITLE,AG_EXNAME,    5
     *AG_PARLIST,AG_MATERIAL,AG_MIXTURE, AG_COMPONENT,AG_MEDIUM,          5
     *AG_OPTION                                                           5
      COMMON/AGCPART/AG_CODE,AG_TRKTYP,AG_MASS,AG_CHARGE,AG_TLIFE,        5
     *AG_BRATIO(6),AG_MODE(6),AG_PDG,AG_ECODE                             5
      COMMON/AGCSTAC/ AG_STACK(AG_LSTACK,15)                              5
C temporarly                                                              5
      EQUIVALENCE (AG_ATTRIBUTE,AG_WORK),(AG_STEMAX,AG_DMAXMS),           5
     *(AG_ALPHAX,AG_ALFAX), (AG_ALPHAY,AG_ALFAY),(AG_ALPHAZ,AG_ALFAZ),    5
     *(AG_TWIST,AG_TWIS),(AG_NPDIV,AG_NPDV),  (AG_P1,AG_HX),(AG_P2,       5
     *AG_HY)                                                              5
      REAL MIL_P,INCH_P                                                   5
      PARAMETER (INCH_P=2.54,MIL_P=0.00254)                               5
C - GEANT description of logical units                                    5
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            5
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      5
      COMMON/GCMAIL/ CHMAIL                                               5
      CHARACTER*132 CHMAIL                                                5
C - Natural constants (or considered as such)                             5
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   5
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        5
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       5
      INTEGER BLOCK_ETTV                                                  5
      INTEGER BLOCK_ETRV                                                  7
      INTEGER BLOCK_ETFV                                                  7
      INTEGER BLOCK_ECLV                                                  7
      INTEGER BLOCK_ETOF                                                  7
      INTEGER BLOCK_TECL                                                  7
      INTEGER BLOCK_ETRD                                                  7
      INTEGER BLOCK_TBOX                                                  7
      INTEGER BLOCK_TGAS                                                  7
      INTEGER BLOCK_TPCB                                                  7
      INTEGER BLOCK_TGLA                                                  7
      INTEGER BLOCK_TGAP                                                  7
      INTEGER BLOCK_TEAA                                                  7
      INTEGER BLOCK_TEAB                                                  7
      INTEGER BLOCK_SCIN                                                  7
      INTEGER BLOCK_TRAD                                                  7
      INTEGER BLOCK_TABS                                                  7
      INTEGER BLOCK_TBOD                                                  7
C -                                                                       8
      REAL EIDV_VERSION                                                   8
      EQUIVALENCE (BANK_EIDV(3),EIDV_VERSION,RE_EIDV_VERSION),            8
     *(NAMES_EIDV(1),NEIDV_VERSION)                                       8
      EQUIVALENCE (COMMEN_EIDV(1),CEIDV_VERSION),(MAP_EIDV(1,1),          8
     *MEIDV_VERSION)                                                      8
      CHARACTER CEIDV_VERSION*40/' '/                                     8
      CHARACTER NEIDV_VERSION*12/'R.VERSION'/                             8
      INTEGER MEIDV_VERSION(2)/1,1/                                       8
      REAL RE_EIDV_VERSION                                                8
C -                                                                       8
      REAL EIDV_EIDCONFIG                                                 8
      EQUIVALENCE (BANK_EIDV(4),EIDV_EIDCONFIG,RE_EIDV_EIDCONFIG),        8
     *(NAMES_EIDV(2),NEIDV_EIDCONFIG)                                     8
      EQUIVALENCE (COMMEN_EIDV(2),CEIDV_EIDCONFIG),(MAP_EIDV(1,2),        8
     *MEIDV_EIDCONFIG)                                                    8
      CHARACTER CEIDV_EIDCONFIG*40/' '/                                   8
      CHARACTER NEIDV_EIDCONFIG*12/'R.EIDCONFIG'/                         8
      INTEGER MEIDV_EIDCONFIG(2)/1,1/                                     8
      REAL RE_EIDV_EIDCONFIG                                              8
C -                                                                       8
      INTEGER LEN_EIDV,LLEN_EIDV,LINK_EIDV                                8
      PARAMETER (LEN_EIDV=4)                                              8
      PARAMETER (LLEN_EIDV=2)                                             8
      INTEGER FLAG_EIDV/0/,MAP_EIDV(2,LLEN_EIDV),IDNUM_EIDV(10)           8
      CHARACTER NAMES_EIDV(LLEN_EIDV)*12,COMMEN_EIDV(LLEN_EIDV)*40        8
      DATA IDNUM_EIDV/10*0/                                               8
      REAL BANK_EIDV(LEN_EIDV)                                            8
      CHARACTER*60 BPATH_EIDV/' '/                                        8
      SAVE FLAG_EIDV,MAP_EIDV,IDNUM_EIDV,NAMES_EIDV,COMMEN_EIDV,          8
     *BPATH_EIDV                                                          8
      DATA LINK_EIDV/0/,BANK_EIDV/LEN_EIDV*0/                             8
C *                                                                       8
C -                                                                       11
      REAL EIDG_VERSION                                                   11
      EQUIVALENCE (BANK_EIDG(3),EIDG_VERSION,RE_EIDG_VERSION),            11
     *(NAMES_EIDG(1),NEIDG_VERSION)                                       11
      EQUIVALENCE (COMMEN_EIDG(1),CEIDG_VERSION),(MAP_EIDG(1,1),          11
     *MEIDG_VERSION)                                                      11
      CHARACTER CEIDG_VERSION*40/' '/                                     11
      CHARACTER NEIDG_VERSION*12/'R.VERSION'/                             11
      INTEGER MEIDG_VERSION(2)/1,1/                                       11
      REAL RE_EIDG_VERSION                                                11
C -                                                                       11
      REAL EIDG_ETTZ                                                      11
      EQUIVALENCE (BANK_EIDG(4),EIDG_ETTZ,RE_EIDG_ETTZ),(NAMES_EIDG(2),   11
     *NEIDG_ETTZ)                                                         11
      EQUIVALENCE (COMMEN_EIDG(2),CEIDG_ETTZ),(MAP_EIDG(1,2),             11
     *MEIDG_ETTZ)                                                         11
      CHARACTER CEIDG_ETTZ*40/' '/                                        11
      CHARACTER NEIDG_ETTZ*12/'R.ETTZ'/                                   11
      INTEGER MEIDG_ETTZ(2)/1,1/                                          11
      REAL RE_EIDG_ETTZ                                                   11
C -                                                                       11
      REAL EIDG_ETRVZ                                                     11
      EQUIVALENCE (BANK_EIDG(5),EIDG_ETRVZ,RE_EIDG_ETRVZ),                11
     *(NAMES_EIDG(3),NEIDG_ETRVZ)                                         11
      EQUIVALENCE (COMMEN_EIDG(3),CEIDG_ETRVZ),(MAP_EIDG(1,3),            11
     *MEIDG_ETRVZ)                                                        11
      CHARACTER CEIDG_ETRVZ*40/' '/                                       11
      CHARACTER NEIDG_ETRVZ*12/'R.ETRVZ'/                                 11
      INTEGER MEIDG_ETRVZ(2)/1,1/                                         11
      REAL RE_EIDG_ETRVZ                                                  11
C -                                                                       11
      REAL EIDG_ETFVZ                                                     11
      EQUIVALENCE (BANK_EIDG(6),EIDG_ETFVZ,RE_EIDG_ETFVZ),                11
     *(NAMES_EIDG(4),NEIDG_ETFVZ)                                         11
      EQUIVALENCE (COMMEN_EIDG(4),CEIDG_ETFVZ),(MAP_EIDG(1,4),            11
     *MEIDG_ETFVZ)                                                        11
      CHARACTER CEIDG_ETFVZ*40/' '/                                       11
      CHARACTER NEIDG_ETFVZ*12/'R.ETFVZ'/                                 11
      INTEGER MEIDG_ETFVZ(2)/1,1/                                         11
      REAL RE_EIDG_ETFVZ                                                  11
C -                                                                       11
      REAL EIDG_ECLVZ                                                     11
      EQUIVALENCE (BANK_EIDG(7),EIDG_ECLVZ,RE_EIDG_ECLVZ),                11
     *(NAMES_EIDG(5),NEIDG_ECLVZ)                                         11
      EQUIVALENCE (COMMEN_EIDG(5),CEIDG_ECLVZ),(MAP_EIDG(1,5),            11
     *MEIDG_ECLVZ)                                                        11
      CHARACTER CEIDG_ECLVZ*40/' '/                                       11
      CHARACTER NEIDG_ECLVZ*12/'R.ECLVZ'/                                 11
      INTEGER MEIDG_ECLVZ(2)/1,1/                                         11
      REAL RE_EIDG_ECLVZ                                                  11
C -                                                                       11
      REAL EIDG_RMIN                                                      11
      EQUIVALENCE (BANK_EIDG(8),EIDG_RMIN,RE_EIDG_RMIN),(NAMES_EIDG(6),   11
     *NEIDG_RMIN)                                                         11
      EQUIVALENCE (COMMEN_EIDG(6),CEIDG_RMIN),(MAP_EIDG(1,6),             11
     *MEIDG_RMIN)                                                         11
      CHARACTER CEIDG_RMIN*40/' '/                                        11
      CHARACTER NEIDG_RMIN*12/'R.RMIN'/                                   11
      INTEGER MEIDG_RMIN(2)/1,1/                                          11
      REAL RE_EIDG_RMIN                                                   11
C -                                                                       11
      REAL EIDG_RMAX                                                      11
      EQUIVALENCE (BANK_EIDG(9),EIDG_RMAX,RE_EIDG_RMAX),(NAMES_EIDG(7),   11
     *NEIDG_RMAX)                                                         11
      EQUIVALENCE (COMMEN_EIDG(7),CEIDG_RMAX),(MAP_EIDG(1,7),             11
     *MEIDG_RMAX)                                                         11
      CHARACTER CEIDG_RMAX*40/' '/                                        11
      CHARACTER NEIDG_RMAX*12/'R.RMAX'/                                   11
      INTEGER MEIDG_RMAX(2)/1,1/                                          11
      REAL RE_EIDG_RMAX                                                   11
C -                                                                       11
      REAL EIDG_DPHI                                                      11
      EQUIVALENCE (BANK_EIDG(10),EIDG_DPHI,RE_EIDG_DPHI),(NAMES_EIDG(8),  11
     *NEIDG_DPHI)                                                         11
      EQUIVALENCE (COMMEN_EIDG(8),CEIDG_DPHI),(MAP_EIDG(1,8),             11
     *MEIDG_DPHI)                                                         11
      CHARACTER CEIDG_DPHI*40/' '/                                        11
      CHARACTER NEIDG_DPHI*12/'R.DPHI'/                                   11
      INTEGER MEIDG_DPHI(2)/1,1/                                          11
      REAL RE_EIDG_DPHI                                                   11
C -                                                                       11
      REAL EIDG_ETTTHICK                                                  11
      EQUIVALENCE (BANK_EIDG(11),EIDG_ETTTHICK,RE_EIDG_ETTTHICK),         11
     *(NAMES_EIDG(9),NEIDG_ETTTHICK)                                      11
      EQUIVALENCE (COMMEN_EIDG(9),CEIDG_ETTTHICK),(MAP_EIDG(1,9),         11
     *MEIDG_ETTTHICK)                                                     11
      CHARACTER CEIDG_ETTTHICK*40/' '/                                    11
      CHARACTER NEIDG_ETTTHICK*12/'R.ETTTHICK'/                           11
      INTEGER MEIDG_ETTTHICK(2)/1,1/                                      11
      REAL RE_EIDG_ETTTHICK                                               11
C -                                                                       11
      REAL EIDG_TFVTHICK                                                  11
      EQUIVALENCE (BANK_EIDG(12),EIDG_TFVTHICK,RE_EIDG_TFVTHICK),         11
     *(NAMES_EIDG(10),NEIDG_TFVTHICK)                                     11
      EQUIVALENCE (COMMEN_EIDG(10),CEIDG_TFVTHICK),(MAP_EIDG(1,10),       11
     *MEIDG_TFVTHICK)                                                     11
      CHARACTER CEIDG_TFVTHICK*40/' '/                                    11
      CHARACTER NEIDG_TFVTHICK*12/'R.TFVTHICK'/                           11
      INTEGER MEIDG_TFVTHICK(2)/1,1/                                      11
      REAL RE_EIDG_TFVTHICK                                               11
C -                                                                       11
      REAL EIDG_TOFTHICK                                                  11
      EQUIVALENCE (BANK_EIDG(13),EIDG_TOFTHICK,RE_EIDG_TOFTHICK),         11
     *(NAMES_EIDG(11),NEIDG_TOFTHICK)                                     11
      EQUIVALENCE (COMMEN_EIDG(11),CEIDG_TOFTHICK),(MAP_EIDG(1,11),       11
     *MEIDG_TOFTHICK)                                                     11
      CHARACTER CEIDG_TOFTHICK*40/' '/                                    11
      CHARACTER NEIDG_TOFTHICK*12/'R.TOFTHICK'/                           11
      INTEGER MEIDG_TOFTHICK(2)/1,1/                                      11
      REAL RE_EIDG_TOFTHICK                                               11
C -                                                                       11
      REAL EIDG_TBXTHICK                                                  11
      EQUIVALENCE (BANK_EIDG(14),EIDG_TBXTHICK,RE_EIDG_TBXTHICK),         11
     *(NAMES_EIDG(12),NEIDG_TBXTHICK)                                     11
      EQUIVALENCE (COMMEN_EIDG(12),CEIDG_TBXTHICK),(MAP_EIDG(1,12),       11
     *MEIDG_TBXTHICK)                                                     11
      CHARACTER CEIDG_TBXTHICK*40/' '/                                    11
      CHARACTER NEIDG_TBXTHICK*12/'R.TBXTHICK'/                           11
      INTEGER MEIDG_TBXTHICK(2)/1,1/                                      11
      REAL RE_EIDG_TBXTHICK                                               11
C -                                                                       11
      REAL EIDG_TPBTHICK                                                  11
      EQUIVALENCE (BANK_EIDG(15),EIDG_TPBTHICK,RE_EIDG_TPBTHICK),         11
     *(NAMES_EIDG(13),NEIDG_TPBTHICK)                                     11
      EQUIVALENCE (COMMEN_EIDG(13),CEIDG_TPBTHICK),(MAP_EIDG(1,13),       11
     *MEIDG_TPBTHICK)                                                     11
      CHARACTER CEIDG_TPBTHICK*40/' '/                                    11
      CHARACTER NEIDG_TPBTHICK*12/'R.TPBTHICK'/                           11
      INTEGER MEIDG_TPBTHICK(2)/1,1/                                      11
      REAL RE_EIDG_TPBTHICK                                               11
C -                                                                       11
      REAL EIDG_TGLTHICK                                                  11
      EQUIVALENCE (BANK_EIDG(16),EIDG_TGLTHICK,RE_EIDG_TGLTHICK),         11
     *(NAMES_EIDG(14),NEIDG_TGLTHICK)                                     11
      EQUIVALENCE (COMMEN_EIDG(14),CEIDG_TGLTHICK),(MAP_EIDG(1,14),       11
     *MEIDG_TGLTHICK)                                                     11
      CHARACTER CEIDG_TGLTHICK*40/' '/                                    11
      CHARACTER NEIDG_TGLTHICK*12/'R.TGLTHICK'/                           11
      INTEGER MEIDG_TGLTHICK(2)/1,1/                                      11
      REAL RE_EIDG_TGLTHICK                                               11
C -                                                                       11
      REAL EIDG_TGPTHICK                                                  11
      EQUIVALENCE (BANK_EIDG(17),EIDG_TGPTHICK,RE_EIDG_TGPTHICK),         11
     *(NAMES_EIDG(15),NEIDG_TGPTHICK)                                     11
      EQUIVALENCE (COMMEN_EIDG(15),CEIDG_TGPTHICK),(MAP_EIDG(1,15),       11
     *MEIDG_TGPTHICK)                                                     11
      CHARACTER CEIDG_TGPTHICK*40/' '/                                    11
      CHARACTER NEIDG_TGPTHICK*12/'R.TGPTHICK'/                           11
      INTEGER MEIDG_TGPTHICK(2)/1,1/                                      11
      REAL RE_EIDG_TGPTHICK                                               11
C -                                                                       11
      REAL EIDG_ELVTHICK                                                  11
      EQUIVALENCE (BANK_EIDG(18),EIDG_ELVTHICK,RE_EIDG_ELVTHICK),         11
     *(NAMES_EIDG(16),NEIDG_ELVTHICK)                                     11
      EQUIVALENCE (COMMEN_EIDG(16),CEIDG_ELVTHICK),(MAP_EIDG(1,16),       11
     *MEIDG_ELVTHICK)                                                     11
      CHARACTER CEIDG_ELVTHICK*40/' '/                                    11
      CHARACTER NEIDG_ELVTHICK*12/'R.ELVTHICK'/                           11
      INTEGER MEIDG_ELVTHICK(2)/1,1/                                      11
      REAL RE_EIDG_ELVTHICK                                               11
C -                                                                       11
      REAL EIDG_ECLTHICK                                                  11
      EQUIVALENCE (BANK_EIDG(19),EIDG_ECLTHICK,RE_EIDG_ECLTHICK),         11
     *(NAMES_EIDG(17),NEIDG_ECLTHICK)                                     11
      EQUIVALENCE (COMMEN_EIDG(17),CEIDG_ECLTHICK),(MAP_EIDG(1,17),       11
     *MEIDG_ECLTHICK)                                                     11
      CHARACTER CEIDG_ECLTHICK*40/' '/                                    11
      CHARACTER NEIDG_ECLTHICK*12/'R.ECLTHICK'/                           11
      INTEGER MEIDG_ECLTHICK(2)/1,1/                                      11
      REAL RE_EIDG_ECLTHICK                                               11
C -                                                                       11
      REAL EIDG_EAATHICK                                                  11
      EQUIVALENCE (BANK_EIDG(20),EIDG_EAATHICK,RE_EIDG_EAATHICK),         11
     *(NAMES_EIDG(18),NEIDG_EAATHICK)                                     11
      EQUIVALENCE (COMMEN_EIDG(18),CEIDG_EAATHICK),(MAP_EIDG(1,18),       11
     *MEIDG_EAATHICK)                                                     11
      CHARACTER CEIDG_EAATHICK*40/' '/                                    11
      CHARACTER NEIDG_EAATHICK*12/'R.EAATHICK'/                           11
      INTEGER MEIDG_EAATHICK(2)/1,1/                                      11
      REAL RE_EIDG_EAATHICK                                               11
C -                                                                       11
      REAL EIDG_EABTHICK                                                  11
      EQUIVALENCE (BANK_EIDG(21),EIDG_EABTHICK,RE_EIDG_EABTHICK),         11
     *(NAMES_EIDG(19),NEIDG_EABTHICK)                                     11
      EQUIVALENCE (COMMEN_EIDG(19),CEIDG_EABTHICK),(MAP_EIDG(1,19),       11
     *MEIDG_EABTHICK)                                                     11
      CHARACTER CEIDG_EABTHICK*40/' '/                                    11
      CHARACTER NEIDG_EABTHICK*12/'R.EABTHICK'/                           11
      INTEGER MEIDG_EABTHICK(2)/1,1/                                      11
      REAL RE_EIDG_EABTHICK                                               11
C -                                                                       11
      REAL EIDG_SCNTHICK                                                  11
      EQUIVALENCE (BANK_EIDG(22),EIDG_SCNTHICK,RE_EIDG_SCNTHICK),         11
     *(NAMES_EIDG(20),NEIDG_SCNTHICK)                                     11
      EQUIVALENCE (COMMEN_EIDG(20),CEIDG_SCNTHICK),(MAP_EIDG(1,20),       11
     *MEIDG_SCNTHICK)                                                     11
      CHARACTER CEIDG_SCNTHICK*40/' '/                                    11
      CHARACTER NEIDG_SCNTHICK*12/'R.SCNTHICK'/                           11
      INTEGER MEIDG_SCNTHICK(2)/1,1/                                      11
      REAL RE_EIDG_SCNTHICK                                               11
C -                                                                       11
      REAL EIDG_TRVTHICK                                                  11
      EQUIVALENCE (BANK_EIDG(23),EIDG_TRVTHICK,RE_EIDG_TRVTHICK),         11
     *(NAMES_EIDG(21),NEIDG_TRVTHICK)                                     11
      EQUIVALENCE (COMMEN_EIDG(21),CEIDG_TRVTHICK),(MAP_EIDG(1,21),       11
     *MEIDG_TRVTHICK)                                                     11
      CHARACTER CEIDG_TRVTHICK*40/' '/                                    11
      CHARACTER NEIDG_TRVTHICK*12/'R.TRVTHICK'/                           11
      INTEGER MEIDG_TRVTHICK(2)/1,1/                                      11
      REAL RE_EIDG_TRVTHICK                                               11
C -                                                                       11
      REAL EIDG_TRDTHICK                                                  11
      EQUIVALENCE (BANK_EIDG(24),EIDG_TRDTHICK,RE_EIDG_TRDTHICK),         11
     *(NAMES_EIDG(22),NEIDG_TRDTHICK)                                     11
      EQUIVALENCE (COMMEN_EIDG(22),CEIDG_TRDTHICK),(MAP_EIDG(1,22),       11
     *MEIDG_TRDTHICK)                                                     11
      CHARACTER CEIDG_TRDTHICK*40/' '/                                    11
      CHARACTER NEIDG_TRDTHICK*12/'R.TRDTHICK'/                           11
      INTEGER MEIDG_TRDTHICK(2)/1,1/                                      11
      REAL RE_EIDG_TRDTHICK                                               11
C -                                                                       11
      REAL EIDG_TRATHICK                                                  11
      EQUIVALENCE (BANK_EIDG(25),EIDG_TRATHICK,RE_EIDG_TRATHICK),         11
     *(NAMES_EIDG(23),NEIDG_TRATHICK)                                     11
      EQUIVALENCE (COMMEN_EIDG(23),CEIDG_TRATHICK),(MAP_EIDG(1,23),       11
     *MEIDG_TRATHICK)                                                     11
      CHARACTER CEIDG_TRATHICK*40/' '/                                    11
      CHARACTER NEIDG_TRATHICK*12/'R.TRATHICK'/                           11
      INTEGER MEIDG_TRATHICK(2)/1,1/                                      11
      REAL RE_EIDG_TRATHICK                                               11
C -                                                                       11
      REAL EIDG_TABTHICK                                                  11
      EQUIVALENCE (BANK_EIDG(26),EIDG_TABTHICK,RE_EIDG_TABTHICK),         11
     *(NAMES_EIDG(24),NEIDG_TABTHICK)                                     11
      EQUIVALENCE (COMMEN_EIDG(24),CEIDG_TABTHICK),(MAP_EIDG(1,24),       11
     *MEIDG_TABTHICK)                                                     11
      CHARACTER CEIDG_TABTHICK*40/' '/                                    11
      CHARACTER NEIDG_TABTHICK*12/'R.TABTHICK'/                           11
      INTEGER MEIDG_TABTHICK(2)/1,1/                                      11
      REAL RE_EIDG_TABTHICK                                               11
C -                                                                       11
      REAL EIDG_TBDTHICK                                                  11
      EQUIVALENCE (BANK_EIDG(27),EIDG_TBDTHICK,RE_EIDG_TBDTHICK),         11
     *(NAMES_EIDG(25),NEIDG_TBDTHICK)                                     11
      EQUIVALENCE (COMMEN_EIDG(25),CEIDG_TBDTHICK),(MAP_EIDG(1,25),       11
     *MEIDG_TBDTHICK)                                                     11
      CHARACTER CEIDG_TBDTHICK*40/' '/                                    11
      CHARACTER NEIDG_TBDTHICK*12/'R.TBDTHICK'/                           11
      INTEGER MEIDG_TBDTHICK(2)/1,1/                                      11
      REAL RE_EIDG_TBDTHICK                                               11
C -                                                                       11
      INTEGER LEN_EIDG,LLEN_EIDG,LINK_EIDG                                11
      PARAMETER (LEN_EIDG=27)                                             11
      PARAMETER (LLEN_EIDG=25)                                            11
      INTEGER FLAG_EIDG/0/,MAP_EIDG(2,LLEN_EIDG),IDNUM_EIDG(10)           11
      CHARACTER NAMES_EIDG(LLEN_EIDG)*12,COMMEN_EIDG(LLEN_EIDG)*40        11
      DATA IDNUM_EIDG/10*0/                                               11
      REAL BANK_EIDG(LEN_EIDG)                                            11
      CHARACTER*60 BPATH_EIDG/' '/                                        11
      SAVE FLAG_EIDG,MAP_EIDG,IDNUM_EIDG,NAMES_EIDG,COMMEN_EIDG,          11
     *BPATH_EIDG                                                          11
      DATA LINK_EIDG/0/,BANK_EIDG/LEN_EIDG*0/                             11
C *                                                                       11
      REAL *4 ZPOSTOF,ZPOSBOX,ZPOSGAS,ZPOSPCB,ZPOSGLA,ZPOSGAP,ZPOSEAA,    13
     *ZPOSEAB,ZPOSSCIN, ZPOSECL                                           13
      REAL *4 ZPOSETT,ZPOSTRV,ZPOSTFV,ZPOSELV,ZPOSTRD,ZPOSRAD,ZPOSABS,    15
     *ZPOSBOD                                                             15
      REAL *4 PHI0                                                        17
      INTEGER *4 I_PHI0,I_PCB,I_GLASS,I_GAP,I_TRDLYR                      19
C *                                                                       21
      CALL AGSBEGM(MODULE,IPRIN)                                          21
      IF (IPRIN.LT.0) RETURN                                              21
C Filling bank EIDV - EID CONFIGURATION CONTROL                           21
      BANK_TITLE='EID CONFIGURATION CONTROL'                              21
      BPATH_EIDV='EIDV*'                                                  21
      IDNUM_EIDV(1)=0                                                     22
C VERSION                                                                 22
      EIDV_VERSION=1                                                      22
      CEIDV_VERSION='VERSION'                                             22
C EID CONFIGURATION                                                       23
      EIDV_EIDCONFIG=1                                                    23
      CEIDV_EIDCONFIG='EID CONFIGURATION'                                 23
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EIDV',    24
     *BPATH_EIDV,IDNUM_EIDV,1, MAP_EIDV,NAMES_EIDV,COMMEN_EIDV,           24
     *BANK_EIDV,LEN_EIDV,LLEN_EIDV,LINK_EIDV,FLAG_EIDV)                   24
C *                                                                       24
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EIDV',    24
     *BPATH_EIDV,IDNUM_EIDV,1, MAP_EIDV,NAMES_EIDV,COMMEN_EIDV,           24
     *BANK_EIDV,LEN_EIDV,LLEN_EIDV,LINK_EIDV,FLAG_EIDV)                   24
C *                                                                       25
      CALL AGSBEGM(MODULE,IPRIN)                                          25
      IF (IPRIN.LT.0) RETURN                                              25
C Filling bank EIDG - EID BASIC GEOMETRY DIMENSIONS                       25
      BANK_TITLE='EID BASIC GEOMETRY DIMENSIONS'                          25
      BPATH_EIDG='EIDG*'                                                  25
      IDNUM_EIDG(1)=0                                                     26
C GEOMETRY VERSION                                                        26
      EIDG_VERSION=1                                                      26
      CEIDG_VERSION='GEOMETRY VERSION'                                    26
C Z POSITION OF ETTIE DETECTOR                                            27
      EIDG_ETTZ=286.00                                                    27
      CEIDG_ETTZ='Z POSITION OF ETTIE DETECTOR'                           27
C Z POSITION OF ETRD                                                      28
      EIDG_ETRVZ=280.00                                                   28
      CEIDG_ETRVZ='Z POSITION OF ETRD'                                    28
C Z POSITION ETOF                                                         29
      EIDG_ETFVZ=299.00                                                   29
      CEIDG_ETFVZ='Z POSITION ETOF'                                       29
C Z POSITION TECL                                                         30
      EIDG_ECLVZ=305.00                                                   30
      CEIDG_ECLVZ='Z POSITION TECL'                                       30
C MOTHERVOLUME RMIN (TPC INNER RADIUS)                                    31
      EIDG_RMIN=46.107                                                    31
      CEIDG_RMIN='MOTHERVOLUME RMIN (TPC INNER RADIUS)'                   31
C MOTHERVOLUME RMIN (TPC OUTER RADIUS)                                    32
      EIDG_RMAX=206.75                                                    32
      CEIDG_RMAX='MOTHERVOLUME RMIN (TPC OUTER RADIUS)'                   32
C SECTOR PHI ACCEPTAN (FOLLOW TPC SECTORS)                                33
      EIDG_DPHI=28.0                                                      33
      CEIDG_DPHI='SECTOR PHI ACCEPTAN (FOLLOW TPC SECTORS)'               33
C ETTIE VOLUME THICK                                                      34
      EIDG_ETTTHICK=48.000                                                34
      CEIDG_ETTTHICK='ETTIE VOLUME THICK'                                 34
C ETFV VOLUME THICK                                                       35
      EIDG_TFVTHICK=2.0000                                                35
      CEIDG_TFVTHICK='ETFV VOLUME THICK'                                  35
C ETOF TOTAL THICK                                                        36
      EIDG_TOFTHICK=1.5202                                                36
      CEIDG_TOFTHICK='ETOF TOTAL THICK'                                   36
C ETOF BOX THICK                                                          37
      EIDG_TBXTHICK=0.300                                                 37
      CEIDG_TBXTHICK='ETOF BOX THICK'                                     37
C ETOF PCB THICK                                                          38
      EIDG_TPBTHICK=0.150                                                 38
      CEIDG_TPBTHICK='ETOF PCB THICK'                                     38
C ETOF GLASS THICK                                                        39
      EIDG_TGLTHICK=0.050                                                 39
      CEIDG_TGLTHICK='ETOF GLASS THICK'                                   39
C ETOF GAS GAP THICK                                                      40
      EIDG_TGPTHICK=0.022                                                 40
      CEIDG_TGPTHICK='ETOF GAS GAP THICK'                                 40
C ECLV VOLUME THICK                                                       41
      EIDG_ELVTHICK=10.00                                                 41
      CEIDG_ELVTHICK='ECLV VOLUME THICK'                                  41
C TECL TOTAL THICK                                                        42
      EIDG_ECLTHICK=8.52                                                  42
      CEIDG_ECLTHICK='TECL TOTAL THICK'                                   42
C ABSORBER 1 THICK                                                        43
      EIDG_EAATHICK=1.68                                                  43
      CEIDG_EAATHICK='ABSORBER 1 THICK'                                   43
C ABSORBER 2 THICK                                                        44
      EIDG_EABTHICK=0.84                                                  44
      CEIDG_EABTHICK='ABSORBER 2 THICK'                                   44
C SCINTILLATOR THICK                                                      45
      EIDG_SCNTHICK=3.00                                                  45
      CEIDG_SCNTHICK='SCINTILLATOR THICK'                                 45
C ETRV VOLUME THICK                                                       46
      EIDG_TRVTHICK=36.0                                                  46
      CEIDG_TRVTHICK='ETRV VOLUME THICK'                                  46
C ETRD LAYER THICK                                                        47
      EIDG_TRDTHICK=12.0                                                  47
      CEIDG_TRDTHICK='ETRD LAYER THICK'                                   47
C TR RADIATOR THICK                                                       48
      EIDG_TRATHICK=5.916                                                 48
      CEIDG_TRATHICK='TR RADIATOR THICK'                                  48
C TR ABSORBER THICK                                                       49
      EIDG_TABTHICK=3.83                                                  49
      CEIDG_TABTHICK='TR ABSORBER THICK'                                  49
C ETRD MOTHER BOARD THICK                                                 50
      EIDG_TBDTHICK=2.00                                                  50
      CEIDG_TBDTHICK='ETRD MOTHER BOARD THICK'                            50
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EIDG',    51
     *BPATH_EIDG,IDNUM_EIDG,1, MAP_EIDG,NAMES_EIDG,COMMEN_EIDG,           51
     *BANK_EIDG,LEN_EIDG,LLEN_EIDG,LINK_EIDG,FLAG_EIDG)                   51
C *                                                                       51
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'EIDG',    51
     *BPATH_EIDG,IDNUM_EIDG,1, MAP_EIDG,NAMES_EIDG,COMMEN_EIDG,           51
     *BANK_EIDG,LEN_EIDG,LLEN_EIDG,LINK_EIDG,FLAG_EIDG)                   51
C use default EIDV bank                                                   52
      BPATH_EIDV='EIDV*'                                                  52
      IDNUM_EIDV(1)=0                                                     52
      CALL AGDGETP('EIDDGEO','*',-999,'EIDV',' ',BANK_EIDV, BPATH_EIDV,   52
     *IDNUM_EIDV,1,MAP_EIDV,NAMES_EIDV,BANK_EIDV,LEN_EIDV,LLEN_EIDV,      52
     *LINK_EIDV,FLAG_EIDV)                                                52
C use EIDG bank                                                           53
      BPATH_EIDG='EIDG*'                                                  53
      IDNUM_EIDG(1)=0                                                     53
      EIDG_VERSION=EIDV_EIDCONFIG                                         53
      CALL AGDGETP('EIDDGEO','*',-999,'EIDG',NEIDG_VERSION,               53
     *RE_EIDG_VERSION,BPATH_EIDG,IDNUM_EIDG,1,MAP_EIDG,NAMES_EIDG,        53
     *BANK_EIDG,LEN_EIDG,LLEN_EIDG,LINK_EIDG,FLAG_EIDG)                   53
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             54
      CALL AGSRESET                                                       54
      AG_A = 28.08                                                        54
      AG_Z = 14                                                           54
      AG_W = 0.6*1*28./60.                                                54
      AG_TITLE = 'SI'                                                     54
      AG_EXNAME = 'COMPONENT'                                             54
      AG_PARLIST = 'A_Z_W'                                                54
      CALL AXCOMPONENT                                                    54
C *                                                                       54
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  54
C =                                                                       54
      CALL AGSRESET                                                       55
      AG_A = 16                                                           55
      AG_Z = 8                                                            55
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               55
      AG_TITLE = 'O'                                                      55
      AG_EXNAME = 'COMPONENT'                                             55
      AG_PARLIST = 'A_Z_W'                                                55
      CALL AXCOMPONENT                                                    55
C *                                                                       55
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 55
      CALL AGSRESET                                                       56
      AG_A = 12                                                           56
      AG_Z = 6                                                            56
      AG_W = 0.4*8*12./174.                                               56
      AG_TITLE = 'C'                                                      56
      AG_EXNAME = 'COMPONENT'                                             56
      AG_PARLIST = 'A_Z_W'                                                56
      CALL AXCOMPONENT                                                    56
C *                                                                       56
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  56
      CALL AGSRESET                                                       57
      AG_A = 1                                                            57
      AG_Z = 1                                                            57
      AG_W = 0.4*14*1./174.                                               57
      AG_TITLE = 'H'                                                      57
      AG_EXNAME = 'COMPONENT'                                             57
      AG_PARLIST = 'A_Z_W'                                                57
      CALL AXCOMPONENT                                                    57
C *                                                                       57
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  57
      CALL AGSRESET                                                       58
      AG_DENS = 1.7                                                       58
      AG_TITLE = 'G10'                                                    58
      AG_EXNAME = 'MIXTURE'                                               58
      AG_PARLIST = 'DENS'                                                 58
      CALL AXMIXTURE                                                      58
C *                                                                       58
C Geant COMPONENT : H ===> A=1 Z=1 W=0.90*2*1./102.+0.+0.05*10*1./58. <=  58
C ==                                                                      58
      CALL AGSRESET                                                       59
      AG_A = 1                                                            59
      AG_Z = 1                                                            59
      AG_W = 0.90*2*1./102.+0.+0.05*10*1./58.                             59
      AG_TITLE = 'H'                                                      59
      AG_EXNAME = 'COMPONENT'                                             59
      AG_PARLIST = 'A_Z_W'                                                59
      CALL AXCOMPONENT                                                    59
C *                                                                       59
C Geant COMPONENT : C ===> A=12 Z=6 W=0.90*2*12./102.+0.+0.05*4*12./58.   59
C <===                                                                    59
      CALL AGSRESET                                                       60
      AG_A = 12                                                           60
      AG_Z = 6                                                            60
      AG_W = 0.90*2*12./102.+0.+0.05*4*12./58.                            60
      AG_TITLE = 'C'                                                      60
      AG_EXNAME = 'COMPONENT'                                             60
      AG_PARLIST = 'A_Z_W'                                                60
      CALL AXCOMPONENT                                                    60
C *                                                                       60
C Geant COMPONENT : F ===> A=19 Z=9 W=0.90*4*19./102.+0.05*6*19./146.+0.  60
C  <===                                                                   60
      CALL AGSRESET                                                       61
      AG_A = 19                                                           61
      AG_Z = 9                                                            61
      AG_W = 0.90*4*19./102.+0.05*6*19./146.+0.                           61
      AG_TITLE = 'F'                                                      61
      AG_EXNAME = 'COMPONENT'                                             61
      AG_PARLIST = 'A_Z_W'                                                61
      CALL AXCOMPONENT                                                    61
C *                                                                       61
C Geant COMPONENT : S ===> A=32 Z=16 W=0.+0.05*1*32./146.+0. <===         61
      CALL AGSRESET                                                       62
      AG_A = 32                                                           62
      AG_Z = 16                                                           62
      AG_W = 0.+0.05*1*32./146.+0.                                        62
      AG_TITLE = 'S'                                                      62
      AG_EXNAME = 'COMPONENT'                                             62
      AG_PARLIST = 'A_Z_W'                                                62
      CALL AXCOMPONENT                                                    62
C *                                                                       62
C Geant MIXTURE : FREON ===> DENS=4.55E-3 <===                            62
      CALL AGSRESET                                                       63
      AG_DENS = 4.55E-3                                                   63
      AG_TITLE = 'FREON'                                                  63
      AG_EXNAME = 'MIXTURE'                                               63
      AG_PARLIST = 'DENS'                                                 63
      CALL AXMIXTURE                                                      63
C *                                                                       63
C Geant COMPONENT : SI ===> A=28 Z=14 W=1. <===                           63
      CALL AGSRESET                                                       64
      AG_A = 28                                                           64
      AG_Z = 14                                                           64
      AG_W = 1.                                                           64
      AG_TITLE = 'SI'                                                     64
      AG_EXNAME = 'COMPONENT'                                             64
      AG_PARLIST = 'A_Z_W'                                                64
      CALL AXCOMPONENT                                                    64
C *                                                                       64
C Geant COMPONENT : O ===> A=16 Z=8 W=2. <===                             64
      CALL AGSRESET                                                       65
      AG_A = 16                                                           65
      AG_Z = 8                                                            65
      AG_W = 2.                                                           65
      AG_TITLE = 'O'                                                      65
      AG_EXNAME = 'COMPONENT'                                             65
      AG_PARLIST = 'A_Z_W'                                                65
      CALL AXCOMPONENT                                                    65
C *                                                                       65
C Geant MIXTURE : GLASS ===> DENS=2.5 <===                                65
      CALL AGSRESET                                                       66
      AG_DENS = 2.5                                                       66
      AG_TITLE = 'GLASS'                                                  66
      AG_EXNAME = 'MIXTURE'                                               66
      AG_PARLIST = 'DENS'                                                 66
      CALL AXMIXTURE                                                      66
C *                                                                       66
C Geant COMPONENT : N ===> A=14.01 Z=14 W=0.008 <===                      66
      CALL AGSRESET                                                       67
      AG_A = 14.01                                                        67
      AG_Z = 14                                                           67
      AG_W = 0.008                                                        67
      AG_TITLE = 'N'                                                      67
      AG_EXNAME = 'COMPONENT'                                             67
      AG_PARLIST = 'A_Z_W'                                                67
      CALL AXCOMPONENT                                                    67
C *                                                                       67
C Geant COMPONENT : O ===> A=16 Z=8 W=0.301 <===                          67
      CALL AGSRESET                                                       68
      AG_A = 16                                                           68
      AG_Z = 8                                                            68
      AG_W = 0.301                                                        68
      AG_TITLE = 'O'                                                      68
      AG_EXNAME = 'COMPONENT'                                             68
      AG_PARLIST = 'A_Z_W'                                                68
      CALL AXCOMPONENT                                                    68
C *                                                                       68
C Geant COMPONENT : C ===> A=12 Z=6 W=0.653 <===                          68
      CALL AGSRESET                                                       69
      AG_A = 12                                                           69
      AG_Z = 6                                                            69
      AG_W = 0.653                                                        69
      AG_TITLE = 'C'                                                      69
      AG_EXNAME = 'COMPONENT'                                             69
      AG_PARLIST = 'A_Z_W'                                                69
      CALL AXCOMPONENT                                                    69
C *                                                                       69
C Geant COMPONENT : H ===> A=1 Z=1 W=0.038 <===                           69
      CALL AGSRESET                                                       70
      AG_A = 1                                                            70
      AG_Z = 1                                                            70
      AG_W = 0.038                                                        70
      AG_TITLE = 'H'                                                      70
      AG_EXNAME = 'COMPONENT'                                             70
      AG_PARLIST = 'A_Z_W'                                                70
      CALL AXCOMPONENT                                                    70
C *                                                                       70
C Geant MIXTURE : RADIATOR ===> DENS=0.10416 <===                         70
      CALL AGSRESET                                                       71
      AG_DENS = 0.10416                                                   71
      AG_TITLE = 'RADIATOR'                                               71
      AG_EXNAME = 'MIXTURE'                                               71
      AG_PARLIST = 'DENS'                                                 71
      CALL AXMIXTURE                                                      71
C *                                                                       71
C Geant COMPONENT : XE ===> A=131.29 Z=54 W=0.979 <===                    71
      CALL AGSRESET                                                       72
      AG_A = 131.29                                                       72
      AG_Z = 54                                                           72
      AG_W = 0.979                                                        72
      AG_TITLE = 'XE'                                                     72
      AG_EXNAME = 'COMPONENT'                                             72
      AG_PARLIST = 'A_Z_W'                                                72
      CALL AXCOMPONENT                                                    72
C *                                                                       72
C Geant COMPONENT : O ===> A=16 Z=8 W=0.0147 <===                         72
      CALL AGSRESET                                                       73
      AG_A = 16                                                           73
      AG_Z = 8                                                            73
      AG_W = 0.0147                                                       73
      AG_TITLE = 'O'                                                      73
      AG_EXNAME = 'COMPONENT'                                             73
      AG_PARLIST = 'A_Z_W'                                                73
      CALL AXCOMPONENT                                                    73
C *                                                                       73
C Geant COMPONENT : C ===> A=12 Z=6 W=0.0063 <===                         73
      CALL AGSRESET                                                       74
      AG_A = 12                                                           74
      AG_Z = 6                                                            74
      AG_W = 0.0063                                                       74
      AG_TITLE = 'C'                                                      74
      AG_EXNAME = 'COMPONENT'                                             74
      AG_PARLIST = 'A_Z_W'                                                74
      CALL AXCOMPONENT                                                    74
C *                                                                       74
C Geant MIXTURE : XE15CO2 ===> DENS=0.0049 <===                           74
      CALL AGSRESET                                                       75
      AG_DENS = 0.0049                                                    75
      AG_TITLE = 'XE15CO2'                                                75
      AG_EXNAME = 'MIXTURE'                                               75
      AG_PARLIST = 'DENS'                                                 75
      CALL AXMIXTURE                                                      75
C *                                                                       75
C Geant COMPONENT : C ===> A=12 Z=6 W=0.923 <===                          75
      CALL AGSRESET                                                       76
      AG_A = 12                                                           76
      AG_Z = 6                                                            76
      AG_W = 0.923                                                        76
      AG_TITLE = 'C'                                                      76
      AG_EXNAME = 'COMPONENT'                                             76
      AG_PARLIST = 'A_Z_W'                                                76
      CALL AXCOMPONENT                                                    76
C *                                                                       76
C Geant COMPONENT : H ===> A=1 Z=1 W=0.077 <===                           76
      CALL AGSRESET                                                       77
      AG_A = 1                                                            77
      AG_Z = 1                                                            77
      AG_W = 0.077                                                        77
      AG_TITLE = 'H'                                                      77
      AG_EXNAME = 'COMPONENT'                                             77
      AG_PARLIST = 'A_Z_W'                                                77
      CALL AXCOMPONENT                                                    77
C *                                                                       77
C Geant MIXTURE : POLYSTYRENE ===> DENS=1.06 <===                         77
      CALL AGSRESET                                                       78
      AG_DENS = 1.06                                                      78
      AG_TITLE = 'POLYSTYRENE'                                            78
      AG_EXNAME = 'MIXTURE'                                               78
      AG_PARLIST = 'DENS'                                                 78
      CALL AXMIXTURE                                                      78
C *                                                                       78
      CALL AGSBEGM(MODULE,IPRIN)                                          78
C Check IPRIN<0                                                           79
      IF (IPRIN.LT.0) RETURN                                              79
C Create ETTV :                                                           79
C Call To Local Procedure ETTV                                            79
      ASSIGN 5020 TO BLOCK_ETTV                                           79
      GO TO  5010                                                         79
5020  CONTINUE                                                            79
C *                                                                       79
      ZPOSETT = EIDG_ETTZ                                                 80
C Geant POSITION : ETTV ===> MOTHER='CAVE' Z=-ZPOSETT THETAZ=180 <===     81
      CALL AGSRESET                                                       81
      AG_MOTHER = 'CAVE'                                                  81
      AG_Z = -ZPOSETT                                                     81
      AG_THETAZ = 180                                                     81
      AG_TITLE = 'ETTV'                                                   81
      AG_EXNAME = 'POSITION'                                              81
      AG_PARLIST = 'MOTHER_Z_THETAZ'                                      81
      CALL AXPOSITION                                                     81
C *                                                                       81
C ----------------------------------------------------------------------  81
C ------------ ETTV                                                       81
      CALL AGSENDM                                                        83
      RETURN                                                              83
C Begin Local Procedure ETTV                                              83
C ETTV IS THE WHOLE ETTIE DETECTOR VOLUME                                 83
5010  CONTINUE                                                            83
         AG_TITLE='ETTV'                                                  83
         AG_MODULE=MODULE                                                 83
         AG_MARK='V00'                                                    83
         CALL AGSPUSH                                                     83
         AG_MODULE=MODULE                                                 83
C    *                                                                    83
C    Geant MATERIAL : AIR ===> inherited arguments used <===              84
         CALL AGSRESET                                                    84
         AG_TITLE = 'AIR'                                                 84
         AG_EXNAME = 'MATERIAL'                                           84
         AG_PARLIST = 'NONE'                                              84
         CALL AXMATERIAL                                                  84
C    *                                                                    84
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           84
         CALL AGSRESET                                                    85
         AG_TITLE = 'STANDARD'                                            85
         AG_EXNAME = 'MEDIUM'                                             85
         AG_PARLIST = 'NONE'                                              85
         CALL AXMEDIUM                                                    85
C    *                                                                    85
C    Geant ATTRIBUTE : ETTV ===> SEEN=1 COLO=6 <===                       85
         CALL AGSRESET                                                    86
         AG_SEEN = 1                                                      86
         AG_COLO = 6                                                      86
         AG_TITLE = 'ETTV'                                                86
         AG_EXNAME = 'ATTRIBUTE'                                          86
         AG_PARLIST = 'SEEN_COLO'                                         86
         CALL AXATTRIBUTE                                                 86
C    *                                                                    86
C    Geant SHAPE : TUBE ===> RMIN=EIDG_RMIN RMAX=EIDG_RMAX, DZ=EIDG_ETTT  86
C    HICK/2 <===                                                          86
         CALL AGSRESET                                                    88
         AG_RMIN = EIDG_RMIN                                              88
         AG_RMAX = EIDG_RMAX                                              88
         AG_DZ = EIDG_ETTTHICK/2                                          88
         AG_TITLE = 'TUBE'                                                88
         AG_EXNAME = 'SHAPE'                                              88
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      88
         CALL AXSHAPE                                                     88
C    *                                                                    88
         ZPOSTRV = EIDG_ETRVZ - EIDG_ETTZ                                 88
C    Create ETRV :                                                        90
C    Call To Local Procedure ETRV                                         90
         ASSIGN 5040 TO BLOCK_ETRV                                        90
         GO TO  5030                                                      90
5040     CONTINUE                                                         90
C    *                                                                    90
C    Geant POSITION : ETRV ===> MOTHER='ETTV' Z=ZPOSTRV <===              91
         CALL AGSRESET                                                    91
         AG_MOTHER = 'ETTV'                                               91
         AG_Z = ZPOSTRV                                                   91
         AG_TITLE = 'ETRV'                                                91
         AG_EXNAME = 'POSITION'                                           91
         AG_PARLIST = 'MOTHER_Z'                                          91
         CALL AXPOSITION                                                  91
C    *                                                                    91
         ZPOSTFV = EIDG_ETFVZ - EIDG_ETTZ                                 91
C    Create ETFV :                                                        93
C    Call To Local Procedure ETFV                                         93
         ASSIGN 5060 TO BLOCK_ETFV                                        93
         GO TO  5050                                                      93
5060     CONTINUE                                                         93
C    *                                                                    93
C    Geant POSITION : ETFV ===> MOTHER='ETTV' Z=ZPOSTFV <===              94
         CALL AGSRESET                                                    94
         AG_MOTHER = 'ETTV'                                               94
         AG_Z = ZPOSTFV                                                   94
         AG_TITLE = 'ETFV'                                                94
         AG_EXNAME = 'POSITION'                                           94
         AG_PARLIST = 'MOTHER_Z'                                          94
         CALL AXPOSITION                                                  94
C    *                                                                    94
         ZPOSELV = EIDG_ECLVZ - EIDG_ETTZ                                 94
C    Create ECLV :                                                        96
C    Call To Local Procedure ECLV                                         96
         ASSIGN 5080 TO BLOCK_ECLV                                        96
         GO TO  5070                                                      96
5080     CONTINUE                                                         96
C    *                                                                    96
C    Geant POSITION : ECLV ===> MOTHER='ETTV' Z=ZPOSELV <===              97
         CALL AGSRESET                                                    97
         AG_MOTHER = 'ETTV'                                               97
         AG_Z = ZPOSELV                                                   97
         AG_TITLE = 'ECLV'                                                97
         AG_EXNAME = 'POSITION'                                           97
         AG_PARLIST = 'MOTHER_Z'                                          97
         CALL AXPOSITION                                                  97
C    *                                                                    97
5011  CALL AGSPOPB                                                        98
      GO TO BLOCK_ETTV, (5010 ,5020)                                      98
C End of Local Procedure                                                  98
C ----------------------------------------------------------------------  99
C ------------ ETRV                                                       99
C Begin Local Procedure ETRV                                              100
C ETRV IS THE ENDCAP TRD DETECTOR VOLUME (3 LAYERS)                       100
5030  CONTINUE                                                            100
         AG_TITLE='ETRV'                                                  100
         AG_MODULE=MODULE                                                 100
         AG_MARK='V00'                                                    100
         CALL AGSPUSH                                                     100
         AG_MODULE=MODULE                                                 100
C    *                                                                    100
C    Geant MATERIAL : AIR ===> inherited arguments used <===              101
         CALL AGSRESET                                                    101
         AG_TITLE = 'AIR'                                                 101
         AG_EXNAME = 'MATERIAL'                                           101
         AG_PARLIST = 'NONE'                                              101
         CALL AXMATERIAL                                                  101
C    *                                                                    101
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           101
         CALL AGSRESET                                                    102
         AG_TITLE = 'STANDARD'                                            102
         AG_EXNAME = 'MEDIUM'                                             102
         AG_PARLIST = 'NONE'                                              102
         CALL AXMEDIUM                                                    102
C    *                                                                    102
C    Geant ATTRIBUTE : ETRV ===> SEEN=1 COLO=7 <===                       102
         CALL AGSRESET                                                    103
         AG_SEEN = 1                                                      103
         AG_COLO = 7                                                      103
         AG_TITLE = 'ETRV'                                                103
         AG_EXNAME = 'ATTRIBUTE'                                          103
         AG_PARLIST = 'SEEN_COLO'                                         103
         CALL AXATTRIBUTE                                                 103
C    *                                                                    103
C    Geant SHAPE : TUBE ===> RMIN=EIDG_RMIN RMAX=EIDG_RMAX, DZ=EIDG_TRVT  103
C    HICK/2 <===                                                          103
         CALL AGSRESET                                                    105
         AG_RMIN = EIDG_RMIN                                              105
         AG_RMAX = EIDG_RMAX                                              105
         AG_DZ = EIDG_TRVTHICK/2                                          105
         AG_TITLE = 'TUBE'                                                105
         AG_EXNAME = 'SHAPE'                                              105
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      105
         CALL AXSHAPE                                                     105
C    *                                                                    105
C    Create ETRD :                                                        106
C    Call To Local Procedure ETRD                                         106
         ASSIGN 5100 TO BLOCK_ETRD                                        106
         GO TO  5090                                                      106
5100     CONTINUE                                                         106
C    *                                                                    106
C    *                                                                    107
         I_TRDLYR=0                                                       107
            GO TO 5113                                                    107
5111        I_TRDLYR=I_TRDLYR+1                                           107
5113        IF(I_TRDLYR-(2).GT.0) GO TO 5112                              107
            ZPOSTRD = (I_TRDLYR-1)*EIDG_TRDTHICK                          108
C       *                                                                 109
            I_PHI0=0                                                      109
               GO TO 5123                                                 109
5121           I_PHI0=I_PHI0+1                                            109
5123           IF(I_PHI0-(11).GT.0) GO TO 5122                            109
               PHI0 = I_PHI0*30                                           110
C          Geant POSITION : ETRD ===> MOTHER='ETRV' Z=ZPOSTRD THETAX=90   111
C          PHIX=PHI0 PHIY=PHI0+90 THETAZ=0 <===                           111
               CALL AGSRESET                                              111
               AG_MOTHER = 'ETRV'                                         111
               AG_Z = ZPOSTRD                                             111
               AG_THETAX = 90                                             111
               AG_PHIX = PHI0                                             111
               AG_PHIY = PHI0+90                                          111
               AG_THETAZ = 0                                              111
               AG_TITLE = 'ETRD'                                          111
               AG_EXNAME = 'POSITION'                                     111
               AG_PARLIST = 'MOTHER_Z_THETAX_PHIX_PHIY_THETAZ'            111
               CALL AXPOSITION                                            111
C          *                                                              111
            GO TO 5121                                                    112
5122        CONTINUE                                                      112
         GO TO 5111                                                       113
5112     CONTINUE                                                         113
5031  CALL AGSPOPB                                                        114
      GO TO BLOCK_ETRV, (5030 ,5040)                                      114
C End of Local Procedure                                                  114
C ----------------------------------------------------------------------  115
C ------------ ECLV                                                       115
C Begin Local Procedure ECLV                                              116
C ECLV IS THE ENDCAP CONVERTOR-SCINTILLATOR VOLUME                        116
5070  CONTINUE                                                            116
         AG_TITLE='ECLV'                                                  116
         AG_MODULE=MODULE                                                 116
         AG_MARK='V00'                                                    116
         CALL AGSPUSH                                                     116
         AG_MODULE=MODULE                                                 116
C    *                                                                    116
C    Geant MATERIAL : AIR ===> inherited arguments used <===              117
         CALL AGSRESET                                                    117
         AG_TITLE = 'AIR'                                                 117
         AG_EXNAME = 'MATERIAL'                                           117
         AG_PARLIST = 'NONE'                                              117
         CALL AXMATERIAL                                                  117
C    *                                                                    117
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           117
         CALL AGSRESET                                                    118
         AG_TITLE = 'STANDARD'                                            118
         AG_EXNAME = 'MEDIUM'                                             118
         AG_PARLIST = 'NONE'                                              118
         CALL AXMEDIUM                                                    118
C    *                                                                    118
C    Geant ATTRIBUTE : ECLV ===> SEEN=1 COLO=8 <===                       118
         CALL AGSRESET                                                    119
         AG_SEEN = 1                                                      119
         AG_COLO = 8                                                      119
         AG_TITLE = 'ECLV'                                                119
         AG_EXNAME = 'ATTRIBUTE'                                          119
         AG_PARLIST = 'SEEN_COLO'                                         119
         CALL AXATTRIBUTE                                                 119
C    *                                                                    119
C    Geant SHAPE : TUBE ===> RMIN=EIDG_RMIN RMAX=EIDG_RMAX, DZ=EIDG_ELVT  119
C    HICK/2 <===                                                          119
         CALL AGSRESET                                                    121
         AG_RMIN = EIDG_RMIN                                              121
         AG_RMAX = EIDG_RMAX                                              121
         AG_DZ = EIDG_ELVTHICK/2                                          121
         AG_TITLE = 'TUBE'                                                121
         AG_EXNAME = 'SHAPE'                                              121
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      121
         CALL AXSHAPE                                                     121
C    *                                                                    121
C    Create TECL :                                                        122
C    Call To Local Procedure TECL                                         122
         ASSIGN 5140 TO BLOCK_TECL                                        122
         GO TO  5130                                                      122
5140     CONTINUE                                                         122
C    *                                                                    122
         ZPOSECL = 0                                                      123
C    *                                                                    124
         I_PHI0=0                                                         124
            GO TO 5153                                                    124
5151        I_PHI0=I_PHI0+1                                               124
5153        IF(I_PHI0-(11).GT.0) GO TO 5152                               124
            PHI0 = I_PHI0*30                                              125
C       Geant POSITION : TECL ===> MOTHER='ECLV' Z=ZPOSECL THETAX=90 PHI  126
C       X=PHI0 PHIY=PHI0+90 THETAZ=0 <===                                 126
            CALL AGSRESET                                                 126
            AG_MOTHER = 'ECLV'                                            126
            AG_Z = ZPOSECL                                                126
            AG_THETAX = 90                                                126
            AG_PHIX = PHI0                                                126
            AG_PHIY = PHI0+90                                             126
            AG_THETAZ = 0                                                 126
            AG_TITLE = 'TECL'                                             126
            AG_EXNAME = 'POSITION'                                        126
            AG_PARLIST = 'MOTHER_Z_THETAX_PHIX_PHIY_THETAZ'               126
            CALL AXPOSITION                                               126
C       *                                                                 126
         GO TO 5151                                                       127
5152     CONTINUE                                                         127
5071  CALL AGSPOPB                                                        128
      GO TO BLOCK_ECLV, (5070 ,5080)                                      128
C End of Local Procedure                                                  128
C ----------------------------------------------------------------------  129
C ------------ ETFV                                                       129
C Begin Local Procedure ETFV                                              130
C ETFV IS THE ENDCAP TOF VOLUME                                           130
5050  CONTINUE                                                            130
         AG_TITLE='ETFV'                                                  130
         AG_MODULE=MODULE                                                 130
         AG_MARK='V00'                                                    130
         CALL AGSPUSH                                                     130
         AG_MODULE=MODULE                                                 130
C    *                                                                    130
C    Geant MATERIAL : AIR ===> inherited arguments used <===              131
         CALL AGSRESET                                                    131
         AG_TITLE = 'AIR'                                                 131
         AG_EXNAME = 'MATERIAL'                                           131
         AG_PARLIST = 'NONE'                                              131
         CALL AXMATERIAL                                                  131
C    *                                                                    131
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           131
         CALL AGSRESET                                                    132
         AG_TITLE = 'STANDARD'                                            132
         AG_EXNAME = 'MEDIUM'                                             132
         AG_PARLIST = 'NONE'                                              132
         CALL AXMEDIUM                                                    132
C    *                                                                    132
C    Geant ATTRIBUTE : ETFV ===> SEEN=1 COLO=6 <===                       132
         CALL AGSRESET                                                    133
         AG_SEEN = 1                                                      133
         AG_COLO = 6                                                      133
         AG_TITLE = 'ETFV'                                                133
         AG_EXNAME = 'ATTRIBUTE'                                          133
         AG_PARLIST = 'SEEN_COLO'                                         133
         CALL AXATTRIBUTE                                                 133
C    *                                                                    133
C    Geant SHAPE : TUBE ===> RMIN=EIDG_RMIN RMAX=EIDG_RMAX, DZ=EIDG_TFVT  133
C    HICK/2 <===                                                          133
         CALL AGSRESET                                                    135
         AG_RMIN = EIDG_RMIN                                              135
         AG_RMAX = EIDG_RMAX                                              135
         AG_DZ = EIDG_TFVTHICK/2                                          135
         AG_TITLE = 'TUBE'                                                135
         AG_EXNAME = 'SHAPE'                                              135
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      135
         CALL AXSHAPE                                                     135
C    *                                                                    135
C    Create ETOF :                                                        136
C    Call To Local Procedure ETOF                                         136
         ASSIGN 5170 TO BLOCK_ETOF                                        136
         GO TO  5160                                                      136
5170     CONTINUE                                                         136
C    *                                                                    136
         ZPOSTOF = 0                                                      137
C    *                                                                    138
         I_PHI0=0                                                         138
            GO TO 5183                                                    138
5181        I_PHI0=I_PHI0+1                                               138
5183        IF(I_PHI0-(11).GT.0) GO TO 5182                               138
            PHI0 = I_PHI0*30                                              139
C       Geant POSITION : ETOF ===> MOTHER='ETFV' Z=ZPOSTOF THETAX=90 PHI  140
C       X=PHI0 PHIY=PHI0+90 THETAZ=0 <===                                 140
            CALL AGSRESET                                                 140
            AG_MOTHER = 'ETFV'                                            140
            AG_Z = ZPOSTOF                                                140
            AG_THETAX = 90                                                140
            AG_PHIX = PHI0                                                140
            AG_PHIY = PHI0+90                                             140
            AG_THETAZ = 0                                                 140
            AG_TITLE = 'ETOF'                                             140
            AG_EXNAME = 'POSITION'                                        140
            AG_PARLIST = 'MOTHER_Z_THETAX_PHIX_PHIY_THETAZ'               140
            CALL AXPOSITION                                               140
C       *                                                                 140
         GO TO 5181                                                       141
5182     CONTINUE                                                         141
5051  CALL AGSPOPB                                                        142
      GO TO BLOCK_ETFV, (5050 ,5060)                                      142
C End of Local Procedure                                                  142
C ----------------------------------------------------------------------  143
C ------------ ETOF                                                       143
C Begin Local Procedure ETOF                                              144
C ETOF IS THE ETOF SUPERMODULE                                            144
5160  CONTINUE                                                            144
         AG_TITLE='ETOF'                                                  144
         AG_MODULE=MODULE                                                 144
         AG_MARK='V00'                                                    144
         CALL AGSPUSH                                                     144
         AG_MODULE=MODULE                                                 144
C    *                                                                    144
C    Geant MATERIAL : AIR ===> inherited arguments used <===              145
         CALL AGSRESET                                                    145
         AG_TITLE = 'AIR'                                                 145
         AG_EXNAME = 'MATERIAL'                                           145
         AG_PARLIST = 'NONE'                                              145
         CALL AXMATERIAL                                                  145
C    *                                                                    145
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           145
         CALL AGSRESET                                                    146
         AG_TITLE = 'STANDARD'                                            146
         AG_EXNAME = 'MEDIUM'                                             146
         AG_PARLIST = 'NONE'                                              146
         CALL AXMEDIUM                                                    146
C    *                                                                    146
C    Geant ATTRIBUTE : ETOF ===> SEEN=1 COLO=8 <===                       146
         CALL AGSRESET                                                    147
         AG_SEEN = 1                                                      147
         AG_COLO = 8                                                      147
         AG_TITLE = 'ETOF'                                                147
         AG_EXNAME = 'ATTRIBUTE'                                          147
         AG_PARLIST = 'SEEN_COLO'                                         147
         CALL AXATTRIBUTE                                                 147
C    *                                                                    147
C    Geant SHAPE : TUBS ===> RMIN=EIDG_RMIN RMAX=EIDG_RMAX, PHI1=-EIDG_D  147
C    PHI/2 PHI2=EIDG_DPHI/2 DZ=EIDG_TOFTHICK/2 <===                       147
         CALL AGSRESET                                                    149
         AG_RMIN = EIDG_RMIN                                              149
         AG_RMAX = EIDG_RMAX                                              149
         AG_PHI1 = -EIDG_DPHI/2                                           149
         AG_PHI2 = EIDG_DPHI/2                                            149
         AG_DZ = EIDG_TOFTHICK/2                                          149
         AG_TITLE = 'TUBS'                                                149
         AG_EXNAME = 'SHAPE'                                              149
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            149
         CALL AXSHAPE                                                     149
C    *                                                                    149
         ZPOSBOX = 0                                                      149
C    Create TBOX :                                                        151
C    Call To Local Procedure TBOX                                         151
         ASSIGN 5200 TO BLOCK_TBOX                                        151
         GO TO  5190                                                      151
5200     CONTINUE                                                         151
C    *                                                                    151
C    Geant POSITION : TBOX ===> MOTHER='ETOF' Z=ZPOSBOX <===              152
         CALL AGSRESET                                                    152
         AG_MOTHER = 'ETOF'                                               152
         AG_Z = ZPOSBOX                                                   152
         AG_TITLE = 'TBOX'                                                152
         AG_EXNAME = 'POSITION'                                           152
         AG_PARLIST = 'MOTHER_Z'                                          152
         CALL AXPOSITION                                                  152
C    *                                                                    152
5161  CALL AGSPOPB                                                        153
      GO TO BLOCK_ETOF, (5160 ,5170)                                      153
C End of Local Procedure                                                  153
C ----------------------------------------------------------------------  154
C ------------ TBOX                                                       154
C Begin Local Procedure TBOX                                              155
C TBOX IS THE ETOF MODULE GAS BOX                                         155
5190  CONTINUE                                                            155
         AG_TITLE='TBOX'                                                  155
         AG_MODULE=MODULE                                                 155
         AG_MARK='V00'                                                    155
         CALL AGSPUSH                                                     155
         AG_MODULE=MODULE                                                 155
C    *                                                                    155
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        156
         CALL AGSRESET                                                    156
         AG_TITLE = 'ALUMINIUM'                                           156
         AG_EXNAME = 'MATERIAL'                                           156
         AG_PARLIST = 'NONE'                                              156
         CALL AXMATERIAL                                                  156
C    *                                                                    156
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           156
         CALL AGSRESET                                                    157
         AG_TITLE = 'STANDARD'                                            157
         AG_EXNAME = 'MEDIUM'                                             157
         AG_PARLIST = 'NONE'                                              157
         CALL AXMEDIUM                                                    157
C    *                                                                    157
C    Geant ATTRIBUTE : TBOX ===> SEEN=1 COLO=3 <===                       157
         CALL AGSRESET                                                    158
         AG_SEEN = 1                                                      158
         AG_COLO = 3                                                      158
         AG_TITLE = 'TBOX'                                                158
         AG_EXNAME = 'ATTRIBUTE'                                          158
         AG_PARLIST = 'SEEN_COLO'                                         158
         CALL AXATTRIBUTE                                                 158
C    *                                                                    158
C    Geant SHAPE : TUBS ===> RMIN=EIDG_RMIN RMAX=EIDG_RMAX, PHI1=-EIDG_D  158
C    PHI/2 PHI2=EIDG_DPHI/2 DZ=EIDG_TOFTHICK/2 <===                       158
         CALL AGSRESET                                                    160
         AG_RMIN = EIDG_RMIN                                              160
         AG_RMAX = EIDG_RMAX                                              160
         AG_PHI1 = -EIDG_DPHI/2                                           160
         AG_PHI2 = EIDG_DPHI/2                                            160
         AG_DZ = EIDG_TOFTHICK/2                                          160
         AG_TITLE = 'TUBS'                                                160
         AG_EXNAME = 'SHAPE'                                              160
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            160
         CALL AXSHAPE                                                     160
C    *                                                                    160
         ZPOSGAS = 0                                                      160
C    Create TGAS :                                                        162
C    Call To Local Procedure TGAS                                         162
         ASSIGN 5220 TO BLOCK_TGAS                                        162
         GO TO  5210                                                      162
5220     CONTINUE                                                         162
C    *                                                                    162
C    Geant POSITION : TGAS ===> MOTHER='TBOX' Z=ZPOSGAS <===              163
         CALL AGSRESET                                                    163
         AG_MOTHER = 'TBOX'                                               163
         AG_Z = ZPOSGAS                                                   163
         AG_TITLE = 'TGAS'                                                163
         AG_EXNAME = 'POSITION'                                           163
         AG_PARLIST = 'MOTHER_Z'                                          163
         CALL AXPOSITION                                                  163
C    *                                                                    163
5191  CALL AGSPOPB                                                        164
      GO TO BLOCK_TBOX, (5190 ,5200)                                      164
C End of Local Procedure                                                  164
C ----------------------------------------------------------------------  165
C ------------ TGAS                                                       165
C Begin Local Procedure TGAS                                              166
C TGAS IS THE ETOF MODULE GAS VOLUME                                      166
5210  CONTINUE                                                            166
         AG_TITLE='TGAS'                                                  166
         AG_MODULE=MODULE                                                 166
         AG_MARK='V00'                                                    166
         CALL AGSPUSH                                                     166
         AG_MODULE=MODULE                                                 166
C    *                                                                    166
C    Geant MATERIAL : FREON ===> inherited arguments used <===            167
         CALL AGSRESET                                                    167
         AG_TITLE = 'FREON'                                               167
         AG_EXNAME = 'MATERIAL'                                           167
         AG_PARLIST = 'NONE'                                              167
         CALL AXMATERIAL                                                  167
C    *                                                                    167
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           167
         CALL AGSRESET                                                    168
         AG_TITLE = 'STANDARD'                                            168
         AG_EXNAME = 'MEDIUM'                                             168
         AG_PARLIST = 'NONE'                                              168
         CALL AXMEDIUM                                                    168
C    *                                                                    168
C    Geant ATTRIBUTE : TGAS ===> SEEN=1 COLO=4 <===                       168
         CALL AGSRESET                                                    169
         AG_SEEN = 1                                                      169
         AG_COLO = 4                                                      169
         AG_TITLE = 'TGAS'                                                169
         AG_EXNAME = 'ATTRIBUTE'                                          169
         AG_PARLIST = 'SEEN_COLO'                                         169
         CALL AXATTRIBUTE                                                 169
C    *                                                                    169
C    Geant SHAPE : TUBS ===> RMIN=EIDG_RMIN RMAX=EIDG_RMAX, PHI1=-EIDG_D  169
C    PHI/2 PHI2=EIDG_DPHI/2 DZ=EIDG_TOFTHICK/2-EIDG_TBXTHICK <===         169
         CALL AGSRESET                                                    171
         AG_RMIN = EIDG_RMIN                                              171
         AG_RMAX = EIDG_RMAX                                              171
         AG_PHI1 = -EIDG_DPHI/2                                           171
         AG_PHI2 = EIDG_DPHI/2                                            171
         AG_DZ = EIDG_TOFTHICK/2-EIDG_TBXTHICK                            171
         AG_TITLE = 'TUBS'                                                171
         AG_EXNAME = 'SHAPE'                                              171
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            171
         CALL AXSHAPE                                                     171
C    *                                                                    171
C    Create TPCB :                                                        172
C    Call To Local Procedure TPCB                                         172
         ASSIGN 5240 TO BLOCK_TPCB                                        172
         GO TO  5230                                                      172
5240     CONTINUE                                                         172
C    *                                                                    172
C    *                                                                    173
         I_PCB=0                                                          173
            GO TO 5253                                                    173
5251        I_PCB=I_PCB+1                                                 173
5253        IF(I_PCB-(1).GT.0) GO TO 5252                                 173
            ZPOSPCB =                                                     174
     *      (I_PCB-0.5)*(EIDG_TGLTHICK*7+EIDG_TGPTHICK*6+EIDG_TPBTHICK)   174
C       Geant POSITION : TPCB ===> MOTHER='TGAS' Z=ZPOSPCB <===           175
            CALL AGSRESET                                                 175
            AG_MOTHER = 'TGAS'                                            175
            AG_Z = ZPOSPCB                                                175
            AG_TITLE = 'TPCB'                                             175
            AG_EXNAME = 'POSITION'                                        175
            AG_PARLIST = 'MOTHER_Z'                                       175
            CALL AXPOSITION                                               175
C       *                                                                 175
         GO TO 5251                                                       176
5252     CONTINUE                                                         176
C    Create TGLA :                                                        177
C    Call To Local Procedure TGLA                                         177
         ASSIGN 5270 TO BLOCK_TGLA                                        177
         GO TO  5260                                                      177
5270     CONTINUE                                                         177
C    *                                                                    177
C    *                                                                    178
         I_GLASS=0                                                        178
            GO TO 5283                                                    178
5281        I_GLASS=I_GLASS+1                                             178
5283        IF(I_GLASS-(6).GT.0) GO TO 5282                               178
            ZPOSGLA = (EIDG_TGLTHICK+EIDG_TGPTHICK)*(I_GLASS-3)           179
C       Geant POSITION : TGLA ===> MOTHER='TGAS' Z=ZPOSGLA <===           180
            CALL AGSRESET                                                 180
            AG_MOTHER = 'TGAS'                                            180
            AG_Z = ZPOSGLA                                                180
            AG_TITLE = 'TGLA'                                             180
            AG_EXNAME = 'POSITION'                                        180
            AG_PARLIST = 'MOTHER_Z'                                       180
            CALL AXPOSITION                                               180
C       *                                                                 180
         GO TO 5281                                                       181
5282     CONTINUE                                                         181
C    Create TGAP :                                                        182
C    Call To Local Procedure TGAP                                         182
         ASSIGN 5300 TO BLOCK_TGAP                                        182
         GO TO  5290                                                      182
5300     CONTINUE                                                         182
C    *                                                                    182
C    *                                                                    183
         I_GAP=0                                                          183
            GO TO 5313                                                    183
5311        I_GAP=I_GAP+1                                                 183
5313        IF(I_GAP-(5).GT.0) GO TO 5312                                 183
            ZPOSGAP = (EIDG_TGLTHICK+EIDG_TGPTHICK)*(I_GLASS-2.5)         184
C       Geant POSITION : TGAP ===> MOTHER='TGAS' Z=ZPOSGAP <===           185
            CALL AGSRESET                                                 185
            AG_MOTHER = 'TGAS'                                            185
            AG_Z = ZPOSGAP                                                185
            AG_TITLE = 'TGAP'                                             185
            AG_EXNAME = 'POSITION'                                        185
            AG_PARLIST = 'MOTHER_Z'                                       185
            CALL AXPOSITION                                               185
C       *                                                                 185
         GO TO 5311                                                       186
5312     CONTINUE                                                         186
5211  CALL AGSPOPB                                                        187
      GO TO BLOCK_TGAS, (5210 ,5220)                                      187
C End of Local Procedure                                                  187
C ----------------------------------------------------------------------  188
C ------------ TPCB                                                       188
C Begin Local Procedure TPCB                                              189
C TPCB IS THE MRPC READOUT BOARD                                          189
5230  CONTINUE                                                            189
         AG_TITLE='TPCB'                                                  189
         AG_MODULE=MODULE                                                 189
         AG_MARK='V00'                                                    189
         CALL AGSPUSH                                                     189
         AG_MODULE=MODULE                                                 189
C    *                                                                    189
C    Geant MATERIAL : G10 ===> inherited arguments used <===              190
         CALL AGSRESET                                                    190
         AG_TITLE = 'G10'                                                 190
         AG_EXNAME = 'MATERIAL'                                           190
         AG_PARLIST = 'NONE'                                              190
         CALL AXMATERIAL                                                  190
C    *                                                                    190
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           190
         CALL AGSRESET                                                    191
         AG_TITLE = 'STANDARD'                                            191
         AG_EXNAME = 'MEDIUM'                                             191
         AG_PARLIST = 'NONE'                                              191
         CALL AXMEDIUM                                                    191
C    *                                                                    191
C    Geant ATTRIBUTE : TPCB ===> SEEN=1 COLO=5 <===                       191
         CALL AGSRESET                                                    192
         AG_SEEN = 1                                                      192
         AG_COLO = 5                                                      192
         AG_TITLE = 'TPCB'                                                192
         AG_EXNAME = 'ATTRIBUTE'                                          192
         AG_PARLIST = 'SEEN_COLO'                                         192
         CALL AXATTRIBUTE                                                 192
C    *                                                                    192
C    Geant SHAPE : TUBS ===> RMIN=EIDG_RMIN RMAX=EIDG_RMAX, PHI1=-EIDG_D  192
C    PHI/2 PHI2=EIDG_DPHI/2 DZ=EIDG_TPBTHICK/2 <===                       192
         CALL AGSRESET                                                    194
         AG_RMIN = EIDG_RMIN                                              194
         AG_RMAX = EIDG_RMAX                                              194
         AG_PHI1 = -EIDG_DPHI/2                                           194
         AG_PHI2 = EIDG_DPHI/2                                            194
         AG_DZ = EIDG_TPBTHICK/2                                          194
         AG_TITLE = 'TUBS'                                                194
         AG_EXNAME = 'SHAPE'                                              194
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            194
         CALL AXSHAPE                                                     194
C    *                                                                    194
5231  CALL AGSPOPB                                                        195
      GO TO BLOCK_TPCB, (5230 ,5240)                                      195
C End of Local Procedure                                                  195
C ----------------------------------------------------------------------  196
C ------------ TGLA                                                       196
C Begin Local Procedure TGLA                                              197
C TGLA IS THE MRPC GLASS PLATE                                            197
5260  CONTINUE                                                            197
         AG_TITLE='TGLA'                                                  197
         AG_MODULE=MODULE                                                 197
         AG_MARK='V00'                                                    197
         CALL AGSPUSH                                                     197
         AG_MODULE=MODULE                                                 197
C    *                                                                    197
C    Geant MATERIAL : GLASS ===> inherited arguments used <===            198
         CALL AGSRESET                                                    198
         AG_TITLE = 'GLASS'                                               198
         AG_EXNAME = 'MATERIAL'                                           198
         AG_PARLIST = 'NONE'                                              198
         CALL AXMATERIAL                                                  198
C    *                                                                    198
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           198
         CALL AGSRESET                                                    199
         AG_TITLE = 'STANDARD'                                            199
         AG_EXNAME = 'MEDIUM'                                             199
         AG_PARLIST = 'NONE'                                              199
         CALL AXMEDIUM                                                    199
C    *                                                                    199
C    Geant ATTRIBUTE : TGLA ===> SEEN=1 COLO=6 <===                       199
         CALL AGSRESET                                                    200
         AG_SEEN = 1                                                      200
         AG_COLO = 6                                                      200
         AG_TITLE = 'TGLA'                                                200
         AG_EXNAME = 'ATTRIBUTE'                                          200
         AG_PARLIST = 'SEEN_COLO'                                         200
         CALL AXATTRIBUTE                                                 200
C    *                                                                    200
C    Geant SHAPE : TUBS ===> RMIN=EIDG_RMIN RMAX=EIDG_RMAX, PHI1=-EIDG_D  200
C    PHI/2 PHI2=EIDG_DPHI/2 DZ=EIDG_TGLTHICK/2 <===                       200
         CALL AGSRESET                                                    202
         AG_RMIN = EIDG_RMIN                                              202
         AG_RMAX = EIDG_RMAX                                              202
         AG_PHI1 = -EIDG_DPHI/2                                           202
         AG_PHI2 = EIDG_DPHI/2                                            202
         AG_DZ = EIDG_TGLTHICK/2                                          202
         AG_TITLE = 'TUBS'                                                202
         AG_EXNAME = 'SHAPE'                                              202
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            202
         CALL AXSHAPE                                                     202
C    *                                                                    202
5261  CALL AGSPOPB                                                        203
      GO TO BLOCK_TGLA, (5260 ,5270)                                      203
C End of Local Procedure                                                  203
C ----------------------------------------------------------------------  204
C ------------ TGAP                                                       204
C Begin Local Procedure TGAP                                              205
C TGAP IS THE MRPC GAS GAP (SENSITIVE)                                    205
5290  CONTINUE                                                            205
         AG_TITLE='TGAP'                                                  205
         AG_MODULE=MODULE                                                 205
         AG_MARK='V00'                                                    205
         CALL AGSPUSH                                                     205
         AG_MODULE=MODULE                                                 205
C    *                                                                    205
C    Geant MATERIAL : FREON ===> inherited arguments used <===            206
         CALL AGSRESET                                                    206
         AG_TITLE = 'FREON'                                               206
         AG_EXNAME = 'MATERIAL'                                           206
         AG_PARLIST = 'NONE'                                              206
         CALL AXMATERIAL                                                  206
C    *                                                                    206
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           206
         CALL AGSRESET                                                    207
         AG_ISVOL = 1                                                     207
         AG_TITLE = 'SENSITIVE'                                           207
         AG_EXNAME = 'MEDIUM'                                             207
         AG_PARLIST = 'ISVOL'                                             207
         CALL AXMEDIUM                                                    207
C    *                                                                    207
C    Geant ATTRIBUTE : TGAP ===> SEEN=1 COLO=8 <===                       207
         CALL AGSRESET                                                    208
         AG_SEEN = 1                                                      208
         AG_COLO = 8                                                      208
         AG_TITLE = 'TGAP'                                                208
         AG_EXNAME = 'ATTRIBUTE'                                          208
         AG_PARLIST = 'SEEN_COLO'                                         208
         CALL AXATTRIBUTE                                                 208
C    *                                                                    208
C    Geant SHAPE : TUBS ===> RMIN=EIDG_RMIN RMAX=EIDG_RMAX, PHI1=-EIDG_D  208
C    PHI/2 PHI2=EIDG_DPHI/2 DZ=EIDG_TGPTHICK/2 <===                       208
         CALL AGSRESET                                                    210
         AG_RMIN = EIDG_RMIN                                              210
         AG_RMAX = EIDG_RMAX                                              210
         AG_PHI1 = -EIDG_DPHI/2                                           210
         AG_PHI2 = EIDG_DPHI/2                                            210
         AG_DZ = EIDG_TGPTHICK/2                                          210
         AG_TITLE = 'TUBS'                                                210
         AG_EXNAME = 'SHAPE'                                              210
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            210
         CALL AXSHAPE                                                     210
C    *                                                                    210
C    Defining Hits: Z:0.0005:S Y:0.0005: Z:0.0005: CX:10: CY:10: CZ:10:   210
C    LPTOT:18:(-3,2), SLENG:0.1:(0,800) TOF:16:(0,1.0E-6) LGAM:16:(-2,+4  210
C    ) STEP:11:(0,10) ELOSS:16:(0,0.01)                                   210
         AG_TITLE = 'TGAP'                                                212
         AG_EXNAME = 'HITS'                                               212
         AG_TYPE=AG_MAGIC                                                 212
         CALL AGSDETV('TGAP')                                             212
C    *                                                                    212
         AG_PARLIST='Z'                                                   212
         AG_TITLE = 'DETH'                                                212
         AG_NBITS = AG_MAGIC                                              212
         AG_BIN=0.0005                                                    212
         AG_OPTION='S'                                                    212
         AG_FHMIN = AG_MAGIC                                              212
         AG_FHMAX = AG_MAGIC                                              212
         CALL AGSDETH (0)                                                 212
C    *                                                                    212
         AG_PARLIST='Y'                                                   212
         AG_TITLE = 'DETH'                                                212
         AG_NBITS = AG_MAGIC                                              212
         AG_BIN=0.0005                                                    212
         AG_FHMIN=AG_MAGIC                                                212
         AG_FHMAX=AG_MAGIC                                                212
         AG_OPTION='*'                                                    212
         CALL AGSDETH (0)                                                 212
C    *                                                                    212
         AG_PARLIST='Z'                                                   212
         AG_TITLE = 'DETH'                                                212
         AG_NBITS = AG_MAGIC                                              212
         AG_BIN=0.0005                                                    212
         AG_FHMIN=AG_MAGIC                                                212
         AG_FHMAX=AG_MAGIC                                                212
         AG_OPTION='*'                                                    212
         CALL AGSDETH (0)                                                 212
C    *                                                                    212
         AG_PARLIST='CX'                                                  212
         AG_TITLE = 'DETH'                                                212
         AG_NBITS = 10                                                    212
         AG_BIN=AG_MAGIC                                                  212
         AG_FHMIN=AG_MAGIC                                                212
         AG_FHMAX=AG_MAGIC                                                212
         AG_OPTION='*'                                                    212
         CALL AGSDETH (0)                                                 212
C    *                                                                    212
         AG_PARLIST='CY'                                                  212
         AG_TITLE = 'DETH'                                                212
         AG_NBITS = 10                                                    212
         AG_BIN=AG_MAGIC                                                  212
         AG_FHMIN=AG_MAGIC                                                212
         AG_FHMAX=AG_MAGIC                                                212
         AG_OPTION='*'                                                    212
         CALL AGSDETH (0)                                                 212
C    *                                                                    212
         AG_PARLIST='CZ'                                                  212
         AG_TITLE = 'DETH'                                                212
         AG_NBITS = 10                                                    212
         AG_BIN=AG_MAGIC                                                  212
         AG_FHMIN=AG_MAGIC                                                212
         AG_FHMAX=AG_MAGIC                                                212
         AG_OPTION='*'                                                    212
         CALL AGSDETH (0)                                                 212
C    *                                                                    212
         AG_PARLIST='LPTOT'                                               212
         AG_TITLE = 'DETH'                                                212
         AG_NBITS = 18                                                    212
         AG_BIN=AG_MAGIC                                                  212
         AG_OPTION='*'                                                    212
         AG_FHMIN = -3                                                    212
         AG_FHMAX = 2                                                     212
         CALL AGSDETH (0)                                                 212
C    *                                                                    212
         AG_PARLIST='SLENG'                                               212
         AG_TITLE = 'DETH'                                                212
         AG_NBITS = AG_MAGIC                                              212
         AG_BIN=0.1                                                       212
         AG_OPTION='*'                                                    212
         AG_FHMIN = 0                                                     212
         AG_FHMAX = 800                                                   212
         CALL AGSDETH (0)                                                 212
C    *                                                                    212
         AG_PARLIST='TOF'                                                 212
         AG_TITLE = 'DETH'                                                212
         AG_NBITS = 16                                                    212
         AG_BIN=AG_MAGIC                                                  212
         AG_OPTION='*'                                                    212
         AG_FHMIN = 0                                                     212
         AG_FHMAX = 1.0E-6                                                212
         CALL AGSDETH (0)                                                 212
C    *                                                                    212
         AG_PARLIST='LGAM'                                                212
         AG_TITLE = 'DETH'                                                212
         AG_NBITS = 16                                                    212
         AG_BIN=AG_MAGIC                                                  212
         AG_OPTION='*'                                                    212
         AG_FHMIN = -2                                                    212
         AG_FHMAX = +4                                                    212
         CALL AGSDETH (0)                                                 212
C    *                                                                    212
         AG_PARLIST='STEP'                                                212
         AG_TITLE = 'DETH'                                                212
         AG_NBITS = 11                                                    212
         AG_BIN=AG_MAGIC                                                  212
         AG_OPTION='*'                                                    212
         AG_FHMIN = 0                                                     212
         AG_FHMAX = 10                                                    212
         CALL AGSDETH (0)                                                 212
C    *                                                                    212
         AG_PARLIST='ELOSS'                                               212
         AG_TITLE = 'DETH'                                                212
         AG_NBITS = 16                                                    212
         AG_BIN=AG_MAGIC                                                  212
         AG_OPTION='*'                                                    212
         AG_FHMIN = 0                                                     212
         AG_FHMAX = 0.01                                                  212
         CALL AGSDETH (0)                                                 212
C    *                                                                    212
         CALL AGSDETU('TGAP')                                             212
C    *                                                                    212
5291  CALL AGSPOPB                                                        213
      GO TO BLOCK_TGAP, (5290 ,5300)                                      213
C End of Local Procedure                                                  213
C ----------------------------------------------------------------------  214
C ------------ TECL                                                       214
C Begin Local Procedure TECL                                              215
C TECL IS THE TECL SUPERMODULE                                            215
5130  CONTINUE                                                            215
         AG_TITLE='TECL'                                                  215
         AG_MODULE=MODULE                                                 215
         AG_MARK='V00'                                                    215
         CALL AGSPUSH                                                     215
         AG_MODULE=MODULE                                                 215
C    *                                                                    215
C    Geant MATERIAL : AIR ===> inherited arguments used <===              216
         CALL AGSRESET                                                    216
         AG_TITLE = 'AIR'                                                 216
         AG_EXNAME = 'MATERIAL'                                           216
         AG_PARLIST = 'NONE'                                              216
         CALL AXMATERIAL                                                  216
C    *                                                                    216
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           216
         CALL AGSRESET                                                    217
         AG_TITLE = 'STANDARD'                                            217
         AG_EXNAME = 'MEDIUM'                                             217
         AG_PARLIST = 'NONE'                                              217
         CALL AXMEDIUM                                                    217
C    *                                                                    217
C    Geant ATTRIBUTE : TECL ===> SEEN=1 COLO=7 <===                       217
         CALL AGSRESET                                                    218
         AG_SEEN = 1                                                      218
         AG_COLO = 7                                                      218
         AG_TITLE = 'TECL'                                                218
         AG_EXNAME = 'ATTRIBUTE'                                          218
         AG_PARLIST = 'SEEN_COLO'                                         218
         CALL AXATTRIBUTE                                                 218
C    *                                                                    218
C    Geant SHAPE : TUBS ===> RMIN=EIDG_RMIN RMAX=EIDG_RMAX, PHI1=-EIDG_D  218
C    PHI/2 PHI2=EIDG_DPHI/2 DZ=EIDG_ECLTHICK/2 <===                       218
         CALL AGSRESET                                                    220
         AG_RMIN = EIDG_RMIN                                              220
         AG_RMAX = EIDG_RMAX                                              220
         AG_PHI1 = -EIDG_DPHI/2                                           220
         AG_PHI2 = EIDG_DPHI/2                                            220
         AG_DZ = EIDG_ECLTHICK/2                                          220
         AG_TITLE = 'TUBS'                                                220
         AG_EXNAME = 'SHAPE'                                              220
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            220
         CALL AXSHAPE                                                     220
C    *                                                                    220
         ZPOSEAA = - EIDG_ECLTHICK/2 + EIDG_EAATHICK/2                    220
C    Create TEAA :                                                        222
C    Call To Local Procedure TEAA                                         222
         ASSIGN 5330 TO BLOCK_TEAA                                        222
         GO TO  5320                                                      222
5330     CONTINUE                                                         222
C    *                                                                    222
C    Geant POSITION : TEAA ===> MOTHER='TECL' Z=ZPOSEAA <===              223
         CALL AGSRESET                                                    223
         AG_MOTHER = 'TECL'                                               223
         AG_Z = ZPOSEAA                                                   223
         AG_TITLE = 'TEAA'                                                223
         AG_EXNAME = 'POSITION'                                           223
         AG_PARLIST = 'MOTHER_Z'                                          223
         CALL AXPOSITION                                                  223
C    *                                                                    223
         ZPOSEAB = - EIDG_ECLTHICK/2 + EIDG_EAATHICK + EIDG_SCNTHICK +    223
     *   EIDG_EABTHICK/2                                                  223
C    Create TEAB :                                                        225
C    Call To Local Procedure TEAB                                         225
         ASSIGN 5350 TO BLOCK_TEAB                                        225
         GO TO  5340                                                      225
5350     CONTINUE                                                         225
C    *                                                                    225
C    Geant POSITION : TEAB ===> MOTHER='TECL' Z=ZPOSEAB <===              226
         CALL AGSRESET                                                    226
         AG_MOTHER = 'TECL'                                               226
         AG_Z = ZPOSEAB                                                   226
         AG_TITLE = 'TEAB'                                                226
         AG_EXNAME = 'POSITION'                                           226
         AG_PARLIST = 'MOTHER_Z'                                          226
         CALL AXPOSITION                                                  226
C    *                                                                    226
C    Create SCIN :                                                        227
C    Call To Local Procedure SCIN                                         227
         ASSIGN 5370 TO BLOCK_SCIN                                        227
         GO TO  5360                                                      227
5370     CONTINUE                                                         227
C    *                                                                    227
         ZPOSSCIN = - EIDG_ECLTHICK/2 + EIDG_EAATHICK + EIDG_SCNTHICK/2   228
C    Geant POSITION : SCIN ===> MOTHER='TECL' Z=ZPOSSCIN <===             229
         CALL AGSRESET                                                    229
         AG_MOTHER = 'TECL'                                               229
         AG_Z = ZPOSSCIN                                                  229
         AG_TITLE = 'SCIN'                                                229
         AG_EXNAME = 'POSITION'                                           229
         AG_PARLIST = 'MOTHER_Z'                                          229
         CALL AXPOSITION                                                  229
C    *                                                                    229
         ZPOSSCIN = EIDG_ECLTHICK/2 - EIDG_SCNTHICK/2                     229
C    Geant POSITION : SCIN ===> MOTHER='TECL' Z=ZPOSSCIN <===             231
         CALL AGSRESET                                                    231
         AG_MOTHER = 'TECL'                                               231
         AG_Z = ZPOSSCIN                                                  231
         AG_TITLE = 'SCIN'                                                231
         AG_EXNAME = 'POSITION'                                           231
         AG_PARLIST = 'MOTHER_Z'                                          231
         CALL AXPOSITION                                                  231
C    *                                                                    231
5131  CALL AGSPOPB                                                        232
      GO TO BLOCK_TECL, (5130 ,5140)                                      232
C End of Local Procedure                                                  232
C ----------------------------------------------------------------------  233
C ------------ TEAA                                                       233
C Begin Local Procedure TEAA                                              234
C TEAA IS THE 1ST LEAD CONVERTOR                                          234
5320  CONTINUE                                                            234
         AG_TITLE='TEAA'                                                  234
         AG_MODULE=MODULE                                                 234
         AG_MARK='V00'                                                    234
         CALL AGSPUSH                                                     234
         AG_MODULE=MODULE                                                 234
C    *                                                                    234
C    Geant MATERIAL : LEAD ===> inherited arguments used <===             235
         CALL AGSRESET                                                    235
         AG_TITLE = 'LEAD'                                                235
         AG_EXNAME = 'MATERIAL'                                           235
         AG_PARLIST = 'NONE'                                              235
         CALL AXMATERIAL                                                  235
C    *                                                                    235
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           235
         CALL AGSRESET                                                    236
         AG_TITLE = 'STANDARD'                                            236
         AG_EXNAME = 'MEDIUM'                                             236
         AG_PARLIST = 'NONE'                                              236
         CALL AXMEDIUM                                                    236
C    *                                                                    236
C    Geant ATTRIBUTE : TEAA ===> SEEN=1 COLO=1 <===                       236
         CALL AGSRESET                                                    237
         AG_SEEN = 1                                                      237
         AG_COLO = 1                                                      237
         AG_TITLE = 'TEAA'                                                237
         AG_EXNAME = 'ATTRIBUTE'                                          237
         AG_PARLIST = 'SEEN_COLO'                                         237
         CALL AXATTRIBUTE                                                 237
C    *                                                                    237
C    Geant SHAPE : TUBS ===> RMIN=EIDG_RMIN RMAX=EIDG_RMAX, PHI1=-EIDG_D  237
C    PHI/2 PHI2=EIDG_DPHI/2 DZ=EIDG_EAATHICK/2 <===                       237
         CALL AGSRESET                                                    239
         AG_RMIN = EIDG_RMIN                                              239
         AG_RMAX = EIDG_RMAX                                              239
         AG_PHI1 = -EIDG_DPHI/2                                           239
         AG_PHI2 = EIDG_DPHI/2                                            239
         AG_DZ = EIDG_EAATHICK/2                                          239
         AG_TITLE = 'TUBS'                                                239
         AG_EXNAME = 'SHAPE'                                              239
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            239
         CALL AXSHAPE                                                     239
C    *                                                                    239
5321  CALL AGSPOPB                                                        240
      GO TO BLOCK_TEAA, (5320 ,5330)                                      240
C End of Local Procedure                                                  240
C ----------------------------------------------------------------------  241
C ------------ TEAB                                                       241
C Begin Local Procedure TEAB                                              242
C TEAB IS THE 2ND LEAD CONVERTOR                                          242
5340  CONTINUE                                                            242
         AG_TITLE='TEAB'                                                  242
         AG_MODULE=MODULE                                                 242
         AG_MARK='V00'                                                    242
         CALL AGSPUSH                                                     242
         AG_MODULE=MODULE                                                 242
C    *                                                                    242
C    Geant MATERIAL : LEAD ===> inherited arguments used <===             243
         CALL AGSRESET                                                    243
         AG_TITLE = 'LEAD'                                                243
         AG_EXNAME = 'MATERIAL'                                           243
         AG_PARLIST = 'NONE'                                              243
         CALL AXMATERIAL                                                  243
C    *                                                                    243
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           243
         CALL AGSRESET                                                    244
         AG_TITLE = 'STANDARD'                                            244
         AG_EXNAME = 'MEDIUM'                                             244
         AG_PARLIST = 'NONE'                                              244
         CALL AXMEDIUM                                                    244
C    *                                                                    244
C    Geant ATTRIBUTE : TEAB ===> SEEN=1 COLO=1 <===                       244
         CALL AGSRESET                                                    245
         AG_SEEN = 1                                                      245
         AG_COLO = 1                                                      245
         AG_TITLE = 'TEAB'                                                245
         AG_EXNAME = 'ATTRIBUTE'                                          245
         AG_PARLIST = 'SEEN_COLO'                                         245
         CALL AXATTRIBUTE                                                 245
C    *                                                                    245
C    Geant SHAPE : TUBS ===> RMIN=EIDG_RMIN RMAX=EIDG_RMAX, PHI1=-EIDG_D  245
C    PHI/2 PHI2=EIDG_DPHI/2 DZ=EIDG_EABTHICK/2 <===                       245
         CALL AGSRESET                                                    247
         AG_RMIN = EIDG_RMIN                                              247
         AG_RMAX = EIDG_RMAX                                              247
         AG_PHI1 = -EIDG_DPHI/2                                           247
         AG_PHI2 = EIDG_DPHI/2                                            247
         AG_DZ = EIDG_EABTHICK/2                                          247
         AG_TITLE = 'TUBS'                                                247
         AG_EXNAME = 'SHAPE'                                              247
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            247
         CALL AXSHAPE                                                     247
C    *                                                                    247
5341  CALL AGSPOPB                                                        248
      GO TO BLOCK_TEAB, (5340 ,5350)                                      248
C End of Local Procedure                                                  248
C ----------------------------------------------------------------------  249
C ------------ SCIN                                                       249
C Begin Local Procedure SCIN                                              250
C SCIN IS THE PLASTIC SCINTILLATOR (SENSITIVE)                            250
5360  CONTINUE                                                            250
         AG_TITLE='SCIN'                                                  250
         AG_MODULE=MODULE                                                 250
         AG_MARK='V00'                                                    250
         CALL AGSPUSH                                                     250
         AG_MODULE=MODULE                                                 250
C    *                                                                    250
C    Geant MATERIAL : POLYSTYRENE ===> inherited arguments used <===      251
         CALL AGSRESET                                                    251
         AG_TITLE = 'POLYSTYRENE'                                         251
         AG_EXNAME = 'MATERIAL'                                           251
         AG_PARLIST = 'NONE'                                              251
         CALL AXMATERIAL                                                  251
C    *                                                                    251
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           251
         CALL AGSRESET                                                    252
         AG_ISVOL = 1                                                     252
         AG_TITLE = 'SENSITIVE'                                           252
         AG_EXNAME = 'MEDIUM'                                             252
         AG_PARLIST = 'ISVOL'                                             252
         CALL AXMEDIUM                                                    252
C    *                                                                    252
C    Geant ATTRIBUTE : SCIN ===> SEEN=1 COLO=8 <===                       252
         CALL AGSRESET                                                    253
         AG_SEEN = 1                                                      253
         AG_COLO = 8                                                      253
         AG_TITLE = 'SCIN'                                                253
         AG_EXNAME = 'ATTRIBUTE'                                          253
         AG_PARLIST = 'SEEN_COLO'                                         253
         CALL AXATTRIBUTE                                                 253
C    *                                                                    253
C    Geant SHAPE : TUBS ===> RMIN=EIDG_RMIN RMAX=EIDG_RMAX, PHI1=-EIDG_D  253
C    PHI/2 PHI2=EIDG_DPHI/2 DZ=EIDG_SCNTHICK/2 <===                       253
         CALL AGSRESET                                                    255
         AG_RMIN = EIDG_RMIN                                              255
         AG_RMAX = EIDG_RMAX                                              255
         AG_PHI1 = -EIDG_DPHI/2                                           255
         AG_PHI2 = EIDG_DPHI/2                                            255
         AG_DZ = EIDG_SCNTHICK/2                                          255
         AG_TITLE = 'TUBS'                                                255
         AG_EXNAME = 'SHAPE'                                              255
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            255
         CALL AXSHAPE                                                     255
C    *                                                                    255
C    Defining Hits: Z:0.0005:S Y:0.0005: Z:0.0005: CX:10: CY:10: CZ:10:   255
C    LPTOT:18:(-3,2), SLENG:0.1:(0,800) TOF:16:(0,1.0E-6) LGAM:16:(-2,+4  255
C    ) STEP:11:(0,10) ELOSS:16:(0,0.01)                                   255
         AG_TITLE = 'SCIN'                                                257
         AG_EXNAME = 'HITS'                                               257
         AG_TYPE=AG_MAGIC                                                 257
         CALL AGSDETV('SCIN')                                             257
C    *                                                                    257
         AG_PARLIST='Z'                                                   257
         AG_TITLE = 'DETH'                                                257
         AG_NBITS = AG_MAGIC                                              257
         AG_BIN=0.0005                                                    257
         AG_OPTION='S'                                                    257
         AG_FHMIN = AG_MAGIC                                              257
         AG_FHMAX = AG_MAGIC                                              257
         CALL AGSDETH (0)                                                 257
C    *                                                                    257
         AG_PARLIST='Y'                                                   257
         AG_TITLE = 'DETH'                                                257
         AG_NBITS = AG_MAGIC                                              257
         AG_BIN=0.0005                                                    257
         AG_FHMIN=AG_MAGIC                                                257
         AG_FHMAX=AG_MAGIC                                                257
         AG_OPTION='*'                                                    257
         CALL AGSDETH (0)                                                 257
C    *                                                                    257
         AG_PARLIST='Z'                                                   257
         AG_TITLE = 'DETH'                                                257
         AG_NBITS = AG_MAGIC                                              257
         AG_BIN=0.0005                                                    257
         AG_FHMIN=AG_MAGIC                                                257
         AG_FHMAX=AG_MAGIC                                                257
         AG_OPTION='*'                                                    257
         CALL AGSDETH (0)                                                 257
C    *                                                                    257
         AG_PARLIST='CX'                                                  257
         AG_TITLE = 'DETH'                                                257
         AG_NBITS = 10                                                    257
         AG_BIN=AG_MAGIC                                                  257
         AG_FHMIN=AG_MAGIC                                                257
         AG_FHMAX=AG_MAGIC                                                257
         AG_OPTION='*'                                                    257
         CALL AGSDETH (0)                                                 257
C    *                                                                    257
         AG_PARLIST='CY'                                                  257
         AG_TITLE = 'DETH'                                                257
         AG_NBITS = 10                                                    257
         AG_BIN=AG_MAGIC                                                  257
         AG_FHMIN=AG_MAGIC                                                257
         AG_FHMAX=AG_MAGIC                                                257
         AG_OPTION='*'                                                    257
         CALL AGSDETH (0)                                                 257
C    *                                                                    257
         AG_PARLIST='CZ'                                                  257
         AG_TITLE = 'DETH'                                                257
         AG_NBITS = 10                                                    257
         AG_BIN=AG_MAGIC                                                  257
         AG_FHMIN=AG_MAGIC                                                257
         AG_FHMAX=AG_MAGIC                                                257
         AG_OPTION='*'                                                    257
         CALL AGSDETH (0)                                                 257
C    *                                                                    257
         AG_PARLIST='LPTOT'                                               257
         AG_TITLE = 'DETH'                                                257
         AG_NBITS = 18                                                    257
         AG_BIN=AG_MAGIC                                                  257
         AG_OPTION='*'                                                    257
         AG_FHMIN = -3                                                    257
         AG_FHMAX = 2                                                     257
         CALL AGSDETH (0)                                                 257
C    *                                                                    257
         AG_PARLIST='SLENG'                                               257
         AG_TITLE = 'DETH'                                                257
         AG_NBITS = AG_MAGIC                                              257
         AG_BIN=0.1                                                       257
         AG_OPTION='*'                                                    257
         AG_FHMIN = 0                                                     257
         AG_FHMAX = 800                                                   257
         CALL AGSDETH (0)                                                 257
C    *                                                                    257
         AG_PARLIST='TOF'                                                 257
         AG_TITLE = 'DETH'                                                257
         AG_NBITS = 16                                                    257
         AG_BIN=AG_MAGIC                                                  257
         AG_OPTION='*'                                                    257
         AG_FHMIN = 0                                                     257
         AG_FHMAX = 1.0E-6                                                257
         CALL AGSDETH (0)                                                 257
C    *                                                                    257
         AG_PARLIST='LGAM'                                                257
         AG_TITLE = 'DETH'                                                257
         AG_NBITS = 16                                                    257
         AG_BIN=AG_MAGIC                                                  257
         AG_OPTION='*'                                                    257
         AG_FHMIN = -2                                                    257
         AG_FHMAX = +4                                                    257
         CALL AGSDETH (0)                                                 257
C    *                                                                    257
         AG_PARLIST='STEP'                                                257
         AG_TITLE = 'DETH'                                                257
         AG_NBITS = 11                                                    257
         AG_BIN=AG_MAGIC                                                  257
         AG_OPTION='*'                                                    257
         AG_FHMIN = 0                                                     257
         AG_FHMAX = 10                                                    257
         CALL AGSDETH (0)                                                 257
C    *                                                                    257
         AG_PARLIST='ELOSS'                                               257
         AG_TITLE = 'DETH'                                                257
         AG_NBITS = 16                                                    257
         AG_BIN=AG_MAGIC                                                  257
         AG_OPTION='*'                                                    257
         AG_FHMIN = 0                                                     257
         AG_FHMAX = 0.01                                                  257
         CALL AGSDETH (0)                                                 257
C    *                                                                    257
         CALL AGSDETU('SCIN')                                             257
C    *                                                                    257
5361  CALL AGSPOPB                                                        258
      GO TO BLOCK_SCIN, (5360 ,5370)                                      258
C End of Local Procedure                                                  258
C ----------------------------------------------------------------------  259
C ------------ ETRD                                                       259
C Begin Local Procedure ETRD                                              260
C ETRD IS THE ETRD SUPERMODULE                                            260
5090  CONTINUE                                                            260
         AG_TITLE='ETRD'                                                  260
         AG_MODULE=MODULE                                                 260
         AG_MARK='V00'                                                    260
         CALL AGSPUSH                                                     260
         AG_MODULE=MODULE                                                 260
C    *                                                                    260
C    Geant MATERIAL : AIR ===> inherited arguments used <===              261
         CALL AGSRESET                                                    261
         AG_TITLE = 'AIR'                                                 261
         AG_EXNAME = 'MATERIAL'                                           261
         AG_PARLIST = 'NONE'                                              261
         CALL AXMATERIAL                                                  261
C    *                                                                    261
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           261
         CALL AGSRESET                                                    262
         AG_TITLE = 'STANDARD'                                            262
         AG_EXNAME = 'MEDIUM'                                             262
         AG_PARLIST = 'NONE'                                              262
         CALL AXMEDIUM                                                    262
C    *                                                                    262
C    Geant ATTRIBUTE : ETRD ===> SEEN=1 COLO=4 <===                       262
         CALL AGSRESET                                                    263
         AG_SEEN = 1                                                      263
         AG_COLO = 4                                                      263
         AG_TITLE = 'ETRD'                                                263
         AG_EXNAME = 'ATTRIBUTE'                                          263
         AG_PARLIST = 'SEEN_COLO'                                         263
         CALL AXATTRIBUTE                                                 263
C    *                                                                    263
C    Geant SHAPE : TUBS ===> RMIN=EIDG_RMIN RMAX=EIDG_RMAX, PHI1=-EIDG_D  263
C    PHI/2 PHI2=EIDG_DPHI/2 DZ=EIDG_TRDTHICK/2 <===                       263
         CALL AGSRESET                                                    265
         AG_RMIN = EIDG_RMIN                                              265
         AG_RMAX = EIDG_RMAX                                              265
         AG_PHI1 = -EIDG_DPHI/2                                           265
         AG_PHI2 = EIDG_DPHI/2                                            265
         AG_DZ = EIDG_TRDTHICK/2                                          265
         AG_TITLE = 'TUBS'                                                265
         AG_EXNAME = 'SHAPE'                                              265
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            265
         CALL AXSHAPE                                                     265
C    *                                                                    265
         ZPOSRAD = - EIDG_TRDTHICK/2 + EIDG_TRATHICK/2                    265
C    Create TRAD :                                                        267
C    Call To Local Procedure TRAD                                         267
         ASSIGN 5390 TO BLOCK_TRAD                                        267
         GO TO  5380                                                      267
5390     CONTINUE                                                         267
C    *                                                                    267
C    Geant POSITION : TRAD ===> MOTHER='ETRD' Z=ZPOSRAD <===              268
         CALL AGSRESET                                                    268
         AG_MOTHER = 'ETRD'                                               268
         AG_Z = ZPOSRAD                                                   268
         AG_TITLE = 'TRAD'                                                268
         AG_EXNAME = 'POSITION'                                           268
         AG_PARLIST = 'MOTHER_Z'                                          268
         CALL AXPOSITION                                                  268
C    *                                                                    268
         ZPOSABS = - EIDG_TRDTHICK/2 + EIDG_TRATHICK + EIDG_TABTHICK/2    268
C    Create TABS :                                                        270
C    Call To Local Procedure TABS                                         270
         ASSIGN 5410 TO BLOCK_TABS                                        270
         GO TO  5400                                                      270
5410     CONTINUE                                                         270
C    *                                                                    270
C    Geant POSITION : TABS ===> MOTHER='ETRD' Z=ZPOSABS <===              271
         CALL AGSRESET                                                    271
         AG_MOTHER = 'ETRD'                                               271
         AG_Z = ZPOSABS                                                   271
         AG_TITLE = 'TABS'                                                271
         AG_EXNAME = 'POSITION'                                           271
         AG_PARLIST = 'MOTHER_Z'                                          271
         CALL AXPOSITION                                                  271
C    *                                                                    271
         ZPOSBOD = - EIDG_TRDTHICK/2 + EIDG_TRATHICK + EIDG_TABTHICK +    271
     *   EIDG_TBDTHICK/2                                                  271
C    Create TBOD :                                                        273
C    Call To Local Procedure TBOD                                         273
         ASSIGN 5430 TO BLOCK_TBOD                                        273
         GO TO  5420                                                      273
5430     CONTINUE                                                         273
C    *                                                                    273
C    Geant POSITION : TBOD ===> MOTHER='ETRD' Z=ZPOSBOD <===              274
         CALL AGSRESET                                                    274
         AG_MOTHER = 'ETRD'                                               274
         AG_Z = ZPOSBOD                                                   274
         AG_TITLE = 'TBOD'                                                274
         AG_EXNAME = 'POSITION'                                           274
         AG_PARLIST = 'MOTHER_Z'                                          274
         CALL AXPOSITION                                                  274
C    *                                                                    274
5091  CALL AGSPOPB                                                        275
      GO TO BLOCK_ETRD, (5090 ,5100)                                      275
C End of Local Procedure                                                  275
C ----------------------------------------------------------------------  276
C ------------ TRAD                                                       276
C Begin Local Procedure TRAD                                              277
C TRAD IS THE TR RADIATOR                                                 277
5380  CONTINUE                                                            277
         AG_TITLE='TRAD'                                                  277
         AG_MODULE=MODULE                                                 277
         AG_MARK='V00'                                                    277
         CALL AGSPUSH                                                     277
         AG_MODULE=MODULE                                                 277
C    *                                                                    277
C    Geant MATERIAL : RADIATOR ===> inherited arguments used <===         278
         CALL AGSRESET                                                    278
         AG_TITLE = 'RADIATOR'                                            278
         AG_EXNAME = 'MATERIAL'                                           278
         AG_PARLIST = 'NONE'                                              278
         CALL AXMATERIAL                                                  278
C    *                                                                    278
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           278
         CALL AGSRESET                                                    279
         AG_TITLE = 'STANDARD'                                            279
         AG_EXNAME = 'MEDIUM'                                             279
         AG_PARLIST = 'NONE'                                              279
         CALL AXMEDIUM                                                    279
C    *                                                                    279
C    Geant ATTRIBUTE : TRAD ===> SEEN=1 COLO=6 <===                       279
         CALL AGSRESET                                                    280
         AG_SEEN = 1                                                      280
         AG_COLO = 6                                                      280
         AG_TITLE = 'TRAD'                                                280
         AG_EXNAME = 'ATTRIBUTE'                                          280
         AG_PARLIST = 'SEEN_COLO'                                         280
         CALL AXATTRIBUTE                                                 280
C    *                                                                    280
C    Geant SHAPE : TUBS ===> RMIN=EIDG_RMIN RMAX=EIDG_RMAX, PHI1=-EIDG_D  280
C    PHI/2 PHI2=EIDG_DPHI/2 DZ=EIDG_TRDTHICK/2 <===                       280
         CALL AGSRESET                                                    282
         AG_RMIN = EIDG_RMIN                                              282
         AG_RMAX = EIDG_RMAX                                              282
         AG_PHI1 = -EIDG_DPHI/2                                           282
         AG_PHI2 = EIDG_DPHI/2                                            282
         AG_DZ = EIDG_TRDTHICK/2                                          282
         AG_TITLE = 'TUBS'                                                282
         AG_EXNAME = 'SHAPE'                                              282
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            282
         CALL AXSHAPE                                                     282
C    *                                                                    282
5381  CALL AGSPOPB                                                        283
      GO TO BLOCK_TRAD, (5380 ,5390)                                      283
C End of Local Procedure                                                  283
C ----------------------------------------------------------------------  284
C ------------ TABS                                                       284
C Begin Local Procedure TABS                                              285
C TABS IS THE TRD XE ABSORBER (SENSITIVE)                                 285
5400  CONTINUE                                                            285
         AG_TITLE='TABS'                                                  285
         AG_MODULE=MODULE                                                 285
         AG_MARK='V00'                                                    285
         CALL AGSPUSH                                                     285
         AG_MODULE=MODULE                                                 285
C    *                                                                    285
C    Geant MATERIAL : XE15CO2 ===> inherited arguments used <===          286
         CALL AGSRESET                                                    286
         AG_TITLE = 'XE15CO2'                                             286
         AG_EXNAME = 'MATERIAL'                                           286
         AG_PARLIST = 'NONE'                                              286
         CALL AXMATERIAL                                                  286
C    *                                                                    286
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           286
         CALL AGSRESET                                                    287
         AG_ISVOL = 1                                                     287
         AG_TITLE = 'SENSITIVE'                                           287
         AG_EXNAME = 'MEDIUM'                                             287
         AG_PARLIST = 'ISVOL'                                             287
         CALL AXMEDIUM                                                    287
C    *                                                                    287
C    Geant ATTRIBUTE : TABS ===> SEEN=1 COLO=8 <===                       287
         CALL AGSRESET                                                    288
         AG_SEEN = 1                                                      288
         AG_COLO = 8                                                      288
         AG_TITLE = 'TABS'                                                288
         AG_EXNAME = 'ATTRIBUTE'                                          288
         AG_PARLIST = 'SEEN_COLO'                                         288
         CALL AXATTRIBUTE                                                 288
C    *                                                                    288
C    Geant SHAPE : TUBS ===> RMIN=EIDG_RMIN RMAX=EIDG_RMAX, PHI1=-EIDG_D  288
C    PHI/2 PHI2=EIDG_DPHI/2 DZ=EIDG_TABTHICK/2 <===                       288
         CALL AGSRESET                                                    290
         AG_RMIN = EIDG_RMIN                                              290
         AG_RMAX = EIDG_RMAX                                              290
         AG_PHI1 = -EIDG_DPHI/2                                           290
         AG_PHI2 = EIDG_DPHI/2                                            290
         AG_DZ = EIDG_TABTHICK/2                                          290
         AG_TITLE = 'TUBS'                                                290
         AG_EXNAME = 'SHAPE'                                              290
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            290
         CALL AXSHAPE                                                     290
C    *                                                                    290
C    Defining Hits: Z:0.0005:S Y:0.0005: Z:0.0005: CX:10: CY:10: CZ:10:   290
C    LPTOT:18:(-3,2), SLENG:0.1:(0,800) TOF:16:(0,1.0E-6) LGAM:16:(-2,+4  290
C    ) STEP:11:(0,10) ELOSS:16:(0,0.01)                                   290
         AG_TITLE = 'TABS'                                                292
         AG_EXNAME = 'HITS'                                               292
         AG_TYPE=AG_MAGIC                                                 292
         CALL AGSDETV('TABS')                                             292
C    *                                                                    292
         AG_PARLIST='Z'                                                   292
         AG_TITLE = 'DETH'                                                292
         AG_NBITS = AG_MAGIC                                              292
         AG_BIN=0.0005                                                    292
         AG_OPTION='S'                                                    292
         AG_FHMIN = AG_MAGIC                                              292
         AG_FHMAX = AG_MAGIC                                              292
         CALL AGSDETH (0)                                                 292
C    *                                                                    292
         AG_PARLIST='Y'                                                   292
         AG_TITLE = 'DETH'                                                292
         AG_NBITS = AG_MAGIC                                              292
         AG_BIN=0.0005                                                    292
         AG_FHMIN=AG_MAGIC                                                292
         AG_FHMAX=AG_MAGIC                                                292
         AG_OPTION='*'                                                    292
         CALL AGSDETH (0)                                                 292
C    *                                                                    292
         AG_PARLIST='Z'                                                   292
         AG_TITLE = 'DETH'                                                292
         AG_NBITS = AG_MAGIC                                              292
         AG_BIN=0.0005                                                    292
         AG_FHMIN=AG_MAGIC                                                292
         AG_FHMAX=AG_MAGIC                                                292
         AG_OPTION='*'                                                    292
         CALL AGSDETH (0)                                                 292
C    *                                                                    292
         AG_PARLIST='CX'                                                  292
         AG_TITLE = 'DETH'                                                292
         AG_NBITS = 10                                                    292
         AG_BIN=AG_MAGIC                                                  292
         AG_FHMIN=AG_MAGIC                                                292
         AG_FHMAX=AG_MAGIC                                                292
         AG_OPTION='*'                                                    292
         CALL AGSDETH (0)                                                 292
C    *                                                                    292
         AG_PARLIST='CY'                                                  292
         AG_TITLE = 'DETH'                                                292
         AG_NBITS = 10                                                    292
         AG_BIN=AG_MAGIC                                                  292
         AG_FHMIN=AG_MAGIC                                                292
         AG_FHMAX=AG_MAGIC                                                292
         AG_OPTION='*'                                                    292
         CALL AGSDETH (0)                                                 292
C    *                                                                    292
         AG_PARLIST='CZ'                                                  292
         AG_TITLE = 'DETH'                                                292
         AG_NBITS = 10                                                    292
         AG_BIN=AG_MAGIC                                                  292
         AG_FHMIN=AG_MAGIC                                                292
         AG_FHMAX=AG_MAGIC                                                292
         AG_OPTION='*'                                                    292
         CALL AGSDETH (0)                                                 292
C    *                                                                    292
         AG_PARLIST='LPTOT'                                               292
         AG_TITLE = 'DETH'                                                292
         AG_NBITS = 18                                                    292
         AG_BIN=AG_MAGIC                                                  292
         AG_OPTION='*'                                                    292
         AG_FHMIN = -3                                                    292
         AG_FHMAX = 2                                                     292
         CALL AGSDETH (0)                                                 292
C    *                                                                    292
         AG_PARLIST='SLENG'                                               292
         AG_TITLE = 'DETH'                                                292
         AG_NBITS = AG_MAGIC                                              292
         AG_BIN=0.1                                                       292
         AG_OPTION='*'                                                    292
         AG_FHMIN = 0                                                     292
         AG_FHMAX = 800                                                   292
         CALL AGSDETH (0)                                                 292
C    *                                                                    292
         AG_PARLIST='TOF'                                                 292
         AG_TITLE = 'DETH'                                                292
         AG_NBITS = 16                                                    292
         AG_BIN=AG_MAGIC                                                  292
         AG_OPTION='*'                                                    292
         AG_FHMIN = 0                                                     292
         AG_FHMAX = 1.0E-6                                                292
         CALL AGSDETH (0)                                                 292
C    *                                                                    292
         AG_PARLIST='LGAM'                                                292
         AG_TITLE = 'DETH'                                                292
         AG_NBITS = 16                                                    292
         AG_BIN=AG_MAGIC                                                  292
         AG_OPTION='*'                                                    292
         AG_FHMIN = -2                                                    292
         AG_FHMAX = +4                                                    292
         CALL AGSDETH (0)                                                 292
C    *                                                                    292
         AG_PARLIST='STEP'                                                292
         AG_TITLE = 'DETH'                                                292
         AG_NBITS = 11                                                    292
         AG_BIN=AG_MAGIC                                                  292
         AG_OPTION='*'                                                    292
         AG_FHMIN = 0                                                     292
         AG_FHMAX = 10                                                    292
         CALL AGSDETH (0)                                                 292
C    *                                                                    292
         AG_PARLIST='ELOSS'                                               292
         AG_TITLE = 'DETH'                                                292
         AG_NBITS = 16                                                    292
         AG_BIN=AG_MAGIC                                                  292
         AG_OPTION='*'                                                    292
         AG_FHMIN = 0                                                     292
         AG_FHMAX = 0.01                                                  292
         CALL AGSDETH (0)                                                 292
C    *                                                                    292
         CALL AGSDETU('TABS')                                             292
C    *                                                                    292
5401  CALL AGSPOPB                                                        293
      GO TO BLOCK_TABS, (5400 ,5410)                                      293
C End of Local Procedure                                                  293
C ----------------------------------------------------------------------  294
C ------------ TBOD                                                       294
C Begin Local Procedure TBOD                                              295
C TBOD IS THE TRD READOUT MOTHER BOARD                                    295
5420  CONTINUE                                                            295
         AG_TITLE='TBOD'                                                  295
         AG_MODULE=MODULE                                                 295
         AG_MARK='V00'                                                    295
         CALL AGSPUSH                                                     295
         AG_MODULE=MODULE                                                 295
C    *                                                                    295
C    Geant MATERIAL : G10 ===> inherited arguments used <===              296
         CALL AGSRESET                                                    296
         AG_TITLE = 'G10'                                                 296
         AG_EXNAME = 'MATERIAL'                                           296
         AG_PARLIST = 'NONE'                                              296
         CALL AXMATERIAL                                                  296
C    *                                                                    296
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           296
         CALL AGSRESET                                                    297
         AG_TITLE = 'STANDARD'                                            297
         AG_EXNAME = 'MEDIUM'                                             297
         AG_PARLIST = 'NONE'                                              297
         CALL AXMEDIUM                                                    297
C    *                                                                    297
C    Geant ATTRIBUTE : TBOD ===> SEEN=1 COLO=7 <===                       297
         CALL AGSRESET                                                    298
         AG_SEEN = 1                                                      298
         AG_COLO = 7                                                      298
         AG_TITLE = 'TBOD'                                                298
         AG_EXNAME = 'ATTRIBUTE'                                          298
         AG_PARLIST = 'SEEN_COLO'                                         298
         CALL AXATTRIBUTE                                                 298
C    *                                                                    298
C    Geant SHAPE : TUBS ===> RMIN=EIDG_RMIN RMAX=EIDG_RMAX, PHI1=-EIDG_D  298
C    PHI/2 PHI2=EIDG_DPHI/2 DZ=EIDG_TBDTHICK/2 <===                       298
         CALL AGSRESET                                                    300
         AG_RMIN = EIDG_RMIN                                              300
         AG_RMAX = EIDG_RMAX                                              300
         AG_PHI1 = -EIDG_DPHI/2                                           300
         AG_PHI2 = EIDG_DPHI/2                                            300
         AG_DZ = EIDG_TBDTHICK/2                                          300
         AG_TITLE = 'TUBS'                                                300
         AG_EXNAME = 'SHAPE'                                              300
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            300
         CALL AXSHAPE                                                     300
C    *                                                                    300
5421  CALL AGSPOPB                                                        301
      GO TO BLOCK_TBOD, (5420 ,5430)                                      301
C End of Local Procedure                                                  301
      WRITE(LOUT,5440)'===>>  [End AgML Module EIDDGEO]  <<==='           301
5440  FORMAT(A32,/,/)                                                     301
C MODULE EIDDGEO                                                          303
      END                                                                 303
