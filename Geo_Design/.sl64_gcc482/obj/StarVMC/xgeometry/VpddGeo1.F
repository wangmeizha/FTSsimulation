C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE VPDDGEO1                                                 1
C VPDDGEO1 IS THE PSEUDO VERTEX POSITION DETECTOR OF STAR                 1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'VPDDGEO1'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'IS THE PSEUDO VERTEX POSITION DETECTOR OF STAR'/     1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C AUTHOR FRANK GEURTS                                                     1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'FRANK GEURTS'/                                2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C CREATED 21 JUNE 2000                                                    2
      CHARACTER*40 CREATED /'21 JUNE 2000'/                               3
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
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            5
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      5
      COMMON/GCMAIL/ CHMAIL                                               5
      CHARACTER*132 CHMAIL                                                5
C - Natural constants (or considered as such)                             5
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   6
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        6
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       6
      INTEGER BLOCK_VPDD                                                  6
      INTEGER BLOCK_VPBP                                                  9
      INTEGER BLOCK_VPBO                                                  9
      INTEGER BLOCK_VPBA                                                  9
      INTEGER BLOCK_VPBB                                                  9
      INTEGER BLOCK_VPBX                                                  9
      INTEGER BLOCK_VFEE                                                  9
      INTEGER BLOCK_VLEM                                                  9
      INTEGER BLOCK_VPIP                                                  9
      INTEGER BLOCK_VPFP                                                  9
      INTEGER BLOCK_VPFA                                                  9
      INTEGER BLOCK_VPFB                                                  9
      INTEGER BLOCK_VPFC                                                  9
      INTEGER BLOCK_VPBI                                                  9
      INTEGER BLOCK_VPST                                                  9
      INTEGER BLOCK_VPSC                                                  9
      INTEGER BLOCK_VPSA                                                  9
      INTEGER BLOCK_VPSB                                                  9
      INTEGER BLOCK_VPSV                                                  9
      INTEGER BLOCK_VPSW                                                  9
      INTEGER BLOCK_VRNG                                                  9
      INTEGER BLOCK_VSEC                                                  9
      INTEGER BLOCK_VDET                                                  9
      INTEGER BLOCK_VXST                                                  9
      INTEGER BLOCK_VCNV                                                  9
      INTEGER BLOCK_VPMT                                                  9
      INTEGER BLOCK_VRAD                                                  9
      INTEGER BLOCK_VPCL                                                  9
      INTEGER BLOCK_VPCF                                                  9
      INTEGER BLOCK_VPCV                                                  9
      INTEGER BLOCK_VPCH                                                  9
      INTEGER BLOCK_VDTI                                                  9
      INTEGER BLOCK_IBEM                                                  9
      INTEGER BLOCK_IBEH                                                  9
      INTEGER BLOCK_IBEV                                                  9
      INTEGER BLOCK_IBEW                                                  9
C -                                                                       10
      REAL VPDV_VERSION                                                   10
      EQUIVALENCE (BANK_VPDV(3),VPDV_VERSION,RE_VPDV_VERSION),            10
     *(NAMES_VPDV(1),NVPDV_VERSION)                                       10
      EQUIVALENCE (COMMEN_VPDV(1),CVPDV_VERSION),(MAP_VPDV(1,1),          10
     *MVPDV_VERSION)                                                      10
      CHARACTER CVPDV_VERSION*40/' '/                                     10
      CHARACTER NVPDV_VERSION*12/'R.VERSION'/                             10
      INTEGER MVPDV_VERSION(2)/1,1/                                       10
      REAL RE_VPDV_VERSION                                                10
C -                                                                       10
      INTEGER VPDV_VPDCONFIG                                              10
      EQUIVALENCE (BANK_VPDV(4),VPDV_VPDCONFIG,RE_VPDV_VPDCONFIG),        10
     *(NAMES_VPDV(2),NVPDV_VPDCONFIG)                                     10
      EQUIVALENCE (COMMEN_VPDV(2),CVPDV_VPDCONFIG),(MAP_VPDV(1,2),        10
     *MVPDV_VPDCONFIG)                                                    10
      CHARACTER CVPDV_VPDCONFIG*40/' '/                                   10
      CHARACTER NVPDV_VPDCONFIG*12/'I.VPDCONFIG'/                         10
      INTEGER MVPDV_VPDCONFIG(2)/1,1/                                     10
      REAL RE_VPDV_VPDCONFIG                                              10
C -                                                                       10
      INTEGER LEN_VPDV,LLEN_VPDV,LINK_VPDV                                10
      PARAMETER (LEN_VPDV=4)                                              10
      PARAMETER (LLEN_VPDV=2)                                             10
      INTEGER FLAG_VPDV/0/,MAP_VPDV(2,LLEN_VPDV),IDNUM_VPDV(10)           10
      CHARACTER NAMES_VPDV(LLEN_VPDV)*12,COMMEN_VPDV(LLEN_VPDV)*40        10
      DATA IDNUM_VPDV/10*0/                                               10
      REAL BANK_VPDV(LEN_VPDV)                                            10
      CHARACTER*60 BPATH_VPDV/' '/                                        10
      SAVE FLAG_VPDV,MAP_VPDV,IDNUM_VPDV,NAMES_VPDV,COMMEN_VPDV,          10
     *BPATH_VPDV                                                          10
      DATA LINK_VPDV/0/,BANK_VPDV/LEN_VPDV*0/                             10
C *                                                                       10
C -                                                                       16
      REAL VPDG_VERSION                                                   16
      EQUIVALENCE (BANK_VPDG(3),VPDG_VERSION,RE_VPDG_VERSION),            16
     *(NAMES_VPDG(1),NVPDG_VERSION)                                       16
      EQUIVALENCE (COMMEN_VPDG(1),CVPDG_VERSION),(MAP_VPDG(1,1),          16
     *MVPDG_VERSION)                                                      16
      CHARACTER CVPDG_VERSION*40/' '/                                     16
      CHARACTER NVPDG_VERSION*12/'R.VERSION'/                             16
      INTEGER MVPDG_VERSION(2)/1,1/                                       16
      REAL RE_VPDG_VERSION                                                16
C -                                                                       16
      REAL VPDG_ZPOSEAST                                                  16
      EQUIVALENCE (BANK_VPDG(4),VPDG_ZPOSEAST,RE_VPDG_ZPOSEAST),          16
     *(NAMES_VPDG(2),NVPDG_ZPOSEAST)                                      16
      EQUIVALENCE (COMMEN_VPDG(2),CVPDG_ZPOSEAST),(MAP_VPDG(1,2),         16
     *MVPDG_ZPOSEAST)                                                     16
      CHARACTER CVPDG_ZPOSEAST*40/' '/                                    16
      CHARACTER NVPDG_ZPOSEAST*12/'R.ZPOSEAST'/                           16
      INTEGER MVPDG_ZPOSEAST(2)/1,1/                                      16
      REAL RE_VPDG_ZPOSEAST                                               16
C -                                                                       16
      REAL VPDG_ZPOSWEST                                                  16
      EQUIVALENCE (BANK_VPDG(5),VPDG_ZPOSWEST,RE_VPDG_ZPOSWEST),          16
     *(NAMES_VPDG(3),NVPDG_ZPOSWEST)                                      16
      EQUIVALENCE (COMMEN_VPDG(3),CVPDG_ZPOSWEST),(MAP_VPDG(1,3),         16
     *MVPDG_ZPOSWEST)                                                     16
      CHARACTER CVPDG_ZPOSWEST*40/' '/                                    16
      CHARACTER NVPDG_ZPOSWEST*12/'R.ZPOSWEST'/                           16
      INTEGER MVPDG_ZPOSWEST(2)/1,1/                                      16
      REAL RE_VPDG_ZPOSWEST                                               16
C -                                                                       16
      REAL VPDG_RMIN                                                      16
      EQUIVALENCE (BANK_VPDG(6),VPDG_RMIN,RE_VPDG_RMIN),(NAMES_VPDG(4),   16
     *NVPDG_RMIN)                                                         16
      EQUIVALENCE (COMMEN_VPDG(4),CVPDG_RMIN),(MAP_VPDG(1,4),             16
     *MVPDG_RMIN)                                                         16
      CHARACTER CVPDG_RMIN*40/' '/                                        16
      CHARACTER NVPDG_RMIN*12/'R.RMIN'/                                   16
      INTEGER MVPDG_RMIN(2)/1,1/                                          16
      REAL RE_VPDG_RMIN                                                   16
C -                                                                       16
      REAL VPDG_RMAX                                                      16
      EQUIVALENCE (BANK_VPDG(7),VPDG_RMAX,RE_VPDG_RMAX),(NAMES_VPDG(5),   16
     *NVPDG_RMAX)                                                         16
      EQUIVALENCE (COMMEN_VPDG(5),CVPDG_RMAX),(MAP_VPDG(1,5),             16
     *MVPDG_RMAX)                                                         16
      CHARACTER CVPDG_RMAX*40/' '/                                        16
      CHARACTER NVPDG_RMAX*12/'R.RMAX'/                                   16
      INTEGER MVPDG_RMAX(2)/1,1/                                          16
      REAL RE_VPDG_RMAX                                                   16
C -                                                                       16
      REAL VPDG_BPWIDTH                                                   16
      EQUIVALENCE (BANK_VPDG(8),VPDG_BPWIDTH,RE_VPDG_BPWIDTH),            16
     *(NAMES_VPDG(6),NVPDG_BPWIDTH)                                       16
      EQUIVALENCE (COMMEN_VPDG(6),CVPDG_BPWIDTH),(MAP_VPDG(1,6),          16
     *MVPDG_BPWIDTH)                                                      16
      CHARACTER CVPDG_BPWIDTH*40/' '/                                     16
      CHARACTER NVPDG_BPWIDTH*12/'R.BPWIDTH'/                             16
      INTEGER MVPDG_BPWIDTH(2)/1,1/                                       16
      REAL RE_VPDG_BPWIDTH                                                16
C -                                                                       16
      REAL VPDG_BPLENGTH                                                  16
      EQUIVALENCE (BANK_VPDG(9),VPDG_BPLENGTH,RE_VPDG_BPLENGTH),          16
     *(NAMES_VPDG(7),NVPDG_BPLENGTH)                                      16
      EQUIVALENCE (COMMEN_VPDG(7),CVPDG_BPLENGTH),(MAP_VPDG(1,7),         16
     *MVPDG_BPLENGTH)                                                     16
      CHARACTER CVPDG_BPLENGTH*40/' '/                                    16
      CHARACTER NVPDG_BPLENGTH*12/'R.BPLENGTH'/                           16
      INTEGER MVPDG_BPLENGTH(2)/1,1/                                      16
      REAL RE_VPDG_BPLENGTH                                               16
C -                                                                       16
      REAL VPDG_BPTHICK                                                   16
      EQUIVALENCE (BANK_VPDG(10),VPDG_BPTHICK,RE_VPDG_BPTHICK),           16
     *(NAMES_VPDG(8),NVPDG_BPTHICK)                                       16
      EQUIVALENCE (COMMEN_VPDG(8),CVPDG_BPTHICK),(MAP_VPDG(1,8),          16
     *MVPDG_BPTHICK)                                                      16
      CHARACTER CVPDG_BPTHICK*40/' '/                                     16
      CHARACTER NVPDG_BPTHICK*12/'R.BPTHICK'/                             16
      INTEGER MVPDG_BPTHICK(2)/1,1/                                       16
      REAL RE_VPDG_BPTHICK                                                16
C -                                                                       16
      REAL VPDG_BXHEIGHT                                                  16
      EQUIVALENCE (BANK_VPDG(11),VPDG_BXHEIGHT,RE_VPDG_BXHEIGHT),         16
     *(NAMES_VPDG(9),NVPDG_BXHEIGHT)                                      16
      EQUIVALENCE (COMMEN_VPDG(9),CVPDG_BXHEIGHT),(MAP_VPDG(1,9),         16
     *MVPDG_BXHEIGHT)                                                     16
      CHARACTER CVPDG_BXHEIGHT*40/' '/                                    16
      CHARACTER NVPDG_BXHEIGHT*12/'R.BXHEIGHT'/                           16
      INTEGER MVPDG_BXHEIGHT(2)/1,1/                                      16
      REAL RE_VPDG_BXHEIGHT                                               16
C -                                                                       16
      REAL VPDG_BXLENGTH                                                  16
      EQUIVALENCE (BANK_VPDG(12),VPDG_BXLENGTH,RE_VPDG_BXLENGTH),         16
     *(NAMES_VPDG(10),NVPDG_BXLENGTH)                                     16
      EQUIVALENCE (COMMEN_VPDG(10),CVPDG_BXLENGTH),(MAP_VPDG(1,10),       16
     *MVPDG_BXLENGTH)                                                     16
      CHARACTER CVPDG_BXLENGTH*40/' '/                                    16
      CHARACTER NVPDG_BXLENGTH*12/'R.BXLENGTH'/                           16
      INTEGER MVPDG_BXLENGTH(2)/1,1/                                      16
      REAL RE_VPDG_BXLENGTH                                               16
C -                                                                       16
      REAL VPDG_BXWIDTH                                                   16
      EQUIVALENCE (BANK_VPDG(13),VPDG_BXWIDTH,RE_VPDG_BXWIDTH),           16
     *(NAMES_VPDG(11),NVPDG_BXWIDTH)                                      16
      EQUIVALENCE (COMMEN_VPDG(11),CVPDG_BXWIDTH),(MAP_VPDG(1,11),        16
     *MVPDG_BXWIDTH)                                                      16
      CHARACTER CVPDG_BXWIDTH*40/' '/                                     16
      CHARACTER NVPDG_BXWIDTH*12/'R.BXWIDTH'/                             16
      INTEGER MVPDG_BXWIDTH(2)/1,1/                                       16
      REAL RE_VPDG_BXWIDTH                                                16
C -                                                                       16
      REAL VPDG_BXTHICK                                                   16
      EQUIVALENCE (BANK_VPDG(14),VPDG_BXTHICK,RE_VPDG_BXTHICK),           16
     *(NAMES_VPDG(12),NVPDG_BXTHICK)                                      16
      EQUIVALENCE (COMMEN_VPDG(12),CVPDG_BXTHICK),(MAP_VPDG(1,12),        16
     *MVPDG_BXTHICK)                                                      16
      CHARACTER CVPDG_BXTHICK*40/' '/                                     16
      CHARACTER NVPDG_BXTHICK*12/'R.BXTHICK'/                             16
      INTEGER MVPDG_BXTHICK(2)/1,1/                                       16
      REAL RE_VPDG_BXTHICK                                                16
C -                                                                       16
      REAL VPDG_BXZPOSC                                                   16
      EQUIVALENCE (BANK_VPDG(15),VPDG_BXZPOSC,RE_VPDG_BXZPOSC),           16
     *(NAMES_VPDG(13),NVPDG_BXZPOSC)                                      16
      EQUIVALENCE (COMMEN_VPDG(13),CVPDG_BXZPOSC),(MAP_VPDG(1,13),        16
     *MVPDG_BXZPOSC)                                                      16
      CHARACTER CVPDG_BXZPOSC*40/' '/                                     16
      CHARACTER NVPDG_BXZPOSC*12/'R.BXZPOSC'/                             16
      INTEGER MVPDG_BXZPOSC(2)/1,1/                                       16
      REAL RE_VPDG_BXZPOSC                                                16
C -                                                                       16
      REAL VPDG_FPWIDTH                                                   16
      EQUIVALENCE (BANK_VPDG(16),VPDG_FPWIDTH,RE_VPDG_FPWIDTH),           16
     *(NAMES_VPDG(14),NVPDG_FPWIDTH)                                      16
      EQUIVALENCE (COMMEN_VPDG(14),CVPDG_FPWIDTH),(MAP_VPDG(1,14),        16
     *MVPDG_FPWIDTH)                                                      16
      CHARACTER CVPDG_FPWIDTH*40/' '/                                     16
      CHARACTER NVPDG_FPWIDTH*12/'R.FPWIDTH'/                             16
      INTEGER MVPDG_FPWIDTH(2)/1,1/                                       16
      REAL RE_VPDG_FPWIDTH                                                16
C -                                                                       16
      REAL VPDG_FPHEIGHT                                                  16
      EQUIVALENCE (BANK_VPDG(17),VPDG_FPHEIGHT,RE_VPDG_FPHEIGHT),         16
     *(NAMES_VPDG(15),NVPDG_FPHEIGHT)                                     16
      EQUIVALENCE (COMMEN_VPDG(15),CVPDG_FPHEIGHT),(MAP_VPDG(1,15),       16
     *MVPDG_FPHEIGHT)                                                     16
      CHARACTER CVPDG_FPHEIGHT*40/' '/                                    16
      CHARACTER NVPDG_FPHEIGHT*12/'R.FPHEIGHT'/                           16
      INTEGER MVPDG_FPHEIGHT(2)/1,1/                                      16
      REAL RE_VPDG_FPHEIGHT                                               16
C -                                                                       16
      REAL VPDG_FPTHICK                                                   16
      EQUIVALENCE (BANK_VPDG(18),VPDG_FPTHICK,RE_VPDG_FPTHICK),           16
     *(NAMES_VPDG(16),NVPDG_FPTHICK)                                      16
      EQUIVALENCE (COMMEN_VPDG(16),CVPDG_FPTHICK),(MAP_VPDG(1,16),        16
     *MVPDG_FPTHICK)                                                      16
      CHARACTER CVPDG_FPTHICK*40/' '/                                     16
      CHARACTER NVPDG_FPTHICK*12/'R.FPTHICK'/                             16
      INTEGER MVPDG_FPTHICK(2)/1,1/                                       16
      REAL RE_VPDG_FPTHICK                                                16
C -                                                                       16
      REAL VPDG_FPAWIDTH                                                  16
      EQUIVALENCE (BANK_VPDG(19),VPDG_FPAWIDTH,RE_VPDG_FPAWIDTH),         16
     *(NAMES_VPDG(17),NVPDG_FPAWIDTH)                                     16
      EQUIVALENCE (COMMEN_VPDG(17),CVPDG_FPAWIDTH),(MAP_VPDG(1,17),       16
     *MVPDG_FPAWIDTH)                                                     16
      CHARACTER CVPDG_FPAWIDTH*40/' '/                                    16
      CHARACTER NVPDG_FPAWIDTH*12/'R.FPAWIDTH'/                           16
      INTEGER MVPDG_FPAWIDTH(2)/1,1/                                      16
      REAL RE_VPDG_FPAWIDTH                                               16
C -                                                                       16
      REAL VPDG_FPAHGHT                                                   16
      EQUIVALENCE (BANK_VPDG(20),VPDG_FPAHGHT,RE_VPDG_FPAHGHT),           16
     *(NAMES_VPDG(18),NVPDG_FPAHGHT)                                      16
      EQUIVALENCE (COMMEN_VPDG(18),CVPDG_FPAHGHT),(MAP_VPDG(1,18),        16
     *MVPDG_FPAHGHT)                                                      16
      CHARACTER CVPDG_FPAHGHT*40/' '/                                     16
      CHARACTER NVPDG_FPAHGHT*12/'R.FPAHGHT'/                             16
      INTEGER MVPDG_FPAHGHT(2)/1,1/                                       16
      REAL RE_VPDG_FPAHGHT                                                16
C -                                                                       16
      REAL VPDG_FPCHGHT                                                   16
      EQUIVALENCE (BANK_VPDG(21),VPDG_FPCHGHT,RE_VPDG_FPCHGHT),           16
     *(NAMES_VPDG(19),NVPDG_FPCHGHT)                                      16
      EQUIVALENCE (COMMEN_VPDG(19),CVPDG_FPCHGHT),(MAP_VPDG(1,19),        16
     *MVPDG_FPCHGHT)                                                      16
      CHARACTER CVPDG_FPCHGHT*40/' '/                                     16
      CHARACTER NVPDG_FPCHGHT*12/'R.FPCHGHT'/                             16
      INTEGER MVPDG_FPCHGHT(2)/1,1/                                       16
      REAL RE_VPDG_FPCHGHT                                                16
C -                                                                       16
      REAL VPDG_FPHHGHT                                                   16
      EQUIVALENCE (BANK_VPDG(22),VPDG_FPHHGHT,RE_VPDG_FPHHGHT),           16
     *(NAMES_VPDG(20),NVPDG_FPHHGHT)                                      16
      EQUIVALENCE (COMMEN_VPDG(20),CVPDG_FPHHGHT),(MAP_VPDG(1,20),        16
     *MVPDG_FPHHGHT)                                                      16
      CHARACTER CVPDG_FPHHGHT*40/' '/                                     16
      CHARACTER NVPDG_FPHHGHT*12/'R.FPHHGHT'/                             16
      INTEGER MVPDG_FPHHGHT(2)/1,1/                                       16
      REAL RE_VPDG_FPHHGHT                                                16
C -                                                                       16
      REAL VPDG_FPHWIDTH                                                  16
      EQUIVALENCE (BANK_VPDG(23),VPDG_FPHWIDTH,RE_VPDG_FPHWIDTH),         16
     *(NAMES_VPDG(21),NVPDG_FPHWIDTH)                                     16
      EQUIVALENCE (COMMEN_VPDG(21),CVPDG_FPHWIDTH),(MAP_VPDG(1,21),       16
     *MVPDG_FPHWIDTH)                                                     16
      CHARACTER CVPDG_FPHWIDTH*40/' '/                                    16
      CHARACTER NVPDG_FPHWIDTH*12/'R.FPHWIDTH'/                           16
      INTEGER MVPDG_FPHWIDTH(2)/1,1/                                      16
      REAL RE_VPDG_FPHWIDTH                                               16
C -                                                                       16
      REAL VPDG_FPHTHICK                                                  16
      EQUIVALENCE (BANK_VPDG(24),VPDG_FPHTHICK,RE_VPDG_FPHTHICK),         16
     *(NAMES_VPDG(22),NVPDG_FPHTHICK)                                     16
      EQUIVALENCE (COMMEN_VPDG(22),CVPDG_FPHTHICK),(MAP_VPDG(1,22),       16
     *MVPDG_FPHTHICK)                                                     16
      CHARACTER CVPDG_FPHTHICK*40/' '/                                    16
      CHARACTER NVPDG_FPHTHICK*12/'R.FPHTHICK'/                           16
      INTEGER MVPDG_FPHTHICK(2)/1,1/                                      16
      REAL RE_VPDG_FPHTHICK                                               16
C -                                                                       16
      REAL VPDG_STTHICK                                                   16
      EQUIVALENCE (BANK_VPDG(25),VPDG_STTHICK,RE_VPDG_STTHICK),           16
     *(NAMES_VPDG(23),NVPDG_STTHICK)                                      16
      EQUIVALENCE (COMMEN_VPDG(23),CVPDG_STTHICK),(MAP_VPDG(1,23),        16
     *MVPDG_STTHICK)                                                      16
      CHARACTER CVPDG_STTHICK*40/' '/                                     16
      CHARACTER NVPDG_STTHICK*12/'R.STTHICK'/                             16
      INTEGER MVPDG_STTHICK(2)/1,1/                                       16
      REAL RE_VPDG_STTHICK                                                16
C -                                                                       16
      REAL VPDG_STHEIGHT                                                  16
      EQUIVALENCE (BANK_VPDG(26),VPDG_STHEIGHT,RE_VPDG_STHEIGHT),         16
     *(NAMES_VPDG(24),NVPDG_STHEIGHT)                                     16
      EQUIVALENCE (COMMEN_VPDG(24),CVPDG_STHEIGHT),(MAP_VPDG(1,24),       16
     *MVPDG_STHEIGHT)                                                     16
      CHARACTER CVPDG_STHEIGHT*40/' '/                                    16
      CHARACTER NVPDG_STHEIGHT*12/'R.STHEIGHT'/                           16
      INTEGER MVPDG_STHEIGHT(2)/1,1/                                      16
      REAL RE_VPDG_STHEIGHT                                               16
C -                                                                       16
      REAL VPDG_STANGLE                                                   16
      EQUIVALENCE (BANK_VPDG(27),VPDG_STANGLE,RE_VPDG_STANGLE),           16
     *(NAMES_VPDG(25),NVPDG_STANGLE)                                      16
      EQUIVALENCE (COMMEN_VPDG(25),CVPDG_STANGLE),(MAP_VPDG(1,25),        16
     *MVPDG_STANGLE)                                                      16
      CHARACTER CVPDG_STANGLE*40/' '/                                     16
      CHARACTER NVPDG_STANGLE*12/'R.STANGLE'/                             16
      INTEGER MVPDG_STANGLE(2)/1,1/                                       16
      REAL RE_VPDG_STANGLE                                                16
C -                                                                       16
      REAL VPDG_STDIAGSZ                                                  16
      EQUIVALENCE (BANK_VPDG(28),VPDG_STDIAGSZ,RE_VPDG_STDIAGSZ),         16
     *(NAMES_VPDG(26),NVPDG_STDIAGSZ)                                     16
      EQUIVALENCE (COMMEN_VPDG(26),CVPDG_STDIAGSZ),(MAP_VPDG(1,26),       16
     *MVPDG_STDIAGSZ)                                                     16
      CHARACTER CVPDG_STDIAGSZ*40/' '/                                    16
      CHARACTER NVPDG_STDIAGSZ*12/'R.STDIAGSZ'/                           16
      INTEGER MVPDG_STDIAGSZ(2)/1,1/                                      16
      REAL RE_VPDG_STDIAGSZ                                               16
C -                                                                       16
      REAL VPDG_SCWIDTH                                                   16
      EQUIVALENCE (BANK_VPDG(29),VPDG_SCWIDTH,RE_VPDG_SCWIDTH),           16
     *(NAMES_VPDG(27),NVPDG_SCWIDTH)                                      16
      EQUIVALENCE (COMMEN_VPDG(27),CVPDG_SCWIDTH),(MAP_VPDG(1,27),        16
     *MVPDG_SCWIDTH)                                                      16
      CHARACTER CVPDG_SCWIDTH*40/' '/                                     16
      CHARACTER NVPDG_SCWIDTH*12/'R.SCWIDTH'/                             16
      INTEGER MVPDG_SCWIDTH(2)/1,1/                                       16
      REAL RE_VPDG_SCWIDTH                                                16
C -                                                                       16
      REAL VPDG_SCHEIGHT                                                  16
      EQUIVALENCE (BANK_VPDG(30),VPDG_SCHEIGHT,RE_VPDG_SCHEIGHT),         16
     *(NAMES_VPDG(28),NVPDG_SCHEIGHT)                                     16
      EQUIVALENCE (COMMEN_VPDG(28),CVPDG_SCHEIGHT),(MAP_VPDG(1,28),       16
     *MVPDG_SCHEIGHT)                                                     16
      CHARACTER CVPDG_SCHEIGHT*40/' '/                                    16
      CHARACTER NVPDG_SCHEIGHT*12/'R.SCHEIGHT'/                           16
      INTEGER MVPDG_SCHEIGHT(2)/1,1/                                      16
      REAL RE_VPDG_SCHEIGHT                                               16
C -                                                                       16
      REAL VPDG_SCLENGTH                                                  16
      EQUIVALENCE (BANK_VPDG(31),VPDG_SCLENGTH,RE_VPDG_SCLENGTH),         16
     *(NAMES_VPDG(29),NVPDG_SCLENGTH)                                     16
      EQUIVALENCE (COMMEN_VPDG(29),CVPDG_SCLENGTH),(MAP_VPDG(1,29),       16
     *MVPDG_SCLENGTH)                                                     16
      CHARACTER CVPDG_SCLENGTH*40/' '/                                    16
      CHARACTER NVPDG_SCLENGTH*12/'R.SCLENGTH'/                           16
      INTEGER MVPDG_SCLENGTH(2)/1,1/                                      16
      REAL RE_VPDG_SCLENGTH                                               16
C -                                                                       16
      REAL VPDG_SCTHICK                                                   16
      EQUIVALENCE (BANK_VPDG(32),VPDG_SCTHICK,RE_VPDG_SCTHICK),           16
     *(NAMES_VPDG(30),NVPDG_SCTHICK)                                      16
      EQUIVALENCE (COMMEN_VPDG(30),CVPDG_SCTHICK),(MAP_VPDG(1,30),        16
     *MVPDG_SCTHICK)                                                      16
      CHARACTER CVPDG_SCTHICK*40/' '/                                     16
      CHARACTER NVPDG_SCTHICK*12/'R.SCTHICK'/                             16
      INTEGER MVPDG_SCTHICK(2)/1,1/                                       16
      REAL RE_VPDG_SCTHICK                                                16
C -                                                                       16
      REAL VPDG_CLHEIGHT                                                  16
      EQUIVALENCE (BANK_VPDG(33),VPDG_CLHEIGHT,RE_VPDG_CLHEIGHT),         16
     *(NAMES_VPDG(31),NVPDG_CLHEIGHT)                                     16
      EQUIVALENCE (COMMEN_VPDG(31),CVPDG_CLHEIGHT),(MAP_VPDG(1,31),       16
     *MVPDG_CLHEIGHT)                                                     16
      CHARACTER CVPDG_CLHEIGHT*40/' '/                                    16
      CHARACTER NVPDG_CLHEIGHT*12/'R.CLHEIGHT'/                           16
      INTEGER MVPDG_CLHEIGHT(2)/1,1/                                      16
      REAL RE_VPDG_CLHEIGHT                                               16
C -                                                                       16
      REAL VPDG_CLWIDTH                                                   16
      EQUIVALENCE (BANK_VPDG(34),VPDG_CLWIDTH,RE_VPDG_CLWIDTH),           16
     *(NAMES_VPDG(32),NVPDG_CLWIDTH)                                      16
      EQUIVALENCE (COMMEN_VPDG(32),CVPDG_CLWIDTH),(MAP_VPDG(1,32),        16
     *MVPDG_CLWIDTH)                                                      16
      CHARACTER CVPDG_CLWIDTH*40/' '/                                     16
      CHARACTER NVPDG_CLWIDTH*12/'R.CLWIDTH'/                             16
      INTEGER MVPDG_CLWIDTH(2)/1,1/                                       16
      REAL RE_VPDG_CLWIDTH                                                16
C -                                                                       16
      REAL VPDG_CLLENGTH                                                  16
      EQUIVALENCE (BANK_VPDG(35),VPDG_CLLENGTH,RE_VPDG_CLLENGTH),         16
     *(NAMES_VPDG(33),NVPDG_CLLENGTH)                                     16
      EQUIVALENCE (COMMEN_VPDG(33),CVPDG_CLLENGTH),(MAP_VPDG(1,33),       16
     *MVPDG_CLLENGTH)                                                     16
      CHARACTER CVPDG_CLLENGTH*40/' '/                                    16
      CHARACTER NVPDG_CLLENGTH*12/'R.CLLENGTH'/                           16
      INTEGER MVPDG_CLLENGTH(2)/1,1/                                      16
      REAL RE_VPDG_CLLENGTH                                               16
C -                                                                       16
      REAL VPDG_CLTHICK                                                   16
      EQUIVALENCE (BANK_VPDG(36),VPDG_CLTHICK,RE_VPDG_CLTHICK),           16
     *(NAMES_VPDG(34),NVPDG_CLTHICK)                                      16
      EQUIVALENCE (COMMEN_VPDG(34),CVPDG_CLTHICK),(MAP_VPDG(1,34),        16
     *MVPDG_CLTHICK)                                                      16
      CHARACTER CVPDG_CLTHICK*40/' '/                                     16
      CHARACTER NVPDG_CLTHICK*12/'R.CLTHICK'/                             16
      INTEGER MVPDG_CLTHICK(2)/1,1/                                       16
      REAL RE_VPDG_CLTHICK                                                16
C -                                                                       16
      REAL VPDG_DETLEN                                                    16
      EQUIVALENCE (BANK_VPDG(37),VPDG_DETLEN,RE_VPDG_DETLEN),             16
     *(NAMES_VPDG(35),NVPDG_DETLEN)                                       16
      EQUIVALENCE (COMMEN_VPDG(35),CVPDG_DETLEN),(MAP_VPDG(1,35),         16
     *MVPDG_DETLEN)                                                       16
      CHARACTER CVPDG_DETLEN*40/' '/                                      16
      CHARACTER NVPDG_DETLEN*12/'R.DETLEN'/                               16
      INTEGER MVPDG_DETLEN(2)/1,1/                                        16
      REAL RE_VPDG_DETLEN                                                 16
C -                                                                       16
      REAL VPDG_DETRAD                                                    16
      EQUIVALENCE (BANK_VPDG(38),VPDG_DETRAD,RE_VPDG_DETRAD),             16
     *(NAMES_VPDG(36),NVPDG_DETRAD)                                       16
      EQUIVALENCE (COMMEN_VPDG(36),CVPDG_DETRAD),(MAP_VPDG(1,36),         16
     *MVPDG_DETRAD)                                                       16
      CHARACTER CVPDG_DETRAD*40/' '/                                      16
      CHARACTER NVPDG_DETRAD*12/'R.DETRAD'/                               16
      INTEGER MVPDG_DETRAD(2)/1,1/                                        16
      REAL RE_VPDG_DETRAD                                                 16
C -                                                                       16
      REAL VPDG_DETFRONT                                                  16
      EQUIVALENCE (BANK_VPDG(39),VPDG_DETFRONT,RE_VPDG_DETFRONT),         16
     *(NAMES_VPDG(37),NVPDG_DETFRONT)                                     16
      EQUIVALENCE (COMMEN_VPDG(37),CVPDG_DETFRONT),(MAP_VPDG(1,37),       16
     *MVPDG_DETFRONT)                                                     16
      CHARACTER CVPDG_DETFRONT*40/' '/                                    16
      CHARACTER NVPDG_DETFRONT*12/'R.DETFRONT'/                           16
      INTEGER MVPDG_DETFRONT(2)/1,1/                                      16
      REAL RE_VPDG_DETFRONT                                               16
C -                                                                       16
      REAL VPDG_CONVTHK                                                   16
      EQUIVALENCE (BANK_VPDG(40),VPDG_CONVTHK,RE_VPDG_CONVTHK),           16
     *(NAMES_VPDG(38),NVPDG_CONVTHK)                                      16
      EQUIVALENCE (COMMEN_VPDG(38),CVPDG_CONVTHK),(MAP_VPDG(1,38),        16
     *MVPDG_CONVTHK)                                                      16
      CHARACTER CVPDG_CONVTHK*40/' '/                                     16
      CHARACTER NVPDG_CONVTHK*12/'R.CONVTHK'/                             16
      INTEGER MVPDG_CONVTHK(2)/1,1/                                       16
      REAL RE_VPDG_CONVTHK                                                16
C -                                                                       16
      REAL VPDG_RADITHK                                                   16
      EQUIVALENCE (BANK_VPDG(41),VPDG_RADITHK,RE_VPDG_RADITHK),           16
     *(NAMES_VPDG(39),NVPDG_RADITHK)                                      16
      EQUIVALENCE (COMMEN_VPDG(39),CVPDG_RADITHK),(MAP_VPDG(1,39),        16
     *MVPDG_RADITHK)                                                      16
      CHARACTER CVPDG_RADITHK*40/' '/                                     16
      CHARACTER NVPDG_RADITHK*12/'R.RADITHK'/                             16
      INTEGER MVPDG_RADITHK(2)/1,1/                                       16
      REAL RE_VPDG_RADITHK                                                16
C -                                                                       16
      REAL VPDG_ELELENG                                                   16
      EQUIVALENCE (BANK_VPDG(42),VPDG_ELELENG,RE_VPDG_ELELENG),           16
     *(NAMES_VPDG(40),NVPDG_ELELENG)                                      16
      EQUIVALENCE (COMMEN_VPDG(40),CVPDG_ELELENG),(MAP_VPDG(1,40),        16
     *MVPDG_ELELENG)                                                      16
      CHARACTER CVPDG_ELELENG*40/' '/                                     16
      CHARACTER NVPDG_ELELENG*12/'R.ELELENG'/                             16
      INTEGER MVPDG_ELELENG(2)/1,1/                                       16
      REAL RE_VPDG_ELELENG                                                16
C -                                                                       16
      REAL VPDG_DRLAYER                                                   16
      EQUIVALENCE (BANK_VPDG(43),VPDG_DRLAYER,RE_VPDG_DRLAYER),           16
     *(NAMES_VPDG(41),NVPDG_DRLAYER)                                      16
      EQUIVALENCE (COMMEN_VPDG(41),CVPDG_DRLAYER),(MAP_VPDG(1,41),        16
     *MVPDG_DRLAYER)                                                      16
      CHARACTER CVPDG_DRLAYER*40/' '/                                     16
      CHARACTER NVPDG_DRLAYER*12/'R.DRLAYER'/                             16
      INTEGER MVPDG_DRLAYER(2)/1,1/                                       16
      REAL RE_VPDG_DRLAYER                                                16
C -                                                                       16
      REAL VPDG_NUMPMT                                                    16
      EQUIVALENCE (BANK_VPDG(44),VPDG_NUMPMT,RE_VPDG_NUMPMT),             16
     *(NAMES_VPDG(42),NVPDG_NUMPMT)                                       16
      EQUIVALENCE (COMMEN_VPDG(42),CVPDG_NUMPMT),(MAP_VPDG(1,42),         16
     *MVPDG_NUMPMT)                                                       16
      CHARACTER CVPDG_NUMPMT*40/' '/                                      16
      CHARACTER NVPDG_NUMPMT*12/'R.NUMPMT'/                               16
      INTEGER MVPDG_NUMPMT(2)/1,1/                                        16
      REAL RE_VPDG_NUMPMT                                                 16
C -                                                                       16
      REAL VPDG_PMTWALL                                                   16
      EQUIVALENCE (BANK_VPDG(45),VPDG_PMTWALL,RE_VPDG_PMTWALL),           16
     *(NAMES_VPDG(43),NVPDG_PMTWALL)                                      16
      EQUIVALENCE (COMMEN_VPDG(43),CVPDG_PMTWALL),(MAP_VPDG(1,43),        16
     *MVPDG_PMTWALL)                                                      16
      CHARACTER CVPDG_PMTWALL*40/' '/                                     16
      CHARACTER NVPDG_PMTWALL*12/'R.PMTWALL'/                             16
      INTEGER MVPDG_PMTWALL(2)/1,1/                                       16
      REAL RE_VPDG_PMTWALL                                                16
C -                                                                       16
      REAL VPDG_PMTRAD                                                    16
      EQUIVALENCE (BANK_VPDG(46),VPDG_PMTRAD,RE_VPDG_PMTRAD),             16
     *(NAMES_VPDG(44),NVPDG_PMTRAD)                                       16
      EQUIVALENCE (COMMEN_VPDG(44),CVPDG_PMTRAD),(MAP_VPDG(1,44),         16
     *MVPDG_PMTRAD)                                                       16
      CHARACTER CVPDG_PMTRAD*40/' '/                                      16
      CHARACTER NVPDG_PMTRAD*12/'R.PMTRAD'/                               16
      INTEGER MVPDG_PMTRAD(2)/1,1/                                        16
      REAL RE_VPDG_PMTRAD                                                 16
C -                                                                       16
      REAL VPDG_PMTLEN                                                    16
      EQUIVALENCE (BANK_VPDG(47),VPDG_PMTLEN,RE_VPDG_PMTLEN),             16
     *(NAMES_VPDG(45),NVPDG_PMTLEN)                                       16
      EQUIVALENCE (COMMEN_VPDG(45),CVPDG_PMTLEN),(MAP_VPDG(1,45),         16
     *MVPDG_PMTLEN)                                                       16
      CHARACTER CVPDG_PMTLEN*40/' '/                                      16
      CHARACTER NVPDG_PMTLEN*12/'R.PMTLEN'/                               16
      INTEGER MVPDG_PMTLEN(2)/1,1/                                        16
      REAL RE_VPDG_PMTLEN                                                 16
C -                                                                       16
      REAL VPDG_IBCHOICE                                                  16
      EQUIVALENCE (BANK_VPDG(48),VPDG_IBCHOICE,RE_VPDG_IBCHOICE),         16
     *(NAMES_VPDG(46),NVPDG_IBCHOICE)                                     16
      EQUIVALENCE (COMMEN_VPDG(46),CVPDG_IBCHOICE),(MAP_VPDG(1,46),       16
     *MVPDG_IBCHOICE)                                                     16
      CHARACTER CVPDG_IBCHOICE*40/' '/                                    16
      CHARACTER NVPDG_IBCHOICE*12/'R.IBCHOICE'/                           16
      INTEGER MVPDG_IBCHOICE(2)/1,1/                                      16
      REAL RE_VPDG_IBCHOICE                                               16
C -                                                                       16
      REAL VPDG_IBPOSYC                                                   16
      EQUIVALENCE (BANK_VPDG(49),VPDG_IBPOSYC,RE_VPDG_IBPOSYC),           16
     *(NAMES_VPDG(47),NVPDG_IBPOSYC)                                      16
      EQUIVALENCE (COMMEN_VPDG(47),CVPDG_IBPOSYC),(MAP_VPDG(1,47),        16
     *MVPDG_IBPOSYC)                                                      16
      CHARACTER CVPDG_IBPOSYC*40/' '/                                     16
      CHARACTER NVPDG_IBPOSYC*12/'R.IBPOSYC'/                             16
      INTEGER MVPDG_IBPOSYC(2)/1,1/                                       16
      REAL RE_VPDG_IBPOSYC                                                16
C -                                                                       16
      REAL VPDG_IBPOSZC                                                   16
      EQUIVALENCE (BANK_VPDG(50),VPDG_IBPOSZC,RE_VPDG_IBPOSZC),           16
     *(NAMES_VPDG(48),NVPDG_IBPOSZC)                                      16
      EQUIVALENCE (COMMEN_VPDG(48),CVPDG_IBPOSZC),(MAP_VPDG(1,48),        16
     *MVPDG_IBPOSZC)                                                      16
      CHARACTER CVPDG_IBPOSZC*40/' '/                                     16
      CHARACTER NVPDG_IBPOSZC*12/'R.IBPOSZC'/                             16
      INTEGER MVPDG_IBPOSZC(2)/1,1/                                       16
      REAL RE_VPDG_IBPOSZC                                                16
C -                                                                       16
      REAL VPDG_IBLENG                                                    16
      EQUIVALENCE (BANK_VPDG(51),VPDG_IBLENG,RE_VPDG_IBLENG),             16
     *(NAMES_VPDG(49),NVPDG_IBLENG)                                       16
      EQUIVALENCE (COMMEN_VPDG(49),CVPDG_IBLENG),(MAP_VPDG(1,49),         16
     *MVPDG_IBLENG)                                                       16
      CHARACTER CVPDG_IBLENG*40/' '/                                      16
      CHARACTER NVPDG_IBLENG*12/'R.IBLENG'/                               16
      INTEGER MVPDG_IBLENG(2)/1,1/                                        16
      REAL RE_VPDG_IBLENG                                                 16
C -                                                                       16
      REAL VPDG_IBTHICKH                                                  16
      EQUIVALENCE (BANK_VPDG(52),VPDG_IBTHICKH,RE_VPDG_IBTHICKH),         16
     *(NAMES_VPDG(50),NVPDG_IBTHICKH)                                     16
      EQUIVALENCE (COMMEN_VPDG(50),CVPDG_IBTHICKH),(MAP_VPDG(1,50),       16
     *MVPDG_IBTHICKH)                                                     16
      CHARACTER CVPDG_IBTHICKH*40/' '/                                    16
      CHARACTER NVPDG_IBTHICKH*12/'R.IBTHICKH'/                           16
      INTEGER MVPDG_IBTHICKH(2)/1,1/                                      16
      REAL RE_VPDG_IBTHICKH                                               16
C -                                                                       16
      REAL VPDG_IBTHICKV                                                  16
      EQUIVALENCE (BANK_VPDG(53),VPDG_IBTHICKV,RE_VPDG_IBTHICKV),         16
     *(NAMES_VPDG(51),NVPDG_IBTHICKV)                                     16
      EQUIVALENCE (COMMEN_VPDG(51),CVPDG_IBTHICKV),(MAP_VPDG(1,51),       16
     *MVPDG_IBTHICKV)                                                     16
      CHARACTER CVPDG_IBTHICKV*40/' '/                                    16
      CHARACTER NVPDG_IBTHICKV*12/'R.IBTHICKV'/                           16
      INTEGER MVPDG_IBTHICKV(2)/1,1/                                      16
      REAL RE_VPDG_IBTHICKV                                               16
C -                                                                       16
      REAL VPDG_IBHEIGHT                                                  16
      EQUIVALENCE (BANK_VPDG(54),VPDG_IBHEIGHT,RE_VPDG_IBHEIGHT),         16
     *(NAMES_VPDG(52),NVPDG_IBHEIGHT)                                     16
      EQUIVALENCE (COMMEN_VPDG(52),CVPDG_IBHEIGHT),(MAP_VPDG(1,52),       16
     *MVPDG_IBHEIGHT)                                                     16
      CHARACTER CVPDG_IBHEIGHT*40/' '/                                    16
      CHARACTER NVPDG_IBHEIGHT*12/'R.IBHEIGHT'/                           16
      INTEGER MVPDG_IBHEIGHT(2)/1,1/                                      16
      REAL RE_VPDG_IBHEIGHT                                               16
C -                                                                       16
      REAL VPDG_IBWIDTH                                                   16
      EQUIVALENCE (BANK_VPDG(55),VPDG_IBWIDTH,RE_VPDG_IBWIDTH),           16
     *(NAMES_VPDG(53),NVPDG_IBWIDTH)                                      16
      EQUIVALENCE (COMMEN_VPDG(53),CVPDG_IBWIDTH),(MAP_VPDG(1,53),        16
     *MVPDG_IBWIDTH)                                                      16
      CHARACTER CVPDG_IBWIDTH*40/' '/                                     16
      CHARACTER NVPDG_IBWIDTH*12/'R.IBWIDTH'/                             16
      INTEGER MVPDG_IBWIDTH(2)/1,1/                                       16
      REAL RE_VPDG_IBWIDTH                                                16
C -                                                                       16
      REAL VPDG_IBWLEN                                                    16
      EQUIVALENCE (BANK_VPDG(56),VPDG_IBWLEN,RE_VPDG_IBWLEN),             16
     *(NAMES_VPDG(54),NVPDG_IBWLEN)                                       16
      EQUIVALENCE (COMMEN_VPDG(54),CVPDG_IBWLEN),(MAP_VPDG(1,54),         16
     *MVPDG_IBWLEN)                                                       16
      CHARACTER CVPDG_IBWLEN*40/' '/                                      16
      CHARACTER NVPDG_IBWLEN*12/'R.IBWLEN'/                               16
      INTEGER MVPDG_IBWLEN(2)/1,1/                                        16
      REAL RE_VPDG_IBWLEN                                                 16
C -                                                                       16
      REAL VPDG_IBWHGHTF                                                  16
      EQUIVALENCE (BANK_VPDG(57),VPDG_IBWHGHTF,RE_VPDG_IBWHGHTF),         16
     *(NAMES_VPDG(55),NVPDG_IBWHGHTF)                                     16
      EQUIVALENCE (COMMEN_VPDG(55),CVPDG_IBWHGHTF),(MAP_VPDG(1,55),       16
     *MVPDG_IBWHGHTF)                                                     16
      CHARACTER CVPDG_IBWHGHTF*40/' '/                                    16
      CHARACTER NVPDG_IBWHGHTF*12/'R.IBWHGHTF'/                           16
      INTEGER MVPDG_IBWHGHTF(2)/1,1/                                      16
      REAL RE_VPDG_IBWHGHTF                                               16
C -                                                                       16
      REAL VPDG_IBWHGHTB                                                  16
      EQUIVALENCE (BANK_VPDG(58),VPDG_IBWHGHTB,RE_VPDG_IBWHGHTB),         16
     *(NAMES_VPDG(56),NVPDG_IBWHGHTB)                                     16
      EQUIVALENCE (COMMEN_VPDG(56),CVPDG_IBWHGHTB),(MAP_VPDG(1,56),       16
     *MVPDG_IBWHGHTB)                                                     16
      CHARACTER CVPDG_IBWHGHTB*40/' '/                                    16
      CHARACTER NVPDG_IBWHGHTB*12/'R.IBWHGHTB'/                           16
      INTEGER MVPDG_IBWHGHTB(2)/1,1/                                      16
      REAL RE_VPDG_IBWHGHTB                                               16
C -                                                                       16
      INTEGER LEN_VPDG,LLEN_VPDG,LINK_VPDG                                16
      PARAMETER (LEN_VPDG=58)                                             16
      PARAMETER (LLEN_VPDG=56)                                            16
      INTEGER FLAG_VPDG/0/,MAP_VPDG(2,LLEN_VPDG),IDNUM_VPDG(10)           16
      CHARACTER NAMES_VPDG(LLEN_VPDG)*12,COMMEN_VPDG(LLEN_VPDG)*40        16
      DATA IDNUM_VPDG/10*0/                                               16
      REAL BANK_VPDG(LEN_VPDG)                                            16
      CHARACTER*60 BPATH_VPDG/' '/                                        16
      SAVE FLAG_VPDG,MAP_VPDG,IDNUM_VPDG,NAMES_VPDG,COMMEN_VPDG,          16
     *BPATH_VPDG                                                          16
      DATA LINK_VPDG/0/,BANK_VPDG/LEN_VPDG*0/                             16
C *                                                                       16
      REAL YBASE,YTOP,CONVLENGTH,DETANGLE,STRUTHEIGHT,YDISPL              17
      REAL ELECTHCK,ELECWID,ELECLEN,XLOC,YLOC,ZLOC,ZPOS                   19
      INTEGER ISEC                                                        21
C *                                                                       23
      CALL AGSBEGM(MODULE,IPRIN)                                          23
      IF (IPRIN.LT.0) RETURN                                              23
C Filling bank VPDV - VPD CONFIGURATION CONTROL                           23
      BANK_TITLE='VPD CONFIGURATION CONTROL'                              23
      BPATH_VPDV='VPDV*'                                                  23
      IDNUM_VPDV(1)=0                                                     24
C VERSION                                                                 24
      VPDV_VERSION=1                                                      24
      CVPDV_VERSION='VERSION'                                             24
C VPD CONFIGURATION                                                       25
      VPDV_VPDCONFIG=1                                                    25
      CVPDV_VPDCONFIG='VPD CONFIGURATION'                                 25
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDV',    26
     *BPATH_VPDV,IDNUM_VPDV,1, MAP_VPDV,NAMES_VPDV,COMMEN_VPDV,           26
     *BANK_VPDV,LEN_VPDV,LLEN_VPDV,LINK_VPDV,FLAG_VPDV)                   26
C *                                                                       26
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDV',    26
     *BPATH_VPDV,IDNUM_VPDV,1, MAP_VPDV,NAMES_VPDV,COMMEN_VPDV,           26
     *BANK_VPDV,LEN_VPDV,LLEN_VPDV,LINK_VPDV,FLAG_VPDV)                   26
C *                                                                       27
      CALL AGSBEGM(MODULE,IPRIN)                                          27
      IF (IPRIN.LT.0) RETURN                                              27
C Filling bank VPDG - PVPD BASIC DIMENSIONS                               27
      BANK_TITLE='PVPD BASIC DIMENSIONS'                                  27
      BPATH_VPDG='VPDG*'                                                  27
      IDNUM_VPDG(1)=0                                                     28
C GEOMETRY VERSION                                                        28
      VPDG_VERSION=1                                                      28
      CVPDG_VERSION='GEOMETRY VERSION'                                    28
C Z POSITION EAST                                                         29
      VPDG_ZPOSEAST=550.0000                                              29
      CVPDG_ZPOSEAST='Z POSITION EAST'                                    29
C Z POSITION WEST                                                         30
      VPDG_ZPOSWEST=550.0000                                              30
      CVPDG_ZPOSWEST='Z POSITION WEST'                                    30
C MOTHERVOLUME RMIN (2.5INCH)                                             31
      VPDG_RMIN=6.35                                                      31
      CVPDG_RMIN='MOTHERVOLUME RMIN (2.5INCH)'                            31
C MOTHERVOLUME RMIN (12.3125INCH)                                         32
      VPDG_RMAX=31.27375                                                  32
      CVPDG_RMAX='MOTHERVOLUME RMIN (12.3125INCH)'                        32
C BASEPLATE WIDTH (12INCH)                                                33
      VPDG_BPWIDTH=30.48                                                  33
      CVPDG_BPWIDTH='BASEPLATE WIDTH (12INCH)'                            33
C BASEPLATE LENGTH (18.5INCH)                                             34
      VPDG_BPLENGTH=46.355                                                34
      CVPDG_BPLENGTH='BASEPLATE LENGTH (18.5INCH)'                        34
C BASEPLATE THICKNESS (0.25INCH)                                          35
      VPDG_BPTHICK=0.635                                                  35
      CVPDG_BPTHICK='BASEPLATE THICKNESS (0.25INCH)'                      35
C FEE BOX HEIGHT (2INCH)                                                  36
      VPDG_BXHEIGHT=5.08                                                  36
      CVPDG_BXHEIGHT='FEE BOX HEIGHT (2INCH)'                             36
C FEE BOX LENGTH (6INCH)                                                  37
      VPDG_BXLENGTH=15.24                                                 37
      CVPDG_BXLENGTH='FEE BOX LENGTH (6INCH)'                             37
C FEE BOX WIDTH (10INCH)                                                  38
      VPDG_BXWIDTH=25.4                                                   38
      CVPDG_BXWIDTH='FEE BOX WIDTH (10INCH)'                              38
C FEE BOX PLATE THICKNESS (0.25INCH)                                      39
      VPDG_BXTHICK=0.635                                                  39
      CVPDG_BXTHICK='FEE BOX PLATE THICKNESS (0.25INCH)'                  39
C FEE CENTRAL Z POS (+/-??) (0.25INCH)                                    40
      VPDG_BXZPOSC=0.635                                                  40
      CVPDG_BXZPOSC='FEE CENTRAL Z POS (+/-??) (0.25INCH)'                40
C FRONTPLATE WIDTH (3INCH)                                                41
      VPDG_FPWIDTH=7.62                                                   41
      CVPDG_FPWIDTH='FRONTPLATE WIDTH (3INCH)'                            41
C FRONTPLATE HEIGHT (10.023INCH)                                          42
      VPDG_FPHEIGHT=25.45842                                              42
      CVPDG_FPHEIGHT='FRONTPLATE HEIGHT (10.023INCH)'                     42
C FRONTPLATE THICKNESS (1INCH)                                            43
      VPDG_FPTHICK=2.54                                                   43
      CVPDG_FPTHICK='FRONTPLATE THICKNESS (1INCH)'                        43
C FRONTPL.PART A WIDTH (2.614INCH)                                        44
      VPDG_FPAWIDTH=6.63956                                               44
      CVPDG_FPAWIDTH='FRONTPL.PART A WIDTH (2.614INCH)'                   44
C FRONTPL.PART A HEIGHT (3.289INCH)                                       45
      VPDG_FPAHGHT=8.35405                                                45
      CVPDG_FPAHGHT='FRONTPL.PART A HEIGHT (3.289INCH)'                   45
C FRONTPL.PART C HEIGHT (4.95INCH)                                        46
      VPDG_FPCHGHT=12.573                                                 46
      CVPDG_FPCHGHT='FRONTPL.PART C HEIGHT (4.95INCH)'                    46
C FRONTPL. HOOK HEIGHT (1.5INCH)                                          47
      VPDG_FPHHGHT=3.81                                                   47
      CVPDG_FPHHGHT='FRONTPL. HOOK HEIGHT (1.5INCH)'                      47
C FRONTPL. HOOK WIDTH (2INCH)                                             48
      VPDG_FPHWIDTH=5.08                                                  48
      CVPDG_FPHWIDTH='FRONTPL. HOOK WIDTH (2INCH)'                        48
C FRONTPL. HOOK THICKNESS (0.25INCH)                                      49
      VPDG_FPHTHICK=0.635                                                 49
      CVPDG_FPHTHICK='FRONTPL. HOOK THICKNESS (0.25INCH)'                 49
C STRUT THICKNESS (0.25INCH)                                              50
      VPDG_STTHICK=0.635                                                  50
      CVPDG_STTHICK='STRUT THICKNESS (0.25INCH)'                          50
C STRUT HEIGHT (9.125 INCH)                                               51
      VPDG_STHEIGHT=23.1775                                               51
      CVPDG_STHEIGHT='STRUT HEIGHT (9.125 INCH)'                          51
C STRUT ANGLE (DEGR)                                                      52
      VPDG_STANGLE=25.                                                    52
      CVPDG_STANGLE='STRUT ANGLE (DEGR)'                                  52
C DIAGNOAL STRUT SIZE (2INCH)                                             53
      VPDG_STDIAGSZ=5.08                                                  53
      CVPDG_STDIAGSZ='DIAGNOAL STRUT SIZE (2INCH)'                        53
C STRUT CLAMP WIDTH (3INCH)                                               54
      VPDG_SCWIDTH=7.62                                                   54
      CVPDG_SCWIDTH='STRUT CLAMP WIDTH (3INCH)'                           54
C STRUT CLAMP HEIGHT (1.5INCH)                                            55
      VPDG_SCHEIGHT=3.81                                                  55
      CVPDG_SCHEIGHT='STRUT CLAMP HEIGHT (1.5INCH)'                       55
C STRUT CLAMP LENGTH (1.5INCH)                                            56
      VPDG_SCLENGTH=3.81                                                  56
      CVPDG_SCLENGTH='STRUT CLAMP LENGTH (1.5INCH)'                       56
C STRUT CLAMP THICKNESS (0.25INCH)                                        57
      VPDG_SCTHICK=0.635                                                  57
      CVPDG_SCTHICK='STRUT CLAMP THICKNESS (0.25INCH)'                    57
C CLAMP HEIGHT (1.375INCH)                                                58
      VPDG_CLHEIGHT=3.4925                                                58
      CVPDG_CLHEIGHT='CLAMP HEIGHT (1.375INCH)'                           58
C CLAMP WIDTH (1.5INCH)                                                   59
      VPDG_CLWIDTH=3.81                                                   59
      CVPDG_CLWIDTH='CLAMP WIDTH (1.5INCH)'                               59
C CLAMP LENGTH (16.25INCH)                                                60
      VPDG_CLLENGTH=41.275                                                60
      CVPDG_CLLENGTH='CLAMP LENGTH (16.25INCH)'                           60
C CLAMP PLATE THICKNESS (0.125INCH)                                       61
      VPDG_CLTHICK=0.3175                                                 61
      CVPDG_CLTHICK='CLAMP PLATE THICKNESS (0.125INCH)'                   61
C PMT ASSEMBLY LENGTH (13INCH)                                            62
      VPDG_DETLEN=33.02                                                   62
      CVPDG_DETLEN='PMT ASSEMBLY LENGTH (13INCH)'                         62
C PMT ASSEMBLY RADIUS (1.5INCH)                                           63
      VPDG_DETRAD=3.81                                                    63
      CVPDG_DETRAD='PMT ASSEMBLY RADIUS (1.5INCH)'                        63
C PMT ASS. FRONTPLATE THICKNESS (0.4INCH)                                 64
      VPDG_DETFRONT=1.016                                                 64
      CVPDG_DETFRONT='PMT ASS. FRONTPLATE THICKNESS (0.4INCH)'            64
C CONVERTER LAYER THICKNESS                                               65
      VPDG_CONVTHK=1.                                                     65
      CVPDG_CONVTHK='CONVERTER LAYER THICKNESS'                           65
C RADIATOR LAYER THICKNESS                                                66
      VPDG_RADITHK=1.                                                     66
      CVPDG_RADITHK='RADIATOR LAYER THICKNESS'                            66
C ELECTRONICS MOUNT LENGTH                                                67
      VPDG_ELELENG=15.0                                                   67
      CVPDG_ELELENG='ELECTRONICS MOUNT LENGTH'                            67
C LAYER RADIAL WIDTH                                                      68
      VPDG_DRLAYER=6.                                                     68
      CVPDG_DRLAYER='LAYER RADIAL WIDTH'                                  68
C NUMBER OF PMT IN LAYER                                                  69
      VPDG_NUMPMT=3                                                       69
      CVPDG_NUMPMT='NUMBER OF PMT IN LAYER'                               69
C PMT WALL THICKNESS                                                      70
      VPDG_PMTWALL=0.1                                                    70
      CVPDG_PMTWALL='PMT WALL THICKNESS'                                  70
C PMT AND DETECTOR RADIUS (1INCH)                                         71
      VPDG_PMTRAD=2.54                                                    71
      CVPDG_PMTRAD='PMT AND DETECTOR RADIUS (1INCH)'                      71
C PMT TUBE LENGTH                                                         72
      VPDG_PMTLEN=8.0                                                     72
      CVPDG_PMTLEN='PMT TUBE LENGTH'                                      72
C ACTIVE/DE-ACTIVATE (0) IBEAM                                            73
      VPDG_IBCHOICE=1                                                     73
      CVPDG_IBCHOICE='ACTIVE/DE-ACTIVATE (0) IBEAM'                       73
C IBEAM CENTRAL YLOCATION (6.5INCH)                                       74
      VPDG_IBPOSYC=-16.51                                                 74
      CVPDG_IBPOSYC='IBEAM CENTRAL YLOCATION (6.5INCH)'                   74
C IBEAM CENTRAL ZPOSITION                                                 75
      VPDG_IBPOSZC=530.477                                                75
      CVPDG_IBPOSZC='IBEAM CENTRAL ZPOSITION'                             75
C IBEAM LENGTH (116.1INCH)                                                76
      VPDG_IBLENG=294.894                                                 76
      CVPDG_IBLENG='IBEAM LENGTH (116.1INCH)'                             76
C IBEAM HORIZ. PLATE THICKNESS (0.29INCH)                                 77
      VPDG_IBTHICKH=0.7366                                                77
      CVPDG_IBTHICKH='IBEAM HORIZ. PLATE THICKNESS (0.29INCH)'            77
C IBEAM VERT. PLATE THICKNESS (0.17INCH)                                  78
      VPDG_IBTHICKV=0.4318                                                78
      CVPDG_IBTHICKV='IBEAM VERT. PLATE THICKNESS (0.17INCH)'             78
C IBEAM HEIGHT (4INCH)                                                    79
      VPDG_IBHEIGHT=10.16                                                 79
      CVPDG_IBHEIGHT='IBEAM HEIGHT (4INCH)'                               79
C IBEAM WIDTH (3INCH)                                                     80
      VPDG_IBWIDTH=7.62                                                   80
      CVPDG_IBWIDTH='IBEAM WIDTH (3INCH)'                                 80
C IBEAM VERT. FRONT PIECE (9INCH)                                         81
      VPDG_IBWLEN=22.86                                                   81
      CVPDG_IBWLEN='IBEAM VERT. FRONT PIECE (9INCH)'                      81
C IBEAM VERT. FRONT PIECE HEIGHT                                          82
      VPDG_IBWHGHTF=4.4489                                                82
      CVPDG_IBWHGHTF='IBEAM VERT. FRONT PIECE HEIGHT'                     82
C IBEAM VERT. FRONT PIECE HEIGHT                                          83
      VPDG_IBWHGHTB=8.3097                                                83
      CVPDG_IBWHGHTB='IBEAM VERT. FRONT PIECE HEIGHT'                     83
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    84
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           84
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   84
C *                                                                       84
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    84
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           84
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   84
C *                                                                       85
      CALL AGSBEGM(MODULE,IPRIN)                                          85
      IF (IPRIN.LT.0) RETURN                                              85
C Filling bank VPDG - PVPD BASIC DIMENSIONS                               85
      BANK_TITLE='PVPD BASIC DIMENSIONS'                                  85
      BPATH_VPDG='VPDG*'                                                  85
      IDNUM_VPDG(1)=0                                                     86
C GEOMETRY VERSION                                                        86
      VPDG_VERSION=2                                                      86
      CVPDG_VERSION='GEOMETRY VERSION'                                    86
C Z POSITION EAST                                                         87
      VPDG_ZPOSEAST=561.2638                                              87
      CVPDG_ZPOSEAST='Z POSITION EAST'                                    87
C Z POSITION WEST                                                         88
      VPDG_ZPOSWEST=563.1688                                              88
      CVPDG_ZPOSWEST='Z POSITION WEST'                                    88
C MOTHERVOLUME RMIN (2.5INCH)                                             89
      VPDG_RMIN=6.35                                                      89
      CVPDG_RMIN='MOTHERVOLUME RMIN (2.5INCH)'                            89
C MOTHERVOLUME RMIN (12.3125INCH)                                         90
      VPDG_RMAX=31.27375                                                  90
      CVPDG_RMAX='MOTHERVOLUME RMIN (12.3125INCH)'                        90
C BASEPLATE WIDTH (12INCH)                                                91
      VPDG_BPWIDTH=30.48                                                  91
      CVPDG_BPWIDTH='BASEPLATE WIDTH (12INCH)'                            91
C BASEPLATE LENGTH (18.5INCH)                                             92
      VPDG_BPLENGTH=46.355                                                92
      CVPDG_BPLENGTH='BASEPLATE LENGTH (18.5INCH)'                        92
C BASEPLATE THICKNESS (0.25INCH)                                          93
      VPDG_BPTHICK=0.635                                                  93
      CVPDG_BPTHICK='BASEPLATE THICKNESS (0.25INCH)'                      93
C FEE BOX HEIGHT (2INCH)                                                  94
      VPDG_BXHEIGHT=5.08                                                  94
      CVPDG_BXHEIGHT='FEE BOX HEIGHT (2INCH)'                             94
C FEE BOX LENGTH (6INCH)                                                  95
      VPDG_BXLENGTH=15.24                                                 95
      CVPDG_BXLENGTH='FEE BOX LENGTH (6INCH)'                             95
C FEE BOX WIDTH (10INCH)                                                  96
      VPDG_BXWIDTH=25.4                                                   96
      CVPDG_BXWIDTH='FEE BOX WIDTH (10INCH)'                              96
C FEE BOX PLATE THICKNESS (0.25INCH)                                      97
      VPDG_BXTHICK=0.635                                                  97
      CVPDG_BXTHICK='FEE BOX PLATE THICKNESS (0.25INCH)'                  97
C FEE CENTRAL Z POS (+/-??) (0.25INCH)                                    98
      VPDG_BXZPOSC=0.635                                                  98
      CVPDG_BXZPOSC='FEE CENTRAL Z POS (+/-??) (0.25INCH)'                98
C FRONTPLATE WIDTH (3INCH)                                                99
      VPDG_FPWIDTH=7.62                                                   99
      CVPDG_FPWIDTH='FRONTPLATE WIDTH (3INCH)'                            99
C FRONTPLATE HEIGHT (10.023INCH)                                          100
      VPDG_FPHEIGHT=25.45842                                              100
      CVPDG_FPHEIGHT='FRONTPLATE HEIGHT (10.023INCH)'                     100
C FRONTPLATE THICKNESS (1INCH)                                            101
      VPDG_FPTHICK=2.54                                                   101
      CVPDG_FPTHICK='FRONTPLATE THICKNESS (1INCH)'                        101
C FRONTPL.PART A WIDTH (2.614INCH)                                        102
      VPDG_FPAWIDTH=6.63956                                               102
      CVPDG_FPAWIDTH='FRONTPL.PART A WIDTH (2.614INCH)'                   102
C FRONTPL.PART A HEIGHT (3.289INCH)                                       103
      VPDG_FPAHGHT=8.35405                                                103
      CVPDG_FPAHGHT='FRONTPL.PART A HEIGHT (3.289INCH)'                   103
C FRONTPL.PART C HEIGHT (4.95INCH)                                        104
      VPDG_FPCHGHT=12.573                                                 104
      CVPDG_FPCHGHT='FRONTPL.PART C HEIGHT (4.95INCH)'                    104
C FRONTPL. HOOK HEIGHT (1.5INCH)                                          105
      VPDG_FPHHGHT=3.81                                                   105
      CVPDG_FPHHGHT='FRONTPL. HOOK HEIGHT (1.5INCH)'                      105
C FRONTPL. HOOK WIDTH (2INCH)                                             106
      VPDG_FPHWIDTH=5.08                                                  106
      CVPDG_FPHWIDTH='FRONTPL. HOOK WIDTH (2INCH)'                        106
C FRONTPL. HOOK THICKNESS (0.25INCH)                                      107
      VPDG_FPHTHICK=0.635                                                 107
      CVPDG_FPHTHICK='FRONTPL. HOOK THICKNESS (0.25INCH)'                 107
C STRUT THICKNESS (0.25INCH)                                              108
      VPDG_STTHICK=0.635                                                  108
      CVPDG_STTHICK='STRUT THICKNESS (0.25INCH)'                          108
C STRUT HEIGHT (9.125 INCH)                                               109
      VPDG_STHEIGHT=23.1775                                               109
      CVPDG_STHEIGHT='STRUT HEIGHT (9.125 INCH)'                          109
C STRUT ANGLE (DEGR)                                                      110
      VPDG_STANGLE=25.                                                    110
      CVPDG_STANGLE='STRUT ANGLE (DEGR)'                                  110
C DIAGNOAL STRUT SIZE (2INCH)                                             111
      VPDG_STDIAGSZ=5.08                                                  111
      CVPDG_STDIAGSZ='DIAGNOAL STRUT SIZE (2INCH)'                        111
C STRUT CLAMP WIDTH (3INCH)                                               112
      VPDG_SCWIDTH=7.62                                                   112
      CVPDG_SCWIDTH='STRUT CLAMP WIDTH (3INCH)'                           112
C STRUT CLAMP HEIGHT (1.5INCH)                                            113
      VPDG_SCHEIGHT=3.81                                                  113
      CVPDG_SCHEIGHT='STRUT CLAMP HEIGHT (1.5INCH)'                       113
C STRUT CLAMP LENGTH (1.5INCH)                                            114
      VPDG_SCLENGTH=3.81                                                  114
      CVPDG_SCLENGTH='STRUT CLAMP LENGTH (1.5INCH)'                       114
C STRUT CLAMP THICKNESS (0.25INCH)                                        115
      VPDG_SCTHICK=0.635                                                  115
      CVPDG_SCTHICK='STRUT CLAMP THICKNESS (0.25INCH)'                    115
C CLAMP HEIGHT (1.375INCH)                                                116
      VPDG_CLHEIGHT=3.4925                                                116
      CVPDG_CLHEIGHT='CLAMP HEIGHT (1.375INCH)'                           116
C CLAMP WIDTH (1.5INCH)                                                   117
      VPDG_CLWIDTH=3.81                                                   117
      CVPDG_CLWIDTH='CLAMP WIDTH (1.5INCH)'                               117
C CLAMP LENGTH (16.25INCH)                                                118
      VPDG_CLLENGTH=41.275                                                118
      CVPDG_CLLENGTH='CLAMP LENGTH (16.25INCH)'                           118
C CLAMP PLATE THICKNESS (0.125INCH)                                       119
      VPDG_CLTHICK=0.3175                                                 119
      CVPDG_CLTHICK='CLAMP PLATE THICKNESS (0.125INCH)'                   119
C PMT ASSEMBLY LENGTH (13INCH)                                            120
      VPDG_DETLEN=33.02                                                   120
      CVPDG_DETLEN='PMT ASSEMBLY LENGTH (13INCH)'                         120
C PMT ASSEMBLY RADIUS (1.5INCH)                                           121
      VPDG_DETRAD=3.81                                                    121
      CVPDG_DETRAD='PMT ASSEMBLY RADIUS (1.5INCH)'                        121
C PMT ASS. FRONTPLATE THICKNESS (0.4INCH)                                 122
      VPDG_DETFRONT=1.016                                                 122
      CVPDG_DETFRONT='PMT ASS. FRONTPLATE THICKNESS (0.4INCH)'            122
C CONVERTER LAYER THICKNESS                                               123
      VPDG_CONVTHK=1.                                                     123
      CVPDG_CONVTHK='CONVERTER LAYER THICKNESS'                           123
C RADIATOR LAYER THICKNESS                                                124
      VPDG_RADITHK=1.                                                     124
      CVPDG_RADITHK='RADIATOR LAYER THICKNESS'                            124
C ELECTRONICS MOUNT LENGTH                                                125
      VPDG_ELELENG=15.0                                                   125
      CVPDG_ELELENG='ELECTRONICS MOUNT LENGTH'                            125
C LAYER RADIAL WIDTH                                                      126
      VPDG_DRLAYER=6.                                                     126
      CVPDG_DRLAYER='LAYER RADIAL WIDTH'                                  126
C NUMBER OF PMT IN LAYER                                                  127
      VPDG_NUMPMT=3                                                       127
      CVPDG_NUMPMT='NUMBER OF PMT IN LAYER'                               127
C PMT WALL THICKNESS                                                      128
      VPDG_PMTWALL=0.1                                                    128
      CVPDG_PMTWALL='PMT WALL THICKNESS'                                  128
C PMT AND DETECTOR RADIUS (1INCH)                                         129
      VPDG_PMTRAD=2.54                                                    129
      CVPDG_PMTRAD='PMT AND DETECTOR RADIUS (1INCH)'                      129
C PMT TUBE LENGTH                                                         130
      VPDG_PMTLEN=8.0                                                     130
      CVPDG_PMTLEN='PMT TUBE LENGTH'                                      130
C ACTIVE/DE-ACTIVATE (0) IBEAM                                            131
      VPDG_IBCHOICE=1                                                     131
      CVPDG_IBCHOICE='ACTIVE/DE-ACTIVATE (0) IBEAM'                       131
C IBEAM CENTRAL YLOCATION (6.5INCH)                                       132
      VPDG_IBPOSYC=-16.51                                                 132
      CVPDG_IBPOSYC='IBEAM CENTRAL YLOCATION (6.5INCH)'                   132
C IBEAM CENTRAL ZPOSITION                                                 133
      VPDG_IBPOSZC=530.477                                                133
      CVPDG_IBPOSZC='IBEAM CENTRAL ZPOSITION'                             133
C IBEAM LENGTH (116.1INCH)                                                134
      VPDG_IBLENG=294.894                                                 134
      CVPDG_IBLENG='IBEAM LENGTH (116.1INCH)'                             134
C IBEAM HORIZ. PLATE THICKNESS (0.29INCH)                                 135
      VPDG_IBTHICKH=0.7366                                                135
      CVPDG_IBTHICKH='IBEAM HORIZ. PLATE THICKNESS (0.29INCH)'            135
C IBEAM VERT. PLATE THICKNESS (0.17INCH)                                  136
      VPDG_IBTHICKV=0.4318                                                136
      CVPDG_IBTHICKV='IBEAM VERT. PLATE THICKNESS (0.17INCH)'             136
C IBEAM HEIGHT (4INCH)                                                    137
      VPDG_IBHEIGHT=10.16                                                 137
      CVPDG_IBHEIGHT='IBEAM HEIGHT (4INCH)'                               137
C IBEAM WIDTH (3INCH)                                                     138
      VPDG_IBWIDTH=7.62                                                   138
      CVPDG_IBWIDTH='IBEAM WIDTH (3INCH)'                                 138
C IBEAM VERT. FRONT PIECE (9INCH)                                         139
      VPDG_IBWLEN=22.86                                                   139
      CVPDG_IBWLEN='IBEAM VERT. FRONT PIECE (9INCH)'                      139
C IBEAM VERT. FRONT PIECE HEIGHT                                          140
      VPDG_IBWHGHTF=4.4489                                                140
      CVPDG_IBWHGHTF='IBEAM VERT. FRONT PIECE HEIGHT'                     140
C IBEAM VERT. FRONT PIECE HEIGHT                                          141
      VPDG_IBWHGHTB=8.3097                                                141
      CVPDG_IBWHGHTB='IBEAM VERT. FRONT PIECE HEIGHT'                     141
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    142
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           142
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   142
C *                                                                       142
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    142
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           142
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   142
C *                                                                       143
      CALL AGSBEGM(MODULE,IPRIN)                                          143
      IF (IPRIN.LT.0) RETURN                                              143
C Filling bank VPDG - PVPD BASIC DIMENSIONS                               143
      BANK_TITLE='PVPD BASIC DIMENSIONS'                                  143
      BPATH_VPDG='VPDG*'                                                  143
      IDNUM_VPDG(1)=0                                                     144
C GEOMETRY VERSION                                                        144
      VPDG_VERSION=3                                                      144
      CVPDG_VERSION='GEOMETRY VERSION'                                    144
C Z POSITION EAST                                                         145
      VPDG_ZPOSEAST=564.4388                                              145
      CVPDG_ZPOSEAST='Z POSITION EAST'                                    145
C Z POSITION WEST                                                         146
      VPDG_ZPOSWEST=563.4069                                              146
      CVPDG_ZPOSWEST='Z POSITION WEST'                                    146
C MOTHERVOLUME RMIN (2.5INCH)                                             147
      VPDG_RMIN=6.35                                                      147
      CVPDG_RMIN='MOTHERVOLUME RMIN (2.5INCH)'                            147
C MOTHERVOLUME RMIN (12.3125INCH)                                         148
      VPDG_RMAX=31.27375                                                  148
      CVPDG_RMAX='MOTHERVOLUME RMIN (12.3125INCH)'                        148
C BASEPLATE WIDTH (12INCH)                                                149
      VPDG_BPWIDTH=30.48                                                  149
      CVPDG_BPWIDTH='BASEPLATE WIDTH (12INCH)'                            149
C BASEPLATE LENGTH (18.5INCH)                                             150
      VPDG_BPLENGTH=46.355                                                150
      CVPDG_BPLENGTH='BASEPLATE LENGTH (18.5INCH)'                        150
C BASEPLATE THICKNESS (0.25INCH)                                          151
      VPDG_BPTHICK=0.635                                                  151
      CVPDG_BPTHICK='BASEPLATE THICKNESS (0.25INCH)'                      151
C FEE BOX HEIGHT (2INCH)                                                  152
      VPDG_BXHEIGHT=5.08                                                  152
      CVPDG_BXHEIGHT='FEE BOX HEIGHT (2INCH)'                             152
C FEE BOX LENGTH (6INCH)                                                  153
      VPDG_BXLENGTH=15.24                                                 153
      CVPDG_BXLENGTH='FEE BOX LENGTH (6INCH)'                             153
C FEE BOX WIDTH (10INCH)                                                  154
      VPDG_BXWIDTH=25.4                                                   154
      CVPDG_BXWIDTH='FEE BOX WIDTH (10INCH)'                              154
C FEE BOX PLATE THICKNESS (0.25INCH)                                      155
      VPDG_BXTHICK=0.635                                                  155
      CVPDG_BXTHICK='FEE BOX PLATE THICKNESS (0.25INCH)'                  155
C FEE CENTRAL Z POS (+/-??) (0.25INCH)                                    156
      VPDG_BXZPOSC=0.635                                                  156
      CVPDG_BXZPOSC='FEE CENTRAL Z POS (+/-??) (0.25INCH)'                156
C FRONTPLATE WIDTH (3INCH)                                                157
      VPDG_FPWIDTH=7.62                                                   157
      CVPDG_FPWIDTH='FRONTPLATE WIDTH (3INCH)'                            157
C FRONTPLATE HEIGHT (10.023INCH)                                          158
      VPDG_FPHEIGHT=25.45842                                              158
      CVPDG_FPHEIGHT='FRONTPLATE HEIGHT (10.023INCH)'                     158
C FRONTPLATE THICKNESS (1INCH)                                            159
      VPDG_FPTHICK=2.54                                                   159
      CVPDG_FPTHICK='FRONTPLATE THICKNESS (1INCH)'                        159
C FRONTPL.PART A WIDTH (2.614INCH)                                        160
      VPDG_FPAWIDTH=6.63956                                               160
      CVPDG_FPAWIDTH='FRONTPL.PART A WIDTH (2.614INCH)'                   160
C FRONTPL.PART A HEIGHT (3.289INCH)                                       161
      VPDG_FPAHGHT=8.35405                                                161
      CVPDG_FPAHGHT='FRONTPL.PART A HEIGHT (3.289INCH)'                   161
C FRONTPL.PART C HEIGHT (4.95INCH)                                        162
      VPDG_FPCHGHT=12.573                                                 162
      CVPDG_FPCHGHT='FRONTPL.PART C HEIGHT (4.95INCH)'                    162
C FRONTPL. HOOK HEIGHT (1.5INCH)                                          163
      VPDG_FPHHGHT=3.81                                                   163
      CVPDG_FPHHGHT='FRONTPL. HOOK HEIGHT (1.5INCH)'                      163
C FRONTPL. HOOK WIDTH (2INCH)                                             164
      VPDG_FPHWIDTH=5.08                                                  164
      CVPDG_FPHWIDTH='FRONTPL. HOOK WIDTH (2INCH)'                        164
C FRONTPL. HOOK THICKNESS (0.25INCH)                                      165
      VPDG_FPHTHICK=0.635                                                 165
      CVPDG_FPHTHICK='FRONTPL. HOOK THICKNESS (0.25INCH)'                 165
C STRUT THICKNESS (0.25INCH)                                              166
      VPDG_STTHICK=0.635                                                  166
      CVPDG_STTHICK='STRUT THICKNESS (0.25INCH)'                          166
C STRUT HEIGHT (9.125 INCH)                                               167
      VPDG_STHEIGHT=23.1775                                               167
      CVPDG_STHEIGHT='STRUT HEIGHT (9.125 INCH)'                          167
C STRUT ANGLE (DEGR)                                                      168
      VPDG_STANGLE=25.                                                    168
      CVPDG_STANGLE='STRUT ANGLE (DEGR)'                                  168
C DIAGNOAL STRUT SIZE (2INCH)                                             169
      VPDG_STDIAGSZ=5.08                                                  169
      CVPDG_STDIAGSZ='DIAGNOAL STRUT SIZE (2INCH)'                        169
C STRUT CLAMP WIDTH (3INCH)                                               170
      VPDG_SCWIDTH=7.62                                                   170
      CVPDG_SCWIDTH='STRUT CLAMP WIDTH (3INCH)'                           170
C STRUT CLAMP HEIGHT (1.5INCH)                                            171
      VPDG_SCHEIGHT=3.81                                                  171
      CVPDG_SCHEIGHT='STRUT CLAMP HEIGHT (1.5INCH)'                       171
C STRUT CLAMP LENGTH (1.5INCH)                                            172
      VPDG_SCLENGTH=3.81                                                  172
      CVPDG_SCLENGTH='STRUT CLAMP LENGTH (1.5INCH)'                       172
C STRUT CLAMP THICKNESS (0.25INCH)                                        173
      VPDG_SCTHICK=0.635                                                  173
      CVPDG_SCTHICK='STRUT CLAMP THICKNESS (0.25INCH)'                    173
C CLAMP HEIGHT (1.375INCH)                                                174
      VPDG_CLHEIGHT=3.4925                                                174
      CVPDG_CLHEIGHT='CLAMP HEIGHT (1.375INCH)'                           174
C CLAMP WIDTH (1.5INCH)                                                   175
      VPDG_CLWIDTH=3.81                                                   175
      CVPDG_CLWIDTH='CLAMP WIDTH (1.5INCH)'                               175
C CLAMP LENGTH (16.25INCH)                                                176
      VPDG_CLLENGTH=41.275                                                176
      CVPDG_CLLENGTH='CLAMP LENGTH (16.25INCH)'                           176
C CLAMP PLATE THICKNESS (0.125INCH)                                       177
      VPDG_CLTHICK=0.3175                                                 177
      CVPDG_CLTHICK='CLAMP PLATE THICKNESS (0.125INCH)'                   177
C PMT ASSEMBLY LENGTH (13INCH)                                            178
      VPDG_DETLEN=33.02                                                   178
      CVPDG_DETLEN='PMT ASSEMBLY LENGTH (13INCH)'                         178
C PMT ASSEMBLY RADIUS (1.5INCH)                                           179
      VPDG_DETRAD=3.81                                                    179
      CVPDG_DETRAD='PMT ASSEMBLY RADIUS (1.5INCH)'                        179
C PMT ASS. FRONTPLATE THICKNESS (0.4INCH)                                 180
      VPDG_DETFRONT=1.016                                                 180
      CVPDG_DETFRONT='PMT ASS. FRONTPLATE THICKNESS (0.4INCH)'            180
C CONVERTER LAYER THICKNESS                                               181
      VPDG_CONVTHK=1.                                                     181
      CVPDG_CONVTHK='CONVERTER LAYER THICKNESS'                           181
C RADIATOR LAYER THICKNESS                                                182
      VPDG_RADITHK=1.                                                     182
      CVPDG_RADITHK='RADIATOR LAYER THICKNESS'                            182
C ELECTRONICS MOUNT LENGTH                                                183
      VPDG_ELELENG=15.0                                                   183
      CVPDG_ELELENG='ELECTRONICS MOUNT LENGTH'                            183
C LAYER RADIAL WIDTH                                                      184
      VPDG_DRLAYER=6.                                                     184
      CVPDG_DRLAYER='LAYER RADIAL WIDTH'                                  184
C NUMBER OF PMT IN LAYER                                                  185
      VPDG_NUMPMT=3                                                       185
      CVPDG_NUMPMT='NUMBER OF PMT IN LAYER'                               185
C PMT WALL THICKNESS                                                      186
      VPDG_PMTWALL=0.1                                                    186
      CVPDG_PMTWALL='PMT WALL THICKNESS'                                  186
C PMT AND DETECTOR RADIUS (1INCH)                                         187
      VPDG_PMTRAD=2.54                                                    187
      CVPDG_PMTRAD='PMT AND DETECTOR RADIUS (1INCH)'                      187
C PMT TUBE LENGTH                                                         188
      VPDG_PMTLEN=8.0                                                     188
      CVPDG_PMTLEN='PMT TUBE LENGTH'                                      188
C ACTIVE/DE-ACTIVATE (0) IBEAM                                            189
      VPDG_IBCHOICE=1                                                     189
      CVPDG_IBCHOICE='ACTIVE/DE-ACTIVATE (0) IBEAM'                       189
C IBEAM CENTRAL YLOCATION (6.5INCH)                                       190
      VPDG_IBPOSYC=-16.51                                                 190
      CVPDG_IBPOSYC='IBEAM CENTRAL YLOCATION (6.5INCH)'                   190
C IBEAM CENTRAL ZPOSITION                                                 191
      VPDG_IBPOSZC=530.477                                                191
      CVPDG_IBPOSZC='IBEAM CENTRAL ZPOSITION'                             191
C IBEAM LENGTH (116.1INCH)                                                192
      VPDG_IBLENG=294.894                                                 192
      CVPDG_IBLENG='IBEAM LENGTH (116.1INCH)'                             192
C IBEAM HORIZ. PLATE THICKNESS (0.29INCH)                                 193
      VPDG_IBTHICKH=0.7366                                                193
      CVPDG_IBTHICKH='IBEAM HORIZ. PLATE THICKNESS (0.29INCH)'            193
C IBEAM VERT. PLATE THICKNESS (0.17INCH)                                  194
      VPDG_IBTHICKV=0.4318                                                194
      CVPDG_IBTHICKV='IBEAM VERT. PLATE THICKNESS (0.17INCH)'             194
C IBEAM HEIGHT (4INCH)                                                    195
      VPDG_IBHEIGHT=10.16                                                 195
      CVPDG_IBHEIGHT='IBEAM HEIGHT (4INCH)'                               195
C IBEAM WIDTH (3INCH)                                                     196
      VPDG_IBWIDTH=7.62                                                   196
      CVPDG_IBWIDTH='IBEAM WIDTH (3INCH)'                                 196
C IBEAM VERT. FRONT PIECE (9INCH)                                         197
      VPDG_IBWLEN=22.86                                                   197
      CVPDG_IBWLEN='IBEAM VERT. FRONT PIECE (9INCH)'                      197
C IBEAM VERT. FRONT PIECE HEIGHT                                          198
      VPDG_IBWHGHTF=4.4489                                                198
      CVPDG_IBWHGHTF='IBEAM VERT. FRONT PIECE HEIGHT'                     198
C IBEAM VERT. FRONT PIECE HEIGHT                                          199
      VPDG_IBWHGHTB=8.3097                                                199
      CVPDG_IBWHGHTB='IBEAM VERT. FRONT PIECE HEIGHT'                     199
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    200
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           200
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   200
C *                                                                       200
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    200
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           200
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   200
C *                                                                       201
      CALL AGSBEGM(MODULE,IPRIN)                                          201
      IF (IPRIN.LT.0) RETURN                                              201
C Filling bank VPDG - PVPD BASIC DIMENSIONS                               201
      BANK_TITLE='PVPD BASIC DIMENSIONS'                                  201
      BPATH_VPDG='VPDG*'                                                  201
      IDNUM_VPDG(1)=0                                                     202
C GEOMETRY VERSION                                                        202
      VPDG_VERSION=4                                                      202
      CVPDG_VERSION='GEOMETRY VERSION'                                    202
C Z POSITION EAST                                                         203
      VPDG_ZPOSEAST=574.5688                                              203
      CVPDG_ZPOSEAST='Z POSITION EAST'                                    203
C Z POSITION WEST                                                         204
      VPDG_ZPOSWEST=573.5669                                              204
      CVPDG_ZPOSWEST='Z POSITION WEST'                                    204
C MOTHERVOLUME RMIN (2.5INCH)                                             205
      VPDG_RMIN=6.35                                                      205
      CVPDG_RMIN='MOTHERVOLUME RMIN (2.5INCH)'                            205
C MOTHERVOLUME RMIN (12.3125INCH)                                         206
      VPDG_RMAX=31.27375                                                  206
      CVPDG_RMAX='MOTHERVOLUME RMIN (12.3125INCH)'                        206
C BASEPLATE WIDTH (12INCH)                                                207
      VPDG_BPWIDTH=30.48                                                  207
      CVPDG_BPWIDTH='BASEPLATE WIDTH (12INCH)'                            207
C BASEPLATE LENGTH (18.5INCH)                                             208
      VPDG_BPLENGTH=46.355                                                208
      CVPDG_BPLENGTH='BASEPLATE LENGTH (18.5INCH)'                        208
C BASEPLATE THICKNESS (0.25INCH)                                          209
      VPDG_BPTHICK=0.635                                                  209
      CVPDG_BPTHICK='BASEPLATE THICKNESS (0.25INCH)'                      209
C FEE BOX HEIGHT (2INCH)                                                  210
      VPDG_BXHEIGHT=5.08                                                  210
      CVPDG_BXHEIGHT='FEE BOX HEIGHT (2INCH)'                             210
C FEE BOX LENGTH (6INCH)                                                  211
      VPDG_BXLENGTH=15.24                                                 211
      CVPDG_BXLENGTH='FEE BOX LENGTH (6INCH)'                             211
C FEE BOX WIDTH (10INCH)                                                  212
      VPDG_BXWIDTH=25.4                                                   212
      CVPDG_BXWIDTH='FEE BOX WIDTH (10INCH)'                              212
C FEE BOX PLATE THICKNESS (0.25INCH)                                      213
      VPDG_BXTHICK=0.635                                                  213
      CVPDG_BXTHICK='FEE BOX PLATE THICKNESS (0.25INCH)'                  213
C FEE CENTRAL Z POS (+/-??) (0.25INCH)                                    214
      VPDG_BXZPOSC=0.635                                                  214
      CVPDG_BXZPOSC='FEE CENTRAL Z POS (+/-??) (0.25INCH)'                214
C FRONTPLATE WIDTH (3INCH)                                                215
      VPDG_FPWIDTH=7.62                                                   215
      CVPDG_FPWIDTH='FRONTPLATE WIDTH (3INCH)'                            215
C FRONTPLATE HEIGHT (10.023INCH)                                          216
      VPDG_FPHEIGHT=25.45842                                              216
      CVPDG_FPHEIGHT='FRONTPLATE HEIGHT (10.023INCH)'                     216
C FRONTPLATE THICKNESS (1INCH)                                            217
      VPDG_FPTHICK=2.54                                                   217
      CVPDG_FPTHICK='FRONTPLATE THICKNESS (1INCH)'                        217
C FRONTPL.PART A WIDTH (2.614INCH)                                        218
      VPDG_FPAWIDTH=6.63956                                               218
      CVPDG_FPAWIDTH='FRONTPL.PART A WIDTH (2.614INCH)'                   218
C FRONTPL.PART A HEIGHT (3.289INCH)                                       219
      VPDG_FPAHGHT=8.35405                                                219
      CVPDG_FPAHGHT='FRONTPL.PART A HEIGHT (3.289INCH)'                   219
C FRONTPL.PART C HEIGHT (4.95INCH)                                        220
      VPDG_FPCHGHT=12.573                                                 220
      CVPDG_FPCHGHT='FRONTPL.PART C HEIGHT (4.95INCH)'                    220
C FRONTPL. HOOK HEIGHT (1.5INCH)                                          221
      VPDG_FPHHGHT=3.81                                                   221
      CVPDG_FPHHGHT='FRONTPL. HOOK HEIGHT (1.5INCH)'                      221
C FRONTPL. HOOK WIDTH (2INCH)                                             222
      VPDG_FPHWIDTH=5.08                                                  222
      CVPDG_FPHWIDTH='FRONTPL. HOOK WIDTH (2INCH)'                        222
C FRONTPL. HOOK THICKNESS (0.25INCH)                                      223
      VPDG_FPHTHICK=0.635                                                 223
      CVPDG_FPHTHICK='FRONTPL. HOOK THICKNESS (0.25INCH)'                 223
C STRUT THICKNESS (0.25INCH)                                              224
      VPDG_STTHICK=0.635                                                  224
      CVPDG_STTHICK='STRUT THICKNESS (0.25INCH)'                          224
C STRUT HEIGHT (9.125 INCH)                                               225
      VPDG_STHEIGHT=23.1775                                               225
      CVPDG_STHEIGHT='STRUT HEIGHT (9.125 INCH)'                          225
C STRUT ANGLE (DEGR)                                                      226
      VPDG_STANGLE=25.                                                    226
      CVPDG_STANGLE='STRUT ANGLE (DEGR)'                                  226
C DIAGNOAL STRUT SIZE (2INCH)                                             227
      VPDG_STDIAGSZ=5.08                                                  227
      CVPDG_STDIAGSZ='DIAGNOAL STRUT SIZE (2INCH)'                        227
C STRUT CLAMP WIDTH (3INCH)                                               228
      VPDG_SCWIDTH=7.62                                                   228
      CVPDG_SCWIDTH='STRUT CLAMP WIDTH (3INCH)'                           228
C STRUT CLAMP HEIGHT (1.5INCH)                                            229
      VPDG_SCHEIGHT=3.81                                                  229
      CVPDG_SCHEIGHT='STRUT CLAMP HEIGHT (1.5INCH)'                       229
C STRUT CLAMP LENGTH (1.5INCH)                                            230
      VPDG_SCLENGTH=3.81                                                  230
      CVPDG_SCLENGTH='STRUT CLAMP LENGTH (1.5INCH)'                       230
C STRUT CLAMP THICKNESS (0.25INCH)                                        231
      VPDG_SCTHICK=0.635                                                  231
      CVPDG_SCTHICK='STRUT CLAMP THICKNESS (0.25INCH)'                    231
C CLAMP HEIGHT (1.375INCH)                                                232
      VPDG_CLHEIGHT=3.4925                                                232
      CVPDG_CLHEIGHT='CLAMP HEIGHT (1.375INCH)'                           232
C CLAMP WIDTH (1.5INCH)                                                   233
      VPDG_CLWIDTH=3.81                                                   233
      CVPDG_CLWIDTH='CLAMP WIDTH (1.5INCH)'                               233
C CLAMP LENGTH (16.25INCH)                                                234
      VPDG_CLLENGTH=41.275                                                234
      CVPDG_CLLENGTH='CLAMP LENGTH (16.25INCH)'                           234
C CLAMP PLATE THICKNESS (0.125INCH)                                       235
      VPDG_CLTHICK=0.3175                                                 235
      CVPDG_CLTHICK='CLAMP PLATE THICKNESS (0.125INCH)'                   235
C PMT ASSEMBLY LENGTH (13INCH)                                            236
      VPDG_DETLEN=33.02                                                   236
      CVPDG_DETLEN='PMT ASSEMBLY LENGTH (13INCH)'                         236
C PMT ASSEMBLY RADIUS (1.5INCH)                                           237
      VPDG_DETRAD=3.81                                                    237
      CVPDG_DETRAD='PMT ASSEMBLY RADIUS (1.5INCH)'                        237
C PMT ASS. FRONTPLATE THICKNESS (0.4INCH)                                 238
      VPDG_DETFRONT=1.016                                                 238
      CVPDG_DETFRONT='PMT ASS. FRONTPLATE THICKNESS (0.4INCH)'            238
C CONVERTER LAYER THICKNESS                                               239
      VPDG_CONVTHK=1.                                                     239
      CVPDG_CONVTHK='CONVERTER LAYER THICKNESS'                           239
C RADIATOR LAYER THICKNESS                                                240
      VPDG_RADITHK=1.                                                     240
      CVPDG_RADITHK='RADIATOR LAYER THICKNESS'                            240
C ELECTRONICS MOUNT LENGTH                                                241
      VPDG_ELELENG=15.0                                                   241
      CVPDG_ELELENG='ELECTRONICS MOUNT LENGTH'                            241
C LAYER RADIAL WIDTH                                                      242
      VPDG_DRLAYER=6.                                                     242
      CVPDG_DRLAYER='LAYER RADIAL WIDTH'                                  242
C NUMBER OF PMT IN LAYER                                                  243
      VPDG_NUMPMT=3                                                       243
      CVPDG_NUMPMT='NUMBER OF PMT IN LAYER'                               243
C PMT WALL THICKNESS                                                      244
      VPDG_PMTWALL=0.1                                                    244
      CVPDG_PMTWALL='PMT WALL THICKNESS'                                  244
C PMT AND DETECTOR RADIUS (1INCH)                                         245
      VPDG_PMTRAD=2.54                                                    245
      CVPDG_PMTRAD='PMT AND DETECTOR RADIUS (1INCH)'                      245
C PMT TUBE LENGTH                                                         246
      VPDG_PMTLEN=8.0                                                     246
      CVPDG_PMTLEN='PMT TUBE LENGTH'                                      246
C ACTIVE/DE-ACTIVATE (0) IBEAM                                            247
      VPDG_IBCHOICE=1                                                     247
      CVPDG_IBCHOICE='ACTIVE/DE-ACTIVATE (0) IBEAM'                       247
C IBEAM CENTRAL YLOCATION (6.5INCH)                                       248
      VPDG_IBPOSYC=-16.51                                                 248
      CVPDG_IBPOSYC='IBEAM CENTRAL YLOCATION (6.5INCH)'                   248
C IBEAM CENTRAL ZPOSITION                                                 249
      VPDG_IBPOSZC=530.477                                                249
      CVPDG_IBPOSZC='IBEAM CENTRAL ZPOSITION'                             249
C IBEAM LENGTH (116.1INCH)                                                250
      VPDG_IBLENG=294.894                                                 250
      CVPDG_IBLENG='IBEAM LENGTH (116.1INCH)'                             250
C IBEAM HORIZ. PLATE THICKNESS (0.29INCH)                                 251
      VPDG_IBTHICKH=0.7366                                                251
      CVPDG_IBTHICKH='IBEAM HORIZ. PLATE THICKNESS (0.29INCH)'            251
C IBEAM VERT. PLATE THICKNESS (0.17INCH)                                  252
      VPDG_IBTHICKV=0.4318                                                252
      CVPDG_IBTHICKV='IBEAM VERT. PLATE THICKNESS (0.17INCH)'             252
C IBEAM HEIGHT (4INCH)                                                    253
      VPDG_IBHEIGHT=10.16                                                 253
      CVPDG_IBHEIGHT='IBEAM HEIGHT (4INCH)'                               253
C IBEAM WIDTH (3INCH)                                                     254
      VPDG_IBWIDTH=7.62                                                   254
      CVPDG_IBWIDTH='IBEAM WIDTH (3INCH)'                                 254
C IBEAM VERT. FRONT PIECE (9INCH)                                         255
      VPDG_IBWLEN=22.86                                                   255
      CVPDG_IBWLEN='IBEAM VERT. FRONT PIECE (9INCH)'                      255
C IBEAM VERT. FRONT PIECE HEIGHT                                          256
      VPDG_IBWHGHTF=4.4489                                                256
      CVPDG_IBWHGHTF='IBEAM VERT. FRONT PIECE HEIGHT'                     256
C IBEAM VERT. FRONT PIECE HEIGHT                                          257
      VPDG_IBWHGHTB=8.3097                                                257
      CVPDG_IBWHGHTB='IBEAM VERT. FRONT PIECE HEIGHT'                     257
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    258
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           258
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   258
C *                                                                       258
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    258
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           258
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   258
C use default VPDV bank                                                   259
      BPATH_VPDV='VPDV*'                                                  259
      IDNUM_VPDV(1)=0                                                     259
      CALL AGDGETP('VPDDGEO1','*',-999,'VPDV',' ',BANK_VPDV, BPATH_VPDV,  259
     *IDNUM_VPDV,1,MAP_VPDV,NAMES_VPDV,BANK_VPDV,LEN_VPDV,LLEN_VPDV,      259
     *LINK_VPDV,FLAG_VPDV)                                                259
C use VPDG bank                                                           260
      BPATH_VPDG='VPDG*'                                                  260
      IDNUM_VPDG(1)=0                                                     260
      VPDG_VERSION=VPDV_VPDCONFIG                                         260
      CALL AGDGETP('VPDDGEO1','*',-999,'VPDG',NVPDG_VERSION,              260
     *RE_VPDG_VERSION,BPATH_VPDG,IDNUM_VPDG,1,MAP_VPDG,NAMES_VPDG,        260
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   260
      CALL AGSBEGM(MODULE,IPRIN)                                          261
C Check IPRIN<0                                                           261
      IF (IPRIN.LT.0) RETURN                                              261
C Create VPDD :                                                           261
C Call To Local Procedure VPDD                                            261
      ASSIGN 5020 TO BLOCK_VPDD                                           261
      GO TO  5010                                                         261
5020  CONTINUE                                                            261
C *                                                                       261
      ZPOS = VPDG_ZPOSWEST                                                262
C Geant POSITION : VPDD ===> MOTHER='CAVE' Z=+ZPOS KONLY='MANY' <===      263
      CALL AGSRESET                                                       263
      AG_MOTHER = 'CAVE'                                                  263
      AG_Z = +ZPOS                                                        263
      AG_KONLY = 'MANY'                                                   263
      AG_TITLE = 'VPDD'                                                   263
      AG_EXNAME = 'POSITION'                                              263
      AG_PARLIST = 'MOTHER_Z_KONLY'                                       263
      CALL AXPOSITION                                                     263
C *                                                                       263
      ZPOS = VPDG_ZPOSEAST                                                263
C Geant POSITION : VPDD ===> MOTHER='CAVE' Z=-ZPOS THETAZ=180 KONLY='MAN  265
C    *Y' <===                                                             265
      CALL AGSRESET                                                       265
      AG_MOTHER = 'CAVE'                                                  265
      AG_Z = -ZPOS                                                        265
      AG_THETAZ = 180                                                     265
      AG_KONLY = 'MANY'                                                   265
      AG_TITLE = 'VPDD'                                                   265
      AG_EXNAME = 'POSITION'                                              265
      AG_PARLIST = 'MOTHER_Z_THETAZ_KONLY'                                265
      CALL AXPOSITION                                                     265
C *                                                                       265
C Check VPDG_IBCHOICE!=0                                                  265
      IF (VPDG_IBCHOICE.NE.0) THEN                                        266
C Geant POSITION : IBEM ===> MOTHER='CAVE' Y=VPDG_IBPOSYC Z=+VPDG_IBPOSZ  267
C C <===                                                                  267
      CALL AGSRESET                                                       267
      AG_MOTHER = 'CAVE'                                                  267
      AG_Y = VPDG_IBPOSYC                                                 267
      AG_Z = +VPDG_IBPOSZC                                                267
      AG_TITLE = 'IBEM'                                                   267
      AG_EXNAME = 'POSITION'                                              267
      AG_PARLIST = 'MOTHER_Y_Z'                                           267
      CALL AXPOSITION                                                     267
C *                                                                       267
C Geant POSITION : IBEM ===> MOTHER='CAVE' Y=VPDG_IBPOSYC Z=-VPDG_IBPOSZ  267
C C THETAZ=180 <===                                                       267
      CALL AGSRESET                                                       268
      AG_MOTHER = 'CAVE'                                                  268
      AG_Y = VPDG_IBPOSYC                                                 268
      AG_Z = -VPDG_IBPOSZC                                                268
      AG_THETAZ = 180                                                     268
      AG_TITLE = 'IBEM'                                                   268
      AG_EXNAME = 'POSITION'                                              268
      AG_PARLIST = 'MOTHER_Y_Z_THETAZ'                                    268
      CALL AXPOSITION                                                     268
C *                                                                       268
      ENDIF                                                               268
C ----------------------------------------------------------------------  270
C ------------ VPDD                                                       270
      CALL AGSENDM                                                        271
      RETURN                                                              271
C Begin Local Procedure VPDD                                              271
C VPDD IS THE WHOLE VPPD ASSEMBLY                                         271
5010  CONTINUE                                                            271
         AG_TITLE='VPDD'                                                  271
         AG_MODULE=MODULE                                                 271
         AG_MARK='V00'                                                    271
         CALL AGSPUSH                                                     271
         AG_MODULE=MODULE                                                 271
C    *                                                                    271
C    Geant MATERIAL : AIR ===> inherited arguments used <===              272
         CALL AGSRESET                                                    272
         AG_TITLE = 'AIR'                                                 272
         AG_EXNAME = 'MATERIAL'                                           272
         AG_PARLIST = 'NONE'                                              272
         CALL AXMATERIAL                                                  272
C    *                                                                    272
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           272
         CALL AGSRESET                                                    273
         AG_TITLE = 'STANDARD'                                            273
         AG_EXNAME = 'MEDIUM'                                             273
         AG_PARLIST = 'NONE'                                              273
         CALL AXMEDIUM                                                    273
C    *                                                                    273
C    Geant ATTRIBUTE : VPDD ===> SEEN=0 SERIAL=ZPOS <===                  273
         CALL AGSRESET                                                    274
         AG_SEEN = 0                                                      274
         AG_SERIAL = ZPOS                                                 274
         AG_TITLE = 'VPDD'                                                274
         AG_EXNAME = 'ATTRIBUTE'                                          274
         AG_PARLIST = 'SEEN_SERIAL'                                       274
         CALL AXATTRIBUTE                                                 274
C    *                                                                    274
C    Geant SHAPE : TUBE ===> RMIN=VPDG_RMIN RMAX=VPDG_RMAX, DZ=VPDG_BPLE  274
C    NGTH/2 <===                                                          274
         CALL AGSRESET                                                    276
         AG_RMIN = VPDG_RMIN                                              276
         AG_RMAX = VPDG_RMAX                                              276
         AG_DZ = VPDG_BPLENGTH/2                                          276
         AG_TITLE = 'TUBE'                                                276
         AG_EXNAME = 'SHAPE'                                              276
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      276
         CALL AXSHAPE                                                     276
C    *                                                                    276
         YBASE= VPDG_IBPOSYC - (VPDG_IBHEIGHT + VPDG_BPTHICK)/2           276
         YTOP = YBASE + VPDG_BPTHICK/2 + VPDG_FPHEIGHT + VPDG_FPCHGHT     278
C    Create VPBP :                                                        279
C    Call To Local Procedure VPBP                                         279
         ASSIGN 5040 TO BLOCK_VPBP                                        279
         GO TO  5030                                                      279
5040     CONTINUE                                                         279
C    *                                                                    279
C    Geant POSITION : VPBP ===> Y=YBASE <===                              280
         CALL AGSRESET                                                    280
         AG_Y = YBASE                                                     280
         AG_TITLE = 'VPBP'                                                280
         AG_EXNAME = 'POSITION'                                           280
         AG_PARLIST = 'Y'                                                 280
         CALL AXPOSITION                                                  280
C    *                                                                    280
C    Create VPBO :                                                        281
C    Call To Local Procedure VPBO                                         281
         ASSIGN 5060 TO BLOCK_VPBO                                        281
         GO TO  5050                                                      281
5060     CONTINUE                                                         281
C    *                                                                    281
         XLOC=(VPDG_BPWIDTH - VPDG_FPWIDTH)/2                             282
         YLOC=YBASE + (VPDG_BPTHICK + VPDG_FPHHGHT)/2                     283
         ZLOC=(VPDG_BPLENGTH - 2*VPDG_FPTHICK - VPDG_FPHHGHT)/2           284
C    Geant POSITION : VPBO ===> X=XLOC Y=YLOC Z=ZLOC THETAZ=180 <===      285
         CALL AGSRESET                                                    285
         AG_X = XLOC                                                      285
         AG_Y = YLOC                                                      285
         AG_Z = ZLOC                                                      285
         AG_THETAZ = 180                                                  285
         AG_TITLE = 'VPBO'                                                285
         AG_EXNAME = 'POSITION'                                           285
         AG_PARLIST = 'X_Y_Z_THETAZ'                                      285
         CALL AXPOSITION                                                  285
C    *                                                                    285
C    Geant POSITION : VPBO ===> X=XLOC Y=YLOC Z=-ZLOC <===                285
         CALL AGSRESET                                                    286
         AG_X = XLOC                                                      286
         AG_Y = YLOC                                                      286
         AG_Z = -ZLOC                                                     286
         AG_TITLE = 'VPBO'                                                286
         AG_EXNAME = 'POSITION'                                           286
         AG_PARLIST = 'X_Y_Z'                                             286
         CALL AXPOSITION                                                  286
C    *                                                                    286
C    Geant POSITION : VPBO ===> X=-XLOC Y=YLOC Z=ZLOC THETAZ=180 <===     286
         CALL AGSRESET                                                    287
         AG_X = -XLOC                                                     287
         AG_Y = YLOC                                                      287
         AG_Z = ZLOC                                                      287
         AG_THETAZ = 180                                                  287
         AG_TITLE = 'VPBO'                                                287
         AG_EXNAME = 'POSITION'                                           287
         AG_PARLIST = 'X_Y_Z_THETAZ'                                      287
         CALL AXPOSITION                                                  287
C    *                                                                    287
C    Geant POSITION : VPBO ===> X=-XLOC Y=YLOC Z=-ZLOC <===               287
         CALL AGSRESET                                                    288
         AG_X = -XLOC                                                     288
         AG_Y = YLOC                                                      288
         AG_Z = -ZLOC                                                     288
         AG_TITLE = 'VPBO'                                                288
         AG_EXNAME = 'POSITION'                                           288
         AG_PARLIST = 'X_Y_Z'                                             288
         CALL AXPOSITION                                                  288
C    *                                                                    288
C    Create VPFA :                                                        289
C    Call To Local Procedure VPFA                                         289
         ASSIGN 5080 TO BLOCK_VPFA                                        289
         GO TO  5070                                                      289
5080     CONTINUE                                                         289
C    *                                                                    289
         YLOC=YTOP - VPDG_FPAHGHT/2                                       290
         ZLOC=(VPDG_BPLENGTH - VPDG_FPTHICK)/2                            291
C    Geant POSITION : VPFA ===> Y=YLOC Z=ZLOC <===                        292
         CALL AGSRESET                                                    292
         AG_Y = YLOC                                                      292
         AG_Z = ZLOC                                                      292
         AG_TITLE = 'VPFA'                                                292
         AG_EXNAME = 'POSITION'                                           292
         AG_PARLIST = 'Y_Z'                                               292
         CALL AXPOSITION                                                  292
C    *                                                                    292
C    Geant POSITION : VPFA ===> Y=YLOC Z=-ZLOC <===                       292
         CALL AGSRESET                                                    293
         AG_Y = YLOC                                                      293
         AG_Z = -ZLOC                                                     293
         AG_TITLE = 'VPFA'                                                293
         AG_EXNAME = 'POSITION'                                           293
         AG_PARLIST = 'Y_Z'                                               293
         CALL AXPOSITION                                                  293
C    *                                                                    293
C    Create VPFB :                                                        294
C    Call To Local Procedure VPFB                                         294
         ASSIGN 5100 TO BLOCK_VPFB                                        294
         GO TO  5090                                                      294
5100     CONTINUE                                                         294
C    *                                                                    294
         XLOC=(VPDG_FPAWIDTH + (VPDG_BPWIDTH - VPDG_FPAWIDTH -            295
     *   2*VPDG_FPWIDTH)/2)/2                                             295
         YLOC=(YTOP - (VPDG_FPCHGHT + VPDG_FPAHGHT)/4)                    296
         ZLOC=(VPDG_BPLENGTH - VPDG_FPTHICK)/2                            297
C    Geant POSITION : VPFB ===> X=XLOC Y=YLOC Z=ZLOC ORT=Y-XZ <===        298
         CALL AGSRESET                                                    298
         AG_X = XLOC                                                      298
         AG_Y = YLOC                                                      298
         AG_Z = ZLOC                                                      298
         AG_ALPHAN=AG_ALPHAN+1                                            298
         AG_ALPHAK(AG_ALPHAN)=IGOR('Y-XZ')                                298
         AG_TITLE = 'VPFB'                                                298
         AG_EXNAME = 'POSITION'                                           298
         AG_PARLIST = 'X_Y_Z_ORT'                                         298
         CALL AXPOSITION                                                  298
C    *                                                                    298
C    Geant POSITION : VPFB ===> X=XLOC Y=YLOC Z=-ZLOC ORT=Y-XZ <===       298
         CALL AGSRESET                                                    299
         AG_X = XLOC                                                      299
         AG_Y = YLOC                                                      299
         AG_Z = -ZLOC                                                     299
         AG_ALPHAN=AG_ALPHAN+1                                            299
         AG_ALPHAK(AG_ALPHAN)=IGOR('Y-XZ')                                299
         AG_TITLE = 'VPFB'                                                299
         AG_EXNAME = 'POSITION'                                           299
         AG_PARLIST = 'X_Y_Z_ORT'                                         299
         CALL AXPOSITION                                                  299
C    *                                                                    299
C    Geant POSITION : VPFB ===> X=-XLOC Y=YLOC Z=ZLOC ORT=YXZ <===        299
         CALL AGSRESET                                                    300
         AG_X = -XLOC                                                     300
         AG_Y = YLOC                                                      300
         AG_Z = ZLOC                                                      300
         AG_ALPHAN=AG_ALPHAN+1                                            300
         AG_ALPHAK(AG_ALPHAN)=IGOR('YXZ')                                 300
         AG_TITLE = 'VPFB'                                                300
         AG_EXNAME = 'POSITION'                                           300
         AG_PARLIST = 'X_Y_Z_ORT'                                         300
         CALL AXPOSITION                                                  300
C    *                                                                    300
C    Geant POSITION : VPFB ===> X=-XLOC Y=YLOC Z=-ZLOC ORT=YXZ <===       300
         CALL AGSRESET                                                    301
         AG_X = -XLOC                                                     301
         AG_Y = YLOC                                                      301
         AG_Z = -ZLOC                                                     301
         AG_ALPHAN=AG_ALPHAN+1                                            301
         AG_ALPHAK(AG_ALPHAN)=IGOR('YXZ')                                 301
         AG_TITLE = 'VPFB'                                                301
         AG_EXNAME = 'POSITION'                                           301
         AG_PARLIST = 'X_Y_Z_ORT'                                         301
         CALL AXPOSITION                                                  301
C    *                                                                    301
C    Create VPFC :                                                        302
C    Call To Local Procedure VPFC                                         302
         ASSIGN 5120 TO BLOCK_VPFC                                        302
         GO TO  5110                                                      302
5120     CONTINUE                                                         302
C    *                                                                    302
         XLOC=(VPDG_BPWIDTH - VPDG_FPWIDTH)/2                             303
         YLOC=(YTOP - (VPDG_FPCHGHT + VPDG_FPWIDTH/2)/2)                  304
         ZLOC=(VPDG_BPLENGTH - VPDG_FPTHICK)/2                            305
C    Geant POSITION : VPFC ===> X=XLOC Y=YLOC Z=ZLOC ORT=Y-XZ <===        306
         CALL AGSRESET                                                    306
         AG_X = XLOC                                                      306
         AG_Y = YLOC                                                      306
         AG_Z = ZLOC                                                      306
         AG_ALPHAN=AG_ALPHAN+1                                            306
         AG_ALPHAK(AG_ALPHAN)=IGOR('Y-XZ')                                306
         AG_TITLE = 'VPFC'                                                306
         AG_EXNAME = 'POSITION'                                           306
         AG_PARLIST = 'X_Y_Z_ORT'                                         306
         CALL AXPOSITION                                                  306
C    *                                                                    306
C    Geant POSITION : VPFC ===> X=XLOC Y=YLOC Z=-ZLOC ORT=Y-XZ <===       306
         CALL AGSRESET                                                    307
         AG_X = XLOC                                                      307
         AG_Y = YLOC                                                      307
         AG_Z = -ZLOC                                                     307
         AG_ALPHAN=AG_ALPHAN+1                                            307
         AG_ALPHAK(AG_ALPHAN)=IGOR('Y-XZ')                                307
         AG_TITLE = 'VPFC'                                                307
         AG_EXNAME = 'POSITION'                                           307
         AG_PARLIST = 'X_Y_Z_ORT'                                         307
         CALL AXPOSITION                                                  307
C    *                                                                    307
C    Geant POSITION : VPFC ===> X=-XLOC Y=YLOC Z=ZLOC ORT=YXZ <===        307
         CALL AGSRESET                                                    308
         AG_X = -XLOC                                                     308
         AG_Y = YLOC                                                      308
         AG_Z = ZLOC                                                      308
         AG_ALPHAN=AG_ALPHAN+1                                            308
         AG_ALPHAK(AG_ALPHAN)=IGOR('YXZ')                                 308
         AG_TITLE = 'VPFC'                                                308
         AG_EXNAME = 'POSITION'                                           308
         AG_PARLIST = 'X_Y_Z_ORT'                                         308
         CALL AXPOSITION                                                  308
C    *                                                                    308
C    Geant POSITION : VPFC ===> X=-XLOC Y=YLOC Z=-ZLOC ORT=YXZ <===       308
         CALL AGSRESET                                                    309
         AG_X = -XLOC                                                     309
         AG_Y = YLOC                                                      309
         AG_Z = -ZLOC                                                     309
         AG_ALPHAN=AG_ALPHAN+1                                            309
         AG_ALPHAK(AG_ALPHAN)=IGOR('YXZ')                                 309
         AG_TITLE = 'VPFC'                                                309
         AG_EXNAME = 'POSITION'                                           309
         AG_PARLIST = 'X_Y_Z_ORT'                                         309
         CALL AXPOSITION                                                  309
C    *                                                                    309
C    Create VPFP :                                                        310
C    Call To Local Procedure VPFP                                         310
         ASSIGN 5140 TO BLOCK_VPFP                                        310
         GO TO  5130                                                      310
5140     CONTINUE                                                         310
C    *                                                                    310
         XLOC=(VPDG_BPWIDTH - VPDG_FPWIDTH)/2                             311
         YLOC=(YBASE + VPDG_BPTHICK/2 + VPDG_FPHEIGHT/2)                  312
         ZLOC=(VPDG_BPLENGTH - VPDG_FPTHICK)/2                            313
C    Geant POSITION : VPFP ===> X=XLOC Y=YLOC Z=ZLOC <===                 314
         CALL AGSRESET                                                    314
         AG_X = XLOC                                                      314
         AG_Y = YLOC                                                      314
         AG_Z = ZLOC                                                      314
         AG_TITLE = 'VPFP'                                                314
         AG_EXNAME = 'POSITION'                                           314
         AG_PARLIST = 'X_Y_Z'                                             314
         CALL AXPOSITION                                                  314
C    *                                                                    314
C    Geant POSITION : VPFP ===> X=XLOC Y=YLOC Z=-ZLOC <===                314
         CALL AGSRESET                                                    315
         AG_X = XLOC                                                      315
         AG_Y = YLOC                                                      315
         AG_Z = -ZLOC                                                     315
         AG_TITLE = 'VPFP'                                                315
         AG_EXNAME = 'POSITION'                                           315
         AG_PARLIST = 'X_Y_Z'                                             315
         CALL AXPOSITION                                                  315
C    *                                                                    315
C    Geant POSITION : VPFP ===> X=-XLOC Y=YLOC Z=ZLOC <===                315
         CALL AGSRESET                                                    316
         AG_X = -XLOC                                                     316
         AG_Y = YLOC                                                      316
         AG_Z = ZLOC                                                      316
         AG_TITLE = 'VPFP'                                                316
         AG_EXNAME = 'POSITION'                                           316
         AG_PARLIST = 'X_Y_Z'                                             316
         CALL AXPOSITION                                                  316
C    *                                                                    316
C    Geant POSITION : VPFP ===> X=-XLOC Y=YLOC Z=-ZLOC <===               316
         CALL AGSRESET                                                    317
         AG_X = -XLOC                                                     317
         AG_Y = YLOC                                                      317
         AG_Z = -ZLOC                                                     317
         AG_TITLE = 'VPFP'                                                317
         AG_EXNAME = 'POSITION'                                           317
         AG_PARLIST = 'X_Y_Z'                                             317
         CALL AXPOSITION                                                  317
C    *                                                                    317
         STRUTHEIGHT = VPDG_STDIAGSZ/COS(VPDG_STANGLE*DEGRAD) +           317
     *   VPDG_BPLENGTH*TAN(VPDG_STANGLE*DEGRAD)                           317
C    Create VPST :                                                        319
C    Call To Local Procedure VPST                                         319
         ASSIGN 5160 TO BLOCK_VPST                                        319
         GO TO  5150                                                      319
5160     CONTINUE                                                         319
C    *                                                                    319
         XLOC=(VPDG_BPWIDTH + VPDG_STTHICK)/2                             320
         YLOC=YTOP - VPDG_FPWIDTH - STRUTHEIGHT/2                         321
C    Geant POSITION : VPST ===> X=XLOC Y=YLOC <===                        322
         CALL AGSRESET                                                    322
         AG_X = XLOC                                                      322
         AG_Y = YLOC                                                      322
         AG_TITLE = 'VPST'                                                322
         AG_EXNAME = 'POSITION'                                           322
         AG_PARLIST = 'X_Y'                                               322
         CALL AXPOSITION                                                  322
C    *                                                                    322
C    Geant POSITION : VPST ===> X=-XLOC Y=YLOC <===                       322
         CALL AGSRESET                                                    323
         AG_X = -XLOC                                                     323
         AG_Y = YLOC                                                      323
         AG_TITLE = 'VPST'                                                323
         AG_EXNAME = 'POSITION'                                           323
         AG_PARLIST = 'X_Y'                                               323
         CALL AXPOSITION                                                  323
C    *                                                                    323
C    Create VPSC :                                                        324
C    Call To Local Procedure VPSC                                         324
         ASSIGN 5180 TO BLOCK_VPSC                                        324
         GO TO  5170                                                      324
5180     CONTINUE                                                         324
C    *                                                                    324
         XLOC=(VPDG_BPWIDTH - VPDG_SCWIDTH)/2                             325
         YLOC=YTOP - VPDG_FPWIDTH - STRUTHEIGHT + VPDG_SCHEIGHT/2         326
         ZLOC=(VPDG_BPLENGTH + VPDG_SCLENGTH)/2                           327
C    Geant POSITION : VPSC ===> X=XLOC Y=YLOC Z=ZLOC ALPHAX=180. <===     328
         CALL AGSRESET                                                    328
         AG_X = XLOC                                                      328
         AG_Y = YLOC                                                      328
         AG_Z = ZLOC                                                      328
         AG_ALPHAN=AG_ALPHAN+1                                            328
         AG_ALPHAK(AG_ALPHAN)=1                                           328
         AG_ALPHAL(AG_ALPHAN)=180.                                        328
         AG_TITLE = 'VPSC'                                                328
         AG_EXNAME = 'POSITION'                                           328
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      328
         CALL AXPOSITION                                                  328
C    *                                                                    328
C    Geant POSITION : VPSC ===> X=-XLOC Y=YLOC Z=ZLOC ALPHAX=180. ALPHAZ  328
C    =180. <===                                                           328
         CALL AGSRESET                                                    329
         AG_X = -XLOC                                                     329
         AG_Y = YLOC                                                      329
         AG_Z = ZLOC                                                      329
         AG_ALPHAN=AG_ALPHAN+1                                            329
         AG_ALPHAK(AG_ALPHAN)=1                                           329
         AG_ALPHAL(AG_ALPHAN)=180.                                        329
         AG_ALPHAN=AG_ALPHAN+1                                            329
         AG_ALPHAK(AG_ALPHAN)=3                                           329
         AG_ALPHAL(AG_ALPHAN)=180.                                        329
         AG_TITLE = 'VPSC'                                                329
         AG_EXNAME = 'POSITION'                                           329
         AG_PARLIST = 'X_Y_Z_ALPHAX_ALPHAZ'                               329
         CALL AXPOSITION                                                  329
C    *                                                                    329
         YLOC=YTOP - VPDG_FPWIDTH - VPDG_SCHEIGHT/2                       329
C    Geant POSITION : VPSC ===> X=XLOC Y=YLOC Z=-ZLOC <===                331
         CALL AGSRESET                                                    331
         AG_X = XLOC                                                      331
         AG_Y = YLOC                                                      331
         AG_Z = -ZLOC                                                     331
         AG_TITLE = 'VPSC'                                                331
         AG_EXNAME = 'POSITION'                                           331
         AG_PARLIST = 'X_Y_Z'                                             331
         CALL AXPOSITION                                                  331
C    *                                                                    331
C    Geant POSITION : VPSC ===> X=-XLOC Y=YLOC Z=-ZLOC ALPHAZ=180. <===   331
         CALL AGSRESET                                                    332
         AG_X = -XLOC                                                     332
         AG_Y = YLOC                                                      332
         AG_Z = -ZLOC                                                     332
         AG_ALPHAN=AG_ALPHAN+1                                            332
         AG_ALPHAK(AG_ALPHAN)=3                                           332
         AG_ALPHAL(AG_ALPHAN)=180.                                        332
         AG_TITLE = 'VPSC'                                                332
         AG_EXNAME = 'POSITION'                                           332
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      332
         CALL AXPOSITION                                                  332
C    *                                                                    332
C    Create VPBX :                                                        333
C    Call To Local Procedure VPBX                                         333
         ASSIGN 5200 TO BLOCK_VPBX                                        333
         GO TO  5190                                                      333
5200     CONTINUE                                                         333
C    *                                                                    333
C    Geant POSITION : VPBX ===> X=-(VPDG_BPWIDTH-VPDG_BXWIDTH)/2 Y=(YBAS  335
C    E-VPDG_BPTHICK/2-VPDG_BXHEIGHT/2) <===                               335
         CALL AGSRESET                                                    335
         AG_X = -(VPDG_BPWIDTH-VPDG_BXWIDTH)/2                            335
         AG_Y = (YBASE-VPDG_BPTHICK/2-VPDG_BXHEIGHT/2)                    335
         AG_TITLE = 'VPBX'                                                335
         AG_EXNAME = 'POSITION'                                           335
         AG_PARLIST = 'X_Y'                                               335
         CALL AXPOSITION                                                  335
C    *                                                                    335
C    Create VRNG :                                                        336
C    Call To Local Procedure VRNG                                         336
         ASSIGN 5220 TO BLOCK_VRNG                                        336
         GO TO  5210                                                      336
5220     CONTINUE                                                         336
C    *                                                                    336
C    Geant POSITION : VRNG ===> KONLY='MANY' <===                         337
         CALL AGSRESET                                                    337
         AG_KONLY = 'MANY'                                                337
         AG_TITLE = 'VRNG'                                                337
         AG_EXNAME = 'POSITION'                                           337
         AG_PARLIST = 'KONLY'                                             337
         CALL AXPOSITION                                                  337
C    *                                                                    337
C    Check VPDG_IBCHOICE!=0                                               337
         IF (VPDG_IBCHOICE.NE.0) THEN                                     338
C    Create IBEM :                                                        339
C    Call To Local Procedure IBEM                                         339
         ASSIGN 5240 TO BLOCK_IBEM                                        339
         GO TO  5230                                                      339
5240     CONTINUE                                                         339
C    *                                                                    339
C    Geant POSITION : IBEM ===> Y=VPDG_IBPOSYC Z=-ZPOS+VPDG_IBPOSZC <===  340
         CALL AGSRESET                                                    340
         AG_Y = VPDG_IBPOSYC                                              340
         AG_Z = -ZPOS+VPDG_IBPOSZC                                        340
         AG_TITLE = 'IBEM'                                                340
         AG_EXNAME = 'POSITION'                                           340
         AG_PARLIST = 'Y_Z'                                               340
         CALL AXPOSITION                                                  340
C    *                                                                    340
         ENDIF                                                            340
5011  CALL AGSPOPB                                                        342
      GO TO BLOCK_VPDD, (5010 ,5020)                                      342
C End of Local Procedure                                                  342
C ----------------------------------------------------------------------  343
C ------------ VPBP                                                       343
C Begin Local Procedure VPBP                                              344
C VPBP IS THE BASE PLATE                                                  344
5030  CONTINUE                                                            344
         AG_TITLE='VPBP'                                                  344
         AG_MODULE=MODULE                                                 344
         AG_MARK='V00'                                                    344
         CALL AGSPUSH                                                     344
         AG_MODULE=MODULE                                                 344
C    *                                                                    344
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        345
         CALL AGSRESET                                                    345
         AG_TITLE = 'ALUMINIUM'                                           345
         AG_EXNAME = 'MATERIAL'                                           345
         AG_PARLIST = 'NONE'                                              345
         CALL AXMATERIAL                                                  345
C    *                                                                    345
C    Geant ATTRIBUTE : VPBP ===> SEEN=1 COLO=5 FILL=6 <===                345
         CALL AGSRESET                                                    346
         AG_SEEN = 1                                                      346
         AG_COLO = 5                                                      346
         AG_FILL = 6                                                      346
         AG_TITLE = 'VPBP'                                                346
         AG_EXNAME = 'ATTRIBUTE'                                          346
         AG_PARLIST = 'SEEN_COLO_FILL'                                    346
         CALL AXATTRIBUTE                                                 346
C    *                                                                    346
C    Geant SHAPE : BOX ===> DX=VPDG_BPWIDTH/2 DY=VPDG_BPTHICK/2 <===      346
         CALL AGSRESET                                                    347
         AG_DX = VPDG_BPWIDTH/2                                           347
         AG_DY = VPDG_BPTHICK/2                                           347
         AG_TITLE = 'BOX'                                                 347
         AG_EXNAME = 'SHAPE'                                              347
         AG_PARLIST = 'DX_DY'                                             347
         CALL AXSHAPE                                                     347
C    *                                                                    347
5031  CALL AGSPOPB                                                        348
      GO TO BLOCK_VPBP, (5030 ,5040)                                      348
C End of Local Procedure                                                  348
C ----------------------------------------------------------------------  349
C ------------ VPBO                                                       349
C Begin Local Procedure VPBO                                              350
C VPBO IS CONTAINER FOR THE HOOK                                          350
5050  CONTINUE                                                            350
         AG_TITLE='VPBO'                                                  350
         AG_MODULE=MODULE                                                 350
         AG_MARK='V00'                                                    350
         CALL AGSPUSH                                                     350
         AG_MODULE=MODULE                                                 350
C    *                                                                    350
C    Geant ATTRIBUTE : VPBO ===> SEEN=0 <===                              351
         CALL AGSRESET                                                    351
         AG_SEEN = 0                                                      351
         AG_TITLE = 'VPBO'                                                351
         AG_EXNAME = 'ATTRIBUTE'                                          351
         AG_PARLIST = 'SEEN'                                              351
         CALL AXATTRIBUTE                                                 351
C    *                                                                    351
C    Geant SHAPE : BOX ===> DX=VPDG_FPHWIDTH/2 DY=VPDG_FPHHGHT/2, DZ=VPD  351
C    G_FPHHGHT/2 <===                                                     351
         CALL AGSRESET                                                    353
         AG_DX = VPDG_FPHWIDTH/2                                          353
         AG_DY = VPDG_FPHHGHT/2                                           353
         AG_DZ = VPDG_FPHHGHT/2                                           353
         AG_TITLE = 'BOX'                                                 353
         AG_EXNAME = 'SHAPE'                                              353
         AG_PARLIST = 'DX_DY_DZ'                                          353
         CALL AXSHAPE                                                     353
C    *                                                                    353
C    Create VPBA :                                                        354
C    Call To Local Procedure VPBA                                         354
         ASSIGN 5260 TO BLOCK_VPBA                                        354
         GO TO  5250                                                      354
5260     CONTINUE                                                         354
C    *                                                                    354
C    Geant POSITION : VPBA ===> Z=-(VPDG_FPHHGHT-VPDG_FPHTHICK)/2 <===    355
         CALL AGSRESET                                                    355
         AG_Z = -(VPDG_FPHHGHT-VPDG_FPHTHICK)/2                           355
         AG_TITLE = 'VPBA'                                                355
         AG_EXNAME = 'POSITION'                                           355
         AG_PARLIST = 'Z'                                                 355
         CALL AXPOSITION                                                  355
C    *                                                                    355
C    Create VPBB :                                                        356
C    Call To Local Procedure VPBB                                         356
         ASSIGN 5280 TO BLOCK_VPBB                                        356
         GO TO  5270                                                      356
5280     CONTINUE                                                         356
C    *                                                                    356
C    Geant POSITION : VPBB ===> Y=-(VPDG_FPHHGHT-VPDG_FPHTHICK)/2 Z=VPDG  357
C    _FPHTHICK/2 <===                                                     357
         CALL AGSRESET                                                    357
         AG_Y = -(VPDG_FPHHGHT-VPDG_FPHTHICK)/2                           357
         AG_Z = VPDG_FPHTHICK/2                                           357
         AG_TITLE = 'VPBB'                                                357
         AG_EXNAME = 'POSITION'                                           357
         AG_PARLIST = 'Y_Z'                                               357
         CALL AXPOSITION                                                  357
C    *                                                                    357
5051  CALL AGSPOPB                                                        358
      GO TO BLOCK_VPBO, (5050 ,5060)                                      358
C End of Local Procedure                                                  358
C ----------------------------------------------------------------------  359
C ------------ VPBA                                                       359
C Begin Local Procedure VPBA                                              360
C VPBA IS THE PART OF THE HOOK THAT MOUNTS TO THE FRONT/BACK PLATE        360
5250  CONTINUE                                                            360
         AG_TITLE='VPBA'                                                  360
         AG_MODULE=MODULE                                                 360
         AG_MARK='V00'                                                    360
         CALL AGSPUSH                                                     360
         AG_MODULE=MODULE                                                 360
C    *                                                                    360
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        361
         CALL AGSRESET                                                    361
         AG_TITLE = 'ALUMINIUM'                                           361
         AG_EXNAME = 'MATERIAL'                                           361
         AG_PARLIST = 'NONE'                                              361
         CALL AXMATERIAL                                                  361
C    *                                                                    361
C    Geant ATTRIBUTE : VPBA ===> SEEN=1 COLO=5 FILL=6 <===                361
         CALL AGSRESET                                                    362
         AG_SEEN = 1                                                      362
         AG_COLO = 5                                                      362
         AG_FILL = 6                                                      362
         AG_TITLE = 'VPBA'                                                362
         AG_EXNAME = 'ATTRIBUTE'                                          362
         AG_PARLIST = 'SEEN_COLO_FILL'                                    362
         CALL AXATTRIBUTE                                                 362
C    *                                                                    362
C    Geant SHAPE : BOX ===> DX=VPDG_FPHWIDTH/2 DY=VPDG_FPHHGHT/2, DZ=VPD  362
C    G_FPHTHICK/2 <===                                                    362
         CALL AGSRESET                                                    364
         AG_DX = VPDG_FPHWIDTH/2                                          364
         AG_DY = VPDG_FPHHGHT/2                                           364
         AG_DZ = VPDG_FPHTHICK/2                                          364
         AG_TITLE = 'BOX'                                                 364
         AG_EXNAME = 'SHAPE'                                              364
         AG_PARLIST = 'DX_DY_DZ'                                          364
         CALL AXSHAPE                                                     364
C    *                                                                    364
5251  CALL AGSPOPB                                                        365
      GO TO BLOCK_VPBA, (5250 ,5260)                                      365
C End of Local Procedure                                                  365
C ----------------------------------------------------------------------  366
C ------------ VPBB                                                       366
C Begin Local Procedure VPBB                                              367
C VPBB IS THE PART OF THE HOOK THAT MOUNTS TO THE BASE PLATE              367
5270  CONTINUE                                                            367
         AG_TITLE='VPBB'                                                  367
         AG_MODULE=MODULE                                                 367
         AG_MARK='V00'                                                    367
         CALL AGSPUSH                                                     367
         AG_MODULE=MODULE                                                 367
C    *                                                                    367
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        368
         CALL AGSRESET                                                    368
         AG_TITLE = 'ALUMINIUM'                                           368
         AG_EXNAME = 'MATERIAL'                                           368
         AG_PARLIST = 'NONE'                                              368
         CALL AXMATERIAL                                                  368
C    *                                                                    368
C    Geant ATTRIBUTE : VPBB ===> SEEN=1 COLO=5 FILL=6 <===                368
         CALL AGSRESET                                                    369
         AG_SEEN = 1                                                      369
         AG_COLO = 5                                                      369
         AG_FILL = 6                                                      369
         AG_TITLE = 'VPBB'                                                369
         AG_EXNAME = 'ATTRIBUTE'                                          369
         AG_PARLIST = 'SEEN_COLO_FILL'                                    369
         CALL AXATTRIBUTE                                                 369
C    *                                                                    369
C    Geant SHAPE : BOX ===> DX=VPDG_FPHWIDTH/2 DY=VPDG_FPHTHICK/2, DZ=(V  369
C    PDG_FPHHGHT-VPDG_FPHTHICK)/2 <===                                    369
         CALL AGSRESET                                                    371
         AG_DX = VPDG_FPHWIDTH/2                                          371
         AG_DY = VPDG_FPHTHICK/2                                          371
         AG_DZ = (VPDG_FPHHGHT-VPDG_FPHTHICK)/2                           371
         AG_TITLE = 'BOX'                                                 371
         AG_EXNAME = 'SHAPE'                                              371
         AG_PARLIST = 'DX_DY_DZ'                                          371
         CALL AXSHAPE                                                     371
C    *                                                                    371
5271  CALL AGSPOPB                                                        372
      GO TO BLOCK_VPBB, (5270 ,5280)                                      372
C End of Local Procedure                                                  372
C ----------------------------------------------------------------------  373
C ------------ VPFP                                                       373
C Begin Local Procedure VPFP                                              374
C VPFP IS A SINGLE RECTANGULAR PIECE OF THE FRONTPANEL                    374
5130  CONTINUE                                                            374
         AG_TITLE='VPFP'                                                  374
         AG_MODULE=MODULE                                                 374
         AG_MARK='V00'                                                    374
         CALL AGSPUSH                                                     374
         AG_MODULE=MODULE                                                 374
C    *                                                                    374
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       375
         CALL AGSRESET                                                    375
         AG_TITLE = 'POLYSTYREN'                                          375
         AG_EXNAME = 'MATERIAL'                                           375
         AG_PARLIST = 'NONE'                                              375
         CALL AXMATERIAL                                                  375
C    *                                                                    375
C    Geant ATTRIBUTE : VPFP ===> SEEN=1 COLO=3 FILL=6 <===                375
         CALL AGSRESET                                                    376
         AG_SEEN = 1                                                      376
         AG_COLO = 3                                                      376
         AG_FILL = 6                                                      376
         AG_TITLE = 'VPFP'                                                376
         AG_EXNAME = 'ATTRIBUTE'                                          376
         AG_PARLIST = 'SEEN_COLO_FILL'                                    376
         CALL AXATTRIBUTE                                                 376
C    *                                                                    376
C    Geant SHAPE : BOX ===> DX=VPDG_FPWIDTH/2 DY=VPDG_FPHEIGHT/2, DZ=VPD  376
C    G_FPTHICK/2 <===                                                     376
         CALL AGSRESET                                                    378
         AG_DX = VPDG_FPWIDTH/2                                           378
         AG_DY = VPDG_FPHEIGHT/2                                          378
         AG_DZ = VPDG_FPTHICK/2                                           378
         AG_TITLE = 'BOX'                                                 378
         AG_EXNAME = 'SHAPE'                                              378
         AG_PARLIST = 'DX_DY_DZ'                                          378
         CALL AXSHAPE                                                     378
C    *                                                                    378
5131  CALL AGSPOPB                                                        379
      GO TO BLOCK_VPFP, (5130 ,5140)                                      379
C End of Local Procedure                                                  379
C ----------------------------------------------------------------------  380
C ------------ VPFA                                                       380
C Begin Local Procedure VPFA                                              381
C VPFA IS THE CENTRAL UPPER PART OF THE FRONTPLATE                        381
5070  CONTINUE                                                            381
         AG_TITLE='VPFA'                                                  381
         AG_MODULE=MODULE                                                 381
         AG_MARK='V00'                                                    381
         CALL AGSPUSH                                                     381
         AG_MODULE=MODULE                                                 381
C    *                                                                    381
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       382
         CALL AGSRESET                                                    382
         AG_TITLE = 'POLYSTYREN'                                          382
         AG_EXNAME = 'MATERIAL'                                           382
         AG_PARLIST = 'NONE'                                              382
         CALL AXMATERIAL                                                  382
C    *                                                                    382
C    Geant ATTRIBUTE : VPFA ===> SEEN=1 COLO=3 FILL=6 <===                382
         CALL AGSRESET                                                    383
         AG_SEEN = 1                                                      383
         AG_COLO = 3                                                      383
         AG_FILL = 6                                                      383
         AG_TITLE = 'VPFA'                                                383
         AG_EXNAME = 'ATTRIBUTE'                                          383
         AG_PARLIST = 'SEEN_COLO_FILL'                                    383
         CALL AXATTRIBUTE                                                 383
C    *                                                                    383
C    Geant SHAPE : BOX ===> DX=VPDG_FPAWIDTH/2 DY=VPDG_FPAHGHT/2, DZ=VPD  383
C    G_FPTHICK/2 <===                                                     383
         CALL AGSRESET                                                    385
         AG_DX = VPDG_FPAWIDTH/2                                          385
         AG_DY = VPDG_FPAHGHT/2                                           385
         AG_DZ = VPDG_FPTHICK/2                                           385
         AG_TITLE = 'BOX'                                                 385
         AG_EXNAME = 'SHAPE'                                              385
         AG_PARLIST = 'DX_DY_DZ'                                          385
         CALL AXSHAPE                                                     385
C    *                                                                    385
5071  CALL AGSPOPB                                                        386
      GO TO BLOCK_VPFA, (5070 ,5080)                                      386
C End of Local Procedure                                                  386
C ----------------------------------------------------------------------  387
C ------------ VPFB                                                       387
C Begin Local Procedure VPFB                                              388
C VPFB IS THE MIDDLE UPPER PART OF THE FRONTPLATE                         388
5090  CONTINUE                                                            388
         AG_TITLE='VPFB'                                                  388
         AG_MODULE=MODULE                                                 388
         AG_MARK='V00'                                                    388
         CALL AGSPUSH                                                     388
         AG_MODULE=MODULE                                                 388
C    *                                                                    388
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       389
         CALL AGSRESET                                                    389
         AG_TITLE = 'POLYSTYREN'                                          389
         AG_EXNAME = 'MATERIAL'                                           389
         AG_PARLIST = 'NONE'                                              389
         CALL AXMATERIAL                                                  389
C    *                                                                    389
C    Geant ATTRIBUTE : VPFB ===> SEEN=1 COLO=3 FILL=6 <===                389
         CALL AGSRESET                                                    390
         AG_SEEN = 1                                                      390
         AG_COLO = 3                                                      390
         AG_FILL = 6                                                      390
         AG_TITLE = 'VPFB'                                                390
         AG_EXNAME = 'ATTRIBUTE'                                          390
         AG_PARLIST = 'SEEN_COLO_FILL'                                    390
         CALL AXATTRIBUTE                                                 390
C    *                                                                    390
C    Geant SHAPE : TRAP ===> DZ=VPDG_FPTHICK/2 THET=0. PHI=0., H1=(VPDG_  390
C    BPWIDTH-VPDG_FPAWIDTH-2*VPDG_FPWIDTH)/4, BL1=VPDG_FPCHGHT/2 TL1=VPD  390
C    G_FPAHGHT/2, ALP1=ATAN(VPDG_FPAHGHT/(VPDG_BPWIDTH-VPDG_FPAWIDTH-2*V  390
C    PDG_FPWIDTH)/2)*RADDEG, H2=(VPDG_BPWIDTH-VPDG_FPAWIDTH-2*VPDG_FPWID  390
C    TH)/4, BL2=VPDG_FPCHGHT/2, TL2=VPDG_FPAHGHT/2 ALP2=ATAN(VPDG_FPAHGH  390
C    T/(VPDG_BPWIDTH-VPDG_FPAWIDTH-2*VPDG_FPWIDTH)/2)*RADDEG <===         390
         CALL AGSRESET                                                    397
         AG_DZ = VPDG_FPTHICK/2                                           397
         AG_THET = 0.                                                     397
         AG_PHI = 0.                                                      397
         AG_H1 = (VPDG_BPWIDTH-VPDG_FPAWIDTH-2*VPDG_FPWIDTH)/4            397
         AG_BL1 = VPDG_FPCHGHT/2                                          397
         AG_TL1 = VPDG_FPAHGHT/2                                          397
         AG_ALP1 = ATAN(VPDG_FPAHGHT/                                     397
     *   (VPDG_BPWIDTH-VPDG_FPAWIDTH-2*VPDG_FPWIDTH)/2)*RADDEG            397
         AG_H2 = (VPDG_BPWIDTH-VPDG_FPAWIDTH-2*VPDG_FPWIDTH)/4            397
         AG_BL2 = VPDG_FPCHGHT/2                                          397
         AG_TL2 = VPDG_FPAHGHT/2                                          397
         AG_ALP2 = ATAN(VPDG_FPAHGHT/                                     397
     *   (VPDG_BPWIDTH-VPDG_FPAWIDTH-2*VPDG_FPWIDTH)/2)*RADDEG            397
         AG_TITLE = 'TRAP'                                                397
         AG_EXNAME = 'SHAPE'                                              397
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       397
         CALL AXSHAPE                                                     397
C    *                                                                    397
5091  CALL AGSPOPB                                                        398
      GO TO BLOCK_VPFB, (5090 ,5100)                                      398
C End of Local Procedure                                                  398
C ----------------------------------------------------------------------  399
C ------------ VPFC                                                       399
C Begin Local Procedure VPFC                                              400
C VPFC IS THE OUTER UPPER PART OF THE FRONTPLATE                          400
5110  CONTINUE                                                            400
         AG_TITLE='VPFC'                                                  400
         AG_MODULE=MODULE                                                 400
         AG_MARK='V00'                                                    400
         CALL AGSPUSH                                                     400
         AG_MODULE=MODULE                                                 400
C    *                                                                    400
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       401
         CALL AGSRESET                                                    401
         AG_TITLE = 'POLYSTYREN'                                          401
         AG_EXNAME = 'MATERIAL'                                           401
         AG_PARLIST = 'NONE'                                              401
         CALL AXMATERIAL                                                  401
C    *                                                                    401
C    Geant ATTRIBUTE : VPFC ===> SEEN=1 COLO=3 FILL=6 <===                401
         CALL AGSRESET                                                    402
         AG_SEEN = 1                                                      402
         AG_COLO = 3                                                      402
         AG_FILL = 6                                                      402
         AG_TITLE = 'VPFC'                                                402
         AG_EXNAME = 'ATTRIBUTE'                                          402
         AG_PARLIST = 'SEEN_COLO_FILL'                                    402
         CALL AXATTRIBUTE                                                 402
C    *                                                                    402
C    Geant SHAPE : TRAP ===> DZ=VPDG_FPTHICK/2 THET=0. PHI=0., H1=VPDG_F  402
C    PWIDTH/2 BL1=(VPDG_FPCHGHT-VPDG_FPWIDTH)/2, TL1=VPDG_FPCHGHT/2 ALP1  402
C    =ATAN(.5)*RADDEG, H2=VPDG_FPWIDTH/2 BL2=(VPDG_FPCHGHT-VPDG_FPWIDTH)  402
C    /2, TL2=VPDG_FPCHGHT/2 ALP2=ATAN(.5)*RADDEG <===                     402
         CALL AGSRESET                                                    407
         AG_DZ = VPDG_FPTHICK/2                                           407
         AG_THET = 0.                                                     407
         AG_PHI = 0.                                                      407
         AG_H1 = VPDG_FPWIDTH/2                                           407
         AG_BL1 = (VPDG_FPCHGHT-VPDG_FPWIDTH)/2                           407
         AG_TL1 = VPDG_FPCHGHT/2                                          407
         AG_ALP1 = ATAN(.5)*RADDEG                                        407
         AG_H2 = VPDG_FPWIDTH/2                                           407
         AG_BL2 = (VPDG_FPCHGHT-VPDG_FPWIDTH)/2                           407
         AG_TL2 = VPDG_FPCHGHT/2                                          407
         AG_ALP2 = ATAN(.5)*RADDEG                                        407
         AG_TITLE = 'TRAP'                                                407
         AG_EXNAME = 'SHAPE'                                              407
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       407
         CALL AXSHAPE                                                     407
C    *                                                                    407
5111  CALL AGSPOPB                                                        408
      GO TO BLOCK_VPFC, (5110 ,5120)                                      408
C End of Local Procedure                                                  408
C ----------------------------------------------------------------------  409
C ------------ VPST                                                       409
C Begin Local Procedure VPST                                              410
C VPST IS THE STRUT VOLUME                                                410
5150  CONTINUE                                                            410
         AG_TITLE='VPST'                                                  410
         AG_MODULE=MODULE                                                 410
         AG_MARK='V00'                                                    410
         CALL AGSPUSH                                                     410
         AG_MODULE=MODULE                                                 410
C    *                                                                    410
C    Geant ATTRIBUTE : VPST ===> SEEN=0 <===                              411
         CALL AGSRESET                                                    411
         AG_SEEN = 0                                                      411
         AG_TITLE = 'VPST'                                                411
         AG_EXNAME = 'ATTRIBUTE'                                          411
         AG_PARLIST = 'SEEN'                                              411
         CALL AXATTRIBUTE                                                 411
C    *                                                                    411
C    Geant SHAPE : BOX ===> DX=VPDG_STTHICK/2 DY=STRUTHEIGHT/2, DZ=(VPDG  411
C    _BPLENGTH+2*VPDG_SCLENGTH)/2 <===                                    411
         CALL AGSRESET                                                    413
         AG_DX = VPDG_STTHICK/2                                           413
         AG_DY = STRUTHEIGHT/2                                            413
         AG_DZ = (VPDG_BPLENGTH+2*VPDG_SCLENGTH)/2                        413
         AG_TITLE = 'BOX'                                                 413
         AG_EXNAME = 'SHAPE'                                              413
         AG_PARLIST = 'DX_DY_DZ'                                          413
         CALL AXSHAPE                                                     413
C    *                                                                    413
C    Create VPSV :                                                        414
C    Call To Local Procedure VPSV                                         414
         ASSIGN 5300 TO BLOCK_VPSV                                        414
         GO TO  5290                                                      414
5300     CONTINUE                                                         414
C    *                                                                    414
C    Geant POSITION : VPSV ===> inherited arguments used <===             415
         CALL AGSRESET                                                    415
         AG_TITLE = 'VPSV'                                                415
         AG_EXNAME = 'POSITION'                                           415
         AG_PARLIST = 'NONE'                                              415
         CALL AXPOSITION                                                  415
C    *                                                                    415
C    Create VPSW :                                                        416
C    Call To Local Procedure VPSW                                         416
         ASSIGN 5320 TO BLOCK_VPSW                                        416
         GO TO  5310                                                      416
5320     CONTINUE                                                         416
C    *                                                                    416
         YDISPL=(VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE*DEGRAD)) +             417
     *   VPDG_SCHEIGHT/2)/2                                               417
C    Geant POSITION : VPSW ===> Y=STRUTHEIGHT/2-YDISPL Z=-(VPDG_BPLENGTH  419
C    +VPDG_SCLENGTH)/2 ORT=YZX ALPHAZ=180. <===                           419
         CALL AGSRESET                                                    419
         AG_Y = STRUTHEIGHT/2-YDISPL                                      419
         AG_Z = -(VPDG_BPLENGTH+VPDG_SCLENGTH)/2                          419
         AG_ALPHAN=AG_ALPHAN+1                                            419
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 419
         AG_ALPHAN=AG_ALPHAN+1                                            419
         AG_ALPHAK(AG_ALPHAN)=3                                           419
         AG_ALPHAL(AG_ALPHAN)=180.                                        419
         AG_TITLE = 'VPSW'                                                419
         AG_EXNAME = 'POSITION'                                           419
         AG_PARLIST = 'Y_Z_ORT_ALPHAZ'                                    419
         CALL AXPOSITION                                                  419
C    *                                                                    419
C    Geant POSITION : VPSW ===> Y=-(STRUTHEIGHT/2-YDISPL) Z=(VPDG_BPLENG  419
C    TH+VPDG_SCLENGTH)/2 ORT=YZX ALPHAY=180. <===                         419
         CALL AGSRESET                                                    421
         AG_Y = -(STRUTHEIGHT/2-YDISPL)                                   421
         AG_Z = (VPDG_BPLENGTH+VPDG_SCLENGTH)/2                           421
         AG_ALPHAN=AG_ALPHAN+1                                            421
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 421
         AG_ALPHAN=AG_ALPHAN+1                                            421
         AG_ALPHAK(AG_ALPHAN)=2                                           421
         AG_ALPHAL(AG_ALPHAN)=180.                                        421
         AG_TITLE = 'VPSW'                                                421
         AG_EXNAME = 'POSITION'                                           421
         AG_PARLIST = 'Y_Z_ORT_ALPHAY'                                    421
         CALL AXPOSITION                                                  421
C    *                                                                    421
5151  CALL AGSPOPB                                                        422
      GO TO BLOCK_VPST, (5150 ,5160)                                      422
C End of Local Procedure                                                  422
C ----------------------------------------------------------------------  423
C ------------ VPSV                                                       423
C Begin Local Procedure VPSV                                              424
C VPSV IS THE ACTUAL STRUT BETWEEN FRONT AND BACKPLATES                   424
5290  CONTINUE                                                            424
         AG_TITLE='VPSV'                                                  424
         AG_MODULE=MODULE                                                 424
         AG_MARK='V00'                                                    424
         CALL AGSPUSH                                                     424
         AG_MODULE=MODULE                                                 424
C    *                                                                    424
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        425
         CALL AGSRESET                                                    425
         AG_TITLE = 'ALUMINIUM'                                           425
         AG_EXNAME = 'MATERIAL'                                           425
         AG_PARLIST = 'NONE'                                              425
         CALL AXMATERIAL                                                  425
C    *                                                                    425
C    Geant ATTRIBUTE : VPSV ===> SEEN=1 COLO=3 FILL=6 <===                425
         CALL AGSRESET                                                    426
         AG_SEEN = 1                                                      426
         AG_COLO = 3                                                      426
         AG_FILL = 6                                                      426
         AG_TITLE = 'VPSV'                                                426
         AG_EXNAME = 'ATTRIBUTE'                                          426
         AG_PARLIST = 'SEEN_COLO_FILL'                                    426
         CALL AXATTRIBUTE                                                 426
C    *                                                                    426
C    Geant SHAPE : TRAP ===> DZ=VPDG_BPLENGTH/2 THET=VPDG_STANGLE, PHI=2  426
C    70. H1=VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE*DEGRAD)), BL1=VPDG_STTHICK  426
C    /2 TL1=VPDG_STTHICK/2, ALP1=0. H2=VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE  426
C    *DEGRAD)), BL2=VPDG_STTHICK/2 TL2=VPDG_STTHICK/2, ALP2=0. <===       426
         CALL AGSRESET                                                    432
         AG_DZ = VPDG_BPLENGTH/2                                          432
         AG_THET = VPDG_STANGLE                                           432
         AG_PHI = 270.                                                    432
         AG_H1 = VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE*DEGRAD))               432
         AG_BL1 = VPDG_STTHICK/2                                          432
         AG_TL1 = VPDG_STTHICK/2                                          432
         AG_ALP1 = 0.                                                     432
         AG_H2 = VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE*DEGRAD))               432
         AG_BL2 = VPDG_STTHICK/2                                          432
         AG_TL2 = VPDG_STTHICK/2                                          432
         AG_ALP2 = 0.                                                     432
         AG_TITLE = 'TRAP'                                                432
         AG_EXNAME = 'SHAPE'                                              432
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       432
         CALL AXSHAPE                                                     432
C    *                                                                    432
5291  CALL AGSPOPB                                                        433
      GO TO BLOCK_VPSV, (5290 ,5300)                                      433
C End of Local Procedure                                                  433
C ----------------------------------------------------------------------  434
C ------------ VPSW                                                       434
C Begin Local Procedure VPSW                                              435
C VPSW IS A TINY PIECE OF ALUMINIUM THAT BELONGS TO THE STRUT             435
5310  CONTINUE                                                            435
         AG_TITLE='VPSW'                                                  435
         AG_MODULE=MODULE                                                 435
         AG_MARK='V00'                                                    435
         CALL AGSPUSH                                                     435
         AG_MODULE=MODULE                                                 435
C    *                                                                    435
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        436
         CALL AGSRESET                                                    436
         AG_TITLE = 'ALUMINIUM'                                           436
         AG_EXNAME = 'MATERIAL'                                           436
         AG_PARLIST = 'NONE'                                              436
         CALL AXMATERIAL                                                  436
C    *                                                                    436
C    Geant ATTRIBUTE : VPSW ===> SEEN=1 COLO=3 FILL=6 <===                436
         CALL AGSRESET                                                    437
         AG_SEEN = 1                                                      437
         AG_COLO = 3                                                      437
         AG_FILL = 6                                                      437
         AG_TITLE = 'VPSW'                                                437
         AG_EXNAME = 'ATTRIBUTE'                                          437
         AG_PARLIST = 'SEEN_COLO_FILL'                                    437
         CALL AXATTRIBUTE                                                 437
C    *                                                                    437
C    Geant SHAPE : TRAP ===> DZ=VPDG_STTHICK/2 THET=0. PHI=0., H1=VPDG_S  437
C    CLENGTH/2 BL1=VPDG_SCHEIGHT/2 TL1=VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE  437
C    *DEGRAD)), ALP1=ATAN((VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE*DEGRAD))-VP  437
C    DG_SCHEIGHT/2)/VPDG_SCLENGTH)*RADDEG, H2=VPDG_SCLENGTH/2 BL2=VPDG_S  437
C    CHEIGHT/2 TL2=VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE*DEGRAD)), ALP2=ATAN  437
C    ((VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE*DEGRAD))-VPDG_SCHEIGHT/2)/VPDG_  437
C    SCLENGTH)*RADDEG <===                                                437
         CALL AGSRESET                                                    442
         AG_DZ = VPDG_STTHICK/2                                           442
         AG_THET = 0.                                                     442
         AG_PHI = 0.                                                      442
         AG_H1 = VPDG_SCLENGTH/2                                          442
         AG_BL1 = VPDG_SCHEIGHT/2                                         442
         AG_TL1 = VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE*DEGRAD))              442
         AG_ALP1 = ATAN((VPDG_STDIAGSZ/                                   442
     *   (2*COS(VPDG_STANGLE*DEGRAD))-VPDG_SCHEIGHT/2)/                   442
     *   VPDG_SCLENGTH)*RADDEG                                            442
         AG_H2 = VPDG_SCLENGTH/2                                          442
         AG_BL2 = VPDG_SCHEIGHT/2                                         442
         AG_TL2 = VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE*DEGRAD))              442
         AG_ALP2 = ATAN((VPDG_STDIAGSZ/                                   442
     *   (2*COS(VPDG_STANGLE*DEGRAD))-VPDG_SCHEIGHT/2)/                   442
     *   VPDG_SCLENGTH)*RADDEG                                            442
         AG_TITLE = 'TRAP'                                                442
         AG_EXNAME = 'SHAPE'                                              442
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       442
         CALL AXSHAPE                                                     442
C    *                                                                    442
5311  CALL AGSPOPB                                                        443
      GO TO BLOCK_VPSW, (5310 ,5320)                                      443
C End of Local Procedure                                                  443
C ----------------------------------------------------------------------  444
C ------------ VPSC                                                       444
C Begin Local Procedure VPSC                                              445
C VPSC IS A CLAMP THAT HOLDS THE STRUT                                    445
5170  CONTINUE                                                            445
         AG_TITLE='VPSC'                                                  445
         AG_MODULE=MODULE                                                 445
         AG_MARK='V00'                                                    445
         CALL AGSPUSH                                                     445
         AG_MODULE=MODULE                                                 445
C    *                                                                    445
C    Geant SHAPE : BOX ===> DX=VPDG_SCWIDTH/2 DY=VPDG_SCHEIGHT/2, DZ=VPD  447
C    G_SCLENGTH/2 <===                                                    447
         CALL AGSRESET                                                    447
         AG_DX = VPDG_SCWIDTH/2                                           447
         AG_DY = VPDG_SCHEIGHT/2                                          447
         AG_DZ = VPDG_SCLENGTH/2                                          447
         AG_TITLE = 'BOX'                                                 447
         AG_EXNAME = 'SHAPE'                                              447
         AG_PARLIST = 'DX_DY_DZ'                                          447
         CALL AXSHAPE                                                     447
C    *                                                                    447
C    Create VPSA :                                                        448
C    Call To Local Procedure VPSA                                         448
         ASSIGN 5340 TO BLOCK_VPSA                                        448
         GO TO  5330                                                      448
5340     CONTINUE                                                         448
C    *                                                                    448
C    Geant POSITION : VPSA ===> X=-VPDG_SCTHICK/2 Z=(VPDG_SCLENGTH-VPDG_  449
C    SCTHICK)/2 <===                                                      449
         CALL AGSRESET                                                    449
         AG_X = -VPDG_SCTHICK/2                                           449
         AG_Z = (VPDG_SCLENGTH-VPDG_SCTHICK)/2                            449
         AG_TITLE = 'VPSA'                                                449
         AG_EXNAME = 'POSITION'                                           449
         AG_PARLIST = 'X_Z'                                               449
         CALL AXPOSITION                                                  449
C    *                                                                    449
C    Create VPSB :                                                        450
C    Call To Local Procedure VPSB                                         450
         ASSIGN 5360 TO BLOCK_VPSB                                        450
         GO TO  5350                                                      450
5360     CONTINUE                                                         450
C    *                                                                    450
C    Geant POSITION : VPSB ===> X=(VPDG_SCWIDTH-VPDG_SCTHICK)/2 <===      451
         CALL AGSRESET                                                    451
         AG_X = (VPDG_SCWIDTH-VPDG_SCTHICK)/2                             451
         AG_TITLE = 'VPSB'                                                451
         AG_EXNAME = 'POSITION'                                           451
         AG_PARLIST = 'X'                                                 451
         CALL AXPOSITION                                                  451
C    *                                                                    451
5171  CALL AGSPOPB                                                        452
      GO TO BLOCK_VPSC, (5170 ,5180)                                      452
C End of Local Procedure                                                  452
C ----------------------------------------------------------------------  453
C ------------ VPSA                                                       453
C Begin Local Procedure VPSA                                              454
C VPSA IS PART OF A STRUT CLAMP THAT HOLDS TO THE FRONTPLATE              454
5330  CONTINUE                                                            454
         AG_TITLE='VPSA'                                                  454
         AG_MODULE=MODULE                                                 454
         AG_MARK='V00'                                                    454
         CALL AGSPUSH                                                     454
         AG_MODULE=MODULE                                                 454
C    *                                                                    454
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        455
         CALL AGSRESET                                                    455
         AG_TITLE = 'ALUMINIUM'                                           455
         AG_EXNAME = 'MATERIAL'                                           455
         AG_PARLIST = 'NONE'                                              455
         CALL AXMATERIAL                                                  455
C    *                                                                    455
C    Geant ATTRIBUTE : VPSA ===> SEEN=1 COLO=3 FILL=6 <===                455
         CALL AGSRESET                                                    456
         AG_SEEN = 1                                                      456
         AG_COLO = 3                                                      456
         AG_FILL = 6                                                      456
         AG_TITLE = 'VPSA'                                                456
         AG_EXNAME = 'ATTRIBUTE'                                          456
         AG_PARLIST = 'SEEN_COLO_FILL'                                    456
         CALL AXATTRIBUTE                                                 456
C    *                                                                    456
C    Geant SHAPE : BOX ===> DX=(VPDG_SCWIDTH-VPDG_SCTHICK)/2, DY=VPDG_SC  456
C    HEIGHT/2 DZ=VPDG_SCTHICK/2 <===                                      456
         CALL AGSRESET                                                    458
         AG_DX = (VPDG_SCWIDTH-VPDG_SCTHICK)/2                            458
         AG_DY = VPDG_SCHEIGHT/2                                          458
         AG_DZ = VPDG_SCTHICK/2                                           458
         AG_TITLE = 'BOX'                                                 458
         AG_EXNAME = 'SHAPE'                                              458
         AG_PARLIST = 'DX_DY_DZ'                                          458
         CALL AXSHAPE                                                     458
C    *                                                                    458
5331  CALL AGSPOPB                                                        459
      GO TO BLOCK_VPSA, (5330 ,5340)                                      459
C End of Local Procedure                                                  459
C ----------------------------------------------------------------------  460
C ------------ VPSB                                                       460
C Begin Local Procedure VPSB                                              461
C VPSB IS PART OF A STRUT CLAMP THAT HOLDS TO THE STRUT                   461
5350  CONTINUE                                                            461
         AG_TITLE='VPSB'                                                  461
         AG_MODULE=MODULE                                                 461
         AG_MARK='V00'                                                    461
         CALL AGSPUSH                                                     461
         AG_MODULE=MODULE                                                 461
C    *                                                                    461
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        462
         CALL AGSRESET                                                    462
         AG_TITLE = 'ALUMINIUM'                                           462
         AG_EXNAME = 'MATERIAL'                                           462
         AG_PARLIST = 'NONE'                                              462
         CALL AXMATERIAL                                                  462
C    *                                                                    462
C    Geant ATTRIBUTE : VPSB ===> SEEN=1 COLO=3 FILL=6 <===                462
         CALL AGSRESET                                                    463
         AG_SEEN = 1                                                      463
         AG_COLO = 3                                                      463
         AG_FILL = 6                                                      463
         AG_TITLE = 'VPSB'                                                463
         AG_EXNAME = 'ATTRIBUTE'                                          463
         AG_PARLIST = 'SEEN_COLO_FILL'                                    463
         CALL AXATTRIBUTE                                                 463
C    *                                                                    463
C    Geant SHAPE : BOX ===> DX=VPDG_SCTHICK/2 DY=VPDG_SCHEIGHT/2, DZ=VPD  463
C    G_SCLENGTH/2 <===                                                    463
         CALL AGSRESET                                                    465
         AG_DX = VPDG_SCTHICK/2                                           465
         AG_DY = VPDG_SCHEIGHT/2                                          465
         AG_DZ = VPDG_SCLENGTH/2                                          465
         AG_TITLE = 'BOX'                                                 465
         AG_EXNAME = 'SHAPE'                                              465
         AG_PARLIST = 'DX_DY_DZ'                                          465
         CALL AXSHAPE                                                     465
C    *                                                                    465
5351  CALL AGSPOPB                                                        466
      GO TO BLOCK_VPSB, (5350 ,5360)                                      466
C End of Local Procedure                                                  466
C ----------------------------------------------------------------------  467
C ------------ VPBX                                                       467
C Begin Local Procedure VPBX                                              468
C VPBX IS THE FEE BOX                                                     468
5190  CONTINUE                                                            468
         AG_TITLE='VPBX'                                                  468
         AG_MODULE=MODULE                                                 468
         AG_MARK='V00'                                                    468
         CALL AGSPUSH                                                     468
         AG_MODULE=MODULE                                                 468
C    *                                                                    468
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        469
         CALL AGSRESET                                                    469
         AG_TITLE = 'ALUMINIUM'                                           469
         AG_EXNAME = 'MATERIAL'                                           469
         AG_PARLIST = 'NONE'                                              469
         CALL AXMATERIAL                                                  469
C    *                                                                    469
C    Geant ATTRIBUTE : VPBX ===> SEEN=1 COLO=2 <===                       469
         CALL AGSRESET                                                    470
         AG_SEEN = 1                                                      470
         AG_COLO = 2                                                      470
         AG_TITLE = 'VPBX'                                                470
         AG_EXNAME = 'ATTRIBUTE'                                          470
         AG_PARLIST = 'SEEN_COLO'                                         470
         CALL AXATTRIBUTE                                                 470
C    *                                                                    470
C    Geant SHAPE : BOX ===> DX=VPDG_BXWIDTH/2 DY=VPDG_BXHEIGHT/2, DZ=VPD  470
C    G_BXLENGTH/2 <===                                                    470
         CALL AGSRESET                                                    472
         AG_DX = VPDG_BXWIDTH/2                                           472
         AG_DY = VPDG_BXHEIGHT/2                                          472
         AG_DZ = VPDG_BXLENGTH/2                                          472
         AG_TITLE = 'BOX'                                                 472
         AG_EXNAME = 'SHAPE'                                              472
         AG_PARLIST = 'DX_DY_DZ'                                          472
         CALL AXSHAPE                                                     472
C    *                                                                    472
C    Create VPBI :                                                        473
C    Call To Local Procedure VPBI                                         473
         ASSIGN 5380 TO BLOCK_VPBI                                        473
         GO TO  5370                                                      473
5380     CONTINUE                                                         473
C    *                                                                    473
C    Geant POSITION : VPBI ===> inherited arguments used <===             474
         CALL AGSRESET                                                    474
         AG_TITLE = 'VPBI'                                                474
         AG_EXNAME = 'POSITION'                                           474
         AG_PARLIST = 'NONE'                                              474
         CALL AXPOSITION                                                  474
C    *                                                                    474
5191  CALL AGSPOPB                                                        475
      GO TO BLOCK_VPBX, (5190 ,5200)                                      475
C End of Local Procedure                                                  475
C ----------------------------------------------------------------------  476
C ------------ VPBI                                                       476
C Begin Local Procedure VPBI                                              477
C VPBI IS THE EMPTY SPACE INSIDE OF THE FEE BOX                           477
5370  CONTINUE                                                            477
         AG_TITLE='VPBI'                                                  477
         AG_MODULE=MODULE                                                 477
         AG_MARK='V00'                                                    477
         CALL AGSPUSH                                                     477
         AG_MODULE=MODULE                                                 477
C    *                                                                    477
C    Geant MATERIAL : AIR ===> inherited arguments used <===              478
         CALL AGSRESET                                                    478
         AG_TITLE = 'AIR'                                                 478
         AG_EXNAME = 'MATERIAL'                                           478
         AG_PARLIST = 'NONE'                                              478
         CALL AXMATERIAL                                                  478
C    *                                                                    478
C    Geant ATTRIBUTE : VPBI ===> SEEN=0 <===                              478
         CALL AGSRESET                                                    479
         AG_SEEN = 0                                                      479
         AG_TITLE = 'VPBI'                                                479
         AG_EXNAME = 'ATTRIBUTE'                                          479
         AG_PARLIST = 'SEEN'                                              479
         CALL AXATTRIBUTE                                                 479
C    *                                                                    479
C    Geant SHAPE : BOX ===> DX=(VPDG_BXWIDTH-VPDG_BXTHICK)/2, DY=(VPDG_B  479
C    XHEIGHT-VPDG_BXTHICK)/2 DZ=(VPDG_BXLENGTH-VPDG_BXTHICK)/2 <===       479
         CALL AGSRESET                                                    481
         AG_DX = (VPDG_BXWIDTH-VPDG_BXTHICK)/2                            481
         AG_DY = (VPDG_BXHEIGHT-VPDG_BXTHICK)/2                           481
         AG_DZ = (VPDG_BXLENGTH-VPDG_BXTHICK)/2                           481
         AG_TITLE = 'BOX'                                                 481
         AG_EXNAME = 'SHAPE'                                              481
         AG_PARLIST = 'DX_DY_DZ'                                          481
         CALL AXSHAPE                                                     481
C    *                                                                    481
         ELECWID=20.3                                                     481
         ELECTHCK=0.17                                                    483
         ELECLEN=5.1                                                      484
C    Create VFEE :                                                        485
C    Call To Local Procedure VFEE                                         485
         ASSIGN 5400 TO BLOCK_VFEE                                        485
         GO TO  5390                                                      485
5400     CONTINUE                                                         485
C    *                                                                    485
C    Geant POSITION : VFEE ===> X=-(VPDG_BXWIDTH-VPDG_BXTHICK-ELECWID)/2  488
C    +2.54 Y=(VPDG_BXHEIGHT-VPDG_BXTHICK)/2-1.77 Z=-(VPDG_BXLENGTH-VPDG_  488
C    BXTHICK-ELECLEN)/2 <===                                              488
         CALL AGSRESET                                                    488
         AG_X = -(VPDG_BXWIDTH-VPDG_BXTHICK-ELECWID)/2+2.54               488
         AG_Y = (VPDG_BXHEIGHT-VPDG_BXTHICK)/2-1.77                       488
         AG_Z = -(VPDG_BXLENGTH-VPDG_BXTHICK-ELECLEN)/2                   488
         AG_TITLE = 'VFEE'                                                488
         AG_EXNAME = 'POSITION'                                           488
         AG_PARLIST = 'X_Y_Z'                                             488
         CALL AXPOSITION                                                  488
C    *                                                                    488
5371  CALL AGSPOPB                                                        489
      GO TO BLOCK_VPBI, (5370 ,5380)                                      489
C End of Local Procedure                                                  489
C ----------------------------------------------------------------------  490
C ------------ VFEE                                                       490
C Begin Local Procedure VFEE                                              491
C VFEE IS THE FEE INSIDE THE BOX                                          491
5390  CONTINUE                                                            491
         AG_TITLE='VFEE'                                                  491
         AG_MODULE=MODULE                                                 491
         AG_MARK='V00'                                                    491
         CALL AGSPUSH                                                     491
         AG_MODULE=MODULE                                                 491
C    *                                                                    491
C    Geant ATTRIBUTE : VFEE ===> SEEN=1 COLO=3 <===                       492
         CALL AGSRESET                                                    492
         AG_SEEN = 1                                                      492
         AG_COLO = 3                                                      492
         AG_TITLE = 'VFEE'                                                492
         AG_EXNAME = 'ATTRIBUTE'                                          492
         AG_PARLIST = 'SEEN_COLO'                                         492
         CALL AXATTRIBUTE                                                 492
C    *                                                                    492
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===          492
         CALL AGSRESET                                                    493
         AG_A = 28.08                                                     493
         AG_Z = 14                                                        493
         AG_W = 0.6*1*28./60.                                             493
         AG_TITLE = 'SI'                                                  493
         AG_EXNAME = 'COMPONENT'                                          493
         AG_PARLIST = 'A_Z_W'                                             493
         CALL AXCOMPONENT                                                 493
C    *                                                                    493
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174.   493
C    <===                                                                 493
         CALL AGSRESET                                                    494
         AG_A = 16                                                        494
         AG_Z = 8                                                         494
         AG_W = 0.6*2*16./60. + 0.4*4*16./174.                            494
         AG_TITLE = 'O'                                                   494
         AG_EXNAME = 'COMPONENT'                                          494
         AG_PARLIST = 'A_Z_W'                                             494
         CALL AXCOMPONENT                                                 494
C    *                                                                    494
C    Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===              494
         CALL AGSRESET                                                    495
         AG_A = 12                                                        495
         AG_Z = 6                                                         495
         AG_W = 0.4*8*12./174.                                            495
         AG_TITLE = 'C'                                                   495
         AG_EXNAME = 'COMPONENT'                                          495
         AG_PARLIST = 'A_Z_W'                                             495
         CALL AXCOMPONENT                                                 495
C    *                                                                    495
C    Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===               495
         CALL AGSRESET                                                    496
         AG_A = 1                                                         496
         AG_Z = 1                                                         496
         AG_W = 0.4*14*1./174.                                            496
         AG_TITLE = 'H'                                                   496
         AG_EXNAME = 'COMPONENT'                                          496
         AG_PARLIST = 'A_Z_W'                                             496
         CALL AXCOMPONENT                                                 496
C    *                                                                    496
C    Geant MIXTURE : G10 ===> DENS=1.7 <===                               496
         CALL AGSRESET                                                    497
         AG_DENS = 1.7                                                    497
         AG_TITLE = 'G10'                                                 497
         AG_EXNAME = 'MIXTURE'                                            497
         AG_PARLIST = 'DENS'                                              497
         CALL AXMIXTURE                                                   497
C    *                                                                    497
C    Geant SHAPE : BOX ===> DX=ELECWID/2 DY=ELECTHCK/2 DZ=ELECLEN/2 <===  497
         CALL AGSRESET                                                    498
         AG_DX = ELECWID/2                                                498
         AG_DY = ELECTHCK/2                                               498
         AG_DZ = ELECLEN/2                                                498
         AG_TITLE = 'BOX'                                                 498
         AG_EXNAME = 'SHAPE'                                              498
         AG_PARLIST = 'DX_DY_DZ'                                          498
         CALL AXSHAPE                                                     498
C    *                                                                    498
C    Create VLEM :                                                        499
C    Call To Local Procedure VLEM                                         499
         ASSIGN 5420 TO BLOCK_VLEM                                        499
         GO TO  5410                                                      499
5420     CONTINUE                                                         499
C    *                                                                    499
         ELECTHCK= 0.17                                                   500
C    Geant POSITION : VLEM ===> X=-7.0 Y=ELECTHCK+(0.7/2) Z=2 <===        501
         CALL AGSRESET                                                    501
         AG_X = -7.0                                                      501
         AG_Y = ELECTHCK+(0.7/2)                                          501
         AG_Z = 2                                                         501
         AG_TITLE = 'VLEM'                                                501
         AG_EXNAME = 'POSITION'                                           501
         AG_PARLIST = 'X_Y_Z'                                             501
         CALL AXPOSITION                                                  501
C    *                                                                    501
C    Geant POSITION : VLEM ===> X=-3.5 Y=ELECTHCK+(0.7/2) Z=2 <===        501
         CALL AGSRESET                                                    502
         AG_X = -3.5                                                      502
         AG_Y = ELECTHCK+(0.7/2)                                          502
         AG_Z = 2                                                         502
         AG_TITLE = 'VLEM'                                                502
         AG_EXNAME = 'POSITION'                                           502
         AG_PARLIST = 'X_Y_Z'                                             502
         CALL AXPOSITION                                                  502
C    *                                                                    502
C    Geant POSITION : VLEM ===> X=0. Y=ELECTHCK+(0.7/2) Z=2 <===          502
         CALL AGSRESET                                                    503
         AG_X = 0.                                                        503
         AG_Y = ELECTHCK+(0.7/2)                                          503
         AG_Z = 2                                                         503
         AG_TITLE = 'VLEM'                                                503
         AG_EXNAME = 'POSITION'                                           503
         AG_PARLIST = 'X_Y_Z'                                             503
         CALL AXPOSITION                                                  503
C    *                                                                    503
C    Geant POSITION : VLEM ===> X=3.5 Y=ELECTHCK+(0.7/2) Z=2 <===         503
         CALL AGSRESET                                                    504
         AG_X = 3.5                                                       504
         AG_Y = ELECTHCK+(0.7/2)                                          504
         AG_Z = 2                                                         504
         AG_TITLE = 'VLEM'                                                504
         AG_EXNAME = 'POSITION'                                           504
         AG_PARLIST = 'X_Y_Z'                                             504
         CALL AXPOSITION                                                  504
C    *                                                                    504
C    Geant POSITION : VLEM ===> X=-7.0 Y=ELECTHCK+(0.7/2) Z=-2 ALPHAX=18  504
C    0 <===                                                               504
         CALL AGSRESET                                                    505
         AG_X = -7.0                                                      505
         AG_Y = ELECTHCK+(0.7/2)                                          505
         AG_Z = -2                                                        505
         AG_ALPHAN=AG_ALPHAN+1                                            505
         AG_ALPHAK(AG_ALPHAN)=1                                           505
         AG_ALPHAL(AG_ALPHAN)=180                                         505
         AG_TITLE = 'VLEM'                                                505
         AG_EXNAME = 'POSITION'                                           505
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      505
         CALL AXPOSITION                                                  505
C    *                                                                    505
C    Geant POSITION : VLEM ===> X=-3.5 Y=ELECTHCK+(0.7/2) Z=-2 ALPHAX=18  505
C    0 <===                                                               505
         CALL AGSRESET                                                    506
         AG_X = -3.5                                                      506
         AG_Y = ELECTHCK+(0.7/2)                                          506
         AG_Z = -2                                                        506
         AG_ALPHAN=AG_ALPHAN+1                                            506
         AG_ALPHAK(AG_ALPHAN)=1                                           506
         AG_ALPHAL(AG_ALPHAN)=180                                         506
         AG_TITLE = 'VLEM'                                                506
         AG_EXNAME = 'POSITION'                                           506
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      506
         CALL AXPOSITION                                                  506
C    *                                                                    506
C    Geant POSITION : VLEM ===> X=0. Y=ELECTHCK+(0.7/2) Z=-2 ALPHAX=180   506
C    <===                                                                 506
         CALL AGSRESET                                                    507
         AG_X = 0.                                                        507
         AG_Y = ELECTHCK+(0.7/2)                                          507
         AG_Z = -2                                                        507
         AG_ALPHAN=AG_ALPHAN+1                                            507
         AG_ALPHAK(AG_ALPHAN)=1                                           507
         AG_ALPHAL(AG_ALPHAN)=180                                         507
         AG_TITLE = 'VLEM'                                                507
         AG_EXNAME = 'POSITION'                                           507
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      507
         CALL AXPOSITION                                                  507
C    *                                                                    507
C    Geant POSITION : VLEM ===> X=3.5 Y=ELECTHCK+(0.7/2) Z=-2 ALPHAX=180  507
C     <===                                                                507
         CALL AGSRESET                                                    508
         AG_X = 3.5                                                       508
         AG_Y = ELECTHCK+(0.7/2)                                          508
         AG_Z = -2                                                        508
         AG_ALPHAN=AG_ALPHAN+1                                            508
         AG_ALPHAK(AG_ALPHAN)=1                                           508
         AG_ALPHAL(AG_ALPHAN)=180                                         508
         AG_TITLE = 'VLEM'                                                508
         AG_EXNAME = 'POSITION'                                           508
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      508
         CALL AXPOSITION                                                  508
C    *                                                                    508
C    Geant POSITION : VLEM ===> X=-6.0 Y=ELECTHCK+(0.7/2) Z=-2 ALPHAX=18  508
C    0 <===                                                               508
         CALL AGSRESET                                                    509
         AG_X = -6.0                                                      509
         AG_Y = ELECTHCK+(0.7/2)                                          509
         AG_Z = -2                                                        509
         AG_ALPHAN=AG_ALPHAN+1                                            509
         AG_ALPHAK(AG_ALPHAN)=1                                           509
         AG_ALPHAL(AG_ALPHAN)=180                                         509
         AG_TITLE = 'VLEM'                                                509
         AG_EXNAME = 'POSITION'                                           509
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      509
         CALL AXPOSITION                                                  509
C    *                                                                    509
C    Geant POSITION : VLEM ===> X=-2.5 Y=ELECTHCK+(0.7/2) Z=-2 ALPHAX=18  509
C    0 <===                                                               509
         CALL AGSRESET                                                    510
         AG_X = -2.5                                                      510
         AG_Y = ELECTHCK+(0.7/2)                                          510
         AG_Z = -2                                                        510
         AG_ALPHAN=AG_ALPHAN+1                                            510
         AG_ALPHAK(AG_ALPHAN)=1                                           510
         AG_ALPHAL(AG_ALPHAN)=180                                         510
         AG_TITLE = 'VLEM'                                                510
         AG_EXNAME = 'POSITION'                                           510
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      510
         CALL AXPOSITION                                                  510
C    *                                                                    510
C    Geant POSITION : VLEM ===> X=1. Y=ELECTHCK+(0.7/2) Z=-2 ALPHAX=180   510
C    <===                                                                 510
         CALL AGSRESET                                                    511
         AG_X = 1.                                                        511
         AG_Y = ELECTHCK+(0.7/2)                                          511
         AG_Z = -2                                                        511
         AG_ALPHAN=AG_ALPHAN+1                                            511
         AG_ALPHAK(AG_ALPHAN)=1                                           511
         AG_ALPHAL(AG_ALPHAN)=180                                         511
         AG_TITLE = 'VLEM'                                                511
         AG_EXNAME = 'POSITION'                                           511
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      511
         CALL AXPOSITION                                                  511
C    *                                                                    511
C    Geant POSITION : VLEM ===> X=4.5 Y=ELECTHCK+(0.7/2) Z=-2 ALPHAX=180  511
C     <===                                                                511
         CALL AGSRESET                                                    512
         AG_X = 4.5                                                       512
         AG_Y = ELECTHCK+(0.7/2)                                          512
         AG_Z = -2                                                        512
         AG_ALPHAN=AG_ALPHAN+1                                            512
         AG_ALPHAK(AG_ALPHAN)=1                                           512
         AG_ALPHAL(AG_ALPHAN)=180                                         512
         AG_TITLE = 'VLEM'                                                512
         AG_EXNAME = 'POSITION'                                           512
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      512
         CALL AXPOSITION                                                  512
C    *                                                                    512
5391  CALL AGSPOPB                                                        513
      GO TO BLOCK_VFEE, (5390 ,5400)                                      513
C End of Local Procedure                                                  513
C ----------------------------------------------------------------------  514
C ------------ VLEM                                                       514
C Begin Local Procedure VLEM                                              515
C VLEM IS A LEMO CONNECTOR ON THE FEE BOARDS                              515
5410  CONTINUE                                                            515
         AG_TITLE='VLEM'                                                  515
         AG_MODULE=MODULE                                                 515
         AG_MARK='V00'                                                    515
         CALL AGSPUSH                                                     515
         AG_MODULE=MODULE                                                 515
C    *                                                                    515
C    Geant ATTRIBUTE : VLEM ===> SEEN=1 COLO=3 <===                       516
         CALL AGSRESET                                                    516
         AG_SEEN = 1                                                      516
         AG_COLO = 3                                                      516
         AG_TITLE = 'VLEM'                                                516
         AG_EXNAME = 'ATTRIBUTE'                                          516
         AG_PARLIST = 'SEEN_COLO'                                         516
         CALL AXATTRIBUTE                                                 516
C    *                                                                    516
C    Geant SHAPE : BOX ===> DX=(0.68/2+(0.9-0.72)/2) DY=(0.68/2), DZ=(2.  516
C    0/2+(0.8+1.0)/2) <===                                                516
         CALL AGSRESET                                                    518
         AG_DX = (0.68/2+(0.9-0.72)/2)                                    518
         AG_DY = (0.68/2)                                                 518
         AG_DZ = (2.0/2+(0.8+1.0)/2)                                      518
         AG_TITLE = 'BOX'                                                 518
         AG_EXNAME = 'SHAPE'                                              518
         AG_PARLIST = 'DX_DY_DZ'                                          518
         CALL AXSHAPE                                                     518
C    *                                                                    518
C    Create VPIP :                                                        519
C    Call To Local Procedure VPIP                                         519
         ASSIGN 5440 TO BLOCK_VPIP                                        519
         GO TO  5430                                                      519
5440     CONTINUE                                                         519
C    *                                                                    519
C    Geant POSITION : VPIP ===> X=(0.9-0.72)/2 Y=0 Z=(0.8+1.0)/2 RMIN=0.  520
C    62/2 RMAX=0.68/2 DZ=2.0/2 <===                                       520
         CALL AGSRESET                                                    520
         AG_X = (0.9-0.72)/2                                              520
         AG_Y = 0                                                         520
         AG_Z = (0.8+1.0)/2                                               520
         AG_RMIN = 0.62/2                                                 520
         AG_RMAX = 0.68/2                                                 520
         AG_DZ = 2.0/2                                                    520
         AG_TITLE = 'VPIP'                                                520
         AG_EXNAME = 'POSITION'                                           520
         AG_PARLIST = 'X_Y_Z_RMIN_RMAX_DZ'                                520
         CALL AXPOSITION                                                  520
C    *                                                                    520
5411  CALL AGSPOPB                                                        521
      GO TO BLOCK_VLEM, (5410 ,5420)                                      521
C End of Local Procedure                                                  521
C ----------------------------------------------------------------------  522
C ------------ VPIP                                                       522
C Begin Local Procedure VPIP                                              523
C VPIP IS THE LONG PIPE                                                   523
5430  CONTINUE                                                            523
         AG_TITLE='VPIP'                                                  523
         AG_MODULE=MODULE                                                 523
         AG_MARK='V00'                                                    523
         CALL AGSPUSH                                                     523
         AG_MODULE=MODULE                                                 523
C    *                                                                    523
C    Geant ATTRIBUTE : VPIP ===> SEEN=1 COLO=7 <===                       524
         CALL AGSRESET                                                    524
         AG_SEEN = 1                                                      524
         AG_COLO = 7                                                      524
         AG_TITLE = 'VPIP'                                                524
         AG_EXNAME = 'ATTRIBUTE'                                          524
         AG_PARLIST = 'SEEN_COLO'                                         524
         CALL AXATTRIBUTE                                                 524
C    *                                                                    524
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        524
         CALL AGSRESET                                                    525
         AG_TITLE = 'ALUMINIUM'                                           525
         AG_EXNAME = 'MATERIAL'                                           525
         AG_PARLIST = 'NONE'                                              525
         CALL AXMATERIAL                                                  525
C    *                                                                    525
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      525
         CALL AGSRESET                                                    526
         AG_RMIN = 0                                                      526
         AG_RMAX = 0                                                      526
         AG_DZ = 0                                                        526
         AG_TITLE = 'TUBE'                                                526
         AG_EXNAME = 'SHAPE'                                              526
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      526
         CALL AXSHAPE                                                     526
C    *                                                                    526
5431  CALL AGSPOPB                                                        527
      GO TO BLOCK_VPIP, (5430 ,5440)                                      527
C End of Local Procedure                                                  527
C ----------------------------------------------------------------------  528
C ------------ VRNG                                                       528
C Begin Local Procedure VRNG                                              529
C VRNG IS A SINGLE VPD RING                                               529
5210  CONTINUE                                                            529
         AG_TITLE='VRNG'                                                  529
         AG_MODULE=MODULE                                                 529
         AG_MARK='V00'                                                    529
         CALL AGSPUSH                                                     529
         AG_MODULE=MODULE                                                 529
C    *                                                                    529
C    Geant SHAPE : TUBE ===> RMIN=VPDG_RMIN RMAX=VPDG_RMIN+2*VPDG_DETRAD  531
C    +VPDG_CLHEIGHT, DZ=VPDG_CLLENGTH/2 <===                              531
         CALL AGSRESET                                                    531
         AG_RMIN = VPDG_RMIN                                              531
         AG_RMAX = VPDG_RMIN+2*VPDG_DETRAD+VPDG_CLHEIGHT                  531
         AG_DZ = VPDG_CLLENGTH/2                                          531
         AG_TITLE = 'TUBE'                                                531
         AG_EXNAME = 'SHAPE'                                              531
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      531
         CALL AXSHAPE                                                     531
C    *                                                                    531
C    *                                                                    531
         ISEC=1                                                           532
            GO TO 5453                                                    532
5451        ISEC=ISEC+1                                                   532
5453        IF(ISEC-(VPDG_NUMPMT).GT.0) GO TO 5452                        532
            DETANGLE=-30+ISEC*(360/VPDG_NUMPMT)                           533
C       Create VSEC :                                                     534
C       Call To Local Procedure VSEC                                      534
            ASSIGN 5470 TO BLOCK_VSEC                                     534
            GO TO  5460                                                   534
5470        CONTINUE                                                      534
C       *                                                                 534
C       Geant POSITION : VSEC ===> ALPHAZ=DETANGLE KONLY='MANY' <===      535
            CALL AGSRESET                                                 535
            AG_ALPHAN=AG_ALPHAN+1                                         535
            AG_ALPHAK(AG_ALPHAN)=3                                        535
            AG_ALPHAL(AG_ALPHAN)=DETANGLE                                 535
            AG_KONLY = 'MANY'                                             535
            AG_TITLE = 'VSEC'                                             535
            AG_EXNAME = 'POSITION'                                        535
            AG_PARLIST = 'ALPHAZ_KONLY'                                   535
            CALL AXPOSITION                                               535
C       *                                                                 535
         GO TO 5451                                                       536
5452     CONTINUE                                                         536
5211  CALL AGSPOPB                                                        537
      GO TO BLOCK_VRNG, (5210 ,5220)                                      537
C End of Local Procedure                                                  537
C ----------------------------------------------------------------------  538
C ------------ VSEC                                                       538
C Begin Local Procedure VSEC                                              539
C VSEC IS ONE VPD SECTOR WITH ALL STUFF INSIDE                            539
5460  CONTINUE                                                            539
         AG_TITLE='VSEC'                                                  539
         AG_MODULE=MODULE                                                 539
         AG_MARK='V00'                                                    539
         CALL AGSPUSH                                                     539
         AG_MODULE=MODULE                                                 539
C    *                                                                    539
C    Geant ATTRIBUTE : VSEC ===> SEEN=1 COLO=5 SERIAL=ISEC <===           540
         CALL AGSRESET                                                    540
         AG_SEEN = 1                                                      540
         AG_COLO = 5                                                      540
         AG_SERIAL = ISEC                                                 540
         AG_TITLE = 'VSEC'                                                540
         AG_EXNAME = 'ATTRIBUTE'                                          540
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  540
         CALL AXATTRIBUTE                                                 540
C    *                                                                    540
C    Geant SHAPE : TUBS ===> PHI1=-360/(2*VPDG_NUMPMT) PHI2=+360/(2*VPDG  540
C    _NUMPMT) <===                                                        540
         CALL AGSRESET                                                    541
         AG_PHI1 = -360/(2*VPDG_NUMPMT)                                   541
         AG_PHI2 = +360/(2*VPDG_NUMPMT)                                   541
         AG_TITLE = 'TUBS'                                                541
         AG_EXNAME = 'SHAPE'                                              541
         AG_PARLIST = 'PHI1_PHI2'                                         541
         CALL AXSHAPE                                                     541
C    *                                                                    541
         YDISPL=VPDG_DETRAD - SQRT(VPDG_DETRAD*VPDG_DETRAD -              541
     *   (VPDG_CLWIDTH/2 - VPDG_CLTHICK)*(VPDG_CLWIDTH/2 -                541
     *   VPDG_CLTHICK))                                                   541
C    Create VDET :                                                        544
C    Call To Local Procedure VDET                                         544
         ASSIGN 5490 TO BLOCK_VDET                                        544
         GO TO  5480                                                      544
5490     CONTINUE                                                         544
C    *                                                                    544
C    Geant POSITION : VDET ===> X=VPDG_RMIN+VPDG_DETRAD <===              545
         CALL AGSRESET                                                    545
         AG_X = VPDG_RMIN+VPDG_DETRAD                                     545
         AG_TITLE = 'VDET'                                                545
         AG_EXNAME = 'POSITION'                                           545
         AG_PARLIST = 'X'                                                 545
         CALL AXPOSITION                                                  545
C    *                                                                    545
C    Check ISEC==3                                                        545
         IF (ISEC.EQ.3) THEN                                              546
C    Create VPCL :                                                        547
C    Call To Local Procedure VPCL                                         547
         ASSIGN 5510 TO BLOCK_VPCL                                        547
         GO TO  5500                                                      547
5510     CONTINUE                                                         547
C    *                                                                    547
C    Geant POSITION : VPCL ===> X=VPDG_RMIN+VPDG_DETRAD Y=(VPDG_DETRAD+V  548
C    PDG_CLHEIGHT/2-YDISPL) <===                                          548
         CALL AGSRESET                                                    548
         AG_X = VPDG_RMIN+VPDG_DETRAD                                     548
         AG_Y = (VPDG_DETRAD+VPDG_CLHEIGHT/2-YDISPL)                      548
         AG_TITLE = 'VPCL'                                                548
         AG_EXNAME = 'POSITION'                                           548
         AG_PARLIST = 'X_Y'                                               548
         CALL AXPOSITION                                                  548
C    *                                                                    548
         ELSE                                                             548
C    Create VPCL :                                                        550
C    Call To Local Procedure VPCL                                         550
         ASSIGN 5520 TO BLOCK_VPCL                                        550
         GO TO  5500                                                      550
5520     CONTINUE                                                         550
C    *                                                                    550
C    Geant POSITION : VPCL ===> X=VPDG_RMIN+VPDG_DETRAD Y=-(VPDG_DETRAD+  551
C    VPDG_CLHEIGHT/2-YDISPL) <===                                         551
         CALL AGSRESET                                                    551
         AG_X = VPDG_RMIN+VPDG_DETRAD                                     551
         AG_Y = -(VPDG_DETRAD+VPDG_CLHEIGHT/2-YDISPL)                     551
         AG_TITLE = 'VPCL'                                                551
         AG_EXNAME = 'POSITION'                                           551
         AG_PARLIST = 'X_Y'                                               551
         CALL AXPOSITION                                                  551
C    *                                                                    551
         ENDIF                                                            551
C    Create VXST :                                                        553
C    Call To Local Procedure VXST                                         553
         ASSIGN 5540 TO BLOCK_VXST                                        553
         GO TO  5530                                                      553
5540     CONTINUE                                                         553
C    *                                                                    553
C    Geant POSITION : VXST ===> X=VPDG_RMIN+VPDG_DETRAD+0.4 Y=+1.5 Z=VPD  554
C    G_DETLEN/2+1.0 <===                                                  554
         CALL AGSRESET                                                    554
         AG_X = VPDG_RMIN+VPDG_DETRAD+0.4                                 554
         AG_Y = +1.5                                                      554
         AG_Z = VPDG_DETLEN/2+1.0                                         554
         AG_TITLE = 'VXST'                                                554
         AG_EXNAME = 'POSITION'                                           554
         AG_PARLIST = 'X_Y_Z'                                             554
         CALL AXPOSITION                                                  554
C    *                                                                    554
C    Geant POSITION : VXST ===> X=VPDG_RMIN+VPDG_DETRAD-0.4 Y=-1.5 Z=VPD  554
C    G_DETLEN/2+1.0 <===                                                  554
         CALL AGSRESET                                                    555
         AG_X = VPDG_RMIN+VPDG_DETRAD-0.4                                 555
         AG_Y = -1.5                                                      555
         AG_Z = VPDG_DETLEN/2+1.0                                         555
         AG_TITLE = 'VXST'                                                555
         AG_EXNAME = 'POSITION'                                           555
         AG_PARLIST = 'X_Y_Z'                                             555
         CALL AXPOSITION                                                  555
C    *                                                                    555
5461  CALL AGSPOPB                                                        556
      GO TO BLOCK_VSEC, (5460 ,5470)                                      556
C End of Local Procedure                                                  556
C ----------------------------------------------------------------------  557
C ------------ VDET                                                       557
C Begin Local Procedure VDET                                              558
C VDET IS A SINGLE DETECTOR (RADIATOR+CONVERTER AND PMT+ELECTRONCS)       558
5480  CONTINUE                                                            558
         AG_TITLE='VDET'                                                  558
         AG_MODULE=MODULE                                                 558
         AG_MARK='V00'                                                    558
         CALL AGSPUSH                                                     558
         AG_MODULE=MODULE                                                 558
C    *                                                                    558
C    Geant MATERIAL : IRON ===> inherited arguments used <===             559
         CALL AGSRESET                                                    559
         AG_TITLE = 'IRON'                                                559
         AG_EXNAME = 'MATERIAL'                                           559
         AG_PARLIST = 'NONE'                                              559
         CALL AXMATERIAL                                                  559
C    *                                                                    559
C    Geant ATTRIBUTE : VDET ===> SEEN=1 COLO=7 SERIAL=0 <===              559
         CALL AGSRESET                                                    560
         AG_SEEN = 1                                                      560
         AG_COLO = 7                                                      560
         AG_SERIAL = 0                                                    560
         AG_TITLE = 'VDET'                                                560
         AG_EXNAME = 'ATTRIBUTE'                                          560
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  560
         CALL AXATTRIBUTE                                                 560
C    *                                                                    560
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=VPDG_DETRAD DZ=VPDG_DETLEN/2 <=  560
C    ==                                                                   560
         CALL AGSRESET                                                    561
         AG_RMIN = 0                                                      561
         AG_RMAX = VPDG_DETRAD                                            561
         AG_DZ = VPDG_DETLEN/2                                            561
         AG_TITLE = 'TUBE'                                                561
         AG_EXNAME = 'SHAPE'                                              561
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      561
         CALL AXSHAPE                                                     561
C    *                                                                    561
C    Create VDTI :                                                        562
C    Call To Local Procedure VDTI                                         562
         ASSIGN 5560 TO BLOCK_VDTI                                        562
         GO TO  5550                                                      562
5560     CONTINUE                                                         562
C    *                                                                    562
C    Geant POSITION : VDTI ===> Z=+VPDG_DETFRONT <===                     563
         CALL AGSRESET                                                    563
         AG_Z = +VPDG_DETFRONT                                            563
         AG_TITLE = 'VDTI'                                                563
         AG_EXNAME = 'POSITION'                                           563
         AG_PARLIST = 'Z'                                                 563
         CALL AXPOSITION                                                  563
C    *                                                                    563
5481  CALL AGSPOPB                                                        564
      GO TO BLOCK_VDET, (5480 ,5490)                                      564
C End of Local Procedure                                                  564
C ----------------------------------------------------------------------  565
C ------------ VDTI                                                       565
C Begin Local Procedure VDTI                                              566
C VDTI IS INNER PART OF THE SINGLE DETECTOR                               566
5550  CONTINUE                                                            566
         AG_TITLE='VDTI'                                                  566
         AG_MODULE=MODULE                                                 566
         AG_MARK='V00'                                                    566
         CALL AGSPUSH                                                     566
         AG_MODULE=MODULE                                                 566
C    *                                                                    566
C    Geant MATERIAL : AIR ===> inherited arguments used <===              567
         CALL AGSRESET                                                    567
         AG_TITLE = 'AIR'                                                 567
         AG_EXNAME = 'MATERIAL'                                           567
         AG_PARLIST = 'NONE'                                              567
         CALL AXMATERIAL                                                  567
C    *                                                                    567
C    Geant ATTRIBUTE : VDTI ===> SEEN=0 <===                              567
         CALL AGSRESET                                                    568
         AG_SEEN = 0                                                      568
         AG_TITLE = 'VDTI'                                                568
         AG_EXNAME = 'ATTRIBUTE'                                          568
         AG_PARLIST = 'SEEN'                                              568
         CALL AXATTRIBUTE                                                 568
C    *                                                                    568
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=VPDG_PMTRAD DZ=VPDG_DETLEN/2-VP  568
C    DG_DETFRONT <===                                                     568
         CALL AGSRESET                                                    569
         AG_RMIN = 0                                                      569
         AG_RMAX = VPDG_PMTRAD                                            569
         AG_DZ = VPDG_DETLEN/2-VPDG_DETFRONT                              569
         AG_TITLE = 'TUBE'                                                569
         AG_EXNAME = 'SHAPE'                                              569
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      569
         CALL AXSHAPE                                                     569
C    *                                                                    569
         CONVLENGTH=VPDG_CONVTHK + VPDG_RADITHK                           569
C    Create VCNV :                                                        571
C    Call To Local Procedure VCNV                                         571
         ASSIGN 5580 TO BLOCK_VCNV                                        571
         GO TO  5570                                                      571
5580     CONTINUE                                                         571
C    *                                                                    571
C    Geant POSITION : VCNV ===> Z=-(VPDG_DETLEN-CONVLENGTH)/2 <===        572
         CALL AGSRESET                                                    572
         AG_Z = -(VPDG_DETLEN-CONVLENGTH)/2                               572
         AG_TITLE = 'VCNV'                                                572
         AG_EXNAME = 'POSITION'                                           572
         AG_PARLIST = 'Z'                                                 572
         CALL AXPOSITION                                                  572
C    *                                                                    572
C    Create VPMT :                                                        573
C    Call To Local Procedure VPMT                                         573
         ASSIGN 5600 TO BLOCK_VPMT                                        573
         GO TO  5590                                                      573
5600     CONTINUE                                                         573
C    *                                                                    573
C    Geant POSITION : VPMT ===> Z=-(VPDG_DETLEN/2-CONVLENGTH-VPDG_PMTLEN  574
C    /2) <===                                                             574
         CALL AGSRESET                                                    574
         AG_Z = -(VPDG_DETLEN/2-CONVLENGTH-VPDG_PMTLEN/2)                 574
         AG_TITLE = 'VPMT'                                                574
         AG_EXNAME = 'POSITION'                                           574
         AG_PARLIST = 'Z'                                                 574
         CALL AXPOSITION                                                  574
C    *                                                                    574
5551  CALL AGSPOPB                                                        575
      GO TO BLOCK_VDTI, (5550 ,5560)                                      575
C End of Local Procedure                                                  575
C ----------------------------------------------------------------------  576
C ------------ VCNV                                                       576
C Begin Local Procedure VCNV                                              577
C VCNV IS CONVERTER LAYER (RADIATOR INCLUDED)                             577
5570  CONTINUE                                                            577
         AG_TITLE='VCNV'                                                  577
         AG_MODULE=MODULE                                                 577
         AG_MARK='V00'                                                    577
         CALL AGSPUSH                                                     577
         AG_MODULE=MODULE                                                 577
C    *                                                                    577
C    Geant MATERIAL : LEAD ===> inherited arguments used <===             578
         CALL AGSRESET                                                    578
         AG_TITLE = 'LEAD'                                                578
         AG_EXNAME = 'MATERIAL'                                           578
         AG_PARLIST = 'NONE'                                              578
         CALL AXMATERIAL                                                  578
C    *                                                                    578
C    Geant ATTRIBUTE : VCNV ===> SEEN=1 COLO=2 <===                       578
         CALL AGSRESET                                                    579
         AG_SEEN = 1                                                      579
         AG_COLO = 2                                                      579
         AG_TITLE = 'VCNV'                                                579
         AG_EXNAME = 'ATTRIBUTE'                                          579
         AG_PARLIST = 'SEEN_COLO'                                         579
         CALL AXATTRIBUTE                                                 579
C    *                                                                    579
C    Geant SHAPE : TUBE ===> DZ=VPDG_CONVTHK/2+VPDG_RADITHK/2 <===        579
         CALL AGSRESET                                                    580
         AG_DZ = VPDG_CONVTHK/2+VPDG_RADITHK/2                            580
         AG_TITLE = 'TUBE'                                                580
         AG_EXNAME = 'SHAPE'                                              580
         AG_PARLIST = 'DZ'                                                580
         CALL AXSHAPE                                                     580
C    *                                                                    580
C    Create VRAD :                                                        581
C    Call To Local Procedure VRAD                                         581
         ASSIGN 5620 TO BLOCK_VRAD                                        581
         GO TO  5610                                                      581
5620     CONTINUE                                                         581
C    *                                                                    581
C    Geant POSITION : VRAD ===> Z=VPDG_CONVTHK/2 <===                     582
         CALL AGSRESET                                                    582
         AG_Z = VPDG_CONVTHK/2                                            582
         AG_TITLE = 'VRAD'                                                582
         AG_EXNAME = 'POSITION'                                           582
         AG_PARLIST = 'Z'                                                 582
         CALL AXPOSITION                                                  582
C    *                                                                    582
5571  CALL AGSPOPB                                                        583
      GO TO BLOCK_VCNV, (5570 ,5580)                                      583
C End of Local Procedure                                                  583
C ----------------------------------------------------------------------  584
C ------------ VRAD                                                       584
C Begin Local Procedure VRAD                                              585
C VRAD IS CERENKOV RADIATOR LAYER                                         585
5610  CONTINUE                                                            585
         AG_TITLE='VRAD'                                                  585
         AG_MODULE=MODULE                                                 585
         AG_MARK='V00'                                                    585
         CALL AGSPUSH                                                     585
         AG_MODULE=MODULE                                                 585
C    *                                                                    585
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       586
         CALL AGSRESET                                                    586
         AG_TITLE = 'POLYSTYREN'                                          586
         AG_EXNAME = 'MATERIAL'                                           586
         AG_PARLIST = 'NONE'                                              586
         CALL AXMATERIAL                                                  586
C    *                                                                    586
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           586
         CALL AGSRESET                                                    587
         AG_ISVOL = 1                                                     587
         AG_TITLE = 'SENSITIVE'                                           587
         AG_EXNAME = 'MEDIUM'                                             587
         AG_PARLIST = 'ISVOL'                                             587
         CALL AXMEDIUM                                                    587
C    *                                                                    587
C    Geant ATTRIBUTE : VRAD ===> SEEN=1 COLO=3 <===                       587
         CALL AGSRESET                                                    588
         AG_SEEN = 1                                                      588
         AG_COLO = 3                                                      588
         AG_TITLE = 'VRAD'                                                588
         AG_EXNAME = 'ATTRIBUTE'                                          588
         AG_PARLIST = 'SEEN_COLO'                                         588
         CALL AXATTRIBUTE                                                 588
C    *                                                                    588
C    Geant SHAPE : TUBE ===> DZ=VPDG_RADITHK/2 <===                       588
         CALL AGSRESET                                                    589
         AG_DZ = VPDG_RADITHK/2                                           589
         AG_TITLE = 'TUBE'                                                589
         AG_EXNAME = 'SHAPE'                                              589
         AG_PARLIST = 'DZ'                                                589
         CALL AXSHAPE                                                     589
C    *                                                                    589
C    Defining Hits: X:0.1:S Y:0.1: Z:0.1: CX:10: CY:10: CZ:10: STEP:0.1:  589
C     SLEN:0.1:(0,600), PTOT:16:(0,100) TOF:18:(0,1.0E-6) ELOSS:16:(0,0.  589
C    01)                                                                  589
         AG_TITLE = 'VRAD'                                                591
         AG_EXNAME = 'HITS'                                               591
         AG_TYPE=AG_MAGIC                                                 591
         CALL AGSDETV('VRAD')                                             591
C    *                                                                    591
         AG_PARLIST='X'                                                   591
         AG_TITLE = 'DETH'                                                591
         AG_NBITS = AG_MAGIC                                              591
         AG_BIN=0.1                                                       591
         AG_OPTION='S'                                                    591
         AG_FHMIN = AG_MAGIC                                              591
         AG_FHMAX = AG_MAGIC                                              591
         CALL AGSDETH (0)                                                 591
C    *                                                                    591
         AG_PARLIST='Y'                                                   591
         AG_TITLE = 'DETH'                                                591
         AG_NBITS = AG_MAGIC                                              591
         AG_BIN=0.1                                                       591
         AG_FHMIN=AG_MAGIC                                                591
         AG_FHMAX=AG_MAGIC                                                591
         AG_OPTION='*'                                                    591
         CALL AGSDETH (0)                                                 591
C    *                                                                    591
         AG_PARLIST='Z'                                                   591
         AG_TITLE = 'DETH'                                                591
         AG_NBITS = AG_MAGIC                                              591
         AG_BIN=0.1                                                       591
         AG_FHMIN=AG_MAGIC                                                591
         AG_FHMAX=AG_MAGIC                                                591
         AG_OPTION='*'                                                    591
         CALL AGSDETH (0)                                                 591
C    *                                                                    591
         AG_PARLIST='CX'                                                  591
         AG_TITLE = 'DETH'                                                591
         AG_NBITS = 10                                                    591
         AG_BIN=AG_MAGIC                                                  591
         AG_FHMIN=AG_MAGIC                                                591
         AG_FHMAX=AG_MAGIC                                                591
         AG_OPTION='*'                                                    591
         CALL AGSDETH (0)                                                 591
C    *                                                                    591
         AG_PARLIST='CY'                                                  591
         AG_TITLE = 'DETH'                                                591
         AG_NBITS = 10                                                    591
         AG_BIN=AG_MAGIC                                                  591
         AG_FHMIN=AG_MAGIC                                                591
         AG_FHMAX=AG_MAGIC                                                591
         AG_OPTION='*'                                                    591
         CALL AGSDETH (0)                                                 591
C    *                                                                    591
         AG_PARLIST='CZ'                                                  591
         AG_TITLE = 'DETH'                                                591
         AG_NBITS = 10                                                    591
         AG_BIN=AG_MAGIC                                                  591
         AG_FHMIN=AG_MAGIC                                                591
         AG_FHMAX=AG_MAGIC                                                591
         AG_OPTION='*'                                                    591
         CALL AGSDETH (0)                                                 591
C    *                                                                    591
         AG_PARLIST='STEP'                                                591
         AG_TITLE = 'DETH'                                                591
         AG_NBITS = AG_MAGIC                                              591
         AG_BIN=0.1                                                       591
         AG_FHMIN=AG_MAGIC                                                591
         AG_FHMAX=AG_MAGIC                                                591
         AG_OPTION='*'                                                    591
         CALL AGSDETH (0)                                                 591
C    *                                                                    591
         AG_PARLIST='SLEN'                                                591
         AG_TITLE = 'DETH'                                                591
         AG_NBITS = AG_MAGIC                                              591
         AG_BIN=0.1                                                       591
         AG_OPTION='*'                                                    591
         AG_FHMIN = 0                                                     591
         AG_FHMAX = 600                                                   591
         CALL AGSDETH (0)                                                 591
C    *                                                                    591
         AG_PARLIST='PTOT'                                                591
         AG_TITLE = 'DETH'                                                591
         AG_NBITS = 16                                                    591
         AG_BIN=AG_MAGIC                                                  591
         AG_OPTION='*'                                                    591
         AG_FHMIN = 0                                                     591
         AG_FHMAX = 100                                                   591
         CALL AGSDETH (0)                                                 591
C    *                                                                    591
         AG_PARLIST='TOF'                                                 591
         AG_TITLE = 'DETH'                                                591
         AG_NBITS = 18                                                    591
         AG_BIN=AG_MAGIC                                                  591
         AG_OPTION='*'                                                    591
         AG_FHMIN = 0                                                     591
         AG_FHMAX = 1.0E-6                                                591
         CALL AGSDETH (0)                                                 591
C    *                                                                    591
         AG_PARLIST='ELOSS'                                               591
         AG_TITLE = 'DETH'                                                591
         AG_NBITS = 16                                                    591
         AG_BIN=AG_MAGIC                                                  591
         AG_OPTION='*'                                                    591
         AG_FHMIN = 0                                                     591
         AG_FHMAX = 0.01                                                  591
         CALL AGSDETH (0)                                                 591
C    *                                                                    591
         CALL AGSDETU('VRAD')                                             591
C    *                                                                    591
5611  CALL AGSPOPB                                                        592
      GO TO BLOCK_VRAD, (5610 ,5620)                                      592
C End of Local Procedure                                                  592
C ----------------------------------------------------------------------  593
C ------------ VPMT                                                       593
C Begin Local Procedure VPMT                                              594
C VPMT IS THE PMT INNER VOLUME                                            594
5590  CONTINUE                                                            594
         AG_TITLE='VPMT'                                                  594
         AG_MODULE=MODULE                                                 594
         AG_MARK='V00'                                                    594
         CALL AGSPUSH                                                     594
         AG_MODULE=MODULE                                                 594
C    *                                                                    594
C    Geant MATERIAL : VACUUM ===> inherited arguments used <===           595
         CALL AGSRESET                                                    595
         AG_TITLE = 'VACUUM'                                              595
         AG_EXNAME = 'MATERIAL'                                           595
         AG_PARLIST = 'NONE'                                              595
         CALL AXMATERIAL                                                  595
C    *                                                                    595
C    Geant ATTRIBUTE : VPMT ===> SEEN=1 COLO=7 <===                       595
         CALL AGSRESET                                                    596
         AG_SEEN = 1                                                      596
         AG_COLO = 7                                                      596
         AG_TITLE = 'VPMT'                                                596
         AG_EXNAME = 'ATTRIBUTE'                                          596
         AG_PARLIST = 'SEEN_COLO'                                         596
         CALL AXATTRIBUTE                                                 596
C    *                                                                    596
C    Geant SHAPE : TUBE ===> RMAX=VPDG_PMTRAD-VPDG_PMTWALL, DZ=VPDG_PMTL  596
C    EN/2-VPDG_PMTWALL <===                                               596
         CALL AGSRESET                                                    598
         AG_RMAX = VPDG_PMTRAD-VPDG_PMTWALL                               598
         AG_DZ = VPDG_PMTLEN/2-VPDG_PMTWALL                               598
         AG_TITLE = 'TUBE'                                                598
         AG_EXNAME = 'SHAPE'                                              598
         AG_PARLIST = 'RMAX_DZ'                                           598
         CALL AXSHAPE                                                     598
C    *                                                                    598
5591  CALL AGSPOPB                                                        599
      GO TO BLOCK_VPMT, (5590 ,5600)                                      599
C End of Local Procedure                                                  599
C ----------------------------------------------------------------------  600
C ------------ VXST                                                       600
C Begin Local Procedure VXST                                              601
C VXST ARE PMT OUTPUT CABLES (JUST TO LOOK NICER)                         601
5530  CONTINUE                                                            601
         AG_TITLE='VXST'                                                  601
         AG_MODULE=MODULE                                                 601
         AG_MARK='V00'                                                    601
         CALL AGSPUSH                                                     601
         AG_MODULE=MODULE                                                 601
C    *                                                                    601
C    Geant ATTRIBUTE : VXST ===> SEEN=1 COLO=7 SERIAL=0 <===              602
         CALL AGSRESET                                                    602
         AG_SEEN = 1                                                      602
         AG_COLO = 7                                                      602
         AG_SERIAL = 0                                                    602
         AG_TITLE = 'VXST'                                                602
         AG_EXNAME = 'ATTRIBUTE'                                          602
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  602
         CALL AXATTRIBUTE                                                 602
C    *                                                                    602
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0.3 DZ=1.0 <===                  602
         CALL AGSRESET                                                    603
         AG_RMIN = 0                                                      603
         AG_RMAX = 0.3                                                    603
         AG_DZ = 1.0                                                      603
         AG_TITLE = 'TUBE'                                                603
         AG_EXNAME = 'SHAPE'                                              603
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      603
         CALL AXSHAPE                                                     603
C    *                                                                    603
5531  CALL AGSPOPB                                                        604
      GO TO BLOCK_VXST, (5530 ,5540)                                      604
C End of Local Procedure                                                  604
C ----------------------------------------------------------------------  605
C ------------ VPCL                                                       605
C Begin Local Procedure VPCL                                              606
C VPCL IS THE BOAT CLAMP                                                  606
5500  CONTINUE                                                            606
         AG_TITLE='VPCL'                                                  606
         AG_MODULE=MODULE                                                 606
         AG_MARK='V00'                                                    606
         CALL AGSPUSH                                                     606
         AG_MODULE=MODULE                                                 606
C    *                                                                    606
C    Geant ATTRIBUTE : VPCL ===> SEEN=1 COLO=6 SERIAL=0 <===              607
         CALL AGSRESET                                                    607
         AG_SEEN = 1                                                      607
         AG_COLO = 6                                                      607
         AG_SERIAL = 0                                                    607
         AG_TITLE = 'VPCL'                                                607
         AG_EXNAME = 'ATTRIBUTE'                                          607
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  607
         CALL AXATTRIBUTE                                                 607
C    *                                                                    607
C    Geant SHAPE : BOX ===> DX=VPDG_CLWIDTH/2 DY=VPDG_CLHEIGHT/2, DZ=VPD  607
C    G_CLLENGTH/2 <===                                                    607
         CALL AGSRESET                                                    609
         AG_DX = VPDG_CLWIDTH/2                                           609
         AG_DY = VPDG_CLHEIGHT/2                                          609
         AG_DZ = VPDG_CLLENGTH/2                                          609
         AG_TITLE = 'BOX'                                                 609
         AG_EXNAME = 'SHAPE'                                              609
         AG_PARLIST = 'DX_DY_DZ'                                          609
         CALL AXSHAPE                                                     609
C    *                                                                    609
C    Create VPCF :                                                        610
C    Call To Local Procedure VPCF                                         610
         ASSIGN 5640 TO BLOCK_VPCF                                        610
         GO TO  5630                                                      610
5640     CONTINUE                                                         610
C    *                                                                    610
C    Geant POSITION : VPCF ===> Z=+(VPDG_CLLENGTH-VPDG_CLTHICK)/2 <===    611
         CALL AGSRESET                                                    611
         AG_Z = +(VPDG_CLLENGTH-VPDG_CLTHICK)/2                           611
         AG_TITLE = 'VPCF'                                                611
         AG_EXNAME = 'POSITION'                                           611
         AG_PARLIST = 'Z'                                                 611
         CALL AXPOSITION                                                  611
C    *                                                                    611
C    Geant POSITION : VPCF ===> Z=-(VPDG_CLLENGTH-VPDG_CLTHICK)/2 <===    611
         CALL AGSRESET                                                    612
         AG_Z = -(VPDG_CLLENGTH-VPDG_CLTHICK)/2                           612
         AG_TITLE = 'VPCF'                                                612
         AG_EXNAME = 'POSITION'                                           612
         AG_PARLIST = 'Z'                                                 612
         CALL AXPOSITION                                                  612
C    *                                                                    612
C    Create VPCH :                                                        613
C    Call To Local Procedure VPCH                                         613
         ASSIGN 5660 TO BLOCK_VPCH                                        613
         GO TO  5650                                                      613
5660     CONTINUE                                                         613
C    *                                                                    613
C    Geant POSITION : VPCH ===> inherited arguments used <===             614
         CALL AGSRESET                                                    614
         AG_TITLE = 'VPCH'                                                614
         AG_EXNAME = 'POSITION'                                           614
         AG_PARLIST = 'NONE'                                              614
         CALL AXPOSITION                                                  614
C    *                                                                    614
C    Create VPCV :                                                        615
C    Call To Local Procedure VPCV                                         615
         ASSIGN 5680 TO BLOCK_VPCV                                        615
         GO TO  5670                                                      615
5680     CONTINUE                                                         615
C    *                                                                    615
C    Geant POSITION : VPCV ===> X=(VPDG_CLWIDTH-VPDG_CLTHICK)/2 Y=-VPDG_  617
C    CLTHICK/2-(VPDG_CLHEIGHT/2-VPDG_CLTHICK/2)/2 <===                    617
         CALL AGSRESET                                                    617
         AG_X = (VPDG_CLWIDTH-VPDG_CLTHICK)/2                             617
         AG_Y = -VPDG_CLTHICK/2-(VPDG_CLHEIGHT/2-VPDG_CLTHICK/2)/2        617
         AG_TITLE = 'VPCV'                                                617
         AG_EXNAME = 'POSITION'                                           617
         AG_PARLIST = 'X_Y'                                               617
         CALL AXPOSITION                                                  617
C    *                                                                    617
C    Geant POSITION : VPCV ===> X=-(VPDG_CLWIDTH-VPDG_CLTHICK)/2 Y=-VPDG  617
C    _CLTHICK/2-(VPDG_CLHEIGHT/2-VPDG_CLTHICK/2)/2 <===                   617
         CALL AGSRESET                                                    619
         AG_X = -(VPDG_CLWIDTH-VPDG_CLTHICK)/2                            619
         AG_Y = -VPDG_CLTHICK/2-(VPDG_CLHEIGHT/2-VPDG_CLTHICK/2)/2        619
         AG_TITLE = 'VPCV'                                                619
         AG_EXNAME = 'POSITION'                                           619
         AG_PARLIST = 'X_Y'                                               619
         CALL AXPOSITION                                                  619
C    *                                                                    619
5501  CALL AGSPOPB                                                        620
      GO TO BLOCK_VPCL, (5500 ,5510,5520)                                 620
C End of Local Procedure                                                  620
C ----------------------------------------------------------------------  621
C ------------ VPCF                                                       621
C Begin Local Procedure VPCF                                              622
C VPCF IS THE FRONT PLATE OF THE BOAT CLAMP                               622
5630  CONTINUE                                                            622
         AG_TITLE='VPCF'                                                  622
         AG_MODULE=MODULE                                                 622
         AG_MARK='V00'                                                    622
         CALL AGSPUSH                                                     622
         AG_MODULE=MODULE                                                 622
C    *                                                                    622
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        623
         CALL AGSRESET                                                    623
         AG_TITLE = 'ALUMINIUM'                                           623
         AG_EXNAME = 'MATERIAL'                                           623
         AG_PARLIST = 'NONE'                                              623
         CALL AXMATERIAL                                                  623
C    *                                                                    623
C    Geant ATTRIBUTE : VPCF ===> SEEN=1 COLO=6 FILL=6 <===                623
         CALL AGSRESET                                                    624
         AG_SEEN = 1                                                      624
         AG_COLO = 6                                                      624
         AG_FILL = 6                                                      624
         AG_TITLE = 'VPCF'                                                624
         AG_EXNAME = 'ATTRIBUTE'                                          624
         AG_PARLIST = 'SEEN_COLO_FILL'                                    624
         CALL AXATTRIBUTE                                                 624
C    *                                                                    624
C    Geant SHAPE : BOX ===> DX=VPDG_CLWIDTH/2 DY=VPDG_CLHEIGHT/2, DZ=VPD  624
C    G_CLTHICK/2 <===                                                     624
         CALL AGSRESET                                                    626
         AG_DX = VPDG_CLWIDTH/2                                           626
         AG_DY = VPDG_CLHEIGHT/2                                          626
         AG_DZ = VPDG_CLTHICK/2                                           626
         AG_TITLE = 'BOX'                                                 626
         AG_EXNAME = 'SHAPE'                                              626
         AG_PARLIST = 'DX_DY_DZ'                                          626
         CALL AXSHAPE                                                     626
C    *                                                                    626
5631  CALL AGSPOPB                                                        627
      GO TO BLOCK_VPCF, (5630 ,5640)                                      627
C End of Local Procedure                                                  627
C ----------------------------------------------------------------------  628
C ------------ VPCH                                                       628
C Begin Local Procedure VPCH                                              629
C VPCH IS THE HORIZONTAL PLATE OF THE BOAT CLAMP                          629
5650  CONTINUE                                                            629
         AG_TITLE='VPCH'                                                  629
         AG_MODULE=MODULE                                                 629
         AG_MARK='V00'                                                    629
         CALL AGSPUSH                                                     629
         AG_MODULE=MODULE                                                 629
C    *                                                                    629
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        630
         CALL AGSRESET                                                    630
         AG_TITLE = 'ALUMINIUM'                                           630
         AG_EXNAME = 'MATERIAL'                                           630
         AG_PARLIST = 'NONE'                                              630
         CALL AXMATERIAL                                                  630
C    *                                                                    630
C    Geant ATTRIBUTE : VPCH ===> SEEN=1 COLO=6 FILL=6 <===                630
         CALL AGSRESET                                                    631
         AG_SEEN = 1                                                      631
         AG_COLO = 6                                                      631
         AG_FILL = 6                                                      631
         AG_TITLE = 'VPCH'                                                631
         AG_EXNAME = 'ATTRIBUTE'                                          631
         AG_PARLIST = 'SEEN_COLO_FILL'                                    631
         CALL AXATTRIBUTE                                                 631
C    *                                                                    631
C    Geant SHAPE : BOX ===> DX=VPDG_CLWIDTH/2 DY=VPDG_CLTHICK/2, DZ=(VPD  631
C    G_CLLENGTH-2*VPDG_CLTHICK)/2 <===                                    631
         CALL AGSRESET                                                    633
         AG_DX = VPDG_CLWIDTH/2                                           633
         AG_DY = VPDG_CLTHICK/2                                           633
         AG_DZ = (VPDG_CLLENGTH-2*VPDG_CLTHICK)/2                         633
         AG_TITLE = 'BOX'                                                 633
         AG_EXNAME = 'SHAPE'                                              633
         AG_PARLIST = 'DX_DY_DZ'                                          633
         CALL AXSHAPE                                                     633
C    *                                                                    633
5651  CALL AGSPOPB                                                        634
      GO TO BLOCK_VPCH, (5650 ,5660)                                      634
C End of Local Procedure                                                  634
C ----------------------------------------------------------------------  635
C ------------ VPCV                                                       635
C Begin Local Procedure VPCV                                              636
C VPCV IS THE VERTICAL PLATE OF THE BOAT CLAMP                            636
5670  CONTINUE                                                            636
         AG_TITLE='VPCV'                                                  636
         AG_MODULE=MODULE                                                 636
         AG_MARK='V00'                                                    636
         CALL AGSPUSH                                                     636
         AG_MODULE=MODULE                                                 636
C    *                                                                    636
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        637
         CALL AGSRESET                                                    637
         AG_TITLE = 'ALUMINIUM'                                           637
         AG_EXNAME = 'MATERIAL'                                           637
         AG_PARLIST = 'NONE'                                              637
         CALL AXMATERIAL                                                  637
C    *                                                                    637
C    Geant ATTRIBUTE : VPCV ===> SEEN=1 COLO=6 FILL=6 <===                637
         CALL AGSRESET                                                    638
         AG_SEEN = 1                                                      638
         AG_COLO = 6                                                      638
         AG_FILL = 6                                                      638
         AG_TITLE = 'VPCV'                                                638
         AG_EXNAME = 'ATTRIBUTE'                                          638
         AG_PARLIST = 'SEEN_COLO_FILL'                                    638
         CALL AXATTRIBUTE                                                 638
C    *                                                                    638
C    Geant SHAPE : BOX ===> DX=VPDG_CLTHICK/2 DY=(VPDG_CLHEIGHT/2-VPDG_C  638
C    LTHICK/2)/2, DZ=(VPDG_CLLENGTH-2*VPDG_CLTHICK)/2 <===                638
         CALL AGSRESET                                                    640
         AG_DX = VPDG_CLTHICK/2                                           640
         AG_DY = (VPDG_CLHEIGHT/2-VPDG_CLTHICK/2)/2                       640
         AG_DZ = (VPDG_CLLENGTH-2*VPDG_CLTHICK)/2                         640
         AG_TITLE = 'BOX'                                                 640
         AG_EXNAME = 'SHAPE'                                              640
         AG_PARLIST = 'DX_DY_DZ'                                          640
         CALL AXSHAPE                                                     640
C    *                                                                    640
5671  CALL AGSPOPB                                                        641
      GO TO BLOCK_VPCV, (5670 ,5680)                                      641
C End of Local Procedure                                                  641
C ----------------------------------------------------------------------  642
C ------------ IBEM                                                       642
C Begin Local Procedure IBEM                                              643
C IBEM IS THE IBEAM STRUCTURE BENEATH THE BELL REDUCER CONE               643
5230  CONTINUE                                                            643
         AG_TITLE='IBEM'                                                  643
         AG_MODULE=MODULE                                                 643
         AG_MARK='V00'                                                    643
         CALL AGSPUSH                                                     643
         AG_MODULE=MODULE                                                 643
C    *                                                                    643
C    Geant MATERIAL : AIR ===> inherited arguments used <===              644
         CALL AGSRESET                                                    644
         AG_TITLE = 'AIR'                                                 644
         AG_EXNAME = 'MATERIAL'                                           644
         AG_PARLIST = 'NONE'                                              644
         CALL AXMATERIAL                                                  644
C    *                                                                    644
C    Geant ATTRIBUTE : IBEM ===> SEEN=0 COLO=2 <===                       644
         CALL AGSRESET                                                    645
         AG_SEEN = 0                                                      645
         AG_COLO = 2                                                      645
         AG_TITLE = 'IBEM'                                                645
         AG_EXNAME = 'ATTRIBUTE'                                          645
         AG_PARLIST = 'SEEN_COLO'                                         645
         CALL AXATTRIBUTE                                                 645
C    *                                                                    645
C    Geant SHAPE : BOX ===> DX=VPDG_IBWIDTH/2 DY=VPDG_IBHEIGHT/2, DZ=VPD  645
C    G_IBLENG/2 <===                                                      645
         CALL AGSRESET                                                    647
         AG_DX = VPDG_IBWIDTH/2                                           647
         AG_DY = VPDG_IBHEIGHT/2                                          647
         AG_DZ = VPDG_IBLENG/2                                            647
         AG_TITLE = 'BOX'                                                 647
         AG_EXNAME = 'SHAPE'                                              647
         AG_PARLIST = 'DX_DY_DZ'                                          647
         CALL AXSHAPE                                                     647
C    *                                                                    647
         AG_DY = VPDG_IBTHICKH/2                                          647
C    Create IBEH :                                                        648
C    Call To Local Procedure IBEH                                         648
         ASSIGN 5700 TO BLOCK_IBEH                                        648
         GO TO  5690                                                      648
5700     CONTINUE                                                         648
C    *                                                                    648
C    Geant POSITION : IBEH ===> Y=+(VPDG_IBHEIGHT-VPDG_IBTHICKH)/2 <===   649
         CALL AGSRESET                                                    649
         AG_Y = +(VPDG_IBHEIGHT-VPDG_IBTHICKH)/2                          649
         AG_TITLE = 'IBEH'                                                649
         AG_EXNAME = 'POSITION'                                           649
         AG_PARLIST = 'Y'                                                 649
         CALL AXPOSITION                                                  649
C    *                                                                    649
         AG_DZ = (VPDG_IBLENG-VPDG_IBWLEN)/2                              649
         AG_DY = VPDG_IBTHICKH/2                                          650
C    Create IBEH :                                                        650
C    Call To Local Procedure IBEH                                         650
         ASSIGN 5710 TO BLOCK_IBEH                                        650
         GO TO  5690                                                      650
5710     CONTINUE                                                         650
C    *                                                                    650
C    Geant POSITION : IBEH ===> Y=-(VPDG_IBHEIGHT-VPDG_IBTHICKH)/2 Z=+VP  651
C    DG_IBWLEN/2 <===                                                     651
         CALL AGSRESET                                                    651
         AG_Y = -(VPDG_IBHEIGHT-VPDG_IBTHICKH)/2                          651
         AG_Z = +VPDG_IBWLEN/2                                            651
         AG_TITLE = 'IBEH'                                                651
         AG_EXNAME = 'POSITION'                                           651
         AG_PARLIST = 'Y_Z'                                               651
         CALL AXPOSITION                                                  651
C    *                                                                    651
C    Create IBEV :                                                        652
C    Call To Local Procedure IBEV                                         652
         ASSIGN 5730 TO BLOCK_IBEV                                        652
         GO TO  5720                                                      652
5730     CONTINUE                                                         652
C    *                                                                    652
C    Geant POSITION : IBEV ===> Z=+VPDG_IBWLEN/2 <===                     653
         CALL AGSRESET                                                    653
         AG_Z = +VPDG_IBWLEN/2                                            653
         AG_TITLE = 'IBEV'                                                653
         AG_EXNAME = 'POSITION'                                           653
         AG_PARLIST = 'Z'                                                 653
         CALL AXPOSITION                                                  653
C    *                                                                    653
C    Create IBEW :                                                        654
C    Call To Local Procedure IBEW                                         654
         ASSIGN 5750 TO BLOCK_IBEW                                        654
         GO TO  5740                                                      654
5750     CONTINUE                                                         654
C    *                                                                    654
C    Geant POSITION : IBEW ===> Y=(VPDG_IBHEIGHT/2-VPDG_IBTHICKH-(VPDG_I  656
C    BWHGHTF+VPDG_IBWHGHTB)/4) Z=-(VPDG_IBLENG-VPDG_IBWLEN)/2 ORT=YZX AL  656
C    PHAZ=180 <===                                                        656
         CALL AGSRESET                                                    656
         AG_Y = (VPDG_IBHEIGHT/                                           656
     *   2-VPDG_IBTHICKH-(VPDG_IBWHGHTF+VPDG_IBWHGHTB)/4)                 656
         AG_Z = -(VPDG_IBLENG-VPDG_IBWLEN)/2                              656
         AG_ALPHAN=AG_ALPHAN+1                                            656
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 656
         AG_ALPHAN=AG_ALPHAN+1                                            656
         AG_ALPHAK(AG_ALPHAN)=3                                           656
         AG_ALPHAL(AG_ALPHAN)=180                                         656
         AG_TITLE = 'IBEW'                                                656
         AG_EXNAME = 'POSITION'                                           656
         AG_PARLIST = 'Y_Z_ORT_ALPHAZ'                                    656
         CALL AXPOSITION                                                  656
C    *                                                                    656
5231  CALL AGSPOPB                                                        657
      GO TO BLOCK_IBEM, (5230 ,5240)                                      657
C End of Local Procedure                                                  657
C ----------------------------------------------------------------------  658
C ------------ IBEH                                                       658
C Begin Local Procedure IBEH                                              659
C IBEH IS A HORIZONTAL IBEAM PLATE                                        659
5690  CONTINUE                                                            659
         AG_TITLE='IBEH'                                                  659
         AG_MODULE=MODULE                                                 659
         AG_MARK='V00'                                                    659
         CALL AGSPUSH                                                     659
         AG_MODULE=MODULE                                                 659
C    *                                                                    659
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        660
         CALL AGSRESET                                                    660
         AG_TITLE = 'ALUMINIUM'                                           660
         AG_EXNAME = 'MATERIAL'                                           660
         AG_PARLIST = 'NONE'                                              660
         CALL AXMATERIAL                                                  660
C    *                                                                    660
C    Geant ATTRIBUTE : IBEH ===> SEEN=1 COLO=2 FILL=6 <===                660
         CALL AGSRESET                                                    661
         AG_SEEN = 1                                                      661
         AG_COLO = 2                                                      661
         AG_FILL = 6                                                      661
         AG_TITLE = 'IBEH'                                                661
         AG_EXNAME = 'ATTRIBUTE'                                          661
         AG_PARLIST = 'SEEN_COLO_FILL'                                    661
         CALL AXATTRIBUTE                                                 661
C    *                                                                    661
C    Geant SHAPE : BOX ===> inherited arguments used <===                 661
         CALL AGSRESET                                                    662
         AG_TITLE = 'BOX'                                                 662
         AG_EXNAME = 'SHAPE'                                              662
         AG_PARLIST = 'NONE'                                              662
         CALL AXSHAPE                                                     662
C    *                                                                    662
5691  CALL AGSPOPB                                                        663
      GO TO BLOCK_IBEH, (5690 ,5700,5710)                                 663
C End of Local Procedure                                                  663
C ----------------------------------------------------------------------  664
C ------------ IBEV                                                       664
C Begin Local Procedure IBEV                                              665
C IBEV IS A VERTICAL IBEAM PLATE                                          665
5720  CONTINUE                                                            665
         AG_TITLE='IBEV'                                                  665
         AG_MODULE=MODULE                                                 665
         AG_MARK='V00'                                                    665
         CALL AGSPUSH                                                     665
         AG_MODULE=MODULE                                                 665
C    *                                                                    665
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        666
         CALL AGSRESET                                                    666
         AG_TITLE = 'ALUMINIUM'                                           666
         AG_EXNAME = 'MATERIAL'                                           666
         AG_PARLIST = 'NONE'                                              666
         CALL AXMATERIAL                                                  666
C    *                                                                    666
C    Geant ATTRIBUTE : IBEV ===> SEEN=1 COLO=2 FILL=6 <===                666
         CALL AGSRESET                                                    667
         AG_SEEN = 1                                                      667
         AG_COLO = 2                                                      667
         AG_FILL = 6                                                      667
         AG_TITLE = 'IBEV'                                                667
         AG_EXNAME = 'ATTRIBUTE'                                          667
         AG_PARLIST = 'SEEN_COLO_FILL'                                    667
         CALL AXATTRIBUTE                                                 667
C    *                                                                    667
C    Geant SHAPE : BOX ===> DX=VPDG_IBTHICKV/2 DY=(VPDG_IBHEIGHT-2*VPDG_  667
C    IBTHICKH)/2, DZ=(VPDG_IBLENG-VPDG_IBWLEN)/2 <===                     667
         CALL AGSRESET                                                    669
         AG_DX = VPDG_IBTHICKV/2                                          669
         AG_DY = (VPDG_IBHEIGHT-2*VPDG_IBTHICKH)/2                        669
         AG_DZ = (VPDG_IBLENG-VPDG_IBWLEN)/2                              669
         AG_TITLE = 'BOX'                                                 669
         AG_EXNAME = 'SHAPE'                                              669
         AG_PARLIST = 'DX_DY_DZ'                                          669
         CALL AXSHAPE                                                     669
C    *                                                                    669
5721  CALL AGSPOPB                                                        670
      GO TO BLOCK_IBEV, (5720 ,5730)                                      670
C End of Local Procedure                                                  670
C ----------------------------------------------------------------------  671
C ------------ IBEW                                                       671
C Begin Local Procedure IBEW                                              672
C IBEW IS THE FIRST PART OF THE IBEAM PLATE                               672
5740  CONTINUE                                                            672
         AG_TITLE='IBEW'                                                  672
         AG_MODULE=MODULE                                                 672
         AG_MARK='V00'                                                    672
         CALL AGSPUSH                                                     672
         AG_MODULE=MODULE                                                 672
C    *                                                                    672
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        673
         CALL AGSRESET                                                    673
         AG_TITLE = 'ALUMINIUM'                                           673
         AG_EXNAME = 'MATERIAL'                                           673
         AG_PARLIST = 'NONE'                                              673
         CALL AXMATERIAL                                                  673
C    *                                                                    673
C    Geant ATTRIBUTE : IBEW ===> SEEN=1 COLO=2 FILL=6 <===                673
         CALL AGSRESET                                                    674
         AG_SEEN = 1                                                      674
         AG_COLO = 2                                                      674
         AG_FILL = 6                                                      674
         AG_TITLE = 'IBEW'                                                674
         AG_EXNAME = 'ATTRIBUTE'                                          674
         AG_PARLIST = 'SEEN_COLO_FILL'                                    674
         CALL AXATTRIBUTE                                                 674
C    *                                                                    674
C    Geant SHAPE : TRAP ===> DZ=VPDG_IBTHICKV/2 THET=0. PHI=0., H1=VPDG_  674
C    IBWLEN/2 BL1=VPDG_IBWHGHTF/2 TL1=VPDG_IBWHGHTB/2, ALP1=ATAN((VPDG_I  674
C    BWHGHTB-VPDG_IBWHGHTF)/VPDG_IBWLEN/2)*RADDEG, H2=VPDG_IBWLEN/2, BL2  674
C    =VPDG_IBWHGHTF/2 TL2=VPDG_IBWHGHTB/2, ALP2=ATAN((VPDG_IBWHGHTB-VPDG  674
C    _IBWHGHTF)/VPDG_IBWLEN/2)*RADDEG <===                                674
         CALL AGSRESET                                                    680
         AG_DZ = VPDG_IBTHICKV/2                                          680
         AG_THET = 0.                                                     680
         AG_PHI = 0.                                                      680
         AG_H1 = VPDG_IBWLEN/2                                            680
         AG_BL1 = VPDG_IBWHGHTF/2                                         680
         AG_TL1 = VPDG_IBWHGHTB/2                                         680
         AG_ALP1 = ATAN((VPDG_IBWHGHTB-VPDG_IBWHGHTF)/VPDG_IBWLEN/        680
     *   2)*RADDEG                                                        680
         AG_H2 = VPDG_IBWLEN/2                                            680
         AG_BL2 = VPDG_IBWHGHTF/2                                         680
         AG_TL2 = VPDG_IBWHGHTB/2                                         680
         AG_ALP2 = ATAN((VPDG_IBWHGHTB-VPDG_IBWHGHTF)/VPDG_IBWLEN/        680
     *   2)*RADDEG                                                        680
         AG_TITLE = 'TRAP'                                                680
         AG_EXNAME = 'SHAPE'                                              680
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       680
         CALL AXSHAPE                                                     680
C    *                                                                    680
5741  CALL AGSPOPB                                                        681
      GO TO BLOCK_IBEW, (5740 ,5750)                                      681
C End of Local Procedure                                                  681
      WRITE(LOUT,5760)'===>>  [End AgML Module VPDDGEO1]  <<==='          681
5760  FORMAT(A32,/,/)                                                     681
C MODULE VPDDGEO1                                                         683
      END                                                                 683
