C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE VPDDGEO3                                                 1
C VPDDGEO3 IS THE STARTDET AND PIPE SUPPORT HARDWARE                      1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'VPDDGEO3'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'IS THE STARTDET AND PIPE SUPPORT HARDWARE'/          1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C AUTHOR W.J. LLOPE, A. OGAWA                                             1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'W.J. LLOPE, A. OGAWA'/                        2
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
      INTEGER BLOCK_VPBP                                                  12
      INTEGER BLOCK_VPBO                                                  12
      INTEGER BLOCK_VPBA                                                  12
      INTEGER BLOCK_VPBB                                                  12
      INTEGER BLOCK_VPBX                                                  12
      INTEGER BLOCK_VFEE                                                  12
      INTEGER BLOCK_VLEM                                                  12
      INTEGER BLOCK_VPIP                                                  12
      INTEGER BLOCK_VPFP                                                  12
      INTEGER BLOCK_VPFA                                                  12
      INTEGER BLOCK_VPFB                                                  12
      INTEGER BLOCK_VPFC                                                  12
      INTEGER BLOCK_VPBI                                                  12
      INTEGER BLOCK_VPST                                                  12
      INTEGER BLOCK_VPSC                                                  12
      INTEGER BLOCK_VPSA                                                  12
      INTEGER BLOCK_VPSB                                                  12
      INTEGER BLOCK_VPSV                                                  12
      INTEGER BLOCK_VPSW                                                  12
      INTEGER BLOCK_VRNG                                                  12
      INTEGER BLOCK_VSEC                                                  12
      INTEGER BLOCK_VDET                                                  12
      INTEGER BLOCK_VXST                                                  12
      INTEGER BLOCK_VCNV                                                  12
      INTEGER BLOCK_VPMT                                                  12
      INTEGER BLOCK_VRAD                                                  12
      INTEGER BLOCK_VPCL                                                  12
      INTEGER BLOCK_VPCF                                                  12
      INTEGER BLOCK_VPCV                                                  12
      INTEGER BLOCK_VPCH                                                  12
      INTEGER BLOCK_VDTI                                                  12
      INTEGER BLOCK_IBEM                                                  12
      INTEGER BLOCK_IBEH                                                  12
      INTEGER BLOCK_IBEV                                                  12
      INTEGER BLOCK_IBEW                                                  12
      INTEGER BLOCK_IBSA                                                  12
      INTEGER BLOCK_IBSB                                                  12
      INTEGER BLOCK_IBSC                                                  12
      INTEGER BLOCK_IBSD                                                  12
      INTEGER BLOCK_IBSE                                                  12
      INTEGER BLOCK_IBSF                                                  12
      INTEGER BLOCK_IBSG                                                  12
      INTEGER BLOCK_IBSH                                                  12
      INTEGER BLOCK_IBAA                                                  12
      INTEGER BLOCK_IBAB                                                  12
      INTEGER BLOCK_IBAC                                                  12
      INTEGER BLOCK_IBST                                                  12
      INTEGER BLOCK_IBBC                                                  12
      INTEGER BLOCK_IBCC                                                  12
      INTEGER BLOCK_IBWA                                                  12
      INTEGER BLOCK_IBWB                                                  12
      INTEGER BLOCK_IBWC                                                  12
      INTEGER BLOCK_IBWD                                                  12
      INTEGER BLOCK_IBWE                                                  12
      INTEGER BLOCK_IBWF                                                  12
      INTEGER BLOCK_IBWG                                                  12
      INTEGER BLOCK_IBWH                                                  12
C -                                                                       13
      REAL VPDV_VERSION                                                   13
      EQUIVALENCE (BANK_VPDV(3),VPDV_VERSION,RE_VPDV_VERSION),            13
     *(NAMES_VPDV(1),NVPDV_VERSION)                                       13
      EQUIVALENCE (COMMEN_VPDV(1),CVPDV_VERSION),(MAP_VPDV(1,1),          13
     *MVPDV_VERSION)                                                      13
      CHARACTER CVPDV_VERSION*40/' '/                                     13
      CHARACTER NVPDV_VERSION*12/'R.VERSION'/                             13
      INTEGER MVPDV_VERSION(2)/1,1/                                       13
      REAL RE_VPDV_VERSION                                                13
C -                                                                       13
      REAL VPDV_VPDCONFIG                                                 13
      EQUIVALENCE (BANK_VPDV(4),VPDV_VPDCONFIG,RE_VPDV_VPDCONFIG),        13
     *(NAMES_VPDV(2),NVPDV_VPDCONFIG)                                     13
      EQUIVALENCE (COMMEN_VPDV(2),CVPDV_VPDCONFIG),(MAP_VPDV(1,2),        13
     *MVPDV_VPDCONFIG)                                                    13
      CHARACTER CVPDV_VPDCONFIG*40/' '/                                   13
      CHARACTER NVPDV_VPDCONFIG*12/'R.VPDCONFIG'/                         13
      INTEGER MVPDV_VPDCONFIG(2)/1,1/                                     13
      REAL RE_VPDV_VPDCONFIG                                              13
C -                                                                       13
      INTEGER LEN_VPDV,LLEN_VPDV,LINK_VPDV                                13
      PARAMETER (LEN_VPDV=4)                                              13
      PARAMETER (LLEN_VPDV=2)                                             13
      INTEGER FLAG_VPDV/0/,MAP_VPDV(2,LLEN_VPDV),IDNUM_VPDV(10)           13
      CHARACTER NAMES_VPDV(LLEN_VPDV)*12,COMMEN_VPDV(LLEN_VPDV)*40        13
      DATA IDNUM_VPDV/10*0/                                               13
      REAL BANK_VPDV(LEN_VPDV)                                            13
      CHARACTER*60 BPATH_VPDV/' '/                                        13
      SAVE FLAG_VPDV,MAP_VPDV,IDNUM_VPDV,NAMES_VPDV,COMMEN_VPDV,          13
     *BPATH_VPDV                                                          13
      DATA LINK_VPDV/0/,BANK_VPDV/LEN_VPDV*0/                             13
C *                                                                       13
C -                                                                       20
      REAL VPDG_VERSION                                                   20
      EQUIVALENCE (BANK_VPDG(3),VPDG_VERSION,RE_VPDG_VERSION),            20
     *(NAMES_VPDG(1),NVPDG_VERSION)                                       20
      EQUIVALENCE (COMMEN_VPDG(1),CVPDG_VERSION),(MAP_VPDG(1,1),          20
     *MVPDG_VERSION)                                                      20
      CHARACTER CVPDG_VERSION*40/' '/                                     20
      CHARACTER NVPDG_VERSION*12/'R.VERSION'/                             20
      INTEGER MVPDG_VERSION(2)/1,1/                                       20
      REAL RE_VPDG_VERSION                                                20
C -                                                                       20
      REAL VPDG_ZPOSEAST                                                  20
      EQUIVALENCE (BANK_VPDG(4),VPDG_ZPOSEAST,RE_VPDG_ZPOSEAST),          20
     *(NAMES_VPDG(2),NVPDG_ZPOSEAST)                                      20
      EQUIVALENCE (COMMEN_VPDG(2),CVPDG_ZPOSEAST),(MAP_VPDG(1,2),         20
     *MVPDG_ZPOSEAST)                                                     20
      CHARACTER CVPDG_ZPOSEAST*40/' '/                                    20
      CHARACTER NVPDG_ZPOSEAST*12/'R.ZPOSEAST'/                           20
      INTEGER MVPDG_ZPOSEAST(2)/1,1/                                      20
      REAL RE_VPDG_ZPOSEAST                                               20
C -                                                                       20
      REAL VPDG_ZPOSWEST                                                  20
      EQUIVALENCE (BANK_VPDG(5),VPDG_ZPOSWEST,RE_VPDG_ZPOSWEST),          20
     *(NAMES_VPDG(3),NVPDG_ZPOSWEST)                                      20
      EQUIVALENCE (COMMEN_VPDG(3),CVPDG_ZPOSWEST),(MAP_VPDG(1,3),         20
     *MVPDG_ZPOSWEST)                                                     20
      CHARACTER CVPDG_ZPOSWEST*40/' '/                                    20
      CHARACTER NVPDG_ZPOSWEST*12/'R.ZPOSWEST'/                           20
      INTEGER MVPDG_ZPOSWEST(2)/1,1/                                      20
      REAL RE_VPDG_ZPOSWEST                                               20
C -                                                                       20
      REAL VPDG_RMIN                                                      20
      EQUIVALENCE (BANK_VPDG(6),VPDG_RMIN,RE_VPDG_RMIN),(NAMES_VPDG(4),   20
     *NVPDG_RMIN)                                                         20
      EQUIVALENCE (COMMEN_VPDG(4),CVPDG_RMIN),(MAP_VPDG(1,4),             20
     *MVPDG_RMIN)                                                         20
      CHARACTER CVPDG_RMIN*40/' '/                                        20
      CHARACTER NVPDG_RMIN*12/'R.RMIN'/                                   20
      INTEGER MVPDG_RMIN(2)/1,1/                                          20
      REAL RE_VPDG_RMIN                                                   20
C -                                                                       20
      REAL VPDG_RMAX                                                      20
      EQUIVALENCE (BANK_VPDG(7),VPDG_RMAX,RE_VPDG_RMAX),(NAMES_VPDG(5),   20
     *NVPDG_RMAX)                                                         20
      EQUIVALENCE (COMMEN_VPDG(5),CVPDG_RMAX),(MAP_VPDG(1,5),             20
     *MVPDG_RMAX)                                                         20
      CHARACTER CVPDG_RMAX*40/' '/                                        20
      CHARACTER NVPDG_RMAX*12/'R.RMAX'/                                   20
      INTEGER MVPDG_RMAX(2)/1,1/                                          20
      REAL RE_VPDG_RMAX                                                   20
C -                                                                       20
      REAL VPDG_BPWIDTH                                                   20
      EQUIVALENCE (BANK_VPDG(8),VPDG_BPWIDTH,RE_VPDG_BPWIDTH),            20
     *(NAMES_VPDG(6),NVPDG_BPWIDTH)                                       20
      EQUIVALENCE (COMMEN_VPDG(6),CVPDG_BPWIDTH),(MAP_VPDG(1,6),          20
     *MVPDG_BPWIDTH)                                                      20
      CHARACTER CVPDG_BPWIDTH*40/' '/                                     20
      CHARACTER NVPDG_BPWIDTH*12/'R.BPWIDTH'/                             20
      INTEGER MVPDG_BPWIDTH(2)/1,1/                                       20
      REAL RE_VPDG_BPWIDTH                                                20
C -                                                                       20
      REAL VPDG_BPLENGTH                                                  20
      EQUIVALENCE (BANK_VPDG(9),VPDG_BPLENGTH,RE_VPDG_BPLENGTH),          20
     *(NAMES_VPDG(7),NVPDG_BPLENGTH)                                      20
      EQUIVALENCE (COMMEN_VPDG(7),CVPDG_BPLENGTH),(MAP_VPDG(1,7),         20
     *MVPDG_BPLENGTH)                                                     20
      CHARACTER CVPDG_BPLENGTH*40/' '/                                    20
      CHARACTER NVPDG_BPLENGTH*12/'R.BPLENGTH'/                           20
      INTEGER MVPDG_BPLENGTH(2)/1,1/                                      20
      REAL RE_VPDG_BPLENGTH                                               20
C -                                                                       20
      REAL VPDG_BPTHICK                                                   20
      EQUIVALENCE (BANK_VPDG(10),VPDG_BPTHICK,RE_VPDG_BPTHICK),           20
     *(NAMES_VPDG(8),NVPDG_BPTHICK)                                       20
      EQUIVALENCE (COMMEN_VPDG(8),CVPDG_BPTHICK),(MAP_VPDG(1,8),          20
     *MVPDG_BPTHICK)                                                      20
      CHARACTER CVPDG_BPTHICK*40/' '/                                     20
      CHARACTER NVPDG_BPTHICK*12/'R.BPTHICK'/                             20
      INTEGER MVPDG_BPTHICK(2)/1,1/                                       20
      REAL RE_VPDG_BPTHICK                                                20
C -                                                                       20
      REAL VPDG_BXHEIGHT                                                  20
      EQUIVALENCE (BANK_VPDG(11),VPDG_BXHEIGHT,RE_VPDG_BXHEIGHT),         20
     *(NAMES_VPDG(9),NVPDG_BXHEIGHT)                                      20
      EQUIVALENCE (COMMEN_VPDG(9),CVPDG_BXHEIGHT),(MAP_VPDG(1,9),         20
     *MVPDG_BXHEIGHT)                                                     20
      CHARACTER CVPDG_BXHEIGHT*40/' '/                                    20
      CHARACTER NVPDG_BXHEIGHT*12/'R.BXHEIGHT'/                           20
      INTEGER MVPDG_BXHEIGHT(2)/1,1/                                      20
      REAL RE_VPDG_BXHEIGHT                                               20
C -                                                                       20
      REAL VPDG_BXLENGTH                                                  20
      EQUIVALENCE (BANK_VPDG(12),VPDG_BXLENGTH,RE_VPDG_BXLENGTH),         20
     *(NAMES_VPDG(10),NVPDG_BXLENGTH)                                     20
      EQUIVALENCE (COMMEN_VPDG(10),CVPDG_BXLENGTH),(MAP_VPDG(1,10),       20
     *MVPDG_BXLENGTH)                                                     20
      CHARACTER CVPDG_BXLENGTH*40/' '/                                    20
      CHARACTER NVPDG_BXLENGTH*12/'R.BXLENGTH'/                           20
      INTEGER MVPDG_BXLENGTH(2)/1,1/                                      20
      REAL RE_VPDG_BXLENGTH                                               20
C -                                                                       20
      REAL VPDG_BXWIDTH                                                   20
      EQUIVALENCE (BANK_VPDG(13),VPDG_BXWIDTH,RE_VPDG_BXWIDTH),           20
     *(NAMES_VPDG(11),NVPDG_BXWIDTH)                                      20
      EQUIVALENCE (COMMEN_VPDG(11),CVPDG_BXWIDTH),(MAP_VPDG(1,11),        20
     *MVPDG_BXWIDTH)                                                      20
      CHARACTER CVPDG_BXWIDTH*40/' '/                                     20
      CHARACTER NVPDG_BXWIDTH*12/'R.BXWIDTH'/                             20
      INTEGER MVPDG_BXWIDTH(2)/1,1/                                       20
      REAL RE_VPDG_BXWIDTH                                                20
C -                                                                       20
      REAL VPDG_BXTHICK                                                   20
      EQUIVALENCE (BANK_VPDG(14),VPDG_BXTHICK,RE_VPDG_BXTHICK),           20
     *(NAMES_VPDG(12),NVPDG_BXTHICK)                                      20
      EQUIVALENCE (COMMEN_VPDG(12),CVPDG_BXTHICK),(MAP_VPDG(1,12),        20
     *MVPDG_BXTHICK)                                                      20
      CHARACTER CVPDG_BXTHICK*40/' '/                                     20
      CHARACTER NVPDG_BXTHICK*12/'R.BXTHICK'/                             20
      INTEGER MVPDG_BXTHICK(2)/1,1/                                       20
      REAL RE_VPDG_BXTHICK                                                20
C -                                                                       20
      REAL VPDG_BXZPOSC                                                   20
      EQUIVALENCE (BANK_VPDG(15),VPDG_BXZPOSC,RE_VPDG_BXZPOSC),           20
     *(NAMES_VPDG(13),NVPDG_BXZPOSC)                                      20
      EQUIVALENCE (COMMEN_VPDG(13),CVPDG_BXZPOSC),(MAP_VPDG(1,13),        20
     *MVPDG_BXZPOSC)                                                      20
      CHARACTER CVPDG_BXZPOSC*40/' '/                                     20
      CHARACTER NVPDG_BXZPOSC*12/'R.BXZPOSC'/                             20
      INTEGER MVPDG_BXZPOSC(2)/1,1/                                       20
      REAL RE_VPDG_BXZPOSC                                                20
C -                                                                       20
      REAL VPDG_FPWIDTH                                                   20
      EQUIVALENCE (BANK_VPDG(16),VPDG_FPWIDTH,RE_VPDG_FPWIDTH),           20
     *(NAMES_VPDG(14),NVPDG_FPWIDTH)                                      20
      EQUIVALENCE (COMMEN_VPDG(14),CVPDG_FPWIDTH),(MAP_VPDG(1,14),        20
     *MVPDG_FPWIDTH)                                                      20
      CHARACTER CVPDG_FPWIDTH*40/' '/                                     20
      CHARACTER NVPDG_FPWIDTH*12/'R.FPWIDTH'/                             20
      INTEGER MVPDG_FPWIDTH(2)/1,1/                                       20
      REAL RE_VPDG_FPWIDTH                                                20
C -                                                                       20
      REAL VPDG_FPHEIGHT                                                  20
      EQUIVALENCE (BANK_VPDG(17),VPDG_FPHEIGHT,RE_VPDG_FPHEIGHT),         20
     *(NAMES_VPDG(15),NVPDG_FPHEIGHT)                                     20
      EQUIVALENCE (COMMEN_VPDG(15),CVPDG_FPHEIGHT),(MAP_VPDG(1,15),       20
     *MVPDG_FPHEIGHT)                                                     20
      CHARACTER CVPDG_FPHEIGHT*40/' '/                                    20
      CHARACTER NVPDG_FPHEIGHT*12/'R.FPHEIGHT'/                           20
      INTEGER MVPDG_FPHEIGHT(2)/1,1/                                      20
      REAL RE_VPDG_FPHEIGHT                                               20
C -                                                                       20
      REAL VPDG_FPTHICK                                                   20
      EQUIVALENCE (BANK_VPDG(18),VPDG_FPTHICK,RE_VPDG_FPTHICK),           20
     *(NAMES_VPDG(16),NVPDG_FPTHICK)                                      20
      EQUIVALENCE (COMMEN_VPDG(16),CVPDG_FPTHICK),(MAP_VPDG(1,16),        20
     *MVPDG_FPTHICK)                                                      20
      CHARACTER CVPDG_FPTHICK*40/' '/                                     20
      CHARACTER NVPDG_FPTHICK*12/'R.FPTHICK'/                             20
      INTEGER MVPDG_FPTHICK(2)/1,1/                                       20
      REAL RE_VPDG_FPTHICK                                                20
C -                                                                       20
      REAL VPDG_FPAWIDTH                                                  20
      EQUIVALENCE (BANK_VPDG(19),VPDG_FPAWIDTH,RE_VPDG_FPAWIDTH),         20
     *(NAMES_VPDG(17),NVPDG_FPAWIDTH)                                     20
      EQUIVALENCE (COMMEN_VPDG(17),CVPDG_FPAWIDTH),(MAP_VPDG(1,17),       20
     *MVPDG_FPAWIDTH)                                                     20
      CHARACTER CVPDG_FPAWIDTH*40/' '/                                    20
      CHARACTER NVPDG_FPAWIDTH*12/'R.FPAWIDTH'/                           20
      INTEGER MVPDG_FPAWIDTH(2)/1,1/                                      20
      REAL RE_VPDG_FPAWIDTH                                               20
C -                                                                       20
      REAL VPDG_FPAHGHT                                                   20
      EQUIVALENCE (BANK_VPDG(20),VPDG_FPAHGHT,RE_VPDG_FPAHGHT),           20
     *(NAMES_VPDG(18),NVPDG_FPAHGHT)                                      20
      EQUIVALENCE (COMMEN_VPDG(18),CVPDG_FPAHGHT),(MAP_VPDG(1,18),        20
     *MVPDG_FPAHGHT)                                                      20
      CHARACTER CVPDG_FPAHGHT*40/' '/                                     20
      CHARACTER NVPDG_FPAHGHT*12/'R.FPAHGHT'/                             20
      INTEGER MVPDG_FPAHGHT(2)/1,1/                                       20
      REAL RE_VPDG_FPAHGHT                                                20
C -                                                                       20
      REAL VPDG_FPCHGHT                                                   20
      EQUIVALENCE (BANK_VPDG(21),VPDG_FPCHGHT,RE_VPDG_FPCHGHT),           20
     *(NAMES_VPDG(19),NVPDG_FPCHGHT)                                      20
      EQUIVALENCE (COMMEN_VPDG(19),CVPDG_FPCHGHT),(MAP_VPDG(1,19),        20
     *MVPDG_FPCHGHT)                                                      20
      CHARACTER CVPDG_FPCHGHT*40/' '/                                     20
      CHARACTER NVPDG_FPCHGHT*12/'R.FPCHGHT'/                             20
      INTEGER MVPDG_FPCHGHT(2)/1,1/                                       20
      REAL RE_VPDG_FPCHGHT                                                20
C -                                                                       20
      REAL VPDG_FPHHGHT                                                   20
      EQUIVALENCE (BANK_VPDG(22),VPDG_FPHHGHT,RE_VPDG_FPHHGHT),           20
     *(NAMES_VPDG(20),NVPDG_FPHHGHT)                                      20
      EQUIVALENCE (COMMEN_VPDG(20),CVPDG_FPHHGHT),(MAP_VPDG(1,20),        20
     *MVPDG_FPHHGHT)                                                      20
      CHARACTER CVPDG_FPHHGHT*40/' '/                                     20
      CHARACTER NVPDG_FPHHGHT*12/'R.FPHHGHT'/                             20
      INTEGER MVPDG_FPHHGHT(2)/1,1/                                       20
      REAL RE_VPDG_FPHHGHT                                                20
C -                                                                       20
      REAL VPDG_FPHWIDTH                                                  20
      EQUIVALENCE (BANK_VPDG(23),VPDG_FPHWIDTH,RE_VPDG_FPHWIDTH),         20
     *(NAMES_VPDG(21),NVPDG_FPHWIDTH)                                     20
      EQUIVALENCE (COMMEN_VPDG(21),CVPDG_FPHWIDTH),(MAP_VPDG(1,21),       20
     *MVPDG_FPHWIDTH)                                                     20
      CHARACTER CVPDG_FPHWIDTH*40/' '/                                    20
      CHARACTER NVPDG_FPHWIDTH*12/'R.FPHWIDTH'/                           20
      INTEGER MVPDG_FPHWIDTH(2)/1,1/                                      20
      REAL RE_VPDG_FPHWIDTH                                               20
C -                                                                       20
      REAL VPDG_FPHTHICK                                                  20
      EQUIVALENCE (BANK_VPDG(24),VPDG_FPHTHICK,RE_VPDG_FPHTHICK),         20
     *(NAMES_VPDG(22),NVPDG_FPHTHICK)                                     20
      EQUIVALENCE (COMMEN_VPDG(22),CVPDG_FPHTHICK),(MAP_VPDG(1,22),       20
     *MVPDG_FPHTHICK)                                                     20
      CHARACTER CVPDG_FPHTHICK*40/' '/                                    20
      CHARACTER NVPDG_FPHTHICK*12/'R.FPHTHICK'/                           20
      INTEGER MVPDG_FPHTHICK(2)/1,1/                                      20
      REAL RE_VPDG_FPHTHICK                                               20
C -                                                                       20
      REAL VPDG_STTHICK                                                   20
      EQUIVALENCE (BANK_VPDG(25),VPDG_STTHICK,RE_VPDG_STTHICK),           20
     *(NAMES_VPDG(23),NVPDG_STTHICK)                                      20
      EQUIVALENCE (COMMEN_VPDG(23),CVPDG_STTHICK),(MAP_VPDG(1,23),        20
     *MVPDG_STTHICK)                                                      20
      CHARACTER CVPDG_STTHICK*40/' '/                                     20
      CHARACTER NVPDG_STTHICK*12/'R.STTHICK'/                             20
      INTEGER MVPDG_STTHICK(2)/1,1/                                       20
      REAL RE_VPDG_STTHICK                                                20
C -                                                                       20
      REAL VPDG_STHEIGHT                                                  20
      EQUIVALENCE (BANK_VPDG(26),VPDG_STHEIGHT,RE_VPDG_STHEIGHT),         20
     *(NAMES_VPDG(24),NVPDG_STHEIGHT)                                     20
      EQUIVALENCE (COMMEN_VPDG(24),CVPDG_STHEIGHT),(MAP_VPDG(1,24),       20
     *MVPDG_STHEIGHT)                                                     20
      CHARACTER CVPDG_STHEIGHT*40/' '/                                    20
      CHARACTER NVPDG_STHEIGHT*12/'R.STHEIGHT'/                           20
      INTEGER MVPDG_STHEIGHT(2)/1,1/                                      20
      REAL RE_VPDG_STHEIGHT                                               20
C -                                                                       20
      REAL VPDG_STANGLE                                                   20
      EQUIVALENCE (BANK_VPDG(27),VPDG_STANGLE,RE_VPDG_STANGLE),           20
     *(NAMES_VPDG(25),NVPDG_STANGLE)                                      20
      EQUIVALENCE (COMMEN_VPDG(25),CVPDG_STANGLE),(MAP_VPDG(1,25),        20
     *MVPDG_STANGLE)                                                      20
      CHARACTER CVPDG_STANGLE*40/' '/                                     20
      CHARACTER NVPDG_STANGLE*12/'R.STANGLE'/                             20
      INTEGER MVPDG_STANGLE(2)/1,1/                                       20
      REAL RE_VPDG_STANGLE                                                20
C -                                                                       20
      REAL VPDG_STDIAGSZ                                                  20
      EQUIVALENCE (BANK_VPDG(28),VPDG_STDIAGSZ,RE_VPDG_STDIAGSZ),         20
     *(NAMES_VPDG(26),NVPDG_STDIAGSZ)                                     20
      EQUIVALENCE (COMMEN_VPDG(26),CVPDG_STDIAGSZ),(MAP_VPDG(1,26),       20
     *MVPDG_STDIAGSZ)                                                     20
      CHARACTER CVPDG_STDIAGSZ*40/' '/                                    20
      CHARACTER NVPDG_STDIAGSZ*12/'R.STDIAGSZ'/                           20
      INTEGER MVPDG_STDIAGSZ(2)/1,1/                                      20
      REAL RE_VPDG_STDIAGSZ                                               20
C -                                                                       20
      REAL VPDG_SCWIDTH                                                   20
      EQUIVALENCE (BANK_VPDG(29),VPDG_SCWIDTH,RE_VPDG_SCWIDTH),           20
     *(NAMES_VPDG(27),NVPDG_SCWIDTH)                                      20
      EQUIVALENCE (COMMEN_VPDG(27),CVPDG_SCWIDTH),(MAP_VPDG(1,27),        20
     *MVPDG_SCWIDTH)                                                      20
      CHARACTER CVPDG_SCWIDTH*40/' '/                                     20
      CHARACTER NVPDG_SCWIDTH*12/'R.SCWIDTH'/                             20
      INTEGER MVPDG_SCWIDTH(2)/1,1/                                       20
      REAL RE_VPDG_SCWIDTH                                                20
C -                                                                       20
      REAL VPDG_SCHEIGHT                                                  20
      EQUIVALENCE (BANK_VPDG(30),VPDG_SCHEIGHT,RE_VPDG_SCHEIGHT),         20
     *(NAMES_VPDG(28),NVPDG_SCHEIGHT)                                     20
      EQUIVALENCE (COMMEN_VPDG(28),CVPDG_SCHEIGHT),(MAP_VPDG(1,28),       20
     *MVPDG_SCHEIGHT)                                                     20
      CHARACTER CVPDG_SCHEIGHT*40/' '/                                    20
      CHARACTER NVPDG_SCHEIGHT*12/'R.SCHEIGHT'/                           20
      INTEGER MVPDG_SCHEIGHT(2)/1,1/                                      20
      REAL RE_VPDG_SCHEIGHT                                               20
C -                                                                       20
      REAL VPDG_SCLENGTH                                                  20
      EQUIVALENCE (BANK_VPDG(31),VPDG_SCLENGTH,RE_VPDG_SCLENGTH),         20
     *(NAMES_VPDG(29),NVPDG_SCLENGTH)                                     20
      EQUIVALENCE (COMMEN_VPDG(29),CVPDG_SCLENGTH),(MAP_VPDG(1,29),       20
     *MVPDG_SCLENGTH)                                                     20
      CHARACTER CVPDG_SCLENGTH*40/' '/                                    20
      CHARACTER NVPDG_SCLENGTH*12/'R.SCLENGTH'/                           20
      INTEGER MVPDG_SCLENGTH(2)/1,1/                                      20
      REAL RE_VPDG_SCLENGTH                                               20
C -                                                                       20
      REAL VPDG_SCTHICK                                                   20
      EQUIVALENCE (BANK_VPDG(32),VPDG_SCTHICK,RE_VPDG_SCTHICK),           20
     *(NAMES_VPDG(30),NVPDG_SCTHICK)                                      20
      EQUIVALENCE (COMMEN_VPDG(30),CVPDG_SCTHICK),(MAP_VPDG(1,30),        20
     *MVPDG_SCTHICK)                                                      20
      CHARACTER CVPDG_SCTHICK*40/' '/                                     20
      CHARACTER NVPDG_SCTHICK*12/'R.SCTHICK'/                             20
      INTEGER MVPDG_SCTHICK(2)/1,1/                                       20
      REAL RE_VPDG_SCTHICK                                                20
C -                                                                       20
      REAL VPDG_CLHEIGHT                                                  20
      EQUIVALENCE (BANK_VPDG(33),VPDG_CLHEIGHT,RE_VPDG_CLHEIGHT),         20
     *(NAMES_VPDG(31),NVPDG_CLHEIGHT)                                     20
      EQUIVALENCE (COMMEN_VPDG(31),CVPDG_CLHEIGHT),(MAP_VPDG(1,31),       20
     *MVPDG_CLHEIGHT)                                                     20
      CHARACTER CVPDG_CLHEIGHT*40/' '/                                    20
      CHARACTER NVPDG_CLHEIGHT*12/'R.CLHEIGHT'/                           20
      INTEGER MVPDG_CLHEIGHT(2)/1,1/                                      20
      REAL RE_VPDG_CLHEIGHT                                               20
C -                                                                       20
      REAL VPDG_CLWIDTH                                                   20
      EQUIVALENCE (BANK_VPDG(34),VPDG_CLWIDTH,RE_VPDG_CLWIDTH),           20
     *(NAMES_VPDG(32),NVPDG_CLWIDTH)                                      20
      EQUIVALENCE (COMMEN_VPDG(32),CVPDG_CLWIDTH),(MAP_VPDG(1,32),        20
     *MVPDG_CLWIDTH)                                                      20
      CHARACTER CVPDG_CLWIDTH*40/' '/                                     20
      CHARACTER NVPDG_CLWIDTH*12/'R.CLWIDTH'/                             20
      INTEGER MVPDG_CLWIDTH(2)/1,1/                                       20
      REAL RE_VPDG_CLWIDTH                                                20
C -                                                                       20
      REAL VPDG_CLLENGTH                                                  20
      EQUIVALENCE (BANK_VPDG(35),VPDG_CLLENGTH,RE_VPDG_CLLENGTH),         20
     *(NAMES_VPDG(33),NVPDG_CLLENGTH)                                     20
      EQUIVALENCE (COMMEN_VPDG(33),CVPDG_CLLENGTH),(MAP_VPDG(1,33),       20
     *MVPDG_CLLENGTH)                                                     20
      CHARACTER CVPDG_CLLENGTH*40/' '/                                    20
      CHARACTER NVPDG_CLLENGTH*12/'R.CLLENGTH'/                           20
      INTEGER MVPDG_CLLENGTH(2)/1,1/                                      20
      REAL RE_VPDG_CLLENGTH                                               20
C -                                                                       20
      REAL VPDG_CLTHICK                                                   20
      EQUIVALENCE (BANK_VPDG(36),VPDG_CLTHICK,RE_VPDG_CLTHICK),           20
     *(NAMES_VPDG(34),NVPDG_CLTHICK)                                      20
      EQUIVALENCE (COMMEN_VPDG(34),CVPDG_CLTHICK),(MAP_VPDG(1,34),        20
     *MVPDG_CLTHICK)                                                      20
      CHARACTER CVPDG_CLTHICK*40/' '/                                     20
      CHARACTER NVPDG_CLTHICK*12/'R.CLTHICK'/                             20
      INTEGER MVPDG_CLTHICK(2)/1,1/                                       20
      REAL RE_VPDG_CLTHICK                                                20
C -                                                                       20
      REAL VPDG_DETLEN                                                    20
      EQUIVALENCE (BANK_VPDG(37),VPDG_DETLEN,RE_VPDG_DETLEN),             20
     *(NAMES_VPDG(35),NVPDG_DETLEN)                                       20
      EQUIVALENCE (COMMEN_VPDG(35),CVPDG_DETLEN),(MAP_VPDG(1,35),         20
     *MVPDG_DETLEN)                                                       20
      CHARACTER CVPDG_DETLEN*40/' '/                                      20
      CHARACTER NVPDG_DETLEN*12/'R.DETLEN'/                               20
      INTEGER MVPDG_DETLEN(2)/1,1/                                        20
      REAL RE_VPDG_DETLEN                                                 20
C -                                                                       20
      REAL VPDG_DETRAD                                                    20
      EQUIVALENCE (BANK_VPDG(38),VPDG_DETRAD,RE_VPDG_DETRAD),             20
     *(NAMES_VPDG(36),NVPDG_DETRAD)                                       20
      EQUIVALENCE (COMMEN_VPDG(36),CVPDG_DETRAD),(MAP_VPDG(1,36),         20
     *MVPDG_DETRAD)                                                       20
      CHARACTER CVPDG_DETRAD*40/' '/                                      20
      CHARACTER NVPDG_DETRAD*12/'R.DETRAD'/                               20
      INTEGER MVPDG_DETRAD(2)/1,1/                                        20
      REAL RE_VPDG_DETRAD                                                 20
C -                                                                       20
      REAL VPDG_DETFRONT                                                  20
      EQUIVALENCE (BANK_VPDG(39),VPDG_DETFRONT,RE_VPDG_DETFRONT),         20
     *(NAMES_VPDG(37),NVPDG_DETFRONT)                                     20
      EQUIVALENCE (COMMEN_VPDG(37),CVPDG_DETFRONT),(MAP_VPDG(1,37),       20
     *MVPDG_DETFRONT)                                                     20
      CHARACTER CVPDG_DETFRONT*40/' '/                                    20
      CHARACTER NVPDG_DETFRONT*12/'R.DETFRONT'/                           20
      INTEGER MVPDG_DETFRONT(2)/1,1/                                      20
      REAL RE_VPDG_DETFRONT                                               20
C -                                                                       20
      REAL VPDG_CONVTHK                                                   20
      EQUIVALENCE (BANK_VPDG(40),VPDG_CONVTHK,RE_VPDG_CONVTHK),           20
     *(NAMES_VPDG(38),NVPDG_CONVTHK)                                      20
      EQUIVALENCE (COMMEN_VPDG(38),CVPDG_CONVTHK),(MAP_VPDG(1,38),        20
     *MVPDG_CONVTHK)                                                      20
      CHARACTER CVPDG_CONVTHK*40/' '/                                     20
      CHARACTER NVPDG_CONVTHK*12/'R.CONVTHK'/                             20
      INTEGER MVPDG_CONVTHK(2)/1,1/                                       20
      REAL RE_VPDG_CONVTHK                                                20
C -                                                                       20
      REAL VPDG_RADITHK                                                   20
      EQUIVALENCE (BANK_VPDG(41),VPDG_RADITHK,RE_VPDG_RADITHK),           20
     *(NAMES_VPDG(39),NVPDG_RADITHK)                                      20
      EQUIVALENCE (COMMEN_VPDG(39),CVPDG_RADITHK),(MAP_VPDG(1,39),        20
     *MVPDG_RADITHK)                                                      20
      CHARACTER CVPDG_RADITHK*40/' '/                                     20
      CHARACTER NVPDG_RADITHK*12/'R.RADITHK'/                             20
      INTEGER MVPDG_RADITHK(2)/1,1/                                       20
      REAL RE_VPDG_RADITHK                                                20
C -                                                                       20
      REAL VPDG_ELELENG                                                   20
      EQUIVALENCE (BANK_VPDG(42),VPDG_ELELENG,RE_VPDG_ELELENG),           20
     *(NAMES_VPDG(40),NVPDG_ELELENG)                                      20
      EQUIVALENCE (COMMEN_VPDG(40),CVPDG_ELELENG),(MAP_VPDG(1,40),        20
     *MVPDG_ELELENG)                                                      20
      CHARACTER CVPDG_ELELENG*40/' '/                                     20
      CHARACTER NVPDG_ELELENG*12/'R.ELELENG'/                             20
      INTEGER MVPDG_ELELENG(2)/1,1/                                       20
      REAL RE_VPDG_ELELENG                                                20
C -                                                                       20
      REAL VPDG_DRLAYER                                                   20
      EQUIVALENCE (BANK_VPDG(43),VPDG_DRLAYER,RE_VPDG_DRLAYER),           20
     *(NAMES_VPDG(41),NVPDG_DRLAYER)                                      20
      EQUIVALENCE (COMMEN_VPDG(41),CVPDG_DRLAYER),(MAP_VPDG(1,41),        20
     *MVPDG_DRLAYER)                                                      20
      CHARACTER CVPDG_DRLAYER*40/' '/                                     20
      CHARACTER NVPDG_DRLAYER*12/'R.DRLAYER'/                             20
      INTEGER MVPDG_DRLAYER(2)/1,1/                                       20
      REAL RE_VPDG_DRLAYER                                                20
C -                                                                       20
      REAL VPDG_NUMPMT                                                    20
      EQUIVALENCE (BANK_VPDG(44),VPDG_NUMPMT,RE_VPDG_NUMPMT),             20
     *(NAMES_VPDG(42),NVPDG_NUMPMT)                                       20
      EQUIVALENCE (COMMEN_VPDG(42),CVPDG_NUMPMT),(MAP_VPDG(1,42),         20
     *MVPDG_NUMPMT)                                                       20
      CHARACTER CVPDG_NUMPMT*40/' '/                                      20
      CHARACTER NVPDG_NUMPMT*12/'R.NUMPMT'/                               20
      INTEGER MVPDG_NUMPMT(2)/1,1/                                        20
      REAL RE_VPDG_NUMPMT                                                 20
C -                                                                       20
      REAL VPDG_PMTWALL                                                   20
      EQUIVALENCE (BANK_VPDG(45),VPDG_PMTWALL,RE_VPDG_PMTWALL),           20
     *(NAMES_VPDG(43),NVPDG_PMTWALL)                                      20
      EQUIVALENCE (COMMEN_VPDG(43),CVPDG_PMTWALL),(MAP_VPDG(1,43),        20
     *MVPDG_PMTWALL)                                                      20
      CHARACTER CVPDG_PMTWALL*40/' '/                                     20
      CHARACTER NVPDG_PMTWALL*12/'R.PMTWALL'/                             20
      INTEGER MVPDG_PMTWALL(2)/1,1/                                       20
      REAL RE_VPDG_PMTWALL                                                20
C -                                                                       20
      REAL VPDG_PMTRAD                                                    20
      EQUIVALENCE (BANK_VPDG(46),VPDG_PMTRAD,RE_VPDG_PMTRAD),             20
     *(NAMES_VPDG(44),NVPDG_PMTRAD)                                       20
      EQUIVALENCE (COMMEN_VPDG(44),CVPDG_PMTRAD),(MAP_VPDG(1,44),         20
     *MVPDG_PMTRAD)                                                       20
      CHARACTER CVPDG_PMTRAD*40/' '/                                      20
      CHARACTER NVPDG_PMTRAD*12/'R.PMTRAD'/                               20
      INTEGER MVPDG_PMTRAD(2)/1,1/                                        20
      REAL RE_VPDG_PMTRAD                                                 20
C -                                                                       20
      REAL VPDG_PMTLEN                                                    20
      EQUIVALENCE (BANK_VPDG(47),VPDG_PMTLEN,RE_VPDG_PMTLEN),             20
     *(NAMES_VPDG(45),NVPDG_PMTLEN)                                       20
      EQUIVALENCE (COMMEN_VPDG(45),CVPDG_PMTLEN),(MAP_VPDG(1,45),         20
     *MVPDG_PMTLEN)                                                       20
      CHARACTER CVPDG_PMTLEN*40/' '/                                      20
      CHARACTER NVPDG_PMTLEN*12/'R.PMTLEN'/                               20
      INTEGER MVPDG_PMTLEN(2)/1,1/                                        20
      REAL RE_VPDG_PMTLEN                                                 20
C -                                                                       20
      REAL VPDG_IBCHOICE                                                  20
      EQUIVALENCE (BANK_VPDG(48),VPDG_IBCHOICE,RE_VPDG_IBCHOICE),         20
     *(NAMES_VPDG(46),NVPDG_IBCHOICE)                                     20
      EQUIVALENCE (COMMEN_VPDG(46),CVPDG_IBCHOICE),(MAP_VPDG(1,46),       20
     *MVPDG_IBCHOICE)                                                     20
      CHARACTER CVPDG_IBCHOICE*40/' '/                                    20
      CHARACTER NVPDG_IBCHOICE*12/'R.IBCHOICE'/                           20
      INTEGER MVPDG_IBCHOICE(2)/1,1/                                      20
      REAL RE_VPDG_IBCHOICE                                               20
C -                                                                       20
      REAL VPDG_IBPOSYC                                                   20
      EQUIVALENCE (BANK_VPDG(49),VPDG_IBPOSYC,RE_VPDG_IBPOSYC),           20
     *(NAMES_VPDG(47),NVPDG_IBPOSYC)                                      20
      EQUIVALENCE (COMMEN_VPDG(47),CVPDG_IBPOSYC),(MAP_VPDG(1,47),        20
     *MVPDG_IBPOSYC)                                                      20
      CHARACTER CVPDG_IBPOSYC*40/' '/                                     20
      CHARACTER NVPDG_IBPOSYC*12/'R.IBPOSYC'/                             20
      INTEGER MVPDG_IBPOSYC(2)/1,1/                                       20
      REAL RE_VPDG_IBPOSYC                                                20
C -                                                                       20
      REAL VPDG_IBPOSZC                                                   20
      EQUIVALENCE (BANK_VPDG(50),VPDG_IBPOSZC,RE_VPDG_IBPOSZC),           20
     *(NAMES_VPDG(48),NVPDG_IBPOSZC)                                      20
      EQUIVALENCE (COMMEN_VPDG(48),CVPDG_IBPOSZC),(MAP_VPDG(1,48),        20
     *MVPDG_IBPOSZC)                                                      20
      CHARACTER CVPDG_IBPOSZC*40/' '/                                     20
      CHARACTER NVPDG_IBPOSZC*12/'R.IBPOSZC'/                             20
      INTEGER MVPDG_IBPOSZC(2)/1,1/                                       20
      REAL RE_VPDG_IBPOSZC                                                20
C -                                                                       20
      REAL VPDG_IBLENG                                                    20
      EQUIVALENCE (BANK_VPDG(51),VPDG_IBLENG,RE_VPDG_IBLENG),             20
     *(NAMES_VPDG(49),NVPDG_IBLENG)                                       20
      EQUIVALENCE (COMMEN_VPDG(49),CVPDG_IBLENG),(MAP_VPDG(1,49),         20
     *MVPDG_IBLENG)                                                       20
      CHARACTER CVPDG_IBLENG*40/' '/                                      20
      CHARACTER NVPDG_IBLENG*12/'R.IBLENG'/                               20
      INTEGER MVPDG_IBLENG(2)/1,1/                                        20
      REAL RE_VPDG_IBLENG                                                 20
C -                                                                       20
      REAL VPDG_IBTHICKH                                                  20
      EQUIVALENCE (BANK_VPDG(52),VPDG_IBTHICKH,RE_VPDG_IBTHICKH),         20
     *(NAMES_VPDG(50),NVPDG_IBTHICKH)                                     20
      EQUIVALENCE (COMMEN_VPDG(50),CVPDG_IBTHICKH),(MAP_VPDG(1,50),       20
     *MVPDG_IBTHICKH)                                                     20
      CHARACTER CVPDG_IBTHICKH*40/' '/                                    20
      CHARACTER NVPDG_IBTHICKH*12/'R.IBTHICKH'/                           20
      INTEGER MVPDG_IBTHICKH(2)/1,1/                                      20
      REAL RE_VPDG_IBTHICKH                                               20
C -                                                                       20
      REAL VPDG_IBTHICKV                                                  20
      EQUIVALENCE (BANK_VPDG(53),VPDG_IBTHICKV,RE_VPDG_IBTHICKV),         20
     *(NAMES_VPDG(51),NVPDG_IBTHICKV)                                     20
      EQUIVALENCE (COMMEN_VPDG(51),CVPDG_IBTHICKV),(MAP_VPDG(1,51),       20
     *MVPDG_IBTHICKV)                                                     20
      CHARACTER CVPDG_IBTHICKV*40/' '/                                    20
      CHARACTER NVPDG_IBTHICKV*12/'R.IBTHICKV'/                           20
      INTEGER MVPDG_IBTHICKV(2)/1,1/                                      20
      REAL RE_VPDG_IBTHICKV                                               20
C -                                                                       20
      REAL VPDG_IBHEIGHT                                                  20
      EQUIVALENCE (BANK_VPDG(54),VPDG_IBHEIGHT,RE_VPDG_IBHEIGHT),         20
     *(NAMES_VPDG(52),NVPDG_IBHEIGHT)                                     20
      EQUIVALENCE (COMMEN_VPDG(52),CVPDG_IBHEIGHT),(MAP_VPDG(1,52),       20
     *MVPDG_IBHEIGHT)                                                     20
      CHARACTER CVPDG_IBHEIGHT*40/' '/                                    20
      CHARACTER NVPDG_IBHEIGHT*12/'R.IBHEIGHT'/                           20
      INTEGER MVPDG_IBHEIGHT(2)/1,1/                                      20
      REAL RE_VPDG_IBHEIGHT                                               20
C -                                                                       20
      REAL VPDG_IBWIDTH                                                   20
      EQUIVALENCE (BANK_VPDG(55),VPDG_IBWIDTH,RE_VPDG_IBWIDTH),           20
     *(NAMES_VPDG(53),NVPDG_IBWIDTH)                                      20
      EQUIVALENCE (COMMEN_VPDG(53),CVPDG_IBWIDTH),(MAP_VPDG(1,53),        20
     *MVPDG_IBWIDTH)                                                      20
      CHARACTER CVPDG_IBWIDTH*40/' '/                                     20
      CHARACTER NVPDG_IBWIDTH*12/'R.IBWIDTH'/                             20
      INTEGER MVPDG_IBWIDTH(2)/1,1/                                       20
      REAL RE_VPDG_IBWIDTH                                                20
C -                                                                       20
      REAL VPDG_IBWLEN                                                    20
      EQUIVALENCE (BANK_VPDG(56),VPDG_IBWLEN,RE_VPDG_IBWLEN),             20
     *(NAMES_VPDG(54),NVPDG_IBWLEN)                                       20
      EQUIVALENCE (COMMEN_VPDG(54),CVPDG_IBWLEN),(MAP_VPDG(1,54),         20
     *MVPDG_IBWLEN)                                                       20
      CHARACTER CVPDG_IBWLEN*40/' '/                                      20
      CHARACTER NVPDG_IBWLEN*12/'R.IBWLEN'/                               20
      INTEGER MVPDG_IBWLEN(2)/1,1/                                        20
      REAL RE_VPDG_IBWLEN                                                 20
C -                                                                       20
      REAL VPDG_IBWHGHTF                                                  20
      EQUIVALENCE (BANK_VPDG(57),VPDG_IBWHGHTF,RE_VPDG_IBWHGHTF),         20
     *(NAMES_VPDG(55),NVPDG_IBWHGHTF)                                     20
      EQUIVALENCE (COMMEN_VPDG(55),CVPDG_IBWHGHTF),(MAP_VPDG(1,55),       20
     *MVPDG_IBWHGHTF)                                                     20
      CHARACTER CVPDG_IBWHGHTF*40/' '/                                    20
      CHARACTER NVPDG_IBWHGHTF*12/'R.IBWHGHTF'/                           20
      INTEGER MVPDG_IBWHGHTF(2)/1,1/                                      20
      REAL RE_VPDG_IBWHGHTF                                               20
C -                                                                       20
      REAL VPDG_IBWHGHTB                                                  20
      EQUIVALENCE (BANK_VPDG(58),VPDG_IBWHGHTB,RE_VPDG_IBWHGHTB),         20
     *(NAMES_VPDG(56),NVPDG_IBWHGHTB)                                     20
      EQUIVALENCE (COMMEN_VPDG(56),CVPDG_IBWHGHTB),(MAP_VPDG(1,56),       20
     *MVPDG_IBWHGHTB)                                                     20
      CHARACTER CVPDG_IBWHGHTB*40/' '/                                    20
      CHARACTER NVPDG_IBWHGHTB*12/'R.IBWHGHTB'/                           20
      INTEGER MVPDG_IBWHGHTB(2)/1,1/                                      20
      REAL RE_VPDG_IBWHGHTB                                               20
C -                                                                       20
      REAL VPDG_EWSHIFT                                                   20
      EQUIVALENCE (BANK_VPDG(59),VPDG_EWSHIFT,RE_VPDG_EWSHIFT),           20
     *(NAMES_VPDG(57),NVPDG_EWSHIFT)                                      20
      EQUIVALENCE (COMMEN_VPDG(57),CVPDG_EWSHIFT),(MAP_VPDG(1,57),        20
     *MVPDG_EWSHIFT)                                                      20
      CHARACTER CVPDG_EWSHIFT*40/' '/                                     20
      CHARACTER NVPDG_EWSHIFT*12/'R.EWSHIFT'/                             20
      INTEGER MVPDG_EWSHIFT(2)/1,1/                                       20
      REAL RE_VPDG_EWSHIFT                                                20
C -                                                                       20
      REAL VPDG_UDSHIFT                                                   20
      EQUIVALENCE (BANK_VPDG(60),VPDG_UDSHIFT,RE_VPDG_UDSHIFT),           20
     *(NAMES_VPDG(58),NVPDG_UDSHIFT)                                      20
      EQUIVALENCE (COMMEN_VPDG(58),CVPDG_UDSHIFT),(MAP_VPDG(1,58),        20
     *MVPDG_UDSHIFT)                                                      20
      CHARACTER CVPDG_UDSHIFT*40/' '/                                     20
      CHARACTER NVPDG_UDSHIFT*12/'R.UDSHIFT'/                             20
      INTEGER MVPDG_UDSHIFT(2)/1,1/                                       20
      REAL RE_VPDG_UDSHIFT                                                20
C -                                                                       20
      REAL VPDG_BOLTSHIFT                                                 20
      EQUIVALENCE (BANK_VPDG(61),VPDG_BOLTSHIFT,RE_VPDG_BOLTSHIFT),       20
     *(NAMES_VPDG(59),NVPDG_BOLTSHIFT)                                    20
      EQUIVALENCE (COMMEN_VPDG(59),CVPDG_BOLTSHIFT),(MAP_VPDG(1,59),      20
     *MVPDG_BOLTSHIFT)                                                    20
      CHARACTER CVPDG_BOLTSHIFT*40/' '/                                   20
      CHARACTER NVPDG_BOLTSHIFT*12/'R.BOLTSHIFT'/                         20
      INTEGER MVPDG_BOLTSHIFT(2)/1,1/                                     20
      REAL RE_VPDG_BOLTSHIFT                                              20
C -                                                                       20
      INTEGER LEN_VPDG,LLEN_VPDG,LINK_VPDG                                20
      PARAMETER (LEN_VPDG=61)                                             20
      PARAMETER (LLEN_VPDG=59)                                            20
      INTEGER FLAG_VPDG/0/,MAP_VPDG(2,LLEN_VPDG),IDNUM_VPDG(10)           20
      CHARACTER NAMES_VPDG(LLEN_VPDG)*12,COMMEN_VPDG(LLEN_VPDG)*40        20
      DATA IDNUM_VPDG/10*0/                                               20
      REAL BANK_VPDG(LEN_VPDG)                                            20
      CHARACTER*60 BPATH_VPDG/' '/                                        20
      SAVE FLAG_VPDG,MAP_VPDG,IDNUM_VPDG,NAMES_VPDG,COMMEN_VPDG,          20
     *BPATH_VPDG                                                          20
      DATA LINK_VPDG/0/,BANK_VPDG/LEN_VPDG*0/                             20
C *                                                                       20
C -                                                                       24
      REAL VPDH_VERSION                                                   24
      EQUIVALENCE (BANK_VPDH(3),VPDH_VERSION,RE_VPDH_VERSION),            24
     *(NAMES_VPDH(1),NVPDH_VERSION)                                       24
      EQUIVALENCE (COMMEN_VPDH(1),CVPDH_VERSION),(MAP_VPDH(1,1),          24
     *MVPDH_VERSION)                                                      24
      CHARACTER CVPDH_VERSION*40/' '/                                     24
      CHARACTER NVPDH_VERSION*12/'R.VERSION'/                             24
      INTEGER MVPDH_VERSION(2)/1,1/                                       24
      REAL RE_VPDH_VERSION                                                24
C -                                                                       24
      REAL VPDH_ZPOSEAST                                                  24
      EQUIVALENCE (BANK_VPDH(4),VPDH_ZPOSEAST,RE_VPDH_ZPOSEAST),          24
     *(NAMES_VPDH(2),NVPDH_ZPOSEAST)                                      24
      EQUIVALENCE (COMMEN_VPDH(2),CVPDH_ZPOSEAST),(MAP_VPDH(1,2),         24
     *MVPDH_ZPOSEAST)                                                     24
      CHARACTER CVPDH_ZPOSEAST*40/' '/                                    24
      CHARACTER NVPDH_ZPOSEAST*12/'R.ZPOSEAST'/                           24
      INTEGER MVPDH_ZPOSEAST(2)/1,1/                                      24
      REAL RE_VPDH_ZPOSEAST                                               24
C -                                                                       24
      REAL VPDH_ZPOSWEST                                                  24
      EQUIVALENCE (BANK_VPDH(5),VPDH_ZPOSWEST,RE_VPDH_ZPOSWEST),          24
     *(NAMES_VPDH(3),NVPDH_ZPOSWEST)                                      24
      EQUIVALENCE (COMMEN_VPDH(3),CVPDH_ZPOSWEST),(MAP_VPDH(1,3),         24
     *MVPDH_ZPOSWEST)                                                     24
      CHARACTER CVPDH_ZPOSWEST*40/' '/                                    24
      CHARACTER NVPDH_ZPOSWEST*12/'R.ZPOSWEST'/                           24
      INTEGER MVPDH_ZPOSWEST(2)/1,1/                                      24
      REAL RE_VPDH_ZPOSWEST                                               24
C -                                                                       24
      REAL VPDH_RMIN                                                      24
      EQUIVALENCE (BANK_VPDH(6),VPDH_RMIN,RE_VPDH_RMIN),(NAMES_VPDH(4),   24
     *NVPDH_RMIN)                                                         24
      EQUIVALENCE (COMMEN_VPDH(4),CVPDH_RMIN),(MAP_VPDH(1,4),             24
     *MVPDH_RMIN)                                                         24
      CHARACTER CVPDH_RMIN*40/' '/                                        24
      CHARACTER NVPDH_RMIN*12/'R.RMIN'/                                   24
      INTEGER MVPDH_RMIN(2)/1,1/                                          24
      REAL RE_VPDH_RMIN                                                   24
C -                                                                       24
      REAL VPDH_RMAX                                                      24
      EQUIVALENCE (BANK_VPDH(7),VPDH_RMAX,RE_VPDH_RMAX),(NAMES_VPDH(5),   24
     *NVPDH_RMAX)                                                         24
      EQUIVALENCE (COMMEN_VPDH(5),CVPDH_RMAX),(MAP_VPDH(1,5),             24
     *MVPDH_RMAX)                                                         24
      CHARACTER CVPDH_RMAX*40/' '/                                        24
      CHARACTER NVPDH_RMAX*12/'R.RMAX'/                                   24
      INTEGER MVPDH_RMAX(2)/1,1/                                          24
      REAL RE_VPDH_RMAX                                                   24
C -                                                                       24
      REAL VPDH_LENGTH                                                    24
      EQUIVALENCE (BANK_VPDH(8),VPDH_LENGTH,RE_VPDH_LENGTH),              24
     *(NAMES_VPDH(6),NVPDH_LENGTH)                                        24
      EQUIVALENCE (COMMEN_VPDH(6),CVPDH_LENGTH),(MAP_VPDH(1,6),           24
     *MVPDH_LENGTH)                                                       24
      CHARACTER CVPDH_LENGTH*40/' '/                                      24
      CHARACTER NVPDH_LENGTH*12/'R.LENGTH'/                               24
      INTEGER MVPDH_LENGTH(2)/1,1/                                        24
      REAL RE_VPDH_LENGTH                                                 24
C -                                                                       24
      REAL VPDH_DETRAD                                                    24
      EQUIVALENCE (BANK_VPDH(9),VPDH_DETRAD,RE_VPDH_DETRAD),              24
     *(NAMES_VPDH(7),NVPDH_DETRAD)                                        24
      EQUIVALENCE (COMMEN_VPDH(7),CVPDH_DETRAD),(MAP_VPDH(1,7),           24
     *MVPDH_DETRAD)                                                       24
      CHARACTER CVPDH_DETRAD*40/' '/                                      24
      CHARACTER NVPDH_DETRAD*12/'R.DETRAD'/                               24
      INTEGER MVPDH_DETRAD(2)/1,1/                                        24
      REAL RE_VPDH_DETRAD                                                 24
C -                                                                       24
      REAL VPDH_DETLEN                                                    24
      EQUIVALENCE (BANK_VPDH(10),VPDH_DETLEN,RE_VPDH_DETLEN),             24
     *(NAMES_VPDH(8),NVPDH_DETLEN)                                        24
      EQUIVALENCE (COMMEN_VPDH(8),CVPDH_DETLEN),(MAP_VPDH(1,8),           24
     *MVPDH_DETLEN)                                                       24
      CHARACTER CVPDH_DETLEN*40/' '/                                      24
      CHARACTER NVPDH_DETLEN*12/'R.DETLEN'/                               24
      INTEGER MVPDH_DETLEN(2)/1,1/                                        24
      REAL RE_VPDH_DETLEN                                                 24
C -                                                                       24
      REAL VPDH_PMTRAD                                                    24
      EQUIVALENCE (BANK_VPDH(11),VPDH_PMTRAD,RE_VPDH_PMTRAD),             24
     *(NAMES_VPDH(9),NVPDH_PMTRAD)                                        24
      EQUIVALENCE (COMMEN_VPDH(9),CVPDH_PMTRAD),(MAP_VPDH(1,9),           24
     *MVPDH_PMTRAD)                                                       24
      CHARACTER CVPDH_PMTRAD*40/' '/                                      24
      CHARACTER NVPDH_PMTRAD*12/'R.PMTRAD'/                               24
      INTEGER MVPDH_PMTRAD(2)/1,1/                                        24
      REAL RE_VPDH_PMTRAD                                                 24
C -                                                                       24
      REAL VPDH_PMTLEN                                                    24
      EQUIVALENCE (BANK_VPDH(12),VPDH_PMTLEN,RE_VPDH_PMTLEN),             24
     *(NAMES_VPDH(10),NVPDH_PMTLEN)                                       24
      EQUIVALENCE (COMMEN_VPDH(10),CVPDH_PMTLEN),(MAP_VPDH(1,10),         24
     *MVPDH_PMTLEN)                                                       24
      CHARACTER CVPDH_PMTLEN*40/' '/                                      24
      CHARACTER NVPDH_PMTLEN*12/'R.PMTLEN'/                               24
      INTEGER MVPDH_PMTLEN(2)/1,1/                                        24
      REAL RE_VPDH_PMTLEN                                                 24
C -                                                                       24
      REAL VPDH_DETWALL                                                   24
      EQUIVALENCE (BANK_VPDH(13),VPDH_DETWALL,RE_VPDH_DETWALL),           24
     *(NAMES_VPDH(11),NVPDH_DETWALL)                                      24
      EQUIVALENCE (COMMEN_VPDH(11),CVPDH_DETWALL),(MAP_VPDH(1,11),        24
     *MVPDH_DETWALL)                                                      24
      CHARACTER CVPDH_DETWALL*40/' '/                                     24
      CHARACTER NVPDH_DETWALL*12/'R.DETWALL'/                             24
      INTEGER MVPDH_DETWALL(2)/1,1/                                       24
      REAL RE_VPDH_DETWALL                                                24
C -                                                                       24
      REAL VPDH_DETFRONT                                                  24
      EQUIVALENCE (BANK_VPDH(14),VPDH_DETFRONT,RE_VPDH_DETFRONT),         24
     *(NAMES_VPDH(12),NVPDH_DETFRONT)                                     24
      EQUIVALENCE (COMMEN_VPDH(12),CVPDH_DETFRONT),(MAP_VPDH(1,12),       24
     *MVPDH_DETFRONT)                                                     24
      CHARACTER CVPDH_DETFRONT*40/' '/                                    24
      CHARACTER NVPDH_DETFRONT*12/'R.DETFRONT'/                           24
      INTEGER MVPDH_DETFRONT(2)/1,1/                                      24
      REAL RE_VPDH_DETFRONT                                               24
C -                                                                       24
      REAL VPDH_LEADTHICK                                                 24
      EQUIVALENCE (BANK_VPDH(15),VPDH_LEADTHICK,RE_VPDH_LEADTHICK),       24
     *(NAMES_VPDH(13),NVPDH_LEADTHICK)                                    24
      EQUIVALENCE (COMMEN_VPDH(13),CVPDH_LEADTHICK),(MAP_VPDH(1,13),      24
     *MVPDH_LEADTHICK)                                                    24
      CHARACTER CVPDH_LEADTHICK*40/' '/                                   24
      CHARACTER NVPDH_LEADTHICK*12/'R.LEADTHICK'/                         24
      INTEGER MVPDH_LEADTHICK(2)/1,1/                                     24
      REAL RE_VPDH_LEADTHICK                                              24
C -                                                                       24
      REAL VPDH_SCINTTHICK                                                24
      EQUIVALENCE (BANK_VPDH(16),VPDH_SCINTTHICK,RE_VPDH_SCINTTHICK),     24
     *(NAMES_VPDH(14),NVPDH_SCINTTHICK)                                   24
      EQUIVALENCE (COMMEN_VPDH(14),CVPDH_SCINTTHICK),(MAP_VPDH(1,14),     24
     *MVPDH_SCINTTHICK)                                                   24
      CHARACTER CVPDH_SCINTTHICK*40/' '/                                  24
      CHARACTER NVPDH_SCINTTHICK*12/'R.SCINTTHICK'/                       24
      INTEGER MVPDH_SCINTTHICK(2)/1,1/                                    24
      REAL RE_VPDH_SCINTTHICK                                             24
C -                                                                       24
      REAL VPDH_RING1_NDET                                                24
      EQUIVALENCE (BANK_VPDH(17),VPDH_RING1_NDET,RE_VPDH_RING1_NDET),     24
     *(NAMES_VPDH(15),NVPDH_RING1_NDET)                                   24
      EQUIVALENCE (COMMEN_VPDH(15),CVPDH_RING1_NDET),(MAP_VPDH(1,15),     24
     *MVPDH_RING1_NDET)                                                   24
      CHARACTER CVPDH_RING1_NDET*40/' '/                                  24
      CHARACTER NVPDH_RING1_NDET*12/'R.RING1_NDET'/                       24
      INTEGER MVPDH_RING1_NDET(2)/1,1/                                    24
      REAL RE_VPDH_RING1_NDET                                             24
C -                                                                       24
      REAL VPDH_RING1_RAD                                                 24
      EQUIVALENCE (BANK_VPDH(18),VPDH_RING1_RAD,RE_VPDH_RING1_RAD),       24
     *(NAMES_VPDH(16),NVPDH_RING1_RAD)                                    24
      EQUIVALENCE (COMMEN_VPDH(16),CVPDH_RING1_RAD),(MAP_VPDH(1,16),      24
     *MVPDH_RING1_RAD)                                                    24
      CHARACTER CVPDH_RING1_RAD*40/' '/                                   24
      CHARACTER NVPDH_RING1_RAD*12/'R.RING1_RAD'/                         24
      INTEGER MVPDH_RING1_RAD(2)/1,1/                                     24
      REAL RE_VPDH_RING1_RAD                                              24
C -                                                                       24
      REAL VPDH_RING1_PHI0                                                24
      EQUIVALENCE (BANK_VPDH(19),VPDH_RING1_PHI0,RE_VPDH_RING1_PHI0),     24
     *(NAMES_VPDH(17),NVPDH_RING1_PHI0)                                   24
      EQUIVALENCE (COMMEN_VPDH(17),CVPDH_RING1_PHI0),(MAP_VPDH(1,17),     24
     *MVPDH_RING1_PHI0)                                                   24
      CHARACTER CVPDH_RING1_PHI0*40/' '/                                  24
      CHARACTER NVPDH_RING1_PHI0*12/'R.RING1_PHI0'/                       24
      INTEGER MVPDH_RING1_PHI0(2)/1,1/                                    24
      REAL RE_VPDH_RING1_PHI0                                             24
C -                                                                       24
      REAL VPDH_RING1_DPHI                                                24
      EQUIVALENCE (BANK_VPDH(20),VPDH_RING1_DPHI,RE_VPDH_RING1_DPHI),     24
     *(NAMES_VPDH(18),NVPDH_RING1_DPHI)                                   24
      EQUIVALENCE (COMMEN_VPDH(18),CVPDH_RING1_DPHI),(MAP_VPDH(1,18),     24
     *MVPDH_RING1_DPHI)                                                   24
      CHARACTER CVPDH_RING1_DPHI*40/' '/                                  24
      CHARACTER NVPDH_RING1_DPHI*12/'R.RING1_DPHI'/                       24
      INTEGER MVPDH_RING1_DPHI(2)/1,1/                                    24
      REAL RE_VPDH_RING1_DPHI                                             24
C -                                                                       24
      REAL VPDH_RING1_KPROJ                                               24
      EQUIVALENCE (BANK_VPDH(21),VPDH_RING1_KPROJ,RE_VPDH_RING1_KPROJ),   24
     *(NAMES_VPDH(19),NVPDH_RING1_KPROJ)                                  24
      EQUIVALENCE (COMMEN_VPDH(19),CVPDH_RING1_KPROJ),(MAP_VPDH(1,19),    24
     *MVPDH_RING1_KPROJ)                                                  24
      CHARACTER CVPDH_RING1_KPROJ*40/' '/                                 24
      CHARACTER NVPDH_RING1_KPROJ*12/'R.RING1_KPROJ'/                     24
      INTEGER MVPDH_RING1_KPROJ(2)/1,1/                                   24
      REAL RE_VPDH_RING1_KPROJ                                            24
C -                                                                       24
      REAL VPDH_RING2_NDET                                                24
      EQUIVALENCE (BANK_VPDH(22),VPDH_RING2_NDET,RE_VPDH_RING2_NDET),     24
     *(NAMES_VPDH(20),NVPDH_RING2_NDET)                                   24
      EQUIVALENCE (COMMEN_VPDH(20),CVPDH_RING2_NDET),(MAP_VPDH(1,20),     24
     *MVPDH_RING2_NDET)                                                   24
      CHARACTER CVPDH_RING2_NDET*40/' '/                                  24
      CHARACTER NVPDH_RING2_NDET*12/'R.RING2_NDET'/                       24
      INTEGER MVPDH_RING2_NDET(2)/1,1/                                    24
      REAL RE_VPDH_RING2_NDET                                             24
C -                                                                       24
      REAL VPDH_RING2_RAD                                                 24
      EQUIVALENCE (BANK_VPDH(23),VPDH_RING2_RAD,RE_VPDH_RING2_RAD),       24
     *(NAMES_VPDH(21),NVPDH_RING2_RAD)                                    24
      EQUIVALENCE (COMMEN_VPDH(21),CVPDH_RING2_RAD),(MAP_VPDH(1,21),      24
     *MVPDH_RING2_RAD)                                                    24
      CHARACTER CVPDH_RING2_RAD*40/' '/                                   24
      CHARACTER NVPDH_RING2_RAD*12/'R.RING2_RAD'/                         24
      INTEGER MVPDH_RING2_RAD(2)/1,1/                                     24
      REAL RE_VPDH_RING2_RAD                                              24
C -                                                                       24
      REAL VPDH_RING2_PHI0                                                24
      EQUIVALENCE (BANK_VPDH(24),VPDH_RING2_PHI0,RE_VPDH_RING2_PHI0),     24
     *(NAMES_VPDH(22),NVPDH_RING2_PHI0)                                   24
      EQUIVALENCE (COMMEN_VPDH(22),CVPDH_RING2_PHI0),(MAP_VPDH(1,22),     24
     *MVPDH_RING2_PHI0)                                                   24
      CHARACTER CVPDH_RING2_PHI0*40/' '/                                  24
      CHARACTER NVPDH_RING2_PHI0*12/'R.RING2_PHI0'/                       24
      INTEGER MVPDH_RING2_PHI0(2)/1,1/                                    24
      REAL RE_VPDH_RING2_PHI0                                             24
C -                                                                       24
      REAL VPDH_RING2_DPHI                                                24
      EQUIVALENCE (BANK_VPDH(25),VPDH_RING2_DPHI,RE_VPDH_RING2_DPHI),     24
     *(NAMES_VPDH(23),NVPDH_RING2_DPHI)                                   24
      EQUIVALENCE (COMMEN_VPDH(23),CVPDH_RING2_DPHI),(MAP_VPDH(1,23),     24
     *MVPDH_RING2_DPHI)                                                   24
      CHARACTER CVPDH_RING2_DPHI*40/' '/                                  24
      CHARACTER NVPDH_RING2_DPHI*12/'R.RING2_DPHI'/                       24
      INTEGER MVPDH_RING2_DPHI(2)/1,1/                                    24
      REAL RE_VPDH_RING2_DPHI                                             24
C -                                                                       24
      REAL VPDH_RING2_KPROJ                                               24
      EQUIVALENCE (BANK_VPDH(26),VPDH_RING2_KPROJ,RE_VPDH_RING2_KPROJ),   24
     *(NAMES_VPDH(24),NVPDH_RING2_KPROJ)                                  24
      EQUIVALENCE (COMMEN_VPDH(24),CVPDH_RING2_KPROJ),(MAP_VPDH(1,24),    24
     *MVPDH_RING2_KPROJ)                                                  24
      CHARACTER CVPDH_RING2_KPROJ*40/' '/                                 24
      CHARACTER NVPDH_RING2_KPROJ*12/'R.RING2_KPROJ'/                     24
      INTEGER MVPDH_RING2_KPROJ(2)/1,1/                                   24
      REAL RE_VPDH_RING2_KPROJ                                            24
C -                                                                       24
      REAL VPDH_RING3_NDET                                                24
      EQUIVALENCE (BANK_VPDH(27),VPDH_RING3_NDET,RE_VPDH_RING3_NDET),     24
     *(NAMES_VPDH(25),NVPDH_RING3_NDET)                                   24
      EQUIVALENCE (COMMEN_VPDH(25),CVPDH_RING3_NDET),(MAP_VPDH(1,25),     24
     *MVPDH_RING3_NDET)                                                   24
      CHARACTER CVPDH_RING3_NDET*40/' '/                                  24
      CHARACTER NVPDH_RING3_NDET*12/'R.RING3_NDET'/                       24
      INTEGER MVPDH_RING3_NDET(2)/1,1/                                    24
      REAL RE_VPDH_RING3_NDET                                             24
C -                                                                       24
      REAL VPDH_RING3_RAD                                                 24
      EQUIVALENCE (BANK_VPDH(28),VPDH_RING3_RAD,RE_VPDH_RING3_RAD),       24
     *(NAMES_VPDH(26),NVPDH_RING3_RAD)                                    24
      EQUIVALENCE (COMMEN_VPDH(26),CVPDH_RING3_RAD),(MAP_VPDH(1,26),      24
     *MVPDH_RING3_RAD)                                                    24
      CHARACTER CVPDH_RING3_RAD*40/' '/                                   24
      CHARACTER NVPDH_RING3_RAD*12/'R.RING3_RAD'/                         24
      INTEGER MVPDH_RING3_RAD(2)/1,1/                                     24
      REAL RE_VPDH_RING3_RAD                                              24
C -                                                                       24
      REAL VPDH_RING3_PHI0                                                24
      EQUIVALENCE (BANK_VPDH(29),VPDH_RING3_PHI0,RE_VPDH_RING3_PHI0),     24
     *(NAMES_VPDH(27),NVPDH_RING3_PHI0)                                   24
      EQUIVALENCE (COMMEN_VPDH(27),CVPDH_RING3_PHI0),(MAP_VPDH(1,27),     24
     *MVPDH_RING3_PHI0)                                                   24
      CHARACTER CVPDH_RING3_PHI0*40/' '/                                  24
      CHARACTER NVPDH_RING3_PHI0*12/'R.RING3_PHI0'/                       24
      INTEGER MVPDH_RING3_PHI0(2)/1,1/                                    24
      REAL RE_VPDH_RING3_PHI0                                             24
C -                                                                       24
      REAL VPDH_RING3_DPHI                                                24
      EQUIVALENCE (BANK_VPDH(30),VPDH_RING3_DPHI,RE_VPDH_RING3_DPHI),     24
     *(NAMES_VPDH(28),NVPDH_RING3_DPHI)                                   24
      EQUIVALENCE (COMMEN_VPDH(28),CVPDH_RING3_DPHI),(MAP_VPDH(1,28),     24
     *MVPDH_RING3_DPHI)                                                   24
      CHARACTER CVPDH_RING3_DPHI*40/' '/                                  24
      CHARACTER NVPDH_RING3_DPHI*12/'R.RING3_DPHI'/                       24
      INTEGER MVPDH_RING3_DPHI(2)/1,1/                                    24
      REAL RE_VPDH_RING3_DPHI                                             24
C -                                                                       24
      REAL VPDH_RING3_KPROJ                                               24
      EQUIVALENCE (BANK_VPDH(31),VPDH_RING3_KPROJ,RE_VPDH_RING3_KPROJ),   24
     *(NAMES_VPDH(29),NVPDH_RING3_KPROJ)                                  24
      EQUIVALENCE (COMMEN_VPDH(29),CVPDH_RING3_KPROJ),(MAP_VPDH(1,29),    24
     *MVPDH_RING3_KPROJ)                                                  24
      CHARACTER CVPDH_RING3_KPROJ*40/' '/                                 24
      CHARACTER NVPDH_RING3_KPROJ*12/'R.RING3_KPROJ'/                     24
      INTEGER MVPDH_RING3_KPROJ(2)/1,1/                                   24
      REAL RE_VPDH_RING3_KPROJ                                            24
C -                                                                       24
      REAL VPDH_EWSHIFT                                                   24
      EQUIVALENCE (BANK_VPDH(32),VPDH_EWSHIFT,RE_VPDH_EWSHIFT),           24
     *(NAMES_VPDH(30),NVPDH_EWSHIFT)                                      24
      EQUIVALENCE (COMMEN_VPDH(30),CVPDH_EWSHIFT),(MAP_VPDH(1,30),        24
     *MVPDH_EWSHIFT)                                                      24
      CHARACTER CVPDH_EWSHIFT*40/' '/                                     24
      CHARACTER NVPDH_EWSHIFT*12/'R.EWSHIFT'/                             24
      INTEGER MVPDH_EWSHIFT(2)/1,1/                                       24
      REAL RE_VPDH_EWSHIFT                                                24
C -                                                                       24
      REAL VPDH_UDSHIFT                                                   24
      EQUIVALENCE (BANK_VPDH(33),VPDH_UDSHIFT,RE_VPDH_UDSHIFT),           24
     *(NAMES_VPDH(31),NVPDH_UDSHIFT)                                      24
      EQUIVALENCE (COMMEN_VPDH(31),CVPDH_UDSHIFT),(MAP_VPDH(1,31),        24
     *MVPDH_UDSHIFT)                                                      24
      CHARACTER CVPDH_UDSHIFT*40/' '/                                     24
      CHARACTER NVPDH_UDSHIFT*12/'R.UDSHIFT'/                             24
      INTEGER MVPDH_UDSHIFT(2)/1,1/                                       24
      REAL RE_VPDH_UDSHIFT                                                24
C -                                                                       24
      REAL VPDH_BOLTSHIFT                                                 24
      EQUIVALENCE (BANK_VPDH(34),VPDH_BOLTSHIFT,RE_VPDH_BOLTSHIFT),       24
     *(NAMES_VPDH(32),NVPDH_BOLTSHIFT)                                    24
      EQUIVALENCE (COMMEN_VPDH(32),CVPDH_BOLTSHIFT),(MAP_VPDH(1,32),      24
     *MVPDH_BOLTSHIFT)                                                    24
      CHARACTER CVPDH_BOLTSHIFT*40/' '/                                   24
      CHARACTER NVPDH_BOLTSHIFT*12/'R.BOLTSHIFT'/                         24
      INTEGER MVPDH_BOLTSHIFT(2)/1,1/                                     24
      REAL RE_VPDH_BOLTSHIFT                                              24
C -                                                                       24
      INTEGER LEN_VPDH,LLEN_VPDH,LINK_VPDH                                24
      PARAMETER (LEN_VPDH=34)                                             24
      PARAMETER (LLEN_VPDH=32)                                            24
      INTEGER FLAG_VPDH/0/,MAP_VPDH(2,LLEN_VPDH),IDNUM_VPDH(10)           24
      CHARACTER NAMES_VPDH(LLEN_VPDH)*12,COMMEN_VPDH(LLEN_VPDH)*40        24
      DATA IDNUM_VPDH/10*0/                                               24
      REAL BANK_VPDH(LEN_VPDH)                                            24
      CHARACTER*60 BPATH_VPDH/' '/                                        24
      SAVE FLAG_VPDH,MAP_VPDH,IDNUM_VPDH,NAMES_VPDH,COMMEN_VPDH,          24
     *BPATH_VPDH                                                          24
      DATA LINK_VPDH/0/,BANK_VPDH/LEN_VPDH*0/                             24
C *                                                                       24
C -                                                                       32
      REAL VPDS_VERSION                                                   32
      EQUIVALENCE (BANK_VPDS(3),VPDS_VERSION,RE_VPDS_VERSION),            32
     *(NAMES_VPDS(1),NVPDS_VERSION)                                       32
      EQUIVALENCE (COMMEN_VPDS(1),CVPDS_VERSION),(MAP_VPDS(1,1),          32
     *MVPDS_VERSION)                                                      32
      CHARACTER CVPDS_VERSION*40/' '/                                     32
      CHARACTER NVPDS_VERSION*12/'R.VERSION'/                             32
      INTEGER MVPDS_VERSION(2)/1,1/                                       32
      REAL RE_VPDS_VERSION                                                32
C -                                                                       32
      REAL VPDS_IBSAZC                                                    32
      EQUIVALENCE (BANK_VPDS(4),VPDS_IBSAZC,RE_VPDS_IBSAZC),              32
     *(NAMES_VPDS(2),NVPDS_IBSAZC)                                        32
      EQUIVALENCE (COMMEN_VPDS(2),CVPDS_IBSAZC),(MAP_VPDS(1,2),           32
     *MVPDS_IBSAZC)                                                       32
      CHARACTER CVPDS_IBSAZC*40/' '/                                      32
      CHARACTER NVPDS_IBSAZC*12/'R.IBSAZC'/                               32
      INTEGER MVPDS_IBSAZC(2)/1,1/                                        32
      REAL RE_VPDS_IBSAZC                                                 32
C -                                                                       32
      REAL VPDS_IBSAYC                                                    32
      EQUIVALENCE (BANK_VPDS(5),VPDS_IBSAYC,RE_VPDS_IBSAYC),              32
     *(NAMES_VPDS(3),NVPDS_IBSAYC)                                        32
      EQUIVALENCE (COMMEN_VPDS(3),CVPDS_IBSAYC),(MAP_VPDS(1,3),           32
     *MVPDS_IBSAYC)                                                       32
      CHARACTER CVPDS_IBSAYC*40/' '/                                      32
      CHARACTER NVPDS_IBSAYC*12/'R.IBSAYC'/                               32
      INTEGER MVPDS_IBSAYC(2)/1,1/                                        32
      REAL RE_VPDS_IBSAYC                                                 32
C -                                                                       32
      REAL VPDS_IBSAXC                                                    32
      EQUIVALENCE (BANK_VPDS(6),VPDS_IBSAXC,RE_VPDS_IBSAXC),              32
     *(NAMES_VPDS(4),NVPDS_IBSAXC)                                        32
      EQUIVALENCE (COMMEN_VPDS(4),CVPDS_IBSAXC),(MAP_VPDS(1,4),           32
     *MVPDS_IBSAXC)                                                       32
      CHARACTER CVPDS_IBSAXC*40/' '/                                      32
      CHARACTER NVPDS_IBSAXC*12/'R.IBSAXC'/                               32
      INTEGER MVPDS_IBSAXC(2)/1,1/                                        32
      REAL RE_VPDS_IBSAXC                                                 32
C -                                                                       32
      REAL VPDS_IBSBZC                                                    32
      EQUIVALENCE (BANK_VPDS(7),VPDS_IBSBZC,RE_VPDS_IBSBZC),              32
     *(NAMES_VPDS(5),NVPDS_IBSBZC)                                        32
      EQUIVALENCE (COMMEN_VPDS(5),CVPDS_IBSBZC),(MAP_VPDS(1,5),           32
     *MVPDS_IBSBZC)                                                       32
      CHARACTER CVPDS_IBSBZC*40/' '/                                      32
      CHARACTER NVPDS_IBSBZC*12/'R.IBSBZC'/                               32
      INTEGER MVPDS_IBSBZC(2)/1,1/                                        32
      REAL RE_VPDS_IBSBZC                                                 32
C -                                                                       32
      REAL VPDS_IBSBYC                                                    32
      EQUIVALENCE (BANK_VPDS(8),VPDS_IBSBYC,RE_VPDS_IBSBYC),              32
     *(NAMES_VPDS(6),NVPDS_IBSBYC)                                        32
      EQUIVALENCE (COMMEN_VPDS(6),CVPDS_IBSBYC),(MAP_VPDS(1,6),           32
     *MVPDS_IBSBYC)                                                       32
      CHARACTER CVPDS_IBSBYC*40/' '/                                      32
      CHARACTER NVPDS_IBSBYC*12/'R.IBSBYC'/                               32
      INTEGER MVPDS_IBSBYC(2)/1,1/                                        32
      REAL RE_VPDS_IBSBYC                                                 32
C -                                                                       32
      REAL VPDS_IBSBXC                                                    32
      EQUIVALENCE (BANK_VPDS(9),VPDS_IBSBXC,RE_VPDS_IBSBXC),              32
     *(NAMES_VPDS(7),NVPDS_IBSBXC)                                        32
      EQUIVALENCE (COMMEN_VPDS(7),CVPDS_IBSBXC),(MAP_VPDS(1,7),           32
     *MVPDS_IBSBXC)                                                       32
      CHARACTER CVPDS_IBSBXC*40/' '/                                      32
      CHARACTER NVPDS_IBSBXC*12/'R.IBSBXC'/                               32
      INTEGER MVPDS_IBSBXC(2)/1,1/                                        32
      REAL RE_VPDS_IBSBXC                                                 32
C -                                                                       32
      REAL VPDS_IBSCZC                                                    32
      EQUIVALENCE (BANK_VPDS(10),VPDS_IBSCZC,RE_VPDS_IBSCZC),             32
     *(NAMES_VPDS(8),NVPDS_IBSCZC)                                        32
      EQUIVALENCE (COMMEN_VPDS(8),CVPDS_IBSCZC),(MAP_VPDS(1,8),           32
     *MVPDS_IBSCZC)                                                       32
      CHARACTER CVPDS_IBSCZC*40/' '/                                      32
      CHARACTER NVPDS_IBSCZC*12/'R.IBSCZC'/                               32
      INTEGER MVPDS_IBSCZC(2)/1,1/                                        32
      REAL RE_VPDS_IBSCZC                                                 32
C -                                                                       32
      REAL VPDS_IBSCYC                                                    32
      EQUIVALENCE (BANK_VPDS(11),VPDS_IBSCYC,RE_VPDS_IBSCYC),             32
     *(NAMES_VPDS(9),NVPDS_IBSCYC)                                        32
      EQUIVALENCE (COMMEN_VPDS(9),CVPDS_IBSCYC),(MAP_VPDS(1,9),           32
     *MVPDS_IBSCYC)                                                       32
      CHARACTER CVPDS_IBSCYC*40/' '/                                      32
      CHARACTER NVPDS_IBSCYC*12/'R.IBSCYC'/                               32
      INTEGER MVPDS_IBSCYC(2)/1,1/                                        32
      REAL RE_VPDS_IBSCYC                                                 32
C -                                                                       32
      REAL VPDS_IBSCXC                                                    32
      EQUIVALENCE (BANK_VPDS(12),VPDS_IBSCXC,RE_VPDS_IBSCXC),             32
     *(NAMES_VPDS(10),NVPDS_IBSCXC)                                       32
      EQUIVALENCE (COMMEN_VPDS(10),CVPDS_IBSCXC),(MAP_VPDS(1,10),         32
     *MVPDS_IBSCXC)                                                       32
      CHARACTER CVPDS_IBSCXC*40/' '/                                      32
      CHARACTER NVPDS_IBSCXC*12/'R.IBSCXC'/                               32
      INTEGER MVPDS_IBSCXC(2)/1,1/                                        32
      REAL RE_VPDS_IBSCXC                                                 32
C -                                                                       32
      REAL VPDS_IBSDZC1                                                   32
      EQUIVALENCE (BANK_VPDS(13),VPDS_IBSDZC1,RE_VPDS_IBSDZC1),           32
     *(NAMES_VPDS(11),NVPDS_IBSDZC1)                                      32
      EQUIVALENCE (COMMEN_VPDS(11),CVPDS_IBSDZC1),(MAP_VPDS(1,11),        32
     *MVPDS_IBSDZC1)                                                      32
      CHARACTER CVPDS_IBSDZC1*40/' '/                                     32
      CHARACTER NVPDS_IBSDZC1*12/'R.IBSDZC1'/                             32
      INTEGER MVPDS_IBSDZC1(2)/1,1/                                       32
      REAL RE_VPDS_IBSDZC1                                                32
C -                                                                       32
      REAL VPDS_IBSDZC2                                                   32
      EQUIVALENCE (BANK_VPDS(14),VPDS_IBSDZC2,RE_VPDS_IBSDZC2),           32
     *(NAMES_VPDS(12),NVPDS_IBSDZC2)                                      32
      EQUIVALENCE (COMMEN_VPDS(12),CVPDS_IBSDZC2),(MAP_VPDS(1,12),        32
     *MVPDS_IBSDZC2)                                                      32
      CHARACTER CVPDS_IBSDZC2*40/' '/                                     32
      CHARACTER NVPDS_IBSDZC2*12/'R.IBSDZC2'/                             32
      INTEGER MVPDS_IBSDZC2(2)/1,1/                                       32
      REAL RE_VPDS_IBSDZC2                                                32
C -                                                                       32
      REAL VPDS_IBSDYC1                                                   32
      EQUIVALENCE (BANK_VPDS(15),VPDS_IBSDYC1,RE_VPDS_IBSDYC1),           32
     *(NAMES_VPDS(13),NVPDS_IBSDYC1)                                      32
      EQUIVALENCE (COMMEN_VPDS(13),CVPDS_IBSDYC1),(MAP_VPDS(1,13),        32
     *MVPDS_IBSDYC1)                                                      32
      CHARACTER CVPDS_IBSDYC1*40/' '/                                     32
      CHARACTER NVPDS_IBSDYC1*12/'R.IBSDYC1'/                             32
      INTEGER MVPDS_IBSDYC1(2)/1,1/                                       32
      REAL RE_VPDS_IBSDYC1                                                32
C -                                                                       32
      REAL VPDS_IBSDYC2                                                   32
      EQUIVALENCE (BANK_VPDS(16),VPDS_IBSDYC2,RE_VPDS_IBSDYC2),           32
     *(NAMES_VPDS(14),NVPDS_IBSDYC2)                                      32
      EQUIVALENCE (COMMEN_VPDS(14),CVPDS_IBSDYC2),(MAP_VPDS(1,14),        32
     *MVPDS_IBSDYC2)                                                      32
      CHARACTER CVPDS_IBSDYC2*40/' '/                                     32
      CHARACTER NVPDS_IBSDYC2*12/'R.IBSDYC2'/                             32
      INTEGER MVPDS_IBSDYC2(2)/1,1/                                       32
      REAL RE_VPDS_IBSDYC2                                                32
C -                                                                       32
      REAL VPDS_IBSDXC                                                    32
      EQUIVALENCE (BANK_VPDS(17),VPDS_IBSDXC,RE_VPDS_IBSDXC),             32
     *(NAMES_VPDS(15),NVPDS_IBSDXC)                                       32
      EQUIVALENCE (COMMEN_VPDS(15),CVPDS_IBSDXC),(MAP_VPDS(1,15),         32
     *MVPDS_IBSDXC)                                                       32
      CHARACTER CVPDS_IBSDXC*40/' '/                                      32
      CHARACTER NVPDS_IBSDXC*12/'R.IBSDXC'/                               32
      INTEGER MVPDS_IBSDXC(2)/1,1/                                        32
      REAL RE_VPDS_IBSDXC                                                 32
C -                                                                       32
      REAL VPDS_IBSEZC1                                                   32
      EQUIVALENCE (BANK_VPDS(18),VPDS_IBSEZC1,RE_VPDS_IBSEZC1),           32
     *(NAMES_VPDS(16),NVPDS_IBSEZC1)                                      32
      EQUIVALENCE (COMMEN_VPDS(16),CVPDS_IBSEZC1),(MAP_VPDS(1,16),        32
     *MVPDS_IBSEZC1)                                                      32
      CHARACTER CVPDS_IBSEZC1*40/' '/                                     32
      CHARACTER NVPDS_IBSEZC1*12/'R.IBSEZC1'/                             32
      INTEGER MVPDS_IBSEZC1(2)/1,1/                                       32
      REAL RE_VPDS_IBSEZC1                                                32
C -                                                                       32
      REAL VPDS_IBSEZC2                                                   32
      EQUIVALENCE (BANK_VPDS(19),VPDS_IBSEZC2,RE_VPDS_IBSEZC2),           32
     *(NAMES_VPDS(17),NVPDS_IBSEZC2)                                      32
      EQUIVALENCE (COMMEN_VPDS(17),CVPDS_IBSEZC2),(MAP_VPDS(1,17),        32
     *MVPDS_IBSEZC2)                                                      32
      CHARACTER CVPDS_IBSEZC2*40/' '/                                     32
      CHARACTER NVPDS_IBSEZC2*12/'R.IBSEZC2'/                             32
      INTEGER MVPDS_IBSEZC2(2)/1,1/                                       32
      REAL RE_VPDS_IBSEZC2                                                32
C -                                                                       32
      REAL VPDS_IBSEYC                                                    32
      EQUIVALENCE (BANK_VPDS(20),VPDS_IBSEYC,RE_VPDS_IBSEYC),             32
     *(NAMES_VPDS(18),NVPDS_IBSEYC)                                       32
      EQUIVALENCE (COMMEN_VPDS(18),CVPDS_IBSEYC),(MAP_VPDS(1,18),         32
     *MVPDS_IBSEYC)                                                       32
      CHARACTER CVPDS_IBSEYC*40/' '/                                      32
      CHARACTER NVPDS_IBSEYC*12/'R.IBSEYC'/                               32
      INTEGER MVPDS_IBSEYC(2)/1,1/                                        32
      REAL RE_VPDS_IBSEYC                                                 32
C -                                                                       32
      REAL VPDS_IBSEXC                                                    32
      EQUIVALENCE (BANK_VPDS(21),VPDS_IBSEXC,RE_VPDS_IBSEXC),             32
     *(NAMES_VPDS(19),NVPDS_IBSEXC)                                       32
      EQUIVALENCE (COMMEN_VPDS(19),CVPDS_IBSEXC),(MAP_VPDS(1,19),         32
     *MVPDS_IBSEXC)                                                       32
      CHARACTER CVPDS_IBSEXC*40/' '/                                      32
      CHARACTER NVPDS_IBSEXC*12/'R.IBSEXC'/                               32
      INTEGER MVPDS_IBSEXC(2)/1,1/                                        32
      REAL RE_VPDS_IBSEXC                                                 32
C -                                                                       32
      REAL VPDS_IBSFZC                                                    32
      EQUIVALENCE (BANK_VPDS(22),VPDS_IBSFZC,RE_VPDS_IBSFZC),             32
     *(NAMES_VPDS(20),NVPDS_IBSFZC)                                       32
      EQUIVALENCE (COMMEN_VPDS(20),CVPDS_IBSFZC),(MAP_VPDS(1,20),         32
     *MVPDS_IBSFZC)                                                       32
      CHARACTER CVPDS_IBSFZC*40/' '/                                      32
      CHARACTER NVPDS_IBSFZC*12/'R.IBSFZC'/                               32
      INTEGER MVPDS_IBSFZC(2)/1,1/                                        32
      REAL RE_VPDS_IBSFZC                                                 32
C -                                                                       32
      REAL VPDS_IBSFYC                                                    32
      EQUIVALENCE (BANK_VPDS(23),VPDS_IBSFYC,RE_VPDS_IBSFYC),             32
     *(NAMES_VPDS(21),NVPDS_IBSFYC)                                       32
      EQUIVALENCE (COMMEN_VPDS(21),CVPDS_IBSFYC),(MAP_VPDS(1,21),         32
     *MVPDS_IBSFYC)                                                       32
      CHARACTER CVPDS_IBSFYC*40/' '/                                      32
      CHARACTER NVPDS_IBSFYC*12/'R.IBSFYC'/                               32
      INTEGER MVPDS_IBSFYC(2)/1,1/                                        32
      REAL RE_VPDS_IBSFYC                                                 32
C -                                                                       32
      REAL VPDS_IBSFXC                                                    32
      EQUIVALENCE (BANK_VPDS(24),VPDS_IBSFXC,RE_VPDS_IBSFXC),             32
     *(NAMES_VPDS(22),NVPDS_IBSFXC)                                       32
      EQUIVALENCE (COMMEN_VPDS(22),CVPDS_IBSFXC),(MAP_VPDS(1,22),         32
     *MVPDS_IBSFXC)                                                       32
      CHARACTER CVPDS_IBSFXC*40/' '/                                      32
      CHARACTER NVPDS_IBSFXC*12/'R.IBSFXC'/                               32
      INTEGER MVPDS_IBSFXC(2)/1,1/                                        32
      REAL RE_VPDS_IBSFXC                                                 32
C -                                                                       32
      REAL VPDS_IBSGZC1                                                   32
      EQUIVALENCE (BANK_VPDS(25),VPDS_IBSGZC1,RE_VPDS_IBSGZC1),           32
     *(NAMES_VPDS(23),NVPDS_IBSGZC1)                                      32
      EQUIVALENCE (COMMEN_VPDS(23),CVPDS_IBSGZC1),(MAP_VPDS(1,23),        32
     *MVPDS_IBSGZC1)                                                      32
      CHARACTER CVPDS_IBSGZC1*40/' '/                                     32
      CHARACTER NVPDS_IBSGZC1*12/'R.IBSGZC1'/                             32
      INTEGER MVPDS_IBSGZC1(2)/1,1/                                       32
      REAL RE_VPDS_IBSGZC1                                                32
C -                                                                       32
      REAL VPDS_IBSGZC2                                                   32
      EQUIVALENCE (BANK_VPDS(26),VPDS_IBSGZC2,RE_VPDS_IBSGZC2),           32
     *(NAMES_VPDS(24),NVPDS_IBSGZC2)                                      32
      EQUIVALENCE (COMMEN_VPDS(24),CVPDS_IBSGZC2),(MAP_VPDS(1,24),        32
     *MVPDS_IBSGZC2)                                                      32
      CHARACTER CVPDS_IBSGZC2*40/' '/                                     32
      CHARACTER NVPDS_IBSGZC2*12/'R.IBSGZC2'/                             32
      INTEGER MVPDS_IBSGZC2(2)/1,1/                                       32
      REAL RE_VPDS_IBSGZC2                                                32
C -                                                                       32
      REAL VPDS_IBSGZC3                                                   32
      EQUIVALENCE (BANK_VPDS(27),VPDS_IBSGZC3,RE_VPDS_IBSGZC3),           32
     *(NAMES_VPDS(25),NVPDS_IBSGZC3)                                      32
      EQUIVALENCE (COMMEN_VPDS(25),CVPDS_IBSGZC3),(MAP_VPDS(1,25),        32
     *MVPDS_IBSGZC3)                                                      32
      CHARACTER CVPDS_IBSGZC3*40/' '/                                     32
      CHARACTER NVPDS_IBSGZC3*12/'R.IBSGZC3'/                             32
      INTEGER MVPDS_IBSGZC3(2)/1,1/                                       32
      REAL RE_VPDS_IBSGZC3                                                32
C -                                                                       32
      REAL VPDS_IBSGYC                                                    32
      EQUIVALENCE (BANK_VPDS(28),VPDS_IBSGYC,RE_VPDS_IBSGYC),             32
     *(NAMES_VPDS(26),NVPDS_IBSGYC)                                       32
      EQUIVALENCE (COMMEN_VPDS(26),CVPDS_IBSGYC),(MAP_VPDS(1,26),         32
     *MVPDS_IBSGYC)                                                       32
      CHARACTER CVPDS_IBSGYC*40/' '/                                      32
      CHARACTER NVPDS_IBSGYC*12/'R.IBSGYC'/                               32
      INTEGER MVPDS_IBSGYC(2)/1,1/                                        32
      REAL RE_VPDS_IBSGYC                                                 32
C -                                                                       32
      REAL VPDS_IBSGXC                                                    32
      EQUIVALENCE (BANK_VPDS(29),VPDS_IBSGXC,RE_VPDS_IBSGXC),             32
     *(NAMES_VPDS(27),NVPDS_IBSGXC)                                       32
      EQUIVALENCE (COMMEN_VPDS(27),CVPDS_IBSGXC),(MAP_VPDS(1,27),         32
     *MVPDS_IBSGXC)                                                       32
      CHARACTER CVPDS_IBSGXC*40/' '/                                      32
      CHARACTER NVPDS_IBSGXC*12/'R.IBSGXC'/                               32
      INTEGER MVPDS_IBSGXC(2)/1,1/                                        32
      REAL RE_VPDS_IBSGXC                                                 32
C -                                                                       32
      REAL VPDS_IBSHZC1                                                   32
      EQUIVALENCE (BANK_VPDS(30),VPDS_IBSHZC1,RE_VPDS_IBSHZC1),           32
     *(NAMES_VPDS(28),NVPDS_IBSHZC1)                                      32
      EQUIVALENCE (COMMEN_VPDS(28),CVPDS_IBSHZC1),(MAP_VPDS(1,28),        32
     *MVPDS_IBSHZC1)                                                      32
      CHARACTER CVPDS_IBSHZC1*40/' '/                                     32
      CHARACTER NVPDS_IBSHZC1*12/'R.IBSHZC1'/                             32
      INTEGER MVPDS_IBSHZC1(2)/1,1/                                       32
      REAL RE_VPDS_IBSHZC1                                                32
C -                                                                       32
      REAL VPDS_IBSHZC2                                                   32
      EQUIVALENCE (BANK_VPDS(31),VPDS_IBSHZC2,RE_VPDS_IBSHZC2),           32
     *(NAMES_VPDS(29),NVPDS_IBSHZC2)                                      32
      EQUIVALENCE (COMMEN_VPDS(29),CVPDS_IBSHZC2),(MAP_VPDS(1,29),        32
     *MVPDS_IBSHZC2)                                                      32
      CHARACTER CVPDS_IBSHZC2*40/' '/                                     32
      CHARACTER NVPDS_IBSHZC2*12/'R.IBSHZC2'/                             32
      INTEGER MVPDS_IBSHZC2(2)/1,1/                                       32
      REAL RE_VPDS_IBSHZC2                                                32
C -                                                                       32
      REAL VPDS_IBSHYC                                                    32
      EQUIVALENCE (BANK_VPDS(32),VPDS_IBSHYC,RE_VPDS_IBSHYC),             32
     *(NAMES_VPDS(30),NVPDS_IBSHYC)                                       32
      EQUIVALENCE (COMMEN_VPDS(30),CVPDS_IBSHYC),(MAP_VPDS(1,30),         32
     *MVPDS_IBSHYC)                                                       32
      CHARACTER CVPDS_IBSHYC*40/' '/                                      32
      CHARACTER NVPDS_IBSHYC*12/'R.IBSHYC'/                               32
      INTEGER MVPDS_IBSHYC(2)/1,1/                                        32
      REAL RE_VPDS_IBSHYC                                                 32
C -                                                                       32
      REAL VPDS_IBSHXC1                                                   32
      EQUIVALENCE (BANK_VPDS(33),VPDS_IBSHXC1,RE_VPDS_IBSHXC1),           32
     *(NAMES_VPDS(31),NVPDS_IBSHXC1)                                      32
      EQUIVALENCE (COMMEN_VPDS(31),CVPDS_IBSHXC1),(MAP_VPDS(1,31),        32
     *MVPDS_IBSHXC1)                                                      32
      CHARACTER CVPDS_IBSHXC1*40/' '/                                     32
      CHARACTER NVPDS_IBSHXC1*12/'R.IBSHXC1'/                             32
      INTEGER MVPDS_IBSHXC1(2)/1,1/                                       32
      REAL RE_VPDS_IBSHXC1                                                32
C -                                                                       32
      REAL VPDS_IBSHXC2                                                   32
      EQUIVALENCE (BANK_VPDS(34),VPDS_IBSHXC2,RE_VPDS_IBSHXC2),           32
     *(NAMES_VPDS(32),NVPDS_IBSHXC2)                                      32
      EQUIVALENCE (COMMEN_VPDS(32),CVPDS_IBSHXC2),(MAP_VPDS(1,32),        32
     *MVPDS_IBSHXC2)                                                      32
      CHARACTER CVPDS_IBSHXC2*40/' '/                                     32
      CHARACTER NVPDS_IBSHXC2*12/'R.IBSHXC2'/                             32
      INTEGER MVPDS_IBSHXC2(2)/1,1/                                       32
      REAL RE_VPDS_IBSHXC2                                                32
C -                                                                       32
      REAL VPDS_BSALENX                                                   32
      EQUIVALENCE (BANK_VPDS(35),VPDS_BSALENX,RE_VPDS_BSALENX),           32
     *(NAMES_VPDS(33),NVPDS_BSALENX)                                      32
      EQUIVALENCE (COMMEN_VPDS(33),CVPDS_BSALENX),(MAP_VPDS(1,33),        32
     *MVPDS_BSALENX)                                                      32
      CHARACTER CVPDS_BSALENX*40/' '/                                     32
      CHARACTER NVPDS_BSALENX*12/'R.BSALENX'/                             32
      INTEGER MVPDS_BSALENX(2)/1,1/                                       32
      REAL RE_VPDS_BSALENX                                                32
C -                                                                       32
      REAL VPDS_BSALENY                                                   32
      EQUIVALENCE (BANK_VPDS(36),VPDS_BSALENY,RE_VPDS_BSALENY),           32
     *(NAMES_VPDS(34),NVPDS_BSALENY)                                      32
      EQUIVALENCE (COMMEN_VPDS(34),CVPDS_BSALENY),(MAP_VPDS(1,34),        32
     *MVPDS_BSALENY)                                                      32
      CHARACTER CVPDS_BSALENY*40/' '/                                     32
      CHARACTER NVPDS_BSALENY*12/'R.BSALENY'/                             32
      INTEGER MVPDS_BSALENY(2)/1,1/                                       32
      REAL RE_VPDS_BSALENY                                                32
C -                                                                       32
      REAL VPDS_BSALENZ                                                   32
      EQUIVALENCE (BANK_VPDS(37),VPDS_BSALENZ,RE_VPDS_BSALENZ),           32
     *(NAMES_VPDS(35),NVPDS_BSALENZ)                                      32
      EQUIVALENCE (COMMEN_VPDS(35),CVPDS_BSALENZ),(MAP_VPDS(1,35),        32
     *MVPDS_BSALENZ)                                                      32
      CHARACTER CVPDS_BSALENZ*40/' '/                                     32
      CHARACTER NVPDS_BSALENZ*12/'R.BSALENZ'/                             32
      INTEGER MVPDS_BSALENZ(2)/1,1/                                       32
      REAL RE_VPDS_BSALENZ                                                32
C -                                                                       32
      REAL VPDS_BAALENZ                                                   32
      EQUIVALENCE (BANK_VPDS(38),VPDS_BAALENZ,RE_VPDS_BAALENZ),           32
     *(NAMES_VPDS(36),NVPDS_BAALENZ)                                      32
      EQUIVALENCE (COMMEN_VPDS(36),CVPDS_BAALENZ),(MAP_VPDS(1,36),        32
     *MVPDS_BAALENZ)                                                      32
      CHARACTER CVPDS_BAALENZ*40/' '/                                     32
      CHARACTER NVPDS_BAALENZ*12/'R.BAALENZ'/                             32
      INTEGER MVPDS_BAALENZ(2)/1,1/                                       32
      REAL RE_VPDS_BAALENZ                                                32
C -                                                                       32
      REAL VPDS_BSBLENY                                                   32
      EQUIVALENCE (BANK_VPDS(39),VPDS_BSBLENY,RE_VPDS_BSBLENY),           32
     *(NAMES_VPDS(37),NVPDS_BSBLENY)                                      32
      EQUIVALENCE (COMMEN_VPDS(37),CVPDS_BSBLENY),(MAP_VPDS(1,37),        32
     *MVPDS_BSBLENY)                                                      32
      CHARACTER CVPDS_BSBLENY*40/' '/                                     32
      CHARACTER NVPDS_BSBLENY*12/'R.BSBLENY'/                             32
      INTEGER MVPDS_BSBLENY(2)/1,1/                                       32
      REAL RE_VPDS_BSBLENY                                                32
C -                                                                       32
      REAL VPDS_BSCLENX                                                   32
      EQUIVALENCE (BANK_VPDS(40),VPDS_BSCLENX,RE_VPDS_BSCLENX),           32
     *(NAMES_VPDS(38),NVPDS_BSCLENX)                                      32
      EQUIVALENCE (COMMEN_VPDS(38),CVPDS_BSCLENX),(MAP_VPDS(1,38),        32
     *MVPDS_BSCLENX)                                                      32
      CHARACTER CVPDS_BSCLENX*40/' '/                                     32
      CHARACTER NVPDS_BSCLENX*12/'R.BSCLENX'/                             32
      INTEGER MVPDS_BSCLENX(2)/1,1/                                       32
      REAL RE_VPDS_BSCLENX                                                32
C -                                                                       32
      REAL VPDS_BSCLENY                                                   32
      EQUIVALENCE (BANK_VPDS(41),VPDS_BSCLENY,RE_VPDS_BSCLENY),           32
     *(NAMES_VPDS(39),NVPDS_BSCLENY)                                      32
      EQUIVALENCE (COMMEN_VPDS(39),CVPDS_BSCLENY),(MAP_VPDS(1,39),        32
     *MVPDS_BSCLENY)                                                      32
      CHARACTER CVPDS_BSCLENY*40/' '/                                     32
      CHARACTER NVPDS_BSCLENY*12/'R.BSCLENY'/                             32
      INTEGER MVPDS_BSCLENY(2)/1,1/                                       32
      REAL RE_VPDS_BSCLENY                                                32
C -                                                                       32
      REAL VPDS_BSCLENZ                                                   32
      EQUIVALENCE (BANK_VPDS(42),VPDS_BSCLENZ,RE_VPDS_BSCLENZ),           32
     *(NAMES_VPDS(40),NVPDS_BSCLENZ)                                      32
      EQUIVALENCE (COMMEN_VPDS(40),CVPDS_BSCLENZ),(MAP_VPDS(1,40),        32
     *MVPDS_BSCLENZ)                                                      32
      CHARACTER CVPDS_BSCLENZ*40/' '/                                     32
      CHARACTER NVPDS_BSCLENZ*12/'R.BSCLENZ'/                             32
      INTEGER MVPDS_BSCLENZ(2)/1,1/                                       32
      REAL RE_VPDS_BSCLENZ                                                32
C -                                                                       32
      REAL VPDS_BACLENZ                                                   32
      EQUIVALENCE (BANK_VPDS(43),VPDS_BACLENZ,RE_VPDS_BACLENZ),           32
     *(NAMES_VPDS(41),NVPDS_BACLENZ)                                      32
      EQUIVALENCE (COMMEN_VPDS(41),CVPDS_BACLENZ),(MAP_VPDS(1,41),        32
     *MVPDS_BACLENZ)                                                      32
      CHARACTER CVPDS_BACLENZ*40/' '/                                     32
      CHARACTER NVPDS_BACLENZ*12/'R.BACLENZ'/                             32
      INTEGER MVPDS_BACLENZ(2)/1,1/                                       32
      REAL RE_VPDS_BACLENZ                                                32
C -                                                                       32
      REAL VPDS_BSDLENX                                                   32
      EQUIVALENCE (BANK_VPDS(44),VPDS_BSDLENX,RE_VPDS_BSDLENX),           32
     *(NAMES_VPDS(42),NVPDS_BSDLENX)                                      32
      EQUIVALENCE (COMMEN_VPDS(42),CVPDS_BSDLENX),(MAP_VPDS(1,42),        32
     *MVPDS_BSDLENX)                                                      32
      CHARACTER CVPDS_BSDLENX*40/' '/                                     32
      CHARACTER NVPDS_BSDLENX*12/'R.BSDLENX'/                             32
      INTEGER MVPDS_BSDLENX(2)/1,1/                                       32
      REAL RE_VPDS_BSDLENX                                                32
C -                                                                       32
      REAL VPDS_BSELENY                                                   32
      EQUIVALENCE (BANK_VPDS(45),VPDS_BSELENY,RE_VPDS_BSELENY),           32
     *(NAMES_VPDS(43),NVPDS_BSELENY)                                      32
      EQUIVALENCE (COMMEN_VPDS(43),CVPDS_BSELENY),(MAP_VPDS(1,43),        32
     *MVPDS_BSELENY)                                                      32
      CHARACTER CVPDS_BSELENY*40/' '/                                     32
      CHARACTER NVPDS_BSELENY*12/'R.BSELENY'/                             32
      INTEGER MVPDS_BSELENY(2)/1,1/                                       32
      REAL RE_VPDS_BSELENY                                                32
C -                                                                       32
      REAL VPDS_BSELENZ                                                   32
      EQUIVALENCE (BANK_VPDS(46),VPDS_BSELENZ,RE_VPDS_BSELENZ),           32
     *(NAMES_VPDS(44),NVPDS_BSELENZ)                                      32
      EQUIVALENCE (COMMEN_VPDS(44),CVPDS_BSELENZ),(MAP_VPDS(1,44),        32
     *MVPDS_BSELENZ)                                                      32
      CHARACTER CVPDS_BSELENZ*40/' '/                                     32
      CHARACTER NVPDS_BSELENZ*12/'R.BSELENZ'/                             32
      INTEGER MVPDS_BSELENZ(2)/1,1/                                       32
      REAL RE_VPDS_BSELENZ                                                32
C -                                                                       32
      REAL VPDS_BSFRMAX                                                   32
      EQUIVALENCE (BANK_VPDS(47),VPDS_BSFRMAX,RE_VPDS_BSFRMAX),           32
     *(NAMES_VPDS(45),NVPDS_BSFRMAX)                                      32
      EQUIVALENCE (COMMEN_VPDS(45),CVPDS_BSFRMAX),(MAP_VPDS(1,45),        32
     *MVPDS_BSFRMAX)                                                      32
      CHARACTER CVPDS_BSFRMAX*40/' '/                                     32
      CHARACTER NVPDS_BSFRMAX*12/'R.BSFRMAX'/                             32
      INTEGER MVPDS_BSFRMAX(2)/1,1/                                       32
      REAL RE_VPDS_BSFRMAX                                                32
C -                                                                       32
      REAL VPDS_BSFLENZ                                                   32
      EQUIVALENCE (BANK_VPDS(48),VPDS_BSFLENZ,RE_VPDS_BSFLENZ),           32
     *(NAMES_VPDS(46),NVPDS_BSFLENZ)                                      32
      EQUIVALENCE (COMMEN_VPDS(46),CVPDS_BSFLENZ),(MAP_VPDS(1,46),        32
     *MVPDS_BSFLENZ)                                                      32
      CHARACTER CVPDS_BSFLENZ*40/' '/                                     32
      CHARACTER NVPDS_BSFLENZ*12/'R.BSFLENZ'/                             32
      INTEGER MVPDS_BSFLENZ(2)/1,1/                                       32
      REAL RE_VPDS_BSFLENZ                                                32
C -                                                                       32
      REAL VPDS_BSSLENZ                                                   32
      EQUIVALENCE (BANK_VPDS(49),VPDS_BSSLENZ,RE_VPDS_BSSLENZ),           32
     *(NAMES_VPDS(47),NVPDS_BSSLENZ)                                      32
      EQUIVALENCE (COMMEN_VPDS(47),CVPDS_BSSLENZ),(MAP_VPDS(1,47),        32
     *MVPDS_BSSLENZ)                                                      32
      CHARACTER CVPDS_BSSLENZ*40/' '/                                     32
      CHARACTER NVPDS_BSSLENZ*12/'R.BSSLENZ'/                             32
      INTEGER MVPDS_BSSLENZ(2)/1,1/                                       32
      REAL RE_VPDS_BSSLENZ                                                32
C -                                                                       32
      REAL VPDS_BSGRMAX                                                   32
      EQUIVALENCE (BANK_VPDS(50),VPDS_BSGRMAX,RE_VPDS_BSGRMAX),           32
     *(NAMES_VPDS(48),NVPDS_BSGRMAX)                                      32
      EQUIVALENCE (COMMEN_VPDS(48),CVPDS_BSGRMAX),(MAP_VPDS(1,48),        32
     *MVPDS_BSGRMAX)                                                      32
      CHARACTER CVPDS_BSGRMAX*40/' '/                                     32
      CHARACTER NVPDS_BSGRMAX*12/'R.BSGRMAX'/                             32
      INTEGER MVPDS_BSGRMAX(2)/1,1/                                       32
      REAL RE_VPDS_BSGRMAX                                                32
C -                                                                       32
      REAL VPDS_BSGLENZ1                                                  32
      EQUIVALENCE (BANK_VPDS(51),VPDS_BSGLENZ1,RE_VPDS_BSGLENZ1),         32
     *(NAMES_VPDS(49),NVPDS_BSGLENZ1)                                     32
      EQUIVALENCE (COMMEN_VPDS(49),CVPDS_BSGLENZ1),(MAP_VPDS(1,49),       32
     *MVPDS_BSGLENZ1)                                                     32
      CHARACTER CVPDS_BSGLENZ1*40/' '/                                    32
      CHARACTER NVPDS_BSGLENZ1*12/'R.BSGLENZ1'/                           32
      INTEGER MVPDS_BSGLENZ1(2)/1,1/                                      32
      REAL RE_VPDS_BSGLENZ1                                               32
C -                                                                       32
      REAL VPDS_BSGLENZ2                                                  32
      EQUIVALENCE (BANK_VPDS(52),VPDS_BSGLENZ2,RE_VPDS_BSGLENZ2),         32
     *(NAMES_VPDS(50),NVPDS_BSGLENZ2)                                     32
      EQUIVALENCE (COMMEN_VPDS(50),CVPDS_BSGLENZ2),(MAP_VPDS(1,50),       32
     *MVPDS_BSGLENZ2)                                                     32
      CHARACTER CVPDS_BSGLENZ2*40/' '/                                    32
      CHARACTER NVPDS_BSGLENZ2*12/'R.BSGLENZ2'/                           32
      INTEGER MVPDS_BSGLENZ2(2)/1,1/                                      32
      REAL RE_VPDS_BSGLENZ2                                               32
C -                                                                       32
      REAL VPDS_BSGLENZ3                                                  32
      EQUIVALENCE (BANK_VPDS(53),VPDS_BSGLENZ3,RE_VPDS_BSGLENZ3),         32
     *(NAMES_VPDS(51),NVPDS_BSGLENZ3)                                     32
      EQUIVALENCE (COMMEN_VPDS(51),CVPDS_BSGLENZ3),(MAP_VPDS(1,51),       32
     *MVPDS_BSGLENZ3)                                                     32
      CHARACTER CVPDS_BSGLENZ3*40/' '/                                    32
      CHARACTER NVPDS_BSGLENZ3*12/'R.BSGLENZ3'/                           32
      INTEGER MVPDS_BSGLENZ3(2)/1,1/                                      32
      REAL RE_VPDS_BSGLENZ3                                               32
C -                                                                       32
      REAL VPDS_BSHLENZ                                                   32
      EQUIVALENCE (BANK_VPDS(54),VPDS_BSHLENZ,RE_VPDS_BSHLENZ),           32
     *(NAMES_VPDS(52),NVPDS_BSHLENZ)                                      32
      EQUIVALENCE (COMMEN_VPDS(52),CVPDS_BSHLENZ),(MAP_VPDS(1,52),        32
     *MVPDS_BSHLENZ)                                                      32
      CHARACTER CVPDS_BSHLENZ*40/' '/                                     32
      CHARACTER NVPDS_BSHLENZ*12/'R.BSHLENZ'/                             32
      INTEGER MVPDS_BSHLENZ(2)/1,1/                                       32
      REAL RE_VPDS_BSHLENZ                                                32
C -                                                                       32
      REAL VPDS_ELECWID                                                   32
      EQUIVALENCE (BANK_VPDS(55),VPDS_ELECWID,RE_VPDS_ELECWID),           32
     *(NAMES_VPDS(53),NVPDS_ELECWID)                                      32
      EQUIVALENCE (COMMEN_VPDS(53),CVPDS_ELECWID),(MAP_VPDS(1,53),        32
     *MVPDS_ELECWID)                                                      32
      CHARACTER CVPDS_ELECWID*40/' '/                                     32
      CHARACTER NVPDS_ELECWID*12/'R.ELECWID'/                             32
      INTEGER MVPDS_ELECWID(2)/1,1/                                       32
      REAL RE_VPDS_ELECWID                                                32
C -                                                                       32
      REAL VPDS_ELECTHCK                                                  32
      EQUIVALENCE (BANK_VPDS(56),VPDS_ELECTHCK,RE_VPDS_ELECTHCK),         32
     *(NAMES_VPDS(54),NVPDS_ELECTHCK)                                     32
      EQUIVALENCE (COMMEN_VPDS(54),CVPDS_ELECTHCK),(MAP_VPDS(1,54),       32
     *MVPDS_ELECTHCK)                                                     32
      CHARACTER CVPDS_ELECTHCK*40/' '/                                    32
      CHARACTER NVPDS_ELECTHCK*12/'R.ELECTHCK'/                           32
      INTEGER MVPDS_ELECTHCK(2)/1,1/                                      32
      REAL RE_VPDS_ELECTHCK                                               32
C -                                                                       32
      REAL VPDS_ELECLEN                                                   32
      EQUIVALENCE (BANK_VPDS(57),VPDS_ELECLEN,RE_VPDS_ELECLEN),           32
     *(NAMES_VPDS(55),NVPDS_ELECLEN)                                      32
      EQUIVALENCE (COMMEN_VPDS(55),CVPDS_ELECLEN),(MAP_VPDS(1,55),        32
     *MVPDS_ELECLEN)                                                      32
      CHARACTER CVPDS_ELECLEN*40/' '/                                     32
      CHARACTER NVPDS_ELECLEN*12/'R.ELECLEN'/                             32
      INTEGER MVPDS_ELECLEN(2)/1,1/                                       32
      REAL RE_VPDS_ELECLEN                                                32
C -                                                                       32
      REAL VPDS_VFEEPOSX                                                  32
      EQUIVALENCE (BANK_VPDS(58),VPDS_VFEEPOSX,RE_VPDS_VFEEPOSX),         32
     *(NAMES_VPDS(56),NVPDS_VFEEPOSX)                                     32
      EQUIVALENCE (COMMEN_VPDS(56),CVPDS_VFEEPOSX),(MAP_VPDS(1,56),       32
     *MVPDS_VFEEPOSX)                                                     32
      CHARACTER CVPDS_VFEEPOSX*40/' '/                                    32
      CHARACTER NVPDS_VFEEPOSX*12/'R.VFEEPOSX'/                           32
      INTEGER MVPDS_VFEEPOSX(2)/1,1/                                      32
      REAL RE_VPDS_VFEEPOSX                                               32
C -                                                                       32
      REAL VPDS_VFEEPOSY                                                  32
      EQUIVALENCE (BANK_VPDS(59),VPDS_VFEEPOSY,RE_VPDS_VFEEPOSY),         32
     *(NAMES_VPDS(57),NVPDS_VFEEPOSY)                                     32
      EQUIVALENCE (COMMEN_VPDS(57),CVPDS_VFEEPOSY),(MAP_VPDS(1,57),       32
     *MVPDS_VFEEPOSY)                                                     32
      CHARACTER CVPDS_VFEEPOSY*40/' '/                                    32
      CHARACTER NVPDS_VFEEPOSY*12/'R.VFEEPOSY'/                           32
      INTEGER MVPDS_VFEEPOSY(2)/1,1/                                      32
      REAL RE_VPDS_VFEEPOSY                                               32
C -                                                                       32
      REAL VPDS_VFEEPOSZ                                                  32
      EQUIVALENCE (BANK_VPDS(60),VPDS_VFEEPOSZ,RE_VPDS_VFEEPOSZ),         32
     *(NAMES_VPDS(58),NVPDS_VFEEPOSZ)                                     32
      EQUIVALENCE (COMMEN_VPDS(58),CVPDS_VFEEPOSZ),(MAP_VPDS(1,58),       32
     *MVPDS_VFEEPOSZ)                                                     32
      CHARACTER CVPDS_VFEEPOSZ*40/' '/                                    32
      CHARACTER NVPDS_VFEEPOSZ*12/'R.VFEEPOSZ'/                           32
      INTEGER MVPDS_VFEEPOSZ(2)/1,1/                                      32
      REAL RE_VPDS_VFEEPOSZ                                               32
C -                                                                       32
      REAL VPDS_VLEMPOSX(15)                                              32
      EQUIVALENCE (BANK_VPDS(61),VPDS_VLEMPOSX,RE_VPDS_VLEMPOSX),         32
     *(NAMES_VPDS(59),NVPDS_VLEMPOSX)                                     32
      EQUIVALENCE (COMMEN_VPDS(59),CVPDS_VLEMPOSX),(MAP_VPDS(1,59),       32
     *MVPDS_VLEMPOSX)                                                     32
      CHARACTER CVPDS_VLEMPOSX*40/' '/                                    32
      CHARACTER NVPDS_VLEMPOSX*12/'R.VLEMPOSX'/                           32
      INTEGER MVPDS_VLEMPOSX(2)/15,1/                                     32
      REAL RE_VPDS_VLEMPOSX                                               32
C -                                                                       32
      REAL VPDS_VLEMPOSY                                                  32
      EQUIVALENCE (BANK_VPDS(76),VPDS_VLEMPOSY,RE_VPDS_VLEMPOSY),         32
     *(NAMES_VPDS(60),NVPDS_VLEMPOSY)                                     32
      EQUIVALENCE (COMMEN_VPDS(60),CVPDS_VLEMPOSY),(MAP_VPDS(1,60),       32
     *MVPDS_VLEMPOSY)                                                     32
      CHARACTER CVPDS_VLEMPOSY*40/' '/                                    32
      CHARACTER NVPDS_VLEMPOSY*12/'R.VLEMPOSY'/                           32
      INTEGER MVPDS_VLEMPOSY(2)/1,1/                                      32
      REAL RE_VPDS_VLEMPOSY                                               32
C -                                                                       32
      REAL VPDS_VLEMPOSZ(15)                                              32
      EQUIVALENCE (BANK_VPDS(77),VPDS_VLEMPOSZ,RE_VPDS_VLEMPOSZ),         32
     *(NAMES_VPDS(61),NVPDS_VLEMPOSZ)                                     32
      EQUIVALENCE (COMMEN_VPDS(61),CVPDS_VLEMPOSZ),(MAP_VPDS(1,61),       32
     *MVPDS_VLEMPOSZ)                                                     32
      CHARACTER CVPDS_VLEMPOSZ*40/' '/                                    32
      CHARACTER NVPDS_VLEMPOSZ*12/'R.VLEMPOSZ'/                           32
      INTEGER MVPDS_VLEMPOSZ(2)/15,1/                                     32
      REAL RE_VPDS_VLEMPOSZ                                               32
C -                                                                       32
      REAL VPDS_VLEMLENX                                                  32
      EQUIVALENCE (BANK_VPDS(92),VPDS_VLEMLENX,RE_VPDS_VLEMLENX),         32
     *(NAMES_VPDS(62),NVPDS_VLEMLENX)                                     32
      EQUIVALENCE (COMMEN_VPDS(62),CVPDS_VLEMLENX),(MAP_VPDS(1,62),       32
     *MVPDS_VLEMLENX)                                                     32
      CHARACTER CVPDS_VLEMLENX*40/' '/                                    32
      CHARACTER NVPDS_VLEMLENX*12/'R.VLEMLENX'/                           32
      INTEGER MVPDS_VLEMLENX(2)/1,1/                                      32
      REAL RE_VPDS_VLEMLENX                                               32
C -                                                                       32
      REAL VPDS_VLEMLENY                                                  32
      EQUIVALENCE (BANK_VPDS(93),VPDS_VLEMLENY,RE_VPDS_VLEMLENY),         32
     *(NAMES_VPDS(63),NVPDS_VLEMLENY)                                     32
      EQUIVALENCE (COMMEN_VPDS(63),CVPDS_VLEMLENY),(MAP_VPDS(1,63),       32
     *MVPDS_VLEMLENY)                                                     32
      CHARACTER CVPDS_VLEMLENY*40/' '/                                    32
      CHARACTER NVPDS_VLEMLENY*12/'R.VLEMLENY'/                           32
      INTEGER MVPDS_VLEMLENY(2)/1,1/                                      32
      REAL RE_VPDS_VLEMLENY                                               32
C -                                                                       32
      REAL VPDS_VLEMLENZ                                                  32
      EQUIVALENCE (BANK_VPDS(94),VPDS_VLEMLENZ,RE_VPDS_VLEMLENZ),         32
     *(NAMES_VPDS(64),NVPDS_VLEMLENZ)                                     32
      EQUIVALENCE (COMMEN_VPDS(64),CVPDS_VLEMLENZ),(MAP_VPDS(1,64),       32
     *MVPDS_VLEMLENZ)                                                     32
      CHARACTER CVPDS_VLEMLENZ*40/' '/                                    32
      CHARACTER NVPDS_VLEMLENZ*12/'R.VLEMLENZ'/                           32
      INTEGER MVPDS_VLEMLENZ(2)/1,1/                                      32
      REAL RE_VPDS_VLEMLENZ                                               32
C -                                                                       32
      REAL VPDS_VPIPPOSX                                                  32
      EQUIVALENCE (BANK_VPDS(95),VPDS_VPIPPOSX,RE_VPDS_VPIPPOSX),         32
     *(NAMES_VPDS(65),NVPDS_VPIPPOSX)                                     32
      EQUIVALENCE (COMMEN_VPDS(65),CVPDS_VPIPPOSX),(MAP_VPDS(1,65),       32
     *MVPDS_VPIPPOSX)                                                     32
      CHARACTER CVPDS_VPIPPOSX*40/' '/                                    32
      CHARACTER NVPDS_VPIPPOSX*12/'R.VPIPPOSX'/                           32
      INTEGER MVPDS_VPIPPOSX(2)/1,1/                                      32
      REAL RE_VPDS_VPIPPOSX                                               32
C -                                                                       32
      REAL VPDS_VPIPPOSY                                                  32
      EQUIVALENCE (BANK_VPDS(96),VPDS_VPIPPOSY,RE_VPDS_VPIPPOSY),         32
     *(NAMES_VPDS(66),NVPDS_VPIPPOSY)                                     32
      EQUIVALENCE (COMMEN_VPDS(66),CVPDS_VPIPPOSY),(MAP_VPDS(1,66),       32
     *MVPDS_VPIPPOSY)                                                     32
      CHARACTER CVPDS_VPIPPOSY*40/' '/                                    32
      CHARACTER NVPDS_VPIPPOSY*12/'R.VPIPPOSY'/                           32
      INTEGER MVPDS_VPIPPOSY(2)/1,1/                                      32
      REAL RE_VPDS_VPIPPOSY                                               32
C -                                                                       32
      REAL VPDS_VPIPPOSZ                                                  32
      EQUIVALENCE (BANK_VPDS(97),VPDS_VPIPPOSZ,RE_VPDS_VPIPPOSZ),         32
     *(NAMES_VPDS(67),NVPDS_VPIPPOSZ)                                     32
      EQUIVALENCE (COMMEN_VPDS(67),CVPDS_VPIPPOSZ),(MAP_VPDS(1,67),       32
     *MVPDS_VPIPPOSZ)                                                     32
      CHARACTER CVPDS_VPIPPOSZ*40/' '/                                    32
      CHARACTER NVPDS_VPIPPOSZ*12/'R.VPIPPOSZ'/                           32
      INTEGER MVPDS_VPIPPOSZ(2)/1,1/                                      32
      REAL RE_VPDS_VPIPPOSZ                                               32
C -                                                                       32
      REAL VPDS_VPIPRMIN                                                  32
      EQUIVALENCE (BANK_VPDS(98),VPDS_VPIPRMIN,RE_VPDS_VPIPRMIN),         32
     *(NAMES_VPDS(68),NVPDS_VPIPRMIN)                                     32
      EQUIVALENCE (COMMEN_VPDS(68),CVPDS_VPIPRMIN),(MAP_VPDS(1,68),       32
     *MVPDS_VPIPRMIN)                                                     32
      CHARACTER CVPDS_VPIPRMIN*40/' '/                                    32
      CHARACTER NVPDS_VPIPRMIN*12/'R.VPIPRMIN'/                           32
      INTEGER MVPDS_VPIPRMIN(2)/1,1/                                      32
      REAL RE_VPDS_VPIPRMIN                                               32
C -                                                                       32
      REAL VPDS_VPIPRMAX                                                  32
      EQUIVALENCE (BANK_VPDS(99),VPDS_VPIPRMAX,RE_VPDS_VPIPRMAX),         32
     *(NAMES_VPDS(69),NVPDS_VPIPRMAX)                                     32
      EQUIVALENCE (COMMEN_VPDS(69),CVPDS_VPIPRMAX),(MAP_VPDS(1,69),       32
     *MVPDS_VPIPRMAX)                                                     32
      CHARACTER CVPDS_VPIPRMAX*40/' '/                                    32
      CHARACTER NVPDS_VPIPRMAX*12/'R.VPIPRMAX'/                           32
      INTEGER MVPDS_VPIPRMAX(2)/1,1/                                      32
      REAL RE_VPDS_VPIPRMAX                                               32
C -                                                                       32
      REAL VPDS_VPIPLENZ                                                  32
      EQUIVALENCE (BANK_VPDS(100),VPDS_VPIPLENZ,RE_VPDS_VPIPLENZ),        32
     *(NAMES_VPDS(70),NVPDS_VPIPLENZ)                                     32
      EQUIVALENCE (COMMEN_VPDS(70),CVPDS_VPIPLENZ),(MAP_VPDS(1,70),       32
     *MVPDS_VPIPLENZ)                                                     32
      CHARACTER CVPDS_VPIPLENZ*40/' '/                                    32
      CHARACTER NVPDS_VPIPLENZ*12/'R.VPIPLENZ'/                           32
      INTEGER MVPDS_VPIPLENZ(2)/1,1/                                      32
      REAL RE_VPDS_VPIPLENZ                                               32
C -                                                                       32
      INTEGER LEN_VPDS,LLEN_VPDS,LINK_VPDS                                32
      PARAMETER (LEN_VPDS=100)                                            32
      PARAMETER (LLEN_VPDS=70)                                            32
      INTEGER FLAG_VPDS/0/,MAP_VPDS(2,LLEN_VPDS),IDNUM_VPDS(10)           32
      CHARACTER NAMES_VPDS(LLEN_VPDS)*12,COMMEN_VPDS(LLEN_VPDS)*40        32
      DATA IDNUM_VPDS/10*0/                                               32
      REAL BANK_VPDS(LEN_VPDS)                                            32
      CHARACTER*60 BPATH_VPDS/' '/                                        32
      SAVE FLAG_VPDS,MAP_VPDS,IDNUM_VPDS,NAMES_VPDS,COMMEN_VPDS,          32
     *BPATH_VPDS                                                          32
      DATA LINK_VPDS/0/,BANK_VPDS/LEN_VPDS*0/                             32
C *                                                                       32
C -                                                                       34
      REAL WBSP_IBEAMD(4)                                                 34
      EQUIVALENCE (BANK_WBSP(3),WBSP_IBEAMD,RE_WBSP_IBEAMD),              34
     *(NAMES_WBSP(1),NWBSP_IBEAMD)                                        34
      EQUIVALENCE (COMMEN_WBSP(1),CWBSP_IBEAMD),(MAP_WBSP(1,1),           34
     *MWBSP_IBEAMD)                                                       34
      CHARACTER CWBSP_IBEAMD*40/' '/                                      34
      CHARACTER NWBSP_IBEAMD*12/'R.IBEAMD'/                               34
      INTEGER MWBSP_IBEAMD(2)/4,1/                                        34
      REAL RE_WBSP_IBEAMD                                                 34
C -                                                                       34
      REAL WBSP_IBEAMP(3)                                                 34
      EQUIVALENCE (BANK_WBSP(7),WBSP_IBEAMP,RE_WBSP_IBEAMP),              34
     *(NAMES_WBSP(2),NWBSP_IBEAMP)                                        34
      EQUIVALENCE (COMMEN_WBSP(2),CWBSP_IBEAMP),(MAP_WBSP(1,2),           34
     *MWBSP_IBEAMP)                                                       34
      CHARACTER CWBSP_IBEAMP*40/' '/                                      34
      CHARACTER NWBSP_IBEAMP*12/'R.IBEAMP'/                               34
      INTEGER MWBSP_IBEAMP(2)/3,1/                                        34
      REAL RE_WBSP_IBEAMP                                                 34
C -                                                                       34
      REAL WBSP_POSTD(4)                                                  34
      EQUIVALENCE (BANK_WBSP(10),WBSP_POSTD,RE_WBSP_POSTD),               34
     *(NAMES_WBSP(3),NWBSP_POSTD)                                         34
      EQUIVALENCE (COMMEN_WBSP(3),CWBSP_POSTD),(MAP_WBSP(1,3),            34
     *MWBSP_POSTD)                                                        34
      CHARACTER CWBSP_POSTD*40/' '/                                       34
      CHARACTER NWBSP_POSTD*12/'R.POSTD'/                                 34
      INTEGER MWBSP_POSTD(2)/4,1/                                         34
      REAL RE_WBSP_POSTD                                                  34
C -                                                                       34
      REAL WBSP_POSTP(3)                                                  34
      EQUIVALENCE (BANK_WBSP(14),WBSP_POSTP,RE_WBSP_POSTP),               34
     *(NAMES_WBSP(4),NWBSP_POSTP)                                         34
      EQUIVALENCE (COMMEN_WBSP(4),CWBSP_POSTP),(MAP_WBSP(1,4),            34
     *MWBSP_POSTP)                                                        34
      CHARACTER CWBSP_POSTP*40/' '/                                       34
      CHARACTER NWBSP_POSTP*12/'R.POSTP'/                                 34
      INTEGER MWBSP_POSTP(2)/3,1/                                         34
      REAL RE_WBSP_POSTP                                                  34
C -                                                                       34
      REAL WBSP_ANGLD(4)                                                  34
      EQUIVALENCE (BANK_WBSP(17),WBSP_ANGLD,RE_WBSP_ANGLD),               34
     *(NAMES_WBSP(5),NWBSP_ANGLD)                                         34
      EQUIVALENCE (COMMEN_WBSP(5),CWBSP_ANGLD),(MAP_WBSP(1,5),            34
     *MWBSP_ANGLD)                                                        34
      CHARACTER CWBSP_ANGLD*40/' '/                                       34
      CHARACTER NWBSP_ANGLD*12/'R.ANGLD'/                                 34
      INTEGER MWBSP_ANGLD(2)/4,1/                                         34
      REAL RE_WBSP_ANGLD                                                  34
C -                                                                       34
      REAL WBSP_ANGLP(3)                                                  34
      EQUIVALENCE (BANK_WBSP(21),WBSP_ANGLP,RE_WBSP_ANGLP),               34
     *(NAMES_WBSP(6),NWBSP_ANGLP)                                         34
      EQUIVALENCE (COMMEN_WBSP(6),CWBSP_ANGLP),(MAP_WBSP(1,6),            34
     *MWBSP_ANGLP)                                                        34
      CHARACTER CWBSP_ANGLP*40/' '/                                       34
      CHARACTER NWBSP_ANGLP*12/'R.ANGLP'/                                 34
      INTEGER MWBSP_ANGLP(2)/3,1/                                         34
      REAL RE_WBSP_ANGLP                                                  34
C -                                                                       34
      REAL WBSP_TRIP(3)                                                   34
      EQUIVALENCE (BANK_WBSP(24),WBSP_TRIP,RE_WBSP_TRIP),(NAMES_WBSP(7),  34
     *NWBSP_TRIP)                                                         34
      EQUIVALENCE (COMMEN_WBSP(7),CWBSP_TRIP),(MAP_WBSP(1,7),             34
     *MWBSP_TRIP)                                                         34
      CHARACTER CWBSP_TRIP*40/' '/                                        34
      CHARACTER NWBSP_TRIP*12/'R.TRIP'/                                   34
      INTEGER MWBSP_TRIP(2)/3,1/                                          34
      REAL RE_WBSP_TRIP                                                   34
C -                                                                       34
      REAL WBSP_TRID(3)                                                   34
      EQUIVALENCE (BANK_WBSP(27),WBSP_TRID,RE_WBSP_TRID),(NAMES_WBSP(8),  34
     *NWBSP_TRID)                                                         34
      EQUIVALENCE (COMMEN_WBSP(8),CWBSP_TRID),(MAP_WBSP(1,8),             34
     *MWBSP_TRID)                                                         34
      CHARACTER CWBSP_TRID*40/' '/                                        34
      CHARACTER NWBSP_TRID*12/'R.TRID'/                                   34
      INTEGER MWBSP_TRID(2)/3,1/                                          34
      REAL RE_WBSP_TRID                                                   34
C -                                                                       34
      INTEGER LEN_WBSP,LLEN_WBSP,LINK_WBSP                                34
      PARAMETER (LEN_WBSP=29)                                             34
      PARAMETER (LLEN_WBSP=8)                                             34
      INTEGER FLAG_WBSP/0/,MAP_WBSP(2,LLEN_WBSP),IDNUM_WBSP(10)           34
      CHARACTER NAMES_WBSP(LLEN_WBSP)*12,COMMEN_WBSP(LLEN_WBSP)*40        34
      DATA IDNUM_WBSP/10*0/                                               34
      REAL BANK_WBSP(LEN_WBSP)                                            34
      CHARACTER*60 BPATH_WBSP/' '/                                        34
      SAVE FLAG_WBSP,MAP_WBSP,IDNUM_WBSP,NAMES_WBSP,COMMEN_WBSP,          34
     *BPATH_WBSP                                                          34
      DATA LINK_WBSP/0/,BANK_WBSP/LEN_WBSP*0/                             34
C *                                                                       34
      REAL YBASE,YTOP,CONVLENGTH,DETANGLE,STRUTHEIGHT,YDISPL              35
      REAL XLOC,YLOC,ZLOC,ZPOS,TEMPOS                                     37
      REAL ZPOSE,ZPOSW,EWSHIFT,UDSHIFT,BOLTSHIFT,LOCANG,PHIANG,DETZINT    39
      INTEGER ISEC                                                        41
      INTEGER KDETSTYLE,KIBEAMSTYLE                                       43
C *                                                                       45
      CALL AGSBEGM(MODULE,IPRIN)                                          45
      IF (IPRIN.LT.0) RETURN                                              45
C Filling bank VPDV - VPD CONFIGURATION CONTROL                           45
      BANK_TITLE='VPD CONFIGURATION CONTROL'                              45
      BPATH_VPDV='VPDV*'                                                  45
      IDNUM_VPDV(1)=0                                                     46
C DEFAULT VERSION (SET IN GEOMETRY.G)                                     46
      VPDV_VERSION=2                                                      46
      CVPDV_VERSION='DEFAULT VERSION (SET IN GEOMETRY.G)'                 46
C DEFAULT CONFIG (SET IN GEOMETRY.G)                                      47
      VPDV_VPDCONFIG=1                                                    47
      CVPDV_VPDCONFIG='DEFAULT CONFIG (SET IN GEOMETRY.G)'                47
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDV',    48
     *BPATH_VPDV,IDNUM_VPDV,1, MAP_VPDV,NAMES_VPDV,COMMEN_VPDV,           48
     *BANK_VPDV,LEN_VPDV,LLEN_VPDV,LINK_VPDV,FLAG_VPDV)                   48
C *                                                                       48
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDV',    48
     *BPATH_VPDV,IDNUM_VPDV,1, MAP_VPDV,NAMES_VPDV,COMMEN_VPDV,           48
     *BANK_VPDV,LEN_VPDV,LLEN_VPDV,LINK_VPDV,FLAG_VPDV)                   48
C *                                                                       49
      CALL AGSBEGM(MODULE,IPRIN)                                          49
      IF (IPRIN.LT.0) RETURN                                              49
C Filling bank VPDG - PVPD BASIC DIMENSIONS                               49
      BANK_TITLE='PVPD BASIC DIMENSIONS'                                  49
      BPATH_VPDG='VPDG*'                                                  49
      IDNUM_VPDG(1)=0                                                     50
C GEOMETRY VERSION                                                        50
      VPDG_VERSION=1                                                      50
      CVPDG_VERSION='GEOMETRY VERSION'                                    50
C Z POSITION EAST                                                         51
      VPDG_ZPOSEAST=550.0000                                              51
      CVPDG_ZPOSEAST='Z POSITION EAST'                                    51
C Z POSITION WEST                                                         52
      VPDG_ZPOSWEST=550.0000                                              52
      CVPDG_ZPOSWEST='Z POSITION WEST'                                    52
C MOTHERVOLUME RMIN (2.5INCH)                                             53
      VPDG_RMIN=6.35                                                      53
      CVPDG_RMIN='MOTHERVOLUME RMIN (2.5INCH)'                            53
C MOTHERVOLUME RMIN (12.3125INCH)                                         54
      VPDG_RMAX=31.27375                                                  54
      CVPDG_RMAX='MOTHERVOLUME RMIN (12.3125INCH)'                        54
C BASEPLATE WIDTH (12INCH)                                                55
      VPDG_BPWIDTH=30.48                                                  55
      CVPDG_BPWIDTH='BASEPLATE WIDTH (12INCH)'                            55
C BASEPLATE LENGTH (18.5INCH)                                             56
      VPDG_BPLENGTH=46.355                                                56
      CVPDG_BPLENGTH='BASEPLATE LENGTH (18.5INCH)'                        56
C BASEPLATE THICKNESS (0.25INCH)                                          57
      VPDG_BPTHICK=0.635                                                  57
      CVPDG_BPTHICK='BASEPLATE THICKNESS (0.25INCH)'                      57
C FEE BOX HEIGHT (2INCH)                                                  58
      VPDG_BXHEIGHT=5.08                                                  58
      CVPDG_BXHEIGHT='FEE BOX HEIGHT (2INCH)'                             58
C FEE BOX LENGTH (6INCH)                                                  59
      VPDG_BXLENGTH=15.24                                                 59
      CVPDG_BXLENGTH='FEE BOX LENGTH (6INCH)'                             59
C FEE BOX WIDTH (10INCH)                                                  60
      VPDG_BXWIDTH=25.4                                                   60
      CVPDG_BXWIDTH='FEE BOX WIDTH (10INCH)'                              60
C FEE BOX PLATE THICKNESS (0.25INCH)                                      61
      VPDG_BXTHICK=0.635                                                  61
      CVPDG_BXTHICK='FEE BOX PLATE THICKNESS (0.25INCH)'                  61
C FEE CENTRAL Z POS (+/-??) (0.25INCH)                                    62
      VPDG_BXZPOSC=0.635                                                  62
      CVPDG_BXZPOSC='FEE CENTRAL Z POS (+/-??) (0.25INCH)'                62
C FRONTPLATE WIDTH (3INCH)                                                63
      VPDG_FPWIDTH=7.62                                                   63
      CVPDG_FPWIDTH='FRONTPLATE WIDTH (3INCH)'                            63
C FRONTPLATE HEIGHT (10.023INCH)                                          64
      VPDG_FPHEIGHT=25.45842                                              64
      CVPDG_FPHEIGHT='FRONTPLATE HEIGHT (10.023INCH)'                     64
C FRONTPLATE THICKNESS (1INCH)                                            65
      VPDG_FPTHICK=2.54                                                   65
      CVPDG_FPTHICK='FRONTPLATE THICKNESS (1INCH)'                        65
C FRONTPL.PART A WIDTH (2.614INCH)                                        66
      VPDG_FPAWIDTH=6.63956                                               66
      CVPDG_FPAWIDTH='FRONTPL.PART A WIDTH (2.614INCH)'                   66
C FRONTPL.PART A HEIGHT (3.289INCH)                                       67
      VPDG_FPAHGHT=8.35405                                                67
      CVPDG_FPAHGHT='FRONTPL.PART A HEIGHT (3.289INCH)'                   67
C FRONTPL.PART C HEIGHT (4.95INCH)                                        68
      VPDG_FPCHGHT=12.573                                                 68
      CVPDG_FPCHGHT='FRONTPL.PART C HEIGHT (4.95INCH)'                    68
C FRONTPL. HOOK HEIGHT (1.5INCH)                                          69
      VPDG_FPHHGHT=3.81                                                   69
      CVPDG_FPHHGHT='FRONTPL. HOOK HEIGHT (1.5INCH)'                      69
C FRONTPL. HOOK WIDTH (2INCH)                                             70
      VPDG_FPHWIDTH=5.08                                                  70
      CVPDG_FPHWIDTH='FRONTPL. HOOK WIDTH (2INCH)'                        70
C FRONTPL. HOOK THICKNESS (0.25INCH)                                      71
      VPDG_FPHTHICK=0.635                                                 71
      CVPDG_FPHTHICK='FRONTPL. HOOK THICKNESS (0.25INCH)'                 71
C STRUT THICKNESS (0.25INCH)                                              72
      VPDG_STTHICK=0.635                                                  72
      CVPDG_STTHICK='STRUT THICKNESS (0.25INCH)'                          72
C STRUT HEIGHT (9.125 INCH)                                               73
      VPDG_STHEIGHT=23.1775                                               73
      CVPDG_STHEIGHT='STRUT HEIGHT (9.125 INCH)'                          73
C STRUT ANGLE (DEGR)                                                      74
      VPDG_STANGLE=25.                                                    74
      CVPDG_STANGLE='STRUT ANGLE (DEGR)'                                  74
C DIAGNOAL STRUT SIZE (2INCH)                                             75
      VPDG_STDIAGSZ=5.08                                                  75
      CVPDG_STDIAGSZ='DIAGNOAL STRUT SIZE (2INCH)'                        75
C STRUT CLAMP WIDTH (3INCH)                                               76
      VPDG_SCWIDTH=7.62                                                   76
      CVPDG_SCWIDTH='STRUT CLAMP WIDTH (3INCH)'                           76
C STRUT CLAMP HEIGHT (1.5INCH)                                            77
      VPDG_SCHEIGHT=3.81                                                  77
      CVPDG_SCHEIGHT='STRUT CLAMP HEIGHT (1.5INCH)'                       77
C STRUT CLAMP LENGTH (1.5INCH)                                            78
      VPDG_SCLENGTH=3.81                                                  78
      CVPDG_SCLENGTH='STRUT CLAMP LENGTH (1.5INCH)'                       78
C STRUT CLAMP THICKNESS (0.25INCH)                                        79
      VPDG_SCTHICK=0.635                                                  79
      CVPDG_SCTHICK='STRUT CLAMP THICKNESS (0.25INCH)'                    79
C CLAMP HEIGHT (1.375INCH)                                                80
      VPDG_CLHEIGHT=3.4925                                                80
      CVPDG_CLHEIGHT='CLAMP HEIGHT (1.375INCH)'                           80
C CLAMP WIDTH (1.5INCH)                                                   81
      VPDG_CLWIDTH=3.81                                                   81
      CVPDG_CLWIDTH='CLAMP WIDTH (1.5INCH)'                               81
C CLAMP LENGTH (16.25INCH)                                                82
      VPDG_CLLENGTH=41.275                                                82
      CVPDG_CLLENGTH='CLAMP LENGTH (16.25INCH)'                           82
C CLAMP PLATE THICKNESS (0.125INCH)                                       83
      VPDG_CLTHICK=0.3175                                                 83
      CVPDG_CLTHICK='CLAMP PLATE THICKNESS (0.125INCH)'                   83
C PMT ASSEMBLY LENGTH (13INCH)                                            84
      VPDG_DETLEN=33.02                                                   84
      CVPDG_DETLEN='PMT ASSEMBLY LENGTH (13INCH)'                         84
C PMT ASSEMBLY RADIUS (1.5INCH)                                           85
      VPDG_DETRAD=3.81                                                    85
      CVPDG_DETRAD='PMT ASSEMBLY RADIUS (1.5INCH)'                        85
C PMT ASS. FRONTPLATE THICKNESS (0.4INCH)                                 86
      VPDG_DETFRONT=1.016                                                 86
      CVPDG_DETFRONT='PMT ASS. FRONTPLATE THICKNESS (0.4INCH)'            86
C CONVERTER LAYER THICKNESS                                               87
      VPDG_CONVTHK=1.                                                     87
      CVPDG_CONVTHK='CONVERTER LAYER THICKNESS'                           87
C RADIATOR LAYER THICKNESS                                                88
      VPDG_RADITHK=1.                                                     88
      CVPDG_RADITHK='RADIATOR LAYER THICKNESS'                            88
C ELECTRONICS MOUNT LENGTH                                                89
      VPDG_ELELENG=15.0                                                   89
      CVPDG_ELELENG='ELECTRONICS MOUNT LENGTH'                            89
C LAYER RADIAL WIDTH                                                      90
      VPDG_DRLAYER=6.                                                     90
      CVPDG_DRLAYER='LAYER RADIAL WIDTH'                                  90
C NUMBER OF PMT IN LAYER                                                  91
      VPDG_NUMPMT=3                                                       91
      CVPDG_NUMPMT='NUMBER OF PMT IN LAYER'                               91
C PMT WALL THICKNESS                                                      92
      VPDG_PMTWALL=0.1                                                    92
      CVPDG_PMTWALL='PMT WALL THICKNESS'                                  92
C PMT AND DETECTOR RADIUS                                                 93
      VPDG_PMTRAD=2.54                                                    93
      CVPDG_PMTRAD='PMT AND DETECTOR RADIUS'                              93
C PMT TUBE LENGTH                                                         94
      VPDG_PMTLEN=8.0                                                     94
      CVPDG_PMTLEN='PMT TUBE LENGTH'                                      94
C ACTIVE/DE-ACTIVATE (0) IBEAM                                            95
      VPDG_IBCHOICE=1                                                     95
      CVPDG_IBCHOICE='ACTIVE/DE-ACTIVATE (0) IBEAM'                       95
C IBEAM CENTRAL YLOCATION (6.5INCH)                                       96
      VPDG_IBPOSYC=-16.51                                                 96
      CVPDG_IBPOSYC='IBEAM CENTRAL YLOCATION (6.5INCH)'                   96
C IBEAM CENTRAL ZPOSITION                                                 97
      VPDG_IBPOSZC=530.477                                                97
      CVPDG_IBPOSZC='IBEAM CENTRAL ZPOSITION'                             97
C IBEAM LENGTH (116.1INCH)                                                98
      VPDG_IBLENG=294.894                                                 98
      CVPDG_IBLENG='IBEAM LENGTH (116.1INCH)'                             98
C IBEAM HORIZ. PLATE THICKNESS (0.29INCH)                                 99
      VPDG_IBTHICKH=0.7366                                                99
      CVPDG_IBTHICKH='IBEAM HORIZ. PLATE THICKNESS (0.29INCH)'            99
C IBEAM VERT. PLATE THICKNESS (0.17INCH)                                  100
      VPDG_IBTHICKV=0.4318                                                100
      CVPDG_IBTHICKV='IBEAM VERT. PLATE THICKNESS (0.17INCH)'             100
C IBEAM HEIGHT (4INCH)                                                    101
      VPDG_IBHEIGHT=10.16                                                 101
      CVPDG_IBHEIGHT='IBEAM HEIGHT (4INCH)'                               101
C IBEAM WIDTH (3INCH)                                                     102
      VPDG_IBWIDTH=7.62                                                   102
      CVPDG_IBWIDTH='IBEAM WIDTH (3INCH)'                                 102
C IBEAM VERT. FRONT PIECE (9INCH)                                         103
      VPDG_IBWLEN=22.86                                                   103
      CVPDG_IBWLEN='IBEAM VERT. FRONT PIECE (9INCH)'                      103
C IBEAM VERT. FRONT PIECE HEIGHT                                          104
      VPDG_IBWHGHTF=4.4489                                                104
      CVPDG_IBWHGHTF='IBEAM VERT. FRONT PIECE HEIGHT'                     104
C IBEAM VERT. FRONT PIECE HEIGHT                                          105
      VPDG_IBWHGHTB=8.3097                                                105
      CVPDG_IBWHGHTB='IBEAM VERT. FRONT PIECE HEIGHT'                     105
C EAST WEST Z SHIFT                                                       106
      VPDG_EWSHIFT=0.0                                                    106
      CVPDG_EWSHIFT='EAST WEST Z SHIFT'                                   106
C UP DOWN Y SHIFT                                                         107
      VPDG_UDSHIFT=0.0                                                    107
      CVPDG_UDSHIFT='UP DOWN Y SHIFT'                                     107
C BOLT ADDITIONAL Y SHIFT                                                 108
      VPDG_BOLTSHIFT=0.0                                                  108
      CVPDG_BOLTSHIFT='BOLT ADDITIONAL Y SHIFT'                           108
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    109
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           109
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   109
C *                                                                       109
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    109
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           109
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   109
C *                                                                       110
      CALL AGSBEGM(MODULE,IPRIN)                                          110
      IF (IPRIN.LT.0) RETURN                                              110
C Filling bank VPDG - PVPD BASIC DIMENSIONS                               110
      BANK_TITLE='PVPD BASIC DIMENSIONS'                                  110
      BPATH_VPDG='VPDG*'                                                  110
      IDNUM_VPDG(1)=0                                                     111
C GEOMETRY VERSION                                                        111
      VPDG_VERSION=2                                                      111
      CVPDG_VERSION='GEOMETRY VERSION'                                    111
C Z POSITION EAST                                                         112
      VPDG_ZPOSEAST=561.2638                                              112
      CVPDG_ZPOSEAST='Z POSITION EAST'                                    112
C Z POSITION WEST                                                         113
      VPDG_ZPOSWEST=563.1688                                              113
      CVPDG_ZPOSWEST='Z POSITION WEST'                                    113
C MOTHERVOLUME RMIN (2.5INCH)                                             114
      VPDG_RMIN=6.35                                                      114
      CVPDG_RMIN='MOTHERVOLUME RMIN (2.5INCH)'                            114
C MOTHERVOLUME RMIN (12.3125INCH)                                         115
      VPDG_RMAX=31.27375                                                  115
      CVPDG_RMAX='MOTHERVOLUME RMIN (12.3125INCH)'                        115
C BASEPLATE WIDTH (12INCH)                                                116
      VPDG_BPWIDTH=30.48                                                  116
      CVPDG_BPWIDTH='BASEPLATE WIDTH (12INCH)'                            116
C BASEPLATE LENGTH (18.5INCH)                                             117
      VPDG_BPLENGTH=46.355                                                117
      CVPDG_BPLENGTH='BASEPLATE LENGTH (18.5INCH)'                        117
C BASEPLATE THICKNESS (0.25INCH)                                          118
      VPDG_BPTHICK=0.635                                                  118
      CVPDG_BPTHICK='BASEPLATE THICKNESS (0.25INCH)'                      118
C FEE BOX HEIGHT (2INCH)                                                  119
      VPDG_BXHEIGHT=5.08                                                  119
      CVPDG_BXHEIGHT='FEE BOX HEIGHT (2INCH)'                             119
C FEE BOX LENGTH (6INCH)                                                  120
      VPDG_BXLENGTH=15.24                                                 120
      CVPDG_BXLENGTH='FEE BOX LENGTH (6INCH)'                             120
C FEE BOX WIDTH (10INCH)                                                  121
      VPDG_BXWIDTH=25.4                                                   121
      CVPDG_BXWIDTH='FEE BOX WIDTH (10INCH)'                              121
C FEE BOX PLATE THICKNESS (0.25INCH)                                      122
      VPDG_BXTHICK=0.635                                                  122
      CVPDG_BXTHICK='FEE BOX PLATE THICKNESS (0.25INCH)'                  122
C FEE CENTRAL Z POS (0.25INCH)                                            123
      VPDG_BXZPOSC=0.635                                                  123
      CVPDG_BXZPOSC='FEE CENTRAL Z POS (0.25INCH)'                        123
C FRONTPLATE WIDTH (3INCH)                                                124
      VPDG_FPWIDTH=7.62                                                   124
      CVPDG_FPWIDTH='FRONTPLATE WIDTH (3INCH)'                            124
C FRONTPLATE HEIGHT (10.023INCH)                                          125
      VPDG_FPHEIGHT=25.45842                                              125
      CVPDG_FPHEIGHT='FRONTPLATE HEIGHT (10.023INCH)'                     125
C FRONTPLATE THICKNESS (1INCH)                                            126
      VPDG_FPTHICK=2.54                                                   126
      CVPDG_FPTHICK='FRONTPLATE THICKNESS (1INCH)'                        126
C FRONTPL.PART A WIDTH (2.614INCH)                                        127
      VPDG_FPAWIDTH=6.63956                                               127
      CVPDG_FPAWIDTH='FRONTPL.PART A WIDTH (2.614INCH)'                   127
C FRONTPL.PART A HEIGHT (3.289INCH)                                       128
      VPDG_FPAHGHT=8.35405                                                128
      CVPDG_FPAHGHT='FRONTPL.PART A HEIGHT (3.289INCH)'                   128
C FRONTPL.PART C HEIGHT (4.95INCH)                                        129
      VPDG_FPCHGHT=12.573                                                 129
      CVPDG_FPCHGHT='FRONTPL.PART C HEIGHT (4.95INCH)'                    129
C FRONTPL. HOOK HEIGHT (1.5INCH)                                          130
      VPDG_FPHHGHT=3.81                                                   130
      CVPDG_FPHHGHT='FRONTPL. HOOK HEIGHT (1.5INCH)'                      130
C FRONTPL. HOOK WIDTH (2INCH)                                             131
      VPDG_FPHWIDTH=5.08                                                  131
      CVPDG_FPHWIDTH='FRONTPL. HOOK WIDTH (2INCH)'                        131
C FRONTPL. HOOK THICKNESS (0.25INCH)                                      132
      VPDG_FPHTHICK=0.635                                                 132
      CVPDG_FPHTHICK='FRONTPL. HOOK THICKNESS (0.25INCH)'                 132
C STRUT THICKNESS (0.25INCH)                                              133
      VPDG_STTHICK=0.635                                                  133
      CVPDG_STTHICK='STRUT THICKNESS (0.25INCH)'                          133
C STRUT HEIGHT (9.125 INCH)                                               134
      VPDG_STHEIGHT=23.1775                                               134
      CVPDG_STHEIGHT='STRUT HEIGHT (9.125 INCH)'                          134
C STRUT ANGLE (DEGR)                                                      135
      VPDG_STANGLE=25.                                                    135
      CVPDG_STANGLE='STRUT ANGLE (DEGR)'                                  135
C DIAGNOAL STRUT SIZE (2INCH)                                             136
      VPDG_STDIAGSZ=5.08                                                  136
      CVPDG_STDIAGSZ='DIAGNOAL STRUT SIZE (2INCH)'                        136
C STRUT CLAMP WIDTH (3INCH)                                               137
      VPDG_SCWIDTH=7.62                                                   137
      CVPDG_SCWIDTH='STRUT CLAMP WIDTH (3INCH)'                           137
C STRUT CLAMP HEIGHT (1.5INCH)                                            138
      VPDG_SCHEIGHT=3.81                                                  138
      CVPDG_SCHEIGHT='STRUT CLAMP HEIGHT (1.5INCH)'                       138
C STRUT CLAMP LENGTH (1.5INCH)                                            139
      VPDG_SCLENGTH=3.81                                                  139
      CVPDG_SCLENGTH='STRUT CLAMP LENGTH (1.5INCH)'                       139
C STRUT CLAMP THICKNESS (0.25INCH)                                        140
      VPDG_SCTHICK=0.635                                                  140
      CVPDG_SCTHICK='STRUT CLAMP THICKNESS (0.25INCH)'                    140
C CLAMP HEIGHT (1.375INCH)                                                141
      VPDG_CLHEIGHT=3.4925                                                141
      CVPDG_CLHEIGHT='CLAMP HEIGHT (1.375INCH)'                           141
C CLAMP WIDTH (1.5INCH)                                                   142
      VPDG_CLWIDTH=3.81                                                   142
      CVPDG_CLWIDTH='CLAMP WIDTH (1.5INCH)'                               142
C CLAMP LENGTH (16.25INCH)                                                143
      VPDG_CLLENGTH=41.275                                                143
      CVPDG_CLLENGTH='CLAMP LENGTH (16.25INCH)'                           143
C CLAMP PLATE THICKNESS (0.125INCH)                                       144
      VPDG_CLTHICK=0.3175                                                 144
      CVPDG_CLTHICK='CLAMP PLATE THICKNESS (0.125INCH)'                   144
C PMT ASSEMBLY LENGTH (13INCH)                                            145
      VPDG_DETLEN=33.02                                                   145
      CVPDG_DETLEN='PMT ASSEMBLY LENGTH (13INCH)'                         145
C PMT ASSEMBLY RADIUS (1.5INCH)                                           146
      VPDG_DETRAD=3.81                                                    146
      CVPDG_DETRAD='PMT ASSEMBLY RADIUS (1.5INCH)'                        146
C PMT ASS. FRONTPLATE THICKNESS (0.4INCH)                                 147
      VPDG_DETFRONT=1.016                                                 147
      CVPDG_DETFRONT='PMT ASS. FRONTPLATE THICKNESS (0.4INCH)'            147
C CONVERTER LAYER THICKNESS                                               148
      VPDG_CONVTHK=1.                                                     148
      CVPDG_CONVTHK='CONVERTER LAYER THICKNESS'                           148
C RADIATOR LAYER THICKNESS                                                149
      VPDG_RADITHK=1.                                                     149
      CVPDG_RADITHK='RADIATOR LAYER THICKNESS'                            149
C ELECTRONICS MOUNT LENGTH                                                150
      VPDG_ELELENG=15.0                                                   150
      CVPDG_ELELENG='ELECTRONICS MOUNT LENGTH'                            150
C LAYER RADIAL WIDTH                                                      151
      VPDG_DRLAYER=6.                                                     151
      CVPDG_DRLAYER='LAYER RADIAL WIDTH'                                  151
C NUMBER OF PMT IN LAYER                                                  152
      VPDG_NUMPMT=3                                                       152
      CVPDG_NUMPMT='NUMBER OF PMT IN LAYER'                               152
C PMT WALL THICKNESS                                                      153
      VPDG_PMTWALL=0.1                                                    153
      CVPDG_PMTWALL='PMT WALL THICKNESS'                                  153
C PMT AND DETECTOR RADIUS                                                 154
      VPDG_PMTRAD=2.54                                                    154
      CVPDG_PMTRAD='PMT AND DETECTOR RADIUS'                              154
C PMT TUBE LENGTH                                                         155
      VPDG_PMTLEN=8.0                                                     155
      CVPDG_PMTLEN='PMT TUBE LENGTH'                                      155
C ACTIVE/DE-ACTIVATE (0) IBEAM                                            156
      VPDG_IBCHOICE=1                                                     156
      CVPDG_IBCHOICE='ACTIVE/DE-ACTIVATE (0) IBEAM'                       156
C IBEAM CENTRAL YLOCATION (6.5INCH)                                       157
      VPDG_IBPOSYC=-16.51                                                 157
      CVPDG_IBPOSYC='IBEAM CENTRAL YLOCATION (6.5INCH)'                   157
C IBEAM CENTRAL ZPOSITION                                                 158
      VPDG_IBPOSZC=530.477                                                158
      CVPDG_IBPOSZC='IBEAM CENTRAL ZPOSITION'                             158
C IBEAM LENGTH (116.1INCH)                                                159
      VPDG_IBLENG=294.894                                                 159
      CVPDG_IBLENG='IBEAM LENGTH (116.1INCH)'                             159
C IBEAM HORIZ. PLATE THICKNESS (0.29INCH)                                 160
      VPDG_IBTHICKH=0.7366                                                160
      CVPDG_IBTHICKH='IBEAM HORIZ. PLATE THICKNESS (0.29INCH)'            160
C IBEAM VERT. PLATE THICKNESS (0.17INCH)                                  161
      VPDG_IBTHICKV=0.4318                                                161
      CVPDG_IBTHICKV='IBEAM VERT. PLATE THICKNESS (0.17INCH)'             161
C IBEAM HEIGHT (4INCH)                                                    162
      VPDG_IBHEIGHT=10.16                                                 162
      CVPDG_IBHEIGHT='IBEAM HEIGHT (4INCH)'                               162
C IBEAM WIDTH (3INCH)                                                     163
      VPDG_IBWIDTH=7.62                                                   163
      CVPDG_IBWIDTH='IBEAM WIDTH (3INCH)'                                 163
C IBEAM VERT. FRONT PIECE (9INCH)                                         164
      VPDG_IBWLEN=22.86                                                   164
      CVPDG_IBWLEN='IBEAM VERT. FRONT PIECE (9INCH)'                      164
C IBEAM VERT. FRONT PIECE HEIGHT                                          165
      VPDG_IBWHGHTF=4.4489                                                165
      CVPDG_IBWHGHTF='IBEAM VERT. FRONT PIECE HEIGHT'                     165
C IBEAM VERT. FRONT PIECE HEIGHT                                          166
      VPDG_IBWHGHTB=8.3097                                                166
      CVPDG_IBWHGHTB='IBEAM VERT. FRONT PIECE HEIGHT'                     166
C EAST WEST Z SHIFT                                                       167
      VPDG_EWSHIFT=0.0                                                    167
      CVPDG_EWSHIFT='EAST WEST Z SHIFT'                                   167
C UP DOWN Y SHIFT                                                         168
      VPDG_UDSHIFT=0.0                                                    168
      CVPDG_UDSHIFT='UP DOWN Y SHIFT'                                     168
C BOLT ADDITIONAL Y SHIFT                                                 169
      VPDG_BOLTSHIFT=0.0                                                  169
      CVPDG_BOLTSHIFT='BOLT ADDITIONAL Y SHIFT'                           169
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    170
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           170
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   170
C *                                                                       170
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    170
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           170
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   170
C *                                                                       171
      CALL AGSBEGM(MODULE,IPRIN)                                          171
      IF (IPRIN.LT.0) RETURN                                              171
C Filling bank VPDG - PVPD BASIC DIMENSIONS                               171
      BANK_TITLE='PVPD BASIC DIMENSIONS'                                  171
      BPATH_VPDG='VPDG*'                                                  171
      IDNUM_VPDG(1)=0                                                     172
C GEOMETRY VERSION                                                        172
      VPDG_VERSION=3                                                      172
      CVPDG_VERSION='GEOMETRY VERSION'                                    172
C Z POSITION EAST                                                         173
      VPDG_ZPOSEAST=564.4388                                              173
      CVPDG_ZPOSEAST='Z POSITION EAST'                                    173
C Z POSITION WEST                                                         174
      VPDG_ZPOSWEST=563.4069                                              174
      CVPDG_ZPOSWEST='Z POSITION WEST'                                    174
C MOTHERVOLUME RMIN (2.5INCH)                                             175
      VPDG_RMIN=6.35                                                      175
      CVPDG_RMIN='MOTHERVOLUME RMIN (2.5INCH)'                            175
C MOTHERVOLUME RMIN (12.3125INCH)                                         176
      VPDG_RMAX=31.27375                                                  176
      CVPDG_RMAX='MOTHERVOLUME RMIN (12.3125INCH)'                        176
C BASEPLATE WIDTH (12INCH)                                                177
      VPDG_BPWIDTH=30.48                                                  177
      CVPDG_BPWIDTH='BASEPLATE WIDTH (12INCH)'                            177
C BASEPLATE LENGTH (18.5INCH)                                             178
      VPDG_BPLENGTH=46.355                                                178
      CVPDG_BPLENGTH='BASEPLATE LENGTH (18.5INCH)'                        178
C BASEPLATE THICKNESS (0.25INCH)                                          179
      VPDG_BPTHICK=0.635                                                  179
      CVPDG_BPTHICK='BASEPLATE THICKNESS (0.25INCH)'                      179
C FEE BOX HEIGHT (2INCH)                                                  180
      VPDG_BXHEIGHT=5.08                                                  180
      CVPDG_BXHEIGHT='FEE BOX HEIGHT (2INCH)'                             180
C FEE BOX LENGTH (6INCH)                                                  181
      VPDG_BXLENGTH=15.24                                                 181
      CVPDG_BXLENGTH='FEE BOX LENGTH (6INCH)'                             181
C FEE BOX WIDTH (10INCH)                                                  182
      VPDG_BXWIDTH=25.4                                                   182
      CVPDG_BXWIDTH='FEE BOX WIDTH (10INCH)'                              182
C FEE BOX PLATE THICKNESS (0.25INCH)                                      183
      VPDG_BXTHICK=0.635                                                  183
      CVPDG_BXTHICK='FEE BOX PLATE THICKNESS (0.25INCH)'                  183
C FEE CENTRAL Z POS (0.25INCH)                                            184
      VPDG_BXZPOSC=0.635                                                  184
      CVPDG_BXZPOSC='FEE CENTRAL Z POS (0.25INCH)'                        184
C FRONTPLATE WIDTH (3INCH)                                                185
      VPDG_FPWIDTH=7.62                                                   185
      CVPDG_FPWIDTH='FRONTPLATE WIDTH (3INCH)'                            185
C FRONTPLATE HEIGHT (10.023INCH)                                          186
      VPDG_FPHEIGHT=25.45842                                              186
      CVPDG_FPHEIGHT='FRONTPLATE HEIGHT (10.023INCH)'                     186
C FRONTPLATE THICKNESS (1INCH)                                            187
      VPDG_FPTHICK=2.54                                                   187
      CVPDG_FPTHICK='FRONTPLATE THICKNESS (1INCH)'                        187
C FRONTPL.PART A WIDTH (2.614INCH)                                        188
      VPDG_FPAWIDTH=6.63956                                               188
      CVPDG_FPAWIDTH='FRONTPL.PART A WIDTH (2.614INCH)'                   188
C FRONTPL.PART A HEIGHT (3.289INCH)                                       189
      VPDG_FPAHGHT=8.35405                                                189
      CVPDG_FPAHGHT='FRONTPL.PART A HEIGHT (3.289INCH)'                   189
C FRONTPL.PART C HEIGHT (4.95INCH)                                        190
      VPDG_FPCHGHT=12.573                                                 190
      CVPDG_FPCHGHT='FRONTPL.PART C HEIGHT (4.95INCH)'                    190
C FRONTPL. HOOK HEIGHT (1.5INCH)                                          191
      VPDG_FPHHGHT=3.81                                                   191
      CVPDG_FPHHGHT='FRONTPL. HOOK HEIGHT (1.5INCH)'                      191
C FRONTPL. HOOK WIDTH (2INCH)                                             192
      VPDG_FPHWIDTH=5.08                                                  192
      CVPDG_FPHWIDTH='FRONTPL. HOOK WIDTH (2INCH)'                        192
C FRONTPL. HOOK THICKNESS (0.25INCH)                                      193
      VPDG_FPHTHICK=0.635                                                 193
      CVPDG_FPHTHICK='FRONTPL. HOOK THICKNESS (0.25INCH)'                 193
C STRUT THICKNESS (0.25INCH)                                              194
      VPDG_STTHICK=0.635                                                  194
      CVPDG_STTHICK='STRUT THICKNESS (0.25INCH)'                          194
C STRUT HEIGHT (9.125 INCH)                                               195
      VPDG_STHEIGHT=23.1775                                               195
      CVPDG_STHEIGHT='STRUT HEIGHT (9.125 INCH)'                          195
C STRUT ANGLE (DEGR)                                                      196
      VPDG_STANGLE=25.                                                    196
      CVPDG_STANGLE='STRUT ANGLE (DEGR)'                                  196
C DIAGNOAL STRUT SIZE (2INCH)                                             197
      VPDG_STDIAGSZ=5.08                                                  197
      CVPDG_STDIAGSZ='DIAGNOAL STRUT SIZE (2INCH)'                        197
C STRUT CLAMP WIDTH (3INCH)                                               198
      VPDG_SCWIDTH=7.62                                                   198
      CVPDG_SCWIDTH='STRUT CLAMP WIDTH (3INCH)'                           198
C STRUT CLAMP HEIGHT (1.5INCH)                                            199
      VPDG_SCHEIGHT=3.81                                                  199
      CVPDG_SCHEIGHT='STRUT CLAMP HEIGHT (1.5INCH)'                       199
C STRUT CLAMP LENGTH (1.5INCH)                                            200
      VPDG_SCLENGTH=3.81                                                  200
      CVPDG_SCLENGTH='STRUT CLAMP LENGTH (1.5INCH)'                       200
C STRUT CLAMP THICKNESS (0.25INCH)                                        201
      VPDG_SCTHICK=0.635                                                  201
      CVPDG_SCTHICK='STRUT CLAMP THICKNESS (0.25INCH)'                    201
C CLAMP HEIGHT (1.375INCH)                                                202
      VPDG_CLHEIGHT=3.4925                                                202
      CVPDG_CLHEIGHT='CLAMP HEIGHT (1.375INCH)'                           202
C CLAMP WIDTH (1.5INCH)                                                   203
      VPDG_CLWIDTH=3.81                                                   203
      CVPDG_CLWIDTH='CLAMP WIDTH (1.5INCH)'                               203
C CLAMP LENGTH (16.25INCH)                                                204
      VPDG_CLLENGTH=41.275                                                204
      CVPDG_CLLENGTH='CLAMP LENGTH (16.25INCH)'                           204
C CLAMP PLATE THICKNESS (0.125INCH)                                       205
      VPDG_CLTHICK=0.3175                                                 205
      CVPDG_CLTHICK='CLAMP PLATE THICKNESS (0.125INCH)'                   205
C PMT ASSEMBLY LENGTH (13INCH)                                            206
      VPDG_DETLEN=33.02                                                   206
      CVPDG_DETLEN='PMT ASSEMBLY LENGTH (13INCH)'                         206
C PMT ASSEMBLY RADIUS (1.5INCH)                                           207
      VPDG_DETRAD=3.81                                                    207
      CVPDG_DETRAD='PMT ASSEMBLY RADIUS (1.5INCH)'                        207
C PMT ASS. FRONTPLATE THICKNESS (0.4INCH)                                 208
      VPDG_DETFRONT=1.016                                                 208
      CVPDG_DETFRONT='PMT ASS. FRONTPLATE THICKNESS (0.4INCH)'            208
C CONVERTER LAYER THICKNESS                                               209
      VPDG_CONVTHK=1.                                                     209
      CVPDG_CONVTHK='CONVERTER LAYER THICKNESS'                           209
C RADIATOR LAYER THICKNESS                                                210
      VPDG_RADITHK=1.                                                     210
      CVPDG_RADITHK='RADIATOR LAYER THICKNESS'                            210
C ELECTRONICS MOUNT LENGTH                                                211
      VPDG_ELELENG=15.0                                                   211
      CVPDG_ELELENG='ELECTRONICS MOUNT LENGTH'                            211
C LAYER RADIAL WIDTH                                                      212
      VPDG_DRLAYER=6.                                                     212
      CVPDG_DRLAYER='LAYER RADIAL WIDTH'                                  212
C NUMBER OF PMT IN LAYER                                                  213
      VPDG_NUMPMT=3                                                       213
      CVPDG_NUMPMT='NUMBER OF PMT IN LAYER'                               213
C PMT WALL THICKNESS                                                      214
      VPDG_PMTWALL=0.1                                                    214
      CVPDG_PMTWALL='PMT WALL THICKNESS'                                  214
C PMT AND DETECTOR RADIUS                                                 215
      VPDG_PMTRAD=2.54                                                    215
      CVPDG_PMTRAD='PMT AND DETECTOR RADIUS'                              215
C PMT TUBE LENGTH                                                         216
      VPDG_PMTLEN=8.0                                                     216
      CVPDG_PMTLEN='PMT TUBE LENGTH'                                      216
C ACTIVE/DE-ACTIVATE (0) IBEAM                                            217
      VPDG_IBCHOICE=1                                                     217
      CVPDG_IBCHOICE='ACTIVE/DE-ACTIVATE (0) IBEAM'                       217
C IBEAM CENTRAL YLOCATION (6.5INCH)                                       218
      VPDG_IBPOSYC=-16.51                                                 218
      CVPDG_IBPOSYC='IBEAM CENTRAL YLOCATION (6.5INCH)'                   218
C IBEAM CENTRAL ZPOSITION                                                 219
      VPDG_IBPOSZC=530.477                                                219
      CVPDG_IBPOSZC='IBEAM CENTRAL ZPOSITION'                             219
C IBEAM LENGTH (116.1INCH)                                                220
      VPDG_IBLENG=294.894                                                 220
      CVPDG_IBLENG='IBEAM LENGTH (116.1INCH)'                             220
C IBEAM HORIZ. PLATE THICKNESS (0.29INCH)                                 221
      VPDG_IBTHICKH=0.7366                                                221
      CVPDG_IBTHICKH='IBEAM HORIZ. PLATE THICKNESS (0.29INCH)'            221
C IBEAM VERT. PLATE THICKNESS (0.17INCH)                                  222
      VPDG_IBTHICKV=0.4318                                                222
      CVPDG_IBTHICKV='IBEAM VERT. PLATE THICKNESS (0.17INCH)'             222
C IBEAM HEIGHT (4INCH)                                                    223
      VPDG_IBHEIGHT=10.16                                                 223
      CVPDG_IBHEIGHT='IBEAM HEIGHT (4INCH)'                               223
C IBEAM WIDTH (3INCH)                                                     224
      VPDG_IBWIDTH=7.62                                                   224
      CVPDG_IBWIDTH='IBEAM WIDTH (3INCH)'                                 224
C IBEAM VERT. FRONT PIECE (9INCH)                                         225
      VPDG_IBWLEN=22.86                                                   225
      CVPDG_IBWLEN='IBEAM VERT. FRONT PIECE (9INCH)'                      225
C IBEAM VERT. FRONT PIECE HEIGHT                                          226
      VPDG_IBWHGHTF=4.4489                                                226
      CVPDG_IBWHGHTF='IBEAM VERT. FRONT PIECE HEIGHT'                     226
C IBEAM VERT. FRONT PIECE HEIGHT                                          227
      VPDG_IBWHGHTB=8.3097                                                227
      CVPDG_IBWHGHTB='IBEAM VERT. FRONT PIECE HEIGHT'                     227
C EAST WEST Z SHIFT                                                       228
      VPDG_EWSHIFT=0.0                                                    228
      CVPDG_EWSHIFT='EAST WEST Z SHIFT'                                   228
C UP DOWN Y SHIFT                                                         229
      VPDG_UDSHIFT=0.0                                                    229
      CVPDG_UDSHIFT='UP DOWN Y SHIFT'                                     229
C BOLT ADDITIONAL Y SHIFT                                                 230
      VPDG_BOLTSHIFT=0.0                                                  230
      CVPDG_BOLTSHIFT='BOLT ADDITIONAL Y SHIFT'                           230
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    231
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           231
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   231
C *                                                                       231
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    231
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           231
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   231
C *                                                                       232
      CALL AGSBEGM(MODULE,IPRIN)                                          232
      IF (IPRIN.LT.0) RETURN                                              232
C Filling bank VPDG - PVPD BASIC DIMENSIONS                               232
      BANK_TITLE='PVPD BASIC DIMENSIONS'                                  232
      BPATH_VPDG='VPDG*'                                                  232
      IDNUM_VPDG(1)=0                                                     233
C GEOMETRY VERSION                                                        233
      VPDG_VERSION=4                                                      233
      CVPDG_VERSION='GEOMETRY VERSION'                                    233
C Z POSITION EAST                                                         234
      VPDG_ZPOSEAST=574.5688                                              234
      CVPDG_ZPOSEAST='Z POSITION EAST'                                    234
C Z POSITION WEST                                                         235
      VPDG_ZPOSWEST=573.5669                                              235
      CVPDG_ZPOSWEST='Z POSITION WEST'                                    235
C MOTHERVOLUME RMIN (2.5INCH)                                             236
      VPDG_RMIN=6.35                                                      236
      CVPDG_RMIN='MOTHERVOLUME RMIN (2.5INCH)'                            236
C MOTHERVOLUME RMIN (12.3125INCH)                                         237
      VPDG_RMAX=31.27375                                                  237
      CVPDG_RMAX='MOTHERVOLUME RMIN (12.3125INCH)'                        237
C BASEPLATE WIDTH (12INCH)                                                238
      VPDG_BPWIDTH=30.48                                                  238
      CVPDG_BPWIDTH='BASEPLATE WIDTH (12INCH)'                            238
C BASEPLATE LENGTH (18.5INCH)                                             239
      VPDG_BPLENGTH=46.355                                                239
      CVPDG_BPLENGTH='BASEPLATE LENGTH (18.5INCH)'                        239
C BASEPLATE THICKNESS (0.25INCH)                                          240
      VPDG_BPTHICK=0.635                                                  240
      CVPDG_BPTHICK='BASEPLATE THICKNESS (0.25INCH)'                      240
C FEE BOX HEIGHT (2INCH)                                                  241
      VPDG_BXHEIGHT=5.08                                                  241
      CVPDG_BXHEIGHT='FEE BOX HEIGHT (2INCH)'                             241
C FEE BOX LENGTH (6INCH)                                                  242
      VPDG_BXLENGTH=15.24                                                 242
      CVPDG_BXLENGTH='FEE BOX LENGTH (6INCH)'                             242
C FEE BOX WIDTH (10INCH)                                                  243
      VPDG_BXWIDTH=25.4                                                   243
      CVPDG_BXWIDTH='FEE BOX WIDTH (10INCH)'                              243
C FEE BOX PLATE THICKNESS (0.25INCH)                                      244
      VPDG_BXTHICK=0.635                                                  244
      CVPDG_BXTHICK='FEE BOX PLATE THICKNESS (0.25INCH)'                  244
C FEE CENTRAL Z POS (0.25INCH)                                            245
      VPDG_BXZPOSC=0.635                                                  245
      CVPDG_BXZPOSC='FEE CENTRAL Z POS (0.25INCH)'                        245
C FRONTPLATE WIDTH (3INCH)                                                246
      VPDG_FPWIDTH=7.62                                                   246
      CVPDG_FPWIDTH='FRONTPLATE WIDTH (3INCH)'                            246
C FRONTPLATE HEIGHT (10.023INCH)                                          247
      VPDG_FPHEIGHT=25.45842                                              247
      CVPDG_FPHEIGHT='FRONTPLATE HEIGHT (10.023INCH)'                     247
C FRONTPLATE THICKNESS (1INCH)                                            248
      VPDG_FPTHICK=2.54                                                   248
      CVPDG_FPTHICK='FRONTPLATE THICKNESS (1INCH)'                        248
C FRONTPL.PART A WIDTH (2.614INCH)                                        249
      VPDG_FPAWIDTH=6.63956                                               249
      CVPDG_FPAWIDTH='FRONTPL.PART A WIDTH (2.614INCH)'                   249
C FRONTPL.PART A HEIGHT (3.289INCH)                                       250
      VPDG_FPAHGHT=8.35405                                                250
      CVPDG_FPAHGHT='FRONTPL.PART A HEIGHT (3.289INCH)'                   250
C FRONTPL.PART C HEIGHT (4.95INCH)                                        251
      VPDG_FPCHGHT=12.573                                                 251
      CVPDG_FPCHGHT='FRONTPL.PART C HEIGHT (4.95INCH)'                    251
C FRONTPL. HOOK HEIGHT (1.5INCH)                                          252
      VPDG_FPHHGHT=3.81                                                   252
      CVPDG_FPHHGHT='FRONTPL. HOOK HEIGHT (1.5INCH)'                      252
C FRONTPL. HOOK WIDTH (2INCH)                                             253
      VPDG_FPHWIDTH=5.08                                                  253
      CVPDG_FPHWIDTH='FRONTPL. HOOK WIDTH (2INCH)'                        253
C FRONTPL. HOOK THICKNESS (0.25INCH)                                      254
      VPDG_FPHTHICK=0.635                                                 254
      CVPDG_FPHTHICK='FRONTPL. HOOK THICKNESS (0.25INCH)'                 254
C STRUT THICKNESS (0.25INCH)                                              255
      VPDG_STTHICK=0.635                                                  255
      CVPDG_STTHICK='STRUT THICKNESS (0.25INCH)'                          255
C STRUT HEIGHT (9.125 INCH)                                               256
      VPDG_STHEIGHT=23.1775                                               256
      CVPDG_STHEIGHT='STRUT HEIGHT (9.125 INCH)'                          256
C STRUT ANGLE (DEGR)                                                      257
      VPDG_STANGLE=25.                                                    257
      CVPDG_STANGLE='STRUT ANGLE (DEGR)'                                  257
C DIAGNOAL STRUT SIZE (2INCH)                                             258
      VPDG_STDIAGSZ=5.08                                                  258
      CVPDG_STDIAGSZ='DIAGNOAL STRUT SIZE (2INCH)'                        258
C STRUT CLAMP WIDTH (3INCH)                                               259
      VPDG_SCWIDTH=7.62                                                   259
      CVPDG_SCWIDTH='STRUT CLAMP WIDTH (3INCH)'                           259
C STRUT CLAMP HEIGHT (1.5INCH)                                            260
      VPDG_SCHEIGHT=3.81                                                  260
      CVPDG_SCHEIGHT='STRUT CLAMP HEIGHT (1.5INCH)'                       260
C STRUT CLAMP LENGTH (1.5INCH)                                            261
      VPDG_SCLENGTH=3.81                                                  261
      CVPDG_SCLENGTH='STRUT CLAMP LENGTH (1.5INCH)'                       261
C STRUT CLAMP THICKNESS (0.25INCH)                                        262
      VPDG_SCTHICK=0.635                                                  262
      CVPDG_SCTHICK='STRUT CLAMP THICKNESS (0.25INCH)'                    262
C CLAMP HEIGHT (1.375INCH)                                                263
      VPDG_CLHEIGHT=3.4925                                                263
      CVPDG_CLHEIGHT='CLAMP HEIGHT (1.375INCH)'                           263
C CLAMP WIDTH (1.5INCH)                                                   264
      VPDG_CLWIDTH=3.81                                                   264
      CVPDG_CLWIDTH='CLAMP WIDTH (1.5INCH)'                               264
C CLAMP LENGTH (16.25INCH)                                                265
      VPDG_CLLENGTH=41.275                                                265
      CVPDG_CLLENGTH='CLAMP LENGTH (16.25INCH)'                           265
C CLAMP PLATE THICKNESS (0.125INCH)                                       266
      VPDG_CLTHICK=0.3175                                                 266
      CVPDG_CLTHICK='CLAMP PLATE THICKNESS (0.125INCH)'                   266
C PMT ASSEMBLY LENGTH (13INCH)                                            267
      VPDG_DETLEN=33.02                                                   267
      CVPDG_DETLEN='PMT ASSEMBLY LENGTH (13INCH)'                         267
C PMT ASSEMBLY RADIUS (1.5INCH)                                           268
      VPDG_DETRAD=3.81                                                    268
      CVPDG_DETRAD='PMT ASSEMBLY RADIUS (1.5INCH)'                        268
C PMT ASS. FRONTPLATE THICKNESS (0.4INCH)                                 269
      VPDG_DETFRONT=1.016                                                 269
      CVPDG_DETFRONT='PMT ASS. FRONTPLATE THICKNESS (0.4INCH)'            269
C CONVERTER LAYER THICKNESS                                               270
      VPDG_CONVTHK=1.                                                     270
      CVPDG_CONVTHK='CONVERTER LAYER THICKNESS'                           270
C RADIATOR LAYER THICKNESS                                                271
      VPDG_RADITHK=1.                                                     271
      CVPDG_RADITHK='RADIATOR LAYER THICKNESS'                            271
C ELECTRONICS MOUNT LENGTH                                                272
      VPDG_ELELENG=15.0                                                   272
      CVPDG_ELELENG='ELECTRONICS MOUNT LENGTH'                            272
C LAYER RADIAL WIDTH                                                      273
      VPDG_DRLAYER=6.                                                     273
      CVPDG_DRLAYER='LAYER RADIAL WIDTH'                                  273
C NUMBER OF PMT IN LAYER                                                  274
      VPDG_NUMPMT=3                                                       274
      CVPDG_NUMPMT='NUMBER OF PMT IN LAYER'                               274
C PMT WALL THICKNESS                                                      275
      VPDG_PMTWALL=0.1                                                    275
      CVPDG_PMTWALL='PMT WALL THICKNESS'                                  275
C PMT AND DETECTOR RADIUS                                                 276
      VPDG_PMTRAD=2.54                                                    276
      CVPDG_PMTRAD='PMT AND DETECTOR RADIUS'                              276
C PMT TUBE LENGTH                                                         277
      VPDG_PMTLEN=8.0                                                     277
      CVPDG_PMTLEN='PMT TUBE LENGTH'                                      277
C ACTIVE/DE-ACTIVATE (0) IBEAM                                            278
      VPDG_IBCHOICE=1                                                     278
      CVPDG_IBCHOICE='ACTIVE/DE-ACTIVATE (0) IBEAM'                       278
C IBEAM CENTRAL YLOCATION (6.5INCH)                                       279
      VPDG_IBPOSYC=-16.51                                                 279
      CVPDG_IBPOSYC='IBEAM CENTRAL YLOCATION (6.5INCH)'                   279
C IBEAM CENTRAL ZPOSITION                                                 280
      VPDG_IBPOSZC=530.477                                                280
      CVPDG_IBPOSZC='IBEAM CENTRAL ZPOSITION'                             280
C IBEAM LENGTH (116.1INCH)                                                281
      VPDG_IBLENG=294.894                                                 281
      CVPDG_IBLENG='IBEAM LENGTH (116.1INCH)'                             281
C IBEAM HORIZ. PLATE THICKNESS (0.29INCH)                                 282
      VPDG_IBTHICKH=0.7366                                                282
      CVPDG_IBTHICKH='IBEAM HORIZ. PLATE THICKNESS (0.29INCH)'            282
C IBEAM VERT. PLATE THICKNESS (0.17INCH)                                  283
      VPDG_IBTHICKV=0.4318                                                283
      CVPDG_IBTHICKV='IBEAM VERT. PLATE THICKNESS (0.17INCH)'             283
C IBEAM HEIGHT (4INCH)                                                    284
      VPDG_IBHEIGHT=10.16                                                 284
      CVPDG_IBHEIGHT='IBEAM HEIGHT (4INCH)'                               284
C IBEAM WIDTH (3INCH)                                                     285
      VPDG_IBWIDTH=7.62                                                   285
      CVPDG_IBWIDTH='IBEAM WIDTH (3INCH)'                                 285
C IBEAM VERT. FRONT PIECE (9INCH)                                         286
      VPDG_IBWLEN=22.86                                                   286
      CVPDG_IBWLEN='IBEAM VERT. FRONT PIECE (9INCH)'                      286
C IBEAM VERT. FRONT PIECE HEIGHT                                          287
      VPDG_IBWHGHTF=4.4489                                                287
      CVPDG_IBWHGHTF='IBEAM VERT. FRONT PIECE HEIGHT'                     287
C IBEAM VERT. FRONT PIECE HEIGHT                                          288
      VPDG_IBWHGHTB=8.3097                                                288
      CVPDG_IBWHGHTB='IBEAM VERT. FRONT PIECE HEIGHT'                     288
C EAST WEST Z SHIFT                                                       289
      VPDG_EWSHIFT=0.0                                                    289
      CVPDG_EWSHIFT='EAST WEST Z SHIFT'                                   289
C UP DOWN Y SHIFT                                                         290
      VPDG_UDSHIFT=0.0                                                    290
      CVPDG_UDSHIFT='UP DOWN Y SHIFT'                                     290
C BOLT ADDITIONAL Y SHIFT                                                 291
      VPDG_BOLTSHIFT=0.0                                                  291
      CVPDG_BOLTSHIFT='BOLT ADDITIONAL Y SHIFT'                           291
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    292
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           292
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   292
C *                                                                       292
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    292
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           292
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   292
C *                                                                       293
      CALL AGSBEGM(MODULE,IPRIN)                                          293
      IF (IPRIN.LT.0) RETURN                                              293
C Filling bank VPDG - PVPD BASIC DIMENSIONS                               293
      BANK_TITLE='PVPD BASIC DIMENSIONS'                                  293
      BPATH_VPDG='VPDG*'                                                  293
      IDNUM_VPDG(1)=0                                                     294
C GEOMETRY VERSION WJL ...USED BY Y2004Y                                  294
      VPDG_VERSION=5                                                      294
      CVPDG_VERSION='GEOMETRY VERSION WJL ...USED BY Y2004Y'              294
C Z POSITION EAST WJL SEE NEXT, PLUS ADD 3/4IN TO TAKE CARE OF E/W OFFSE  295
C T (WAS 574.5688)                                                        295
      VPDG_ZPOSEAST=570.0                                                 295
      CVPDG_ZPOSEAST='Z POSITION EAST WJL SEE NEXT, PLUS ADD 3/4IN TO TA  295
     *KE CARE OF E/W OFFSET (WAS 574.5688)'                               295
C Z POSITION WEST WJL VIA CADD: 362.2+(194.3-11.6)+2.54*(18.25/2)=568.08  296
C  (WAS 573.5669)                                                         296
      VPDG_ZPOSWEST=568.1                                                 296
      CVPDG_ZPOSWEST='Z POSITION WEST WJL VIA CADD: 362.2+(194.3-11.6)+2  296
     *.54*(18.25/2)=568.08 (WAS 573.5669)'                                296
C MOTHERVOLUME RMIN (2.5INCH)                                             297
      VPDG_RMIN=6.35                                                      297
      CVPDG_RMIN='MOTHERVOLUME RMIN (2.5INCH)'                            297
C MOTHERVOLUME RMIN (12.3125INCH)                                         298
      VPDG_RMAX=31.27375                                                  298
      CVPDG_RMAX='MOTHERVOLUME RMIN (12.3125INCH)'                        298
C BASEPLATE WIDTH (12INCH)                                                299
      VPDG_BPWIDTH=30.48                                                  299
      CVPDG_BPWIDTH='BASEPLATE WIDTH (12INCH)'                            299
C BASEPLATE LENGTH (18.5INCH)                                             300
      VPDG_BPLENGTH=46.355                                                300
      CVPDG_BPLENGTH='BASEPLATE LENGTH (18.5INCH)'                        300
C BASEPLATE THICKNESS (0.25INCH)                                          301
      VPDG_BPTHICK=0.635                                                  301
      CVPDG_BPTHICK='BASEPLATE THICKNESS (0.25INCH)'                      301
C FEE BOX HEIGHT (2INCH)                                                  302
      VPDG_BXHEIGHT=5.08                                                  302
      CVPDG_BXHEIGHT='FEE BOX HEIGHT (2INCH)'                             302
C FEE BOX LENGTH (6INCH)                                                  303
      VPDG_BXLENGTH=15.24                                                 303
      CVPDG_BXLENGTH='FEE BOX LENGTH (6INCH)'                             303
C FEE BOX WIDTH (10INCH)                                                  304
      VPDG_BXWIDTH=25.4                                                   304
      CVPDG_BXWIDTH='FEE BOX WIDTH (10INCH)'                              304
C FEE BOX PLATE THICKNESS WJL SHOULD BE 50MILS!! MODIFIED (WAS 0.635)     305
      VPDG_BXTHICK=0.127                                                  305
      CVPDG_BXTHICK='FEE BOX PLATE THICKNESS WJL SHOULD BE 50MILS!! MODI  305
     *FIED (WAS 0.635)'                                                   305
C FEE CENTRAL Z POS (0.25INCH)                                            306
      VPDG_BXZPOSC=0.635                                                  306
      CVPDG_BXZPOSC='FEE CENTRAL Z POS (0.25INCH)'                        306
C FRONTPLATE WIDTH (3INCH)                                                307
      VPDG_FPWIDTH=7.62                                                   307
      CVPDG_FPWIDTH='FRONTPLATE WIDTH (3INCH)'                            307
C FRONTPLATE HEIGHT (10.023INCH)                                          308
      VPDG_FPHEIGHT=25.45842                                              308
      CVPDG_FPHEIGHT='FRONTPLATE HEIGHT (10.023INCH)'                     308
C FRONTPLATE THICKNESS (1INCH)                                            309
      VPDG_FPTHICK=2.54                                                   309
      CVPDG_FPTHICK='FRONTPLATE THICKNESS (1INCH)'                        309
C FRONTPL.PART A WIDTH (2.614INCH)                                        310
      VPDG_FPAWIDTH=6.63956                                               310
      CVPDG_FPAWIDTH='FRONTPL.PART A WIDTH (2.614INCH)'                   310
C FRONTPL.PART A HEIGHT (3.289INCH)                                       311
      VPDG_FPAHGHT=8.35405                                                311
      CVPDG_FPAHGHT='FRONTPL.PART A HEIGHT (3.289INCH)'                   311
C FRONTPL.PART C HEIGHT (4.95INCH)                                        312
      VPDG_FPCHGHT=12.573                                                 312
      CVPDG_FPCHGHT='FRONTPL.PART C HEIGHT (4.95INCH)'                    312
C FRONTPL. HOOK HEIGHT (1.5INCH)                                          313
      VPDG_FPHHGHT=3.81                                                   313
      CVPDG_FPHHGHT='FRONTPL. HOOK HEIGHT (1.5INCH)'                      313
C FRONTPL. HOOK WIDTH (2INCH)                                             314
      VPDG_FPHWIDTH=5.08                                                  314
      CVPDG_FPHWIDTH='FRONTPL. HOOK WIDTH (2INCH)'                        314
C FRONTPL. HOOK THICKNESS (0.25INCH)                                      315
      VPDG_FPHTHICK=0.635                                                 315
      CVPDG_FPHTHICK='FRONTPL. HOOK THICKNESS (0.25INCH)'                 315
C STRUT THICKNESS (0.25INCH)                                              316
      VPDG_STTHICK=0.635                                                  316
      CVPDG_STTHICK='STRUT THICKNESS (0.25INCH)'                          316
C STRUT HEIGHT (9.125 INCH)                                               317
      VPDG_STHEIGHT=23.1775                                               317
      CVPDG_STHEIGHT='STRUT HEIGHT (9.125 INCH)'                          317
C STRUT ANGLE (DEGR)                                                      318
      VPDG_STANGLE=25.                                                    318
      CVPDG_STANGLE='STRUT ANGLE (DEGR)'                                  318
C DIAGNOAL STRUT SIZE (2INCH)                                             319
      VPDG_STDIAGSZ=5.08                                                  319
      CVPDG_STDIAGSZ='DIAGNOAL STRUT SIZE (2INCH)'                        319
C STRUT CLAMP WIDTH (3INCH)                                               320
      VPDG_SCWIDTH=7.62                                                   320
      CVPDG_SCWIDTH='STRUT CLAMP WIDTH (3INCH)'                           320
C STRUT CLAMP HEIGHT (1.5INCH)                                            321
      VPDG_SCHEIGHT=3.81                                                  321
      CVPDG_SCHEIGHT='STRUT CLAMP HEIGHT (1.5INCH)'                       321
C STRUT CLAMP LENGTH (1.5INCH)                                            322
      VPDG_SCLENGTH=3.81                                                  322
      CVPDG_SCLENGTH='STRUT CLAMP LENGTH (1.5INCH)'                       322
C STRUT CLAMP THICKNESS (0.25INCH)                                        323
      VPDG_SCTHICK=0.635                                                  323
      CVPDG_SCTHICK='STRUT CLAMP THICKNESS (0.25INCH)'                    323
C CLAMP HEIGHT (1.375INCH)                                                324
      VPDG_CLHEIGHT=3.4925                                                324
      CVPDG_CLHEIGHT='CLAMP HEIGHT (1.375INCH)'                           324
C CLAMP WIDTH (1.5INCH)                                                   325
      VPDG_CLWIDTH=3.81                                                   325
      CVPDG_CLWIDTH='CLAMP WIDTH (1.5INCH)'                               325
C CLAMP LENGTH (16.25INCH)                                                326
      VPDG_CLLENGTH=41.275                                                326
      CVPDG_CLLENGTH='CLAMP LENGTH (16.25INCH)'                           326
C CLAMP PLATE THICKNESS (0.125INCH)                                       327
      VPDG_CLTHICK=0.3175                                                 327
      CVPDG_CLTHICK='CLAMP PLATE THICKNESS (0.125INCH)'                   327
C PMT ASSEMBLY LENGTH (13INCH)                                            328
      VPDG_DETLEN=33.02                                                   328
      CVPDG_DETLEN='PMT ASSEMBLY LENGTH (13INCH)'                         328
C PMT ASSEMBLY RADIUS (1.5INCH)                                           329
      VPDG_DETRAD=3.81                                                    329
      CVPDG_DETRAD='PMT ASSEMBLY RADIUS (1.5INCH)'                        329
C PMT ASS. FRONTPLATE THICKNESS (0.4INCH)                                 330
      VPDG_DETFRONT=1.016                                                 330
      CVPDG_DETFRONT='PMT ASS. FRONTPLATE THICKNESS (0.4INCH)'            330
C CONVERTER LAYER THICKNESS                                               331
      VPDG_CONVTHK=1.                                                     331
      CVPDG_CONVTHK='CONVERTER LAYER THICKNESS'                           331
C RADIATOR LAYER THICKNESS                                                332
      VPDG_RADITHK=1.                                                     332
      CVPDG_RADITHK='RADIATOR LAYER THICKNESS'                            332
C ELECTRONICS MOUNT LENGTH                                                333
      VPDG_ELELENG=15.0                                                   333
      CVPDG_ELELENG='ELECTRONICS MOUNT LENGTH'                            333
C LAYER RADIAL WIDTH                                                      334
      VPDG_DRLAYER=6.                                                     334
      CVPDG_DRLAYER='LAYER RADIAL WIDTH'                                  334
C NUMBER OF PMT IN LAYER                                                  335
      VPDG_NUMPMT=3                                                       335
      CVPDG_NUMPMT='NUMBER OF PMT IN LAYER'                               335
C PMT WALL THICKNESS                                                      336
      VPDG_PMTWALL=0.1                                                    336
      CVPDG_PMTWALL='PMT WALL THICKNESS'                                  336
C PMT AND DETECTOR RADIUS                                                 337
      VPDG_PMTRAD=2.54                                                    337
      CVPDG_PMTRAD='PMT AND DETECTOR RADIUS'                              337
C PMT TUBE LENGTH                                                         338
      VPDG_PMTLEN=8.0                                                     338
      CVPDG_PMTLEN='PMT TUBE LENGTH'                                      338
C ACTIVE/DE-ACTIVATE (0) IBEAM                                            339
      VPDG_IBCHOICE=1                                                     339
      CVPDG_IBCHOICE='ACTIVE/DE-ACTIVATE (0) IBEAM'                       339
C IBEAM CENTRAL YPOS (6.5INCH)                                            340
      VPDG_IBPOSYC=-16.51                                                 340
      CVPDG_IBPOSYC='IBEAM CENTRAL YPOS (6.5INCH)'                        340
C IBEAM CENTRAL ZPOS WJL SHOULD BE 362.2+(12.125*2.54)+(IBLENG/2)=540.45  341
C  MODIFIED, NOTE EAS                                                     341
      VPDG_IBPOSZC=540.45                                                 341
      CVPDG_IBPOSZC='IBEAM CENTRAL ZPOS WJL SHOULD BE 362.2+(12.125*2.54  341
     *)+(IBLENG/2)=540.45 MODIFIED, NOTE EAS'                             341
C IBEAM LENGTH (116.1INCH)                                                342
      VPDG_IBLENG=294.894                                                 342
      CVPDG_IBLENG='IBEAM LENGTH (116.1INCH)'                             342
C IBEAM HORIZ. PLATE THICKNESS (0.29INCH)                                 343
      VPDG_IBTHICKH=0.7366                                                343
      CVPDG_IBTHICKH='IBEAM HORIZ. PLATE THICKNESS (0.29INCH)'            343
C IBEAM VERT. PLATE THICKNESS (0.17INCH)                                  344
      VPDG_IBTHICKV=0.4318                                                344
      CVPDG_IBTHICKV='IBEAM VERT. PLATE THICKNESS (0.17INCH)'             344
C IBEAM HEIGHT (4INCH)                                                    345
      VPDG_IBHEIGHT=10.16                                                 345
      CVPDG_IBHEIGHT='IBEAM HEIGHT (4INCH)'                               345
C IBEAM WIDTH (3INCH)                                                     346
      VPDG_IBWIDTH=7.62                                                   346
      CVPDG_IBWIDTH='IBEAM WIDTH (3INCH)'                                 346
C IBEAM VERT. FRONT PIECE (9INCH)                                         347
      VPDG_IBWLEN=22.86                                                   347
      CVPDG_IBWLEN='IBEAM VERT. FRONT PIECE (9INCH)'                      347
C IBEAM VERT. FRONT PIECE HEIGHT                                          348
      VPDG_IBWHGHTF=4.4489                                                348
      CVPDG_IBWHGHTF='IBEAM VERT. FRONT PIECE HEIGHT'                     348
C IBEAM VERT. FRONT PIECE HEIGHT                                          349
      VPDG_IBWHGHTB=8.3097                                                349
      CVPDG_IBWHGHTB='IBEAM VERT. FRONT PIECE HEIGHT'                     349
C EAST WEST Z SHIFT (0.75*2.54)                                           350
      VPDG_EWSHIFT=1.905                                                  350
      CVPDG_EWSHIFT='EAST WEST Z SHIFT (0.75*2.54)'                       350
C UP DOWN Y SHIFT                                                         351
      VPDG_UDSHIFT=0.0                                                    351
      CVPDG_UDSHIFT='UP DOWN Y SHIFT'                                     351
C BOLT ADDITIONAL Y SHIFT                                                 352
      VPDG_BOLTSHIFT=0.0                                                  352
      CVPDG_BOLTSHIFT='BOLT ADDITIONAL Y SHIFT'                           352
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    353
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           353
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   353
C *                                                                       353
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    353
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           353
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   353
C *                                                                       354
      CALL AGSBEGM(MODULE,IPRIN)                                          354
      IF (IPRIN.LT.0) RETURN                                              354
C Filling bank VPDG - PVPD BASIC DIMENSIONS                               354
      BANK_TITLE='PVPD BASIC DIMENSIONS'                                  354
      BPATH_VPDG='VPDG*'                                                  354
      IDNUM_VPDG(1)=0                                                     355
C GEOMETRY VERSION WJL ...USED BY Y2005, Y2006A, Y2006B                   355
      VPDG_VERSION=6                                                      355
      CVPDG_VERSION='GEOMETRY VERSION WJL ...USED BY Y2005, Y2006A, Y200  355
     *6B'                                                                 355
C Z POSITION EAST WJL FROM NEWER CADD: 220.720IN+(18.25IN/2)=229.845IN    356
      VPDG_ZPOSEAST=583.806                                               356
      CVPDG_ZPOSEAST='Z POSITION EAST WJL FROM NEWER CADD: 220.720IN+(18  356
     *.25IN/2)=229.845IN'                                                 356
C Z POSITION WEST WJL FROM NEWER CADD: 220.595IN+(18.25IN/2)=229.720IN    357
      VPDG_ZPOSWEST=583.488                                               357
      CVPDG_ZPOSWEST='Z POSITION WEST WJL FROM NEWER CADD: 220.595IN+(18  357
     *.25IN/2)=229.720IN'                                                 357
C MOTHERVOLUME RMIN (2.5INCH)                                             358
      VPDG_RMIN=6.35                                                      358
      CVPDG_RMIN='MOTHERVOLUME RMIN (2.5INCH)'                            358
C MOTHERVOLUME RMIN (12.3125INCH)                                         359
      VPDG_RMAX=31.27375                                                  359
      CVPDG_RMAX='MOTHERVOLUME RMIN (12.3125INCH)'                        359
C BASEPLATE WIDTH (12INCH)                                                360
      VPDG_BPWIDTH=30.48                                                  360
      CVPDG_BPWIDTH='BASEPLATE WIDTH (12INCH)'                            360
C BASEPLATE LENGTH (18.5INCH)                                             361
      VPDG_BPLENGTH=46.355                                                361
      CVPDG_BPLENGTH='BASEPLATE LENGTH (18.5INCH)'                        361
C BASEPLATE THICKNESS (0.25INCH)                                          362
      VPDG_BPTHICK=0.635                                                  362
      CVPDG_BPTHICK='BASEPLATE THICKNESS (0.25INCH)'                      362
C FEE BOX HEIGHT (2INCH)                                                  363
      VPDG_BXHEIGHT=5.08                                                  363
      CVPDG_BXHEIGHT='FEE BOX HEIGHT (2INCH)'                             363
C FEE BOX LENGTH (6INCH)                                                  364
      VPDG_BXLENGTH=15.24                                                 364
      CVPDG_BXLENGTH='FEE BOX LENGTH (6INCH)'                             364
C FEE BOX WIDTH (10INCH)                                                  365
      VPDG_BXWIDTH=25.4                                                   365
      CVPDG_BXWIDTH='FEE BOX WIDTH (10INCH)'                              365
C FEE BOX PLATE THICKNESS WJL SHOULD BE 50MILS!! MODIFIED (WAS 0.635)     366
      VPDG_BXTHICK=0.127                                                  366
      CVPDG_BXTHICK='FEE BOX PLATE THICKNESS WJL SHOULD BE 50MILS!! MODI  366
     *FIED (WAS 0.635)'                                                   366
C FEE CENTRAL Z POS (0.25INCH)                                            367
      VPDG_BXZPOSC=0.635                                                  367
      CVPDG_BXZPOSC='FEE CENTRAL Z POS (0.25INCH)'                        367
C FRONTPLATE WIDTH (3INCH)                                                368
      VPDG_FPWIDTH=7.62                                                   368
      CVPDG_FPWIDTH='FRONTPLATE WIDTH (3INCH)'                            368
C FRONTPLATE HEIGHT (10.023INCH)                                          369
      VPDG_FPHEIGHT=25.45842                                              369
      CVPDG_FPHEIGHT='FRONTPLATE HEIGHT (10.023INCH)'                     369
C FRONTPLATE THICKNESS (1INCH)                                            370
      VPDG_FPTHICK=2.54                                                   370
      CVPDG_FPTHICK='FRONTPLATE THICKNESS (1INCH)'                        370
C FRONTPL.PART A WIDTH (2.614INCH)                                        371
      VPDG_FPAWIDTH=6.63956                                               371
      CVPDG_FPAWIDTH='FRONTPL.PART A WIDTH (2.614INCH)'                   371
C FRONTPL.PART A HEIGHT (3.289INCH)                                       372
      VPDG_FPAHGHT=8.35405                                                372
      CVPDG_FPAHGHT='FRONTPL.PART A HEIGHT (3.289INCH)'                   372
C FRONTPL.PART C HEIGHT (4.95INCH)                                        373
      VPDG_FPCHGHT=12.573                                                 373
      CVPDG_FPCHGHT='FRONTPL.PART C HEIGHT (4.95INCH)'                    373
C FRONTPL. HOOK HEIGHT (1.5INCH)                                          374
      VPDG_FPHHGHT=3.81                                                   374
      CVPDG_FPHHGHT='FRONTPL. HOOK HEIGHT (1.5INCH)'                      374
C FRONTPL. HOOK WIDTH (2INCH)                                             375
      VPDG_FPHWIDTH=5.08                                                  375
      CVPDG_FPHWIDTH='FRONTPL. HOOK WIDTH (2INCH)'                        375
C FRONTPL. HOOK THICKNESS (0.25INCH)                                      376
      VPDG_FPHTHICK=0.635                                                 376
      CVPDG_FPHTHICK='FRONTPL. HOOK THICKNESS (0.25INCH)'                 376
C STRUT THICKNESS (0.25INCH)                                              377
      VPDG_STTHICK=0.635                                                  377
      CVPDG_STTHICK='STRUT THICKNESS (0.25INCH)'                          377
C STRUT HEIGHT (9.125 INCH)                                               378
      VPDG_STHEIGHT=23.1775                                               378
      CVPDG_STHEIGHT='STRUT HEIGHT (9.125 INCH)'                          378
C STRUT ANGLE (DEGR)                                                      379
      VPDG_STANGLE=25.                                                    379
      CVPDG_STANGLE='STRUT ANGLE (DEGR)'                                  379
C DIAGNOAL STRUT SIZE (2INCH)                                             380
      VPDG_STDIAGSZ=5.08                                                  380
      CVPDG_STDIAGSZ='DIAGNOAL STRUT SIZE (2INCH)'                        380
C STRUT CLAMP WIDTH (3INCH)                                               381
      VPDG_SCWIDTH=7.62                                                   381
      CVPDG_SCWIDTH='STRUT CLAMP WIDTH (3INCH)'                           381
C STRUT CLAMP HEIGHT (1.5INCH)                                            382
      VPDG_SCHEIGHT=3.81                                                  382
      CVPDG_SCHEIGHT='STRUT CLAMP HEIGHT (1.5INCH)'                       382
C STRUT CLAMP LENGTH (1.5INCH)                                            383
      VPDG_SCLENGTH=3.81                                                  383
      CVPDG_SCLENGTH='STRUT CLAMP LENGTH (1.5INCH)'                       383
C STRUT CLAMP THICKNESS (0.25INCH)                                        384
      VPDG_SCTHICK=0.635                                                  384
      CVPDG_SCTHICK='STRUT CLAMP THICKNESS (0.25INCH)'                    384
C CLAMP HEIGHT (1.375INCH)                                                385
      VPDG_CLHEIGHT=3.4925                                                385
      CVPDG_CLHEIGHT='CLAMP HEIGHT (1.375INCH)'                           385
C CLAMP WIDTH (1.5INCH)                                                   386
      VPDG_CLWIDTH=3.81                                                   386
      CVPDG_CLWIDTH='CLAMP WIDTH (1.5INCH)'                               386
C CLAMP LENGTH (16.25INCH)                                                387
      VPDG_CLLENGTH=41.275                                                387
      CVPDG_CLLENGTH='CLAMP LENGTH (16.25INCH)'                           387
C CLAMP PLATE THICKNESS (0.125INCH)                                       388
      VPDG_CLTHICK=0.3175                                                 388
      CVPDG_CLTHICK='CLAMP PLATE THICKNESS (0.125INCH)'                   388
C PMT ASSEMBLY LENGTH (13INCH)                                            389
      VPDG_DETLEN=33.02                                                   389
      CVPDG_DETLEN='PMT ASSEMBLY LENGTH (13INCH)'                         389
C PMT ASSEMBLY RADIUS (1.5INCH)                                           390
      VPDG_DETRAD=3.81                                                    390
      CVPDG_DETRAD='PMT ASSEMBLY RADIUS (1.5INCH)'                        390
C PMT ASS. FRONTPLATE THICKNESS (0.4INCH)                                 391
      VPDG_DETFRONT=1.016                                                 391
      CVPDG_DETFRONT='PMT ASS. FRONTPLATE THICKNESS (0.4INCH)'            391
C CONVERTER LAYER THICKNESS                                               392
      VPDG_CONVTHK=1.                                                     392
      CVPDG_CONVTHK='CONVERTER LAYER THICKNESS'                           392
C RADIATOR LAYER THICKNESS                                                393
      VPDG_RADITHK=1.                                                     393
      CVPDG_RADITHK='RADIATOR LAYER THICKNESS'                            393
C ELECTRONICS MOUNT LENGTH                                                394
      VPDG_ELELENG=15.0                                                   394
      CVPDG_ELELENG='ELECTRONICS MOUNT LENGTH'                            394
C LAYER RADIAL WIDTH                                                      395
      VPDG_DRLAYER=6.                                                     395
      CVPDG_DRLAYER='LAYER RADIAL WIDTH'                                  395
C NUMBER OF PMT IN LAYER                                                  396
      VPDG_NUMPMT=3                                                       396
      CVPDG_NUMPMT='NUMBER OF PMT IN LAYER'                               396
C PMT WALL THICKNESS                                                      397
      VPDG_PMTWALL=0.1                                                    397
      CVPDG_PMTWALL='PMT WALL THICKNESS'                                  397
C PMT AND DETECTOR RADIUS                                                 398
      VPDG_PMTRAD=2.54                                                    398
      CVPDG_PMTRAD='PMT AND DETECTOR RADIUS'                              398
C PMT TUBE LENGTH                                                         399
      VPDG_PMTLEN=8.0                                                     399
      CVPDG_PMTLEN='PMT TUBE LENGTH'                                      399
C ACTIVE/DE-ACTIVATE (0) IBEAM                                            400
      VPDG_IBCHOICE=1                                                     400
      CVPDG_IBCHOICE='ACTIVE/DE-ACTIVATE (0) IBEAM'                       400
C IBEAM CENTRAL YPOS (6.5INCH)                                            401
      VPDG_IBPOSYC=-16.51                                                 401
      CVPDG_IBPOSYC='IBEAM CENTRAL YPOS (6.5INCH)'                        401
C IBEAM CENTRAL ZPOS WJL SHOULD BE 362.2+(12.125*2.54)+(IBLENG/2)=540.45  402
C  MODIFIED, NOTE EAS                                                     402
      VPDG_IBPOSZC=540.45                                                 402
      CVPDG_IBPOSZC='IBEAM CENTRAL ZPOS WJL SHOULD BE 362.2+(12.125*2.54  402
     *)+(IBLENG/2)=540.45 MODIFIED, NOTE EAS'                             402
C IBEAM LENGTH (116.1INCH)                                                403
      VPDG_IBLENG=294.894                                                 403
      CVPDG_IBLENG='IBEAM LENGTH (116.1INCH)'                             403
C IBEAM HORIZ. PLATE THICKNESS (0.29INCH)                                 404
      VPDG_IBTHICKH=0.7366                                                404
      CVPDG_IBTHICKH='IBEAM HORIZ. PLATE THICKNESS (0.29INCH)'            404
C IBEAM VERT. PLATE THICKNESS (0.17INCH)                                  405
      VPDG_IBTHICKV=0.4318                                                405
      CVPDG_IBTHICKV='IBEAM VERT. PLATE THICKNESS (0.17INCH)'             405
C IBEAM HEIGHT (4INCH)                                                    406
      VPDG_IBHEIGHT=10.16                                                 406
      CVPDG_IBHEIGHT='IBEAM HEIGHT (4INCH)'                               406
C IBEAM WIDTH (3INCH)                                                     407
      VPDG_IBWIDTH=7.62                                                   407
      CVPDG_IBWIDTH='IBEAM WIDTH (3INCH)'                                 407
C IBEAM VERT. FRONT PIECE (9INCH)                                         408
      VPDG_IBWLEN=22.86                                                   408
      CVPDG_IBWLEN='IBEAM VERT. FRONT PIECE (9INCH)'                      408
C IBEAM VERT. FRONT PIECE HEIGHT                                          409
      VPDG_IBWHGHTF=4.4489                                                409
      CVPDG_IBWHGHTF='IBEAM VERT. FRONT PIECE HEIGHT'                     409
C IBEAM VERT. FRONT PIECE HEIGHT                                          410
      VPDG_IBWHGHTB=8.3097                                                410
      CVPDG_IBWHGHTB='IBEAM VERT. FRONT PIECE HEIGHT'                     410
C EAST WEST Z SHIFT (0.75*2.54)                                           411
      VPDG_EWSHIFT=1.905                                                  411
      CVPDG_EWSHIFT='EAST WEST Z SHIFT (0.75*2.54)'                       411
C UP DOWN Y SHIFT                                                         412
      VPDG_UDSHIFT=0.0                                                    412
      CVPDG_UDSHIFT='UP DOWN Y SHIFT'                                     412
C BOLT ADDITIONAL Y SHIFT                                                 413
      VPDG_BOLTSHIFT=0.0                                                  413
      CVPDG_BOLTSHIFT='BOLT ADDITIONAL Y SHIFT'                           413
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    414
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           414
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   414
C *                                                                       414
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    414
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           414
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   414
C *                                                                       415
      CALL AGSBEGM(MODULE,IPRIN)                                          415
      IF (IPRIN.LT.0) RETURN                                              415
C Filling bank VPDH - UPVPD BASIC DIMENSIONS                              415
      BANK_TITLE='UPVPD BASIC DIMENSIONS'                                 415
      BPATH_VPDH='VPDH*'                                                  415
      IDNUM_VPDH(1)=0                                                     416
C GEOMETRY VERSION WJL ...USED BY Y2006A --- AND FUTURE 2007 RUN          416
      VPDH_VERSION=7                                                      416
      CVPDH_VERSION='GEOMETRY VERSION WJL ...USED BY Y2006A --- AND FUTU  416
     *RE 2007 RUN'                                                        416
C Z POSITION EAST (MORE OR LESS ARBITRARY)                                417
      VPDH_ZPOSEAST=571.45                                                417
      CVPDH_ZPOSEAST='Z POSITION EAST (MORE OR LESS ARBITRARY)'           417
C Z POSITION WEST (MORE OR LESS ARBITRARY)                                418
      VPDH_ZPOSWEST=571.45                                                418
      CVPDH_ZPOSWEST='Z POSITION WEST (MORE OR LESS ARBITRARY)'           418
C MOTHERVOLUME RMIN (2 5/8IN)                                             419
      VPDH_RMIN=6.6675                                                    419
      CVPDH_RMIN='MOTHERVOLUME RMIN (2 5/8IN)'                            419
C MOTHERVOLUME RMAX (6 1/2IN)                                             420
      VPDH_RMAX=16.51+0.3                                                 420
      CVPDH_RMAX='MOTHERVOLUME RMAX (6 1/2IN)'                            420
C MOTHERVOLUME LENGTH                                                     421
      VPDH_LENGTH=32.                                                     421
      CVPDH_LENGTH='MOTHERVOLUME LENGTH'                                  421
C DET ASSY CYLNDER RADIUS (2IN DIAMETER)                                  422
      VPDH_DETRAD=2.54                                                    422
      CVPDH_DETRAD='DET ASSY CYLNDER RADIUS (2IN DIAMETER)'               422
C DET ASSY CYLINDER RADIUS (8IN)                                          423
      VPDH_DETLEN=20.32                                                   423
      CVPDH_DETLEN='DET ASSY CYLINDER RADIUS (8IN)'                       423
C PMT CYLINDER RADIUS (1.5IN DIAMETER)                                    424
      VPDH_PMTRAD=1.905                                                   424
      CVPDH_PMTRAD='PMT CYLINDER RADIUS (1.5IN DIAMETER)'                 424
C PMT CYLINDER LENGTH (1.5IN)                                             425
      VPDH_PMTLEN=1.905                                                   425
      CVPDH_PMTLEN='PMT CYLINDER LENGTH (1.5IN)'                          425
C DET ASSY CYLINDER WALL THICKNESS (50MILS)                               426
      VPDH_DETWALL=0.127                                                  426
      CVPDH_DETWALL='DET ASSY CYLINDER WALL THICKNESS (50MILS)'           426
C DET ASSY FRONT AND BACK PLATE THICKNESSES (1/8IN)                       427
      VPDH_DETFRONT=0.3175                                                427
      CVPDH_DETFRONT='DET ASSY FRONT AND BACK PLATE THICKNESSES (1/       427
     *8IN)'                                                               427
C LEAD CONVERTER LAYER THICKNESS (1CM)                                    428
      VPDH_LEADTHICK=1.0                                                  428
      CVPDH_LEADTHICK='LEAD CONVERTER LAYER THICKNESS (1CM)'              428
C SCINTILLATOR LAYER THICKNESS (1CM)                                      429
      VPDH_SCINTTHICK=1.0                                                 429
      CVPDH_SCINTTHICK='SCINTILLATOR LAYER THICKNESS (1CM)'               429
C UPVPD NUMBER OF TUBES IN RING 1                                         430
      VPDH_RING1_NDET=10                                                  430
      CVPDH_RING1_NDET='UPVPD NUMBER OF TUBES IN RING 1'                  430
C UPVPD GEO PARAMETER (4IN)                                               431
      VPDH_RING1_RAD=10.16                                                431
      CVPDH_RING1_RAD='UPVPD GEO PARAMETER (4IN)'                         431
C UPVPD GEO PARAMETER                                                     432
      VPDH_RING1_PHI0=-49.                                                432
      CVPDH_RING1_PHI0='UPVPD GEO PARAMETER'                              432
C UPVPD GEO PARAMETER                                                     433
      VPDH_RING1_DPHI=31.                                                 433
      CVPDH_RING1_DPHI='UPVPD GEO PARAMETER'                              433
C UPVPD GEO PARAMETER                                                     434
      VPDH_RING1_KPROJ=0                                                  434
      CVPDH_RING1_KPROJ='UPVPD GEO PARAMETER'                             434
C UPVPD NUMBER OF TUBES IN RING 2                                         435
      VPDH_RING2_NDET=9                                                   435
      CVPDH_RING2_NDET='UPVPD NUMBER OF TUBES IN RING 2'                  435
C UPVPD GEO PARAMETER (5 5/8IN)                                           436
      VPDH_RING2_RAD=14.2875                                              436
      CVPDH_RING2_RAD='UPVPD GEO PARAMETER (5 5/8IN)'                     436
C UPVPD GEO PARAMETER                                                     437
      VPDH_RING2_PHI0=-33.5                                               437
      CVPDH_RING2_PHI0='UPVPD GEO PARAMETER'                              437
C UPVPD GEO PARAMETER                                                     438
      VPDH_RING2_DPHI=31.                                                 438
      CVPDH_RING2_DPHI='UPVPD GEO PARAMETER'                              438
C UPVPD GEO PARAMETER                                                     439
      VPDH_RING2_KPROJ=0                                                  439
      CVPDH_RING2_KPROJ='UPVPD GEO PARAMETER'                             439
C UPVPD NUMBER OF TUBES IN RING 3                                         440
      VPDH_RING3_NDET=0                                                   440
      CVPDH_RING3_NDET='UPVPD NUMBER OF TUBES IN RING 3'                  440
C UPVPD GEO PARAMETER (5 5/8IN)                                           441
      VPDH_RING3_RAD=99.                                                  441
      CVPDH_RING3_RAD='UPVPD GEO PARAMETER (5 5/8IN)'                     441
C UPVPD GEO PARAMETER                                                     442
      VPDH_RING3_PHI0=99.                                                 442
      CVPDH_RING3_PHI0='UPVPD GEO PARAMETER'                              442
C UPVPD GEO PARAMETER                                                     443
      VPDH_RING3_DPHI=99.                                                 443
      CVPDH_RING3_DPHI='UPVPD GEO PARAMETER'                              443
C UPVPD GEO PARAMETER                                                     444
      VPDH_RING3_KPROJ=0                                                  444
      CVPDH_RING3_KPROJ='UPVPD GEO PARAMETER'                             444
C EAST WEST Z SHIFT (0.75*2.54)                                           445
      VPDH_EWSHIFT=1.905                                                  445
      CVPDH_EWSHIFT='EAST WEST Z SHIFT (0.75*2.54)'                       445
C UP DOWN Y SHIFT                                                         446
      VPDH_UDSHIFT=0.0                                                    446
      CVPDH_UDSHIFT='UP DOWN Y SHIFT'                                     446
C BOLT ADDITIONAL Y SHIFT                                                 447
      VPDH_BOLTSHIFT=0.0                                                  447
      CVPDH_BOLTSHIFT='BOLT ADDITIONAL Y SHIFT'                           447
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDH',    448
     *BPATH_VPDH,IDNUM_VPDH,1, MAP_VPDH,NAMES_VPDH,COMMEN_VPDH,           448
     *BANK_VPDH,LEN_VPDH,LLEN_VPDH,LINK_VPDH,FLAG_VPDH)                   448
C *                                                                       448
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDH',    448
     *BPATH_VPDH,IDNUM_VPDH,1, MAP_VPDH,NAMES_VPDH,COMMEN_VPDH,           448
     *BANK_VPDH,LEN_VPDH,LLEN_VPDH,LINK_VPDH,FLAG_VPDH)                   448
C *                                                                       449
      CALL AGSBEGM(MODULE,IPRIN)                                          449
      IF (IPRIN.LT.0) RETURN                                              449
C Filling bank VPDH - UPVPD BASIC DIMENSIONS                              449
      BANK_TITLE='UPVPD BASIC DIMENSIONS'                                 449
      BPATH_VPDH='VPDH*'                                                  449
      IDNUM_VPDH(1)=0                                                     450
C GEOMETRY VERSION WJL ...USED BY Y2006B                                  450
      VPDH_VERSION=8                                                      450
      CVPDH_VERSION='GEOMETRY VERSION WJL ...USED BY Y2006B'              450
C Z POSITION EAST (MORE OR LESS ARBITRARY)                                451
      VPDH_ZPOSEAST=571.45                                                451
      CVPDH_ZPOSEAST='Z POSITION EAST (MORE OR LESS ARBITRARY)'           451
C Z POSITION WEST (MORE OR LESS ARBITRARY)                                452
      VPDH_ZPOSWEST=571.45                                                452
      CVPDH_ZPOSWEST='Z POSITION WEST (MORE OR LESS ARBITRARY)'           452
C MOTHERVOLUME RMIN (2 5/8IN)                                             453
      VPDH_RMIN=6.6675                                                    453
      CVPDH_RMIN='MOTHERVOLUME RMIN (2 5/8IN)'                            453
C MOTHERVOLUME RMAX (6 1/2IN)                                             454
      VPDH_RMAX=16.51+0.3                                                 454
      CVPDH_RMAX='MOTHERVOLUME RMAX (6 1/2IN)'                            454
C MOTHERVOLUME LENGTH                                                     455
      VPDH_LENGTH=32.                                                     455
      CVPDH_LENGTH='MOTHERVOLUME LENGTH'                                  455
C DET ASSY CYLNDER RADIUS (2IN DIAMETER)                                  456
      VPDH_DETRAD=2.54                                                    456
      CVPDH_DETRAD='DET ASSY CYLNDER RADIUS (2IN DIAMETER)'               456
C DET ASSY CYLINDER RADIUS (8IN)                                          457
      VPDH_DETLEN=20.32                                                   457
      CVPDH_DETLEN='DET ASSY CYLINDER RADIUS (8IN)'                       457
C PMT CYLINDER RADIUS (1.5IN DIAMETER)                                    458
      VPDH_PMTRAD=1.905                                                   458
      CVPDH_PMTRAD='PMT CYLINDER RADIUS (1.5IN DIAMETER)'                 458
C PMT CYLINDER LENGTH (1.5IN)                                             459
      VPDH_PMTLEN=1.905                                                   459
      CVPDH_PMTLEN='PMT CYLINDER LENGTH (1.5IN)'                          459
C DET ASSY CYLINDER WALL THICKNESS (50MILS)                               460
      VPDH_DETWALL=0.127                                                  460
      CVPDH_DETWALL='DET ASSY CYLINDER WALL THICKNESS (50MILS)'           460
C DET ASSY FRONT AND BACK PLATE THICKNESSES (1/8IN)                       461
      VPDH_DETFRONT=0.3175                                                461
      CVPDH_DETFRONT='DET ASSY FRONT AND BACK PLATE THICKNESSES (1/       461
     *8IN)'                                                               461
C LEAD CONVERTER LAYER THICKNESS (1CM)                                    462
      VPDH_LEADTHICK=1.0                                                  462
      CVPDH_LEADTHICK='LEAD CONVERTER LAYER THICKNESS (1CM)'              462
C SCINTILLATOR LAYER THICKNESS (1CM)                                      463
      VPDH_SCINTTHICK=1.0                                                 463
      CVPDH_SCINTTHICK='SCINTILLATOR LAYER THICKNESS (1CM)'               463
C UPVPD NUMBER OF TUBES IN RING 1                                         464
      VPDH_RING1_NDET=11                                                  464
      CVPDH_RING1_NDET='UPVPD NUMBER OF TUBES IN RING 1'                  464
C UPVPD GEO PARAMETER (3.74IN)                                            465
      VPDH_RING1_RAD=9.5                                                  465
      CVPDH_RING1_RAD='UPVPD GEO PARAMETER (3.74IN)'                      465
C UPVPD GEO PARAMETER (360/11)                                            466
      VPDH_RING1_PHI0=-73.6364                                            466
      CVPDH_RING1_PHI0='UPVPD GEO PARAMETER (360/11)'                     466
C UPVPD GEO PARAMETER (-90+(360/11/2))                                    467
      VPDH_RING1_DPHI=32.7272                                             467
      CVPDH_RING1_DPHI='UPVPD GEO PARAMETER (-90+(360/11/2))'             467
C UPVPD GEO PARAMETER                                                     468
      VPDH_RING1_KPROJ=0                                                  468
      CVPDH_RING1_KPROJ='UPVPD GEO PARAMETER'                             468
C UPVPD NUMBER OF TUBES IN RING 2                                         469
      VPDH_RING2_NDET=11                                                  469
      CVPDH_RING2_NDET='UPVPD NUMBER OF TUBES IN RING 2'                  469
C UPVPD GEO PARAMETER (5.394IN)                                           470
      VPDH_RING2_RAD=13.7                                                 470
      CVPDH_RING2_RAD='UPVPD GEO PARAMETER (5.394IN)'                     470
C UPVPD GEO PARAMETER                                                     471
      VPDH_RING2_PHI0=-90                                                 471
      CVPDH_RING2_PHI0='UPVPD GEO PARAMETER'                              471
C UPVPD GEO PARAMETER (360/11)                                            472
      VPDH_RING2_DPHI=32.7272                                             472
      CVPDH_RING2_DPHI='UPVPD GEO PARAMETER (360/11)'                     472
C UPVPD GEO PARAMETER                                                     473
      VPDH_RING2_KPROJ=0                                                  473
      CVPDH_RING2_KPROJ='UPVPD GEO PARAMETER'                             473
C UPVPD NUMBER OF TUBES IN RING 3                                         474
      VPDH_RING3_NDET=0                                                   474
      CVPDH_RING3_NDET='UPVPD NUMBER OF TUBES IN RING 3'                  474
C UPVPD GEO PARAMETER (5 5/8IN)                                           475
      VPDH_RING3_RAD=99.                                                  475
      CVPDH_RING3_RAD='UPVPD GEO PARAMETER (5 5/8IN)'                     475
C UPVPD GEO PARAMETER                                                     476
      VPDH_RING3_PHI0=99.                                                 476
      CVPDH_RING3_PHI0='UPVPD GEO PARAMETER'                              476
C UPVPD GEO PARAMETER                                                     477
      VPDH_RING3_DPHI=99.                                                 477
      CVPDH_RING3_DPHI='UPVPD GEO PARAMETER'                              477
C UPVPD GEO PARAMETER                                                     478
      VPDH_RING3_KPROJ=0                                                  478
      CVPDH_RING3_KPROJ='UPVPD GEO PARAMETER'                             478
C EAST WEST Z SHIFT (0.75*2.54)                                           479
      VPDH_EWSHIFT=1.905                                                  479
      CVPDH_EWSHIFT='EAST WEST Z SHIFT (0.75*2.54)'                       479
C UP DOWN Y SHIFT                                                         480
      VPDH_UDSHIFT=-5.715                                                 480
      CVPDH_UDSHIFT='UP DOWN Y SHIFT'                                     480
C BOLT ADDITIONAL Y SHIFT                                                 481
      VPDH_BOLTSHIFT=1.0                                                  481
      CVPDH_BOLTSHIFT='BOLT ADDITIONAL Y SHIFT'                           481
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDH',    482
     *BPATH_VPDH,IDNUM_VPDH,1, MAP_VPDH,NAMES_VPDH,COMMEN_VPDH,           482
     *BANK_VPDH,LEN_VPDH,LLEN_VPDH,LINK_VPDH,FLAG_VPDH)                   482
C *                                                                       482
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDH',    482
     *BPATH_VPDH,IDNUM_VPDH,1, MAP_VPDH,NAMES_VPDH,COMMEN_VPDH,           482
     *BANK_VPDH,LEN_VPDH,LLEN_VPDH,LINK_VPDH,FLAG_VPDH)                   482
C *                                                                       483
      CALL AGSBEGM(MODULE,IPRIN)                                          483
      IF (IPRIN.LT.0) RETURN                                              483
C Filling bank VPDH - UPVPD BASIC DIMENSIONS                              483
      BANK_TITLE='UPVPD BASIC DIMENSIONS'                                 483
      BPATH_VPDH='VPDH*'                                                  483
      IDNUM_VPDH(1)=0                                                     484
C GEOMETRY VERSION WJL ...USED BY Y2006C                                  484
      VPDH_VERSION=9                                                      484
      CVPDH_VERSION='GEOMETRY VERSION WJL ...USED BY Y2006C'              484
C Z POSITION EAST (MORE OR LESS ARBITRARY)                                485
      VPDH_ZPOSEAST=571.45                                                485
      CVPDH_ZPOSEAST='Z POSITION EAST (MORE OR LESS ARBITRARY)'           485
C Z POSITION WEST (MORE OR LESS ARBITRARY)                                486
      VPDH_ZPOSWEST=571.45                                                486
      CVPDH_ZPOSWEST='Z POSITION WEST (MORE OR LESS ARBITRARY)'           486
C MOTHERVOLUME RMIN (2 5/8IN)                                             487
      VPDH_RMIN=6.6675                                                    487
      CVPDH_RMIN='MOTHERVOLUME RMIN (2 5/8IN)'                            487
C MOTHERVOLUME RMAX (7 3/4IN)                                             488
      VPDH_RMAX=19.685                                                    488
      CVPDH_RMAX='MOTHERVOLUME RMAX (7 3/4IN)'                            488
C MOTHERVOLUME LENGTH                                                     489
      VPDH_LENGTH=32.                                                     489
      CVPDH_LENGTH='MOTHERVOLUME LENGTH'                                  489
C DET ASSY CYLNDER RADIUS (2IN DIAMETER)                                  490
      VPDH_DETRAD=2.54                                                    490
      CVPDH_DETRAD='DET ASSY CYLNDER RADIUS (2IN DIAMETER)'               490
C DET ASSY CYLINDER RADIUS (8IN)                                          491
      VPDH_DETLEN=20.32                                                   491
      CVPDH_DETLEN='DET ASSY CYLINDER RADIUS (8IN)'                       491
C PMT CYLINDER RADIUS (1.5IN DIAMETER)                                    492
      VPDH_PMTRAD=1.905                                                   492
      CVPDH_PMTRAD='PMT CYLINDER RADIUS (1.5IN DIAMETER)'                 492
C PMT CYLINDER LENGTH (1.5IN)                                             493
      VPDH_PMTLEN=1.905                                                   493
      CVPDH_PMTLEN='PMT CYLINDER LENGTH (1.5IN)'                          493
C DET ASSY CYLINDER WALL THICKNESS (50MILS)                               494
      VPDH_DETWALL=0.127                                                  494
      CVPDH_DETWALL='DET ASSY CYLINDER WALL THICKNESS (50MILS)'           494
C DET ASSY FRONT AND BACK PLATE THICKNESSES (1/8IN)                       495
      VPDH_DETFRONT=0.3175                                                495
      CVPDH_DETFRONT='DET ASSY FRONT AND BACK PLATE THICKNESSES (1/       495
     *8IN)'                                                               495
C LEAD CONVERTER LAYER THICKNESS (1CM)                                    496
      VPDH_LEADTHICK=1.0                                                  496
      CVPDH_LEADTHICK='LEAD CONVERTER LAYER THICKNESS (1CM)'              496
C SCINTILLATOR LAYER THICKNESS (1CM)                                      497
      VPDH_SCINTTHICK=1.0                                                 497
      CVPDH_SCINTTHICK='SCINTILLATOR LAYER THICKNESS (1CM)'               497
C UPVPD NUMBER OF TUBES IN RING 1                                         498
      VPDH_RING1_NDET=11                                                  498
      CVPDH_RING1_NDET='UPVPD NUMBER OF TUBES IN RING 1'                  498
C UPVPD GEO PARAMETER (3.74IN)                                            499
      VPDH_RING1_RAD=9.5                                                  499
      CVPDH_RING1_RAD='UPVPD GEO PARAMETER (3.74IN)'                      499
C UPVPD GEO PARAMETER (360/11)                                            500
      VPDH_RING1_PHI0=-73.6364                                            500
      CVPDH_RING1_PHI0='UPVPD GEO PARAMETER (360/11)'                     500
C UPVPD GEO PARAMETER (-90+(360/11/2))                                    501
      VPDH_RING1_DPHI=32.7272                                             501
      CVPDH_RING1_DPHI='UPVPD GEO PARAMETER (-90+(360/11/2))'             501
C UPVPD GEO PARAMETER                                                     502
      VPDH_RING1_KPROJ=0                                                  502
      CVPDH_RING1_KPROJ='UPVPD GEO PARAMETER'                             502
C UPVPD NUMBER OF TUBES IN RING 2                                         503
      VPDH_RING2_NDET=11                                                  503
      CVPDH_RING2_NDET='UPVPD NUMBER OF TUBES IN RING 2'                  503
C UPVPD GEO PARAMETER (5.394IN)                                           504
      VPDH_RING2_RAD=13.7                                                 504
      CVPDH_RING2_RAD='UPVPD GEO PARAMETER (5.394IN)'                     504
C UPVPD GEO PARAMETER                                                     505
      VPDH_RING2_PHI0=-90                                                 505
      CVPDH_RING2_PHI0='UPVPD GEO PARAMETER'                              505
C UPVPD GEO PARAMETER (360/11)                                            506
      VPDH_RING2_DPHI=32.7272                                             506
      CVPDH_RING2_DPHI='UPVPD GEO PARAMETER (360/11)'                     506
C UPVPD GEO PARAMETER                                                     507
      VPDH_RING2_KPROJ=0                                                  507
      CVPDH_RING2_KPROJ='UPVPD GEO PARAMETER'                             507
C UPVPD NUMBER OF TUBES IN RING 3                                         508
      VPDH_RING3_NDET=11                                                  508
      CVPDH_RING3_NDET='UPVPD NUMBER OF TUBES IN RING 3'                  508
C UPVPD GEO PARAMETER                                                     509
      VPDH_RING3_RAD=16.8                                                 509
      CVPDH_RING3_RAD='UPVPD GEO PARAMETER'                               509
C UPVPD GEO PARAMETER                                                     510
      VPDH_RING3_PHI0=-73.6364                                            510
      CVPDH_RING3_PHI0='UPVPD GEO PARAMETER'                              510
C UPVPD GEO PARAMETER                                                     511
      VPDH_RING3_DPHI=32.7272                                             511
      CVPDH_RING3_DPHI='UPVPD GEO PARAMETER'                              511
C UPVPD GEO PARAMETER                                                     512
      VPDH_RING3_KPROJ=0                                                  512
      CVPDH_RING3_KPROJ='UPVPD GEO PARAMETER'                             512
C EAST WEST Z SHIFT (0.75*2.54)                                           513
      VPDH_EWSHIFT=1.905                                                  513
      CVPDH_EWSHIFT='EAST WEST Z SHIFT (0.75*2.54)'                       513
C UP DOWN Y SHIFT                                                         514
      VPDH_UDSHIFT=-8.255                                                 514
      CVPDH_UDSHIFT='UP DOWN Y SHIFT'                                     514
C BOLT ADDITIONAL Y SHIFT                                                 515
      VPDH_BOLTSHIFT=1.5                                                  515
      CVPDH_BOLTSHIFT='BOLT ADDITIONAL Y SHIFT'                           515
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDH',    516
     *BPATH_VPDH,IDNUM_VPDH,1, MAP_VPDH,NAMES_VPDH,COMMEN_VPDH,           516
     *BANK_VPDH,LEN_VPDH,LLEN_VPDH,LINK_VPDH,FLAG_VPDH)                   516
C *                                                                       516
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDH',    516
     *BPATH_VPDH,IDNUM_VPDH,1, MAP_VPDH,NAMES_VPDH,COMMEN_VPDH,           516
     *BANK_VPDH,LEN_VPDH,LLEN_VPDH,LINK_VPDH,FLAG_VPDH)                   516
C *                                                                       517
      CALL AGSBEGM(MODULE,IPRIN)                                          517
      IF (IPRIN.LT.0) RETURN                                              517
C Filling bank VPDH - UPVPD BASIC DIMENSIONS                              517
      BANK_TITLE='UPVPD BASIC DIMENSIONS'                                 517
      BPATH_VPDH='VPDH*'                                                  517
      IDNUM_VPDH(1)=0                                                     518
C GEOMETRY VERSION WJL ...USED BY Y2006D                                  518
      VPDH_VERSION=10                                                     518
      CVPDH_VERSION='GEOMETRY VERSION WJL ...USED BY Y2006D'              518
C Z POSITION EAST (MORE OR LESS ARBITRARY)                                519
      VPDH_ZPOSEAST=571.45                                                519
      CVPDH_ZPOSEAST='Z POSITION EAST (MORE OR LESS ARBITRARY)'           519
C Z POSITION WEST (MORE OR LESS ARBITRARY)                                520
      VPDH_ZPOSWEST=571.45                                                520
      CVPDH_ZPOSWEST='Z POSITION WEST (MORE OR LESS ARBITRARY)'           520
C MOTHERVOLUME RMIN (2 5/8IN)                                             521
      VPDH_RMIN=6.6675                                                    521
      CVPDH_RMIN='MOTHERVOLUME RMIN (2 5/8IN)'                            521
C MOTHERVOLUME RMAX (7 3/4IN)                                             522
      VPDH_RMAX=19.685                                                    522
      CVPDH_RMAX='MOTHERVOLUME RMAX (7 3/4IN)'                            522
C MOTHERVOLUME LENGTH                                                     523
      VPDH_LENGTH=32.                                                     523
      CVPDH_LENGTH='MOTHERVOLUME LENGTH'                                  523
C DET ASSY CYLNDER RADIUS (2IN DIAMETER)                                  524
      VPDH_DETRAD=2.54                                                    524
      CVPDH_DETRAD='DET ASSY CYLNDER RADIUS (2IN DIAMETER)'               524
C DET ASSY CYLINDER RADIUS (8IN)                                          525
      VPDH_DETLEN=20.32                                                   525
      CVPDH_DETLEN='DET ASSY CYLINDER RADIUS (8IN)'                       525
C PMT CYLINDER RADIUS (1.5IN DIAMETER)                                    526
      VPDH_PMTRAD=1.905                                                   526
      CVPDH_PMTRAD='PMT CYLINDER RADIUS (1.5IN DIAMETER)'                 526
C PMT CYLINDER LENGTH (1.5IN)                                             527
      VPDH_PMTLEN=1.905                                                   527
      CVPDH_PMTLEN='PMT CYLINDER LENGTH (1.5IN)'                          527
C DET ASSY CYLINDER WALL THICKNESS (50MILS)                               528
      VPDH_DETWALL=0.127                                                  528
      CVPDH_DETWALL='DET ASSY CYLINDER WALL THICKNESS (50MILS)'           528
C DET ASSY FRONT AND BACK PLATE THICKNESSES (1/8IN)                       529
      VPDH_DETFRONT=0.3175                                                529
      CVPDH_DETFRONT='DET ASSY FRONT AND BACK PLATE THICKNESSES (1/       529
     *8IN)'                                                               529
C LEAD CONVERTER LAYER THICKNESS (1CM)                                    530
      VPDH_LEADTHICK=1.0                                                  530
      CVPDH_LEADTHICK='LEAD CONVERTER LAYER THICKNESS (1CM)'              530
C SCINTILLATOR LAYER THICKNESS (1CM)                                      531
      VPDH_SCINTTHICK=1.0                                                 531
      CVPDH_SCINTTHICK='SCINTILLATOR LAYER THICKNESS (1CM)'               531
C UPVPD NUMBER OF TUBES IN RING 1                                         532
      VPDH_RING1_NDET=11                                                  532
      CVPDH_RING1_NDET='UPVPD NUMBER OF TUBES IN RING 1'                  532
C UPVPD GEO PARAMETER (3.74IN)                                            533
      VPDH_RING1_RAD=9.5                                                  533
      CVPDH_RING1_RAD='UPVPD GEO PARAMETER (3.74IN)'                      533
C UPVPD GEO PARAMETER (360/11)                                            534
      VPDH_RING1_PHI0=-73.6364                                            534
      CVPDH_RING1_PHI0='UPVPD GEO PARAMETER (360/11)'                     534
C UPVPD GEO PARAMETER (-90+(360/11/2))                                    535
      VPDH_RING1_DPHI=32.7272                                             535
      CVPDH_RING1_DPHI='UPVPD GEO PARAMETER (-90+(360/11/2))'             535
C UPVPD GEO PARAMETER                                                     536
      VPDH_RING1_KPROJ=1                                                  536
      CVPDH_RING1_KPROJ='UPVPD GEO PARAMETER'                             536
C UPVPD NUMBER OF TUBES IN RING 2                                         537
      VPDH_RING2_NDET=11                                                  537
      CVPDH_RING2_NDET='UPVPD NUMBER OF TUBES IN RING 2'                  537
C UPVPD GEO PARAMETER (5.394IN)                                           538
      VPDH_RING2_RAD=13.7                                                 538
      CVPDH_RING2_RAD='UPVPD GEO PARAMETER (5.394IN)'                     538
C UPVPD GEO PARAMETER                                                     539
      VPDH_RING2_PHI0=-90                                                 539
      CVPDH_RING2_PHI0='UPVPD GEO PARAMETER'                              539
C UPVPD GEO PARAMETER (360/11)                                            540
      VPDH_RING2_DPHI=32.7272                                             540
      CVPDH_RING2_DPHI='UPVPD GEO PARAMETER (360/11)'                     540
C UPVPD GEO PARAMETER                                                     541
      VPDH_RING2_KPROJ=1                                                  541
      CVPDH_RING2_KPROJ='UPVPD GEO PARAMETER'                             541
C UPVPD NUMBER OF TUBES IN RING 3                                         542
      VPDH_RING3_NDET=11                                                  542
      CVPDH_RING3_NDET='UPVPD NUMBER OF TUBES IN RING 3'                  542
C UPVPD GEO PARAMETER                                                     543
      VPDH_RING3_RAD=16.8                                                 543
      CVPDH_RING3_RAD='UPVPD GEO PARAMETER'                               543
C UPVPD GEO PARAMETER                                                     544
      VPDH_RING3_PHI0=-73.6364                                            544
      CVPDH_RING3_PHI0='UPVPD GEO PARAMETER'                              544
C UPVPD GEO PARAMETER                                                     545
      VPDH_RING3_DPHI=32.7272                                             545
      CVPDH_RING3_DPHI='UPVPD GEO PARAMETER'                              545
C UPVPD GEO PARAMETER                                                     546
      VPDH_RING3_KPROJ=1                                                  546
      CVPDH_RING3_KPROJ='UPVPD GEO PARAMETER'                             546
C EAST WEST Z SHIFT (0.75*2.54)                                           547
      VPDH_EWSHIFT=1.905                                                  547
      CVPDH_EWSHIFT='EAST WEST Z SHIFT (0.75*2.54)'                       547
C UP DOWN Y SHIFT                                                         548
      VPDH_UDSHIFT=-8.255                                                 548
      CVPDH_UDSHIFT='UP DOWN Y SHIFT'                                     548
C BOLT ADDITIONAL Y SHIFT                                                 549
      VPDH_BOLTSHIFT=1.5                                                  549
      CVPDH_BOLTSHIFT='BOLT ADDITIONAL Y SHIFT'                           549
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDH',    550
     *BPATH_VPDH,IDNUM_VPDH,1, MAP_VPDH,NAMES_VPDH,COMMEN_VPDH,           550
     *BANK_VPDH,LEN_VPDH,LLEN_VPDH,LINK_VPDH,FLAG_VPDH)                   550
C *                                                                       550
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDH',    550
     *BPATH_VPDH,IDNUM_VPDH,1, MAP_VPDH,NAMES_VPDH,COMMEN_VPDH,           550
     *BANK_VPDH,LEN_VPDH,LLEN_VPDH,LINK_VPDH,FLAG_VPDH)                   550
C *                                                                       551
      CALL AGSBEGM(MODULE,IPRIN)                                          551
      IF (IPRIN.LT.0) RETURN                                              551
C Filling bank VPDS - PIPE-SUPPORT MATERIAL                               551
      BANK_TITLE='PIPE-SUPPORT MATERIAL'                                  551
      BPATH_VPDS='VPDS*'                                                  551
      IDNUM_VPDS(1)=0                                                     552
C FIRST VERSION IN YEAR 2007                                              552
      VPDS_VERSION=1                                                      552
      CVPDS_VERSION='FIRST VERSION IN YEAR 2007'                          552
C IBSA POSITION Z CENTER                                                  553
      VPDS_IBSAZC=692.91                                                  553
      CVPDS_IBSAZC='IBSA POSITION Z CENTER'                               553
C IBSA POSITION Y CENTER                                                  554
      VPDS_IBSAYC=-60.10                                                  554
      CVPDS_IBSAYC='IBSA POSITION Y CENTER'                               554
C IBSA POSITION X CENTER                                                  555
      VPDS_IBSAXC=24.11                                                   555
      CVPDS_IBSAXC='IBSA POSITION X CENTER'                               555
C IBSB POSITION Z CENTER                                                  556
      VPDS_IBSBZC=631.20                                                  556
      CVPDS_IBSBZC='IBSB POSITION Z CENTER'                               556
C IBSB POSITION Y CENTER = (YC + UDSHIFT)                                 557
      VPDS_IBSBYC=-69.50                                                  557
      CVPDS_IBSBYC='IBSB POSITION Y CENTER = (YC + UDSHIFT)'              557
C IBSB POSITION X CENTER                                                  558
      VPDS_IBSBXC=24.11                                                   558
      CVPDS_IBSBXC='IBSB POSITION X CENTER'                               558
C IBSC POSITION Z CENTER                                                  559
      VPDS_IBSCZC=581.10                                                  559
      CVPDS_IBSCZC='IBSC POSITION Z CENTER'                               559
C IBSC POSITION Y CENTER = (YC + UDSHIFT)                                 560
      VPDS_IBSCYC=-23.60                                                  560
      CVPDS_IBSCYC='IBSC POSITION Y CENTER = (YC + UDSHIFT)'              560
C IBSC POSITION X CENTER                                                  561
      VPDS_IBSCXC=0.00                                                    561
      CVPDS_IBSCXC='IBSC POSITION X CENTER'                               561
C IBSD POSITION Z CENTER 1                                                562
      VPDS_IBSDZC1=397.70                                                 562
      CVPDS_IBSDZC1='IBSD POSITION Z CENTER 1'                            562
C IBSD POSITION Z CENTER 2                                                563
      VPDS_IBSDZC2=442.80                                                 563
      CVPDS_IBSDZC2='IBSD POSITION Z CENTER 2'                            563
C IBSD POSITION Y CENTER 1 = (YC+UDSHIFT)                                 564
      VPDS_IBSDYC1=-11.01                                                 564
      CVPDS_IBSDYC1='IBSD POSITION Y CENTER 1 = (YC+UDSHIFT)'             564
C IBSD POSITION Y CENTER 2                                                565
      VPDS_IBSDYC2=-7.20                                                  565
      CVPDS_IBSDYC2='IBSD POSITION Y CENTER 2'                            565
C IBSD POSITION X CENTER                                                  566
      VPDS_IBSDXC=0.00                                                    566
      CVPDS_IBSDXC='IBSD POSITION X CENTER'                               566
C IBSE POSITION Z CENTER 1                                                567
      VPDS_IBSEZC1=397.70                                                 567
      CVPDS_IBSEZC1='IBSE POSITION Z CENTER 1'                            567
C IBSE POSITION Z CENTER 2                                                568
      VPDS_IBSEZC2=442.80                                                 568
      CVPDS_IBSEZC2='IBSE POSITION Z CENTER 2'                            568
C IBSE POSITION Y CENTER                                                  569
      VPDS_IBSEYC=-13.44                                                  569
      CVPDS_IBSEYC='IBSE POSITION Y CENTER'                               569
C IBSE POSITION X CENTER                                                  570
      VPDS_IBSEXC=6.99                                                    570
      CVPDS_IBSEXC='IBSE POSITION X CENTER'                               570
C IBSF POSITION Z CENTER                                                  571
      VPDS_IBSFZC=411.70                                                  571
      CVPDS_IBSFZC='IBSF POSITION Z CENTER'                               571
C IBSF POSITION Y CENTER = (YC + UDSHIFT)                                 572
      VPDS_IBSFYC=-16.51                                                  572
      CVPDS_IBSFYC='IBSF POSITION Y CENTER = (YC + UDSHIFT)'              572
C IBSF POSITION X CENTER                                                  573
      VPDS_IBSFXC=0.00                                                    573
      CVPDS_IBSFXC='IBSF POSITION X CENTER'                               573
C IBSE POSITION Z CENTER 1                                                574
      VPDS_IBSGZC1=397.70                                                 574
      CVPDS_IBSGZC1='IBSE POSITION Z CENTER 1'                            574
C IBSE POSITION Z CENTER 2                                                575
      VPDS_IBSGZC2=442.80                                                 575
      CVPDS_IBSGZC2='IBSE POSITION Z CENTER 2'                            575
C IBSG POSITION Z CENTER OFFSET                                           576
      VPDS_IBSGZC3=2.54                                                   576
      CVPDS_IBSGZC3='IBSG POSITION Z CENTER OFFSET'                       576
C IBSG POSITION Y CENTER = (YC+UDSHIFT+BOLTSHIFT)                         577
      VPDS_IBSGYC=-17.57                                                  577
      CVPDS_IBSGYC='IBSG POSITION Y CENTER = (YC+UDSHIFT+BOLTSHIFT)'      577
C IBSG POSITION X CENTER                                                  578
      VPDS_IBSGXC=0.00                                                    578
      CVPDS_IBSGXC='IBSG POSITION X CENTER'                               578
C IBSH POSITION Z CENTER 1                                                579
      VPDS_IBSHZC1=397.70                                                 579
      CVPDS_IBSHZC1='IBSH POSITION Z CENTER 1'                            579
C IBSH POSITION Z CENTER 2                                                580
      VPDS_IBSHZC2=442.80                                                 580
      CVPDS_IBSHZC2='IBSH POSITION Z CENTER 2'                            580
C IBSH POSITION Y CENTER                                                  581
      VPDS_IBSHYC=0.00                                                    581
      CVPDS_IBSHYC='IBSH POSITION Y CENTER'                               581
C IBSH POSITION X CENTER 1                                                582
      VPDS_IBSHXC1=6.35                                                   582
      CVPDS_IBSHXC1='IBSH POSITION X CENTER 1'                            582
C IBSH POSITION X CENTER 2                                                583
      VPDS_IBSHXC2=7.62                                                   583
      CVPDS_IBSHXC2='IBSH POSITION X CENTER 2'                            583
C IBSA X LENGTH                                                           584
      VPDS_BSALENX=7.62                                                   584
      CVPDS_BSALENX='IBSA X LENGTH'                                       584
C IBSA Y LENGTH                                                           585
      VPDS_BSALENY=84.00                                                  585
      CVPDS_BSALENY='IBSA Y LENGTH'                                       585
C IBSA Z LENGTH                                                           586
      VPDS_BSALENZ=7.62                                                   586
      CVPDS_BSALENZ='IBSA Z LENGTH'                                       586
C IBAA Z LENGTH                                                           587
      VPDS_BAALENZ=0.95                                                   587
      CVPDS_BAALENZ='IBAA Z LENGTH'                                       587
C IBSB Y LENGTH                                                           588
      VPDS_BSBLENY=142.4                                                  588
      CVPDS_BSBLENY='IBSB Y LENGTH'                                       588
C IBSC X LENGTH                                                           589
      VPDS_BSCLENX=40.64                                                  589
      CVPDS_BSCLENX='IBSC X LENGTH'                                       589
C IBSC Y LENGTH                                                           590
      VPDS_BSCLENY=4.01                                                   590
      CVPDS_BSCLENY='IBSC Y LENGTH'                                       590
C IBSC Z LENGTH                                                           591
      VPDS_BSCLENZ=10.16                                                  591
      CVPDS_BSCLENZ='IBSC Z LENGTH'                                       591
C IBAC Z LENGTH                                                           592
      VPDS_BACLENZ=0.64                                                   592
      CVPDS_BACLENZ='IBAC Z LENGTH'                                       592
C IBSD X LENGTH                                                           593
      VPDS_BSDLENX=20.32                                                  593
      CVPDS_BSDLENX='IBSD X LENGTH'                                       593
C IBSE Y LENGTH                                                           594
      VPDS_BSELENY=8.26                                                   594
      CVPDS_BSELENY='IBSE Y LENGTH'                                       594
C IBSE Z LENGTH                                                           595
      VPDS_BSELENZ=6.35                                                   595
      CVPDS_BSELENZ='IBSE Z LENGTH'                                       595
C IBSF RMAX                                                               596
      VPDS_BSFRMAX=0.95                                                   596
      CVPDS_BSFRMAX='IBSF RMAX'                                           596
C IBSF Z LENGTH                                                           597
      VPDS_BSFLENZ=182.88                                                 597
      CVPDS_BSFLENZ='IBSF Z LENGTH'                                       597
C IBST Z LENGTH                                                           598
      VPDS_BSSLENZ=7.62                                                   598
      CVPDS_BSSLENZ='IBST Z LENGTH'                                       598
C IBSG RMAX                                                               599
      VPDS_BSGRMAX=0.64                                                   599
      CVPDS_BSGRMAX='IBSG RMAX'                                           599
C IBSG Z LENGTH 1 FOR VPDCONFIG<8                                         600
      VPDS_BSGLENZ1=10.16                                                 600
      CVPDS_BSGLENZ1='IBSG Z LENGTH 1 FOR VPDCONFIG<8'                    600
C IBSG Z LENGTH 2 FOR VPDCONFIG=8                                         601
      VPDS_BSGLENZ2=12.70                                                 601
      CVPDS_BSGLENZ2='IBSG Z LENGTH 2 FOR VPDCONFIG=8'                    601
C IBSG Z LENGTH 3 FOR VPDCONFIG ELSE                                      602
      VPDS_BSGLENZ3=15.24                                                 602
      CVPDS_BSGLENZ3='IBSG Z LENGTH 3 FOR VPDCONFIG ELSE'                 602
C IBSH Z LENGTH                                                           603
      VPDS_BSHLENZ=5.08                                                   603
      CVPDS_BSHLENZ='IBSH Z LENGTH'                                       603
C ELECTRONIC BOX WIDTH                                                    604
      VPDS_ELECWID=20.3                                                   604
      CVPDS_ELECWID='ELECTRONIC BOX WIDTH'                                604
C ELECTRONIC BOX THICKNESS                                                605
      VPDS_ELECTHCK=0.17                                                  605
      CVPDS_ELECTHCK='ELECTRONIC BOX THICKNESS'                           605
C ELECTRONIC BOX LENGTH                                                   606
      VPDS_ELECLEN=5.10                                                   606
      CVPDS_ELECLEN='ELECTRONIC BOX LENGTH'                               606
C VFEE POSITION X CENTER                                                  607
      VPDS_VFEEPOSX=0.31                                                  607
      CVPDS_VFEEPOSX='VFEE POSITION X CENTER'                             607
C VFEE POSITION Y CENTER                                                  608
      VPDS_VFEEPOSY=0.45                                                  608
      CVPDS_VFEEPOSY='VFEE POSITION Y CENTER'                             608
C VFEE POSITION Z CENTER                                                  609
      VPDS_VFEEPOSZ=-4.75                                                 609
      CVPDS_VFEEPOSZ='VFEE POSITION Z CENTER'                             609
C VLEM POSITION X CENTER                                                  612
      VPDS_VLEMPOSX(1)=-7.0                                               612
      VPDS_VLEMPOSX(2)=-3.5                                               612
      VPDS_VLEMPOSX(3)=0.0                                                612
      VPDS_VLEMPOSX(4)=3.5                                                612
      VPDS_VLEMPOSX(5)=7.0                                                612
      VPDS_VLEMPOSX(6)=-7.0                                               612
      VPDS_VLEMPOSX(7)=-3.5                                               612
      VPDS_VLEMPOSX(8)=0.0                                                612
      VPDS_VLEMPOSX(9)=3.5                                                612
      VPDS_VLEMPOSX(10)=7.0                                               612
      VPDS_VLEMPOSX(11)=-6.0                                              612
      VPDS_VLEMPOSX(12)=-2.5                                              612
      VPDS_VLEMPOSX(13)=1.0                                               612
      VPDS_VLEMPOSX(14)=4.5                                               612
      VPDS_VLEMPOSX(15)=8.0                                               612
      CVPDS_VLEMPOSX='VLEM POSITION X CENTER'                             612
C VLEM POSITION Y CENTER                                                  613
      VPDS_VLEMPOSY=0.52                                                  613
      CVPDS_VLEMPOSY='VLEM POSITION Y CENTER'                             613
C VLEM POSITION Z CENTER                                                  616
      VPDS_VLEMPOSZ(1)=2.0                                                616
      VPDS_VLEMPOSZ(2)=2.0                                                616
      VPDS_VLEMPOSZ(3)=2.0                                                616
      VPDS_VLEMPOSZ(4)=2.0                                                616
      VPDS_VLEMPOSZ(5)=2.0                                                616
      VPDS_VLEMPOSZ(6)=-2.0                                               616
      VPDS_VLEMPOSZ(7)=-2.0                                               616
      VPDS_VLEMPOSZ(8)=-2.0                                               616
      VPDS_VLEMPOSZ(9)=-2.0                                               616
      VPDS_VLEMPOSZ(10)=-2.0                                              616
      VPDS_VLEMPOSZ(11)=-2.0                                              616
      VPDS_VLEMPOSZ(12)=-2.0                                              616
      VPDS_VLEMPOSZ(13)=-2.0                                              616
      VPDS_VLEMPOSZ(14)=-2.0                                              616
      VPDS_VLEMPOSZ(15)=-2.0                                              616
      CVPDS_VLEMPOSZ='VLEM POSITION Z CENTER'                             616
C VLEM X LENGTH                                                           617
      VPDS_VLEMLENX=0.86                                                  617
      CVPDS_VLEMLENX='VLEM X LENGTH'                                      617
C VLEM Y LENGTH                                                           618
      VPDS_VLEMLENY=0.68                                                  618
      CVPDS_VLEMLENY='VLEM Y LENGTH'                                      618
C VLEM Z LENGTH                                                           619
      VPDS_VLEMLENZ=3.8                                                   619
      CVPDS_VLEMLENZ='VLEM Z LENGTH'                                      619
C VPIP POSITION X CENTER                                                  620
      VPDS_VPIPPOSX=0.09                                                  620
      CVPDS_VPIPPOSX='VPIP POSITION X CENTER'                             620
C VPIP POSITION Y CENTER                                                  621
      VPDS_VPIPPOSY=0.0                                                   621
      CVPDS_VPIPPOSY='VPIP POSITION Y CENTER'                             621
C VPIP POSITION Z CENTER                                                  622
      VPDS_VPIPPOSZ=0.9                                                   622
      CVPDS_VPIPPOSZ='VPIP POSITION Z CENTER'                             622
C VPIP RMIN                                                               623
      VPDS_VPIPRMIN=0.31                                                  623
      CVPDS_VPIPRMIN='VPIP RMIN'                                          623
C VPIP RMAX                                                               624
      VPDS_VPIPRMAX=0.34                                                  624
      CVPDS_VPIPRMAX='VPIP RMAX'                                          624
C VPIP Z LENGTH                                                           625
      VPDS_VPIPLENZ=2.0                                                   625
      CVPDS_VPIPLENZ='VPIP Z LENGTH'                                      625
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDS',    626
     *BPATH_VPDS,IDNUM_VPDS,1, MAP_VPDS,NAMES_VPDS,COMMEN_VPDS,           626
     *BANK_VPDS,LEN_VPDS,LLEN_VPDS,LINK_VPDS,FLAG_VPDS)                   626
C *                                                                       626
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDS',    626
     *BPATH_VPDS,IDNUM_VPDS,1, MAP_VPDS,NAMES_VPDS,COMMEN_VPDS,           626
     *BANK_VPDS,LEN_VPDS,LLEN_VPDS,LINK_VPDS,FLAG_VPDS)                   626
C *                                                                       627
      CALL AGSBEGM(MODULE,IPRIN)                                          627
      IF (IPRIN.LT.0) RETURN                                              627
C Filling bank WBSP - NEW WEST BEAMPIPE SUPPORT                           627
      BANK_TITLE='NEW WEST BEAMPIPE SUPPORT'                              627
      BPATH_WBSP='WBSP*'                                                  627
      IDNUM_WBSP(1)=0                                                     629
C NEW WEST BEAMPIPE SUPPORT IBEAM DIM (DX,DY,DZ,THICKNESS)                629
      WBSP_IBEAMD(1)=6.0*2.54                                             629
      WBSP_IBEAMD(2)=6.0*2.54                                             629
      WBSP_IBEAMD(3)=123*2.54                                             629
      WBSP_IBEAMD(4)=0.24*2.54                                            629
      CWBSP_IBEAMD='NEW WEST BEAMPIPE SUPPORT IBEAM DIM (DX,DY,DZ,THICKN  629
     *ESS)'                                                               629
C NEW WEST BEAMPIPE SUPPORT IBEAM POS (X,Y,Z)                             630
      WBSP_IBEAMP(1)=0.0                                                  630
      WBSP_IBEAMP(2)=-23.16                                               630
      WBSP_IBEAMP(3)=547.05                                               630
      CWBSP_IBEAMP='NEW WEST BEAMPIPE SUPPORT IBEAM POS (X,Y,Z)'          630
C NEW WEST BEAMPIPE SUPPORT POST DIM (DX,DY,DZ,THICKNESS)                 632
      WBSP_POSTD(1)=6.0*2.54                                              632
      WBSP_POSTD(2)=37.05*2.54                                            632
      WBSP_POSTD(3)=4.0*2.54                                              632
      WBSP_POSTD(4)=0.5*2.54                                              632
      CWBSP_POSTD='NEW WEST BEAMPIPE SUPPORT POST DIM (DX,DY,DZ,THICKNES  632
     *S)'                                                                 632
C NEW WEST BEAMPIPE SUPPORT POST POS (X,Y,Z)                              634
      WBSP_POSTP(1)=0.0                                                   634
      WBSP_POSTP(2)=-79.10                                                634
      WBSP_POSTP(3)=676.91+2.0*2.5                                        634
      CWBSP_POSTP='NEW WEST BEAMPIPE SUPPORT POST POS (X,Y,Z)'            634
C NEW WEST BEAMPIPE SUPPORT ANGLE DIM (DX,DY,DZ,THICKNESS)                638
      WBSP_ANGLD(1)=34.535*1.41421356*2.54                                638
      WBSP_ANGLD(2)=4.0*2.54                                              638
      WBSP_ANGLD(3)=4.0*2.54                                              638
      WBSP_ANGLD(4)=0.5*2.54                                              638
      CWBSP_ANGLD='NEW WEST BEAMPIPE SUPPORT ANGLE DIM (DX,DY,DZ,THICKNE  638
     *SS)'                                                                638
C NEW WEST BEAMPIPE SUPPORT ANGLE POS (X,Y,Z)                             639
      WBSP_ANGLP(1)=0.0                                                   639
      WBSP_ANGLP(2)=-71.68                                                639
      WBSP_ANGLP(3)=634.7                                                 639
      CWBSP_ANGLP='NEW WEST BEAMPIPE SUPPORT ANGLE POS (X,Y,Z)'           639
C NEW WEST BEAMPIPE SUPPORT TRINAGLE DIM (DX,DY,DZ)                       640
      WBSP_TRID(1)=0.5*2.54                                               640
      WBSP_TRID(2)=8.0*2.54                                               640
      WBSP_TRID(3)=8.0*2.54                                               640
      CWBSP_TRID='NEW WEST BEAMPIPE SUPPORT TRINAGLE DIM (DX,DY,DZ)'      640
C NEW WEST BEAMPIPE SUPPORT TRIANGLE POS (X,Y,Z)                          641
      WBSP_TRIP(1)=0.0                                                    641
      WBSP_TRIP(2)=-70.00                                                 641
      WBSP_TRIP(3)=600                                                    641
      CWBSP_TRIP='NEW WEST BEAMPIPE SUPPORT TRIANGLE POS (X,Y,Z)'         641
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'WBSP',    642
     *BPATH_WBSP,IDNUM_WBSP,1, MAP_WBSP,NAMES_WBSP,COMMEN_WBSP,           642
     *BANK_WBSP,LEN_WBSP,LLEN_WBSP,LINK_WBSP,FLAG_WBSP)                   642
C *                                                                       642
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'WBSP',    642
     *BPATH_WBSP,IDNUM_WBSP,1, MAP_WBSP,NAMES_WBSP,COMMEN_WBSP,           642
     *BANK_WBSP,LEN_WBSP,LLEN_WBSP,LINK_WBSP,FLAG_WBSP)                   642
C use default VPDV bank                                                   643
      BPATH_VPDV='VPDV*'                                                  643
      IDNUM_VPDV(1)=0                                                     643
      CALL AGDGETP('VPDDGEO3','*',-999,'VPDV',' ',BANK_VPDV, BPATH_VPDV,  643
     *IDNUM_VPDV,1,MAP_VPDV,NAMES_VPDV,BANK_VPDV,LEN_VPDV,LLEN_VPDV,      643
     *LINK_VPDV,FLAG_VPDV)                                                643
C use default VPDS bank                                                   644
      BPATH_VPDS='VPDS*'                                                  644
      IDNUM_VPDS(1)=0                                                     644
      CALL AGDGETP('VPDDGEO3','*',-999,'VPDS',' ',BANK_VPDS, BPATH_VPDS,  644
     *IDNUM_VPDS,1,MAP_VPDS,NAMES_VPDS,BANK_VPDS,LEN_VPDS,LLEN_VPDS,      644
     *LINK_VPDS,FLAG_VPDS)                                                644
C Check VPDV_VPDCONFIG<=4                                                 645
      IF (VPDV_VPDCONFIG.LE.4) THEN                                       645
      KDETSTYLE = 0                                                       646
C use VPDG bank                                                           647
      BPATH_VPDG='VPDG*'                                                  647
      IDNUM_VPDG(1)=0                                                     647
      VPDG_VERSION=VPDV_VPDCONFIG                                         647
      CALL AGDGETP('VPDDGEO3','*',-999,'VPDG',NVPDG_VERSION,              647
     *RE_VPDG_VERSION,BPATH_VPDG,IDNUM_VPDG,1,MAP_VPDG,NAMES_VPDG,        647
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   647
      ZPOSE = VPDG_ZPOSEAST                                               648
      ZPOSW = VPDG_ZPOSWEST                                               649
      EWSHIFT = VPDG_EWSHIFT                                              650
      UDSHIFT = VPDG_UDSHIFT                                              651
      BOLTSHIFT = VPDG_BOLTSHIFT                                          652
      ELSEIF (VPDV_VPDCONFIG.EQ.5.OR.VPDV_VPDCONFIG.EQ.6) THEN            653
      KDETSTYLE = 1                                                       654
C use VPDG bank                                                           655
      BPATH_VPDG='VPDG*'                                                  655
      IDNUM_VPDG(1)=0                                                     655
      VPDG_VERSION=VPDV_VPDCONFIG                                         655
      CALL AGDGETP('VPDDGEO3','*',-999,'VPDG',NVPDG_VERSION,              655
     *RE_VPDG_VERSION,BPATH_VPDG,IDNUM_VPDG,1,MAP_VPDG,NAMES_VPDG,        655
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   655
      ZPOSE = VPDG_ZPOSEAST                                               656
      ZPOSW = VPDG_ZPOSWEST                                               657
      EWSHIFT = VPDG_EWSHIFT                                              658
      UDSHIFT = VPDG_UDSHIFT                                              659
      BOLTSHIFT = VPDG_BOLTSHIFT                                          660
      ELSEIF (VPDV_VPDCONFIG.GE.7) THEN                                   661
      KDETSTYLE = 2                                                       662
C use VPDG bank                                                           663
      BPATH_VPDG='VPDG*'                                                  663
      IDNUM_VPDG(1)=0                                                     663
      VPDG_VERSION=6                                                      663
      CALL AGDGETP('VPDDGEO3','*',-999,'VPDG',NVPDG_VERSION,              663
     *RE_VPDG_VERSION,BPATH_VPDG,IDNUM_VPDG,1,MAP_VPDG,NAMES_VPDG,        663
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   663
C use VPDH bank                                                           664
      BPATH_VPDH='VPDH*'                                                  664
      IDNUM_VPDH(1)=0                                                     664
      VPDH_VERSION=VPDV_VPDCONFIG                                         664
      CALL AGDGETP('VPDDGEO3','*',-999,'VPDH',NVPDH_VERSION,              664
     *RE_VPDH_VERSION,BPATH_VPDH,IDNUM_VPDH,1,MAP_VPDH,NAMES_VPDH,        664
     *BANK_VPDH,LEN_VPDH,LLEN_VPDH,LINK_VPDH,FLAG_VPDH)                   664
      ZPOSE = VPDH_ZPOSEAST                                               665
      ZPOSW = VPDH_ZPOSWEST                                               666
      EWSHIFT = VPDH_EWSHIFT                                              667
      UDSHIFT = VPDH_UDSHIFT                                              668
      BOLTSHIFT = VPDH_BOLTSHIFT                                          669
      ENDIF                                                               670
      CALL AGSBEGM(MODULE,IPRIN)                                          671
C Check IPRIN<0                                                           671
      IF (IPRIN.LT.0) RETURN                                              671
C Create VPDD :                                                           671
C Call To Local Procedure VPDD                                            671
      ASSIGN 5020 TO BLOCK_VPDD                                           671
      GO TO  5010                                                         671
5020  CONTINUE                                                            671
C *                                                                       671
      ZPOS = ZPOSW                                                        672
C Geant POSITION : VPDD ===> MOTHER='CAVE' Z=+ZPOS <===                   673
      CALL AGSRESET                                                       673
      AG_MOTHER = 'CAVE'                                                  673
      AG_Z = +ZPOS                                                        673
      AG_TITLE = 'VPDD'                                                   673
      AG_EXNAME = 'POSITION'                                              673
      AG_PARLIST = 'MOTHER_Z'                                             673
      CALL AXPOSITION                                                     673
C *                                                                       673
      ZPOS = ZPOSE                                                        673
C Geant POSITION : VPDD ===> MOTHER='CAVE' Z=-ZPOS THETAZ=180 <===        675
      CALL AGSRESET                                                       675
      AG_MOTHER = 'CAVE'                                                  675
      AG_Z = -ZPOS                                                        675
      AG_THETAZ = 180                                                     675
      AG_TITLE = 'VPDD'                                                   675
      AG_EXNAME = 'POSITION'                                              675
      AG_PARLIST = 'MOTHER_Z_THETAZ'                                      675
      CALL AXPOSITION                                                     675
C *                                                                       675
      KIBEAMSTYLE=0                                                       675
C Check VPDG_IBCHOICE!=0                                                  677
      IF (VPDG_IBCHOICE.NE.0) THEN                                        677
      KIBEAMSTYLE=1                                                       678
C Check VPDV_VPDCONFIG>4                                                  679
      IF (VPDV_VPDCONFIG.GT.4) THEN                                       679
      KIBEAMSTYLE=2                                                       680
C Check VPDV_VPDCONFIG>=8                                                 681
      IF (VPDV_VPDCONFIG.GE.8) THEN                                       681
      KIBEAMSTYLE=3                                                       682
      ENDIF                                                               683
      ENDIF                                                               684
      ENDIF                                                               685
      KIBEAMSTYLE=4                                                       686
C Check KIBEAMSTYLE>0                                                     687
      IF (KIBEAMSTYLE.GT.0) THEN                                          687
      CALL AGSBEGM(MODULE,IPRIN)                                          688
C Check IPRIN<0                                                           688
      IF (IPRIN.LT.0) RETURN                                              688
C Create IBEM :                                                           688
C Call To Local Procedure IBEM                                            688
      ASSIGN 5040 TO BLOCK_IBEM                                           688
      GO TO  5030                                                         688
5040  CONTINUE                                                            688
C *                                                                       688
C Check KIBEAMSTYLE<4                                                     689
      IF (KIBEAMSTYLE.LT.4) THEN                                          689
C Geant POSITION : IBEM ===> MOTHER='CAVE' Y=VPDG_IBPOSYC+UDSHIFT Z=+(VP  690
C DG_IBPOSZC) <===                                                        690
      CALL AGSRESET                                                       690
      AG_MOTHER = 'CAVE'                                                  690
      AG_Y = VPDG_IBPOSYC+UDSHIFT                                         690
      AG_Z = +(VPDG_IBPOSZC)                                              690
      AG_TITLE = 'IBEM'                                                   690
      AG_EXNAME = 'POSITION'                                              690
      AG_PARLIST = 'MOTHER_Y_Z'                                           690
      CALL AXPOSITION                                                     690
C *                                                                       690
      ENDIF                                                               690
C Geant POSITION : IBEM ===> MOTHER='CAVE' Y=VPDG_IBPOSYC+UDSHIFT Z=-(VP  693
C DG_IBPOSZC+EWSHIFT) THETAZ=180 <===                                     693
      CALL AGSRESET                                                       693
      AG_MOTHER = 'CAVE'                                                  693
      AG_Y = VPDG_IBPOSYC+UDSHIFT                                         693
      AG_Z = -(VPDG_IBPOSZC+EWSHIFT)                                      693
      AG_THETAZ = 180                                                     693
      AG_TITLE = 'IBEM'                                                   693
      AG_EXNAME = 'POSITION'                                              693
      AG_PARLIST = 'MOTHER_Y_Z_THETAZ'                                    693
      CALL AXPOSITION                                                     693
C *                                                                       693
C Check KIBEAMSTYLE>1                                                     693
      IF (KIBEAMSTYLE.GT.1) THEN                                          694
      CALL AGSBEGM(MODULE,IPRIN)                                          695
C Check IPRIN<0                                                           695
      IF (IPRIN.LT.0) RETURN                                              695
C Create IBSA :                                                           695
C Call To Local Procedure IBSA                                            695
      ASSIGN 5060 TO BLOCK_IBSA                                           695
      GO TO  5050                                                         695
5060  CONTINUE                                                            695
C *                                                                       695
C Check KIBEAMSTYLE<4                                                     696
      IF (KIBEAMSTYLE.LT.4) THEN                                          696
C Geant POSITION : IBSA ===> MOTHER='CAVE' X=VPDS_IBSAXC Y=VPDS_IBSAYC Z  697
C =VPDS_IBSAZC <===                                                       697
      CALL AGSRESET                                                       697
      AG_MOTHER = 'CAVE'                                                  697
      AG_X = VPDS_IBSAXC                                                  697
      AG_Y = VPDS_IBSAYC                                                  697
      AG_Z = VPDS_IBSAZC                                                  697
      AG_TITLE = 'IBSA'                                                   697
      AG_EXNAME = 'POSITION'                                              697
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         697
      CALL AXPOSITION                                                     697
C *                                                                       697
C Geant POSITION : IBSA ===> MOTHER='CAVE' X=-VPDS_IBSAXC Y=VPDS_IBSAYC   697
C Z=VPDS_IBSAZC ORT=-X-YZ <===                                            697
      CALL AGSRESET                                                       698
      AG_MOTHER = 'CAVE'                                                  698
      AG_X = -VPDS_IBSAXC                                                 698
      AG_Y = VPDS_IBSAYC                                                  698
      AG_Z = VPDS_IBSAZC                                                  698
      AG_ALPHAN=AG_ALPHAN+1                                               698
      AG_ALPHAK(AG_ALPHAN)=IGOR('-X-YZ')                                  698
      AG_TITLE = 'IBSA'                                                   698
      AG_EXNAME = 'POSITION'                                              698
      AG_PARLIST = 'MOTHER_X_Y_Z_ORT'                                     698
      CALL AXPOSITION                                                     698
C *                                                                       698
      ENDIF                                                               698
C Geant POSITION : IBSA ===> MOTHER='CAVE' X=VPDS_IBSAXC Y=VPDS_IBSAYC Z  701
C =-(VPDS_IBSAZC+EWSHIFT) ORT=-X-YZ <===                                  701
      CALL AGSRESET                                                       701
      AG_MOTHER = 'CAVE'                                                  701
      AG_X = VPDS_IBSAXC                                                  701
      AG_Y = VPDS_IBSAYC                                                  701
      AG_Z = -(VPDS_IBSAZC+EWSHIFT)                                       701
      AG_ALPHAN=AG_ALPHAN+1                                               701
      AG_ALPHAK(AG_ALPHAN)=IGOR('-X-YZ')                                  701
      AG_TITLE = 'IBSA'                                                   701
      AG_EXNAME = 'POSITION'                                              701
      AG_PARLIST = 'MOTHER_X_Y_Z_ORT'                                     701
      CALL AXPOSITION                                                     701
C *                                                                       701
C Geant POSITION : IBSA ===> MOTHER='CAVE' X=-VPDS_IBSAXC Y=VPDS_IBSAYC   701
C Z=-(VPDS_IBSAZC+EWSHIFT) ORT=-X-Y-Z <===                                701
      CALL AGSRESET                                                       703
      AG_MOTHER = 'CAVE'                                                  703
      AG_X = -VPDS_IBSAXC                                                 703
      AG_Y = VPDS_IBSAYC                                                  703
      AG_Z = -(VPDS_IBSAZC+EWSHIFT)                                       703
      AG_ALPHAN=AG_ALPHAN+1                                               703
      AG_ALPHAK(AG_ALPHAN)=IGOR('-X-Y-Z')                                 703
      AG_TITLE = 'IBSA'                                                   703
      AG_EXNAME = 'POSITION'                                              703
      AG_PARLIST = 'MOTHER_X_Y_Z_ORT'                                     703
      CALL AXPOSITION                                                     703
C *                                                                       703
      CALL AGSBEGM(MODULE,IPRIN)                                          703
C Check IPRIN<0                                                           704
      IF (IPRIN.LT.0) RETURN                                              704
C Create IBSB :                                                           704
C Call To Local Procedure IBSB                                            704
      ASSIGN 5080 TO BLOCK_IBSB                                           704
      GO TO  5070                                                         704
5080  CONTINUE                                                            704
C *                                                                       704
C Check KIBEAMSTYLE<4                                                     705
      IF (KIBEAMSTYLE.LT.4) THEN                                          705
C Geant POSITION : IBSB ===> MOTHER='CAVE' X=VPDS_IBSBXC Y=VPDS_IBSBYC+U  707
C DSHIFT Z=VPDS_IBSBZC ALPHAX=-45 <===                                    707
      CALL AGSRESET                                                       707
      AG_MOTHER = 'CAVE'                                                  707
      AG_X = VPDS_IBSBXC                                                  707
      AG_Y = VPDS_IBSBYC+UDSHIFT                                          707
      AG_Z = VPDS_IBSBZC                                                  707
      AG_ALPHAN=AG_ALPHAN+1                                               707
      AG_ALPHAK(AG_ALPHAN)=1                                              707
      AG_ALPHAL(AG_ALPHAN)=-45                                            707
      AG_TITLE = 'IBSB'                                                   707
      AG_EXNAME = 'POSITION'                                              707
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  707
      CALL AXPOSITION                                                     707
C *                                                                       707
C Geant POSITION : IBSB ===> MOTHER='CAVE' X=-VPDS_IBSBXC Y=VPDS_IBSBYC+  707
C UDSHIFT Z=VPDS_IBSBZC ALPHAY=90 ALPHAX=-45 <===                         707
      CALL AGSRESET                                                       709
      AG_MOTHER = 'CAVE'                                                  709
      AG_X = -VPDS_IBSBXC                                                 709
      AG_Y = VPDS_IBSBYC+UDSHIFT                                          709
      AG_Z = VPDS_IBSBZC                                                  709
      AG_ALPHAN=AG_ALPHAN+1                                               709
      AG_ALPHAK(AG_ALPHAN)=2                                              709
      AG_ALPHAL(AG_ALPHAN)=90                                             709
      AG_ALPHAN=AG_ALPHAN+1                                               709
      AG_ALPHAK(AG_ALPHAN)=1                                              709
      AG_ALPHAL(AG_ALPHAN)=-45                                            709
      AG_TITLE = 'IBSB'                                                   709
      AG_EXNAME = 'POSITION'                                              709
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAY_ALPHAX'                           709
      CALL AXPOSITION                                                     709
C *                                                                       709
      ENDIF                                                               709
C Geant POSITION : IBSB ===> MOTHER='CAVE' X=VPDS_IBSBXC Y=VPDS_IBSBYC+U  712
C DSHIFT Z=-(VPDS_IBSBZC+EWSHIFT) ALPHAY=-90 ALPHAX=45 <===               712
      CALL AGSRESET                                                       712
      AG_MOTHER = 'CAVE'                                                  712
      AG_X = VPDS_IBSBXC                                                  712
      AG_Y = VPDS_IBSBYC+UDSHIFT                                          712
      AG_Z = -(VPDS_IBSBZC+EWSHIFT)                                       712
      AG_ALPHAN=AG_ALPHAN+1                                               712
      AG_ALPHAK(AG_ALPHAN)=2                                              712
      AG_ALPHAL(AG_ALPHAN)=-90                                            712
      AG_ALPHAN=AG_ALPHAN+1                                               712
      AG_ALPHAK(AG_ALPHAN)=1                                              712
      AG_ALPHAL(AG_ALPHAN)=45                                             712
      AG_TITLE = 'IBSB'                                                   712
      AG_EXNAME = 'POSITION'                                              712
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAY_ALPHAX'                           712
      CALL AXPOSITION                                                     712
C *                                                                       712
C Geant POSITION : IBSB ===> MOTHER='CAVE' X=-VPDS_IBSBXC Y=VPDS_IBSBYC+  712
C UDSHIFT Z=-(VPDS_IBSBZC+EWSHIFT) ALPHAY=180 ALPHAX=45 <===              712
      CALL AGSRESET                                                       714
      AG_MOTHER = 'CAVE'                                                  714
      AG_X = -VPDS_IBSBXC                                                 714
      AG_Y = VPDS_IBSBYC+UDSHIFT                                          714
      AG_Z = -(VPDS_IBSBZC+EWSHIFT)                                       714
      AG_ALPHAN=AG_ALPHAN+1                                               714
      AG_ALPHAK(AG_ALPHAN)=2                                              714
      AG_ALPHAL(AG_ALPHAN)=180                                            714
      AG_ALPHAN=AG_ALPHAN+1                                               714
      AG_ALPHAK(AG_ALPHAN)=1                                              714
      AG_ALPHAL(AG_ALPHAN)=45                                             714
      AG_TITLE = 'IBSB'                                                   714
      AG_EXNAME = 'POSITION'                                              714
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAY_ALPHAX'                           714
      CALL AXPOSITION                                                     714
C *                                                                       714
      CALL AGSBEGM(MODULE,IPRIN)                                          714
C Check IPRIN<0                                                           715
      IF (IPRIN.LT.0) RETURN                                              715
C Create IBSC :                                                           715
C Call To Local Procedure IBSC                                            715
      ASSIGN 5100 TO BLOCK_IBSC                                           715
      GO TO  5090                                                         715
5100  CONTINUE                                                            715
C *                                                                       715
C Check KIBEAMSTYLE<4                                                     716
      IF (KIBEAMSTYLE.LT.4) THEN                                          716
C Geant POSITION : IBSC ===> MOTHER='CAVE' Y=VPDS_IBSCYC+UDSHIFT Z=VPDS_  717
C IBSCZC <===                                                             717
      CALL AGSRESET                                                       717
      AG_MOTHER = 'CAVE'                                                  717
      AG_Y = VPDS_IBSCYC+UDSHIFT                                          717
      AG_Z = VPDS_IBSCZC                                                  717
      AG_TITLE = 'IBSC'                                                   717
      AG_EXNAME = 'POSITION'                                              717
      AG_PARLIST = 'MOTHER_Y_Z'                                           717
      CALL AXPOSITION                                                     717
C *                                                                       717
      ENDIF                                                               717
C Geant POSITION : IBSC ===> MOTHER='CAVE' Y=VPDS_IBSCYC+UDSHIFT Z=-(VPD  719
C S_IBSCZC+EWSHIFT) THETAZ=180 <===                                       719
      CALL AGSRESET                                                       719
      AG_MOTHER = 'CAVE'                                                  719
      AG_Y = VPDS_IBSCYC+UDSHIFT                                          719
      AG_Z = -(VPDS_IBSCZC+EWSHIFT)                                       719
      AG_THETAZ = 180                                                     719
      AG_TITLE = 'IBSC'                                                   719
      AG_EXNAME = 'POSITION'                                              719
      AG_PARLIST = 'MOTHER_Y_Z_THETAZ'                                    719
      CALL AXPOSITION                                                     719
C *                                                                       719
      CALL AGSBEGM(MODULE,IPRIN)                                          719
C Check IPRIN<0                                                           720
      IF (IPRIN.LT.0) RETURN                                              720
C Create IBSD :                                                           720
C Call To Local Procedure IBSD                                            720
      ASSIGN 5120 TO BLOCK_IBSD                                           720
      GO TO  5110                                                         720
5120  CONTINUE                                                            720
C *                                                                       720
C Check KIBEAMSTYLE<4                                                     721
      IF (KIBEAMSTYLE.LT.4) THEN                                          721
C Geant POSITION : IBSD ===> MOTHER='CAVE' X=0 Y=VPDS_IBSDYC1+UDSHIFT Z=  722
C VPDS_IBSDZC1 <===                                                       722
      CALL AGSRESET                                                       722
      AG_MOTHER = 'CAVE'                                                  722
      AG_X = 0                                                            722
      AG_Y = VPDS_IBSDYC1+UDSHIFT                                         722
      AG_Z = VPDS_IBSDZC1                                                 722
      AG_TITLE = 'IBSD'                                                   722
      AG_EXNAME = 'POSITION'                                              722
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         722
      CALL AXPOSITION                                                     722
C *                                                                       722
C Geant POSITION : IBSD ===> MOTHER='CAVE' X=0 Y=VPDS_IBSDYC1+UDSHIFT Z=  722
C VPDS_IBSDZC2 <===                                                       722
      CALL AGSRESET                                                       723
      AG_MOTHER = 'CAVE'                                                  723
      AG_X = 0                                                            723
      AG_Y = VPDS_IBSDYC1+UDSHIFT                                         723
      AG_Z = VPDS_IBSDZC2                                                 723
      AG_TITLE = 'IBSD'                                                   723
      AG_EXNAME = 'POSITION'                                              723
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         723
      CALL AXPOSITION                                                     723
C *                                                                       723
      ENDIF                                                               723
C Geant POSITION : IBSD ===> MOTHER='CAVE' X=0 Y=VPDS_IBSDYC1+UDSHIFT Z=  726
C -(VPDS_IBSDZC1+EWSHIFT) THETAZ=180 <===                                 726
      CALL AGSRESET                                                       726
      AG_MOTHER = 'CAVE'                                                  726
      AG_X = 0                                                            726
      AG_Y = VPDS_IBSDYC1+UDSHIFT                                         726
      AG_Z = -(VPDS_IBSDZC1+EWSHIFT)                                      726
      AG_THETAZ = 180                                                     726
      AG_TITLE = 'IBSD'                                                   726
      AG_EXNAME = 'POSITION'                                              726
      AG_PARLIST = 'MOTHER_X_Y_Z_THETAZ'                                  726
      CALL AXPOSITION                                                     726
C *                                                                       726
C Geant POSITION : IBSD ===> MOTHER='CAVE' X=0 Y=VPDS_IBSDYC1+UDSHIFT Z=  726
C -(VPDS_IBSDZC2+EWSHIFT) THETAZ=180 <===                                 726
      CALL AGSRESET                                                       728
      AG_MOTHER = 'CAVE'                                                  728
      AG_X = 0                                                            728
      AG_Y = VPDS_IBSDYC1+UDSHIFT                                         728
      AG_Z = -(VPDS_IBSDZC2+EWSHIFT)                                      728
      AG_THETAZ = 180                                                     728
      AG_TITLE = 'IBSD'                                                   728
      AG_EXNAME = 'POSITION'                                              728
      AG_PARLIST = 'MOTHER_X_Y_Z_THETAZ'                                  728
      CALL AXPOSITION                                                     728
C *                                                                       728
C Check KIBEAMSTYLE<4                                                     728
      IF (KIBEAMSTYLE.LT.4) THEN                                          729
C Geant POSITION : IBSD ===> MOTHER='CAVE' X=0 Y=VPDS_IBSDYC2 Z=VPDS_IBS  730
C DZC1 <===                                                               730
      CALL AGSRESET                                                       730
      AG_MOTHER = 'CAVE'                                                  730
      AG_X = 0                                                            730
      AG_Y = VPDS_IBSDYC2                                                 730
      AG_Z = VPDS_IBSDZC1                                                 730
      AG_TITLE = 'IBSD'                                                   730
      AG_EXNAME = 'POSITION'                                              730
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         730
      CALL AXPOSITION                                                     730
C *                                                                       730
C Geant POSITION : IBSD ===> MOTHER='CAVE' X=0 Y=VPDS_IBSDYC2 Z=VPDS_IBS  730
C DZC2 <===                                                               730
      CALL AGSRESET                                                       731
      AG_MOTHER = 'CAVE'                                                  731
      AG_X = 0                                                            731
      AG_Y = VPDS_IBSDYC2                                                 731
      AG_Z = VPDS_IBSDZC2                                                 731
      AG_TITLE = 'IBSD'                                                   731
      AG_EXNAME = 'POSITION'                                              731
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         731
      CALL AXPOSITION                                                     731
C *                                                                       731
      ENDIF                                                               731
C Geant POSITION : IBSD ===> MOTHER='CAVE' X=0 Y=VPDS_IBSDYC2 Z=-(VPDS_I  733
C BSDZC1+EWSHIFT) THETAZ=180 <===                                         733
      CALL AGSRESET                                                       733
      AG_MOTHER = 'CAVE'                                                  733
      AG_X = 0                                                            733
      AG_Y = VPDS_IBSDYC2                                                 733
      AG_Z = -(VPDS_IBSDZC1+EWSHIFT)                                      733
      AG_THETAZ = 180                                                     733
      AG_TITLE = 'IBSD'                                                   733
      AG_EXNAME = 'POSITION'                                              733
      AG_PARLIST = 'MOTHER_X_Y_Z_THETAZ'                                  733
      CALL AXPOSITION                                                     733
C *                                                                       733
C Geant POSITION : IBSD ===> MOTHER='CAVE' X=0 Y=VPDS_IBSDYC2 Z=-(VPDS_I  733
C BSDZC2+EWSHIFT) THETAZ=180 <===                                         733
      CALL AGSRESET                                                       734
      AG_MOTHER = 'CAVE'                                                  734
      AG_X = 0                                                            734
      AG_Y = VPDS_IBSDYC2                                                 734
      AG_Z = -(VPDS_IBSDZC2+EWSHIFT)                                      734
      AG_THETAZ = 180                                                     734
      AG_TITLE = 'IBSD'                                                   734
      AG_EXNAME = 'POSITION'                                              734
      AG_PARLIST = 'MOTHER_X_Y_Z_THETAZ'                                  734
      CALL AXPOSITION                                                     734
C *                                                                       734
      TEMPOS = (VPDG_IBPOSYC+VPDG_IBTHICKH+(VPDG_IBHEIGHT/2.))            734
      CALL AGSBEGM(MODULE,IPRIN)                                          736
C Check IPRIN<0                                                           736
      IF (IPRIN.LT.0) RETURN                                              736
C Create IBSE :                                                           736
C Call To Local Procedure IBSE                                            736
      ASSIGN 5140 TO BLOCK_IBSE                                           736
      GO TO  5130                                                         736
5140  CONTINUE                                                            736
C *                                                                       736
C Check KIBEAMSTYLE<4                                                     737
      IF (KIBEAMSTYLE.LT.4) THEN                                          737
C Geant POSITION : IBSE ===> MOTHER='CAVE' X=+VPDS_IBSEXC Y=VPDS_IBSEYC   738
C Z=VPDS_IBSEZC1 KONLY='MANY' <===                                        738
      CALL AGSRESET                                                       738
      AG_MOTHER = 'CAVE'                                                  738
      AG_X = +VPDS_IBSEXC                                                 738
      AG_Y = VPDS_IBSEYC                                                  738
      AG_Z = VPDS_IBSEZC1                                                 738
      AG_KONLY = 'MANY'                                                   738
      AG_TITLE = 'IBSE'                                                   738
      AG_EXNAME = 'POSITION'                                              738
      AG_PARLIST = 'MOTHER_X_Y_Z_KONLY'                                   738
      CALL AXPOSITION                                                     738
C *                                                                       738
C Geant POSITION : IBSE ===> MOTHER='CAVE' X=-VPDS_IBSEXC Y=VPDS_IBSEYC   738
C Z=VPDS_IBSEZC1 KONLY='MANY' <===                                        738
      CALL AGSRESET                                                       739
      AG_MOTHER = 'CAVE'                                                  739
      AG_X = -VPDS_IBSEXC                                                 739
      AG_Y = VPDS_IBSEYC                                                  739
      AG_Z = VPDS_IBSEZC1                                                 739
      AG_KONLY = 'MANY'                                                   739
      AG_TITLE = 'IBSE'                                                   739
      AG_EXNAME = 'POSITION'                                              739
      AG_PARLIST = 'MOTHER_X_Y_Z_KONLY'                                   739
      CALL AXPOSITION                                                     739
C *                                                                       739
C Geant POSITION : IBSE ===> MOTHER='CAVE' X=+VPDS_IBSEXC Y=VPDS_IBSEYC   739
C Z=VPDS_IBSEZC2 KONLY='MANY' <===                                        739
      CALL AGSRESET                                                       740
      AG_MOTHER = 'CAVE'                                                  740
      AG_X = +VPDS_IBSEXC                                                 740
      AG_Y = VPDS_IBSEYC                                                  740
      AG_Z = VPDS_IBSEZC2                                                 740
      AG_KONLY = 'MANY'                                                   740
      AG_TITLE = 'IBSE'                                                   740
      AG_EXNAME = 'POSITION'                                              740
      AG_PARLIST = 'MOTHER_X_Y_Z_KONLY'                                   740
      CALL AXPOSITION                                                     740
C *                                                                       740
C Geant POSITION : IBSE ===> MOTHER='CAVE' X=-VPDS_IBSEXC Y=VPDS_IBSEYC   740
C Z=VPDS_IBSEZC2 KONLY='MANY' <===                                        740
      CALL AGSRESET                                                       741
      AG_MOTHER = 'CAVE'                                                  741
      AG_X = -VPDS_IBSEXC                                                 741
      AG_Y = VPDS_IBSEYC                                                  741
      AG_Z = VPDS_IBSEZC2                                                 741
      AG_KONLY = 'MANY'                                                   741
      AG_TITLE = 'IBSE'                                                   741
      AG_EXNAME = 'POSITION'                                              741
      AG_PARLIST = 'MOTHER_X_Y_Z_KONLY'                                   741
      CALL AXPOSITION                                                     741
C *                                                                       741
      ENDIF                                                               741
C Geant POSITION : IBSE ===> MOTHER='CAVE' X=+VPDS_IBSEXC Y=VPDS_IBSEYC   744
C Z=-(VPDS_IBSEZC1+EWSHIFT) KONLY='MANY' <===                             744
      CALL AGSRESET                                                       744
      AG_MOTHER = 'CAVE'                                                  744
      AG_X = +VPDS_IBSEXC                                                 744
      AG_Y = VPDS_IBSEYC                                                  744
      AG_Z = -(VPDS_IBSEZC1+EWSHIFT)                                      744
      AG_KONLY = 'MANY'                                                   744
      AG_TITLE = 'IBSE'                                                   744
      AG_EXNAME = 'POSITION'                                              744
      AG_PARLIST = 'MOTHER_X_Y_Z_KONLY'                                   744
      CALL AXPOSITION                                                     744
C *                                                                       744
C Geant POSITION : IBSE ===> MOTHER='CAVE' X=-VPDS_IBSEXC Y=VPDS_IBSEYC   744
C Z=-(VPDS_IBSEZC1+EWSHIFT) KONLY='MANY' <===                             744
      CALL AGSRESET                                                       746
      AG_MOTHER = 'CAVE'                                                  746
      AG_X = -VPDS_IBSEXC                                                 746
      AG_Y = VPDS_IBSEYC                                                  746
      AG_Z = -(VPDS_IBSEZC1+EWSHIFT)                                      746
      AG_KONLY = 'MANY'                                                   746
      AG_TITLE = 'IBSE'                                                   746
      AG_EXNAME = 'POSITION'                                              746
      AG_PARLIST = 'MOTHER_X_Y_Z_KONLY'                                   746
      CALL AXPOSITION                                                     746
C *                                                                       746
C Geant POSITION : IBSE ===> MOTHER='CAVE' X=+VPDS_IBSEXC Y=VPDS_IBSEYC   746
C Z=-(VPDS_IBSEZC2+EWSHIFT) KONLY='MANY' <===                             746
      CALL AGSRESET                                                       748
      AG_MOTHER = 'CAVE'                                                  748
      AG_X = +VPDS_IBSEXC                                                 748
      AG_Y = VPDS_IBSEYC                                                  748
      AG_Z = -(VPDS_IBSEZC2+EWSHIFT)                                      748
      AG_KONLY = 'MANY'                                                   748
      AG_TITLE = 'IBSE'                                                   748
      AG_EXNAME = 'POSITION'                                              748
      AG_PARLIST = 'MOTHER_X_Y_Z_KONLY'                                   748
      CALL AXPOSITION                                                     748
C *                                                                       748
C Geant POSITION : IBSE ===> MOTHER='CAVE' X=-VPDS_IBSEXC Y=VPDS_IBSEYC   748
C Z=-(VPDS_IBSEZC2+EWSHIFT) KONLY='MANY' <===                             748
      CALL AGSRESET                                                       750
      AG_MOTHER = 'CAVE'                                                  750
      AG_X = -VPDS_IBSEXC                                                 750
      AG_Y = VPDS_IBSEYC                                                  750
      AG_Z = -(VPDS_IBSEZC2+EWSHIFT)                                      750
      AG_KONLY = 'MANY'                                                   750
      AG_TITLE = 'IBSE'                                                   750
      AG_EXNAME = 'POSITION'                                              750
      AG_PARLIST = 'MOTHER_X_Y_Z_KONLY'                                   750
      CALL AXPOSITION                                                     750
C *                                                                       750
      CALL AGSBEGM(MODULE,IPRIN)                                          750
C Check IPRIN<0                                                           751
      IF (IPRIN.LT.0) RETURN                                              751
C Create IBSF :                                                           751
C Call To Local Procedure IBSF                                            751
      ASSIGN 5160 TO BLOCK_IBSF                                           751
      GO TO  5150                                                         751
5160  CONTINUE                                                            751
C *                                                                       751
C Check KIBEAMSTYLE<4                                                     752
      IF (KIBEAMSTYLE.LT.4) THEN                                          752
C Geant POSITION : IBSF ===> MOTHER='CAVE' Y=VPDG_IBPOSYC+UDSHIFT Z=VPDS  753
C _IBSFZC ALPHAY=90 <===                                                  753
      CALL AGSRESET                                                       753
      AG_MOTHER = 'CAVE'                                                  753
      AG_Y = VPDG_IBPOSYC+UDSHIFT                                         753
      AG_Z = VPDS_IBSFZC                                                  753
      AG_ALPHAN=AG_ALPHAN+1                                               753
      AG_ALPHAK(AG_ALPHAN)=2                                              753
      AG_ALPHAL(AG_ALPHAN)=90                                             753
      AG_TITLE = 'IBSF'                                                   753
      AG_EXNAME = 'POSITION'                                              753
      AG_PARLIST = 'MOTHER_Y_Z_ALPHAY'                                    753
      CALL AXPOSITION                                                     753
C *                                                                       753
      ENDIF                                                               753
C Geant POSITION : IBSF ===> MOTHER='CAVE' Y=VPDG_IBPOSYC+UDSHIFT Z=-(VP  755
C DS_IBSFZC+EWSHIFT) ALPHAY=90 <===                                       755
      CALL AGSRESET                                                       755
      AG_MOTHER = 'CAVE'                                                  755
      AG_Y = VPDG_IBPOSYC+UDSHIFT                                         755
      AG_Z = -(VPDS_IBSFZC+EWSHIFT)                                       755
      AG_ALPHAN=AG_ALPHAN+1                                               755
      AG_ALPHAK(AG_ALPHAN)=2                                              755
      AG_ALPHAL(AG_ALPHAN)=90                                             755
      AG_TITLE = 'IBSF'                                                   755
      AG_EXNAME = 'POSITION'                                              755
      AG_PARLIST = 'MOTHER_Y_Z_ALPHAY'                                    755
      CALL AXPOSITION                                                     755
C *                                                                       755
      CALL AGSBEGM(MODULE,IPRIN)                                          755
C Check IPRIN<0                                                           756
      IF (IPRIN.LT.0) RETURN                                              756
C Create IBST :                                                           756
C Call To Local Procedure IBST                                            756
      ASSIGN 5180 TO BLOCK_IBST                                           756
      GO TO  5170                                                         756
5180  CONTINUE                                                            756
C *                                                                       756
C Check KIBEAMSTYLE<4                                                     757
      IF (KIBEAMSTYLE.LT.4) THEN                                          757
C Geant POSITION : IBST ===> MOTHER='IBEM' Y=0 Z=+(VPDS_IBSFZC)-(VPDG_IB  758
C POSZC) ALPHAY=90 <===                                                   758
      CALL AGSRESET                                                       758
      AG_MOTHER = 'IBEM'                                                  758
      AG_Y = 0                                                            758
      AG_Z = +(VPDS_IBSFZC)-(VPDG_IBPOSZC)                                758
      AG_ALPHAN=AG_ALPHAN+1                                               758
      AG_ALPHAK(AG_ALPHAN)=2                                              758
      AG_ALPHAL(AG_ALPHAN)=90                                             758
      AG_TITLE = 'IBST'                                                   758
      AG_EXNAME = 'POSITION'                                              758
      AG_PARLIST = 'MOTHER_Y_Z_ALPHAY'                                    758
      CALL AXPOSITION                                                     758
C *                                                                       758
      ENDIF                                                               758
C Geant POSITION : IBST ===> MOTHER='IBEM' Y=0 Z=-(VPDS_IBSFZC+EWSHIFT)+  760
C (VPDG_IBPOSZC) ALPHAY=90 <===                                           760
      CALL AGSRESET                                                       760
      AG_MOTHER = 'IBEM'                                                  760
      AG_Y = 0                                                            760
      AG_Z = -(VPDS_IBSFZC+EWSHIFT)+(VPDG_IBPOSZC)                        760
      AG_ALPHAN=AG_ALPHAN+1                                               760
      AG_ALPHAK(AG_ALPHAN)=2                                              760
      AG_ALPHAL(AG_ALPHAN)=90                                             760
      AG_TITLE = 'IBST'                                                   760
      AG_EXNAME = 'POSITION'                                              760
      AG_PARLIST = 'MOTHER_Y_Z_ALPHAY'                                    760
      CALL AXPOSITION                                                     760
C *                                                                       760
      CALL AGSBEGM(MODULE,IPRIN)                                          760
C Check IPRIN<0                                                           761
      IF (IPRIN.LT.0) RETURN                                              761
C Create IBSG :                                                           761
C Call To Local Procedure IBSG                                            761
      ASSIGN 5200 TO BLOCK_IBSG                                           761
      GO TO  5190                                                         761
5200  CONTINUE                                                            761
C *                                                                       761
C Check KIBEAMSTYLE<4                                                     762
      IF (KIBEAMSTYLE.LT.4) THEN                                          762
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=+VPDS_IBSGXC Y=UDSHIFT+BOLT  764
C SHIFT+VPDS_IBSGYC Z=VPDS_IBSGZC1-VPDS_IBSGZC3 ALPHAX=90 <===            764
      CALL AGSRESET                                                       764
      AG_MOTHER = 'CAVE'                                                  764
      AG_X = +VPDS_IBSGXC                                                 764
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                764
      AG_Z = VPDS_IBSGZC1-VPDS_IBSGZC3                                    764
      AG_ALPHAN=AG_ALPHAN+1                                               764
      AG_ALPHAK(AG_ALPHAN)=1                                              764
      AG_ALPHAL(AG_ALPHAN)=90                                             764
      AG_TITLE = 'IBSG'                                                   764
      AG_EXNAME = 'POSITION'                                              764
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  764
      CALL AXPOSITION                                                     764
C *                                                                       764
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=-VPDS_IBSGXC Y=UDSHIFT+BOLT  764
C SHIFT+VPDS_IBSGYC Z=VPDS_IBSGZC1-VPDS_IBSGZC3 ALPHAX=90 <===            764
      CALL AGSRESET                                                       766
      AG_MOTHER = 'CAVE'                                                  766
      AG_X = -VPDS_IBSGXC                                                 766
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                766
      AG_Z = VPDS_IBSGZC1-VPDS_IBSGZC3                                    766
      AG_ALPHAN=AG_ALPHAN+1                                               766
      AG_ALPHAK(AG_ALPHAN)=1                                              766
      AG_ALPHAL(AG_ALPHAN)=90                                             766
      AG_TITLE = 'IBSG'                                                   766
      AG_EXNAME = 'POSITION'                                              766
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  766
      CALL AXPOSITION                                                     766
C *                                                                       766
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=+VPDS_IBSGXC Y=UDSHIFT+BOLT  766
C SHIFT+VPDS_IBSGYC Z=VPDS_IBSGZC1+VPDS_IBSGZC3 ALPHAX=90 <===            766
      CALL AGSRESET                                                       768
      AG_MOTHER = 'CAVE'                                                  768
      AG_X = +VPDS_IBSGXC                                                 768
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                768
      AG_Z = VPDS_IBSGZC1+VPDS_IBSGZC3                                    768
      AG_ALPHAN=AG_ALPHAN+1                                               768
      AG_ALPHAK(AG_ALPHAN)=1                                              768
      AG_ALPHAL(AG_ALPHAN)=90                                             768
      AG_TITLE = 'IBSG'                                                   768
      AG_EXNAME = 'POSITION'                                              768
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  768
      CALL AXPOSITION                                                     768
C *                                                                       768
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=-VPDS_IBSGXC Y=UDSHIFT+BOLT  768
C SHIFT+VPDS_IBSGYC Z=VPDS_IBSGZC1+VPDS_IBSGZC3 ALPHAX=90 <===            768
      CALL AGSRESET                                                       770
      AG_MOTHER = 'CAVE'                                                  770
      AG_X = -VPDS_IBSGXC                                                 770
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                770
      AG_Z = VPDS_IBSGZC1+VPDS_IBSGZC3                                    770
      AG_ALPHAN=AG_ALPHAN+1                                               770
      AG_ALPHAK(AG_ALPHAN)=1                                              770
      AG_ALPHAL(AG_ALPHAN)=90                                             770
      AG_TITLE = 'IBSG'                                                   770
      AG_EXNAME = 'POSITION'                                              770
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  770
      CALL AXPOSITION                                                     770
C *                                                                       770
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=+VPDS_IBSGXC Y=UDSHIFT+BOLT  770
C SHIFT+VPDS_IBSGYC Z=VPDS_IBSGZC2-VPDS_IBSGZC3 ALPHAX=90 <===            770
      CALL AGSRESET                                                       772
      AG_MOTHER = 'CAVE'                                                  772
      AG_X = +VPDS_IBSGXC                                                 772
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                772
      AG_Z = VPDS_IBSGZC2-VPDS_IBSGZC3                                    772
      AG_ALPHAN=AG_ALPHAN+1                                               772
      AG_ALPHAK(AG_ALPHAN)=1                                              772
      AG_ALPHAL(AG_ALPHAN)=90                                             772
      AG_TITLE = 'IBSG'                                                   772
      AG_EXNAME = 'POSITION'                                              772
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  772
      CALL AXPOSITION                                                     772
C *                                                                       772
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=-VPDS_IBSGXC Y=UDSHIFT+BOLT  772
C SHIFT+VPDS_IBSGYC Z=VPDS_IBSGZC2-VPDS_IBSGZC3 ALPHAX=90 <===            772
      CALL AGSRESET                                                       774
      AG_MOTHER = 'CAVE'                                                  774
      AG_X = -VPDS_IBSGXC                                                 774
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                774
      AG_Z = VPDS_IBSGZC2-VPDS_IBSGZC3                                    774
      AG_ALPHAN=AG_ALPHAN+1                                               774
      AG_ALPHAK(AG_ALPHAN)=1                                              774
      AG_ALPHAL(AG_ALPHAN)=90                                             774
      AG_TITLE = 'IBSG'                                                   774
      AG_EXNAME = 'POSITION'                                              774
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  774
      CALL AXPOSITION                                                     774
C *                                                                       774
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=+VPDS_IBSGXC Y=UDSHIFT+BOLT  774
C SHIFT+VPDS_IBSGYC Z=VPDS_IBSGZC2+VPDS_IBSGZC3 ALPHAX=90 <===            774
      CALL AGSRESET                                                       776
      AG_MOTHER = 'CAVE'                                                  776
      AG_X = +VPDS_IBSGXC                                                 776
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                776
      AG_Z = VPDS_IBSGZC2+VPDS_IBSGZC3                                    776
      AG_ALPHAN=AG_ALPHAN+1                                               776
      AG_ALPHAK(AG_ALPHAN)=1                                              776
      AG_ALPHAL(AG_ALPHAN)=90                                             776
      AG_TITLE = 'IBSG'                                                   776
      AG_EXNAME = 'POSITION'                                              776
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  776
      CALL AXPOSITION                                                     776
C *                                                                       776
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=-VPDS_IBSGXC Y=UDSHIFT+BOLT  776
C SHIFT+VPDS_IBSGYC Z=VPDS_IBSGZC2+VPDS_IBSGZC3 ALPHAX=90 <===            776
      CALL AGSRESET                                                       778
      AG_MOTHER = 'CAVE'                                                  778
      AG_X = -VPDS_IBSGXC                                                 778
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                778
      AG_Z = VPDS_IBSGZC2+VPDS_IBSGZC3                                    778
      AG_ALPHAN=AG_ALPHAN+1                                               778
      AG_ALPHAK(AG_ALPHAN)=1                                              778
      AG_ALPHAL(AG_ALPHAN)=90                                             778
      AG_TITLE = 'IBSG'                                                   778
      AG_EXNAME = 'POSITION'                                              778
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  778
      CALL AXPOSITION                                                     778
C *                                                                       778
      ENDIF                                                               778
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=+VPDS_IBSGXC Y=UDSHIFT+BOLT  781
C SHIFT+VPDS_IBSGYC Z=-(VPDS_IBSGZC1-VPDS_IBSGZC3+EWSHIFT) ALPHAX=90 <==  781
C =                                                                       781
      CALL AGSRESET                                                       781
      AG_MOTHER = 'CAVE'                                                  781
      AG_X = +VPDS_IBSGXC                                                 781
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                781
      AG_Z = -(VPDS_IBSGZC1-VPDS_IBSGZC3+EWSHIFT)                         781
      AG_ALPHAN=AG_ALPHAN+1                                               781
      AG_ALPHAK(AG_ALPHAN)=1                                              781
      AG_ALPHAL(AG_ALPHAN)=90                                             781
      AG_TITLE = 'IBSG'                                                   781
      AG_EXNAME = 'POSITION'                                              781
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  781
      CALL AXPOSITION                                                     781
C *                                                                       781
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=-VPDS_IBSGXC Y=UDSHIFT+BOLT  781
C SHIFT+VPDS_IBSGYC Z=-(VPDS_IBSGZC1-VPDS_IBSGZC3+EWSHIFT) ALPHAX=90 <==  781
C =                                                                       781
      CALL AGSRESET                                                       783
      AG_MOTHER = 'CAVE'                                                  783
      AG_X = -VPDS_IBSGXC                                                 783
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                783
      AG_Z = -(VPDS_IBSGZC1-VPDS_IBSGZC3+EWSHIFT)                         783
      AG_ALPHAN=AG_ALPHAN+1                                               783
      AG_ALPHAK(AG_ALPHAN)=1                                              783
      AG_ALPHAL(AG_ALPHAN)=90                                             783
      AG_TITLE = 'IBSG'                                                   783
      AG_EXNAME = 'POSITION'                                              783
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  783
      CALL AXPOSITION                                                     783
C *                                                                       783
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=+VPDS_IBSGXC Y=UDSHIFT+BOLT  783
C SHIFT+VPDS_IBSGYC Z=-(VPDS_IBSGZC1+VPDS_IBSGZC3+EWSHIFT) ALPHAX=90 <==  783
C =                                                                       783
      CALL AGSRESET                                                       785
      AG_MOTHER = 'CAVE'                                                  785
      AG_X = +VPDS_IBSGXC                                                 785
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                785
      AG_Z = -(VPDS_IBSGZC1+VPDS_IBSGZC3+EWSHIFT)                         785
      AG_ALPHAN=AG_ALPHAN+1                                               785
      AG_ALPHAK(AG_ALPHAN)=1                                              785
      AG_ALPHAL(AG_ALPHAN)=90                                             785
      AG_TITLE = 'IBSG'                                                   785
      AG_EXNAME = 'POSITION'                                              785
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  785
      CALL AXPOSITION                                                     785
C *                                                                       785
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=-VPDS_IBSGXC Y=UDSHIFT+BOLT  785
C SHIFT+VPDS_IBSGYC Z=-(VPDS_IBSGZC1+VPDS_IBSGZC3+EWSHIFT) ALPHAX=90 <==  785
C =                                                                       785
      CALL AGSRESET                                                       787
      AG_MOTHER = 'CAVE'                                                  787
      AG_X = -VPDS_IBSGXC                                                 787
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                787
      AG_Z = -(VPDS_IBSGZC1+VPDS_IBSGZC3+EWSHIFT)                         787
      AG_ALPHAN=AG_ALPHAN+1                                               787
      AG_ALPHAK(AG_ALPHAN)=1                                              787
      AG_ALPHAL(AG_ALPHAN)=90                                             787
      AG_TITLE = 'IBSG'                                                   787
      AG_EXNAME = 'POSITION'                                              787
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  787
      CALL AXPOSITION                                                     787
C *                                                                       787
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=+VPDS_IBSGXC Y=UDSHIFT+BOLT  787
C SHIFT+VPDS_IBSGYC Z=-(VPDS_IBSGZC2-VPDS_IBSGZC3+EWSHIFT) ALPHAX=90 <==  787
C =                                                                       787
      CALL AGSRESET                                                       789
      AG_MOTHER = 'CAVE'                                                  789
      AG_X = +VPDS_IBSGXC                                                 789
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                789
      AG_Z = -(VPDS_IBSGZC2-VPDS_IBSGZC3+EWSHIFT)                         789
      AG_ALPHAN=AG_ALPHAN+1                                               789
      AG_ALPHAK(AG_ALPHAN)=1                                              789
      AG_ALPHAL(AG_ALPHAN)=90                                             789
      AG_TITLE = 'IBSG'                                                   789
      AG_EXNAME = 'POSITION'                                              789
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  789
      CALL AXPOSITION                                                     789
C *                                                                       789
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=-VPDS_IBSGXC Y=UDSHIFT+BOLT  789
C SHIFT+VPDS_IBSGYC Z=-(VPDS_IBSGZC2-VPDS_IBSGZC3+EWSHIFT) ALPHAX=90 <==  789
C =                                                                       789
      CALL AGSRESET                                                       791
      AG_MOTHER = 'CAVE'                                                  791
      AG_X = -VPDS_IBSGXC                                                 791
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                791
      AG_Z = -(VPDS_IBSGZC2-VPDS_IBSGZC3+EWSHIFT)                         791
      AG_ALPHAN=AG_ALPHAN+1                                               791
      AG_ALPHAK(AG_ALPHAN)=1                                              791
      AG_ALPHAL(AG_ALPHAN)=90                                             791
      AG_TITLE = 'IBSG'                                                   791
      AG_EXNAME = 'POSITION'                                              791
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  791
      CALL AXPOSITION                                                     791
C *                                                                       791
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=+VPDS_IBSGXC Y=UDSHIFT+BOLT  791
C SHIFT+VPDS_IBSGYC Z=-(VPDS_IBSGZC2+VPDS_IBSGZC3+EWSHIFT) ALPHAX=90 <==  791
C =                                                                       791
      CALL AGSRESET                                                       793
      AG_MOTHER = 'CAVE'                                                  793
      AG_X = +VPDS_IBSGXC                                                 793
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                793
      AG_Z = -(VPDS_IBSGZC2+VPDS_IBSGZC3+EWSHIFT)                         793
      AG_ALPHAN=AG_ALPHAN+1                                               793
      AG_ALPHAK(AG_ALPHAN)=1                                              793
      AG_ALPHAL(AG_ALPHAN)=90                                             793
      AG_TITLE = 'IBSG'                                                   793
      AG_EXNAME = 'POSITION'                                              793
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  793
      CALL AXPOSITION                                                     793
C *                                                                       793
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=-VPDS_IBSGXC Y=UDSHIFT+BOLT  793
C SHIFT+VPDS_IBSGYC Z=-(VPDS_IBSGZC2+VPDS_IBSGZC3+EWSHIFT) ALPHAX=90 <==  793
C =                                                                       793
      CALL AGSRESET                                                       795
      AG_MOTHER = 'CAVE'                                                  795
      AG_X = -VPDS_IBSGXC                                                 795
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                795
      AG_Z = -(VPDS_IBSGZC2+VPDS_IBSGZC3+EWSHIFT)                         795
      AG_ALPHAN=AG_ALPHAN+1                                               795
      AG_ALPHAK(AG_ALPHAN)=1                                              795
      AG_ALPHAL(AG_ALPHAN)=90                                             795
      AG_TITLE = 'IBSG'                                                   795
      AG_EXNAME = 'POSITION'                                              795
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  795
      CALL AXPOSITION                                                     795
C *                                                                       795
      CALL AGSBEGM(MODULE,IPRIN)                                          795
C Check IPRIN<0                                                           796
      IF (IPRIN.LT.0) RETURN                                              796
C Create IBSH :                                                           796
C Call To Local Procedure IBSH                                            796
      ASSIGN 5220 TO BLOCK_IBSH                                           796
      GO TO  5210                                                         796
5220  CONTINUE                                                            796
C *                                                                       796
C Check KIBEAMSTYLE<4                                                     797
      IF (KIBEAMSTYLE.LT.4) THEN                                          797
C Geant POSITION : IBSH ===> MOTHER='CAVE' X=+VPDS_IBSHXC1 Z=+VPDS_IBSHZ  798
C C1 ALPHAY=90 <===                                                       798
      CALL AGSRESET                                                       798
      AG_MOTHER = 'CAVE'                                                  798
      AG_X = +VPDS_IBSHXC1                                                798
      AG_Z = +VPDS_IBSHZC1                                                798
      AG_ALPHAN=AG_ALPHAN+1                                               798
      AG_ALPHAK(AG_ALPHAN)=2                                              798
      AG_ALPHAL(AG_ALPHAN)=90                                             798
      AG_TITLE = 'IBSH'                                                   798
      AG_EXNAME = 'POSITION'                                              798
      AG_PARLIST = 'MOTHER_X_Z_ALPHAY'                                    798
      CALL AXPOSITION                                                     798
C *                                                                       798
C Geant POSITION : IBSH ===> MOTHER='CAVE' X=-VPDS_IBSHXC1 Z=+VPDS_IBSHZ  798
C C1 ALPHAY=90 <===                                                       798
      CALL AGSRESET                                                       799
      AG_MOTHER = 'CAVE'                                                  799
      AG_X = -VPDS_IBSHXC1                                                799
      AG_Z = +VPDS_IBSHZC1                                                799
      AG_ALPHAN=AG_ALPHAN+1                                               799
      AG_ALPHAK(AG_ALPHAN)=2                                              799
      AG_ALPHAL(AG_ALPHAN)=90                                             799
      AG_TITLE = 'IBSH'                                                   799
      AG_EXNAME = 'POSITION'                                              799
      AG_PARLIST = 'MOTHER_X_Z_ALPHAY'                                    799
      CALL AXPOSITION                                                     799
C *                                                                       799
C Geant POSITION : IBSH ===> MOTHER='CAVE' X=+VPDS_IBSHXC2 Z=+VPDS_IBSHZ  799
C C2 ALPHAY=90 <===                                                       799
      CALL AGSRESET                                                       800
      AG_MOTHER = 'CAVE'                                                  800
      AG_X = +VPDS_IBSHXC2                                                800
      AG_Z = +VPDS_IBSHZC2                                                800
      AG_ALPHAN=AG_ALPHAN+1                                               800
      AG_ALPHAK(AG_ALPHAN)=2                                              800
      AG_ALPHAL(AG_ALPHAN)=90                                             800
      AG_TITLE = 'IBSH'                                                   800
      AG_EXNAME = 'POSITION'                                              800
      AG_PARLIST = 'MOTHER_X_Z_ALPHAY'                                    800
      CALL AXPOSITION                                                     800
C *                                                                       800
C Geant POSITION : IBSH ===> MOTHER='CAVE' X=-VPDS_IBSHXC2 Z=+VPDS_IBSHZ  800
C C2 ALPHAY=90 <===                                                       800
      CALL AGSRESET                                                       801
      AG_MOTHER = 'CAVE'                                                  801
      AG_X = -VPDS_IBSHXC2                                                801
      AG_Z = +VPDS_IBSHZC2                                                801
      AG_ALPHAN=AG_ALPHAN+1                                               801
      AG_ALPHAK(AG_ALPHAN)=2                                              801
      AG_ALPHAL(AG_ALPHAN)=90                                             801
      AG_TITLE = 'IBSH'                                                   801
      AG_EXNAME = 'POSITION'                                              801
      AG_PARLIST = 'MOTHER_X_Z_ALPHAY'                                    801
      CALL AXPOSITION                                                     801
C *                                                                       801
      ENDIF                                                               801
C Geant POSITION : IBSH ===> MOTHER='CAVE' X=+VPDS_IBSHXC1 Z=-(VPDS_IBSH  803
C ZC1+EWSHIFT) ALPHAY=90 <===                                             803
      CALL AGSRESET                                                       803
      AG_MOTHER = 'CAVE'                                                  803
      AG_X = +VPDS_IBSHXC1                                                803
      AG_Z = -(VPDS_IBSHZC1+EWSHIFT)                                      803
      AG_ALPHAN=AG_ALPHAN+1                                               803
      AG_ALPHAK(AG_ALPHAN)=2                                              803
      AG_ALPHAL(AG_ALPHAN)=90                                             803
      AG_TITLE = 'IBSH'                                                   803
      AG_EXNAME = 'POSITION'                                              803
      AG_PARLIST = 'MOTHER_X_Z_ALPHAY'                                    803
      CALL AXPOSITION                                                     803
C *                                                                       803
C Geant POSITION : IBSH ===> MOTHER='CAVE' X=-VPDS_IBSHXC1 Z=-(VPDS_IBSH  803
C ZC1+EWSHIFT) ALPHAY=90 <===                                             803
      CALL AGSRESET                                                       804
      AG_MOTHER = 'CAVE'                                                  804
      AG_X = -VPDS_IBSHXC1                                                804
      AG_Z = -(VPDS_IBSHZC1+EWSHIFT)                                      804
      AG_ALPHAN=AG_ALPHAN+1                                               804
      AG_ALPHAK(AG_ALPHAN)=2                                              804
      AG_ALPHAL(AG_ALPHAN)=90                                             804
      AG_TITLE = 'IBSH'                                                   804
      AG_EXNAME = 'POSITION'                                              804
      AG_PARLIST = 'MOTHER_X_Z_ALPHAY'                                    804
      CALL AXPOSITION                                                     804
C *                                                                       804
C Geant POSITION : IBSH ===> MOTHER='CAVE' X=+VPDS_IBSHXC2 Z=-(VPDS_IBSH  804
C ZC2+EWSHIFT) ALPHAY=90 <===                                             804
      CALL AGSRESET                                                       805
      AG_MOTHER = 'CAVE'                                                  805
      AG_X = +VPDS_IBSHXC2                                                805
      AG_Z = -(VPDS_IBSHZC2+EWSHIFT)                                      805
      AG_ALPHAN=AG_ALPHAN+1                                               805
      AG_ALPHAK(AG_ALPHAN)=2                                              805
      AG_ALPHAL(AG_ALPHAN)=90                                             805
      AG_TITLE = 'IBSH'                                                   805
      AG_EXNAME = 'POSITION'                                              805
      AG_PARLIST = 'MOTHER_X_Z_ALPHAY'                                    805
      CALL AXPOSITION                                                     805
C *                                                                       805
C Geant POSITION : IBSH ===> MOTHER='CAVE' X=-VPDS_IBSHXC2 Z=-(VPDS_IBSH  805
C ZC2+EWSHIFT) ALPHAY=90 <===                                             805
      CALL AGSRESET                                                       806
      AG_MOTHER = 'CAVE'                                                  806
      AG_X = -VPDS_IBSHXC2                                                806
      AG_Z = -(VPDS_IBSHZC2+EWSHIFT)                                      806
      AG_ALPHAN=AG_ALPHAN+1                                               806
      AG_ALPHAK(AG_ALPHAN)=2                                              806
      AG_ALPHAL(AG_ALPHAN)=90                                             806
      AG_TITLE = 'IBSH'                                                   806
      AG_EXNAME = 'POSITION'                                              806
      AG_PARLIST = 'MOTHER_X_Z_ALPHAY'                                    806
      CALL AXPOSITION                                                     806
C *                                                                       806
      ENDIF                                                               806
C Check KIBEAMSTYLE>3                                                     808
      IF (KIBEAMSTYLE.GT.3) THEN                                          808
      CALL AGSBEGM(MODULE,IPRIN)                                          809
C Check IPRIN<0                                                           809
      IF (IPRIN.LT.0) RETURN                                              809
C Create IBWA :                                                           809
C Call To Local Procedure IBWA                                            809
      ASSIGN 5240 TO BLOCK_IBWA                                           809
      GO TO  5230                                                         809
5240  CONTINUE                                                            809
C *                                                                       809
      CALL AGSBEGM(MODULE,IPRIN)                                          810
C Check IPRIN<0                                                           810
      IF (IPRIN.LT.0) RETURN                                              810
C Create IBWD :                                                           810
C Call To Local Procedure IBWD                                            810
      ASSIGN 5260 TO BLOCK_IBWD                                           810
      GO TO  5250                                                         810
5260  CONTINUE                                                            810
C *                                                                       810
      CALL AGSBEGM(MODULE,IPRIN)                                          811
C Check IPRIN<0                                                           811
      IF (IPRIN.LT.0) RETURN                                              811
C Create IBWF :                                                           811
C Call To Local Procedure IBWF                                            811
      ASSIGN 5280 TO BLOCK_IBWF                                           811
      GO TO  5270                                                         811
5280  CONTINUE                                                            811
C *                                                                       811
      CALL AGSBEGM(MODULE,IPRIN)                                          812
C Check IPRIN<0                                                           812
      IF (IPRIN.LT.0) RETURN                                              812
C Create IBWH :                                                           812
C Call To Local Procedure IBWH                                            812
      ASSIGN 5300 TO BLOCK_IBWH                                           812
      GO TO  5290                                                         812
5300  CONTINUE                                                            812
C *                                                                       812
C Geant POSITION : IBWA ===> MOTHER='CAVE' X=WBSP_IBEAMP(1) Y=WBSP_IBEAM  813
C P(2) Z=WBSP_IBEAMP(3) <===                                              813
      CALL AGSRESET                                                       813
      AG_MOTHER = 'CAVE'                                                  813
      AG_X = WBSP_IBEAMP(1)                                               813
      AG_Y = WBSP_IBEAMP(2)                                               813
      AG_Z = WBSP_IBEAMP(3)                                               813
      AG_TITLE = 'IBWA'                                                   813
      AG_EXNAME = 'POSITION'                                              813
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         813
      CALL AXPOSITION                                                     813
C *                                                                       813
C Geant POSITION : IBWD ===> MOTHER='CAVE' X=WBSP_POSTP(1) Y=WBSP_POSTP(  813
C 2) Z=WBSP_POSTP(3) <===                                                 813
      CALL AGSRESET                                                       814
      AG_MOTHER = 'CAVE'                                                  814
      AG_X = WBSP_POSTP(1)                                                814
      AG_Y = WBSP_POSTP(2)                                                814
      AG_Z = WBSP_POSTP(3)                                                814
      AG_TITLE = 'IBWD'                                                   814
      AG_EXNAME = 'POSITION'                                              814
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         814
      CALL AXPOSITION                                                     814
C *                                                                       814
C Geant POSITION : IBWF ===> MOTHER='CAVE' X=WBSP_ANGLP(1) Y=WBSP_ANGLP(  814
C 2) Z=WBSP_ANGLP(3) ALPHAZ=90 ALPHAX=-45 <===                            814
      CALL AGSRESET                                                       816
      AG_MOTHER = 'CAVE'                                                  816
      AG_X = WBSP_ANGLP(1)                                                816
      AG_Y = WBSP_ANGLP(2)                                                816
      AG_Z = WBSP_ANGLP(3)                                                816
      AG_ALPHAN=AG_ALPHAN+1                                               816
      AG_ALPHAK(AG_ALPHAN)=3                                              816
      AG_ALPHAL(AG_ALPHAN)=90                                             816
      AG_ALPHAN=AG_ALPHAN+1                                               816
      AG_ALPHAK(AG_ALPHAN)=1                                              816
      AG_ALPHAL(AG_ALPHAN)=-45                                            816
      AG_TITLE = 'IBWF'                                                   816
      AG_EXNAME = 'POSITION'                                              816
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAZ_ALPHAX'                           816
      CALL AXPOSITION                                                     816
C *                                                                       816
C Geant POSITION : IBWH ===> MOTHER='CAVE' X=WBSP_TRIP(1) Y=WBSP_TRIP(2)  816
C  Z=WBSP_TRIP(3) <===                                                    816
      CALL AGSRESET                                                       817
      AG_MOTHER = 'CAVE'                                                  817
      AG_X = WBSP_TRIP(1)                                                 817
      AG_Y = WBSP_TRIP(2)                                                 817
      AG_Z = WBSP_TRIP(3)                                                 817
      AG_TITLE = 'IBWH'                                                   817
      AG_EXNAME = 'POSITION'                                              817
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         817
      CALL AXPOSITION                                                     817
C *                                                                       817
      ENDIF                                                               817
      ENDIF                                                               819
C ----------------------------------------------------------------------  820
C ------------ VPDD                                                       820
      CALL AGSENDM                                                        821
      RETURN                                                              821
C Begin Local Procedure VPDD                                              821
C VPDD IS THE WHOLE VPPD ASSEMBLY                                         821
5010  CONTINUE                                                            821
         AG_TITLE='VPDD'                                                  821
         AG_MODULE=MODULE                                                 821
         AG_MARK='V00'                                                    821
         CALL AGSPUSH                                                     821
         AG_MODULE=MODULE                                                 821
C    *                                                                    821
C    Geant MATERIAL : AIR ===> inherited arguments used <===              822
         CALL AGSRESET                                                    822
         AG_TITLE = 'AIR'                                                 822
         AG_EXNAME = 'MATERIAL'                                           822
         AG_PARLIST = 'NONE'                                              822
         CALL AXMATERIAL                                                  822
C    *                                                                    822
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           822
         CALL AGSRESET                                                    823
         AG_TITLE = 'STANDARD'                                            823
         AG_EXNAME = 'MEDIUM'                                             823
         AG_PARLIST = 'NONE'                                              823
         CALL AXMEDIUM                                                    823
C    *                                                                    823
C    Geant ATTRIBUTE : VPDD ===> SEEN=1 COLO=7 SERIAL=ZPOS <===           823
         CALL AGSRESET                                                    824
         AG_SEEN = 1                                                      824
         AG_COLO = 7                                                      824
         AG_SERIAL = ZPOS                                                 824
         AG_TITLE = 'VPDD'                                                824
         AG_EXNAME = 'ATTRIBUTE'                                          824
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  824
         CALL AXATTRIBUTE                                                 824
C    *                                                                    824
C    Check KDETSTYLE<2                                                    824
         IF (KDETSTYLE.LT.2) THEN                                         825
C    Geant SHAPE : TUBS ===> RMIN=VPDG_RMIN RMAX=VPDG_RMAX, PHI1=-65.0 P  827
C    HI2=245.0 DZ=VPDG_BPLENGTH/2 <===                                    827
         CALL AGSRESET                                                    827
         AG_RMIN = VPDG_RMIN                                              827
         AG_RMAX = VPDG_RMAX                                              827
         AG_PHI1 = -65.0                                                  827
         AG_PHI2 = 245.0                                                  827
         AG_DZ = VPDG_BPLENGTH/2                                          827
         AG_TITLE = 'TUBS'                                                827
         AG_EXNAME = 'SHAPE'                                              827
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            827
         CALL AXSHAPE                                                     827
C    *                                                                    827
         ELSE                                                             827
C    Geant SHAPE : TUBS ===> RMIN=VPDH_RMIN RMAX=VPDH_RMAX, PHI1=-65.0 P  830
C    HI2=245.0 DZ=VPDH_LENGTH/2 <===                                      830
         CALL AGSRESET                                                    830
         AG_RMIN = VPDH_RMIN                                              830
         AG_RMAX = VPDH_RMAX                                              830
         AG_PHI1 = -65.0                                                  830
         AG_PHI2 = 245.0                                                  830
         AG_DZ = VPDH_LENGTH/2                                            830
         AG_TITLE = 'TUBS'                                                830
         AG_EXNAME = 'SHAPE'                                              830
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            830
         CALL AXSHAPE                                                     830
C    *                                                                    830
         ENDIF                                                            830
C    Check KDETSTYLE<2                                                    832
         IF (KDETSTYLE.LT.2) THEN                                         832
         YBASE= VPDG_IBPOSYC - (VPDG_IBHEIGHT + VPDG_BPTHICK)/2           833
         YTOP = YBASE + VPDG_BPTHICK/2+ VPDG_FPHEIGHT + VPDG_FPCHGHT      834
C    Create VPBP :                                                        835
C    Call To Local Procedure VPBP                                         835
         ASSIGN 5320 TO BLOCK_VPBP                                        835
         GO TO  5310                                                      835
5320     CONTINUE                                                         835
C    *                                                                    835
C    Geant POSITION : VPBP ===> Y=YBASE <===                              836
         CALL AGSRESET                                                    836
         AG_Y = YBASE                                                     836
         AG_TITLE = 'VPBP'                                                836
         AG_EXNAME = 'POSITION'                                           836
         AG_PARLIST = 'Y'                                                 836
         CALL AXPOSITION                                                  836
C    *                                                                    836
C    Create VPBO :                                                        837
C    Call To Local Procedure VPBO                                         837
         ASSIGN 5340 TO BLOCK_VPBO                                        837
         GO TO  5330                                                      837
5340     CONTINUE                                                         837
C    *                                                                    837
         XLOC=(VPDG_BPWIDTH-VPDG_FPWIDTH)/2                               838
         YLOC=YBASE+(VPDG_BPTHICK+VPDG_FPHHGHT)/2                         839
         ZLOC=(VPDG_BPLENGTH-2*VPDG_FPTHICK-VPDG_FPHHGHT)/2               840
C    Geant POSITION : VPBO ===> X=XLOC Y=YLOC Z=ZLOC THETAZ=180 <===      841
         CALL AGSRESET                                                    841
         AG_X = XLOC                                                      841
         AG_Y = YLOC                                                      841
         AG_Z = ZLOC                                                      841
         AG_THETAZ = 180                                                  841
         AG_TITLE = 'VPBO'                                                841
         AG_EXNAME = 'POSITION'                                           841
         AG_PARLIST = 'X_Y_Z_THETAZ'                                      841
         CALL AXPOSITION                                                  841
C    *                                                                    841
C    Geant POSITION : VPBO ===> X=XLOC Y=YLOC Z=-ZLOC <===                841
         CALL AGSRESET                                                    842
         AG_X = XLOC                                                      842
         AG_Y = YLOC                                                      842
         AG_Z = -ZLOC                                                     842
         AG_TITLE = 'VPBO'                                                842
         AG_EXNAME = 'POSITION'                                           842
         AG_PARLIST = 'X_Y_Z'                                             842
         CALL AXPOSITION                                                  842
C    *                                                                    842
C    Geant POSITION : VPBO ===> X=-XLOC Y=YLOC Z=ZLOC THETAZ=180 <===     842
         CALL AGSRESET                                                    843
         AG_X = -XLOC                                                     843
         AG_Y = YLOC                                                      843
         AG_Z = ZLOC                                                      843
         AG_THETAZ = 180                                                  843
         AG_TITLE = 'VPBO'                                                843
         AG_EXNAME = 'POSITION'                                           843
         AG_PARLIST = 'X_Y_Z_THETAZ'                                      843
         CALL AXPOSITION                                                  843
C    *                                                                    843
C    Geant POSITION : VPBO ===> X=-XLOC Y=YLOC Z=-ZLOC <===               843
         CALL AGSRESET                                                    844
         AG_X = -XLOC                                                     844
         AG_Y = YLOC                                                      844
         AG_Z = -ZLOC                                                     844
         AG_TITLE = 'VPBO'                                                844
         AG_EXNAME = 'POSITION'                                           844
         AG_PARLIST = 'X_Y_Z'                                             844
         CALL AXPOSITION                                                  844
C    *                                                                    844
C    Create VPFA :                                                        845
C    Call To Local Procedure VPFA                                         845
         ASSIGN 5360 TO BLOCK_VPFA                                        845
         GO TO  5350                                                      845
5360     CONTINUE                                                         845
C    *                                                                    845
         YLOC=YTOP-VPDG_FPAHGHT/2                                         846
         ZLOC=(VPDG_BPLENGTH - VPDG_FPTHICK)/2                            847
C    Geant POSITION : VPFA ===> Y=YLOC Z=ZLOC <===                        848
         CALL AGSRESET                                                    848
         AG_Y = YLOC                                                      848
         AG_Z = ZLOC                                                      848
         AG_TITLE = 'VPFA'                                                848
         AG_EXNAME = 'POSITION'                                           848
         AG_PARLIST = 'Y_Z'                                               848
         CALL AXPOSITION                                                  848
C    *                                                                    848
C    Geant POSITION : VPFA ===> Y=YLOC Z=-ZLOC <===                       848
         CALL AGSRESET                                                    849
         AG_Y = YLOC                                                      849
         AG_Z = -ZLOC                                                     849
         AG_TITLE = 'VPFA'                                                849
         AG_EXNAME = 'POSITION'                                           849
         AG_PARLIST = 'Y_Z'                                               849
         CALL AXPOSITION                                                  849
C    *                                                                    849
C    Create VPFB :                                                        850
C    Call To Local Procedure VPFB                                         850
         ASSIGN 5380 TO BLOCK_VPFB                                        850
         GO TO  5370                                                      850
5380     CONTINUE                                                         850
C    *                                                                    850
         XLOC=                                                            851
     *   (VPDG_FPAWIDTH+(VPDG_BPWIDTH-VPDG_FPAWIDTH-2*VPDG_FPWIDTH)/2)/   851
     *   2                                                                851
         YLOC=(YTOP - (VPDG_FPCHGHT+VPDG_FPAHGHT)/4)                      852
         ZLOC=(VPDG_BPLENGTH - VPDG_FPTHICK)/2                            853
C    Geant POSITION : VPFB ===> X=XLOC Y=YLOC Z=ZLOC ORT=Y-XZ <===        854
         CALL AGSRESET                                                    854
         AG_X = XLOC                                                      854
         AG_Y = YLOC                                                      854
         AG_Z = ZLOC                                                      854
         AG_ALPHAN=AG_ALPHAN+1                                            854
         AG_ALPHAK(AG_ALPHAN)=IGOR('Y-XZ')                                854
         AG_TITLE = 'VPFB'                                                854
         AG_EXNAME = 'POSITION'                                           854
         AG_PARLIST = 'X_Y_Z_ORT'                                         854
         CALL AXPOSITION                                                  854
C    *                                                                    854
C    Geant POSITION : VPFB ===> X=XLOC Y=YLOC Z=-ZLOC ORT=Y-XZ <===       854
         CALL AGSRESET                                                    855
         AG_X = XLOC                                                      855
         AG_Y = YLOC                                                      855
         AG_Z = -ZLOC                                                     855
         AG_ALPHAN=AG_ALPHAN+1                                            855
         AG_ALPHAK(AG_ALPHAN)=IGOR('Y-XZ')                                855
         AG_TITLE = 'VPFB'                                                855
         AG_EXNAME = 'POSITION'                                           855
         AG_PARLIST = 'X_Y_Z_ORT'                                         855
         CALL AXPOSITION                                                  855
C    *                                                                    855
C    Geant POSITION : VPFB ===> X=-XLOC Y=YLOC Z=ZLOC ORT=YXZ <===        855
         CALL AGSRESET                                                    856
         AG_X = -XLOC                                                     856
         AG_Y = YLOC                                                      856
         AG_Z = ZLOC                                                      856
         AG_ALPHAN=AG_ALPHAN+1                                            856
         AG_ALPHAK(AG_ALPHAN)=IGOR('YXZ')                                 856
         AG_TITLE = 'VPFB'                                                856
         AG_EXNAME = 'POSITION'                                           856
         AG_PARLIST = 'X_Y_Z_ORT'                                         856
         CALL AXPOSITION                                                  856
C    *                                                                    856
C    Geant POSITION : VPFB ===> X=-XLOC Y=YLOC Z=-ZLOC ORT=YXZ <===       856
         CALL AGSRESET                                                    857
         AG_X = -XLOC                                                     857
         AG_Y = YLOC                                                      857
         AG_Z = -ZLOC                                                     857
         AG_ALPHAN=AG_ALPHAN+1                                            857
         AG_ALPHAK(AG_ALPHAN)=IGOR('YXZ')                                 857
         AG_TITLE = 'VPFB'                                                857
         AG_EXNAME = 'POSITION'                                           857
         AG_PARLIST = 'X_Y_Z_ORT'                                         857
         CALL AXPOSITION                                                  857
C    *                                                                    857
C    Create VPFC :                                                        858
C    Call To Local Procedure VPFC                                         858
         ASSIGN 5400 TO BLOCK_VPFC                                        858
         GO TO  5390                                                      858
5400     CONTINUE                                                         858
C    *                                                                    858
         XLOC=(VPDG_BPWIDTH-VPDG_FPWIDTH)/2                               859
         YLOC=(YTOP-(VPDG_FPCHGHT+VPDG_FPWIDTH/2)/2)                      860
         ZLOC=(VPDG_BPLENGTH - VPDG_FPTHICK)/2                            861
C    Geant POSITION : VPFC ===> X=XLOC Y=YLOC Z=ZLOC ORT=Y-XZ <===        862
         CALL AGSRESET                                                    862
         AG_X = XLOC                                                      862
         AG_Y = YLOC                                                      862
         AG_Z = ZLOC                                                      862
         AG_ALPHAN=AG_ALPHAN+1                                            862
         AG_ALPHAK(AG_ALPHAN)=IGOR('Y-XZ')                                862
         AG_TITLE = 'VPFC'                                                862
         AG_EXNAME = 'POSITION'                                           862
         AG_PARLIST = 'X_Y_Z_ORT'                                         862
         CALL AXPOSITION                                                  862
C    *                                                                    862
C    Geant POSITION : VPFC ===> X=XLOC Y=YLOC Z=-ZLOC ORT=Y-XZ <===       862
         CALL AGSRESET                                                    863
         AG_X = XLOC                                                      863
         AG_Y = YLOC                                                      863
         AG_Z = -ZLOC                                                     863
         AG_ALPHAN=AG_ALPHAN+1                                            863
         AG_ALPHAK(AG_ALPHAN)=IGOR('Y-XZ')                                863
         AG_TITLE = 'VPFC'                                                863
         AG_EXNAME = 'POSITION'                                           863
         AG_PARLIST = 'X_Y_Z_ORT'                                         863
         CALL AXPOSITION                                                  863
C    *                                                                    863
C    Geant POSITION : VPFC ===> X=-XLOC Y=YLOC Z=ZLOC ORT=YXZ <===        863
         CALL AGSRESET                                                    864
         AG_X = -XLOC                                                     864
         AG_Y = YLOC                                                      864
         AG_Z = ZLOC                                                      864
         AG_ALPHAN=AG_ALPHAN+1                                            864
         AG_ALPHAK(AG_ALPHAN)=IGOR('YXZ')                                 864
         AG_TITLE = 'VPFC'                                                864
         AG_EXNAME = 'POSITION'                                           864
         AG_PARLIST = 'X_Y_Z_ORT'                                         864
         CALL AXPOSITION                                                  864
C    *                                                                    864
C    Geant POSITION : VPFC ===> X=-XLOC Y=YLOC Z=-ZLOC ORT=YXZ <===       864
         CALL AGSRESET                                                    865
         AG_X = -XLOC                                                     865
         AG_Y = YLOC                                                      865
         AG_Z = -ZLOC                                                     865
         AG_ALPHAN=AG_ALPHAN+1                                            865
         AG_ALPHAK(AG_ALPHAN)=IGOR('YXZ')                                 865
         AG_TITLE = 'VPFC'                                                865
         AG_EXNAME = 'POSITION'                                           865
         AG_PARLIST = 'X_Y_Z_ORT'                                         865
         CALL AXPOSITION                                                  865
C    *                                                                    865
C    Create VPFP :                                                        866
C    Call To Local Procedure VPFP                                         866
         ASSIGN 5420 TO BLOCK_VPFP                                        866
         GO TO  5410                                                      866
5420     CONTINUE                                                         866
C    *                                                                    866
         XLOC=(VPDG_BPWIDTH-VPDG_FPWIDTH)/2                               867
         YLOC=(YBASE+VPDG_BPTHICK/2+ VPDG_FPHEIGHT/2)                     868
         ZLOC=(VPDG_BPLENGTH-VPDG_FPTHICK)/2                              869
C    Geant POSITION : VPFP ===> X=XLOC Y=YLOC Z=ZLOC <===                 870
         CALL AGSRESET                                                    870
         AG_X = XLOC                                                      870
         AG_Y = YLOC                                                      870
         AG_Z = ZLOC                                                      870
         AG_TITLE = 'VPFP'                                                870
         AG_EXNAME = 'POSITION'                                           870
         AG_PARLIST = 'X_Y_Z'                                             870
         CALL AXPOSITION                                                  870
C    *                                                                    870
C    Geant POSITION : VPFP ===> X=XLOC Y=YLOC Z=-ZLOC <===                870
         CALL AGSRESET                                                    871
         AG_X = XLOC                                                      871
         AG_Y = YLOC                                                      871
         AG_Z = -ZLOC                                                     871
         AG_TITLE = 'VPFP'                                                871
         AG_EXNAME = 'POSITION'                                           871
         AG_PARLIST = 'X_Y_Z'                                             871
         CALL AXPOSITION                                                  871
C    *                                                                    871
C    Geant POSITION : VPFP ===> X=-XLOC Y=YLOC Z=ZLOC <===                871
         CALL AGSRESET                                                    872
         AG_X = -XLOC                                                     872
         AG_Y = YLOC                                                      872
         AG_Z = ZLOC                                                      872
         AG_TITLE = 'VPFP'                                                872
         AG_EXNAME = 'POSITION'                                           872
         AG_PARLIST = 'X_Y_Z'                                             872
         CALL AXPOSITION                                                  872
C    *                                                                    872
C    Geant POSITION : VPFP ===> X=-XLOC Y=YLOC Z=-ZLOC <===               872
         CALL AGSRESET                                                    873
         AG_X = -XLOC                                                     873
         AG_Y = YLOC                                                      873
         AG_Z = -ZLOC                                                     873
         AG_TITLE = 'VPFP'                                                873
         AG_EXNAME = 'POSITION'                                           873
         AG_PARLIST = 'X_Y_Z'                                             873
         CALL AXPOSITION                                                  873
C    *                                                                    873
         STRUTHEIGHT = VPDG_STDIAGSZ/COS(VPDG_STANGLE*DEGRAD) +           873
     *   VPDG_BPLENGTH*TAN(VPDG_STANGLE*DEGRAD)                           873
C    Create VPST :                                                        875
C    Call To Local Procedure VPST                                         875
         ASSIGN 5440 TO BLOCK_VPST                                        875
         GO TO  5430                                                      875
5440     CONTINUE                                                         875
C    *                                                                    875
         XLOC=(VPDG_BPWIDTH+VPDG_STTHICK)/2                               876
         YLOC=YTOP-VPDG_FPWIDTH-STRUTHEIGHT/2                             877
C    Geant POSITION : VPST ===> X=XLOC Y=YLOC <===                        878
         CALL AGSRESET                                                    878
         AG_X = XLOC                                                      878
         AG_Y = YLOC                                                      878
         AG_TITLE = 'VPST'                                                878
         AG_EXNAME = 'POSITION'                                           878
         AG_PARLIST = 'X_Y'                                               878
         CALL AXPOSITION                                                  878
C    *                                                                    878
C    Geant POSITION : VPST ===> X=-XLOC Y=YLOC <===                       878
         CALL AGSRESET                                                    879
         AG_X = -XLOC                                                     879
         AG_Y = YLOC                                                      879
         AG_TITLE = 'VPST'                                                879
         AG_EXNAME = 'POSITION'                                           879
         AG_PARLIST = 'X_Y'                                               879
         CALL AXPOSITION                                                  879
C    *                                                                    879
C    Create VPSC :                                                        880
C    Call To Local Procedure VPSC                                         880
         ASSIGN 5460 TO BLOCK_VPSC                                        880
         GO TO  5450                                                      880
5460     CONTINUE                                                         880
C    *                                                                    880
         XLOC=(VPDG_BPWIDTH-VPDG_SCWIDTH)/2                               881
         YLOC=YTOP-VPDG_FPWIDTH-STRUTHEIGHT+VPDG_SCHEIGHT/2               882
         ZLOC=(VPDG_BPLENGTH+VPDG_SCLENGTH)/2                             883
C    Geant POSITION : VPSC ===> X=XLOC Y=YLOC Z=ZLOC ALPHAX=180. <===     884
         CALL AGSRESET                                                    884
         AG_X = XLOC                                                      884
         AG_Y = YLOC                                                      884
         AG_Z = ZLOC                                                      884
         AG_ALPHAN=AG_ALPHAN+1                                            884
         AG_ALPHAK(AG_ALPHAN)=1                                           884
         AG_ALPHAL(AG_ALPHAN)=180.                                        884
         AG_TITLE = 'VPSC'                                                884
         AG_EXNAME = 'POSITION'                                           884
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      884
         CALL AXPOSITION                                                  884
C    *                                                                    884
C    Geant POSITION : VPSC ===> X=-XLOC Y=YLOC Z=ZLOC ALPHAX=180. ALPHAZ  884
C    =180. <===                                                           884
         CALL AGSRESET                                                    885
         AG_X = -XLOC                                                     885
         AG_Y = YLOC                                                      885
         AG_Z = ZLOC                                                      885
         AG_ALPHAN=AG_ALPHAN+1                                            885
         AG_ALPHAK(AG_ALPHAN)=1                                           885
         AG_ALPHAL(AG_ALPHAN)=180.                                        885
         AG_ALPHAN=AG_ALPHAN+1                                            885
         AG_ALPHAK(AG_ALPHAN)=3                                           885
         AG_ALPHAL(AG_ALPHAN)=180.                                        885
         AG_TITLE = 'VPSC'                                                885
         AG_EXNAME = 'POSITION'                                           885
         AG_PARLIST = 'X_Y_Z_ALPHAX_ALPHAZ'                               885
         CALL AXPOSITION                                                  885
C    *                                                                    885
         YLOC=YTOP-VPDG_FPWIDTH-VPDG_SCHEIGHT/2                           885
C    Geant POSITION : VPSC ===> X=XLOC Y=YLOC Z=-ZLOC <===                887
         CALL AGSRESET                                                    887
         AG_X = XLOC                                                      887
         AG_Y = YLOC                                                      887
         AG_Z = -ZLOC                                                     887
         AG_TITLE = 'VPSC'                                                887
         AG_EXNAME = 'POSITION'                                           887
         AG_PARLIST = 'X_Y_Z'                                             887
         CALL AXPOSITION                                                  887
C    *                                                                    887
C    Geant POSITION : VPSC ===> X=-XLOC Y=YLOC Z=-ZLOC ALPHAZ=180. <===   887
         CALL AGSRESET                                                    888
         AG_X = -XLOC                                                     888
         AG_Y = YLOC                                                      888
         AG_Z = -ZLOC                                                     888
         AG_ALPHAN=AG_ALPHAN+1                                            888
         AG_ALPHAK(AG_ALPHAN)=3                                           888
         AG_ALPHAL(AG_ALPHAN)=180.                                        888
         AG_TITLE = 'VPSC'                                                888
         AG_EXNAME = 'POSITION'                                           888
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      888
         CALL AXPOSITION                                                  888
C    *                                                                    888
C    Check VPDV_VPDCONFIG<4                                               888
         IF (VPDV_VPDCONFIG.LT.4) THEN                                    889
C    Create VPBX :                                                        890
C    Call To Local Procedure VPBX                                         890
         ASSIGN 5480 TO BLOCK_VPBX                                        890
         GO TO  5470                                                      890
5480     CONTINUE                                                         890
C    *                                                                    890
C    Geant POSITION : VPBX ===> X=-(VPDG_BPWIDTH-VPDG_BXWIDTH)/2 Y=(YBAS  892
C    E-VPDG_BPTHICK/2-VPDG_BXHEIGHT/2) <===                               892
         CALL AGSRESET                                                    892
         AG_X = -(VPDG_BPWIDTH-VPDG_BXWIDTH)/2                            892
         AG_Y = (YBASE-VPDG_BPTHICK/2-VPDG_BXHEIGHT/2)                    892
         AG_TITLE = 'VPBX'                                                892
         AG_EXNAME = 'POSITION'                                           892
         AG_PARLIST = 'X_Y'                                               892
         CALL AXPOSITION                                                  892
C    *                                                                    892
         ELSE                                                             892
C    Create VPBX :                                                        894
C    Call To Local Procedure VPBX                                         894
         ASSIGN 5490 TO BLOCK_VPBX                                        894
         GO TO  5470                                                      894
5490     CONTINUE                                                         894
C    *                                                                    894
C    Geant POSITION : VPBX ===> X=-(VPDG_BPWIDTH+2.*VPDG_STTHICK+VPDG_BX  896
C    HEIGHT)/2. Y=-4 THETAX=150 ALPHAZ=90 <===                            896
         CALL AGSRESET                                                    896
         AG_X = -(VPDG_BPWIDTH+2.*VPDG_STTHICK+VPDG_BXHEIGHT)/2.          896
         AG_Y = -4                                                        896
         AG_THETAX = 150                                                  896
         AG_ALPHAN=AG_ALPHAN+1                                            896
         AG_ALPHAK(AG_ALPHAN)=3                                           896
         AG_ALPHAL(AG_ALPHAN)=90                                          896
         AG_TITLE = 'VPBX'                                                896
         AG_EXNAME = 'POSITION'                                           896
         AG_PARLIST = 'X_Y_THETAX_ALPHAZ'                                 896
         CALL AXPOSITION                                                  896
C    *                                                                    896
         ENDIF                                                            896
         ENDIF                                                            898
C    Create VRNG :                                                        899
C    Call To Local Procedure VRNG                                         899
         ASSIGN 5510 TO BLOCK_VRNG                                        899
         GO TO  5500                                                      899
5510     CONTINUE                                                         899
C    *                                                                    899
C    Geant POSITION : VRNG ===> KONLY='MANY' <===                         900
         CALL AGSRESET                                                    900
         AG_KONLY = 'MANY'                                                900
         AG_TITLE = 'VRNG'                                                900
         AG_EXNAME = 'POSITION'                                           900
         AG_PARLIST = 'KONLY'                                             900
         CALL AXPOSITION                                                  900
C    *                                                                    900
5011  CALL AGSPOPB                                                        901
      GO TO BLOCK_VPDD, (5010 ,5020)                                      901
C End of Local Procedure                                                  901
C ----------------------------------------------------------------------  902
C ------------ VPBP                                                       902
C Begin Local Procedure VPBP                                              903
C VPBP IS THE BASE PLATE                                                  903
5310  CONTINUE                                                            903
         AG_TITLE='VPBP'                                                  903
         AG_MODULE=MODULE                                                 903
         AG_MARK='V00'                                                    903
         CALL AGSPUSH                                                     903
         AG_MODULE=MODULE                                                 903
C    *                                                                    903
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        904
         CALL AGSRESET                                                    904
         AG_TITLE = 'ALUMINIUM'                                           904
         AG_EXNAME = 'MATERIAL'                                           904
         AG_PARLIST = 'NONE'                                              904
         CALL AXMATERIAL                                                  904
C    *                                                                    904
C    Geant ATTRIBUTE : VPBP ===> SEEN=1 COLO=5 FILL=6 <===                904
         CALL AGSRESET                                                    905
         AG_SEEN = 1                                                      905
         AG_COLO = 5                                                      905
         AG_FILL = 6                                                      905
         AG_TITLE = 'VPBP'                                                905
         AG_EXNAME = 'ATTRIBUTE'                                          905
         AG_PARLIST = 'SEEN_COLO_FILL'                                    905
         CALL AXATTRIBUTE                                                 905
C    *                                                                    905
C    Geant SHAPE : BOX ===> DX=VPDG_BPWIDTH/2 DY=VPDG_BPTHICK/2 <===      905
         CALL AGSRESET                                                    906
         AG_DX = VPDG_BPWIDTH/2                                           906
         AG_DY = VPDG_BPTHICK/2                                           906
         AG_TITLE = 'BOX'                                                 906
         AG_EXNAME = 'SHAPE'                                              906
         AG_PARLIST = 'DX_DY'                                             906
         CALL AXSHAPE                                                     906
C    *                                                                    906
5311  CALL AGSPOPB                                                        907
      GO TO BLOCK_VPBP, (5310 ,5320)                                      907
C End of Local Procedure                                                  907
C ----------------------------------------------------------------------  908
C ------------ VPBO                                                       908
C Begin Local Procedure VPBO                                              909
C VPBO IS CONTAINER FOR THE HOOK                                          909
5330  CONTINUE                                                            909
         AG_TITLE='VPBO'                                                  909
         AG_MODULE=MODULE                                                 909
         AG_MARK='V00'                                                    909
         CALL AGSPUSH                                                     909
         AG_MODULE=MODULE                                                 909
C    *                                                                    909
C    Geant ATTRIBUTE : VPBO ===> SEEN=0 <===                              910
         CALL AGSRESET                                                    910
         AG_SEEN = 0                                                      910
         AG_TITLE = 'VPBO'                                                910
         AG_EXNAME = 'ATTRIBUTE'                                          910
         AG_PARLIST = 'SEEN'                                              910
         CALL AXATTRIBUTE                                                 910
C    *                                                                    910
C    Geant SHAPE : BOX ===> DX=VPDG_FPHWIDTH/2 DY=VPDG_FPHHGHT/2, DZ=VPD  910
C    G_FPHHGHT/2 <===                                                     910
         CALL AGSRESET                                                    912
         AG_DX = VPDG_FPHWIDTH/2                                          912
         AG_DY = VPDG_FPHHGHT/2                                           912
         AG_DZ = VPDG_FPHHGHT/2                                           912
         AG_TITLE = 'BOX'                                                 912
         AG_EXNAME = 'SHAPE'                                              912
         AG_PARLIST = 'DX_DY_DZ'                                          912
         CALL AXSHAPE                                                     912
C    *                                                                    912
C    Create VPBA :                                                        913
C    Call To Local Procedure VPBA                                         913
         ASSIGN 5530 TO BLOCK_VPBA                                        913
         GO TO  5520                                                      913
5530     CONTINUE                                                         913
C    *                                                                    913
C    Geant POSITION : VPBA ===> Z=-(VPDG_FPHHGHT-VPDG_FPHTHICK)/2 <===    914
         CALL AGSRESET                                                    914
         AG_Z = -(VPDG_FPHHGHT-VPDG_FPHTHICK)/2                           914
         AG_TITLE = 'VPBA'                                                914
         AG_EXNAME = 'POSITION'                                           914
         AG_PARLIST = 'Z'                                                 914
         CALL AXPOSITION                                                  914
C    *                                                                    914
C    Create VPBB :                                                        915
C    Call To Local Procedure VPBB                                         915
         ASSIGN 5550 TO BLOCK_VPBB                                        915
         GO TO  5540                                                      915
5550     CONTINUE                                                         915
C    *                                                                    915
C    Geant POSITION : VPBB ===> Y=-(VPDG_FPHHGHT-VPDG_FPHTHICK)/2 Z=VPDG  916
C    _FPHTHICK/2 <===                                                     916
         CALL AGSRESET                                                    916
         AG_Y = -(VPDG_FPHHGHT-VPDG_FPHTHICK)/2                           916
         AG_Z = VPDG_FPHTHICK/2                                           916
         AG_TITLE = 'VPBB'                                                916
         AG_EXNAME = 'POSITION'                                           916
         AG_PARLIST = 'Y_Z'                                               916
         CALL AXPOSITION                                                  916
C    *                                                                    916
5331  CALL AGSPOPB                                                        917
      GO TO BLOCK_VPBO, (5330 ,5340)                                      917
C End of Local Procedure                                                  917
C ----------------------------------------------------------------------  918
C ------------ VPBA                                                       918
C Begin Local Procedure VPBA                                              919
C VPBA IS THE PART OF THE HOOK THAT MOUNTS TO THE FRONT/BACK PLATE        919
5520  CONTINUE                                                            919
         AG_TITLE='VPBA'                                                  919
         AG_MODULE=MODULE                                                 919
         AG_MARK='V00'                                                    919
         CALL AGSPUSH                                                     919
         AG_MODULE=MODULE                                                 919
C    *                                                                    919
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        920
         CALL AGSRESET                                                    920
         AG_TITLE = 'ALUMINIUM'                                           920
         AG_EXNAME = 'MATERIAL'                                           920
         AG_PARLIST = 'NONE'                                              920
         CALL AXMATERIAL                                                  920
C    *                                                                    920
C    Geant ATTRIBUTE : VPBA ===> SEEN=1 COLO=5 FILL=6 <===                920
         CALL AGSRESET                                                    921
         AG_SEEN = 1                                                      921
         AG_COLO = 5                                                      921
         AG_FILL = 6                                                      921
         AG_TITLE = 'VPBA'                                                921
         AG_EXNAME = 'ATTRIBUTE'                                          921
         AG_PARLIST = 'SEEN_COLO_FILL'                                    921
         CALL AXATTRIBUTE                                                 921
C    *                                                                    921
C    Geant SHAPE : BOX ===> DX=VPDG_FPHWIDTH/2 DY=VPDG_FPHHGHT/2, DZ=VPD  921
C    G_FPHTHICK/2 <===                                                    921
         CALL AGSRESET                                                    923
         AG_DX = VPDG_FPHWIDTH/2                                          923
         AG_DY = VPDG_FPHHGHT/2                                           923
         AG_DZ = VPDG_FPHTHICK/2                                          923
         AG_TITLE = 'BOX'                                                 923
         AG_EXNAME = 'SHAPE'                                              923
         AG_PARLIST = 'DX_DY_DZ'                                          923
         CALL AXSHAPE                                                     923
C    *                                                                    923
5521  CALL AGSPOPB                                                        924
      GO TO BLOCK_VPBA, (5520 ,5530)                                      924
C End of Local Procedure                                                  924
C ----------------------------------------------------------------------  925
C ------------ VPBB                                                       925
C Begin Local Procedure VPBB                                              926
C VPBB IS THE PART OF THE HOOK THAT MOUNTS TO THE BASE PLATE              926
5540  CONTINUE                                                            926
         AG_TITLE='VPBB'                                                  926
         AG_MODULE=MODULE                                                 926
         AG_MARK='V00'                                                    926
         CALL AGSPUSH                                                     926
         AG_MODULE=MODULE                                                 926
C    *                                                                    926
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        927
         CALL AGSRESET                                                    927
         AG_TITLE = 'ALUMINIUM'                                           927
         AG_EXNAME = 'MATERIAL'                                           927
         AG_PARLIST = 'NONE'                                              927
         CALL AXMATERIAL                                                  927
C    *                                                                    927
C    Geant ATTRIBUTE : VPBB ===> SEEN=1 COLO=5 FILL=6 <===                927
         CALL AGSRESET                                                    928
         AG_SEEN = 1                                                      928
         AG_COLO = 5                                                      928
         AG_FILL = 6                                                      928
         AG_TITLE = 'VPBB'                                                928
         AG_EXNAME = 'ATTRIBUTE'                                          928
         AG_PARLIST = 'SEEN_COLO_FILL'                                    928
         CALL AXATTRIBUTE                                                 928
C    *                                                                    928
C    Geant SHAPE : BOX ===> DX=VPDG_FPHWIDTH/2 DY=VPDG_FPHTHICK/2, DZ=(V  928
C    PDG_FPHHGHT-VPDG_FPHTHICK)/2 <===                                    928
         CALL AGSRESET                                                    930
         AG_DX = VPDG_FPHWIDTH/2                                          930
         AG_DY = VPDG_FPHTHICK/2                                          930
         AG_DZ = (VPDG_FPHHGHT-VPDG_FPHTHICK)/2                           930
         AG_TITLE = 'BOX'                                                 930
         AG_EXNAME = 'SHAPE'                                              930
         AG_PARLIST = 'DX_DY_DZ'                                          930
         CALL AXSHAPE                                                     930
C    *                                                                    930
5541  CALL AGSPOPB                                                        931
      GO TO BLOCK_VPBB, (5540 ,5550)                                      931
C End of Local Procedure                                                  931
C ----------------------------------------------------------------------  932
C ------------ VPFP                                                       932
C Begin Local Procedure VPFP                                              933
C VPFP IS A SINGLE RECTANGULAR PIECE OF THE FRONTPANEL                    933
5410  CONTINUE                                                            933
         AG_TITLE='VPFP'                                                  933
         AG_MODULE=MODULE                                                 933
         AG_MARK='V00'                                                    933
         CALL AGSPUSH                                                     933
         AG_MODULE=MODULE                                                 933
C    *                                                                    933
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       934
         CALL AGSRESET                                                    934
         AG_TITLE = 'POLYSTYREN'                                          934
         AG_EXNAME = 'MATERIAL'                                           934
         AG_PARLIST = 'NONE'                                              934
         CALL AXMATERIAL                                                  934
C    *                                                                    934
C    Geant ATTRIBUTE : VPFP ===> SEEN=1 COLO=3 FILL=6 <===                934
         CALL AGSRESET                                                    935
         AG_SEEN = 1                                                      935
         AG_COLO = 3                                                      935
         AG_FILL = 6                                                      935
         AG_TITLE = 'VPFP'                                                935
         AG_EXNAME = 'ATTRIBUTE'                                          935
         AG_PARLIST = 'SEEN_COLO_FILL'                                    935
         CALL AXATTRIBUTE                                                 935
C    *                                                                    935
C    Geant SHAPE : BOX ===> DX=VPDG_FPWIDTH/2 DY=VPDG_FPHEIGHT/2, DZ=VPD  935
C    G_FPTHICK/2 <===                                                     935
         CALL AGSRESET                                                    937
         AG_DX = VPDG_FPWIDTH/2                                           937
         AG_DY = VPDG_FPHEIGHT/2                                          937
         AG_DZ = VPDG_FPTHICK/2                                           937
         AG_TITLE = 'BOX'                                                 937
         AG_EXNAME = 'SHAPE'                                              937
         AG_PARLIST = 'DX_DY_DZ'                                          937
         CALL AXSHAPE                                                     937
C    *                                                                    937
5411  CALL AGSPOPB                                                        938
      GO TO BLOCK_VPFP, (5410 ,5420)                                      938
C End of Local Procedure                                                  938
C ----------------------------------------------------------------------  939
C ------------ VPFA                                                       939
C Begin Local Procedure VPFA                                              940
C VPFA IS THE CENTRAL UPPER PART OF THE FRONTPLATE                        940
5350  CONTINUE                                                            940
         AG_TITLE='VPFA'                                                  940
         AG_MODULE=MODULE                                                 940
         AG_MARK='V00'                                                    940
         CALL AGSPUSH                                                     940
         AG_MODULE=MODULE                                                 940
C    *                                                                    940
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       941
         CALL AGSRESET                                                    941
         AG_TITLE = 'POLYSTYREN'                                          941
         AG_EXNAME = 'MATERIAL'                                           941
         AG_PARLIST = 'NONE'                                              941
         CALL AXMATERIAL                                                  941
C    *                                                                    941
C    Geant ATTRIBUTE : VPFA ===> SEEN=1 COLO=3 FILL=6 <===                941
         CALL AGSRESET                                                    942
         AG_SEEN = 1                                                      942
         AG_COLO = 3                                                      942
         AG_FILL = 6                                                      942
         AG_TITLE = 'VPFA'                                                942
         AG_EXNAME = 'ATTRIBUTE'                                          942
         AG_PARLIST = 'SEEN_COLO_FILL'                                    942
         CALL AXATTRIBUTE                                                 942
C    *                                                                    942
C    Geant SHAPE : BOX ===> DX=VPDG_FPAWIDTH/2 DY=VPDG_FPAHGHT/2, DZ=VPD  942
C    G_FPTHICK/2 <===                                                     942
         CALL AGSRESET                                                    944
         AG_DX = VPDG_FPAWIDTH/2                                          944
         AG_DY = VPDG_FPAHGHT/2                                           944
         AG_DZ = VPDG_FPTHICK/2                                           944
         AG_TITLE = 'BOX'                                                 944
         AG_EXNAME = 'SHAPE'                                              944
         AG_PARLIST = 'DX_DY_DZ'                                          944
         CALL AXSHAPE                                                     944
C    *                                                                    944
5351  CALL AGSPOPB                                                        945
      GO TO BLOCK_VPFA, (5350 ,5360)                                      945
C End of Local Procedure                                                  945
C ----------------------------------------------------------------------  946
C ------------ VPFB                                                       946
C Begin Local Procedure VPFB                                              947
C VPFB IS THE MIDDLE UPPER PART OF THE FRONTPLATE                         947
5370  CONTINUE                                                            947
         AG_TITLE='VPFB'                                                  947
         AG_MODULE=MODULE                                                 947
         AG_MARK='V00'                                                    947
         CALL AGSPUSH                                                     947
         AG_MODULE=MODULE                                                 947
C    *                                                                    947
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       948
         CALL AGSRESET                                                    948
         AG_TITLE = 'POLYSTYREN'                                          948
         AG_EXNAME = 'MATERIAL'                                           948
         AG_PARLIST = 'NONE'                                              948
         CALL AXMATERIAL                                                  948
C    *                                                                    948
C    Geant ATTRIBUTE : VPFB ===> SEEN=1 COLO=3 FILL=6 <===                948
         CALL AGSRESET                                                    949
         AG_SEEN = 1                                                      949
         AG_COLO = 3                                                      949
         AG_FILL = 6                                                      949
         AG_TITLE = 'VPFB'                                                949
         AG_EXNAME = 'ATTRIBUTE'                                          949
         AG_PARLIST = 'SEEN_COLO_FILL'                                    949
         CALL AXATTRIBUTE                                                 949
C    *                                                                    949
C    Geant SHAPE : TRAP ===> DZ=VPDG_FPTHICK/2 THET=0. PHI=0., H1=(VPDG_  949
C    BPWIDTH-VPDG_FPAWIDTH-2*VPDG_FPWIDTH)/4, BL1=VPDG_FPCHGHT/2 TL1=VPD  949
C    G_FPAHGHT/2, ALP1=ATAN(VPDG_FPAHGHT/(VPDG_BPWIDTH-VPDG_FPAWIDTH-2*V  949
C    PDG_FPWIDTH)/2)*RADDEG, H2=(VPDG_BPWIDTH-VPDG_FPAWIDTH-2*VPDG_FPWID  949
C    TH)/4, BL2=VPDG_FPCHGHT/2, TL2=VPDG_FPAHGHT/2 ALP2=ATAN(VPDG_FPAHGH  949
C    T/(VPDG_BPWIDTH-VPDG_FPAWIDTH-2*VPDG_FPWIDTH)/2)*RADDEG <===         949
         CALL AGSRESET                                                    956
         AG_DZ = VPDG_FPTHICK/2                                           956
         AG_THET = 0.                                                     956
         AG_PHI = 0.                                                      956
         AG_H1 = (VPDG_BPWIDTH-VPDG_FPAWIDTH-2*VPDG_FPWIDTH)/4            956
         AG_BL1 = VPDG_FPCHGHT/2                                          956
         AG_TL1 = VPDG_FPAHGHT/2                                          956
         AG_ALP1 = ATAN(VPDG_FPAHGHT/                                     956
     *   (VPDG_BPWIDTH-VPDG_FPAWIDTH-2*VPDG_FPWIDTH)/2)*RADDEG            956
         AG_H2 = (VPDG_BPWIDTH-VPDG_FPAWIDTH-2*VPDG_FPWIDTH)/4            956
         AG_BL2 = VPDG_FPCHGHT/2                                          956
         AG_TL2 = VPDG_FPAHGHT/2                                          956
         AG_ALP2 = ATAN(VPDG_FPAHGHT/                                     956
     *   (VPDG_BPWIDTH-VPDG_FPAWIDTH-2*VPDG_FPWIDTH)/2)*RADDEG            956
         AG_TITLE = 'TRAP'                                                956
         AG_EXNAME = 'SHAPE'                                              956
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       956
         CALL AXSHAPE                                                     956
C    *                                                                    956
5371  CALL AGSPOPB                                                        957
      GO TO BLOCK_VPFB, (5370 ,5380)                                      957
C End of Local Procedure                                                  957
C ----------------------------------------------------------------------  958
C ------------ VPFC                                                       958
C Begin Local Procedure VPFC                                              959
C VPFC IS THE OUTER UPPER PART OF THE FRONTPLATE                          959
5390  CONTINUE                                                            959
         AG_TITLE='VPFC'                                                  959
         AG_MODULE=MODULE                                                 959
         AG_MARK='V00'                                                    959
         CALL AGSPUSH                                                     959
         AG_MODULE=MODULE                                                 959
C    *                                                                    959
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       960
         CALL AGSRESET                                                    960
         AG_TITLE = 'POLYSTYREN'                                          960
         AG_EXNAME = 'MATERIAL'                                           960
         AG_PARLIST = 'NONE'                                              960
         CALL AXMATERIAL                                                  960
C    *                                                                    960
C    Geant ATTRIBUTE : VPFC ===> SEEN=1 COLO=3 FILL=6 <===                960
         CALL AGSRESET                                                    961
         AG_SEEN = 1                                                      961
         AG_COLO = 3                                                      961
         AG_FILL = 6                                                      961
         AG_TITLE = 'VPFC'                                                961
         AG_EXNAME = 'ATTRIBUTE'                                          961
         AG_PARLIST = 'SEEN_COLO_FILL'                                    961
         CALL AXATTRIBUTE                                                 961
C    *                                                                    961
C    Geant SHAPE : TRAP ===> DZ=VPDG_FPTHICK/2 THET=0. PHI=0., H1=VPDG_F  961
C    PWIDTH/2 BL1=(VPDG_FPCHGHT-VPDG_FPWIDTH)/2, TL1=VPDG_FPCHGHT/2 ALP1  961
C    =ATAN(.5)*RADDEG, H2=VPDG_FPWIDTH/2 BL2=(VPDG_FPCHGHT-VPDG_FPWIDTH)  961
C    /2, TL2=VPDG_FPCHGHT/2 ALP2=ATAN(.5)*RADDEG <===                     961
         CALL AGSRESET                                                    966
         AG_DZ = VPDG_FPTHICK/2                                           966
         AG_THET = 0.                                                     966
         AG_PHI = 0.                                                      966
         AG_H1 = VPDG_FPWIDTH/2                                           966
         AG_BL1 = (VPDG_FPCHGHT-VPDG_FPWIDTH)/2                           966
         AG_TL1 = VPDG_FPCHGHT/2                                          966
         AG_ALP1 = ATAN(.5)*RADDEG                                        966
         AG_H2 = VPDG_FPWIDTH/2                                           966
         AG_BL2 = (VPDG_FPCHGHT-VPDG_FPWIDTH)/2                           966
         AG_TL2 = VPDG_FPCHGHT/2                                          966
         AG_ALP2 = ATAN(.5)*RADDEG                                        966
         AG_TITLE = 'TRAP'                                                966
         AG_EXNAME = 'SHAPE'                                              966
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       966
         CALL AXSHAPE                                                     966
C    *                                                                    966
5391  CALL AGSPOPB                                                        967
      GO TO BLOCK_VPFC, (5390 ,5400)                                      967
C End of Local Procedure                                                  967
C ----------------------------------------------------------------------  968
C ------------ VPST                                                       968
C Begin Local Procedure VPST                                              969
C VPST IS THE STRUT VOLUME                                                969
5430  CONTINUE                                                            969
         AG_TITLE='VPST'                                                  969
         AG_MODULE=MODULE                                                 969
         AG_MARK='V00'                                                    969
         CALL AGSPUSH                                                     969
         AG_MODULE=MODULE                                                 969
C    *                                                                    969
C    Geant ATTRIBUTE : VPST ===> SEEN=0 <===                              970
         CALL AGSRESET                                                    970
         AG_SEEN = 0                                                      970
         AG_TITLE = 'VPST'                                                970
         AG_EXNAME = 'ATTRIBUTE'                                          970
         AG_PARLIST = 'SEEN'                                              970
         CALL AXATTRIBUTE                                                 970
C    *                                                                    970
C    Geant SHAPE : BOX ===> DX=VPDG_STTHICK/2 DY=STRUTHEIGHT/2, DZ=(VPDG  970
C    _BPLENGTH+2*VPDG_SCLENGTH)/2 <===                                    970
         CALL AGSRESET                                                    972
         AG_DX = VPDG_STTHICK/2                                           972
         AG_DY = STRUTHEIGHT/2                                            972
         AG_DZ = (VPDG_BPLENGTH+2*VPDG_SCLENGTH)/2                        972
         AG_TITLE = 'BOX'                                                 972
         AG_EXNAME = 'SHAPE'                                              972
         AG_PARLIST = 'DX_DY_DZ'                                          972
         CALL AXSHAPE                                                     972
C    *                                                                    972
C    Create VPSV :                                                        973
C    Call To Local Procedure VPSV                                         973
         ASSIGN 5570 TO BLOCK_VPSV                                        973
         GO TO  5560                                                      973
5570     CONTINUE                                                         973
C    *                                                                    973
C    Geant POSITION : VPSV ===> inherited arguments used <===             974
         CALL AGSRESET                                                    974
         AG_TITLE = 'VPSV'                                                974
         AG_EXNAME = 'POSITION'                                           974
         AG_PARLIST = 'NONE'                                              974
         CALL AXPOSITION                                                  974
C    *                                                                    974
C    Create VPSW :                                                        975
C    Call To Local Procedure VPSW                                         975
         ASSIGN 5590 TO BLOCK_VPSW                                        975
         GO TO  5580                                                      975
5590     CONTINUE                                                         975
C    *                                                                    975
         YDISPL=(VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE*DEGRAD)) +             976
     *   VPDG_SCHEIGHT/2)/2                                               976
C    Geant POSITION : VPSW ===> Y=STRUTHEIGHT/2-YDISPL Z=-(VPDG_BPLENGTH  978
C    +VPDG_SCLENGTH)/2 ORT=YZX ALPHAZ=180. <===                           978
         CALL AGSRESET                                                    978
         AG_Y = STRUTHEIGHT/2-YDISPL                                      978
         AG_Z = -(VPDG_BPLENGTH+VPDG_SCLENGTH)/2                          978
         AG_ALPHAN=AG_ALPHAN+1                                            978
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 978
         AG_ALPHAN=AG_ALPHAN+1                                            978
         AG_ALPHAK(AG_ALPHAN)=3                                           978
         AG_ALPHAL(AG_ALPHAN)=180.                                        978
         AG_TITLE = 'VPSW'                                                978
         AG_EXNAME = 'POSITION'                                           978
         AG_PARLIST = 'Y_Z_ORT_ALPHAZ'                                    978
         CALL AXPOSITION                                                  978
C    *                                                                    978
C    Geant POSITION : VPSW ===> Y=-(STRUTHEIGHT/2-YDISPL) Z=(VPDG_BPLENG  978
C    TH+VPDG_SCLENGTH)/2 ORT=YZX ALPHAY=180. <===                         978
         CALL AGSRESET                                                    980
         AG_Y = -(STRUTHEIGHT/2-YDISPL)                                   980
         AG_Z = (VPDG_BPLENGTH+VPDG_SCLENGTH)/2                           980
         AG_ALPHAN=AG_ALPHAN+1                                            980
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 980
         AG_ALPHAN=AG_ALPHAN+1                                            980
         AG_ALPHAK(AG_ALPHAN)=2                                           980
         AG_ALPHAL(AG_ALPHAN)=180.                                        980
         AG_TITLE = 'VPSW'                                                980
         AG_EXNAME = 'POSITION'                                           980
         AG_PARLIST = 'Y_Z_ORT_ALPHAY'                                    980
         CALL AXPOSITION                                                  980
C    *                                                                    980
5431  CALL AGSPOPB                                                        981
      GO TO BLOCK_VPST, (5430 ,5440)                                      981
C End of Local Procedure                                                  981
C ----------------------------------------------------------------------  982
C ------------ VPSV                                                       982
C Begin Local Procedure VPSV                                              983
C VPSV IS THE ACTUAL STRUT BETWEEN FRONT AND BACKPLATES                   983
5560  CONTINUE                                                            983
         AG_TITLE='VPSV'                                                  983
         AG_MODULE=MODULE                                                 983
         AG_MARK='V00'                                                    983
         CALL AGSPUSH                                                     983
         AG_MODULE=MODULE                                                 983
C    *                                                                    983
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        984
         CALL AGSRESET                                                    984
         AG_TITLE = 'ALUMINIUM'                                           984
         AG_EXNAME = 'MATERIAL'                                           984
         AG_PARLIST = 'NONE'                                              984
         CALL AXMATERIAL                                                  984
C    *                                                                    984
C    Geant ATTRIBUTE : VPSV ===> SEEN=1 COLO=3 FILL=6 <===                984
         CALL AGSRESET                                                    985
         AG_SEEN = 1                                                      985
         AG_COLO = 3                                                      985
         AG_FILL = 6                                                      985
         AG_TITLE = 'VPSV'                                                985
         AG_EXNAME = 'ATTRIBUTE'                                          985
         AG_PARLIST = 'SEEN_COLO_FILL'                                    985
         CALL AXATTRIBUTE                                                 985
C    *                                                                    985
C    Geant SHAPE : TRAP ===> DZ=VPDG_BPLENGTH/2 THET=VPDG_STANGLE, PHI=2  985
C    70. H1=VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE*DEGRAD)), BL1=VPDG_STTHICK  985
C    /2 TL1=VPDG_STTHICK/2, ALP1=0. H2=VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE  985
C    *DEGRAD)), BL2=VPDG_STTHICK/2 TL2=VPDG_STTHICK/2, ALP2=0. <===       985
         CALL AGSRESET                                                    991
         AG_DZ = VPDG_BPLENGTH/2                                          991
         AG_THET = VPDG_STANGLE                                           991
         AG_PHI = 270.                                                    991
         AG_H1 = VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE*DEGRAD))               991
         AG_BL1 = VPDG_STTHICK/2                                          991
         AG_TL1 = VPDG_STTHICK/2                                          991
         AG_ALP1 = 0.                                                     991
         AG_H2 = VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE*DEGRAD))               991
         AG_BL2 = VPDG_STTHICK/2                                          991
         AG_TL2 = VPDG_STTHICK/2                                          991
         AG_ALP2 = 0.                                                     991
         AG_TITLE = 'TRAP'                                                991
         AG_EXNAME = 'SHAPE'                                              991
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       991
         CALL AXSHAPE                                                     991
C    *                                                                    991
5561  CALL AGSPOPB                                                        992
      GO TO BLOCK_VPSV, (5560 ,5570)                                      992
C End of Local Procedure                                                  992
C ----------------------------------------------------------------------  993
C ------------ VPSW                                                       993
C Begin Local Procedure VPSW                                              994
C VPSW IS A TINY PIECE OF ALUMINIUM THAT BELONGS TO THE STRUT             994
5580  CONTINUE                                                            994
         AG_TITLE='VPSW'                                                  994
         AG_MODULE=MODULE                                                 994
         AG_MARK='V00'                                                    994
         CALL AGSPUSH                                                     994
         AG_MODULE=MODULE                                                 994
C    *                                                                    994
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        995
         CALL AGSRESET                                                    995
         AG_TITLE = 'ALUMINIUM'                                           995
         AG_EXNAME = 'MATERIAL'                                           995
         AG_PARLIST = 'NONE'                                              995
         CALL AXMATERIAL                                                  995
C    *                                                                    995
C    Geant ATTRIBUTE : VPSW ===> SEEN=1 COLO=3 FILL=6 <===                995
         CALL AGSRESET                                                    996
         AG_SEEN = 1                                                      996
         AG_COLO = 3                                                      996
         AG_FILL = 6                                                      996
         AG_TITLE = 'VPSW'                                                996
         AG_EXNAME = 'ATTRIBUTE'                                          996
         AG_PARLIST = 'SEEN_COLO_FILL'                                    996
         CALL AXATTRIBUTE                                                 996
C    *                                                                    996
C    Geant SHAPE : TRAP ===> DZ=VPDG_STTHICK/2 THET=0. PHI=0., H1=VPDG_S  996
C    CLENGTH/2 BL1=VPDG_SCHEIGHT/2 TL1=VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE  996
C    *DEGRAD)), ALP1=ATAN((VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE*DEGRAD))-VP  996
C    DG_SCHEIGHT/2)/VPDG_SCLENGTH)*RADDEG, H2=VPDG_SCLENGTH/2 BL2=VPDG_S  996
C    CHEIGHT/2 TL2=VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE*DEGRAD)), ALP2=ATAN  996
C    ((VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE*DEGRAD))-VPDG_SCHEIGHT/2)/VPDG_  996
C    SCLENGTH)*RADDEG <===                                                996
         CALL AGSRESET                                                    1001
         AG_DZ = VPDG_STTHICK/2                                           1001
         AG_THET = 0.                                                     1001
         AG_PHI = 0.                                                      1001
         AG_H1 = VPDG_SCLENGTH/2                                          1001
         AG_BL1 = VPDG_SCHEIGHT/2                                         1001
         AG_TL1 = VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE*DEGRAD))              1001
         AG_ALP1 = ATAN((VPDG_STDIAGSZ/                                   1001
     *   (2*COS(VPDG_STANGLE*DEGRAD))-VPDG_SCHEIGHT/2)/                   1001
     *   VPDG_SCLENGTH)*RADDEG                                            1001
         AG_H2 = VPDG_SCLENGTH/2                                          1001
         AG_BL2 = VPDG_SCHEIGHT/2                                         1001
         AG_TL2 = VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE*DEGRAD))              1001
         AG_ALP2 = ATAN((VPDG_STDIAGSZ/                                   1001
     *   (2*COS(VPDG_STANGLE*DEGRAD))-VPDG_SCHEIGHT/2)/                   1001
     *   VPDG_SCLENGTH)*RADDEG                                            1001
         AG_TITLE = 'TRAP'                                                1001
         AG_EXNAME = 'SHAPE'                                              1001
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       1001
         CALL AXSHAPE                                                     1001
C    *                                                                    1001
5581  CALL AGSPOPB                                                        1002
      GO TO BLOCK_VPSW, (5580 ,5590)                                      1002
C End of Local Procedure                                                  1002
C ----------------------------------------------------------------------  1003
C ------------ VPSC                                                       1003
C Begin Local Procedure VPSC                                              1004
C VPSC IS A CLAMP THAT HOLDS THE STRUT                                    1004
5450  CONTINUE                                                            1004
         AG_TITLE='VPSC'                                                  1004
         AG_MODULE=MODULE                                                 1004
         AG_MARK='V00'                                                    1004
         CALL AGSPUSH                                                     1004
         AG_MODULE=MODULE                                                 1004
C    *                                                                    1004
C    Geant SHAPE : BOX ===> DX=VPDG_SCWIDTH/2 DY=VPDG_SCHEIGHT/2, DZ=VPD  1006
C    G_SCLENGTH/2 <===                                                    1006
         CALL AGSRESET                                                    1006
         AG_DX = VPDG_SCWIDTH/2                                           1006
         AG_DY = VPDG_SCHEIGHT/2                                          1006
         AG_DZ = VPDG_SCLENGTH/2                                          1006
         AG_TITLE = 'BOX'                                                 1006
         AG_EXNAME = 'SHAPE'                                              1006
         AG_PARLIST = 'DX_DY_DZ'                                          1006
         CALL AXSHAPE                                                     1006
C    *                                                                    1006
C    Create VPSA :                                                        1007
C    Call To Local Procedure VPSA                                         1007
         ASSIGN 5610 TO BLOCK_VPSA                                        1007
         GO TO  5600                                                      1007
5610     CONTINUE                                                         1007
C    *                                                                    1007
C    Geant POSITION : VPSA ===> X=-VPDG_SCTHICK/2 Z=(VPDG_SCLENGTH-VPDG_  1008
C    SCTHICK)/2 <===                                                      1008
         CALL AGSRESET                                                    1008
         AG_X = -VPDG_SCTHICK/2                                           1008
         AG_Z = (VPDG_SCLENGTH-VPDG_SCTHICK)/2                            1008
         AG_TITLE = 'VPSA'                                                1008
         AG_EXNAME = 'POSITION'                                           1008
         AG_PARLIST = 'X_Z'                                               1008
         CALL AXPOSITION                                                  1008
C    *                                                                    1008
C    Create VPSB :                                                        1009
C    Call To Local Procedure VPSB                                         1009
         ASSIGN 5630 TO BLOCK_VPSB                                        1009
         GO TO  5620                                                      1009
5630     CONTINUE                                                         1009
C    *                                                                    1009
C    Geant POSITION : VPSB ===> X=(VPDG_SCWIDTH-VPDG_SCTHICK)/2 <===      1010
         CALL AGSRESET                                                    1010
         AG_X = (VPDG_SCWIDTH-VPDG_SCTHICK)/2                             1010
         AG_TITLE = 'VPSB'                                                1010
         AG_EXNAME = 'POSITION'                                           1010
         AG_PARLIST = 'X'                                                 1010
         CALL AXPOSITION                                                  1010
C    *                                                                    1010
5451  CALL AGSPOPB                                                        1011
      GO TO BLOCK_VPSC, (5450 ,5460)                                      1011
C End of Local Procedure                                                  1011
C ----------------------------------------------------------------------  1012
C ------------ VPSA                                                       1012
C Begin Local Procedure VPSA                                              1013
C VPSA IS PART OF A STRUT CLAMP THAT HOLDS TO THE FRONTPLATE              1013
5600  CONTINUE                                                            1013
         AG_TITLE='VPSA'                                                  1013
         AG_MODULE=MODULE                                                 1013
         AG_MARK='V00'                                                    1013
         CALL AGSPUSH                                                     1013
         AG_MODULE=MODULE                                                 1013
C    *                                                                    1013
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1014
         CALL AGSRESET                                                    1014
         AG_TITLE = 'ALUMINIUM'                                           1014
         AG_EXNAME = 'MATERIAL'                                           1014
         AG_PARLIST = 'NONE'                                              1014
         CALL AXMATERIAL                                                  1014
C    *                                                                    1014
C    Geant ATTRIBUTE : VPSA ===> SEEN=1 COLO=3 FILL=6 <===                1014
         CALL AGSRESET                                                    1015
         AG_SEEN = 1                                                      1015
         AG_COLO = 3                                                      1015
         AG_FILL = 6                                                      1015
         AG_TITLE = 'VPSA'                                                1015
         AG_EXNAME = 'ATTRIBUTE'                                          1015
         AG_PARLIST = 'SEEN_COLO_FILL'                                    1015
         CALL AXATTRIBUTE                                                 1015
C    *                                                                    1015
C    Geant SHAPE : BOX ===> DX=(VPDG_SCWIDTH-VPDG_SCTHICK)/2, DY=VPDG_SC  1015
C    HEIGHT/2 DZ=VPDG_SCTHICK/2 <===                                      1015
         CALL AGSRESET                                                    1017
         AG_DX = (VPDG_SCWIDTH-VPDG_SCTHICK)/2                            1017
         AG_DY = VPDG_SCHEIGHT/2                                          1017
         AG_DZ = VPDG_SCTHICK/2                                           1017
         AG_TITLE = 'BOX'                                                 1017
         AG_EXNAME = 'SHAPE'                                              1017
         AG_PARLIST = 'DX_DY_DZ'                                          1017
         CALL AXSHAPE                                                     1017
C    *                                                                    1017
5601  CALL AGSPOPB                                                        1018
      GO TO BLOCK_VPSA, (5600 ,5610)                                      1018
C End of Local Procedure                                                  1018
C ----------------------------------------------------------------------  1019
C ------------ VPSB                                                       1019
C Begin Local Procedure VPSB                                              1020
C VPSB IS PART OF A STRUT CLAMP THAT HOLDS TO THE STRUT                   1020
5620  CONTINUE                                                            1020
         AG_TITLE='VPSB'                                                  1020
         AG_MODULE=MODULE                                                 1020
         AG_MARK='V00'                                                    1020
         CALL AGSPUSH                                                     1020
         AG_MODULE=MODULE                                                 1020
C    *                                                                    1020
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1021
         CALL AGSRESET                                                    1021
         AG_TITLE = 'ALUMINIUM'                                           1021
         AG_EXNAME = 'MATERIAL'                                           1021
         AG_PARLIST = 'NONE'                                              1021
         CALL AXMATERIAL                                                  1021
C    *                                                                    1021
C    Geant ATTRIBUTE : VPSB ===> SEEN=1 COLO=3 FILL=6 <===                1021
         CALL AGSRESET                                                    1022
         AG_SEEN = 1                                                      1022
         AG_COLO = 3                                                      1022
         AG_FILL = 6                                                      1022
         AG_TITLE = 'VPSB'                                                1022
         AG_EXNAME = 'ATTRIBUTE'                                          1022
         AG_PARLIST = 'SEEN_COLO_FILL'                                    1022
         CALL AXATTRIBUTE                                                 1022
C    *                                                                    1022
C    Geant SHAPE : BOX ===> DX=VPDG_SCTHICK/2 DY=VPDG_SCHEIGHT/2, DZ=VPD  1022
C    G_SCLENGTH/2 <===                                                    1022
         CALL AGSRESET                                                    1024
         AG_DX = VPDG_SCTHICK/2                                           1024
         AG_DY = VPDG_SCHEIGHT/2                                          1024
         AG_DZ = VPDG_SCLENGTH/2                                          1024
         AG_TITLE = 'BOX'                                                 1024
         AG_EXNAME = 'SHAPE'                                              1024
         AG_PARLIST = 'DX_DY_DZ'                                          1024
         CALL AXSHAPE                                                     1024
C    *                                                                    1024
5621  CALL AGSPOPB                                                        1025
      GO TO BLOCK_VPSB, (5620 ,5630)                                      1025
C End of Local Procedure                                                  1025
C ----------------------------------------------------------------------  1026
C ------------ VPBX                                                       1026
C Begin Local Procedure VPBX                                              1027
C VPBX IS THE FEE BOX                                                     1027
5470  CONTINUE                                                            1027
         AG_TITLE='VPBX'                                                  1027
         AG_MODULE=MODULE                                                 1027
         AG_MARK='V00'                                                    1027
         CALL AGSPUSH                                                     1027
         AG_MODULE=MODULE                                                 1027
C    *                                                                    1027
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1028
         CALL AGSRESET                                                    1028
         AG_TITLE = 'ALUMINIUM'                                           1028
         AG_EXNAME = 'MATERIAL'                                           1028
         AG_PARLIST = 'NONE'                                              1028
         CALL AXMATERIAL                                                  1028
C    *                                                                    1028
C    Geant ATTRIBUTE : VPBX ===> SEEN=1 COLO=2 <===                       1028
         CALL AGSRESET                                                    1029
         AG_SEEN = 1                                                      1029
         AG_COLO = 2                                                      1029
         AG_TITLE = 'VPBX'                                                1029
         AG_EXNAME = 'ATTRIBUTE'                                          1029
         AG_PARLIST = 'SEEN_COLO'                                         1029
         CALL AXATTRIBUTE                                                 1029
C    *                                                                    1029
C    Geant SHAPE : BOX ===> DX=VPDG_BXWIDTH/2 DY=VPDG_BXHEIGHT/2, DZ=VPD  1029
C    G_BXLENGTH/2 <===                                                    1029
         CALL AGSRESET                                                    1031
         AG_DX = VPDG_BXWIDTH/2                                           1031
         AG_DY = VPDG_BXHEIGHT/2                                          1031
         AG_DZ = VPDG_BXLENGTH/2                                          1031
         AG_TITLE = 'BOX'                                                 1031
         AG_EXNAME = 'SHAPE'                                              1031
         AG_PARLIST = 'DX_DY_DZ'                                          1031
         CALL AXSHAPE                                                     1031
C    *                                                                    1031
C    Create VPBI :                                                        1032
C    Call To Local Procedure VPBI                                         1032
         ASSIGN 5650 TO BLOCK_VPBI                                        1032
         GO TO  5640                                                      1032
5650     CONTINUE                                                         1032
C    *                                                                    1032
C    Geant POSITION : VPBI ===> inherited arguments used <===             1033
         CALL AGSRESET                                                    1033
         AG_TITLE = 'VPBI'                                                1033
         AG_EXNAME = 'POSITION'                                           1033
         AG_PARLIST = 'NONE'                                              1033
         CALL AXPOSITION                                                  1033
C    *                                                                    1033
5471  CALL AGSPOPB                                                        1034
      GO TO BLOCK_VPBX, (5470 ,5480,5490)                                 1034
C End of Local Procedure                                                  1034
C ----------------------------------------------------------------------  1035
C ------------ VPBI                                                       1035
C Begin Local Procedure VPBI                                              1036
C VPBI IS THE EMPTY SPACE INSIDE OF THE FEE BOX                           1036
5640  CONTINUE                                                            1036
         AG_TITLE='VPBI'                                                  1036
         AG_MODULE=MODULE                                                 1036
         AG_MARK='V00'                                                    1036
         CALL AGSPUSH                                                     1036
         AG_MODULE=MODULE                                                 1036
C    *                                                                    1036
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1037
         CALL AGSRESET                                                    1037
         AG_TITLE = 'AIR'                                                 1037
         AG_EXNAME = 'MATERIAL'                                           1037
         AG_PARLIST = 'NONE'                                              1037
         CALL AXMATERIAL                                                  1037
C    *                                                                    1037
C    Geant ATTRIBUTE : VPBI ===> SEEN=0 <===                              1037
         CALL AGSRESET                                                    1038
         AG_SEEN = 0                                                      1038
         AG_TITLE = 'VPBI'                                                1038
         AG_EXNAME = 'ATTRIBUTE'                                          1038
         AG_PARLIST = 'SEEN'                                              1038
         CALL AXATTRIBUTE                                                 1038
C    *                                                                    1038
C    Geant SHAPE : BOX ===> DX=(VPDG_BXWIDTH-VPDG_BXTHICK)/2, DY=(VPDG_B  1038
C    XHEIGHT-VPDG_BXTHICK)/2 DZ=(VPDG_BXLENGTH-VPDG_BXTHICK)/2 <===       1038
         CALL AGSRESET                                                    1040
         AG_DX = (VPDG_BXWIDTH-VPDG_BXTHICK)/2                            1040
         AG_DY = (VPDG_BXHEIGHT-VPDG_BXTHICK)/2                           1040
         AG_DZ = (VPDG_BXLENGTH-VPDG_BXTHICK)/2                           1040
         AG_TITLE = 'BOX'                                                 1040
         AG_EXNAME = 'SHAPE'                                              1040
         AG_PARLIST = 'DX_DY_DZ'                                          1040
         CALL AXSHAPE                                                     1040
C    *                                                                    1040
C    Create VFEE :                                                        1041
C    Call To Local Procedure VFEE                                         1041
         ASSIGN 5670 TO BLOCK_VFEE                                        1041
         GO TO  5660                                                      1041
5670     CONTINUE                                                         1041
C    *                                                                    1041
C    Geant POSITION : VFEE ===> X=VPDS_VFEEPOSX Y=VPDS_VFEEPOSY Z=VPDS_V  1042
C    FEEPOSZ <===                                                         1042
         CALL AGSRESET                                                    1042
         AG_X = VPDS_VFEEPOSX                                             1042
         AG_Y = VPDS_VFEEPOSY                                             1042
         AG_Z = VPDS_VFEEPOSZ                                             1042
         AG_TITLE = 'VFEE'                                                1042
         AG_EXNAME = 'POSITION'                                           1042
         AG_PARLIST = 'X_Y_Z'                                             1042
         CALL AXPOSITION                                                  1042
C    *                                                                    1042
5641  CALL AGSPOPB                                                        1043
      GO TO BLOCK_VPBI, (5640 ,5650)                                      1043
C End of Local Procedure                                                  1043
C ----------------------------------------------------------------------  1044
C ------------ VFEE                                                       1044
C Begin Local Procedure VFEE                                              1045
C VFEE IS THE FEE INSIDE THE BOX                                          1045
5660  CONTINUE                                                            1045
         AG_TITLE='VFEE'                                                  1045
         AG_MODULE=MODULE                                                 1045
         AG_MARK='V00'                                                    1045
         CALL AGSPUSH                                                     1045
         AG_MODULE=MODULE                                                 1045
C    *                                                                    1045
C    Geant ATTRIBUTE : VFEE ===> SEEN=1 COLO=3 <===                       1046
         CALL AGSRESET                                                    1046
         AG_SEEN = 1                                                      1046
         AG_COLO = 3                                                      1046
         AG_TITLE = 'VFEE'                                                1046
         AG_EXNAME = 'ATTRIBUTE'                                          1046
         AG_PARLIST = 'SEEN_COLO'                                         1046
         CALL AXATTRIBUTE                                                 1046
C    *                                                                    1046
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===          1046
         CALL AGSRESET                                                    1047
         AG_A = 28.08                                                     1047
         AG_Z = 14                                                        1047
         AG_W = 0.6*1*28./60.                                             1047
         AG_TITLE = 'SI'                                                  1047
         AG_EXNAME = 'COMPONENT'                                          1047
         AG_PARLIST = 'A_Z_W'                                             1047
         CALL AXCOMPONENT                                                 1047
C    *                                                                    1047
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174.   1047
C    <===                                                                 1047
         CALL AGSRESET                                                    1048
         AG_A = 16                                                        1048
         AG_Z = 8                                                         1048
         AG_W = 0.6*2*16./60. + 0.4*4*16./174.                            1048
         AG_TITLE = 'O'                                                   1048
         AG_EXNAME = 'COMPONENT'                                          1048
         AG_PARLIST = 'A_Z_W'                                             1048
         CALL AXCOMPONENT                                                 1048
C    *                                                                    1048
C    Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===              1048
         CALL AGSRESET                                                    1049
         AG_A = 12                                                        1049
         AG_Z = 6                                                         1049
         AG_W = 0.4*8*12./174.                                            1049
         AG_TITLE = 'C'                                                   1049
         AG_EXNAME = 'COMPONENT'                                          1049
         AG_PARLIST = 'A_Z_W'                                             1049
         CALL AXCOMPONENT                                                 1049
C    *                                                                    1049
C    Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===               1049
         CALL AGSRESET                                                    1050
         AG_A = 1                                                         1050
         AG_Z = 1                                                         1050
         AG_W = 0.4*14*1./174.                                            1050
         AG_TITLE = 'H'                                                   1050
         AG_EXNAME = 'COMPONENT'                                          1050
         AG_PARLIST = 'A_Z_W'                                             1050
         CALL AXCOMPONENT                                                 1050
C    *                                                                    1050
C    Geant MIXTURE : G10 ===> DENS=1.7 <===                               1050
         CALL AGSRESET                                                    1051
         AG_DENS = 1.7                                                    1051
         AG_TITLE = 'G10'                                                 1051
         AG_EXNAME = 'MIXTURE'                                            1051
         AG_PARLIST = 'DENS'                                              1051
         CALL AXMIXTURE                                                   1051
C    *                                                                    1051
C    Geant SHAPE : BOX ===> DX=VPDS_ELECWID/2 DY=VPDS_ELECTHCK/2, DZ=VPD  1051
C    S_ELECLEN/2 <===                                                     1051
         CALL AGSRESET                                                    1053
         AG_DX = VPDS_ELECWID/2                                           1053
         AG_DY = VPDS_ELECTHCK/2                                          1053
         AG_DZ = VPDS_ELECLEN/2                                           1053
         AG_TITLE = 'BOX'                                                 1053
         AG_EXNAME = 'SHAPE'                                              1053
         AG_PARLIST = 'DX_DY_DZ'                                          1053
         CALL AXSHAPE                                                     1053
C    *                                                                    1053
C    Create VLEM :                                                        1054
C    Call To Local Procedure VLEM                                         1054
         ASSIGN 5690 TO BLOCK_VLEM                                        1054
         GO TO  5680                                                      1054
5690     CONTINUE                                                         1054
C    *                                                                    1054
C    Geant POSITION : VLEM ===> X=VPDS_VLEMPOSX(1) Y=VPDS_VLEMPOSY Z=VPD  1055
C    S_VLEMPOSZ(1) <===                                                   1055
         CALL AGSRESET                                                    1055
         AG_X = VPDS_VLEMPOSX(1)                                          1055
         AG_Y = VPDS_VLEMPOSY                                             1055
         AG_Z = VPDS_VLEMPOSZ(1)                                          1055
         AG_TITLE = 'VLEM'                                                1055
         AG_EXNAME = 'POSITION'                                           1055
         AG_PARLIST = 'X_Y_Z'                                             1055
         CALL AXPOSITION                                                  1055
C    *                                                                    1055
C    Geant POSITION : VLEM ===> X=VPDS_VLEMPOSX(2) Y=VPDS_VLEMPOSY Z=VPD  1055
C    S_VLEMPOSZ(2) <===                                                   1055
         CALL AGSRESET                                                    1056
         AG_X = VPDS_VLEMPOSX(2)                                          1056
         AG_Y = VPDS_VLEMPOSY                                             1056
         AG_Z = VPDS_VLEMPOSZ(2)                                          1056
         AG_TITLE = 'VLEM'                                                1056
         AG_EXNAME = 'POSITION'                                           1056
         AG_PARLIST = 'X_Y_Z'                                             1056
         CALL AXPOSITION                                                  1056
C    *                                                                    1056
C    Geant POSITION : VLEM ===> X=VPDS_VLEMPOSX(3) Y=VPDS_VLEMPOSY Z=VPD  1056
C    S_VLEMPOSZ(3) <===                                                   1056
         CALL AGSRESET                                                    1057
         AG_X = VPDS_VLEMPOSX(3)                                          1057
         AG_Y = VPDS_VLEMPOSY                                             1057
         AG_Z = VPDS_VLEMPOSZ(3)                                          1057
         AG_TITLE = 'VLEM'                                                1057
         AG_EXNAME = 'POSITION'                                           1057
         AG_PARLIST = 'X_Y_Z'                                             1057
         CALL AXPOSITION                                                  1057
C    *                                                                    1057
C    Geant POSITION : VLEM ===> X=VPDS_VLEMPOSX(4) Y=VPDS_VLEMPOSY Z=VPD  1057
C    S_VLEMPOSZ(4) <===                                                   1057
         CALL AGSRESET                                                    1058
         AG_X = VPDS_VLEMPOSX(4)                                          1058
         AG_Y = VPDS_VLEMPOSY                                             1058
         AG_Z = VPDS_VLEMPOSZ(4)                                          1058
         AG_TITLE = 'VLEM'                                                1058
         AG_EXNAME = 'POSITION'                                           1058
         AG_PARLIST = 'X_Y_Z'                                             1058
         CALL AXPOSITION                                                  1058
C    *                                                                    1058
C    Geant POSITION : VLEM ===> X=VPDS_VLEMPOSX(6) Y=VPDS_VLEMPOSY Z=VPD  1058
C    S_VLEMPOSZ(6) ALPHAX=180 <===                                        1058
         CALL AGSRESET                                                    1059
         AG_X = VPDS_VLEMPOSX(6)                                          1059
         AG_Y = VPDS_VLEMPOSY                                             1059
         AG_Z = VPDS_VLEMPOSZ(6)                                          1059
         AG_ALPHAN=AG_ALPHAN+1                                            1059
         AG_ALPHAK(AG_ALPHAN)=1                                           1059
         AG_ALPHAL(AG_ALPHAN)=180                                         1059
         AG_TITLE = 'VLEM'                                                1059
         AG_EXNAME = 'POSITION'                                           1059
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1059
         CALL AXPOSITION                                                  1059
C    *                                                                    1059
C    Geant POSITION : VLEM ===> X=VPDS_VLEMPOSX(7) Y=VPDS_VLEMPOSY Z=VPD  1059
C    S_VLEMPOSZ(7) ALPHAX=180 <===                                        1059
         CALL AGSRESET                                                    1060
         AG_X = VPDS_VLEMPOSX(7)                                          1060
         AG_Y = VPDS_VLEMPOSY                                             1060
         AG_Z = VPDS_VLEMPOSZ(7)                                          1060
         AG_ALPHAN=AG_ALPHAN+1                                            1060
         AG_ALPHAK(AG_ALPHAN)=1                                           1060
         AG_ALPHAL(AG_ALPHAN)=180                                         1060
         AG_TITLE = 'VLEM'                                                1060
         AG_EXNAME = 'POSITION'                                           1060
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1060
         CALL AXPOSITION                                                  1060
C    *                                                                    1060
C    Geant POSITION : VLEM ===> X=VPDS_VLEMPOSX(8) Y=VPDS_VLEMPOSY Z=VPD  1060
C    S_VLEMPOSZ(8) ALPHAX=180 <===                                        1060
         CALL AGSRESET                                                    1061
         AG_X = VPDS_VLEMPOSX(8)                                          1061
         AG_Y = VPDS_VLEMPOSY                                             1061
         AG_Z = VPDS_VLEMPOSZ(8)                                          1061
         AG_ALPHAN=AG_ALPHAN+1                                            1061
         AG_ALPHAK(AG_ALPHAN)=1                                           1061
         AG_ALPHAL(AG_ALPHAN)=180                                         1061
         AG_TITLE = 'VLEM'                                                1061
         AG_EXNAME = 'POSITION'                                           1061
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1061
         CALL AXPOSITION                                                  1061
C    *                                                                    1061
C    Geant POSITION : VLEM ===> X=VPDS_VLEMPOSX(9) Y=VPDS_VLEMPOSY Z=VPD  1061
C    S_VLEMPOSZ(9) ALPHAX=180 <===                                        1061
         CALL AGSRESET                                                    1062
         AG_X = VPDS_VLEMPOSX(9)                                          1062
         AG_Y = VPDS_VLEMPOSY                                             1062
         AG_Z = VPDS_VLEMPOSZ(9)                                          1062
         AG_ALPHAN=AG_ALPHAN+1                                            1062
         AG_ALPHAK(AG_ALPHAN)=1                                           1062
         AG_ALPHAL(AG_ALPHAN)=180                                         1062
         AG_TITLE = 'VLEM'                                                1062
         AG_EXNAME = 'POSITION'                                           1062
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1062
         CALL AXPOSITION                                                  1062
C    *                                                                    1062
C    Geant POSITION : VLEM ===> X=VPDS_VLEMPOSX(11) Y=VPDS_VLEMPOSY Z=VP  1062
C    DS_VLEMPOSZ(11) ALPHAX=180 <===                                      1062
         CALL AGSRESET                                                    1064
         AG_X = VPDS_VLEMPOSX(11)                                         1064
         AG_Y = VPDS_VLEMPOSY                                             1064
         AG_Z = VPDS_VLEMPOSZ(11)                                         1064
         AG_ALPHAN=AG_ALPHAN+1                                            1064
         AG_ALPHAK(AG_ALPHAN)=1                                           1064
         AG_ALPHAL(AG_ALPHAN)=180                                         1064
         AG_TITLE = 'VLEM'                                                1064
         AG_EXNAME = 'POSITION'                                           1064
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1064
         CALL AXPOSITION                                                  1064
C    *                                                                    1064
C    Geant POSITION : VLEM ===> X=VPDS_VLEMPOSX(12) Y=VPDS_VLEMPOSY Z=VP  1064
C    DS_VLEMPOSZ(12) ALPHAX=180 <===                                      1064
         CALL AGSRESET                                                    1066
         AG_X = VPDS_VLEMPOSX(12)                                         1066
         AG_Y = VPDS_VLEMPOSY                                             1066
         AG_Z = VPDS_VLEMPOSZ(12)                                         1066
         AG_ALPHAN=AG_ALPHAN+1                                            1066
         AG_ALPHAK(AG_ALPHAN)=1                                           1066
         AG_ALPHAL(AG_ALPHAN)=180                                         1066
         AG_TITLE = 'VLEM'                                                1066
         AG_EXNAME = 'POSITION'                                           1066
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1066
         CALL AXPOSITION                                                  1066
C    *                                                                    1066
C    Geant POSITION : VLEM ===> X=VPDS_VLEMPOSX(13) Y=VPDS_VLEMPOSY Z=VP  1066
C    DS_VLEMPOSZ(13) ALPHAX=180 <===                                      1066
         CALL AGSRESET                                                    1068
         AG_X = VPDS_VLEMPOSX(13)                                         1068
         AG_Y = VPDS_VLEMPOSY                                             1068
         AG_Z = VPDS_VLEMPOSZ(13)                                         1068
         AG_ALPHAN=AG_ALPHAN+1                                            1068
         AG_ALPHAK(AG_ALPHAN)=1                                           1068
         AG_ALPHAL(AG_ALPHAN)=180                                         1068
         AG_TITLE = 'VLEM'                                                1068
         AG_EXNAME = 'POSITION'                                           1068
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1068
         CALL AXPOSITION                                                  1068
C    *                                                                    1068
C    Geant POSITION : VLEM ===> X=VPDS_VLEMPOSX(14) Y=VPDS_VLEMPOSY Z=VP  1068
C    DS_VLEMPOSZ(14) ALPHAX=180 <===                                      1068
         CALL AGSRESET                                                    1070
         AG_X = VPDS_VLEMPOSX(14)                                         1070
         AG_Y = VPDS_VLEMPOSY                                             1070
         AG_Z = VPDS_VLEMPOSZ(14)                                         1070
         AG_ALPHAN=AG_ALPHAN+1                                            1070
         AG_ALPHAK(AG_ALPHAN)=1                                           1070
         AG_ALPHAL(AG_ALPHAN)=180                                         1070
         AG_TITLE = 'VLEM'                                                1070
         AG_EXNAME = 'POSITION'                                           1070
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1070
         CALL AXPOSITION                                                  1070
C    *                                                                    1070
5661  CALL AGSPOPB                                                        1071
      GO TO BLOCK_VFEE, (5660 ,5670)                                      1071
C End of Local Procedure                                                  1071
C ----------------------------------------------------------------------  1072
C ------------ VLEM                                                       1072
C Begin Local Procedure VLEM                                              1073
C VLEM IS A LEMO CONNECTOR ON THE FEE BOARDS                              1073
5680  CONTINUE                                                            1073
         AG_TITLE='VLEM'                                                  1073
         AG_MODULE=MODULE                                                 1073
         AG_MARK='V00'                                                    1073
         CALL AGSPUSH                                                     1073
         AG_MODULE=MODULE                                                 1073
C    *                                                                    1073
C    Geant ATTRIBUTE : VLEM ===> SEEN=1 COLO=3 <===                       1074
         CALL AGSRESET                                                    1074
         AG_SEEN = 1                                                      1074
         AG_COLO = 3                                                      1074
         AG_TITLE = 'VLEM'                                                1074
         AG_EXNAME = 'ATTRIBUTE'                                          1074
         AG_PARLIST = 'SEEN_COLO'                                         1074
         CALL AXATTRIBUTE                                                 1074
C    *                                                                    1074
C    Geant SHAPE : BOX ===> DX=VPDS_VLEMLENX/2. DY=VPDS_VLEMLENY/2., DZ=  1074
C    VPDS_VLEMLENZ/2. <===                                                1074
         CALL AGSRESET                                                    1076
         AG_DX = VPDS_VLEMLENX/2.                                         1076
         AG_DY = VPDS_VLEMLENY/2.                                         1076
         AG_DZ = VPDS_VLEMLENZ/2.                                         1076
         AG_TITLE = 'BOX'                                                 1076
         AG_EXNAME = 'SHAPE'                                              1076
         AG_PARLIST = 'DX_DY_DZ'                                          1076
         CALL AXSHAPE                                                     1076
C    *                                                                    1076
C    Create VPIP :                                                        1077
C    Call To Local Procedure VPIP                                         1077
         ASSIGN 5710 TO BLOCK_VPIP                                        1077
         GO TO  5700                                                      1077
5710     CONTINUE                                                         1077
C    *                                                                    1077
C    Geant POSITION : VPIP ===> X=VPDS_VPIPPOSX Y=0 Z=VPDS_VPIPPOSZ RMIN  1079
C    =VPDS_VPIPRMIN RMAX=VPDS_VPIPRMAX DZ=VPDS_VPIPLENZ/2 <===            1079
         CALL AGSRESET                                                    1079
         AG_X = VPDS_VPIPPOSX                                             1079
         AG_Y = 0                                                         1079
         AG_Z = VPDS_VPIPPOSZ                                             1079
         AG_RMIN = VPDS_VPIPRMIN                                          1079
         AG_RMAX = VPDS_VPIPRMAX                                          1079
         AG_DZ = VPDS_VPIPLENZ/2                                          1079
         AG_TITLE = 'VPIP'                                                1079
         AG_EXNAME = 'POSITION'                                           1079
         AG_PARLIST = 'X_Y_Z_RMIN_RMAX_DZ'                                1079
         CALL AXPOSITION                                                  1079
C    *                                                                    1079
5681  CALL AGSPOPB                                                        1080
      GO TO BLOCK_VLEM, (5680 ,5690)                                      1080
C End of Local Procedure                                                  1080
C ----------------------------------------------------------------------  1081
C ------------ VPIP                                                       1081
C Begin Local Procedure VPIP                                              1082
C VPIP IS THE LONG PIPE                                                   1082
5700  CONTINUE                                                            1082
         AG_TITLE='VPIP'                                                  1082
         AG_MODULE=MODULE                                                 1082
         AG_MARK='V00'                                                    1082
         CALL AGSPUSH                                                     1082
         AG_MODULE=MODULE                                                 1082
C    *                                                                    1082
C    Geant ATTRIBUTE : VPIP ===> SEEN=1 COLO=7 <===                       1083
         CALL AGSRESET                                                    1083
         AG_SEEN = 1                                                      1083
         AG_COLO = 7                                                      1083
         AG_TITLE = 'VPIP'                                                1083
         AG_EXNAME = 'ATTRIBUTE'                                          1083
         AG_PARLIST = 'SEEN_COLO'                                         1083
         CALL AXATTRIBUTE                                                 1083
C    *                                                                    1083
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1083
         CALL AGSRESET                                                    1084
         AG_TITLE = 'ALUMINIUM'                                           1084
         AG_EXNAME = 'MATERIAL'                                           1084
         AG_PARLIST = 'NONE'                                              1084
         CALL AXMATERIAL                                                  1084
C    *                                                                    1084
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      1084
         CALL AGSRESET                                                    1085
         AG_RMIN = 0                                                      1085
         AG_RMAX = 0                                                      1085
         AG_DZ = 0                                                        1085
         AG_TITLE = 'TUBE'                                                1085
         AG_EXNAME = 'SHAPE'                                              1085
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1085
         CALL AXSHAPE                                                     1085
C    *                                                                    1085
5701  CALL AGSPOPB                                                        1086
      GO TO BLOCK_VPIP, (5700 ,5710)                                      1086
C End of Local Procedure                                                  1086
C ----------------------------------------------------------------------  1087
C ------------ VRNG                                                       1087
C Begin Local Procedure VRNG                                              1088
C VRNG IS A SINGLE PVPD RING OR THE ENTIRE UPVPD                          1088
5500  CONTINUE                                                            1088
         AG_TITLE='VRNG'                                                  1088
         AG_MODULE=MODULE                                                 1088
         AG_MARK='V00'                                                    1088
         CALL AGSPUSH                                                     1088
         AG_MODULE=MODULE                                                 1088
C    *                                                                    1088
C    Check KDETSTYLE<2                                                    1089
         IF (KDETSTYLE.LT.2) THEN                                         1089
C    Geant SHAPE : TUBS ===> RMIN=VPDG_RMIN RMAX=VPDG_RMIN+2*VPDG_DETRAD  1092
C    +VPDG_CLHEIGHT, PHI1=-65.0 PHI2=245.0, DZ=VPDG_CLLENGTH/2 <===       1092
         CALL AGSRESET                                                    1092
         AG_RMIN = VPDG_RMIN                                              1092
         AG_RMAX = VPDG_RMIN+2*VPDG_DETRAD+VPDG_CLHEIGHT                  1092
         AG_PHI1 = -65.0                                                  1092
         AG_PHI2 = 245.0                                                  1092
         AG_DZ = VPDG_CLLENGTH/2                                          1092
         AG_TITLE = 'TUBS'                                                1092
         AG_EXNAME = 'SHAPE'                                              1092
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1092
         CALL AXSHAPE                                                     1092
C    *                                                                    1092
C    *                                                                    1092
         ISEC=1                                                           1093
            GO TO 5723                                                    1093
5721        ISEC=ISEC+1                                                   1093
5723        IF(ISEC-(VPDG_NUMPMT).GT.0) GO TO 5722                        1093
            DETANGLE=-30+ISEC*(360/VPDG_NUMPMT)                           1094
C       Create VSEC :                                                     1095
C       Call To Local Procedure VSEC                                      1095
            ASSIGN 5740 TO BLOCK_VSEC                                     1095
            GO TO  5730                                                   1095
5740        CONTINUE                                                      1095
C       *                                                                 1095
C       Geant POSITION : VSEC ===> ALPHAZ=DETANGLE KONLY='MANY' <===      1096
            CALL AGSRESET                                                 1096
            AG_ALPHAN=AG_ALPHAN+1                                         1096
            AG_ALPHAK(AG_ALPHAN)=3                                        1096
            AG_ALPHAL(AG_ALPHAN)=DETANGLE                                 1096
            AG_KONLY = 'MANY'                                             1096
            AG_TITLE = 'VSEC'                                             1096
            AG_EXNAME = 'POSITION'                                        1096
            AG_PARLIST = 'ALPHAZ_KONLY'                                   1096
            CALL AXPOSITION                                               1096
C       *                                                                 1096
         GO TO 5721                                                       1097
5722     CONTINUE                                                         1097
         ELSEIF (KDETSTYLE.EQ.2) THEN                                     1098
C    Geant SHAPE : TUBS ===> RMIN=VPDH_RMIN RMAX=VPDH_RMAX, PHI1=-65.0 P  1100
C    HI2=245.0 DZ=VPDH_LENGTH/2 <===                                      1100
         CALL AGSRESET                                                    1100
         AG_RMIN = VPDH_RMIN                                              1100
         AG_RMAX = VPDH_RMAX                                              1100
         AG_PHI1 = -65.0                                                  1100
         AG_PHI2 = 245.0                                                  1100
         AG_DZ = VPDH_LENGTH/2                                            1100
         AG_TITLE = 'TUBS'                                                1100
         AG_EXNAME = 'SHAPE'                                              1100
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1100
         CALL AXSHAPE                                                     1100
C    *                                                                    1100
C    Create VDET :                                                        1101
C    Call To Local Procedure VDET                                         1101
         ASSIGN 5760 TO BLOCK_VDET                                        1101
         GO TO  5750                                                      1101
5760     CONTINUE                                                         1101
C    *                                                                    1101
         DETZINT = -(VPDH_DETLEN/                                         1102
     *   2.0)+VPDH_DETFRONT+(VPDH_LEADTHICK+VPDH_SCINTTHICK)              1102
         LOCANG=0.                                                        1103
C    Check VPDH_RING1_KPROJ>0                                             1104
         IF (VPDH_RING1_KPROJ.GT.0) THEN                                  1104
         LOCANG=DEGRAD*0.                                                 1105
         LOCANG=ATAN((VPDH_RING1_RAD)/                                    1106
     *   (0.5*VPDH_ZPOSWEST+0.5*VPDH_ZPOSEAST+DETZINT))                   1106
         ENDIF                                                            1107
C    *                                                                    1108
         ISEC=0                                                           1108
            GO TO 5773                                                    1108
5771        ISEC=ISEC+1                                                   1108
5773        IF(ISEC-(VPDH_RING1_NDET-1).GT.0) GO TO 5772                  1108
            PHIANG = DEGRAD*(VPDH_RING1_PHI0 + ISEC*VPDH_RING1_DPHI)      1109
            XLOC =                                                        1110
     *      (VPDH_RING1_RAD+0.5*VPDH_DETLEN*SIN(LOCANG))*COS(PHIANG)      1110
            YLOC =                                                        1111
     *      (VPDH_RING1_RAD+0.5*VPDH_DETLEN*SIN(LOCANG))*SIN(PHIANG)      1111
C       Geant POSITION : VDET ===> X=XLOC Y=YLOC THETAX=90.+(COS(PHIANG)  1113
C       *LOCANG) THETAY=90.+(SIN(PHIANG)*LOCANG) KONLY='MANY' <===        1113
            CALL AGSRESET                                                 1113
            AG_X = XLOC                                                   1113
            AG_Y = YLOC                                                   1113
            AG_THETAX = 90.+(COS(PHIANG)*LOCANG)                          1113
            AG_THETAY = 90.+(SIN(PHIANG)*LOCANG)                          1113
            AG_KONLY = 'MANY'                                             1113
            AG_TITLE = 'VDET'                                             1113
            AG_EXNAME = 'POSITION'                                        1113
            AG_PARLIST = 'X_Y_THETAX_THETAY_KONLY'                        1113
            CALL AXPOSITION                                               1113
C       *                                                                 1113
         GO TO 5771                                                       1114
5772     CONTINUE                                                         1114
         LOCANG=0.                                                        1115
C    Check VPDH_RING2_KPROJ>0                                             1116
         IF (VPDH_RING2_KPROJ.GT.0) THEN                                  1116
         LOCANG=DEGRAD*4.                                                 1117
         LOCANG=ATAN((VPDH_RING2_RAD)/                                    1118
     *   (0.5*VPDH_ZPOSWEST+0.5*VPDH_ZPOSEAST+DETZINT))                   1118
         ENDIF                                                            1119
C    *                                                                    1120
         ISEC=0                                                           1120
            GO TO 5783                                                    1120
5781        ISEC=ISEC+1                                                   1120
5783        IF(ISEC-(VPDH_RING2_NDET-1).GT.0) GO TO 5782                  1120
            PHIANG = DEGRAD*(VPDH_RING2_PHI0 + ISEC*VPDH_RING2_DPHI)      1121
            XLOC =                                                        1122
     *      (VPDH_RING2_RAD+0.5*VPDH_DETLEN*SIN(LOCANG))*COS(PHIANG)      1122
            YLOC =                                                        1123
     *      (VPDH_RING2_RAD+0.5*VPDH_DETLEN*SIN(LOCANG))*SIN(PHIANG)      1123
C       Geant POSITION : VDET ===> X=XLOC Y=YLOC THETAX=90.+(COS(PHIANG)  1125
C       *LOCANG) THETAY=90.+(SIN(PHIANG)*LOCANG) KONLY='MANY' <===        1125
            CALL AGSRESET                                                 1125
            AG_X = XLOC                                                   1125
            AG_Y = YLOC                                                   1125
            AG_THETAX = 90.+(COS(PHIANG)*LOCANG)                          1125
            AG_THETAY = 90.+(SIN(PHIANG)*LOCANG)                          1125
            AG_KONLY = 'MANY'                                             1125
            AG_TITLE = 'VDET'                                             1125
            AG_EXNAME = 'POSITION'                                        1125
            AG_PARLIST = 'X_Y_THETAX_THETAY_KONLY'                        1125
            CALL AXPOSITION                                               1125
C       *                                                                 1125
         GO TO 5781                                                       1126
5782     CONTINUE                                                         1126
         LOCANG=0.                                                        1127
C    Check VPDH_RING3_KPROJ>0                                             1128
         IF (VPDH_RING3_KPROJ.GT.0) THEN                                  1128
         LOCANG=DEGRAD*8.                                                 1129
         LOCANG=ATAN((VPDH_RING3_RAD)/                                    1130
     *   (0.5*VPDH_ZPOSWEST+0.5*VPDH_ZPOSEAST+DETZINT))                   1130
         ENDIF                                                            1131
C    *                                                                    1132
         ISEC=0                                                           1132
            GO TO 5793                                                    1132
5791        ISEC=ISEC+1                                                   1132
5793        IF(ISEC-(VPDH_RING3_NDET-1).GT.0) GO TO 5792                  1132
            PHIANG = DEGRAD*(VPDH_RING3_PHI0 + ISEC*VPDH_RING3_DPHI)      1133
            XLOC =                                                        1134
     *      (VPDH_RING3_RAD+0.5*VPDH_DETLEN*SIN(LOCANG))*COS(PHIANG)      1134
            YLOC =                                                        1135
     *      (VPDH_RING3_RAD+0.5*VPDH_DETLEN*SIN(LOCANG))*SIN(PHIANG)      1135
C       Geant POSITION : VDET ===> X=XLOC Y=YLOC THETAX=90.+(COS(PHIANG)  1137
C       *LOCANG) THETAY=90.+(SIN(PHIANG)*LOCANG) KONLY='MANY' <===        1137
            CALL AGSRESET                                                 1137
            AG_X = XLOC                                                   1137
            AG_Y = YLOC                                                   1137
            AG_THETAX = 90.+(COS(PHIANG)*LOCANG)                          1137
            AG_THETAY = 90.+(SIN(PHIANG)*LOCANG)                          1137
            AG_KONLY = 'MANY'                                             1137
            AG_TITLE = 'VDET'                                             1137
            AG_EXNAME = 'POSITION'                                        1137
            AG_PARLIST = 'X_Y_THETAX_THETAY_KONLY'                        1137
            CALL AXPOSITION                                               1137
C       *                                                                 1137
         GO TO 5791                                                       1138
5792     CONTINUE                                                         1138
         ENDIF                                                            1139
5501  CALL AGSPOPB                                                        1140
      GO TO BLOCK_VRNG, (5500 ,5510)                                      1140
C End of Local Procedure                                                  1140
C ----------------------------------------------------------------------  1141
C ------------ VSEC                                                       1141
C Begin Local Procedure VSEC                                              1142
C VSEC IS ONE PVPD SECTOR WITH ALL STUFF INSIDE                           1142
5730  CONTINUE                                                            1142
         AG_TITLE='VSEC'                                                  1142
         AG_MODULE=MODULE                                                 1142
         AG_MARK='V00'                                                    1142
         CALL AGSPUSH                                                     1142
         AG_MODULE=MODULE                                                 1142
C    *                                                                    1142
C    Geant ATTRIBUTE : VSEC ===> SEEN=1 COLO=5 SERIAL=ISEC <===           1143
         CALL AGSRESET                                                    1143
         AG_SEEN = 1                                                      1143
         AG_COLO = 5                                                      1143
         AG_SERIAL = ISEC                                                 1143
         AG_TITLE = 'VSEC'                                                1143
         AG_EXNAME = 'ATTRIBUTE'                                          1143
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  1143
         CALL AXATTRIBUTE                                                 1143
C    *                                                                    1143
C    Geant SHAPE : TUBS ===> PHI1=-360/(2*VPDG_NUMPMT) PHI2=+360/(2*VPDG  1143
C    _NUMPMT) <===                                                        1143
         CALL AGSRESET                                                    1144
         AG_PHI1 = -360/(2*VPDG_NUMPMT)                                   1144
         AG_PHI2 = +360/(2*VPDG_NUMPMT)                                   1144
         AG_TITLE = 'TUBS'                                                1144
         AG_EXNAME = 'SHAPE'                                              1144
         AG_PARLIST = 'PHI1_PHI2'                                         1144
         CALL AXSHAPE                                                     1144
C    *                                                                    1144
         YDISPL=VPDG_DETRAD - SQRT(VPDG_DETRAD*VPDG_DETRAD -              1144
     *   (VPDG_CLWIDTH/2 - VPDG_CLTHICK)*(VPDG_CLWIDTH/2 -                1144
     *   VPDG_CLTHICK))                                                   1144
C    Create VDET :                                                        1147
C    Call To Local Procedure VDET                                         1147
         ASSIGN 5800 TO BLOCK_VDET                                        1147
         GO TO  5750                                                      1147
5800     CONTINUE                                                         1147
C    *                                                                    1147
C    Geant POSITION : VDET ===> X=VPDG_RMIN+VPDG_DETRAD <===              1148
         CALL AGSRESET                                                    1148
         AG_X = VPDG_RMIN+VPDG_DETRAD                                     1148
         AG_TITLE = 'VDET'                                                1148
         AG_EXNAME = 'POSITION'                                           1148
         AG_PARLIST = 'X'                                                 1148
         CALL AXPOSITION                                                  1148
C    *                                                                    1148
C    Check ISEC==3                                                        1148
         IF (ISEC.EQ.3) THEN                                              1149
C    Create VPCL :                                                        1150
C    Call To Local Procedure VPCL                                         1150
         ASSIGN 5820 TO BLOCK_VPCL                                        1150
         GO TO  5810                                                      1150
5820     CONTINUE                                                         1150
C    *                                                                    1150
C    Geant POSITION : VPCL ===> X=VPDG_RMIN+VPDG_DETRAD Y=(VPDG_DETRAD+V  1151
C    PDG_CLHEIGHT/2-YDISPL) <===                                          1151
         CALL AGSRESET                                                    1151
         AG_X = VPDG_RMIN+VPDG_DETRAD                                     1151
         AG_Y = (VPDG_DETRAD+VPDG_CLHEIGHT/2-YDISPL)                      1151
         AG_TITLE = 'VPCL'                                                1151
         AG_EXNAME = 'POSITION'                                           1151
         AG_PARLIST = 'X_Y'                                               1151
         CALL AXPOSITION                                                  1151
C    *                                                                    1151
         ELSE                                                             1151
C    Create VPCL :                                                        1153
C    Call To Local Procedure VPCL                                         1153
         ASSIGN 5830 TO BLOCK_VPCL                                        1153
         GO TO  5810                                                      1153
5830     CONTINUE                                                         1153
C    *                                                                    1153
C    Geant POSITION : VPCL ===> X=VPDG_RMIN+VPDG_DETRAD Y=-(VPDG_DETRAD+  1155
C    VPDG_CLHEIGHT/2-YDISPL) ALPHAZ=180 <===                              1155
         CALL AGSRESET                                                    1155
         AG_X = VPDG_RMIN+VPDG_DETRAD                                     1155
         AG_Y = -(VPDG_DETRAD+VPDG_CLHEIGHT/2-YDISPL)                     1155
         AG_ALPHAN=AG_ALPHAN+1                                            1155
         AG_ALPHAK(AG_ALPHAN)=3                                           1155
         AG_ALPHAL(AG_ALPHAN)=180                                         1155
         AG_TITLE = 'VPCL'                                                1155
         AG_EXNAME = 'POSITION'                                           1155
         AG_PARLIST = 'X_Y_ALPHAZ'                                        1155
         CALL AXPOSITION                                                  1155
C    *                                                                    1155
         ENDIF                                                            1155
C    Create VXST :                                                        1157
C    Call To Local Procedure VXST                                         1157
         ASSIGN 5850 TO BLOCK_VXST                                        1157
         GO TO  5840                                                      1157
5850     CONTINUE                                                         1157
C    *                                                                    1157
C    Geant POSITION : VXST ===> X=VPDG_RMIN+VPDG_DETRAD+0.4 Y=+1.5 Z=VPD  1158
C    G_DETLEN/2+1.0 <===                                                  1158
         CALL AGSRESET                                                    1158
         AG_X = VPDG_RMIN+VPDG_DETRAD+0.4                                 1158
         AG_Y = +1.5                                                      1158
         AG_Z = VPDG_DETLEN/2+1.0                                         1158
         AG_TITLE = 'VXST'                                                1158
         AG_EXNAME = 'POSITION'                                           1158
         AG_PARLIST = 'X_Y_Z'                                             1158
         CALL AXPOSITION                                                  1158
C    *                                                                    1158
C    Geant POSITION : VXST ===> X=VPDG_RMIN+VPDG_DETRAD-0.4 Y=-1.5 Z=VPD  1158
C    G_DETLEN/2+1.0 <===                                                  1158
         CALL AGSRESET                                                    1159
         AG_X = VPDG_RMIN+VPDG_DETRAD-0.4                                 1159
         AG_Y = -1.5                                                      1159
         AG_Z = VPDG_DETLEN/2+1.0                                         1159
         AG_TITLE = 'VXST'                                                1159
         AG_EXNAME = 'POSITION'                                           1159
         AG_PARLIST = 'X_Y_Z'                                             1159
         CALL AXPOSITION                                                  1159
C    *                                                                    1159
5731  CALL AGSPOPB                                                        1160
      GO TO BLOCK_VSEC, (5730 ,5740)                                      1160
C End of Local Procedure                                                  1160
C ----------------------------------------------------------------------  1161
C ------------ VDET                                                       1161
C Begin Local Procedure VDET                                              1162
C VDET IS A SINGLE DETECTOR (RADIATOR+CONVERTER AND PMT+ELECTRONCS)       1162
5750  CONTINUE                                                            1162
         AG_TITLE='VDET'                                                  1162
         AG_MODULE=MODULE                                                 1162
         AG_MARK='V00'                                                    1162
         CALL AGSPUSH                                                     1162
         AG_MODULE=MODULE                                                 1162
C    *                                                                    1162
C    Check KDETSTYLE<2                                                    1163
         IF (KDETSTYLE.LT.2) THEN                                         1163
C    Geant MATERIAL : IRON ===> inherited arguments used <===             1164
         CALL AGSRESET                                                    1164
         AG_TITLE = 'IRON'                                                1164
         AG_EXNAME = 'MATERIAL'                                           1164
         AG_PARLIST = 'NONE'                                              1164
         CALL AXMATERIAL                                                  1164
C    *                                                                    1164
C    Geant ATTRIBUTE : VDET ===> SEEN=1 COLO=1 SERIAL=0 <===              1164
         CALL AGSRESET                                                    1165
         AG_SEEN = 1                                                      1165
         AG_COLO = 1                                                      1165
         AG_SERIAL = 0                                                    1165
         AG_TITLE = 'VDET'                                                1165
         AG_EXNAME = 'ATTRIBUTE'                                          1165
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  1165
         CALL AXATTRIBUTE                                                 1165
C    *                                                                    1165
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=VPDG_DETRAD DZ=VPDG_DETLEN/2 <=  1165
C    ==                                                                   1165
         CALL AGSRESET                                                    1166
         AG_RMIN = 0                                                      1166
         AG_RMAX = VPDG_DETRAD                                            1166
         AG_DZ = VPDG_DETLEN/2                                            1166
         AG_TITLE = 'TUBE'                                                1166
         AG_EXNAME = 'SHAPE'                                              1166
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1166
         CALL AXSHAPE                                                     1166
C    *                                                                    1166
C    Create VDTI :                                                        1167
C    Call To Local Procedure VDTI                                         1167
         ASSIGN 5870 TO BLOCK_VDTI                                        1167
         GO TO  5860                                                      1167
5870     CONTINUE                                                         1167
C    *                                                                    1167
C    Geant POSITION : VDTI ===> Z=+VPDG_DETFRONT <===                     1168
         CALL AGSRESET                                                    1168
         AG_Z = +VPDG_DETFRONT                                            1168
         AG_TITLE = 'VDTI'                                                1168
         AG_EXNAME = 'POSITION'                                           1168
         AG_PARLIST = 'Z'                                                 1168
         CALL AXPOSITION                                                  1168
C    *                                                                    1168
         ELSEIF (KDETSTYLE.EQ.2) THEN                                     1168
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1170
         CALL AGSRESET                                                    1170
         AG_TITLE = 'ALUMINIUM'                                           1170
         AG_EXNAME = 'MATERIAL'                                           1170
         AG_PARLIST = 'NONE'                                              1170
         CALL AXMATERIAL                                                  1170
C    *                                                                    1170
C    Geant ATTRIBUTE : VDET ===> SEEN=1 COLO=1 SERIAL=0 <===              1170
         CALL AGSRESET                                                    1171
         AG_SEEN = 1                                                      1171
         AG_COLO = 1                                                      1171
         AG_SERIAL = 0                                                    1171
         AG_TITLE = 'VDET'                                                1171
         AG_EXNAME = 'ATTRIBUTE'                                          1171
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  1171
         CALL AXATTRIBUTE                                                 1171
C    *                                                                    1171
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=VPDH_DETRAD DZ=VPDH_DETLEN/2 <=  1171
C    ==                                                                   1171
         CALL AGSRESET                                                    1172
         AG_RMIN = 0                                                      1172
         AG_RMAX = VPDH_DETRAD                                            1172
         AG_DZ = VPDH_DETLEN/2                                            1172
         AG_TITLE = 'TUBE'                                                1172
         AG_EXNAME = 'SHAPE'                                              1172
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1172
         CALL AXSHAPE                                                     1172
C    *                                                                    1172
C    Create VDTI :                                                        1173
C    Call To Local Procedure VDTI                                         1173
         ASSIGN 5880 TO BLOCK_VDTI                                        1173
         GO TO  5860                                                      1173
5880     CONTINUE                                                         1173
C    *                                                                    1173
C    Geant POSITION : VDTI ===> Z=0 <===                                  1174
         CALL AGSRESET                                                    1174
         AG_Z = 0                                                         1174
         AG_TITLE = 'VDTI'                                                1174
         AG_EXNAME = 'POSITION'                                           1174
         AG_PARLIST = 'Z'                                                 1174
         CALL AXPOSITION                                                  1174
C    *                                                                    1174
         ENDIF                                                            1174
5751  CALL AGSPOPB                                                        1176
      GO TO BLOCK_VDET, (5750 ,5760,5800)                                 1176
C End of Local Procedure                                                  1176
C ----------------------------------------------------------------------  1177
C ------------ VDTI                                                       1177
C Begin Local Procedure VDTI                                              1178
C VDTI IS INNER PART OF THE SINGLE DETECTOR                               1178
5860  CONTINUE                                                            1178
         AG_TITLE='VDTI'                                                  1178
         AG_MODULE=MODULE                                                 1178
         AG_MARK='V00'                                                    1178
         CALL AGSPUSH                                                     1178
         AG_MODULE=MODULE                                                 1178
C    *                                                                    1178
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1179
         CALL AGSRESET                                                    1179
         AG_TITLE = 'AIR'                                                 1179
         AG_EXNAME = 'MATERIAL'                                           1179
         AG_PARLIST = 'NONE'                                              1179
         CALL AXMATERIAL                                                  1179
C    *                                                                    1179
C    Geant ATTRIBUTE : VDTI ===> SEEN=1 COLO=1 <===                       1179
         CALL AGSRESET                                                    1180
         AG_SEEN = 1                                                      1180
         AG_COLO = 1                                                      1180
         AG_TITLE = 'VDTI'                                                1180
         AG_EXNAME = 'ATTRIBUTE'                                          1180
         AG_PARLIST = 'SEEN_COLO'                                         1180
         CALL AXATTRIBUTE                                                 1180
C    *                                                                    1180
C    Check KDETSTYLE<2                                                    1180
         IF (KDETSTYLE.LT.2) THEN                                         1181
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=VPDG_PMTRAD DZ=VPDG_DETLEN/2-VP  1182
C    DG_DETFRONT <===                                                     1182
         CALL AGSRESET                                                    1182
         AG_RMIN = 0                                                      1182
         AG_RMAX = VPDG_PMTRAD                                            1182
         AG_DZ = VPDG_DETLEN/2-VPDG_DETFRONT                              1182
         AG_TITLE = 'TUBE'                                                1182
         AG_EXNAME = 'SHAPE'                                              1182
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1182
         CALL AXSHAPE                                                     1182
C    *                                                                    1182
         CONVLENGTH=VPDG_CONVTHK+VPDG_RADITHK                             1182
C    Create VCNV :                                                        1184
C    Call To Local Procedure VCNV                                         1184
         ASSIGN 5900 TO BLOCK_VCNV                                        1184
         GO TO  5890                                                      1184
5900     CONTINUE                                                         1184
C    *                                                                    1184
C    Geant POSITION : VCNV ===> Z=-(VPDG_DETLEN-CONVLENGTH)/2 <===        1185
         CALL AGSRESET                                                    1185
         AG_Z = -(VPDG_DETLEN-CONVLENGTH)/2                               1185
         AG_TITLE = 'VCNV'                                                1185
         AG_EXNAME = 'POSITION'                                           1185
         AG_PARLIST = 'Z'                                                 1185
         CALL AXPOSITION                                                  1185
C    *                                                                    1185
C    Create VPMT :                                                        1186
C    Call To Local Procedure VPMT                                         1186
         ASSIGN 5920 TO BLOCK_VPMT                                        1186
         GO TO  5910                                                      1186
5920     CONTINUE                                                         1186
C    *                                                                    1186
C    Geant POSITION : VPMT ===> Z=-(VPDG_DETLEN/2-CONVLENGTH-VPDG_PMTLEN  1187
C    /2) <===                                                             1187
         CALL AGSRESET                                                    1187
         AG_Z = -(VPDG_DETLEN/2-CONVLENGTH-VPDG_PMTLEN/2)                 1187
         AG_TITLE = 'VPMT'                                                1187
         AG_EXNAME = 'POSITION'                                           1187
         AG_PARLIST = 'Z'                                                 1187
         CALL AXPOSITION                                                  1187
C    *                                                                    1187
         ELSEIF (KDETSTYLE.EQ.2) THEN                                     1187
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=VPDH_DETRAD-VPDH_DETWALL, DZ=(V  1190
C    PDH_DETLEN/2.)-(2.0*VPDH_DETFRONT) <===                              1190
         CALL AGSRESET                                                    1190
         AG_RMIN = 0                                                      1190
         AG_RMAX = VPDH_DETRAD-VPDH_DETWALL                               1190
         AG_DZ = (VPDH_DETLEN/2.)-(2.0*VPDH_DETFRONT)                     1190
         AG_TITLE = 'TUBE'                                                1190
         AG_EXNAME = 'SHAPE'                                              1190
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1190
         CALL AXSHAPE                                                     1190
C    *                                                                    1190
C    Create VCNV :                                                        1191
C    Call To Local Procedure VCNV                                         1191
         ASSIGN 5930 TO BLOCK_VCNV                                        1191
         GO TO  5890                                                      1191
5930     CONTINUE                                                         1191
C    *                                                                    1191
C    Geant POSITION : VCNV ===> X=0 Z=-(VPDH_DETLEN/2.0)+VPDH_DETFRONT+(  1193
C    VPDH_LEADTHICK+VPDH_SCINTTHICK) <===                                 1193
         CALL AGSRESET                                                    1193
         AG_X = 0                                                         1193
         AG_Z = -(VPDH_DETLEN/                                            1193
     *   2.0)+VPDH_DETFRONT+(VPDH_LEADTHICK+VPDH_SCINTTHICK)              1193
         AG_TITLE = 'VCNV'                                                1193
         AG_EXNAME = 'POSITION'                                           1193
         AG_PARLIST = 'X_Z'                                               1193
         CALL AXPOSITION                                                  1193
C    *                                                                    1193
         ENDIF                                                            1193
5861  CALL AGSPOPB                                                        1195
      GO TO BLOCK_VDTI, (5860 ,5870,5880)                                 1195
C End of Local Procedure                                                  1195
C ----------------------------------------------------------------------  1196
C ------------ VCNV                                                       1196
C Begin Local Procedure VCNV                                              1197
C VCNV IS CONVERTER LAYER (RADIATOR INCLUDED)                             1197
5890  CONTINUE                                                            1197
         AG_TITLE='VCNV'                                                  1197
         AG_MODULE=MODULE                                                 1197
         AG_MARK='V00'                                                    1197
         CALL AGSPUSH                                                     1197
         AG_MODULE=MODULE                                                 1197
C    *                                                                    1197
C    Geant MATERIAL : LEAD ===> inherited arguments used <===             1198
         CALL AGSRESET                                                    1198
         AG_TITLE = 'LEAD'                                                1198
         AG_EXNAME = 'MATERIAL'                                           1198
         AG_PARLIST = 'NONE'                                              1198
         CALL AXMATERIAL                                                  1198
C    *                                                                    1198
C    Geant ATTRIBUTE : VCNV ===> SEEN=1 COLO=2 <===                       1198
         CALL AGSRESET                                                    1199
         AG_SEEN = 1                                                      1199
         AG_COLO = 2                                                      1199
         AG_TITLE = 'VCNV'                                                1199
         AG_EXNAME = 'ATTRIBUTE'                                          1199
         AG_PARLIST = 'SEEN_COLO'                                         1199
         CALL AXATTRIBUTE                                                 1199
C    *                                                                    1199
C    Check KDETSTYLE<2                                                    1199
         IF (KDETSTYLE.LT.2) THEN                                         1200
C    Geant SHAPE : TUBE ===> DZ=VPDG_CONVTHK/2+VPDG_RADITHK/2 <===        1201
         CALL AGSRESET                                                    1201
         AG_DZ = VPDG_CONVTHK/2+VPDG_RADITHK/2                            1201
         AG_TITLE = 'TUBE'                                                1201
         AG_EXNAME = 'SHAPE'                                              1201
         AG_PARLIST = 'DZ'                                                1201
         CALL AXSHAPE                                                     1201
C    *                                                                    1201
C    Create VRAD :                                                        1202
C    Call To Local Procedure VRAD                                         1202
         ASSIGN 5950 TO BLOCK_VRAD                                        1202
         GO TO  5940                                                      1202
5950     CONTINUE                                                         1202
C    *                                                                    1202
C    Geant POSITION : VRAD ===> Z=VPDG_CONVTHK/2 <===                     1203
         CALL AGSRESET                                                    1203
         AG_Z = VPDG_CONVTHK/2                                            1203
         AG_TITLE = 'VRAD'                                                1203
         AG_EXNAME = 'POSITION'                                           1203
         AG_PARLIST = 'Z'                                                 1203
         CALL AXPOSITION                                                  1203
C    *                                                                    1203
         ELSEIF (KDETSTYLE.EQ.2) THEN                                     1203
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=VPDH_PMTRAD DZ=(VPDH_LEADTHICK+  1205
C    VPDH_SCINTTHICK)/2. <===                                             1205
         CALL AGSRESET                                                    1205
         AG_RMIN = 0                                                      1205
         AG_RMAX = VPDH_PMTRAD                                            1205
         AG_DZ = (VPDH_LEADTHICK+VPDH_SCINTTHICK)/2.                      1205
         AG_TITLE = 'TUBE'                                                1205
         AG_EXNAME = 'SHAPE'                                              1205
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1205
         CALL AXSHAPE                                                     1205
C    *                                                                    1205
C    Create VRAD :                                                        1206
C    Call To Local Procedure VRAD                                         1206
         ASSIGN 5960 TO BLOCK_VRAD                                        1206
         GO TO  5940                                                      1206
5960     CONTINUE                                                         1206
C    *                                                                    1206
C    Geant POSITION : VRAD ===> Z=VPDH_LEADTHICK/2. <===                  1207
         CALL AGSRESET                                                    1207
         AG_Z = VPDH_LEADTHICK/2.                                         1207
         AG_TITLE = 'VRAD'                                                1207
         AG_EXNAME = 'POSITION'                                           1207
         AG_PARLIST = 'Z'                                                 1207
         CALL AXPOSITION                                                  1207
C    *                                                                    1207
         ENDIF                                                            1207
5891  CALL AGSPOPB                                                        1209
      GO TO BLOCK_VCNV, (5890 ,5900,5930)                                 1209
C End of Local Procedure                                                  1209
C ----------------------------------------------------------------------  1210
C ------------ VRAD                                                       1210
C Begin Local Procedure VRAD                                              1211
C VRAD IS LIGHT-PRODUCING LAYER (SCINTILLATOR OR QUARTZ)                  1211
5940  CONTINUE                                                            1211
         AG_TITLE='VRAD'                                                  1211
         AG_MODULE=MODULE                                                 1211
         AG_MARK='V00'                                                    1211
         CALL AGSPUSH                                                     1211
         AG_MODULE=MODULE                                                 1211
C    *                                                                    1211
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       1212
         CALL AGSRESET                                                    1212
         AG_TITLE = 'POLYSTYREN'                                          1212
         AG_EXNAME = 'MATERIAL'                                           1212
         AG_PARLIST = 'NONE'                                              1212
         CALL AXMATERIAL                                                  1212
C    *                                                                    1212
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           1212
         CALL AGSRESET                                                    1213
         AG_ISVOL = 1                                                     1213
         AG_TITLE = 'SENSITIVE'                                           1213
         AG_EXNAME = 'MEDIUM'                                             1213
         AG_PARLIST = 'ISVOL'                                             1213
         CALL AXMEDIUM                                                    1213
C    *                                                                    1213
C    Geant ATTRIBUTE : VRAD ===> SEEN=1 COLO=3 <===                       1213
         CALL AGSRESET                                                    1214
         AG_SEEN = 1                                                      1214
         AG_COLO = 3                                                      1214
         AG_TITLE = 'VRAD'                                                1214
         AG_EXNAME = 'ATTRIBUTE'                                          1214
         AG_PARLIST = 'SEEN_COLO'                                         1214
         CALL AXATTRIBUTE                                                 1214
C    *                                                                    1214
C    Check KDETSTYLE<2                                                    1214
         IF (KDETSTYLE.LT.2) THEN                                         1215
C    Geant SHAPE : TUBE ===> DZ=VPDG_RADITHK/2 <===                       1216
         CALL AGSRESET                                                    1216
         AG_DZ = VPDG_RADITHK/2                                           1216
         AG_TITLE = 'TUBE'                                                1216
         AG_EXNAME = 'SHAPE'                                              1216
         AG_PARLIST = 'DZ'                                                1216
         CALL AXSHAPE                                                     1216
C    *                                                                    1216
C    Defining Hits: X:0.1:S Y:0.1: Z:0.1: CX:10: CY:10: CZ:10: STEP:0.1:  1216
C     SLEN:0.1:(0,600), PTOT:16:(0,100) TOF:18:(0,1.0E-6) ELOSS:16:(0,0.  1216
C    01)                                                                  1216
         AG_TITLE = 'VRAD'                                                1218
         AG_EXNAME = 'HITS'                                               1218
         AG_TYPE=AG_MAGIC                                                 1218
         CALL AGSDETV('VRAD')                                             1218
C    *                                                                    1218
         AG_PARLIST='X'                                                   1218
         AG_TITLE = 'DETH'                                                1218
         AG_NBITS = AG_MAGIC                                              1218
         AG_BIN=0.1                                                       1218
         AG_OPTION='S'                                                    1218
         AG_FHMIN = AG_MAGIC                                              1218
         AG_FHMAX = AG_MAGIC                                              1218
         CALL AGSDETH (0)                                                 1218
C    *                                                                    1218
         AG_PARLIST='Y'                                                   1218
         AG_TITLE = 'DETH'                                                1218
         AG_NBITS = AG_MAGIC                                              1218
         AG_BIN=0.1                                                       1218
         AG_FHMIN=AG_MAGIC                                                1218
         AG_FHMAX=AG_MAGIC                                                1218
         AG_OPTION='*'                                                    1218
         CALL AGSDETH (0)                                                 1218
C    *                                                                    1218
         AG_PARLIST='Z'                                                   1218
         AG_TITLE = 'DETH'                                                1218
         AG_NBITS = AG_MAGIC                                              1218
         AG_BIN=0.1                                                       1218
         AG_FHMIN=AG_MAGIC                                                1218
         AG_FHMAX=AG_MAGIC                                                1218
         AG_OPTION='*'                                                    1218
         CALL AGSDETH (0)                                                 1218
C    *                                                                    1218
         AG_PARLIST='CX'                                                  1218
         AG_TITLE = 'DETH'                                                1218
         AG_NBITS = 10                                                    1218
         AG_BIN=AG_MAGIC                                                  1218
         AG_FHMIN=AG_MAGIC                                                1218
         AG_FHMAX=AG_MAGIC                                                1218
         AG_OPTION='*'                                                    1218
         CALL AGSDETH (0)                                                 1218
C    *                                                                    1218
         AG_PARLIST='CY'                                                  1218
         AG_TITLE = 'DETH'                                                1218
         AG_NBITS = 10                                                    1218
         AG_BIN=AG_MAGIC                                                  1218
         AG_FHMIN=AG_MAGIC                                                1218
         AG_FHMAX=AG_MAGIC                                                1218
         AG_OPTION='*'                                                    1218
         CALL AGSDETH (0)                                                 1218
C    *                                                                    1218
         AG_PARLIST='CZ'                                                  1218
         AG_TITLE = 'DETH'                                                1218
         AG_NBITS = 10                                                    1218
         AG_BIN=AG_MAGIC                                                  1218
         AG_FHMIN=AG_MAGIC                                                1218
         AG_FHMAX=AG_MAGIC                                                1218
         AG_OPTION='*'                                                    1218
         CALL AGSDETH (0)                                                 1218
C    *                                                                    1218
         AG_PARLIST='STEP'                                                1218
         AG_TITLE = 'DETH'                                                1218
         AG_NBITS = AG_MAGIC                                              1218
         AG_BIN=0.1                                                       1218
         AG_FHMIN=AG_MAGIC                                                1218
         AG_FHMAX=AG_MAGIC                                                1218
         AG_OPTION='*'                                                    1218
         CALL AGSDETH (0)                                                 1218
C    *                                                                    1218
         AG_PARLIST='SLEN'                                                1218
         AG_TITLE = 'DETH'                                                1218
         AG_NBITS = AG_MAGIC                                              1218
         AG_BIN=0.1                                                       1218
         AG_OPTION='*'                                                    1218
         AG_FHMIN = 0                                                     1218
         AG_FHMAX = 600                                                   1218
         CALL AGSDETH (0)                                                 1218
C    *                                                                    1218
         AG_PARLIST='PTOT'                                                1218
         AG_TITLE = 'DETH'                                                1218
         AG_NBITS = 16                                                    1218
         AG_BIN=AG_MAGIC                                                  1218
         AG_OPTION='*'                                                    1218
         AG_FHMIN = 0                                                     1218
         AG_FHMAX = 100                                                   1218
         CALL AGSDETH (0)                                                 1218
C    *                                                                    1218
         AG_PARLIST='TOF'                                                 1218
         AG_TITLE = 'DETH'                                                1218
         AG_NBITS = 18                                                    1218
         AG_BIN=AG_MAGIC                                                  1218
         AG_OPTION='*'                                                    1218
         AG_FHMIN = 0                                                     1218
         AG_FHMAX = 1.0E-6                                                1218
         CALL AGSDETH (0)                                                 1218
C    *                                                                    1218
         AG_PARLIST='ELOSS'                                               1218
         AG_TITLE = 'DETH'                                                1218
         AG_NBITS = 16                                                    1218
         AG_BIN=AG_MAGIC                                                  1218
         AG_OPTION='*'                                                    1218
         AG_FHMIN = 0                                                     1218
         AG_FHMAX = 0.01                                                  1218
         CALL AGSDETH (0)                                                 1218
C    *                                                                    1218
         CALL AGSDETU('VRAD')                                             1218
C    *                                                                    1218
         ELSEIF (KDETSTYLE.EQ.2) THEN                                     1219
C    Geant SHAPE : TUBE ===> DZ=VPDH_SCINTTHICK/2. <===                   1220
         CALL AGSRESET                                                    1220
         AG_DZ = VPDH_SCINTTHICK/2.                                       1220
         AG_TITLE = 'TUBE'                                                1220
         AG_EXNAME = 'SHAPE'                                              1220
         AG_PARLIST = 'DZ'                                                1220
         CALL AXSHAPE                                                     1220
C    *                                                                    1220
C    Defining Hits: X:0.1:S Y:0.1: Z:0.1: CX:10: CY:10: CZ:10: STEP:0.1:  1220
C     SLEN:0.1:(0,600), PTOT:16:(0,100) TOF:18:(0,1.0E-6) ELOSS:16:(0,0.  1220
C    01)                                                                  1220
         AG_TITLE = 'VRAD'                                                1222
         AG_EXNAME = 'HITS'                                               1222
         AG_TYPE=AG_MAGIC                                                 1222
         CALL AGSDETV('VRAD')                                             1222
C    *                                                                    1222
         AG_PARLIST='X'                                                   1222
         AG_TITLE = 'DETH'                                                1222
         AG_NBITS = AG_MAGIC                                              1222
         AG_BIN=0.1                                                       1222
         AG_OPTION='S'                                                    1222
         AG_FHMIN = AG_MAGIC                                              1222
         AG_FHMAX = AG_MAGIC                                              1222
         CALL AGSDETH (0)                                                 1222
C    *                                                                    1222
         AG_PARLIST='Y'                                                   1222
         AG_TITLE = 'DETH'                                                1222
         AG_NBITS = AG_MAGIC                                              1222
         AG_BIN=0.1                                                       1222
         AG_FHMIN=AG_MAGIC                                                1222
         AG_FHMAX=AG_MAGIC                                                1222
         AG_OPTION='*'                                                    1222
         CALL AGSDETH (0)                                                 1222
C    *                                                                    1222
         AG_PARLIST='Z'                                                   1222
         AG_TITLE = 'DETH'                                                1222
         AG_NBITS = AG_MAGIC                                              1222
         AG_BIN=0.1                                                       1222
         AG_FHMIN=AG_MAGIC                                                1222
         AG_FHMAX=AG_MAGIC                                                1222
         AG_OPTION='*'                                                    1222
         CALL AGSDETH (0)                                                 1222
C    *                                                                    1222
         AG_PARLIST='CX'                                                  1222
         AG_TITLE = 'DETH'                                                1222
         AG_NBITS = 10                                                    1222
         AG_BIN=AG_MAGIC                                                  1222
         AG_FHMIN=AG_MAGIC                                                1222
         AG_FHMAX=AG_MAGIC                                                1222
         AG_OPTION='*'                                                    1222
         CALL AGSDETH (0)                                                 1222
C    *                                                                    1222
         AG_PARLIST='CY'                                                  1222
         AG_TITLE = 'DETH'                                                1222
         AG_NBITS = 10                                                    1222
         AG_BIN=AG_MAGIC                                                  1222
         AG_FHMIN=AG_MAGIC                                                1222
         AG_FHMAX=AG_MAGIC                                                1222
         AG_OPTION='*'                                                    1222
         CALL AGSDETH (0)                                                 1222
C    *                                                                    1222
         AG_PARLIST='CZ'                                                  1222
         AG_TITLE = 'DETH'                                                1222
         AG_NBITS = 10                                                    1222
         AG_BIN=AG_MAGIC                                                  1222
         AG_FHMIN=AG_MAGIC                                                1222
         AG_FHMAX=AG_MAGIC                                                1222
         AG_OPTION='*'                                                    1222
         CALL AGSDETH (0)                                                 1222
C    *                                                                    1222
         AG_PARLIST='STEP'                                                1222
         AG_TITLE = 'DETH'                                                1222
         AG_NBITS = AG_MAGIC                                              1222
         AG_BIN=0.1                                                       1222
         AG_FHMIN=AG_MAGIC                                                1222
         AG_FHMAX=AG_MAGIC                                                1222
         AG_OPTION='*'                                                    1222
         CALL AGSDETH (0)                                                 1222
C    *                                                                    1222
         AG_PARLIST='SLEN'                                                1222
         AG_TITLE = 'DETH'                                                1222
         AG_NBITS = AG_MAGIC                                              1222
         AG_BIN=0.1                                                       1222
         AG_OPTION='*'                                                    1222
         AG_FHMIN = 0                                                     1222
         AG_FHMAX = 600                                                   1222
         CALL AGSDETH (0)                                                 1222
C    *                                                                    1222
         AG_PARLIST='PTOT'                                                1222
         AG_TITLE = 'DETH'                                                1222
         AG_NBITS = 16                                                    1222
         AG_BIN=AG_MAGIC                                                  1222
         AG_OPTION='*'                                                    1222
         AG_FHMIN = 0                                                     1222
         AG_FHMAX = 100                                                   1222
         CALL AGSDETH (0)                                                 1222
C    *                                                                    1222
         AG_PARLIST='TOF'                                                 1222
         AG_TITLE = 'DETH'                                                1222
         AG_NBITS = 18                                                    1222
         AG_BIN=AG_MAGIC                                                  1222
         AG_OPTION='*'                                                    1222
         AG_FHMIN = 0                                                     1222
         AG_FHMAX = 1.0E-6                                                1222
         CALL AGSDETH (0)                                                 1222
C    *                                                                    1222
         AG_PARLIST='ELOSS'                                               1222
         AG_TITLE = 'DETH'                                                1222
         AG_NBITS = 16                                                    1222
         AG_BIN=AG_MAGIC                                                  1222
         AG_OPTION='*'                                                    1222
         AG_FHMIN = 0                                                     1222
         AG_FHMAX = 0.01                                                  1222
         CALL AGSDETH (0)                                                 1222
C    *                                                                    1222
         CALL AGSDETU('VRAD')                                             1222
C    *                                                                    1222
         ENDIF                                                            1223
5941  CALL AGSPOPB                                                        1224
      GO TO BLOCK_VRAD, (5940 ,5950,5960)                                 1224
C End of Local Procedure                                                  1224
C ----------------------------------------------------------------------  1225
C ------------ VPMT                                                       1225
C Begin Local Procedure VPMT                                              1226
C VPMT IS THE PMT INNER VOLUME                                            1226
5910  CONTINUE                                                            1226
         AG_TITLE='VPMT'                                                  1226
         AG_MODULE=MODULE                                                 1226
         AG_MARK='V00'                                                    1226
         CALL AGSPUSH                                                     1226
         AG_MODULE=MODULE                                                 1226
C    *                                                                    1226
C    Geant MATERIAL : VACUUM ===> inherited arguments used <===           1227
         CALL AGSRESET                                                    1227
         AG_TITLE = 'VACUUM'                                              1227
         AG_EXNAME = 'MATERIAL'                                           1227
         AG_PARLIST = 'NONE'                                              1227
         CALL AXMATERIAL                                                  1227
C    *                                                                    1227
C    Geant ATTRIBUTE : VPMT ===> SEEN=1 COLO=7 <===                       1227
         CALL AGSRESET                                                    1228
         AG_SEEN = 1                                                      1228
         AG_COLO = 7                                                      1228
         AG_TITLE = 'VPMT'                                                1228
         AG_EXNAME = 'ATTRIBUTE'                                          1228
         AG_PARLIST = 'SEEN_COLO'                                         1228
         CALL AXATTRIBUTE                                                 1228
C    *                                                                    1228
C    Geant SHAPE : TUBE ===> RMAX=VPDG_PMTRAD-VPDG_PMTWALL, DZ=VPDG_PMTL  1228
C    EN/2-VPDG_PMTWALL <===                                               1228
         CALL AGSRESET                                                    1230
         AG_RMAX = VPDG_PMTRAD-VPDG_PMTWALL                               1230
         AG_DZ = VPDG_PMTLEN/2-VPDG_PMTWALL                               1230
         AG_TITLE = 'TUBE'                                                1230
         AG_EXNAME = 'SHAPE'                                              1230
         AG_PARLIST = 'RMAX_DZ'                                           1230
         CALL AXSHAPE                                                     1230
C    *                                                                    1230
5911  CALL AGSPOPB                                                        1231
      GO TO BLOCK_VPMT, (5910 ,5920)                                      1231
C End of Local Procedure                                                  1231
C ----------------------------------------------------------------------  1232
C ------------ VXST                                                       1232
C Begin Local Procedure VXST                                              1233
C VXST ARE PMT OUTPUT CABLES (JUST TO LOOK NICER)                         1233
5840  CONTINUE                                                            1233
         AG_TITLE='VXST'                                                  1233
         AG_MODULE=MODULE                                                 1233
         AG_MARK='V00'                                                    1233
         CALL AGSPUSH                                                     1233
         AG_MODULE=MODULE                                                 1233
C    *                                                                    1233
C    Geant ATTRIBUTE : VXST ===> SEEN=1 COLO=7 SERIAL=0 <===              1234
         CALL AGSRESET                                                    1234
         AG_SEEN = 1                                                      1234
         AG_COLO = 7                                                      1234
         AG_SERIAL = 0                                                    1234
         AG_TITLE = 'VXST'                                                1234
         AG_EXNAME = 'ATTRIBUTE'                                          1234
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  1234
         CALL AXATTRIBUTE                                                 1234
C    *                                                                    1234
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0.3 DZ=1.0 <===                  1234
         CALL AGSRESET                                                    1235
         AG_RMIN = 0                                                      1235
         AG_RMAX = 0.3                                                    1235
         AG_DZ = 1.0                                                      1235
         AG_TITLE = 'TUBE'                                                1235
         AG_EXNAME = 'SHAPE'                                              1235
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1235
         CALL AXSHAPE                                                     1235
C    *                                                                    1235
5841  CALL AGSPOPB                                                        1236
      GO TO BLOCK_VXST, (5840 ,5850)                                      1236
C End of Local Procedure                                                  1236
C ----------------------------------------------------------------------  1237
C ------------ VPCL                                                       1237
C Begin Local Procedure VPCL                                              1238
C VPCL IS THE BOAT CLAMP                                                  1238
5810  CONTINUE                                                            1238
         AG_TITLE='VPCL'                                                  1238
         AG_MODULE=MODULE                                                 1238
         AG_MARK='V00'                                                    1238
         CALL AGSPUSH                                                     1238
         AG_MODULE=MODULE                                                 1238
C    *                                                                    1238
C    Geant ATTRIBUTE : VPCL ===> SEEN=1 COLO=6 SERIAL=0 <===              1239
         CALL AGSRESET                                                    1239
         AG_SEEN = 1                                                      1239
         AG_COLO = 6                                                      1239
         AG_SERIAL = 0                                                    1239
         AG_TITLE = 'VPCL'                                                1239
         AG_EXNAME = 'ATTRIBUTE'                                          1239
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  1239
         CALL AXATTRIBUTE                                                 1239
C    *                                                                    1239
C    Geant SHAPE : BOX ===> DX=VPDG_CLWIDTH/2 DY=VPDG_CLHEIGHT/2, DZ=VPD  1239
C    G_CLLENGTH/2 <===                                                    1239
         CALL AGSRESET                                                    1241
         AG_DX = VPDG_CLWIDTH/2                                           1241
         AG_DY = VPDG_CLHEIGHT/2                                          1241
         AG_DZ = VPDG_CLLENGTH/2                                          1241
         AG_TITLE = 'BOX'                                                 1241
         AG_EXNAME = 'SHAPE'                                              1241
         AG_PARLIST = 'DX_DY_DZ'                                          1241
         CALL AXSHAPE                                                     1241
C    *                                                                    1241
C    Create VPCF :                                                        1242
C    Call To Local Procedure VPCF                                         1242
         ASSIGN 5980 TO BLOCK_VPCF                                        1242
         GO TO  5970                                                      1242
5980     CONTINUE                                                         1242
C    *                                                                    1242
C    Geant POSITION : VPCF ===> Z=+(VPDG_CLLENGTH-VPDG_CLTHICK)/2 <===    1243
         CALL AGSRESET                                                    1243
         AG_Z = +(VPDG_CLLENGTH-VPDG_CLTHICK)/2                           1243
         AG_TITLE = 'VPCF'                                                1243
         AG_EXNAME = 'POSITION'                                           1243
         AG_PARLIST = 'Z'                                                 1243
         CALL AXPOSITION                                                  1243
C    *                                                                    1243
C    Geant POSITION : VPCF ===> Z=-(VPDG_CLLENGTH-VPDG_CLTHICK)/2 <===    1243
         CALL AGSRESET                                                    1244
         AG_Z = -(VPDG_CLLENGTH-VPDG_CLTHICK)/2                           1244
         AG_TITLE = 'VPCF'                                                1244
         AG_EXNAME = 'POSITION'                                           1244
         AG_PARLIST = 'Z'                                                 1244
         CALL AXPOSITION                                                  1244
C    *                                                                    1244
C    Create VPCH :                                                        1245
C    Call To Local Procedure VPCH                                         1245
         ASSIGN 6000 TO BLOCK_VPCH                                        1245
         GO TO  5990                                                      1245
6000     CONTINUE                                                         1245
C    *                                                                    1245
C    Geant POSITION : VPCH ===> inherited arguments used <===             1246
         CALL AGSRESET                                                    1246
         AG_TITLE = 'VPCH'                                                1246
         AG_EXNAME = 'POSITION'                                           1246
         AG_PARLIST = 'NONE'                                              1246
         CALL AXPOSITION                                                  1246
C    *                                                                    1246
C    Create VPCV :                                                        1247
C    Call To Local Procedure VPCV                                         1247
         ASSIGN 6020 TO BLOCK_VPCV                                        1247
         GO TO  6010                                                      1247
6020     CONTINUE                                                         1247
C    *                                                                    1247
C    Geant POSITION : VPCV ===> X=(VPDG_CLWIDTH-VPDG_CLTHICK)/2 Y=-VPDG_  1249
C    CLTHICK/2-(VPDG_CLHEIGHT/2-VPDG_CLTHICK/2)/2 <===                    1249
         CALL AGSRESET                                                    1249
         AG_X = (VPDG_CLWIDTH-VPDG_CLTHICK)/2                             1249
         AG_Y = -VPDG_CLTHICK/2-(VPDG_CLHEIGHT/2-VPDG_CLTHICK/2)/2        1249
         AG_TITLE = 'VPCV'                                                1249
         AG_EXNAME = 'POSITION'                                           1249
         AG_PARLIST = 'X_Y'                                               1249
         CALL AXPOSITION                                                  1249
C    *                                                                    1249
C    Geant POSITION : VPCV ===> X=-(VPDG_CLWIDTH-VPDG_CLTHICK)/2 Y=-VPDG  1249
C    _CLTHICK/2-(VPDG_CLHEIGHT/2-VPDG_CLTHICK/2)/2 <===                   1249
         CALL AGSRESET                                                    1251
         AG_X = -(VPDG_CLWIDTH-VPDG_CLTHICK)/2                            1251
         AG_Y = -VPDG_CLTHICK/2-(VPDG_CLHEIGHT/2-VPDG_CLTHICK/2)/2        1251
         AG_TITLE = 'VPCV'                                                1251
         AG_EXNAME = 'POSITION'                                           1251
         AG_PARLIST = 'X_Y'                                               1251
         CALL AXPOSITION                                                  1251
C    *                                                                    1251
5811  CALL AGSPOPB                                                        1252
      GO TO BLOCK_VPCL, (5810 ,5820,5830)                                 1252
C End of Local Procedure                                                  1252
C ----------------------------------------------------------------------  1253
C ------------ VPCF                                                       1253
C Begin Local Procedure VPCF                                              1254
C VPCF IS THE FRONT PLATE OF THE BOAT CLAMP                               1254
5970  CONTINUE                                                            1254
         AG_TITLE='VPCF'                                                  1254
         AG_MODULE=MODULE                                                 1254
         AG_MARK='V00'                                                    1254
         CALL AGSPUSH                                                     1254
         AG_MODULE=MODULE                                                 1254
C    *                                                                    1254
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1255
         CALL AGSRESET                                                    1255
         AG_TITLE = 'ALUMINIUM'                                           1255
         AG_EXNAME = 'MATERIAL'                                           1255
         AG_PARLIST = 'NONE'                                              1255
         CALL AXMATERIAL                                                  1255
C    *                                                                    1255
C    Geant ATTRIBUTE : VPCF ===> SEEN=1 COLO=6 FILL=6 <===                1255
         CALL AGSRESET                                                    1256
         AG_SEEN = 1                                                      1256
         AG_COLO = 6                                                      1256
         AG_FILL = 6                                                      1256
         AG_TITLE = 'VPCF'                                                1256
         AG_EXNAME = 'ATTRIBUTE'                                          1256
         AG_PARLIST = 'SEEN_COLO_FILL'                                    1256
         CALL AXATTRIBUTE                                                 1256
C    *                                                                    1256
C    Geant SHAPE : BOX ===> DX=VPDG_CLWIDTH/2 DY=VPDG_CLHEIGHT/2, DZ=VPD  1256
C    G_CLTHICK/2 <===                                                     1256
         CALL AGSRESET                                                    1258
         AG_DX = VPDG_CLWIDTH/2                                           1258
         AG_DY = VPDG_CLHEIGHT/2                                          1258
         AG_DZ = VPDG_CLTHICK/2                                           1258
         AG_TITLE = 'BOX'                                                 1258
         AG_EXNAME = 'SHAPE'                                              1258
         AG_PARLIST = 'DX_DY_DZ'                                          1258
         CALL AXSHAPE                                                     1258
C    *                                                                    1258
5971  CALL AGSPOPB                                                        1259
      GO TO BLOCK_VPCF, (5970 ,5980)                                      1259
C End of Local Procedure                                                  1259
C ----------------------------------------------------------------------  1260
C ------------ VPCH                                                       1260
C Begin Local Procedure VPCH                                              1261
C VPCH IS THE HORIZONTAL PLATE OF THE BOAT CLAMP                          1261
5990  CONTINUE                                                            1261
         AG_TITLE='VPCH'                                                  1261
         AG_MODULE=MODULE                                                 1261
         AG_MARK='V00'                                                    1261
         CALL AGSPUSH                                                     1261
         AG_MODULE=MODULE                                                 1261
C    *                                                                    1261
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1262
         CALL AGSRESET                                                    1262
         AG_TITLE = 'ALUMINIUM'                                           1262
         AG_EXNAME = 'MATERIAL'                                           1262
         AG_PARLIST = 'NONE'                                              1262
         CALL AXMATERIAL                                                  1262
C    *                                                                    1262
C    Geant ATTRIBUTE : VPCH ===> SEEN=1 COLO=6 FILL=6 <===                1262
         CALL AGSRESET                                                    1263
         AG_SEEN = 1                                                      1263
         AG_COLO = 6                                                      1263
         AG_FILL = 6                                                      1263
         AG_TITLE = 'VPCH'                                                1263
         AG_EXNAME = 'ATTRIBUTE'                                          1263
         AG_PARLIST = 'SEEN_COLO_FILL'                                    1263
         CALL AXATTRIBUTE                                                 1263
C    *                                                                    1263
C    Geant SHAPE : BOX ===> DX=VPDG_CLWIDTH/2 DY=VPDG_CLTHICK/2, DZ=(VPD  1263
C    G_CLLENGTH-2*VPDG_CLTHICK)/2 <===                                    1263
         CALL AGSRESET                                                    1265
         AG_DX = VPDG_CLWIDTH/2                                           1265
         AG_DY = VPDG_CLTHICK/2                                           1265
         AG_DZ = (VPDG_CLLENGTH-2*VPDG_CLTHICK)/2                         1265
         AG_TITLE = 'BOX'                                                 1265
         AG_EXNAME = 'SHAPE'                                              1265
         AG_PARLIST = 'DX_DY_DZ'                                          1265
         CALL AXSHAPE                                                     1265
C    *                                                                    1265
5991  CALL AGSPOPB                                                        1266
      GO TO BLOCK_VPCH, (5990 ,6000)                                      1266
C End of Local Procedure                                                  1266
C ----------------------------------------------------------------------  1267
C ------------ VPCV                                                       1267
C Begin Local Procedure VPCV                                              1268
C VPCV IS THE VERTICAL PLATE OF THE BOAT CLAMP                            1268
6010  CONTINUE                                                            1268
         AG_TITLE='VPCV'                                                  1268
         AG_MODULE=MODULE                                                 1268
         AG_MARK='V00'                                                    1268
         CALL AGSPUSH                                                     1268
         AG_MODULE=MODULE                                                 1268
C    *                                                                    1268
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1269
         CALL AGSRESET                                                    1269
         AG_TITLE = 'ALUMINIUM'                                           1269
         AG_EXNAME = 'MATERIAL'                                           1269
         AG_PARLIST = 'NONE'                                              1269
         CALL AXMATERIAL                                                  1269
C    *                                                                    1269
C    Geant ATTRIBUTE : VPCV ===> SEEN=1 COLO=6 FILL=6 <===                1269
         CALL AGSRESET                                                    1270
         AG_SEEN = 1                                                      1270
         AG_COLO = 6                                                      1270
         AG_FILL = 6                                                      1270
         AG_TITLE = 'VPCV'                                                1270
         AG_EXNAME = 'ATTRIBUTE'                                          1270
         AG_PARLIST = 'SEEN_COLO_FILL'                                    1270
         CALL AXATTRIBUTE                                                 1270
C    *                                                                    1270
C    Geant SHAPE : BOX ===> DX=VPDG_CLTHICK/2 DY=(VPDG_CLHEIGHT/2-VPDG_C  1270
C    LTHICK/2)/2, DZ=(VPDG_CLLENGTH-2*VPDG_CLTHICK)/2 <===                1270
         CALL AGSRESET                                                    1272
         AG_DX = VPDG_CLTHICK/2                                           1272
         AG_DY = (VPDG_CLHEIGHT/2-VPDG_CLTHICK/2)/2                       1272
         AG_DZ = (VPDG_CLLENGTH-2*VPDG_CLTHICK)/2                         1272
         AG_TITLE = 'BOX'                                                 1272
         AG_EXNAME = 'SHAPE'                                              1272
         AG_PARLIST = 'DX_DY_DZ'                                          1272
         CALL AXSHAPE                                                     1272
C    *                                                                    1272
6011  CALL AGSPOPB                                                        1273
      GO TO BLOCK_VPCV, (6010 ,6020)                                      1273
C End of Local Procedure                                                  1273
C ----------------------------------------------------------------------  1274
C ------------ IBEM                                                       1274
C Begin Local Procedure IBEM                                              1275
C IBEM IS THE IBEAM STRUCTURE BENEATH THE BELL REDUCER CONE               1275
5030  CONTINUE                                                            1275
         AG_TITLE='IBEM'                                                  1275
         AG_MODULE=MODULE                                                 1275
         AG_MARK='V00'                                                    1275
         CALL AGSPUSH                                                     1275
         AG_MODULE=MODULE                                                 1275
C    *                                                                    1275
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1276
         CALL AGSRESET                                                    1276
         AG_TITLE = 'AIR'                                                 1276
         AG_EXNAME = 'MATERIAL'                                           1276
         AG_PARLIST = 'NONE'                                              1276
         CALL AXMATERIAL                                                  1276
C    *                                                                    1276
C    Geant ATTRIBUTE : IBEM ===> SEEN=0 COLO=2 <===                       1276
         CALL AGSRESET                                                    1277
         AG_SEEN = 0                                                      1277
         AG_COLO = 2                                                      1277
         AG_TITLE = 'IBEM'                                                1277
         AG_EXNAME = 'ATTRIBUTE'                                          1277
         AG_PARLIST = 'SEEN_COLO'                                         1277
         CALL AXATTRIBUTE                                                 1277
C    *                                                                    1277
C    Geant SHAPE : BOX ===> DX=VPDG_IBWIDTH/2 DY=VPDG_IBHEIGHT/2, DZ=VPD  1277
C    G_IBLENG/2 <===                                                      1277
         CALL AGSRESET                                                    1279
         AG_DX = VPDG_IBWIDTH/2                                           1279
         AG_DY = VPDG_IBHEIGHT/2                                          1279
         AG_DZ = VPDG_IBLENG/2                                            1279
         AG_TITLE = 'BOX'                                                 1279
         AG_EXNAME = 'SHAPE'                                              1279
         AG_PARLIST = 'DX_DY_DZ'                                          1279
         CALL AXSHAPE                                                     1279
C    *                                                                    1279
         AG_DY = VPDG_IBTHICKH/2                                          1279
C    Create IBEH :                                                        1280
C    Call To Local Procedure IBEH                                         1280
         ASSIGN 6040 TO BLOCK_IBEH                                        1280
         GO TO  6030                                                      1280
6040     CONTINUE                                                         1280
C    *                                                                    1280
C    Geant POSITION : IBEH ===> Y=+(VPDG_IBHEIGHT-VPDG_IBTHICKH)/2 <===   1281
         CALL AGSRESET                                                    1281
         AG_Y = +(VPDG_IBHEIGHT-VPDG_IBTHICKH)/2                          1281
         AG_TITLE = 'IBEH'                                                1281
         AG_EXNAME = 'POSITION'                                           1281
         AG_PARLIST = 'Y'                                                 1281
         CALL AXPOSITION                                                  1281
C    *                                                                    1281
         AG_DZ = (VPDG_IBLENG-VPDG_IBWLEN)/2                              1281
         AG_DY = VPDG_IBTHICKH/2                                          1282
C    Create IBEH :                                                        1282
C    Call To Local Procedure IBEH                                         1282
         ASSIGN 6050 TO BLOCK_IBEH                                        1282
         GO TO  6030                                                      1282
6050     CONTINUE                                                         1282
C    *                                                                    1282
C    Geant POSITION : IBEH ===> Y=-(VPDG_IBHEIGHT-VPDG_IBTHICKH)/2 Z=+VP  1283
C    DG_IBWLEN/2 <===                                                     1283
         CALL AGSRESET                                                    1283
         AG_Y = -(VPDG_IBHEIGHT-VPDG_IBTHICKH)/2                          1283
         AG_Z = +VPDG_IBWLEN/2                                            1283
         AG_TITLE = 'IBEH'                                                1283
         AG_EXNAME = 'POSITION'                                           1283
         AG_PARLIST = 'Y_Z'                                               1283
         CALL AXPOSITION                                                  1283
C    *                                                                    1283
C    Create IBEV :                                                        1284
C    Call To Local Procedure IBEV                                         1284
         ASSIGN 6070 TO BLOCK_IBEV                                        1284
         GO TO  6060                                                      1284
6070     CONTINUE                                                         1284
C    *                                                                    1284
C    Geant POSITION : IBEV ===> Z=+VPDG_IBWLEN/2 <===                     1285
         CALL AGSRESET                                                    1285
         AG_Z = +VPDG_IBWLEN/2                                            1285
         AG_TITLE = 'IBEV'                                                1285
         AG_EXNAME = 'POSITION'                                           1285
         AG_PARLIST = 'Z'                                                 1285
         CALL AXPOSITION                                                  1285
C    *                                                                    1285
C    Create IBEW :                                                        1286
C    Call To Local Procedure IBEW                                         1286
         ASSIGN 6090 TO BLOCK_IBEW                                        1286
         GO TO  6080                                                      1286
6090     CONTINUE                                                         1286
C    *                                                                    1286
C    Geant POSITION : IBEW ===> Y=(VPDG_IBHEIGHT/2-VPDG_IBTHICKH-(VPDG_I  1288
C    BWHGHTF+VPDG_IBWHGHTB)/4) Z=-(VPDG_IBLENG-VPDG_IBWLEN)/2 ORT=YZX AL  1288
C    PHAZ=180 <===                                                        1288
         CALL AGSRESET                                                    1288
         AG_Y = (VPDG_IBHEIGHT/                                           1288
     *   2-VPDG_IBTHICKH-(VPDG_IBWHGHTF+VPDG_IBWHGHTB)/4)                 1288
         AG_Z = -(VPDG_IBLENG-VPDG_IBWLEN)/2                              1288
         AG_ALPHAN=AG_ALPHAN+1                                            1288
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 1288
         AG_ALPHAN=AG_ALPHAN+1                                            1288
         AG_ALPHAK(AG_ALPHAN)=3                                           1288
         AG_ALPHAL(AG_ALPHAN)=180                                         1288
         AG_TITLE = 'IBEW'                                                1288
         AG_EXNAME = 'POSITION'                                           1288
         AG_PARLIST = 'Y_Z_ORT_ALPHAZ'                                    1288
         CALL AXPOSITION                                                  1288
C    *                                                                    1288
5031  CALL AGSPOPB                                                        1289
      GO TO BLOCK_IBEM, (5030 ,5040)                                      1289
C End of Local Procedure                                                  1289
C ----------------------------------------------------------------------  1290
C ------------ IBEH                                                       1290
C Begin Local Procedure IBEH                                              1291
C IBEH IS A HORIZONTAL IBEAM PLATE                                        1291
6030  CONTINUE                                                            1291
         AG_TITLE='IBEH'                                                  1291
         AG_MODULE=MODULE                                                 1291
         AG_MARK='V00'                                                    1291
         CALL AGSPUSH                                                     1291
         AG_MODULE=MODULE                                                 1291
C    *                                                                    1291
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1292
         CALL AGSRESET                                                    1292
         AG_TITLE = 'ALUMINIUM'                                           1292
         AG_EXNAME = 'MATERIAL'                                           1292
         AG_PARLIST = 'NONE'                                              1292
         CALL AXMATERIAL                                                  1292
C    *                                                                    1292
C    Geant ATTRIBUTE : IBEH ===> SEEN=1 COLO=2 FILL=7 <===                1292
         CALL AGSRESET                                                    1293
         AG_SEEN = 1                                                      1293
         AG_COLO = 2                                                      1293
         AG_FILL = 7                                                      1293
         AG_TITLE = 'IBEH'                                                1293
         AG_EXNAME = 'ATTRIBUTE'                                          1293
         AG_PARLIST = 'SEEN_COLO_FILL'                                    1293
         CALL AXATTRIBUTE                                                 1293
C    *                                                                    1293
C    Geant SHAPE : BOX ===> inherited arguments used <===                 1293
         CALL AGSRESET                                                    1294
         AG_TITLE = 'BOX'                                                 1294
         AG_EXNAME = 'SHAPE'                                              1294
         AG_PARLIST = 'NONE'                                              1294
         CALL AXSHAPE                                                     1294
C    *                                                                    1294
6031  CALL AGSPOPB                                                        1295
      GO TO BLOCK_IBEH, (6030 ,6040,6050)                                 1295
C End of Local Procedure                                                  1295
C ----------------------------------------------------------------------  1296
C ------------ IBEV                                                       1296
C Begin Local Procedure IBEV                                              1297
C IBEV IS A VERTICAL IBEAM PLATE                                          1297
6060  CONTINUE                                                            1297
         AG_TITLE='IBEV'                                                  1297
         AG_MODULE=MODULE                                                 1297
         AG_MARK='V00'                                                    1297
         CALL AGSPUSH                                                     1297
         AG_MODULE=MODULE                                                 1297
C    *                                                                    1297
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1298
         CALL AGSRESET                                                    1298
         AG_TITLE = 'ALUMINIUM'                                           1298
         AG_EXNAME = 'MATERIAL'                                           1298
         AG_PARLIST = 'NONE'                                              1298
         CALL AXMATERIAL                                                  1298
C    *                                                                    1298
C    Geant ATTRIBUTE : IBEV ===> SEEN=1 COLO=2 FILL=7 <===                1298
         CALL AGSRESET                                                    1299
         AG_SEEN = 1                                                      1299
         AG_COLO = 2                                                      1299
         AG_FILL = 7                                                      1299
         AG_TITLE = 'IBEV'                                                1299
         AG_EXNAME = 'ATTRIBUTE'                                          1299
         AG_PARLIST = 'SEEN_COLO_FILL'                                    1299
         CALL AXATTRIBUTE                                                 1299
C    *                                                                    1299
C    Geant SHAPE : BOX ===> DX=VPDG_IBTHICKV/2 DY=(VPDG_IBHEIGHT-2*VPDG_  1299
C    IBTHICKH)/2, DZ=(VPDG_IBLENG-VPDG_IBWLEN)/2 <===                     1299
         CALL AGSRESET                                                    1301
         AG_DX = VPDG_IBTHICKV/2                                          1301
         AG_DY = (VPDG_IBHEIGHT-2*VPDG_IBTHICKH)/2                        1301
         AG_DZ = (VPDG_IBLENG-VPDG_IBWLEN)/2                              1301
         AG_TITLE = 'BOX'                                                 1301
         AG_EXNAME = 'SHAPE'                                              1301
         AG_PARLIST = 'DX_DY_DZ'                                          1301
         CALL AXSHAPE                                                     1301
C    *                                                                    1301
6061  CALL AGSPOPB                                                        1302
      GO TO BLOCK_IBEV, (6060 ,6070)                                      1302
C End of Local Procedure                                                  1302
C ----------------------------------------------------------------------  1303
C ------------ IBEW                                                       1303
C Begin Local Procedure IBEW                                              1304
C IBEW IS THE FIRST PART OF THE IBEAM PLATE                               1304
6080  CONTINUE                                                            1304
         AG_TITLE='IBEW'                                                  1304
         AG_MODULE=MODULE                                                 1304
         AG_MARK='V00'                                                    1304
         CALL AGSPUSH                                                     1304
         AG_MODULE=MODULE                                                 1304
C    *                                                                    1304
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1305
         CALL AGSRESET                                                    1305
         AG_TITLE = 'ALUMINIUM'                                           1305
         AG_EXNAME = 'MATERIAL'                                           1305
         AG_PARLIST = 'NONE'                                              1305
         CALL AXMATERIAL                                                  1305
C    *                                                                    1305
C    Geant ATTRIBUTE : IBEW ===> SEEN=1 COLO=2 FILL=7 <===                1305
         CALL AGSRESET                                                    1306
         AG_SEEN = 1                                                      1306
         AG_COLO = 2                                                      1306
         AG_FILL = 7                                                      1306
         AG_TITLE = 'IBEW'                                                1306
         AG_EXNAME = 'ATTRIBUTE'                                          1306
         AG_PARLIST = 'SEEN_COLO_FILL'                                    1306
         CALL AXATTRIBUTE                                                 1306
C    *                                                                    1306
C    Geant SHAPE : TRAP ===> DZ=VPDG_IBTHICKV/2 THET=0. PHI=0., H1=VPDG_  1306
C    IBWLEN/2 BL1=VPDG_IBWHGHTF/2 TL1=VPDG_IBWHGHTB/2, ALP1=ATAN((VPDG_I  1306
C    BWHGHTB-VPDG_IBWHGHTF)/VPDG_IBWLEN/2)*RADDEG, H2=VPDG_IBWLEN/2, BL2  1306
C    =VPDG_IBWHGHTF/2 TL2=VPDG_IBWHGHTB/2, ALP2=ATAN((VPDG_IBWHGHTB-VPDG  1306
C    _IBWHGHTF)/VPDG_IBWLEN/2)*RADDEG <===                                1306
         CALL AGSRESET                                                    1312
         AG_DZ = VPDG_IBTHICKV/2                                          1312
         AG_THET = 0.                                                     1312
         AG_PHI = 0.                                                      1312
         AG_H1 = VPDG_IBWLEN/2                                            1312
         AG_BL1 = VPDG_IBWHGHTF/2                                         1312
         AG_TL1 = VPDG_IBWHGHTB/2                                         1312
         AG_ALP1 = ATAN((VPDG_IBWHGHTB-VPDG_IBWHGHTF)/VPDG_IBWLEN/        1312
     *   2)*RADDEG                                                        1312
         AG_H2 = VPDG_IBWLEN/2                                            1312
         AG_BL2 = VPDG_IBWHGHTF/2                                         1312
         AG_TL2 = VPDG_IBWHGHTB/2                                         1312
         AG_ALP2 = ATAN((VPDG_IBWHGHTB-VPDG_IBWHGHTF)/VPDG_IBWLEN/        1312
     *   2)*RADDEG                                                        1312
         AG_TITLE = 'TRAP'                                                1312
         AG_EXNAME = 'SHAPE'                                              1312
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       1312
         CALL AXSHAPE                                                     1312
C    *                                                                    1312
6081  CALL AGSPOPB                                                        1313
      GO TO BLOCK_IBEW, (6080 ,6090)                                      1313
C End of Local Procedure                                                  1313
C ----------------------------------------------------------------------  1314
C ------------ IBSA                                                       1314
C Begin Local Procedure IBSA                                              1315
C IBSA IS THE VERTICAL POST ON THE BALCONY (ENVELOPE)                     1315
5050  CONTINUE                                                            1315
         AG_TITLE='IBSA'                                                  1315
         AG_MODULE=MODULE                                                 1315
         AG_MARK='V00'                                                    1315
         CALL AGSPUSH                                                     1315
         AG_MODULE=MODULE                                                 1315
C    *                                                                    1315
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1316
         CALL AGSRESET                                                    1316
         AG_TITLE = 'AIR'                                                 1316
         AG_EXNAME = 'MATERIAL'                                           1316
         AG_PARLIST = 'NONE'                                              1316
         CALL AXMATERIAL                                                  1316
C    *                                                                    1316
C    Geant ATTRIBUTE : IBSA ===> SEEN=0 COLO=5 <===                       1316
         CALL AGSRESET                                                    1317
         AG_SEEN = 0                                                      1317
         AG_COLO = 5                                                      1317
         AG_TITLE = 'IBSA'                                                1317
         AG_EXNAME = 'ATTRIBUTE'                                          1317
         AG_PARLIST = 'SEEN_COLO'                                         1317
         CALL AXATTRIBUTE                                                 1317
C    *                                                                    1317
C    Geant SHAPE : BOX ===> DX=VPDS_BSALENX/2. DY=VPDS_BSALENY/2., DZ=VP  1317
C    DS_BSALENZ/2. <===                                                   1317
         CALL AGSRESET                                                    1319
         AG_DX = VPDS_BSALENX/2.                                          1319
         AG_DY = VPDS_BSALENY/2.                                          1319
         AG_DZ = VPDS_BSALENZ/2.                                          1319
         AG_TITLE = 'BOX'                                                 1319
         AG_EXNAME = 'SHAPE'                                              1319
         AG_PARLIST = 'DX_DY_DZ'                                          1319
         CALL AXSHAPE                                                     1319
C    *                                                                    1319
C    Create IBAA :                                                        1320
C    Call To Local Procedure IBAA                                         1320
         ASSIGN 6110 TO BLOCK_IBAA                                        1320
         GO TO  6100                                                      1320
6110     CONTINUE                                                         1320
C    *                                                                    1320
C    Geant POSITION : IBAA ===> X=0 Z=-(VPDS_BSALENX-VPDS_BAALENZ)/2. <=  1321
C    ==                                                                   1321
         CALL AGSRESET                                                    1321
         AG_X = 0                                                         1321
         AG_Z = -(VPDS_BSALENX-VPDS_BAALENZ)/2.                           1321
         AG_TITLE = 'IBAA'                                                1321
         AG_EXNAME = 'POSITION'                                           1321
         AG_PARLIST = 'X_Z'                                               1321
         CALL AXPOSITION                                                  1321
C    *                                                                    1321
C    Geant POSITION : IBAA ===> X=-(VPDS_BSALENX-VPDS_BAALENZ)/2. Z=0 AL  1321
C    PHAY=90 <===                                                         1321
         CALL AGSRESET                                                    1322
         AG_X = -(VPDS_BSALENX-VPDS_BAALENZ)/2.                           1322
         AG_Z = 0                                                         1322
         AG_ALPHAN=AG_ALPHAN+1                                            1322
         AG_ALPHAK(AG_ALPHAN)=2                                           1322
         AG_ALPHAL(AG_ALPHAN)=90                                          1322
         AG_TITLE = 'IBAA'                                                1322
         AG_EXNAME = 'POSITION'                                           1322
         AG_PARLIST = 'X_Z_ALPHAY'                                        1322
         CALL AXPOSITION                                                  1322
C    *                                                                    1322
5051  CALL AGSPOPB                                                        1323
      GO TO BLOCK_IBSA, (5050 ,5060)                                      1323
C End of Local Procedure                                                  1323
C ----------------------------------------------------------------------  1324
C ------------ IBAA                                                       1324
C Begin Local Procedure IBAA                                              1325
C IBAA IS THE VERTICAL POST ON THE BALCONY (ALUMINUM)                     1325
6100  CONTINUE                                                            1325
         AG_TITLE='IBAA'                                                  1325
         AG_MODULE=MODULE                                                 1325
         AG_MARK='V00'                                                    1325
         CALL AGSPUSH                                                     1325
         AG_MODULE=MODULE                                                 1325
C    *                                                                    1325
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1326
         CALL AGSRESET                                                    1326
         AG_TITLE = 'ALUMINIUM'                                           1326
         AG_EXNAME = 'MATERIAL'                                           1326
         AG_PARLIST = 'NONE'                                              1326
         CALL AXMATERIAL                                                  1326
C    *                                                                    1326
C    Geant ATTRIBUTE : IBAA ===> SEEN=1 COLO=1 FILL=0 <===                1326
         CALL AGSRESET                                                    1327
         AG_SEEN = 1                                                      1327
         AG_COLO = 1                                                      1327
         AG_FILL = 0                                                      1327
         AG_TITLE = 'IBAA'                                                1327
         AG_EXNAME = 'ATTRIBUTE'                                          1327
         AG_PARLIST = 'SEEN_COLO_FILL'                                    1327
         CALL AXATTRIBUTE                                                 1327
C    *                                                                    1327
C    Geant SHAPE : BOX ===> DX=VPDS_BSALENX/2. DY=VPDS_BSALENY/2., DZ=VP  1327
C    DS_BAALENZ/2. <===                                                   1327
         CALL AGSRESET                                                    1329
         AG_DX = VPDS_BSALENX/2.                                          1329
         AG_DY = VPDS_BSALENY/2.                                          1329
         AG_DZ = VPDS_BAALENZ/2.                                          1329
         AG_TITLE = 'BOX'                                                 1329
         AG_EXNAME = 'SHAPE'                                              1329
         AG_PARLIST = 'DX_DY_DZ'                                          1329
         CALL AXSHAPE                                                     1329
C    *                                                                    1329
6101  CALL AGSPOPB                                                        1330
      GO TO BLOCK_IBAA, (6100 ,6110)                                      1330
C End of Local Procedure                                                  1330
C ----------------------------------------------------------------------  1331
C ------------ IBSB                                                       1331
C Begin Local Procedure IBSB                                              1332
C IBSB IS THE DIAGONAL POST FROM THE BALCONY (ENVELOPE)                   1332
5070  CONTINUE                                                            1332
         AG_TITLE='IBSB'                                                  1332
         AG_MODULE=MODULE                                                 1332
         AG_MARK='V00'                                                    1332
         CALL AGSPUSH                                                     1332
         AG_MODULE=MODULE                                                 1332
C    *                                                                    1332
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1333
         CALL AGSRESET                                                    1333
         AG_TITLE = 'AIR'                                                 1333
         AG_EXNAME = 'MATERIAL'                                           1333
         AG_PARLIST = 'NONE'                                              1333
         CALL AXMATERIAL                                                  1333
C    *                                                                    1333
C    Geant ATTRIBUTE : IBSB ===> SEEN=0 COLO=5 <===                       1333
         CALL AGSRESET                                                    1334
         AG_SEEN = 0                                                      1334
         AG_COLO = 5                                                      1334
         AG_TITLE = 'IBSB'                                                1334
         AG_EXNAME = 'ATTRIBUTE'                                          1334
         AG_PARLIST = 'SEEN_COLO'                                         1334
         CALL AXATTRIBUTE                                                 1334
C    *                                                                    1334
C    Geant SHAPE : BOX ===> DX=VPDS_BSALENX/2. DY=VPDS_BSBLENY/2., DZ=VP  1334
C    DS_BSALENZ/2. <===                                                   1334
         CALL AGSRESET                                                    1336
         AG_DX = VPDS_BSALENX/2.                                          1336
         AG_DY = VPDS_BSBLENY/2.                                          1336
         AG_DZ = VPDS_BSALENZ/2.                                          1336
         AG_TITLE = 'BOX'                                                 1336
         AG_EXNAME = 'SHAPE'                                              1336
         AG_PARLIST = 'DX_DY_DZ'                                          1336
         CALL AXSHAPE                                                     1336
C    *                                                                    1336
C    Create IBAB :                                                        1337
C    Call To Local Procedure IBAB                                         1337
         ASSIGN 6130 TO BLOCK_IBAB                                        1337
         GO TO  6120                                                      1337
6130     CONTINUE                                                         1337
C    *                                                                    1337
C    Geant POSITION : IBAB ===> X=0 Z=(VPDS_BSALENX-VPDS_BAALENZ)/2. <==  1338
C    =                                                                    1338
         CALL AGSRESET                                                    1338
         AG_X = 0                                                         1338
         AG_Z = (VPDS_BSALENX-VPDS_BAALENZ)/2.                            1338
         AG_TITLE = 'IBAB'                                                1338
         AG_EXNAME = 'POSITION'                                           1338
         AG_PARLIST = 'X_Z'                                               1338
         CALL AXPOSITION                                                  1338
C    *                                                                    1338
C    Geant POSITION : IBAB ===> X=-(VPDS_BSALENX-VPDS_BAALENZ)/2. Z=0 AL  1338
C    PHAY=90 <===                                                         1338
         CALL AGSRESET                                                    1339
         AG_X = -(VPDS_BSALENX-VPDS_BAALENZ)/2.                           1339
         AG_Z = 0                                                         1339
         AG_ALPHAN=AG_ALPHAN+1                                            1339
         AG_ALPHAK(AG_ALPHAN)=2                                           1339
         AG_ALPHAL(AG_ALPHAN)=90                                          1339
         AG_TITLE = 'IBAB'                                                1339
         AG_EXNAME = 'POSITION'                                           1339
         AG_PARLIST = 'X_Z_ALPHAY'                                        1339
         CALL AXPOSITION                                                  1339
C    *                                                                    1339
5071  CALL AGSPOPB                                                        1340
      GO TO BLOCK_IBSB, (5070 ,5080)                                      1340
C End of Local Procedure                                                  1340
C ----------------------------------------------------------------------  1341
C ------------ IBAB                                                       1341
C Begin Local Procedure IBAB                                              1342
C IBAB IS THE DIAGONAL POST FROM THE BALCONY (ALUMINUM)                   1342
6120  CONTINUE                                                            1342
         AG_TITLE='IBAB'                                                  1342
         AG_MODULE=MODULE                                                 1342
         AG_MARK='V00'                                                    1342
         CALL AGSPUSH                                                     1342
         AG_MODULE=MODULE                                                 1342
C    *                                                                    1342
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1343
         CALL AGSRESET                                                    1343
         AG_TITLE = 'ALUMINIUM'                                           1343
         AG_EXNAME = 'MATERIAL'                                           1343
         AG_PARLIST = 'NONE'                                              1343
         CALL AXMATERIAL                                                  1343
C    *                                                                    1343
C    Geant ATTRIBUTE : IBAB ===> SEEN=1 COLO=1 FILL=0 <===                1343
         CALL AGSRESET                                                    1344
         AG_SEEN = 1                                                      1344
         AG_COLO = 1                                                      1344
         AG_FILL = 0                                                      1344
         AG_TITLE = 'IBAB'                                                1344
         AG_EXNAME = 'ATTRIBUTE'                                          1344
         AG_PARLIST = 'SEEN_COLO_FILL'                                    1344
         CALL AXATTRIBUTE                                                 1344
C    *                                                                    1344
C    Geant SHAPE : BOX ===> DX=VPDS_BSALENX/2. DY=VPDS_BSBLENY/2., DZ=VP  1344
C    DS_BAALENZ/2. <===                                                   1344
         CALL AGSRESET                                                    1346
         AG_DX = VPDS_BSALENX/2.                                          1346
         AG_DY = VPDS_BSBLENY/2.                                          1346
         AG_DZ = VPDS_BAALENZ/2.                                          1346
         AG_TITLE = 'BOX'                                                 1346
         AG_EXNAME = 'SHAPE'                                              1346
         AG_PARLIST = 'DX_DY_DZ'                                          1346
         CALL AXSHAPE                                                     1346
C    *                                                                    1346
6121  CALL AGSPOPB                                                        1347
      GO TO BLOCK_IBAB, (6120 ,6130)                                      1347
C End of Local Procedure                                                  1347
C ----------------------------------------------------------------------  1348
C ------------ IBSC                                                       1348
C Begin Local Procedure IBSC                                              1349
C IBSC IS THE CROSS POST BELOW THE I-BEAM (ENVELOPE)                      1349
5090  CONTINUE                                                            1349
         AG_TITLE='IBSC'                                                  1349
         AG_MODULE=MODULE                                                 1349
         AG_MARK='V00'                                                    1349
         CALL AGSPUSH                                                     1349
         AG_MODULE=MODULE                                                 1349
C    *                                                                    1349
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1350
         CALL AGSRESET                                                    1350
         AG_TITLE = 'AIR'                                                 1350
         AG_EXNAME = 'MATERIAL'                                           1350
         AG_PARLIST = 'NONE'                                              1350
         CALL AXMATERIAL                                                  1350
C    *                                                                    1350
C    Geant ATTRIBUTE : IBSC ===> SEEN=0 COLO=7 <===                       1350
         CALL AGSRESET                                                    1351
         AG_SEEN = 0                                                      1351
         AG_COLO = 7                                                      1351
         AG_TITLE = 'IBSC'                                                1351
         AG_EXNAME = 'ATTRIBUTE'                                          1351
         AG_PARLIST = 'SEEN_COLO'                                         1351
         CALL AXATTRIBUTE                                                 1351
C    *                                                                    1351
C    Geant SHAPE : BOX ===> DX=VPDS_BSCLENX/2. DY=VPDS_BSCLENY/2., DZ=VP  1351
C    DS_BSCLENZ/2. <===                                                   1351
         CALL AGSRESET                                                    1353
         AG_DX = VPDS_BSCLENX/2.                                          1353
         AG_DY = VPDS_BSCLENY/2.                                          1353
         AG_DZ = VPDS_BSCLENZ/2.                                          1353
         AG_TITLE = 'BOX'                                                 1353
         AG_EXNAME = 'SHAPE'                                              1353
         AG_PARLIST = 'DX_DY_DZ'                                          1353
         CALL AXSHAPE                                                     1353
C    *                                                                    1353
C    Create IBAC :                                                        1354
C    Call To Local Procedure IBAC                                         1354
         ASSIGN 6150 TO BLOCK_IBAC                                        1354
         GO TO  6140                                                      1354
6150     CONTINUE                                                         1354
C    *                                                                    1354
C    Geant POSITION : IBAC ===> X=0 Y=0 Z=+(VPDS_BSCLENZ-VPDS_BACLENZ)/2  1355
C    . <===                                                               1355
         CALL AGSRESET                                                    1355
         AG_X = 0                                                         1355
         AG_Y = 0                                                         1355
         AG_Z = +(VPDS_BSCLENZ-VPDS_BACLENZ)/2.                           1355
         AG_TITLE = 'IBAC'                                                1355
         AG_EXNAME = 'POSITION'                                           1355
         AG_PARLIST = 'X_Y_Z'                                             1355
         CALL AXPOSITION                                                  1355
C    *                                                                    1355
C    Geant POSITION : IBAC ===> X=0 Y=0 Z=-(VPDS_BSCLENZ-VPDS_BACLENZ)/2  1355
C    . <===                                                               1355
         CALL AGSRESET                                                    1356
         AG_X = 0                                                         1356
         AG_Y = 0                                                         1356
         AG_Z = -(VPDS_BSCLENZ-VPDS_BACLENZ)/2.                           1356
         AG_TITLE = 'IBAC'                                                1356
         AG_EXNAME = 'POSITION'                                           1356
         AG_PARLIST = 'X_Y_Z'                                             1356
         CALL AXPOSITION                                                  1356
C    *                                                                    1356
C    Create IBBC :                                                        1357
C    Call To Local Procedure IBBC                                         1357
         ASSIGN 6170 TO BLOCK_IBBC                                        1357
         GO TO  6160                                                      1357
6170     CONTINUE                                                         1357
C    *                                                                    1357
C    Geant POSITION : IBBC ===> X=0 Y=(VPDS_BSCLENY-VPDS_BACLENZ)/2. Z=0  1358
C     <===                                                                1358
         CALL AGSRESET                                                    1358
         AG_X = 0                                                         1358
         AG_Y = (VPDS_BSCLENY-VPDS_BACLENZ)/2.                            1358
         AG_Z = 0                                                         1358
         AG_TITLE = 'IBBC'                                                1358
         AG_EXNAME = 'POSITION'                                           1358
         AG_PARLIST = 'X_Y_Z'                                             1358
         CALL AXPOSITION                                                  1358
C    *                                                                    1358
C    Create IBCC :                                                        1359
C    Call To Local Procedure IBCC                                         1359
         ASSIGN 6190 TO BLOCK_IBCC                                        1359
         GO TO  6180                                                      1359
6190     CONTINUE                                                         1359
C    *                                                                    1359
C    Geant POSITION : IBCC ===> X=+(VPDS_BSCLENX-VPDS_BACLENZ)/2. Y=0 Z=  1360
C    0 <===                                                               1360
         CALL AGSRESET                                                    1360
         AG_X = +(VPDS_BSCLENX-VPDS_BACLENZ)/2.                           1360
         AG_Y = 0                                                         1360
         AG_Z = 0                                                         1360
         AG_TITLE = 'IBCC'                                                1360
         AG_EXNAME = 'POSITION'                                           1360
         AG_PARLIST = 'X_Y_Z'                                             1360
         CALL AXPOSITION                                                  1360
C    *                                                                    1360
C    Create IBCC :                                                        1361
C    Call To Local Procedure IBCC                                         1361
         ASSIGN 6200 TO BLOCK_IBCC                                        1361
         GO TO  6180                                                      1361
6200     CONTINUE                                                         1361
C    *                                                                    1361
C    Geant POSITION : IBCC ===> X=-(VPDS_BSCLENX-VPDS_BACLENZ)/2. Y=0 Z=  1362
C    0 <===                                                               1362
         CALL AGSRESET                                                    1362
         AG_X = -(VPDS_BSCLENX-VPDS_BACLENZ)/2.                           1362
         AG_Y = 0                                                         1362
         AG_Z = 0                                                         1362
         AG_TITLE = 'IBCC'                                                1362
         AG_EXNAME = 'POSITION'                                           1362
         AG_PARLIST = 'X_Y_Z'                                             1362
         CALL AXPOSITION                                                  1362
C    *                                                                    1362
5091  CALL AGSPOPB                                                        1363
      GO TO BLOCK_IBSC, (5090 ,5100)                                      1363
C End of Local Procedure                                                  1363
C ----------------------------------------------------------------------  1364
C ------------ IBAC                                                       1364
C Begin Local Procedure IBAC                                              1365
C IBAC IS VERTICAL PARTS OF THE CROSS POST BELOW THE I-BEAM (ALUMINUM)    1365
6140  CONTINUE                                                            1365
         AG_TITLE='IBAC'                                                  1365
         AG_MODULE=MODULE                                                 1365
         AG_MARK='V00'                                                    1365
         CALL AGSPUSH                                                     1365
         AG_MODULE=MODULE                                                 1365
C    *                                                                    1365
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1366
         CALL AGSRESET                                                    1366
         AG_TITLE = 'ALUMINIUM'                                           1366
         AG_EXNAME = 'MATERIAL'                                           1366
         AG_PARLIST = 'NONE'                                              1366
         CALL AXMATERIAL                                                  1366
C    *                                                                    1366
C    Geant ATTRIBUTE : IBAC ===> SEEN=1 COLO=1 FILL=0 <===                1366
         CALL AGSRESET                                                    1367
         AG_SEEN = 1                                                      1367
         AG_COLO = 1                                                      1367
         AG_FILL = 0                                                      1367
         AG_TITLE = 'IBAC'                                                1367
         AG_EXNAME = 'ATTRIBUTE'                                          1367
         AG_PARLIST = 'SEEN_COLO_FILL'                                    1367
         CALL AXATTRIBUTE                                                 1367
C    *                                                                    1367
C    Geant SHAPE : BOX ===> DX=VPDS_BSCLENX/2.-VPDS_BACLENZ, DY=VPDS_BSC  1367
C    LENY/2 DZ=VPDS_BACLENZ/2. <===                                       1367
         CALL AGSRESET                                                    1369
         AG_DX = VPDS_BSCLENX/2.-VPDS_BACLENZ                             1369
         AG_DY = VPDS_BSCLENY/2                                           1369
         AG_DZ = VPDS_BACLENZ/2.                                          1369
         AG_TITLE = 'BOX'                                                 1369
         AG_EXNAME = 'SHAPE'                                              1369
         AG_PARLIST = 'DX_DY_DZ'                                          1369
         CALL AXSHAPE                                                     1369
C    *                                                                    1369
6141  CALL AGSPOPB                                                        1370
      GO TO BLOCK_IBAC, (6140 ,6150)                                      1370
C End of Local Procedure                                                  1370
C ----------------------------------------------------------------------  1371
C ------------ IBBC                                                       1371
C Begin Local Procedure IBBC                                              1372
C IBBC IS THE HORIZONTAL PART OF THE CROSS POST BELOW THE I-BEAM (ALUMIN  1372
C UM)                                                                     1372
6160  CONTINUE                                                            1372
         AG_TITLE='IBBC'                                                  1372
         AG_MODULE=MODULE                                                 1372
         AG_MARK='V00'                                                    1372
         CALL AGSPUSH                                                     1372
         AG_MODULE=MODULE                                                 1372
C    *                                                                    1372
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1373
         CALL AGSRESET                                                    1373
         AG_TITLE = 'ALUMINIUM'                                           1373
         AG_EXNAME = 'MATERIAL'                                           1373
         AG_PARLIST = 'NONE'                                              1373
         CALL AXMATERIAL                                                  1373
C    *                                                                    1373
C    Geant ATTRIBUTE : IBBC ===> SEEN=1 COLO=1 FILL=0 <===                1373
         CALL AGSRESET                                                    1374
         AG_SEEN = 1                                                      1374
         AG_COLO = 1                                                      1374
         AG_FILL = 0                                                      1374
         AG_TITLE = 'IBBC'                                                1374
         AG_EXNAME = 'ATTRIBUTE'                                          1374
         AG_PARLIST = 'SEEN_COLO_FILL'                                    1374
         CALL AXATTRIBUTE                                                 1374
C    *                                                                    1374
C    Geant SHAPE : BOX ===> DX=VPDS_BSCLENX/2.-VPDS_BACLENZ, DY=VPDS_BAC  1374
C    LENZ/2. DZ=VPDS_BSCLENZ/2. <===                                      1374
         CALL AGSRESET                                                    1376
         AG_DX = VPDS_BSCLENX/2.-VPDS_BACLENZ                             1376
         AG_DY = VPDS_BACLENZ/2.                                          1376
         AG_DZ = VPDS_BSCLENZ/2.                                          1376
         AG_TITLE = 'BOX'                                                 1376
         AG_EXNAME = 'SHAPE'                                              1376
         AG_PARLIST = 'DX_DY_DZ'                                          1376
         CALL AXSHAPE                                                     1376
C    *                                                                    1376
6161  CALL AGSPOPB                                                        1377
      GO TO BLOCK_IBBC, (6160 ,6170)                                      1377
C End of Local Procedure                                                  1377
C ----------------------------------------------------------------------  1378
C ------------ IBCC                                                       1378
C Begin Local Procedure IBCC                                              1379
C IBCC IS THE END CAPS ON THE CROSS POST BELOW THE I-BEAM (ALUMINUM)      1379
6180  CONTINUE                                                            1379
         AG_TITLE='IBCC'                                                  1379
         AG_MODULE=MODULE                                                 1379
         AG_MARK='V00'                                                    1379
         CALL AGSPUSH                                                     1379
         AG_MODULE=MODULE                                                 1379
C    *                                                                    1379
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1380
         CALL AGSRESET                                                    1380
         AG_TITLE = 'ALUMINIUM'                                           1380
         AG_EXNAME = 'MATERIAL'                                           1380
         AG_PARLIST = 'NONE'                                              1380
         CALL AXMATERIAL                                                  1380
C    *                                                                    1380
C    Geant ATTRIBUTE : IBCC ===> SEEN=1 COLO=1 FILL=0 <===                1380
         CALL AGSRESET                                                    1381
         AG_SEEN = 1                                                      1381
         AG_COLO = 1                                                      1381
         AG_FILL = 0                                                      1381
         AG_TITLE = 'IBCC'                                                1381
         AG_EXNAME = 'ATTRIBUTE'                                          1381
         AG_PARLIST = 'SEEN_COLO_FILL'                                    1381
         CALL AXATTRIBUTE                                                 1381
C    *                                                                    1381
C    Geant SHAPE : BOX ===> DX=VPDS_BACLENZ/2. DY=VPDS_BSCLENY/2., DZ=VP  1381
C    DS_BSCLENZ/2. <===                                                   1381
         CALL AGSRESET                                                    1383
         AG_DX = VPDS_BACLENZ/2.                                          1383
         AG_DY = VPDS_BSCLENY/2.                                          1383
         AG_DZ = VPDS_BSCLENZ/2.                                          1383
         AG_TITLE = 'BOX'                                                 1383
         AG_EXNAME = 'SHAPE'                                              1383
         AG_PARLIST = 'DX_DY_DZ'                                          1383
         CALL AXSHAPE                                                     1383
C    *                                                                    1383
6181  CALL AGSPOPB                                                        1384
      GO TO BLOCK_IBCC, (6180 ,6190,6200)                                 1384
C End of Local Procedure                                                  1384
C ----------------------------------------------------------------------  1385
C ------------ IBSD                                                       1385
C Begin Local Procedure IBSD                                              1387
C IBSD ARE THE HORIZONTAL PLATES THAT HOLD THE PIPE-SUPPORT BRACKETS, (A  1387
C LUMINUM)                                                                1387
5110  CONTINUE                                                            1387
         AG_TITLE='IBSD'                                                  1387
         AG_MODULE=MODULE                                                 1387
         AG_MARK='V00'                                                    1387
         CALL AGSPUSH                                                     1387
         AG_MODULE=MODULE                                                 1387
C    *                                                                    1387
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1388
         CALL AGSRESET                                                    1388
         AG_TITLE = 'ALUMINIUM'                                           1388
         AG_EXNAME = 'MATERIAL'                                           1388
         AG_PARLIST = 'NONE'                                              1388
         CALL AXMATERIAL                                                  1388
C    *                                                                    1388
C    Geant ATTRIBUTE : IBSD ===> SEEN=1 COLO=1 FILL=0 <===                1388
         CALL AGSRESET                                                    1389
         AG_SEEN = 1                                                      1389
         AG_COLO = 1                                                      1389
         AG_FILL = 0                                                      1389
         AG_TITLE = 'IBSD'                                                1389
         AG_EXNAME = 'ATTRIBUTE'                                          1389
         AG_PARLIST = 'SEEN_COLO_FILL'                                    1389
         CALL AXATTRIBUTE                                                 1389
C    *                                                                    1389
C    Geant SHAPE : BOX ===> DX=VPDS_BSDLENX/2. DY=VPDS_BACLENZ/2, DZ=VPD  1389
C    S_BSALENX/2. <===                                                    1389
         CALL AGSRESET                                                    1391
         AG_DX = VPDS_BSDLENX/2.                                          1391
         AG_DY = VPDS_BACLENZ/2                                           1391
         AG_DZ = VPDS_BSALENX/2.                                          1391
         AG_TITLE = 'BOX'                                                 1391
         AG_EXNAME = 'SHAPE'                                              1391
         AG_PARLIST = 'DX_DY_DZ'                                          1391
         CALL AXSHAPE                                                     1391
C    *                                                                    1391
5111  CALL AGSPOPB                                                        1392
      GO TO BLOCK_IBSD, (5110 ,5120)                                      1392
C End of Local Procedure                                                  1392
C ----------------------------------------------------------------------  1393
C ------------ IBSE                                                       1393
C Begin Local Procedure IBSE                                              1394
C IBSE ARE THE VERTICAL PARTS OF THE PIPE-SUPPORT BRACKETS (ALUMINUM)     1394
5130  CONTINUE                                                            1394
         AG_TITLE='IBSE'                                                  1394
         AG_MODULE=MODULE                                                 1394
         AG_MARK='V00'                                                    1394
         CALL AGSPUSH                                                     1394
         AG_MODULE=MODULE                                                 1394
C    *                                                                    1394
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1395
         CALL AGSRESET                                                    1395
         AG_TITLE = 'ALUMINIUM'                                           1395
         AG_EXNAME = 'MATERIAL'                                           1395
         AG_PARLIST = 'NONE'                                              1395
         CALL AXMATERIAL                                                  1395
C    *                                                                    1395
C    Geant ATTRIBUTE : IBSE ===> SEEN=1 COLO=1 FILL=0 <===                1395
         CALL AGSRESET                                                    1396
         AG_SEEN = 1                                                      1396
         AG_COLO = 1                                                      1396
         AG_FILL = 0                                                      1396
         AG_TITLE = 'IBSE'                                                1396
         AG_EXNAME = 'ATTRIBUTE'                                          1396
         AG_PARLIST = 'SEEN_COLO_FILL'                                    1396
         CALL AXATTRIBUTE                                                 1396
C    *                                                                    1396
C    Geant SHAPE : BOX ===> DX=VPDS_BAALENZ/2. DY=VPDS_BSELENY/2, DZ=VPD  1396
C    S_BSELENZ/2. <===                                                    1396
         CALL AGSRESET                                                    1398
         AG_DX = VPDS_BAALENZ/2.                                          1398
         AG_DY = VPDS_BSELENY/2                                           1398
         AG_DZ = VPDS_BSELENZ/2.                                          1398
         AG_TITLE = 'BOX'                                                 1398
         AG_EXNAME = 'SHAPE'                                              1398
         AG_PARLIST = 'DX_DY_DZ'                                          1398
         CALL AXSHAPE                                                     1398
C    *                                                                    1398
5131  CALL AGSPOPB                                                        1399
      GO TO BLOCK_IBSE, (5130 ,5140)                                      1399
C End of Local Procedure                                                  1399
C ----------------------------------------------------------------------  1400
C ------------ IBSF                                                       1400
C Begin Local Procedure IBSF                                              1401
C IBSF ARE THE LONG THREADED RODS FOR X-SUPPORT OF THE I-BEAM             1401
5150  CONTINUE                                                            1401
         AG_TITLE='IBSF'                                                  1401
         AG_MODULE=MODULE                                                 1401
         AG_MARK='V00'                                                    1401
         CALL AGSPUSH                                                     1401
         AG_MODULE=MODULE                                                 1401
C    *                                                                    1401
C    Geant MATERIAL : IRON ===> inherited arguments used <===             1402
         CALL AGSRESET                                                    1402
         AG_TITLE = 'IRON'                                                1402
         AG_EXNAME = 'MATERIAL'                                           1402
         AG_PARLIST = 'NONE'                                              1402
         CALL AXMATERIAL                                                  1402
C    *                                                                    1402
C    Geant ATTRIBUTE : IBSF ===> SEEN=1 COLO=4 <===                       1402
         CALL AGSRESET                                                    1403
         AG_SEEN = 1                                                      1403
         AG_COLO = 4                                                      1403
         AG_TITLE = 'IBSF'                                                1403
         AG_EXNAME = 'ATTRIBUTE'                                          1403
         AG_PARLIST = 'SEEN_COLO'                                         1403
         CALL AXATTRIBUTE                                                 1403
C    *                                                                    1403
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=VPDS_BSFRMAX DZ=VPDS_BSFLENZ/2.  1403
C     <===                                                                1403
         CALL AGSRESET                                                    1404
         AG_RMIN = 0                                                      1404
         AG_RMAX = VPDS_BSFRMAX                                           1404
         AG_DZ = VPDS_BSFLENZ/2.                                          1404
         AG_TITLE = 'TUBE'                                                1404
         AG_EXNAME = 'SHAPE'                                              1404
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1404
         CALL AXSHAPE                                                     1404
C    *                                                                    1404
5151  CALL AGSPOPB                                                        1405
      GO TO BLOCK_IBSF, (5150 ,5160)                                      1405
C End of Local Procedure                                                  1405
C ----------------------------------------------------------------------  1406
C ------------ IBST                                                       1406
C Begin Local Procedure IBST                                              1408
C IBST ARE THE LONG THREADED RODS FOR X-SUPPORT OF THE I-BEAM, SHORT, ST  1408
C UBS INSIDE IBEM!                                                        1408
5170  CONTINUE                                                            1408
         AG_TITLE='IBST'                                                  1408
         AG_MODULE=MODULE                                                 1408
         AG_MARK='V00'                                                    1408
         CALL AGSPUSH                                                     1408
         AG_MODULE=MODULE                                                 1408
C    *                                                                    1408
C    Geant MATERIAL : IRON ===> inherited arguments used <===             1409
         CALL AGSRESET                                                    1409
         AG_TITLE = 'IRON'                                                1409
         AG_EXNAME = 'MATERIAL'                                           1409
         AG_PARLIST = 'NONE'                                              1409
         CALL AXMATERIAL                                                  1409
C    *                                                                    1409
C    Geant ATTRIBUTE : IBST ===> SEEN=0 COLO=4 <===                       1409
         CALL AGSRESET                                                    1410
         AG_SEEN = 0                                                      1410
         AG_COLO = 4                                                      1410
         AG_TITLE = 'IBST'                                                1410
         AG_EXNAME = 'ATTRIBUTE'                                          1410
         AG_PARLIST = 'SEEN_COLO'                                         1410
         CALL AXATTRIBUTE                                                 1410
C    *                                                                    1410
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=VPDS_BSFRMAX DZ=VPDS_BSSLENZ/2.  1410
C     <===                                                                1410
         CALL AGSRESET                                                    1411
         AG_RMIN = 0                                                      1411
         AG_RMAX = VPDS_BSFRMAX                                           1411
         AG_DZ = VPDS_BSSLENZ/2.                                          1411
         AG_TITLE = 'TUBE'                                                1411
         AG_EXNAME = 'SHAPE'                                              1411
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1411
         CALL AXSHAPE                                                     1411
C    *                                                                    1411
5171  CALL AGSPOPB                                                        1412
      GO TO BLOCK_IBST, (5170 ,5180)                                      1412
C End of Local Procedure                                                  1412
C ----------------------------------------------------------------------  1413
C ------------ IBSG                                                       1413
C Begin Local Procedure IBSG                                              1414
C IBSG ARE THE VERTICAL BOLTS TO THE PIPE-SUPPORT BRACKETS                1414
5190  CONTINUE                                                            1414
         AG_TITLE='IBSG'                                                  1414
         AG_MODULE=MODULE                                                 1414
         AG_MARK='V00'                                                    1414
         CALL AGSPUSH                                                     1414
         AG_MODULE=MODULE                                                 1414
C    *                                                                    1414
C    Geant MATERIAL : IRON ===> inherited arguments used <===             1415
         CALL AGSRESET                                                    1415
         AG_TITLE = 'IRON'                                                1415
         AG_EXNAME = 'MATERIAL'                                           1415
         AG_PARLIST = 'NONE'                                              1415
         CALL AXMATERIAL                                                  1415
C    *                                                                    1415
C    Geant ATTRIBUTE : IBSG ===> SEEN=1 COLO=4 <===                       1415
         CALL AGSRESET                                                    1416
         AG_SEEN = 1                                                      1416
         AG_COLO = 4                                                      1416
         AG_TITLE = 'IBSG'                                                1416
         AG_EXNAME = 'ATTRIBUTE'                                          1416
         AG_PARLIST = 'SEEN_COLO'                                         1416
         CALL AXATTRIBUTE                                                 1416
C    *                                                                    1416
C    Check VPDV_VPDCONFIG<8                                               1416
         IF (VPDV_VPDCONFIG.LT.8) THEN                                    1417
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=VPDS_BSGRMAX DZ=VPDS_BSGLENZ1/2  1418
C    . <===                                                               1418
         CALL AGSRESET                                                    1418
         AG_RMIN = 0                                                      1418
         AG_RMAX = VPDS_BSGRMAX                                           1418
         AG_DZ = VPDS_BSGLENZ1/2.                                         1418
         AG_TITLE = 'TUBE'                                                1418
         AG_EXNAME = 'SHAPE'                                              1418
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1418
         CALL AXSHAPE                                                     1418
C    *                                                                    1418
         ELSEIF (VPDV_VPDCONFIG.EQ.8) THEN                                1418
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=VPDS_BSGRMAX DZ=VPDS_BSGLENZ2/2  1420
C    . <===                                                               1420
         CALL AGSRESET                                                    1420
         AG_RMIN = 0                                                      1420
         AG_RMAX = VPDS_BSGRMAX                                           1420
         AG_DZ = VPDS_BSGLENZ2/2.                                         1420
         AG_TITLE = 'TUBE'                                                1420
         AG_EXNAME = 'SHAPE'                                              1420
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1420
         CALL AXSHAPE                                                     1420
C    *                                                                    1420
         ELSE                                                             1420
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=VPDS_BSGRMAX DZ=VPDS_BSGLENZ3/2  1422
C    . <===                                                               1422
         CALL AGSRESET                                                    1422
         AG_RMIN = 0                                                      1422
         AG_RMAX = VPDS_BSGRMAX                                           1422
         AG_DZ = VPDS_BSGLENZ3/2.                                         1422
         AG_TITLE = 'TUBE'                                                1422
         AG_EXNAME = 'SHAPE'                                              1422
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1422
         CALL AXSHAPE                                                     1422
C    *                                                                    1422
         ENDIF                                                            1422
5191  CALL AGSPOPB                                                        1424
      GO TO BLOCK_IBSG, (5190 ,5200)                                      1424
C End of Local Procedure                                                  1424
C ----------------------------------------------------------------------  1425
C ------------ IBSH                                                       1425
C Begin Local Procedure IBSH                                              1426
C IBSH ARE THE CROSS-BOLTS FROM THE PIPE-SUPPORT BRACKETS TO THE PIPE     1426
5210  CONTINUE                                                            1426
         AG_TITLE='IBSH'                                                  1426
         AG_MODULE=MODULE                                                 1426
         AG_MARK='V00'                                                    1426
         CALL AGSPUSH                                                     1426
         AG_MODULE=MODULE                                                 1426
C    *                                                                    1426
C    Geant MATERIAL : IRON ===> inherited arguments used <===             1427
         CALL AGSRESET                                                    1427
         AG_TITLE = 'IRON'                                                1427
         AG_EXNAME = 'MATERIAL'                                           1427
         AG_PARLIST = 'NONE'                                              1427
         CALL AXMATERIAL                                                  1427
C    *                                                                    1427
C    Geant ATTRIBUTE : IBSH ===> SEEN=1 COLO=4 <===                       1427
         CALL AGSRESET                                                    1428
         AG_SEEN = 1                                                      1428
         AG_COLO = 4                                                      1428
         AG_TITLE = 'IBSH'                                                1428
         AG_EXNAME = 'ATTRIBUTE'                                          1428
         AG_PARLIST = 'SEEN_COLO'                                         1428
         CALL AXATTRIBUTE                                                 1428
C    *                                                                    1428
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=VPDS_BSGRMAX DZ=VPDS_BSHLENZ/2.  1428
C     <===                                                                1428
         CALL AGSRESET                                                    1429
         AG_RMIN = 0                                                      1429
         AG_RMAX = VPDS_BSGRMAX                                           1429
         AG_DZ = VPDS_BSHLENZ/2.                                          1429
         AG_TITLE = 'TUBE'                                                1429
         AG_EXNAME = 'SHAPE'                                              1429
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1429
         CALL AXSHAPE                                                     1429
C    *                                                                    1429
5211  CALL AGSPOPB                                                        1430
      GO TO BLOCK_IBSH, (5210 ,5220)                                      1430
C End of Local Procedure                                                  1430
C ----------------------------------------------------------------------  1431
C ------------ IBWA                                                       1431
C Begin Local Procedure IBWA                                              1432
C IBWA IS THE NEW WEST IBEAM FOR BEAMPIPE SUPPORT                         1432
5230  CONTINUE                                                            1432
         AG_TITLE='IBWA'                                                  1432
         AG_MODULE=MODULE                                                 1432
         AG_MARK='V00'                                                    1432
         CALL AGSPUSH                                                     1432
         AG_MODULE=MODULE                                                 1432
C    *                                                                    1432
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1433
         CALL AGSRESET                                                    1433
         AG_TITLE = 'AIR'                                                 1433
         AG_EXNAME = 'MATERIAL'                                           1433
         AG_PARLIST = 'NONE'                                              1433
         CALL AXMATERIAL                                                  1433
C    *                                                                    1433
C    Geant ATTRIBUTE : IBWA ===> SEEN=1 COLO=2 <===                       1433
         CALL AGSRESET                                                    1434
         AG_SEEN = 1                                                      1434
         AG_COLO = 2                                                      1434
         AG_TITLE = 'IBWA'                                                1434
         AG_EXNAME = 'ATTRIBUTE'                                          1434
         AG_PARLIST = 'SEEN_COLO'                                         1434
         CALL AXATTRIBUTE                                                 1434
C    *                                                                    1434
C    Geant SHAPE : BOX ===> DX=WBSP_IBEAMD(1)/2.0 DY=WBSP_IBEAMD(2)/2.0,  1434
C     DZ=WBSP_IBEAMD(3)/2.0 <===                                          1434
         CALL AGSRESET                                                    1436
         AG_DX = WBSP_IBEAMD(1)/2.0                                       1436
         AG_DY = WBSP_IBEAMD(2)/2.0                                       1436
         AG_DZ = WBSP_IBEAMD(3)/2.0                                       1436
         AG_TITLE = 'BOX'                                                 1436
         AG_EXNAME = 'SHAPE'                                              1436
         AG_PARLIST = 'DX_DY_DZ'                                          1436
         CALL AXSHAPE                                                     1436
C    *                                                                    1436
C    Create IBWB :                                                        1437
C    Call To Local Procedure IBWB                                         1437
         ASSIGN 6220 TO BLOCK_IBWB                                        1437
         GO TO  6210                                                      1437
6220     CONTINUE                                                         1437
C    *                                                                    1437
C    Create IBWC :                                                        1438
C    Call To Local Procedure IBWC                                         1438
         ASSIGN 6240 TO BLOCK_IBWC                                        1438
         GO TO  6230                                                      1438
6240     CONTINUE                                                         1438
C    *                                                                    1438
C    Create IBWD :                                                        1439
C    Call To Local Procedure IBWD                                         1439
         ASSIGN 6250 TO BLOCK_IBWD                                        1439
         GO TO  5250                                                      1439
6250     CONTINUE                                                         1439
C    *                                                                    1439
C    Geant POSITION : IBWB ===> X=0 Y=0 Z=0 <===                          1440
         CALL AGSRESET                                                    1440
         AG_X = 0                                                         1440
         AG_Y = 0                                                         1440
         AG_Z = 0                                                         1440
         AG_TITLE = 'IBWB'                                                1440
         AG_EXNAME = 'POSITION'                                           1440
         AG_PARLIST = 'X_Y_Z'                                             1440
         CALL AXPOSITION                                                  1440
C    *                                                                    1440
C    Geant POSITION : IBWC ===> X=0 Y=-WBSP_IBEAMD(1)/2.0+WBSP_IBEAMD(4)  1440
C    /2.0 Z=0.0 <===                                                      1440
         CALL AGSRESET                                                    1441
         AG_X = 0                                                         1441
         AG_Y = -WBSP_IBEAMD(1)/2.0+WBSP_IBEAMD(4)/2.0                    1441
         AG_Z = 0.0                                                       1441
         AG_TITLE = 'IBWC'                                                1441
         AG_EXNAME = 'POSITION'                                           1441
         AG_PARLIST = 'X_Y_Z'                                             1441
         CALL AXPOSITION                                                  1441
C    *                                                                    1441
C    Geant POSITION : IBWC ===> X=0 Y=+WBSP_IBEAMD(1)/2.0-WBSP_IBEAMD(4)  1441
C    /2.0 Z=0.0 <===                                                      1441
         CALL AGSRESET                                                    1442
         AG_X = 0                                                         1442
         AG_Y = +WBSP_IBEAMD(1)/2.0-WBSP_IBEAMD(4)/2.0                    1442
         AG_Z = 0.0                                                       1442
         AG_TITLE = 'IBWC'                                                1442
         AG_EXNAME = 'POSITION'                                           1442
         AG_PARLIST = 'X_Y_Z'                                             1442
         CALL AXPOSITION                                                  1442
C    *                                                                    1442
5231  CALL AGSPOPB                                                        1443
      GO TO BLOCK_IBWA, (5230 ,5240)                                      1443
C End of Local Procedure                                                  1443
C ----------------------------------------------------------------------  1444
C ------------ IBWB                                                       1444
C Begin Local Procedure IBWB                                              1445
C IBWB IS THE CENTER PIECE FOR NEW WEST IBEAM FOR BEAMPIPE SUPPORT        1445
6210  CONTINUE                                                            1445
         AG_TITLE='IBWB'                                                  1445
         AG_MODULE=MODULE                                                 1445
         AG_MARK='V00'                                                    1445
         CALL AGSPUSH                                                     1445
         AG_MODULE=MODULE                                                 1445
C    *                                                                    1445
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1446
         CALL AGSRESET                                                    1446
         AG_TITLE = 'ALUMINIUM'                                           1446
         AG_EXNAME = 'MATERIAL'                                           1446
         AG_PARLIST = 'NONE'                                              1446
         CALL AXMATERIAL                                                  1446
C    *                                                                    1446
C    Geant ATTRIBUTE : IBWB ===> SEEN=1 COLO=4 <===                       1446
         CALL AGSRESET                                                    1447
         AG_SEEN = 1                                                      1447
         AG_COLO = 4                                                      1447
         AG_TITLE = 'IBWB'                                                1447
         AG_EXNAME = 'ATTRIBUTE'                                          1447
         AG_PARLIST = 'SEEN_COLO'                                         1447
         CALL AXATTRIBUTE                                                 1447
C    *                                                                    1447
C    Geant SHAPE : BOX ===> DX=WBSP_IBEAMD(4)/2.0 DY=WBSP_IBEAMD(2)/2.0-  1447
C    WBSP_IBEAMD(4), DZ=WBSP_IBEAMD(3)/2.0 <===                           1447
         CALL AGSRESET                                                    1449
         AG_DX = WBSP_IBEAMD(4)/2.0                                       1449
         AG_DY = WBSP_IBEAMD(2)/2.0-WBSP_IBEAMD(4)                        1449
         AG_DZ = WBSP_IBEAMD(3)/2.0                                       1449
         AG_TITLE = 'BOX'                                                 1449
         AG_EXNAME = 'SHAPE'                                              1449
         AG_PARLIST = 'DX_DY_DZ'                                          1449
         CALL AXSHAPE                                                     1449
C    *                                                                    1449
6211  CALL AGSPOPB                                                        1450
      GO TO BLOCK_IBWB, (6210 ,6220)                                      1450
C End of Local Procedure                                                  1450
C ----------------------------------------------------------------------  1451
C ------------ IBWC                                                       1451
C Begin Local Procedure IBWC                                              1452
C IBWC IS THE TOP PIECE FOR NEW WEST IBEAM FOR BEAMPIPE SUPPORT           1452
6230  CONTINUE                                                            1452
         AG_TITLE='IBWC'                                                  1452
         AG_MODULE=MODULE                                                 1452
         AG_MARK='V00'                                                    1452
         CALL AGSPUSH                                                     1452
         AG_MODULE=MODULE                                                 1452
C    *                                                                    1452
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1453
         CALL AGSRESET                                                    1453
         AG_TITLE = 'ALUMINIUM'                                           1453
         AG_EXNAME = 'MATERIAL'                                           1453
         AG_PARLIST = 'NONE'                                              1453
         CALL AXMATERIAL                                                  1453
C    *                                                                    1453
C    Geant ATTRIBUTE : IBWC ===> SEEN=1 COLO=4 <===                       1453
         CALL AGSRESET                                                    1454
         AG_SEEN = 1                                                      1454
         AG_COLO = 4                                                      1454
         AG_TITLE = 'IBWC'                                                1454
         AG_EXNAME = 'ATTRIBUTE'                                          1454
         AG_PARLIST = 'SEEN_COLO'                                         1454
         CALL AXATTRIBUTE                                                 1454
C    *                                                                    1454
C    Geant SHAPE : BOX ===> DX=WBSP_IBEAMD(1)/2.0 DY=WBSP_IBEAMD(4)/2.0,  1454
C     DZ=WBSP_IBEAMD(3)/2.0 <===                                          1454
         CALL AGSRESET                                                    1456
         AG_DX = WBSP_IBEAMD(1)/2.0                                       1456
         AG_DY = WBSP_IBEAMD(4)/2.0                                       1456
         AG_DZ = WBSP_IBEAMD(3)/2.0                                       1456
         AG_TITLE = 'BOX'                                                 1456
         AG_EXNAME = 'SHAPE'                                              1456
         AG_PARLIST = 'DX_DY_DZ'                                          1456
         CALL AXSHAPE                                                     1456
C    *                                                                    1456
6231  CALL AGSPOPB                                                        1457
      GO TO BLOCK_IBWC, (6230 ,6240)                                      1457
C End of Local Procedure                                                  1457
C ----------------------------------------------------------------------  1458
C ------------ IBWD                                                       1458
C Begin Local Procedure IBWD                                              1459
C IBWD IS THE POST FOR NEW WEST BEAMPIPE SUPPORT                          1459
5250  CONTINUE                                                            1459
         AG_TITLE='IBWD'                                                  1459
         AG_MODULE=MODULE                                                 1459
         AG_MARK='V00'                                                    1459
         CALL AGSPUSH                                                     1459
         AG_MODULE=MODULE                                                 1459
C    *                                                                    1459
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1460
         CALL AGSRESET                                                    1460
         AG_TITLE = 'ALUMINIUM'                                           1460
         AG_EXNAME = 'MATERIAL'                                           1460
         AG_PARLIST = 'NONE'                                              1460
         CALL AXMATERIAL                                                  1460
C    *                                                                    1460
C    Geant ATTRIBUTE : IBWD ===> SEEN=1 COLO=4 <===                       1460
         CALL AGSRESET                                                    1461
         AG_SEEN = 1                                                      1461
         AG_COLO = 4                                                      1461
         AG_TITLE = 'IBWD'                                                1461
         AG_EXNAME = 'ATTRIBUTE'                                          1461
         AG_PARLIST = 'SEEN_COLO'                                         1461
         CALL AXATTRIBUTE                                                 1461
C    *                                                                    1461
C    Geant SHAPE : BOX ===> DX=WBSP_POSTD(1)/2.0 DY=WBSP_POSTD(2)/2.0, D  1461
C    Z=WBSP_POSTD(3)/2.0 <===                                             1461
         CALL AGSRESET                                                    1463
         AG_DX = WBSP_POSTD(1)/2.0                                        1463
         AG_DY = WBSP_POSTD(2)/2.0                                        1463
         AG_DZ = WBSP_POSTD(3)/2.0                                        1463
         AG_TITLE = 'BOX'                                                 1463
         AG_EXNAME = 'SHAPE'                                              1463
         AG_PARLIST = 'DX_DY_DZ'                                          1463
         CALL AXSHAPE                                                     1463
C    *                                                                    1463
C    Create IBWE :                                                        1464
C    Call To Local Procedure IBWE                                         1464
         ASSIGN 6270 TO BLOCK_IBWE                                        1464
         GO TO  6260                                                      1464
6270     CONTINUE                                                         1464
C    *                                                                    1464
C    Geant POSITION : IBWE ===> X=0 Y=0 Z=0 <===                          1465
         CALL AGSRESET                                                    1465
         AG_X = 0                                                         1465
         AG_Y = 0                                                         1465
         AG_Z = 0                                                         1465
         AG_TITLE = 'IBWE'                                                1465
         AG_EXNAME = 'POSITION'                                           1465
         AG_PARLIST = 'X_Y_Z'                                             1465
         CALL AXPOSITION                                                  1465
C    *                                                                    1465
5251  CALL AGSPOPB                                                        1466
      GO TO BLOCK_IBWD, (5250 ,5260,6250)                                 1466
C End of Local Procedure                                                  1466
C ----------------------------------------------------------------------  1467
C ------------ IBWE                                                       1467
C Begin Local Procedure IBWE                                              1468
C IBWE IS THE AIR IN POST FOR NEW WEST BEAMPIPE SUPPORT                   1468
6260  CONTINUE                                                            1468
         AG_TITLE='IBWE'                                                  1468
         AG_MODULE=MODULE                                                 1468
         AG_MARK='V00'                                                    1468
         CALL AGSPUSH                                                     1468
         AG_MODULE=MODULE                                                 1468
C    *                                                                    1468
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1469
         CALL AGSRESET                                                    1469
         AG_TITLE = 'AIR'                                                 1469
         AG_EXNAME = 'MATERIAL'                                           1469
         AG_PARLIST = 'NONE'                                              1469
         CALL AXMATERIAL                                                  1469
C    *                                                                    1469
C    Geant ATTRIBUTE : IBWE ===> SEEN=1 COLO=4 <===                       1469
         CALL AGSRESET                                                    1470
         AG_SEEN = 1                                                      1470
         AG_COLO = 4                                                      1470
         AG_TITLE = 'IBWE'                                                1470
         AG_EXNAME = 'ATTRIBUTE'                                          1470
         AG_PARLIST = 'SEEN_COLO'                                         1470
         CALL AXATTRIBUTE                                                 1470
C    *                                                                    1470
C    Geant SHAPE : BOX ===> DX=WBSP_POSTD(1)/2.0-WBSP_POSTD(4), DY=WBSP_  1470
C    POSTD(2)/2.0 DZ=WBSP_POSTD(3)/2.0-WBSP_POSTD(4) <===                 1470
         CALL AGSRESET                                                    1472
         AG_DX = WBSP_POSTD(1)/2.0-WBSP_POSTD(4)                          1472
         AG_DY = WBSP_POSTD(2)/2.0                                        1472
         AG_DZ = WBSP_POSTD(3)/2.0-WBSP_POSTD(4)                          1472
         AG_TITLE = 'BOX'                                                 1472
         AG_EXNAME = 'SHAPE'                                              1472
         AG_PARLIST = 'DX_DY_DZ'                                          1472
         CALL AXSHAPE                                                     1472
C    *                                                                    1472
6261  CALL AGSPOPB                                                        1473
      GO TO BLOCK_IBWE, (6260 ,6270)                                      1473
C End of Local Procedure                                                  1473
C ----------------------------------------------------------------------  1474
C ------------ IBWF                                                       1474
C Begin Local Procedure IBWF                                              1475
C IBWF IS THE ANGLED-POST FOR NEW WEST BEAMPIPE SUPPORT                   1475
5270  CONTINUE                                                            1475
         AG_TITLE='IBWF'                                                  1475
         AG_MODULE=MODULE                                                 1475
         AG_MARK='V00'                                                    1475
         CALL AGSPUSH                                                     1475
         AG_MODULE=MODULE                                                 1475
C    *                                                                    1475
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1476
         CALL AGSRESET                                                    1476
         AG_TITLE = 'ALUMINIUM'                                           1476
         AG_EXNAME = 'MATERIAL'                                           1476
         AG_PARLIST = 'NONE'                                              1476
         CALL AXMATERIAL                                                  1476
C    *                                                                    1476
C    Geant ATTRIBUTE : IBWF ===> SEEN=1 COLO=4 <===                       1476
         CALL AGSRESET                                                    1477
         AG_SEEN = 1                                                      1477
         AG_COLO = 4                                                      1477
         AG_TITLE = 'IBWF'                                                1477
         AG_EXNAME = 'ATTRIBUTE'                                          1477
         AG_PARLIST = 'SEEN_COLO'                                         1477
         CALL AXATTRIBUTE                                                 1477
C    *                                                                    1477
C    Geant SHAPE : TRD1 ===> DX1=WBSP_ANGLD(1)/2.0 DX2=WBSP_ANGLD(1)/2.0  1477
C    -WBSP_ANGLD(3), DY=WBSP_ANGLD(2)/2.0, DZ=WBSP_ANGLD(3)/2.0 <===      1477
         CALL AGSRESET                                                    1480
         AG_DX1 = WBSP_ANGLD(1)/2.0                                       1480
         AG_DX2 = WBSP_ANGLD(1)/2.0-WBSP_ANGLD(3)                         1480
         AG_DY = WBSP_ANGLD(2)/2.0                                        1480
         AG_DZ = WBSP_ANGLD(3)/2.0                                        1480
         AG_TITLE = 'TRD1'                                                1480
         AG_EXNAME = 'SHAPE'                                              1480
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     1480
         CALL AXSHAPE                                                     1480
C    *                                                                    1480
C    Create IBWG :                                                        1481
C    Call To Local Procedure IBWG                                         1481
         ASSIGN 6290 TO BLOCK_IBWG                                        1481
         GO TO  6280                                                      1481
6290     CONTINUE                                                         1481
C    *                                                                    1481
C    Geant POSITION : IBWG ===> X=0 Y=0 Z=0 <===                          1482
         CALL AGSRESET                                                    1482
         AG_X = 0                                                         1482
         AG_Y = 0                                                         1482
         AG_Z = 0                                                         1482
         AG_TITLE = 'IBWG'                                                1482
         AG_EXNAME = 'POSITION'                                           1482
         AG_PARLIST = 'X_Y_Z'                                             1482
         CALL AXPOSITION                                                  1482
C    *                                                                    1482
5271  CALL AGSPOPB                                                        1483
      GO TO BLOCK_IBWF, (5270 ,5280)                                      1483
C End of Local Procedure                                                  1483
C ----------------------------------------------------------------------  1484
C ------------ IBWG                                                       1484
C Begin Local Procedure IBWG                                              1485
C IBWG IS THE AIR IN ANGLED-POST FOR NEW WEST BEAMPIPE SUPPORT            1485
6280  CONTINUE                                                            1485
         AG_TITLE='IBWG'                                                  1485
         AG_MODULE=MODULE                                                 1485
         AG_MARK='V00'                                                    1485
         CALL AGSPUSH                                                     1485
         AG_MODULE=MODULE                                                 1485
C    *                                                                    1485
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1486
         CALL AGSRESET                                                    1486
         AG_TITLE = 'AIR'                                                 1486
         AG_EXNAME = 'MATERIAL'                                           1486
         AG_PARLIST = 'NONE'                                              1486
         CALL AXMATERIAL                                                  1486
C    *                                                                    1486
C    Geant ATTRIBUTE : IBWG ===> SEEN=1 COLO=4 <===                       1486
         CALL AGSRESET                                                    1487
         AG_SEEN = 1                                                      1487
         AG_COLO = 4                                                      1487
         AG_TITLE = 'IBWG'                                                1487
         AG_EXNAME = 'ATTRIBUTE'                                          1487
         AG_PARLIST = 'SEEN_COLO'                                         1487
         CALL AXATTRIBUTE                                                 1487
C    *                                                                    1487
C    Geant SHAPE : TRD1 ===> DX1=WBSP_ANGLD(1)/2.0-WBSP_ANGLD(4), DX2=WB  1487
C    SP_ANGLD(1)/2.0-WBSP_ANGLD(3)+WBSP_ANGLD(4), DY=WBSP_ANGLD(2)/2.0-W  1487
C    BSP_ANGLD(4), DZ=WBSP_ANGLD(3)/2.0-WBSP_ANGLD(4) <===                1487
         CALL AGSRESET                                                    1491
         AG_DX1 = WBSP_ANGLD(1)/2.0-WBSP_ANGLD(4)                         1491
         AG_DX2 = WBSP_ANGLD(1)/2.0-WBSP_ANGLD(3)+WBSP_ANGLD(4)           1491
         AG_DY = WBSP_ANGLD(2)/2.0-WBSP_ANGLD(4)                          1491
         AG_DZ = WBSP_ANGLD(3)/2.0-WBSP_ANGLD(4)                          1491
         AG_TITLE = 'TRD1'                                                1491
         AG_EXNAME = 'SHAPE'                                              1491
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     1491
         CALL AXSHAPE                                                     1491
C    *                                                                    1491
6281  CALL AGSPOPB                                                        1492
      GO TO BLOCK_IBWG, (6280 ,6290)                                      1492
C End of Local Procedure                                                  1492
C ----------------------------------------------------------------------  1493
C ------------ IBWH                                                       1493
C Begin Local Procedure IBWH                                              1494
C IBWH IS THE TRIANGL PIECE FOR NEW WEST BEAMPIPE SUPPORT                 1494
5290  CONTINUE                                                            1494
         AG_TITLE='IBWH'                                                  1494
         AG_MODULE=MODULE                                                 1494
         AG_MARK='V00'                                                    1494
         CALL AGSPUSH                                                     1494
         AG_MODULE=MODULE                                                 1494
C    *                                                                    1494
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1495
         CALL AGSRESET                                                    1495
         AG_TITLE = 'ALUMINIUM'                                           1495
         AG_EXNAME = 'MATERIAL'                                           1495
         AG_PARLIST = 'NONE'                                              1495
         CALL AXMATERIAL                                                  1495
C    *                                                                    1495
C    Geant ATTRIBUTE : IBWH ===> SEEN=1 COLO=4 <===                       1495
         CALL AGSRESET                                                    1496
         AG_SEEN = 1                                                      1496
         AG_COLO = 4                                                      1496
         AG_TITLE = 'IBWH'                                                1496
         AG_EXNAME = 'ATTRIBUTE'                                          1496
         AG_PARLIST = 'SEEN_COLO'                                         1496
         CALL AXATTRIBUTE                                                 1496
C    *                                                                    1496
C    Geant SHAPE : TRAP ===> DZ=WBSP_TRID(3)/2.0 THET=45.0/2.0, PHI=0 H1  1496
C    =0.0 BL1=WBSP_TRID(1)/2.0 TL1=WBSP_TRID(1)/2.0, ALP1=0 H2=WBSP_TRID  1496
C    (2)/2.0 BL2=WBSP_TRID(1)/2.0, TL2=WBSP_TRID(1)/2.0 ALP2=0 <===       1496
         CALL AGSRESET                                                    1500
         AG_DZ = WBSP_TRID(3)/2.0                                         1500
         AG_THET = 45.0/2.0                                               1500
         AG_PHI = 0                                                       1500
         AG_H1 = 0.0                                                      1500
         AG_BL1 = WBSP_TRID(1)/2.0                                        1500
         AG_TL1 = WBSP_TRID(1)/2.0                                        1500
         AG_ALP1 = 0                                                      1500
         AG_H2 = WBSP_TRID(2)/2.0                                         1500
         AG_BL2 = WBSP_TRID(1)/2.0                                        1500
         AG_TL2 = WBSP_TRID(1)/2.0                                        1500
         AG_ALP2 = 0                                                      1500
         AG_TITLE = 'TRAP'                                                1500
         AG_EXNAME = 'SHAPE'                                              1500
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       1500
         CALL AXSHAPE                                                     1500
C    *                                                                    1500
5291  CALL AGSPOPB                                                        1501
      GO TO BLOCK_IBWH, (5290 ,5300)                                      1501
C End of Local Procedure                                                  1501
      WRITE(LOUT,6300)'===>>  [End AgML Module VPDDGEO3]  <<==='          1501
6300  FORMAT(A32,/,/)                                                     1501
C MODULE VPDDGEO3                                                         1503
      END                                                                 1503
